//! R serialization through the public persistent-stream API.
//!
//! `toVector` writes portable XDR bytes with format version 3. The returned
//! RAWSXP and values returned by `fromVector` are unprotected. Both operations
//! can allocate in R and longjmp; their native stream state is unwind-safe.

const std = @import("std");
const R = @import("R");
const cleanup = @import("cleanup");
const err = @import("error");
const memory = @import("memory.zig");
const protect = @import("protect.zig");

/// R serialization versions supported by the public stream API.
pub const Version = enum(c_int) {
    v2 = 2,
    v3 = 3,
};

pub const SerializeError = error{
    NullInput,
    ExpectedRaw,
};

pub fn errorMessage(error_value: SerializeError) []const u8 {
    return switch (error_value) {
        error.NullInput => "serialization input is null",
        error.ExpectedRaw => "expected RAWSXP serialized input",
    };
}

const OutputContext = struct {
    allocator: std.mem.Allocator,
    bytes: std.ArrayList(u8) = .empty,
};

fn outputContext(stream: R.R_outpstream_t) *OutputContext {
    if (stream == null) err.signal("serialization output stream is null");
    const data = stream[0].data orelse err.signal("serialization output state is null");
    return @ptrCast(@alignCast(data));
}

fn outChar(stream: R.R_outpstream_t, value: c_int) callconv(.c) void {
    if (value < 0 or value > std.math.maxInt(u8)) err.signal("serialization produced an invalid byte");
    const context = outputContext(stream);
    context.bytes.append(context.allocator, @intCast(value)) catch
        err.signal("out of memory during serialization");
}

fn outBytes(stream: R.R_outpstream_t, data: ?*anyopaque, len: c_int) callconv(.c) void {
    if (len < 0) err.signal("serialization produced an invalid byte count");
    const count: usize = @intCast(len);
    if (count == 0) return;
    const raw = data orelse err.signal("serialization produced null bytes");
    const bytes = @as([*]const u8, @ptrCast(raw))[0..count];
    const context = outputContext(stream);
    context.bytes.appendSlice(context.allocator, bytes) catch
        err.signal("out of memory during serialization");
}

const SerializeRequest = struct {
    value: R.SEXP,
    version: Version,
};

fn serializeCall(data: ?*anyopaque) R.SEXP {
    const request: *SerializeRequest = @ptrCast(@alignCast(data orelse
        err.signal("serialization request is null")));
    var input = protect.scoped(request.value);
    defer input.deinit();

    var arena = memory.UnwindArena.init();
    defer arena.deinit();
    var context = OutputContext{ .allocator = arena.allocator() };
    var stream: R.struct_R_outpstream_st = .{};
    R.R_InitOutPStream(
        &stream,
        @ptrCast(&context),
        R.R_pstream_xdr_format,
        @intFromEnum(request.version),
        outChar,
        outBytes,
        null,
        R.R_NilValue,
    );
    R.R_Serialize(input.get(), &stream);

    if (context.bytes.items.len > std.math.maxInt(R.R_xlen_t)) {
        err.signal("serialized output exceeds R_xlen_t");
    }
    var result = protect.scoped(R.Rf_allocVector(R.RAWSXP, @intCast(context.bytes.items.len)));
    defer result.deinit();
    @memcpy(R.RAW(result.get())[0..context.bytes.items.len], context.bytes.items);
    return result.get();
}

/// Serializes with portable XDR encoding and an explicit R format version.
pub fn toVectorVersion(value: R.SEXP, version: Version) R.SEXP {
    if (value == null) err.signal(errorMessage(error.NullInput));
    var request = SerializeRequest{ .value = value, .version = version };
    return cleanup.protectCallData(serializeCall, @ptrCast(&request));
}

/// Serializes with portable XDR encoding and R serialization version 3.
pub fn toVector(value: R.SEXP) R.SEXP {
    return toVectorVersion(value, .v3);
}

const InputContext = struct {
    bytes: []const u8,
    offset: usize = 0,
};

fn inputContext(stream: R.R_inpstream_t) *InputContext {
    if (stream == null) err.signal("serialization input stream is null");
    const data = stream[0].data orelse err.signal("serialization input state is null");
    return @ptrCast(@alignCast(data));
}

fn readBytes(stream: R.R_inpstream_t, destination: ?*anyopaque, len: c_int) void {
    if (len < 0) err.signal("serialization requested an invalid byte count");
    const count: usize = @intCast(len);
    if (count == 0) return;
    const output = destination orelse err.signal("serialization requested a null destination");
    const context = inputContext(stream);
    if (context.offset > context.bytes.len or count > context.bytes.len - context.offset) {
        err.signal("unexpected end of serialized input");
    }
    @memcpy(
        @as([*]u8, @ptrCast(output))[0..count],
        context.bytes[context.offset..][0..count],
    );
    context.offset += count;
}

fn inChar(stream: R.R_inpstream_t) callconv(.c) c_int {
    var byte: u8 = undefined;
    readBytes(stream, @ptrCast(&byte), 1);
    return byte;
}

fn inBytes(stream: R.R_inpstream_t, destination: ?*anyopaque, len: c_int) callconv(.c) void {
    readBytes(stream, destination, len);
}

fn unserializeCall(data: ?*anyopaque) R.SEXP {
    const serialized: R.SEXP = @ptrCast(@alignCast(data orelse
        err.signal("unserialization request is null")));
    var input = protect.scoped(serialized);
    defer input.deinit();

    const raw_len = R.XLENGTH(input.get());
    if (raw_len < 0) err.signal("serialized input has negative length");
    var context = InputContext{
        .bytes = R.RAW(input.get())[0..@as(usize, @intCast(raw_len))],
    };
    var stream: R.struct_R_inpstream_st = .{};
    R.R_InitInPStream(
        &stream,
        @ptrCast(&context),
        R.R_pstream_any_format,
        inChar,
        inBytes,
        null,
        R.R_NilValue,
    );
    return R.R_Unserialize(&stream);
}

/// Rejects non-raw input before R attempts to decode it.
/// Malformed raw bytes still raise an R error through the active unwind path.
pub fn fromVectorChecked(serialized: R.SEXP) SerializeError!R.SEXP {
    if (serialized == null) return error.NullInput;
    if (R.TYPEOF(serialized) != R.RAWSXP) return error.ExpectedRaw;
    return cleanup.protectCallData(unserializeCall, @ptrCast(serialized));
}

/// Decodes a RAWSXP or raises an R error with a stable wrong-type message.
pub fn fromVector(serialized: R.SEXP) R.SEXP {
    return fromVectorChecked(serialized) catch |error_value|
        err.signal(errorMessage(error_value));
}

test "serialization contract types compile" {
    try std.testing.expectEqual(@as(c_int, 2), @intFromEnum(Version.v2));
    try std.testing.expectEqual(@as(c_int, 3), @intFromEnum(Version.v3));
    try std.testing.expectEqual(@TypeOf(toVector), fn (R.SEXP) R.SEXP);
    try std.testing.expectEqual(@TypeOf(fromVectorChecked), fn (R.SEXP) SerializeError!R.SEXP);
}
