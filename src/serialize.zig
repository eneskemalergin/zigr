//! R serialization through public R APIs.
//!
//! `toVector` writes portable XDR bytes with format version 3. The returned
//! RAWSXP and values returned by `fromVector` are unprotected. Both operations
//! can allocate in R and longjmp. Callers keep input SEXPs reachable across
//! each operation. Serialization may invoke an input ALTREP class's
//! serialization or element callbacks. Ordinary raw inputs use R's public
//! `unserialize` operation; ALTREP raw inputs use the persistent-stream API so
//! decoding can read regions without requesting a contiguous data pointer.
//!
//! R describes the custom persistent-stream API as highly experimental. The
//! ALTREP input path is pinned to the installed `Rinternals.h` declarations.

const std = @import("std");
const R = @import("R");
const cleanup = @import("cleanup");
const err = @import("error");
const protect = @import("protect.zig");

/// Supported R serialization format versions.
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

const SerializeRequest = struct {
    value: R.SEXP,
    version: Version,
};

fn serializeCall(data: ?*anyopaque) R.SEXP {
    const request: *SerializeRequest = @ptrCast(@alignCast(data orelse
        err.signal("serialization request is null")));
    var input = protect.scoped(request.value);
    defer input.deinit();
    var version = protect.scoped(R.Rf_ScalarInteger(@intFromEnum(request.version)));
    defer version.deinit();
    var call = protect.scoped(R.Rf_lang4(
        R.Rf_install("serialize"),
        input.get(),
        R.R_NilValue,
        version.get(),
    ));
    defer call.deinit();
    R.SET_TAG(R.CDR(R.CDR(R.CDR(call.get()))), R.Rf_install("version"));
    const result = R.Rf_eval(call.get(), R.R_BaseEnv);
    if (result == null or R.TYPEOF(result) != R.RAWSXP) {
        err.signal("serialization did not return a raw vector");
    }
    return result;
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
    serialized: R.SEXP,
    len: R.R_xlen_t,
    offset: R.R_xlen_t = 0,
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
    const requested: R.R_xlen_t = @intCast(count);
    if (context.offset < 0 or context.offset > context.len or requested > context.len - context.offset) {
        err.signal("unexpected end of serialized input");
    }
    const bytes: [*]u8 = @ptrCast(output);
    if (R.ALTREP(context.serialized) == 0) {
        const source = R.RAW(context.serialized);
        @memcpy(bytes[0..count], source[@as(usize, @intCast(context.offset))..][0..count]);
        context.offset += requested;
        return;
    }
    var copied: R.R_xlen_t = 0;
    while (copied < requested) {
        const got = R.RAW_GET_REGION(
            context.serialized,
            context.offset + copied,
            requested - copied,
            bytes + @as(usize, @intCast(copied)),
        );
        if (got <= 0 or got > requested - copied) {
            err.signal("serialized ALTREP input returned an invalid region length");
        }
        copied += got;
    }
    context.offset += requested;
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

    if (R.ALTREP(input.get()) == 0) {
        var call = protect.scoped(R.Rf_lang2(R.Rf_install("unserialize"), input.get()));
        defer call.deinit();
        return R.Rf_eval(call.get(), R.R_BaseEnv);
    }

    const raw_len = R.XLENGTH(input.get());
    if (raw_len < 0) err.signal("serialized input has negative length");
    var context = InputContext{
        .serialized = input.get(),
        .len = raw_len,
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
