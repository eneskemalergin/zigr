//! Narrow R atomic-vector wrapper.

const std = @import("std");
const R = @import("R");
const convert = @import("convert.zig");
const sexp_mod = @import("sexp.zig");

fn supportsElement(comptime T: type) bool {
    return T == f64 or T == i32 or T == u8 or T == convert.Rcomplex;
}

fn supportsArithmetic(comptime T: type) bool {
    return T == f64 or T == i32;
}

fn requireArithmetic(comptime T: type) void {
    if (!supportsArithmetic(T)) @compileError("RVector arithmetic supports only f64 and i32");
}

pub fn RVector(comptime T: type) type {
    if (!supportsElement(T)) @compileError("RVector supports f64, i32, u8, and convert.Rcomplex");

    return struct {
        sexp: R.SEXP,

        const Self = @This();

        fn expectType(sexp: R.SEXP) !void {
            const expected: c_int = @intCast(convert.typeToSEXPTYPE(T));
            if (expected != sexp_mod.typeTag(sexp)) return error.UnexpectedType;
        }

        pub fn init(sexp: R.SEXP) !Self {
            try expectType(sexp);
            return .{ .sexp = sexp };
        }

        pub fn len(self: Self) usize {
            const l = R.XLENGTH(self.sexp);
            return @as(usize, @intCast(if (l < 0) @as(R.R_xlen_t, 0) else l));
        }

        pub fn asSEXP(self: Self) R.SEXP {
            return self.sexp;
        }

        /// A borrowed view must not outlive its R call. For owned fallback
        /// storage, the allocator must remain valid during R unwinding.
        pub fn view(self: Self, allocator: std.mem.Allocator) !convert.SliceView(T) {
            if (comptime T == f64) return try convert.toRealSliceView(allocator, self.sexp);
            if (comptime T == i32) return try convert.toIntSliceView(allocator, self.sexp);
            if (comptime T == u8) return try convert.toRawSliceView(allocator, self.sexp);
            return try convert.toComplexSliceView(allocator, self.sexp);
        }

        pub fn addScalar(self: Self, scalar: T) R.SEXP {
            comptime requireArithmetic(T);
            return mapScalar(self, scalar, struct {
                fn op(a: T, b: T) T {
                    return a + b;
                }
            }.op);
        }

        pub fn subScalar(self: Self, scalar: T) R.SEXP {
            comptime requireArithmetic(T);
            return mapScalar(self, scalar, struct {
                fn op(a: T, b: T) T {
                    return a - b;
                }
            }.op);
        }

        pub fn mulScalar(self: Self, scalar: T) R.SEXP {
            comptime requireArithmetic(T);
            return mapScalar(self, scalar, struct {
                fn op(a: T, b: T) T {
                    return a * b;
                }
            }.op);
        }

        pub fn divScalar(self: Self, scalar: T) R.SEXP {
            comptime requireArithmetic(T);
            return mapScalar(self, scalar, struct {
                fn op(a: T, b: T) T {
                    return a / b;
                }
            }.op);
        }

        /// Copies the vector into one final R-owned result. The one-pass
        /// access keeps ordinary storage borrowed and streams an ALTREP
        /// fallback through its bounded region buffer.
        pub fn copy(self: Self) R.SEXP {
            var result = convert.ResultBuilder(T).init(self.len());
            defer result.deinit();

            var input = convert.toVectorAccess(T, .one_pass, std.heap.page_allocator, self.sexp) catch |err| convert.signalError(err);
            defer input.deinit();
            const output = result.mutableSlice();
            var offset: usize = 0;
            while (input.next() catch |err| convert.signalError(err)) |chunk| {
                @memcpy(output[offset .. offset + chunk.len], chunk);
                offset += chunk.len;
            }
            return result.finish();
        }

        pub fn add(self: Self, other: Self, allocator: std.mem.Allocator) R.SEXP {
            comptime requireArithmetic(T);
            return mapBinary(self, other, allocator, struct {
                fn op(a: T, b: T) T {
                    return a + b;
                }
            }.op);
        }

        pub fn sub(self: Self, other: Self, allocator: std.mem.Allocator) R.SEXP {
            comptime requireArithmetic(T);
            return mapBinary(self, other, allocator, struct {
                fn op(a: T, b: T) T {
                    return a - b;
                }
            }.op);
        }

        pub fn sum(self: Self) if (T == i32) i64 else f64 {
            comptime requireArithmetic(T);
            if (comptime T == i32) return convert.sumInt(self.sexp);
            return convert.sum(self.sexp);
        }

        fn mapScalar(self: Self, scalar: T, comptime op: fn (T, T) T) R.SEXP {
            var result = convert.ResultBuilder(T).init(self.len());
            defer result.deinit();

            var input = convert.toVectorAccess(T, .one_pass, std.heap.page_allocator, self.sexp) catch |err| convert.signalError(err);
            defer input.deinit();
            const out = result.mutableSlice();
            var offset: usize = 0;
            while (input.next() catch |err| convert.signalError(err)) |chunk| {
                for (chunk) |value| {
                    out[offset] = op(value, scalar);
                    offset += 1;
                }
            }
            return result.finish();
        }

        fn mapBinary(self: Self, other: Self, allocator: std.mem.Allocator, comptime op: fn (T, T) T) R.SEXP {
            const lhs_len = self.len();
            const rhs_len = other.len();
            const n = if (lhs_len == 0 or rhs_len == 0) @as(usize, 0) else @max(lhs_len, rhs_len);
            var result = convert.ResultBuilder(T).init(n);
            defer result.deinit();

            var lhs_view = self.view(allocator) catch |convert_error| convert.signalError(convert_error);
            defer lhs_view.deinit();
            var rhs_view = other.view(allocator) catch |convert_error| convert.signalError(convert_error);
            defer rhs_view.deinit();
            const lhs = lhs_view.constSlice();
            const rhs = rhs_view.constSlice();

            const out = result.mutableSlice();
            for (0..n) |index| {
                out[index] = op(lhs[index % lhs.len], rhs[index % rhs.len]);
            }
            return result.finish();
        }
    };
}

test "RVector type instantiation" {
    _ = RVector(f64);
    _ = RVector(i32);
    _ = RVector(u8);
    _ = RVector(convert.Rcomplex);
}

test "RVector support classification" {
    try std.testing.expect(supportsElement(f64));
    try std.testing.expect(supportsElement(i32));
    try std.testing.expect(supportsElement(u8));
    try std.testing.expect(supportsElement(convert.Rcomplex));
    try std.testing.expect(!supportsElement(bool));
    try std.testing.expect(supportsArithmetic(f64));
    try std.testing.expect(supportsArithmetic(i32));
    try std.testing.expect(!supportsArithmetic(u8));
    try std.testing.expect(!supportsArithmetic(convert.Rcomplex));
}
