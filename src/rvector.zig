//! Narrow R atomic-vector wrapper.

const std = @import("std");
const R = @import("R");
const convert = @import("convert.zig");
const sexp_mod = @import("sexp.zig");

pub fn RVector(comptime T: type) type {
    return struct {
        sexp: R.SEXP,

        const Self = @This();

        fn typeName() []const u8 {
            return @typeName(T);
        }

        fn expectType(sexp: R.SEXP) !void {
            const expected: u5 = @truncate(@as(u6, @intCast(convert.typeToSEXPTYPE(T))));
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

        /// A borrowed view must not outlive its R call.
        pub fn view(self: Self, allocator: std.mem.Allocator) !convert.SliceView(T) {
            return switch (T) {
                f64 => try convert.toRealSliceView(allocator, self.sexp),
                i32 => try convert.toIntSliceView(allocator, self.sexp),
                u8 => try convert.toRawSliceView(allocator, self.sexp),
                convert.Rcomplex => try convert.toComplexSliceView(allocator, self.sexp),
                else => unreachable,
            };
        }

        pub fn addScalar(self: Self, scalar: T) R.SEXP {
            return mapScalar(self, scalar, struct {
                fn op(a: T, b: T) T {
                    return a + b;
                }
            }.op);
        }

        pub fn subScalar(self: Self, scalar: T) R.SEXP {
            return mapScalar(self, scalar, struct {
                fn op(a: T, b: T) T {
                    return a - b;
                }
            }.op);
        }

        pub fn mulScalar(self: Self, scalar: T) R.SEXP {
            return mapScalar(self, scalar, struct {
                fn op(a: T, b: T) T {
                    return a * b;
                }
            }.op);
        }

        pub fn divScalar(self: Self, scalar: T) R.SEXP {
            return mapScalar(self, scalar, struct {
                fn op(a: T, b: T) T {
                    return a / b;
                }
            }.op);
        }

        pub fn add(self: Self, other: Self, allocator: std.mem.Allocator) R.SEXP {
            return mapBinary(self, other, allocator, struct {
                fn op(a: T, b: T) T {
                    return a + b;
                }
            }.op);
        }

        pub fn sub(self: Self, other: Self, allocator: std.mem.Allocator) R.SEXP {
            return mapBinary(self, other, allocator, struct {
                fn op(a: T, b: T) T {
                    return a - b;
                }
            }.op);
        }

        pub fn sum(self: Self) switch (T) {
            i32 => i64,
            else => T,
        } {
            return switch (T) {
                i32 => convert.sumInt(self.sexp),
                f64 => convert.sum(self.sexp),
                else => @compileError("sum is only supported for numeric RVector types"),
            };
        }

        fn mapScalar(self: Self, scalar: T, comptime op: fn (T, T) T) R.SEXP {
            var result = convert.ResultBuilder(T).init(self.len());
            defer result.deinit();

            var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
            defer arena.deinit();
            var input_view = self.view(arena.allocator()) catch convert.signalError(error.UnexpectedType);
            defer input_view.deinit();
            const data = input_view.constSlice();

            const out = result.mutableSlice();
            for (data, 0..) |value, index| out[index] = op(value, scalar);
            return result.finish();
        }

        fn mapBinary(self: Self, other: Self, allocator: std.mem.Allocator, comptime op: fn (T, T) T) R.SEXP {
            const lhs_len = self.len();
            const rhs_len = other.len();
            const n = if (lhs_len == 0 or rhs_len == 0) @as(usize, 0) else @max(lhs_len, rhs_len);
            var result = convert.ResultBuilder(T).init(n);
            defer result.deinit();

            var lhs_view = self.view(allocator) catch convert.signalError(error.UnexpectedType);
            defer lhs_view.deinit();
            var rhs_view = other.view(allocator) catch convert.signalError(error.UnexpectedType);
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
