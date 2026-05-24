//! Narrow vector wrapper over R atomic vectors.
//!
//! Zig does not support user-defined operator overloading, so arithmetic is
//! exposed through named methods rather than `+` or `*` syntax.

const std = @import("std");
const R = @import("R");
const convert = @import("convert.zig");
const protect = @import("protect.zig");

pub fn RVector(comptime T: type) type {
    return struct {
        sexp: R.SEXP,

        const Self = @This();

        fn typeName() []const u8 {
            return @typeName(T);
        }

        fn expectType(sexp: R.SEXP) !void {
            if (R.TYPEOF(sexp) != convert.typeToSEXPTYPE(T)) return error.UnexpectedType;
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

        pub fn view(self: Self, allocator: std.mem.Allocator) ![]const T {
            return switch (T) {
                f64 => try convert.toRealSliceView(allocator, self.sexp),
                i32 => try convert.toIntSliceView(allocator, self.sexp),
                u8 => try convert.toRawSlice(allocator, self.sexp),
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

        pub fn sum(self: Self) T {
            return switch (T) {
                f64 => convert.sum(self.sexp),
                i32 => @intCast(convert.sumInt(self.sexp)),
                else => @compileError("sum is only supported for numeric RVector types"),
            };
        }

        fn allocResult(n: usize) protect.ScopedProtect {
            return protect.scoped(R.Rf_allocVector(convert.typeToSEXPTYPE(T), @intCast(n)));
        }

        fn resultSlice(result: R.SEXP) []T {
            const rlen = R.XLENGTH(result);
            return convert.dataPtr(T, result)[0..@as(usize, @intCast(if (rlen < 0) @as(R.R_xlen_t, 0) else rlen))];
        }

        fn mapScalar(self: Self, scalar: T, comptime op: fn (T, T) T) R.SEXP {
            var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
            defer arena.deinit();
            const data = self.view(arena.allocator()) catch convert.signalError(error.UnexpectedType);

            var result = allocResult(data.len);
            defer result.deinit();
            const out = resultSlice(result.get());
            for (data, 0..) |value, index| out[index] = op(value, scalar);
            return result.get();
        }

        fn mapBinary(self: Self, other: Self, allocator: std.mem.Allocator, comptime op: fn (T, T) T) R.SEXP {
            const lhs = self.view(allocator) catch convert.signalError(error.UnexpectedType);
            const rhs = other.view(allocator) catch convert.signalError(error.UnexpectedType);
            const n = if (lhs.len == 0 or rhs.len == 0) @as(usize, 0) else @max(lhs.len, rhs.len);

            var result = allocResult(n);
            defer result.deinit();
            const out = resultSlice(result.get());
            for (0..n) |index| {
                out[index] = op(lhs[index % lhs.len], rhs[index % rhs.len]);
            }
            return result.get();
        }
    };
}

test "RVector type instantiation" {
    _ = RVector(f64);
    _ = RVector(i32);
    _ = RVector(u8);
    _ = RVector(convert.Rcomplex);
}
