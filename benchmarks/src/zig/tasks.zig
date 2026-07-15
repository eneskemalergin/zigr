//! Historical task kernels grouped by responsibility while retaining a private namespace per task.

const task_01_vectorsum = struct {
    const R = @import("R");
    const simd = @import("simd");
    const raw = @import("zigr").raw;

    const SEXP = R.SEXP;

    export fn zigr_bench_vectorsum(vec: SEXP) SEXP {
        const slice = raw.real(vec);
        var total: f64 = 0.0;

        var i: usize = 0;
        const n = slice.len;

        const lanes = simd.f64_lanes;
        if (n >= lanes) {
            var vec_total: @Vector(lanes, f64) = @splat(0.0);
            const end = n - (n % lanes);
            while (i < end) : (i += lanes) {
                vec_total += slice[i..][0..lanes].*;
            }
            total += @reduce(.Add, vec_total);
        }

        while (i < n) : (i += 1) {
            total += slice[i];
        }

        return R.Rf_ScalarReal(total);
    }
};

const task_02_elem_ops = struct {
    const R = @import("R");
    const std = @import("std");

    const SEXP = R.SEXP;

    export fn zigr_bench_elem_ops(vec_sexp: SEXP) SEXP {
        const n = @as(usize, @intCast(R.XLENGTH(vec_sexp)));
        const src = R.REAL(vec_sexp);

        const result = R.Rf_protect(R.Rf_allocMatrix(R.REALSXP, @intCast(n), 4));
        defer R.Rf_unprotect(1);
        const rp = @as([*]f64, @ptrCast(R.REAL(result)));

        for (0..n) |i| {
            const v = src[i];
            rp[i] = @abs(v);
            rp[i + n] = if (v > 0) @log(v) else 0.0;
            rp[i + 2 * n] = @exp(v);
            rp[i + 3 * n] = if (v >= 0) @sqrt(v) else 0.0;
        }

        return result;
    }
};

const task_03_memcpy_bandwidth = struct {
    const std = @import("std");
    const R = @import("R");
    const raw = @import("zigr").raw;

    const SEXP = R.SEXP;
    const repeats: usize = 2;
    const strategy_names = [_][:0]const u8{ "copy_temp", "copy_out", "fill_out" };

    fn sumSlice(data: []const f64) f64 {
        var total: f64 = 0.0;
        for (data) |value| total += value;
        return total;
    }

    fn setNames(result: SEXP) void {
        const names = R.Rf_protect(R.Rf_allocVector(R.STRSXP, strategy_names.len));
        defer R.Rf_unprotect(1);

        for (strategy_names, 0..) |name, index| {
            R.SET_STRING_ELT(names, @intCast(index), R.Rf_mkChar(name));
        }
        _ = R.Rf_setAttrib(result, R.R_NamesSymbol, names);
    }

    export fn zigr_bench_memcpy_bandwidth(vec: SEXP) SEXP {
        const input = raw.real(vec);
        const result = R.Rf_protect(R.Rf_allocVector(R.REALSXP, strategy_names.len));
        defer R.Rf_unprotect(1);
        const out = R.REAL(result);

        var copy_temp_total: f64 = 0.0;
        var copy_out_total: f64 = 0.0;
        var fill_out_total: f64 = 0.0;

        for (0..repeats) |_| {
            const temp = std.heap.c_allocator.alloc(f64, input.len) catch unreachable;
            defer std.heap.c_allocator.free(temp);
            std.mem.copyForwards(f64, temp, input);
            copy_temp_total += sumSlice(temp);

            const copy_out = R.Rf_protect(R.Rf_allocVector(R.REALSXP, @intCast(input.len)));
            const copy_slice = @as([*]f64, @ptrCast(R.REAL(copy_out)))[0..input.len];
            std.mem.copyForwards(f64, copy_slice, input);
            copy_out_total += sumSlice(copy_slice);
            R.Rf_unprotect(1);

            const fill_out = R.Rf_protect(R.Rf_allocVector(R.REALSXP, @intCast(input.len)));
            const fill_slice = @as([*]f64, @ptrCast(R.REAL(fill_out)))[0..input.len];
            for (input, 0..) |value, index| {
                fill_slice[index] = value + 0.5;
            }
            fill_out_total += sumSlice(fill_slice);
            R.Rf_unprotect(1);
        }

        out[0] = copy_temp_total;
        out[1] = copy_out_total;
        out[2] = fill_out_total;
        setNames(result);
        return result;
    }
};

const task_04_sort = struct {
    const std = @import("std");
    const R = @import("R");
    const raw = @import("zigr").raw;

    const SEXP = R.SEXP;

    fn radixSortF64(items: []f64) void {
        if (items.len < 2) return;

        const n = items.len;
        const buf = @as([*]u64, @ptrCast(@alignCast(items.ptr)))[0..n];
        const sign_bit: u64 = 1 << 63;

        for (buf) |*b| {
            const v = b.*;
            b.* = if (v & sign_bit != 0) ~v else v ^ sign_bit;
        }

        var stack_temp: [256]u64 = undefined;
        const temp = if (n <= 256) blk: {
            break :blk stack_temp[0..n];
        } else blk: {
            const ptr = std.heap.c_allocator.alloc(u64, n) catch unreachable;
            break :blk ptr;
        };
        const needs_free = n > 256;

        var src: []u64 = buf;
        var dst: []u64 = temp;
        var counts: [256]usize = undefined;
        var shift: usize = 0;
        while (shift < 64) : (shift += 8) {
            const s: u6 = @intCast(shift);
            @memset(counts[0..], 0);
            for (src) |v| counts[(v >> s) & 0xFF] += 1;

            var total: usize = 0;
            for (&counts) |*c| {
                const old = c.*;
                c.* = total;
                total += old;
            }

            for (src) |v| {
                const digit = (v >> s) & 0xFF;
                dst[counts[digit]] = v;
                counts[digit] += 1;
            }

            const swap = src;
            src = dst;
            dst = swap;
        }

        if (needs_free) std.heap.c_allocator.free(temp);

        for (buf) |*b| {
            const v = b.*;
            b.* = if (v & sign_bit != 0) v ^ sign_bit else ~v;
        }
    }

    export fn zigr_bench_sort(vec_sexp: SEXP) SEXP {
        const src = raw.real(vec_sexp);
        const n = src.len;

        const result = R.Rf_protect(R.Rf_allocVector(R.REALSXP, @intCast(n)));
        defer R.Rf_unprotect(1);
        const rp = raw.realMut(result);
        @memcpy(rp, src);

        radixSortF64(rp);

        return result;
    }
};

const task_05_fib_recursive = struct {
    const R = @import("R");

    const SEXP = R.SEXP;

    fn fib(n: i64) i64 {
        if (n <= 1) return n;
        return fib(n - 1) + fib(n - 2);
    }

    export fn zigr_bench_fib_recursive(n_sexp: SEXP) SEXP {
        const n = @as(i64, @intCast(R.Rf_asInteger(n_sexp)));
        const result = fib(n);
        // Keep the reference result exact beyond the i32 range.
        return R.Rf_ScalarReal(@as(f64, @floatFromInt(result)));
    }
};

const task_06_broadcast = struct {
    const R = @import("R");
    const simd = @import("simd");
    const raw = @import("zigr").raw;

    const SEXP = R.SEXP;

    export fn zigr_bench_broadcast(vec_sexp: SEXP, scalar_sexp: SEXP) SEXP {
        const src = raw.real(vec_sexp);
        const scalar = raw.real(scalar_sexp)[0];
        const n = src.len;

        const lanes = simd.f64_lanes;
        const s: @Vector(lanes, f64) = @splat(scalar);
        var i: usize = 0;
        var vec_total: @Vector(lanes, f64) = @splat(0.0);
        if (n >= lanes) {
            const end = n - (n % lanes);
            while (i < end) : (i += lanes) {
                vec_total += src[i..][0..lanes].* + s;
            }
        }
        var total = @reduce(.Add, vec_total);
        while (i < n) : (i += 1) total += src[i] + scalar;
        return R.Rf_ScalarReal(total);
    }
};

const task_07a_protect_shallow = struct {
    const R = @import("R");
    const protect = @import("zigr").protect;

    const SEXP = R.SEXP;

    export fn zigr_bench_protect_shallow(vec: SEXP) SEXP {
        for (0..100) |_| {
            for (0..10) |_| _ = protect.protect(vec);
            protect.unprotectN(10);
        }
        return R.Rf_ScalarInteger(0);
    }
};

const task_07b_protect_scaling = struct {
    const R = @import("R");
    const protect = @import("zigr").protect;

    const SEXP = R.SEXP;

    export fn zigr_bench_protect_scaling(vec: SEXP) SEXP {
        for (0..100) |_| {
            for (0..10) |_| {
                for (0..10000) |_| _ = protect.protect(vec);
                protect.unprotectN(10000);
            }
        }
        return R.Rf_ScalarInteger(0);
    }
};

const task_08_type_dispatch = struct {
    const R = @import("R");
    const sexp = @import("zigr").sexp;
    const SEXP = R.SEXP;

    export fn zigr_bench_type_dispatch(arg: SEXP) SEXP {
        const t0 = R.VECTOR_ELT(arg, 0);
        const t1 = R.VECTOR_ELT(arg, 1);
        const t2 = R.VECTOR_ELT(arg, 2);
        var total: i32 = 0;
        for (0..2048) |_| {
            total += switch (sexp.typeTag(t0)) {
                14 => 1,
                13 => 2,
                16 => 3,
                else => 0,
            };
            total += switch (sexp.typeTag(t1)) {
                14 => 1,
                13 => 2,
                16 => 3,
                else => 0,
            };
            total += switch (sexp.typeTag(t2)) {
                14 => 1,
                13 => 2,
                16 => 3,
                else => 0,
            };
        }
        return R.Rf_ScalarInteger(total);
    }
};

const task_09_longjmp_safety = struct {
    const R = @import("R");
    const raw = @import("zigr").raw;

    const SEXP = R.SEXP;
    const repeats: usize = 512;
    const strategy_names = [_][:0]const u8{ "direct", "try_ok", "try_err", "unwind_ok" };

    const UnwindState = struct {
        input: []const f64,
        bias: f64,
    };

    fn adjustedSum(input: []const f64, bias: f64) f64 {
        var total: f64 = 0.0;
        for (input) |value| total += value + bias;
        return total;
    }

    fn fillAdjusted(vec: SEXP, input: []const f64, bias: f64) void {
        const data = R.REAL(vec);
        for (input, 0..) |value, index| data[index] = value + bias;
    }

    fn setNames(result: SEXP) void {
        const names = R.Rf_protect(R.Rf_allocVector(R.STRSXP, strategy_names.len));
        defer R.Rf_unprotect(1);

        for (strategy_names, 0..) |name, index| {
            R.SET_STRING_ELT(names, @intCast(index), R.Rf_mkChar(name));
        }
        _ = R.Rf_setAttrib(result, R.R_NamesSymbol, names);
    }

    fn unwindNoop(_: ?*anyopaque, _: R.Rboolean) callconv(.c) void {}

    fn unwindOkCallback(data: ?*anyopaque) callconv(.c) SEXP {
        const state = @as(*const UnwindState, @ptrCast(@alignCast(data.?)));
        return R.Rf_ScalarReal(adjustedSum(state.input, state.bias));
    }

    export fn zigr_bench_longjmp_safety(vec: SEXP) SEXP {
        const input = raw.real(vec);
        const result = R.Rf_protect(R.Rf_allocVector(R.REALSXP, strategy_names.len));
        defer R.Rf_unprotect(1);

        const out = R.REAL(result);
        const sum_sym = R.Rf_install("sum");
        const stop_sym = R.Rf_install("stop");
        const stop_msg = R.Rf_protect(R.Rf_mkString("task32"));
        defer R.Rf_unprotect(1);
        const stop_call = R.Rf_protect(R.Rf_lang2(stop_sym, stop_msg));
        defer R.Rf_unprotect(1);

        var direct_total: f64 = 0.0;
        var try_ok_total: f64 = 0.0;
        var try_err_total: f64 = 0.0;
        var unwind_ok_total: f64 = 0.0;

        for (0..repeats) |repeat_index| {
            const bias = (@as(f64, @floatFromInt(repeat_index)) + 1.0) * 0.001;
            direct_total += adjustedSum(input, bias);

            const temp = R.Rf_protect(R.Rf_allocVector(R.REALSXP, @intCast(input.len)));
            fillAdjusted(temp, input, bias);
            const expr = R.Rf_protect(R.Rf_lang2(sum_sym, temp));
            var err: c_int = 0;
            const eval_result = R.R_tryEvalSilent(expr, R.R_GlobalEnv, &err);
            try_ok_total += R.REAL(eval_result)[0];
            R.Rf_unprotect(2);

            err = 0;
            _ = R.R_tryEvalSilent(stop_call, R.R_GlobalEnv, &err);
            if (err != 0) try_err_total += 1.0;

            var state = UnwindState{ .input = input, .bias = bias };
            const cont = R.Rf_protect(R.R_MakeUnwindCont());
            const unwind_result = R.R_UnwindProtect(unwindOkCallback, &state, unwindNoop, null, cont);
            unwind_ok_total += R.REAL(unwind_result)[0];
            R.Rf_unprotect(1);
        }

        out[0] = direct_total;
        out[1] = try_ok_total;
        out[2] = try_err_total;
        out[3] = unwind_ok_total;
        setNames(result);
        return result;
    }
};

const task_10_sexp_create = struct {
    const R = @import("R");
    const SEXP = R.SEXP;

    export fn zigr_bench_sexp_create(_: SEXP) SEXP {
        for (0..10) |_| {
            for (0..10000) |_| {
                _ = R.Rf_protect(R.Rf_allocVector(R.REALSXP, 1));
            }
            R.Rf_unprotect(10000);
        }
        return R.Rf_ScalarInteger(0);
    }
};

const task_11_sexp_inspect = struct {
    const R = @import("R");
    const sexp = @import("zigr").sexp;
    const SEXP = R.SEXP;

    fn isVectorTag(tag: u5) bool {
        return switch (tag) {
            10, 13, 14, 15, 16, 19, 20, 24 => true,
            else => false,
        };
    }

    export fn zigr_bench_sexp_inspect(arg: SEXP) SEXP {
        const n = sexp.xlength(arg);
        var elts: [5]SEXP = undefined;
        for (0..n) |i| elts[i] = R.VECTOR_ELT(arg, @intCast(i));

        var per_elt: [5]i32 = undefined;
        for (0..n) |i| {
            const tag = sexp.typeTag(elts[i]);
            per_elt[i] = @as(i32, tag) + @as(i32, @intFromBool(isVectorTag(tag))) + @as(i32, @intFromBool(tag == 14));
        }

        var total: i32 = 0;
        for (0..10000) |_| {
            for (0..n) |i| total += per_elt[i];
        }
        return R.Rf_ScalarInteger(total);
    }
};

const task_12_matrix_transpose = struct {
    const R = @import("R");
    const raw = @import("zigr").raw;
    const SEXP = R.SEXP;
    const block_len: usize = 32;

    export fn zigr_bench_matrix_transpose(mat_sexp: SEXP) SEXP {
        const nr = @as(usize, @intCast(R.Rf_nrows(mat_sexp)));
        const nc = @as(usize, @intCast(R.Rf_ncols(mat_sexp)));
        const data = raw.real(mat_sexp);

        const result = R.Rf_protect(R.Rf_allocMatrix(R.REALSXP, @intCast(nc), @intCast(nr)));
        defer R.Rf_unprotect(1);
        const rp = raw.realMut(result);

        var jj: usize = 0;
        while (jj < nc) : (jj += block_len) {
            const j_end = @min(jj + block_len, nc);
            var ii: usize = 0;
            while (ii < nr) : (ii += block_len) {
                const i_end = @min(ii + block_len, nr);
                var i: usize = ii;
                while (i < i_end) : (i += 1) {
                    const out_row = rp[i * nc .. (i + 1) * nc];
                    var j: usize = jj;
                    while (j < j_end) : (j += 1) {
                        out_row[j] = data[i + (j * nr)];
                    }
                }
            }
        }

        return result;
    }
};

const task_13_matrix_rowsums = struct {
    const R = @import("R");
    const raw = @import("zigr").raw;

    const SEXP = R.SEXP;

    export fn zigr_bench_matrix_rowsums(mat_sexp: SEXP) SEXP {
        const nr = @as(usize, @intCast(R.Rf_nrows(mat_sexp)));
        const nc = @as(usize, @intCast(R.Rf_ncols(mat_sexp)));
        const data = raw.real(mat_sexp);

        const result = R.Rf_protect(R.Rf_allocVector(R.REALSXP, @intCast(nr)));
        defer R.Rf_unprotect(1);
        const rp = raw.realMut(result);
        @memset(rp, 0.0);

        for (0..nc) |j| {
            const col = data[j * nr ..][0..nr];
            for (0..nr) |i| rp[i] += col[i];
        }

        return result;
    }
};

const task_14_matrix_rowcol_means = struct {
    const R = @import("R");
    const raw = @import("zigr").raw;

    const SEXP = R.SEXP;

    export fn zigr_bench_matrix_rowcol_means(mat_sexp: SEXP) SEXP {
        const nr = @as(usize, @intCast(R.Rf_nrows(mat_sexp)));
        const nc = @as(usize, @intCast(R.Rf_ncols(mat_sexp)));
        const data = raw.real(mat_sexp);

        const row_means = R.Rf_protect(R.Rf_allocVector(R.REALSXP, @intCast(nr)));
        const col_sums = R.Rf_protect(R.Rf_allocVector(R.REALSXP, @intCast(nc)));
        defer R.Rf_unprotect(2);

        const rm = raw.realMut(row_means);
        const cs = raw.realMut(col_sums);

        @memset(rm, 0.0);

        // R does not promise SIMD alignment, so this loop must accept any address.
        for (0..nc) |j| {
            const col = data[j * nr ..][0..nr];
            var i: usize = 0;
            var col_acc: @Vector(4, f64) = @splat(0.0);
            while (i + 4 <= nr) : (i += 4) {
                const v: @Vector(4, f64) = col[i..][0..4].*;
                col_acc += v;
                const rp: *align(1) @Vector(4, f64) = @ptrCast(&rm[i]);
                rp.* += v;
            }
            cs[j] = @reduce(.Add, col_acc);
            while (i < nr) : (i += 1) {
                rm[i] += col[i];
                cs[j] += col[i];
            }
        }

        const inv_nc = 1.0 / @as(f64, @floatFromInt(nc));
        for (0..nr) |i| rm[i] *= inv_nc;

        const result = R.Rf_protect(R.Rf_allocVector(R.VECSXP, 2));
        defer R.Rf_unprotect(1);
        _ = R.SET_VECTOR_ELT(result, 0, row_means);
        _ = R.SET_VECTOR_ELT(result, 1, col_sums);

        return result;
    }
};

const task_15_dataframe_filter = struct {
    const R = @import("R");
    const dataframe = @import("zigr").dataframe;
    const raw = @import("zigr").raw;

    const SEXP = R.SEXP;

    export fn zigr_bench_dataframe_filter(df_sexp: SEXP) SEXP {
        const df = dataframe.DataFrame.wrap(df_sexp) orelse return R.R_NilValue;
        const nrows = @as(usize, @intCast(df.rowCount()));

        const x_idx = df.columnIndex("x") orelse return R.R_NilValue;
        const y_idx = df.columnIndex("y") orelse return R.R_NilValue;
        const grp_idx = df.columnIndex("grp") orelse return R.R_NilValue;

        const x_col = df.columnByIndex(x_idx);
        const y_col = df.columnByIndex(y_idx);
        const grp_col = df.columnByIndex(grp_idx);

        const x_slice = raw.real(x_col);
        const y_slice = raw.real(y_col);
        const grp_raw = raw.int(grp_col);

        var max_grp: i32 = 0;
        for (0..nrows) |i| {
            if (x_slice[i] > 0.0 and grp_raw[i] > max_grp) max_grp = grp_raw[i];
        }

        const ngroups = @as(usize, @intCast(max_grp));
        const grp_out = R.Rf_protect(R.Rf_allocVector(R.INTSXP, @intCast(ngroups)));
        defer R.Rf_unprotect(1);
        const sum_out = R.Rf_protect(R.Rf_allocVector(R.REALSXP, @intCast(ngroups)));
        defer R.Rf_unprotect(1);

        const grp_out_slice = raw.intMut(grp_out);
        const sums = raw.realMut(sum_out);
        @memset(sums, 0.0);

        for (0..ngroups) |i| grp_out_slice[i] = @intCast(i + 1);

        for (0..nrows) |i| {
            if (x_slice[i] > 0.0) {
                sums[@as(usize, @intCast(grp_raw[i] - 1))] += x_slice[i] / y_slice[i];
            }
        }

        const cols = [_]SEXP{ grp_out, sum_out };
        const result = R.Rf_protect(dataframe.build(&.{ "grp", "z_sum" }, cols[0..]));
        defer R.Rf_unprotect(1);
        return result;
    }
};

const task_16_list_access = struct {
    const R = @import("R");
    const sexp = @import("zigr").sexp;
    const SEXP = R.SEXP;

    export fn zigr_bench_list_access(arg: SEXP) SEXP {
        const n = sexp.xlength(arg);
        var total: f64 = 0.0;
        for (0..n) |i| {
            const elt = sexp.fastVectorElt(arg, i);
            const data = sexp.fastDataPtr(elt) orelse return R.R_NilValue;
            total += @as([*]const f64, @ptrCast(@alignCast(data)))[0];
        }
        return R.Rf_ScalarReal(total);
    }
};

const task_17_string_concat = struct {
    const std = @import("std");
    const R = @import("R");
    const sexp = @import("zigr").sexp;
    const SEXP = R.SEXP;
    const sep = ", ";

    export fn zigr_bench_string_concat(vec: SEXP) SEXP {
        const n = sexp.xlength(vec);

        var total: usize = 0;
        var output_encoding: R.cetype_t = @intCast(R.CE_UTF8);
        for (0..n) |i| {
            const elt = sexp.fastVectorElt(vec, i);
            if (elt == R.R_NaString) {
                total += 2;
            } else {
                if (sexp.fastGetCharCE(elt) == R.CE_BYTES) output_encoding = @intCast(R.CE_BYTES);
                total += sexp.charsxpBytes(elt).len;
            }
        }
        if (n > 1) total += (n - 1) * sep.len;

        // This buffer outlives R allocations made while assembling the result.
        const buf = std.heap.c_allocator.alloc(u8, total) catch unreachable;
        defer std.heap.c_allocator.free(buf);

        var pos: usize = 0;
        for (0..n) |i| {
            const elt = sexp.fastVectorElt(vec, i);
            if (elt == R.R_NaString) {
                @memcpy(buf[pos..][0..2], "NA");
                pos += 2;
            } else {
                const bytes = sexp.charsxpBytes(elt);
                const len = bytes.len;
                if (len > 0) {
                    @memcpy(buf[pos..][0..len], bytes);
                    pos += len;
                }
            }
            if (i + 1 < n) {
                @memcpy(buf[pos..][0..sep.len], sep);
                pos += sep.len;
            }
        }

        const out = R.Rf_protect(R.Rf_allocVector(R.STRSXP, 1));
        defer R.Rf_unprotect(1);
        const cs = R.Rf_mkCharLenCE(buf.ptr, @intCast(pos), output_encoding);
        R.SET_STRING_ELT(out, 0, cs);
        return out;
    }
};

const task_18_string_nchar = struct {
    const R = @import("R");
    const sexp = @import("zigr").sexp;
    const SEXP = R.SEXP;

    export fn zigr_bench_string_nchar(vec: SEXP) SEXP {
        const n = sexp.xlength(vec);
        var total: i64 = 0;
        for (0..n) |i| {
            const elt = sexp.fastVectorElt(vec, i);
            if (elt == R.R_NaString) continue;
            total += @as(i64, @intCast(sexp.fastLength(elt)));
        }
        return R.Rf_ScalarInteger(@intCast(total));
    }
};

const task_19_string_encoding = struct {
    const R = @import("R");
    const sexp = @import("zigr").sexp;

    export fn zigr_bench_string_encoding(vec: R.SEXP) R.SEXP {
        const n = sexp.xlength(vec);
        var total: i32 = 0;
        for (0..n) |i| {
            const elt = sexp.fastVectorElt(vec, i);
            total += @intFromBool(sexp.fastGetCharCE(elt) == R.CE_UTF8);
        }
        return R.Rf_ScalarInteger(total);
    }
};

const task_20_factor_ops = struct {
    const R = @import("R");
    const zigr = @import("zigr");
    const sexp = zigr.sexp;
    const factor = zigr.factor;

    export fn zigr_bench_factor_ops(vec: R.SEXP) R.SEXP {
        const n = sexp.xlength(vec);
        const fac = factor.asFactor(vec);
        const codes: [*]c_int = @ptrCast(R.INTEGER(fac));
        var total: i64 = 0;
        for (0..n) |i| {
            const c = codes[i];
            if (c != R.R_NaInt) total += @as(i64, @intCast(c));
        }
        return R.Rf_ScalarInteger(@intCast(total));
    }
};

const task_21_attrib_ops = struct {
    const R = @import("R");
    const zigr = @import("zigr");
    const sexp = zigr.sexp;
    const attrib = zigr.attrib;

    export fn zigr_bench_attrib_ops(vec: R.SEXP) R.SEXP {
        attrib.setClass(vec, "bench_class");

        const cr_sym = R.Rf_install("creator");
        const cr_val = R.Rf_protect(R.Rf_allocVector(R.STRSXP, 1));
        R.SET_STRING_ELT(cr_val, 0, R.Rf_mkChar("zigr_bench"));
        attrib.setAttrib(vec, cr_sym, cr_val);
        R.Rf_unprotect(1);

        const got_cls = attrib.getAttrib(vec, R.R_ClassSymbol);
        const got_cr = attrib.getAttrib(vec, cr_sym);

        var total: i32 = 0;
        const nc = sexp.fastLength(got_cls);
        for (0..@as(usize, @intCast(nc))) |i| {
            const elt = sexp.fastVectorElt(got_cls, @intCast(i));
            total += @as(i32, @intCast(sexp.fastLength(elt)));
        }
        const ncr = sexp.fastLength(got_cr);
        for (0..@as(usize, @intCast(ncr))) |i| {
            const elt = sexp.fastVectorElt(got_cr, @intCast(i));
            total += @as(i32, @intCast(sexp.fastLength(elt)));
        }
        return R.Rf_ScalarInteger(total);
    }
};

const task_22_s4_slot_access = struct {
    const std = @import("std");
    const R = @import("R");
    const s4 = @import("zigr").s4;

    var class_defined = std.atomic.Value(bool).init(false);

    export fn zigr_bench_s4_slot_access(vec: R.SEXP) R.SEXP {
        if (!class_defined.load(.monotonic)) {
            const class_name = R.Rf_protect(R.Rf_mkString("BenchS4"));
            const slot_type = R.Rf_protect(R.Rf_mkString("numeric"));
            const rep_call = R.Rf_protect(R.Rf_lang2(R.Rf_install("representation"), slot_type));
            R.SET_TAG(R.CDR(rep_call), R.Rf_install("slot_x"));
            const sc_call = R.Rf_protect(R.Rf_lang3(R.Rf_install("setClass"), class_name, rep_call));
            var err: c_int = 0;
            _ = R.R_tryEvalSilent(sc_call, R.R_GlobalEnv, &err);
            R.Rf_unprotect(4);
            if (err != 0) return vec;
            class_defined.store(true, .monotonic);
        }

        const obj = R.Rf_protect(s4.newObject("BenchS4"));
        const assigned = R.Rf_protect(s4.setSlot(obj, "slot_x", vec));
        const result = s4.getSlot(assigned, "slot_x");
        R.Rf_unprotect(2);
        return result;
    }
};

const task_23_na_propagation = struct {
    const R = @import("R");
    const convert = @import("zigr").convert;

    const SEXP = R.SEXP;

    export fn zigr_bench_na_prop(vec: SEXP) SEXP {
        return R.Rf_ScalarReal(convert.mean_narm(vec));
    }
};

const task_24_long_vector_idx = struct {
    const R = @import("R");

    export fn zigr_bench_long_vector_idx(vec: R.SEXP) R.SEXP {
        const n = R.XLENGTH(vec);
        var total: i64 = 0;
        var i: i64 = 0;
        while (i < n) {
            total += @as(i64, @intCast(R.INTEGER_ELT(vec, i)));
            i += 10000;
        }
        return R.Rf_ScalarReal(@as(f64, @floatFromInt(total)));
    }
};

const task_25_l1_arithmetic = struct {
    const R = @import("R");
    const convert = @import("zigr").convert;

    export fn zigr_bench_l1_arithmetic(vec: R.SEXP) R.SEXP {
        var total: f64 = 0.0;
        for (0..2500) |_| {
            total += convert.scaleAdd(vec, 0.5, 0.5);
        }
        return R.Rf_ScalarReal(total);
    }
};

const task_26_matmul = struct {
    const R = @import("R");

    const SEXP = R.SEXP;

    const dgemm_ = @extern(*const fn (transa: [*c]const u8, transb: [*c]const u8, m: [*c]const c_int, n: [*c]const c_int, k: [*c]const c_int, alpha: [*c]const f64, a: [*c]const f64, lda: [*c]const c_int, b: [*c]const f64, ldb: [*c]const c_int, beta: [*c]const f64, c: [*c]f64, ldc: [*c]const c_int, len_a: c_int, len_b: c_int) callconv(.c) void, .{ .name = "dgemm_" });

    export fn zigr_bench_blas_matmul(a_sexp: SEXP, b_sexp: SEXP) SEXP {
        const n = R.Rf_nrows(a_sexp);
        const m = R.Rf_ncols(b_sexp);
        const k = R.Rf_ncols(a_sexp);

        const result = R.Rf_protect(R.Rf_allocVector(R.REALSXP, n * m));
        const dims = R.Rf_protect(R.Rf_allocVector(R.INTSXP, 2));
        defer R.Rf_unprotect(2);
        R.INTEGER(dims)[0] = n;
        R.INTEGER(dims)[1] = m;
        _ = R.Rf_setAttrib(result, R.R_DimSymbol, dims);
        const rp = @as([*]f64, @ptrCast(R.REAL(result)));

        const alpha: f64 = 1.0;
        const beta: f64 = 0.0;
        const notrans: u8 = 'N';

        dgemm_(
            @ptrCast(&notrans),
            @ptrCast(&notrans),
            @ptrCast(&n),
            @ptrCast(&m),
            @ptrCast(&k),
            @ptrCast(&alpha),
            @ptrCast(R.REAL(a_sexp)),
            @ptrCast(&n), // lda = n (rows of A)
            @ptrCast(R.REAL(b_sexp)),
            @ptrCast(&k), // ldb = k (rows of B)
            @ptrCast(&beta),
            @ptrCast(rp),
            @ptrCast(&n), // ldc = n (rows of C)
            1,
            1,
        );

        return result;
    }
};

const task_27_crossprod = struct {
    const R = @import("R");

    const SEXP = R.SEXP;

    const dsyrk_ = @extern(*const fn (uplo: [*c]const u8, trans: [*c]const u8, n: [*c]const c_int, k: [*c]const c_int, alpha: [*c]const f64, a: [*c]const f64, lda: [*c]const c_int, beta: [*c]const f64, c: [*c]f64, ldc: [*c]const c_int, len_up: c_int, len_tr: c_int) callconv(.c) void, .{ .name = "dsyrk_" });

    export fn zigr_bench_crossprod(x_sexp: SEXP) SEXP {
        const nr = R.Rf_nrows(x_sexp);
        const nc = R.Rf_ncols(x_sexp);
        const n = nc;

        const result = R.Rf_protect(R.Rf_allocMatrix(R.REALSXP, nc, nc));
        defer R.Rf_unprotect(1);
        const rp = @as([*]f64, @ptrCast(R.REAL(result)));

        const alpha: f64 = 1.0;
        const beta: f64 = 0.0;
        const uplo: u8 = 'U';
        const trans: u8 = 'T';

        dsyrk_(
            @ptrCast(&uplo),
            @ptrCast(&trans),
            @ptrCast(&nc),
            @ptrCast(&nr),
            @ptrCast(&alpha),
            @ptrCast(R.REAL(x_sexp)),
            @ptrCast(&nr),
            @ptrCast(&beta),
            @ptrCast(rp),
            @ptrCast(&nc),
            1,
            1,
        );

        const nu = @as(usize, @intCast(n));
        for (0..nu) |i| {
            for (0..i) |j| {
                rp[j * nu + i] = rp[i * nu + j];
            }
        }

        return result;
    }
};

const task_29_lm_fit = struct {
    const R = @import("R");
    const raw = @import("zigr").raw;

    const SEXP = R.SEXP;

    const dsyrk_ = @extern(*const fn (uplo: [*c]const u8, trans: [*c]const u8, n: [*c]const c_int, k: [*c]const c_int, alpha: [*c]const f64, a: [*c]const f64, lda: [*c]const c_int, beta: [*c]const f64, c: [*c]f64, ldc: [*c]const c_int, len_up: c_int, len_tr: c_int) callconv(.c) void, .{ .name = "dsyrk_" });
    const dgemm_ = @extern(*const fn (transa: [*c]const u8, transb: [*c]const u8, m: [*c]const c_int, n: [*c]const c_int, k: [*c]const c_int, alpha: [*c]const f64, a: [*c]const f64, lda: [*c]const c_int, b: [*c]const f64, ldb: [*c]const c_int, beta: [*c]const f64, c: [*c]f64, ldc: [*c]const c_int, len_a: c_int, len_b: c_int) callconv(.c) void, .{ .name = "dgemm_" });
    const dpotrf_ = @extern(*const fn (uplo: [*c]const u8, n: [*c]const c_int, a: [*c]f64, lda: [*c]const c_int, info: [*c]c_int, len_up: c_int) callconv(.c) void, .{ .name = "dpotrf_" });
    const dtrsm_ = @extern(*const fn (side: [*c]const u8, uplo: [*c]const u8, transa: [*c]const u8, diag: [*c]const u8, m: [*c]const c_int, n: [*c]const c_int, alpha: [*c]const f64, a: [*c]const f64, lda: [*c]const c_int, b: [*c]f64, ldb: [*c]const c_int, len_s: c_int, len_u: c_int, len_t: c_int, len_d: c_int) callconv(.c) void, .{ .name = "dtrsm_" });

    export fn zigr_bench_lm(x_sexp: SEXP, y_sexp: SEXP) SEXP {
        const n = R.Rf_nrows(x_sexp);
        const p = R.Rf_ncols(x_sexp);

        const x_data = raw.real(x_sexp);
        const y_data = raw.real(y_sexp);

        const xtx = R.Rf_protect(R.Rf_allocMatrix(R.REALSXP, p, p));
        const xty_prot = R.Rf_protect(R.Rf_allocVector(R.REALSXP, p));
        defer R.Rf_unprotect(2);
        const xtx_rp = R.REAL(xtx);
        const xty_rp = R.REAL(xty_prot);

        const alpha: f64 = 1.0;
        const beta: f64 = 0.0;
        const notrans: u8 = 'N';
        const trans: u8 = 'T';
        const uplo: u8 = 'U';
        const one: c_int = 1;

        dsyrk_(ptr(&uplo), ptr(&trans), ptr(&p), ptr(&n), ptr(&alpha), x_data.ptr, ptr(&n), ptr(&beta), xtx_rp, ptr(&p), 1, 1);

        dgemm_(ptr(&trans), ptr(&notrans), ptr(&p), ptr(&one), ptr(&n), ptr(&alpha), x_data.ptr, ptr(&n), y_data.ptr, ptr(&n), ptr(&beta), xty_rp, ptr(&p), 1, 1);

        var info: c_int = 0;
        dpotrf_(ptr(&uplo), ptr(&p), xtx_rp, ptr(&p), @ptrCast(&info), 1);

        const side: u8 = 'L';
        const diag: u8 = 'N';
        dtrsm_(ptr(&side), ptr(&uplo), ptr(&trans), ptr(&diag), ptr(&p), ptr(&one), ptr(&alpha), xtx_rp, ptr(&p), xty_rp, ptr(&p), 1, 1, 1, 1);

        dtrsm_(ptr(&side), ptr(&uplo), ptr(&notrans), ptr(&diag), ptr(&p), ptr(&one), ptr(&alpha), xtx_rp, ptr(&p), xty_rp, ptr(&p), 1, 1, 1, 1);

        return xty_prot;
    }

    fn ptr(x: anytype) [*c]const @TypeOf(x.*) {
        return @ptrCast(x);
    }
};

const task_30_altrep_create = struct {
    const R = @import("R");

    export fn zigr_bench_altrep_create(vec: R.SEXP) R.SEXP {
        const call = R.Rf_protect(R.Rf_lang2(R.Rf_install("seq_len"), vec));
        var err: c_int = 0;
        const result = R.R_tryEvalSilent(call, R.R_GlobalEnv, &err);
        R.Rf_unprotect(1);
        if (err != 0) return R.R_NilValue;
        return result;
    }
};

const task_31_altrep_materialize = struct {
    const R = @import("R");

    export fn zigr_bench_altrep_materialize(vec: R.SEXP) R.SEXP {
        const call = R.Rf_protect(R.Rf_lang2(R.Rf_install("seq_len"), vec));
        var err: c_int = 0;
        const alt = R.R_tryEvalSilent(call, R.R_GlobalEnv, &err);
        R.Rf_unprotect(1);
        if (err != 0) return R.R_NilValue;
        // Rf_duplicate forces ALTREP to materialize its backing store.
        // Rf_coerceVector may return the same ALTREP when types match.
        const mat = R.Rf_duplicate(alt);
        const nu = @as(usize, @intCast(R.XLENGTH(mat)));
        const data: [*]c_int = @ptrCast(R.INTEGER(mat));
        const sum = data[0] + data[nu - 1];
        return R.Rf_ScalarInteger(sum);
    }
};

const task_32_altrep_elt_walk = struct {
    const R = @import("R");

    export fn zigr_bench_altrep_elt_walk(vec: R.SEXP) R.SEXP {
        const call = R.Rf_protect(R.Rf_lang2(R.Rf_install("seq_len"), vec));
        var err: c_int = 0;
        const alt = R.R_tryEvalSilent(call, R.R_GlobalEnv, &err);
        R.Rf_unprotect(1);
        if (err != 0) return R.R_NilValue;
        const n = R.XLENGTH(alt);
        var total: i64 = 0;
        var i: i64 = 0;
        while (i < n) {
            total += @as(i64, @intCast(R.INTEGER_ELT(alt, i)));
            i += 1;
        }
        return R.Rf_ScalarReal(@as(f64, @floatFromInt(total)));
    }
};

const task_33_altrep_region_read = struct {
    const R = @import("R");
    const zigr = @import("zigr");

    export fn zigr_bench_altrep_region_read(vec: R.SEXP) R.SEXP {
        const call = R.Rf_protect(R.Rf_lang2(R.Rf_install("seq_len"), vec));
        var err: c_int = 0;
        const alt = R.R_tryEvalSilent(call, R.R_GlobalEnv, &err);
        R.Rf_unprotect(1);
        if (err != 0) return R.R_NilValue;
        return R.Rf_ScalarReal(@as(f64, @floatFromInt(zigr.convert.sumInt(alt))));
    }
};

const task_34_altrep_sum_via_R = struct {
    const R = @import("R");

    export fn zigr_bench_altrep_sum_via_R(vec: R.SEXP) R.SEXP {
        const call = R.Rf_protect(R.Rf_lang2(R.Rf_install("seq_len"), vec));
        var err: c_int = 0;
        const alt = R.R_tryEvalSilent(call, R.R_GlobalEnv, &err);
        if (err != 0) {
            R.Rf_unprotect(1);
            return R.R_NilValue;
        }
        const sum_call = R.Rf_protect(R.Rf_lang2(R.Rf_install("sum"), alt));
        const res = R.R_tryEvalSilent(sum_call, R.R_GlobalEnv, &err);
        R.Rf_unprotect(2);
        if (err != 0) return R.R_NilValue;
        return res;
    }
};

const task_35_altrep_sum_native = struct {
    const R = @import("R");

    export fn zigr_bench_altrep_sum_native(vec: R.SEXP) R.SEXP {
        const call = R.Rf_protect(R.Rf_lang2(R.Rf_install("seq_len"), vec));
        var err: c_int = 0;
        const alt = R.R_tryEvalSilent(call, R.R_GlobalEnv, &err);
        R.Rf_unprotect(1);
        if (err != 0) return R.R_NilValue;
        const n = R.XLENGTH(alt);
        var total: i64 = 0;
        var i: i64 = 0;
        while (i < n) {
            total += @as(i64, @intCast(R.INTEGER_ELT(alt, i)));
            i += 1;
        }
        return R.Rf_ScalarReal(@as(f64, @floatFromInt(total)));
    }
};

const task_36_altrep_min_max = struct {
    const R = @import("R");

    export fn zigr_bench_altrep_min_max(vec: R.SEXP) R.SEXP {
        const call = R.Rf_protect(R.Rf_lang2(R.Rf_install("seq_len"), vec));
        var err: c_int = 0;
        const alt = R.R_tryEvalSilent(call, R.R_GlobalEnv, &err);
        R.Rf_unprotect(1);
        if (err != 0) return R.R_NilValue;
        const n = R.XLENGTH(alt);
        var min_val: c_int = R.INTEGER_ELT(alt, 0);
        var max_val: c_int = min_val;
        var i: i64 = 1;
        while (i < n) {
            const v = R.INTEGER_ELT(alt, i);
            if (v < min_val) min_val = v;
            if (v > max_val) max_val = v;
            i += 1;
        }
        return R.Rf_ScalarInteger(max_val - min_val);
    }
};

const task_37_altrep_no_na_query = struct {
    const R = @import("R");

    export fn zigr_bench_altrep_no_na_query(vec: R.SEXP) R.SEXP {
        const call = R.Rf_protect(R.Rf_lang2(R.Rf_install("seq_len"), vec));
        var err: c_int = 0;
        const alt = R.R_tryEvalSilent(call, R.R_GlobalEnv, &err);
        R.Rf_unprotect(1);
        if (err != 0) return R.R_NilValue;
        const n = R.XLENGTH(alt);
        var has_na: c_int = 0;
        var i: i64 = 0;
        while (i < n) {
            if (R.INTEGER_ELT(alt, i) == R.R_NaInt) {
                has_na = 1;
                break;
            }
            i += 1;
        }
        return R.Rf_ScalarInteger(has_na);
    }
};

const task_38_struct_convert = struct {
    const R = @import("R");
    const SEXP = R.SEXP;

    export fn zigr_bench_struct_convert(vec: SEXP) SEXP {
        const names = R.Rf_protect(R.Rf_allocVector(R.STRSXP, 10));
        defer R.Rf_unprotect(1);
        const field_names = [_][:0]const u8{ "id", "count", "level", "flag", "enabled", "ratio", "offset", "scale", "weights", "indices" };
        for (field_names, 0..) |name, i| R.SET_STRING_ELT(names, @intCast(i), R.Rf_mkChar(name));

        const result = R.Rf_protect(R.Rf_allocVector(R.VECSXP, 10));
        defer R.Rf_unprotect(1);
        _ = R.Rf_setAttrib(result, R.R_NamesSymbol, names);

        _ = R.SET_VECTOR_ELT(result, 0, R.Rf_ScalarInteger(R.Rf_asInteger(R.VECTOR_ELT(vec, 0))));
        _ = R.SET_VECTOR_ELT(result, 1, R.Rf_ScalarInteger(R.Rf_asInteger(R.VECTOR_ELT(vec, 1))));
        _ = R.SET_VECTOR_ELT(result, 2, R.Rf_ScalarInteger(R.Rf_asInteger(R.VECTOR_ELT(vec, 2))));
        _ = R.SET_VECTOR_ELT(result, 3, R.Rf_ScalarLogical(R.Rf_asLogical(R.VECTOR_ELT(vec, 3))));
        _ = R.SET_VECTOR_ELT(result, 4, R.Rf_ScalarLogical(R.Rf_asLogical(R.VECTOR_ELT(vec, 4))));
        _ = R.SET_VECTOR_ELT(result, 5, R.Rf_ScalarReal(R.Rf_asReal(R.VECTOR_ELT(vec, 5))));
        _ = R.SET_VECTOR_ELT(result, 6, R.Rf_ScalarReal(R.Rf_asReal(R.VECTOR_ELT(vec, 6))));
        _ = R.SET_VECTOR_ELT(result, 7, R.Rf_ScalarReal(R.Rf_asReal(R.VECTOR_ELT(vec, 7))));
        _ = R.SET_VECTOR_ELT(result, 8, R.VECTOR_ELT(vec, 8));
        _ = R.SET_VECTOR_ELT(result, 9, R.VECTOR_ELT(vec, 9));

        return result;
    }
};

const task_39_r_eval = struct {
    const R = @import("R");
    const zigr_eval = @import("zigr").eval;

    export fn zigr_bench_r_eval(vec: R.SEXP) R.SEXP {
        const args = [_]R.SEXP{vec};
        const sum_res = zigr_eval.callIn("sum", args[0..], R.R_GlobalEnv);
        const sum_val = R.REAL(sum_res)[0];

        const mean_res = zigr_eval.callIn("mean", args[0..], R.R_GlobalEnv);
        const mean_val = R.REAL(mean_res)[0];

        return R.Rf_ScalarReal(sum_val + mean_val);
    }
};

const task_40_r_tryeval = struct {
    const R = @import("R");
    const zigr = @import("zigr");
    const zigr_eval = zigr.eval;
    const lang = zigr.lang;
    const protect = zigr.protect;

    export fn zigr_bench_r_tryeval(_: R.SEXP) R.SEXP {
        var count: c_int = 0;
        for (0..512) |_| {
            var message = protect.scoped(R.Rf_mkString("task40"));
            defer message.deinit();
            var call = protect.scoped(lang.buildNamedCall("stop", .{message.get()}));
            defer call.deinit();
            if (zigr_eval.tryEvalSilent(call.get(), R.R_GlobalEnv) == null) count += 1;
        }
        return R.Rf_ScalarInteger(count);
    }
};

const task_41_serialize_roundtrip = struct {
    const R = @import("R");
    const zigr = @import("zigr");

    export fn zigr_bench_serialize_roundtrip(vec: R.SEXP) R.SEXP {
        const serialized = R.Rf_protect(zigr.serialize.toVector(vec));
        defer R.Rf_unprotect(1);
        const result = R.Rf_protect(zigr.serialize.fromVector(serialized));
        defer R.Rf_unprotect(1);

        const n = R.XLENGTH(result);
        const xp: [*]const f64 = @ptrCast(R.REAL(result));
        var total: f64 = 0.0;
        for (0..@as(usize, @intCast(n))) |i| total += xp[i];
        return R.Rf_ScalarReal(total);
    }
};

const task_42_external_ptr = struct {
    const R = @import("R");
    const zigr = @import("zigr");

    const externalptr = zigr.externalptr;

    const BenchmarkState = struct { value: i32 };

    fn deinitBenchmarkState(_: *BenchmarkState) void {}

    export fn zigr_bench_external_ptr(value: R.SEXP) R.SEXP {
        const expected = R.Rf_asInteger(value);
        var result = zigr.protect.scoped(externalptr.createTyped(
            BenchmarkState,
            .{ .value = expected },
            deinitBenchmarkState,
        ));
        defer result.deinit();
        const restored = externalptr.checkedPointer(BenchmarkState, result.get()) catch |pointer_error|
            zigr.@"error".signal(externalptr.errorMessage(pointer_error));
        if (restored.value != expected) zigr.@"error".signal("external-pointer benchmark lost its typed state");
        return result.get();
    }
};

const task_43_rng_stress = struct {
    const R = @import("R");
    const zigr = @import("zigr");

    threadlocal var rng_result: R.SEXP = null;

    fn fillNormalDraws() R.SEXP {
        const result = rng_result orelse zigr.@"error".signal("RNG benchmark result is not initialized");
        const values = R.REAL(result)[0..@as(usize, @intCast(R.XLENGTH(result)))];
        for (values) |*value| value.* = R.norm_rand();
        return result;
    }

    export fn zigr_bench_rng_stress(vec: R.SEXP) R.SEXP {
        const n = R.Rf_asInteger(vec);
        if (n < 0 or n == R.R_NaInt) zigr.@"error".signal("RNG benchmark expected a non-negative length");
        const result = R.Rf_protect(R.Rf_allocVector(R.REALSXP, n));
        defer R.Rf_unprotect(1);
        rng_result = result;
        defer rng_result = null;
        return zigr.rng.withRng(fillNormalDraws);
    }
};

const task_48_weakref_lifecycle = struct {
    const R = @import("R");
    const zigr = @import("zigr");

    export fn zigr_bench_weakref_lifecycle(count_sxp: R.SEXP) R.SEXP {
        const count = R.Rf_asInteger(count_sxp);
        if (count < 0 or count == R.R_NaInt) zigr.@"error".signal("weak-reference benchmark expected a non-negative count");

        var key = zigr.protect.scoped(R.R_NewEnv(R.R_EmptyEnv, 0, 29));
        defer key.deinit();
        var value = zigr.protect.scoped(R.Rf_ScalarReal(42.0));
        defer value.deinit();

        for (0..@as(usize, @intCast(count))) |_| {
            var reference = zigr.protect.scoped(zigr.weakref.make(key.get(), value.get(), null, false));
            defer reference.deinit();
            const stored = zigr.weakref.value(reference.get());
            if (zigr.weakref.key(reference.get()) != key.get() or
                R.TYPEOF(stored) != R.REALSXP or R.XLENGTH(stored) != 1 or R.REAL(stored)[0] != 42.0)
            {
                return R.R_NilValue;
            }
        }
        return R.Rf_ScalarInteger(count);
    }
};

const task_49_owned_altrep_create = struct {
    //! Representative callback sequence for a package-registered owned integer ALTREP.
    //!
    //! The exported construction name preserves task 49's stable manifest and routine identity.

    const std = @import("std");
    const R = @import("R");
    const zigr = @import("zigr");

    const BenchAltInteger = zigr.altrep_create.AltInteger("zigr_benchmarks", "benchmark_owned_integer");
    const REGION_CAPACITY = 4096;

    pub fn register(info: *R.DllInfo) void {
        BenchAltInteger.register(info);
    }

    fn isBenchmarkClass(value: R.SEXP) bool {
        return R.TYPEOF(value) == R.INTSXP and
            R.ALTREP(value) != 0 and
            std.mem.eql(u8, zigr.altrep.className(value), "benchmark_owned_integer") and
            std.mem.eql(u8, zigr.altrep.classPackage(value), "zigr_benchmarks");
    }

    fn numericScalar(value: R.SEXP) f64 {
        const value_type = R.TYPEOF(value);
        if (value_type != R.INTSXP and value_type != R.REALSXP) {
            zigr.@"error".signal("owned ALTREP callback benchmark received a non-numeric summary");
        }
        if (R.XLENGTH(value) != 1) {
            zigr.@"error".signal("owned ALTREP callback benchmark expected a scalar summary");
        }
        return switch (value_type) {
            R.INTSXP => blk: {
                const result = R.INTEGER_ELT(value, 0);
                if (result == R.R_NaInt) {
                    zigr.@"error".signal("owned ALTREP callback benchmark received an NA summary");
                }
                break :blk @floatFromInt(result);
            },
            R.REALSXP => blk: {
                const result = R.REAL_ELT(value, 0);
                if (R.ISNA(result) != 0 or R.ISNAN(result)) {
                    zigr.@"error".signal("owned ALTREP callback benchmark received a missing summary");
                }
                break :blk result;
            },
            else => unreachable,
        };
    }

    fn summary(name: []const u8, value: R.SEXP) R.SEXP {
        const args = [_]R.SEXP{value};
        return zigr.eval.callIn(name, args[0..], R.R_BaseEnv);
    }

    fn regionSum(value: R.SEXP) i64 {
        var buffer: [REGION_CAPACITY]i32 = undefined;
        const length = R.XLENGTH(value);
        var offset: R.R_xlen_t = 0;
        var total: i64 = 0;
        while (offset < length) {
            const requested = @min(length - offset, @as(R.R_xlen_t, @intCast(buffer.len)));
            const received = R.INTEGER_GET_REGION(value, offset, requested, buffer[0..].ptr);
            if (received <= 0 or received > requested) {
                zigr.@"error".signal("owned ALTREP callback benchmark received an invalid region");
            }
            for (buffer[0..@as(usize, @intCast(received))]) |item| total += item;
            offset += received;
        }
        return total;
    }

    export fn zigr_bench_owned_altrep_create(length_sxp: R.SEXP) R.SEXP {
        const len = zigr.convert.toIntScalar(length_sxp) catch |error_value|
            zigr.convert.signalError(error_value);
        if (len <= 0) {
            zigr.@"error".signal("owned ALTREP callback benchmark expected a positive length");
        }

        var source = zigr.protect.scoped(R.Rf_allocVector(R.INTSXP, len));
        defer source.deinit();
        const values = R.INTEGER(source.get())[0..@as(usize, @intCast(len))];
        for (values, 0..) |*value, index| value.* = @intCast(index + 1);

        var result = zigr.protect.scoped(BenchAltInteger.init(values));
        defer result.deinit();
        if (!isBenchmarkClass(result.get()) or R.XLENGTH(result.get()) != len) {
            zigr.@"error".signal("owned ALTREP callback benchmark constructed an invalid result");
        }

        var sum_result = zigr.protect.scoped(summary("sum", result.get()));
        defer sum_result.deinit();
        var min_result = zigr.protect.scoped(summary("min", result.get()));
        defer min_result.deinit();
        var max_result = zigr.protect.scoped(summary("max", result.get()));
        defer max_result.deinit();
        const expected_sum = @divExact(@as(i64, len) * (@as(i64, len) + 1), 2);
        const sum_value = numericScalar(sum_result.get());
        const min_value = numericScalar(min_result.get());
        const max_value = numericScalar(max_result.get());
        if (sum_value != @as(f64, @floatFromInt(expected_sum)) or min_value != 1.0 or max_value != @as(f64, @floatFromInt(len))) {
            zigr.@"error".signal("owned ALTREP callback benchmark received invalid summaries");
        }

        const region_sum = regionSum(result.get());
        if (region_sum != expected_sum) {
            zigr.@"error".signal("owned ALTREP callback benchmark received invalid region values");
        }

        var deep = zigr.protect.scoped(R.Rf_duplicate(result.get()));
        defer deep.deinit();
        var shallow = zigr.protect.scoped(R.Rf_shallow_duplicate(result.get()));
        defer shallow.deinit();
        if (R.TYPEOF(deep.get()) != R.INTSXP or R.ALTREP(deep.get()) != 0 or R.XLENGTH(deep.get()) != len or
            R.TYPEOF(shallow.get()) != R.INTSXP or R.ALTREP(shallow.get()) != 0 or R.XLENGTH(shallow.get()) != len)
        {
            zigr.@"error".signal("owned ALTREP callback benchmark received invalid duplicates");
        }
        R.INTEGER(deep.get())[0] = -1;
        R.INTEGER(shallow.get())[@as(usize, @intCast(len - 1))] = -2;
        if (R.INTEGER_ELT(result.get(), 0) != 1 or R.INTEGER_ELT(result.get(), len - 1) != len or
            R.INTEGER(deep.get())[0] != -1 or R.INTEGER(shallow.get())[@as(usize, @intCast(len - 1))] != -2 or
            (len > 1 and (R.INTEGER(deep.get())[@as(usize, @intCast(len - 1))] != len or R.INTEGER(shallow.get())[0] != 1)))
        {
            zigr.@"error".signal("owned ALTREP callback benchmark duplicates are not independent");
        }

        const sortedness = R.INTEGER_IS_SORTED(result.get());
        const no_na = R.INTEGER_NO_NA(result.get());
        if (sortedness != R.SORTED_INCR or no_na != 1) {
            zigr.@"error".signal("owned ALTREP callback benchmark received invalid metadata");
        }

        var serialized = zigr.protect.scoped(zigr.serialize.toVector(result.get()));
        defer serialized.deinit();
        var restored = zigr.protect.scoped(zigr.serialize.fromVector(serialized.get()));
        defer restored.deinit();
        if (!isBenchmarkClass(restored.get()) or R.XLENGTH(restored.get()) != len or
            R.INTEGER_ELT(restored.get(), 0) != 1 or R.INTEGER_ELT(restored.get(), len - 1) != len)
        {
            zigr.@"error".signal("owned ALTREP callback benchmark restored an invalid class");
        }

        const names = [_][]const u8{
            "sum",
            "min",
            "max",
            "region_sum",
            "sortedness",
            "no_na",
            "deep_first",
            "shallow_last",
            "restored_first",
            "restored_last",
        };
        var output = zigr.protect.scoped(R.Rf_allocVector(R.REALSXP, names.len));
        defer output.deinit();
        const output_values = R.REAL(output.get());
        output_values[0] = sum_value;
        output_values[1] = min_value;
        output_values[2] = max_value;
        output_values[3] = @floatFromInt(region_sum);
        output_values[4] = @floatFromInt(sortedness);
        output_values[5] = @floatFromInt(no_na);
        output_values[6] = @floatFromInt(R.INTEGER(deep.get())[0]);
        output_values[7] = @floatFromInt(R.INTEGER(shallow.get())[@as(usize, @intCast(len - 1))]);
        output_values[8] = @floatFromInt(R.INTEGER_ELT(restored.get(), 0));
        output_values[9] = @floatFromInt(R.INTEGER_ELT(restored.get(), len - 1));
        zigr.attrib.setNames(output.get(), names[0..]);
        return output.get();
    }
};

pub fn registerOwnedAltrep(info: *@import("R").DllInfo) void {
    task_49_owned_altrep_create.register(info);
}

const task_28_cholesky = struct {
    const R = @import("R");

    const SEXP = R.SEXP;

    const dpotrf_ = @extern(*const fn (uplo: [*c]const u8, n: [*c]const c_int, a: [*c]f64, lda: [*c]const c_int, info: [*c]c_int, len_up: c_int) callconv(.c) void, .{ .name = "dpotrf_" });

    export fn zigr_bench_cholesky(a_sexp: SEXP) SEXP {
        const n = R.Rf_nrows(a_sexp);
        const nu = @as(usize, @intCast(n));
        const len = nu * nu;

        const result = R.Rf_protect(R.Rf_allocMatrix(R.REALSXP, n, n));
        const rp = R.REAL(result);
        const src = R.REAL(a_sexp);
        @memcpy(rp[0..len], src[0..len]);

        var info: c_int = 0;
        const uplo: u8 = 'U';
        dpotrf_(ptr(&uplo), ptr(&n), rp, ptr(&n), @ptrCast(&info), 1);

        var col: usize = 0;
        while (col < nu) : (col += 1) {
            var row: usize = col + 1;
            while (row < nu) : (row += 1) {
                rp[col * nu + row] = 0.0;
            }
        }

        R.Rf_unprotect(1);
        return result;
    }

    fn ptr(x: anytype) [*c]const @TypeOf(x.*) {
        return @ptrCast(x);
    }
};

comptime {
    _ = task_01_vectorsum.zigr_bench_vectorsum;
    _ = task_02_elem_ops.zigr_bench_elem_ops;
    _ = task_03_memcpy_bandwidth.zigr_bench_memcpy_bandwidth;
    _ = task_04_sort.zigr_bench_sort;
    _ = task_05_fib_recursive.zigr_bench_fib_recursive;
    _ = task_06_broadcast.zigr_bench_broadcast;
    _ = task_07a_protect_shallow.zigr_bench_protect_shallow;
    _ = task_07b_protect_scaling.zigr_bench_protect_scaling;
    _ = task_08_type_dispatch.zigr_bench_type_dispatch;
    _ = task_09_longjmp_safety.zigr_bench_longjmp_safety;
    _ = task_10_sexp_create.zigr_bench_sexp_create;
    _ = task_11_sexp_inspect.zigr_bench_sexp_inspect;
    _ = task_12_matrix_transpose.zigr_bench_matrix_transpose;
    _ = task_13_matrix_rowsums.zigr_bench_matrix_rowsums;
    _ = task_14_matrix_rowcol_means.zigr_bench_matrix_rowcol_means;
    _ = task_15_dataframe_filter.zigr_bench_dataframe_filter;
    _ = task_16_list_access.zigr_bench_list_access;
    _ = task_17_string_concat.zigr_bench_string_concat;
    _ = task_18_string_nchar.zigr_bench_string_nchar;
    _ = task_19_string_encoding.zigr_bench_string_encoding;
    _ = task_20_factor_ops.zigr_bench_factor_ops;
    _ = task_21_attrib_ops.zigr_bench_attrib_ops;
    _ = task_22_s4_slot_access.zigr_bench_s4_slot_access;
    _ = task_23_na_propagation.zigr_bench_na_prop;
    _ = task_24_long_vector_idx.zigr_bench_long_vector_idx;
    _ = task_25_l1_arithmetic.zigr_bench_l1_arithmetic;
    _ = task_26_matmul.zigr_bench_blas_matmul;
    _ = task_27_crossprod.zigr_bench_crossprod;
    _ = task_28_cholesky.zigr_bench_cholesky;
    _ = task_29_lm_fit.zigr_bench_lm;
    _ = task_30_altrep_create.zigr_bench_altrep_create;
    _ = task_31_altrep_materialize.zigr_bench_altrep_materialize;
    _ = task_32_altrep_elt_walk.zigr_bench_altrep_elt_walk;
    _ = task_33_altrep_region_read.zigr_bench_altrep_region_read;
    _ = task_34_altrep_sum_via_R.zigr_bench_altrep_sum_via_R;
    _ = task_35_altrep_sum_native.zigr_bench_altrep_sum_native;
    _ = task_36_altrep_min_max.zigr_bench_altrep_min_max;
    _ = task_37_altrep_no_na_query.zigr_bench_altrep_no_na_query;
    _ = task_38_struct_convert.zigr_bench_struct_convert;
    _ = task_39_r_eval.zigr_bench_r_eval;
    _ = task_40_r_tryeval.zigr_bench_r_tryeval;
    _ = task_41_serialize_roundtrip.zigr_bench_serialize_roundtrip;
    _ = task_42_external_ptr.zigr_bench_external_ptr;
    _ = task_43_rng_stress.zigr_bench_rng_stress;
    _ = task_48_weakref_lifecycle.zigr_bench_weakref_lifecycle;
    _ = task_49_owned_altrep_create.zigr_bench_owned_altrep_create;
}
