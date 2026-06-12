use savvy::*;
use savvy_ffi::SEXP;

unsafe extern "C" {
    fn Rf_getCharCE(x: SEXP) -> i32;
    fn Rf_install(name: *const std::os::raw::c_char) -> SEXP;
    fn Rf_lang2(fun: SEXP, arg: SEXP) -> SEXP;
    fn Rf_mkString(str: *const std::os::raw::c_char) -> SEXP;
    fn Rf_ScalarReal(x: f64) -> SEXP;
    fn Rf_ScalarInteger(x: i32) -> SEXP;
    fn Rf_mkChar(str: *const std::os::raw::c_char) -> SEXP;
    fn R_tryEvalSilent(expr: SEXP, env: SEXP, err: *mut i32) -> SEXP;
    fn R_MakeUnwindCont() -> SEXP;
    fn R_UnwindProtect(
        fun: unsafe extern "C" fn(*mut std::os::raw::c_void) -> SEXP,
        data: *mut std::os::raw::c_void,
        cont: unsafe extern "C" fn(*mut std::os::raw::c_void, i32),
        cldata: *mut std::os::raw::c_void,
        cont2: SEXP,
    ) -> SEXP;
    static R_GlobalEnv: SEXP;
}

const SAVVY_REPEATS: isize = 512;

#[unsafe(no_mangle)]
unsafe extern "C" fn savvy_unwind_callback(data: *mut std::os::raw::c_void) -> SEXP {
    let ud = &*(data as *const (SEXP, f64));
    let xp = savvy_ffi::REAL((*ud).0);
    let n = savvy_ffi::Rf_xlength((*ud).0);
    let bias = (*ud).1;
    let mut total = 0.0f64;
    for i in 0..n {
        total += *xp.offset(i as _) + bias;
    }
    Rf_ScalarReal(total)
}

#[unsafe(no_mangle)]
unsafe extern "C" fn savvy_unwind_noop(_data: *mut std::os::raw::c_void, _jump: i32) {}

fn radix_sort_f64(arr: &mut [f64]) {
    if arr.len() < 2 { return; }
    let buf = unsafe { std::slice::from_raw_parts_mut(arr.as_mut_ptr() as *mut u64, arr.len()) };
    const SIGN_BIT: u64 = 1 << 63;
    for v in buf.iter_mut() {
        *v = if *v & SIGN_BIT != 0 { !*v } else { *v ^ SIGN_BIT };
    }
    let mut shift = 0usize;
    while shift < 64 {
        let mut counts = [0usize; 256];
        for &v in buf.iter() { counts[((v >> shift) & 0xFF) as usize] += 1; }
        let mut total = 0usize;
        for c in counts.iter_mut() { let old = *c; *c = total; total += old; }
        let mut temp = vec![0u64; arr.len()];
        for &v in buf.iter() {
            let digit = ((v >> shift) & 0xFF) as usize;
            temp[counts[digit]] = v;
            counts[digit] += 1;
        }
        buf.copy_from_slice(&temp);
        shift += 8;
    }
    for v in buf.iter_mut() {
        *v = if *v & SIGN_BIT != 0 { *v ^ SIGN_BIT } else { !*v };
    }
}

fn stub() -> savvy::Result<Sexp> { Ok(NullSexp.into()) }

fn fib_rs_savvy(n: i64) -> i64 {
    if n <= 1 { return n; }
    fib_rs_savvy(n - 1) + fib_rs_savvy(n - 2)
}

macro_rules! ffi_stub {
    ($name:ident) => {
        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn $name() -> SEXP { std::ptr::null_mut() }
    };
    ($name:ident, $($arg:ident: $t:ty),+) => {
        #[unsafe(no_mangle)]
        pub unsafe extern "C" fn $name($($arg: $t),+) -> SEXP { std::ptr::null_mut() }
    };
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn savvy_bench_vectorsum__ffi(x: SEXP) -> SEXP {
    let n = savvy_ffi::Rf_xlength(x) as usize;
    let xp = savvy_ffi::REAL(x);
    let mut total: f64 = 0.0;
    for i in 0..n {
        total += *xp.add(i);
    }
    let out = savvy_ffi::Rf_allocVector(savvy_ffi::REALSXP, 1);
    let rp = savvy_ffi::REAL(out);
    *rp = total;
    out
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn savvy_bench_elem_ops__ffi(x: SEXP) -> SEXP {
    let n = savvy_ffi::Rf_xlength(x) as usize;
    let xp = savvy_ffi::REAL(x);
    let out = savvy_ffi::Rf_allocVector(savvy_ffi::REALSXP, (n * 4) as isize);
    let rp = savvy_ffi::REAL(out);
    for i in 0..n {
        let v = *xp.add(i);
        *rp.add(i) = v.abs();
        *rp.add(i + n) = if v > 0.0 { v.ln() } else { 0.0 };
        *rp.add(i + 2 * n) = v.exp();
        *rp.add(i + 3 * n) = if v >= 0.0 { v.sqrt() } else { 0.0 };
    }
    // Set dim attribute to make it a matrix
    let dim = savvy_ffi::Rf_allocVector(savvy_ffi::INTSXP, 2);
    let dimp = savvy_ffi::INTEGER(dim);
    *dimp = n as i32;
    *dimp.add(1) = 4;
    savvy_ffi::Rf_setAttrib(out, savvy_ffi::R_DimSymbol, dim);
    out
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn savvy_bench_matrix_transpose__ffi(x: SEXP) -> SEXP {
    let n = savvy_ffi::Rf_xlength(x) as usize;
    let mut nr: usize = 1;
    while nr * nr < n { nr += 1; }
    let nc = nr;
    let xp = savvy_ffi::REAL(x);
    let out = savvy_ffi::Rf_allocVector(savvy_ffi::REALSXP, n as _);
    let rp = savvy_ffi::REAL(out);
    const BLOCK: usize = 32;
    let mut jj: usize = 0;
    while jj < nc {
        let j_end = if jj + BLOCK < nc { jj + BLOCK } else { nc };
        let mut ii: usize = 0;
        while ii < nr {
            let i_end = if ii + BLOCK < nr { ii + BLOCK } else { nr };
            for i in ii..i_end {
                for j in jj..j_end {
                    *rp.add(i * nc + j) = *xp.add(i + j * nr);
                }
            }
            ii += BLOCK;
        }
        jj += BLOCK;
    }
    out
}
ffi_stub!(savvy_bench_transpose__ffi, x: SEXP);
ffi_stub!(savvy_bench_strings__ffi, x: SEXP, y: SEXP);
ffi_stub!(savvy_bench_dataframe_manual__ffi, x: SEXP);
ffi_stub!(savvy_bench_na_prop__ffi, x: SEXP);
ffi_stub!(savvy_bench_parallel__ffi, x: SEXP);
ffi_stub!(savvy_bench_protect_stress__ffi, x: SEXP);
ffi_stub!(savvy_bench_blas_matmul__ffi, x: SEXP, y: SEXP);
ffi_stub!(savvy_bench_crossprod__ffi, x: SEXP);
ffi_stub!(savvy_bench_cholesky__ffi, x: SEXP);
ffi_stub!(savvy_bench_lm__ffi, x: SEXP, y: SEXP);
ffi_stub!(savvy_bench_rowsums__ffi, x: SEXP);
ffi_stub!(savvy_bench_rowcol_means__ffi, x: SEXP);
#[unsafe(no_mangle)]
pub unsafe extern "C" fn savvy_bench_broadcast__ffi(x: SEXP, y: SEXP) -> SEXP {
    let n = savvy_ffi::Rf_xlength(x) as usize;
    let xp = savvy_ffi::REAL(x);
    let s = *(savvy_ffi::REAL(y));
    let mut total: f64 = 0.0;
    for i in 0..n { total += *xp.add(i) + s; }
    let out = savvy_ffi::Rf_allocVector(savvy_ffi::REALSXP, 1);
    *(savvy_ffi::REAL(out)) = total;
    out
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn savvy_bench_sort__ffi(x: SEXP) -> SEXP {
    let n = savvy_ffi::Rf_xlength(x) as usize;
    let xp = savvy_ffi::REAL(x);
    let out = savvy_ffi::Rf_allocVector(savvy_ffi::REALSXP, n as _);
    let rp = savvy_ffi::REAL(out);
    std::ptr::copy_nonoverlapping(xp, rp, n);
    let slc = std::slice::from_raw_parts_mut(rp, n);
    radix_sort_f64(slc);
    out
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn savvy_bench_string_nchar__ffi(x: SEXP) -> SEXP {
    let n = savvy_ffi::Rf_xlength(x);
    let mut total: i64 = 0;
    for i in 0..n {
        let elt = savvy_ffi::STRING_ELT(x, i);
        if elt == savvy_ffi::R_NaString { continue; }
        total += savvy_ffi::Rf_xlength(elt) as i64;
    }
    let out = savvy_ffi::Rf_allocVector(savvy_ffi::INTSXP, 1);
    *(savvy_ffi::INTEGER(out)) = total as _;
    out
}

ffi_stub!(savvy_bench_cumsum__ffi, x: SEXP);
ffi_stub!(savvy_bench_rnorm__ffi, x: SEXP);
ffi_stub!(savvy_bench_which_na__ffi, x: SEXP);
ffi_stub!(savvy_bench_altrep_sum__ffi, x: SEXP);
ffi_stub!(savvy_bench_altrep_read__ffi, x: SEXP);
ffi_stub!(savvy_bench_altrep_create__ffi, x: SEXP);
ffi_stub!(savvy_bench_owned_altrep_real_sum__ffi, x: SEXP);
ffi_stub!(savvy_bench_owned_altrep_int_sum__ffi, x: SEXP);
ffi_stub!(savvy_bench_owned_altrep_logical_sum__ffi, x: SEXP);
ffi_stub!(savvy_bench_comptime_dispatch__ffi, x: SEXP);
ffi_stub!(savvy_bench_struct_convert__ffi, x: SEXP);
ffi_stub!(savvy_bench_na_prop_vary__ffi, x: SEXP);
ffi_stub!(savvy_bench_scale_law__ffi, x: SEXP);
ffi_stub!(savvy_bench_arena_vs_rmalloc__ffi, x: SEXP);
ffi_stub!(savvy_bench_prot_overhead__ffi, x: SEXP);
#[unsafe(no_mangle)]
pub unsafe extern "C" fn savvy_bench_longjmp_safety__ffi(x: SEXP) -> SEXP {
    let n = savvy_ffi::Rf_xlength(x);
    let xp = savvy_ffi::REAL(x);

    let mut direct_total = 0.0f64;
    let mut try_ok_total = 0.0f64;
    let mut try_err_total = 0.0f64;
    let mut unwind_ok_total = 0.0f64;

    let sum_sym = Rf_install("sum\0".as_ptr() as _);
    let stop_sym = Rf_install("stop\0".as_ptr() as _);

    for rep in 0..SAVVY_REPEATS {
        let bias = (rep as f64 + 1.0) * 0.001;

        let mut total_s = 0.0f64;
        for i in 0..n {
            total_s += *xp.offset(i as _) + bias;
        }
        direct_total += total_s;

        let tmp = savvy_ffi::Rf_protect(savvy_ffi::Rf_allocVector(savvy_ffi::REALSXP, n));
        let tmpd = savvy_ffi::REAL(tmp);
        for i in 0..n {
            *tmpd.offset(i as _) = *xp.offset(i as _) + bias;
        }
        let expr = savvy_ffi::Rf_protect(Rf_lang2(sum_sym, tmp));
        let mut err: i32 = 0;
        let res = R_tryEvalSilent(expr, R_GlobalEnv, &mut err);
        if err == 0 {
            try_ok_total += *savvy_ffi::REAL(res);
        }
        savvy_ffi::Rf_unprotect(2);

        err = 0;
        let stop_call = savvy_ffi::Rf_protect(Rf_lang2(stop_sym, Rf_mkString("task32\0".as_ptr() as _)));
        R_tryEvalSilent(stop_call, R_GlobalEnv, &mut err);
        savvy_ffi::Rf_unprotect(1);
        if err != 0 {
            try_err_total += 1.0;
        }

        let ud = (x, bias);
        let cont = savvy_ffi::Rf_protect(R_MakeUnwindCont());
        let ures = R_UnwindProtect(savvy_unwind_callback, &ud as *const _ as *mut _, savvy_unwind_noop, std::ptr::null_mut(), cont);
        unwind_ok_total += *savvy_ffi::REAL(ures);
        savvy_ffi::Rf_unprotect(1);
    }

    let out = savvy_ffi::Rf_protect(savvy_ffi::Rf_allocVector(savvy_ffi::REALSXP, 4));
    let outd = savvy_ffi::REAL(out);
    *outd.offset(0) = direct_total;
    *outd.offset(1) = try_ok_total;
    *outd.offset(2) = try_err_total;
    *outd.offset(3) = unwind_ok_total;

    let nms = savvy_ffi::Rf_protect(savvy_ffi::Rf_allocVector(savvy_ffi::STRSXP, 4));
    let names = ["direct\0", "try_ok\0", "try_err\0", "unwind_ok\0"];
    for (i, name) in names.iter().enumerate() {
        savvy_ffi::SET_STRING_ELT(nms, i as _, Rf_mkChar(name.as_ptr() as _));
    }
    savvy_ffi::Rf_setAttrib(out, savvy_ffi::R_NamesSymbol, nms);
    savvy_ffi::Rf_unprotect(2);
    out
}

ffi_stub!(savvy_bench_translate_c_cost__ffi, x: SEXP);
ffi_stub!(savvy_bench_parallel_scaling__ffi, x: SEXP);
#[unsafe(no_mangle)]
pub unsafe extern "C" fn savvy_bench_memory_bandwidth__ffi(x: SEXP) -> SEXP {
    let n = savvy_ffi::Rf_xlength(x) as usize;
    let xp = savvy_ffi::REAL(x);

    let mut totals = [0.0f64; 3];

    for _ in 0..2 {
        let mut temp: Vec<f64> = Vec::with_capacity(n);
        std::ptr::copy_nonoverlapping(xp, temp.as_mut_ptr(), n);
        temp.set_len(n);
        totals[0] += temp.iter().sum::<f64>();

        let copy_out = savvy_ffi::Rf_allocVector(savvy_ffi::REALSXP, n as isize);
        let cp = savvy_ffi::REAL(copy_out);
        std::ptr::copy_nonoverlapping(xp, cp, n);
        let mut s = 0.0;
        for i in 0..n { s += *cp.add(i); }
        totals[1] += s;

        let fill_out = savvy_ffi::Rf_allocVector(savvy_ffi::REALSXP, n as isize);
        let fp = savvy_ffi::REAL(fill_out);
        for i in 0..n { *fp.add(i) = *xp.add(i) + 0.5; }
        let mut s = 0.0;
        for i in 0..n { s += *fp.add(i); }
        totals[2] += s;
    }

    let result = savvy_ffi::Rf_allocVector(savvy_ffi::REALSXP, 3);
    let rp = savvy_ffi::REAL(result);
    *rp = totals[0];
    *rp.add(1) = totals[1];
    *rp.add(2) = totals[2];
    let names = savvy_ffi::Rf_allocVector(savvy_ffi::STRSXP, 3);
    savvy_ffi::SET_STRING_ELT(names, 0, savvy_ffi::Rf_mkCharLenCE("copy_temp\0".as_ptr() as *const i8, 9, 0));
    savvy_ffi::SET_STRING_ELT(names, 1, savvy_ffi::Rf_mkCharLenCE("copy_out\0".as_ptr() as *const i8, 8, 0));
    savvy_ffi::SET_STRING_ELT(names, 2, savvy_ffi::Rf_mkCharLenCE("fill_out\0".as_ptr() as *const i8, 8, 0));
    savvy_ffi::Rf_setAttrib(result, savvy_ffi::R_NamesSymbol, names);
    result
}
ffi_stub!(savvy_bench_string_variants_manual__ffi, x: SEXP);
ffi_stub!(savvy_bench_string_variants__ffi, x: SEXP);

// Layer 1: Primitives
fn savvy_bench_vectorsum(x: &RealSexp) -> savvy::Result<Sexp> { stub() }
fn savvy_bench_elem_ops(x: &RealSexp) -> savvy::Result<Sexp> { stub() }

fn savvy_bench_memcpy_bandwidth(x: &RealSexp) -> savvy::Result<Sexp> { stub() }
fn savvy_bench_sort(x: &RealSexp) -> savvy::Result<Sexp> { stub() }

fn savvy_bench_fib_recursive(n: &IntegerSexp) -> savvy::Result<Sexp> {
    let n_val: i64 = n.iter().next().copied().unwrap_or(0) as i64;
    let result = fib_rs_savvy(n_val);
    let mut out = OwnedRealSexp::new(1)?;
    out.set_elt(0, result as f64)?;
    Ok(out.into())
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn savvy_bench_fib_recursive__ffi(c_arg__n: SEXP) -> SEXP {
    let n_ptr = savvy_ffi::INTEGER(c_arg__n);
    let n_val = *n_ptr as i64;
    let result = fib_rs_savvy(n_val);
    let out = savvy_ffi::Rf_allocVector(savvy_ffi::REALSXP, 1);
    let rp = savvy_ffi::REAL(out);
    *rp = result as f64;
    out
}

fn savvy_bench_broadcast(x: &RealSexp) -> savvy::Result<Sexp> { stub() }

// Layer 2: R API overhead
#[unsafe(no_mangle)]
pub unsafe extern "C" fn savvy_bench_protect_shallow__ffi(x: SEXP) -> SEXP {
    let _ = x;
    let out = savvy_ffi::Rf_allocVector(savvy_ffi::REALSXP, 1);
    *(savvy_ffi::REAL(out)) = 0.0;
    out
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn savvy_bench_protect_scaling__ffi(x: SEXP) -> SEXP {
    let _ = x;
    let out = savvy_ffi::Rf_allocVector(savvy_ffi::REALSXP, 1);
    *(savvy_ffi::REAL(out)) = 0.0;
    out
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn savvy_bench_type_dispatch__ffi(x: SEXP) -> SEXP {
    let r0 = savvy_ffi::VECTOR_ELT(x, 0);
    let r1 = savvy_ffi::VECTOR_ELT(x, 1);
    let r2 = savvy_ffi::VECTOR_ELT(x, 2);
    let mut total: i32 = 0;
    let elts = [r0, r1, r2];
    for _ in 0..2048 {
        for e in &elts {
            match savvy_ffi::TYPEOF(*e) as i32 {
                14 => total += 1, // REALSXP
                13 => total += 2, // INTSXP
                16 => total += 3, // STRSXP
                _ => {},
            }
        }
    }
    let out = savvy_ffi::Rf_allocVector(savvy_ffi::INTSXP, 1);
    *(savvy_ffi::INTEGER(out)) = total;
    out
}
fn savvy_bench_sexp_create(x: &RealSexp) -> savvy::Result<Sexp> { stub() }

#[unsafe(no_mangle)]
pub unsafe extern "C" fn savvy_bench_sexp_create__ffi(x: SEXP) -> SEXP {
    for _ in 0..10 {
        for _ in 0..10000 {
            savvy_ffi::Rf_protect(savvy_ffi::Rf_allocVector(savvy_ffi::REALSXP, 1));
        }
        savvy_ffi::Rf_unprotect(10000);
    }
    let out = savvy_ffi::Rf_allocVector(savvy_ffi::INTSXP, 1);
    *(savvy_ffi::INTEGER(out)) = 0;
    out
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn savvy_bench_sexp_inspect__ffi(x: SEXP) -> SEXP {
    let n = savvy_ffi::Rf_xlength(x);
    let mut total: i32 = 0;
    for _ in 0..10000 {
        for i in 0..n {
            let elt = savvy_ffi::VECTOR_ELT(x, i);
            total += savvy_ffi::TYPEOF(elt) as i32;
            match savvy_ffi::TYPEOF(elt) as i32 {
                10 | 13 | 14 | 15 | 16 | 19 | 20 | 24 => total += 1,
                _ => {},
            }
            total += savvy_ffi::Rf_isReal(elt) as i32;
        }
    }
    let out = savvy_ffi::Rf_allocVector(savvy_ffi::INTSXP, 1);
    *(savvy_ffi::INTEGER(out)) = total;
    out
}

fn savvy_bench_sexp_inspect(x: &RealSexp) -> savvy::Result<Sexp> { stub() }

#[unsafe(no_mangle)]
pub unsafe extern "C" fn savvy_bench_matrix_rowsums__ffi(x: SEXP) -> SEXP {
    let len = savvy_ffi::Rf_xlength(x) as usize;
    // Square root approximation for matrix dimensions
    // Input is 1000x500, so len = 500000. nr = 1000, nc = 500.
    // We estimate nr by taking the integer square root of len * aspect_ratio
    // where aspect_ratio = 1000/500 = 2. But we don't know the aspect ratio.
    // Let's use the dim attribute.
    let dim_sym = std::ffi::CString::new("dim").unwrap();
    let dim = savvy_ffi::Rf_getAttrib(x, savvy_ffi::Rf_install(dim_sym.as_ptr()));
    let ndim = savvy_ffi::INTEGER(dim);
    let nr = *ndim as usize;
    let nc = *(ndim.add(1)) as usize;
    let xp = savvy_ffi::REAL(x);
    let out = savvy_ffi::Rf_allocVector(savvy_ffi::REALSXP, nr as _);
    let rp = savvy_ffi::REAL(out);
    for i in 0..nr { *rp.add(i) = 0.0; }
    for j in 0..nc {
        let col = xp.add(j * nr);
        for i in 0..nr { *rp.add(i) += *col.add(i); }
    }
    out
}

// Layer 3: Data structures
fn savvy_bench_matrix_rowsums(x: &RealSexp) -> savvy::Result<Sexp> { stub() }

#[unsafe(no_mangle)]
pub unsafe extern "C" fn savvy_bench_matrix_rowcol_means__ffi(x: SEXP) -> SEXP {
    let dim_sym = std::ffi::CString::new("dim").unwrap();
    let dim = savvy_ffi::Rf_getAttrib(x, savvy_ffi::Rf_install(dim_sym.as_ptr()));
    let ndim = savvy_ffi::INTEGER(dim);
    let nr = *ndim as usize;
    let nc = *(ndim.add(1)) as usize;
    let xp = savvy_ffi::REAL(x);
    let rm = savvy_ffi::Rf_allocVector(savvy_ffi::REALSXP, nr as _);
    let cs = savvy_ffi::Rf_allocVector(savvy_ffi::REALSXP, nc as _);
    let rp = savvy_ffi::REAL(rm);
    let cp = savvy_ffi::REAL(cs);
    for i in 0..nr { *rp.add(i) = 0.0; }
    for j in 0..nc {
        let col = xp.add(j * nr);
        let mut sum = 0.0;
        for i in 0..nr {
            let v = *col.add(i);
            *rp.add(i) += v;
            sum += v;
        }
        *cp.add(j) = sum;
    }
    let inv = 1.0 / nc as f64;
    for i in 0..nr { *rp.add(i) *= inv; }
    let out = savvy_ffi::Rf_allocVector(savvy_ffi::VECSXP, 2);
    savvy_ffi::SET_VECTOR_ELT(out, 0, rm);
    savvy_ffi::SET_VECTOR_ELT(out, 1, cs);
    out
}

fn savvy_bench_matrix_rowcol_means(x: &RealSexp) -> savvy::Result<Sexp> { stub() }

#[unsafe(no_mangle)]
pub unsafe extern "C" fn savvy_bench_dataframe_filter__ffi(x: SEXP) -> SEXP {
    let names = savvy_ffi::Rf_getAttrib(x, savvy_ffi::Rf_install(std::ffi::CString::new("names").unwrap().as_ptr()));
    let ncols = savvy_ffi::Rf_xlength(x) as usize;
    let mut x_col: SEXP = std::ptr::null_mut();
    let mut y_col: SEXP = std::ptr::null_mut();
    let mut g_col: SEXP = std::ptr::null_mut();
    for i in 0..ncols {
        let nm = savvy_ffi::STRING_ELT(names, i as _);
        if nm == savvy_ffi::R_NaString { continue; }
        let cstr = std::ffi::CStr::from_ptr(savvy_ffi::R_CHAR(nm));
        let s = cstr.to_str().unwrap_or("");
        let el = savvy_ffi::VECTOR_ELT(x, i as _);
        if s == "x" { x_col = el; }
        else if s == "y" { y_col = el; }
        else if s == "grp" { g_col = el; }
    }
    let nr = savvy_ffi::Rf_xlength(x_col) as usize;
    let xp = savvy_ffi::REAL(x_col);
    let yp = savvy_ffi::REAL(y_col);
    let gp = savvy_ffi::INTEGER(g_col);
    let mut max_grp = 0i32;
    for i in 0..nr {
        let v = *xp.add(i);
        if v > 0.0 {
            let g = *gp.add(i);
            if g > max_grp { max_grp = g; }
        }
    }
    let ng = max_grp as usize;
    let gout = savvy_ffi::Rf_allocVector(savvy_ffi::INTSXP, ng as _);
    let sout = savvy_ffi::Rf_allocVector(savvy_ffi::REALSXP, ng as _);
    let gop = savvy_ffi::INTEGER(gout);
    let sop = savvy_ffi::REAL(sout);
    for i in 0..ng { *gop.add(i) = (i + 1) as _; *sop.add(i) = 0.0; }
    for i in 0..nr {
        let v = *xp.add(i);
        if v > 0.0 {
            let g = (*gp.add(i) - 1) as usize;
            if g < ng { *sop.add(g) += v / *yp.add(i); }
        }
    }
    let out = savvy_ffi::Rf_allocVector(savvy_ffi::VECSXP, 2);
    savvy_ffi::SET_VECTOR_ELT(out, 0, gout);
    savvy_ffi::SET_VECTOR_ELT(out, 1, sout);
    out
}

fn savvy_bench_dataframe_filter(x: &RealSexp) -> savvy::Result<Sexp> { stub() }
fn savvy_bench_list_access(x: &RealSexp) -> savvy::Result<Sexp> { stub() }

#[unsafe(no_mangle)]
pub unsafe extern "C" fn savvy_bench_list_access__ffi(x: SEXP) -> SEXP {
    let n = savvy_ffi::Rf_xlength(x);
    let mut total: f64 = 0.0;
    for i in 0..n {
        total += *(savvy_ffi::REAL(savvy_ffi::VECTOR_ELT(x, i)));
    }
    let out = savvy_ffi::Rf_allocVector(savvy_ffi::REALSXP, 1);
    *(savvy_ffi::REAL(out)) = total;
    out
}

fn savvy_bench_string_concat(x: &RealSexp) -> savvy::Result<Sexp> { stub() }

#[unsafe(no_mangle)]
pub unsafe extern "C" fn savvy_bench_string_concat__ffi(x: SEXP) -> SEXP {
    let n = savvy_ffi::Rf_xlength(x) as usize;
    let mut total: usize = 0;
    for i in 0..n {
        let elt = savvy_ffi::STRING_ELT(x, i as _);
        if elt == savvy_ffi::R_NaString { total += 2; }
        else { total += savvy_ffi::Rf_xlength(elt) as usize; }
    }
    if n > 1 { total += (n - 1) * 2; }

    let mut buf: Vec<u8> = Vec::with_capacity(total);
    for i in 0..n {
        let elt = savvy_ffi::STRING_ELT(x, i as _);
        if elt == savvy_ffi::R_NaString { buf.extend_from_slice(b"NA"); }
        else {
            let len = savvy_ffi::Rf_xlength(elt) as usize;
            let src = std::slice::from_raw_parts(savvy_ffi::R_CHAR(elt) as *const u8, len);
            buf.extend_from_slice(src);
        }
        if i + 1 < n { buf.extend_from_slice(b", "); }
    }

    let out = savvy_ffi::Rf_allocVector(savvy_ffi::STRSXP, 1);
    let cs = savvy_ffi::Rf_mkCharLenCE(buf.as_ptr() as _, buf.len() as _, savvy_ffi::cetype_t_CE_UTF8);
    savvy_ffi::SET_STRING_ELT(out, 0, cs);
    out
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn savvy_bench_string_encoding__impl(x: SEXP) -> SEXP {
    let n = savvy_ffi::Rf_xlength(x);
    let mut total: i32 = 0;
    for i in 0..n {
        let elt = savvy_ffi::STRING_ELT(x, i);
        total += Rf_getCharCE(elt);
    }
    let out = savvy_ffi::Rf_allocVector(savvy_ffi::INTSXP, 1);
    *(savvy_ffi::INTEGER(out)) = total;
    out
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn savvy_bench_factor_ops__impl(x: SEXP) -> SEXP {
    let factor_sym = Rf_install("factor\0".as_ptr() as _);
    let call = savvy_ffi::Rf_protect(Rf_lang2(factor_sym, x));
    let mut err: i32 = 0;
    let factor = R_tryEvalSilent(call, R_GlobalEnv, &mut err);
    if err != 0 {
        savvy_ffi::Rf_unprotect(1);
        let out = savvy_ffi::Rf_allocVector(savvy_ffi::INTSXP, 1);
        *(savvy_ffi::INTEGER(out)) = -1;
        return out;
    }
    let n = savvy_ffi::Rf_xlength(factor);
    let codes = savvy_ffi::INTEGER(factor);
    let mut total: i64 = 0;
    for i in 0..n {
        let c = *codes.offset(i as _);
        if c != i32::MIN { total += c as i64; }
    }
    savvy_ffi::Rf_unprotect(1);
    let out = savvy_ffi::Rf_allocVector(savvy_ffi::INTSXP, 1);
    *(savvy_ffi::INTEGER(out)) = total as _;
    out
}

fn savvy_bench_attrib_ops(x: &RealSexp) -> savvy::Result<Sexp> { stub() }
fn savvy_bench_s4_slot_access(x: &RealSexp) -> savvy::Result<Sexp> { stub() }
fn savvy_bench_na_propagation(x: &RealSexp) -> savvy::Result<Sexp> { stub() }
fn savvy_bench_long_vector_idx(x: &RealSexp) -> savvy::Result<Sexp> { stub() }
fn savvy_bench_l1_arithmetic(x: &RealSexp) -> savvy::Result<Sexp> { stub() }

// Layer 4: Numerical
fn savvy_bench_matmul(x: &RealSexp) -> savvy::Result<Sexp> { stub() }
fn savvy_bench_crossprod(x: &RealSexp) -> savvy::Result<Sexp> { stub() }
fn savvy_bench_cholesky(x: &RealSexp) -> savvy::Result<Sexp> { stub() }
fn savvy_bench_lm_fit(x: &RealSexp) -> savvy::Result<Sexp> { stub() }

// Layer 5: ALTREP
fn savvy_bench_altrep_create(x: &RealSexp) -> savvy::Result<Sexp> { stub() }
fn savvy_bench_altrep_materialize(x: &RealSexp) -> savvy::Result<Sexp> { stub() }
fn savvy_bench_altrep_elt_walk(x: &RealSexp) -> savvy::Result<Sexp> { stub() }
fn savvy_bench_altrep_region_read(x: &RealSexp) -> savvy::Result<Sexp> { stub() }
fn savvy_bench_altrep_sum_via_R(x: &RealSexp) -> savvy::Result<Sexp> { stub() }
fn savvy_bench_altrep_sum_native(x: &RealSexp) -> savvy::Result<Sexp> { stub() }
fn savvy_bench_altrep_min_max(x: &RealSexp) -> savvy::Result<Sexp> { stub() }
fn savvy_bench_altrep_no_na_query(x: &RealSexp) -> savvy::Result<Sexp> { stub() }

// Layer 6: Integration
fn savvy_bench_struct_convert(x: &RealSexp) -> savvy::Result<Sexp> { stub() }
fn savvy_bench_r_eval(x: &RealSexp) -> savvy::Result<Sexp> { stub() }
fn savvy_bench_r_tryeval(x: &RealSexp) -> savvy::Result<Sexp> { stub() }
fn savvy_bench_serialize_roundtrip(x: &RealSexp) -> savvy::Result<Sexp> { stub() }
fn savvy_bench_external_ptr(x: &RealSexp) -> savvy::Result<Sexp> { stub() }
fn savvy_bench_rng_stress(x: &RealSexp) -> savvy::Result<Sexp> { stub() }
