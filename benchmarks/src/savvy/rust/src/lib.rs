use savvy::*;
use savvy_ffi::SEXP;

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
ffi_stub!(savvy_bench_broadcast__ffi, x: SEXP, y: SEXP);
ffi_stub!(savvy_bench_sort__ffi, x: SEXP);
ffi_stub!(savvy_bench_cumsum__ffi, x: SEXP);
ffi_stub!(savvy_bench_rnorm__ffi, x: SEXP);
ffi_stub!(savvy_bench_string_nchar__ffi, x: SEXP);
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
ffi_stub!(savvy_bench_longjmp_safety__ffi, x: SEXP);
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
fn savvy_bench_protect_shallow(x: &RealSexp) -> savvy::Result<Sexp> { stub() }
fn savvy_bench_protect_scaling(x: &RealSexp) -> savvy::Result<Sexp> { stub() }
fn savvy_bench_type_dispatch(x: &RealSexp) -> savvy::Result<Sexp> { stub() }
fn savvy_bench_longjmp_safety(x: &RealSexp) -> savvy::Result<Sexp> { stub() }
fn savvy_bench_sexp_create(x: &RealSexp) -> savvy::Result<Sexp> { stub() }
fn savvy_bench_sexp_inspect(x: &RealSexp) -> savvy::Result<Sexp> { stub() }

// Layer 3: Data structures
fn savvy_bench_matrix_transpose(x: &RealSexp) -> savvy::Result<Sexp> { stub() }
fn savvy_bench_matrix_rowsums(x: &RealSexp) -> savvy::Result<Sexp> { stub() }
fn savvy_bench_matrix_rowcol_means(x: &RealSexp) -> savvy::Result<Sexp> { stub() }
fn savvy_bench_dataframe_filter(x: &RealSexp) -> savvy::Result<Sexp> { stub() }
fn savvy_bench_list_access(x: &RealSexp) -> savvy::Result<Sexp> { stub() }
fn savvy_bench_string_concat(x: &RealSexp) -> savvy::Result<Sexp> { stub() }
fn savvy_bench_string_nchar(x: &RealSexp) -> savvy::Result<Sexp> { stub() }
fn savvy_bench_string_encoding(x: &RealSexp) -> savvy::Result<Sexp> { stub() }
fn savvy_bench_factor_ops(x: &RealSexp) -> savvy::Result<Sexp> { stub() }
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
