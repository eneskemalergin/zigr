use extendr_api::prelude::*;

fn stub() -> Robj { ().into() }

fn fib_rs(n: i64) -> i64 {
    if n <= 1 { return n; }
    fib_rs(n - 1) + fib_rs(n - 2)
}

// Layer 1: Primitives
#[extendr]
fn extendr_bench_vectorsum(x: Robj) -> Robj {
    let mut total: f64 = 0.0;
    if let Some(slice) = x.as_real_slice() {
        for &v in slice {
            total += v;
        }
    }
    r!(total)
}
#[extendr]
fn extendr_bench_elem_ops(x: Robj) -> Robj {
    let len = x.len() as usize;
    let slice = x.as_real_slice().unwrap_or(&[]);
    let mat = RMatrix::new_matrix(len, 4, |i, j| {
        let v = slice[i];
        match j {
            0 => v.abs(),
            1 => if v > 0.0 { v.ln() } else { 0.0 },
            2 => v.exp(),
            _ => if v >= 0.0 { v.sqrt() } else { 0.0 },
        }
    });
    mat.into()
}
#[extendr]
fn extendr_bench_memcpy_bandwidth(x: Robj) -> Robj {
    let n = x.len() as usize;
    let slice = x.as_real_slice().unwrap_or(&[]);

    let mut totals = [0.0f64; 3];
    for _ in 0..2 {
        let mut temp: Vec<f64> = vec![0.0; n];
        temp.copy_from_slice(slice);
        totals[0] += temp.iter().sum::<f64>();

        let n_isize = n as isize;
        let mut copy_out = unsafe { Robj::from_sexp(extendr_ffi::Rf_allocVector(extendr_ffi::SEXPTYPE::REALSXP, n_isize)) };
        if let Some(cs) = copy_out.as_real_slice_mut() {
            cs.copy_from_slice(slice);
            totals[1] += cs.iter().sum::<f64>();
        }

        let mut fill_out = unsafe { Robj::from_sexp(extendr_ffi::Rf_allocVector(extendr_ffi::SEXPTYPE::REALSXP, n_isize)) };
        if let Some(fs) = fill_out.as_real_slice_mut() {
            for (j, &val) in slice.iter().enumerate() {
                fs[j] = val + 0.5;
            }
            totals[2] += fs.iter().sum::<f64>();
        }
    }

    let out = Robj::from(totals.to_vec());
    let nms = ["copy_temp", "copy_out", "fill_out"];
    let names_sexp = Robj::from(nms.to_vec());
    let _ = unsafe { extendr_ffi::Rf_namesgets(out.get(), names_sexp.get()) };
    out
}
#[extendr]
fn extendr_bench_sort(x: Robj) -> Robj { stub() }
#[extendr]
fn extendr_bench_fib_recursive(n: Robj) -> Robj {
    let n_val: i64 = n.as_integer().unwrap_or(0) as i64;
    let result = fib_rs(n_val);
    r!(result as f64)
}
#[extendr]
fn extendr_bench_broadcast(x: Robj, y: Robj) -> Robj { stub() }

// Layer 2: R API overhead
#[extendr]
fn extendr_bench_protect_shallow(x: Robj) -> Robj { stub() }
#[extendr]
fn extendr_bench_protect_scaling(x: Robj) -> Robj { stub() }
#[extendr]
fn extendr_bench_type_dispatch(x: Robj) -> Robj { stub() }
#[extendr]
fn extendr_bench_longjmp_safety(x: Robj) -> Robj { stub() }
#[extendr]
fn extendr_bench_sexp_create(x: Robj) -> Robj { stub() }
#[extendr]
fn extendr_bench_sexp_inspect(x: Robj) -> Robj { stub() }

// Layer 3: Data structures
#[extendr]
fn extendr_bench_matrix_transpose(x: Robj) -> Robj { stub() }
#[extendr]
fn extendr_bench_matrix_rowsums(x: Robj) -> Robj { stub() }
#[extendr]
fn extendr_bench_matrix_rowcol_means(x: Robj) -> Robj { stub() }
#[extendr]
fn extendr_bench_dataframe_filter(x: Robj) -> Robj { stub() }
#[extendr]
fn extendr_bench_list_access(x: Robj) -> Robj { stub() }
#[extendr]
fn extendr_bench_string_concat(x: Robj) -> Robj { stub() }
#[extendr]
fn extendr_bench_string_nchar(x: Robj) -> Robj { stub() }
#[extendr]
fn extendr_bench_string_encoding(x: Robj) -> Robj { stub() }
#[extendr]
fn extendr_bench_factor_ops(x: Robj) -> Robj { stub() }
#[extendr]
fn extendr_bench_attrib_ops(x: Robj) -> Robj { stub() }
#[extendr]
fn extendr_bench_s4_slot_access(x: Robj) -> Robj { stub() }
#[extendr]
fn extendr_bench_na_propagation(x: Robj) -> Robj { stub() }
#[extendr]
fn extendr_bench_long_vector_idx(x: Robj) -> Robj { stub() }
#[extendr]
fn extendr_bench_l1_arithmetic(x: Robj) -> Robj { stub() }

// Layer 4: Numerical
#[extendr]
fn extendr_bench_matmul(x: Robj) -> Robj { stub() }
#[extendr]
fn extendr_bench_crossprod(x: Robj) -> Robj { stub() }
#[extendr]
fn extendr_bench_cholesky(x: Robj) -> Robj { stub() }
#[extendr]
fn extendr_bench_lm_fit(x: Robj) -> Robj { stub() }

// Layer 5: ALTREP
#[extendr]
fn extendr_bench_altrep_create(x: Robj) -> Robj { stub() }
#[extendr]
fn extendr_bench_altrep_materialize(x: Robj) -> Robj { stub() }
#[extendr]
fn extendr_bench_altrep_elt_walk(x: Robj) -> Robj { stub() }
#[extendr]
fn extendr_bench_altrep_region_read(x: Robj) -> Robj { stub() }
#[extendr]
fn extendr_bench_altrep_sum_via_R(x: Robj) -> Robj { stub() }
#[extendr]
fn extendr_bench_altrep_sum_native(x: Robj) -> Robj { stub() }
#[extendr]
fn extendr_bench_altrep_min_max(x: Robj) -> Robj { stub() }
#[extendr]
fn extendr_bench_altrep_no_na_query(x: Robj) -> Robj { stub() }

// Layer 6: Integration
#[extendr]
fn extendr_bench_struct_convert(x: Robj) -> Robj { stub() }
#[extendr]
fn extendr_bench_r_eval(x: Robj) -> Robj { stub() }
#[extendr]
fn extendr_bench_r_tryeval(x: Robj) -> Robj { stub() }
#[extendr]
fn extendr_bench_serialize_roundtrip(x: Robj) -> Robj { stub() }
#[extendr]
fn extendr_bench_external_ptr(x: Robj) -> Robj { stub() }
#[extendr]
fn extendr_bench_rng_stress(x: Robj) -> Robj { stub() }

extendr_module! {
    mod extendr_benchmarks;
    fn extendr_bench_vectorsum;
    fn extendr_bench_elem_ops;
    fn extendr_bench_memcpy_bandwidth;
    fn extendr_bench_sort;
    fn extendr_bench_fib_recursive;
    fn extendr_bench_broadcast;
    fn extendr_bench_protect_shallow;
    fn extendr_bench_protect_scaling;
    fn extendr_bench_type_dispatch;
    fn extendr_bench_longjmp_safety;
    fn extendr_bench_sexp_create;
    fn extendr_bench_sexp_inspect;
    fn extendr_bench_matrix_transpose;
    fn extendr_bench_matrix_rowsums;
    fn extendr_bench_matrix_rowcol_means;
    fn extendr_bench_dataframe_filter;
    fn extendr_bench_list_access;
    fn extendr_bench_string_concat;
    fn extendr_bench_string_nchar;
    fn extendr_bench_string_encoding;
    fn extendr_bench_factor_ops;
    fn extendr_bench_attrib_ops;
    fn extendr_bench_s4_slot_access;
    fn extendr_bench_na_propagation;
    fn extendr_bench_long_vector_idx;
    fn extendr_bench_l1_arithmetic;
    fn extendr_bench_matmul;
    fn extendr_bench_crossprod;
    fn extendr_bench_cholesky;
    fn extendr_bench_lm_fit;
    fn extendr_bench_altrep_create;
    fn extendr_bench_altrep_materialize;
    fn extendr_bench_altrep_elt_walk;
    fn extendr_bench_altrep_region_read;
    fn extendr_bench_altrep_sum_via_R;
    fn extendr_bench_altrep_sum_native;
    fn extendr_bench_altrep_min_max;
    fn extendr_bench_altrep_no_na_query;
    fn extendr_bench_struct_convert;
    fn extendr_bench_r_eval;
    fn extendr_bench_r_tryeval;
    fn extendr_bench_serialize_roundtrip;
    fn extendr_bench_external_ptr;
    fn extendr_bench_rng_stress;
}
