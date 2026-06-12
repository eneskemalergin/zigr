use extendr_api::prelude::*;

extern "C" {
    fn Rf_getCharCE(x: extendr_ffi::SEXP) -> i32;
    fn Rf_install(name: *const std::os::raw::c_char) -> extendr_ffi::SEXP;
    fn Rf_lang2(fun: extendr_ffi::SEXP, arg: extendr_ffi::SEXP) -> extendr_ffi::SEXP;
    fn Rf_mkString(str: *const std::os::raw::c_char) -> extendr_ffi::SEXP;
    fn Rf_ScalarReal(x: f64) -> extendr_ffi::SEXP;
    fn Rf_ScalarInteger(x: i32) -> extendr_ffi::SEXP;
    fn Rf_setAttrib(x: extendr_ffi::SEXP, name: extendr_ffi::SEXP, val: extendr_ffi::SEXP);
    fn Rf_mkChar(str: *const std::os::raw::c_char) -> extendr_ffi::SEXP;
    fn R_tryEvalSilent(expr: extendr_ffi::SEXP, env: extendr_ffi::SEXP, err: *mut i32) -> extendr_ffi::SEXP;
    fn R_MakeUnwindCont() -> extendr_ffi::SEXP;
    fn R_UnwindProtect(
        fun: unsafe extern "C" fn(*mut std::os::raw::c_void) -> extendr_ffi::SEXP,
        data: *mut std::os::raw::c_void,
        cont: unsafe extern "C" fn(*mut std::os::raw::c_void, i32),
        cldata: *mut std::os::raw::c_void,
        cont2: extendr_ffi::SEXP,
    ) -> extendr_ffi::SEXP;
    static R_GlobalEnv: extendr_ffi::SEXP;
}

fn stub() -> Robj { ().into() }

fn fib_rs(n: i64) -> i64 {
    if n <= 1 { return n; }
    fib_rs(n - 1) + fib_rs(n - 2)
}

fn radix_sort_f64(arr: &mut [f64]) {
    if arr.len() < 2 { return; }
    let buf = unsafe { std::slice::from_raw_parts_mut(arr.as_mut_ptr() as *mut u64, arr.len()) };
    const SIGN_BIT: u64 = 1 << 63;
    for v in buf.iter_mut() {
        *v = if *v & SIGN_BIT != 0 { !*v } else { *v ^ SIGN_BIT };
    }
    // LSD radix sort, 8 bits per pass, 8 passes for 64-bit
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
fn extendr_bench_sort(x: Robj) -> Robj {
    let n = x.len() as usize;
    let mut data = vec![0.0f64; n];
    if let Some(slice) = x.as_real_slice() {
        data.copy_from_slice(slice);
    }
    radix_sort_f64(&mut data);
    r!(data)
}
#[extendr]
fn extendr_bench_fib_recursive(n: Robj) -> Robj {
    let n_val: i64 = n.as_integer().unwrap_or(0) as i64;
    let result = fib_rs(n_val);
    r!(result as f64)
}
#[extendr]
fn extendr_bench_broadcast(x: Robj, y: Robj) -> Robj {
    let slice = x.as_real_slice().unwrap_or(&[]);
    let s = y.as_real().unwrap_or(0.0);
    let mut total: f64 = 0.0;
    for &v in slice { total += v + s; }
    r!(total)
}

// Layer 2: R API overhead
#[extendr]
fn extendr_bench_protect_shallow(x: Robj) -> Robj {
    let s = unsafe { std::mem::transmute::<Robj, extendr_ffi::SEXP>(x) };
    for _ in 0..100 {
        for _ in 0..10 { unsafe { extendr_ffi::Rf_protect(s); } }
        unsafe { extendr_ffi::Rf_unprotect(10); }
    }
    r!(0i32)
}
#[extendr]
fn extendr_bench_protect_scaling(x: Robj) -> Robj {
    let s = unsafe { std::mem::transmute::<Robj, extendr_ffi::SEXP>(x) };
    for _ in 0..100 {
        for _ in 0..10 {
            for _ in 0..10000 { unsafe { extendr_ffi::Rf_protect(s); } }
            unsafe { extendr_ffi::Rf_unprotect(10000); }
        }
    }
    r!(0i32)
}
#[extendr]
fn extendr_bench_type_dispatch(x: Robj) -> Robj {
    let list = x.as_list().unwrap();
    let mut it = list.iter();
    let sa = unsafe { std::mem::transmute::<Robj, extendr_ffi::SEXP>(it.next().unwrap().1.clone()) };
    let sb = unsafe { std::mem::transmute::<Robj, extendr_ffi::SEXP>(it.next().unwrap().1.clone()) };
    let sc = unsafe { std::mem::transmute::<Robj, extendr_ffi::SEXP>(it.next().unwrap().1.clone()) };
    let mut total: i32 = 0;
    for _ in 0..2048 {
        total += match unsafe { extendr_ffi::TYPEOF(sa) } as i32 { 14 => 1, 13 => 2, 16 => 3, _ => 0 };
        total += match unsafe { extendr_ffi::TYPEOF(sb) } as i32 { 14 => 1, 13 => 2, 16 => 3, _ => 0 };
        total += match unsafe { extendr_ffi::TYPEOF(sc) } as i32 { 14 => 1, 13 => 2, 16 => 3, _ => 0 };
    }
    r!(total)
}

const EXTENDR_REPEATS: isize = 512;

unsafe extern "C" fn extendr_unwind_noop(_data: *mut std::os::raw::c_void, _jump: i32) {}

unsafe extern "C" fn extendr_unwind_callback(data: *mut std::os::raw::c_void) -> extendr_ffi::SEXP {
    let ud = &*(data as *const (extendr_ffi::SEXP, f64));
    let xp = extendr_ffi::REAL((*ud).0);
    let n = extendr_ffi::Rf_xlength((*ud).0);
    let bias = (*ud).1;
    let mut total = 0.0f64;
    for i in 0..n {
        total += *xp.offset(i) + bias;
    }
    Rf_ScalarReal(total)
}

#[extendr]
fn extendr_bench_longjmp_safety(x: Robj) -> Robj {
    let s = unsafe { std::mem::transmute::<Robj, extendr_ffi::SEXP>(x) };
    let n = unsafe { extendr_ffi::Rf_xlength(s) };
    let xp = unsafe { extendr_ffi::REAL(s) };

    let mut direct_total = 0.0f64;
    let mut try_ok_total = 0.0f64;
    let mut try_err_total = 0.0f64;
    let mut unwind_ok_total = 0.0f64;

    let sum_sym = unsafe { Rf_install("sum\0".as_ptr() as _) };
    let stop_sym = unsafe { Rf_install("stop\0".as_ptr() as _) };

    for rep in 0..EXTENDR_REPEATS {
        let bias = (rep as f64 + 1.0) * 0.001;

        for i in 0..n {
            direct_total += unsafe { *xp.offset(i) + bias };
        }

        let tmp = unsafe { extendr_ffi::Rf_protect(extendr_ffi::Rf_allocVector(extendr_ffi::SEXPTYPE::REALSXP, n)) };
        let tmpd = unsafe { extendr_ffi::REAL(tmp) };
        for i in 0..n {
            unsafe { *tmpd.offset(i) = *xp.offset(i) + bias };
        }
        let expr = unsafe { extendr_ffi::Rf_protect(Rf_lang2(sum_sym, tmp)) };
        let mut err: i32 = 0;
        let res = unsafe { R_tryEvalSilent(expr, R_GlobalEnv, &mut err) };
        if err == 0 {
            try_ok_total += unsafe { *extendr_ffi::REAL(res) };
        }
        unsafe { extendr_ffi::Rf_unprotect(2) };

        err = 0;
        let stop_call = unsafe { extendr_ffi::Rf_protect(Rf_lang2(stop_sym, Rf_mkString("task32\0".as_ptr() as _))) };
        unsafe { R_tryEvalSilent(stop_call, R_GlobalEnv, &mut err) };
        unsafe { extendr_ffi::Rf_unprotect(1) };
        if err != 0 { try_err_total += 1.0; }

        let ud = (s, bias);
        let cont = unsafe { extendr_ffi::Rf_protect(R_MakeUnwindCont()) };
        let ures = unsafe { R_UnwindProtect(extendr_unwind_callback, &ud as *const _ as *mut _, extendr_unwind_noop, std::ptr::null_mut(), cont) };
        unwind_ok_total += unsafe { *extendr_ffi::REAL(ures) };
        unsafe { extendr_ffi::Rf_unprotect(1) };
    }

    let mut out = unsafe { Robj::from_sexp(extendr_ffi::Rf_allocVector(extendr_ffi::SEXPTYPE::REALSXP, 4)) };
    if let Some(slice) = out.as_real_slice_mut() {
        slice[0] = direct_total;
        slice[1] = try_ok_total;
        slice[2] = try_err_total;
        slice[3] = unwind_ok_total;
    }
    out.set_names(&["direct", "try_ok", "try_err", "unwind_ok"]).ok();
    out
}

#[extendr]
fn extendr_bench_sexp_create(x: Robj) -> Robj {
    let _ = x;
    for _ in 0..10 {
        for _ in 0..10000 {
            unsafe { extendr_ffi::Rf_protect(extendr_ffi::Rf_allocVector(extendr_ffi::SEXPTYPE::REALSXP, 1_isize)); }
        }
        unsafe { extendr_ffi::Rf_unprotect(10000); }
    }
    r!(0i32)
}

#[extendr]
fn extendr_bench_sexp_inspect(x: Robj) -> Robj {
    let list = x.as_list().unwrap();
    let mut it = list.iter();
    let mut elts: Vec<extendr_ffi::SEXP> = Vec::new();
    while let Some((_, e)) = it.next() {
        elts.push(unsafe { std::mem::transmute::<Robj, extendr_ffi::SEXP>(e.clone()) });
    }
    let mut total: i32 = 0;
    for _ in 0..10000 {
        for &s in &elts {
            unsafe {
                total += extendr_ffi::TYPEOF(s) as i32;
                total += extendr_ffi::Rf_isVector(s) as i32;
                total += extendr_ffi::Rf_isReal(s) as i32;
            }
        }
    }
    r!(total)
}

#[extendr]
fn extendr_bench_matrix_transpose(x: Robj) -> Robj {
    let s = unsafe { std::mem::transmute::<Robj, extendr_ffi::SEXP>(x) };
    let nr = unsafe { extendr_ffi::Rf_nrows(s) } as usize;
    let nc = unsafe { extendr_ffi::Rf_ncols(s) } as usize;
    let xp = unsafe { extendr_ffi::REAL(s) };

    let out = unsafe { extendr_ffi::Rf_allocMatrix(extendr_ffi::SEXPTYPE::REALSXP, nc as _, nr as _) };
    let rp = unsafe { extendr_ffi::REAL(out) };

    const BLOCK: usize = 32;
    let mut jj = 0;
    while jj < nc {
        let j_end = std::cmp::min(jj + BLOCK, nc);
        let mut ii = 0;
        while ii < nr {
            let i_end = std::cmp::min(ii + BLOCK, nr);
            for i in ii..i_end {
                let out_row = unsafe { rp.add(i * nc) };
                for j in jj..j_end {
                    unsafe { *out_row.add(j) = *xp.add(i + j * nr); }
                }
            }
            ii += BLOCK;
        }
        jj += BLOCK;
    }

    unsafe { Robj::from_sexp(out) }
}

#[extendr]
fn extendr_bench_matrix_rowsums(x: Robj) -> Robj {
    let s = unsafe { std::mem::transmute::<Robj, extendr_ffi::SEXP>(x) };
    let nr = unsafe { extendr_ffi::Rf_nrows(s) } as usize;
    let nc = unsafe { extendr_ffi::Rf_ncols(s) } as usize;
    let xp = unsafe { extendr_ffi::REAL(s) };

    let out = unsafe { extendr_ffi::Rf_allocVector(extendr_ffi::SEXPTYPE::REALSXP, nr as _) };
    let rp = unsafe { extendr_ffi::REAL(out) };
    for i in 0..nr { unsafe { *rp.add(i) = 0.0; } }

    for j in 0..nc {
        let col = unsafe { xp.add(j * nr) };
        for i in 0..nr {
            unsafe { *rp.add(i) += *col.add(i); }
        }
    }

    unsafe { Robj::from_sexp(out) }
}

#[extendr]
fn extendr_bench_matrix_rowcol_means(x: Robj) -> Robj {
    let s = unsafe { std::mem::transmute::<Robj, extendr_ffi::SEXP>(x) };
    let nr = unsafe { extendr_ffi::Rf_nrows(s) } as usize;
    let nc = unsafe { extendr_ffi::Rf_ncols(s) } as usize;
    let xp = unsafe { extendr_ffi::REAL(s) };
    let rm = unsafe { extendr_ffi::Rf_allocVector(extendr_ffi::SEXPTYPE::REALSXP, nr as _) };
    let cs = unsafe { extendr_ffi::Rf_allocVector(extendr_ffi::SEXPTYPE::REALSXP, nc as _) };
    let rp = unsafe { extendr_ffi::REAL(rm) };
    let cp = unsafe { extendr_ffi::REAL(cs) };
    for i in 0..nr { unsafe { *rp.add(i) = 0.0; } }
    for j in 0..nc {
        let col = unsafe { xp.add(j * nr) };
        let mut sum = 0.0;
        for i in 0..nr {
            let v = unsafe { *col.add(i) };
            unsafe { *rp.add(i) += v; }
            sum += v;
        }
        unsafe { *cp.add(j) = sum; }
    }
    let inv = 1.0 / nc as f64;
    for i in 0..nr { unsafe { *rp.add(i) *= inv; } }
    let out = unsafe { extendr_ffi::Rf_allocVector(extendr_ffi::SEXPTYPE::VECSXP, 2) };
    unsafe { extendr_ffi::SET_VECTOR_ELT(out, 0, rm); }
    unsafe { extendr_ffi::SET_VECTOR_ELT(out, 1, cs); }
    unsafe { Robj::from_sexp(out) }
}

#[extendr]
fn extendr_bench_dataframe_filter(x: Robj) -> Robj {
    let list = x.as_list().unwrap();
    let mut it = list.iter();
    let (_, x_robj) = it.next().unwrap();
    let (_, y_robj) = it.next().unwrap();
    let (_, grp_robj) = it.next().unwrap();
    let xs = unsafe { std::mem::transmute::<Robj, extendr_ffi::SEXP>(x_robj.clone()) };
    let ys = unsafe { std::mem::transmute::<Robj, extendr_ffi::SEXP>(y_robj.clone()) };
    let gs = unsafe { std::mem::transmute::<Robj, extendr_ffi::SEXP>(grp_robj.clone()) };
    let nr = unsafe { extendr_ffi::Rf_xlength(xs) } as usize;
    let xp = unsafe { extendr_ffi::REAL(xs) };
    let yp = unsafe { extendr_ffi::REAL(ys) };
    let gp = unsafe { extendr_ffi::INTEGER(gs) };

    let mut max_grp = 0i32;
    for i in 0..nr {
        let v = unsafe { *xp.add(i) };
        if v > 0.0 {
            let g = unsafe { *gp.add(i) };
            if g > max_grp { max_grp = g; }
        }
    }
    let ng = max_grp as usize;
    let gout = unsafe { extendr_ffi::Rf_allocVector(extendr_ffi::SEXPTYPE::INTSXP, ng as _) };
    let sout = unsafe { extendr_ffi::Rf_allocVector(extendr_ffi::SEXPTYPE::REALSXP, ng as _) };
    let gop = unsafe { extendr_ffi::INTEGER(gout) };
    let sop = unsafe { extendr_ffi::REAL(sout) };
    for i in 0..ng { unsafe { *gop.add(i) = (i + 1) as _; *sop.add(i) = 0.0; } }
    for i in 0..nr {
        let v = unsafe { *xp.add(i) };
        if v > 0.0 {
            let g = (unsafe { *gp.add(i) } - 1) as usize;
            if g < ng { unsafe { *sop.add(g) += v / *yp.add(i); } }
        }
    }
    let out = unsafe { extendr_ffi::Rf_allocVector(extendr_ffi::SEXPTYPE::VECSXP, 2) };
    unsafe { extendr_ffi::SET_VECTOR_ELT(out, 0, gout); }
    unsafe { extendr_ffi::SET_VECTOR_ELT(out, 1, sout); }
    unsafe { Robj::from_sexp(out) }
}

#[extendr]
fn extendr_bench_list_access(x: Robj) -> Robj {
    let list = x.as_list().unwrap();
    let mut total: f64 = 0.0;
    let mut it = list.iter();
    while let Some((_, e)) = it.next() {
        let s = unsafe { std::mem::transmute::<Robj, extendr_ffi::SEXP>(e.clone()) };
        total += unsafe { *extendr_ffi::REAL(s) };
    }
    r!(total)
}

#[extendr]
fn extendr_bench_string_concat(x: Robj) -> Robj {
    let s = unsafe { std::mem::transmute::<Robj, extendr_ffi::SEXP>(x) };
    let n = unsafe { extendr_ffi::Rf_xlength(s) } as usize;
    let mut total: usize = 0;
    for i in 0..n {
        let elt = unsafe { extendr_ffi::STRING_ELT(s, i as _) };
        if elt == unsafe { extendr_ffi::R_NaString } { total += 2; }
        else { total += unsafe { extendr_ffi::Rf_xlength(elt) } as usize; }
    }
    if n > 1 { total += (n - 1) * 2; }

    let mut buf: Vec<u8> = Vec::with_capacity(total);
    for i in 0..n {
        let elt = unsafe { extendr_ffi::STRING_ELT(s, i as _) };
        if elt == unsafe { extendr_ffi::R_NaString } {
            buf.extend_from_slice(b"NA");
        } else {
            let len = unsafe { extendr_ffi::Rf_xlength(elt) } as usize;
            let src = unsafe { std::slice::from_raw_parts(extendr_ffi::R_CHAR(elt) as *const u8, len) };
            buf.extend_from_slice(src);
        }
        if i + 1 < n { buf.extend_from_slice(b", "); }
    }

    let out = unsafe { extendr_ffi::Rf_allocVector(extendr_ffi::SEXPTYPE::STRSXP, 1) };
    let cs = unsafe { extendr_ffi::Rf_mkCharLenCE(buf.as_ptr() as _, buf.len() as _, extendr_ffi::cetype_t::CE_UTF8) };
    unsafe { extendr_ffi::SET_STRING_ELT(out, 0, cs); }
    unsafe { Robj::from_sexp(out) }
}

#[extendr]
fn extendr_bench_string_nchar(x: Robj) -> Robj {
    let s = unsafe { std::mem::transmute::<Robj, extendr_ffi::SEXP>(x) };
    let n = unsafe { extendr_ffi::Rf_xlength(s) } as usize;
    let mut total: i64 = 0;
    for i in 0..n {
        let elt = unsafe { extendr_ffi::STRING_ELT(s, i as _) };
        if elt == unsafe { extendr_ffi::R_NaString } { continue; }
        total += unsafe { extendr_ffi::Rf_xlength(elt) } as i64;
    }
    r!(total as i32)
}

#[extendr]
fn extendr_bench_string_encoding(x: Robj) -> Robj {
    let s = unsafe { std::mem::transmute::<Robj, extendr_ffi::SEXP>(x) };
    let n = unsafe { extendr_ffi::Rf_xlength(s) } as usize;
    let mut total: i32 = 0;
    for i in 0..n {
        let elt = unsafe { extendr_ffi::STRING_ELT(s, i as _) };
        total += unsafe { Rf_getCharCE(elt) };
    }
    r!(total)
}
#[extendr]
fn extendr_bench_factor_ops(x: Robj) -> Robj {
    let s = unsafe { std::mem::transmute::<Robj, extendr_ffi::SEXP>(x) };
    let factor_sym = unsafe { Rf_install("factor\0".as_ptr() as _) };
    let call = unsafe { extendr_ffi::Rf_protect(Rf_lang2(factor_sym, s)) };
    let mut err: i32 = 0;
    let factor = unsafe { R_tryEvalSilent(call, R_GlobalEnv, &mut err) };
    if err != 0 {
        unsafe { extendr_ffi::Rf_unprotect(1) };
        return r!(i32::MIN);
    }
    let n = unsafe { extendr_ffi::Rf_xlength(factor) };
    let codes = unsafe { extendr_ffi::INTEGER(factor) };
    let mut total: i64 = 0;
    for i in 0..n {
        let c = unsafe { *codes.offset(i) };
        if c != std::os::raw::c_int::MIN { total += c as i64; }
    }
    unsafe { extendr_ffi::Rf_unprotect(1) };
    r!(total as i32)
}
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
