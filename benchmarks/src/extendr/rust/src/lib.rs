use extendr_api::prelude::*;

extern "C" {
    fn Rf_getCharCE(x: extendr_ffi::SEXP) -> i32;
    fn Rf_install(name: *const std::os::raw::c_char) -> extendr_ffi::SEXP;
    fn Rf_lang2(fun: extendr_ffi::SEXP, arg: extendr_ffi::SEXP) -> extendr_ffi::SEXP;
    fn Rf_lang3(fun: extendr_ffi::SEXP, arg1: extendr_ffi::SEXP, arg2: extendr_ffi::SEXP) -> extendr_ffi::SEXP;
    fn Rf_mkString(str: *const std::os::raw::c_char) -> extendr_ffi::SEXP;
    fn Rf_ScalarReal(x: f64) -> extendr_ffi::SEXP;
    fn Rf_ScalarInteger(x: i32) -> extendr_ffi::SEXP;
    fn Rf_ScalarLogical(x: i32) -> extendr_ffi::SEXP;
    fn Rf_setAttrib(x: extendr_ffi::SEXP, name: extendr_ffi::SEXP, val: extendr_ffi::SEXP);
    fn Rf_getAttrib(x: extendr_ffi::SEXP, name: extendr_ffi::SEXP) -> extendr_ffi::SEXP;
    fn Rf_classgets(x: extendr_ffi::SEXP, val: extendr_ffi::SEXP) -> extendr_ffi::SEXP;
    fn Rf_duplicate(x: extendr_ffi::SEXP) -> extendr_ffi::SEXP;
    fn Rf_asInteger(x: extendr_ffi::SEXP) -> i32;
    fn Rf_asLogical(x: extendr_ffi::SEXP) -> i32;
    fn Rf_asReal(x: extendr_ffi::SEXP) -> f64;
    fn R_MakeExternalPtr(data: *mut std::os::raw::c_void, tag: extendr_ffi::SEXP, prot: extendr_ffi::SEXP) -> extendr_ffi::SEXP;
    fn GetRNGstate();
    fn PutRNGstate();
    fn norm_rand() -> f64;
    fn VECTOR_ELT(x: extendr_ffi::SEXP, i: isize) -> extendr_ffi::SEXP;
    fn LENGTH(x: extendr_ffi::SEXP) -> i32;
    fn Rf_mkChar(str: *const std::os::raw::c_char) -> extendr_ffi::SEXP;
    fn Rf_allocVector(kind: u32, len: isize) -> extendr_ffi::SEXP;
    fn R_tryEvalSilent(expr: extendr_ffi::SEXP, env: extendr_ffi::SEXP, err: *mut i32) -> extendr_ffi::SEXP;
    fn R_MakeUnwindCont() -> extendr_ffi::SEXP;
    fn R_UnwindProtect(
        fun: unsafe extern "C" fn(*mut std::os::raw::c_void) -> extendr_ffi::SEXP,
        data: *mut std::os::raw::c_void,
        cont: unsafe extern "C" fn(*mut std::os::raw::c_void, i32),
        cldata: *mut std::os::raw::c_void,
        cont2: extendr_ffi::SEXP,
    ) -> extendr_ffi::SEXP;
    fn R_do_slot(obj: extendr_ffi::SEXP, name: extendr_ffi::SEXP) -> extendr_ffi::SEXP;
    fn R_do_slot_assign(obj: extendr_ffi::SEXP, name: extendr_ffi::SEXP, val: extendr_ffi::SEXP);
    fn INTEGER_ELT(x: extendr_ffi::SEXP, i: isize) -> i32;
    fn INTEGER_GET_REGION(x: extendr_ffi::SEXP, i: isize, n: isize, buf: *mut i32) -> isize;
    fn Rf_nrows(x: extendr_ffi::SEXP) -> i32;
    fn Rf_ncols(x: extendr_ffi::SEXP) -> i32;
    fn SET_STRING_ELT(x: extendr_ffi::SEXP, i: isize, v: extendr_ffi::SEXP);
    fn REAL(x: extendr_ffi::SEXP) -> *mut f64;
    static R_GlobalEnv: extendr_ffi::SEXP;
    static R_ClassSymbol: extendr_ffi::SEXP;
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
fn extendr_bench_attrib_ops(x: Robj) -> Robj {
    let s = unsafe { std::mem::transmute::<Robj, extendr_ffi::SEXP>(x) };

    let cls_val = unsafe { extendr_ffi::Rf_protect(extendr_ffi::Rf_allocVector(extendr_ffi::SEXPTYPE::STRSXP, 1)) };
    unsafe { extendr_ffi::SET_STRING_ELT(cls_val, 0, Rf_mkChar("bench_class\0".as_ptr() as _)) };
    unsafe { Rf_classgets(s, cls_val) };
    unsafe { extendr_ffi::Rf_unprotect(1) };

    let cr_sym = unsafe { Rf_install("creator\0".as_ptr() as _) };
    let cr_val = unsafe { extendr_ffi::Rf_protect(extendr_ffi::Rf_allocVector(extendr_ffi::SEXPTYPE::STRSXP, 1)) };
    unsafe { extendr_ffi::SET_STRING_ELT(cr_val, 0, Rf_mkChar("zigr_bench\0".as_ptr() as _)) };
    unsafe { Rf_setAttrib(s, cr_sym, cr_val) };
    unsafe { extendr_ffi::Rf_unprotect(1) };

    let got_cls = unsafe { Rf_getAttrib(s, R_ClassSymbol) };
    let got_cr = unsafe { Rf_getAttrib(s, cr_sym) };

    let mut total: i32 = 0;
    let nc = unsafe { extendr_ffi::Rf_xlength(got_cls) };
    for i in 0..nc {
        let elt = unsafe { extendr_ffi::STRING_ELT(got_cls, i) };
        total += unsafe { extendr_ffi::Rf_xlength(elt) as i32 };
    }
    let ncr = unsafe { extendr_ffi::Rf_xlength(got_cr) };
    for i in 0..ncr {
        let elt = unsafe { extendr_ffi::STRING_ELT(got_cr, i) };
        total += unsafe { extendr_ffi::Rf_xlength(elt) as i32 };
    }
    r!(total)
}
#[extendr]
fn extendr_bench_s4_slot_access(x: Robj) -> Robj {
    let s = unsafe { std::mem::transmute::<Robj, extendr_ffi::SEXP>(x) };

    let class_expr = unsafe { extendr_ffi::Rf_protect(extendr_ffi::Rf_allocVector(extendr_ffi::SEXPTYPE::STRSXP, 1)) };
    unsafe { SET_STRING_ELT(class_expr, 0, Rf_mkChar("setClass(\"BenchS4\", representation(slot_x = \"numeric\"))\0".as_ptr() as _)) };
    let parse_call = unsafe { extendr_ffi::Rf_protect(Rf_lang2(Rf_install("parse\0".as_ptr() as _), class_expr)) };
    let mut err: i32 = 0;
    let parsed = unsafe { R_tryEvalSilent(parse_call, R_GlobalEnv, &mut err) };
    if err == 0 { unsafe { R_tryEvalSilent(parsed, R_GlobalEnv, &mut err) }; }
    unsafe { extendr_ffi::Rf_unprotect(2) };

    let new_call = unsafe { extendr_ffi::Rf_protect(Rf_lang2(Rf_install("new\0".as_ptr() as _), Rf_mkString("BenchS4\0".as_ptr() as _))) };
    err = 0;
    let obj = unsafe { extendr_ffi::Rf_protect(R_tryEvalSilent(new_call, R_GlobalEnv, &mut err)) };
    unsafe { extendr_ffi::Rf_unprotect(1) };
    if err != 0 { unsafe { extendr_ffi::Rf_unprotect(1) }; return unsafe { std::mem::transmute::<extendr_ffi::SEXP, Robj>(s) }; }

    let slot_sym = unsafe { Rf_install("slot_x\0".as_ptr() as _) };
    unsafe { R_do_slot_assign(obj, slot_sym, s) };
    let result = unsafe { R_do_slot(obj, slot_sym) };
    unsafe { extendr_ffi::Rf_unprotect(1) };
    unsafe { std::mem::transmute::<extendr_ffi::SEXP, Robj>(result) }
}
#[extendr]
fn extendr_bench_na_propagation(x: Robj) -> Robj {
    let mut total = 0.0f64;
    let mut count = 0i64;
    if let Some(slice) = x.as_real_slice() {
        for &v in slice {
            if !v.is_nan() { total += v; count += 1; }
        }
    }
    r!(if count > 0 { total / count as f64 } else { f64::NAN })
}
#[extendr]
fn extendr_bench_long_vector_idx(x: Robj) -> Robj {
    let s = unsafe { std::mem::transmute::<Robj, extendr_ffi::SEXP>(x) };
    let n = unsafe { extendr_ffi::Rf_xlength(s) };
    let mut total: i64 = 0;
    let mut i: isize = 0;
    while i < n {
        total += unsafe { INTEGER_ELT(s, i) as i64 };
        i += 10000;
    }
    r!(total as f64)
}
#[extendr]
fn extendr_bench_l1_arithmetic(x: Robj) -> Robj {
    let mut total = 0.0f64;
    if let Some(slice) = x.as_real_slice() {
        for _ in 0..2500 {
            for &v in slice {
                total += v * 0.5 + 0.5;
            }
        }
    }
    r!(total)
}

// Layer 4: Numerical
extern "C" {
    fn dgemm_(transa: *mut u8, transb: *mut u8, m: *mut i32, n: *mut i32, k: *mut i32,
              alpha: *mut f64, a: *mut f64, lda: *mut i32, b: *mut f64, ldb: *mut i32,
              beta: *mut f64, c: *mut f64, ldc: *mut i32);
    fn dsyrk_(uplo: *mut u8, trans: *mut u8, n: *mut i32, k: *mut i32,
              alpha: *mut f64, a: *mut f64, lda: *mut i32,
              beta: *mut f64, c: *mut f64, ldc: *mut i32);
    fn dpotrf_(uplo: *mut u8, n: *mut i32, a: *mut f64, lda: *mut i32, info: *mut i32);
    fn dtrsm_(side: *mut u8, uplo: *mut u8, transa: *mut u8, diag: *mut u8,
              m: *mut i32, n: *mut i32, alpha: *mut f64, a: *mut f64, lda: *mut i32,
              b: *mut f64, ldb: *mut i32);
}

#[extendr]
fn extendr_bench_matmul(a: Robj, b: Robj) -> Robj {
    let sa = unsafe { std::mem::transmute::<Robj, extendr_ffi::SEXP>(a) };
    let sb = unsafe { std::mem::transmute::<Robj, extendr_ffi::SEXP>(b) };
    let mut n = unsafe { extendr_ffi::Rf_nrows(sa) };
    let mut m = unsafe { extendr_ffi::Rf_ncols(sb) };
    let mut k = unsafe { extendr_ffi::Rf_ncols(sa) };

    let result = unsafe { extendr_ffi::Rf_protect(extendr_ffi::Rf_allocVector(extendr_ffi::SEXPTYPE::REALSXP, (n as isize) * (m as isize))) };
    let dims = unsafe { extendr_ffi::Rf_protect(extendr_ffi::Rf_allocVector(extendr_ffi::SEXPTYPE::INTSXP, 2)) };
    unsafe {
        *extendr_ffi::INTEGER(dims).offset(0) = n;
        *extendr_ffi::INTEGER(dims).offset(1) = m;
        extendr_ffi::Rf_setAttrib(result, Rf_install("dim\0".as_ptr() as _), dims);
    }
    unsafe { extendr_ffi::Rf_unprotect(1) };

    let mut notrans: u8 = b'N';
    let mut alpha: f64 = 1.0;
    let mut beta: f64 = 0.0;
    unsafe {
        dgemm_(&mut notrans, &mut notrans, &mut n, &mut m, &mut k,
               &mut alpha, extendr_ffi::REAL(sa), &mut n,
               extendr_ffi::REAL(sb), &mut k,
               &mut beta, extendr_ffi::REAL(result), &mut n);
    }

    unsafe { extendr_ffi::Rf_unprotect(1) };
    unsafe { std::mem::transmute::<extendr_ffi::SEXP, Robj>(result) }
}
#[extendr]
fn extendr_bench_crossprod(x: Robj) -> Robj {
    let s = unsafe { std::mem::transmute::<Robj, extendr_ffi::SEXP>(x) };
    let mut n = unsafe { extendr_ffi::Rf_ncols(s) };
    let mut k = unsafe { extendr_ffi::Rf_nrows(s) };

    let mut out = unsafe { Robj::from_sexp(extendr_ffi::Rf_allocMatrix(extendr_ffi::SEXPTYPE::REALSXP, n, n)) };
    let rp = if let Some(slice) = out.as_real_slice_mut() { slice.as_mut_ptr() } else { return r!(0.0) };

    let mut alpha: f64 = 1.0;
    let mut beta: f64 = 0.0;
    let mut uplo: u8 = b'U';
    let mut trans: u8 = b'T';
    unsafe {
        dsyrk_(&mut uplo, &mut trans, &mut n, &mut k,
               &mut alpha, extendr_ffi::REAL(s), &mut k,
               &mut beta, rp, &mut n);
    }
    if let Some(slice) = out.as_real_slice_mut() {
        let nn = n as usize;
        for i in 0..nn {
            for j in 0..i {
                slice[j * nn + i] = slice[i * nn + j];
            }
        }
    }
    out
}
#[extendr]
fn extendr_bench_cholesky(x: Robj) -> Robj {
    let s = unsafe { std::mem::transmute::<Robj, extendr_ffi::SEXP>(x) };
    let mut n = unsafe { extendr_ffi::Rf_nrows(s) };
    let n_usize = n as usize;
    let n_isize = n as isize;

    let mut out = unsafe { Robj::from_sexp(extendr_ffi::Rf_allocMatrix(extendr_ffi::SEXPTYPE::REALSXP, n, n)) };
    let slice = if let Some(s) = out.as_real_slice_mut() { s } else { return r!(0.0) };

    let src = unsafe { extendr_ffi::REAL(s) };
    for i in 0..n_usize * n_usize {
        slice[i] = unsafe { *src.offset(i as isize) };
    }

    let mut uplo: u8 = b'U';
    let mut lwork = n;
    let mut info: i32 = 0;
    unsafe {
        dpotrf_(&mut uplo, &mut n, slice.as_mut_ptr(), &mut lwork, &mut info);
    }

    for col in 0..n_usize {
        for row in (col + 1)..n_usize {
            slice[col * n_usize + row] = 0.0;
        }
    }
    out
}
#[extendr]
fn extendr_bench_lm_fit(x: Robj, y: Robj) -> Robj {
    let sx = unsafe { std::mem::transmute::<Robj, extendr_ffi::SEXP>(x) };
    let sy = unsafe { std::mem::transmute::<Robj, extendr_ffi::SEXP>(y) };
    let mut n = unsafe { extendr_ffi::Rf_nrows(sx) };
    let mut p = unsafe { extendr_ffi::Rf_ncols(sx) };

    let xtx = unsafe { extendr_ffi::Rf_protect(extendr_ffi::Rf_allocMatrix(extendr_ffi::SEXPTYPE::REALSXP, p, p)) };
    let xty = unsafe { extendr_ffi::Rf_protect(extendr_ffi::Rf_allocVector(extendr_ffi::SEXPTYPE::REALSXP, p as isize)) };
    let xtx_rp = unsafe { extendr_ffi::REAL(xtx) };
    let xty_rp = unsafe { extendr_ffi::REAL(xty) };

    let mut alpha: f64 = 1.0;
    let mut beta: f64 = 0.0;
    let mut notrans: u8 = b'N';
    let mut trans: u8 = b'T';
    let mut one: i32 = 1;

    unsafe {
        dgemm_(&mut trans, &mut notrans, &mut p, &mut p, &mut n, &mut alpha,
               extendr_ffi::REAL(sx), &mut n, extendr_ffi::REAL(sx), &mut n, &mut beta, xtx_rp, &mut p);
        dgemm_(&mut trans, &mut notrans, &mut p, &mut one, &mut n, &mut alpha,
               extendr_ffi::REAL(sx), &mut n, extendr_ffi::REAL(sy), &mut n, &mut beta, xty_rp, &mut p);
    }

    let mut info: i32 = 0;
    let mut uplo: u8 = b'U';
    unsafe { dpotrf_(&mut uplo, &mut p, xtx_rp, &mut p, &mut info); }

    let mut side: u8 = b'L';
    let mut diag: u8 = b'N';
    unsafe {
        dtrsm_(&mut side, &mut uplo, &mut trans, &mut diag, &mut p, &mut one, &mut alpha, xtx_rp, &mut p, xty_rp, &mut p);
        dtrsm_(&mut side, &mut uplo, &mut notrans, &mut diag, &mut p, &mut one, &mut alpha, xtx_rp, &mut p, xty_rp, &mut p);
    }

    unsafe { extendr_ffi::Rf_unprotect(2) };
    unsafe { Robj::from_sexp(xty) }
}

// Layer 5: ALTREP
#[extendr]
fn extendr_bench_altrep_create(x: Robj) -> Robj {
    let s = unsafe { std::mem::transmute::<Robj, extendr_ffi::SEXP>(x) };
    let call = unsafe { extendr_ffi::Rf_protect(Rf_lang2(Rf_install("seq_len\0".as_ptr() as _), s)) };
    let mut err: i32 = 0;
    let result = unsafe { R_tryEvalSilent(call, R_GlobalEnv, &mut err) };
    unsafe { extendr_ffi::Rf_unprotect(1) };
    if err != 0 { return r!(0i32); }
    unsafe { Robj::from_sexp(result) }
}
#[extendr]
fn extendr_bench_altrep_materialize(x: Robj) -> Robj {
    let s = unsafe { std::mem::transmute::<Robj, extendr_ffi::SEXP>(x) };
    let call = unsafe { extendr_ffi::Rf_protect(Rf_lang2(Rf_install("seq_len\0".as_ptr() as _), s)) };
    let mut err: i32 = 0;
    let alt = unsafe { R_tryEvalSilent(call, R_GlobalEnv, &mut err) };
    unsafe { extendr_ffi::Rf_unprotect(1) };
    if err != 0 { return r!(0i32); }
    let mat = unsafe { extendr_ffi::Rf_duplicate(alt) };
    let n = unsafe { extendr_ffi::LENGTH(mat) };
    let data = unsafe { extendr_ffi::INTEGER(mat) };
    let sum = unsafe { *data.offset(0) as i64 + *data.offset((n - 1) as isize) as i64 };
    r!(sum as i32)
}
#[extendr]
fn extendr_bench_altrep_elt_walk(x: Robj) -> Robj {
    let s = unsafe { std::mem::transmute::<Robj, extendr_ffi::SEXP>(x) };
    let call = unsafe { extendr_ffi::Rf_protect(Rf_lang2(Rf_install("seq_len\0".as_ptr() as _), s)) };
    let mut err: i32 = 0;
    let alt = unsafe { R_tryEvalSilent(call, R_GlobalEnv, &mut err) };
    unsafe { extendr_ffi::Rf_unprotect(1) };
    if err != 0 { return r!(0i32); }
    let n = unsafe { extendr_ffi::Rf_xlength(alt) };
    let mut total: i64 = 0;
    for i in 0..n {
        total += unsafe { INTEGER_ELT(alt, i) as i64 };
    }
    r!(total as f64)
}
#[extendr]
fn extendr_bench_altrep_region_read(x: Robj) -> Robj {
    let s = unsafe { std::mem::transmute::<Robj, extendr_ffi::SEXP>(x) };
    let call = unsafe { extendr_ffi::Rf_protect(Rf_lang2(Rf_install("seq_len\0".as_ptr() as _), s)) };
    let mut err: i32 = 0;
    let alt = unsafe { R_tryEvalSilent(call, R_GlobalEnv, &mut err) };
    unsafe { extendr_ffi::Rf_unprotect(1) };
    if err != 0 { return r!(0i32); }
    let n = unsafe { extendr_ffi::Rf_xlength(alt) };
    const CHUNK: isize = 4096;
    let mut buf: Vec<i32> = vec![0; 4096];
    let mut total: i64 = 0;
    let mut i: isize = 0;
    while i < n {
        let want = if n - i < CHUNK { n - i } else { CHUNK };
        let got = unsafe { INTEGER_GET_REGION(alt, i, want, buf.as_mut_ptr()) };
        for j in 0..got {
            total += buf[j as usize] as i64;
        }
        i += got;
    }
    r!(total as f64)
}
#[extendr]
fn extendr_bench_altrep_sum_via_R(x: Robj) -> Robj {
    let s = unsafe { std::mem::transmute::<Robj, extendr_ffi::SEXP>(x) };
    let call = unsafe { extendr_ffi::Rf_protect(Rf_lang2(Rf_install("seq_len\0".as_ptr() as _), s)) };
    let mut err: i32 = 0;
    let alt = unsafe { R_tryEvalSilent(call, R_GlobalEnv, &mut err) };
    if err != 0 { unsafe { extendr_ffi::Rf_unprotect(1) }; return r!(0i32); }
    let sum_call = unsafe { extendr_ffi::Rf_protect(Rf_lang2(Rf_install("sum\0".as_ptr() as _), alt)) };
    let res = unsafe { R_tryEvalSilent(sum_call, R_GlobalEnv, &mut err) };
    unsafe { extendr_ffi::Rf_unprotect(2) };
    if err != 0 { return r!(0i32); }
    unsafe { Robj::from_sexp(res) }
}
#[extendr]
fn extendr_bench_altrep_sum_native(x: Robj) -> Robj {
    extendr_bench_altrep_elt_walk(x)
}
#[extendr]
fn extendr_bench_altrep_min_max(x: Robj) -> Robj {
    let s = unsafe { std::mem::transmute::<Robj, extendr_ffi::SEXP>(x) };
    let call = unsafe { extendr_ffi::Rf_protect(Rf_lang2(Rf_install("seq_len\0".as_ptr() as _), s)) };
    let mut err: i32 = 0;
    let alt = unsafe { R_tryEvalSilent(call, R_GlobalEnv, &mut err) };
    unsafe { extendr_ffi::Rf_unprotect(1) };
    if err != 0 { return r!(0i32); }
    let n = unsafe { extendr_ffi::Rf_xlength(alt) };
    let mut min_val = unsafe { INTEGER_ELT(alt, 0) };
    let mut max_val = min_val;
    for i in 1..n {
        let v = unsafe { INTEGER_ELT(alt, i) };
        if v < min_val { min_val = v; }
        if v > max_val { max_val = v; }
    }
    r!(max_val - min_val)
}
#[extendr]
fn extendr_bench_altrep_no_na_query(x: Robj) -> Robj {
    let s = unsafe { std::mem::transmute::<Robj, extendr_ffi::SEXP>(x) };
    let call = unsafe { extendr_ffi::Rf_protect(Rf_lang2(Rf_install("seq_len\0".as_ptr() as _), s)) };
    let mut err: i32 = 0;
    let alt = unsafe { R_tryEvalSilent(call, R_GlobalEnv, &mut err) };
    unsafe { extendr_ffi::Rf_unprotect(1) };
    if err != 0 { return r!(0i32); }
    let n = unsafe { extendr_ffi::Rf_xlength(alt) };
    let mut has_na: i32 = 0;
    for i in 0..n {
        if unsafe { INTEGER_ELT(alt, i) } == i32::MIN { has_na = 1; break; }
    }
    r!(has_na)
}
#[extendr]
fn extendr_bench_struct_convert(x: Robj) -> Robj {
    let s = unsafe { std::mem::transmute::<Robj, extendr_ffi::SEXP>(x) };
    let field_names: [&[u8]; 10] = [
        b"id\0", b"count\0", b"level\0", b"flag\0", b"enabled\0",
        b"ratio\0", b"offset\0", b"scale\0", b"weights\0", b"indices\0",
    ];
    let names = unsafe { extendr_ffi::Rf_protect(extendr_ffi::Rf_allocVector(extendr_ffi::SEXPTYPE::STRSXP, 10)) };
    for i in 0usize..10 {
        unsafe { extendr_ffi::SET_STRING_ELT(names, i as isize, Rf_mkChar(field_names[i].as_ptr() as _)) };
    }
    let result = unsafe { extendr_ffi::Rf_protect(extendr_ffi::Rf_allocVector(extendr_ffi::SEXPTYPE::VECSXP, 10)) };
    unsafe { extendr_ffi::Rf_setAttrib(result, extendr_ffi::R_NamesSymbol, names) };
    unsafe {
        extendr_ffi::SET_VECTOR_ELT(result, 0, Rf_ScalarInteger(Rf_asInteger(VECTOR_ELT(s, 0))));
        extendr_ffi::SET_VECTOR_ELT(result, 1, Rf_ScalarInteger(Rf_asInteger(VECTOR_ELT(s, 1))));
        extendr_ffi::SET_VECTOR_ELT(result, 2, Rf_ScalarInteger(Rf_asInteger(VECTOR_ELT(s, 2))));
        extendr_ffi::SET_VECTOR_ELT(result, 3, Rf_ScalarLogical(Rf_asLogical(VECTOR_ELT(s, 3))));
        extendr_ffi::SET_VECTOR_ELT(result, 4, Rf_ScalarLogical(Rf_asLogical(VECTOR_ELT(s, 4))));
        extendr_ffi::SET_VECTOR_ELT(result, 5, Rf_ScalarReal(Rf_asReal(VECTOR_ELT(s, 5))));
        extendr_ffi::SET_VECTOR_ELT(result, 6, Rf_ScalarReal(Rf_asReal(VECTOR_ELT(s, 6))));
        extendr_ffi::SET_VECTOR_ELT(result, 7, Rf_ScalarReal(Rf_asReal(VECTOR_ELT(s, 7))));
        extendr_ffi::SET_VECTOR_ELT(result, 8, VECTOR_ELT(s, 8));
        extendr_ffi::SET_VECTOR_ELT(result, 9, VECTOR_ELT(s, 9));
    }
    unsafe { extendr_ffi::Rf_unprotect(2) };
    unsafe { std::mem::transmute::<extendr_ffi::SEXP, Robj>(result) }
}
#[extendr]
fn extendr_bench_r_eval(x: Robj) -> Robj {
    let s = unsafe { std::mem::transmute::<Robj, extendr_ffi::SEXP>(x) };
    let sum_call = unsafe { extendr_ffi::Rf_protect(Rf_lang2(Rf_install("sum\0".as_ptr() as _), s)) };
    let mut err: i32 = 0;
    let sum_res = unsafe { R_tryEvalSilent(sum_call, R_GlobalEnv, &mut err) };
    let sum_val = unsafe { *extendr_ffi::REAL(sum_res) };
    unsafe { extendr_ffi::Rf_unprotect(1) };

    let mean_call = unsafe { extendr_ffi::Rf_protect(Rf_lang2(Rf_install("mean\0".as_ptr() as _), s)) };
    err = 0;
    let mean_res = unsafe { R_tryEvalSilent(mean_call, R_GlobalEnv, &mut err) };
    let mean_val = unsafe { *extendr_ffi::REAL(mean_res) };
    unsafe { extendr_ffi::Rf_unprotect(1) };

    r!(sum_val + mean_val)
}
#[extendr]
fn extendr_bench_r_tryeval(x: Robj) -> Robj {
    let _ = x;
    let mut count: i32 = 0;
    for _ in 0..512 {
        let call = unsafe { extendr_ffi::Rf_protect(Rf_lang2(Rf_install("stop\0".as_ptr() as _), Rf_mkString("task40\0".as_ptr() as _))) };
        let mut err: i32 = 0;
        unsafe { R_tryEvalSilent(call, R_GlobalEnv, &mut err) };
        unsafe { extendr_ffi::Rf_unprotect(1) };
        if err != 0 { count += 1; }
    }
    r!(count)
}
#[extendr]
fn extendr_bench_serialize_roundtrip(x: Robj) -> Robj {
    let s = unsafe { std::mem::transmute::<Robj, extendr_ffi::SEXP>(x) };
    let ser_call = unsafe { extendr_ffi::Rf_protect(Rf_lang3(Rf_install("serialize\0".as_ptr() as _), s, extendr_ffi::R_NilValue)) };
    let mut err: i32 = 0;
    let conn = unsafe { R_tryEvalSilent(ser_call, R_GlobalEnv, &mut err) };
    unsafe { extendr_ffi::Rf_unprotect(1) };

    let unser_call = unsafe { extendr_ffi::Rf_protect(Rf_lang2(Rf_install("unserialize\0".as_ptr() as _), conn)) };
    err = 0;
    let result = unsafe { R_tryEvalSilent(unser_call, R_GlobalEnv, &mut err) };
    unsafe { extendr_ffi::Rf_unprotect(1) };

    let n = unsafe { extendr_ffi::Rf_xlength(result) };
    let xp = unsafe { extendr_ffi::REAL(result) };
    let mut total = 0.0f64;
    for i in 0..n { total += unsafe { *xp.offset(i) }; }
    r!(total)
}
#[extendr]
fn extendr_bench_external_ptr(x: Robj) -> Robj {
    let _ = x;
    let mut dummy: u8 = 0;
    let ptr = unsafe { extendr_ffi::Rf_protect(R_MakeExternalPtr(&mut dummy as *mut u8 as *mut _, extendr_ffi::R_NilValue, extendr_ffi::R_NilValue)) };
    unsafe { extendr_ffi::Rf_unprotect(1) };
    unsafe { std::mem::transmute::<extendr_ffi::SEXP, Robj>(ptr) }
}
#[extendr]
fn extendr_bench_rng_stress(x: Robj) -> Robj {
    let s = unsafe { std::mem::transmute::<Robj, extendr_ffi::SEXP>(x) };
    let n = unsafe { Rf_asInteger(s) };
    let result = unsafe { extendr_ffi::Rf_protect(extendr_ffi::Rf_allocVector(extendr_ffi::SEXPTYPE::REALSXP, n as isize)) };
    let rp = unsafe { extendr_ffi::REAL(result) };
    unsafe { GetRNGstate(); }
    for i in 0..n {
        unsafe { *rp.offset(i as isize) = norm_rand(); }
    }
    unsafe { PutRNGstate(); }
    unsafe { extendr_ffi::Rf_unprotect(1) };
    unsafe { std::mem::transmute::<extendr_ffi::SEXP, Robj>(result) }
}

#[no_mangle]
pub unsafe extern "C" fn extendr_ffi_matmul(a: extendr_ffi::SEXP, b: extendr_ffi::SEXP) -> extendr_ffi::SEXP {
    let mut n = extendr_ffi::Rf_nrows(a);
    let mut m = extendr_ffi::Rf_ncols(b);
    let mut k = extendr_ffi::Rf_ncols(a);
    let result = extendr_ffi::Rf_protect(extendr_ffi::Rf_allocVector(extendr_ffi::SEXPTYPE::REALSXP, (n as isize) * (m as isize)));
    let dims = extendr_ffi::Rf_protect(extendr_ffi::Rf_allocVector(extendr_ffi::SEXPTYPE::INTSXP, 2));
    *extendr_ffi::INTEGER(dims).offset(0) = n;
    *extendr_ffi::INTEGER(dims).offset(1) = m;
    extendr_ffi::Rf_setAttrib(result, extendr_ffi::R_DimSymbol, dims);
    let mut notrans: u8 = b'N';
    let mut alpha: f64 = 1.0;
    let mut beta: f64 = 0.0;
    dgemm_(&mut notrans, &mut notrans, &mut n, &mut m, &mut k,
           &mut alpha, extendr_ffi::REAL(a), &mut n,
           extendr_ffi::REAL(b), &mut k,
           &mut beta, extendr_ffi::REAL(result), &mut n);
    extendr_ffi::Rf_unprotect(2);
    result
}

#[no_mangle]
pub unsafe extern "C" fn extendr_ffi_external_ptr(x: extendr_ffi::SEXP) -> extendr_ffi::SEXP {
    let _ = x;
    let mut dummy: u8 = 0;
    let ptr = extendr_ffi::Rf_protect(R_MakeExternalPtr(&mut dummy as *mut u8 as *mut _, extendr_ffi::R_NilValue, extendr_ffi::R_NilValue));
    extendr_ffi::Rf_unprotect(1);
    ptr
}

#[no_mangle]
pub unsafe extern "C" fn extendr_ffi_struct_convert(x: extendr_ffi::SEXP) -> extendr_ffi::SEXP {
    let field_names: [&[u8]; 10] = [
        b"id\0", b"count\0", b"level\0", b"flag\0", b"enabled\0",
        b"ratio\0", b"offset\0", b"scale\0", b"weights\0", b"indices\0",
    ];
    let names = extendr_ffi::Rf_protect(extendr_ffi::Rf_allocVector(extendr_ffi::SEXPTYPE::STRSXP, 10));
    for i in 0usize..10 {
        extendr_ffi::SET_STRING_ELT(names, i as isize, Rf_mkChar(field_names[i].as_ptr() as _));
    }
    let result = extendr_ffi::Rf_protect(extendr_ffi::Rf_allocVector(extendr_ffi::SEXPTYPE::VECSXP, 10));
    extendr_ffi::Rf_setAttrib(result, extendr_ffi::R_NamesSymbol, names);
    extendr_ffi::SET_VECTOR_ELT(result, 0, Rf_ScalarInteger(Rf_asInteger(VECTOR_ELT(x, 0))));
    extendr_ffi::SET_VECTOR_ELT(result, 1, Rf_ScalarInteger(Rf_asInteger(VECTOR_ELT(x, 1))));
    extendr_ffi::SET_VECTOR_ELT(result, 2, Rf_ScalarInteger(Rf_asInteger(VECTOR_ELT(x, 2))));
    extendr_ffi::SET_VECTOR_ELT(result, 3, Rf_ScalarLogical(Rf_asLogical(VECTOR_ELT(x, 3))));
    extendr_ffi::SET_VECTOR_ELT(result, 4, Rf_ScalarLogical(Rf_asLogical(VECTOR_ELT(x, 4))));
    extendr_ffi::SET_VECTOR_ELT(result, 5, Rf_ScalarReal(Rf_asReal(VECTOR_ELT(x, 5))));
    extendr_ffi::SET_VECTOR_ELT(result, 6, Rf_ScalarReal(Rf_asReal(VECTOR_ELT(x, 6))));
    extendr_ffi::SET_VECTOR_ELT(result, 7, Rf_ScalarReal(Rf_asReal(VECTOR_ELT(x, 7))));
    extendr_ffi::SET_VECTOR_ELT(result, 8, VECTOR_ELT(x, 8));
    extendr_ffi::SET_VECTOR_ELT(result, 9, VECTOR_ELT(x, 9));
    extendr_ffi::Rf_unprotect(2);
    result
}

#[no_mangle]
pub unsafe extern "C" fn extendr_ffi_rng_stress(x: extendr_ffi::SEXP) -> extendr_ffi::SEXP {
    let n = Rf_asInteger(x);
    let result = extendr_ffi::Rf_protect(extendr_ffi::Rf_allocVector(extendr_ffi::SEXPTYPE::REALSXP, n as isize));
    let rp = extendr_ffi::REAL(result);
    GetRNGstate();
    for i in 0..n {
        *rp.offset(i as isize) = norm_rand();
    }
    PutRNGstate();
    extendr_ffi::Rf_unprotect(1);
    result
}

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
