use extendr_api::prelude::*;
use extendr_ffi::{
    GetRNGstate, INTEGER, INTEGER_GET_REGION, LOGICAL, PutRNGstate, R_ClassSymbol, R_GlobalEnv, R_NaInt,
    R_NaReal, R_NaString, R_NamesSymbol, R_RowNamesSymbol, R_CHAR, REAL, R_IsNA,
    Rf_allocMatrix, R_NilValue, Rf_ScalarInteger, Rf_allocVector, Rf_getAttrib, Rf_install,
    Rf_isInteger, Rf_isLogical, Rf_isReal, Rf_ncols, Rf_nrows, Rf_protect, Rf_setAttrib,
    Rf_unprotect, Rf_xlength, SET_STRING_ELT, SET_VECTOR_ELT, STRING_ELT, VECTOR_ELT, SEXP,
    SEXPTYPE,
};
use std::ffi::{CStr, CString};
use std::ffi::c_void;
use std::os::raw::{c_char, c_int};
use std::thread;

#[repr(C)]
#[derive(Copy, Clone)]
struct RAltRepClass {
    ptr: SEXP,
}

const ALTREP_CREATE_MAX_LEN: usize = 1_000_000;
const COMPTIME_DISPATCH_REPEATS: usize = 256;
const ALLOCATION_REPEATS: usize = 100;
const PROTECT_OVERHEAD_REPEATS: usize = 4096;
const LONGJMP_SAFETY_REPEATS: usize = 512;
const TRANSLATE_C_COST_REPEATS: usize = 512;
const MEMORY_BANDWIDTH_REPEATS: usize = 2;

static mut EXTENDR_ALTREP_CREATE_CLASS: RAltRepClass = RAltRepClass {
    ptr: std::ptr::null_mut(),
};
static mut EXTENDR_ALTREP_CREATE_CLASS_READY: bool = false;
static mut EXTENDR_ALTREP_BACKING: [f64; ALTREP_CREATE_MAX_LEN] = [0.0; ALTREP_CREATE_MAX_LEN];
static mut EXTENDR_ALTREP_BACKING_INIT: usize = 0;
static mut EXTENDR_ALTINT_CREATE_CLASS: RAltRepClass = RAltRepClass {
    ptr: std::ptr::null_mut(),
};
static mut EXTENDR_ALTINT_CREATE_CLASS_READY: bool = false;
static mut EXTENDR_ALTINT_BACKING: [i32; ALTREP_CREATE_MAX_LEN] = [0; ALTREP_CREATE_MAX_LEN];
static mut EXTENDR_ALTINT_BACKING_INIT: usize = 0;
static mut EXTENDR_ALTLOGICAL_CREATE_CLASS: RAltRepClass = RAltRepClass {
    ptr: std::ptr::null_mut(),
};
static mut EXTENDR_ALTLOGICAL_CREATE_CLASS_READY: bool = false;
static mut EXTENDR_ALTLOGICAL_BACKING: [i32; ALTREP_CREATE_MAX_LEN] = [0; ALTREP_CREATE_MAX_LEN];
static mut EXTENDR_ALTLOGICAL_BACKING_INIT: usize = 0;

unsafe extern "C" {
    fn Rf_eval(expr: SEXP, env: SEXP) -> SEXP;

    fn Rf_lang2(fun: SEXP, arg: SEXP) -> SEXP;

    fn Rf_mkChar(name: *const c_char) -> SEXP;

    fn R_ProtectWithIndex(value: SEXP, index: *mut c_int);

    fn R_Reprotect(value: SEXP, index: c_int);

    fn R_tryEvalSilent(expr: SEXP, env: SEXP, err: *mut c_int) -> SEXP;

    fn R_MakeUnwindCont() -> SEXP;

    fn R_UnwindProtect(
        fun: unsafe extern "C" fn(*mut c_void) -> SEXP,
        data: *mut c_void,
        cleanfun: unsafe extern "C" fn(*mut c_void, c_int),
        cleandata: *mut c_void,
        cont: SEXP,
    ) -> SEXP;

    fn norm_rand() -> f64;

    fn dgemm_(
        transa: *const c_char,
        transb: *const c_char,
        m: *const c_int,
        n: *const c_int,
        k: *const c_int,
        alpha: *const f64,
        a: *const f64,
        lda: *const c_int,
        b: *const f64,
        ldb: *const c_int,
        beta: *const f64,
        c: *mut f64,
        ldc: *const c_int,
        len_a: c_int,
        len_b: c_int,
    );

    fn dsyrk_(
        uplo: *const c_char,
        trans: *const c_char,
        n: *const c_int,
        k: *const c_int,
        alpha: *const f64,
        a: *const f64,
        lda: *const c_int,
        beta: *const f64,
        c: *mut f64,
        ldc: *const c_int,
        len_uplo: c_int,
        len_trans: c_int,
    );

    fn dpotrf_(
        uplo: *const c_char,
        n: *const c_int,
        a: *mut f64,
        lda: *const c_int,
        info: *mut c_int,
        len_uplo: c_int,
    );

    fn dtrsm_(
        side: *const c_char,
        uplo: *const c_char,
        transa: *const c_char,
        diag: *const c_char,
        m: *const c_int,
        n: *const c_int,
        alpha: *const f64,
        a: *const f64,
        lda: *const c_int,
        b: *mut f64,
        ldb: *const c_int,
        len_side: c_int,
        len_uplo: c_int,
        len_transa: c_int,
        len_diag: c_int,
    );

    fn R_new_altrep(aclass: RAltRepClass, data1: SEXP, data2: SEXP) -> SEXP;

    fn R_make_altreal_class(
        cname: *const c_char,
        pname: *const c_char,
        info: *mut c_void,
    ) -> RAltRepClass;

    fn R_make_altinteger_class(
        cname: *const c_char,
        pname: *const c_char,
        info: *mut c_void,
    ) -> RAltRepClass;

    fn R_make_altlogical_class(
        cname: *const c_char,
        pname: *const c_char,
        info: *mut c_void,
    ) -> RAltRepClass;

    fn R_set_altrep_Length_method(cls: RAltRepClass, fun: unsafe extern "C" fn(SEXP) -> isize);

    fn R_set_altvec_Dataptr_method(
        cls: RAltRepClass,
        fun: unsafe extern "C" fn(SEXP, c_int) -> *mut c_void,
    );

    fn R_set_altrep_Duplicate_method(
        cls: RAltRepClass,
        fun: unsafe extern "C" fn(SEXP, c_int) -> SEXP,
    );

    fn R_set_altreal_Elt_method(
        cls: RAltRepClass,
        fun: unsafe extern "C" fn(SEXP, isize) -> f64,
    );

    fn R_set_altinteger_Elt_method(
        cls: RAltRepClass,
        fun: unsafe extern "C" fn(SEXP, isize) -> i32,
    );

    fn R_set_altlogical_Elt_method(
        cls: RAltRepClass,
        fun: unsafe extern "C" fn(SEXP, isize) -> i32,
    );

    fn R_set_altreal_Get_region_method(
        cls: RAltRepClass,
        fun: unsafe extern "C" fn(SEXP, isize, isize, *mut f64) -> isize,
    );

    fn R_set_altinteger_Get_region_method(
        cls: RAltRepClass,
        fun: unsafe extern "C" fn(SEXP, isize, isize, *mut i32) -> isize,
    );

    fn R_set_altlogical_Get_region_method(
        cls: RAltRepClass,
        fun: unsafe extern "C" fn(SEXP, isize, isize, *mut i32) -> isize,
    );

    fn R_altrep_data2(x: SEXP) -> SEXP;
}

unsafe extern "C" fn extendr_altrep_create_length(x: SEXP) -> isize {
    *INTEGER(R_altrep_data2(x)) as isize
}

unsafe extern "C" fn extendr_altrep_create_elt(_x: SEXP, i: isize) -> f64 {
    EXTENDR_ALTREP_BACKING[i as usize]
}

unsafe extern "C" fn extendr_altrep_create_dataptr(_x: SEXP, _writable: c_int) -> *mut c_void {
    std::ptr::addr_of_mut!(EXTENDR_ALTREP_BACKING).cast::<f64>() as *mut c_void
}

unsafe extern "C" fn extendr_altrep_create_get_region(
    x: SEXP,
    i: isize,
    n: isize,
    buf: *mut f64,
) -> isize {
    let len = extendr_altrep_create_length(x);
    if i >= len {
        return 0;
    }

    let available = len - i;
    let count = n.min(available);
    std::ptr::copy_nonoverlapping(
        std::ptr::addr_of!(EXTENDR_ALTREP_BACKING).cast::<f64>().add(i as usize),
        buf,
        count as usize,
    );
    count
}

unsafe extern "C" fn extendr_altrep_create_duplicate(x: SEXP, _deep: c_int) -> SEXP {
    let len = extendr_altrep_create_length(x);
    let out = Rf_protect(Rf_allocVector(SEXPTYPE::REALSXP, len));
    std::ptr::copy_nonoverlapping(
        std::ptr::addr_of!(EXTENDR_ALTREP_BACKING).cast::<f64>(),
        REAL(out),
        len as usize,
    );
    Rf_unprotect(1);
    out
}

unsafe fn extendr_altrep_create_ensure_class() {
    if EXTENDR_ALTREP_CREATE_CLASS_READY {
        return;
    }

    EXTENDR_ALTREP_CREATE_CLASS =
        R_make_altreal_class(c"bench_altreal_create_extendr".as_ptr(), c"extendr_benchmarks".as_ptr(), std::ptr::null_mut());
    R_set_altrep_Length_method(EXTENDR_ALTREP_CREATE_CLASS, extendr_altrep_create_length);
    R_set_altreal_Elt_method(EXTENDR_ALTREP_CREATE_CLASS, extendr_altrep_create_elt);
    R_set_altvec_Dataptr_method(EXTENDR_ALTREP_CREATE_CLASS, extendr_altrep_create_dataptr);
    R_set_altrep_Duplicate_method(EXTENDR_ALTREP_CREATE_CLASS, extendr_altrep_create_duplicate);
    R_set_altreal_Get_region_method(
        EXTENDR_ALTREP_CREATE_CLASS,
        extendr_altrep_create_get_region,
    );
    EXTENDR_ALTREP_CREATE_CLASS_READY = true;
}

unsafe fn extendr_altrep_create_ensure_backing(n: usize) {
    if EXTENDR_ALTREP_BACKING_INIT >= n {
        return;
    }

    let base = std::ptr::addr_of_mut!(EXTENDR_ALTREP_BACKING).cast::<f64>();
    for index in EXTENDR_ALTREP_BACKING_INIT..n {
        *base.add(index) = (index + 1) as f64;
    }
    EXTENDR_ALTREP_BACKING_INIT = n;
}

unsafe extern "C" fn extendr_altint_create_length(x: SEXP) -> isize {
    *INTEGER(R_altrep_data2(x)) as isize
}

unsafe extern "C" fn extendr_altint_create_elt(_x: SEXP, i: isize) -> i32 {
    EXTENDR_ALTINT_BACKING[i as usize]
}

unsafe extern "C" fn extendr_altint_create_dataptr(_x: SEXP, _writable: c_int) -> *mut c_void {
    std::ptr::addr_of_mut!(EXTENDR_ALTINT_BACKING).cast::<i32>() as *mut c_void
}

unsafe extern "C" fn extendr_altint_create_get_region(
    x: SEXP,
    i: isize,
    n: isize,
    buf: *mut i32,
) -> isize {
    let len = extendr_altint_create_length(x);
    if i >= len {
        return 0;
    }

    let available = len - i;
    let count = n.min(available);
    std::ptr::copy_nonoverlapping(
        std::ptr::addr_of!(EXTENDR_ALTINT_BACKING).cast::<i32>().add(i as usize),
        buf,
        count as usize,
    );
    count
}

unsafe extern "C" fn extendr_altint_create_duplicate(x: SEXP, _deep: c_int) -> SEXP {
    let len = extendr_altint_create_length(x);
    let out = Rf_protect(Rf_allocVector(SEXPTYPE::INTSXP, len));
    std::ptr::copy_nonoverlapping(
        std::ptr::addr_of!(EXTENDR_ALTINT_BACKING).cast::<i32>(),
        INTEGER(out),
        len as usize,
    );
    Rf_unprotect(1);
    out
}

unsafe fn extendr_altint_create_ensure_class() {
    if EXTENDR_ALTINT_CREATE_CLASS_READY {
        return;
    }

    EXTENDR_ALTINT_CREATE_CLASS = R_make_altinteger_class(
        c"bench_altinteger_owned_sum_extendr".as_ptr(),
        c"extendr_benchmarks".as_ptr(),
        std::ptr::null_mut(),
    );
    R_set_altrep_Length_method(EXTENDR_ALTINT_CREATE_CLASS, extendr_altint_create_length);
    R_set_altinteger_Elt_method(EXTENDR_ALTINT_CREATE_CLASS, extendr_altint_create_elt);
    R_set_altvec_Dataptr_method(EXTENDR_ALTINT_CREATE_CLASS, extendr_altint_create_dataptr);
    R_set_altrep_Duplicate_method(EXTENDR_ALTINT_CREATE_CLASS, extendr_altint_create_duplicate);
    R_set_altinteger_Get_region_method(
        EXTENDR_ALTINT_CREATE_CLASS,
        extendr_altint_create_get_region,
    );
    EXTENDR_ALTINT_CREATE_CLASS_READY = true;
}

unsafe fn extendr_altint_create_ensure_backing(n: usize) {
    if EXTENDR_ALTINT_BACKING_INIT >= n {
        return;
    }

    let base = std::ptr::addr_of_mut!(EXTENDR_ALTINT_BACKING).cast::<i32>();
    for index in EXTENDR_ALTINT_BACKING_INIT..n {
        *base.add(index) = ((index % 1024) + 1) as i32;
    }
    EXTENDR_ALTINT_BACKING_INIT = n;
}

unsafe extern "C" fn extendr_altlogical_create_length(x: SEXP) -> isize {
    *INTEGER(R_altrep_data2(x)) as isize
}

unsafe extern "C" fn extendr_altlogical_create_elt(_x: SEXP, i: isize) -> i32 {
    EXTENDR_ALTLOGICAL_BACKING[i as usize]
}

unsafe extern "C" fn extendr_altlogical_create_dataptr(
    _x: SEXP,
    _writable: c_int,
) -> *mut c_void {
    std::ptr::addr_of_mut!(EXTENDR_ALTLOGICAL_BACKING).cast::<i32>() as *mut c_void
}

unsafe extern "C" fn extendr_altlogical_create_get_region(
    x: SEXP,
    i: isize,
    n: isize,
    buf: *mut i32,
) -> isize {
    let len = extendr_altlogical_create_length(x);
    if i >= len {
        return 0;
    }

    let available = len - i;
    let count = n.min(available);
    std::ptr::copy_nonoverlapping(
        std::ptr::addr_of!(EXTENDR_ALTLOGICAL_BACKING)
            .cast::<i32>()
            .add(i as usize),
        buf,
        count as usize,
    );
    count
}

unsafe extern "C" fn extendr_altlogical_create_duplicate(x: SEXP, _deep: c_int) -> SEXP {
    let len = extendr_altlogical_create_length(x);
    let out = Rf_protect(Rf_allocVector(SEXPTYPE::LGLSXP, len));
    std::ptr::copy_nonoverlapping(
        std::ptr::addr_of!(EXTENDR_ALTLOGICAL_BACKING).cast::<i32>(),
        LOGICAL(out),
        len as usize,
    );
    Rf_unprotect(1);
    out
}

unsafe fn extendr_altlogical_create_ensure_class() {
    if EXTENDR_ALTLOGICAL_CREATE_CLASS_READY {
        return;
    }

    EXTENDR_ALTLOGICAL_CREATE_CLASS = R_make_altlogical_class(
        c"bench_altlogical_owned_sum_extendr".as_ptr(),
        c"extendr_benchmarks".as_ptr(),
        std::ptr::null_mut(),
    );
    R_set_altrep_Length_method(
        EXTENDR_ALTLOGICAL_CREATE_CLASS,
        extendr_altlogical_create_length,
    );
    R_set_altlogical_Elt_method(
        EXTENDR_ALTLOGICAL_CREATE_CLASS,
        extendr_altlogical_create_elt,
    );
    R_set_altvec_Dataptr_method(
        EXTENDR_ALTLOGICAL_CREATE_CLASS,
        extendr_altlogical_create_dataptr,
    );
    R_set_altrep_Duplicate_method(
        EXTENDR_ALTLOGICAL_CREATE_CLASS,
        extendr_altlogical_create_duplicate,
    );
    R_set_altlogical_Get_region_method(
        EXTENDR_ALTLOGICAL_CREATE_CLASS,
        extendr_altlogical_create_get_region,
    );
    EXTENDR_ALTLOGICAL_CREATE_CLASS_READY = true;
}

unsafe fn extendr_altlogical_create_ensure_backing(n: usize) {
    if EXTENDR_ALTLOGICAL_BACKING_INIT >= n {
        return;
    }

    let base = std::ptr::addr_of_mut!(EXTENDR_ALTLOGICAL_BACKING).cast::<i32>();
    for index in EXTENDR_ALTLOGICAL_BACKING_INIT..n {
        *base.add(index) = if (index & 1) == 0 { 1 } else { 0 };
    }
    EXTENDR_ALTLOGICAL_BACKING_INIT = n;
}

fn dispatch_sum_atomic(sexp: SEXP) -> f64 {
    unsafe {
        if bool::from(Rf_isLogical(sexp)) {
            int_slice(sexp)
                .iter()
                .map(|&value| if value != 0 { 1.0 } else { 0.0 })
                .sum::<f64>()
        } else if bool::from(Rf_isReal(sexp)) {
            real_slice(sexp).iter().copied().sum::<f64>()
        } else if bool::from(Rf_isInteger(sexp)) {
            int_slice(sexp).iter().map(|&value| value as f64).sum::<f64>()
        } else {
            0.0
        }
    }
}

#[derive(Clone)]
struct StructConvertPayload {
    id: i32,
    count: i32,
    level: i32,
    flag: bool,
    enabled: bool,
    ratio: f64,
    offset: f64,
    scale: f64,
    weights: Vec<f64>,
    indices: Vec<i32>,
}

unsafe fn find_named(list_sexp: SEXP, key: &str) -> SEXP {
    let names = Rf_getAttrib(list_sexp, R_NamesSymbol);
    let n = Rf_xlength(list_sexp) as usize;
    for index in 0..n {
        let elt = STRING_ELT(names, index as _);
        if elt == R_NaString {
            continue;
        }
        let label = CStr::from_ptr(R_CHAR(elt)).to_str().unwrap_or_default();
        if label == key {
            return VECTOR_ELT(list_sexp, index as _);
        }
    }
    panic!("missing field '{}' in extendr bench_struct_convert", key);
}

fn fib_i32(n: i32) -> i32 {
    if n <= 1 {
        return n;
    }

    let mut a = 0;
    let mut b = 1;
    for _ in 2..=n {
        let next = a + b;
        a = b;
        b = next;
    }
    b
}

fn radix_sort_f64(items: &mut [f64]) {
    if items.len() < 2 {
        return;
    }

    let sign_bit = 1_u64 << 63;
    let buf = unsafe { std::slice::from_raw_parts_mut(items.as_mut_ptr() as *mut u64, items.len()) };

    for value in buf.iter_mut() {
        let current = *value;
        *value = if current & sign_bit != 0 {
            !current
        } else {
            current ^ sign_bit
        };
    }

    let mut temp = vec![0_u64; items.len()];
    let mut counts = [0_usize; 256];
    let mut shift = 0_usize;
    while shift < 64 {
        counts.fill(0);
        for &value in buf.iter() {
            counts[((value >> shift) & 0xFF) as usize] += 1;
        }

        let mut total = 0_usize;
        for count in &mut counts {
            let old = *count;
            *count = total;
            total += old;
        }

        for &value in buf.iter() {
            let digit = ((value >> shift) & 0xFF) as usize;
            temp[counts[digit]] = value;
            counts[digit] += 1;
        }

        buf.copy_from_slice(&temp);
        shift += 8;
    }

    for value in buf.iter_mut() {
        let current = *value;
        *value = if current & sign_bit != 0 {
            current ^ sign_bit
        } else {
            !current
        };
    }
}

unsafe fn real_slice(sexp: SEXP) -> &'static [f64] {
    std::slice::from_raw_parts(REAL(sexp), Rf_xlength(sexp) as usize)
}

unsafe fn int_slice(sexp: SEXP) -> &'static [i32] {
    std::slice::from_raw_parts(INTEGER(sexp), Rf_xlength(sexp) as usize)
}

fn scalar_integer(value: i32) -> Robj {
    value.into()
}

fn scalar_real(value: f64) -> Robj {
    value.into()
}

fn sum_real_slice(data: &[f64]) -> f64 {
    data.iter().copied().sum::<f64>()
}

fn na_mean_slice(data: &[f64]) -> f64 {
    let mut sum = 0.0;
    let mut count = 0_usize;

    for &value in data {
        if unsafe { R_IsNA(value) } != 0 {
            continue;
        }
        sum += value;
        count += 1;
    }

    if count == 0 {
        unsafe { R_NaReal }
    } else {
        sum / count as f64
    }
}

fn allocation_bench_total(data: &[f64]) -> f64 {
    let mut total = 0.0;

    for repeat in 0..ALLOCATION_REPEATS {
        let bias = (repeat as f64 + 1.0) * 0.001;
        let mut temp = vec![0.0_f64; data.len()];

        for (index, value) in data.iter().enumerate() {
            temp[index] = *value + bias;
        }
        total += sum_real_slice(&temp);
    }

    total
}

fn longjmp_direct_total(data: &[f64], bias: f64) -> f64 {
    data.iter().map(|value| *value + bias).sum::<f64>()
}

struct ExtendrUnwindState {
    data: *const f64,
    len: usize,
    bias: f64,
}

unsafe extern "C" fn extendr_unwind_noop(_data: *mut c_void, _jump: c_int) {}

unsafe extern "C" fn extendr_unwind_ok(data: *mut c_void) -> SEXP {
    let state = &*(data as *const ExtendrUnwindState);
    let input = std::slice::from_raw_parts(state.data, state.len);
    scalar_real(longjmp_direct_total(input, state.bias)).get()
}

unsafe fn make_adjusted_temp(input: &[f64], bias: f64) -> SEXP {
    let temp = Rf_allocVector(SEXPTYPE::REALSXP, input.len() as _);
    let data = std::slice::from_raw_parts_mut(REAL(temp), input.len());
    for (index, value) in input.iter().enumerate() {
        data[index] = *value + bias;
    }
    temp
}

fn longjmp_safety_results(data: &[f64]) -> [f64; 4] {
    unsafe {
        let sum_sym = Rf_install(c"sum".as_ptr());
        let stop_sym = Rf_install(c"stop".as_ptr());
        let stop_msg = Rf_protect(Rf_allocVector(SEXPTYPE::STRSXP, 1));
        SET_STRING_ELT(stop_msg, 0, Rf_mkChar(c"task32".as_ptr()));
        let stop_call = Rf_protect(Rf_lang2(stop_sym, stop_msg));

        let mut totals = [0.0_f64; 4];
        for repeat in 0..LONGJMP_SAFETY_REPEATS {
            let bias = (repeat as f64 + 1.0) * 0.001;
            totals[0] += longjmp_direct_total(data, bias);

            let temp = Rf_protect(make_adjusted_temp(data, bias));
            let expr = Rf_protect(Rf_lang2(sum_sym, temp));
            let mut err = 0;
            let eval_result = R_tryEvalSilent(expr, R_GlobalEnv, &mut err);
            totals[1] += *REAL(eval_result);
            Rf_unprotect(2);

            err = 0;
            let _ = R_tryEvalSilent(stop_call, R_GlobalEnv, &mut err);
            if err != 0 {
                totals[2] += 1.0;
            }

            let mut state = ExtendrUnwindState { data: data.as_ptr(), len: data.len(), bias };
            let cont = Rf_protect(R_MakeUnwindCont());
            let unwind_result = R_UnwindProtect(
                extendr_unwind_ok,
                (&mut state as *mut ExtendrUnwindState).cast::<c_void>(),
                extendr_unwind_noop,
                std::ptr::null_mut(),
                cont,
            );
            totals[3] += *REAL(unwind_result);
            Rf_unprotect(1);
        }

        Rf_unprotect(2);
        totals
    }
}

unsafe fn make_named_longjmp_result(values: &[f64; 4]) -> SEXP {
    let result = Rf_protect(Rf_allocVector(SEXPTYPE::REALSXP, 4));
    let names = Rf_protect(Rf_allocVector(SEXPTYPE::STRSXP, 4));
    std::slice::from_raw_parts_mut(REAL(result), 4).copy_from_slice(values);
    SET_STRING_ELT(names, 0, Rf_mkChar(c"direct".as_ptr()));
    SET_STRING_ELT(names, 1, Rf_mkChar(c"try_ok".as_ptr()));
    SET_STRING_ELT(names, 2, Rf_mkChar(c"try_err".as_ptr()));
    SET_STRING_ELT(names, 3, Rf_mkChar(c"unwind_ok".as_ptr()));
    Rf_setAttrib(result, R_NamesSymbol, names);
    Rf_unprotect(1);
    Rf_unprotect(1);
    result
}

fn translate_c_cost_results(data: &[f64]) -> [f64; 4] {
    let mut totals = [0.0_f64; 4];

    for repeat in 0..TRANSLATE_C_COST_REPEATS {
        let bias = (repeat as f64 + 1.0) * 0.001;
        for value in data {
            let shifted = *value + bias;
            totals[0] += (shifted - 0.75).abs();
            totals[1] += shifted.ln();
            totals[2] += shifted.exp();
            totals[3] += shifted.sqrt();
        }
    }

    totals
}

unsafe fn make_named_math_result(values: &[f64; 4]) -> SEXP {
    let result = Rf_protect(Rf_allocVector(SEXPTYPE::REALSXP, 4));
    let names = Rf_protect(Rf_allocVector(SEXPTYPE::STRSXP, 4));
    std::slice::from_raw_parts_mut(REAL(result), 4).copy_from_slice(values);
    SET_STRING_ELT(names, 0, Rf_mkChar(c"abs".as_ptr()));
    SET_STRING_ELT(names, 1, Rf_mkChar(c"log".as_ptr()));
    SET_STRING_ELT(names, 2, Rf_mkChar(c"exp".as_ptr()));
    SET_STRING_ELT(names, 3, Rf_mkChar(c"sqrt".as_ptr()));
    Rf_setAttrib(result, R_NamesSymbol, names);
    Rf_unprotect(1);
    Rf_unprotect(1);
    result
}

fn memory_bandwidth_results(data: &[f64]) -> [f64; 3] {
    let mut copy_temp_total = 0.0_f64;
    let mut copy_out_total = 0.0_f64;
    let mut fill_out_total = 0.0_f64;

    for _ in 0..MEMORY_BANDWIDTH_REPEATS {
        let mut temp = vec![0.0_f64; data.len()];
        temp.copy_from_slice(data);
        copy_temp_total += temp.iter().copied().sum::<f64>();

        unsafe {
            let copy_out = Rf_protect(Rf_allocVector(SEXPTYPE::REALSXP, data.len() as _));
            let copy_slice = std::slice::from_raw_parts_mut(REAL(copy_out), data.len());
            copy_slice.copy_from_slice(data);
            copy_out_total += copy_slice.iter().copied().sum::<f64>();
            Rf_unprotect(1);

            let fill_out = Rf_protect(Rf_allocVector(SEXPTYPE::REALSXP, data.len() as _));
            let fill_slice = std::slice::from_raw_parts_mut(REAL(fill_out), data.len());
            for (index, value) in data.iter().enumerate() {
                fill_slice[index] = *value + 0.5;
            }
            fill_out_total += fill_slice.iter().copied().sum::<f64>();
            Rf_unprotect(1);
        }
    }

    [copy_temp_total, copy_out_total, fill_out_total]
}

unsafe fn make_named_memory_bandwidth_result(values: &[f64; 3]) -> SEXP {
    let result = Rf_protect(Rf_allocVector(SEXPTYPE::REALSXP, 3));
    let names = Rf_protect(Rf_allocVector(SEXPTYPE::STRSXP, 3));
    std::slice::from_raw_parts_mut(REAL(result), 3).copy_from_slice(values);
    SET_STRING_ELT(names, 0, Rf_mkChar(c"copy_temp".as_ptr()));
    SET_STRING_ELT(names, 1, Rf_mkChar(c"copy_out".as_ptr()));
    SET_STRING_ELT(names, 2, Rf_mkChar(c"fill_out".as_ptr()));
    Rf_setAttrib(result, R_NamesSymbol, names);
    Rf_unprotect(1);
    Rf_unprotect(1);
    result
}

fn parallel_scaling_sum(data: &[f64], requested_threads: usize) -> f64 {
    if data.is_empty() {
        return 0.0;
    }

    let actual_threads = requested_threads.min(data.len());
    if actual_threads <= 1 {
        return data.iter().copied().sum::<f64>();
    }

    let chunk_size = data.len() / actual_threads;
    thread::scope(|scope| {
        let mut handles = Vec::with_capacity(actual_threads);
        for index in 0..actual_threads {
            let start = index * chunk_size;
            let end = if index + 1 == actual_threads {
                data.len()
            } else {
                (index + 1) * chunk_size
            };
            let chunk = &data[start..end];
            handles.push(scope.spawn(move || chunk.iter().copied().sum::<f64>()));
        }
        handles.into_iter().map(|handle| handle.join().unwrap()).sum::<f64>()
    })
}

fn parallel_scaling_results(data: &[f64]) -> [f64; 5] {
    [
        parallel_scaling_sum(data, 1),
        parallel_scaling_sum(data, 2),
        parallel_scaling_sum(data, 4),
        parallel_scaling_sum(data, 8),
        parallel_scaling_sum(data, 16),
    ]
}

unsafe fn make_named_parallel_scaling_result(values: &[f64; 5]) -> SEXP {
    let result = Rf_protect(Rf_allocVector(SEXPTYPE::REALSXP, 5));
    let names = Rf_protect(Rf_allocVector(SEXPTYPE::STRSXP, 5));
    std::slice::from_raw_parts_mut(REAL(result), 5).copy_from_slice(values);
    SET_STRING_ELT(names, 0, Rf_mkChar(c"threads_1".as_ptr()));
    SET_STRING_ELT(names, 1, Rf_mkChar(c"threads_2".as_ptr()));
    SET_STRING_ELT(names, 2, Rf_mkChar(c"threads_4".as_ptr()));
    SET_STRING_ELT(names, 3, Rf_mkChar(c"threads_8".as_ptr()));
    SET_STRING_ELT(names, 4, Rf_mkChar(c"threads_16".as_ptr()));
    Rf_setAttrib(result, R_NamesSymbol, names);
    Rf_unprotect(1);
    Rf_unprotect(1);
    result
}

unsafe fn string_variants_result(vec: SEXP) -> SEXP {
    let n = Rf_xlength(vec) as usize;
    let mut concat = String::new();
    let mut first = true;
    let mut nchar_sum = 0_i32;
    let mut prefix_match = 0_i32;

    let result = Rf_protect(Rf_allocVector(SEXPTYPE::VECSXP, 5));
    let names = Rf_protect(Rf_allocVector(SEXPTYPE::STRSXP, 5));
    let concat_out = Rf_protect(Rf_allocVector(SEXPTYPE::STRSXP, 1));
    let nchar_out = Rf_protect(Rf_ScalarInteger(0));
    let prefix_out = Rf_protect(Rf_ScalarInteger(0));
    let extract_out = Rf_protect(Rf_allocVector(SEXPTYPE::STRSXP, n as _));
    let upper_out = Rf_protect(Rf_allocVector(SEXPTYPE::STRSXP, n as _));

    for index in 0..n {
        let elt = STRING_ELT(vec, index as _);
        if elt == R_NaString {
            SET_STRING_ELT(extract_out, index as _, R_NaString);
            SET_STRING_ELT(upper_out, index as _, R_NaString);
            continue;
        }

        let bytes = CStr::from_ptr(R_CHAR(elt)).to_bytes();
        let text = std::str::from_utf8(bytes).unwrap_or_default();
        nchar_sum += bytes.len() as i32;
        if bytes.starts_with(b"abc") {
            prefix_match += 1;
        }

        if !first {
            concat.push(',');
        }
        concat.push_str(text);
        first = false;

        let sub = CString::new(&text[..text.len().min(3)]).unwrap();
        SET_STRING_ELT(extract_out, index as _, Rf_mkChar(sub.as_ptr()));

        let upper = CString::new(text.to_ascii_uppercase()).unwrap();
        SET_STRING_ELT(upper_out, index as _, Rf_mkChar(upper.as_ptr()));
    }

    let concat_c = CString::new(concat).unwrap();
    SET_STRING_ELT(concat_out, 0, Rf_mkChar(concat_c.as_ptr()));
    *INTEGER(nchar_out) = nchar_sum;
    *INTEGER(prefix_out) = prefix_match;

    SET_VECTOR_ELT(result, 0, concat_out);
    SET_VECTOR_ELT(result, 1, nchar_out);
    SET_VECTOR_ELT(result, 2, prefix_out);
    SET_VECTOR_ELT(result, 3, extract_out);
    SET_VECTOR_ELT(result, 4, upper_out);

    SET_STRING_ELT(names, 0, Rf_mkChar(c"concat".as_ptr()));
    SET_STRING_ELT(names, 1, Rf_mkChar(c"nchar_sum".as_ptr()));
    SET_STRING_ELT(names, 2, Rf_mkChar(c"prefix_match".as_ptr()));
    SET_STRING_ELT(names, 3, Rf_mkChar(c"extract_substr".as_ptr()));
    SET_STRING_ELT(names, 4, Rf_mkChar(c"to_upper".as_ptr()));
    Rf_setAttrib(result, R_NamesSymbol, names);
    Rf_unprotect(7);
    result
}

unsafe fn fill_sum_alloc(sexp: SEXP, input: &[f64], bias: f64) -> f64 {
    let data = std::slice::from_raw_parts_mut(REAL(sexp), input.len());
    let mut total = 0.0;

    for (index, value) in input.iter().enumerate() {
        let adjusted = *value + bias;
        data[index] = adjusted;
        total += adjusted;
    }

    total
}

fn protect_overhead_results(data: &[f64]) -> [f64; 5] {
    unsafe {
        let mut totals = [0.0_f64; 5];

        for repeat in 0..PROTECT_OVERHEAD_REPEATS {
            let bias = (repeat as f64 + 1.0) * 0.001;
            let temp = Rf_allocVector(SEXPTYPE::REALSXP, data.len() as _);
            totals[0] += fill_sum_alloc(temp, data, bias);
        }

        for repeat in 0..PROTECT_OVERHEAD_REPEATS {
            let bias = (repeat as f64 + 1.0) * 0.001;
            let temp = Rf_protect(Rf_allocVector(SEXPTYPE::REALSXP, data.len() as _));
            totals[1] += fill_sum_alloc(temp, data, bias);
            Rf_unprotect(1);
        }

        for repeat in 0..PROTECT_OVERHEAD_REPEATS {
            let bias = (repeat as f64 + 1.0) * 0.001;
            let temp = Rf_protect(Rf_allocVector(SEXPTYPE::REALSXP, data.len() as _));
            totals[2] += fill_sum_alloc(temp, data, bias);
        }
        Rf_unprotect(PROTECT_OVERHEAD_REPEATS as _);

        for repeat in 0..PROTECT_OVERHEAD_REPEATS {
            let bias = (repeat as f64 + 1.0) * 0.001;
            let temp = Rf_allocVector(SEXPTYPE::REALSXP, data.len() as _);
            extendr_ffi::R_PreserveObject(temp);
            totals[3] += fill_sum_alloc(temp, data, bias);
            extendr_ffi::R_ReleaseObject(temp);
        }

        let mut protect_index: c_int = 0;
        R_ProtectWithIndex(R_NilValue, &mut protect_index);
        for repeat in 0..PROTECT_OVERHEAD_REPEATS {
            let bias = (repeat as f64 + 1.0) * 0.001;
            let temp = Rf_allocVector(SEXPTYPE::REALSXP, data.len() as _);
            R_Reprotect(temp, protect_index);
            totals[4] += fill_sum_alloc(temp, data, bias);
        }
        Rf_unprotect(1);

        totals
    }
}

unsafe fn make_named_real_result(values: &[f64; 5]) -> SEXP {
    let result = Rf_protect(Rf_allocVector(SEXPTYPE::REALSXP, 5));
    let names = Rf_protect(Rf_allocVector(SEXPTYPE::STRSXP, 5));
    std::slice::from_raw_parts_mut(REAL(result), 5).copy_from_slice(values);

    SET_STRING_ELT(names, 0, Rf_mkChar(c"unsafe".as_ptr()));
    SET_STRING_ELT(names, 1, Rf_mkChar(c"manual".as_ptr()));
    SET_STRING_ELT(names, 2, Rf_mkChar(c"batch".as_ptr()));
    SET_STRING_ELT(names, 3, Rf_mkChar(c"preserve".as_ptr()));
    SET_STRING_ELT(names, 4, Rf_mkChar(c"reprotect".as_ptr()));
    Rf_setAttrib(result, R_NamesSymbol, names);
    Rf_unprotect(1);
    Rf_unprotect(1);
    result
}

#[extendr]
fn bench_fib(n: i32) -> Robj {
    scalar_integer(fib_i32(n))
}

#[extendr]
fn bench_vectorsum(vec: Robj) -> Robj {
    scalar_real(sum_real_slice(unsafe { real_slice(vec.get()) }))
}

#[extendr]
fn bench_transpose(mat: Robj) -> Robj {
    let mat_sexp = unsafe { mat.get() };
    let nr = unsafe { Rf_nrows(mat_sexp) as usize };
    let nc = unsafe { Rf_ncols(mat_sexp) as usize };
    let data = unsafe { real_slice(mat_sexp) };

    unsafe {
        let result = Rf_protect(Rf_allocMatrix(SEXPTYPE::REALSXP, nc as _, nr as _));
        let rp = std::slice::from_raw_parts_mut(REAL(result), nr * nc);
        for i in 0..nr {
            for j in 0..nc {
                rp[j * nr + i] = data[i * nc + j];
            }
        }
        Rf_unprotect(1);
        Robj::from_sexp(result)
    }
}

#[extendr]
fn bench_strings(vec: Robj, sep: String) -> Robj {
    let vec_sexp = unsafe { vec.get() };
    let n = unsafe { Rf_xlength(vec_sexp) as usize };
    let mut out = String::new();

    for i in 0..n {
        let elt = unsafe { STRING_ELT(vec_sexp, i as _) };
        if elt != unsafe { R_NaString } {
            let s = unsafe { CStr::from_ptr(R_CHAR(elt)) };
            out.push_str(s.to_str().unwrap_or_default());
        }
        out.push_str(&sep);
    }

    out.into()
}

#[extendr]
fn bench_dataframe(df: Robj) -> Robj {
    let df_sexp = unsafe { df.get() };
    let names = unsafe { Rf_getAttrib(df_sexp, R_NamesSymbol) };
    let ncols = unsafe { Rf_xlength(df_sexp) as usize };

    let mut x_sexp = unsafe { extendr_ffi::R_NilValue };
    let mut y_sexp = unsafe { extendr_ffi::R_NilValue };
    let mut grp_sexp = unsafe { extendr_ffi::R_NilValue };

    for i in 0..ncols {
        let name = unsafe { STRING_ELT(names, i as _) };
        let label = unsafe { CStr::from_ptr(R_CHAR(name)) }.to_str().unwrap_or_default();
        match label {
            "x" => x_sexp = unsafe { VECTOR_ELT(df_sexp, i as _) },
            "y" => y_sexp = unsafe { VECTOR_ELT(df_sexp, i as _) },
            "grp" => grp_sexp = unsafe { VECTOR_ELT(df_sexp, i as _) },
            _ => {}
        }
    }

    let x = unsafe { real_slice(x_sexp) };
    let y = unsafe { real_slice(y_sexp) };
    let grp = unsafe { int_slice(grp_sexp) };

    let mut max_grp = 0_i32;
    for i in 0..x.len() {
        if x[i] > 0.0 && grp[i] > max_grp {
            max_grp = grp[i];
        }
    }

    let mut sums = vec![0.0_f64; max_grp as usize];
    for i in 0..x.len() {
        if x[i] > 0.0 {
            sums[(grp[i] - 1) as usize] += x[i] / y[i];
        }
    }

    unsafe {
        let grp_out = Rf_protect(Rf_allocVector(SEXPTYPE::INTSXP, max_grp as _));
        let sum_out = Rf_protect(Rf_allocVector(SEXPTYPE::REALSXP, max_grp as _));
        let grp_slice = std::slice::from_raw_parts_mut(INTEGER(grp_out), max_grp as usize);
        let sum_slice = std::slice::from_raw_parts_mut(REAL(sum_out), max_grp as usize);
        for i in 0..max_grp as usize {
            grp_slice[i] = (i + 1) as i32;
            sum_slice[i] = sums[i];
        }

        let col_names = Rf_protect(Rf_allocVector(SEXPTYPE::STRSXP, 2));
        SET_STRING_ELT(col_names, 0, Rf_mkChar(c"grp".as_ptr()));
        SET_STRING_ELT(col_names, 1, Rf_mkChar(c"z_sum".as_ptr()));

        let result = Rf_protect(Rf_allocVector(SEXPTYPE::VECSXP, 2));
        SET_VECTOR_ELT(result, 0, grp_out);
        SET_VECTOR_ELT(result, 1, sum_out);
        Rf_setAttrib(result, R_NamesSymbol, col_names);

        let cls = Rf_protect(Rf_allocVector(SEXPTYPE::STRSXP, 1));
        SET_STRING_ELT(cls, 0, Rf_mkChar(c"data.frame".as_ptr()));
        Rf_setAttrib(result, R_ClassSymbol, cls);

        let rn = Rf_protect(Rf_allocVector(SEXPTYPE::INTSXP, 2));
        let row_names = std::slice::from_raw_parts_mut(INTEGER(rn), 2);
        row_names[0] = R_NaInt;
        row_names[1] = -(max_grp as i32);
        Rf_setAttrib(result, R_RowNamesSymbol, rn);

        Rf_unprotect(6);
        Robj::from_sexp(result)
    }
}

#[extendr]
fn bench_na_prop(vec: Robj) -> Robj {
    scalar_real(na_mean_slice(unsafe { real_slice(vec.get()) }))
}

#[extendr]
fn bench_parallel(vec: Robj) -> Robj {
    let data = unsafe { real_slice(vec.get()) };
    let nthreads = 4_usize;
    let chunk_size = data.len() / nthreads;

    let total = thread::scope(|scope| {
        let mut handles = Vec::with_capacity(nthreads);
        for index in 0..nthreads {
            let start = index * chunk_size;
            let end = if index + 1 == nthreads { data.len() } else { (index + 1) * chunk_size };
            let chunk = &data[start..end];
            handles.push(scope.spawn(move || chunk.iter().copied().sum::<f64>()));
        }
        handles.into_iter().map(|handle| handle.join().unwrap()).sum::<f64>()
    });

    scalar_real(total)
}

#[extendr]
fn bench_protect_stress(n: i32) -> Robj {
    unsafe {
        for _ in 0..n {
            Rf_protect(Rf_allocVector(SEXPTYPE::REALSXP, 1));
        }
        Rf_unprotect(n);
    }
    scalar_integer(0)
}

#[extendr]
fn bench_blas_matmul(a: Robj, b: Robj) -> Robj {
    let a_sexp = unsafe { a.get() };
    let b_sexp = unsafe { b.get() };
    let n = unsafe { Rf_nrows(a_sexp) as c_int };
    let m = unsafe { Rf_ncols(b_sexp) as c_int };
    let k = unsafe { Rf_ncols(a_sexp) as c_int };

    unsafe {
        let result = Rf_protect(Rf_allocMatrix(SEXPTYPE::REALSXP, n as _, m as _));
        let alpha = 1.0;
        let beta = 0.0;
        let notrans = b'N' as c_char;

        dgemm_(&notrans, &notrans, &n, &m, &k, &alpha, REAL(a_sexp), &n, REAL(b_sexp), &k, &beta, REAL(result), &n, 1, 1);

        Rf_unprotect(1);
        Robj::from_sexp(result)
    }
}

#[extendr]
fn bench_crossprod(x: Robj) -> Robj {
    let x_sexp = unsafe { x.get() };
    let nr = unsafe { Rf_nrows(x_sexp) as c_int };
    let nc = unsafe { Rf_ncols(x_sexp) as c_int };

    unsafe {
        let result = Rf_protect(Rf_allocMatrix(SEXPTYPE::REALSXP, nc as _, nc as _));
        let rp = std::slice::from_raw_parts_mut(REAL(result), (nc * nc) as usize);
        let alpha = 1.0;
        let beta = 0.0;
        let uplo = b'U' as c_char;
        let trans = b'T' as c_char;

        dsyrk_(&uplo, &trans, &nc, &nr, &alpha, REAL(x_sexp), &nr, &beta, rp.as_mut_ptr(), &nc, 1, 1);

        for i in 0..nc as usize {
            for j in 0..i {
                rp[i * nc as usize + j] = rp[j * nc as usize + i];
            }
        }

        Rf_unprotect(1);
        Robj::from_sexp(result)
    }
}

#[extendr]
fn bench_cholesky(a: Robj) -> Robj {
    let a_sexp = unsafe { a.get() };
    let n = unsafe { Rf_nrows(a_sexp) as c_int };
    let src = unsafe { real_slice(a_sexp) };

    unsafe {
        let result = Rf_protect(Rf_allocMatrix(SEXPTYPE::REALSXP, n as _, n as _));
        let rp = std::slice::from_raw_parts_mut(REAL(result), (n * n) as usize);
        rp.copy_from_slice(src);

        let uplo = b'U' as c_char;
        let mut info = 0;
        dpotrf_(&uplo, &n, rp.as_mut_ptr(), &n, &mut info, 1);

        for col in 0..n as usize {
            for row in (col + 1)..n as usize {
                rp[col * n as usize + row] = 0.0;
            }
        }

        Rf_unprotect(1);
        Robj::from_sexp(result)
    }
}

#[extendr]
fn bench_lm(x: Robj, y: Robj) -> Robj {
    let x_sexp = unsafe { x.get() };
    let y_sexp = unsafe { y.get() };
    let n = unsafe { Rf_nrows(x_sexp) as c_int };
    let p = unsafe { Rf_ncols(x_sexp) as c_int };
    let x_data = unsafe { REAL(x_sexp) };
    let y_data = unsafe { REAL(y_sexp) };
    let mut xtx = vec![0.0_f64; (p * p) as usize];
    let mut xty = vec![0.0_f64; p as usize];
    let alpha = 1.0;
    let beta = 0.0;
    let notrans = b'N' as c_char;
    let trans = b'T' as c_char;
    let one = 1 as c_int;

    unsafe {
        dgemm_(&trans, &notrans, &p, &p, &n, &alpha, x_data, &n, x_data, &n, &beta, xtx.as_mut_ptr(), &p, 1, 1);
        dgemm_(&trans, &notrans, &p, &one, &n, &alpha, x_data, &n, y_data, &n, &beta, xty.as_mut_ptr(), &p, 1, 1);

        let uplo = b'U' as c_char;
        let mut info = 0;
        dpotrf_(&uplo, &p, xtx.as_mut_ptr(), &p, &mut info, 1);

        let side = b'L' as c_char;
        let diag = b'N' as c_char;
        dtrsm_(&side, &uplo, &trans, &diag, &p, &one, &alpha, xtx.as_ptr(), &p, xty.as_mut_ptr(), &p, 1, 1, 1, 1);
        dtrsm_(&side, &uplo, &notrans, &diag, &p, &one, &alpha, xtx.as_ptr(), &p, xty.as_mut_ptr(), &p, 1, 1, 1, 1);

        let result = Rf_protect(Rf_allocVector(SEXPTYPE::REALSXP, p as _));
        std::slice::from_raw_parts_mut(REAL(result), p as usize).copy_from_slice(&xty);
        Rf_unprotect(1);
        Robj::from_sexp(result)
    }
}

#[extendr]
fn bench_rowsums(mat: Robj) -> Robj {
    let mat_sexp = unsafe { mat.get() };
    let nr = unsafe { Rf_nrows(mat_sexp) as usize };
    let nc = unsafe { Rf_ncols(mat_sexp) as usize };
    let data = unsafe { real_slice(mat_sexp) };
    let mut sums = vec![0.0_f64; nr];

    for j in 0..nc {
        for i in 0..nr {
            sums[i] += data[i + j * nr];
        }
    }

    sums.into()
}

#[extendr]
fn bench_elem_ops(vec: Robj) -> Robj {
    let src = unsafe { real_slice(vec.get()) };
    let n = src.len();

    unsafe {
        let result = Rf_protect(Rf_allocMatrix(SEXPTYPE::REALSXP, n as _, 4));
        let rp = std::slice::from_raw_parts_mut(REAL(result), n * 4);
        for i in 0..n {
            let value = src[i];
            rp[i] = value.abs();
            rp[i + n] = if value > 0.0 { value.ln() } else { 0.0 };
            rp[i + 2 * n] = value.exp();
            rp[i + 3 * n] = if value >= 0.0 { value.sqrt() } else { 0.0 };
        }
        Rf_unprotect(1);
        Robj::from_sexp(result)
    }
}

#[extendr]
fn bench_rowcol_means(mat: Robj) -> Robj {
    let mat_sexp = unsafe { mat.get() };
    let nr = unsafe { Rf_nrows(mat_sexp) as usize };
    let nc = unsafe { Rf_ncols(mat_sexp) as usize };
    let data = unsafe { real_slice(mat_sexp) };
    let mut row_means = vec![0.0_f64; nr];
    let mut col_sums = vec![0.0_f64; nc];

    for i in 0..nr {
        let mut sum = 0.0;
        for j in 0..nc {
            sum += data[i + j * nr];
        }
        row_means[i] = sum / nc as f64;
    }

    for j in 0..nc {
        let mut sum = 0.0;
        for i in 0..nr {
            sum += data[i + j * nr];
        }
        col_sums[j] = sum;
    }

    unsafe {
        let row_out = Rf_protect(Rf_allocVector(SEXPTYPE::REALSXP, nr as _));
        let col_out = Rf_protect(Rf_allocVector(SEXPTYPE::REALSXP, nc as _));
        std::slice::from_raw_parts_mut(REAL(row_out), nr).copy_from_slice(&row_means);
        std::slice::from_raw_parts_mut(REAL(col_out), nc).copy_from_slice(&col_sums);

        let result = Rf_protect(Rf_allocVector(SEXPTYPE::VECSXP, 2));
        SET_VECTOR_ELT(result, 0, row_out);
        SET_VECTOR_ELT(result, 1, col_out);

        Rf_unprotect(3);
        Robj::from_sexp(result)
    }
}

#[extendr]
fn bench_broadcast(vec: Robj, scalar: f64) -> Robj {
    let vec_sexp = unsafe { vec.get() };
    let n = unsafe { Rf_xlength(vec_sexp) as usize };
    let src = unsafe { real_slice(vec_sexp) };
    unsafe {
        let result = Rf_protect(Rf_allocVector(SEXPTYPE::REALSXP, n as _));
        let rp = std::slice::from_raw_parts_mut(REAL(result), n);
        for i in 0..n {
            rp[i] = src[i] + scalar;
        }
        Rf_unprotect(1);
        Robj::from_sexp(result)
    }
}

#[extendr]
fn bench_sort(vec: Robj) -> Robj {
    let mut out = unsafe { real_slice(vec.get()) }.to_vec();
    radix_sort_f64(&mut out);
    out.into()
}

#[extendr]
fn bench_cumsum(vec: Robj) -> Robj {
    let vec_sexp = unsafe { vec.get() };
    let n = unsafe { Rf_xlength(vec_sexp) as usize };
    let src = unsafe { real_slice(vec_sexp) };
    unsafe {
        let result = Rf_protect(Rf_allocVector(SEXPTYPE::REALSXP, n as _));
        let rp = std::slice::from_raw_parts_mut(REAL(result), n);
        let mut total = 0.0;
        for i in 0..n {
            total += src[i];
            rp[i] = total;
        }
        Rf_unprotect(1);
        Robj::from_sexp(result)
    }
}

#[extendr]
fn bench_rnorm(n: i32) -> Robj {
    unsafe {
        GetRNGstate();
        let result = Rf_protect(Rf_allocVector(SEXPTYPE::REALSXP, n as _));
        let rp = std::slice::from_raw_parts_mut(REAL(result), n as usize);
        for value in rp.iter_mut() {
            *value = norm_rand();
        }
        PutRNGstate();
        Rf_unprotect(1);
        Robj::from_sexp(result)
    }
}

#[extendr]
fn bench_string_nchar(vec: Robj) -> Robj {
    let vec_sexp = unsafe { vec.get() };
    let n = unsafe { Rf_xlength(vec_sexp) as usize };
    let mut total = 0_i32;

    for i in 0..n {
        let elt = unsafe { STRING_ELT(vec_sexp, i as _) };
        if elt != unsafe { R_NaString } {
            total += unsafe { CStr::from_ptr(R_CHAR(elt)).to_bytes().len() as i32 };
        }
    }

    scalar_integer(total)
}

#[extendr]
fn bench_which_na(vec: Robj) -> Robj {
    unsafe { real_slice(vec.get()) }
        .iter()
        .enumerate()
        .filter_map(|(index, value)| if value.is_nan() { Some((index + 1) as i32) } else { None })
        .collect::<Vec<i32>>()
        .into()
}

#[extendr]
fn bench_altrep_sum(vec: Robj) -> Robj {
    let sexp = unsafe { vec.get() };
    unsafe {
        let sum_sym = Rf_install(c"sum".as_ptr());
        let call = Rf_protect(Rf_lang2(sum_sym, sexp));
        let result = Rf_eval(call, R_GlobalEnv);
        Rf_unprotect(1);
        Robj::from_sexp(result)
    }
}

#[extendr]
fn bench_altrep_read(vec: Robj) -> Robj {
    let sexp = unsafe { vec.get() };
    let n = unsafe { Rf_xlength(sexp) };
    let mut first = 0_i32;
    let mut last = 0_i32;
    unsafe {
        INTEGER_GET_REGION(sexp, 0, 1, &mut first);
        INTEGER_GET_REGION(sexp, n - 1, 1, &mut last);
    }
    vec![first, last].into()
}

#[extendr]
fn bench_altrep_create(n: i32) -> Robj {
    if n < 0 || n as usize > ALTREP_CREATE_MAX_LEN {
        return ().into();
    }

    unsafe {
        extendr_altrep_create_ensure_class();
        extendr_altrep_create_ensure_backing(n as usize);
        let len = Rf_protect(Rf_ScalarInteger(n));
        let result = R_new_altrep(EXTENDR_ALTREP_CREATE_CLASS, R_NilValue, len);
        Rf_unprotect(1);
        Robj::from_sexp(result)
    }
}

#[extendr]
fn bench_owned_altrep_real_sum(n: i32) -> Robj {
    if n < 0 || n as usize > ALTREP_CREATE_MAX_LEN {
        return ().into();
    }

    unsafe {
        extendr_altrep_create_ensure_class();
        extendr_altrep_create_ensure_backing(n as usize);
        let len = Rf_protect(Rf_ScalarInteger(n));
        let vec = Rf_protect(R_new_altrep(EXTENDR_ALTREP_CREATE_CLASS, R_NilValue, len));
        let data = REAL(vec);
        let mut total = 0.0;
        for index in 0..n as usize {
            total += *data.add(index);
        }
        Rf_unprotect(2);
        scalar_real(total)
    }
}

#[extendr]
fn bench_owned_altrep_int_sum(n: i32) -> Robj {
    if n < 0 || n as usize > ALTREP_CREATE_MAX_LEN {
        return ().into();
    }

    unsafe {
        extendr_altint_create_ensure_class();
        extendr_altint_create_ensure_backing(n as usize);
        let len = Rf_protect(Rf_ScalarInteger(n));
        let vec = Rf_protect(R_new_altrep(EXTENDR_ALTINT_CREATE_CLASS, R_NilValue, len));
        let data = INTEGER(vec);
        let mut total = 0.0;
        for index in 0..n as usize {
            total += *data.add(index) as f64;
        }
        Rf_unprotect(2);
        scalar_real(total)
    }
}

#[extendr]
fn bench_owned_altrep_logical_sum(n: i32) -> Robj {
    if n < 0 || n as usize > ALTREP_CREATE_MAX_LEN {
        return ().into();
    }

    unsafe {
        extendr_altlogical_create_ensure_class();
        extendr_altlogical_create_ensure_backing(n as usize);
        let len = Rf_protect(Rf_ScalarInteger(n));
        let vec = Rf_protect(R_new_altrep(EXTENDR_ALTLOGICAL_CREATE_CLASS, R_NilValue, len));
        let data = LOGICAL(vec);
        let mut total = 0.0;
        for index in 0..n as usize {
            if *data.add(index) != 0 {
                total += 1.0;
            }
        }
        Rf_unprotect(2);
        scalar_real(total)
    }
}

#[extendr]
fn bench_comptime_dispatch(inputs: Robj) -> Robj {
    let inputs_sexp = unsafe { inputs.get() };
    let n_inputs = unsafe { Rf_xlength(inputs_sexp) as usize };
    let mut total = 0.0;

    for _ in 0..COMPTIME_DISPATCH_REPEATS {
        for index in 0..n_inputs {
            total += dispatch_sum_atomic(unsafe { VECTOR_ELT(inputs_sexp, index as _) });
        }
    }

    scalar_real(total)
}

#[extendr]
fn bench_struct_convert(input: Robj) -> Robj {
    let input_sexp = unsafe { input.get() };
    let payload = unsafe {
        let weights_sexp = find_named(input_sexp, "weights");
        let indices_sexp = find_named(input_sexp, "indices");

        StructConvertPayload {
            id: *INTEGER(find_named(input_sexp, "id")),
            count: *INTEGER(find_named(input_sexp, "count")),
            level: *INTEGER(find_named(input_sexp, "level")),
            flag: *INTEGER(find_named(input_sexp, "flag")) != 0,
            enabled: *INTEGER(find_named(input_sexp, "enabled")) != 0,
            ratio: *REAL(find_named(input_sexp, "ratio")),
            offset: *REAL(find_named(input_sexp, "offset")),
            scale: *REAL(find_named(input_sexp, "scale")),
            weights: real_slice(weights_sexp).to_vec(),
            indices: int_slice(indices_sexp).to_vec(),
        }
    };

    unsafe {
        let result = Rf_protect(Rf_allocVector(SEXPTYPE::VECSXP, 10));
        let names = Rf_protect(Rf_allocVector(SEXPTYPE::STRSXP, 10));
        let weights_out = Rf_protect(Rf_allocVector(SEXPTYPE::REALSXP, payload.weights.len() as _));
        let indices_out = Rf_protect(Rf_allocVector(SEXPTYPE::INTSXP, payload.indices.len() as _));
        let id = Rf_protect(Rf_ScalarInteger(payload.id));
        let count = Rf_protect(Rf_ScalarInteger(payload.count));
        let level = Rf_protect(Rf_ScalarInteger(payload.level));
        let flag = Rf_protect(Robj::from(payload.flag).get());
        let enabled = Rf_protect(Robj::from(payload.enabled).get());
        let ratio = Rf_protect(scalar_real(payload.ratio).get());
        let offset = Rf_protect(scalar_real(payload.offset).get());
        let scale = Rf_protect(scalar_real(payload.scale).get());

        std::slice::from_raw_parts_mut(REAL(weights_out), payload.weights.len())
            .copy_from_slice(&payload.weights);
        std::slice::from_raw_parts_mut(INTEGER(indices_out), payload.indices.len())
            .copy_from_slice(&payload.indices);

        SET_VECTOR_ELT(result, 0, id);
        SET_VECTOR_ELT(result, 1, count);
        SET_VECTOR_ELT(result, 2, level);
        SET_VECTOR_ELT(result, 3, flag);
        SET_VECTOR_ELT(result, 4, enabled);
        SET_VECTOR_ELT(result, 5, ratio);
        SET_VECTOR_ELT(result, 6, offset);
        SET_VECTOR_ELT(result, 7, scale);
        SET_VECTOR_ELT(result, 8, weights_out);
        SET_VECTOR_ELT(result, 9, indices_out);

        SET_STRING_ELT(names, 0, Rf_mkChar(c"id".as_ptr()));
        SET_STRING_ELT(names, 1, Rf_mkChar(c"count".as_ptr()));
        SET_STRING_ELT(names, 2, Rf_mkChar(c"level".as_ptr()));
        SET_STRING_ELT(names, 3, Rf_mkChar(c"flag".as_ptr()));
        SET_STRING_ELT(names, 4, Rf_mkChar(c"enabled".as_ptr()));
        SET_STRING_ELT(names, 5, Rf_mkChar(c"ratio".as_ptr()));
        SET_STRING_ELT(names, 6, Rf_mkChar(c"offset".as_ptr()));
        SET_STRING_ELT(names, 7, Rf_mkChar(c"scale".as_ptr()));
        SET_STRING_ELT(names, 8, Rf_mkChar(c"weights".as_ptr()));
        SET_STRING_ELT(names, 9, Rf_mkChar(c"indices".as_ptr()));
        Rf_setAttrib(result, R_NamesSymbol, names);

        Rf_unprotect(12);
        Robj::from_sexp(result)
    }
}

#[extendr]
fn bench_na_prop_vary(inputs: Robj) -> Robj {
    let inputs_sexp = unsafe { inputs.get() };
    let n_inputs = unsafe { Rf_xlength(inputs_sexp) as usize };

    unsafe {
        let result = Rf_protect(Rf_allocVector(SEXPTYPE::REALSXP, n_inputs as _));
        let out = std::slice::from_raw_parts_mut(REAL(result), n_inputs);

        for (index, slot) in out.iter_mut().enumerate() {
            let vec = VECTOR_ELT(inputs_sexp, index as _);
            *slot = na_mean_slice(real_slice(vec));
        }

        Rf_setAttrib(result, R_NamesSymbol, Rf_getAttrib(inputs_sexp, R_NamesSymbol));
        Rf_unprotect(1);
        Robj::from_sexp(result)
    }
}

#[extendr]
fn bench_scale_law(inputs: Robj) -> Robj {
    let inputs_sexp = unsafe { inputs.get() };
    let n_inputs = unsafe { Rf_xlength(inputs_sexp) as usize };

    unsafe {
        let result = Rf_protect(Rf_allocVector(SEXPTYPE::REALSXP, n_inputs as _));
        let out = std::slice::from_raw_parts_mut(REAL(result), n_inputs);

        for (index, slot) in out.iter_mut().enumerate() {
            let vec = VECTOR_ELT(inputs_sexp, index as _);
            *slot = sum_real_slice(real_slice(vec));
        }

        Rf_setAttrib(result, R_NamesSymbol, Rf_getAttrib(inputs_sexp, R_NamesSymbol));
        Rf_unprotect(1);
        Robj::from_sexp(result)
    }
}

#[extendr]
fn bench_arena_vs_rmalloc(vec: Robj) -> Robj {
    scalar_real(allocation_bench_total(unsafe { real_slice(vec.get()) }))
}

#[unsafe(no_mangle)]
pub extern "C" fn bench_arena_vs_rmalloc_manual__ffi(c_arg__vec: SEXP) -> SEXP {
    unsafe { scalar_real(allocation_bench_total(real_slice(c_arg__vec))).get() }
}

#[unsafe(no_mangle)]
pub extern "C" fn bench_prot_overhead_manual__ffi(c_arg__vec: SEXP) -> SEXP {
    unsafe { make_named_real_result(&protect_overhead_results(real_slice(c_arg__vec))) }
}

#[unsafe(no_mangle)]
pub extern "C" fn bench_longjmp_safety_manual__ffi(c_arg__vec: SEXP) -> SEXP {
    unsafe { make_named_longjmp_result(&longjmp_safety_results(real_slice(c_arg__vec))) }
}

#[unsafe(no_mangle)]
pub extern "C" fn bench_translate_c_cost_manual__ffi(c_arg__vec: SEXP) -> SEXP {
    unsafe { make_named_math_result(&translate_c_cost_results(real_slice(c_arg__vec))) }
}

#[unsafe(no_mangle)]
pub extern "C" fn bench_string_variants_manual__ffi(c_arg__vec: SEXP) -> SEXP {
    unsafe { string_variants_result(c_arg__vec) }
}

#[unsafe(no_mangle)]
pub extern "C" fn bench_parallel_scaling_manual__ffi(c_arg__vec: SEXP) -> SEXP {
    unsafe { make_named_parallel_scaling_result(&parallel_scaling_results(real_slice(c_arg__vec))) }
}

#[unsafe(no_mangle)]
pub extern "C" fn bench_memory_bandwidth_manual__ffi(c_arg__vec: SEXP) -> SEXP {
    unsafe { make_named_memory_bandwidth_result(&memory_bandwidth_results(real_slice(c_arg__vec))) }
}

extendr_module! {
    mod extendr_benchmarks;
    fn bench_fib;
    fn bench_vectorsum;
    fn bench_transpose;
    fn bench_strings;
    fn bench_dataframe;
    fn bench_na_prop;
    fn bench_parallel;
    fn bench_protect_stress;
    fn bench_blas_matmul;
    fn bench_crossprod;
    fn bench_cholesky;
    fn bench_lm;
    fn bench_rowsums;
    fn bench_elem_ops;
    fn bench_rowcol_means;
    fn bench_broadcast;
    fn bench_sort;
    fn bench_cumsum;
    fn bench_rnorm;
    fn bench_string_nchar;
    fn bench_which_na;
    fn bench_altrep_sum;
    fn bench_altrep_read;
    fn bench_altrep_create;
    fn bench_owned_altrep_real_sum;
    fn bench_owned_altrep_int_sum;
    fn bench_owned_altrep_logical_sum;
    fn bench_comptime_dispatch;
    fn bench_struct_convert;
    fn bench_na_prop_vary;
    fn bench_scale_law;
    fn bench_arena_vs_rmalloc;
}