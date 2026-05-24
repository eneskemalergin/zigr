use savvy::{
    IntegerSexp, NotAvailableValue, OwnedListSexp, OwnedRealSexp, RealSexp, Sexp, StringSexp,
    savvy,
};
use savvy_ffi::{
    R_GlobalEnv, R_NaInt, R_NilValue, Rf_eval, Rf_install,
};
use std::ffi::{CStr, CString};
use std::ffi::c_void;
use std::os::raw::{c_char, c_int};
use std::thread;

#[repr(C)]
#[derive(Copy, Clone)]
struct RAltRepClass {
    ptr: savvy_ffi::SEXP,
}

const ALTREP_CREATE_MAX_LEN: usize = 1_000_000;
const COMPTIME_DISPATCH_REPEATS: usize = 256;
const ALLOCATION_REPEATS: usize = 100;
const PROTECT_OVERHEAD_REPEATS: usize = 4096;
const LONGJMP_SAFETY_REPEATS: usize = 512;
const TRANSLATE_C_COST_REPEATS: usize = 512;

static mut SAVVY_ALTREP_CREATE_CLASS: RAltRepClass = RAltRepClass {
    ptr: std::ptr::null_mut(),
};
static mut SAVVY_ALTREP_CREATE_CLASS_READY: bool = false;
static mut SAVVY_ALTREP_BACKING: [f64; ALTREP_CREATE_MAX_LEN] = [0.0; ALTREP_CREATE_MAX_LEN];
static mut SAVVY_ALTREP_BACKING_INIT: usize = 0;
static mut SAVVY_ALTINT_CREATE_CLASS: RAltRepClass = RAltRepClass {
    ptr: std::ptr::null_mut(),
};
static mut SAVVY_ALTINT_CREATE_CLASS_READY: bool = false;
static mut SAVVY_ALTINT_BACKING: [i32; ALTREP_CREATE_MAX_LEN] = [0; ALTREP_CREATE_MAX_LEN];
static mut SAVVY_ALTINT_BACKING_INIT: usize = 0;
static mut SAVVY_ALTLOGICAL_CREATE_CLASS: RAltRepClass = RAltRepClass {
    ptr: std::ptr::null_mut(),
};
static mut SAVVY_ALTLOGICAL_CREATE_CLASS_READY: bool = false;
static mut SAVVY_ALTLOGICAL_BACKING: [i32; ALTREP_CREATE_MAX_LEN] = [0; ALTREP_CREATE_MAX_LEN];
static mut SAVVY_ALTLOGICAL_BACKING_INIT: usize = 0;

unsafe extern "C" {
    fn GetRNGstate();

    fn PutRNGstate();

    fn INTEGER_GET_REGION(x: savvy_ffi::SEXP, i: isize, n: isize, buf: *mut i32) -> isize;

    fn Rf_lang2(fun: savvy_ffi::SEXP, arg: savvy_ffi::SEXP) -> savvy_ffi::SEXP;

    fn Rf_mkChar(name: *const c_char) -> savvy_ffi::SEXP;

    fn R_ProtectWithIndex(value: savvy_ffi::SEXP, index: *mut c_int);

    fn R_Reprotect(value: savvy_ffi::SEXP, index: c_int);

    fn R_ReleaseObject(value: savvy_ffi::SEXP);

    fn R_tryEvalSilent(expr: savvy_ffi::SEXP, env: savvy_ffi::SEXP, err: *mut c_int) -> savvy_ffi::SEXP;

    fn R_MakeUnwindCont() -> savvy_ffi::SEXP;

    fn R_UnwindProtect(
        fun: unsafe extern "C" fn(*mut c_void) -> savvy_ffi::SEXP,
        data: *mut c_void,
        cleanfun: unsafe extern "C" fn(*mut c_void, c_int),
        cleandata: *mut c_void,
        cont: savvy_ffi::SEXP,
    ) -> savvy_ffi::SEXP;

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

    fn R_new_altrep(
        aclass: RAltRepClass,
        data1: savvy_ffi::SEXP,
        data2: savvy_ffi::SEXP,
    ) -> savvy_ffi::SEXP;

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

    fn R_set_altrep_Length_method(
        cls: RAltRepClass,
        fun: unsafe extern "C" fn(savvy_ffi::SEXP) -> isize,
    );

    fn R_set_altvec_Dataptr_method(
        cls: RAltRepClass,
        fun: unsafe extern "C" fn(savvy_ffi::SEXP, c_int) -> *mut c_void,
    );

    fn R_set_altrep_Duplicate_method(
        cls: RAltRepClass,
        fun: unsafe extern "C" fn(savvy_ffi::SEXP, c_int) -> savvy_ffi::SEXP,
    );

    fn R_set_altreal_Elt_method(
        cls: RAltRepClass,
        fun: unsafe extern "C" fn(savvy_ffi::SEXP, isize) -> f64,
    );

    fn R_set_altinteger_Elt_method(
        cls: RAltRepClass,
        fun: unsafe extern "C" fn(savvy_ffi::SEXP, isize) -> i32,
    );

    fn R_set_altlogical_Elt_method(
        cls: RAltRepClass,
        fun: unsafe extern "C" fn(savvy_ffi::SEXP, isize) -> i32,
    );

    fn R_set_altreal_Get_region_method(
        cls: RAltRepClass,
        fun: unsafe extern "C" fn(savvy_ffi::SEXP, isize, isize, *mut f64) -> isize,
    );

    fn R_set_altinteger_Get_region_method(
        cls: RAltRepClass,
        fun: unsafe extern "C" fn(savvy_ffi::SEXP, isize, isize, *mut i32) -> isize,
    );

    fn R_set_altlogical_Get_region_method(
        cls: RAltRepClass,
        fun: unsafe extern "C" fn(savvy_ffi::SEXP, isize, isize, *mut i32) -> isize,
    );

    fn R_altrep_data2(x: savvy_ffi::SEXP) -> savvy_ffi::SEXP;
}

unsafe extern "C" fn savvy_altrep_create_length(x: savvy_ffi::SEXP) -> isize {
    *savvy_ffi::INTEGER(R_altrep_data2(x)) as isize
}

unsafe extern "C" fn savvy_altrep_create_elt(_x: savvy_ffi::SEXP, i: isize) -> f64 {
    SAVVY_ALTREP_BACKING[i as usize]
}

unsafe extern "C" fn savvy_altrep_create_dataptr(
    _x: savvy_ffi::SEXP,
    _writable: c_int,
) -> *mut c_void {
    std::ptr::addr_of_mut!(SAVVY_ALTREP_BACKING).cast::<f64>() as *mut c_void
}

unsafe extern "C" fn savvy_altrep_create_get_region(
    x: savvy_ffi::SEXP,
    i: isize,
    n: isize,
    buf: *mut f64,
) -> isize {
    let len = savvy_altrep_create_length(x);
    if i >= len {
        return 0;
    }

    let available = len - i;
    let count = n.min(available);
    std::ptr::copy_nonoverlapping(
        std::ptr::addr_of!(SAVVY_ALTREP_BACKING).cast::<f64>().add(i as usize),
        buf,
        count as usize,
    );
    count
}

unsafe extern "C" fn savvy_altrep_create_duplicate(
    x: savvy_ffi::SEXP,
    _deep: c_int,
) -> savvy_ffi::SEXP {
    let len = savvy_altrep_create_length(x);
    let out = savvy_ffi::Rf_protect(savvy_ffi::Rf_allocVector(savvy_ffi::REALSXP, len));
    std::ptr::copy_nonoverlapping(
        std::ptr::addr_of!(SAVVY_ALTREP_BACKING).cast::<f64>(),
        savvy_ffi::REAL(out),
        len as usize,
    );
    savvy_ffi::Rf_unprotect(1);
    out
}

unsafe fn savvy_altrep_create_ensure_class() {
    if SAVVY_ALTREP_CREATE_CLASS_READY {
        return;
    }

    SAVVY_ALTREP_CREATE_CLASS = R_make_altreal_class(
        c"bench_altreal_create_savvy".as_ptr(),
        c"savvy_benchmarks".as_ptr(),
        std::ptr::null_mut(),
    );
    R_set_altrep_Length_method(SAVVY_ALTREP_CREATE_CLASS, savvy_altrep_create_length);
    R_set_altreal_Elt_method(SAVVY_ALTREP_CREATE_CLASS, savvy_altrep_create_elt);
    R_set_altvec_Dataptr_method(SAVVY_ALTREP_CREATE_CLASS, savvy_altrep_create_dataptr);
    R_set_altrep_Duplicate_method(SAVVY_ALTREP_CREATE_CLASS, savvy_altrep_create_duplicate);
    R_set_altreal_Get_region_method(SAVVY_ALTREP_CREATE_CLASS, savvy_altrep_create_get_region);
    SAVVY_ALTREP_CREATE_CLASS_READY = true;
}

unsafe fn savvy_altrep_create_ensure_backing(n: usize) {
    if SAVVY_ALTREP_BACKING_INIT >= n {
        return;
    }

    let base = std::ptr::addr_of_mut!(SAVVY_ALTREP_BACKING).cast::<f64>();
    for index in SAVVY_ALTREP_BACKING_INIT..n {
        *base.add(index) = (index + 1) as f64;
    }
    SAVVY_ALTREP_BACKING_INIT = n;
}

unsafe extern "C" fn savvy_altint_create_length(x: savvy_ffi::SEXP) -> isize {
    *savvy_ffi::INTEGER(R_altrep_data2(x)) as isize
}

unsafe extern "C" fn savvy_altint_create_elt(_x: savvy_ffi::SEXP, i: isize) -> i32 {
    SAVVY_ALTINT_BACKING[i as usize]
}

unsafe extern "C" fn savvy_altint_create_dataptr(
    _x: savvy_ffi::SEXP,
    _writable: c_int,
) -> *mut c_void {
    std::ptr::addr_of_mut!(SAVVY_ALTINT_BACKING).cast::<i32>() as *mut c_void
}

unsafe extern "C" fn savvy_altint_create_get_region(
    x: savvy_ffi::SEXP,
    i: isize,
    n: isize,
    buf: *mut i32,
) -> isize {
    let len = savvy_altint_create_length(x);
    if i >= len {
        return 0;
    }

    let available = len - i;
    let count = n.min(available);
    std::ptr::copy_nonoverlapping(
        std::ptr::addr_of!(SAVVY_ALTINT_BACKING).cast::<i32>().add(i as usize),
        buf,
        count as usize,
    );
    count
}

unsafe extern "C" fn savvy_altint_create_duplicate(
    x: savvy_ffi::SEXP,
    _deep: c_int,
) -> savvy_ffi::SEXP {
    let len = savvy_altint_create_length(x);
    let out = savvy_ffi::Rf_protect(savvy_ffi::Rf_allocVector(savvy_ffi::INTSXP, len));
    std::ptr::copy_nonoverlapping(
        std::ptr::addr_of!(SAVVY_ALTINT_BACKING).cast::<i32>(),
        savvy_ffi::INTEGER(out),
        len as usize,
    );
    savvy_ffi::Rf_unprotect(1);
    out
}

unsafe fn savvy_altint_create_ensure_class() {
    if SAVVY_ALTINT_CREATE_CLASS_READY {
        return;
    }

    SAVVY_ALTINT_CREATE_CLASS = R_make_altinteger_class(
        c"bench_altinteger_owned_sum_savvy".as_ptr(),
        c"savvy_benchmarks".as_ptr(),
        std::ptr::null_mut(),
    );
    R_set_altrep_Length_method(SAVVY_ALTINT_CREATE_CLASS, savvy_altint_create_length);
    R_set_altinteger_Elt_method(SAVVY_ALTINT_CREATE_CLASS, savvy_altint_create_elt);
    R_set_altvec_Dataptr_method(SAVVY_ALTINT_CREATE_CLASS, savvy_altint_create_dataptr);
    R_set_altrep_Duplicate_method(SAVVY_ALTINT_CREATE_CLASS, savvy_altint_create_duplicate);
    R_set_altinteger_Get_region_method(SAVVY_ALTINT_CREATE_CLASS, savvy_altint_create_get_region);
    SAVVY_ALTINT_CREATE_CLASS_READY = true;
}

unsafe fn savvy_altint_create_ensure_backing(n: usize) {
    if SAVVY_ALTINT_BACKING_INIT >= n {
        return;
    }

    let base = std::ptr::addr_of_mut!(SAVVY_ALTINT_BACKING).cast::<i32>();
    for index in SAVVY_ALTINT_BACKING_INIT..n {
        *base.add(index) = ((index % 1024) + 1) as i32;
    }
    SAVVY_ALTINT_BACKING_INIT = n;
}

unsafe extern "C" fn savvy_altlogical_create_length(x: savvy_ffi::SEXP) -> isize {
    *savvy_ffi::INTEGER(R_altrep_data2(x)) as isize
}

unsafe extern "C" fn savvy_altlogical_create_elt(_x: savvy_ffi::SEXP, i: isize) -> i32 {
    SAVVY_ALTLOGICAL_BACKING[i as usize]
}

unsafe extern "C" fn savvy_altlogical_create_dataptr(
    _x: savvy_ffi::SEXP,
    _writable: c_int,
) -> *mut c_void {
    std::ptr::addr_of_mut!(SAVVY_ALTLOGICAL_BACKING).cast::<i32>() as *mut c_void
}

unsafe extern "C" fn savvy_altlogical_create_get_region(
    x: savvy_ffi::SEXP,
    i: isize,
    n: isize,
    buf: *mut i32,
) -> isize {
    let len = savvy_altlogical_create_length(x);
    if i >= len {
        return 0;
    }

    let available = len - i;
    let count = n.min(available);
    std::ptr::copy_nonoverlapping(
        std::ptr::addr_of!(SAVVY_ALTLOGICAL_BACKING)
            .cast::<i32>()
            .add(i as usize),
        buf,
        count as usize,
    );
    count
}

unsafe extern "C" fn savvy_altlogical_create_duplicate(
    x: savvy_ffi::SEXP,
    _deep: c_int,
) -> savvy_ffi::SEXP {
    let len = savvy_altlogical_create_length(x);
    let out = savvy_ffi::Rf_protect(savvy_ffi::Rf_allocVector(savvy_ffi::LGLSXP, len));
    std::ptr::copy_nonoverlapping(
        std::ptr::addr_of!(SAVVY_ALTLOGICAL_BACKING).cast::<i32>(),
        savvy_ffi::LOGICAL(out),
        len as usize,
    );
    savvy_ffi::Rf_unprotect(1);
    out
}

unsafe fn savvy_altlogical_create_ensure_class() {
    if SAVVY_ALTLOGICAL_CREATE_CLASS_READY {
        return;
    }

    SAVVY_ALTLOGICAL_CREATE_CLASS = R_make_altlogical_class(
        c"bench_altlogical_owned_sum_savvy".as_ptr(),
        c"savvy_benchmarks".as_ptr(),
        std::ptr::null_mut(),
    );
    R_set_altrep_Length_method(SAVVY_ALTLOGICAL_CREATE_CLASS, savvy_altlogical_create_length);
    R_set_altlogical_Elt_method(SAVVY_ALTLOGICAL_CREATE_CLASS, savvy_altlogical_create_elt);
    R_set_altvec_Dataptr_method(
        SAVVY_ALTLOGICAL_CREATE_CLASS,
        savvy_altlogical_create_dataptr,
    );
    R_set_altrep_Duplicate_method(
        SAVVY_ALTLOGICAL_CREATE_CLASS,
        savvy_altlogical_create_duplicate,
    );
    R_set_altlogical_Get_region_method(
        SAVVY_ALTLOGICAL_CREATE_CLASS,
        savvy_altlogical_create_get_region,
    );
    SAVVY_ALTLOGICAL_CREATE_CLASS_READY = true;
}

unsafe fn savvy_altlogical_create_ensure_backing(n: usize) {
    if SAVVY_ALTLOGICAL_BACKING_INIT >= n {
        return;
    }

    let base = std::ptr::addr_of_mut!(SAVVY_ALTLOGICAL_BACKING).cast::<i32>();
    for index in SAVVY_ALTLOGICAL_BACKING_INIT..n {
        *base.add(index) = if (index & 1) == 0 { 1 } else { 0 };
    }
    SAVVY_ALTLOGICAL_BACKING_INIT = n;
}

fn dispatch_sum_atomic(sexp: savvy_ffi::SEXP) -> f64 {
    let n = unsafe { savvy_ffi::Rf_xlength(sexp) as usize };

    unsafe {
        match savvy_ffi::TYPEOF(sexp) as u32 {
            savvy_ffi::REALSXP => std::slice::from_raw_parts(savvy_ffi::REAL(sexp), n)
                .iter()
                .copied()
                .sum::<f64>(),
            savvy_ffi::INTSXP => std::slice::from_raw_parts(savvy_ffi::INTEGER(sexp), n)
                .iter()
                .map(|&value| value as f64)
                .sum::<f64>(),
            savvy_ffi::LGLSXP => std::slice::from_raw_parts(savvy_ffi::LOGICAL(sexp), n)
                .iter()
                .map(|&value| if value != 0 { 1.0 } else { 0.0 })
                .sum::<f64>(),
            _ => 0.0,
        }
    }
}

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

fn find_named(list_sexp: savvy_ffi::SEXP, key: &str) -> savvy::Result<savvy_ffi::SEXP> {
    let names = unsafe { savvy_ffi::Rf_getAttrib(list_sexp, savvy_ffi::R_NamesSymbol) };
    let n = unsafe { savvy_ffi::Rf_xlength(list_sexp) as usize };

    for index in 0..n {
        let elt = unsafe { savvy_ffi::STRING_ELT(names, index as _) };
        if elt == unsafe { savvy_ffi::R_NaString } {
            continue;
        }
        let label = unsafe { CStr::from_ptr(savvy_ffi::R_CHAR(elt)) }
            .to_str()
            .unwrap_or_default();
        if label == key {
            return Ok(unsafe { savvy_ffi::VECTOR_ELT(list_sexp, index as _) });
        }
    }

    Err(savvy::Error::new(&format!("missing field '{key}' in savvy bench_struct_convert")))
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

fn matrix_dims(dim: Option<&[i32]>) -> savvy::Result<(usize, usize)> {
    let dim = dim.ok_or_else(|| savvy::Error::new("expected matrix input"))?;
    if dim.len() != 2 {
        return Err(savvy::Error::new("expected 2D matrix input"));
    }
    Ok((dim[0] as usize, dim[1] as usize))
}

fn sum_real_slice(data: &[f64]) -> f64 {
    data.iter().copied().sum::<f64>()
}

fn na_mean_slice(data: &[f64]) -> f64 {
    let mut sum = 0.0;
    let mut count = 0_usize;

    for &value in data {
        if value.is_na() {
            continue;
        }
        sum += value;
        count += 1;
    }

    if count == 0 {
        f64::na()
    } else {
        sum / count as f64
    }
}

unsafe fn savvy_fill_sum_alloc(sexp: savvy_ffi::SEXP, input: &[f64], bias: f64) -> f64 {
    let data = std::slice::from_raw_parts_mut(savvy_ffi::REAL(sexp), input.len());
    let mut total = 0.0;

    for (index, value) in input.iter().enumerate() {
        let adjusted = *value + bias;
        data[index] = adjusted;
        total += adjusted;
    }

    total
}

fn savvy_protect_overhead_results(input: &[f64]) -> [f64; 5] {
    unsafe {
        let mut totals = [0.0_f64; 5];

        for repeat in 0..PROTECT_OVERHEAD_REPEATS {
            let bias = (repeat as f64 + 1.0) * 0.001;
            let temp = savvy_ffi::Rf_allocVector(savvy_ffi::REALSXP, input.len() as _);
            totals[0] += savvy_fill_sum_alloc(temp, input, bias);
        }

        for repeat in 0..PROTECT_OVERHEAD_REPEATS {
            let bias = (repeat as f64 + 1.0) * 0.001;
            let temp = savvy_ffi::Rf_protect(savvy_ffi::Rf_allocVector(savvy_ffi::REALSXP, input.len() as _));
            totals[1] += savvy_fill_sum_alloc(temp, input, bias);
            savvy_ffi::Rf_unprotect(1);
        }

        for repeat in 0..PROTECT_OVERHEAD_REPEATS {
            let bias = (repeat as f64 + 1.0) * 0.001;
            let temp = savvy_ffi::Rf_protect(savvy_ffi::Rf_allocVector(savvy_ffi::REALSXP, input.len() as _));
            totals[2] += savvy_fill_sum_alloc(temp, input, bias);
        }
        savvy_ffi::Rf_unprotect(PROTECT_OVERHEAD_REPEATS as _);

        for repeat in 0..PROTECT_OVERHEAD_REPEATS {
            let bias = (repeat as f64 + 1.0) * 0.001;
            let temp = savvy_ffi::Rf_allocVector(savvy_ffi::REALSXP, input.len() as _);
            savvy_ffi::R_PreserveObject(temp);
            totals[3] += savvy_fill_sum_alloc(temp, input, bias);
            R_ReleaseObject(temp);
        }

        let mut protect_index: c_int = 0;
        R_ProtectWithIndex(savvy_ffi::R_NilValue, &mut protect_index);
        for repeat in 0..PROTECT_OVERHEAD_REPEATS {
            let bias = (repeat as f64 + 1.0) * 0.001;
            let temp = savvy_ffi::Rf_allocVector(savvy_ffi::REALSXP, input.len() as _);
            R_Reprotect(temp, protect_index);
            totals[4] += savvy_fill_sum_alloc(temp, input, bias);
        }
        savvy_ffi::Rf_unprotect(1);

        totals
    }
}

struct SavvyUnwindState {
    data: *const f64,
    len: usize,
    bias: f64,
}

fn savvy_adjusted_sum(input: &[f64], bias: f64) -> f64 {
    input.iter().map(|value| *value + bias).sum::<f64>()
}

unsafe extern "C" fn savvy_unwind_noop(_data: *mut c_void, _jump: c_int) {}

unsafe extern "C" fn savvy_unwind_ok(data: *mut c_void) -> savvy_ffi::SEXP {
    let state = &*(data as *const SavvyUnwindState);
    let input = std::slice::from_raw_parts(state.data, state.len);
    <f64 as TryInto<Sexp>>::try_into(savvy_adjusted_sum(input, state.bias)).unwrap().0
}

unsafe fn savvy_make_adjusted_temp(input: &[f64], bias: f64) -> savvy_ffi::SEXP {
    let temp = savvy_ffi::Rf_allocVector(savvy_ffi::REALSXP, input.len() as _);
    let data = std::slice::from_raw_parts_mut(savvy_ffi::REAL(temp), input.len());
    for (index, value) in input.iter().enumerate() {
        data[index] = *value + bias;
    }
    temp
}

fn savvy_longjmp_safety_results(input: &[f64]) -> [f64; 4] {
    unsafe {
        let sum_sym = Rf_install(c"sum".as_ptr());
        let stop_sym = Rf_install(c"stop".as_ptr());
        let stop_msg = savvy_ffi::Rf_protect(savvy_ffi::Rf_allocVector(savvy_ffi::STRSXP, 1));
        savvy_ffi::SET_STRING_ELT(stop_msg, 0, Rf_mkChar(c"task32".as_ptr()));
        let stop_call = savvy_ffi::Rf_protect(Rf_lang2(stop_sym, stop_msg));

        let mut totals = [0.0_f64; 4];
        for repeat in 0..LONGJMP_SAFETY_REPEATS {
            let bias = (repeat as f64 + 1.0) * 0.001;
            totals[0] += savvy_adjusted_sum(input, bias);

            let temp = savvy_ffi::Rf_protect(savvy_make_adjusted_temp(input, bias));
            let expr = savvy_ffi::Rf_protect(Rf_lang2(sum_sym, temp));
            let mut err = 0;
            let eval_result = R_tryEvalSilent(expr, R_GlobalEnv, &mut err);
            totals[1] += *savvy_ffi::REAL(eval_result);
            savvy_ffi::Rf_unprotect(2);

            err = 0;
            let _ = R_tryEvalSilent(stop_call, R_GlobalEnv, &mut err);
            if err != 0 {
                totals[2] += 1.0;
            }

            let mut state = SavvyUnwindState { data: input.as_ptr(), len: input.len(), bias };
            let cont = savvy_ffi::Rf_protect(R_MakeUnwindCont());
            let unwind_result = R_UnwindProtect(
                savvy_unwind_ok,
                (&mut state as *mut SavvyUnwindState).cast::<c_void>(),
                savvy_unwind_noop,
                std::ptr::null_mut(),
                cont,
            );
            totals[3] += *savvy_ffi::REAL(unwind_result);
            savvy_ffi::Rf_unprotect(1);
        }

        savvy_ffi::Rf_unprotect(2);
        totals
    }
}

fn savvy_translate_c_cost_results(input: &[f64]) -> [f64; 4] {
    let mut totals = [0.0_f64; 4];

    for repeat in 0..TRANSLATE_C_COST_REPEATS {
        let bias = (repeat as f64 + 1.0) * 0.001;
        for value in input {
            let shifted = *value + bias;
            totals[0] += (shifted - 0.75).abs();
            totals[1] += shifted.ln();
            totals[2] += shifted.exp();
            totals[3] += shifted.sqrt();
        }
    }

    totals
}

#[savvy]
fn bench_fib(n: i32) -> savvy::Result<Sexp> {
    fib_i32(n).try_into()
}

#[savvy]
fn bench_vectorsum(vec: RealSexp) -> savvy::Result<Sexp> {
    sum_real_slice(vec.as_slice()).try_into()
}

#[savvy]
fn bench_transpose(mat: RealSexp) -> savvy::Result<Sexp> {
    let (nr, nc) = matrix_dims(mat.get_dim())?;
    let data = mat.as_slice();
    let mut out = OwnedRealSexp::new(nr * nc)?;
    out.set_dim(&[nc as i32, nr as i32])?;
    let out_slice = out.as_mut_slice();

    for i in 0..nr {
        for j in 0..nc {
            out_slice[j * nr + i] = data[i * nc + j];
        }
    }

    out.into()
}

#[savvy]
fn bench_strings(vec: StringSexp, sep: &str) -> savvy::Result<Sexp> {
    let mut out = String::new();

    for value in vec.iter() {
        if value.is_na() {
            continue;
        }
        out.push_str(value);
        out.push_str(sep);
    }

    out.try_into()
}

#[savvy]
fn bench_string_variants(vec: StringSexp) -> savvy::Result<Sexp> {
    let mut concat = String::new();
    let mut first = true;
    let mut nchar_sum = 0_i32;
    let mut prefix_match = 0_i32;
    let mut extract = Vec::<String>::with_capacity(vec.len());
    let mut upper = Vec::<String>::with_capacity(vec.len());

    for value in vec.iter() {
        if value.is_na() {
            extract.push(String::new());
            upper.push(String::new());
            continue;
        }

        let text = value.to_string();
        nchar_sum += text.len() as i32;
        if text.starts_with("abc") {
            prefix_match += 1;
        }
        if !first {
            concat.push(',');
        }
        concat.push_str(&text);
        first = false;

        extract.push(text[..text.len().min(3)].to_string());
        upper.push(text.to_ascii_uppercase());
    }

    let concat_sexp: Sexp = concat.try_into()?;
    let nchar_sexp: Sexp = nchar_sum.try_into()?;
    let prefix_sexp: Sexp = prefix_match.try_into()?;
    let extract_sexp: Sexp = extract.try_into()?;
    let upper_sexp: Sexp = upper.try_into()?;

    let mut out = OwnedListSexp::new(5, true)?;
    out.set_name_and_value(0, "concat", concat_sexp)?;
    out.set_name_and_value(1, "nchar_sum", nchar_sexp)?;
    out.set_name_and_value(2, "prefix_match", prefix_sexp)?;
    out.set_name_and_value(3, "extract_substr", extract_sexp)?;
    out.set_name_and_value(4, "to_upper", upper_sexp)?;
    out.into()
}

fn bench_string_variants_manual_impl(c_arg_vec: savvy_ffi::SEXP) -> savvy::Result<Sexp> {
    let n = unsafe { savvy_ffi::Rf_xlength(c_arg_vec) as usize };
    let mut concat = String::new();
    let mut first = true;
    let mut nchar_sum = 0_i32;
    let mut prefix_match = 0_i32;

    unsafe {
        let result = savvy_ffi::Rf_protect(savvy_ffi::Rf_allocVector(savvy_ffi::VECSXP, 5));
        let names = savvy_ffi::Rf_protect(savvy_ffi::Rf_allocVector(savvy_ffi::STRSXP, 5));
        let concat_out = savvy_ffi::Rf_protect(savvy_ffi::Rf_allocVector(savvy_ffi::STRSXP, 1));
        let nchar_out = savvy_ffi::Rf_protect(savvy_ffi::Rf_allocVector(savvy_ffi::INTSXP, 1));
        let prefix_out = savvy_ffi::Rf_protect(savvy_ffi::Rf_allocVector(savvy_ffi::INTSXP, 1));
        let extract_out = savvy_ffi::Rf_protect(savvy_ffi::Rf_allocVector(savvy_ffi::STRSXP, n as _));
        let upper_out = savvy_ffi::Rf_protect(savvy_ffi::Rf_allocVector(savvy_ffi::STRSXP, n as _));

        for index in 0..n {
            let value = savvy_ffi::STRING_ELT(c_arg_vec, index as _);
            if value == savvy_ffi::R_NaString {
                savvy_ffi::SET_STRING_ELT(extract_out, index as _, savvy_ffi::R_NaString);
                savvy_ffi::SET_STRING_ELT(upper_out, index as _, savvy_ffi::R_NaString);
                continue;
            }

            let text = CStr::from_ptr(savvy_ffi::R_CHAR(value))
                .to_str()
                .unwrap_or_default();
            nchar_sum += text.len() as i32;
            if text.starts_with("abc") {
                prefix_match += 1;
            }
            if !first {
                concat.push(',');
            }
            concat.push_str(text);
            first = false;

            let sub = CString::new(text[..text.len().min(3)].to_string()).unwrap();
            savvy_ffi::SET_STRING_ELT(extract_out, index as _, Rf_mkChar(sub.as_ptr()));

            let upper = CString::new(text.to_ascii_uppercase()).unwrap();
            savvy_ffi::SET_STRING_ELT(upper_out, index as _, Rf_mkChar(upper.as_ptr()));
        }

        let concat_c = CString::new(concat).unwrap();
        savvy_ffi::SET_STRING_ELT(concat_out, 0, Rf_mkChar(concat_c.as_ptr()));
        *savvy_ffi::INTEGER(nchar_out) = nchar_sum;
        *savvy_ffi::INTEGER(prefix_out) = prefix_match;

        savvy_ffi::SET_VECTOR_ELT(result, 0, concat_out);
        savvy_ffi::SET_VECTOR_ELT(result, 1, nchar_out);
        savvy_ffi::SET_VECTOR_ELT(result, 2, prefix_out);
        savvy_ffi::SET_VECTOR_ELT(result, 3, extract_out);
        savvy_ffi::SET_VECTOR_ELT(result, 4, upper_out);

        savvy_ffi::SET_STRING_ELT(names, 0, Rf_mkChar(c"concat".as_ptr()));
        savvy_ffi::SET_STRING_ELT(names, 1, Rf_mkChar(c"nchar_sum".as_ptr()));
        savvy_ffi::SET_STRING_ELT(names, 2, Rf_mkChar(c"prefix_match".as_ptr()));
        savvy_ffi::SET_STRING_ELT(names, 3, Rf_mkChar(c"extract_substr".as_ptr()));
        savvy_ffi::SET_STRING_ELT(names, 4, Rf_mkChar(c"to_upper".as_ptr()));
        savvy_ffi::Rf_setAttrib(result, savvy_ffi::R_NamesSymbol, names);
        savvy_ffi::Rf_unprotect(7);
        Ok(Sexp(result))
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn savvy_bench_string_variants_manual__ffi(
    c_arg_vec: savvy_ffi::SEXP,
) -> savvy_ffi::SEXP {
    match bench_string_variants_manual_impl(c_arg_vec) {
        Ok(value) => value.0,
        Err(error) => savvy::handle_error(error),
    }
}

fn bench_dataframe_impl(df_sexp: savvy_ffi::SEXP) -> savvy::Result<Sexp> {
    let names = unsafe { savvy_ffi::Rf_getAttrib(df_sexp, savvy_ffi::R_NamesSymbol) };
    let ncols = unsafe { savvy_ffi::Rf_xlength(df_sexp) as usize };
    let mut x_sexp = unsafe { savvy_ffi::R_NilValue };
    let mut y_sexp = unsafe { savvy_ffi::R_NilValue };
    let mut grp_sexp = unsafe { savvy_ffi::R_NilValue };

    for i in 0..ncols {
        let name = unsafe { savvy_ffi::STRING_ELT(names, i as _) };
        let label = unsafe { CStr::from_ptr(savvy_ffi::R_CHAR(name)) }
            .to_str()
            .unwrap_or_default();
        match label {
            "x" => x_sexp = unsafe { savvy_ffi::VECTOR_ELT(df_sexp, i as _) },
            "y" => y_sexp = unsafe { savvy_ffi::VECTOR_ELT(df_sexp, i as _) },
            "grp" => grp_sexp = unsafe { savvy_ffi::VECTOR_ELT(df_sexp, i as _) },
            _ => {}
        }
    }

    if x_sexp == unsafe { savvy_ffi::R_NilValue }
        || y_sexp == unsafe { savvy_ffi::R_NilValue }
        || grp_sexp == unsafe { savvy_ffi::R_NilValue }
    {
        return Err(savvy::Error::new("missing dataframe columns"));
    }

    let nrows = unsafe { savvy_ffi::Rf_xlength(x_sexp) as usize };
    let x = unsafe { std::slice::from_raw_parts(savvy_ffi::REAL(x_sexp), nrows) };
    let y = unsafe { std::slice::from_raw_parts(savvy_ffi::REAL(y_sexp), nrows) };
    let grp = unsafe { std::slice::from_raw_parts(savvy_ffi::INTEGER(grp_sexp), nrows) };

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
        let grp_out = savvy_ffi::Rf_protect(savvy_ffi::Rf_allocVector(savvy_ffi::INTSXP, max_grp as _));
        let sum_out = savvy_ffi::Rf_protect(savvy_ffi::Rf_allocVector(savvy_ffi::REALSXP, max_grp as _));
        let grp_slice = std::slice::from_raw_parts_mut(savvy_ffi::INTEGER(grp_out), max_grp as usize);
        let sum_slice = std::slice::from_raw_parts_mut(savvy_ffi::REAL(sum_out), max_grp as usize);
        for i in 0..max_grp as usize {
            grp_slice[i] = (i + 1) as i32;
            sum_slice[i] = sums[i];
        }

        let cn = savvy_ffi::Rf_protect(savvy_ffi::Rf_allocVector(savvy_ffi::STRSXP, 2));
        savvy_ffi::SET_STRING_ELT(cn, 0, Rf_mkChar(c"grp".as_ptr()));
        savvy_ffi::SET_STRING_ELT(cn, 1, Rf_mkChar(c"z_sum".as_ptr()));

        let res = savvy_ffi::Rf_protect(savvy_ffi::Rf_allocVector(savvy_ffi::VECSXP, 2));
        savvy_ffi::SET_VECTOR_ELT(res, 0, grp_out);
        savvy_ffi::SET_VECTOR_ELT(res, 1, sum_out);
        savvy_ffi::Rf_setAttrib(res, savvy_ffi::R_NamesSymbol, cn);

        let cls = savvy_ffi::Rf_protect(savvy_ffi::Rf_allocVector(savvy_ffi::STRSXP, 1));
        savvy_ffi::SET_STRING_ELT(cls, 0, Rf_mkChar(c"data.frame".as_ptr()));
        savvy_ffi::Rf_setAttrib(res, savvy_ffi::R_ClassSymbol, cls);

        let rn = savvy_ffi::Rf_protect(savvy_ffi::Rf_allocVector(savvy_ffi::INTSXP, 2));
        let row_names = std::slice::from_raw_parts_mut(savvy_ffi::INTEGER(rn), 2);
        row_names[0] = R_NaInt;
        row_names[1] = -max_grp;
        let row_names_sym = Rf_install(c"row.names".as_ptr());
        savvy_ffi::Rf_setAttrib(res, row_names_sym, rn);

        savvy_ffi::Rf_unprotect(6);
        Ok(Sexp(res))
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn savvy_bench_dataframe_manual__ffi(c_arg_df: savvy_ffi::SEXP) -> savvy_ffi::SEXP {
    match bench_dataframe_impl(c_arg_df) {
        Ok(value) => value.0,
        Err(error) => savvy::handle_error(error),
    }
}

#[savvy]
fn bench_na_prop(vec: RealSexp) -> savvy::Result<Sexp> {
    na_mean_slice(vec.as_slice()).try_into()
}

#[savvy]
fn bench_parallel(vec: RealSexp) -> savvy::Result<Sexp> {
    let data = vec.as_slice();
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

    total.try_into()
}

fn savvy_parallel_scaling_sum(data: &[f64], requested_threads: usize) -> f64 {
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

fn savvy_parallel_scaling_results(data: &[f64]) -> [f64; 5] {
    [
        savvy_parallel_scaling_sum(data, 1),
        savvy_parallel_scaling_sum(data, 2),
        savvy_parallel_scaling_sum(data, 4),
        savvy_parallel_scaling_sum(data, 8),
        savvy_parallel_scaling_sum(data, 16),
    ]
}

#[savvy]
fn bench_parallel_scaling(vec: RealSexp) -> savvy::Result<Sexp> {
    let values = savvy_parallel_scaling_results(vec.as_slice());
    unsafe {
        let result = savvy_ffi::Rf_protect(savvy_ffi::Rf_allocVector(savvy_ffi::REALSXP, 5));
        let names = savvy_ffi::Rf_protect(savvy_ffi::Rf_allocVector(savvy_ffi::STRSXP, 5));
        std::slice::from_raw_parts_mut(savvy_ffi::REAL(result), 5).copy_from_slice(&values);
        savvy_ffi::SET_STRING_ELT(names, 0, Rf_mkChar(c"threads_1".as_ptr()));
        savvy_ffi::SET_STRING_ELT(names, 1, Rf_mkChar(c"threads_2".as_ptr()));
        savvy_ffi::SET_STRING_ELT(names, 2, Rf_mkChar(c"threads_4".as_ptr()));
        savvy_ffi::SET_STRING_ELT(names, 3, Rf_mkChar(c"threads_8".as_ptr()));
        savvy_ffi::SET_STRING_ELT(names, 4, Rf_mkChar(c"threads_16".as_ptr()));
        savvy_ffi::Rf_setAttrib(result, savvy_ffi::R_NamesSymbol, names);
        savvy_ffi::Rf_unprotect(2);
        Ok(Sexp(result))
    }
}

#[savvy]
fn bench_memory_bandwidth(vec: RealSexp) -> savvy::Result<Sexp> {
    let data = vec.as_slice();
    let mut copy_temp_total = 0.0_f64;
    let mut copy_out_total = 0.0_f64;
    let mut fill_out_total = 0.0_f64;

    for _ in 0..2 {
        let mut temp = vec![0.0_f64; data.len()];
        temp.copy_from_slice(data);
        copy_temp_total += temp.iter().copied().sum::<f64>();

        unsafe {
            let copy_out = savvy_ffi::Rf_protect(savvy_ffi::Rf_allocVector(savvy_ffi::REALSXP, data.len() as _));
            let copy_slice = std::slice::from_raw_parts_mut(savvy_ffi::REAL(copy_out), data.len());
            copy_slice.copy_from_slice(data);
            copy_out_total += copy_slice.iter().copied().sum::<f64>();
            savvy_ffi::Rf_unprotect(1);

            let fill_out = savvy_ffi::Rf_protect(savvy_ffi::Rf_allocVector(savvy_ffi::REALSXP, data.len() as _));
            let fill_slice = std::slice::from_raw_parts_mut(savvy_ffi::REAL(fill_out), data.len());
            for (index, value) in data.iter().enumerate() {
                fill_slice[index] = *value + 0.5;
            }
            fill_out_total += fill_slice.iter().copied().sum::<f64>();
            savvy_ffi::Rf_unprotect(1);
        }
    }

    unsafe {
        let result = savvy_ffi::Rf_protect(savvy_ffi::Rf_allocVector(savvy_ffi::REALSXP, 3));
        let names = savvy_ffi::Rf_protect(savvy_ffi::Rf_allocVector(savvy_ffi::STRSXP, 3));
        let out = std::slice::from_raw_parts_mut(savvy_ffi::REAL(result), 3);
        out[0] = copy_temp_total;
        out[1] = copy_out_total;
        out[2] = fill_out_total;
        savvy_ffi::SET_STRING_ELT(names, 0, Rf_mkChar(c"copy_temp".as_ptr()));
        savvy_ffi::SET_STRING_ELT(names, 1, Rf_mkChar(c"copy_out".as_ptr()));
        savvy_ffi::SET_STRING_ELT(names, 2, Rf_mkChar(c"fill_out".as_ptr()));
        savvy_ffi::Rf_setAttrib(result, savvy_ffi::R_NamesSymbol, names);
        savvy_ffi::Rf_unprotect(2);
        Ok(Sexp(result))
    }
}

#[savvy]
fn bench_na_prop_vary(inputs: Sexp) -> savvy::Result<Sexp> {
    inputs.assert_list()?;
    let inputs_sexp = inputs.0;
    let n_inputs = unsafe { savvy_ffi::Rf_xlength(inputs_sexp) as usize };

    unsafe {
        let result = savvy_ffi::Rf_protect(savvy_ffi::Rf_allocVector(savvy_ffi::REALSXP, n_inputs as _));
        let out = std::slice::from_raw_parts_mut(savvy_ffi::REAL(result), n_inputs);

        for (index, slot) in out.iter_mut().enumerate() {
            let vec = savvy_ffi::VECTOR_ELT(inputs_sexp, index as _);
            let len = savvy_ffi::Rf_xlength(vec) as usize;
            let data = std::slice::from_raw_parts(savvy_ffi::REAL(vec), len);
            *slot = na_mean_slice(data);
        }

        savvy_ffi::Rf_setAttrib(result, savvy_ffi::R_NamesSymbol, savvy_ffi::Rf_getAttrib(inputs_sexp, savvy_ffi::R_NamesSymbol));
        savvy_ffi::Rf_unprotect(1);
        Ok(Sexp(result))
    }
}

#[savvy]
fn bench_scale_law(inputs: Sexp) -> savvy::Result<Sexp> {
    inputs.assert_list()?;
    let inputs_sexp = inputs.0;
    let n_inputs = unsafe { savvy_ffi::Rf_xlength(inputs_sexp) as usize };

    unsafe {
        let result = savvy_ffi::Rf_protect(savvy_ffi::Rf_allocVector(savvy_ffi::REALSXP, n_inputs as _));
        let out = std::slice::from_raw_parts_mut(savvy_ffi::REAL(result), n_inputs);

        for (index, slot) in out.iter_mut().enumerate() {
            let vec = savvy_ffi::VECTOR_ELT(inputs_sexp, index as _);
            let len = savvy_ffi::Rf_xlength(vec) as usize;
            let data = std::slice::from_raw_parts(savvy_ffi::REAL(vec), len);
            *slot = sum_real_slice(data);
        }

        savvy_ffi::Rf_setAttrib(result, savvy_ffi::R_NamesSymbol, savvy_ffi::Rf_getAttrib(inputs_sexp, savvy_ffi::R_NamesSymbol));
        savvy_ffi::Rf_unprotect(1);
        Ok(Sexp(result))
    }
}

#[savvy]
fn bench_arena_vs_rmalloc(vec: RealSexp) -> savvy::Result<Sexp> {
    let src = vec.as_slice();
    let mut total = 0.0;

    for repeat in 0..ALLOCATION_REPEATS {
        let bias = (repeat as f64 + 1.0) * 0.001;
        let mut temp = vec![0.0_f64; src.len()];

        for (index, value) in src.iter().enumerate() {
            temp[index] = *value + bias;
        }
        total += sum_real_slice(&temp);
    }

    total.try_into()
}

#[savvy]
fn bench_prot_overhead(vec: RealSexp) -> savvy::Result<Sexp> {
    let values = savvy_protect_overhead_results(vec.as_slice());
    unsafe {
        let result = savvy_ffi::Rf_protect(savvy_ffi::Rf_allocVector(savvy_ffi::REALSXP, 5));
        let names = savvy_ffi::Rf_protect(savvy_ffi::Rf_allocVector(savvy_ffi::STRSXP, 5));
        std::slice::from_raw_parts_mut(savvy_ffi::REAL(result), 5).copy_from_slice(&values);

        savvy_ffi::SET_STRING_ELT(names, 0, Rf_mkChar(c"unsafe".as_ptr()));
        savvy_ffi::SET_STRING_ELT(names, 1, Rf_mkChar(c"manual".as_ptr()));
        savvy_ffi::SET_STRING_ELT(names, 2, Rf_mkChar(c"batch".as_ptr()));
        savvy_ffi::SET_STRING_ELT(names, 3, Rf_mkChar(c"preserve".as_ptr()));
        savvy_ffi::SET_STRING_ELT(names, 4, Rf_mkChar(c"reprotect".as_ptr()));
        savvy_ffi::Rf_setAttrib(result, savvy_ffi::R_NamesSymbol, names);
        savvy_ffi::Rf_unprotect(2);
        Ok(Sexp(result))
    }
}

#[savvy]
fn bench_protect_stress(n: i32) -> savvy::Result<Sexp> {
    unsafe {
        for _ in 0..n {
            savvy_ffi::Rf_protect(savvy_ffi::Rf_allocVector(savvy_ffi::REALSXP, 1));
        }
        savvy_ffi::Rf_unprotect(n);
    }
    0.try_into()
}

#[savvy]
fn bench_blas_matmul(a: RealSexp, b: RealSexp) -> savvy::Result<Sexp> {
    let (n_rows, k_cols) = matrix_dims(a.get_dim())?;
    let (k_rhs, m_cols) = matrix_dims(b.get_dim())?;
    if k_cols != k_rhs {
        return Err(savvy::Error::new("non-conformable matrices"));
    }
    let n = n_rows as c_int;
    let m = m_cols as c_int;
    let k = k_cols as c_int;
    let mut out = OwnedRealSexp::new((n * m) as usize)?;
    out.set_dim(&[n, m])?;
    let alpha = 1.0;
    let beta = 0.0;
    let notrans = b'N' as c_char;

    unsafe {
        dgemm_(&notrans, &notrans, &n, &m, &k, &alpha, a.as_slice().as_ptr(), &n, b.as_slice().as_ptr(), &k, &beta, out.as_mut_slice().as_mut_ptr(), &n, 1, 1);
    }

    out.into()
}

#[savvy]
fn bench_crossprod(x: RealSexp) -> savvy::Result<Sexp> {
    let (nr_rows, nc_cols) = matrix_dims(x.get_dim())?;
    let nr = nr_rows as c_int;
    let nc = nc_cols as c_int;
    let mut out = OwnedRealSexp::new((nc * nc) as usize)?;
    out.set_dim(&[nc, nc])?;
    let out_slice = out.as_mut_slice();
    let alpha = 1.0;
    let beta = 0.0;
    let uplo = b'U' as c_char;
    let trans = b'T' as c_char;

    unsafe {
        dsyrk_(&uplo, &trans, &nc, &nr, &alpha, x.as_slice().as_ptr(), &nr, &beta, out_slice.as_mut_ptr(), &nc, 1, 1);
    }

    for i in 0..nc as usize {
        for j in 0..i {
            out_slice[i * nc as usize + j] = out_slice[j * nc as usize + i];
        }
    }

    out.into()
}

#[savvy]
fn bench_cholesky(a: RealSexp) -> savvy::Result<Sexp> {
    let (n_rows, n_cols) = matrix_dims(a.get_dim())?;
    if n_rows != n_cols {
        return Err(savvy::Error::new("expected square matrix"));
    }
    let n = n_rows as c_int;
    let mut out: OwnedRealSexp = a.as_slice().to_vec().try_into()?;
    out.set_dim(&[n, n])?;
    let out_slice = out.as_mut_slice();
    let uplo = b'U' as c_char;
    let mut info = 0;

    unsafe {
        dpotrf_(&uplo, &n, out_slice.as_mut_ptr(), &n, &mut info, 1);
    }

    for col in 0..n as usize {
        for row in (col + 1)..n as usize {
            out_slice[col * n as usize + row] = 0.0;
        }
    }

    out.into()
}

#[savvy]
fn bench_lm(x: RealSexp, y: RealSexp) -> savvy::Result<Sexp> {
    let (n_rows, p_cols) = matrix_dims(x.get_dim())?;
    let n = n_rows as c_int;
    let p = p_cols as c_int;
    let mut xtx = vec![0.0_f64; (p * p) as usize];
    let mut xty = vec![0.0_f64; p as usize];
    let alpha = 1.0;
    let beta = 0.0;
    let notrans = b'N' as c_char;
    let trans = b'T' as c_char;
    let one = 1 as c_int;

    unsafe {
        dgemm_(&trans, &notrans, &p, &p, &n, &alpha, x.as_slice().as_ptr(), &n, x.as_slice().as_ptr(), &n, &beta, xtx.as_mut_ptr(), &p, 1, 1);
        dgemm_(&trans, &notrans, &p, &one, &n, &alpha, x.as_slice().as_ptr(), &n, y.as_slice().as_ptr(), &n, &beta, xty.as_mut_ptr(), &p, 1, 1);

        let uplo = b'U' as c_char;
        let mut info = 0;
        dpotrf_(&uplo, &p, xtx.as_mut_ptr(), &p, &mut info, 1);

        let side = b'L' as c_char;
        let diag = b'N' as c_char;
        dtrsm_(&side, &uplo, &trans, &diag, &p, &one, &alpha, xtx.as_ptr(), &p, xty.as_mut_ptr(), &p, 1, 1, 1, 1);
        dtrsm_(&side, &uplo, &notrans, &diag, &p, &one, &alpha, xtx.as_ptr(), &p, xty.as_mut_ptr(), &p, 1, 1, 1, 1);
    }

    xty.try_into()
}

#[savvy]
fn bench_rowsums(mat: RealSexp) -> savvy::Result<Sexp> {
    let (nr, nc) = matrix_dims(mat.get_dim())?;
    let data = mat.as_slice();
    let mut sums = vec![0.0_f64; nr];

    for j in 0..nc {
        for i in 0..nr {
            sums[i] += data[i + j * nr];
        }
    }

    sums.try_into()
}

#[savvy]
fn bench_elem_ops(vec: RealSexp) -> savvy::Result<Sexp> {
    let src = vec.as_slice();
    let n = src.len();
    let mut out = OwnedRealSexp::new(n * 4)?;
    out.set_dim(&[n as i32, 4_i32])?;
    let out_slice = out.as_mut_slice();

    for i in 0..n {
        let value = src[i];
        out_slice[i] = value.abs();
        out_slice[i + n] = if value > 0.0 { value.ln() } else { 0.0 };
        out_slice[i + 2 * n] = value.exp();
        out_slice[i + 3 * n] = if value >= 0.0 { value.sqrt() } else { 0.0 };
    }

    out.into()
}

#[savvy]
fn bench_rowcol_means(mat: RealSexp) -> savvy::Result<Sexp> {
    let (nr, nc) = matrix_dims(mat.get_dim())?;
    let data = mat.as_slice();
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

    let mut out = OwnedListSexp::new(2, false)?;
    out.set_value(0, <Vec<f64> as TryInto<Sexp>>::try_into(row_means)?)?;
    out.set_value(1, <Vec<f64> as TryInto<Sexp>>::try_into(col_sums)?)?;
    out.into()
}

#[savvy]
fn bench_broadcast(vec: RealSexp, scalar: f64) -> savvy::Result<Sexp> {
    let n = vec.len();
    let src = vec.as_slice();
    let mut out = OwnedRealSexp::new(n)?;
    let out_slice = out.as_mut_slice();
    for i in 0..n {
        out_slice[i] = src[i] + scalar;
    }
    out.into()
}

#[savvy]
fn bench_sort(vec: RealSexp) -> savvy::Result<Sexp> {
    let mut out = vec.as_slice().to_vec();
    radix_sort_f64(&mut out);
    out.try_into()
}

#[savvy]
fn bench_cumsum(vec: RealSexp) -> savvy::Result<Sexp> {
    let n = vec.len();
    let src = vec.as_slice();
    let mut out = OwnedRealSexp::new(n)?;
    let out_slice = out.as_mut_slice();
    let mut total = 0.0;
    for i in 0..n {
        total += src[i];
        out_slice[i] = total;
    }
    out.into()
}

#[savvy]
fn bench_rnorm(n: i32) -> savvy::Result<Sexp> {
    let mut out = OwnedRealSexp::new(n as usize)?;
    unsafe {
        GetRNGstate();
        for value in out.as_mut_slice().iter_mut() {
            *value = norm_rand();
        }
        PutRNGstate();
    }
    out.into()
}

#[savvy]
fn bench_string_nchar(vec: StringSexp) -> savvy::Result<Sexp> {
    let mut total = 0_i32;

    for value in vec.iter() {
        if value.is_na() {
            continue;
        }
        total += value.len() as i32;
    }

    total.try_into()
}

#[savvy]
fn bench_which_na(vec: RealSexp) -> savvy::Result<Sexp> {
    vec.as_slice()
        .iter()
        .enumerate()
        .filter_map(|(index, value)| if value.is_na() { Some((index + 1) as i32) } else { None })
        .collect::<Vec<i32>>()
        .try_into()
}

#[savvy]
fn bench_altrep_sum(vec: Sexp) -> savvy::Result<Sexp> {
    let result = unsafe {
        let sum_sym = Rf_install(c"sum".as_ptr());
        let call = savvy::unwind_protect(|| Rf_lang2(sum_sym, vec.0))?;
        savvy::unwind_protect(|| Rf_eval(call, R_GlobalEnv))?
    };
    Ok(Sexp(result))
}

#[savvy]
fn bench_longjmp_safety(vec: RealSexp) -> savvy::Result<Sexp> {
    let values = savvy_longjmp_safety_results(vec.as_slice());
    unsafe {
        let result = savvy_ffi::Rf_protect(savvy_ffi::Rf_allocVector(savvy_ffi::REALSXP, 4));
        let names = savvy_ffi::Rf_protect(savvy_ffi::Rf_allocVector(savvy_ffi::STRSXP, 4));
        std::slice::from_raw_parts_mut(savvy_ffi::REAL(result), 4).copy_from_slice(&values);
        savvy_ffi::SET_STRING_ELT(names, 0, Rf_mkChar(c"direct".as_ptr()));
        savvy_ffi::SET_STRING_ELT(names, 1, Rf_mkChar(c"try_ok".as_ptr()));
        savvy_ffi::SET_STRING_ELT(names, 2, Rf_mkChar(c"try_err".as_ptr()));
        savvy_ffi::SET_STRING_ELT(names, 3, Rf_mkChar(c"unwind_ok".as_ptr()));
        savvy_ffi::Rf_setAttrib(result, savvy_ffi::R_NamesSymbol, names);
        savvy_ffi::Rf_unprotect(2);
        Ok(Sexp(result))
    }
}

#[savvy]
fn bench_translate_c_cost(vec: RealSexp) -> savvy::Result<Sexp> {
    let values = savvy_translate_c_cost_results(vec.as_slice());
    unsafe {
        let result = savvy_ffi::Rf_protect(savvy_ffi::Rf_allocVector(savvy_ffi::REALSXP, 4));
        let names = savvy_ffi::Rf_protect(savvy_ffi::Rf_allocVector(savvy_ffi::STRSXP, 4));
        std::slice::from_raw_parts_mut(savvy_ffi::REAL(result), 4).copy_from_slice(&values);
        savvy_ffi::SET_STRING_ELT(names, 0, Rf_mkChar(c"abs".as_ptr()));
        savvy_ffi::SET_STRING_ELT(names, 1, Rf_mkChar(c"log".as_ptr()));
        savvy_ffi::SET_STRING_ELT(names, 2, Rf_mkChar(c"exp".as_ptr()));
        savvy_ffi::SET_STRING_ELT(names, 3, Rf_mkChar(c"sqrt".as_ptr()));
        savvy_ffi::Rf_setAttrib(result, savvy_ffi::R_NamesSymbol, names);
        savvy_ffi::Rf_unprotect(2);
        Ok(Sexp(result))
    }
}

#[savvy]
fn bench_altrep_read(vec: IntegerSexp) -> savvy::Result<Sexp> {
    let n = vec.len();
    let mut first = 0_i32;
    let mut last = 0_i32;
    unsafe {
        INTEGER_GET_REGION(vec.inner(), 0, 1, &mut first);
        INTEGER_GET_REGION(vec.inner(), (n - 1) as _, 1, &mut last);
    }
    vec![first, last].try_into()
}

#[savvy]
fn bench_altrep_create(n: i32) -> savvy::Result<Sexp> {
    if n < 0 || n as usize > ALTREP_CREATE_MAX_LEN {
        return Err(savvy::Error::new("n out of range for savvy_bench_altrep_create"));
    }

    unsafe {
        savvy_altrep_create_ensure_class();
        savvy_altrep_create_ensure_backing(n as usize);
        let len = savvy_ffi::Rf_protect(savvy_ffi::Rf_ScalarInteger(n));
        let result = R_new_altrep(SAVVY_ALTREP_CREATE_CLASS, R_NilValue, len);
        savvy_ffi::Rf_unprotect(1);
        Ok(Sexp(result))
    }
}

#[savvy]
fn bench_owned_altrep_real_sum(n: i32) -> savvy::Result<Sexp> {
    if n < 0 || n as usize > ALTREP_CREATE_MAX_LEN {
        return Err(savvy::Error::new(
            "n out of range for savvy_bench_owned_altrep_real_sum",
        ));
    }

    unsafe {
        savvy_altrep_create_ensure_class();
        savvy_altrep_create_ensure_backing(n as usize);
        let len = savvy_ffi::Rf_protect(savvy_ffi::Rf_ScalarInteger(n));
        let vec = savvy_ffi::Rf_protect(R_new_altrep(SAVVY_ALTREP_CREATE_CLASS, R_NilValue, len));
        let data = savvy_ffi::REAL(vec);
        let mut total = 0.0;
        for index in 0..n as usize {
            total += *data.add(index);
        }
        savvy_ffi::Rf_unprotect(2);
        total.try_into()
    }
}

#[savvy]
fn bench_owned_altrep_int_sum(n: i32) -> savvy::Result<Sexp> {
    if n < 0 || n as usize > ALTREP_CREATE_MAX_LEN {
        return Err(savvy::Error::new(
            "n out of range for savvy_bench_owned_altrep_int_sum",
        ));
    }

    unsafe {
        savvy_altint_create_ensure_class();
        savvy_altint_create_ensure_backing(n as usize);
        let len = savvy_ffi::Rf_protect(savvy_ffi::Rf_ScalarInteger(n));
        let vec = savvy_ffi::Rf_protect(R_new_altrep(SAVVY_ALTINT_CREATE_CLASS, R_NilValue, len));
        let data = savvy_ffi::INTEGER(vec);
        let mut total = 0.0;
        for index in 0..n as usize {
            total += *data.add(index) as f64;
        }
        savvy_ffi::Rf_unprotect(2);
        total.try_into()
    }
}

#[savvy]
fn bench_owned_altrep_logical_sum(n: i32) -> savvy::Result<Sexp> {
    if n < 0 || n as usize > ALTREP_CREATE_MAX_LEN {
        return Err(savvy::Error::new(
            "n out of range for savvy_bench_owned_altrep_logical_sum",
        ));
    }

    unsafe {
        savvy_altlogical_create_ensure_class();
        savvy_altlogical_create_ensure_backing(n as usize);
        let len = savvy_ffi::Rf_protect(savvy_ffi::Rf_ScalarInteger(n));
        let vec = savvy_ffi::Rf_protect(R_new_altrep(SAVVY_ALTLOGICAL_CREATE_CLASS, R_NilValue, len));
        let data = savvy_ffi::LOGICAL(vec);
        let mut total = 0.0;
        for index in 0..n as usize {
            if *data.add(index) != 0 {
                total += 1.0;
            }
        }
        savvy_ffi::Rf_unprotect(2);
        total.try_into()
    }
}

#[savvy]
fn bench_comptime_dispatch(inputs: Sexp) -> savvy::Result<Sexp> {
    inputs.assert_list()?;
    let n_inputs = unsafe { savvy_ffi::Rf_xlength(inputs.0) as usize };
    let mut total = 0.0;

    for _ in 0..COMPTIME_DISPATCH_REPEATS {
        for index in 0..n_inputs {
            total += dispatch_sum_atomic(unsafe { savvy_ffi::VECTOR_ELT(inputs.0, index as _) });
        }
    }

    total.try_into()
}

#[savvy]
fn bench_struct_convert(input: Sexp) -> savvy::Result<Sexp> {
    input.assert_list()?;
    let input_sexp = input.0;

    let payload = StructConvertPayload {
        id: unsafe { *savvy_ffi::INTEGER(find_named(input_sexp, "id")?) },
        count: unsafe { *savvy_ffi::INTEGER(find_named(input_sexp, "count")?) },
        level: unsafe { *savvy_ffi::INTEGER(find_named(input_sexp, "level")?) },
        flag: unsafe { *savvy_ffi::LOGICAL(find_named(input_sexp, "flag")?) != 0 },
        enabled: unsafe { *savvy_ffi::LOGICAL(find_named(input_sexp, "enabled")?) != 0 },
        ratio: unsafe { *savvy_ffi::REAL(find_named(input_sexp, "ratio")?) },
        offset: unsafe { *savvy_ffi::REAL(find_named(input_sexp, "offset")?) },
        scale: unsafe { *savvy_ffi::REAL(find_named(input_sexp, "scale")?) },
        weights: {
            let sexp = find_named(input_sexp, "weights")?;
            let n = unsafe { savvy_ffi::Rf_xlength(sexp) as usize };
            unsafe { std::slice::from_raw_parts(savvy_ffi::REAL(sexp), n) }.to_vec()
        },
        indices: {
            let sexp = find_named(input_sexp, "indices")?;
            let n = unsafe { savvy_ffi::Rf_xlength(sexp) as usize };
            unsafe { std::slice::from_raw_parts(savvy_ffi::INTEGER(sexp), n) }.to_vec()
        },
    };

    let id: Sexp = payload.id.try_into()?;
    let count: Sexp = payload.count.try_into()?;
    let level: Sexp = payload.level.try_into()?;
    let flag: Sexp = payload.flag.try_into()?;
    let enabled: Sexp = payload.enabled.try_into()?;
    let ratio: Sexp = payload.ratio.try_into()?;
    let offset: Sexp = payload.offset.try_into()?;
    let scale: Sexp = payload.scale.try_into()?;
    let weights: Sexp = payload.weights.try_into()?;
    let indices: Sexp = payload.indices.try_into()?;

    let mut out = OwnedListSexp::new(10, true)?;
    out.set_name_and_value(0, "id", id)?;
    out.set_name_and_value(1, "count", count)?;
    out.set_name_and_value(2, "level", level)?;
    out.set_name_and_value(3, "flag", flag)?;
    out.set_name_and_value(4, "enabled", enabled)?;
    out.set_name_and_value(5, "ratio", ratio)?;
    out.set_name_and_value(6, "offset", offset)?;
    out.set_name_and_value(7, "scale", scale)?;
    out.set_name_and_value(8, "weights", weights)?;
    out.set_name_and_value(9, "indices", indices)?;
    out.into()
}