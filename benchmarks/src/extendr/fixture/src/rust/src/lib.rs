use extendr_api::prelude::*;
use std::sync::atomic::{AtomicI32, Ordering};

static CONSTRUCTOR_COUNT: AtomicI32 = AtomicI32::new(0);
static METHOD_COUNT: AtomicI32 = AtomicI32::new(0);
static ERROR_COUNT: AtomicI32 = AtomicI32::new(0);
static FINALIZER_COUNT: AtomicI32 = AtomicI32::new(0);

fn fixture_failure(message: &str) -> Error {
    Error::Other(message.to_string())
}

#[extendr]
fn fixture_zero() -> i32 {
    1
}

#[extendr]
fn fixture_scalar(value: Doubles) -> std::result::Result<f64, Error> {
    if value.len() != 1 {
        return Err(fixture_failure("scalar input must have length one"));
    }
    let scalar = value.elt(0);
    if scalar.is_na() {
        return Err(fixture_failure("scalar input must not be NA"));
    }
    Ok(scalar.0)
}

#[extendr]
fn fixture_numeric(value: Doubles) -> Doubles {
    value.iter().map(|element| element.0 * 2.0).collect()
}

#[extendr]
fn fixture_altrep_integer(value: Integers) -> f64 {
    let mut total = 0.0;
    for index in 0..value.len() {
        let element = value.elt(index);
        if element.is_na() {
            return Rfloat::na().0;
        }
        total += element.0 as f64;
    }
    total
}

#[extendr]
fn fixture_strings(value: Strings) -> i32 {
    value.iter().filter(|element| !element.is_na()).count() as i32
}

#[extendr]
fn fixture_raw(value: Raw) -> Raw {
    Raw::from_bytes(value.as_slice())
}

#[extendr]
fn fixture_complex(value: Complexes) -> Complexes {
    value.iter().map(|element| element.0).collect()
}

#[extendr]
fn fixture_logical_counts(value: Logicals) -> Integers {
    let mut counts = [0, 0, 0];
    for element in value.iter() {
        if element.is_na() {
            counts[2] += 1;
        } else if element.is_true() {
            counts[1] += 1;
        } else {
            counts[0] += 1;
        }
    }
    let mut result = Integers::from_values(counts);
    result.set_names(["false", "true", "missing"]).unwrap();
    result
}

#[extendr]
fn fixture_schema(value: List) -> std::result::Result<List, Error> {
    if value.len() != 4 {
        return Err(fixture_failure("schema must have four fields"));
    }
    let names: Vec<_> = value
        .names()
        .ok_or_else(|| fixture_failure("schema must be named"))?
        .collect();
    if names != ["id", "count", "ratio", "enabled"] {
        return Err(fixture_failure(
            "schema names do not match the declared order",
        ));
    }
    let attributes = call!("attributes", value.clone())?;
    let attribute_list = List::try_from(&attributes)?;
    let attribute_names: Vec<_> = attribute_list
        .names()
        .ok_or_else(|| fixture_failure("schema attributes must be named"))?
        .collect();
    if attribute_list.len() != 1 || attribute_names != ["names"] {
        return Err(fixture_failure(
            "schema must have names and no other attributes",
        ));
    }
    let fields: Vec<_> = value.values().collect();
    if !fields[0].is_integer()
        || !fields[1].is_integer()
        || !fields[2].is_real()
        || !fields[3].is_logical()
    {
        return Err(fixture_failure(
            "schema fields do not match the declared types",
        ));
    }
    let id = Integers::try_from(&fields[0])?;
    let count = Integers::try_from(&fields[1])?;
    let ratio = Doubles::try_from(&fields[2])?;
    let enabled = Logicals::try_from(&fields[3])?;
    if id.len() != 1 || count.len() != 1 || ratio.len() != 1 || enabled.len() != 1 {
        return Err(fixture_failure("schema fields must be scalar"));
    }
    if id.elt(0).is_na() || count.elt(0).is_na() || ratio.elt(0).is_na() || enabled.elt(0).is_na() {
        return Err(fixture_failure("schema scalar fields must not be NA"));
    }
    Ok(value)
}

#[extendr]
fn fixture_error(_trigger: f64) -> std::result::Result<(), Error> {
    ERROR_COUNT.fetch_add(1, Ordering::Relaxed);
    Err(fixture_failure("fixture error"))
}

#[extendr]
fn fixture_lifecycle_reset() {
    CONSTRUCTOR_COUNT.store(0, Ordering::Relaxed);
    METHOD_COUNT.store(0, Ordering::Relaxed);
    ERROR_COUNT.store(0, Ordering::Relaxed);
    FINALIZER_COUNT.store(0, Ordering::Relaxed);
}

#[extendr]
fn fixture_lifecycle_counts() -> Integers {
    let mut result = Integers::from_values([
        CONSTRUCTOR_COUNT.load(Ordering::Relaxed),
        METHOD_COUNT.load(Ordering::Relaxed),
        ERROR_COUNT.load(Ordering::Relaxed),
        FINALIZER_COUNT.load(Ordering::Relaxed),
    ]);
    result
        .set_names(["constructor", "method", "error", "finalizer"])
        .unwrap();
    result
}

#[extendr]
fn fixture_outputs() -> List {
    let numeric = Doubles::from_values([1.5, Rfloat::na().0]);
    let mut string = Strings::new_with_na(1);
    string.set_elt(0, Rstr::from("fixture"));
    let raw = Raw::from_bytes(&[1, 2, 3]);
    let complex: Complexes = [c64::new(1.0, 2.0), c64::na()].into_iter().collect();
    let mut logical = Logicals::new(3);
    logical.set_elt(0, false.into());
    logical.set_elt(1, true.into());
    logical.set_elt(2, Rbool::na());
    list!(
        numeric = numeric,
        string = string,
        raw = raw,
        complex = complex,
        logical = logical,
        list = list!(value = 7)
    )
}

#[extendr]
fn bench_vector_sum(x: Doubles) -> std::result::Result<Robj, Error> {
    call!("sum", x)
}

#[extendr]
fn bench_numeric_transform(x: Doubles) -> Doubles {
    fixture_numeric(x)
}

#[extendr]
fn bench_broadcast(x: Doubles, scalar: f64) -> f64 {
    let mut total = 0.0;
    let mut correction = 0.0;
    for value in x.iter().map(|v| v.0 + scalar) {
        let adjusted = value - correction;
        let next = total + adjusted;
        correction = (next - total) - adjusted;
        total = next;
    }
    total
}

#[extendr]
fn bench_sort(x: Doubles) -> Doubles {
    let mut values: Vec<f64> = x.iter().map(|v| v.0).collect();
    values.sort_by(|a, b| a.total_cmp(b));
    values.into_iter().collect()
}

#[extendr]
fn bench_missing_mean(x: Doubles) -> std::result::Result<Robj, Error> {
    call!("mean", x, 0.0, true)
}

#[extendr]
fn bench_transpose(x: Robj) -> std::result::Result<Robj, Error> {
    call!("t", x)
}

#[extendr]
fn bench_rowcol(x: Robj) -> std::result::Result<List, Error> {
    Ok(list!(
        row_means = call!("rowMeans", x.clone())?,
        column_sums = call!("colSums", x)?
    ))
}

#[extendr]
fn bench_matmul(x: Robj, y: Robj) -> std::result::Result<Robj, Error> {
    let operator = call!("get", "%*%", call!("baseenv")?)?;
    operator
        .as_function()
        .ok_or_else(|| fixture_failure("matrix multiplication operator was not a function"))?
        .call(pairlist!(x, y))
}

#[extendr]
fn bench_dataframe(data: List) -> std::result::Result<Robj, Error> {
    let x = Doubles::try_from(data.elt(0)?)?;
    let y = Doubles::try_from(data.elt(1)?)?;
    let group = Integers::try_from(call!("as.integer", data.elt(2)?)?)?;
    let mut totals = [0.0; 10];
    for index in 0..x.len() {
        let value = x.elt(index).0;
        if !value.is_nan() && value > 0.0 {
            totals[(group.elt(index).0 - 1) as usize] += value / y.elt(index).0;
        }
    }
    call!(
        "data.frame",
        grp = Integers::from_values([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]),
        z_sum = Doubles::from_values(totals)
    )
}

#[extendr]
fn bench_list_sum(x: List) -> std::result::Result<Robj, Error> {
    let sum_function = call!("get", "sum", envir = call!("baseenv")?)?;
    let totals = call!("vapply", x, sum_function, Doubles::from_values([0.0]))?;
    call!("sum", totals)
}

#[extendr]
fn bench_string_concat(x: Strings) -> std::result::Result<Robj, Error> {
    call!("paste", x, collapse = ", ")
}

#[extendr]
fn bench_string_metadata(x: Strings) -> std::result::Result<Integers, Error> {
    let lengths = Integers::try_from(&call!("nchar", x.clone(), "bytes")?)?;
    let marks = Strings::try_from(&call!("Encoding", x.clone())?)?;
    let mut result = [0, 0, 0, 0, 0];
    for index in 0..x.len() {
        if x.elt(index).is_na() {
            result[4] += 1;
        } else {
            result[0] += lengths.elt(index).0;
        }
        let mark = marks.elt(index);
        match mark.as_ref() {
            "UTF-8" => result[1] += 1,
            "latin1" => result[2] += 1,
            "bytes" => result[3] += 1,
            _ => {}
        }
    }
    let mut output = Integers::from_values(result);
    output.set_names(["bytes", "utf8", "latin1", "bytes_marked", "missing"])?;
    Ok(output)
}

#[extendr]
fn bench_factor(x: Strings) -> std::result::Result<Robj, Error> {
    let levels = call!(
        "sprintf",
        "level_%03d",
        Integers::from_values((1..=100).collect::<Vec<_>>())
    )?;
    call!("factor", x, levels = levels)
}

#[extendr]
fn bench_attributes(x: Doubles) -> Robj {
    let mut result = Doubles::from_values(x.iter().map(|element| element.0).collect::<Vec<_>>());
    result.set_class(["bench_class"]).unwrap();
    result.set_attrib("creator", "zigr_bench").unwrap();
    drop(result.class());
    drop(result.get_attrib("creator"));
    result.into()
}

#[extendr]
fn bench_s4(x: Doubles) -> std::result::Result<Robj, Error> {
    let object = call!("new", "BenchS4", slot_x = x)?;
    call!("slot", object, "slot_x")
}

#[extendr]
fn bench_logical_counts(x: Logicals) -> Integers {
    fixture_logical_counts(x)
}

#[extendr]
fn bench_raw_copy(x: Raw) -> Raw {
    fixture_raw(x)
}

#[extendr]
fn bench_complex_conjugate(x: Complexes) -> Complexes {
    x.iter().map(|v| c64::new(v.re().0, -v.im().0)).collect()
}

#[extendr]
fn bench_schema(x: List) -> std::result::Result<List, Error> {
    fixture_schema(x)
}

#[extendr]
fn bench_altrep_sum(x: Integers) -> f64 {
    fixture_altrep_integer(x)
}

#[extendr]
fn bench_altrep_index(x: Integers) -> f64 {
    (0..x.len()).step_by(10000).map(|i| x.elt(i).0 as f64).sum()
}

#[extendr]
fn bench_altrep_materialize(x: Integers) -> Integers {
    x.iter().map(|v| v.0).collect()
}

#[extendr]
fn bench_eval(x: Doubles) -> std::result::Result<Robj, Error> {
    let expression = call!("parse", text = "sum(x) + mean(x)")?;
    let environment = call!("list2env", list!(x = x), parent = call!("baseenv")?)?;
    call!("eval", expression, environment)
}

#[extendr]
fn bench_serialize(x: Robj) -> std::result::Result<Robj, Error> {
    let bytes = call!("serialize", x, Robj::from(()), version = 3)?;
    call!("unserialize", bytes)
}

#[extendr]
fn bench_rng(n: i32) -> std::result::Result<Robj, Error> {
    call!("rnorm", n)
}

#[extendr]
fn bench_outputs() -> List {
    fixture_outputs()
}

#[extendr]
struct FixtureState {
    value: i32,
}

#[extendr]
impl FixtureState {
    fn new() -> Self {
        CONSTRUCTOR_COUNT.fetch_add(1, Ordering::Relaxed);
        Self { value: 0 }
    }

    fn increment(&mut self, amount: Integers) -> std::result::Result<i32, Error> {
        if amount.len() != 1 || amount.elt(0).is_na() {
            return Err(fixture_failure(
                "fixture method amount must be one non-missing INTEGER value",
            ));
        }
        METHOD_COUNT.fetch_add(1, Ordering::Relaxed);
        self.value += amount.elt(0).0;
        Ok(self.value)
    }

    fn read(&self) -> i32 {
        METHOD_COUNT.fetch_add(1, Ordering::Relaxed);
        self.value
    }
}

impl Drop for FixtureState {
    fn drop(&mut self) {
        FINALIZER_COUNT.fetch_add(1, Ordering::Relaxed);
    }
}

extendr_module! {
    mod zigr_extendr;
    fn fixture_zero;
    fn fixture_scalar;
    fn fixture_numeric;
    fn fixture_altrep_integer;
    fn fixture_strings;
    fn fixture_raw;
    fn fixture_complex;
    fn fixture_logical_counts;
    fn fixture_schema;
    fn fixture_error;
    fn fixture_lifecycle_reset;
    fn fixture_lifecycle_counts;
    fn fixture_outputs;
    fn bench_vector_sum;
    fn bench_numeric_transform;
    fn bench_broadcast;
    fn bench_sort;
    fn bench_missing_mean;
    fn bench_transpose;
    fn bench_rowcol;
    fn bench_matmul;
    fn bench_dataframe;
    fn bench_list_sum;
    fn bench_string_concat;
    fn bench_string_metadata;
    fn bench_factor;
    fn bench_attributes;
    fn bench_s4;
    fn bench_logical_counts;
    fn bench_raw_copy;
    fn bench_complex_conjugate;
    fn bench_schema;
    fn bench_altrep_sum;
    fn bench_altrep_index;
    fn bench_altrep_materialize;
    fn bench_eval;
    fn bench_serialize;
    fn bench_rng;
    fn bench_outputs;
    impl FixtureState;
}
