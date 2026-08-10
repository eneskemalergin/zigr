use savvy::*;
use std::sync::atomic::{AtomicI32, Ordering};

static CONSTRUCTOR_COUNT: AtomicI32 = AtomicI32::new(0);
static METHOD_COUNT: AtomicI32 = AtomicI32::new(0);
static ERROR_COUNT: AtomicI32 = AtomicI32::new(0);
static FINALIZER_COUNT: AtomicI32 = AtomicI32::new(0);

fn call_r1(code: &str, value: Sexp) -> savvy::Result<Sexp> {
    let function = FunctionSexp::try_from(Sexp(eval_parse_text(code)?.inner()))?;
    let mut args = FunctionArgs::new();
    args.add("", value)?;
    let result = function.call(args)?;
    Ok(result.into())
}

fn call_r2(code: &str, first: Sexp, second: Sexp) -> savvy::Result<Sexp> {
    let function = FunctionSexp::try_from(Sexp(eval_parse_text(code)?.inner()))?;
    let mut args = FunctionArgs::new();
    args.add("", first)?;
    args.add("", second)?;
    let result = function.call(args)?;
    Ok(result.into())
}

#[savvy]
fn fixture_zero() -> savvy::Result<Sexp> {
    OwnedIntegerSexp::try_from_scalar(1)?.into()
}

#[savvy]
fn fixture_scalar(value: RealSexp) -> savvy::Result<Sexp> {
    if value.len() != 1 || value.as_slice()[0].is_na() {
        return Err(savvy_err!(
            "scalar input must be one non-missing REAL value"
        ));
    }
    OwnedRealSexp::try_from_scalar(value.as_slice()[0])?.into()
}

#[savvy]
fn fixture_numeric(value: RealSexp) -> savvy::Result<Sexp> {
    let mut result = OwnedRealSexp::new(value.len())?;
    for (index, element) in value.as_slice().iter().enumerate() {
        result.set_elt(index, element * 2.0)?;
    }
    result.into()
}

#[savvy]
fn fixture_altrep_integer(value: IntegerSexp) -> savvy::Result<Sexp> {
    let mut total = 0.0;
    for element in value.iter() {
        if element.is_na() {
            let mut result = OwnedRealSexp::new(1)?;
            result.set_na(0)?;
            return result.into();
        }
        total += *element as f64;
    }
    OwnedRealSexp::try_from_scalar(total)?.into()
}

#[savvy]
fn fixture_strings(value: StringSexp) -> savvy::Result<Sexp> {
    let count = value.iter().filter(|element| !element.is_na()).count() as i32;
    OwnedIntegerSexp::try_from_scalar(count)?.into()
}

#[savvy]
fn fixture_raw(value: RawSexp) -> savvy::Result<Sexp> {
    OwnedRawSexp::try_from_slice(value.as_slice())?.into()
}

#[savvy]
fn fixture_complex(value: ComplexSexp) -> savvy::Result<Sexp> {
    OwnedComplexSexp::try_from_slice(value.as_slice())?.into()
}

#[savvy]
fn fixture_logical_counts(value: LogicalSexp) -> savvy::Result<Sexp> {
    let mut counts = [0, 0, 0];
    for element in value.as_slice_raw() {
        if element.is_na() {
            counts[2] += 1;
        } else if *element == 1 {
            counts[1] += 1;
        } else {
            counts[0] += 1;
        }
    }
    let mut result = OwnedIntegerSexp::try_from_slice(counts)?;
    result.set_names(["false", "true", "missing"])?;
    result.into()
}

#[savvy]
fn fixture_schema(value: ListSexp) -> savvy::Result<Sexp> {
    if value.len() != 4
        || !value
            .names_iter()
            .eq(["id", "count", "ratio", "enabled"].into_iter())
    {
        return Err(savvy_err!("schema names do not match the declared order"));
    }
    let attributes_function_result = eval_parse_text("base::attributes")?;
    let attributes_function = FunctionSexp::try_from(Sexp(attributes_function_result.inner()))?;
    let mut attributes_args = FunctionArgs::new();
    attributes_args.add("", Sexp(value.inner()))?;
    let attributes_result = attributes_function.call(attributes_args)?;
    let attributes = ListSexp::try_from(Sexp(attributes_result.inner()))?;
    if attributes.len() != 1 || !attributes.names_iter().eq(["names"].into_iter()) {
        return Err(savvy_err!("schema must have names and no other attributes"));
    }
    let mut fields = value.values_iter();
    let id = fields.next().unwrap();
    let count = fields.next().unwrap();
    let ratio = fields.next().unwrap();
    let enabled = fields.next().unwrap();
    if !id.is_integer() || !count.is_integer() || !ratio.is_real() || !enabled.is_logical() {
        return Err(savvy_err!("schema fields do not match the declared types"));
    }
    let _: i32 = id.try_into()?;
    let _: i32 = count.try_into()?;
    let _: f64 = ratio.try_into()?;
    let _: bool = enabled.try_into()?;
    Ok(value.into())
}

#[savvy]
fn fixture_error(_trigger: f64) -> savvy::Result<()> {
    ERROR_COUNT.fetch_add(1, Ordering::Relaxed);
    Err(savvy_err!("fixture error"))
}

#[savvy]
fn fixture_lifecycle_reset() -> savvy::Result<()> {
    CONSTRUCTOR_COUNT.store(0, Ordering::Relaxed);
    METHOD_COUNT.store(0, Ordering::Relaxed);
    ERROR_COUNT.store(0, Ordering::Relaxed);
    FINALIZER_COUNT.store(0, Ordering::Relaxed);
    Ok(())
}

#[savvy]
fn fixture_lifecycle_counts() -> savvy::Result<Sexp> {
    let mut result = OwnedIntegerSexp::try_from_slice([
        CONSTRUCTOR_COUNT.load(Ordering::Relaxed),
        METHOD_COUNT.load(Ordering::Relaxed),
        ERROR_COUNT.load(Ordering::Relaxed),
        FINALIZER_COUNT.load(Ordering::Relaxed),
    ])?;
    result.set_names(["constructor", "method", "error", "finalizer"])?;
    result.into()
}

#[savvy]
fn fixture_outputs() -> savvy::Result<Sexp> {
    let mut numeric = OwnedRealSexp::try_from_slice([1.5, 0.0])?;
    numeric.set_na(1)?;
    let mut string = OwnedStringSexp::new(1)?;
    string.set_elt(0, "fixture")?;
    let raw = OwnedRawSexp::try_from_slice([1, 2, 3])?;
    let mut complex = OwnedComplexSexp::new(2)?;
    complex.set_elt(0, Complex64::new(1.0, 2.0))?;
    complex.set_na(1)?;
    let mut logical = OwnedLogicalSexp::try_from_slice([false, true, false])?;
    logical.set_na(2)?;
    let nested_value = OwnedIntegerSexp::try_from_scalar(7)?;
    let mut nested = OwnedListSexp::new(1, true)?;
    nested.set_name_and_value(0, "value", nested_value)?;
    let mut result = OwnedListSexp::new(6, true)?;
    result.set_name_and_value(0, "numeric", numeric)?;
    result.set_name_and_value(1, "string", string)?;
    result.set_name_and_value(2, "raw", raw)?;
    result.set_name_and_value(3, "complex", complex)?;
    result.set_name_and_value(4, "logical", logical)?;
    result.set_name_and_value(5, "list", nested)?;
    result.into()
}

#[savvy]
fn bench_vector_sum(x: RealSexp) -> savvy::Result<Sexp> {
    call_r1("base::sum", Sexp(x.inner()))
}

#[savvy]
fn bench_numeric_transform(x: RealSexp) -> savvy::Result<Sexp> {
    fixture_numeric(x)
}

#[savvy]
fn bench_broadcast(x: RealSexp, scalar: f64) -> savvy::Result<Sexp> {
    let mut total = 0.0;
    let mut correction = 0.0;
    for value in x.as_slice().iter().map(|value| value + scalar) {
        let adjusted = value - correction;
        let next = total + adjusted;
        correction = (next - total) - adjusted;
        total = next;
    }
    OwnedRealSexp::try_from_scalar(total)?.into()
}

#[savvy]
fn bench_sort(x: RealSexp) -> savvy::Result<Sexp> {
    let mut values = x.as_slice().to_vec();
    values.sort_by(|a, b| a.total_cmp(b));
    OwnedRealSexp::try_from_slice(values)?.into()
}

#[savvy]
fn bench_missing_mean(x: RealSexp) -> savvy::Result<Sexp> {
    call_r1("function(x) mean(x,na.rm=TRUE)", Sexp(x.inner()))
}

#[savvy]
fn bench_transpose(x: RealSexp) -> savvy::Result<Sexp> {
    let dimensions = x
        .get_dim()
        .ok_or_else(|| savvy_err!("transpose input must be a matrix"))?;
    let rows = dimensions[0] as usize;
    let columns = dimensions[1] as usize;
    let mut result = OwnedRealSexp::new(x.len())?;
    for column in 0..columns {
        for row in 0..rows {
            result.as_mut_slice()[column + row * columns] = x.as_slice()[row + column * rows];
        }
    }
    result.set_dim(&[columns, rows])?;
    result.into()
}

#[savvy]
fn bench_rowcol(x: RealSexp) -> savvy::Result<Sexp> {
    let dimensions = x
        .get_dim()
        .ok_or_else(|| savvy_err!("row and column input must be a matrix"))?;
    let rows = dimensions[0] as usize;
    let columns = dimensions[1] as usize;
    let mut row_means = OwnedRealSexp::new(rows)?;
    let mut column_sums = OwnedRealSexp::new(columns)?;
    for column in 0..columns {
        for row in 0..rows {
            let value = x.as_slice()[row + column * rows];
            row_means.as_mut_slice()[row] += value;
            column_sums.as_mut_slice()[column] += value;
        }
    }
    for value in row_means.as_mut_slice() {
        *value /= columns as f64;
    }
    let mut result = OwnedListSexp::new(2, true)?;
    result.set_name_and_value(0, "row_means", row_means)?;
    result.set_name_and_value(1, "column_sums", column_sums)?;
    result.into()
}

#[savvy]
fn bench_matmul(x: Sexp, y: Sexp) -> savvy::Result<Sexp> {
    call_r2("function(x,y) x %*% y", x, y)
}

#[savvy]
fn bench_dataframe(data: ListSexp) -> savvy::Result<Sexp> {
    let x = RealSexp::try_from(
        data.get("x")
            .ok_or_else(|| savvy_err!("data frame is missing x"))?,
    )?;
    let y = RealSexp::try_from(
        data.get("y")
            .ok_or_else(|| savvy_err!("data frame is missing y"))?,
    )?;
    let group_value = data
        .get("grp")
        .ok_or_else(|| savvy_err!("data frame is missing grp"))?;
    let group = IntegerSexp(group_value.0);
    let mut totals = [0.0; 10];
    for index in 0..x.len() {
        let value = x.as_slice()[index];
        if !value.is_nan() && value > 0.0 {
            totals[(group.as_slice()[index] - 1) as usize] += value / y.as_slice()[index];
        }
    }
    let groups = OwnedIntegerSexp::try_from_slice([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])?;
    let sums = OwnedRealSexp::try_from_slice(totals)?;
    let mut result = OwnedListSexp::new(2, true)?;
    result.set_name_and_value(0, "grp", groups)?;
    result.set_name_and_value(1, "z_sum", sums)?;
    result.set_class(["data.frame"])?;
    let mut row_names = OwnedIntegerSexp::new(2)?;
    row_names.set_na(0)?;
    row_names.as_mut_slice()[1] = -10;
    result.set_attrib("row.names", Sexp(row_names.inner()))?;
    result.into()
}

#[savvy]
fn bench_list_sum(x: ListSexp) -> savvy::Result<Sexp> {
    call_r1("function(x) sum(vapply(x,sum,numeric(1)))", Sexp(x.inner()))
}

#[savvy]
fn bench_string_concat(x: Sexp) -> savvy::Result<Sexp> {
    call_r1("function(x) paste(x,collapse=', ')", x)
}

#[savvy]
fn bench_string_metadata(x: Sexp) -> savvy::Result<Sexp> {
    call_r1(
        "function(x) { e <- Encoding(x); c(bytes=sum(nchar(x,type='bytes'),na.rm=TRUE),utf8=sum(e=='UTF-8'),latin1=sum(e=='latin1'),bytes_marked=sum(e=='bytes'),missing=sum(is.na(x))) }",
        x,
    )
}

#[savvy]
fn bench_factor(x: StringSexp) -> savvy::Result<Sexp> {
    let mut result = OwnedIntegerSexp::new(x.len())?;
    for (index, value) in x.iter().enumerate() {
        if value.is_na() {
            result.set_na(index)?;
        } else {
            let code = value
                .strip_prefix("level_")
                .and_then(|suffix| suffix.parse::<i32>().ok())
                .filter(|code| (1..=100).contains(code))
                .ok_or_else(|| savvy_err!("factor value is outside the declared levels"))?;
            result.as_mut_slice()[index] = code;
        }
    }
    let labels: Vec<String> = (1..=100).map(|code| format!("level_{code:03}")).collect();
    let levels = OwnedStringSexp::try_from_slice(labels)?;
    result.set_attrib("levels", Sexp(levels.inner()))?;
    result.set_class(["factor"])?;
    result.into()
}

#[savvy]
fn bench_attributes(x: RealSexp) -> savvy::Result<Sexp> {
    let mut copy = OwnedRealSexp::new(x.len())?;
    for (index, element) in x.as_slice().iter().enumerate() {
        copy.set_elt(index, *element)?;
    }
    let mut result = Sexp(copy.inner());
    result.set_class(["bench_class"])?;
    let creator = OwnedStringSexp::try_from_slice(["zigr_bench"])?;
    result.set_attrib("creator", Sexp(creator.inner()))?;
    drop(result.get_class());
    drop(result.get_attrib("creator")?);
    Ok(result)
}

#[savvy]
fn bench_s4(x: Sexp) -> savvy::Result<Sexp> {
    call_r1(
        "function(x) methods::slot(methods::new('BenchS4',slot_x=x),'slot_x')",
        x,
    )
}

#[savvy]
fn bench_logical_counts(x: LogicalSexp) -> savvy::Result<Sexp> {
    fixture_logical_counts(x)
}

#[savvy]
fn bench_raw_copy(x: RawSexp) -> savvy::Result<Sexp> {
    fixture_raw(x)
}

#[savvy]
fn bench_complex_conjugate(x: ComplexSexp) -> savvy::Result<Sexp> {
    let mut result = OwnedComplexSexp::new(x.len())?;
    for (index, value) in x.as_slice().iter().enumerate() {
        result.set_elt(index, Complex64::new(value.re, -value.im))?;
    }
    result.into()
}

#[savvy]
fn bench_schema(x: ListSexp) -> savvy::Result<Sexp> {
    fixture_schema(x)
}

#[savvy]
fn bench_altrep_sum(x: IntegerSexp) -> savvy::Result<Sexp> {
    fixture_altrep_integer(x)
}

#[savvy]
fn bench_altrep_index(x: IntegerSexp) -> savvy::Result<Sexp> {
    let total = (0..x.len())
        .step_by(32)
        .map(|i| x.as_slice()[i] as f64)
        .sum::<f64>();
    OwnedRealSexp::try_from_scalar(total)?.into()
}

#[savvy]
fn bench_altrep_materialize(x: IntegerSexp) -> savvy::Result<Sexp> {
    OwnedIntegerSexp::try_from_slice(x.as_slice())?.into()
}

#[savvy]
fn bench_eval(x: Sexp) -> savvy::Result<Sexp> {
    call_r1(
        "function(x) eval(quote(sum(x)+mean(x)),list2env(list(x=x),parent=baseenv()))",
        x,
    )
}

#[savvy]
fn bench_serialize(x: Sexp) -> savvy::Result<Sexp> {
    call_r1("function(x) unserialize(serialize(x,NULL,version=3L))", x)
}

#[savvy]
fn bench_rng(n: i32) -> savvy::Result<Sexp> {
    let value = OwnedIntegerSexp::try_from_scalar(n)?;
    call_r1("stats::rnorm", Sexp(value.inner()))
}

#[savvy]
fn bench_outputs() -> savvy::Result<Sexp> {
    fixture_outputs()
}

#[savvy]
struct FixtureState {
    value: i32,
}

#[savvy]
impl FixtureState {
    fn new() -> savvy::Result<Self> {
        CONSTRUCTOR_COUNT.fetch_add(1, Ordering::Relaxed);
        Ok(Self { value: 0 })
    }

    fn increment(&mut self, amount: i32) -> savvy::Result<Sexp> {
        METHOD_COUNT.fetch_add(1, Ordering::Relaxed);
        self.value += amount;
        OwnedIntegerSexp::try_from_scalar(self.value)?.into()
    }

    fn read(&self) -> savvy::Result<Sexp> {
        METHOD_COUNT.fetch_add(1, Ordering::Relaxed);
        OwnedIntegerSexp::try_from_scalar(self.value)?.into()
    }
}

impl Drop for FixtureState {
    fn drop(&mut self) {
        FINALIZER_COUNT.fetch_add(1, Ordering::Relaxed);
    }
}
