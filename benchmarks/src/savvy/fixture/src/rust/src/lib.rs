use savvy::*;
use std::sync::atomic::{AtomicI32, Ordering};

static CONSTRUCTOR_COUNT: AtomicI32 = AtomicI32::new(0);
static METHOD_COUNT: AtomicI32 = AtomicI32::new(0);
static ERROR_COUNT: AtomicI32 = AtomicI32::new(0);
static FINALIZER_COUNT: AtomicI32 = AtomicI32::new(0);

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
