use extendr_api::prelude::*;

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
    (0..value.len())
        .map(|index| value.elt(index).0 as f64)
        .sum()
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
    Err(fixture_failure("fixture error"))
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
struct FixtureState {
    value: i32,
}

#[extendr]
impl FixtureState {
    fn new() -> Self {
        Self { value: 0 }
    }

    fn increment(&mut self, amount: i32) -> i32 {
        self.value += amount;
        self.value
    }

    fn read(&self) -> i32 {
        self.value
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
    fn fixture_outputs;
    impl FixtureState;
}
