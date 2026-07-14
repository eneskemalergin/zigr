#include <Rcpp.h>

#include <algorithm>
#include <string>
#include <vector>

namespace {

struct FixtureLifecycleCounts {
  int constructor = 0;
  int method = 0;
  int error = 0;
  int finalizer = 0;
};

FixtureLifecycleCounts lifecycle_counts;

void require_length(R_xlen_t actual, R_xlen_t expected, const char* label) {
  if (actual != expected) {
    Rcpp::stop("%s must have length %lld", label, static_cast<long long>(expected));
  }
}

void validate_schema(const Rcpp::List& value) {
  require_length(value.size(), 4, "schema");
  std::vector<std::string> attribute_names = value.attributeNames();
  if (attribute_names.size() != 1 || attribute_names[0] != "names") {
    Rcpp::stop("schema must have names and no other attributes");
  }

  Rcpp::CharacterVector names = value.names();
  const char* expected_names[] = {"id", "count", "ratio", "enabled"};
  require_length(names.size(), 4, "schema names");
  for (R_xlen_t index = 0; index < names.size(); ++index) {
    if (Rcpp::CharacterVector::is_na(names[index]) ||
        Rcpp::as<std::string>(names[index]) != expected_names[index]) {
      Rcpp::stop("schema names do not match the declared order");
    }
  }

  if (!Rcpp::is<Rcpp::IntegerVector>(value[0]) ||
      !Rcpp::is<Rcpp::IntegerVector>(value[1]) ||
      !Rcpp::is<Rcpp::NumericVector>(value[2]) ||
      !Rcpp::is<Rcpp::LogicalVector>(value[3])) {
    Rcpp::stop("schema fields do not match the declared types");
  }
  Rcpp::IntegerVector id(value[0]);
  Rcpp::IntegerVector count(value[1]);
  Rcpp::NumericVector ratio(value[2]);
  Rcpp::LogicalVector enabled(value[3]);
  require_length(id.size(), 1, "schema id");
  require_length(count.size(), 1, "schema count");
  require_length(ratio.size(), 1, "schema ratio");
  require_length(enabled.size(), 1, "schema enabled");
  if (Rcpp::IntegerVector::is_na(id[0]) || Rcpp::IntegerVector::is_na(count[0]) ||
      R_IsNA(ratio[0]) || Rcpp::LogicalVector::is_na(enabled[0])) {
    Rcpp::stop("schema scalar fields must not be NA");
  }
}

class FixtureState {
 public:
  FixtureState() : value_(0) { ++lifecycle_counts.constructor; }

  ~FixtureState() { ++lifecycle_counts.finalizer; }

  int increment(Rcpp::RObject amount) {
    if (!Rcpp::is<Rcpp::IntegerVector>(amount)) {
      Rcpp::stop("fixture method amount must be an INTEGER vector");
    }
    Rcpp::IntegerVector scalar(amount);
    require_length(scalar.size(), 1, "fixture method amount");
    if (Rcpp::IntegerVector::is_na(scalar[0])) {
      Rcpp::stop("fixture method amount must not be NA");
    }
    ++lifecycle_counts.method;
    value_ += scalar[0];
    return value_;
  }

  int read() const {
    ++lifecycle_counts.method;
    return value_;
  }

 private:
  int value_;
};

}  // namespace

// [[Rcpp::export]]
int fixture_zero() { return 1; }

// [[Rcpp::export]]
double fixture_scalar(Rcpp::RObject value) {
  if (!Rcpp::is<Rcpp::NumericVector>(value)) {
    Rcpp::stop("scalar input must be a REAL vector");
  }
  Rcpp::NumericVector scalar(value);
  require_length(scalar.size(), 1, "scalar input");
  if (R_IsNA(scalar[0])) Rcpp::stop("scalar input must not be NA");
  return scalar[0];
}

// [[Rcpp::export]]
Rcpp::NumericVector fixture_numeric(Rcpp::RObject value) {
  if (!Rcpp::is<Rcpp::NumericVector>(value)) {
    Rcpp::stop("numeric fixture expected a REAL vector");
  }
  Rcpp::NumericVector typed(value);
  Rcpp::NumericVector result(typed.size());
  for (R_xlen_t index = 0; index < typed.size(); ++index) result[index] = typed[index] * 2.0;
  return result;
}

// [[Rcpp::export]]
double fixture_altrep_integer(Rcpp::RObject value) {
  if (!Rcpp::is<Rcpp::IntegerVector>(value)) {
    Rcpp::stop("ALTREP fixture expected an INTEGER vector");
  }
  Rcpp::IntegerVector typed(value);
  double total = 0.0;
  for (int element : typed) {
    if (element == NA_INTEGER) return NA_REAL;
    total += element;
  }
  return total;
}

// [[Rcpp::export]]
int fixture_strings(Rcpp::RObject value) {
  if (!Rcpp::is<Rcpp::CharacterVector>(value)) {
    Rcpp::stop("string fixture expected a CHARACTER vector");
  }
  Rcpp::CharacterVector typed(value);
  int count = 0;
  for (R_xlen_t index = 0; index < typed.size(); ++index) {
    if (!Rcpp::CharacterVector::is_na(typed[index])) ++count;
  }
  return count;
}

// [[Rcpp::export]]
Rcpp::RawVector fixture_raw(Rcpp::RObject value) {
  if (!Rcpp::is<Rcpp::RawVector>(value)) {
    Rcpp::stop("raw fixture expected a RAWSXP vector");
  }
  Rcpp::RawVector typed(value);
  Rcpp::RawVector result(typed.size());
  std::copy(typed.begin(), typed.end(), result.begin());
  return result;
}

// [[Rcpp::export]]
Rcpp::ComplexVector fixture_complex(Rcpp::RObject value) {
  if (!Rcpp::is<Rcpp::ComplexVector>(value)) {
    Rcpp::stop("complex fixture expected a CPLXSXP vector");
  }
  Rcpp::ComplexVector typed(value);
  Rcpp::ComplexVector result(typed.size());
  std::copy(typed.begin(), typed.end(), result.begin());
  return result;
}

// [[Rcpp::export]]
Rcpp::IntegerVector fixture_logical_counts(Rcpp::RObject value) {
  if (!Rcpp::is<Rcpp::LogicalVector>(value)) {
    Rcpp::stop("logical fixture expected a LGLSXP vector");
  }
  Rcpp::LogicalVector typed(value);
  Rcpp::IntegerVector counts = Rcpp::IntegerVector::create(0, 0, 0);
  for (int element : typed) {
    if (element == NA_LOGICAL) {
      ++counts[2];
    } else if (element) {
      ++counts[1];
    } else {
      ++counts[0];
    }
  }
  counts.names() = Rcpp::CharacterVector::create("false", "true", "missing");
  return counts;
}

// [[Rcpp::export]]
Rcpp::List fixture_schema(Rcpp::List value) {
  validate_schema(value);
  return value;
}

// [[Rcpp::export]]
void fixture_error(double trigger) {
  (void)trigger;
  ++lifecycle_counts.error;
  Rcpp::stop("fixture error");
}

// [[Rcpp::export]]
void fixture_lifecycle_reset() { lifecycle_counts = FixtureLifecycleCounts{}; }

// [[Rcpp::export]]
Rcpp::IntegerVector fixture_lifecycle_counts() {
  return Rcpp::IntegerVector::create(
      Rcpp::Named("constructor") = lifecycle_counts.constructor,
      Rcpp::Named("method") = lifecycle_counts.method,
      Rcpp::Named("error") = lifecycle_counts.error,
      Rcpp::Named("finalizer") = lifecycle_counts.finalizer);
}

// [[Rcpp::export]]
Rcpp::List fixture_outputs() {
  Rcpp::ComplexVector complex(2);
  complex[0] = Rcomplex{1.0, 2.0};
  complex[1] = Rcomplex{NA_REAL, NA_REAL};
  return Rcpp::List::create(
      Rcpp::Named("numeric") = Rcpp::NumericVector::create(1.5, NA_REAL),
      Rcpp::Named("string") = Rcpp::CharacterVector::create("fixture"),
      Rcpp::Named("raw") = Rcpp::RawVector::create(1, 2, 3),
      Rcpp::Named("complex") = complex,
      Rcpp::Named("logical") = Rcpp::LogicalVector::create(false, true, NA_LOGICAL),
      Rcpp::Named("list") = Rcpp::List::create(Rcpp::Named("value") = 7));
}

RCPP_MODULE(zigr_fixture_module) {
  Rcpp::class_<FixtureState>("FixtureState")
      .constructor()
      .method("increment", &FixtureState::increment)
      .method("read", &FixtureState::read);
}
