#include <Rcpp.h>

#include <algorithm>
#include <cmath>
#include <numeric>
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

// [[Rcpp::export]]
double bench_vector_sum(Rcpp::NumericVector x) {
  return std::accumulate(x.begin(), x.end(), 0.0);
}

// [[Rcpp::export]]
Rcpp::NumericVector bench_numeric_transform(Rcpp::NumericVector x) {
  return fixture_numeric(x);
}

// [[Rcpp::export]]
double bench_broadcast(Rcpp::NumericVector x, double scalar) {
  long double total = 0.0;
  for (double value : x) total += value + scalar;
  return static_cast<double>(total);
}

// [[Rcpp::export]]
Rcpp::NumericVector bench_sort(Rcpp::NumericVector x) {
  Rcpp::NumericVector result = Rcpp::clone(x);
  std::sort(result.begin(), result.end());
  return result;
}

// [[Rcpp::export]]
double bench_missing_mean(Rcpp::NumericVector x) {
  double total = 0.0;
  R_xlen_t count = 0;
  for (double value : x) if (!R_IsNA(value) && !R_IsNaN(value)) { total += value; ++count; }
  return total / static_cast<double>(count);
}

// [[Rcpp::export]]
Rcpp::NumericMatrix bench_transpose(Rcpp::NumericMatrix x) {
  Rcpp::NumericMatrix result(x.ncol(), x.nrow());
  for (int col = 0; col < x.ncol(); ++col) for (int row = 0; row < x.nrow(); ++row) result(col, row) = x(row, col);
  return result;
}

// [[Rcpp::export]]
Rcpp::List bench_rowcol(Rcpp::NumericMatrix x) {
  Rcpp::NumericVector rows(x.nrow()), columns(x.ncol());
  for (int col = 0; col < x.ncol(); ++col) for (int row = 0; row < x.nrow(); ++row) {
    rows[row] += x(row, col) / x.ncol();
    columns[col] += x(row, col);
  }
  return Rcpp::List::create(Rcpp::Named("row_means") = rows, Rcpp::Named("column_sums") = columns);
}

// [[Rcpp::export]]
SEXP bench_matmul(Rcpp::NumericMatrix x, Rcpp::NumericMatrix y) {
  return Rcpp::Function("%*%", R_BaseEnv)(x, y);
}

// [[Rcpp::export]]
Rcpp::DataFrame bench_dataframe(Rcpp::DataFrame data) {
  Rcpp::NumericVector x = data["x"], y = data["y"];
  Rcpp::IntegerVector group = data["grp"];
  Rcpp::NumericVector totals(10);
  for (R_xlen_t i = 0; i < x.size(); ++i) if (!R_IsNA(x[i]) && !R_IsNaN(x[i]) && x[i] > 0) totals[group[i] - 1] += x[i] / y[i];
  return Rcpp::DataFrame::create(Rcpp::Named("grp") = Rcpp::seq(1, 10), Rcpp::Named("z_sum") = totals);
}

// [[Rcpp::export]]
double bench_list_sum(Rcpp::List x) {
  double total = 0.0;
  for (SEXP item : x) { Rcpp::NumericVector values(item); total = std::accumulate(values.begin(), values.end(), total); }
  return total;
}

// [[Rcpp::export]]
SEXP bench_string_concat(Rcpp::CharacterVector x) {
  return Rcpp::Function("paste", R_BaseEnv)(x, Rcpp::Named("collapse") = ", ");
}

// [[Rcpp::export]]
Rcpp::IntegerVector bench_string_metadata(Rcpp::CharacterVector x) {
  int bytes = 0, utf8 = 0, latin1 = 0, marked = 0, missing = 0;
  for (R_xlen_t i = 0; i < x.size(); ++i) {
    SEXP value = x[i];
    if (value == NA_STRING) { ++missing; continue; }
    bytes += LENGTH(value);
    switch (Rf_getCharCE(value)) {
      case CE_UTF8: ++utf8; break;
      case CE_LATIN1: ++latin1; break;
      case CE_BYTES: ++marked; break;
      default: break;
    }
  }
  return Rcpp::IntegerVector::create(Rcpp::Named("bytes") = bytes, Rcpp::Named("utf8") = utf8,
    Rcpp::Named("latin1") = latin1, Rcpp::Named("bytes_marked") = marked, Rcpp::Named("missing") = missing);
}

// [[Rcpp::export]]
SEXP bench_factor(Rcpp::CharacterVector x) {
  return Rcpp::Function("factor", R_BaseEnv)(x, Rcpp::Named("levels") = Rcpp::Function("sprintf", R_BaseEnv)("level_%03d", Rcpp::seq(1, 100)));
}

// [[Rcpp::export]]
SEXP bench_attributes(SEXP x) {
  SEXP result = PROTECT(Rf_duplicate(x));
  Rf_setAttrib(result, R_ClassSymbol, Rcpp::CharacterVector::create("bench_class"));
  Rf_setAttrib(result, Rf_install("creator"), Rcpp::CharacterVector::create("zigr_bench"));
  (void) Rf_getAttrib(result, R_ClassSymbol);
  (void) Rf_getAttrib(result, Rf_install("creator"));
  UNPROTECT(1);
  return result;
}

// [[Rcpp::export]]
SEXP bench_s4(Rcpp::NumericVector x) {
  SEXP object = Rcpp::Function("new", Rcpp::Environment::namespace_env("methods"))("BenchS4", Rcpp::Named("slot_x") = x);
  return R_do_slot(object, Rf_install("slot_x"));
}

// [[Rcpp::export]]
Rcpp::IntegerVector bench_logical_counts(Rcpp::LogicalVector x) { return fixture_logical_counts(x); }

// [[Rcpp::export]]
Rcpp::RawVector bench_raw_copy(Rcpp::RawVector x) { return fixture_raw(x); }

// [[Rcpp::export]]
Rcpp::ComplexVector bench_complex_conjugate(Rcpp::ComplexVector x) {
  Rcpp::ComplexVector result(x.size());
  for (R_xlen_t i = 0; i < x.size(); ++i) result[i] = Rcomplex{x[i].r, -x[i].i};
  return result;
}

// [[Rcpp::export]]
Rcpp::List bench_schema(Rcpp::List x) { return fixture_schema(x); }

// [[Rcpp::export]]
double bench_altrep_sum(Rcpp::IntegerVector x) { return fixture_altrep_integer(x); }

// [[Rcpp::export]]
double bench_altrep_index(Rcpp::IntegerVector x) {
  double total = 0.0;
  for (R_xlen_t i = 0; i < x.size(); i += 10000) total += x[i];
  return total;
}

// [[Rcpp::export]]
Rcpp::IntegerVector bench_altrep_materialize(Rcpp::IntegerVector x) { return Rcpp::clone(x); }

// [[Rcpp::export]]
double bench_eval(Rcpp::NumericVector x) {
  Rcpp::Environment env = Rcpp::new_env(R_BaseEnv);
  env["x"] = x;
  SEXP expression = Rcpp::Function("parse", R_BaseEnv)(Rcpp::Named("text") = "sum(x) + mean(x)");
  return Rcpp::as<double>(Rcpp::Function("eval", R_BaseEnv)(expression, env));
}

// [[Rcpp::export]]
SEXP bench_serialize(SEXP x) {
  Rcpp::RawVector bytes = Rcpp::Function("serialize", R_BaseEnv)(x, R_NilValue, Rcpp::Named("version") = 3);
  return Rcpp::Function("unserialize", R_BaseEnv)(bytes);
}

// [[Rcpp::export]]
Rcpp::NumericVector bench_rng(int n) { return Rcpp::rnorm(n); }

// [[Rcpp::export]]
Rcpp::List bench_outputs() { return fixture_outputs(); }

RCPP_MODULE(zigr_fixture_module) {
  Rcpp::class_<FixtureState>("FixtureState")
      .constructor()
      .method("increment", &FixtureState::increment)
      .method("read", &FixtureState::read);
}
