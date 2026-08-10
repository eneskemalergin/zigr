#include <cpp11.hpp>

#include <algorithm>
#include <cfloat>
#include <cmath>
#include <string>
#include <vector>

#include "zigrCpp11_types.h"

using namespace cpp11::literals;

namespace {

struct FixtureLifecycleCounts {
  int constructor = 0;
  int method = 0;
  int error = 0;
  int finalizer = 0;
};

FixtureLifecycleCounts lifecycle_counts;

double exact_real_sum(const cpp11::doubles& values) {
  long double total = 0.0;
  bool na_seen = false, nan_seen = false;
  for (double value : values) {
    if (ISNAN(value)) {
      if (ISNA(value)) na_seen = true;
      else nan_seen = true;
    } else {
      total += static_cast<long double>(value);
    }
  }
  if (na_seen) return NA_REAL;
  if (nan_seen) return R_NaN;
  if (total > DBL_MAX) return R_PosInf;
  if (total < -DBL_MAX) return R_NegInf;
  return static_cast<double>(total);
}

double exact_real_mean_narm(const cpp11::doubles& values) {
  long double total = 0.0;
  R_xlen_t count = 0;
  for (double value : values) {
    if (!ISNAN(value)) {
      total += static_cast<long double>(value);
      ++count;
    }
  }
  if (count == 0) return R_NaN;

  long double divisor = static_cast<long double>(count);
  bool finite_total = R_FINITE(static_cast<double>(total));
  long double result;
  if (finite_total) {
    result = total / divisor;
  } else {
    long double scaled_total = 0.0;
    double scaled_divisor = static_cast<double>(count);
    for (double value : values) {
      if (!ISNAN(value)) scaled_total += static_cast<long double>(value / scaled_divisor);
    }
    result = scaled_total;
  }
  if (R_FINITE(static_cast<double>(result))) {
    long double correction = 0.0;
    for (double value : values) {
      if (ISNAN(value)) continue;
      correction += finite_total
        ? static_cast<long double>(value) - result
        : (static_cast<long double>(value) - result) / divisor;
    }
    result += finite_total ? correction / divisor : correction;
  }
  return static_cast<double>(result);
}

void require_length(R_xlen_t actual, R_xlen_t expected, const char* label) {
  if (actual != expected) {
    cpp11::stop("%s must have length %lld", label, static_cast<long long>(expected));
  }
}

void require_non_missing_real(double value, const char* label) {
  if (cpp11::is_na(value)) {
    cpp11::stop("%s must not be NA", label);
  }
}

bool is_na_string(cpp11::r_string value) {
  return cpp11::is_na(value);
}

void validate_schema(const cpp11::list& value) {
  require_length(value.size(), 4, "schema");

  cpp11::sexp attributes = cpp11::package("base")["attributes"](value);
  if (attributes == R_NilValue) {
    cpp11::stop("schema must have names and no other attributes");
  }
  cpp11::list attribute_list(attributes);
  cpp11::strings attribute_names(attribute_list.names());
  if (attribute_list.size() != 1 || attribute_names.size() != 1 ||
      static_cast<std::string>(attribute_names[0]) != "names") {
    cpp11::stop("schema must have names and no other attributes");
  }

  cpp11::strings names(value.names());
  const char* expected_names[] = {"id", "count", "ratio", "enabled"};
  require_length(names.size(), 4, "schema names");
  for (R_xlen_t index = 0; index < names.size(); ++index) {
    if (is_na_string(names[index]) ||
        static_cast<std::string>(names[index]) != expected_names[index]) {
      cpp11::stop("schema names do not match the declared order");
    }
  }

  cpp11::integers id(value[0]);
  cpp11::integers count(value[1]);
  cpp11::doubles ratio(value[2]);
  cpp11::logicals enabled(value[3]);
  require_length(id.size(), 1, "schema id");
  require_length(count.size(), 1, "schema count");
  require_length(ratio.size(), 1, "schema ratio");
  require_length(enabled.size(), 1, "schema enabled");
  const cpp11::r_bool enabled_value = enabled[0];
  if (cpp11::is_na(id[0]) || cpp11::is_na(count[0]) ||
      cpp11::is_na(ratio[0]) || cpp11::is_na(enabled_value)) {
    cpp11::stop("schema scalar fields must not be NA");
  }
}

int optional_real_state(cpp11::sexp value) {
  SEXP data = value.data();
  if (data == R_NilValue) {
    return 0;
  }
  cpp11::doubles typed_value(data);
  require_length(typed_value.size(), 1, "optional input");
  return cpp11::is_na(typed_value[0]) ? 0 : 1;
}

double numeric_sum(const cpp11::doubles& value) {
  double total = 0.0;
  for (double element : value) {
    total += element;
  }
  return total;
}

double integer_sum(const cpp11::integers& value) {
  double total = 0.0;
  for (int element : value) {
    if (element == NA_INTEGER) return NA_REAL;
    total += element;
  }
  return total;
}

}  // namespace

[[cpp11::register]] int fixture_zero() { return 1; }

[[cpp11::register]] double fixture_scalar(cpp11::doubles value) {
  require_length(value.size(), 1, "scalar input");
  require_non_missing_real(value[0], "scalar input");
  return value[0];
}

[[cpp11::register]] cpp11::integers fixture_logical_counts(
    cpp11::logicals value) {
  cpp11::writable::integers counts(3);
  counts[0] = 0;
  counts[1] = 0;
  counts[2] = 0;
  for (cpp11::r_bool element : value) {
    if (cpp11::is_na(element)) {
      ++counts[2];
    } else if (element) {
      ++counts[1];
    } else {
      ++counts[0];
    }
  }
  counts.names() = {"false", "true", "missing"};
  return counts;
}

[[cpp11::register]] int fixture_optional(cpp11::sexp value) {
  return optional_real_state(value);
}

[[cpp11::register]] cpp11::doubles fixture_numeric(cpp11::doubles value) {
  cpp11::writable::doubles result(value.size());
  for (R_xlen_t index = 0; index < value.size(); ++index) {
    result[index] = value[index] * 2.0;
  }
  return result;
}

[[cpp11::register]] double fixture_altrep_integer(cpp11::integers value) {
  return integer_sum(value);
}

[[cpp11::register]] int fixture_strings(cpp11::strings value) {
  int count = 0;
  for (cpp11::r_string element : value) {
    if (!is_na_string(element)) {
      ++count;
    }
  }
  return count;
}

[[cpp11::register]] cpp11::raws fixture_raw(cpp11::raws value) {
  cpp11::writable::raws result(value.size());
  for (R_xlen_t index = 0; index < value.size(); ++index) {
    result[index] = value[index];
  }
  return result;
}

[[cpp11::register]] cpp11::list fixture_schema(cpp11::list value) {
  validate_schema(value);
  return value;
}

[[cpp11::register]] cpp11::external_pointer<FixtureState> diagnostic_state_new() {
  return cpp11::external_pointer<FixtureState>(new FixtureState(0));
}

[[cpp11::register]] int diagnostic_state_method(
    cpp11::external_pointer<FixtureState> state, int amount) {
  if (state.get() == nullptr) {
    cpp11::stop("fixture state pointer is cleared");
  }
  state->value += amount;
  return state->value;
}

[[cpp11::register]] int diagnostic_state_read(
    cpp11::external_pointer<FixtureState> state) {
  if (state.get() == nullptr) cpp11::stop("fixture state pointer is cleared");
  return state->value;
}

[[cpp11::register]] void fixture_error(double trigger) {
  (void)trigger;
  ++lifecycle_counts.error;
  cpp11::stop("fixture error");
}

[[cpp11::register]] void fixture_lifecycle_reset() {
  lifecycle_counts = FixtureLifecycleCounts{};
}

[[cpp11::register]] cpp11::integers fixture_lifecycle_counts() {
  cpp11::writable::integers result(4);
  result[0] = lifecycle_counts.constructor;
  result[1] = lifecycle_counts.method;
  result[2] = lifecycle_counts.error;
  result[3] = lifecycle_counts.finalizer;
  result.names() = {"constructor", "method", "error", "finalizer"};
  return result;
}

[[cpp11::register]] double boundary_zero() { return 1.0; }

[[cpp11::register]] double boundary_numeric(cpp11::doubles value) {
  return numeric_sum(value);
}

[[cpp11::register]] int boundary_string(cpp11::strings value) {
  int total = 0;
  for (cpp11::r_string element : value) {
    if (!is_na_string(element)) {
      ++total;
    }
  }
  return total;
}

[[cpp11::register]] int boundary_raw(cpp11::raws value) {
  int total = 0;
  for (Rbyte element : value) {
    total += element;
  }
  return total;
}

[[cpp11::register]] cpp11::external_pointer<FixtureState>
bench_external_ptr(cpp11::integers value) {
  require_length(value.size(), 1, "external pointer input");
  return cpp11::external_pointer<FixtureState>(new FixtureState(value[0]));
}

[[cpp11::register]] double bench_vector_sum(cpp11::doubles x) {
  return exact_real_sum(x);
}

[[cpp11::register]] cpp11::doubles bench_numeric_transform(cpp11::doubles x) {
  return fixture_numeric(x);
}

[[cpp11::register]] double bench_broadcast(cpp11::doubles x, double scalar) {
  long double total = 0.0;
  for (double value : x) total += value + scalar;
  return static_cast<double>(total);
}

[[cpp11::register]] cpp11::doubles bench_sort(cpp11::doubles x) {
  std::vector<double> sorted(x.begin(), x.end());
  std::sort(sorted.begin(), sorted.end());
  cpp11::writable::doubles result(sorted.size());
  for (R_xlen_t i = 0; i < result.size(); ++i) result[i] = sorted[i];
  return result;
}

[[cpp11::register]] double bench_missing_mean(cpp11::doubles x) {
  return exact_real_mean_narm(x);
}

[[cpp11::register]] cpp11::sexp bench_transpose(cpp11::sexp x) {
  return cpp11::package("base")["t"](x);
}

[[cpp11::register]] cpp11::sexp bench_rowcol(cpp11::sexp x) {
  cpp11::writable::list result(2);
  result[0] = cpp11::package("base")["rowMeans"](x);
  result[1] = cpp11::package("base")["colSums"](x);
  result.names() = {"row_means", "column_sums"};
  return result;
}

[[cpp11::register]] cpp11::sexp bench_matmul(cpp11::sexp x, cpp11::sexp y) {
  return cpp11::package("base")["%*%"](x, y);
}

[[cpp11::register]] cpp11::sexp bench_dataframe(cpp11::list data) {
  cpp11::doubles x(data[0]), y(data[1]);
  cpp11::integers group(data[2]);
  cpp11::writable::doubles totals(10);
  cpp11::writable::integers groups(10);
  for (R_xlen_t i = 0; i < 10; ++i) { totals[i] = 0.0; groups[i] = i + 1; }
  for (R_xlen_t i = 0; i < x.size(); ++i) {
    if (!R_IsNA(x[i]) && !R_IsNaN(x[i]) && x[i] > 0.0) totals[group[i] - 1] += x[i] / y[i];
  }
  return cpp11::package("base")["data.frame"]("grp"_nm = groups, "z_sum"_nm = totals);
}

[[cpp11::register]] double bench_list_sum(cpp11::list x) {
  long double total = 0.0;
  bool na_seen = false, nan_seen = false;
  for (SEXP item : x) {
    double item_total = exact_real_sum(cpp11::doubles(item));
    if (ISNA(item_total)) na_seen = true;
    else if (ISNAN(item_total)) nan_seen = true;
    else total += static_cast<long double>(item_total);
  }
  if (na_seen) return NA_REAL;
  if (nan_seen) return R_NaN;
  if (total > DBL_MAX) return R_PosInf;
  if (total < -DBL_MAX) return R_NegInf;
  return static_cast<double>(total);
}

[[cpp11::register]] cpp11::sexp bench_string_concat(cpp11::strings x) {
  return cpp11::package("base")["paste"](x, "collapse"_nm = ", ");
}

[[cpp11::register]] cpp11::integers bench_string_metadata(cpp11::strings x) {
  cpp11::writable::integers result(5);
  for (R_xlen_t i = 0; i < result.size(); ++i) result[i] = 0;
  for (SEXP value : x) {
    if (value == NA_STRING) { ++result[4]; continue; }
    result[0] += LENGTH(value);
    switch (Rf_getCharCE(value)) {
      case CE_UTF8: ++result[1]; break;
      case CE_LATIN1: ++result[2]; break;
      case CE_BYTES: ++result[3]; break;
      default: break;
    }
  }
  result.names() = {"bytes", "utf8", "latin1", "bytes_marked", "missing"};
  return result;
}

[[cpp11::register]] cpp11::sexp bench_factor(cpp11::strings x) {
  cpp11::sexp levels = cpp11::package("base")["sprintf"]("level_%03d", cpp11::package("base")["seq_len"](100));
  return cpp11::package("base")["factor"](x, "levels"_nm = levels);
}

[[cpp11::register]] cpp11::sexp bench_attributes(cpp11::sexp x) {
  cpp11::sexp result(Rf_duplicate(x));
  Rf_setAttrib(result, R_ClassSymbol, cpp11::as_sexp(cpp11::writable::strings({"bench_class"})));
  Rf_setAttrib(result, Rf_install("creator"), cpp11::as_sexp(cpp11::writable::strings({"zigr_bench"})));
  (void) Rf_getAttrib(result, R_ClassSymbol);
  (void) Rf_getAttrib(result, Rf_install("creator"));
  return result;
}

[[cpp11::register]] cpp11::sexp bench_s4(cpp11::sexp x) {
  cpp11::sexp object = cpp11::package("methods")["new"]("BenchS4", "slot_x"_nm = x);
  return R_do_slot(object, Rf_install("slot_x"));
}

[[cpp11::register]] cpp11::integers bench_logical_counts(cpp11::logicals x) { return fixture_logical_counts(x); }
[[cpp11::register]] cpp11::raws bench_raw_copy(cpp11::raws x) { return fixture_raw(x); }

[[cpp11::register]] cpp11::sexp bench_complex_conjugate(cpp11::sexp x) {
  R_xlen_t n = Rf_xlength(x);
  cpp11::sexp result = cpp11::package("base")["complex"]("length.out"_nm = n);
  Rcomplex* source = COMPLEX(x);
  Rcomplex* target = COMPLEX(result);
  for (R_xlen_t i = 0; i < n; ++i) target[i] = Rcomplex{source[i].r, -source[i].i};
  return result;
}

[[cpp11::register]] cpp11::list bench_schema(cpp11::list x) { return fixture_schema(x); }
[[cpp11::register]] double bench_altrep_sum(cpp11::integers x) { return integer_sum(x); }

[[cpp11::register]] double bench_altrep_index(cpp11::integers x) {
  double total = 0.0;
  for (R_xlen_t i = 0; i < x.size(); i += 32) total += x[i];
  return total;
}

[[cpp11::register]] cpp11::integers bench_altrep_materialize(cpp11::integers x) {
  cpp11::writable::integers result(x.size());
  for (R_xlen_t i = 0; i < x.size(); ++i) result[i] = x[i];
  return result;
}

[[cpp11::register]] cpp11::sexp bench_eval(cpp11::sexp x) {
  cpp11::writable::list bindings({x});
  bindings.names() = {"x"};
  cpp11::sexp environment = cpp11::package("base")["list2env"](bindings, "parent"_nm = cpp11::package("base")["baseenv"]());
  cpp11::sexp expression = cpp11::package("base")["parse"]("text"_nm = "sum(x) + mean(x)");
  return cpp11::package("base")["eval"](expression, environment);
}

[[cpp11::register]] cpp11::sexp bench_serialize(cpp11::sexp x) {
  cpp11::sexp bytes = cpp11::package("base")["serialize"](x, R_NilValue, "version"_nm = 3);
  return cpp11::package("base")["unserialize"](bytes);
}

[[cpp11::register]] cpp11::sexp bench_rng(int n) { return cpp11::package("stats")["rnorm"](n); }

[[cpp11::register]] cpp11::sexp bench_outputs() {
  cpp11::writable::doubles numeric({1.5, NA_REAL});
  cpp11::writable::strings string({"fixture"});
  cpp11::writable::raws raw({1, 2, 3});
  cpp11::writable::doubles real({1.0, NA_REAL}), imaginary({2.0, NA_REAL});
  cpp11::sexp complex = cpp11::package("base")["complex"]("real"_nm = real, "imaginary"_nm = imaginary);
  cpp11::writable::logicals logical({false, true, NA_LOGICAL});
  cpp11::writable::list nested({cpp11::as_sexp(7)});
  nested.names() = {"value"};
  cpp11::writable::list result({numeric, string, raw, complex, logical, nested});
  result.names() = {"numeric", "string", "raw", "complex", "logical", "list"};
  return result;
}
