#include <cpp11.hpp>

#include <string>

#include "zigrCpp11_types.h"

using namespace cpp11::literals;

namespace {

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

[[cpp11::register]] void fixture_error(double trigger) {
  (void)trigger;
  cpp11::stop("fixture error");
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
