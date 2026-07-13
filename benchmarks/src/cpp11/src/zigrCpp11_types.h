#pragma once

#include <cpp11.hpp>

struct FixtureState {
  int value;

  explicit FixtureState(int initial_value) : value(initial_value) {}
};
