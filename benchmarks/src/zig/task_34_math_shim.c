#include <dlfcn.h>
#include <math.h>
#include <stddef.h>
#include <stdlib.h>

typedef double (*task34_math_fn)(double);

static void *task34_math_handle = NULL;
static task34_math_fn task34_log_fn = NULL;
static task34_math_fn task34_exp_fn = NULL;
static task34_math_fn task34_sqrt_fn = NULL;

static task34_math_fn task34_load_fn(const char *name) {
  if (task34_math_handle == NULL) {
    task34_math_handle = dlopen("libm.so.6", RTLD_LAZY | RTLD_LOCAL);
    if (task34_math_handle == NULL) abort();
  }

  task34_math_fn fn = (task34_math_fn)dlsym(task34_math_handle, name);
  if (fn == NULL) abort();
  return fn;
}

void zigr_task34_kernel(const double *src, size_t n, double *out) {
  if (task34_log_fn == NULL) task34_log_fn = task34_load_fn("log");
  if (task34_exp_fn == NULL) task34_exp_fn = task34_load_fn("exp");
  if (task34_sqrt_fn == NULL) task34_sqrt_fn = task34_load_fn("sqrt");

  task34_math_fn log_fn = task34_log_fn;
  task34_math_fn exp_fn = task34_exp_fn;
  task34_math_fn sqrt_fn = task34_sqrt_fn;

  double abs_total = 0.0;
  double log_total = 0.0;
  double exp_total = 0.0;
  double sqrt_total = 0.0;

  for (size_t repeat = 0; repeat < 512; ++repeat) {
    const double bias = ((double)repeat + 1.0) * 0.001;
    size_t i = 0;

    for (; i + 1 < n; i += 2) {
      const double shifted0 = src[i] + bias;
      const double shifted1 = src[i + 1] + bias;
      const double diff0 = shifted0 - 0.75;
      const double diff1 = shifted1 - 0.75;

      abs_total += diff0 < 0.0 ? -diff0 : diff0;
      log_total += log_fn(shifted0);
      exp_total += exp_fn(shifted0);
      sqrt_total += sqrt_fn(shifted0);

      abs_total += diff1 < 0.0 ? -diff1 : diff1;
      log_total += log_fn(shifted1);
      exp_total += exp_fn(shifted1);
      sqrt_total += sqrt_fn(shifted1);
    }

    for (; i < n; ++i) {
      const double shifted = src[i] + bias;
      const double diff = shifted - 0.75;

      abs_total += diff < 0.0 ? -diff : diff;
      log_total += log_fn(shifted);
      exp_total += exp_fn(shifted);
      sqrt_total += sqrt_fn(shifted);
    }
  }

  out[0] = abs_total;
  out[1] = log_total;
  out[2] = exp_total;
  out[3] = sqrt_total;
}