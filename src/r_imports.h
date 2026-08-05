#include <R.h>
#include <Rversion.h>
#include <Rinternals.h>
#include <R_ext/Error.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Utils.h>
#include <R_ext/Altrep.h>
#include <R_ext/Random.h>

/* Zig 0.16 translate-c keeps Rcomplex opaque because it is a C union. */
void zigr_set_altcomplex_elt_method(R_altrep_class_t cls);
void zigr_altcomplex_elt_parts_impl(SEXP x, R_xlen_t i, double *real, double *imaginary);
void zigr_complex_elt_parts(SEXP x, R_xlen_t i, double *real, double *imaginary);
