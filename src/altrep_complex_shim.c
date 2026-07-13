#include "r_imports.h"

_Static_assert(sizeof(Rcomplex) == 2 * sizeof(double),
               "Rcomplex must contain two doubles");
_Static_assert(_Alignof(Rcomplex) == _Alignof(double),
               "Rcomplex alignment must match double");

static Rcomplex zigr_altcomplex_elt(SEXP x, R_xlen_t i)
{
    Rcomplex value;
    zigr_altcomplex_elt_parts_impl(x, i, &value.r, &value.i);
    return value;
}

void zigr_set_altcomplex_elt_method(R_altrep_class_t cls)
{
    R_set_altcomplex_Elt_method(cls, zigr_altcomplex_elt);
}

void zigr_complex_elt_parts(SEXP x, R_xlen_t i, double *real, double *imaginary)
{
    Rcomplex value = COMPLEX_ELT(x, i);
    *real = value.r;
    *imaginary = value.i;
}
