# Build C_Call benchmark shared library from per-task source files.
# Uses R's default CFLAGS (via R CMD config) for a fair comparison.
# This matches what R CMD SHLIB would use.

R_INCLUDE ?= $(shell Rscript -e 'cat(R.home("include"))')
R_LIB ?= $(shell Rscript -e 'cat(R.home("lib"))')
R_CFLAGS ?= $(shell R CMD config CFLAGS)
PKG_CFLAGS = -I$(R_INCLUDE) $(R_CFLAGS)
PKG_LIBS = -L$(R_LIB) -lR -lpthread -lblas

SRCDIR = $(dir $(firstword $(MAKEFILE_LIST)))
SRC = $(addprefix $(SRCDIR), \
      task_01_vectorsum.c \
      task_02_elem_ops.c \
      task_03_memcpy_bandwidth.c \
      task_04_sort.c \
      task_05_fib_recursive.c \
      task_06_broadcast.c \
      task_07a_protect_shallow.c \
      task_07b_protect_scaling.c \
      task_08_type_dispatch.c \
      task_09_longjmp_safety.c \
      task_10_sexp_create.c \
      task_11_sexp_inspect.c \
      task_12_matrix_transpose.c \
      task_13_matrix_rowsums.c \
      task_14_matrix_rowcol_means.c \
      task_15_dataframe_filter.c \
      task_16_list_access.c \
      task_17_string_concat.c \
      task_18_string_nchar.c \
      task_19_string_encoding.c \
      task_20_factor_ops.c \
      task_21_attrib_ops.c \
      task_22_s4_slot_access.c \
      task_23_na_propagation.c \
      task_24_long_vector_idx.c \
      task_25_l1_arithmetic.c \
      task_26_matmul.c \
      task_27_crossprod.c \
      task_28_cholesky.c \
      task_29_lm_fit.c \
      task_30_altrep_create.c \
      task_31_altrep_materialize.c \
      task_32_altrep_elt_walk.c \
      task_33_altrep_region_read.c \
      task_34_altrep_sum_via_R.c \
      task_35_altrep_sum_native.c \
      task_36_altrep_min_max.c \
      task_37_altrep_no_na_query.c \
      task_38_struct_convert.c \
      task_39_r_eval.c \
      task_40_r_tryeval.c \
      task_41_serialize_roundtrip.c \
      task_42_external_ptr.c \
      task_43_rng_stress.c \
      register.c)
OUT = $(SRCDIR)bench.c.so

all: $(OUT)

$(OUT): $(SRC)
	$(CC) $(PKG_CFLAGS) -fPIC -shared -o $@ $(SRC) $(PKG_LIBS)

clean:
	rm -f $(OUT)
