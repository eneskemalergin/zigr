# Build C_Call benchmark shared library from per-task source files.
# Uses R's default CFLAGS (via R CMD config) for a fair comparison.
# This matches what R CMD SHLIB would use.

R_INCLUDE ?= $(shell Rscript -e 'cat(R.home("include"))')
R_LIB ?= $(shell Rscript -e 'cat(R.home("lib"))')
R_CFLAGS ?= $(shell R CMD config CFLAGS)
PKG_CFLAGS = -I$(R_INCLUDE) $(R_CFLAGS)
PKG_LIBS = -L$(R_LIB) -lR -lpthread -lblas

# Detect if running from benchmarks/ or src/c_call/
SRCDIR = $(dir $(firstword $(MAKEFILE_LIST)))
SRC = $(addprefix $(SRCDIR),task_01_fib.c task_02_vectorsum.c \
      task_03_transpose.c task_04_strings.c \
      task_05_dataframe.c task_06_na_prop.c \
      task_07_parallel.c task_09_protect.c \
      task_10_blas_matmul.c task_11_crossprod.c \
      task_12_cholesky.c task_13_lm.c task_14_rowsums.c \
      task_15_elem_ops.c task_16_rowcol_means.c \
      task_17_broadcast.c task_18_sort.c \
      task_19_cumsum.c task_20_rnorm.c \
      task_21_string_nchar.c task_22_which_na.c \
      task_23_altrep_sum.c task_24_altrep_read.c \
      task_25_altrep_create.c \
      task_26_comptime_dispatch.c \
      task_27_struct_convert.c \
      task_28_na_prop_vary.c \
      task_29_scale_law.c \
      task_30_arena_vs_rmalloc.c \
      task_31_prot_overhead.c \
      task_32_longjmp_safety.c \
      task_34_translate_c_cost.c \
      task_35_string_variants.c \
      task_36_parallel_scaling.c \
      task_37_memory_bandwidth.c \
      task_38_owned_altrep_real_sum.c \
      task_39_owned_altrep_int_sum.c \
      task_40_owned_altrep_logical_sum.c \
      register.c)
OUT = $(SRCDIR)bench.c.so

all: $(OUT)

$(OUT): $(SRC)
	$(CC) $(PKG_CFLAGS) -fPIC -shared -o $@ $(SRC) $(PKG_LIBS)

clean:
	rm -f $(OUT)
