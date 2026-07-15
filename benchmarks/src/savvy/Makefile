R_INCLUDE ?= $(shell Rscript -e 'cat(R.home("include"))')

CARGO_TARGET_DIR ?= rust/target
STATLIB = $(CARGO_TARGET_DIR)/release/libsavvy_benchmarks.a
OUT = savvy_benchmarks.so

all: $(OUT)

$(STATLIB): rust/Cargo.toml rust/Cargo.lock rust/src/lib.rs api.h init.c
	export PATH="$(PATH):$(HOME)/.cargo/bin" && \
	  export R_INCLUDE_DIR="$(R_INCLUDE)" && \
	  cargo build --release --manifest-path rust/Cargo.toml --target-dir "$(CARGO_TARGET_DIR)"

$(OUT): $(STATLIB) init.c api.h
	rm -f $(OUT)
	PKG_CPPFLAGS="-I$(R_INCLUDE)" PKG_LIBS="-lblas" R CMD SHLIB -o $(OUT) init.c $(STATLIB)

clean:
	rm -f $(OUT) $(STATLIB) init.o

.PHONY: all clean
