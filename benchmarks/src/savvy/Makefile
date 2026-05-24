R_INCLUDE ?= $(shell Rscript -e 'cat(R.home("include"))')

TARGET_DIR = rust/target
STATLIB = $(TARGET_DIR)/release/libsavvy_benchmarks.a
OUT = savvy_benchmarks.so

all: $(OUT)

$(STATLIB): rust/Cargo.toml rust/src/lib.rs api.h init.c
	export PATH="$(PATH):$(HOME)/.cargo/bin" && \
	  export R_INCLUDE_DIR="$(R_INCLUDE)" && \
	  cargo build --release --manifest-path rust/Cargo.toml --target-dir $(TARGET_DIR)

$(OUT): $(STATLIB) init.c api.h
	PKG_CPPFLAGS="-I$(R_INCLUDE)" R CMD SHLIB -o $(OUT) init.c $(STATLIB)

clean:
	rm -f $(OUT) $(STATLIB) init.o
	rm -rf $(TARGET_DIR)

.PHONY: all clean