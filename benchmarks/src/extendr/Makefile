R_HOME ?= $(shell R RHOME)
R_INCLUDE ?= $(shell Rscript -e 'cat(R.home("include"))')

TARGET_DIR = rust/target
STATLIB = $(TARGET_DIR)/release/libextendr_benchmarks.a
OUT = extendr_benchmarks.so

all: $(OUT)

$(STATLIB): rust/Cargo.toml rust/src/lib.rs
	export PATH="$(PATH):$(HOME)/.cargo/bin" && \
	  cargo build --release --manifest-path rust/Cargo.toml --target-dir $(TARGET_DIR)

$(OUT): $(STATLIB) entrypoint.c
	PKG_CPPFLAGS="-I$(R_INCLUDE)" PKG_LIBS="-lblas" R CMD SHLIB -o $(OUT) entrypoint.c -Wl,--whole-archive $(STATLIB) -Wl,--no-whole-archive

clean:
	rm -f $(OUT) $(STATLIB) entrypoint.o
	rm -rf $(TARGET_DIR)

.PHONY: all clean