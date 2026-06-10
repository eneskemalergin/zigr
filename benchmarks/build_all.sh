#!/bin/bash
# Build benchmark runners for zigr testing.
set -e

R_INCLUDE=$(Rscript -e 'cat(R.home("include"))')
R_LIB=$(Rscript -e 'cat(R.home("lib"))')

echo "=== Zig (zigR) ==="
R_INCLUDE=$R_INCLUDE R_LIB=$R_LIB zig build -Doptimize=ReleaseFast

echo "=== C (.Call) ==="
cd src/c_call && make -f Makefile R_INCLUDE=$R_INCLUDE R_LIB=$R_LIB && cd ../..

echo "=== Rcpp (C++) ==="
PKG_CPPFLAGS=$(Rscript -e 'cat(paste0("-I", system.file("include", package="Rcpp")))') \
  R CMD SHLIB -o src/cpp/rcpp_benchmarks.so src/cpp/main.cpp

echo "=== extendr (Rust) ==="
make -C src/extendr

echo "=== Savvy (Rust) ==="
make -C src/savvy

echo "=== Done ==="
echo ""
echo "Built runners:"
ls -lh zig-out/lib/libzigr_benchmarks.so \
      src/c_call/bench.c.so \
      src/cpp/rcpp_benchmarks.so \
      src/extendr/extendr_benchmarks.so \
      src/savvy/savvy_benchmarks.so \
      2>/dev/null
