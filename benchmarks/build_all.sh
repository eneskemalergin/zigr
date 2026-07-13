#!/bin/bash
set -euo pipefail

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
cd "$SCRIPT_DIR"

Rscript tests/test_evidence_schema.R

ZIG_BIN=${ZIG:-}
if [ -z "$ZIG_BIN" ]; then
  ZIG_BIN=$(command -v zig || true)
fi
if [ -z "$ZIG_BIN" ] && [ -x "$SCRIPT_DIR/../zig-0.16.0/zig" ]; then
  ZIG_BIN="$SCRIPT_DIR/../zig-0.16.0/zig"
fi
if [ -z "$ZIG_BIN" ] || [ ! -x "$ZIG_BIN" ]; then
  echo "zig executable not found; set ZIG or install zig" >&2
  exit 1
fi

if [ -z "${R_INCLUDE:-}" ]; then
  R_INCLUDE=$(Rscript -e 'p <- c(file.path(R.home(), "include"), file.path(R.home(), "../share/R/include"), "/usr/share/R/include"); p <- p[dir.exists(p)]; if (!length(p)) quit(status = 1); cat(normalizePath(p[[1]]))')
fi
if [ -z "${R_LIB:-}" ]; then
  R_LIB=$(Rscript -e 'p <- file.path(R.home(), "lib"); if (!dir.exists(p)) quit(status = 1); cat(normalizePath(p))')
fi
if [ ! -d "$R_INCLUDE" ] || [ ! -d "$R_LIB" ]; then
  echo "R include/lib directories are invalid: R_INCLUDE=$R_INCLUDE R_LIB=$R_LIB" >&2
  exit 1
fi

OPTIMIZE=${ZIGR_OPTIMIZE:-ReleaseFast}
ZIG_ARGS=("-Doptimize=$OPTIMIZE" "-Dr-include=$R_INCLUDE" "-Dr-lib=$R_LIB")
CHECKED_SEXP=$(printf '%s' "${ZIGR_CHECKED_SEXP:-false}" | tr '[:upper:]' '[:lower:]')
case "$CHECKED_SEXP" in
  1|true|yes|on) ZIG_ARGS+=("-Dchecked-sexp=true") ;;
  0|false|no|off|'') ;;
  *)
    echo "ZIGR_CHECKED_SEXP must be a boolean value" >&2
    exit 1
    ;;
esac
if [ -n "${ZIGR_TARGET:-}" ] && [ "$ZIGR_TARGET" != "native" ]; then
  ZIG_ARGS+=("-Dtarget=$ZIGR_TARGET")
fi
if [ -n "${ZIGR_CPU_FEATURES:-}" ] && [ "$ZIGR_CPU_FEATURES" != "default" ]; then
  ZIG_ARGS+=("-Dcpu=$ZIGR_CPU_FEATURES")
fi
ZIG_CACHE_DIR=${ZIG_CACHE_DIR:-$SCRIPT_DIR/.zig-cache}
ZIG_GLOBAL_CACHE_DIR=${ZIG_GLOBAL_CACHE_DIR:-$SCRIPT_DIR/.zig-global-cache}
mkdir -p "$ZIG_CACHE_DIR" "$ZIG_GLOBAL_CACHE_DIR"
ZIG_CACHE_ARGS=("--cache-dir" "$ZIG_CACHE_DIR" "--global-cache-dir" "$ZIG_GLOBAL_CACHE_DIR")

echo "=== Zig (zigR) ==="
R_INCLUDE=$R_INCLUDE R_LIB=$R_LIB "$ZIG_BIN" build "${ZIG_ARGS[@]}" "${ZIG_CACHE_ARGS[@]}"

echo "=== C (.Call) ==="
cd src/c_call && make -f Makefile R_INCLUDE=$R_INCLUDE R_LIB=$R_LIB && cd ../..

echo "=== Rcpp (C++) ==="
PKG_CPPFLAGS=$(Rscript -e 'cat(paste0("-I", system.file("include", package="Rcpp")))') \
  R CMD SHLIB -o src/cpp/rcpp_benchmarks.so src/cpp/main.cpp

echo "=== cpp11 (C++) ==="
CPP11_LIBRARY="$SCRIPT_DIR/tmp/cpp11-library"
mkdir -p "$CPP11_LIBRARY"
R CMD INSTALL --preclean --clean --no-multiarch --library="$CPP11_LIBRARY" src/cpp11
Rscript runner_subprocess.R --runner=cpp11 --check-only

echo "=== extendr (Rust) ==="
make -C src/extendr

echo "=== Savvy (Rust) ==="
make -C src/savvy

echo "=== Done ==="
echo ""
echo "Built runners:"
ls -lh zig-out/lib/zigr_benchmarks.so \
      src/c_call/bench.so \
      src/cpp/rcpp_benchmarks.so \
      tmp/cpp11-library/zigrCpp11/libs/zigrCpp11.so \
      src/extendr/extendr_benchmarks.so \
      src/savvy/savvy_benchmarks.so \
      2>/dev/null
