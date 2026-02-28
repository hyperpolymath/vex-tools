#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
RESULTS_DIR="$ROOT_DIR/benchmarks/results"
LATEST_JSON="$RESULTS_DIR/latest.json"
TIMESTAMP="$(date -u +%Y%m%dT%H%M%SZ)"
STAMPED_JSON="$RESULTS_DIR/$TIMESTAMP.json"

export VEXOMETER_BENCH_DB_BACKEND="${VEXOMETER_BENCH_DB_BACKEND:-verisimdb}"

mkdir -p "$RESULTS_DIR"

cd "$ROOT_DIR"

echo "== building benchmark runner =="
gprbuild -p -P tests/vexometer_bench.gpr

echo "== running benchmarks =="
./bin/benchmark_runner > "$LATEST_JSON"
cp "$LATEST_JSON" "$STAMPED_JSON"

echo "benchmark summary: $LATEST_JSON"
echo "timestamped copy: $STAMPED_JSON"
echo "case reports: $RESULTS_DIR/cases"

if [[ "${VEXOMETER_BENCH_DB_BACKEND}" == "verisimdb" ]]; then
  echo "== publishing benchmark summary to verisimdb =="
  if ! ./scripts/publish-bench-to-verisim.sh "$LATEST_JSON"; then
    if [[ "${VEXOMETER_BENCH_REQUIRE_DB:-0}" == "1" ]]; then
      echo "verisimdb publish failed and VEXOMETER_BENCH_REQUIRE_DB=1" >&2
      exit 1
    fi
    echo "warning: verisimdb publish failed (continuing)" >&2
  fi
fi
