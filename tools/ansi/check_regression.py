#!/usr/bin/env python3
"""Fail when current ANSI run introduces failures not present in baseline."""

from __future__ import annotations

import argparse
import json
from pathlib import Path


def load_json(path: Path) -> dict:
    with path.open("r", encoding="utf-8") as f:
        return json.load(f)


def failing_ids(result: dict) -> set[str]:
    tests = result.get("tests", {})
    out: set[str] = set()
    for test_id, payload in tests.items():
        status = str(payload.get("status", "")).lower()
        if status == "fail":
            out.add(test_id)
    return out


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--baseline", required=True, help="Baseline JSON path")
    parser.add_argument("--current", required=True, help="Current result JSON path")
    parser.add_argument("--runtime", required=True, choices=["sbcl", "habu"])
    args = parser.parse_args()

    baseline = load_json(Path(args.baseline))
    current = load_json(Path(args.current))

    baseline_run = baseline.get("runs", {}).get(args.runtime)
    if baseline_run is None:
        raise SystemExit(f"error: runtime {args.runtime!r} not present in baseline")

    expected = set(baseline_run.get("failing_tests", []))
    observed = failing_ids(current)
    unexpected = sorted(observed - expected)

    report = {
        "runtime": args.runtime,
        "expected_failures": sorted(expected),
        "observed_failures": sorted(observed),
        "unexpected_failures": unexpected,
    }
    print(json.dumps(report, indent=2, sort_keys=True))

    if unexpected:
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
