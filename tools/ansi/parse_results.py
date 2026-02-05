#!/usr/bin/env python3
"""Normalize ANSI runner logs into stable JSON keyed by test id."""

from __future__ import annotations

import argparse
import json
import re
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, Iterable, Optional


STATUS_ORDER = {"fail": 3, "pass": 2, "skip": 1, "other": 0}
STATUS_ALIASES = {
    "ok": "pass",
    "pass": "pass",
    "passed": "pass",
    "success": "pass",
    "notok": "fail",
    "not_ok": "fail",
    "fail": "fail",
    "failed": "fail",
    "error": "fail",
    "skip": "skip",
    "skipped": "skip",
    "xfail": "skip",
}

TEST_PATTERNS = [
    # TAP-ish: ok 12 test-id / not ok 12 test-id
    re.compile(
        r"^\s*(?P<tap>not\s+ok|ok)\s+(?:\d+\s+)?(?P<id>[A-Za-z0-9_./:+<>=*%-]+)\s*$",
        re.IGNORECASE,
    ),
    # PASS test-id / FAIL test-id
    re.compile(
        r"^\s*(?P<status>pass|passed|fail|failed|skip|skipped|error)\s*[:\-\s]\s*(?P<id>[A-Za-z0-9_./:+<>=*%-]+)\s*$",
        re.IGNORECASE,
    ),
    # test-id: PASS
    re.compile(
        r"^\s*(?P<id>[A-Za-z0-9_./:+<>=*%-]+)\s*[:\-\s]\s*(?P<status>pass|passed|fail|failed|skip|skipped|error)\s*$",
        re.IGNORECASE,
    ),
]

EXIT_RE = re.compile(r"^#\s*exit_code:\s*(\d+)\s*$")
STARTED_RE = re.compile(r"^#\s*started_utc:\s*(.+?)\s*$")


def normalize_status(raw: str) -> str:
    key = raw.strip().lower().replace(" ", "_")
    return STATUS_ALIASES.get(key, "other")


def status_from_tap(token: str) -> str:
    return "fail" if token.strip().lower().startswith("not") else "pass"


def normalize_test_id(raw: str) -> str:
    test_id = raw.strip()
    while test_id and test_id[-1] in (":", ","):
        test_id = test_id[:-1]
    return test_id


def default_output_path(log_path: Path) -> Path:
    out = str(log_path)
    out = out.replace("/raw/", "/results/")
    if out.endswith(".log"):
        out = out[:-4] + ".json"
    else:
        out = out + ".json"
    return Path(out)


@dataclass
class TestEntry:
    ordinal: int
    line_numbers: list[int] = field(default_factory=list)
    raw_statuses: list[str] = field(default_factory=list)
    status: str = "other"

    def update(self, raw_status: str, line_no: int) -> None:
        st = normalize_status(raw_status)
        if STATUS_ORDER[st] > STATUS_ORDER[self.status]:
            self.status = st
        self.raw_statuses.append(raw_status)
        self.line_numbers.append(line_no)


def parse_lines(lines: Iterable[str]) -> tuple[Dict[str, TestEntry], Optional[int], Optional[str]]:
    tests: Dict[str, TestEntry] = {}
    next_ordinal = 1
    exit_code: Optional[int] = None
    started_utc: Optional[str] = None

    for idx, line in enumerate(lines, start=1):
        text = line.rstrip("\n")
        started_match = STARTED_RE.match(text)
        if started_match and started_utc is None:
            started_utc = started_match.group(1)
            continue

        exit_match = EXIT_RE.match(text)
        if exit_match:
            exit_code = int(exit_match.group(1))
            continue

        matched = False
        for pat in TEST_PATTERNS:
            m = pat.match(text)
            if not m:
                continue

            if "tap" in m.groupdict() and m.group("tap"):
                raw_status = status_from_tap(m.group("tap"))
            else:
                raw_status = m.group("status")
            test_id = normalize_test_id(m.group("id"))

            entry = tests.get(test_id)
            if entry is None:
                entry = TestEntry(ordinal=next_ordinal)
                tests[test_id] = entry
                next_ordinal += 1
            entry.update(raw_status, idx)
            matched = True
            break

        if matched:
            continue

    return tests, exit_code, started_utc


def summarize_counts(tests: Dict[str, TestEntry]) -> dict:
    counts = {"pass": 0, "fail": 0, "skip": 0, "other": 0}
    for entry in tests.values():
        counts[entry.status] += 1
    counts["total"] = counts["pass"] + counts["fail"] + counts["skip"] + counts["other"]
    return counts


def serialize_tests(tests: Dict[str, TestEntry]) -> dict:
    out = {}
    for test_id in sorted(tests.keys()):
        entry = tests[test_id]
        out[test_id] = {
            "status": entry.status,
            "ordinal": entry.ordinal,
            "line_numbers": entry.line_numbers,
            "raw_statuses": entry.raw_statuses,
        }
    return out


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("log", help="Raw runner log path")
    ap.add_argument("--out", help="Output JSON path")
    args = ap.parse_args()

    log_path = Path(args.log)
    if not log_path.exists():
        raise SystemExit(f"error: log not found: {log_path}")

    out_path = Path(args.out) if args.out else default_output_path(log_path)
    out_path.parent.mkdir(parents=True, exist_ok=True)

    lines = log_path.read_text(encoding="utf-8", errors="replace").splitlines()
    tests, exit_code, started_utc = parse_lines(lines)

    if not tests:
        fallback_status = "pass" if (exit_code is None or exit_code == 0) else "fail"
        entry = TestEntry(ordinal=1, status=fallback_status)
        if exit_code is not None:
            entry.raw_statuses.append(f"exit_code:{exit_code}")
        tests["__run__"] = entry

    counts = summarize_counts(tests)
    data = {
        "source_log": str(log_path),
        "run_started_utc": started_utc,
        "exit_code": exit_code,
        "counts": counts,
        "tests": serialize_tests(tests),
    }

    out_path.write_text(json.dumps(data, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    print(str(out_path))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
