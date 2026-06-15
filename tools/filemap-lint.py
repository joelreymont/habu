#!/usr/bin/env python3
"""filemap-lint: keep FILEMAP.md useful as an agent navigation index."""
from __future__ import annotations

import pathlib
import re
import sys


FILEMAP = pathlib.Path("FILEMAP.md")
REQUIRED = {
    "AGENTS.md",
    "LLM.md",
    "LESSONS.md",
    "STATUS.md",
    "TRUSTED.md",
    "src/core/checker.f",
    "src/core/render.f",
    "src/habu/aot.f",
    "src/habu/build.f",
    "tools/check.sh",
    "tools/hb-build.sh",
    "tools/signature-lint.py",
    "tools/aot-lint.py",
    "tools/forth_lex.py",
    "tools/diag-origin.py",
    "tools/json-only.f",
    "tools/check-all-errors.py",
    "tools/diag-to-sarif.f",
    "tools/public-signatures.py",
    "tools/aot-call-report.f",
    "test/run.sh",
    "test/t-sh-jdiag.fs",
    "bench/llm/tasks.tsv",
}


def pathish(text: str) -> bool:
    return "/" in text or text.endswith((".md", ".sh", ".py", ".f", ".fs", ".tsv"))


def main() -> int:
    if not FILEMAP.exists():
        print("FILEMAP-MISSING FILEMAP.md: navigation index is required")
        return 1
    text = FILEMAP.read_text()
    paths = {path for path in re.findall(r"`([^`]+)`", text) if pathish(path)}
    bad = 0
    for path in sorted(paths):
        if not pathlib.Path(path).exists():
            print(f"FILEMAP-STALE FILEMAP.md: `{path}` does not exist")
            bad += 1
    for path in sorted(REQUIRED - paths):
        print(f"FILEMAP-MISSING FILEMAP.md: required entry `{path}` is absent")
        bad += 1
    print(f"filemap-lint: {len(paths)} path(s), {bad} finding(s)")
    return 1 if bad else 0


if __name__ == "__main__":
    raise SystemExit(main())
