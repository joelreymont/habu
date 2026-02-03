#!/usr/bin/env python3

from __future__ import annotations

import sys
from collections import Counter
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
DOC = ROOT / "docs" / "cl-symbols.md"
SBCL = ROOT / "docs" / "cl-symbols-sbcl.txt"


def die(msg: str) -> None:
    print(msg, file=sys.stderr)
    raise SystemExit(1)


def parse_doc_symbols(md: str) -> tuple[list[str], Counter[str]]:
    rows: list[str] = []
    st_cnt: Counter[str] = Counter()

    in_summary = False
    for line in md.splitlines():
        if line.startswith("## Summary"):
            in_summary = True
        if in_summary:
            continue

        if not line.startswith("|"):
            continue
        if set(line.strip()) <= set("|- "):
            continue

        parts = [p.strip() for p in line.strip().strip("|").split("|")]
        if len(parts) < 2:
            continue
        sym, st = parts[0], parts[1]
        if sym == "Symbol" and st == "Status":
            continue

        if st not in {"✓", "⚠", "✗"}:
            die(f"{DOC}: unexpected status {st!r} for symbol {sym!r}")

        rows.append(sym)
        st_cnt[st] += 1

    return rows, st_cnt


def main() -> None:
    if not DOC.exists():
        die(f"missing {DOC}")
    if not SBCL.exists():
        die(f"missing {SBCL} (run sbcl export once and commit it)")

    sb_syms = [ln.strip() for ln in SBCL.read_text(encoding="utf-8").splitlines() if ln.strip()]
    if len(sb_syms) != 978:
        die(f"{SBCL}: expected 978 symbols, got {len(sb_syms)}")

    doc_md = DOC.read_text(encoding="utf-8")
    doc_rows, st_cnt = parse_doc_symbols(doc_md)
    doc_set = {s.upper() for s in doc_rows}
    sb_set = set(sb_syms)

    missing = sorted(sb_set - doc_set)
    extra = sorted(doc_set - sb_set)

    print(f"doc entries: {len(doc_rows)}")
    print(f"doc unique:  {len(doc_set)}")
    print(f"sbcl unique: {len(sb_set)}")
    print(f"status: ✓ {st_cnt.get('✓', 0)} | ⚠ {st_cnt.get('⚠', 0)} | ✗ {st_cnt.get('✗', 0)}")

    if missing:
        print(f"\nmissing from doc ({len(missing)}):")
        for s in missing:
            print(s)
    if extra:
        print(f"\nextra in doc ({len(extra)}):")
        for s in extra:
            print(s)

    if missing or extra:
        raise SystemExit(1)


if __name__ == "__main__":
    main()

