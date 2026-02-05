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


def parse_doc_symbols(md: str) -> tuple[list[tuple[str, str]], Counter[str]]:
    rows: list[tuple[str, str]] = []
    row_st: Counter[str] = Counter()

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

        rows.append((sym, st))
        row_st[st] += 1

    return rows, row_st


def rollup_status(sts: set[str]) -> str:
    # Overall status per ANSI symbol:
    # - ✗ only if *all* entries are missing
    # - ✓ only if *all* entries are implemented
    # - ⚠ for mixed roles (e.g. function done, var missing) or any partial
    if sts == {"✓"}:
        return "✓"
    if sts == {"✗"}:
        return "✗"
    return "⚠"


def main() -> None:
    if not DOC.exists():
        die(f"missing {DOC}")
    if not SBCL.exists():
        die(f"missing {SBCL} (run sbcl export once and commit it)")

    sb_syms = [ln.strip() for ln in SBCL.read_text(encoding="utf-8").splitlines() if ln.strip()]
    if len(sb_syms) != 978:
        die(f"{SBCL}: expected 978 symbols, got {len(sb_syms)}")

    doc_md = DOC.read_text(encoding="utf-8")
    doc_rows, row_st = parse_doc_symbols(doc_md)

    doc_syms = [s.upper() for s, _ in doc_rows]
    doc_set = set(doc_syms)
    sb_set = set(sb_syms)

    missing = sorted(sb_set - doc_set)
    extra = sorted(doc_set - sb_set)

    dup_rows = len(doc_syms) - len(doc_set)
    if dup_rows:
        print(f"doc entries: {len(doc_rows)} ({dup_rows} duplicate rows)")
    else:
        print(f"doc entries: {len(doc_rows)}")
    print(f"doc unique:  {len(doc_set)}")
    print(f"sbcl unique: {len(sb_set)}")
    print(f"row status: ✓ {row_st.get('✓', 0)} | ⚠ {row_st.get('⚠', 0)} | ✗ {row_st.get('✗', 0)}")

    by_sym: dict[str, set[str]] = {}
    for sym, st in doc_rows:
        s = sym.upper()
        by_sym.setdefault(s, set()).add(st)

    sym_st: Counter[str] = Counter()
    sym_missing: list[str] = []
    sym_partial: list[str] = []
    for s, sts in by_sym.items():
        st = rollup_status(sts)
        sym_st[st] += 1
        if st == "✗":
            sym_missing.append(s)
        elif st == "⚠":
            sym_partial.append(s)

    print(f"sym status: ✓ {sym_st.get('✓', 0)} | ⚠ {sym_st.get('⚠', 0)} | ✗ {sym_st.get('✗', 0)}")

    if sym_missing:
        sym_missing.sort()
        print(f"\nmissing symbols (✗) ({len(sym_missing)}):")
        for s in sym_missing:
            print(s)
    if sym_partial:
        sym_partial.sort()
        print(f"\npartial symbols (⚠) ({len(sym_partial)}):")
        for s in sym_partial:
            print(s)

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
