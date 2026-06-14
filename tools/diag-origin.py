#!/usr/bin/env python3
"""Inject checker diagnostic origin markers before colon definitions."""
from __future__ import annotations

import pathlib
import sys

sys.dont_write_bytecode = True

from forth_lex import scan


def usage() -> None:
    print("usage: tools/diag-origin.py file", file=sys.stderr)
    raise SystemExit(64)


def origin_for(tokens: list, idx: int):
    if idx + 1 < len(tokens) and tokens[idx + 1].kind == "word":
        return tokens[idx + 1]
    return tokens[idx]


def rewrite(src: str) -> str:
    toks = scan(src)
    inserts: dict[int, list[str]] = {}
    for i, tok in enumerate(toks):
        if tok.kind == "word" and tok.text == ":":
            origin = origin_for(toks, i)
            inserts.setdefault(tok.byte, []).append(
                f"\n{origin.line} {origin.column} {origin.byte} DIAG-ORIGIN!\n"
            )

    out: list[str] = []
    pos = 0
    for byte in sorted(inserts):
        out.append(src[pos:byte])
        out.extend(inserts[byte])
        pos = byte
    out.append(src[pos:])
    return "".join(out)


def main(argv: list[str]) -> int:
    if len(argv) != 1:
        usage()
    path = pathlib.Path(argv[0])
    sys.stdout.write(rewrite(path.read_text()))
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
