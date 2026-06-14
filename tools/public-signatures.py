#!/usr/bin/env python3
"""Emit a JSON manifest of typed public Forth definitions."""
from __future__ import annotations

import json
import pathlib
import sys

sys.dont_write_bytecode = True

from forth_lex import Token, scan


def usage() -> None:
    print("usage: tools/public-signatures.py file ...", file=sys.stderr)
    raise SystemExit(64)


def is_project_word(name: str) -> bool:
    return any(ch.isalpha() for ch in name) and name.upper() == name


def exports(tokens: list[Token]) -> set[str]:
    out: set[str] = set()
    for i, tok in enumerate(tokens):
        if tok.kind == "word" and tok.text.upper() == "EXPORT" and i + 1 < len(tokens):
            out.add(tokens[i + 1].text.upper())
    return out


def defs(path: pathlib.Path) -> list[dict[str, object]]:
    src = path.read_text()
    toks = scan(src)
    exported = exports(toks)
    out: list[dict[str, object]] = []
    i = 0
    while i < len(toks):
        tok = toks[i]
        if tok.kind == "word" and tok.text == ":" and i + 2 < len(toks):
            name = toks[i + 1]
            sig = toks[i + 2]
            if name.kind == "word" and sig.kind == "comment" and "--" in sig.content:
                public = name.text.upper() in exported or is_project_word(name.text)
                if public:
                    out.append({
                        "schema_version": 1,
                        "word": name.text.upper(),
                        "file": str(path),
                        "line": name.line,
                        "column": name.column,
                        "byte_start": name.byte,
                        "signature": f"({sig.content.strip()})",
                        "exported": name.text.upper() in exported,
                    })
            i += 2
        i += 1
    return out


def main(argv: list[str]) -> int:
    if not argv:
        usage()
    items: list[dict[str, object]] = []
    for name in argv:
        items.extend(defs(pathlib.Path(name)))
    json.dump({"schema_version": 1, "definitions": items}, sys.stdout, indent=2)
    sys.stdout.write("\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
