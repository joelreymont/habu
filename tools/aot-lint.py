#!/usr/bin/env python3
"""Reject source forms unsupported by stripped AOT.

The stripped AOT image has no persistent data region or compiler state. Runtime
data-space words that work in the full engine can otherwise compile into a tiny
binary that faults later. This lint fails before the maker runs.
"""
from __future__ import annotations

import json
import pathlib
import sys

sys.dont_write_bytecode = True

from forth_lex import scan


UNSAFE = {
    "@",
    "!",
    "c@",
    "c!",
    "here",
    "allot",
    ",",
    "c,",
    "create",
    "compile,",
    "patch32",
}


def usage() -> None:
    print("usage: tools/aot-lint.py [--json] [--label name] file ...", file=sys.stderr)
    raise SystemExit(64)


def findings(path: pathlib.Path, label: str) -> list[dict[str, object]]:
    toks = scan(path.read_text())
    bad: list[dict[str, object]] = []
    expect_name = False
    current_word = ""
    for tok in toks:
        if tok.kind != "word":
            continue
        word = tok.text.lower()
        if expect_name:
            current_word = tok.text
            expect_name = False
            continue
        if word == ":":
            expect_name = True
            current_word = ""
            continue
        if word == ";":
            current_word = ""
            continue
        if word in UNSAFE:
            bad.append({
                "code": "E-AOT-UNSUPPORTED",
                "file": label,
                "line": tok.line,
                "column": tok.column,
                "byte_start": tok.byte,
                "byte_end": tok.byte + len(tok.text),
                "word": current_word,
                "token": tok.text,
                "reason": "stripped AOT has no persistent data region",
                "suggestion": "stripped AOT has no persistent data region; use --repl/snapshot for data-space words or remove the runtime data access",
            })
    return bad


def main(argv: list[str]) -> int:
    json_out = False
    labels: dict[str, str] = {}
    files: list[str] = []
    pending_label: str | None = None
    i = 0
    while i < len(argv):
        arg = argv[i]
        if arg == "--json":
            json_out = True
            i += 1
        elif arg == "--label":
            if i + 1 >= len(argv):
                usage()
            pending_label = argv[i + 1]
            i += 2
        elif arg.startswith("-"):
            usage()
        else:
            files.append(arg)
            if pending_label is not None:
                labels[arg] = pending_label
                pending_label = None
            i += 1
    if not files:
        usage()

    nbad = 0
    for name in files:
        path = pathlib.Path(name)
        label = labels.get(name, name)
        for item in findings(path, label):
            nbad += 1
            if json_out:
                print(json.dumps(item, separators=(",", ":")))
            else:
                print(
                    f"{item['code']} {item['file']}:{item['line']}:{item['column']}: "
                    f"`{item['token']}` is not supported by stripped AOT"
                )
    if not json_out and nbad:
        print(f"aot-lint: {nbad} finding(s)")
    return 1 if nbad else 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
