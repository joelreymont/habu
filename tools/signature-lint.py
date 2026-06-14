#!/usr/bin/env python3
"""Reject colon definitions without an explicit stack-effect signature.

Strict mode is for agent-facing source: every colon definition must be either
typed immediately after the name, or explicitly marked `( infer )` / `( private )`.
The lexer understands the Forth comment and string forms used in this repo so
colons inside comments or strings do not become false positives.
"""
from __future__ import annotations

import json
import pathlib
import sys

sys.dont_write_bytecode = True

from forth_lex import Token, scan

OPTOUT = {"infer", "private", "infer private", "private infer"}


def usage() -> None:
    print("usage: tools/signature-lint.py [--json] [--label name] file ...", file=sys.stderr)
    raise SystemExit(64)


def sig_or_optout(tok: Token) -> bool:
    if tok.kind != "comment":
        return False
    norm = " ".join(tok.content.strip().lower().split())
    return "--" in tok.content or norm in OPTOUT


def findings(path: pathlib.Path, label: str) -> list[dict[str, object]]:
    toks = scan(path.read_text())
    bad: list[dict[str, object]] = []
    i = 0
    while i < len(toks):
        tok = toks[i]
        if tok.kind == "word" and tok.text.lower() == ":":
            if i + 1 >= len(toks) or toks[i + 1].kind != "word":
                bad.append({
                    "code": "E-MISSING-NAME",
                    "file": label,
                    "line": tok.line,
                    "column": tok.column,
                    "byte_start": tok.byte,
                    "word": "",
                    "suggestion": "add a word name after ':'",
                })
                i += 1
                continue
            name = toks[i + 1]
            sig = toks[i + 2] if i + 2 < len(toks) else None
            if sig is None or not sig_or_optout(sig):
                bad.append({
                    "code": "E-MISSING-SIGNATURE",
                    "file": label,
                    "line": name.line,
                    "column": name.column,
                    "byte_start": name.byte,
                    "word": name.text,
                    "suggestion": "add `( in -- out )` immediately after the word name, or mark `( infer )` / `( private )`",
                })
            i += 2
        else:
            i += 1
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
                    f"`{item['word']}` needs `( in -- out )`, `( infer )`, or `( private )`"
                )
    if not json_out and nbad:
        print(f"signature-lint: {nbad} finding(s)")
    return 1 if nbad else 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
