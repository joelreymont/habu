#!/usr/bin/env python3
"""Run the native checker over each top-level colon definition."""
from __future__ import annotations

import argparse
import json
import pathlib
import subprocess
import sys

sys.dont_write_bytecode = True

from forth_lex import Token, scan


HOOK = """: CHECK-SH-HOOK ( n n -- n )
   CHECK!  dup -1 <> IF s" check.sh: check did not certify" 70 die THEN ;
' CHECK-SH-HOOK set-check
"""


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--json-errors", action="store_true")
    parser.add_argument("--label", required=True)
    parser.add_argument("source")
    return parser.parse_args(argv)


def origin_for(tokens: list[Token], idx: int) -> Token:
    if idx + 1 < len(tokens) and tokens[idx + 1].kind == "word":
        return tokens[idx + 1]
    return tokens[idx]


def definitions(src: str) -> list[tuple[str, Token]]:
    toks = scan(src)
    out: list[tuple[str, Token]] = []
    i = 0
    while i < len(toks):
        tok = toks[i]
        if tok.kind == "word" and tok.text == ":":
            origin = origin_for(toks, i)
            j = i + 1
            while j < len(toks):
                if toks[j].kind == "word" and toks[j].text == ";":
                    end = toks[j].byte + len(toks[j].text)
                    out.append((src[tok.byte:end], origin))
                    i = j
                    break
                j += 1
        i += 1
    return out


def json_lines(stderr: str) -> list[str]:
    out: list[str] = []
    for line in stderr.splitlines():
        stripped = line.strip()
        if not stripped.startswith("{"):
            continue
        try:
            obj = json.loads(stripped)
        except json.JSONDecodeError:
            continue
        if isinstance(obj, dict):
            out.append(stripped)
    return out


def run_one(label: str, json_errors: bool, accepted: list[str], definition: str, origin: Token):
    prefix = ["0 set-check", f's" {label}" DIAG-FILE!']
    if json_errors:
        prefix.append("-1 JSON-DIAGS !")
    prefix.append(HOOK)
    prefix.extend(accepted)
    prefix.append(f"{origin.line} {origin.column} {origin.byte} DIAG-ORIGIN!")
    prefix.append(definition)
    program = "\n".join(prefix) + "\n"

    return subprocess.run(["bin/habu"], input=program, text=True, capture_output=True)


def main(argv: list[str]) -> int:
    args = parse_args(argv)
    src = pathlib.Path(args.source).read_text()
    accepted: list[str] = []
    failed = False
    raw_failure = 0

    for definition, origin in definitions(src):
        proc = run_one(args.label, args.json_errors, accepted, definition, origin)
        if proc.returncode == 0:
            accepted.append(definition)
            continue
        failed = True
        if args.json_errors:
            lines = json_lines(proc.stderr)
            if lines:
                for line in lines:
                    print(line, file=sys.stderr)
            else:
                sys.stderr.write(proc.stderr)
                raw_failure = proc.returncode
        else:
            sys.stderr.write(proc.stderr)

    if raw_failure:
        return raw_failure
    return 70 if failed else 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
