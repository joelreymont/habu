#!/usr/bin/env python3
"""Print only JSON diagnostic lines when present, else pass stderr through."""
from __future__ import annotations

import json
import pathlib
import sys


def usage() -> None:
    print("usage: tools/json-only.py stderr-file", file=sys.stderr)
    raise SystemExit(64)


def main(argv: list[str]) -> int:
    if len(argv) != 1:
        usage()
    path = pathlib.Path(argv[0])
    text = path.read_text()
    json_lines: list[str] = []
    for line in text.splitlines():
        stripped = line.strip()
        if not stripped.startswith("{"):
            continue
        try:
            obj = json.loads(stripped)
        except json.JSONDecodeError:
            continue
        if isinstance(obj, dict):
            json_lines.append(stripped)

    if json_lines:
        for line in json_lines:
            print(line)
    else:
        sys.stderr.write(text)
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
