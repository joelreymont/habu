#!/usr/bin/env python3
"""
Simple Lisp parenthesis checker.

Scans Lisp source while ignoring strings, character literals, and comments to
report unmatched opening/closing parentheses with line/column locations.
"""

from __future__ import annotations

import argparse
import sys
from dataclasses import dataclass
from pathlib import Path


@dataclass
class ParenIssue:
    kind: str  # "unmatched-open" or "unmatched-close"
    line: int
    column: int
    context: str


def _scan(text: str) -> list[ParenIssue]:
    issues: list[ParenIssue] = []
    stack: list[tuple[int, int]] = []  # (line, column) of '('

    line = 1
    col = 0
    in_string = False
    escape = False
    in_line_comment = False
    block_comment_depth = 0
    skip_next_char_literal = False

    i = 0
    length = len(text)
    while i < length:
        ch = text[i]
        col += 1

        if ch == "\n":
            line += 1
            col = 0
            in_line_comment = False
            escape = False
            i += 1
            continue

        if in_line_comment:
            i += 1
            continue

        if block_comment_depth > 0:
            if ch == "|" and i + 1 < length and text[i + 1] == "#":
                block_comment_depth -= 1
                i += 2
            else:
                i += 1
            continue

        if skip_next_char_literal:
            skip_next_char_literal = False
            i += 1
            continue

        if in_string:
            if escape:
                escape = False
            elif ch == "\\":
                escape = True
            elif ch == '"':
                in_string = False
            i += 1
            continue

        # Not in string/comment
        if ch == ";":
            in_line_comment = True
            i += 1
            continue

        if ch == "#" and i + 1 < length:
            nxt = text[i + 1]
            if nxt == "\\":
                skip_next_char_literal = True
                i += 2
                continue
            if nxt == "|":
                block_comment_depth += 1
                i += 2
                continue

        if ch == '"':
            in_string = True
            i += 1
            continue

        if ch == "(":
            stack.append((line, col))
        elif ch == ")":
            if stack:
                stack.pop()
            else:
                issues.append(
                    ParenIssue(
                        kind="unmatched-close",
                        line=line,
                        column=col,
                        context="unexpected ')'",
                    )
                )
        i += 1

    for open_line, open_col in stack:
        issues.append(
            ParenIssue(
                kind="unmatched-open",
                line=open_line,
                column=open_col,
                context="unclosed '('",
            )
        )

    return issues


def check_file(path: Path) -> list[ParenIssue]:
    try:
        text = path.read_text()
    except Exception as exc:  # noqa: BLE001
        raise SystemExit(f"Failed to read {path}: {exc}") from exc
    return _scan(text)


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(description="Find unmatched parentheses in Lisp files.")
    parser.add_argument("files", nargs="+", type=Path, help="Paths to Lisp files to check.")
    args = parser.parse_args(argv)

    exit_code = 0
    for file_path in args.files:
        issues = check_file(file_path)
        if not issues:
            print(f"{file_path}: OK")
            continue
        exit_code = 1
        print(f"{file_path}: {len(issues)} issue(s)")
        for issue in issues:
            print(f"  {issue.kind} at {issue.line}:{issue.column} - {issue.context}")
    return exit_code


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
