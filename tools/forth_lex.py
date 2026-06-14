#!/usr/bin/env python3
"""Small Forth lexer for source lints.

It recognizes the comment and string forms used in this repository. Tokens are
byte-like offsets for the project ASCII source files; line and column are
1-based.
"""
from __future__ import annotations

from dataclasses import dataclass


@dataclass(frozen=True)
class Token:
    kind: str
    text: str
    byte: int
    line: int
    column: int
    content: str = ""


STRING_OPENERS = {'s"', '."', 'c"'}


def scan(src: str) -> list[Token]:
    out: list[Token] = []
    i = 0
    line = 1
    column = 1

    def advance() -> str:
        nonlocal i, line, column
        ch = src[i]
        i += 1
        if ch == "\n":
            line += 1
            column = 1
        else:
            column += 1
        return ch

    def skip_to_quote() -> None:
        while i < len(src):
            ch = advance()
            if ch == '"':
                return

    while i < len(src):
        ch = src[i]
        if ch.isspace():
            advance()
            continue
        start_i, start_line, start_col = i, line, column
        if ch == "\\":
            while i < len(src) and src[i] != "\n":
                advance()
            continue
        if ch == "(":
            advance()
            content_start = i
            while i < len(src) and src[i] != ")":
                advance()
            content = src[content_start:i]
            if i < len(src):
                advance()
            out.append(Token("comment", src[start_i:i], start_i, start_line, start_col, content))
            continue
        while i < len(src) and not src[i].isspace():
            advance()
        text = src[start_i:i]
        out.append(Token("word", text, start_i, start_line, start_col))
        if text.lower() in STRING_OPENERS:
            skip_to_quote()
    return out
