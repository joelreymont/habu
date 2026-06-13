#!/usr/bin/env python3
"""repl-lint: REPL-baked code must never `die`/`bye` — those WRITE-then-exit, so
a recoverable user error (a full breakpoint table, a bad token) would take down
the whole interactive session instead of being caught by the REPL's
uncaught-throw recovery (print a message + roll back the line + read again).
Interactive code raises errors with `throw`; only the build-time makers
(hbi/build/snap/stage2 drivers) may `die`, because there exiting IS the failure.

The baked-into-the-REPL file list is derived from hbi.f's `*-SRC` words (the
sources READ-REPL bakes as the engine's LSRC), with the known three as a
backstop. Caught a real one twice: debug.f BP+ table-full, and the impulse to
`die` from the stepper.
"""
import re, sys, pathlib

hbi = pathlib.Path("src/habu/hbi.f").read_text()
baked = re.findall(r'-SRC\s+s" (src/[^"]+\.f)"', hbi)
baked = sorted(set(baked) | {"src/habu/repl.f", "src/habu/stepper.f", "src/habu/debug.f"})

FATAL = {"die", "bye"}
bad = 0
for f in baked:
    p = pathlib.Path(f)
    if not p.exists():
        continue
    for n, line in enumerate(p.read_text().splitlines(), 1):
        code = line.split("\\", 1)[0]                 # strip line comment
        code = re.sub(r's" [^"]*"', " ", code)        # strip string literals
        code = re.sub(r'\( [^)]*\)', " ", code)       # strip paren comments
        for tok in code.split():
            if tok.lower() in FATAL:
                print(f"FATAL-IN-REPL {f}:{n}: `{tok}` exits the session — "
                      f"use `throw` (the REPL recovers); `die` is for build-time makers only")
                bad += 1
print(f"repl-lint: {bad} finding(s)")
sys.exit(1 if bad else 0)
