#!/usr/bin/env python3
"""repl-pty: drive bin/hbi on a pseudo-terminal and assert the interactive
REPL contract: prompt, evaluation + " ok", undefined-word recovery (no exit,
no " ok", state rolled back), definitions usable after an error, EOF exit 0.
The same binary stays batch on a pipe — that path is covered by hb-suite."""
import os, pty, select, sys, time

HBI = os.path.join(os.path.dirname(__file__), '..', 'bin', 'hbi')

pid, fd = pty.fork()
if pid == 0:
    os.execv(HBI, [HBI])

def drain(t=0.5):
    out = b''
    end = time.time() + t
    while time.time() < end:
        r, _, _ = select.select([fd], [], [], 0.1)
        if r:
            try:
                out += os.read(fd, 4096)
            except OSError:
                break
    return out

def step(send, want, reject=b''):
    if send:
        os.write(fd, send)
    got = drain()
    for w in want:
        if w not in got:
            print(f"FAIL: sent {send!r}, wanted {w!r}, got {got!r}")
            sys.exit(1)
    if reject and reject in got:
        print(f"FAIL: sent {send!r}, must not see {reject!r}, got {got!r}")
        sys.exit(1)

step(b'', [b' ok', b'habu> '])                       # baked repl.f loaded
step(b'1 2 + .\n', [b'3', b' ok', b'habu> '])
step(b'frobnicate\n', [b'frobnicate?', b'habu> '], reject=b'ok')
step(b': SQ dup * ;\n', [b' ok'])                    # compiles fine after the error
step(b'7 SQ .\n', [b'49', b' ok'])
step(b'1 2 + ..\x7f\n', [b'3', b' ok'])             # backspace edits the line
step(b'garbage\x03', [b'habu> '], reject=b'garbage?')  # ^C cancels (never interpreted)
step(b'5 .\n', [b'5', b' ok'])
step(b'13 .\x1b[D\x1b[D\x1b[D0\n', [b'103', b' ok'])  # arrows + mid-line insert
step(b'\x1b[A\n', [b'103', b' ok'])                 # history up re-runs the line
step(b': SQ dup * ;\n', [b' ok'])                    # breakpoints: define, arm, hit, resume
step(b': IN1 1 + ;\n', [b' ok'])
step(b"' SQ BP+\n' IN1 BP+\n", [b' ok'])            # TWO breakpoints at once
step(b'7 SQ .\n', [b'habu-bp:', b'49'])              # SQ's BRK fires then resumes
step(b'9 IN1 .\n', [b'habu-bp:', b'10'])             # IN1's BRK fires too (multiple)
step(b'6 SQ .\n', [b'36'], reject=b'habu-bp:')       # one-shot: gone on the next call
step(b': PB dup + ;\n', [b' ok'])                    # persistent: fires EVERY call
step(b"' PB BP*\n", [b' ok'])
step(b'5 PB .\n', [b'habu-bp:', b'10'])
step(b'6 PB .\n', [b'habu-bp:', b'12'])              # still armed
step(b"' PB BP-\n", [b' ok'])
step(b'2 \' PB BPN\n', [b' ok'])                     # skip-count: silent first 2 hits
step(b'3 PB .\n', [b'6'], reject=b'habu-bp:')        # hit 1 (skipped)
step(b'3 PB .\n', [b'6'], reject=b'habu-bp:')        # hit 2 (skipped)
step(b'3 PB .\n', [b'habu-bp:', b'6'])               # hit 3 -> breaks
step(b'step 2 3 + .\n', [b'step> 2', b'step> 3', b'step> +', b'5'])  # token stepper
step(b'8 .\n', [b'8', b' ok'])                       # stepper hands back cleanly
step(b'99 throw\n', [b'?', b'habu> '], reject=b'ok') # uncaught throw recovers on a tty
step(b'6 .\n', [b'6', b' ok'])                       # still alive, state clean
os.write(fd, b'\x04')                                # ^D
for _ in range(50):                                  # exit may lag typed-ahead EOF
    time.sleep(0.1)
    got = os.waitpid(pid, os.WNOHANG)
    if got[0]:
        break
else:
    os.kill(pid, 9)
    print("FAIL: no exit on EOF")
    sys.exit(1)
if os.waitstatus_to_exitcode(got[1]) != 0:
    print(f"FAIL: exit status {got[1]}")
    sys.exit(1)
print("PASS: tty REPL (prompt, eval, recovery, EOF)")
