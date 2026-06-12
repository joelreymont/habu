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
