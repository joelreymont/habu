#!/usr/bin/env python3
"""shadow-lint: toolchain colon definitions must not shadow engine PRIM names.
The engine's dict is later-wins and the checker records later sigs over PTAB,
so a toolchain word named like a prim silently replaces it for every program
the toolchain-loaded engine compiles (the vsjit fold helpers were once named
f+/f-/f* — float ops vanished on any warm engine)."""
import re, pathlib, subprocess, sys

prims = set()
t = pathlib.Path("src/habu/habu1.f").read_text()
for m in re.finditer(r's" ([^"]+)" \[\'\] \S+ FPRIM', t):
    prims.add(m.group(1).lower())
files = subprocess.run(["./tools/srclist.sh", "snap"], capture_output=True, text=True).stdout.split()
bad = 0
for f in files:
    for m in re.finditer(r"^: (\S+)", pathlib.Path(f).read_text(), re.M):
        if m.group(1).lower() in prims:
            print(f"SHADOW {f}: toolchain `: {m.group(1)}` hides the prim")
            bad += 1
if bad:
    print(f"shadow-lint: {bad} collision(s)"); sys.exit(1)
print("shadow-lint: clean")
