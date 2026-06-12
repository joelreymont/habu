#!/bin/sh
# the canonical habu compiler source, in dependency order (the fixpoint payload).
# util/checker/render come FIRST so the check hook (inserted before sha256.f by
# build/bootstrap) covers the entire toolchain — asm vocabulary included.
# optional arg picks the driver appended last: stage2 (default) or hbi
D=${1:-stage2}
echo "src/core/util.f src/core/checker.f src/core/render.f \
src/core/sha256.f src/arch/arm64/asm.f src/arch/arm64/icode.f \
src/arch/arm64/mnem.f src/os/macos/sys.f \
src/habu/rt.f src/habu/crash.f src/os/macos/macho.f src/os/macos/sign2.f \
src/habu/habu1.f src/habu/prof.f src/habu/regalloc.f src/habu/vsjit.f src/habu/habu2.f src/habu/$D.f"
