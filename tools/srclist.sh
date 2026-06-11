#!/bin/sh
# the canonical habu compiler source, in dependency order (the fixpoint payload).
# optional arg picks the driver appended last: stage2 (default) or hbi
D=${1:-stage2}
echo "src/core/sha256.f src/arch/arm64/asm.f src/arch/arm64/icode.f \
src/arch/arm64/mnem.f src/core/util.f src/core/checker.f src/core/render.f \
src/habu/rt.f src/habu/crash.f src/os/macos/macho.f src/os/macos/sign2.f \
src/habu/habu1.f src/habu/prof.f src/habu/vsjit.f src/habu/habu2.f src/habu/$D.f"
