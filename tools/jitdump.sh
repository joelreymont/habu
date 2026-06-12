#!/bin/sh
# jitdump.sh '<habu program>' WORD — disassemble WORD's JIT-compiled code on the
# working-tree engine (probe.sh path). The vsjit microscope: shows exactly what
# the runtime : compiler emitted (fused branches, register ops, literal chains).
cd "$(dirname "$0")/.."
PROBE_FILES="src/arch/arm64/disasm.f tools/jitdump.f" exec ./tools/probe.sh "$1 ' $2 JD"
