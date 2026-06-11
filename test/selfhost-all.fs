\ selfhost-all.fs — the self-host gate. Runs the native checker, the drift guard,
\ and the byte-identical self-rebuild fixpoint so they can't rot. Slow (builds +
\ codesign + exec). Run: gforth test/selfhost-all.fs -e bye
require t-sh-check.fs
require t-sh-prims.fs
require t-sh-render.fs
require t-sh-asm.fs
require t-sh-cg.fs
require t-sh-walk.fs
require t-sh-opt.fs
require t-sh-vs.fs
require t-sh-if.fs
require t-sh-loop.fs
require t-sh-mem.fs
require t-sh-cmp.fs
require t-sh-spill.fs
require t-sh-hex.fs
require t-sh-coload.fs
require t-sh-undef.fs
require t-sh-die.fs
require t-sh-catch.fs
require t-sh-disasm.fs
require t-sh-debug.fs
require t-sh-sha.fs
require t-sh-sign.fs
require t-sh-drift.fs
require t-selfrebuild.fs
