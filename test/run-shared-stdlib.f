\ run-shared-stdlib.f - parent-loaded shared setup for forked workers.
\
\ Loads the stdlib tool base once; stdlib slices plus the dictionary and
\ diagnostics families (TR-SHARED-BASE?) fork after this setup and inherit
\ it copy-on-write, so their require lists load only the gate-lib deltas.
\ Do not widen this base with family gate libs: the parent load is serial
\ on the critical path of every post-setup fork (measured 7952ms -> 16825ms
\ and a 16.5s -> 28.3s hot gate when the diag/dict/engine/debug/aot-neg
\ sets were preloaded here), and src/habu/aot-closure.f cannot share an
\ image with tools/lint/clobber-lint.f (both define CX).

require test/gate-stdlib-lib.f
require test/gate-stdlib-inline-lib.f

GSI-TOOL-BASE
