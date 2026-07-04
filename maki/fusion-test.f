\ maki/fusion-test.f - op-fusion = concatenation: each node maps to its tile
\ word(s), the chain accumulates, unknown/overflow fail closed.
\ Reopens `package FUSION` so the fusion words resolve by bare (un-prefixed) name (the
\ test is part of the module); the harness words (T-*) and E-FUSE resolve via the package
\ global fallback.

require lib/test.f
require maki/fusion.f

\ maki-ns-lint: boundary FUSION - op-fusion subsystem test (reopens package FUSION)
package FUSION

T-RESET

\ each elementwise op lowers to its tile word(s) (concatenating these IS the fusion)
OP-SCALE OP-WORDS s"  a SCALE-V4"          STR= TTRUE
OP-ADD   OP-WORDS s"  y g LOAD-V4 ADD-V4"  STR= TTRUE
OP-RELU  OP-WORDS s"  RELU-V4"             STR= TTRUE

\ pushing the subgraph [Mul, Add, Relu] accumulates a 3-op chain
RESET
OP-SCALE OP!  OP-ADD OP!  OP-RELU OP!
NOPS @  3 T=

\ an unknown / unfusible op is REJECTED loudly (never silently approximated)
: BAD-FUSE ( -- )  99 OP-WORDS 2drop ;
' BAD-FUSE E-FUSE TTHROWS

\ a chain longer than MAX-OPS fails closed (no silent truncation)
: OVERFLOW-FUSE ( -- )  RESET  MAX-OPS 1+ 0 do  OP-SCALE OP!  loop ;
' OVERFLOW-FUSE E-FUSE TTHROWS

T-REPORT

end-package
