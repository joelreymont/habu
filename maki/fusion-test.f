\ maki/fusion-test.f - op-fusion = concatenation: each node maps to its tile
\ word(s), the chain accumulates, unknown/overflow fail closed.

T-RESET

\ each elementwise op lowers to its tile word(s) (concatenating these IS the fusion)
OP-SCALE FUSE-OP-WORDS s"  a SCALE-V4"          STR= TTRUE
OP-ADD   FUSE-OP-WORDS s"  y g LOAD-V4 ADD-V4"  STR= TTRUE
OP-RELU  FUSE-OP-WORDS s"  RELU-V4"             STR= TTRUE

\ pushing the subgraph [Mul, Add, Relu] accumulates a 3-op chain
FUSE-RESET
OP-SCALE FUSE-OP!  OP-ADD FUSE-OP!  OP-RELU FUSE-OP!
FUSE-NOPS @  3 T=

\ an unknown / unfusible op is REJECTED loudly (never silently approximated)
: BAD-FUSE ( -- )  99 FUSE-OP-WORDS 2drop ;
' BAD-FUSE E-FUSE TTHROWS

\ a chain longer than FUSE-MAX fails closed (no silent truncation)
: OVERFLOW-FUSE ( -- )  FUSE-RESET  FUSE-MAX 1+ 0 do  OP-SCALE FUSE-OP!  loop ;
' OVERFLOW-FUSE E-FUSE TTHROWS

T-REPORT
