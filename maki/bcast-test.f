\ maki/bcast-test.f - checked tests for the broadcast-operand classifier.
\ Each of the four legal classes (full / 1xC row / Rx1 col / 1x1 scalar) and the illegal
\ (non-1-non-full dim) sentinel, plus the degenerate R=1 / C=1 targets where classes
\ coincide and FULL (the cheapest load) must win. Mirrors the executor EX-BC@ mapping.

require lib/test.f
require maki/bcast.f

package MAKI

T-RESET

\ ---- the four legal classes against a 4x8 region ---------------------------
4 8 4 8 BC-CLASS BC-FULL    T=      \ RxC : the full region shape
1 8 4 8 BC-CLASS BC-ROW     T=      \ 1xC : row-broadcast (bias)
4 1 4 8 BC-CLASS BC-COL     T=      \ Rx1 : column-broadcast
1 1 4 8 BC-CLASS BC-SCALAR  T=      \ 1x1 : scalar-broadcast (scale)

\ ---- illegal: a dim that is neither 1 nor full -----------------------------
3 8 4 8 BC-CLASS BC-ILLEGAL T=      \ rows 3 is neither 1 nor 4
4 3 4 8 BC-CLASS BC-ILLEGAL T=      \ cols 3 is neither 1 nor 8
2 5 4 8 BC-CLASS BC-ILLEGAL T=      \ both dims illegal

\ ---- degenerate targets: classes coincide, FULL must win (cheapest load) ----
1 8 1 8 BC-CLASS BC-FULL    T=      \ R=1 : full and row coincide -> full
4 1 4 1 BC-CLASS BC-FULL    T=      \ C=1 : full and col coincide -> full
1 1 1 1 BC-CLASS BC-FULL    T=      \ 1x1 target : all classes coincide -> full

T-REPORT

end-package
