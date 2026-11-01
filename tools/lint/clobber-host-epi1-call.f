\ clobber-host-epi1-call.f - fixture for a caller of clobber-host-epi1-def.f's
\ LEPI1, read as text by clobber-lint-test.f (never loaded).

\ LEPI1 leaves x12 alone: no finding.
: EMIT-EPI1-CALLER ( -- )
   12 0 MOVZ,
   LEPI1 LABEL@ BL,
   4 12 0 LDRB, ;
