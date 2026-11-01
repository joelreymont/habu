\ clobber-join-exit.f - fixture for an entry after an exit, read as text by
\ clobber-lint-test.f (never loaded). A global entry starts a fresh path.

variable LJOIN-CALLEE
variable LJOIN-QUIT
variable LJOIN-NEXT

: EMIT-JOIN-CALLEE ( -- )
   LJOIN-CALLEE LABEL@ LBL,
   12 0 MOVZ,  RET, ;

\ The call clobbers x30, but the exit ends that path; RET, reads the x30 of
\ LJOIN-NEXT's own callers: no finding.
: EMIT-JOIN-EXIT ( -- )
   LJOIN-QUIT LABEL@ LBL,
   LJOIN-CALLEE LABEL@ BL,
   0 0 MOVZ,  NR-EXIT-GROUP SYS,
   LJOIN-NEXT LABEL@ LBL,
   RET, ;
