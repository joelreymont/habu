\ aot.f - thin AOT maker entry.
\
\ Load after src/habu/aot-lib.f.

: GO ( -- )
   AOT-RUNTIME-ARGS
   READ-PROG
   SENTSET
   ['] USER-HOOK set-check
   AOT-PB@ DATA-VA INP-CELL + !
   AOT-PB@ PN @ + DATA-VA INE-CELL + ! ;

GO
