\ Read as source by clobber-lint-test.f; never executed.
\ The escaped string ends at its closing quote: a tokenizer that ran it past the
\ quote swallowed EMIT-LITERAL's semicolon, joined the two definitions and
\ carried the x12 write into a false clobber.
: EMIT-LITERAL ( -- ) 12 9 0 ADDI, s\" \n" ;
: EMIT-NEXT ( -- ) 10 11 PROT-GUARD:CALL 4 12 0 LDRB, ;

\ Emitter-shaped literal text is inert, but the real stale read after it counts.
: EMIT-REAL ( -- )
   s\" \n\" : FAKE ; LFAKE LABEL@ LBL, 10 11 PROT-GUARD:CALL"
   .( : FAKE ; 10 11 PROT-GUARD:CALL )
   12 9 0 ADDI,
   10 11 PROT-GUARD:CALL
   4 12 0 LDRB, ;
