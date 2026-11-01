\ clobber-host-def.f - fixture defining a host word for clobber-host-call.f,
\ read as text by clobber-lint-test.f (never loaded).

: REWRITE-ARG ( n -- ) {: r:n :}
   r 0 MOVZ, ;
