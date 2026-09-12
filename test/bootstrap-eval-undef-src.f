\ bootstrap-eval-undef-src.f - an undefined word inside evaluate must not leak.

\ src/core/layout-buffer.f DYNAMIC-BUFFER builds an accessor as source text and
\ evaluates it, and it never reads EVALERR-CELL. So an engine whose in-evaluate
\ undefined-word path rolls back and RETURNS, recording the failure only in that
\ cell, hands the caller a definition whose body calls nothing and keeps
\ interpreting: the stage0 build then died of SIGSEGV somewhere later instead of
\ naming the token. Native retired that leg as fail-open (src/habu/habu2.f
\ EM-COMPILE-UNDEF) in favour of a catchable rc-70 throw, and this fixture is the
\ stage0 mirror's proof of the same two-part contract:
\   caught   - with a handler in scope the failure arrives as throw 70,
\   fatal    - with no handler it exits 70 and never reaches the LEAKED line.
\ Both halves are needed. Rolling back and returning passes the fatal half's rc
\ only by accident, and a blunt exit inside evaluate would pass the fatal half
\ while failing the caught one.

TRUSTED: EU-TRY ( -- n )
   [: s" : ZZ-CAUGHT ( -- ) ZZ-NO-SUCH-WORD ;" evaluate ;] catch ;

: EU-CHECK ( n -- )
   70 = 0= if s" bootstrap-eval-undef: evaluate failure is not a catchable 70" 1 die then ;

s" BOOTSTRAP-EVAL-UNDEF-ARMED" type cr

EU-TRY EU-CHECK

s" : ZZ-LEAK ( -- ) ZZ-NO-SUCH-PKG:ZZ-NO-SUCH-WORD ;" evaluate

s" BOOTSTRAP-EVAL-UNDEF-LEAKED" type cr
