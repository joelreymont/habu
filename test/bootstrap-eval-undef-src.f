\ bootstrap-eval-undef-src.f - an undefined word inside evaluate must not leak.

\ src/core/layout-buffer.f DYNAMIC-BUFFER evaluates an accessor built as source
\ text and never reads EVALERR-CELL, so an in-evaluate undefined word that rolls
\ back and returns hands it a body that calls nothing, and the stage0 build dies
\ of SIGSEGV later instead of naming the token. Native throws rc 70 instead
\ (src/habu/habu2.f EM-COMPILE-UNDEF); this is the stage0 mirror's proof:
\   caught - with a handler in scope the failure arrives as throw 70,
\   fatal  - with no handler it exits 70 and never reaches the LEAKED line.
\ Both halves are needed: rolling back and returning passes the fatal rc only by
\ accident, and a blunt exit inside evaluate passes the fatal half but not the
\ caught one.

: EU-CHECK ( n -- )
   70 = 0= if s" bootstrap-eval-undef: evaluate failure is not a catchable 70" 1 die then ;

s" BOOTSTRAP-EVAL-UNDEF-ARMED" type cr

\ The caught throw restores the depth catch saw, so the string's two cells sit
\ under the code.
s" : ZZ-CAUGHT ( -- ) ZZ-NO-SUCH-WORD ;" ' evaluate catch EU-CHECK 2drop

s" : ZZ-LEAK ( -- ) ZZ-NO-SUCH-PKG:ZZ-NO-SUCH-WORD ;" evaluate

s" BOOTSTRAP-EVAL-UNDEF-LEAKED" type cr
