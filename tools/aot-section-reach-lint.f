\ aot-section-reach-lint.f - nothing may reach into the baked AOT payload section
\ with ADR,. The section grows to megabytes once the compiler chain is captured and
\ ADR, reaches 1 MiB, so a far ADR, fails only at chain scale (`icode: adr out of
\ reach`). A reference into it goes through src/habu/habu2.f TADR, whatever a
\ label's offset is, so the section's rows may reorder freely.
\ Run: bin/hb --load tools/aot-section-reach-lint.f

require src/lib/errors.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/vector.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require src/lib/source-lex.f
require tools/lint/label-triple.f

package AOT-REACH-LINT

\ The lint's own findings code, in the unclaimed gap the sibling lints use.
-4820 constant E-AOT-REACH-LEX

create AR-SLAB LINT-SLAB:CELLS cells allot

variable BAD  variable LI  variable IN-SECT

: RESET ( -- )
   LINT-LABEL-TRIPLE:RESET  LINT-FALSE IN-SECT ! ;

\ Overflow of the shared label table is a named refusal: a dropped member is one
\ this lint stops checking.
: CAP-FAIL ( -- )
   s" aot-section-reach-lint: more section labels than LMAX" 1 die ;

: ADD-LABEL ( ptr u8 n -- )
   LINT-LABEL-TRIPLE:ADD-LABEL LINT-NOT IF CAP-FAIL THEN ;

\ ---- pass 1: the section's own labels ---------------------------------------
\ Members are read from EMIT-AOT-SEED itself (`X LABEL@ LBL,` inside it binds X), so
\ a row added to the section is covered at once. IN-SECT opens on the
\ `: EMIT-AOT-SEED` pair and closes on its `;`, both whole WORD tokens.
: SECT-OPEN? ( n -- bool ) {: k:n :}
   k 1+ LINT-LEX:COUNT >= IF LINT-FALSE exit THEN
   k s" :" LINT-LABEL-TRIPLE:LEX-WORD= LINT-NOT IF LINT-FALSE exit THEN
   k 1+ s" EMIT-AOT-SEED" LINT-LABEL-TRIPLE:LEX-WORD= ;

: COLLECT-TOKEN ( n -- ) {: k:n :}
   IN-SECT @ LINT-NOT IF
      k SECT-OPEN? IF LINT-TRUE IN-SECT ! THEN
      exit
   THEN
   k s" ;" LINT-LABEL-TRIPLE:LEX-WORD= IF LINT-FALSE IN-SECT ! exit THEN
   k s" LBL," LINT-LABEL-TRIPLE:TRIPLE? IF
      k LINT-LEX:TOKEN LINT-LABEL-TRIPLE:TAIL ADD-LABEL
   THEN ;

: COLLECT ( -- )
   0 LI !
   begin LI @ LINT-LEX:COUNT < while
      LI @ COLLECT-TOKEN
      LI @ 1+ LI !
   repeat ;

\ ---- pass 2: every ADR, site, anywhere in the file --------------------------
: REPORT ( ptr u8 n n -- ) {: pa:ptr pu:n k:n :}
   s" AOT-REACH " type pa pu type s" :" type k LINT-LEX:LINE@ LINT-MAIN-N$ type
   s" : `" type k LINT-LEX:TOKEN type
   s" ` is in the AOT payload section: use TADR,, not ADR," type cr
   BAD @ 1+ BAD ! ;

: CHECK-TOKEN ( ptr u8 n n -- ) {: pa:ptr pu:n k:n :}
   k s" ADR," LINT-LABEL-TRIPLE:TRIPLE? LINT-NOT IF exit THEN
   k LINT-LEX:TOKEN LINT-LABEL-TRIPLE:TAIL LINT-LABEL-TRIPLE:LABEL?
   IF pa pu k REPORT THEN ;

: CHECK ( ptr u8 n -- ) {: pa:ptr pu:n :}
   0 LI !
   begin LI @ LINT-LEX:COUNT < while
      pa pu LI @ CHECK-TOKEN
      LI @ 1+ LI !
   repeat ;

\ A lexer diagnostic means the scan stopped early and every later token is
\ invisible - including, possibly, the ADR, this lint exists to find. Fail closed
\ and name the file rather than report a clean scan of half of it.
: LEX-FAIL ( ptr u8 n -- ) {: pa:ptr pu:n :}
   s" aot-section-reach-lint: source did not lex: " type pa pu type
   s"  (line " type LINT-LEX:ERROR-LINE@ LINT-MAIN-N$ type s" )" type cr
   E-AOT-REACH-LEX throw ;

public

\ The label bound after the section, past anything it can grow into. It is not
\ bound inside EMIT-AOT-SEED, so pass 1 cannot find it and it is named here.
: SEED-LABEL ( -- ) s" LIMGEND" ADD-LABEL ;

\ Scan one already-loaded source; the file entry and every test fixture go through
\ it. The three-token shape, the qualified-name tail rule and the label table live
\ in tools/lint/label-triple.f. On the string-aware lexer (src/lib/source-lex.f) a
\ triple inside a comment, a string or out of order is no binding.
: SCAN-SOURCE ( ptr u8 n ptr u8 n -- ) {: pa:ptr pu:n a:ptr u:n :}
   a u LINT-LEX:SOURCE
   LINT-LEX:ERROR? IF pa pu LEX-FAIL THEN
   RESET
   COLLECT
   SEED-LABEL
   pa pu CHECK ;

: SCAN-FILE ( ptr u8 n -- ) {: pa:ptr pu:n :}
   pa pu AR-SLAB LINT-SLAB:LOAD
   pa pu  AR-SLAB LINT-SLAB:TEXT  SCAN-SOURCE ;

: LABELS-FOUND ( -- n ) LINT-LABEL-TRIPLE:LABELS ;
: FINDINGS ( -- n ) BAD @ ;
: FINDINGS-RESET ( -- ) 0 BAD ! ;

private

: MAIN ( -- )
   FINDINGS-RESET
   s" src/habu/habu2.f" SCAN-FILE
   BAD @ 0 > IF
      s" aot-section-reach-lint: " type BAD @ LINT-MAIN-N$ type s"  finding(s)" type cr
      s" aot-section-reach-lint: ADR, into the AOT payload section" 1 die
   THEN
   s" aot-section-reach-lint: clean (" type LABELS-FOUND LINT-MAIN-N$ type s"  section label(s) checked)" type cr ;
MAIN

;package
