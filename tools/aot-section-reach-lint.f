\ aot-section-reach-lint.f — nothing may reach into the baked AOT payload
\ section with ADR,.
\
\ WHY A LINT AND NOT A MEASUREMENT. The AOT section is the last of the three
\ parts of an emitted image (src/arch/arm64/icode.f derives the code window from
\ all three). It holds the captured compiler blob, the dictionary records, the
\ relocation tables, the name pool and the captured DATA window: 30 KiB in the
\ small engine and megabytes once the compiler chain is captured. ADR, reaches
\ 1 MiB, so a reader that uses it works on every image anyone builds by hand and
\ fails on the one that matters — the build dies `icode: adr out of reach`, at
\ chain scale only, far from the line that caused it. That is exactly what
\ happened (dot habu-reach-the-seed-d1326596: 29 sites at once).
\ So the rule is about the SECTION and not about any measured distance: a
\ reference into it goes through src/habu/habu2.f TADR,, whatever a particular
\ label's offset happens to be today. Reordering the section's rows is then free,
\ and this lint is what keeps the rule from decaying into the list of labels that
\ were far enough to notice on the day it was written.
\
\ HOW THE SET IS DERIVED, NOT LISTED. The section's labels are read out of
\ EMIT-AOT-SEED itself: inside that definition, `X LABEL@ LBL,` binds X here, so
\ X is a member. Nothing is spelled out, so a row added to the section is covered
\ the moment it is written. LIMGEND joins them because it is bound after the
\ section, past everything the section can grow into.
\
\ Structure, not text: the three-token shape, the qualified-name tail rule and
\ the label table live in tools/lint/label-triple.f, shared with the sibling
\ startup lint and stated there. The scan runs on the shared string-aware lexer
\ (tools/lint/source-lex.f), so the same three tokens inside a comment, inside a
\ string literal, or in the wrong order are not a binding and not a reference.
\ tools/aot-section-reach-lint-test.f pins each of those.
\
\ Run: bin/hb --load tools/aot-section-reach-lint.f

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/vector.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/source-lex.f
require tools/lint/label-triple.f

package AOT-REACH-LINT

\ The lint's own findings code, in the unclaimed gap the sibling lints use.
-4820 constant E-AOT-REACH-LEX

create AR-SLAB LINT-SLAB:CELLS cells allot

variable BAD  variable LI  variable IN-SECT

: RESET ( -- )
   LINT-LABEL-TRIPLE:RESET  LINT-FALSE IN-SECT ! ;

\ The section binds 33 labels today, well inside the shared table's cap. Its
\ overflow is this named refusal rather than a silent truncation, because a
\ dropped member is a member this lint stops checking.
: CAP-FAIL ( -- )
   s" aot-section-reach-lint: more section labels than LMAX" 1 die ;

: ADD-LABEL ( ptr u8 n -- )
   LINT-LABEL-TRIPLE:ADD-LABEL LINT-NOT IF CAP-FAIL THEN ;

\ ---- pass 1: the section's own labels ---------------------------------------
\ IN-SECT opens on the `: EMIT-AOT-SEED` pair and closes on the definition's `;`.
\ Both are whole WORD tokens, so neither can be forged from prose.
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

\ Scan one already-loaded source. This is the whole lint; the file entry below
\ and every fixture in the test go through it.
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
