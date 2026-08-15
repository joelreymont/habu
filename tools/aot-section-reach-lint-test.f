\ aot-section-reach-lint-test.f — fixtures for the AOT-section reach rule.
\
\ Every case goes through AOT-REACH-LINT:SCAN-SOURCE, the same word the real
\ scan of src/habu/habu2.f uses; nothing here re-implements the rule. The
\ fixtures are built to fool a text search: the forbidden three tokens appear in
\ a comment, inside a string literal, split across the wrong roles and in the
\ wrong order, and a member is bound twice. A lint that grepped for `LAOTNAMES`
\ or for `ADR,` would report on most of them.
\ Run: bin/hb --load tools/aot-section-reach-lint-test.f

require lib/test.f
require tools/aot-section-reach-lint.f

package AOT-REACH-LINT-TEST
using AOT-REACH-LINT

\ Every fixture opens the section the way habu2.f does, so the member set is
\ derived by the lint rather than asserted here.
: SCAN ( ptr u8 n -- ) {: a:ptr u:n :}
   FINDINGS-RESET
   s" [fixture]" a u SCAN-SOURCE ;

\ ---- the two verdicts on a real member --------------------------------------
: T-ADR-REPORTED ( -- )
   s" : EMIT-AOT-SEED LAOTNAMES LABEL@ LBL, ; : R 5 LAOTNAMES LABEL@ ADR, ;" SCAN
   FINDINGS 1 T=
   LABELS-FOUND 2 T= ;                          \ the member plus LIMGEND

: T-TADR-CLEAN ( -- )
   s" : EMIT-AOT-SEED LAOTNAMES LABEL@ LBL, ; : R 5 10 LAOTNAMES LABEL@ TADR, ;" SCAN
   FINDINGS 0 T= ;

\ A label the section does NOT bind keeps its ADR,: that is how the engine code
\ half is measured, so over-refusing it would delete the window's first term.
: T-NON-MEMBER-CLEAN ( -- )
   s" : EMIT-AOT-SEED LAOTNAMES LABEL@ LBL, ; : R 5 LDICT LABEL@ ADR, ;" SCAN
   FINDINGS 0 T= ;

\ LSRC is exactly that case and it is the one that matters: the baked-source
\ reader must keep ADR,.
: T-LSRC-CLEAN ( -- )
   s" : EMIT-AOT-SEED LAOTNAMES LABEL@ LBL, ; : R 12 LSRC LABEL@ ADR, ;" SCAN
   FINDINGS 0 T= ;

\ LIMGEND is bound after the section, so it is a member without appearing in it.
: T-IMAGE-END-REPORTED ( -- )
   s" : EMIT-AOT-SEED LAOTNAMES LABEL@ LBL, ; : R 14 LIMGEND LABEL@ ADR, ;" SCAN
   FINDINGS 1 T= ;

\ ---- fixtures built to fool a text search -----------------------------------
\ PART A: the binding is inside a line comment, so LAOTNAMES is not a member and
\ the ADR, below is not a finding. A substring search sees both lines.
: T-BINDING-IN-COMMENT ( -- )
   s\" : EMIT-AOT-SEED \\ LAOTNAMES LABEL@ LBL,\n ; : R 5 LAOTNAMES LABEL@ ADR, ;" SCAN
   FINDINGS 0 T=
   LABELS-FOUND 1 T= ;                          \ LIMGEND alone

\ PART B: the same three tokens inside a paren comment.
: T-BINDING-IN-PAREN ( -- )
   s" : EMIT-AOT-SEED ( LAOTNAMES LABEL@ LBL, ) ; : R 5 LAOTNAMES LABEL@ ADR, ;" SCAN
   FINDINGS 0 T= ;

\ PART C: the binding is a string literal's payload. The lexer consumes the body
\ opaquely, exactly as the engine parser does, so it binds nothing.
: T-BINDING-IN-STRING ( -- )
   s\" : EMIT-AOT-SEED s\q LAOTNAMES LABEL@ LBL,\q ; : R 5 LAOTNAMES LABEL@ ADR, ;" SCAN
   FINDINGS 0 T= ;

\ PART D: a real member, but the offending ADR, is written inside a comment and
\ inside a string. Neither is a reference.
: T-REFERENCE-HIDDEN ( -- )
   s\" : EMIT-AOT-SEED LAOTNAMES LABEL@ LBL, ; : R \\ 5 LAOTNAMES LABEL@ ADR,\n s\q 5 LAOTNAMES LABEL@ ADR,\q ;" SCAN
   FINDINGS 0 T= ;

\ PART E: WRONG ROLE. `LABEL@ LAOTNAMES LBL,` and `LAOTNAMES ADR, LABEL@` carry
\ all three tokens with the roles permuted. Neither is a binding or a reference.
: T-WRONG-ROLE ( -- )
   s" : EMIT-AOT-SEED LABEL@ LAOTNAMES LBL, ; : R 5 LAOTNAMES ADR, LABEL@ ;" SCAN
   FINDINGS 0 T=
   LABELS-FOUND 1 T= ;

\ PART F: WRONG SECTION. The binding is in a neighbouring definition, not in
\ EMIT-AOT-SEED, so the label is not a member of this section.
: T-BOUND-ELSEWHERE ( -- )
   s" : OTHER LAOTNAMES LABEL@ LBL, ; : EMIT-AOT-SEED ; : R 5 LAOTNAMES LABEL@ ADR, ;" SCAN
   FINDINGS 0 T= ;

\ PART G: the definition's closer ends the section. A binding after it is out.
: T-SECTION-CLOSES ( -- )
   s" : EMIT-AOT-SEED LAOTDICT LABEL@ LBL, ; LAOTNAMES LABEL@ LBL, : R 5 LAOTNAMES LABEL@ ADR, ;" SCAN
   FINDINGS 0 T= ;

\ PART H: DUPLICATED. One label bound twice is one member, and its reference is
\ one finding, not two.
: T-DUPLICATE-BINDING ( -- )
   s" : EMIT-AOT-SEED LAOTNAMES LABEL@ LBL, LAOTNAMES LABEL@ LBL, ; : R 5 LAOTNAMES LABEL@ ADR, ;" SCAN
   FINDINGS 1 T= ;

\ PART I: REORDERED. The rule is the section, not the order, so moving a row
\ inside it changes no verdict. This is what makes an intra-section reorder free.
: T-REORDER-INVARIANT ( -- )
   s" : EMIT-AOT-SEED LAOTDICT LABEL@ LBL, LAOTNAMES LABEL@ LBL, ; : R 5 LAOTNAMES LABEL@ ADR, ;" SCAN
   FINDINGS 1 T=
   s" : EMIT-AOT-SEED LAOTNAMES LABEL@ LBL, LAOTDICT LABEL@ LBL, ; : R 5 LAOTNAMES LABEL@ ADR, ;" SCAN
   FINDINGS 1 T= ;

\ ---- qualified and bare spellings are one name ------------------------------
\ The section binds AOT-XTSITE:LROWS and the reader inside that package spells it
\ LROWS. Both directions must match, or half the real sites go unchecked.
: T-QUALIFIED-BINDING ( -- )
   s" : EMIT-AOT-SEED AOT-XTSITE:LROWS LABEL@ LBL, ; : R 21 LROWS LABEL@ ADR, ;" SCAN
   FINDINGS 1 T= ;

: T-QUALIFIED-REFERENCE ( -- )
   s" : EMIT-AOT-SEED LROWS LABEL@ LBL, ; : R 21 AOT-XTSITE:LROWS LABEL@ ADR, ;" SCAN
   FINDINGS 1 T= ;

\ A name is not its own suffix: LNAMES must not match LAOTNAMES.
: T-SUFFIX-IS-NOT-A-TAIL ( -- )
   s" : EMIT-AOT-SEED LAOTNAMES LABEL@ LBL, ; : R 5 LNAMES LABEL@ ADR, ;" SCAN
   FINDINGS 0 T= ;

\ ---- the real file ----------------------------------------------------------
\ The production scan, through the same entry, over the source the rule is about.
: T-REAL-FILE ( -- )
   FINDINGS-RESET
   s" src/habu/habu2.f" SCAN-FILE
   FINDINGS 0 T=
   LABELS-FOUND 28 T= ;

: MAIN ( -- )
   T-RESET
   T-ADR-REPORTED
   T-TADR-CLEAN
   T-NON-MEMBER-CLEAN
   T-LSRC-CLEAN
   T-IMAGE-END-REPORTED
   T-BINDING-IN-COMMENT
   T-BINDING-IN-PAREN
   T-BINDING-IN-STRING
   T-REFERENCE-HIDDEN
   T-WRONG-ROLE
   T-BOUND-ELSEWHERE
   T-SECTION-CLOSES
   T-DUPLICATE-BINDING
   T-REORDER-INVARIANT
   T-QUALIFIED-BINDING
   T-QUALIFIED-REFERENCE
   T-SUFFIX-IS-NOT-A-TAIL
   T-REAL-FILE
   T-REPORT
   s" aot-section-reach-lint-test: ok" type cr ;
MAIN

;using
;package
