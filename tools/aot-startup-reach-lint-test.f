\ aot-startup-reach-lint-test.f — fixtures for the startup reach rule.
\
\ Every case goes through AOT-STARTUP-REACH-LINT:SCAN-SOURCE, the same word the
\ real scan of the three emitter files uses; nothing here re-implements the rule.
\ The fixtures are built to fool a text search: the three tokens appear in a
\ comment, inside a string literal, in the wrong order, and the binding sits on
\ either side of the site and in a neighbouring definition. A lint that grepped
\ for `ADR,` would report on most of them.
\ Run: bin/hb --load tools/aot-startup-reach-lint-test.f

require lib/test.f
require tools/aot-startup-reach-lint.f

package AOT-STARTUP-REACH-LINT-TEST
using AOT-STARTUP-REACH-LINT

: SCAN ( ptr u8 n -- ) {: a:ptr u:n :}
   FINDINGS-RESET
   s" [fixture]" a u SCAN-SOURCE ;

\ ---- the two verdicts on a far label ----------------------------------------
\ The label is bound in another definition, so nothing bounds the distance from
\ this site to it: that is the shape the four converted sites had.
: T-FAR-REPORTED ( -- )
   s" : BINDER LFAR LABEL@ LBL, ; : EMIT 11 LFAR LABEL@ ADR, ;" SCAN
   FINDINGS 1 T= ;

: T-TEXT-ADR-CLEAN ( -- )
   s" : BINDER LFAR LABEL@ LBL, ; : EMIT 11 12 LFAR LABEL@ TEXT-ADR, ;" SCAN
   FINDINGS 0 T= ;

\ LTEXT is the base TEXT-ADR, itself reaches, at text offset zero behind every
\ startup site. Over-refusing it would delete the form's own first term.
: T-LTEXT-CLEAN ( -- )
   s" : BINDER LTEXT LABEL@ LBL, ; : EMIT 11 LTEXT LABEL@ ADR, ;" SCAN
   FINDINGS 0 T= ;

\ ---- a definition's body is the bound, in both directions -------------------
: T-BOUND-BEFORE-CLEAN ( -- )
   s" : EMIT LNEAR LABEL@ LBL, 11 LNEAR LABEL@ ADR, ;" SCAN
   FINDINGS 0 T= ;

: T-BOUND-AFTER-CLEAN ( -- )
   s" : EMIT 11 LNEAR LABEL@ ADR, LNEAR LABEL@ LBL, ;" SCAN
   FINDINGS 0 T= ;

\ The definition's closer ends the body. A binding past it is another region.
: T-DEFINITION-CLOSES ( -- )
   s" : EMIT 11 LFAR LABEL@ ADR, ; LFAR LABEL@ LBL," SCAN
   FINDINGS 1 T= ;

\ A site with no enclosing definition has no body to be bounded by, so it is a
\ finding even where the same file binds the label at top level.
: T-TOP-LEVEL-SITE ( -- )
   s" LFAR LABEL@ LBL, 11 LFAR LABEL@ ADR," SCAN
   FINDINGS 1 T= ;

\ ---- fixtures built to fool a text search -----------------------------------
\ PART A: the site is inside a line comment and inside a string literal. Neither
\ is a reference, so a real far label goes unreported here.
: T-SITE-HIDDEN ( -- )
   s\" : BINDER LFAR LABEL@ LBL, ; : EMIT \\ 11 LFAR LABEL@ ADR,\n s\q 11 LFAR LABEL@ ADR,\q ;" SCAN
   FINDINGS 0 T= ;

\ PART B: the binding is inside a line comment, so it binds nothing and the site
\ below it in the same definition is a finding. A substring search sees both.
: T-BINDING-IN-COMMENT ( -- )
   s\" : EMIT \\ LNEAR LABEL@ LBL,\n 11 LNEAR LABEL@ ADR, ;" SCAN
   FINDINGS 1 T= ;

\ PART C: the same three tokens inside a paren comment.
: T-BINDING-IN-PAREN ( -- )
   s" : EMIT ( LNEAR LABEL@ LBL, ) 11 LNEAR LABEL@ ADR, ;" SCAN
   FINDINGS 1 T= ;

\ PART D: the binding is a string literal's payload. The lexer consumes the body
\ opaquely, exactly as the engine parser does, so it binds nothing.
: T-BINDING-IN-STRING ( -- )
   s\" : EMIT s\q LNEAR LABEL@ LBL,\q 11 LNEAR LABEL@ ADR, ;" SCAN
   FINDINGS 1 T= ;

\ PART E: WRONG ROLE. `LABEL@ LNEAR LBL,` and `LNEAR ADR, LABEL@` carry all three
\ tokens with the roles permuted. Neither is a binding or a reference.
: T-WRONG-ROLE ( -- )
   s" : EMIT LABEL@ LNEAR LBL, 11 LNEAR ADR, LABEL@ ;" SCAN
   FINDINGS 0 T= ;

\ ---- qualified and bare spellings are one name ------------------------------
: T-QUALIFIED-BINDING ( -- )
   s" : EMIT BP-CALLER:LNEAR LABEL@ LBL, 11 LNEAR LABEL@ ADR, ;" SCAN
   FINDINGS 0 T= ;

: T-QUALIFIED-REFERENCE ( -- )
   s" : EMIT LNEAR LABEL@ LBL, 11 BP-CALLER:LNEAR LABEL@ ADR, ;" SCAN
   FINDINGS 0 T= ;

\ A name is not its own suffix: LNAMES must not be excused by LAOTNAMES.
: T-SUFFIX-IS-NOT-A-TAIL ( -- )
   s" : EMIT LAOTNAMES LABEL@ LBL, 11 LNAMES LABEL@ ADR, ;" SCAN
   FINDINGS 1 T= ;

\ ---- the real files ---------------------------------------------------------
\ The production scan, through the same entry, over the sources the rule is
\ about. src/habu/rt.f passes the model unchanged: both its ADR, sites name a
\ message label its own definition binds.
: T-REAL-FILES ( -- )
   FINDINGS-RESET
   s" src/habu/aot-lib.f" SCAN-FILE
   s" src/habu/crash.f" SCAN-FILE
   s" src/habu/rt.f" SCAN-FILE
   FINDINGS 0 T= ;

: MAIN ( -- )
   T-RESET
   T-FAR-REPORTED
   T-TEXT-ADR-CLEAN
   T-LTEXT-CLEAN
   T-BOUND-BEFORE-CLEAN
   T-BOUND-AFTER-CLEAN
   T-DEFINITION-CLOSES
   T-TOP-LEVEL-SITE
   T-SITE-HIDDEN
   T-BINDING-IN-COMMENT
   T-BINDING-IN-PAREN
   T-BINDING-IN-STRING
   T-WRONG-ROLE
   T-QUALIFIED-BINDING
   T-QUALIFIED-REFERENCE
   T-SUFFIX-IS-NOT-A-TAIL
   T-REAL-FILES
   T-REPORT
   s" aot-startup-reach-lint-test: ok" type cr ;
MAIN

;using
;package
