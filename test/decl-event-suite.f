\ decl-event-suite.f — behavior + rollback suite for the shared declaration
\ syntax-event transaction (src/core/decl-event.f, package DECL-EVENT; dot
\ habu-type-declarations-shared-14ab0e48). Run BY THE ENGINE over stdin, exactly
\ like test/type-family-rollback-suite.f (the transaction, registry, and checker
\ frame words resolve only at top-level interpret, never inside a checked ':'
\ body):
\     bin/hb < test/decl-event-suite.f
\ Proves: atomic publication + event reflection; malformed and nested streams roll
\ back EVERY watermark (event arena, field ordinal, variant ordinal, current
\ variant, field-record cursor, and — via the enclosing checker candidate frame —
\ the family/variant/schema registry cursors); STRUCTURE and ENUM consumers observe
\ identical field events; the field-record name gate throws pass through unchanged;
\ duplicate POLICY / DERIVE and out-of-range arity reject; snapshot identity is
\ deterministic. A failure prints F<index> + detail; REPORT exits 1 on any fail.
\
\ Each field-publishing case uses its OWN family so committed field rows never
\ collide on slot/byte layout across cases; header-only cases reuse one family.

variable #FAIL
variable #CASE

: T-FAIL ( -- )
   [char] F emit #CASE @ .
   #FAIL @ 1 + #FAIL ! ;
: T= ( n n -- ) {: got:n want:n :}
   #CASE @ 1 + #CASE !
   got want <> if
      T-FAIL s" assert: expected " type want . s" got " type got . cr
   then ;
: T-TRUE ( bool -- ) {: b:bool :}
   #CASE @ 1 + #CASE !
   b 0= if T-FAIL s" assert: expected true" type cr then ;

\ whitebox boundary (dot habu-hb-crash-bare-c5be6634): sealed pre-hook registry /
\ checker-frame colon words probed at top level go through named trusted shims.
TRUSTED: TWX-TFAM-RESET ( -- ) TFAM-RESET ;
TRUSTED: TWX-SCHEMA-RESET ( -- ) SCHEMA-RESET ;
TRUSTED: TWX-TFAM-DECL ( ptr u8 n n ptr u8 n n n -- n ) TFAM-DECL ;
TRUSTED: TWX-SCHEMA-PARAM ( n -- n ) SCHEMA-PARAM ;
TRUSTED: TWX-SCHEMA-ROOT+ ( n -- n ) SCHEMA-ROOT+ ;
TRUSTED: TWX-CAND-START ( -- ) CHECK-CANDIDATE-START ;
TRUSTED: TWX-CAND-DONE ( n -- n ) CHECK-CANDIDATE-DONE ;

\ scratch cells.
variable TOK   variable ITOK          \ live + inner transaction tokens
variable SCHROOT                       \ a param-0 schema root shared by every field
variable TC                            \ caught throw code
variable IDA   variable IDB   variable IDC
variable VAR-S   variable VAR-E
variable P-TFAM   variable P-SUMV   variable P-PF   variable P-SCHN
variable P-SCHR   variable P-STRU   variable P-DEV
\ per-case family ids (all arity 2 so a param-0 field schema is legal).
variable FP1   variable FE2   variable FD3   variable FS7   variable FV7   variable FN8

TWX-TFAM-RESET
TWX-SCHEMA-RESET
DECL-EVENT:RESET
s" de" CHECKER-PACKAGE-PUBLIC s" p1" 2 TK-PRODUCT TWX-TFAM-DECL FP1 !
s" de" CHECKER-PACKAGE-PUBLIC s" e2" 2 TK-SUM     TWX-TFAM-DECL FE2 !
s" de" CHECKER-PACKAGE-PUBLIC s" d3" 2 TK-PRODUCT TWX-TFAM-DECL FD3 !
s" de" CHECKER-PACKAGE-PUBLIC s" s7" 2 TK-PRODUCT TWX-TFAM-DECL FS7 !
s" de" CHECKER-PACKAGE-PUBLIC s" v7" 2 TK-SUM     TWX-TFAM-DECL FV7 !
s" de" CHECKER-PACKAGE-PUBLIC s" n8" 2 TK-PRODUCT TWX-TFAM-DECL FN8 !
0 TWX-SCHEMA-PARAM TWX-SCHEMA-ROOT+ SCHROOT !

\ ---------------------------------------------------------------------------
\ 1. Atomic publication + event reflection. A product declaration with a header
\    (arity) and one field publishes as one contiguous stream; nothing is visible
\    before PUBLISH.
\ ---------------------------------------------------------------------------
TYPE-FIELD:COUNT P-PF !
DECL-EVENT:OPEN TOK !
TOK @ FP1 @ DECL-EVENT:DECL TOK !
TOK @ FP1 @ 2 DECL-EVENT:ARITY TOK !
TOK @ FP1 @ s" x" SCHROOT @ 0 1 0 CELL CELL 0 DECL-EVENT:FIELD TOK !
DECL-EVENT:COUNT 0 T=                                  \ nothing published yet
TOK @ FP1 @ P-PF @ DECL-EVENT:FIELD-SCHEMA@ SCHROOT @ T= \ exact live token/family/row may read provisional schema
TOK @ FE2 @ P-PF @ ' DECL-EVENT:FIELD-SCHEMA@ catch TC ! drop drop drop
TC @ 7173 T=                                            \ foreign family cannot use the token
TOK @ DECL-EVENT:PUBLISH
DECL-EVENT:COUNT 3 T=                                  \ DECL, ARITY, FIELD published atomically
0 DECL-EVENT:DECL? T-TRUE
1 DECL-EVENT:ARITY? T-TRUE
2 DECL-EVENT:FIELD? T-TRUE
2 DECL-EVENT:FAMILY@ FP1 @ T=
2 DECL-EVENT:VAR@ DECL-EVENT:NO-VARIANT T=            \ structure field: no variant
TYPE-FIELD:COUNT P-PF @ 1 + T=                         \ exactly one field committed
TOK @ FP1 @ P-PF @ ' DECL-EVENT:FIELD-SCHEMA@ catch TC ! drop drop drop
TC @ 7161 T=                                            \ consumed token cannot read the now-committed row
' DECL-EVENT:CURRENT catch TC !
TC @ 7161 T=                                            \ CURRENT is coordinator-owned, never a lazy opener

\ ---------------------------------------------------------------------------
\ 2. Rollback restores EVERY watermark. Inside a checker candidate, open a sum
\    declaration, register a variant, add a payload field, then ROLLBACK the
\    event transaction and pop the candidate; every registry high-water returns
\    to its pre-declaration baseline and nothing new publishes.
\ ---------------------------------------------------------------------------
TFAM-N@ P-TFAM !   SUMV-N@ P-SUMV !   TYPE-FIELD:COUNT P-PF !
SCHEMA-N@ P-SCHN !   SCHEMA-ROOT-N@ P-SCHR !   TF-STR-U@ P-STRU !
DECL-EVENT:COUNT P-DEV !
TWX-CAND-START
   DECL-EVENT:OPEN TOK !
   TOK @ FE2 @ DECL-EVENT:DECL TOK !
   TOK @ FE2 @ s" va" DECL-EVENT:VARIANT TOK !         \ registers a variant (SUMV-ADD)
   DECL-EVENT:CURRENT-VARIANT SUMV-N@ 1 - T=           \ selector is the just-added variant id
   TOK @ FE2 @ s" y" SCHROOT @ 0 1 0 CELL CELL 0 DECL-EVENT:FIELD TOK !
   TOK @ DECL-EVENT:ROLLBACK                            \ retire events + field rows + selector
0 TWX-CAND-DONE drop                                    \ retire the variant + any schema
TFAM-N@ P-TFAM @ T=
SUMV-N@ P-SUMV @ T=                                     \ variant retired
TYPE-FIELD:COUNT P-PF @ T=                              \ field row retired
SCHEMA-N@ P-SCHN @ T=
SCHEMA-ROOT-N@ P-SCHR @ T=
TF-STR-U@ P-STRU @ T=                                   \ interned names retired
DECL-EVENT:COUNT P-DEV @ T=                             \ nothing published
DECL-EVENT:CURRENT-VARIANT DECL-EVENT:NO-VARIANT T=     \ selector restored

\ ---------------------------------------------------------------------------
\ 3. Field name-gate throw passes through unchanged and the stream rolls back.
\    A duplicate field name is rejected by the field record (E-TFAM-DUP 7102),
\    not a second gate here; the rejected declaration leaves no committed field.
\ ---------------------------------------------------------------------------
TYPE-FIELD:COUNT P-PF !
TWX-CAND-START
   DECL-EVENT:OPEN TOK !
   TOK @ FD3 @ DECL-EVENT:DECL TOK !
   TOK @ FD3 @ s" dup" SCHROOT @ 0 1 0 CELL CELL 0 DECL-EVENT:FIELD TOK !
   TOK @ FD3 @ s" dup" SCHROOT @ 1 1 CELL CELL CELL 0 ' DECL-EVENT:FIELD catch
      TC ! 2drop 2drop 2drop 2drop 2drop drop            \ restore the 11 inputs catch pushed back
   TOK @ DECL-EVENT:ROLLBACK
0 TWX-CAND-DONE drop
TC @ 7102 T=                                            \ E-TFAM-DUP surfaced from the field record
TYPE-FIELD:COUNT P-PF @ T=                              \ both provisional rows retired

\ ---------------------------------------------------------------------------
\ 4. Duplicate POLICY clause in one declaration rejects (E-DEV-DUP-POLICY 7163).
\ ---------------------------------------------------------------------------
DECL-EVENT:OPEN TOK !
TOK @ FP1 @ DECL-EVENT:DECL TOK !
TOK @ FP1 @ 0 DECL-EVENT:POLICY TOK !                   \ first POLICY ok
TOK @ FP1 @ 1 ' DECL-EVENT:POLICY catch TC ! drop drop drop
TC @ 7163 T=
TOK @ DECL-EVENT:ROLLBACK

\ ---------------------------------------------------------------------------
\ 5. DERIVE: the same feature twice rejects (E-DEV-DUP-DERIVE 7164); two distinct
\    features are accepted.
\ ---------------------------------------------------------------------------
DECL-EVENT:OPEN TOK !
TOK @ FP1 @ DECL-EVENT:DECL TOK !
TOK @ FP1 @ 1 DECL-EVENT:DERIVE TOK !                   \ feature 1 (eq)
TOK @ FP1 @ 2 DECL-EVENT:DERIVE TOK !                   \ feature 2 (hash) — distinct, ok
TOK @ FP1 @ 1 ' DECL-EVENT:DERIVE catch TC ! drop drop drop
TC @ 7164 T=
TOK @ DECL-EVENT:ROLLBACK

\ ---------------------------------------------------------------------------
\ 6. Arity outside [0, 26] rejects (E-DEV-ARITY 7108).
\ ---------------------------------------------------------------------------
DECL-EVENT:OPEN TOK !
TOK @ FP1 @ DECL-EVENT:DECL TOK !
TOK @ FP1 @ 27 ' DECL-EVENT:ARITY catch TC ! drop drop drop
TC @ 7108 T=
TOK @ DECL-EVENT:ROLLBACK

\ ---------------------------------------------------------------------------
\ 7. STRUCTURE and ENUM consumers observe IDENTICAL field events: the same field
\    event kind, differing only in the current-variant selector. Structure field
\    carries NO-VARIANT; enum payload field carries the open variant id.
\ ---------------------------------------------------------------------------
DECL-EVENT:RESET
DECL-EVENT:OPEN TOK !
TOK @ FS7 @ DECL-EVENT:DECL TOK !
TOK @ FS7 @ s" sx" SCHROOT @ 0 1 0 CELL CELL 0 DECL-EVENT:FIELD TOK !
TOK @ DECL-EVENT:PUBLISH
1 DECL-EVENT:FIELD? T-TRUE                              \ structure field event (DECL=0, FIELD=1)
1 DECL-EVENT:VAR@ VAR-S !                               \ its selector = NO-VARIANT
DECL-EVENT:RESET
DECL-EVENT:OPEN TOK !
TOK @ FV7 @ DECL-EVENT:DECL TOK !
TOK @ FV7 @ s" ex" DECL-EVENT:VARIANT TOK !
TOK @ FV7 @ s" ey" SCHROOT @ 0 1 0 CELL CELL 0 DECL-EVENT:FIELD TOK !
TOK @ DECL-EVENT:PUBLISH
2 DECL-EVENT:FIELD? T-TRUE                              \ enum payload field event (DECL=0, VARIANT=1, FIELD=2)
2 DECL-EVENT:VAR@ VAR-E !                               \ its selector = the open variant id
VAR-S @ DECL-EVENT:NO-VARIANT T=                        \ structure: no variant
VAR-E @ DECL-EVENT:NO-VARIANT <> T-TRUE                 \ enum: a real variant selector

\ ---------------------------------------------------------------------------
\ 8. Nested event streams: an inner OPEN…ROLLBACK retires only the inner
\    watermarks; the outer frame survives and publishes.
\ ---------------------------------------------------------------------------
DECL-EVENT:RESET
DECL-EVENT:OPEN TOK !                                   \ outer
TOK @ FN8 @ DECL-EVENT:DECL TOK !
TOK @ FN8 @ s" n0" SCHROOT @ 0 1 0 CELL CELL 0 DECL-EVENT:FIELD TOK !
DECL-EVENT:OPEN ITOK !                                  \ inner
ITOK @ FN8 @ s" n1" SCHROOT @ 1 1 CELL CELL CELL 0 DECL-EVENT:FIELD ITOK !
ITOK @ DECL-EVENT:ROLLBACK                              \ retire only the inner field/event
TOK @ DECL-EVENT:PUBLISH                                \ outer publishes n0 only
DECL-EVENT:COUNT 2 T=                                   \ DECL + n0 (n1 rolled back)
0 DECL-EVENT:DECL? T-TRUE
1 DECL-EVENT:FIELD? T-TRUE

\ ---------------------------------------------------------------------------
\ 9. Deterministic snapshot identity: identical declarations fold to an identical
\    identity; a different declaration folds to a different one.
\ ---------------------------------------------------------------------------
DECL-EVENT:RESET
DECL-EVENT:OPEN TOK !
TOK @ FP1 @ DECL-EVENT:DECL TOK !
TOK @ FP1 @ 2 DECL-EVENT:ARITY TOK !
TOK @ DECL-EVENT:PUBLISH
DECL-EVENT:IDENTITY IDA !
DECL-EVENT:RESET
DECL-EVENT:OPEN TOK !
TOK @ FP1 @ DECL-EVENT:DECL TOK !
TOK @ FP1 @ 2 DECL-EVENT:ARITY TOK !
TOK @ DECL-EVENT:PUBLISH
DECL-EVENT:IDENTITY IDB !
DECL-EVENT:RESET
DECL-EVENT:OPEN TOK !
TOK @ FP1 @ DECL-EVENT:DECL TOK !
TOK @ FP1 @ 3 DECL-EVENT:ARITY TOK !                    \ different arity
TOK @ DECL-EVENT:PUBLISH
DECL-EVENT:IDENTITY IDC !
IDA @ IDB @ T=                                          \ same declaration -> same identity
IDA @ IDC @ <> T-TRUE                                   \ different declaration -> different identity

\ ---------------------------------------------------------------------------
\ 10. A stale / non-LIFO token is rejected (E-DEV-TX 7161).
\ ---------------------------------------------------------------------------
DECL-EVENT:OPEN TOK !
TOK @ 1 + FP1 @ ' DECL-EVENT:DECL catch TC ! drop drop
TC @ 7161 T=
TOK @ DECL-EVENT:ROLLBACK

\ ---------------------------------------------------------------------------
: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" decl-event-suite: failures" 1 die ;
REPORT
