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
\ deterministic; standalone PUBLISH cannot bypass the contiguity preflight.
\ A failure prints F<index> + detail; REPORT exits 1 on any fail.
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
   TOK @ FE2 @ s" valid-a" DECL-EVENT:VARIANT TOK !    \ registers a variant (SUMV-ADD)
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
ITOK @ FN8 @ DECL-EVENT:DECL ITOK !
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
\ 11. Standalone PUBLISH runs the same contiguity preflight as the coordinator.
\     Inject one field row through this package's own field token without its
\     matching event; PUBLISH rejects before either published high-water moves.
\ ---------------------------------------------------------------------------
package DECL-EVENT

: TEST-ADD-UNTRACKED-FIELD ( n -- ) {: fam:n :}
   DEV-TX-TOP DEVTX.FLDTOK @ fam DEV-NO-VARIANT
   s" untracked" SCHROOT @ 0 1 0 CELL CELL 0 DEV-FLD-ADD drop ;

: TEST-PUBLISH ( -- ) TOK @ PUBLISH ;

: TEST-PUBLISH-CATCH ( -- n )
   [: TEST-PUBLISH ;] catch ;

: TEST-STANDALONE-PREFLIGHT ( -- )
   COUNT P-DEV !
   TYPE-FIELD:COUNT P-PF !
   OPEN TOK !
   TOK @ FD3 @ DECL TOK !
   FD3 @ TEST-ADD-UNTRACKED-FIELD
   TEST-PUBLISH-CATCH TC !
   TC @ E-DEV-STATE T=
   COUNT P-DEV @ T=
   TYPE-FIELD:COUNT P-PF @ T=
   DEPTH 1 T=
   TC @ 0 <> if TOK @ ROLLBACK then
   DEPTH 0 T= ;

TEST-STANDALONE-PREFLIGHT

\ ---------------------------------------------------------------------------
\ 12. A frame opened before a nested declaration still receives a distinct
\     owner when its own declaration event is emitted.
\ ---------------------------------------------------------------------------
: TEST-INNER-PAYLOAD ( -- )
   TOK @ FV7 @ IDA @ PAYLOAD-N drop ;

: TEST-OWNER-BEFORE-DECL ( -- )
   TWX-CAND-START
   OPEN TOK !
   OPEN ITOK !
   ITOK @ FV7 @ DECL ITOK !
   ITOK @ FV7 @ s" owner-inner" VARIANT ITOK !
   CURRENT-VARIANT IDA !
   ITOK @ FV7 @ s" inner-field" SCHROOT @ 0 1 0 CELL CELL 0 FIELD ITOK !
   ITOK @ FV7 @ END-VARIANT ITOK !
   ITOK @ PUBLISH
   TOK @ FV7 @ DECL TOK !
   TOK @ FV7 @ s" owner-outer" VARIANT TOK !
   CURRENT-VARIANT IDB !
   TOK @ FV7 @ s" outer-field" SCHROOT @ 1 1 CELL CELL CELL 0 FIELD TOK !
   TOK @ FV7 @ END-VARIANT TOK !
   TOK @ FV7 @ IDB @ PAYLOAD-N 1 T=
   [: TEST-INNER-PAYLOAD ;] catch TC !
   TC @ E-DEV-FIELD-SCOPE T=
   TOK @ ROLLBACK
   0 TWX-CAND-DONE drop ;

TEST-OWNER-BEFORE-DECL

\ ---------------------------------------------------------------------------
\ 13. Reversible publication closes the provisional payload view before the
\     coordinator finalizes the frame.
\ ---------------------------------------------------------------------------
E-PF-TX constant TEST-E-PF-TX

: TEST-PAYLOAD-QUERY ( -- )
   TOK @ FV7 @ VAR-E @ PAYLOAD-N drop ;

: TEST-PAYLOAD-WIDTH ( -- )
   TOK @ FV7 @ VAR-E @ 0 PAYLOAD-WIDTH@ drop ;

: TEST-FIELD-CELLS ( -- )
   DEV-TX-TOP DEVTX.FLDTOK @ FV7 @ P-PF @ DEV-FLD-TX-CELLS-FOR drop ;

: TEST-PUBLISHED-PAYLOAD-HIDDEN ( -- )
   OPEN TOK !
   TOK @ FV7 @ DECL TOK !
   TOK @ FV7 @ s" hidden" VARIANT TOK !
   CURRENT-VARIANT VAR-E !
   TOK @ FV7 @ s" hidden-field" SCHROOT @ 0 1 0 CELL CELL 0 FIELD TOK !
   DEV-N @ 1 - DEV-PROV-FLD@ P-PF !
   [: TEST-PAYLOAD-WIDTH ;] catch TC !
   TC @ E-DEV-FIELD-SCOPE T=
   TOK @ FV7 @ END-VARIANT TOK !
   TOK @ FV7 @ VAR-E @ 0 PAYLOAD-WIDTH@ 1 T=
   TOK @ DEV-PREPARE
   TOK @ DEV-COMMIT
   [: TEST-FIELD-CELLS ;] catch TC !
   TC @ TEST-E-PF-TX T=
   [: TEST-PAYLOAD-QUERY ;] catch TC !
   TC @ E-DEV-TX T=
   TOK @ DEV-FINALIZE ;

TEST-PUBLISHED-PAYLOAD-HIDDEN

\ ---------------------------------------------------------------------------
\ 14. A field-snapshot failure changes no participant state.  The only raw
\     test seam swaps the pre-hook field token serial and captures its complete
\     transaction state; it stays private to DECL-EVENT.
\ ---------------------------------------------------------------------------
$7FFFFFFFFFFFFFFF constant TEST-SERIAL-MAX

variable B-DEV-N      variable B-DEV-PUB
variable B-FLD-BASE   variable B-FLD-ORD
variable B-VAR-ORD    variable B-CUR-VAR
variable B-DEV-DEPTH  variable B-DEV-SERIAL
variable C-DEV-N      variable C-DEV-PUB
variable C-FLD-BASE   variable C-FLD-ORD
variable C-VAR-ORD    variable C-CUR-VAR
variable C-DEV-DEPTH  variable C-DEV-SERIAL

variable S-PF-N       variable S-PF-PUB
variable S-PF-DEPTH   variable S-PF-SERIAL
variable B-PF-N       variable B-PF-PUB
variable B-PF-DEPTH   variable B-PF-SERIAL
variable C-PF-N       variable C-PF-PUB
variable C-PF-DEPTH   variable C-PF-SERIAL

TRUSTED: TEST-PF-SWAP ( n -- )
   PF-N @ S-PF-N !
   PF-COMMIT-N @ S-PF-PUB !
   PF-TX-DEPTH @ S-PF-DEPTH !
   PF-TX-SERIAL @ S-PF-SERIAL !
   PF-TX-SERIAL ! ;

: SAVE-B-DEV ( -- )
   DEV-N @ B-DEV-N !             DEV-PUB-N @ B-DEV-PUB !
   DEV-BASE-FLD @ B-FLD-BASE !   DEV-FLD-ORD @ B-FLD-ORD !
   DEV-VAR-ORD @ B-VAR-ORD !     DEV-CUR-VAR @ B-CUR-VAR !
   DEV-TX-DEPTH @ B-DEV-DEPTH !  DEV-TX-SERIAL @ B-DEV-SERIAL ! ;

: SAVE-C-DEV ( -- )
   DEV-N @ C-DEV-N !             DEV-PUB-N @ C-DEV-PUB !
   DEV-BASE-FLD @ C-FLD-BASE !   DEV-FLD-ORD @ C-FLD-ORD !
   DEV-VAR-ORD @ C-VAR-ORD !     DEV-CUR-VAR @ C-CUR-VAR !
   DEV-TX-DEPTH @ C-DEV-DEPTH !  DEV-TX-SERIAL @ C-DEV-SERIAL ! ;

: RESTORE-B-DEV ( -- )
   B-DEV-N @ DEV-N !             B-DEV-PUB @ DEV-PUB-N !
   B-FLD-BASE @ DEV-BASE-FLD !   B-FLD-ORD @ DEV-FLD-ORD !
   B-VAR-ORD @ DEV-VAR-ORD !     B-CUR-VAR @ DEV-CUR-VAR !
   B-DEV-DEPTH @ DEV-TX-DEPTH !  B-DEV-SERIAL @ DEV-TX-SERIAL ! ;

: CHECK-B-DEV ( -- )
   DEV-N @ B-DEV-N @ T=             DEV-PUB-N @ B-DEV-PUB @ T=
   DEV-BASE-FLD @ B-FLD-BASE @ T=   DEV-FLD-ORD @ B-FLD-ORD @ T=
   DEV-VAR-ORD @ B-VAR-ORD @ T=     DEV-CUR-VAR @ B-CUR-VAR @ T=
   DEV-TX-DEPTH @ B-DEV-DEPTH @ T=  DEV-TX-SERIAL @ B-DEV-SERIAL @ T= ;

: CHECK-C-DEV ( -- )
   DEV-N @ C-DEV-N @ T=             DEV-PUB-N @ C-DEV-PUB @ T=
   DEV-BASE-FLD @ C-FLD-BASE @ T=   DEV-FLD-ORD @ C-FLD-ORD @ T=
   DEV-VAR-ORD @ C-VAR-ORD @ T=     DEV-CUR-VAR @ C-CUR-VAR @ T=
   DEV-TX-DEPTH @ C-DEV-DEPTH @ T=  DEV-TX-SERIAL @ C-DEV-SERIAL @ T= ;

: SAVE-B-PF ( -- )
   S-PF-N @ B-PF-N !          S-PF-PUB @ B-PF-PUB !
   S-PF-DEPTH @ B-PF-DEPTH !  S-PF-SERIAL @ B-PF-SERIAL ! ;

: SAVE-C-PF ( -- )
   S-PF-N @ C-PF-N !          S-PF-PUB @ C-PF-PUB !
   S-PF-DEPTH @ C-PF-DEPTH !  S-PF-SERIAL @ C-PF-SERIAL ! ;

: CHECK-B-PF ( -- )
   S-PF-N @ B-PF-N @ T=          S-PF-PUB @ B-PF-PUB @ T=
   S-PF-DEPTH @ B-PF-DEPTH @ T=  S-PF-SERIAL @ TEST-SERIAL-MAX T= ;

: CHECK-C-PF ( -- )
   S-PF-N @ C-PF-N @ T=          S-PF-PUB @ C-PF-PUB @ T=
   S-PF-DEPTH @ C-PF-DEPTH @ T=  S-PF-SERIAL @ C-PF-SERIAL @ T= ;

: SNAP-BODY ( -- ) ;
: SNAP-RUN ( -- ) [: SNAP-BODY ;] GENERATED-DECL:RUN ;
: SNAP-CATCH ( -- n ) [: SNAP-RUN ;] catch ;

: SEED-SNAPSHOT-BASE ( -- )
   RESET
   OPEN TOK !
   TOK @ FV7 @ DECL TOK !
   TOK @ FV7 @ s" snap-base" VARIANT TOK !
   TOK @ FV7 @ s" snap-field" SCHROOT @ 0 1 0 CELL CELL 0 FIELD TOK !
   TOK @ PUBLISH ;

: TEST-SNAPSHOT-FAILURE ( -- )
   SEED-SNAPSHOT-BASE
   SAVE-B-DEV
   0 TEST-PF-SWAP  SAVE-B-PF
   B-PF-SERIAL @ TEST-PF-SWAP

   SNAP-RUN
   SAVE-C-DEV
   0 TEST-PF-SWAP  SAVE-C-PF
   C-PF-SERIAL @ TEST-PF-SWAP

   RESTORE-B-DEV
   B-PF-SERIAL @ TEST-PF-SWAP
   TEST-SERIAL-MAX TEST-PF-SWAP
   SNAP-CATCH TC !
   TEST-SERIAL-MAX TEST-PF-SWAP

   TC @ TEST-E-PF-TX T=
   CHECK-B-DEV
   CHECK-B-PF

   B-PF-SERIAL @ TEST-PF-SWAP
   SNAP-RUN
   CHECK-C-DEV
   0 TEST-PF-SWAP
   CHECK-C-PF
   C-PF-SERIAL @ TEST-PF-SWAP ;

TEST-SNAPSHOT-FAILURE

\ ---------------------------------------------------------------------------
\ 15. The raw VARIANT event applies the sealed name policy before SUMV, ordinal,
\ event, field, or selector mutation. Rejection changes neither the live frame
\ nor any registry; rollback then restores the complete pre-transaction state.
\ A family owned only by another package remains a legal variant name.
\ ---------------------------------------------------------------------------
s" " CHECKER-PACKAGE-PUBLIC s" raw-global" 0 TK-ENUM TWX-TFAM-DECL drop
s" decl-event" CHECKER-PACKAGE-PUBLIC s" raw-local" 0 TK-ENUM TWX-TFAM-DECL drop
s" other-event-test" CHECKER-PACKAGE-PUBLIC s" raw-foreign" 0 TK-ENUM TWX-TFAM-DECL drop
VALUE-RECORD event-record payload n END-VALUE-RECORD

variable VN-A         variable VN-U
variable VN-TFAM      variable VN-STR       variable VN-PK
variable VN-SUMV      variable VN-LAY       variable VN-SCH
variable VN-ROOT      variable VN-PF        variable VN-PFP
variable VN-ON        variable VN-OPUB      variable VN-OBASE
variable VN-OFORD     variable VN-OVORD     variable VN-OCUR
variable VN-ODEPTH
variable VN-IN        variable VN-IPUB      variable VN-IBASE
variable VN-IFORD     variable VN-IVORD     variable VN-ICUR
variable VN-IDEPTH    variable VN-ISERIAL
variable VN-RK        variable VN-RF        variable VN-RV
variable VN-RFLD      variable VN-ROWN
variable VN-FEVN      variable VN-FFORD     variable VN-FVORD
variable VN-FCUR      variable VN-FFLDTOK   variable VN-FTOK
variable VN-FPUB      variable VN-FSTATE    variable VN-FFAM
variable VN-FOWNER

: VN-SAVE-REG ( -- )
   TFAM-N@ VN-TFAM !          TF-STR-U@ VN-STR !
   TF-PK-N@ VN-PK !           SUMV-N@ VN-SUMV !
   LAY-N@ VN-LAY !            SCHEMA-N@ VN-SCH !
   SCHEMA-ROOT-N@ VN-ROOT !   TYPE-FIELD:COUNT VN-PF !
   DEV-FLD-PROVISIONAL-COUNT VN-PFP ! ;

: VN-CHECK-REG ( -- )
   TFAM-N@ VN-TFAM @ T=          TF-STR-U@ VN-STR @ T=
   TF-PK-N@ VN-PK @ T=           SUMV-N@ VN-SUMV @ T=
   LAY-N@ VN-LAY @ T=            SCHEMA-N@ VN-SCH @ T=
   SCHEMA-ROOT-N@ VN-ROOT @ T=   TYPE-FIELD:COUNT VN-PF @ T=
   DEV-FLD-PROVISIONAL-COUNT VN-PFP @ T= ;

: VN-SAVE-OUT ( -- )
   DEV-N @ VN-ON !               DEV-PUB-N @ VN-OPUB !
   DEV-BASE-FLD @ VN-OBASE !     DEV-FLD-ORD @ VN-OFORD !
   DEV-VAR-ORD @ VN-OVORD !      DEV-CUR-VAR @ VN-OCUR !
   DEV-TX-DEPTH @ VN-ODEPTH ! ;

: VN-CHECK-OUT ( -- )
   DEV-N @ VN-ON @ T=               DEV-PUB-N @ VN-OPUB @ T=
   DEV-BASE-FLD @ VN-OBASE @ T=     DEV-FLD-ORD @ VN-OFORD @ T=
   DEV-VAR-ORD @ VN-OVORD @ T=      DEV-CUR-VAR @ VN-OCUR @ T=
   DEV-TX-DEPTH @ VN-ODEPTH @ T= ;

: VN-SAVE-IN ( -- )
   DEV-N @ VN-IN !               DEV-PUB-N @ VN-IPUB !
   DEV-BASE-FLD @ VN-IBASE !     DEV-FLD-ORD @ VN-IFORD !
   DEV-VAR-ORD @ VN-IVORD !      DEV-CUR-VAR @ VN-ICUR !
   DEV-TX-DEPTH @ VN-IDEPTH !    DEV-TX-SERIAL @ VN-ISERIAL !
   DEV-CUR-START {: row:n :}
   row DEV-PROV-KIND@ VN-RK !    row DEV-PROV-FAM@ VN-RF !
   row DEV-PROV-VAR@ VN-RV !     row DEV-PROV-FLD@ VN-RFLD !
   row DEV-PROV-OWNER@ VN-ROWN !
   DEV-TX-TOP {: frame:ptr :}
   frame DEVTX.EVN @ VN-FEVN !       frame DEVTX.FLDORD @ VN-FFORD !
   frame DEVTX.VARORD @ VN-FVORD !   frame DEVTX.CURVAR @ VN-FCUR !
   frame DEVTX.FLDTOK @ VN-FFLDTOK ! frame DEVTX.TOK @ VN-FTOK !
   frame DEVTX.PUBN @ VN-FPUB !      frame DEVTX.STATE @ VN-FSTATE !
   frame DEVTX.FAM @ VN-FFAM !       frame DEVTX.OWNER @ VN-FOWNER ! ;

: VN-CHECK-IN ( -- )
   DEV-N @ VN-IN @ T=               DEV-PUB-N @ VN-IPUB @ T=
   DEV-BASE-FLD @ VN-IBASE @ T=     DEV-FLD-ORD @ VN-IFORD @ T=
   DEV-VAR-ORD @ VN-IVORD @ T=      DEV-CUR-VAR @ VN-ICUR @ T=
   DEV-TX-DEPTH @ VN-IDEPTH @ T=    DEV-TX-SERIAL @ VN-ISERIAL @ T=
   DEV-CUR-START {: row:n :}
   row DEV-PROV-KIND@ VN-RK @ T=    row DEV-PROV-FAM@ VN-RF @ T=
   row DEV-PROV-VAR@ VN-RV @ T=     row DEV-PROV-FLD@ VN-RFLD @ T=
   row DEV-PROV-OWNER@ VN-ROWN @ T=
   DEV-TX-TOP {: frame:ptr :}
   frame DEVTX.EVN @ VN-FEVN @ T=
   frame DEVTX.FLDORD @ VN-FFORD @ T=
   frame DEVTX.VARORD @ VN-FVORD @ T=
   frame DEVTX.CURVAR @ VN-FCUR @ T=
   frame DEVTX.FLDTOK @ VN-FFLDTOK @ T=
   frame DEVTX.TOK @ VN-FTOK @ T=
   frame DEVTX.PUBN @ VN-FPUB @ T=
   frame DEVTX.STATE @ VN-FSTATE @ T=
   frame DEVTX.FAM @ VN-FFAM @ T=
   frame DEVTX.OWNER @ VN-FOWNER @ T= ;

: VN-CALL ( -- )
   TOK @ FV7 @ VN-A @ VN-U @ VARIANT drop ;

: VN-REJECT ( ptr u8 n n -- ) {: a:ptr u:n want:n :}
   a VN-A !  u VN-U !
   VN-SAVE-REG  VN-SAVE-OUT
   TWX-CAND-START
   OPEN TOK !
   TOK @ FV7 @ DECL TOK !
   VN-SAVE-IN
   [: VN-CALL ;] catch TC !
   TC @ want T=
   VN-CHECK-IN  VN-CHECK-REG
   TOK @ ROLLBACK
   0 TWX-CAND-DONE drop
   VN-CHECK-OUT  VN-CHECK-REG ;

: VN-ACCEPT ( ptr u8 n -- ) {: a:ptr u:n :}
   a VN-A !  u VN-U !
   VN-SAVE-REG  VN-SAVE-OUT
   TWX-CAND-START
   OPEN TOK !
   TOK @ FV7 @ DECL TOK !
   [: VN-CALL ;] catch TC !
   TC @ 0 T=
   SUMV-N@ VN-SUMV @ 1 + T=
   DEV-VAR-ORD @ 1 T=
   DEV-CUR-VAR @ DECL-EVENT:NO-VARIANT <> T-TRUE
   TOK @ ROLLBACK
   0 TWX-CAND-DONE drop
   VN-CHECK-OUT  VN-CHECK-REG ;

s" " 7107 VN-REJECT
s" n" 7110 VN-REJECT
s" q" 7110 VN-REJECT
s" if" 7110 VN-REJECT
s" variant" 7110 VN-REJECT
s" bool" 7110 VN-REJECT
s" event-record" 7110 VN-REJECT
s" space-x" 7110 VN-REJECT
s" raw-global" 7110 VN-REJECT
s" raw-local" 7110 VN-REJECT
s" raw-foreign" VN-ACCEPT
s" ready" VN-ACCEPT

\ ---------------------------------------------------------------------------
\ 16. Every family-scoped mutator rejects a foreign family and an unbound
\     sentinel before changing the live event, field, registry, or frame state.
\ ---------------------------------------------------------------------------
variable WF-N       variable WF-PUB
variable WF-BASE    variable WF-FORD
variable WF-VORD    variable WF-CUR
variable WF-DEPTH   variable WF-SERIAL
variable WF-PFN     variable WF-PF-DEPTH
variable WF-TOK     variable WF-FLDTOK
variable WF-FAM     variable WF-OWNER
variable WF-FEVN    variable WF-FFORD
variable WF-FVORD   variable WF-FCUR
variable WF-FPUB    variable WF-FSTATE
variable WF-K0      variable WF-F0      variable WF-V0
variable WF-L0      variable WF-O0
variable WF-K1      variable WF-F1      variable WF-V1
variable WF-L1      variable WF-O1
variable WF-CALL-FAM
variable WF-PK      variable WF-LAY

: WF-REG-SAVE ( -- )
   TFAM-N@ P-TFAM !       SUMV-N@ P-SUMV !
   SCHEMA-N@ P-SCHN !     SCHEMA-ROOT-N@ P-SCHR !
   TF-STR-U@ P-STRU !      TF-PK-N@ WF-PK !
   LAY-N@ WF-LAY !         TYPE-FIELD:COUNT P-PF ! ;

: WF-REG-SAME ( -- )
   TFAM-N@ P-TFAM @ T=       SUMV-N@ P-SUMV @ T=
   SCHEMA-N@ P-SCHN @ T=     SCHEMA-ROOT-N@ P-SCHR @ T=
   TF-STR-U@ P-STRU @ T=      TF-PK-N@ WF-PK @ T=
   LAY-N@ WF-LAY @ T=         TYPE-FIELD:COUNT P-PF @ T= ;

: WF-EVENT-SAVE ( -- )
   DEV-N @ 0 > IF
      0 DEV-PROV-KIND@ WF-K0 !
      0 DEV-PROV-FAM@ WF-F0 !
      0 DEV-PROV-VAR@ WF-V0 !
      0 DEV-PROV-FLD@ WF-L0 !
      0 DEV-PROV-OWNER@ WF-O0 !
   THEN
   DEV-N @ 1 > IF
      1 DEV-PROV-KIND@ WF-K1 !
      1 DEV-PROV-FAM@ WF-F1 !
      1 DEV-PROV-VAR@ WF-V1 !
      1 DEV-PROV-FLD@ WF-L1 !
      1 DEV-PROV-OWNER@ WF-O1 !
   THEN ;

: WF-EVENT-SAME ( -- )
   DEV-N @ 0 > IF
      0 DEV-PROV-KIND@ WF-K0 @ T=
      0 DEV-PROV-FAM@ WF-F0 @ T=
      0 DEV-PROV-VAR@ WF-V0 @ T=
      0 DEV-PROV-FLD@ WF-L0 @ T=
      0 DEV-PROV-OWNER@ WF-O0 @ T=
   THEN
   DEV-N @ 1 > IF
      1 DEV-PROV-KIND@ WF-K1 @ T=
      1 DEV-PROV-FAM@ WF-F1 @ T=
      1 DEV-PROV-VAR@ WF-V1 @ T=
      1 DEV-PROV-FLD@ WF-L1 @ T=
      1 DEV-PROV-OWNER@ WF-O1 @ T=
   THEN ;

: WF-STATE-SAVE ( -- )
   DEV-N @ WF-N !             DEV-PUB-N @ WF-PUB !
   DEV-BASE-FLD @ WF-BASE !   DEV-FLD-ORD @ WF-FORD !
   DEV-VAR-ORD @ WF-VORD !    DEV-CUR-VAR @ WF-CUR !
   DEV-TX-DEPTH @ WF-DEPTH !  DEV-TX-SERIAL @ WF-SERIAL !
   DEV-FLD-PROVISIONAL-COUNT WF-PFN !
   TYPE-FIELD:TX-DEPTH WF-PF-DEPTH !
   DEV-TX-TOP {: frame:ptr :}
   frame DEVTX.EVN @ WF-FEVN !       frame DEVTX.FLDORD @ WF-FFORD !
   frame DEVTX.VARORD @ WF-FVORD !   frame DEVTX.CURVAR @ WF-FCUR !
   frame DEVTX.FLDTOK @ WF-FLDTOK !  frame DEVTX.TOK @ WF-TOK !
   frame DEVTX.PUBN @ WF-FPUB !      frame DEVTX.STATE @ WF-FSTATE !
   frame DEVTX.FAM @ WF-FAM !        frame DEVTX.OWNER @ WF-OWNER !
   WF-EVENT-SAVE ;

: WF-STATE-SAME ( -- )
   DEV-N @ WF-N @ T=             DEV-PUB-N @ WF-PUB @ T=
   DEV-BASE-FLD @ WF-BASE @ T=   DEV-FLD-ORD @ WF-FORD @ T=
   DEV-VAR-ORD @ WF-VORD @ T=    DEV-CUR-VAR @ WF-CUR @ T=
   DEV-TX-DEPTH @ WF-DEPTH @ T=  DEV-TX-SERIAL @ WF-SERIAL @ T=
   DEV-FLD-PROVISIONAL-COUNT WF-PFN @ T=
   TYPE-FIELD:TX-DEPTH WF-PF-DEPTH @ T=
   DEV-TX-TOP {: frame:ptr :}
   frame DEVTX.EVN @ WF-FEVN @ T=
   frame DEVTX.FLDORD @ WF-FFORD @ T=
   frame DEVTX.VARORD @ WF-FVORD @ T=
   frame DEVTX.CURVAR @ WF-FCUR @ T=
   frame DEVTX.FLDTOK @ WF-FLDTOK @ T=
   frame DEVTX.TOK @ WF-TOK @ T=
   frame DEVTX.PUBN @ WF-FPUB @ T=
   frame DEVTX.STATE @ WF-FSTATE @ T=
   frame DEVTX.FAM @ WF-FAM @ T=
   frame DEVTX.OWNER @ WF-OWNER @ T=
   WF-EVENT-SAME ;

: WF-START ( -- )
   RESET
   TWX-CAND-START
   OPEN TOK !
   TOK @ FV7 @ DECL TOK ! ;

: WF-START-UNBOUND ( -- )
   RESET
   TWX-CAND-START
   OPEN TOK ! ;

: WF-FINISH ( -- )
   TOK @ ROLLBACK
   0 TWX-CAND-DONE drop
   DEPTH 0 T=
   CURRENT-VARIANT NO-VARIANT T= ;

: WF-RUN-WANT ( [ -- ] n -- ) {: op want:n :} \ typed-local-lint: allow-bare-local
   WF-REG-SAVE
   WF-STATE-SAVE
   op catch TC !
   TC @ want T=
   WF-STATE-SAME
   WF-REG-SAME ;

: WF-RUN ( [ -- ] -- ) {: op :} \ typed-local-lint: allow-bare-local
   op E-DEV-FAMILY-SCOPE WF-RUN-WANT ;

: WF-BAD-ARITY ( -- ) TOK @ WF-CALL-FAM @ 1 ARITY drop ;
: WF-BAD-POLICY ( -- ) TOK @ WF-CALL-FAM @ 0 POLICY drop ;
: WF-BAD-DERIVE ( -- ) TOK @ WF-CALL-FAM @ 9 DERIVE drop ;
: WF-BAD-VARIANT ( -- ) TOK @ WF-CALL-FAM @ s" wf-variant" VARIANT drop ;
: WF-BAD-END ( -- ) TOK @ WF-CALL-FAM @ END-VARIANT drop ;
: WF-BAD-DECL ( -- ) TOK @ WF-CALL-FAM @ DECL drop ;
: WF-BAD-FIELD ( -- )
   TOK @ WF-CALL-FAM @ s" wf-field" SCHROOT @
   0 1 0 CELL CELL 0 FIELD drop ;

FE2 @ WF-CALL-FAM !
WF-START  ' WF-BAD-ARITY WF-RUN  WF-FINISH
WF-START  ' WF-BAD-POLICY WF-RUN  WF-FINISH
WF-START  ' WF-BAD-DERIVE WF-RUN  WF-FINISH
WF-START  ' WF-BAD-VARIANT WF-RUN  WF-FINISH
WF-START
TOK @ FV7 @ s" wf-open" VARIANT TOK !
' WF-BAD-END WF-RUN
WF-FINISH
WF-START  ' WF-BAD-DECL E-DEV-STATE WF-RUN-WANT  WF-FINISH
WF-START  ' WF-BAD-FIELD WF-RUN  WF-FINISH

DEV-NO-FAMILY WF-CALL-FAM !
WF-START-UNBOUND  ' WF-BAD-ARITY WF-RUN  WF-FINISH
WF-START-UNBOUND  ' WF-BAD-POLICY WF-RUN  WF-FINISH
WF-START-UNBOUND  ' WF-BAD-DERIVE WF-RUN  WF-FINISH
WF-START-UNBOUND  ' WF-BAD-VARIANT WF-RUN  WF-FINISH
WF-START-UNBOUND  ' WF-BAD-END WF-RUN  WF-FINISH
WF-START-UNBOUND  ' WF-BAD-DECL WF-RUN  WF-FINISH
WF-START-UNBOUND  ' WF-BAD-FIELD WF-RUN  WF-FINISH

\ All provisional payload readers use the same family authority. An unbound
\ frame and the sentinel are not a declaration-family capability.
: WF-BAD-PAYLOAD-N ( -- )
   TOK @ DEV-NO-FAMILY DEV-NO-VARIANT PAYLOAD-N drop ;

: WF-BAD-PAYLOAD-SCHEMA ( -- )
   TOK @ DEV-NO-FAMILY DEV-NO-VARIANT 0 PAYLOAD-SCHEMA@ drop ;

: WF-BAD-PAYLOAD-WIDTH ( -- )
   TOK @ DEV-NO-FAMILY DEV-NO-VARIANT 0 PAYLOAD-WIDTH@ drop ;

: WF-BAD-PAYLOAD-CELLS ( -- )
   TOK @ DEV-NO-FAMILY DEV-NO-VARIANT PAYLOAD-CELLS drop ;

WF-START-UNBOUND  ' WF-BAD-PAYLOAD-N WF-RUN       WF-FINISH
WF-START-UNBOUND  ' WF-BAD-PAYLOAD-SCHEMA WF-RUN  WF-FINISH
WF-START-UNBOUND  ' WF-BAD-PAYLOAD-WIDTH WF-RUN   WF-FINISH
WF-START-UNBOUND  ' WF-BAD-PAYLOAD-CELLS WF-RUN   WF-FINISH

\ ---------------------------------------------------------------------------
\ 17. Every provisional payload query rejects reordered fields, a foreign event
\     family, or a different valid field id before reading that field row.
\ ---------------------------------------------------------------------------
create HF-ROW-SAVE DEV-REC allot

variable HF-SCH
variable HF-EVA   variable HF-EVB
variable HF-FLDA  variable HF-FLDB
variable HF-SA    variable HF-SB
variable HF-CA    variable HF-CB

: HF-SAVE ( -- )
   WF-REG-SAVE
   WF-STATE-SAVE
   DEV-TX-TOP DEVTX.FLDTOK @ FV7 @ HF-FLDA @
      DEV-FLD-TX-SCHEMA-FOR HF-SA !
   DEV-TX-TOP DEVTX.FLDTOK @ FV7 @ HF-FLDB @
      DEV-FLD-TX-SCHEMA-FOR HF-SB !
   DEV-TX-TOP DEVTX.FLDTOK @ FV7 @ HF-FLDA @
      DEV-FLD-TX-CELLS-FOR HF-CA !
   DEV-TX-TOP DEVTX.FLDTOK @ FV7 @ HF-FLDB @
      DEV-FLD-TX-CELLS-FOR HF-CB ! ;

: HF-SAME ( -- )
   WF-STATE-SAME
   WF-REG-SAME
   DEV-TX-TOP DEVTX.FLDTOK @ FV7 @ HF-FLDA @
      DEV-FLD-TX-SCHEMA-FOR HF-SA @ T=
   DEV-TX-TOP DEVTX.FLDTOK @ FV7 @ HF-FLDB @
      DEV-FLD-TX-SCHEMA-FOR HF-SB @ T=
   DEV-TX-TOP DEVTX.FLDTOK @ FV7 @ HF-FLDA @
      DEV-FLD-TX-CELLS-FOR HF-CA @ T=
   DEV-TX-TOP DEVTX.FLDTOK @ FV7 @ HF-FLDB @
      DEV-FLD-TX-CELLS-FOR HF-CB @ T=
   TOK @ FV7 @ VAR-E @ PAYLOAD-N 2 T=
   TOK @ FV7 @ VAR-E @ 0 PAYLOAD-SCHEMA@ SCHROOT @ T=
   TOK @ FV7 @ VAR-E @ 1 PAYLOAD-SCHEMA@ HF-SCH @ T=
   TOK @ FV7 @ VAR-E @ 0 PAYLOAD-WIDTH@ 1 T=
   TOK @ FV7 @ VAR-E @ 1 PAYLOAD-WIDTH@ 1 T=
   TOK @ FV7 @ VAR-E @ PAYLOAD-CELLS 2 T= ;

: HF-OPEN ( -- )
   RESET
   TWX-CAND-START
   OPEN TOK !
   TOK @ FV7 @ DECL TOK !
   TOK @ FV7 @ s" hf-variant" VARIANT TOK !
   CURRENT-VARIANT VAR-E !
   TOK @ FV7 @ s" hf-first" SCHROOT @
      0 1 0 CELL CELL 0 FIELD TOK !
   1 TWX-SCHEMA-PARAM TWX-SCHEMA-ROOT+ HF-SCH !
   TOK @ FV7 @ s" hf-second" HF-SCH @
      1 1 CELL CELL CELL 0 FIELD TOK !
   TOK @ FV7 @ END-VARIANT TOK !
   DEV-N @ 3 - dup HF-EVA !
   dup DEV-PROV-FLD@ HF-FLDA !
   1 + dup HF-EVB !
   DEV-PROV-FLD@ HF-FLDB !
   TOK @ FV7 @ VAR-E @ PAYLOAD-N 2 T=
   TOK @ FV7 @ VAR-E @ 0 PAYLOAD-WIDTH@ 1 T=
   TOK @ FV7 @ VAR-E @ 1 PAYLOAD-WIDTH@ 1 T=
   TOK @ FV7 @ VAR-E @ PAYLOAD-CELLS 2 T=
   HF-SAVE ;

: HF-CLOSE ( -- )
   TOK @ ROLLBACK
   0 TWX-CAND-DONE drop
   DEV-N @ 0 T=             DEV-PUB-N @ 0 T=
   DEV-FLD-ORD @ 0 T=       DEV-VAR-ORD @ 0 T=
   DEV-CUR-VAR @ DEV-NO-VARIANT T=
   DEV-TX-DEPTH @ 0 T=
   DEV-FLD-PROVISIONAL-COUNT DEV-FLD-COUNT T= ;

: HF-PAYLOAD-N ( -- )
   TOK @ FV7 @ VAR-E @ PAYLOAD-N drop ;

: HF-PAYLOAD-SCHEMA ( -- )
   TOK @ FV7 @ VAR-E @ 0 PAYLOAD-SCHEMA@ drop ;

: HF-PAYLOAD-WIDTH ( -- )
   TOK @ FV7 @ VAR-E @ 0 PAYLOAD-WIDTH@ drop ;

: HF-PAYLOAD-CELLS ( -- )
   TOK @ FV7 @ VAR-E @ PAYLOAD-CELLS drop ;

: HF-REJECT-QUERIES ( -- )
   [: HF-PAYLOAD-N ;] catch TC !       TC @ E-DEV-FIELD-SCOPE T=
   [: HF-PAYLOAD-SCHEMA ;] catch TC !  TC @ E-DEV-FIELD-SCOPE T=
   [: HF-PAYLOAD-WIDTH ;] catch TC !   TC @ E-DEV-FIELD-SCOPE T=
   [: HF-PAYLOAD-CELLS ;] catch TC !   TC @ E-DEV-FIELD-SCOPE T= ;

: HF-SWAP-EVENTS ( -- )
   HF-EVA @ DEV-ROW BYTE-VIEW HF-ROW-SAVE BYTE-VIEW DEV-REC BYTE-COPY
   HF-EVB @ DEV-ROW BYTE-VIEW HF-EVA @ DEV-ROW BYTE-VIEW DEV-REC BYTE-COPY
   HF-ROW-SAVE BYTE-VIEW HF-EVB @ DEV-ROW BYTE-VIEW DEV-REC BYTE-COPY ;

: HF-ORDER ( -- )
   HF-OPEN
   HF-SWAP-EVENTS
   HF-REJECT-QUERIES
   HF-SWAP-EVENTS
   HF-SAME
   HF-CLOSE ;

: HF-FAMILY ( -- )
   HF-OPEN
   FE2 @ HF-EVA @ DEV-ROW DEV.FAM !
   HF-REJECT-QUERIES
   FV7 @ HF-EVA @ DEV-ROW DEV.FAM !
   HF-SAME
   HF-CLOSE ;

: HF-FIELD-ID ( -- )
   HF-OPEN
   HF-FLDB @ HF-EVA @ DEV-ROW DEV.FLD !
   HF-REJECT-QUERIES
   HF-FLDA @ HF-EVA @ DEV-ROW DEV.FLD !
   HF-SAME
   HF-CLOSE ;

HF-ORDER
HF-FAMILY
HF-FIELD-ID

\ ---------------------------------------------------------------------------
\ 18. The field owner's public provisional-cell reader validates the exact live
\     transaction, family, and row range before reading PF.CELLS.  A different
\     valid family row in the same transaction is not authority for this family.
\ ---------------------------------------------------------------------------
variable TXC-TOK
variable TXC-OWN
variable TXC-FOREIGN
variable TXC-END

: TXC-OPEN ( -- )
   HF-OPEN
   DEV-TX-TOP DEVTX.FLDTOK @ TXC-TOK !
   HF-FLDA @ TXC-OWN !
   DEV-FLD-PROVISIONAL-COUNT TXC-FOREIGN !
   TXC-TOK @ FD3 @ DEV-NO-VARIANT
      s" txc-foreign" SCHROOT @ 0 1 0 CELL CELL 0 DEV-FLD-ADD drop
   DEV-FLD-PROVISIONAL-COUNT TXC-END !
   HF-SAVE ;

: TXC-WRONG-TOKEN ( -- )
   TXC-TOK @ 1 + FV7 @ TXC-OWN @
   DEV-FLD-TX-CELLS-FOR drop ;

: TXC-WRONG-FAMILY ( -- )
   TXC-TOK @ FD3 @ TXC-OWN @
   DEV-FLD-TX-CELLS-FOR drop ;

: TXC-FOREIGN-ROW ( -- )
   TXC-TOK @ FV7 @ TXC-FOREIGN @
   DEV-FLD-TX-CELLS-FOR drop ;

: TXC-NEGATIVE ( -- )
   TXC-TOK @ FV7 @ -1
   DEV-FLD-TX-CELLS-FOR drop ;

: TXC-ONE-PAST ( -- )
   TXC-TOK @ FV7 @ TXC-END @
   DEV-FLD-TX-CELLS-FOR drop ;

TXC-OPEN

TXC-TOK @ FV7 @ TXC-OWN @ TYPE-FIELD-OWNER:TX-CELLS-FOR 1 T=

' TXC-WRONG-TOKEN catch TC !
TC @ E-PF-TX T=

' TXC-WRONG-FAMILY catch TC !
TC @ E-PF-OWNER T=

' TXC-FOREIGN-ROW catch TC !
TC @ E-PF-OWNER T=

' TXC-NEGATIVE catch TC !
TC @ E-PF-ID T=

' TXC-ONE-PAST catch TC !
TC @ E-PF-ID T=

HF-SAME
HF-CLOSE

\ ---------------------------------------------------------------------------
\ 19. DECL is single-assignment per event frame. Both same-family and
\     wrong-family rebinding reject before any event, family, field, or
\     rollback state changes.
\ ---------------------------------------------------------------------------
variable DG-FAM
variable DG-N
variable DG-PUB
variable DG-ID
variable DG-TFAM
variable DG-STRU
variable DG-PK
variable DG-SUMV
variable DG-LAY
variable DG-SCH
variable DG-ROOT
variable DG-KIND
variable DG-FLD
variable DG-FLD-PROV
variable DG-FLD-DEPTH
variable DG-CHK-DEPTH
variable DG-DEV-DEPTH
variable DG-SERIAL
variable DG-FEVN
variable DG-FFLDORD
variable DG-FVARORD
variable DG-FCURVAR
variable DG-FFLDTOK
variable DG-FTOK
variable DG-FPUBN
variable DG-FSTATE
variable DG-FFAM
variable DG-FOWNER
variable DG-OLD-FAM
variable DG-NEW-FAM
variable DG-OTHER-FAM

: DG-FRAME-SAVE ( -- )
   DEV-TX-TOP {: r:ptr :}
   r DEVTX.EVN @ DG-FEVN !
   r DEVTX.FLDORD @ DG-FFLDORD !
   r DEVTX.VARORD @ DG-FVARORD !
   r DEVTX.CURVAR @ DG-FCURVAR !
   r DEVTX.FLDTOK @ DG-FFLDTOK !
   r DEVTX.TOK @ DG-FTOK !
   r DEVTX.PUBN @ DG-FPUBN !
   r DEVTX.STATE @ DG-FSTATE !
   r DEVTX.FAM @ DG-FFAM !
   r DEVTX.OWNER @ DG-FOWNER ! ;

: DG-FRAME-SAME ( -- )
   DEV-TX-TOP {: r:ptr :}
   r DEVTX.EVN @ DG-FEVN @ T=
   r DEVTX.FLDORD @ DG-FFLDORD @ T=
   r DEVTX.VARORD @ DG-FVARORD @ T=
   r DEVTX.CURVAR @ DG-FCURVAR @ T=
   r DEVTX.FLDTOK @ DG-FFLDTOK @ T=
   r DEVTX.TOK @ DG-FTOK @ T=
   r DEVTX.PUBN @ DG-FPUBN @ T=
   r DEVTX.STATE @ DG-FSTATE @ T=
   r DEVTX.FAM @ DG-FFAM @ T=
   r DEVTX.OWNER @ DG-FOWNER @ T= ;

: DG-SAVE ( n -- ) {: fam:n :}
   fam DG-FAM !
   DEV-N @ DG-N !
   DEV-PUB-N @ DG-PUB !
   IDENTITY DG-ID !
   TFAM-N@ DG-TFAM !
   TF-STR-U@ DG-STRU !
   TF-PK-N@ DG-PK !
   SUMV-N@ DG-SUMV !
   LAY-N@ DG-LAY !
   SCHEMA-N@ DG-SCH !
   SCHEMA-ROOT-N@ DG-ROOT !
   fam TFAM-KIND@ DG-KIND !
   TYPE-FIELD:COUNT DG-FLD !
   DEV-FLD-PROVISIONAL-COUNT DG-FLD-PROV !
   TYPE-FIELD:TX-DEPTH DG-FLD-DEPTH !
   CHECKER-SCOPE-DEPTH DG-CHK-DEPTH !
   DEV-TX-DEPTH @ DG-DEV-DEPTH !
   DEV-TX-SERIAL @ DG-SERIAL !
   DEV-TX-DEPTH @ 0 > IF DG-FRAME-SAVE THEN ;

: DG-SAME ( -- )
   DEV-N @ DG-N @ T=
   DEV-PUB-N @ DG-PUB @ T=
   IDENTITY DG-ID @ T=
   TFAM-N@ DG-TFAM @ T=
   TF-STR-U@ DG-STRU @ T=
   TF-PK-N@ DG-PK @ T=
   SUMV-N@ DG-SUMV @ T=
   LAY-N@ DG-LAY @ T=
   SCHEMA-N@ DG-SCH @ T=
   SCHEMA-ROOT-N@ DG-ROOT @ T=
   DG-FAM @ TFAM-KIND@ DG-KIND @ T=
   TYPE-FIELD:COUNT DG-FLD @ T=
   DEV-FLD-PROVISIONAL-COUNT DG-FLD-PROV @ T=
   TYPE-FIELD:TX-DEPTH DG-FLD-DEPTH @ T=
   CHECKER-SCOPE-DEPTH DG-CHK-DEPTH @ T=
   DEV-TX-DEPTH @ DG-DEV-DEPTH @ T=
   DEV-TX-SERIAL @ DG-SERIAL @ T=
   DEV-TX-DEPTH @ 0 > IF DG-FRAME-SAME THEN ;

: DG-DECL-FP1 ( -- )
   TOK @ FP1 @ DECL drop ;

: DG-DECL-FE2 ( -- )
   TOK @ FE2 @ DECL drop ;

: DG-RESET ( -- )
   RESET ;

RESET
OPEN TOK !
TOK @ FP1 @ DECL TOK !
FP1 @ DG-SAVE
' DG-DECL-FP1 catch TC !
TC @ E-DEV-STATE T=
DG-SAME
' DG-DECL-FE2 catch TC !
TC @ E-DEV-STATE T=
DG-SAME
TOK @ ROLLBACK

\ ---------------------------------------------------------------------------
\ 20. RESET is fail-closed before every store while either an event frame or
\     a checker rollback frame is live, and succeeds after both close.
\ ---------------------------------------------------------------------------
RESET
OPEN TOK !
TOK @ FP1 @ DECL TOK !
FP1 @ DG-SAVE
' DG-RESET catch TC !
TC @ E-DEV-TX T=
DG-SAME
TOK @ ROLLBACK

TWX-CAND-START
FP1 @ DG-SAVE
' DG-RESET catch TC !
TC @ E-DEV-TX T=
DG-SAME
0 TWX-CAND-DONE drop
' DG-RESET catch TC !
TC @ 0 T=
DEPTH 0 T=
CHECKER-SCOPE-DEPTH 0 T=

\ ---------------------------------------------------------------------------
\ 21. A family created after the live checker savepoint may publish only one
\     declaration. A fresh event frame cannot bind it again under
\     that savepoint, and rejection preserves every owner watermark.
\ ---------------------------------------------------------------------------
RESET
TWX-CAND-START
s" dg" CHECKER-PACKAGE-PUBLIC s" fresh" 0 TK-ENUM
   TWX-TFAM-DECL DG-NEW-FAM !
OPEN TOK !
TOK @ DG-NEW-FAM @ DECL TOK !
TOK @ PUBLISH
OPEN TOK !
DG-NEW-FAM @ DG-SAVE
: DG-DECL-NEW ( -- )
   TOK @ DG-NEW-FAM @ DECL drop ;

: DG-DECL-OTHER ( -- )
   TOK @ DG-OTHER-FAM @ DECL drop ;

' DG-DECL-NEW catch TC !
TC @ E-DEV-FAMILY-SCOPE T=
DG-SAME
TOK @ ROLLBACK

s" dg" CHECKER-PACKAGE-PUBLIC s" other" 0 TK-ENUM
   TWX-TFAM-DECL DG-OTHER-FAM !
OPEN TOK !
' DG-DECL-OTHER catch TC !
TC @ 0 T=
TOK @ ROLLBACK

0 TWX-CAND-DONE drop
' DG-RESET catch TC !
TC @ 0 T=

\ ---------------------------------------------------------------------------
\ 22. A family older than the current checker savepoint is not provisional.
\     Its earlier published declaration therefore does not block a fresh
\     frame that is rolled back before publication.
\ ---------------------------------------------------------------------------
RESET
OPEN TOK !
TOK @ FP1 @ DECL TOK !
TOK @ PUBLISH
TWX-CAND-START
OPEN TOK !
' DG-DECL-FP1 catch TC !
TC @ 0 T=
TOK @ ROLLBACK
0 TWX-CAND-DONE drop
RESET

\ ---------------------------------------------------------------------------
\ 23. Rolling back the first event frame before publication leaves no stale
\     declaration claim. After checker rollback reuses the family id, the new
\     savepoint may bind it normally.
\ ---------------------------------------------------------------------------
TWX-CAND-START
s" dg" CHECKER-PACKAGE-PUBLIC s" reused" 0 TK-ENUM
   TWX-TFAM-DECL DG-OLD-FAM !
OPEN TOK !
TOK @ DG-OLD-FAM @ DECL TOK !
TOK @ ROLLBACK
0 TWX-CAND-DONE drop

TWX-CAND-START
s" dg" CHECKER-PACKAGE-PUBLIC s" reused" 0 TK-ENUM
   TWX-TFAM-DECL DG-NEW-FAM !
DG-NEW-FAM @ DG-OLD-FAM @ T=
OPEN TOK !
' DG-DECL-NEW catch TC !
TC @ 0 T=
TOK @ ROLLBACK
0 TWX-CAND-DONE drop
RESET

\ ---------------------------------------------------------------------------
\ 24. The owner queries were public while DECL-EVENT captured them, then the
\     generated-declaration protection owner retired both names before user
\     source. The compiled bridges remain the sole live capabilities.
\ ---------------------------------------------------------------------------
: DG-QUALIFIED-ABSENT? ( ptr u8 n -- bool )
   XREF-FIND XREF-FOUND? 0= ;

s" TYPE-FAMILY-OWNER:PROVISIONAL?" DG-QUALIFIED-ABSENT? T-TRUE
s" TYPE-FAMILY-OWNER:RESET-ALLOWED?" DG-QUALIFIED-ABSENT? T-TRUE
s" DG-CAP-P ( n -- bool ) TYPE-FAMILY-OWNER:PROVISIONAL?"
   CHECK-CANDIDATE! 1 T=
s" DG-CAP-R ( -- bool ) TYPE-FAMILY-OWNER:RESET-ALLOWED?"
   CHECK-CANDIDATE! 1 T=

;package

\ ---------------------------------------------------------------------------
: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" decl-event-suite: failures" 1 die ;
REPORT
