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

using SCHEMA-REG

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
   s" untracked" SCHROOT @ 0 1 0 CELL CELL 0 TYPE-FIELD-OWNER:ADD drop ;

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
   DEV-TX-TOP DEVTX.FLDTOK @ FV7 @ P-PF @ TYPE-FIELD-OWNER:TX-CELLS-FOR drop ;

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
\ 14. A committed field-owner frame makes a nested generated declaration fail
\     through the public lifecycle.  The rejected declaration changes neither
\     participant's public state, and rollback retires the owner token.
\ ---------------------------------------------------------------------------
variable HOLD-TOK     variable HOLD-NEXT
variable HOLD-EVN     variable HOLD-ID
variable HOLD-EDEPTH  variable HOLD-FLD
variable HOLD-FDEPTH

: SNAP-BODY ( -- ) ;
: SNAP-RUN ( -- ) [: SNAP-BODY ;] GENERATED-DECL:RUN ;
: SNAP-CATCH ( -- n ) [: SNAP-RUN ;] catch ;

: HOLD-SAVE ( -- )
   COUNT HOLD-EVN !
   IDENTITY HOLD-ID !
   DEPTH HOLD-EDEPTH !
   TYPE-FIELD:COUNT HOLD-FLD !
   TYPE-FIELD:TX-DEPTH HOLD-FDEPTH ! ;

: HOLD-SAME ( -- )
   COUNT HOLD-EVN @ T=
   IDENTITY HOLD-ID @ T=
   DEPTH HOLD-EDEPTH @ T=
   TYPE-FIELD:COUNT HOLD-FLD @ T=
   TYPE-FIELD:TX-DEPTH HOLD-FDEPTH @ T= ;

: HOLD-OWNER ( -- )
   OPEN HOLD-TOK !
   HOLD-TOK @ FN8 @ DECL HOLD-TOK !
   HOLD-TOK @ FN8 @ 2 ARITY HOLD-TOK !
   HOLD-TOK @ FN8 @ s" held" SCHROOT @ 1 1 CELL CELL CELL 0 FIELD HOLD-TOK !
   HOLD-TOK @ PREPARE
   HOLD-TOK @ COMMIT ;

: TEST-SNAPSHOT-FAILURE ( -- )
   HOLD-OWNER
   DEPTH 1 T=
   TYPE-FIELD:TX-DEPTH 1 T=
   HOLD-SAVE
   SNAP-CATCH TC !
   TC @ TEST-E-PF-TX T=
   HOLD-SAME
   HOLD-TOK @ ROLLBACK
   DEPTH 0 T=
   TYPE-FIELD:TX-DEPTH 0 T=
   OPEN HOLD-NEXT !
   HOLD-NEXT @ HOLD-TOK @ 1 + T=
   HOLD-NEXT @ ROLLBACK
   DEPTH 0 T=
   TYPE-FIELD:TX-DEPTH 0 T= ;

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
   TYPE-FIELD:COUNT VN-PFP ! ;

: VN-CHECK-REG ( -- )
   TFAM-N@ VN-TFAM @ T=          TF-STR-U@ VN-STR @ T=
   TF-PK-N@ VN-PK @ T=           SUMV-N@ VN-SUMV @ T=
   LAY-N@ VN-LAY @ T=            SCHEMA-N@ VN-SCH @ T=
   SCHEMA-ROOT-N@ VN-ROOT @ T=   TYPE-FIELD:COUNT VN-PF @ T= ;

: VN-CHECK-OPEN-REG ( -- )
   VN-CHECK-REG
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
   VN-CHECK-IN  VN-CHECK-OPEN-REG
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

: WF-RUN ( [ -- ] -- ) {: op :} \ typed-local-lint: allow-bare-local
   WF-REG-SAVE
   WF-STATE-SAVE
   op catch TC !
   TC @ E-DEV-FAMILY-SCOPE T=
   WF-STATE-SAME
   WF-REG-SAME ;

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
WF-START  ' WF-BAD-DECL WF-RUN  WF-FINISH
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
variable HF-BASE-PF

: HF-SAVE ( -- )
   WF-REG-SAVE
   WF-STATE-SAVE
   DEV-TX-TOP DEVTX.FLDTOK @ FV7 @ HF-FLDA @
      TYPE-FIELD-OWNER:TX-SCHEMA-FOR HF-SA !
   DEV-TX-TOP DEVTX.FLDTOK @ FV7 @ HF-FLDB @
      TYPE-FIELD-OWNER:TX-SCHEMA-FOR HF-SB !
   DEV-TX-TOP DEVTX.FLDTOK @ FV7 @ HF-FLDA @
      TYPE-FIELD-OWNER:TX-CELLS-FOR HF-CA !
   DEV-TX-TOP DEVTX.FLDTOK @ FV7 @ HF-FLDB @
      TYPE-FIELD-OWNER:TX-CELLS-FOR HF-CB ! ;

: HF-SAME ( -- )
   WF-STATE-SAME
   WF-REG-SAME
   DEV-TX-TOP DEVTX.FLDTOK @ FV7 @ HF-FLDA @
      TYPE-FIELD-OWNER:TX-SCHEMA-FOR HF-SA @ T=
   DEV-TX-TOP DEVTX.FLDTOK @ FV7 @ HF-FLDB @
      TYPE-FIELD-OWNER:TX-SCHEMA-FOR HF-SB @ T=
   DEV-TX-TOP DEVTX.FLDTOK @ FV7 @ HF-FLDA @
      TYPE-FIELD-OWNER:TX-CELLS-FOR HF-CA @ T=
   DEV-TX-TOP DEVTX.FLDTOK @ FV7 @ HF-FLDB @
      TYPE-FIELD-OWNER:TX-CELLS-FOR HF-CB @ T=
   TOK @ FV7 @ VAR-E @ PAYLOAD-N 2 T=
   TOK @ FV7 @ VAR-E @ 0 PAYLOAD-SCHEMA@ SCHROOT @ T=
   TOK @ FV7 @ VAR-E @ 1 PAYLOAD-SCHEMA@ HF-SCH @ T=
   TOK @ FV7 @ VAR-E @ 0 PAYLOAD-WIDTH@ 1 T=
   TOK @ FV7 @ VAR-E @ 1 PAYLOAD-WIDTH@ 1 T=
   TOK @ FV7 @ VAR-E @ PAYLOAD-CELLS 2 T= ;

: HF-OPEN ( -- )
   TYPE-FIELD:COUNT HF-BASE-PF !
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
   TYPE-FIELD:COUNT HF-BASE-PF @ T= ;

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
      s" txc-foreign" SCHROOT @ 0 1 0 CELL CELL 0 TYPE-FIELD-OWNER:ADD drop
   DEV-FLD-PROVISIONAL-COUNT TXC-END !
   HF-SAVE ;

: TXC-WRONG-TOKEN ( -- )
   TXC-TOK @ 1 + FV7 @ TXC-OWN @
   TYPE-FIELD-OWNER:TX-CELLS-FOR drop ;

: TXC-WRONG-FAMILY ( -- )
   TXC-TOK @ FD3 @ TXC-OWN @
   TYPE-FIELD-OWNER:TX-CELLS-FOR drop ;

: TXC-FOREIGN-ROW ( -- )
   TXC-TOK @ FV7 @ TXC-FOREIGN @
   TYPE-FIELD-OWNER:TX-CELLS-FOR drop ;

: TXC-NEGATIVE ( -- )
   TXC-TOK @ FV7 @ -1
   TYPE-FIELD-OWNER:TX-CELLS-FOR drop ;

: TXC-ONE-PAST ( -- )
   TXC-TOK @ FV7 @ TXC-END @
   TYPE-FIELD-OWNER:TX-CELLS-FOR drop ;

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
\ 19. The coordinator participant drives the exact tokens it opened, and its
\     cleanup retires every frame a declaration body left open above them.
\
\     Before this, the participant read the LIVE TOP frame at every phase. A body
\     that opened one nested frame and never closed it therefore had that frame
\     prepared, published and released in place of the participant's own, whose
\     event and field frames both survived the transaction. The checker
\     participant finalizes last, found a field-transaction depth that no longer
\     matched its savepoint, and threw E-PF-TX out of cleanup — which poisons the
\     single production coordinator for the whole process, so every later
\     declaration failed with E-TRANSACTION-POISONED.
\
\     One deliberate behaviour change comes with this. A hostile or confused body
\     that retires the participant's OWN frame (it can: DECL-EVENT is reopenable,
\     the pre-existing posture) used to leave the participant reading whatever
\     frame was on top and silently no-op. It now fails closed: the saved token
\     no longer names a live frame, cleanup rejects before touching anything, and
\     the coordinator poisons. That is the correct trade — once the frame the
\     participant opened is gone, its watermarks are unrecoverable, so there is
\     nothing left to restore and pretending otherwise is what corrupted the
\     coordinator in the first place.
\
\     These are measured failures, not hypotheticals. Each mutation below was
\     applied to the production source, the engine was refreshed, and this suite
\     was rerun:
\       - DEV-PART-TOKEN reading DEV-TX-TOP again instead of the saved slot:
\         4 assertions fail in 19e/19f — the leaked frame is retired but the
\         participant's own event frame survives the transaction.
\       - DEV-RETIRE-THROUGH taking the top frame instead of DEV-TX-INDEX:
\         14 assertions fail, starting with 19b's stale-event reject, which
\         stops throwing at all, and the suite dies on an uncaught E-PF-TX.
\       - TYPE-FIELD-OWNER ROLLBACK-THROUGH taking the top frame instead of
\         TX-INDEX: 19a's field-depth restore and 19b's untouched-marks
\         assertion both fail, and the suite dies on an uncaught E-PF-TX.
\ ---------------------------------------------------------------------------
\ Section 19 owns one family of its own so its field slots never collide with a
\ row an earlier section committed.
variable FR9
s" de" CHECKER-PACKAGE-PUBLIC s" r9" 2 TK-PRODUCT TWX-TFAM-DECL FR9 !

$100000 constant RT-STALE       \ added to a live token to name one never minted
-7195 constant RT-BODY-CODE     \ a declaration body's own, unrelated failure

variable RT-TOK    variable RT-FLD
variable RT-N      variable RT-PUB    variable RT-BASE
variable RT-FORD   variable RT-VORD   variable RT-CUR
variable RT-EDEPTH variable RT-FDEPTH
variable RT-RC     variable RT-EV0    variable RT-PF0

: RT-SAVE ( -- )                \ every transaction mark a retire-through restores
   DEV-N @ RT-N !               DEV-PUB-N @ RT-PUB !
   DEV-BASE-FLD @ RT-BASE !     DEV-FLD-ORD @ RT-FORD !
   DEV-VAR-ORD @ RT-VORD !      DEV-CUR-VAR @ RT-CUR !
   DEV-TX-DEPTH @ RT-EDEPTH !   TYPE-FIELD:TX-DEPTH RT-FDEPTH ! ;

: RT-SAME ( -- )
   DEV-N @ RT-N @ T=              DEV-PUB-N @ RT-PUB @ T=
   DEV-BASE-FLD @ RT-BASE @ T=    DEV-FLD-ORD @ RT-FORD @ T=
   DEV-VAR-ORD @ RT-VORD @ T=     DEV-CUR-VAR @ RT-CUR @ T=
   DEV-TX-DEPTH @ RT-EDEPTH @ T=  TYPE-FIELD:TX-DEPTH RT-FDEPTH @ T= ;

: RT-MARK ( -- ) WF-REG-SAVE RT-SAVE ;
: RT-RESTORED ( -- ) RT-SAME WF-REG-SAME ;

: RT-BASE-FRAME ( ptr u8 n -- )    \ one frame that owns a declaration and a field
   {: na:ptr nu:n :}
   OPEN RT-TOK !
   DEV-TX-TOP DEVTX.FLDTOK @ RT-FLD !
   RT-TOK @ FR9 @ DECL RT-TOK !
   RT-TOK @ FR9 @ na nu SCHROOT @ 0 1 0 CELL CELL 0 FIELD RT-TOK ! ;

: RT-DESCEND ( ptr u8 n n -- n )   \ one nested frame with a declaration and a field
   {: na:ptr nu:n slot:n :}
   OPEN {: tok:n :}
   tok FR9 @ DECL {: dtok:n :}
   dtok FR9 @ na nu SCHROOT @ slot 1 slot cells CELL CELL 0 FIELD ;

: RT-RETIRE ( -- ) RT-TOK @ RT-FLD @ DEV-RETIRE-THROUGH ;
: RT-STALE-EVENT ( -- ) RT-TOK @ RT-STALE + RT-FLD @ DEV-RETIRE-THROUGH ;
: RT-STALE-FIELD ( -- ) RT-TOK @ RT-FLD @ RT-STALE + DEV-RETIRE-THROUGH ;

\ Both tokens live, but belonging to DIFFERENT frames. Each stack would find its
\ own token and retire to a different depth, desynchronising the two stacks with
\ no error, so the pair itself has to be cross-checked.
variable RT-OUTER-TOK   variable RT-INNER-FLD
: RT-MIXED-PAIR ( -- ) RT-OUTER-TOK @ RT-INNER-FLD @ DEV-RETIRE-THROUGH ;

\ Reaching the field owner's OWN not-in-stack guard needs the pair to agree while
\ the field token is dead, which only a narrow private swap of the frame's stored
\ token can produce — the pair cross-check above now rejects every route that
\ leaves them disagreeing. Swap it, prove the owner rejects pre-mutation, swap it
\ back.
variable RT-SAVED-FLD
: RT-FLD-CELL ( -- ptr a ) RT-TOK @ DEV-TX-INDEX DEV-TX-AT DEVTX.FLDTOK ;
: RT-DESYNC-FLD ( -- )
   RT-FLD-CELL @ RT-SAVED-FLD !
   RT-SAVED-FLD @ RT-STALE + RT-FLD-CELL ! ;
: RT-RESYNC-FLD ( -- ) RT-SAVED-FLD @ RT-FLD-CELL ! ;
: RT-DEAD-FIELD ( -- ) RT-TOK @ RT-FLD-CELL @ DEV-RETIRE-THROUGH ;

\ 19a. Two leaked descendants are retired last-in first-out and the target
\      frame's own marks are the ones left standing.
TWX-CAND-START
RT-MARK
s" rt-base" RT-BASE-FRAME
s" rt-d1" 1 RT-DESCEND drop
s" rt-d2" 2 RT-DESCEND drop
DEV-TX-DEPTH @ RT-EDEPTH @ 3 + T=
TYPE-FIELD:TX-DEPTH RT-FDEPTH @ 3 + T=
RT-RETIRE
RT-RESTORED
0 TWX-CAND-DONE drop

\ 19b. Every way of presenting a bad token pair rejects BEFORE the first mutation
\      and leaves the leaked frames exactly as they were: a dead event token (the
\      index lookup here), a live pair from two different frames (the pair
\      cross-check here), and a dead field token (the owner's own cleanup vector).
TWX-CAND-START
s" rt-guard" RT-BASE-FRAME
s" rt-g1" 1 RT-DESCEND drop
RT-MARK
' RT-STALE-EVENT catch TC !
TC @ E-DEV-TX T=
RT-RESTORED
' RT-STALE-FIELD catch TC !
TC @ E-DEV-TX T=
RT-RESTORED
\ both tokens live, but from different frames: the pair cross-check rejects it
\ before either stack moves. Without that check the event side retires to the
\ outer frame and the field side only to the inner one, and the stacks end at
\ different depths with rc 0.
RT-TOK @ RT-OUTER-TOK !
DEV-TX-TOP DEVTX.FLDTOK @ RT-INNER-FLD !
' RT-MIXED-PAIR catch TC !
TC @ E-DEV-TX T=
RT-RESTORED
\ pair agreeing but the field token dead: the owner's own guard is what rejects
RT-DESYNC-FLD
' RT-DEAD-FIELD catch TC !
TC @ E-PF-TX T=
RT-RESYNC-FLD
RT-RESTORED
RT-RETIRE
DEV-TX-DEPTH @ RT-EDEPTH @ 2 - T=
0 TWX-CAND-DONE drop

\ 19c. Retiring a COMMITTED frame puts every published mark back, including the
\      committed field high-water the outer commit advanced.
TWX-CAND-START
RT-MARK
s" rt-commit" RT-BASE-FRAME
RT-TOK @ PREPARE
RT-TOK @ COMMIT
DEV-PUB-N @ RT-PUB @ 2 + T=
TYPE-FIELD:COUNT P-PF @ 1 + T=
RT-RETIRE
RT-RESTORED
0 TWX-CAND-DONE drop

\ 19d. A nested frame that succeeds on its own is still provisional: retiring the
\      outer frame through the same path puts BOTH frames' marks back.
TWX-CAND-START
RT-MARK
s" rt-outer" RT-BASE-FRAME
s" rt-inner" 1 RT-DESCEND PUBLISH
DEV-TX-DEPTH @ RT-EDEPTH @ 1 + T=
TYPE-FIELD:TX-DEPTH RT-FDEPTH @ 1 + T=
DEV-PUB-N @ RT-PUB @ T=
RT-RETIRE
RT-RESTORED
0 TWX-CAND-DONE drop

\ 19e. The production coordinator. A body that leaks one nested frame is now
\      rejected in PREPARE, cleaned up in ROLLBACK, and leaves no poison; a body
\      that leaks and then fails surfaces only its OWN error.
\
\      The PHASE assertion is load-bearing, not decoration. DEV-COMMIT repeats
\      the same token proof, so gutting DEV-PART-PREPARE to `( n -- n ) ;` still
\      rejects the leak with the identical code, identical depths and no poison —
\      only one phase later, after the checker participant has already reversibly
\      committed. Asserting the code alone lets that mutant live; asserting the
\      failing phase and participant kills it.
: RT-LEAK-BODY ( -- ) OPEN drop ;
: RT-LEAK-RUN ( -- ) [: RT-LEAK-BODY ;] GENERATED-DECL:RUN ;
: RT-LEAK-CATCH ( -- n ) [: RT-LEAK-RUN ;] catch ;

: RT-LEAK-FAIL-BODY ( -- ) OPEN drop RT-BODY-CODE throw ;
: RT-LEAK-FAIL-RUN ( -- ) [: RT-LEAK-FAIL-BODY ;] GENERATED-DECL:RUN ;
: RT-LEAK-FAIL-CATCH ( -- n ) [: RT-LEAK-FAIL-RUN ;] catch ;

: RT-HEALTHY ( -- )
   GENERATED-DECL:POISONED? 0= T-TRUE
   GENERATED-DECL:DEPTH 0 T=
   RT-RESTORED ;

RT-MARK
RT-LEAK-CATCH RT-RC !
RT-RC @ E-DEV-TX T=
GENERATED-DECL:LAST-FAILURE-PHASE GENERATED-DECL:PHASE-PREPARE T=
GENERATED-DECL:LAST-FAILURE-PARTICIPANT DEV-PARTICIPANT T=
RT-HEALTHY

RT-LEAK-FAIL-CATCH RT-RC !
RT-RC @ RT-BODY-CODE T=
RT-HEALTHY

\ 19f. And the next declaration through the same coordinator still publishes:
\      the participant's own frame is the one prepared, committed and released.
: RT-GOOD-BODY ( -- )
   CURRENT {: tok:n :}
   tok FR9 @ DECL {: dtok:n :}
   dtok FR9 @ s" rt-after" SCHROOT @ 0 1 0 CELL CELL 0 FIELD drop ;
: RT-GOOD-RUN ( -- ) [: RT-GOOD-BODY ;] GENERATED-DECL:RUN ;
: RT-GOOD-CATCH ( -- n ) [: RT-GOOD-RUN ;] catch ;

COUNT RT-EV0 !
TYPE-FIELD:COUNT RT-PF0 !
RT-GOOD-CATCH 0 T=
COUNT RT-EV0 @ 2 + T=
TYPE-FIELD:COUNT RT-PF0 @ 1 + T=
DEPTH 0 T=
TYPE-FIELD:TX-DEPTH 0 T=
GENERATED-DECL:POISONED? 0= T-TRUE

;package

\ ---------------------------------------------------------------------------
: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" decl-event-suite: failures" 1 die ;
REPORT

;using
