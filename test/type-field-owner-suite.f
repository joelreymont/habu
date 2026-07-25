\ type-field-owner-suite.f — retained rollback-frame lifecycle regression.

require lib/test.f
require lib/test/outcome.f
require lib/test/subject.f
require test/checker-assert.f

T-RESET

package TYPE-FIELD-OWNER-TEST

$400 constant CAP
1000 constant TIMEOUT-MS
7161 constant E-EVENT-TX
70 constant CHECK-REJECT-RC

create OUT CAP allot
create ERR CAP allot

variable FAM
variable SCH
variable TOK
variable OUTER-TOK
variable INNER-TOK
variable NEXT-TOK
variable BASE-N
variable BASE-EVENT
variable BASE-STR
variable AFTER-ADD-STR
variable EVENT-N
variable EVENT-ID
variable FIELD-N
variable STRING-N
variable RC

\ --- retired product-field lifecycle names -----------------------------------
\ Three independent boundaries, weakest first. Dictionary absence and the
\ whole-load rc-70 failure are SUPPLEMENTAL: both only say the name is currently
\ undefined, which any later source could change by defining it. The acceptance
\ evidence is REJECT-VERDICT below — the checker's retired-token set answers 0
\ (rejected) before it ever consults a signature or primitive row, so the name
\ stays rejected even if something defines it again.

: ABSENT ( ptr u8 n -- )
   0 search-wl 0= TTRUE ;

: QUALIFIED-ABSENT ( ptr u8 n -- )
   XREF-FIND XREF-FOUND? 0= TTRUE ;

: INTERPRETER-ABSENCE ( -- )
   s" PF-BEGIN" ABSENT
   s" PF-ADD" ABSENT
   s" PF-PUBLISH" ABSENT
   s" PF-RELEASE" ABSENT
   s" PF-FINALIZE" ABSENT
   s" PF-COMMIT" ABSENT
   s" PF-ROLLBACK" ABSENT ;

\ supplemental: a real child load of the bad definition exits 70 naming the token
: LOAD-REJECTS ( ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n name:ptr nameu:n :}
   src srcu OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS SUBJECT:RUN
   CHECK-REJECT-RC T-OUTCOME-EXITED=
   LEN>N {: erru:n :}
   LEN>N {: outu:n :}
   outu 0 T=
   ERR erru s" E-UNDEFINED" CONTAINS? TTRUE
   ERR erru name nameu CONTAINS? TTRUE ;

: LOAD-ABSENCE ( -- )
   s" : TFO-NO-BEGIN ( -- ) PF-BEGIN ;" s" PF-BEGIN" LOAD-REJECTS
   s" : TFO-NO-ADD ( -- ) PF-ADD ;" s" PF-ADD" LOAD-REJECTS
   s" : TFO-NO-PUBLISH ( -- ) PF-PUBLISH ;" s" PF-PUBLISH" LOAD-REJECTS
   s" : TFO-NO-RELEASE ( -- ) PF-RELEASE ;" s" PF-RELEASE" LOAD-REJECTS
   s" : TFO-NO-FINALIZE ( -- ) PF-FINALIZE ;" s" PF-FINALIZE" LOAD-REJECTS
   s" : TFO-NO-COMMIT ( -- ) PF-COMMIT ;" s" PF-COMMIT" LOAD-REJECTS
   s" : TFO-NO-ROLLBACK ( -- ) PF-ROLLBACK ;" s" PF-ROLLBACK" LOAD-REJECTS ;

\ acceptance: production checker verdict is 0 (rejected), never 1 (uncheckable)
: REJECT-VERDICT ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 0 T= ;
: CERTIFY-VERDICT ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! -1 T= ;

: CHECKER-RETIRED ( -- )
   s" TFO-R1 ( -- n ) PF-BEGIN" REJECT-VERDICT
   s" TFO-R2 ( n n n ptr u8 n n n n n n n n -- n ) PF-ADD" REJECT-VERDICT
   s" TFO-R3 ( n -- ) PF-PUBLISH" REJECT-VERDICT
   s" TFO-R4 ( -- ) PF-RELEASE" REJECT-VERDICT
   s" TFO-R5 ( n -- ) PF-FINALIZE" REJECT-VERDICT
   s" TFO-R6 ( n -- ) PF-COMMIT" REJECT-VERDICT
   s" TFO-R7 ( n -- ) PF-ROLLBACK" REJECT-VERDICT
   \ case folding is exact-token, so spelling variants reject identically
   s" TFO-R8 ( -- n ) pf-begin" REJECT-VERDICT
   s" TFO-R9 ( n -- ) Pf-RollBack" REJECT-VERDICT ;

\ The match is exact, never a prefix or substring. These neighbours must NOT be
\ caught by the retired set, or it would be silently over-broad. They are
\ uncheckable (1) rather than certified because they are registry-internal names
\ with no checker axiom — the point is only that they are not REJECTED (0), so
\ the assertion stays true if any of them later gains an axiom.
: NOT-RETIRED ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 0 <> TTRUE ;

: CHECKER-NEIGHBOURS-LIVE ( -- )
   s" TFO-N1 ( -- n ) PF-NO-VARIANT" NOT-RETIRED
   s" TFO-N2 ( -- n ) PF-FLAGS-NONE" NOT-RETIRED
   \ PF-COMMIT-N has a retired name as a strict prefix; prefix matching would
   \ wrongly reject it
   s" TFO-N3 ( -- n ) PF-COMMIT-N" NOT-RETIRED
   \ and a longer name that merely contains one
   s" TFO-N4 ( -- n ) TFO-PF-BEGIN-LOCAL" NOT-RETIRED
   \ a {: :} local spelled like a retired name shadows the retirement exactly as
   \ it shadows every other resolution: a local reference is resolved before any
   \ dictionary lookup, so it can never reach global position
   s" TFO-N6 ( n -- n ) {: pf-add:n :} pf-add" CERTIFY-VERDICT
   s" TFO-N7 ( n -- n ) {: pf-commit:n :} pf-commit" CERTIFY-VERDICT
   \ positive control: the qualified owner API is untouched by the reject set
   s" TFO-N5 ( -- n ) TYPE-FIELD:COUNT" CERTIFY-VERDICT ;

\ Every public owner operation certifies as an ordinary checked qualified call
\ with its exact row — this is what replaced the deleted DEV-FLD-* trust shims.
: CHECKER-OWNER-API ( -- )
   s" TFO-A1 ( -- n ) TYPE-FIELD-OWNER:OPEN" CERTIFY-VERDICT
   s" TFO-A2 ( n n n ptr u8 n n n n n n n n -- n ) TYPE-FIELD-OWNER:ADD" CERTIFY-VERDICT
   s" TFO-A3 ( n -- n ) TYPE-FIELD-OWNER:PREPARE" CERTIFY-VERDICT
   s" TFO-A4 ( n -- ) TYPE-FIELD-OWNER:COMMIT" CERTIFY-VERDICT
   s" TFO-A5 ( n -- ) TYPE-FIELD-OWNER:FINALIZE" CERTIFY-VERDICT
   s" TFO-A6 ( n -- ) TYPE-FIELD-OWNER:ROLLBACK" CERTIFY-VERDICT
   s" TFO-A7 ( n n n -- n ) TYPE-FIELD-OWNER:TX-SCHEMA-FOR" CERTIFY-VERDICT
   s" TFO-A8 ( n n n -- n ) TYPE-FIELD-OWNER:TX-CELLS-FOR" CERTIFY-VERDICT ;

\ The axiom rows carry real roles, so a mis-ordered or mis-typed argument list
\ is rejected rather than certified. Without the PPRIM rows these would all be
\ uncheckable (1) instead of rejected (0).
: CHECKER-OWNER-ROLES ( -- )
   \ name pointer and length swapped in ADD's field-name pair
   s" TFO-B1 ( n n n n ptr u8 n n n n n n n -- n ) TYPE-FIELD-OWNER:ADD" REJECT-VERDICT
   \ phase token is not a plain cell
   s" TFO-B2 ( bool n n ptr u8 n n n n n n n n -- n ) TYPE-FIELD-OWNER:ADD" REJECT-VERDICT
   s" TFO-B3 ( bool -- ) TYPE-FIELD-OWNER:COMMIT" REJECT-VERDICT
   s" TFO-B4 ( bool -- ) TYPE-FIELD-OWNER:FINALIZE" REJECT-VERDICT
   s" TFO-B5 ( bool -- ) TYPE-FIELD-OWNER:ROLLBACK" REJECT-VERDICT
   s" TFO-B6 ( ptr u8 n -- n ) TYPE-FIELD-OWNER:PREPARE" REJECT-VERDICT
   \ TX-SCHEMA-FOR takes three cells, not a string plus a cell
   s" TFO-B7 ( n ptr u8 n -- n ) TYPE-FIELD-OWNER:TX-SCHEMA-FOR" REJECT-VERDICT
   \ wrong output arity is rejected too
   s" TFO-B8 ( n -- ) TYPE-FIELD-OWNER:PREPARE" REJECT-VERDICT ;

\ Every owner word with an active PPRIM row must still RESOLVE after the
\ generated-declaration protection pass has run. `undefine` retires a dictionary
\ entry and its usig metadata but NOT a primitive axiom, so undefining one of
\ these would leave the checker certifying calls the runtime cannot resolve —
\ a definition that passes CHECK and then fails at load.
\
\ This is a measured failure, not a hypothetical: adding
\ `undefine TYPE-FIELD-OWNER:TX-SCHEMA-FOR` to
\ src/core/generated-declaration-protection.f makes exactly this assertion fail
\ (dictionary absent, checker verdict -1) on every run. The check covers all
\ eight axiom-carrying words rather than the one that happened to be undefined
\ before, so any future protection edit is caught by name.
: OWNER-WORD-LIVE ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n src:ptr srcu:n :}
   name nameu XREF-FIND XREF-FOUND? TTRUE
   src srcu CERTIFY-VERDICT ;

\ --- the multi-frame cleanup capability has no source-level surface ------------
\ ROLLBACK-THROUGH retires a whole chain of field frames at once. It is private
\ to this owner and reachable only through the deferred TDECL-FIELD-CLEANUP-XT
\ that src/core/generated-declaration-protection.f retires once the compiled
\ declaration-event participant has bound it. Scope that claim exactly: what is
\ closed is the TYPE-FIELD-OWNER surface — the package is sealed, so no source
\ can reopen it and pick a frame out of the middle of the field stack. It does
\ NOT mean the whole path is unreachable: DECL-EVENT is reopenable (pre-existing
\ posture, which is how this repo's own suites drive the participant), so source
\ that reopens DECL-EVENT can still call DEV-RETIRE-THROUGH. That entry is
\ guarded on its own terms — live event token, matching field token, and the
\ field chain checked here — rather than by being unnameable.
\ Three independent boundaries, weakest first:
\ the name is gone from the dictionary, the checker no longer knows it, and a
\ real child load naming it exits 70 with E-UNDEFINED.
\
\ A checker PRIMITIVE row would have defeated the whole arrangement: `undefine`
\ retires a dictionary entry and its usig metadata but NOT a primitive axiom, so
\ a PPRIM row for the cleanup vector would keep certifying calls that the runtime
\ can no longer resolve. That is why this seam is a deferred word and not a
\ primitive, and why the assertion below is on ABSENCE rather than on a role.
\
\ Measured, not hypothetical: commenting out the `undefine TDECL-FIELD-CLEANUP-XT`
\ line in src/core/generated-declaration-protection.f and refreshing the engine
\ makes the first assertion below fail on every run.
: CLEANUP-SEAM-ABSENT ( -- )
   s" TDECL-FIELD-CLEANUP-XT" ABSENT
   s" TDECL-FIELD-CLEANUP-XT" CHECKER-DEFINED? 0= TTRUE
   s" : TFO-NO-CLEANUP ( n -- ) TDECL-FIELD-CLEANUP-XT ;"
      s" TDECL-FIELD-CLEANUP-XT" LOAD-REJECTS
   s" ROLLBACK-THROUGH" ABSENT
   s" TYPE-FIELD-OWNER:ROLLBACK-THROUGH" QUALIFIED-ABSENT
   s" TYPE-FIELD-OWNER:ROLLBACK-THROUGH" CHECKER-DEFINED? 0= TTRUE
   s" : TFO-NO-THROUGH ( n -- ) TYPE-FIELD-OWNER:ROLLBACK-THROUGH ;"
      s" ROLLBACK-THROUGH" LOAD-REJECTS
   s" TYPE-FIELD-OWNER:TX-INDEX" QUALIFIED-ABSENT ;

\ --- the total release capability has no source-level surface either -----------
\ The declaration transaction's release phase runs after every reversible commit
\ has published, so the participant that owns a field frame has to discard it
\ without being able to reject. The public FINALIZE cannot do that job: it
\ re-proves the token, the frame state, and the watermarks, and each of those
\ proofs throws. The discard is this owner's private RELEASE, reached by the
\ compiled declaration-event participant through the deferred
\ TDECL-FIELD-RELEASE-XT that src/core/generated-declaration-protection.f retires
\ once that sole caller is bound. Same reasoning and the same three independent
\ boundaries as the cleanup seam above: the name is gone from the dictionary, the
\ checker no longer knows it, and a real child load naming it exits 70 with
\ E-UNDEFINED. Deferred rather than a primitive row for the same reason - an
\ axiom survives `undefine` and would keep certifying calls the runtime can no
\ longer resolve.
\
\ Measured, not hypothetical: deleting the `undefine TDECL-FIELD-RELEASE-XT` line
\ in src/core/generated-declaration-protection.f and refreshing the engine makes
\ the first assertion below fail on every run.
: RELEASE-SEAM-ABSENT ( -- )
   s" TDECL-FIELD-RELEASE-XT" ABSENT
   s" TDECL-FIELD-RELEASE-XT" CHECKER-DEFINED? 0= TTRUE
   s" : TFO-NO-RELEASE ( -- ) TDECL-FIELD-RELEASE-XT ;"
      s" TDECL-FIELD-RELEASE-XT" LOAD-REJECTS
   s" RELEASE" ABSENT
   s" TYPE-FIELD-OWNER:RELEASE" QUALIFIED-ABSENT
   s" TYPE-FIELD-OWNER:RELEASE" CHECKER-DEFINED? 0= TTRUE
   s" : TFO-NO-OWNER-RELEASE ( -- ) TYPE-FIELD-OWNER:RELEASE ;"
      s" RELEASE" LOAD-REJECTS ;

: OWNER-API-RUNTIME-LIVE ( -- )
   s" TYPE-FIELD-OWNER:OPEN"
      s" TFO-L1 ( -- n ) TYPE-FIELD-OWNER:OPEN" OWNER-WORD-LIVE
   s" TYPE-FIELD-OWNER:ADD"
      s" TFO-L2 ( n n n ptr u8 n n n n n n n n -- n ) TYPE-FIELD-OWNER:ADD"
      OWNER-WORD-LIVE
   s" TYPE-FIELD-OWNER:PREPARE"
      s" TFO-L3 ( n -- n ) TYPE-FIELD-OWNER:PREPARE" OWNER-WORD-LIVE
   s" TYPE-FIELD-OWNER:COMMIT"
      s" TFO-L4 ( n -- ) TYPE-FIELD-OWNER:COMMIT" OWNER-WORD-LIVE
   s" TYPE-FIELD-OWNER:FINALIZE"
      s" TFO-L5 ( n -- ) TYPE-FIELD-OWNER:FINALIZE" OWNER-WORD-LIVE
   s" TYPE-FIELD-OWNER:ROLLBACK"
      s" TFO-L6 ( n -- ) TYPE-FIELD-OWNER:ROLLBACK" OWNER-WORD-LIVE
   s" TYPE-FIELD-OWNER:TX-SCHEMA-FOR"
      s" TFO-L7 ( n n n -- n ) TYPE-FIELD-OWNER:TX-SCHEMA-FOR" OWNER-WORD-LIVE
   s" TYPE-FIELD-OWNER:TX-CELLS-FOR"
      s" TFO-L8 ( n n n -- n ) TYPE-FIELD-OWNER:TX-CELLS-FOR" OWNER-WORD-LIVE ;

: COMMITTED-EVENT-STABLE ( -- )
   DECL-EVENT:COUNT EVENT-N @ T=
   DECL-EVENT:IDENTITY EVENT-ID @ T=
   DECL-EVENT:DEPTH 1 T=
   TYPE-FIELD:COUNT FIELD-N @ T=
   TF-STR-U@ STRING-N @ T=
   DECL-EVENT:CURRENT-VARIANT DECL-EVENT:NO-VARIANT T= ;

: TRY-CURRENT ( -- )
   DECL-EVENT:CURRENT drop ;
: TRY-DECL ( -- )
   TOK @ FAM @ DECL-EVENT:DECL drop ;
: TRY-ARITY ( -- )
   TOK @ FAM @ 0 DECL-EVENT:ARITY drop ;
: TRY-POLICY ( -- )
   TOK @ FAM @ 0 DECL-EVENT:POLICY drop ;
: TRY-DERIVE ( -- )
   TOK @ FAM @ 1 DECL-EVENT:DERIVE drop ;
: TRY-VARIANT ( -- )
   TOK @ FAM @ s" late-variant" DECL-EVENT:VARIANT drop ;
: TRY-END-VARIANT ( -- )
   TOK @ FAM @ DECL-EVENT:END-VARIANT drop ;
: TRY-FIELD ( -- )
   TOK @ FAM @ s" late-field" SCH @
   7 1 7 cells CELL CELL 0 DECL-EVENT:FIELD drop ;
: TRY-FIELD-SCHEMA ( -- )
   TOK @ FAM @ FIELD-N @ 1 - DECL-EVENT:FIELD-SCHEMA@ drop ;
: TRY-PREPARE ( -- )
   TOK @ DECL-EVENT:PREPARE ;
: TRY-COMMIT ( -- )
   TOK @ DECL-EVENT:COMMIT ;
: TRY-PUBLISH ( -- )
   TOK @ DECL-EVENT:PUBLISH ;

public

: REOPEN-SEALED ( -- )
   S\" package TYPE-FIELD-OWNER\nTX-TOP\n;package"
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS SUBJECT:RUN
   ENGINE-ERROR:SEAL-PACKAGE T-OUTCOME-EXITED=
   LEN>N {: erru:n :}
   LEN>N {: outu:n :}
   outu 0 T=
   ERR erru s" TYPE-FIELD-OWNER" CONTAINS? TTRUE ;

\ The seal is what makes ROLLBACK-THROUGH's privacy load-bearing rather than
\ decorative: absence from the dictionary only means nothing can NAME it, while
\ the seal is what stops source from reopening the package and calling it by its
\ bare private tail. Assert that route closes with the same exit 84, so a future
\ change that unseals this package fails here instead of silently handing every
\ caller a way to retire field frames out of order.
: REOPEN-CLEANUP-SEALED ( -- )
   S\" package TYPE-FIELD-OWNER\n0 ROLLBACK-THROUGH\n;package"
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS SUBJECT:RUN
   ENGINE-ERROR:SEAL-PACKAGE T-OUTCOME-EXITED=
   LEN>N {: erru:n :}
   LEN>N {: outu:n :}
   outu 0 T=
   ERR erru s" TYPE-FIELD-OWNER" CONTAINS? TTRUE ;

private

DECL-EVENT:COUNT BASE-EVENT !

public

PRODUCT row 0
   FIELD seed n
;PRODUCT

private

BASE-EVENT @ DECL-EVENT:FAMILY@ FAM !
BASE-EVENT @ 2 + DECL-EVENT:FIELD@ TYPE-FIELD:SCHEMA@ SCH !

\ PREPARE returns the exact provisional count without changing any owner.
TYPE-FIELD:COUNT BASE-N !
TF-STR-U@ BASE-STR !
TYPE-FIELD-OWNER:OPEN TOK !
TOK @ FAM @ TYPE-FIELD:NO-VARIANT s" state" SCH @
   1 1 CELL CELL CELL 0 TYPE-FIELD-OWNER:ADD TOK !
TF-STR-U@ AFTER-ADD-STR !
TOK @ 1 + ' TYPE-FIELD-OWNER:PREPARE catch RC ! drop
RC @ E-PF-TX T=
TOK @ TYPE-FIELD-OWNER:PREPARE BASE-N @ 1 + T=
TYPE-FIELD:COUNT BASE-N @ T=
TF-STR-U@ AFTER-ADD-STR @ T=
TYPE-FIELD:TX-DEPTH 1 T=
TOK @ TYPE-FIELD-OWNER:COMMIT
TYPE-FIELD:COUNT BASE-N @ 1 + T=
TYPE-FIELD:TX-DEPTH 1 T=
TOK @ ' TYPE-FIELD-OWNER:COMMIT catch RC ! drop
RC @ E-PF-TX T=
TYPE-FIELD:COUNT BASE-N @ 1 + T=
TYPE-FIELD:TX-DEPTH 1 T=
TOK @ TYPE-FIELD-OWNER:ROLLBACK
TYPE-FIELD:COUNT BASE-N @ T=
TF-STR-U@ BASE-STR @ T=
TYPE-FIELD:TX-DEPTH 0 T=

\ FINALIZE rejects an open frame, then releases one committed frame.
TYPE-FIELD:COUNT BASE-N !
TYPE-FIELD-OWNER:OPEN TOK !
TOK @ ' TYPE-FIELD-OWNER:FINALIZE catch RC ! drop
RC @ E-PF-TX T=
TYPE-FIELD:TX-DEPTH 1 T=
TOK @ TYPE-FIELD-OWNER:PREPARE BASE-N @ T=
TOK @ TYPE-FIELD-OWNER:COMMIT
TYPE-FIELD:TX-DEPTH 1 T=
TOK @ TYPE-FIELD-OWNER:FINALIZE
TYPE-FIELD:TX-DEPTH 0 T=
TOK @ ' TYPE-FIELD-OWNER:ROLLBACK catch RC ! drop
RC @ E-PF-TX T=

\ A committed parent rejects nested OPEN before any watermark changes.
TYPE-FIELD:COUNT BASE-N !
TF-STR-U@ BASE-STR !
TYPE-FIELD-OWNER:OPEN OUTER-TOK !
OUTER-TOK @ FAM @ TYPE-FIELD:NO-VARIANT s" parent-finalize" SCH @
   2 1 2 cells CELL CELL 0 TYPE-FIELD-OWNER:ADD OUTER-TOK !
TF-STR-U@ AFTER-ADD-STR !
OUTER-TOK @ TYPE-FIELD-OWNER:PREPARE BASE-N @ 1 + T=
OUTER-TOK @ TYPE-FIELD-OWNER:COMMIT
' TYPE-FIELD-OWNER:OPEN catch RC !
RC @ E-PF-TX T=
TYPE-FIELD:TX-DEPTH 1 T=
TYPE-FIELD:COUNT BASE-N @ 1 + T=
TF-STR-U@ AFTER-ADD-STR @ T=
OUTER-TOK @ TYPE-FIELD-OWNER:FINALIZE
TYPE-FIELD:TX-DEPTH 0 T=
TYPE-FIELD-OWNER:OPEN NEXT-TOK !
NEXT-TOK @ OUTER-TOK @ 1 + T=
NEXT-TOK @ TYPE-FIELD-OWNER:ROLLBACK

\ Committed rollback restores the marks captured before the append.
TYPE-FIELD:COUNT BASE-N !
TF-STR-U@ BASE-STR !
TYPE-FIELD-OWNER:OPEN OUTER-TOK !
OUTER-TOK @ FAM @ TYPE-FIELD:NO-VARIANT s" parent-rollback" SCH @
   3 1 3 cells CELL CELL 0 TYPE-FIELD-OWNER:ADD OUTER-TOK !
TF-STR-U@ AFTER-ADD-STR !
OUTER-TOK @ TYPE-FIELD-OWNER:PREPARE BASE-N @ 1 + T=
OUTER-TOK @ TYPE-FIELD-OWNER:COMMIT
' TYPE-FIELD-OWNER:OPEN catch RC !
RC @ E-PF-TX T=
TYPE-FIELD:TX-DEPTH 1 T=
TYPE-FIELD:COUNT BASE-N @ 1 + T=
TF-STR-U@ AFTER-ADD-STR @ T=
OUTER-TOK @ TYPE-FIELD-OWNER:ROLLBACK
TYPE-FIELD:TX-DEPTH 0 T=
TYPE-FIELD:COUNT BASE-N @ T=
TF-STR-U@ BASE-STR @ T=

\ Nested success stays provisional until its open parent commits.
TYPE-FIELD:COUNT BASE-N !
TF-STR-U@ BASE-STR !
TYPE-FIELD-OWNER:OPEN OUTER-TOK !
OUTER-TOK @ FAM @ TYPE-FIELD:NO-VARIANT s" outer" SCH @
   4 1 4 cells CELL CELL 0 TYPE-FIELD-OWNER:ADD OUTER-TOK !
TYPE-FIELD-OWNER:OPEN INNER-TOK !
INNER-TOK @ FAM @ TYPE-FIELD:NO-VARIANT s" inner" SCH @
   5 1 5 cells CELL CELL 0 TYPE-FIELD-OWNER:ADD INNER-TOK !
INNER-TOK @ TYPE-FIELD-OWNER:PREPARE BASE-N @ 2 + T=
INNER-TOK @ TYPE-FIELD-OWNER:COMMIT
INNER-TOK @ TYPE-FIELD-OWNER:FINALIZE
TYPE-FIELD:COUNT BASE-N @ T=
TYPE-FIELD:TX-DEPTH 1 T=
OUTER-TOK @ TYPE-FIELD-OWNER:PREPARE BASE-N @ 2 + T=
OUTER-TOK @ TYPE-FIELD-OWNER:ROLLBACK
TYPE-FIELD:COUNT BASE-N @ T=
TF-STR-U@ BASE-STR @ T=
TYPE-FIELD:TX-DEPTH 0 T=

\ A committed declaration frame is immutable until its owner finalizes or
\ rolls it back. Every token-bearing operation that requires an open frame
\ rejects the exact retained token before changing published or live state.
DECL-EVENT:COUNT BASE-EVENT !
TYPE-FIELD:COUNT BASE-N !
TF-STR-U@ BASE-STR !
DECL-EVENT:OPEN TOK !
TOK @ FAM @ DECL-EVENT:DECL TOK !
TOK @ FAM @ s" event-rollback" SCH @
   6 1 6 cells CELL CELL 0 DECL-EVENT:FIELD TOK !
TOK @ DECL-EVENT:PREPARE
TOK @ DECL-EVENT:COMMIT
DECL-EVENT:COUNT EVENT-N !
DECL-EVENT:IDENTITY EVENT-ID !
TYPE-FIELD:COUNT FIELD-N !
TF-STR-U@ STRING-N !
COMMITTED-EVENT-STABLE

' TRY-CURRENT catch RC !
RC @ E-EVENT-TX T=
COMMITTED-EVENT-STABLE
' TRY-DECL catch RC !
RC @ E-EVENT-TX T=
COMMITTED-EVENT-STABLE
' TRY-ARITY catch RC !
RC @ E-EVENT-TX T=
COMMITTED-EVENT-STABLE
' TRY-POLICY catch RC !
RC @ E-EVENT-TX T=
COMMITTED-EVENT-STABLE
' TRY-DERIVE catch RC !
RC @ E-EVENT-TX T=
COMMITTED-EVENT-STABLE
' TRY-VARIANT catch RC !
RC @ E-EVENT-TX T=
COMMITTED-EVENT-STABLE
' TRY-END-VARIANT catch RC !
RC @ E-EVENT-TX T=
COMMITTED-EVENT-STABLE
' TRY-FIELD catch RC !
RC @ E-EVENT-TX T=
COMMITTED-EVENT-STABLE
' TRY-FIELD-SCHEMA catch RC !
RC @ E-EVENT-TX T=
COMMITTED-EVENT-STABLE
' TRY-PREPARE catch RC !
RC @ E-EVENT-TX T=
COMMITTED-EVENT-STABLE
' TRY-COMMIT catch RC !
RC @ E-EVENT-TX T=
COMMITTED-EVENT-STABLE
' TRY-PUBLISH catch RC !
RC @ E-EVENT-TX T=
COMMITTED-EVENT-STABLE

TOK @ DECL-EVENT:ROLLBACK
DECL-EVENT:COUNT BASE-EVENT @ T=
TYPE-FIELD:COUNT BASE-N @ T=
TF-STR-U@ BASE-STR @ T=
DECL-EVENT:DEPTH 0 T=

\ FINALIZE releases the retained frame while preserving its publication.
DECL-EVENT:COUNT BASE-EVENT !
TYPE-FIELD:COUNT BASE-N !
DECL-EVENT:OPEN TOK !
TOK @ FAM @ DECL-EVENT:DECL TOK !
TOK @ FAM @ s" event-finalize" SCH @
   6 1 6 cells CELL CELL 0 DECL-EVENT:FIELD TOK !
TOK @ DECL-EVENT:PREPARE
TOK @ DECL-EVENT:COMMIT
DECL-EVENT:COUNT BASE-EVENT @ 2 + T=
TYPE-FIELD:COUNT BASE-N @ 1 + T=
DECL-EVENT:DEPTH 1 T=
TOK @ DECL-EVENT:FINALIZE
DECL-EVENT:COUNT BASE-EVENT @ 2 + T=
TYPE-FIELD:COUNT BASE-N @ 1 + T=
DECL-EVENT:DEPTH 0 T=

INTERPRETER-ABSENCE
LOAD-ABSENCE
CHECKER-RETIRED
CHECKER-NEIGHBOURS-LIVE
CHECKER-OWNER-API
CHECKER-OWNER-ROLES
OWNER-API-RUNTIME-LIVE
CLEANUP-SEAM-ABSENT
RELEASE-SEAM-ABSENT

;package

TYPE-FIELD-OWNER-TEST:REOPEN-SEALED
TYPE-FIELD-OWNER-TEST:REOPEN-CLEANUP-SEALED
T-REPORT
s" type-field-owner-suite: ok" type cr
