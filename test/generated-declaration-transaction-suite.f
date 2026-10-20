\ generated-declaration-transaction-suite.f - atomic declaration publication.
\ Run: bin/hb --load test/generated-declaration-transaction-suite.f
\
\ Every case drives DECLARATION-TRANSACTION:RUN through real declarations and
\ real bodies, and fails it at one point: the body, the checker frame's prepare,
\ the constructor participant's commit, the generated-word evaluator, or a
\ declaration opened inside another, and once under a multi-error load that
\ continues past the refusal. Each asserts that every participant rolled back
\ to its savepoint.

require src/lib/test.f
require tools/prot-wid-probe.f
require test/checker-assert.f
require test/decl-diag-capture.f   \ DECL-DIAG: the check tool's own declaration-packet capture

using SCHEMA-REG
using TFAM
using TYPE-DECL

T-RESET

package GENERATED-DECLARATION-TXN-TEST

-7190 constant E-INJECT
-7192 constant E-BODY
70 constant E-GENERATED-REJECT
7161 constant E-DEV-TX            \ src/core/decl-event.f's, private there
7175 constant E-DICTIONARY-CAP    \ src/core/generated-declaration-dictionary.f's, private there

PTR-VARIABLE SOURCE-A
variable SOURCE-U
variable FAMILY-N0
variable VARIANT-N0
variable FIELD-N0
variable SCHEMA-N0
variable SCHEMA-ROOT-N0
variable LAYOUT-N0
variable STRING-N0
\ The observable for "the protection owner did not publish" is the size of the
\ protected SET, not prot-wid-room. Room used to count free registry slots, which a
\ rollback restored; it now counts free wordlist IDS, and a rolled-back declaration
\ has still consumed the ids it allocated -- ids are never recycled. Reading the set
\ separates the property under test (nothing was published) from an unrelated fact
\ about the allocator, which the old proxy silently conflated.
variable PROTECTION-ROOM-N0
variable GENERATED-N
variable GENERATED-NAME-PREFLIGHT-N
variable GENERATED-PLAN-BROKEN
variable GENERATED-XREF-ABSENT
variable GENERATED-CHECKER-ABSENT
variable GENERATED-VARIANT-STAGED
variable GENERATED-PROTECTION-UNPUBLISHED
variable GENERATED-DICTIONARY-UNPUBLISHED
variable NATIVE-NDICT-N0
variable NATIVE-CP-N0
PTR-VARIABLE NATIVE-DP-N0

variable CHECKER-DEPTH-N0
variable FIELD-DEPTH-N0
variable EVENT-DEPTH-N0
variable STAGED-N0
variable OUTER-REF-SLOT
variable OUTER-REF-OCC
variable FAILED-REF-SLOT
variable FAILED-REF-OCC
variable WID-N0
variable WID-N1
variable CAPACITY-PREFLIGHT-N
variable AFTER-FAIL-FAM
variable AFTER-FAIL-FIELD

\ A 253-byte package and a two-byte family derive a 256-byte constructor
\ namespace (package, '-', family), one byte beyond the package-name limit.
create CTOR-CAP-SRC 272 allot
variable CTOR-CAP-U

: CTOR-CAP-APP ( ptr u8 n -- ) {: a:ptr u:n :}
   a CTOR-CAP-SRC CTOR-CAP-U @ + u BYTE-COPY
   u CTOR-CAP-U +! ;

: CTOR-CAP-OPEN$ ( -- ptr u8 n )
   0 CTOR-CAP-U !
   s" package " CTOR-CAP-APP
   253 0 DO
      112 CTOR-CAP-SRC CTOR-CAP-U @ + c!
      1 CTOR-CAP-U +!
   LOOP
   s"  public" CTOR-CAP-APP
   CTOR-CAP-SRC CTOR-CAP-U @ ;

: CTOR-CAP-PKG$ ( -- ptr u8 n ) CTOR-CAP-SRC 8 + 253 ;

: BREAK-GENERATED-EFFECT ( ptr u8 n -- ) {: a:ptr u:n :}
   0 GENERATED-PLAN-BROKEN !
   a u + 1 + dup c@ 40 <> IF
      drop s" generated declaration test: signature opener moved" 76 die
   THEN
   63 swap c!
   -1 GENERATED-PLAN-BROKEN ! ;

: EVALUATE-GENERATED ( ptr u8 n -- )
   GENERATED-N @ 1 + GENERATED-N !
   INCLUDE-EVALUATE ;

: RECORD-PREFLIGHT-NONPUBLICATION ( -- )
   s" GDTX:RED" XREF-FIND XREF-FOUND? 0= GENERATED-XREF-ABSENT !
   s" GDTX:RED" CHECKER-RESOLVES? 0= GENERATED-CHECKER-ABSENT !
   \ The variants already exist when constructor generation runs, and the
   \ declaration transaction can still roll them back on a later failure.
   SUMV-N@ VARIANT-N0 @ > GENERATED-VARIANT-STAGED !
   PROT-WID-PROBE:COUNT PROTECTION-ROOM-N0 @ = GENERATED-PROTECTION-UNPUBLISHED !
   ndict@ NATIVE-NDICT-N0 @ =
   cp@ NATIVE-CP-N0 @ = and
   here NATIVE-DP-N0 @ = and GENERATED-DICTIONARY-UNPUBLISHED ! ;

: PREFLIGHT-GENERATED-NAME ( ptr u8 n -- ) {: a:ptr u:n :}
   GENERATED-NAME-PREFLIGHT-N @ 1 + GENERATED-NAME-PREFLIGHT-N !
   a u GENERATED-DECL-NAME-PREFLIGHT:CHECK
   GENERATED-NAME-PREFLIGHT-N @ 2 = IF
      RECORD-PREFLIGHT-NONPUBLICATION
      a u BREAK-GENERATED-EFFECT
   THEN ;

: INSTALL-SECOND-WORD-FAILURE ( -- )
   0 GENERATED-N !
   0 GENERATED-NAME-PREFLIGHT-N !
   0 GENERATED-PLAN-BROKEN !
   0 GENERATED-XREF-ABSENT !
   0 GENERATED-CHECKER-ABSENT !
   0 GENERATED-VARIANT-STAGED !
   0 GENERATED-PROTECTION-UNPUBLISHED !
   0 GENERATED-DICTIONARY-UNPUBLISHED !
   [: EVALUATE-GENERATED ;] is TYPE-DECL:TDECL-EVAL-XT
   [: PREFLIGHT-GENERATED-NAME ;] is TYPE-DECL:TDECL-NAME-PREFLIGHT-XT ;

: REJECT-GENERATED-NAME ( ptr u8 n -- )
   GENERATED-NAME-PREFLIGHT-N @ 1 + GENERATED-NAME-PREFLIGHT-N !
   GENERATED-DECL-NAME-PREFLIGHT:CHECK
   E-INJECT throw ;

: INSTALL-NAME-PREFLIGHT-FAILURE ( -- )
   0 GENERATED-N !
   0 GENERATED-NAME-PREFLIGHT-N !
   [: EVALUATE-GENERATED ;] is TYPE-DECL:TDECL-EVAL-XT
   [: REJECT-GENERATED-NAME ;] is TYPE-DECL:TDECL-NAME-PREFLIGHT-XT ;

: FIRST-GENERATED-WORD-U ( ptr u8 n -- n ) {: a:ptr u:n :}
   0
   BEGIN dup u < WHILE
      a over + c@ 10 = IF 1 + EXIT THEN
      1 +
   REPEAT
   drop u ;

: EVALUATE-FIRST-GENERATED-WORD-THEN-FAIL ( ptr u8 n -- ) {: a:ptr u:n :}
   GENERATED-N @ 1 + GENERATED-N !
   a u FIRST-GENERATED-WORD-U {: first-u:n :}
   a first-u INCLUDE-EVALUATE
   E-INJECT throw ;

: INSTALL-EVALUATOR-FAILURE ( -- )
   0 GENERATED-N !
   0 GENERATED-NAME-PREFLIGHT-N !
   [: EVALUATE-FIRST-GENERATED-WORD-THEN-FAIL ;] is TYPE-DECL:TDECL-EVAL-XT
   [: GENERATED-DECL-NAME-PREFLIGHT:CHECK ;] is TYPE-DECL:TDECL-NAME-PREFLIGHT-XT ;

: RESTORE-GENERATED-EVALUATOR ( -- )
   [: INCLUDE-EVALUATE ;] is TYPE-DECL:TDECL-EVAL-XT
   [: GENERATED-DECL-NAME-PREFLIGHT:CHECK ;] is TYPE-DECL:TDECL-NAME-PREFLIGHT-XT ;

: EVALUATE-SOURCE ( -- )
   SOURCE-A @ SOURCE-U @ INCLUDE-EVALUATE ;

: EVALUATE-CATCH ( ptr u8 n -- n )
   SOURCE-U ! SOURCE-A !
   [: EVALUATE-SOURCE ;] catch ;

: WORD-ABSENT? ( ptr u8 n -- bool )
   0 search-wl 0= ;

: QUALIFIED-ABSENT? ( ptr u8 n -- bool )
   XREF-FIND XREF-FOUND? 0= ;

: ASSERT-NO-STALE-CONSTRUCTOR ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 0 T= ;

: TRY-EVENT-CURRENT ( -- )
   DECL-EVENT:CURRENT drop ;

: RECORD-BASELINE ( -- )
   TFAM-N@ FAMILY-N0 !
   SUMV-N@ VARIANT-N0 !
   TYPE-FIELD:COUNT FIELD-N0 !
   SCHEMA-N@ SCHEMA-N0 !
   SCHEMA-ROOT-N@ SCHEMA-ROOT-N0 !
   LAY-N@ LAYOUT-N0 !
   TF-STR-U@ STRING-N0 !
   PROT-WID-PROBE:COUNT PROTECTION-ROOM-N0 ! ;

: RECORD-OWNER-DEPTHS ( -- )
   CHECKER-SCOPE-DEPTH CHECKER-DEPTH-N0 !
   TYPE-FIELD:TX-DEPTH FIELD-DEPTH-N0 !
   DECL-EVENT:DEPTH EVENT-DEPTH-N0 !
   GENERATED-DECL-PROTECTION:STAGED-COUNT STAGED-N0 ! ;

: RECORD-NATIVE-HIGHWATERS ( -- )
   ndict@ NATIVE-NDICT-N0 !
   cp@ NATIVE-CP-N0 !
   here NATIVE-DP-N0 ! ;

: WID-NEXT ( -- n )
   data-base WIDN-CELL + @ ;

: ASSERT-NATIVE-HIGHWATERS ( -- )
   ndict@ NATIVE-NDICT-N0 @ T=
   cp@ NATIVE-CP-N0 @ T=
   here NATIVE-DP-N0 @ = TTRUE ;

: ASSERT-PRODUCTION-IDLE ( -- )
   DECLARATION-TRANSACTION:ACTIVE? TFALSE ;

: ASSERT-OWNER-DEPTHS ( -- )
   CHECKER-SCOPE-DEPTH CHECKER-DEPTH-N0 @ T=
   TYPE-FIELD:TX-DEPTH FIELD-DEPTH-N0 @ T=
   DECL-EVENT:DEPTH EVENT-DEPTH-N0 @ T=
   GENERATED-DECL-PROTECTION:STAGED-COUNT STAGED-N0 @ T=
   ASSERT-PRODUCTION-IDLE ;

: ASSERT-UNCHANGED ( -- )
   TFAM-N@ FAMILY-N0 @ T=
   SUMV-N@ VARIANT-N0 @ T=
   TYPE-FIELD:COUNT FIELD-N0 @ T=
   SCHEMA-N@ SCHEMA-N0 @ T=
   SCHEMA-ROOT-N@ SCHEMA-ROOT-N0 @ T=
   LAY-N@ LAYOUT-N0 @ T=
   TF-STR-U@ STRING-N0 @ T=
   PROT-WID-PROBE:COUNT PROTECTION-ROOM-N0 @ T= ;

\ ---- a body failure rolls the dictionary and data space back ----------------

: EVALUATE-DATA-WORD ( -- )
   s" package gddata public variable CELL ;package" INCLUDE-EVALUATE ;

: DATA-FAIL-BODY ( -- )
   EVALUATE-DATA-WORD
   E-BODY throw ;

: RUN-DATA-FAIL ( -- )
   [: DATA-FAIL-BODY ;] DECLARATION-TRANSACTION:RUN ;

: RUN-DATA-FAIL-CATCH ( -- n )
   [: RUN-DATA-FAIL ;] catch ;

: TEST-DATA-ROLLBACK ( -- )
   RECORD-OWNER-DEPTHS RECORD-NATIVE-HIGHWATERS
   RUN-DATA-FAIL-CATCH E-BODY T=
   ASSERT-OWNER-DEPTHS ASSERT-NATIVE-HIGHWATERS
   s" GDDATA:CELL" QUALIFIED-ABSENT? TTRUE ;

\ ---- a declaration inside an ordinary checker scope keeps that scope --------

: TEST-OUTER-CHECKER-SCOPE ( -- )
   RECORD-OWNER-DEPTHS
   CHECKER-SCOPE-START
   CHECKER-SCOPE-DEPTH CHECKER-DEPTH-N0 @ 1 + T=
   s" STRUCTURE gdouterscope FIELD x n ;STRUCTURE" EVALUATE-CATCH 0 T=
   CHECKER-SCOPE-DEPTH CHECKER-DEPTH-N0 @ 1 + T=
   s" GDOUTERSCOPE-ROUNDTRIP ( n -- n ) GDOUTERSCOPE:MAKE GDOUTERSCOPE:UNMAKE"
      CHECK-QUIET-CANDIDATE! -1 T=
   CHECKER-SCOPE-DONE
   ASSERT-OWNER-DEPTHS ;

\ ---- the checker frame's prepare refuses a body that leaked a scope ---------

: EVALUATE-CHECKER-LEAK-WORD ( -- )
   s" package gdcheckerleak public : MARK ( -- n ) 1 ; ;package" INCLUDE-EVALUATE ;

: CHECKER-LEAK-BODY ( -- )
   EVALUATE-CHECKER-LEAK-WORD
   CHECKER-SCOPE-START ;

: RUN-CHECKER-LEAK-BODY ( -- )
   [: CHECKER-LEAK-BODY ;] DECLARATION-TRANSACTION:RUN ;

: RUN-CHECKER-LEAK ( -- n )
   [: RUN-CHECKER-LEAK-BODY ;] catch ;

: TEST-CHECKER-LEAK-PREPARE ( -- )
   RECORD-BASELINE RECORD-OWNER-DEPTHS RECORD-NATIVE-HIGHWATERS
   CHECKER-SCOPE-START
   CHECKER-SCOPE-DEPTH CHECKER-DEPTH-N0 @ 1 + T=
   RUN-CHECKER-LEAK DECLARATION-TRANSACTION:E-FRAME T=
   CHECKER-SCOPE-DEPTH CHECKER-DEPTH-N0 @ 1 + T=
   ASSERT-UNCHANGED ASSERT-NATIVE-HIGHWATERS
   s" GDCHECKERLEAK:MARK" QUALIFIED-ABSENT? TTRUE
   CHECKER-SCOPE-DONE
   ASSERT-OWNER-DEPTHS ;

\ Frame records are reused as the rollback stack rises and falls, so the owner
\ mark must be rewritten on every push, not just when a declaration claims the
\ frame. This test seeds one slot with a finished declaration's mark and then
\ makes an ordinary scope land on that exact slot inside a later declaration:
\ two scopes, one successful declaration (its frame takes the third slot and is
\ released), one scope closed, then a declaration whose body leaks a scope onto
\ the seeded slot. A push that failed to reset the mark would leave the
\ declaration mark in place and PREPARE would accept the leaked scope as the
\ declaration's own frame.
: EVALUATE-RECYCLE-SEED-WORD ( -- )
   s" package gdrecycleseed public : MARK ( -- n ) 1 ; ;package" INCLUDE-EVALUATE ;

: RUN-RECYCLE-SEED ( -- )
   [: EVALUATE-RECYCLE-SEED-WORD ;] DECLARATION-TRANSACTION:RUN ;

: EVALUATE-RECYCLE-LEAK-WORD ( -- )
   s" package gdrecycleleak public : MARK ( -- n ) 1 ; ;package" INCLUDE-EVALUATE ;

: RECYCLE-LEAK-BODY ( -- )
   EVALUATE-RECYCLE-LEAK-WORD
   CHECKER-SCOPE-START ;

: RUN-RECYCLE-LEAK-BODY ( -- )
   [: RECYCLE-LEAK-BODY ;] DECLARATION-TRANSACTION:RUN ;

: RUN-RECYCLE-LEAK ( -- n )
   [: RUN-RECYCLE-LEAK-BODY ;] catch ;

: TEST-RECYCLED-SLOT-LEAK ( -- )
   RECORD-OWNER-DEPTHS RECORD-NATIVE-HIGHWATERS
   CHECKER-SCOPE-START CHECKER-SCOPE-START
   CHECKER-SCOPE-DEPTH CHECKER-DEPTH-N0 @ 2 + T=
   RUN-RECYCLE-SEED
   CHECKER-SCOPE-DEPTH CHECKER-DEPTH-N0 @ 2 + T=
   CHECKER-SCOPE-DONE
   RUN-RECYCLE-LEAK DECLARATION-TRANSACTION:E-FRAME T=
   CHECKER-SCOPE-DEPTH CHECKER-DEPTH-N0 @ 1 + T=
   s" GDRECYCLELEAK:MARK" QUALIFIED-ABSENT? TTRUE
   s" GDRECYCLESEED:MARK" QUALIFIED-ABSENT? 0= TTRUE
   CHECKER-SCOPE-DONE
   ASSERT-OWNER-DEPTHS ;

\ ---- a rolled-back word's references go stale, and its slot is reused ------

: EVALUATE-OUTER-WORD ( -- )
   s" package gdrow public : MARK ( -- n ) 1 ; ;package" INCLUDE-EVALUATE ;

: OUTER-BODY ( -- )
   EVALUATE-OUTER-WORD
   s" GDROW:MARK" XREF-FIND DEF-OCC:SELECT
   OUTER-REF-OCC ! OUTER-REF-SLOT ! ;

: OUTER-FAIL-BODY ( -- )
   OUTER-BODY
   E-BODY throw ;

: RUN-OUTER-FAIL-BODY ( -- )
   [: OUTER-FAIL-BODY ;] DECLARATION-TRANSACTION:RUN ;

: RUN-OUTER-FAIL ( -- n )
   [: RUN-OUTER-FAIL-BODY ;] catch ;

: TEST-DECLARATION-ROLLBACK ( -- )
   RECORD-BASELINE RECORD-OWNER-DEPTHS RECORD-NATIVE-HIGHWATERS
   WID-NEXT WID-N0 !
   CHECKER-SCOPE-START
   CHECKER-SCOPE-DEPTH CHECKER-DEPTH-N0 @ 1 + T=
   RUN-OUTER-FAIL E-BODY T=
   ASSERT-UNCHANGED
   CHECKER-SCOPE-DEPTH CHECKER-DEPTH-N0 @ 1 + T=
   ASSERT-NATIVE-HIGHWATERS
   WID-NEXT dup WID-N1 ! WID-N0 @ > TTRUE
   s" GDROW:MARK" QUALIFIED-ABSENT? TTRUE
   [: OUTER-REF-SLOT @ OUTER-REF-OCC @ DEF-OCC:RESOLVE drop ;]
      DEF-OCC:E-STALE TTHROWSQ
   OUTER-REF-SLOT @ FAILED-REF-SLOT !
   OUTER-REF-OCC @ FAILED-REF-OCC !

   [: OUTER-BODY ;] DECLARATION-TRANSACTION:RUN
   WID-NEXT WID-N1 @ > TTRUE
   s" GDROW:MARK" QUALIFIED-ABSENT? 0= TTRUE
   OUTER-REF-SLOT @ FAILED-REF-SLOT @ T=
   OUTER-REF-OCC @ FAILED-REF-OCC @ T<>
   [: FAILED-REF-SLOT @ FAILED-REF-OCC @ DEF-OCC:RESOLVE drop ;]
      DEF-OCC:E-STALE TTHROWSQ
   OUTER-REF-SLOT @ OUTER-REF-OCC @ DEF-OCC:RESOLVE XREF-START 0<> TTRUE
   s" GDROW:MARK 1 T=" INCLUDE-EVALUATE
   CHECKER-SCOPE-DEPTH CHECKER-DEPTH-N0 @ 1 + T=
   CHECKER-SCOPE-DONE
   ASSERT-OWNER-DEPTHS ;

\ ---- a declaration opened inside another is refused, and the outer rolls back

: RUN-NESTED-EMPTY ( -- )
   [: [: ;] DECLARATION-TRANSACTION:RUN ;] DECLARATION-TRANSACTION:RUN ;

: RUN-NESTED-EMPTY-CATCH ( -- n )
   [: RUN-NESTED-EMPTY ;] catch ;

: EVALUATE-NESTED-DECLARATION ( -- )
   s" STRUCTURE gdnested FIELD x n ;STRUCTURE" INCLUDE-EVALUATE ;

: RUN-NESTED-DECLARATION ( -- )
   [: EVALUATE-NESTED-DECLARATION ;] DECLARATION-TRANSACTION:RUN ;

: RUN-NESTED-DECLARATION-CATCH ( -- n )
   [: RUN-NESTED-DECLARATION ;] catch ;

: TEST-NESTED-REFUSED ( -- )
   RECORD-BASELINE RECORD-OWNER-DEPTHS RECORD-NATIVE-HIGHWATERS
   RUN-NESTED-EMPTY-CATCH DECLARATION-TRANSACTION:E-ACTIVE T=
   ASSERT-UNCHANGED ASSERT-OWNER-DEPTHS ASSERT-NATIVE-HIGHWATERS
   \ The front end is refused before it reads the family name.
   DECL-DIAG:PROSE
   RUN-NESTED-DECLARATION-CATCH DECLARATION-TRANSACTION:E-ACTIVE T=
   s" habu: bad structure declaration '': a declaration is already open"
   DECL-DIAG:HAS? TTRUE
   DECL-DIAG:OFF
   ASSERT-UNCHANGED ASSERT-OWNER-DEPTHS ASSERT-NATIVE-HIGHWATERS
   s" GDNESTED:MAKE" QUALIFIED-ABSENT? TTRUE ;

\ ---- generation failures clear every provisional authority -----------------

: TEST-NAME-PREFLIGHT-CLEARS-AUTHORITY ( -- )
   RECORD-BASELINE RECORD-OWNER-DEPTHS RECORD-NATIVE-HIGHWATERS
   INSTALL-NAME-PREFLIGHT-FAILURE
   \ The vehicle is the global ENUM keyword, which is the unified front end. Its
   \ rejects are rendered as a declaration packet before they are rethrown, so the
   \ expected diagnostic is captured and asserted rather than left on stderr — the
   \ legacy compact definer printed nothing for a generation-phase failure, since
   \ its constructor step ran outside the body its reporter guarded.
   DECL-DIAG:PROSE
   s" ENUM gdnamefail left right ;ENUM" EVALUATE-CATCH E-INJECT T=
   s" habu: bad enum declaration 'gdnamefail': declaration failed at 'gdnamefail'"
   DECL-DIAG:HAS? TTRUE
   DECL-DIAG:OFF
   RESTORE-GENERATED-EVALUATOR
   GENERATED-N @ 0 T=
   GENERATED-NAME-PREFLIGHT-N @ 1 T=
   s" GDNAMEFAIL:LEFT ( -- ) 0" ASSERT-NO-STALE-CONSTRUCTOR
   s" GDNAMEFAIL:LEFT" QUALIFIED-ABSENT? TTRUE
   ASSERT-UNCHANGED ASSERT-OWNER-DEPTHS ASSERT-NATIVE-HIGHWATERS ;

: TEST-GENERATOR-ROLLBACK ( -- )
   RECORD-BASELINE RECORD-OWNER-DEPTHS
   RECORD-NATIVE-HIGHWATERS
   INSTALL-SECOND-WORD-FAILURE
   DECL-DIAG:PROSE
   s" ENUM gdtx red blue ;ENUM" EVALUATE-CATCH
      E-GENERATED-REJECT T=
   s" habu: bad enum declaration 'gdtx': declaration failed at 'gdtx'"
   DECL-DIAG:HAS? TTRUE
   DECL-DIAG:OFF
   RESTORE-GENERATED-EVALUATOR
   GENERATED-N @ 0 T=
   GENERATED-NAME-PREFLIGHT-N @ 2 T=
   s" plan broken at the second word" T-LABEL GENERATED-PLAN-BROKEN @ TTRUE
   s" xref absent when generation ran" T-LABEL GENERATED-XREF-ABSENT @ TTRUE
   s" checker row absent when generation ran" T-LABEL GENERATED-CHECKER-ABSENT @ TTRUE
   s" variants staged when generation ran" T-LABEL GENERATED-VARIANT-STAGED @ TTRUE
   s" protection unpublished when generation ran" T-LABEL
   GENERATED-PROTECTION-UNPUBLISHED @ TTRUE
   s" dictionary unpublished when generation ran" T-LABEL
   GENERATED-DICTIONARY-UNPUBLISHED @ TTRUE
   ASSERT-UNCHANGED
   ASSERT-OWNER-DEPTHS
   ASSERT-NATIVE-HIGHWATERS
   s" GDTX:RED" WORD-ABSENT? TTRUE
   s" GDTX:RED" QUALIFIED-ABSENT? TTRUE
   s" GDTX:BLUE" QUALIFIED-ABSENT? TTRUE
   s" GDTX:RED" CHECKER-RESOLVES? 0= TTRUE
   s" GDTX:BLUE" CHECKER-RESOLVES? 0= TTRUE
   s" GDTX:RED ( -- ) 0" ASSERT-NO-STALE-CONSTRUCTOR
   s" package gdtx ;package" EVALUATE-CATCH 0 T=
   ASSERT-PRODUCTION-IDLE ;

: TEST-EVALUATOR-FAILURE-CLEARS-AUTHORITY ( -- )
   RECORD-BASELINE RECORD-OWNER-DEPTHS RECORD-NATIVE-HIGHWATERS
   INSTALL-EVALUATOR-FAILURE
   \ PRODUCT generates inside the body its reporter guards, as ENUM does, so the
   \ generation fault is rendered at the family name before it is rethrown.
   DECL-DIAG:PROSE
   s" PRODUCT gdevalfail FIELD value n ;PRODUCT" EVALUATE-CATCH E-INJECT T=
   s" habu: bad product declaration 'gdevalfail': declaration failed at 'gdevalfail'"
   DECL-DIAG:HAS? TTRUE
   DECL-DIAG:OFF
   RESTORE-GENERATED-EVALUATOR
   GENERATED-N @ 1 T=
   s" GDEVALFAIL:MAKE" QUALIFIED-ABSENT? TTRUE
   s" GDEVALFAIL:UNMAKE" QUALIFIED-ABSENT? TTRUE
   s" GDEVALFAIL-MAKE ( n -- gdevalfail ) GDEVALFAIL:MAKE"
      ASSERT-NO-STALE-CONSTRUCTOR
   ASSERT-UNCHANGED ASSERT-OWNER-DEPTHS ASSERT-NATIVE-HIGHWATERS
   [: TRY-EVENT-CURRENT ;] catch E-DEV-TX T=

   \ The next real declaration consumes the exact rolled-back field id.
   s" PRODUCT gdafterfail FIELD clean n ;PRODUCT" EVALUATE-CATCH 0 T=
   TYPE-FIELD:COUNT FIELD-N0 @ 1 + T=
   s" gdafterfail" TFAM-ACTIVE-PKG$ 2swap TFAM-SIG-RESOLVE drop AFTER-FAIL-FAM !
   FIELD-N0 @ AFTER-FAIL-FIELD !
   AFTER-FAIL-FIELD @ TYPE-FIELD:FAMILY@ AFTER-FAIL-FAM @ T=
   AFTER-FAIL-FIELD @ TYPE-FIELD:VARIANT@ TYPE-FIELD:NO-VARIANT T=
   AFTER-FAIL-FIELD @ TYPE-FIELD:NAME$ s" clean" T$=
   AFTER-FAIL-FIELD @ TYPE-FIELD:SCHEMA@ SCHEMA-ROOT-N0 @ T=
   AFTER-FAIL-FIELD @ TYPE-FIELD:SLOT@ 0 T=
   AFTER-FAIL-FIELD @ TYPE-FIELD:CELLS@ 1 T=
   AFTER-FAIL-FIELD @ TYPE-FIELD:BYTE-OFF@ 0 T=
   AFTER-FAIL-FIELD @ TYPE-FIELD:BYTES@ CELL T=
   AFTER-FAIL-FIELD @ TYPE-FIELD:ALIGN@ CELL T=
   AFTER-FAIL-FIELD @ TYPE-FIELD:FLAGS@ 0 T=
   ASSERT-OWNER-DEPTHS ;

\ ---- a multi-error load reports a refused product and continues ------------
\ The path the all-errors check drives (tools/check-all-errors-core.f SESSION):
\ the reject is counted and swallowed only after its transaction rolled back,
\ so the next product consumes the refused one's field id.

: MULTI-ERROR-PRODUCTS ( -- )
   s" gdmebad" s" FIELD x n FIELD x n" CHECKER-DEFPRODUCT
   s" gdmegood" s" FIELD clean n" CHECKER-DEFPRODUCT ;

: TEST-MULTI-ERROR-ROLLBACK ( -- )
   RECORD-BASELINE RECORD-OWNER-DEPTHS RECORD-NATIVE-HIGHWATERS
   DECL-DIAG:PROSE
   MULTI-ERR-BEGIN
   [: MULTI-ERROR-PRODUCTS ;] catch {: rc:n :}
   MULTI-ERR-END {: rejects:n :}
   rc 0 T=
   rejects 1 T=
   s" habu: bad product declaration 'gdmebad': duplicate field at 'x'"
   DECL-DIAG:HAS? TTRUE
   DECL-DIAG:OFF
   TFAM-N@ FAMILY-N0 @ 1 + T=
   TYPE-FIELD:COUNT FIELD-N0 @ 1 + T=
   FIELD-N0 @ TYPE-FIELD:NAME$ s" clean" T$=
   [: TRY-EVENT-CURRENT ;] catch E-DEV-TX T=
   ASSERT-OWNER-DEPTHS ASSERT-NATIVE-HIGHWATERS ;

: TEST-CTOR-PACKAGE-CAP ( -- )
   s" PRODUCT xy FIELD value n ;PRODUCT" {: src:ptr su:n :}
   CTOR-CAP-OPEN$ EVALUATE-CATCH 0 T=
   RECORD-BASELINE RECORD-OWNER-DEPTHS RECORD-NATIVE-HIGHWATERS

   \ PRODUCT uses the legacy declaration reporter after recording a field.
   DECL-DIAG:JSON
   src su 1 1 0 DIAG-SOURCE!
   src su EVALUATE-CATCH E-PACKAGE-NAME-CAP T=
   s\" \"decl\":\"product\",\"family\":\"xy\",\"token\":\"xy\",\"reason\":\"constructor package name exceeds 255 bytes\""
   DECL-DIAG:HAS? TTRUE
   s\" \"line\":1,\"column\":9,\"byte_start\":8,\"byte_end\":10"
   DECL-DIAG:HAS? TTRUE
   DIAG-SOURCE-OFF
   DECL-DIAG:OFF
   ASSERT-UNCHANGED ASSERT-OWNER-DEPTHS ASSERT-NATIVE-HIGHWATERS
   CTOR-CAP-PKG$ s" xy" TFAM-FIND-IN nip TFALSE

   \ ENUM reaches the same limit from its unified constructor participant.
   DECL-DIAG:PROSE
   s" ENUM xy red ;ENUM" EVALUATE-CATCH E-PACKAGE-NAME-CAP T=
   s" bad enum declaration 'xy': constructor package name exceeds 255 bytes at 'xy'"
   DECL-DIAG:HAS? TTRUE
   DECL-DIAG:OFF
   ASSERT-UNCHANGED ASSERT-OWNER-DEPTHS ASSERT-NATIVE-HIGHWATERS
   CTOR-CAP-PKG$ s" xy" TFAM-FIND-IN nip TFALSE
   s" ;package" EVALUATE-CATCH 0 T= ;

\ Test-only friend boundary for the installed generated-plan capacity hook. A
\ plain checked body since this suite became a WHITEBOX-SUITE: the hook's name
\ carries its ordinary effect on an engine whose seal never ran.
: RUN-CAPACITY-PREFLIGHT ( -- )
   s" GDOUTERSCOPE:MAKE" CAPACITY-PREFLIGHT-N @ TYPE-DECL:TDECL-CAPACITY-PREFLIGHT-XT ;

: CAPACITY-PREFLIGHT-CATCH ( n -- n )
   CAPACITY-PREFLIGHT-N !
   [: RUN-CAPACITY-PREFLIGHT ;] catch ;

: TEST-PLAN-CAPACITY-PREFLIGHT ( -- )
   s" GDOUTERSCOPE:MAKE" 2 GENERATED-DECL-NAME-PREFLIGHT:DICTIONARY-RECORDS 2 T=
   s" GDCAP-MISSING:MAKE" 2 GENERATED-DECL-NAME-PREFLIGHT:DICTIONARY-RECORDS 3 T=
   s" GDOUTERSCOPE:MAKE" GENERATED-DECL-NAME-PREFLIGHT:NEW-WORDLIST? 0= TTRUE
   s" GDCAP-MISSING:MAKE" GENERATED-DECL-NAME-PREFLIGHT:NEW-WORDLIST? TTRUE
   DICT-CAP ndict@ - {: room:n :}
   RECORD-NATIVE-HIGHWATERS
   room CAPACITY-PREFLIGHT-CATCH 0 T=
   room 1 + CAPACITY-PREFLIGHT-CATCH E-DICTIONARY-CAP T=
   ASSERT-NATIVE-HIGHWATERS
   ASSERT-PRODUCTION-IDLE ;

: TEST-SUCCESS-PUBLICATION ( -- )
   PROT-WID-PROBE:COUNT PROTECTION-ROOM-N0 !
   s" PRODUCT gdok FIELD x n ;PRODUCT" EVALUATE-CATCH 0 T=
   PROT-WID-PROBE:COUNT PROTECTION-ROOM-N0 @ 1 + T=
   s" GDOK-USE ( n -- gdok ) GDOK:MAKE"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" package gdok ;package" EVALUATE-CATCH 7111 T=
   ASSERT-PRODUCTION-IDLE ;

\ The runner holds the only live capability onto each participant's steps and
\ the checker's private declaration-frame words: no public seam reaches them.
: TEST-CLOSED-WORLD ( -- )
   s" CHECKER-DECL-FRAME:PART-ROLLBACK" QUALIFIED-ABSENT? TTRUE
   s" DECL-EVENT:PART-RELEASE" QUALIFIED-ABSENT? TTRUE
   s" GENERATED-DECL-DICTIONARY:ROLLBACK" QUALIFIED-ABSENT? TTRUE
   s" GENERATED-DECL-PROTECTION:PUBLISH" QUALIFIED-ABSENT? TTRUE
   s" CHECKER-DECL-FRAME:RELEASE" QUALIFIED-ABSENT? TTRUE
   s" CHECKER-DECL-FRAME:RELEASE" CHECKER-RESOLVES? 0= TTRUE ;

public

: RUN ( -- )
   TEST-OUTER-CHECKER-SCOPE
   TEST-CHECKER-LEAK-PREPARE
   TEST-RECYCLED-SLOT-LEAK
   TEST-DECLARATION-ROLLBACK
   TEST-NESTED-REFUSED
   TEST-DATA-ROLLBACK
   TEST-NAME-PREFLIGHT-CLEARS-AUTHORITY
   TEST-GENERATOR-ROLLBACK
   TEST-EVALUATOR-FAILURE-CLEARS-AUTHORITY
   TEST-MULTI-ERROR-ROLLBACK
   TEST-CTOR-PACKAGE-CAP
   TEST-PLAN-CAPACITY-PREFLIGHT
   TEST-SUCCESS-PUBLICATION
   TEST-CLOSED-WORLD
   T-REPORT ;

;package

GENERATED-DECLARATION-TXN-TEST:RUN

;using
;using
;using
