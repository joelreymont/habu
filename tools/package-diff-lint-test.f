\ package-diff-lint-test.f - hostile exact-diff package ownership fixtures.
\ Run: bin/hb --load tools/package-diff-lint-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require lib/fs-mutate.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/source-lex.f
require tools/package-diff-lint-core.f

package PACKAGE-DIFF-TEST
private

$4000 constant SOURCE-CAP
$8000 constant DIFF-CAP
$4000 constant OUT-CAP
10 constant LF-C
34 constant DQUOTE-C

create ROOT-BUF FS-PATH-CAP allot
create PATH-BUF FS-PATH-CAP allot
create SOURCE-BUF SOURCE-CAP allot
create DIFF-BUF DIFF-CAP allot
create OUT-BUF OUT-CAP allot

variable ROOT-U
variable PATH-U
variable SOURCE-U
variable SOURCE-START
variable DIFF-U

: COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr up:ptr :}
   a dst u BYTE-COPY
   u up ! ;

: ROOT$ ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ ;

: SOURCE$ ( -- ptr u8 n )
   SOURCE-BUF SOURCE-U @ ;

: SOURCE-RESET ( -- )
   0 SOURCE-U ! ;

: SOURCE+ ( ptr u8 n -- ) {: a:ptr u:n :}
   SOURCE-U @ u + SOURCE-CAP > if E-FS-CAPACITY throw then
   a SOURCE-BUF SOURCE-U @ + u BYTE-COPY
   SOURCE-U @ u + SOURCE-U ! ;

: SOURCE-C ( n -- ) {: c:n :}
   SOURCE-U @ SOURCE-CAP >= if E-FS-CAPACITY throw then
   c SOURCE-BUF SOURCE-U @ + c!
   SOURCE-U @ 1+ SOURCE-U ! ;

: SOURCE-LINE ( ptr u8 n -- )
   SOURCE+ LF-C SOURCE-C ;

: SOURCE-STRING-LINE ( -- )
   s" : TEXT ( -- ) s" SOURCE+
   DQUOTE-C SOURCE-C
   s" : FORGED package FORGED ;package" SOURCE+
   DQUOTE-C SOURCE-C
   s"  drop ;" SOURCE-LINE ;

: DIFF$ ( -- ptr u8 n )
   DIFF-BUF DIFF-U @ ;

: DIFF-RESET ( -- )
   0 DIFF-U ! ;

: DIFF+ ( ptr u8 n -- ) {: a:ptr u:n :}
   DIFF-U @ u + DIFF-CAP > if E-FS-CAPACITY throw then
   a DIFF-BUF DIFF-U @ + u BYTE-COPY
   DIFF-U @ u + DIFF-U ! ;

: DIFF-C ( n -- ) {: c:n :}
   DIFF-U @ DIFF-CAP >= if E-FS-CAPACITY throw then
   c DIFF-BUF DIFF-U @ + c!
   DIFF-U @ 1+ DIFF-U ! ;

: FULL-PATH ( ptr u8 n -- ptr u8 n ) {: path:ptr pathu:n :}
   ROOT$ path pathu PATH-BUF JOIN-PATH PATH-U !
   PATH-BUF PATH-U @ ;

: WRITE-SOURCE ( ptr u8 n -- )
   FULL-PATH SOURCE$ WRITE-ALL ;

: LF ( -- )
   LF-C DIFF-C ;

: U+ ( n -- ) {: u:n :}
   u 9 > if u 10 / RECURSE then
   u 10 mod 48 + DIFF-C ;

: DIFF-HEAD ( ptr u8 n -- ) {: path:ptr pathu:n :}
   s" diff --git a/" DIFF+ path pathu DIFF+
   s"  b/" DIFF+ path pathu DIFF+ LF ;

: ADD-HEAD ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu DIFF-HEAD
   s" new file mode 100644" DIFF+ LF
   s" index 0000000..abcdef0" DIFF+ LF
   s" --- /dev/null" DIFF+ LF
   s" +++ b/" DIFF+ path pathu DIFF+ LF ;

: MODIFY-HEAD ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu DIFF-HEAD
   s" index 1234567..abcdef0 100644" DIFF+ LF
   s" --- a/" DIFF+ path pathu DIFF+ LF
   s" +++ b/" DIFF+ path pathu DIFF+ LF ;

: ADDED-SOURCE-LINES ( -- )
   0 SOURCE-START !
   0 begin dup SOURCE-U @ < while
      dup SOURCE-BUF + c@ LF-C = if
         43 DIFF-C
         SOURCE-BUF SOURCE-START @ + over SOURCE-START @ - DIFF+ LF
         dup 1+ SOURCE-START !
      then
      1+
   repeat drop ;

: ADD-SOURCE-SECTION ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu WRITE-SOURCE
   path pathu ADD-HEAD
   s" @@ -0,0 +1," DIFF+
   SOURCE$ LF-C LINT-COUNT-CHAR U+
   s"  @@" DIFF+ LF
   ADDED-SOURCE-LINES ;

: RUN-BUILT ( -- n n )
   PACKAGE-DIFF:RESET
   ROOT$ PACKAGE-DIFF:ROOT!
   OUT-BUF OUT-CAP LINT-OUT-BUFFER!
   [: DIFF$ PACKAGE-DIFF:SOURCE PACKAGE-DIFF:FINISH ;] catch {: rc:n :}
   PACKAGE-DIFF:FINDINGS {: bad:n :}
   LINT-OUT-BUFFER-OFF
   rc bad ;

: EXPECT-CLEAN ( -- )
   RUN-BUILT swap 0 T= 0 T= ;

: EXPECT-FINDINGS ( n -- ) {: want:n :}
   RUN-BUILT {: rc:n bad:n :}
   rc 1 T=
   bad want T= ;

: PREPARE ( -- )
   CLEANUP-RESET
   s" habu-package-diff-lint" TMPDIR-MKDIR ROOT-BUF ROOT-U COPY!
   ROOT$ CLEANUP-TREE+
   ROOT$ s" lib" PATH-BUF JOIN-PATH PATH-U ! PATH-BUF PATH-U @ MAKE-DIRS
   ROOT$ s" lib/type" PATH-BUF JOIN-PATH PATH-U ! PATH-BUF PATH-U @ MAKE-DIRS
   ROOT$ s" tools" PATH-BUF JOIN-PATH PATH-U ! PATH-BUF PATH-U @ MAKE-DIRS
   ROOT$ s" test" PATH-BUF JOIN-PATH PATH-U ! PATH-BUF PATH-U @ MAKE-DIRS
   ROOT$ s" src/core" PATH-BUF JOIN-PATH PATH-U ! PATH-BUF PATH-U @ MAKE-DIRS ;

: GLOBAL-SOURCE ( ptr u8 n -- )
   SOURCE-RESET
   s" : " SOURCE+ SOURCE+ s"  ( -- n ) 1 ;" SOURCE-LINE ;

: TEST-GLOBAL-OWNERS ( -- )
   DIFF-RESET
   s" LIB-LEAK" GLOBAL-SOURCE s" lib/global.f" ADD-SOURCE-SECTION
   s" TOOL-LEAK" GLOBAL-SOURCE s" tools/global.f" ADD-SOURCE-SECTION
   s" TEST-LEAK" GLOBAL-SOURCE s" test/global.f" ADD-SOURCE-SECTION
   3 EXPECT-FINDINGS ;

: TEST-CASE-CLOSE-REOPEN ( -- )
   SOURCE-RESET
   s" PaCkAgE MOD" SOURCE-LINE
   s" : OK ( -- n ) 1 ;" SOURCE-LINE
   s" ;PaCkAgE" SOURCE-LINE
   s" cHeCkEd: LEAK ( -- n ) 2 ;" SOURCE-LINE
   s" package MOD" SOURCE-LINE
   s" : OK2 ( -- n ) 3 ;" SOURCE-LINE
   s" ;package" SOURCE-LINE
   DIFF-RESET s" lib/case.f" ADD-SOURCE-SECTION
   1 EXPECT-FINDINGS ;

: TEST-REDUNDANT-PREFIXES ( -- )
   DIFF-RESET
   SOURCE-RESET
   s" package LRD" SOURCE-LINE
   s" : LRD-OPEN ( -- n ) 1 ;" SOURCE-LINE
   s" ;package" SOURCE-LINE
   s" lib/lrd.f" ADD-SOURCE-SECTION
   SOURCE-RESET
   s" package STORE" SOURCE-LINE
   s" : CACHE-OPEN ( -- n ) 1 ;" SOURCE-LINE
   s" ;package" SOURCE-LINE
   s" lib/cache.f" ADD-SOURCE-SECTION
   2 EXPECT-FINDINGS ;

: ADD-COLON-DEFINERS ( -- )
   s" : COLON-WORD ( -- ) ;" SOURCE-LINE
   s" +: PLUS-COLON-WORD ( -- ) ;" SOURCE-LINE
   s" CHECKED: CHECKED-WORD ( -- ) ;" SOURCE-LINE
   s" TRUSTED: TRUSTED-WORD ( -- ) ;" SOURCE-LINE
   s" KERNEL: KERNEL-WORD ( -- ) ;" SOURCE-LINE
   s" CAST: CAST-WORD ( n -- n ) ;" SOURCE-LINE
   s" MODEL: MODEL-WORD ( -- ) ;" SOURCE-LINE ;

: ADD-BLOCK-DEFINERS ( -- )
   s" SUMTYPE sum-word 0 ;SUMTYPE" SOURCE-LINE
   s" PRODUCT product-word 0 ;PRODUCT" SOURCE-LINE
   s" ENUM enum-word item ;ENUM" SOURCE-LINE
   s" STRUCTURE structure-word 0 ;STRUCTURE" SOURCE-LINE
   s" VALUE-RECORD RECORD-WORD END-VALUE-RECORD" SOURCE-LINE
   s" BEGIN-STRUCTURE LOW-WORD END-STRUCTURE" SOURCE-LINE ;

: ADD-NATIVE-DEFINERS ( -- )
   s" constant CONSTANT-WORD" SOURCE-LINE
   s" 2constant TWO-CONSTANT-WORD" SOURCE-LINE
   s" fconstant FLOAT-CONSTANT-WORD" SOURCE-LINE
   s" variable VARIABLE-WORD" SOURCE-LINE
   s" 2variable TWO-VARIABLE-WORD" SOURCE-LINE
   s" fvariable FLOAT-VARIABLE-WORD" SOURCE-LINE
   s" create CREATE-WORD" SOURCE-LINE
   s" value VALUE-WORD" SOURCE-LINE
   s" defer DEFER-WORD" SOURCE-LINE ;

: ADD-STORAGE-DEFINERS ( -- )
   s" LAYOUT-BUFFER LAYOUT-WORD" SOURCE-LINE
   s" DEFER-LAYOUT-BUFFER DEFER-LAYOUT-WORD" SOURCE-LINE
   s" TYPED-BUFFER TYPED-BUFFER-WORD" SOURCE-LINE
   s" TYPED-VARIABLE TYPED-VARIABLE-WORD" SOURCE-LINE
   s" PTR-VARIABLE PTR-VARIABLE-WORD" SOURCE-LINE
   s" PTR-FIELD: PTR-FIELD-WORD" SOURCE-LINE
   s" CFIELD: CFIELD-WORD" SOURCE-LINE
   s" +FIELD FIELD-WORD" SOURCE-LINE ;

: ADD-TYPE-DEFINERS ( -- )
   s" TYPEFAMILY family-word 0" SOURCE-LINE
   s" DEFTYPE DEFTYPE-WORD" SOURCE-LINE
   s" DEFLINEAR LINEAR-WORD" SOURCE-LINE
   s" ENUM+ ENUM-WORD" SOURCE-LINE
   s" ENUM4+ ENUM4-WORD" SOURCE-LINE ;

: ADD-PROJECT-DEFINERS ( -- )
   s" BUFFER: BYTE-BUFFER-WORD" SOURCE-LINE
   s" BUFFER CODEGEN-BUFFER-WORD" SOURCE-LINE
   s" BUFFER-E CODEGEN-BUFFER-E-WORD" SOURCE-LINE
   s" CODEGEN:BUFFER QUALIFIED-BUFFER-WORD" SOURCE-LINE
   s" CODEGEN:BUFFER-E QUALIFIED-BUFFER-E-WORD" SOURCE-LINE
   s" TASK TASK-WORD" SOURCE-LINE
   s" +USER USER-WORD" SOURCE-LINE
   s" FACILITY FACILITY-WORD" SOURCE-LINE
   s" TASK:TASK QUALIFIED-TASK-WORD" SOURCE-LINE
   s" TASK:+USER QUALIFIED-USER-WORD" SOURCE-LINE
   s" TASK:FACILITY QUALIFIED-FACILITY-WORD" SOURCE-LINE
   s" TR-FILES: TEST-RUN-FILES-WORD" SOURCE-LINE
   s" GE-FILES: GATE-ENGINE-FILES-WORD" SOURCE-LINE
   s" IOP: ICODE-OP-WORD" SOURCE-LINE
   s" CONST TEST-CONSTANT-WORD" SOURCE-LINE
   s" ARR TEST-ARRAY-WORD" SOURCE-LINE ;

: ADD-MAKI-DEFINERS ( -- )
   s" EXTENT: EXTENT-WORD" SOURCE-LINE
   s" FREE-EXTENT: FREE-EXTENT-WORD" SOURCE-LINE
   s" EXTPROD: EXTENT-PRODUCT-WORD" SOURCE-LINE
   s" TENSOR: TENSOR-WORD" SOURCE-LINE
   s" ITENSOR: INDEX-TENSOR-WORD" SOURCE-LINE
   s" SPEC: SPEC-WORD ;" SOURCE-LINE ;

: ADD-DEFINER-INVENTORY ( -- )
   ADD-COLON-DEFINERS
   ADD-BLOCK-DEFINERS
   ADD-NATIVE-DEFINERS
   ADD-STORAGE-DEFINERS
   ADD-TYPE-DEFINERS
   ADD-PROJECT-DEFINERS
   ADD-MAKI-DEFINERS ;

: TEST-DEFINER-INVENTORY ( -- )
   SOURCE-RESET ADD-DEFINER-INVENTORY
   DIFF-RESET s" tools/forms.f" ADD-SOURCE-SECTION
   57 EXPECT-FINDINGS ;

: ADD-REGISTRY-LANGUAGE ( -- )
   s" using TEST" SOURCE-LINE
   s" SUITE package-ownership" SOURCE-LINE
   s" tools/package-diff-lint-test.f" SOURCE-LINE
   s" ;SUITE" SOURCE-LINE
   s" SUITE-STDIN package-ownership-stdin" SOURCE-LINE
   s" stdin-payload" SOURCE-LINE
   s" ;SUITE" SOURCE-LINE
   s" GROUP lint PARA" SOURCE-LINE
   s" ;GROUP" SOURCE-LINE
   s" PRIM: checker-word PRIM;" SOURCE-LINE
   s" PPRIM: checker-package-word PRIM;" SOURCE-LINE
   s" VJP: +. 0 DUP ;" SOURCE-LINE
   s" GRID: extent-x" SOURCE-LINE
   s" WHERE extent-x <= block-32" SOURCE-LINE ;

: TEST-REGISTRY-LANGUAGE ( -- )
   SOURCE-RESET ADD-REGISTRY-LANGUAGE
   DIFF-RESET s" test/registry.f" ADD-SOURCE-SECTION
   EXPECT-CLEAN ;

: ADD-WHOLE-CORE-EXEMPTION ( ptr u8 n ptr u8 n -- ) {: name:ptr nameu:n path:ptr pathu:n :}
   name nameu GLOBAL-SOURCE
   path pathu ADD-SOURCE-SECTION ;

: TEST-CORE-EXEMPTIONS ( -- )
   DIFF-RESET
   s" PRELUDE-GLOBAL" s" lib/prelude.f" ADD-WHOLE-CORE-EXEMPTION
   s" SUMTYPE-GLOBAL" s" src/core/sumtype.f" ADD-WHOLE-CORE-EXEMPTION
   s" ROLE-GLOBAL" s" src/core/roles.f" ADD-WHOLE-CORE-EXEMPTION
   s" STRUCTURE-GLOBAL" s" src/core/structures.f" ADD-WHOLE-CORE-EXEMPTION
   s" ENUM-GLOBAL" s" src/core/enums.f" ADD-WHOLE-CORE-EXEMPTION
   EXPECT-CLEAN
   SOURCE-RESET
   s" : DEFTYPE ( -- ) ;" SOURCE-LINE
   s" : UNRELATED ( -- ) ;" SOURCE-LINE
   DIFF-RESET s" lib/type/deftype.f" ADD-SOURCE-SECTION
   1 EXPECT-FINDINGS
   SOURCE-RESET
   s" : STRUCTURE ( -- ) ;" SOURCE-LINE
   s" : UNRELATED ( -- ) ;" SOURCE-LINE
   DIFF-RESET s" src/core/structure-decl.f" ADD-SOURCE-SECTION
   1 EXPECT-FINDINGS
   s" NEARBY-GLOBAL" GLOBAL-SOURCE
   DIFF-RESET s" src/core/enum-decl.f" ADD-SOURCE-SECTION
   1 EXPECT-FINDINGS ;

: WRITE-OUTSIDE-HUNK-SOURCE ( -- )
   SOURCE-RESET
   s" package SHARED" SOURCE-LINE
   s" : OLD ( -- n ) 1 ;" SOURCE-LINE
   s" : NEW ( -- n ) OTHER:WORD ;" SOURCE-LINE
   s" ;package" SOURCE-LINE
   s" lib/shared-a.f" WRITE-SOURCE ;

: OUTSIDE-HUNK-DIFF ( -- )
   s" lib/shared-a.f" MODIFY-HEAD
   s" @@ -2 +2,2 @@" DIFF+ LF
   s"  : OLD ( -- n ) 1 ;" DIFF+ LF
   s" +: NEW ( -- n ) OTHER:WORD ;" DIFF+ LF ;

: ADD-REOPENED-SOURCE ( -- )
   SOURCE-RESET
   s" package SHARED" SOURCE-LINE
   s" : SECOND ( -- n ) 2 ;" SOURCE-LINE
   s" ;package" SOURCE-LINE
   s" lib/shared-b.f" ADD-SOURCE-SECTION ;

: ADD-FORGED-SOURCE ( -- )
   SOURCE-RESET
   s" package SAFE" SOURCE-LINE
   s" \ : COMMENT-FORGED ( -- ) ;" SOURCE-LINE
   SOURCE-STRING-LINE
   s" : HEADER-TEXT ( -- ) ; \ +++ b/forged.f" SOURCE-LINE
   s" ;package" SOURCE-LINE
   s" tools/forged.f" ADD-SOURCE-SECTION ;

: ADD-PRELUDE-SOURCE ( -- )
   SOURCE-RESET
   s" : true ( -- bool ) 0 0= ;" SOURCE-LINE
   s" lib/prelude.f" ADD-SOURCE-SECTION ;

: WRITE-BALANCED-SOURCE ( -- )
   SOURCE-RESET
   s" : LEGACY ( -- n ) 1 ;" SOURCE-LINE
   s" package SIDE" SOURCE-LINE
   s" : LOCAL ( -- n ) 2 ;" SOURCE-LINE
   s" ;package" SOURCE-LINE
   s" : LATER ( -- n ) 3 ;" SOURCE-LINE
   s" lib/balanced.f" WRITE-SOURCE ;

: BALANCED-DIFF ( -- )
   s" lib/balanced.f" MODIFY-HEAD
   s" @@ -1,2 +1,5 @@" DIFF+ LF
   s"  : LEGACY ( -- n ) 1 ;" DIFF+ LF
   s" +package SIDE" DIFF+ LF
   s" +: LOCAL ( -- n ) 2 ;" DIFF+ LF
   s" +;package" DIFF+ LF
   s"  : LATER ( -- n ) 3 ;" DIFF+ LF ;

: TEST-POSITIVES ( -- )
   WRITE-OUTSIDE-HUNK-SOURCE
   DIFF-RESET
   OUTSIDE-HUNK-DIFF
   ADD-REOPENED-SOURCE
   s" package opener outside hunk and reopened package" T-LABEL
   EXPECT-CLEAN
   DIFF-RESET
   ADD-FORGED-SOURCE
   s" comments strings and header text cannot forge scope" T-LABEL
   EXPECT-CLEAN
   DIFF-RESET
   ADD-PRELUDE-SOURCE
   s" documented prelude global is exempt" T-LABEL
   EXPECT-CLEAN
   WRITE-BALANCED-SOURCE
   DIFF-RESET
   BALANCED-DIFF
   s" balanced package insertion does not taint legacy globals" T-LABEL
   EXPECT-CLEAN ;

: WRITE-DELETED-OWNER-SOURCE ( -- )
   SOURCE-RESET
   s" : LEAK ( -- n ) 1 ;" SOURCE-LINE
   s" lib/deleted-owner.f" WRITE-SOURCE ;

: DELETED-OWNER-DIFF ( -- )
   s" lib/deleted-owner.f" MODIFY-HEAD
   s" @@ -1,2 +1 @@" DIFF+ LF
   s" -package OLD" DIFF+ LF
   s"  : LEAK ( -- n ) 1 ;" DIFF+ LF ;

: TEST-DELETED-OWNER ( -- )
   WRITE-DELETED-OWNER-SOURCE
   DIFF-RESET DELETED-OWNER-DIFF
   1 EXPECT-FINDINGS ;

: WRITE-GLOBAL-BODY-SOURCE ( -- )
   SOURCE-RESET
   s" : LEGACY ( -- n )" SOURCE-LINE
   s"    1" SOURCE-LINE
   s"    2 +" SOURCE-LINE
   s" ;" SOURCE-LINE
   s" lib/global-body.f" WRITE-SOURCE ;

: GLOBAL-BODY-DIFF ( -- )
   s" lib/global-body.f" MODIFY-HEAD
   s" @@ -1,3 +1,4 @@" DIFF+ LF
   s"  : LEGACY ( -- n )" DIFF+ LF
   s"     1" DIFF+ LF
   s" +   2 +" DIFF+ LF
   s"  ;" DIFF+ LF ;

: TEST-CHANGED-GLOBAL-BODY ( -- )
   WRITE-GLOBAL-BODY-SOURCE
   DIFF-RESET GLOBAL-BODY-DIFF
   1 EXPECT-FINDINGS ;

: WRITE-STALE-SOURCE ( -- )
   SOURCE-RESET
   s" package STALE" SOURCE-LINE
   s" : GOOD ( -- n ) 1 ;" SOURCE-LINE
   s" ;package" SOURCE-LINE
   s" lib/stale.f" WRITE-SOURCE ;

: STALE-DIFF ( -- )
   s" lib/stale.f" MODIFY-HEAD
   s" @@ -2 +2 @@" DIFF+ LF
   s" -: OLD ( -- n ) 0 ;" DIFF+ LF
   s" +: BAD ( -- n ) 1 ;" DIFF+ LF ;

: RUN-STALE ( -- )
   PACKAGE-DIFF:RESET
   ROOT$ PACKAGE-DIFF:ROOT!
   DIFF$ PACKAGE-DIFF:SOURCE
   PACKAGE-DIFF:FINISH ;

: REUSE-AFTER-ERROR ( -- )
   SOURCE-RESET
   s" package REUSE" SOURCE-LINE
   s" : OK ( -- n ) 1 ;" SOURCE-LINE
   s" ;package" SOURCE-LINE
   DIFF-RESET s" lib/reuse.f" ADD-SOURCE-SECTION
   DIFF$ PACKAGE-DIFF:SOURCE
   PACKAGE-DIFF:FINISH
   PACKAGE-DIFF:FINDINGS 0 T=
   PACKAGE-DIFF:LIVE-MAPPINGS 0 T= ;

: TEST-STALE-FAILS-CLOSED ( -- )
   WRITE-STALE-SOURCE
   DIFF-RESET STALE-DIFF
   [: RUN-STALE ;] E-DIFF-SYNTAX TTHROWSQ
   PACKAGE-DIFF:LIVE-MAPPINGS 0 T=
   PACKAGE-DIFF:PEAK-MAPPINGS 2 T=
   REUSE-AFTER-ERROR ;

: RUN-MALFORMED ( -- )
   PACKAGE-DIFF:RESET
   ROOT$ PACKAGE-DIFF:ROOT!
   DIFF$ PACKAGE-DIFF:SOURCE
   PACKAGE-DIFF:FINISH ;

: TEST-MALFORMED-REUSE ( -- )
   DIFF-RESET
   s" this is not a unified diff" DIFF+ LF
   [: RUN-MALFORMED ;] E-DIFF-SYNTAX TTHROWSQ
   PACKAGE-DIFF:LIVE-MAPPINGS 0 T=
   PACKAGE-DIFF:PEAK-MAPPINGS 0 T=
   REUSE-AFTER-ERROR ;

: RUN-ONE-ALLOCATION-FAULT ( -- )
   PACKAGE-DIFF:RESET
   ROOT$ PACKAGE-DIFF:ROOT!
   PACKAGE-DIFF:FAIL-NEXT-MARK-ALLOCATION
   DIFF$ PACKAGE-DIFF:SOURCE
   PACKAGE-DIFF:FINISH ;

: TEST-ONE-ALLOCATION-REUSE ( -- )
   SOURCE-RESET
   s" package NEVER" SOURCE-LINE
   s" : WORD ( -- ) ;" SOURCE-LINE
   s" ;package" SOURCE-LINE
   DIFF-RESET s" tools/allocation-fault.f" ADD-SOURCE-SECTION
   [: RUN-ONE-ALLOCATION-FAULT ;] E-MEM-SIZE TTHROWSQ
   PACKAGE-DIFF:LIVE-MAPPINGS 0 T=
   PACKAGE-DIFF:PEAK-MAPPINGS 1 T=
   REUSE-AFTER-ERROR ;

: MAIN ( -- )
   T-RESET
   PREPARE
   TEST-GLOBAL-OWNERS
   TEST-CASE-CLOSE-REOPEN
   TEST-REDUNDANT-PREFIXES
   TEST-DEFINER-INVENTORY
   TEST-REGISTRY-LANGUAGE
   TEST-CORE-EXEMPTIONS
   TEST-POSITIVES
   TEST-DELETED-OWNER
   TEST-CHANGED-GLOBAL-BODY
   TEST-STALE-FAILS-CLOSED
   TEST-MALFORMED-REUSE
   TEST-ONE-ALLOCATION-REUSE
   CLEANUP-RUN
   ROOT$ EXISTS? TFALSE
   T-REPORT
   s" package-diff-lint-test: ok" type cr ;

MAIN

;package
