\ codegen-role.f - structural codegen-role checks over live emitter output.
\
\ Replaces the last BF-PREFLIGHT textual asserts (dot habu-preflight-codegen-role):
\ same-type codegen roles the checker cannot express are proven against the REAL
\ emitters instead of by source substring match. The word under check is
\ extracted from the actual stage source, certified with CHECK!, compiled
\ against the live arm64 emitter primitives (asm.f/icode.f/mnem.f - the trio
\ the object-image writer drives), executed, and the emitted machine words plus
\ fixup records are decoded and asserted:
\ - CLOC-MAIN role (src/habu/habu2.f C-LOCAL-REF): every CLOC-MAIN use must
\   record exactly one pending label-relative B26 branch fixup targeting the
\   CLOC-MAIN label (opcode B, zero delta, site recorded) - never a raw fetch.
\ - spawn descriptor-slot role (src/habu/habu1.f SPAWN-DARWIN-ZERO-ADESC /
\   -ZERO-ATTR): the zero loop must store the zeroed register to [SP, off] for
\   exactly the full 8-byte slot progression base..base+size-8 (the SZA-I
\   indexed address form; the historic Darwin spawn-underflow `over +` form
\   cannot produce it).
\ Extraction is fail-closed: a missing definition/constant/use throws.
\
\ Each check region compiles its extracted definitions under generation-suffixed
\ names (SZA-I-A, CLOC-MAIN-B, ...) so repeated checks in one session never
\ redefine a name. Dictionary rollback (test/prop-test-core.f CHK-MARK) is NOT
\ used here: a CHECK!-certified definition that references a region variable
\ poisons later CHECK! calls after rollback re-defines that variable (stale
\ registry record; dot habu-check-records-go-4f62cd2e).
\
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f.
\ Exercised by tools/codegen-role-test.f (gate suite codegen-role).

require lib/errors.f
require lib/string.f
require lib/adt/option.f                 \ option<idx> INDEX-OF consumer (switchover wave A)
require lib/memory.f
require lib/fs.f

\ tool-local named throw codes (tools/json.f precedent): -7200..-7299
-7200 constant E-CGR-SRC     \ extraction failed: definition/constant/use not found
-7201 constant E-CGR-EVAL    \ extracted source failed checker/compile on live emitters
-7202 constant E-CGR-SPAWN   \ spawn zero loop: wrong count/opcode/register/slot offset
-7203 constant E-CGR-CLOC    \ CLOC-MAIN use is not a pending label-relative branch
-7204 constant E-CGR-CAP     \ tool capacity exceeded (generations/buffers)

$14000000 constant CGR-B-PEND       \ B with zero delta: pending forward fixup
$D2800000 constant CGR-MOVZ-0-BASE  \ movz xN,#0 with rd masked out
$FFFFFFE0 constant CGR-MOVZ-0-MASK
$F9000000 constant CGR-STR-BASE     \ str xN,[xM,#imm] unsigned offset
$FFC00000 constant CGR-STR-MASK
31 constant CGR-SP                  \ spawn slots live on the runtime stack

: CGR-WORD? ( ptr u8 n -- bool )
   XREF-FIND 0= 0= ;

: CGR-LOAD-ASM ( -- )
   s" ASM-INIT" CGR-WORD? if exit then
   s" src/arch/arm64/asm.f" included
   s" src/arch/arm64/icode.f" included
   s" src/arch/arm64/mnem.f" included ;
CGR-LOAD-ASM

\ ---- emitted-word decode ----
variable CGR-WP

: CGR-WP@ ( -- ptr u8 )
   CGR-WP 0 ptr-field @ ;

: CGR-WORD@ ( n -- w )
   CW@ CGR-WP 0 ptr-field !
   CGR-WP@ 0 CODE-BYTE+ c@
   CGR-WP@ 1 CODE-BYTE+ c@ 8 lshift or
   CGR-WP@ 2 CODE-BYTE+ c@ 16 lshift or
   CGR-WP@ 3 CODE-BYTE+ c@ 24 lshift or ;

: CGR-STR? ( w -- bool )
   CGR-STR-MASK and CGR-STR-BASE = ;

: CGR-STR-RT ( w -- n )
   $1F and ;

: CGR-STR-RN ( w -- n )
   5 rshift $1F and ;

: CGR-STR-OFF ( w -- n )
   10 rshift $FFF and 8 * ;

: CGR-MOVZ-0? ( w -- bool )
   CGR-MOVZ-0-MASK and CGR-MOVZ-0-BASE = ;

\ ---- source extraction (fail-closed) ----
: CGR-DEF$ ( ptr u8 n ptr u8 n -- ptr u8 n ) {: a:ptr u:n head:ptr headu:n :}
   a u head headu FIND-SUB {: s:n :}
   s 0 < if s" codegen-role: definition not found" type cr E-CGR-SRC throw then
   a s + u s - s" ;" FIND-SUB {: e:n :}
   e 0 < if s" codegen-role: definition end not found" type cr E-CGR-SRC throw then
   a s + e 1+ ;

: CGR-LINE-START ( ptr u8 n n -- n ) {: a:ptr u:n from:n :}
   from begin dup 0 > while
      dup 1- a + c@ STR-LF = if exit then
      1-
   repeat ;

: CGR-LINE$ ( ptr u8 n ptr u8 n -- ptr u8 n ) {: a:ptr u:n key:ptr keyu:n :}
   a u key keyu FIND-SUB {: s:n :}
   s 0 < if s" codegen-role: source line not found" type cr E-CGR-SRC throw then
   a u s CGR-LINE-START {: ls:n :}
   a ls +
   a ls + u ls - STR-LF INDEX-OF MATCH option
     none OF u ls - ENDOF
     some OF IDX>N ENDOF
   ;MATCH ;

: CGR-WS? ( c -- bool )
   dup STR-SPACE = over STR-TAB = or over STR-LF = or swap STR-CR = or ;

: CGR-SKIP-WS ( ptr u8 n n -- n ) {: a:ptr u:n from:n :}
   from begin dup u < while
      dup a + c@ CGR-WS? 0= if exit then
      1+
   repeat ;

: CGR-SKIP-TOKEN ( ptr u8 n n -- n ) {: a:ptr u:n from:n :}
   from begin dup u < while
      dup a + c@ CGR-WS? if exit then
      1+
   repeat ;

: CGR-TOKEN-END ( ptr u8 n n -- n ) {: a:ptr u:n from:n :}
   a u a u from CGR-SKIP-WS CGR-SKIP-TOKEN ;

: CGR-LINE-NUM ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u 0 CGR-SKIP-WS {: ts:n :}
   a u ts CGR-SKIP-TOKEN {: te:n :}
   a ts + te ts - STR>NUMBER? 0= if
      s" codegen-role: constant line has no leading number" type cr E-CGR-SRC throw
   then ;

\ ---- generation names ----
\ Every check region gets a fresh generation letter; extracted names compile as
\ NAME-<letter> so no name is ever redefined in the session.
26 constant CGR-GEN-CAP
65 constant CGR-GEN-BASE    \ 'A'
45 constant CGR-DASH        \ '-'
variable CGR-GEN

: CGR-GEN+ ( -- )
   CGR-GEN @ 1+ dup CGR-GEN-CAP < 0= if
      s" codegen-role: out of generation names" type cr E-CGR-CAP throw
   then
   CGR-GEN ! ;

: CGR-GEN-C ( -- c )
   CGR-GEN @ CGR-GEN-BASE + ;

\ ---- scratch buffers ----
$1000 constant CGR-SB-CAP
variable CGR-SB-A
variable CGR-SB-LEN

: CGR-SB-BUF ( -- ptr u8 )
   CGR-SB-A @ 0= if
      CGR-SB-CAP MEM-ALLOC-BYTES drop
      CGR-SB-A 0 ptr-field !
   then
   CGR-SB-A 0 ptr-field @ ;

: CGR-SB-RESET ( -- )
   0 >LEN CGR-SB-LEN ! ;

: CGR-SB+ ( ptr u8 n -- )
   CGR-SB-BUF CGR-SB-CAP CGR-SB-LEN BUF-APPEND ;

: CGR-SB-C+ ( c -- )
   CGR-SB-BUF CGR-SB-CAP CGR-SB-LEN BUF-APPEND-C ;

: CGR-SB$ ( -- ptr u8 n )
   CGR-SB-BUF CGR-SB-LEN @ LEN>N ;

: CGR-SB-NAME+ ( ptr u8 n -- )
   CGR-SB+ CGR-DASH CGR-SB-C+ CGR-GEN-C CGR-SB-C+ ;

$1000 constant CGR-NB-CAP
variable CGR-NB-A
variable CGR-NB-LEN
variable CGR-NI

: CGR-NB-BUF ( -- ptr u8 )
   CGR-NB-A @ 0= if
      CGR-NB-CAP MEM-ALLOC-BYTES drop
      CGR-NB-A 0 ptr-field !
   then
   CGR-NB-A 0 ptr-field @ ;

: CGR-NB-C+ ( c -- )
   CGR-NB-BUF CGR-NB-CAP CGR-NB-LEN BUF-APPEND-C ;

: CGR-NB$ ( -- ptr u8 n )
   CGR-NB-BUF CGR-NB-LEN @ LEN>N ;

\ CHECK! is line-oriented: a multi-line body is rejected outright, so extracted
\ definitions are whitespace-normalized to one line first. Extracted emitter
\ definitions and use-site fragments carry no `\` comments; a future one would
\ fail the check closed, not silently pass.
: CGR-NORM ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   0 >LEN CGR-NB-LEN !
   0 CGR-NI !
   begin CGR-NI @ u < while
      a CGR-NI @ + c@ dup CGR-WS? if drop STR-SPACE then CGR-NB-C+
      CGR-NI @ 1+ CGR-NI !
   repeat
   CGR-NB$ ;

\ ---- token rename: NAME -> NAME-<gen> over a normalized text ----
4 constant CGR-RN-CAP
variable CGR-RN-N
variable CGR-RN0-A  variable CGR-RN0-U
variable CGR-RN1-A  variable CGR-RN1-U
variable CGR-RN2-A  variable CGR-RN2-U
variable CGR-RN3-A  variable CGR-RN3-U

: CGR-RN-RESET ( -- )
   0 CGR-RN-N ! ;

: CGR-RN-SLOT-A ( n -- ptr a )
   dup 0 = if drop CGR-RN0-A exit then
   dup 1 = if drop CGR-RN1-A exit then
   dup 2 = if drop CGR-RN2-A exit then
   drop CGR-RN3-A ;

: CGR-RN-SLOT-U ( n -- ptr a )
   dup 0 = if drop CGR-RN0-U exit then
   dup 1 = if drop CGR-RN1-U exit then
   dup 2 = if drop CGR-RN2-U exit then
   drop CGR-RN3-U ;

: CGR-RN+ ( ptr u8 n -- ) {: a:ptr u:n :}
   CGR-RN-N @ CGR-RN-CAP < 0= if
      s" codegen-role: too many rename needles" type cr E-CGR-CAP throw
   then
   a CGR-RN-N @ CGR-RN-SLOT-A 0 ptr-field !
   u CGR-RN-N @ CGR-RN-SLOT-U !
   CGR-RN-N @ 1+ CGR-RN-N ! ;

: CGR-RN$ ( n -- ptr u8 n )
   dup CGR-RN-SLOT-A 0 ptr-field @ swap CGR-RN-SLOT-U @ ;

variable CGR-RJ

: CGR-RENAMED? ( ptr u8 n -- bool ) {: t:ptr tu:n :}
   0 CGR-RJ !
   begin CGR-RJ @ CGR-RN-N @ < while
      t tu CGR-RJ @ CGR-RN$ STR= if 0 0= exit then
      CGR-RJ @ 1+ CGR-RJ !
   repeat
   0 0= 0= ;

variable CGR-RP
variable CGR-RTS
variable CGR-RTE

: CGR-RTOK$ ( -- ptr u8 n )
   CGR-NB$ drop CGR-RTS @ + CGR-RTE @ CGR-RTS @ - ;

: CGR-RN-TOKEN+ ( -- )
   CGR-SB-LEN @ LEN>N 0 > if STR-SPACE CGR-SB-C+ then
   CGR-RTOK$ CGR-SB+
   CGR-RTOK$ CGR-RENAMED? if
      CGR-DASH CGR-SB-C+ CGR-GEN-C CGR-SB-C+
   then ;

\ rebuild NB text into SB, appending -<gen> to every needle token
: CGR-RENAME$ ( -- ptr u8 n )
   CGR-SB-RESET
   0 CGR-RP !
   begin
      CGR-NB$ CGR-RP @ CGR-SKIP-WS CGR-RTS !
      CGR-NB$ CGR-RTS @ CGR-SKIP-TOKEN CGR-RTE !
      CGR-RTE @ CGR-RTS @ > while
      CGR-RN-TOKEN+
      CGR-RTE @ CGR-RP !
   repeat
   CGR-SB$ ;

: CGR-PREP$ ( ptr u8 n -- ptr u8 n )
   CGR-NORM 2drop CGR-RENAME$ ;

\ ---- dynamic-compile boundaries ----
\ The checker intentionally rejects `evaluate` inside checked definitions
\ (INCLUDE-EVALUATE precedent); extracted real-source text crosses here and any
\ failure is renamed E-CGR-EVAL by the wrappers below.
TRUSTED: CGR-EVALUATE ( ptr u8 n -- ) evaluate ;

\ Checker boundary: certify an extracted definition before compiling it, the
\ test/prop-test-core.f CHK pattern. This keeps corrupted definitions on the
\ catchable CHECK! verdict path instead of the evaluate compile-error path
\ (an undefined word thrown inside evaluate under catch crashes the engine
\ natively today - dot habu-undefined-word-in-d9dc3452).
TRUSTED: CGR-CHECK! ( ptr u8 n -- n ) CHECK! ;

\ Post-certification compile: CHECK! has already registered the definition
\ record and a checked re-compile hits strict duplicate rejection, so the
\ certified text compiles with the hook off and the hook is reinstalled right
\ after (test/prop-test-core.f CHK-COMPILE-CERT precedent). Leaving the hook
\ off would compile later eval'd support definitions untyped and poison later
\ CHECK! calls. CGR-HOOK mirrors the image's frozen session hook
\ (src/habu/snap-lib.f SNAP-CHECK-HOOK) byte for byte.
TRUSTED: CGR-EVALUATE-UNCHECKED ( ptr u8 n -- ) 0 set-check evaluate ;
TRUSTED: CGR-HOOK ( ptr u8 n -- n ) CHECK! dup -1 <> IF 70 throw THEN ;
TRUSTED: CGR-HOOK! ( -- ) ['] CGR-HOOK set-check ;

variable CGR-EV-A
variable CGR-EV-U

: CGR-EV$ ( -- ptr u8 n )
   CGR-EV-A 0 ptr-field @ CGR-EV-U @ ;

: CGR-EVAL-GO ( -- )
   CGR-EV$ CGR-EVALUATE ;

: CGR-EVAL ( ptr u8 n -- ) {: a:ptr u:n :}
   a CGR-EV-A 0 ptr-field !  u CGR-EV-U !
   [: CGR-EVAL-GO ;] catch dup 0 <> if
      drop
      s" codegen-role: extracted source failed on live emitters:" type cr
      CGR-EV$ type cr
      E-CGR-EVAL throw
   then drop ;

: CGR-EVAL-CERT-GO ( -- )
   CGR-EV$ CGR-EVALUATE-UNCHECKED ;

: CGR-EVAL-CERT ( ptr u8 n -- ) {: a:ptr u:n :}
   a CGR-EV-A 0 ptr-field !  u CGR-EV-U !
   [: CGR-EVAL-CERT-GO ;] catch
   CGR-HOOK!
   dup 0 <> if
      drop
      s" codegen-role: certified definition failed to compile:" type cr
      CGR-EV$ type cr
      E-CGR-EVAL throw
   then drop ;

\ Definition text is `: NAME ( sig ) body ;`; CHECK! takes it without the
\ leading `: ` and trailing ` ;` (test/prop-test-core.f CHK-BODY$ shape).
: CGR-EVAL-DEF ( ptr u8 n -- ) {: a:ptr u:n :}
   a 2 + u 4 - CGR-CHECK! -1 <> if
      s" codegen-role: extracted definition failed the checker:" type cr
      a u type cr
      E-CGR-EVAL throw
   then
   a u CGR-EVAL-CERT ;

\ ---- shared check state ----
variable CGR-TXT-A
variable CGR-TXT-U

: CGR-TXT$ ( -- ptr u8 n )
   CGR-TXT-A 0 ptr-field @ CGR-TXT-U @ ;

\ ---- spawn descriptor-slot role ----
variable CGR-BASE
variable CGR-SIZE
variable CGR-RD
variable CGR-I

: CGR-SPAWN-COUNT ( -- )
   ASM-CP @ CGR-SIZE @ 8 / 1+ <> if
      s" codegen-role: spawn zero loop emitted wrong word count" type cr
      E-CGR-SPAWN throw
   then ;

: CGR-SPAWN-RD ( -- )
   0 CGR-WORD@ dup CGR-MOVZ-0? 0= if
      drop s" codegen-role: spawn zero loop does not start with movz xN,#0" type cr
      E-CGR-SPAWN throw
   then
   $1F and CGR-RD ! ;

: CGR-SPAWN-SLOT ( n -- ) {: i:n :}
   i CGR-WORD@ {: w:n :}
   w CGR-STR? 0= if
      s" codegen-role: spawn zero loop word is not a 64-bit store" type cr
      E-CGR-SPAWN throw
   then
   w CGR-STR-RT CGR-RD @ <> if
      s" codegen-role: spawn store does not store the zeroed register" type cr
      E-CGR-SPAWN throw
   then
   w CGR-STR-RN CGR-SP <> if
      s" codegen-role: spawn store base register is not SP" type cr
      E-CGR-SPAWN throw
   then
   w CGR-STR-OFF CGR-BASE @ i 1- 8 * + <> if
      s" codegen-role: spawn store misses its descriptor slot offset" type cr
      E-CGR-SPAWN throw
   then ;

: CGR-SPAWN-DECODE ( -- )
   CGR-SPAWN-COUNT
   CGR-SPAWN-RD
   1 CGR-I !
   begin CGR-I @ ASM-CP @ < while
      CGR-I @ CGR-SPAWN-SLOT
      CGR-I @ 1+ CGR-I !
   repeat ;

: CGR-SPAWN-PREP ( ptr u8 n ptr u8 n -- ) {: offk:ptr offku:n sizek:ptr sizeku:n :}
   CGR-TXT$ s" variable SZA-I" CGR-LINE$ CGR-PREP$ CGR-EVAL
   CGR-TXT$ offk offku CGR-LINE$ 2dup CGR-LINE-NUM CGR-BASE ! CGR-PREP$ CGR-EVAL
   CGR-TXT$ sizek sizeku CGR-LINE$ 2dup CGR-LINE-NUM CGR-SIZE ! CGR-PREP$ CGR-EVAL ;

: CGR-SPAWN-DUT ( ptr u8 n -- )
   CGR-TXT$ 2swap CGR-DEF$ CGR-PREP$ CGR-EVAL-DEF ;

: CGR-SPAWN-RUN$ ( ptr u8 n -- ptr u8 n )
   CGR-SB-RESET
   s" ASM-INIT " CGR-SB+
   CGR-SB-NAME+
   CGR-SB$ ;

: CGR-SPAWN-ADESC-CHECK ( -- )
   CGR-GEN+
   CGR-RN-RESET
   s" SZA-I" CGR-RN+
   s" SPAWN-ADESC-OFF" CGR-RN+
   s" SPAWN-ADESC-SIZE" CGR-RN+
   s" SPAWN-DARWIN-ZERO-ADESC" CGR-RN+
   s" constant SPAWN-ADESC-OFF" s" constant SPAWN-ADESC-SIZE" CGR-SPAWN-PREP
   s" : SPAWN-DARWIN-ZERO-ADESC" CGR-SPAWN-DUT
   s" SPAWN-DARWIN-ZERO-ADESC" CGR-SPAWN-RUN$ CGR-EVAL
   CGR-SPAWN-DECODE ;

: CGR-SPAWN-ATTR-CHECK ( -- )
   CGR-GEN+
   CGR-RN-RESET
   s" SZA-I" CGR-RN+
   s" SPAWN-ATTR-OFF" CGR-RN+
   s" SPAWN-ATTR-SIZE" CGR-RN+
   s" SPAWN-DARWIN-ZERO-ATTR" CGR-RN+
   s" constant SPAWN-ATTR-OFF" s" constant SPAWN-ATTR-SIZE" CGR-SPAWN-PREP
   s" : SPAWN-DARWIN-ZERO-ATTR" CGR-SPAWN-DUT
   s" SPAWN-DARWIN-ZERO-ATTR" CGR-SPAWN-RUN$ CGR-EVAL
   CGR-SPAWN-DECODE ;

\ ---- CLOC-MAIN label-relative branch role ----
variable CGR-BODY-A
variable CGR-BODY-U
variable CGR-LBL-N
variable CGR-OCC
variable CGR-USES

: CGR-BODY$ ( -- ptr u8 n )
   CGR-BODY-A 0 ptr-field @ CGR-BODY-U @ ;

: CGR-NEXT-USE ( n -- n ) {: from:n :}
   CGR-BODY$ {: a:ptr u:n :}
   from u < 0= if -1 exit then
   a from + u from - s" CLOC-MAIN" FIND-SUB dup 0 < if exit then
   from + ;

: CGR-USE-SPAN$ ( n -- ptr u8 n ) {: occ:n :}
   CGR-BODY$ {: a:ptr u:n :}
   a u occ CGR-SKIP-TOKEN {: e0:n :}
   a u e0 CGR-TOKEN-END {: e1:n :}
   a u e1 CGR-TOKEN-END {: e2:n :}
   a occ + e2 occ - ;

: CGR-CLOC-ASSERT ( -- )
   NFX @ 1 <> if
      s" codegen-role: CLOC-MAIN use recorded no pending branch fixup" type cr
      E-CGR-CLOC throw
   then
   ASM-CP @ 1 <> if
      s" codegen-role: CLOC-MAIN use emitted extra words" type cr
      E-CGR-CLOC throw
   then
   0 cells FXK + @ 0 <> if
      s" codegen-role: CLOC-MAIN fixup is not B26 kind" type cr
      E-CGR-CLOC throw
   then
   0 cells FXL + @ CGR-LBL-N @ <> if
      s" codegen-role: CLOC-MAIN fixup targets another label" type cr
      E-CGR-CLOC throw
   then
   0 cells FXS + @ 0 <> if
      s" codegen-role: CLOC-MAIN fixup site is not the branch word" type cr
      E-CGR-CLOC throw
   then
   0 CGR-WORD@ CGR-B-PEND <> if
      s" codegen-role: CLOC-MAIN branch word is not a pending B" type cr
      E-CGR-CLOC throw
   then ;

: CGR-CLOC-VAR$ ( -- ptr u8 n )
   CGR-SB-RESET
   s" variable " CGR-SB+
   s" CLOC-MAIN" CGR-SB-NAME+
   CGR-SB$ ;

: CGR-CLOC-FRAG$ ( -- ptr u8 n )
   CGR-SB-RESET
   s" : " CGR-SB+
   s" CGR-FRAG" CGR-SB-NAME+
   s"  ( -- ) " CGR-SB+
   CGR-OCC @ CGR-USE-SPAN$ CGR-SB+
   s"  ;" CGR-SB+
   CGR-SB$ ;

: CGR-CLOC-RUN$ ( -- ptr u8 n )
   CGR-SB-RESET
   s" ASM-INIT LBL " CGR-SB+
   s" CLOC-MAIN" CGR-SB-NAME+
   s"  LABEL! " CGR-SB+
   s" CLOC-MAIN" CGR-SB-NAME+
   s"  @ CGR-LBL-N ! " CGR-SB+
   s" CGR-FRAG" CGR-SB-NAME+
   CGR-SB$ ;

: CGR-CLOC-FRAG-CHECK ( -- )
   CGR-GEN+
   CGR-RN-RESET
   s" CLOC-MAIN" CGR-RN+
   CGR-CLOC-VAR$ CGR-EVAL
   CGR-CLOC-FRAG$ CGR-PREP$ CGR-EVAL-DEF
   CGR-CLOC-RUN$ CGR-EVAL
   CGR-CLOC-ASSERT ;

: CGR-CLOC-USES ( -- )
   0 CGR-USES !
   0 CGR-OCC !
   begin CGR-OCC @ CGR-NEXT-USE dup 0 < 0= while
      CGR-OCC !
      CGR-CLOC-FRAG-CHECK
      CGR-USES @ 1+ CGR-USES !
      CGR-OCC @ 1+ CGR-OCC !
   repeat drop ;

: CGR-CHECK-CLOC ( ptr u8 n -- ) {: a:ptr u:n :}
   a CGR-TXT-A 0 ptr-field !  u CGR-TXT-U !
   a u s" : C-LOCAL-REF " CGR-DEF$ {: b:ptr bu:n :}
   b CGR-BODY-A 0 ptr-field !  bu CGR-BODY-U !
   CGR-CLOC-USES
   CGR-USES @ 0= if
      s" codegen-role: C-LOCAL-REF has no CLOC-MAIN use" type cr E-CGR-SRC throw
   then ;

: CGR-CHECK-SPAWN ( ptr u8 n -- ) {: a:ptr u:n :}
   a CGR-TXT-A 0 ptr-field !  u CGR-TXT-U !
   CGR-SPAWN-ADESC-CHECK
   CGR-SPAWN-ATTR-CHECK ;
