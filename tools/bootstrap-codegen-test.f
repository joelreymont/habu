\ bootstrap-codegen-test.f - native source regression for bootstrap codegen hard cutover.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f tools/bootstrap-codegen-test.f

require lib/errors.f
require lib/string.f
require lib/adt/option.f                 \ option<CAD-NUM:index> STR:FIND-SUB consumer
require lib/test.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/source-lex.f

\ habu2.f is 262,867 bytes on the ENGINE-ERROR cutover tree; 25% headroom is
\ 328,584 bytes, so the next power-of-two arena is $80000.
$80000 constant BCG-CAP

create BCG-BUF BCG-CAP allot
variable BCG-LEN

: BCG-SOURCE ( -- ptr u8 n )
   BCG-BUF BCG-LEN @ ;

: BCG-LOAD ( ptr u8 n -- )
   BCG-BUF BCG-CAP READ-ALL BCG-LEN ! ;

: BCG-HAS? ( ptr u8 n -- bool )
   BCG-SOURCE 2swap CONTAINS? ;

: BCG-MUST-HAVE ( ptr u8 n -- )
   BCG-HAS? TTRUE ;

: BCG-MUST-LACK ( ptr u8 n -- )
   BCG-HAS? 0= TTRUE ;

\ typed STR:FIND-SUB boundary: route byte-lengths through the STR: role surface,
\ project the option<CAD-NUM:index> result back to the switchover option<idx>.
package CAD-NUM
public
: BCG-IX>N ( CAD-NUM:index -- n ) INDEX>N ;
;package
: BCG-FIND ( ptr u8 n ptr u8 n -- option<idx> ) {: a:ptr u:n b:ptr v:n :}
   a u STR:LENGTH b v STR:LENGTH STR:FIND-SUB MATCH option
     none OF OPTION:NONE ENDOF
     some OF CAD-NUM:BCG-IX>N >IDX OPTION:SOME ENDOF
   ;MATCH ;

: BCG-POS ( ptr u8 n -- option<idx> )
   BCG-SOURCE 2swap BCG-FIND ;

: BCG-POS-FOUND ( ptr u8 n -- n )
   BCG-POS MATCH option
     none OF STR-FALSE TTRUE -1 ENDOF
     some OF STR-TRUE TTRUE IDX>N ENDOF
   ;MATCH ;

package BCG-CAP

32 constant TOK-CAP

create PCT-TOK TOK-CAP allot
create CAP-TOK TOK-CAP allot
create IBUF-TOK TOK-CAP allot
variable PCT-U
variable CAP-U
variable IBUF-U
variable DEF-I
variable DEF-N

: TOK=CI ( n ptr u8 n -- bool ) {: idx:n a:ptr u:n :}
   idx LEX-TOK a u LINT-STR=CI ;

: DEF? ( n ptr u8 n -- bool ) {: idx:n name:ptr nameu:n :}
   idx 0 <= if 0 0= 0= exit then
   idx 1 + L# @ >= if 0 0= 0= exit then
   idx 1 - LK@ L-WORD <> if 0 0= 0= exit then
   idx LK@ L-WORD <> if 0 0= 0= exit then
   idx 1 + LK@ L-WORD <> if 0 0= 0= exit then
   idx s" constant" TOK=CI 0= if 0 0= 0= exit then
   idx 1 + name nameu TOK=CI ;

: DEF-SCAN ( ptr u8 n -- ) {: name:ptr nameu:n :}
   BCG-SOURCE LEX-SOURCE
   -1 DEF-I !
   0 DEF-N !
   0 begin dup L# @ < while
      dup name nameu DEF? if
         dup DEF-I !
         DEF-N @ 1 + DEF-N !
      then
      1+
   repeat drop ;

: DEF-VALUE ( ptr u8 n -- ptr u8 n )
   DEF-SCAN
   DEF-N @ 1 = dup TTRUE 0= if s" " exit then
   DEF-I @ 1 - LEX-TOK ;

: TOK-SAVE ( ptr u8 n ptr u8 ptr n -- )
   {: src:ptr u:n dst:ptr lenp:ptr :}
   u TOK-CAP <= TTRUE
   src dst u BYTE-COPY
   u lenp ! ;

: SAVE-TOKENS ( -- )
   s" SOURCE-HEADROOM-PCT" DEF-VALUE
   PCT-TOK PCT-U TOK-SAVE
   s" SOURCE-ARENA-CAP" DEF-VALUE
   CAP-TOK CAP-U TOK-SAVE
   s" IBUFSZ" DEF-VALUE
   IBUF-TOK IBUF-U TOK-SAVE ;

: CHECK-TOKENS ( -- )
   s" SOURCE-HEADROOM-PCT" DEF-VALUE
   PCT-TOK PCT-U @ T$=
   s" SOURCE-ARENA-CAP" DEF-VALUE
   CAP-TOK CAP-U @ T$=
   s" IBUFSZ" DEF-VALUE
   IBUF-TOK IBUF-U @ T$= ;

: OWNER ( -- )
   s" SOURCE-HEADROOM-PCT" DEF-SCAN DEF-N @ 1 T=
   s" SOURCE-ARENA-CAP" DEF-SCAN DEF-N @ 1 T=
   s" IBUFSZ" DEF-SCAN DEF-N @ 1 T= ;

public

: TEST ( -- )
   SOURCE-HEADROOM-PCT 25 T=
   SOURCE-ARENA-CAP IBUFSZ T=
   s" src/habu/layout.f" BCG-LOAD
   OWNER
   SAVE-TOKENS
   s" bootstrap/cg/forth.fs" BCG-LOAD
   OWNER
   CHECK-TOKENS
   s" src/habu/stage2.f" BCG-LOAD
   s" S2-SOURCE-CAP" DEF-VALUE s" SOURCE-ARENA-CAP" T$=
   s" src/habu/maker.f" BCG-LOAD
   s" MK-SOURCE-CAP" DEF-VALUE s" SOURCE-ARENA-CAP" T$= ;

;package

: BCG-MUST-BEFORE ( ptr u8 n ptr u8 n -- ) {: earlier:ptr earlieru later:ptr lateru :}
   earlier earlieru BCG-POS-FOUND
   later lateru BCG-POS-FOUND
   < TTRUE ;

: BCG-FIND-AFTER ( n ptr u8 n -- option<idx> ) {: start:n needle:ptr nu:n :}
   BCG-SOURCE {: src:ptr srcu :}
   start 0 < if OPTION:NONE exit then
   start srcu >= if OPTION:NONE exit then
   src start + srcu start - needle nu BCG-FIND MATCH option
     none OF OPTION:NONE ENDOF
     some OF IDX>N start + >IDX OPTION:SOME ENDOF
   ;MATCH ;

: BCG-AFTER-FOUND ( n ptr u8 n -- n )              \ assert found after start; found index
   BCG-FIND-AFTER MATCH option
     none OF STR-FALSE TTRUE -1 ENDOF
     some OF STR-TRUE TTRUE IDX>N ENDOF
   ;MATCH ;

: BCG-MUST-NOT-FIND-BEFORE ( n n ptr u8 n -- ) {: start end needle:ptr nu :}
   start needle nu BCG-FIND-AFTER MATCH option
     none OF exit ENDOF
     some OF IDX>N ENDOF
   ;MATCH {: pos:n :}
   pos end >= TTRUE ;

: BCG-MUST-FIND-BEFORE ( n n ptr u8 n -- )
   {: start end needle:ptr nu :}
   start needle nu BCG-FIND-AFTER MATCH option
     none OF STR-FALSE TTRUE ENDOF
     some OF STR-TRUE TTRUE IDX>N end < TTRUE ENDOF
   ;MATCH ;

package BCG-MANIFEST

$4000 constant CAP
0 constant MODE-FORTH
1 constant MODE-SOURCE
2 constant MODE-CAT
3 constant MODE-ARRAY
4 constant MODE-COMMON
36 constant DOLLAR

create LOAD-BUF CAP allot
create PATH-BUF CAP allot
create PROVIDE-BUF CAP allot
create EXPECT-BUF CAP allot
create RECOVERY-BUF CAP allot
create FIXPOINT-BUF CAP allot
variable LOAD-U
variable PATH-U
variable PROVIDE-U
variable EXPECT-U
variable RECOVERY-U
variable FIXPOINT-U
variable SCAN-OFF
variable SCAN-N
variable UNIQUE-I
variable UNIQUE-J
create SEP 124 c,
create LF 10 c,

: LOAD+ ( ptr u8 n -- )
   LOAD-BUF CAP LOAD-U BUF-APPEND ;

: PATH+ ( ptr u8 n -- )
   PATH-BUF CAP PATH-U BUF-APPEND ;

: PROVIDE+ ( ptr u8 n -- )
   PROVIDE-BUF CAP PROVIDE-U BUF-APPEND ;

: EXPECT+ ( ptr u8 n -- )
   EXPECT-BUF CAP EXPECT-U BUF-APPEND ;

: RECOVERY+ ( ptr u8 n -- )
   RECOVERY-BUF CAP RECOVERY-U BUF-APPEND ;

: FIXPOINT+ ( ptr u8 n -- )
   FIXPOINT-BUF CAP FIXPOINT-U BUF-APPEND ;

: LOAD$ ( -- ptr u8 n )
   LOAD-BUF LOAD-U @ ;

: PATH$ ( -- ptr u8 n )
   PATH-BUF PATH-U @ ;

: PROVIDE$ ( -- ptr u8 n )
   PROVIDE-BUF PROVIDE-U @ ;

: EXPECT$ ( -- ptr u8 n )
   EXPECT-BUF EXPECT-U @ ;

: RECOVERY$ ( -- ptr u8 n )
   RECOVERY-BUF RECOVERY-U @ ;

: FIXPOINT$ ( -- ptr u8 n )
   FIXPOINT-BUF FIXPOINT-U @ ;

\ typed-local-lint: allow-bare-local - needle preserves its ptr u8 role.
: UNIQUE-POS ( ptr u8 n -- n ) {: needle nu:n :}
   needle nu BCG-POS-FOUND {: pos:n :}
   pos 1+ needle nu BCG-FIND-AFTER MATCH option
     none OF ENDOF
     some OF drop STR-FALSE TTRUE ENDOF
   ;MATCH
   pos ;

\ typed-local-lint: allow-bare-local - markers and source preserve ptr u8 roles.
: RANGE$ ( ptr u8 n ptr u8 n -- ptr u8 n ) {: first fu:n after au:n :}
   first fu UNIQUE-POS {: start:n :}
   after au UNIQUE-POS {: end:n :}
   start end < TTRUE
   BCG-SOURCE {: src:ptr srcu:n :}
   end srcu <= TTRUE
   src start + end start - ;

\ typed-local-lint: allow-bare-local - quoted source token preserves ptr u8.
: QPATH$ ( ptr u8 n -- ptr u8 n ) {: a u:n :}
   u 0 > TTRUE
   a u 1- + c@ DQUOTE = TTRUE
   a u 1- ;

: UNQUOTE$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u 1 > if
      a c@ DQUOTE = a u 1- + c@ DQUOTE = and if a 1+ u 2 - exit then
   then
   a u ;

: ROW-TOKENS? ( -- bool )
   SN# @ 5 = if STR-TRUE exit then
   SN# @ 6 = if 5 S@ s" ;" STR= exit then
   STR-FALSE ;

\ typed-local-lint: allow-bare-local - q preserves its byte-span callback effect.
: EMIT-FORTH-ROW ( [ ptr u8 n -- ] -- ) {: q :}
   0 S@ q execute  SEP 1 q execute
   1 S@ q execute  SEP 1 q execute
   3 S@ QPATH$ q execute  LF 1 q execute ;

\ typed-local-lint: allow-bare-local - op and q preserve token/callback roles.
: FORTH-LINE ( ptr u8 n ptr u8 n [ ptr u8 n -- ] -- bool )
   {: line lu:n op opu:n q :}
   line lu SPLIT-WHITESPACE
   ROW-TOKENS? 0= if STR-FALSE exit then
   4 S@ op opu STR= 0= if STR-FALSE exit then
   2 S@ S\" s\"" T$=
   3 S@ QPATH$ 2drop
   q EMIT-FORTH-ROW
   STR-TRUE ;

\ typed-local-lint: allow-bare-local - q preserves its byte-span callback effect.
: EMIT-FILE ( ptr u8 n [ ptr u8 n -- ] -- ) {: path pu:n q :}
   path pu q execute
   LF 1 q execute ;

\ typed-local-lint: allow-bare-local - line/op/q preserve byte-span roles.
: SOURCE-LINE ( ptr u8 n ptr u8 n [ ptr u8 n -- ] -- bool )
   {: line lu:n op opu:n q :}
   line lu SPLIT-WHITESPACE
   ROW-TOKENS? 0= if STR-FALSE exit then
   4 S@ op opu STR= 0= if STR-FALSE exit then
   0 S@ s" out" T$=
   1 S@ s" outu" T$=
   2 S@ S\" s\"" T$=
   3 S@ QPATH$ q EMIT-FILE
   STR-TRUE ;

\ typed-local-lint: allow-bare-local - line/q preserve byte-span roles.
: CAT-LINE ( ptr u8 n [ ptr u8 n -- ] -- bool ) {: line lu:n q :}
   line lu SPLIT-WHITESPACE
   SN# @ 4 <> if STR-FALSE exit then
   0 S@ s" cat" STR= 0= if STR-FALSE exit then
   2 S@ s" >>" T$=
   3 S@ S\" \"$out\"" T$=
   1 S@ q EMIT-FILE
   STR-TRUE ;

\ typed-local-lint: allow-bare-local - line/q preserve byte-span roles.
: ARRAY-LINE ( ptr u8 n [ ptr u8 n -- ] -- bool ) {: line lu:n q :}
   line lu SPLIT-WHITESPACE
   SN# @ 1 <> if STR-FALSE exit then
   0 S@ UNQUOTE$ {: path:ptr pu:n :}
   path pu s" src/" STARTS-WITH? 0= if
      pu 0= if STR-FALSE exit then
      path c@ DOLLAR <> if STR-FALSE exit then
   then
   path pu q EMIT-FILE
   STR-TRUE ;

\ typed-local-lint: allow-bare-local - line/q preserve byte-span roles.
: COMMON-LINE ( ptr u8 n [ ptr u8 n -- ] -- bool ) {: line lu:n q :}
   line lu SPLIT-WHITESPACE
   SN# @ 3 < if STR-FALSE exit then
   0 S@ s" out" STR= 0= if STR-FALSE exit then
   1 S@ s" outu" STR= 0= if STR-FALSE exit then
   2 S@ S\" s\"" STR= if
      ROW-TOKENS? 0= if STR-FALSE exit then
      4 S@ s" BF-APPEND-SOURCE" STR= 0= if STR-FALSE exit then
      3 S@ QPATH$ q EMIT-FILE
      STR-TRUE exit
   then
   SN# @ 3 = SN# @ 4 = or 0= if STR-FALSE exit then
   SN# @ 4 = if 3 S@ s" ;" T$= then
   2 S@ q EMIT-FILE
   STR-TRUE ;

\ typed-local-lint: allow-bare-local - line/op/q preserve byte-span roles.
: CAPTURE-LINE ( ptr u8 n n ptr u8 n [ ptr u8 n -- ] -- bool )
   {: line lu:n mode:n op opu:n q :}
   mode case
      MODE-FORTH of line lu op opu q FORTH-LINE endof
      MODE-SOURCE of line lu op opu q SOURCE-LINE endof
      MODE-CAT of line lu q CAT-LINE endof
      MODE-ARRAY of line lu q ARRAY-LINE endof
      MODE-COMMON of line lu q COMMON-LINE endof
      STR-FALSE swap
   endcase ;

\ typed-local-lint: allow-bare-local - source/op/q and split line keep roles.
: CAPTURE-SPAN ( ptr u8 n n ptr u8 n [ ptr u8 n -- ] -- n )
   {: src srcu:n mode:n op opu:n q :}
   0 SCAN-OFF !  0 SCAN-N !
   begin SCAN-OFF @ srcu <= while
      src srcu STR-LF SCAN-OFF @ SPLIT-NEXT 0= if 2drop drop SCAN-N @ exit then
      {: line:ptr lu:n next:n :}
      next SCAN-OFF !
      line lu mode op opu q CAPTURE-LINE if 1 SCAN-N +! then
   repeat
   SCAN-N @ ;

\ typed-local-lint: allow-bare-local - markers/op/q preserve byte-span roles.
: CAPTURE ( ptr u8 n ptr u8 n n ptr u8 n [ ptr u8 n -- ] -- n )
   {: first fu:n after au:n mode:n op opu:n q :}
   first fu after au RANGE$
   mode op opu q CAPTURE-SPAN ;

\ typed-local-lint: allow-bare-local - expected row spans remain byte strings.
: EXPECT-ROW ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: kind ku:n var vu:n path pu:n :}
   kind ku EXPECT+  SEP 1 EXPECT+
   var vu EXPECT+   SEP 1 EXPECT+
   path pu EXPECT+  LF 1 EXPECT+ ;

: EXPECT-CHECKER ( -- )
   s" PFX-COMMON" s" LPUTIL" s" src/core/util.f" EXPECT-ROW
   s" PFX-COMMON" s" LPCELL" s" src/core/cell.f" EXPECT-ROW
   s" PFX-COMMON" s" LPPTRSTORAGE" s" src/core/pointer-storage.f" EXPECT-ROW
   s" PFX-COMMON" s" LPENGINEERROR" s" src/core/engine-error.f" EXPECT-ROW
   s" PFX-COMMON" s" LPCHECKER" s" src/core/checker.f" EXPECT-ROW
   s" PFX-COMMON" s" LPENGINEERROREFFECTS" s" src/core/engine-error-effects.f" EXPECT-ROW
   s" PFX-COMMON" s" LPLOWERCERTBASE" s" src/core/lower-cert-base.f" EXPECT-ROW
   s" PFX-COMMON" s" LPTYPESCHEMA" s" src/core/type-schema.f" EXPECT-ROW
   s" PFX-COMMON" s" LPTYPEFAM" s" src/core/type-family.f" EXPECT-ROW
   s" PFX-COMMON" s" LPRENDER" s" src/core/render.f" EXPECT-ROW
   s" PFX-COMMON" s" LPSUMTYPE" s" src/core/sumtype.f" EXPECT-ROW
   s" PFX-COMMON" s" LPLAYOUTBUF" s" src/core/layout-buffer.f" EXPECT-ROW
   s" PFX-COMMON" s" LPLAYOUTVALID" s" src/core/layout-valid.f" EXPECT-ROW
   s" PFX-COMMON" s" LPHOOK" s" src/core/check-hook.f" EXPECT-ROW
   s" PFX-COMMON" s" LPCELLEFF" s" src/core/cell-effects.f" EXPECT-ROW
   s" PFX-COMMON" s" LPPTRSTORAGEEFF" s" src/core/pointer-storage-effects.f" EXPECT-ROW ;

: EXPECT-CORE ( -- )
   s" PFX-COMMON" s" LPSTRUCTURES" s" src/core/structures.f" EXPECT-ROW
   s" PFX-COMMON" s" LPROLES" s" src/core/roles.f" EXPECT-ROW
   s" PFX-COMMON" s" LPBYTES" s" src/core/bytes.f" EXPECT-ROW
   s" PFX-LINUX" s" LPLINUXTARGET" s" src/os/linux/target.f" EXPECT-ROW
   s" PFX-MACOS" s" LPMACOSTARGET" s" src/os/macos/target.f" EXPECT-ROW
   s" PFX-LINUX" s" LPLINUXLAYOUT" s" src/os/linux/layout.f" EXPECT-ROW
   s" PFX-MACOS" s" LPMACOSLAYOUT" s" src/os/macos/layout.f" EXPECT-ROW
   s" PFX-COMMON" s" LPHABULAYOUT" s" src/habu/layout.f" EXPECT-ROW
   s" PFX-COMMON" s" LPENVBASE" s" src/os/env-base.f" EXPECT-ROW
   s" PFX-COMMON" s" LPINCLUDE" s" src/core/include.f" EXPECT-ROW
   s" PFX-COMMON" s" LPENUMS" s" src/core/enums.f" EXPECT-ROW
   s" PFX-COMMON" s" LPEXECVECTOR" s" src/core/exec-vector.f" EXPECT-ROW
   s" PFX-COMMON" s" LPSHA256" s" src/core/sha256.f" EXPECT-ROW
   s" PFX-COMMON" s" LPTFAMSHA" s" src/core/type-family-sha.f" EXPECT-ROW
   s" PFX-COMMON" s" LPCOMBINATORS" s" src/core/combinators.f" EXPECT-ROW
   s" PFX-COMMON" s" LPXREF" s" src/habu/xref.f" EXPECT-ROW
   s" PFX-COMMON" s" LPLAYOUTSEAL" s" src/core/layout-buffer-seal.f" EXPECT-ROW ;

: EXPECT-NATIVE ( -- )
   0 EXPECT-U !
   EXPECT-CHECKER
   EXPECT-CORE
   s" PFX-COMMON" s" LPLOWERCERTSEAL" s" src/core/lower-cert-seal.f" EXPECT-ROW
   s" PFX-COMMON" s" LPSCRIPTARGV" s" src/os/script-argv.f" EXPECT-ROW
   s" PFX-COMMON" s" LPINTMARK" s" src/core/internal-mark.f" EXPECT-ROW
   s" PFX-COMMON" s" LPTOPROW" s" src/core/top-row.f" EXPECT-ROW ;

: EXPECT-GFORTH ( -- )
   0 EXPECT-U !
   EXPECT-CHECKER
   EXPECT-CORE
   s" PFX-COMMON" s" LPSCRIPTARGV" s" src/os/script-argv.f" EXPECT-ROW ;

: ASSERT-UNIQUE ( ptr u8 n n -- ) {: manifest:ptr mu:n want:n :}
   manifest mu SPLIT-LINES
   SN# @ want T=
   0 UNIQUE-I !
   begin UNIQUE-I @ SN# @ < while
      UNIQUE-I @ 1+ UNIQUE-J !
      begin UNIQUE-J @ SN# @ < while
         UNIQUE-I @ S@ UNIQUE-J @ S@ STR= 0= TTRUE
         UNIQUE-J @ 1+ UNIQUE-J !
      repeat
      UNIQUE-I @ 1+ UNIQUE-I !
   repeat ;

: ASSERT-ABSENT ( ptr u8 n -- )
   LOAD$ 2swap CONTAINS? 0= TTRUE ;

: ASSERT-EQUAL ( n -- ) {: want:n :}
   LOAD$ PATH$ T$=
   LOAD$ PROVIDE$ T$=
   LOAD$ EXPECT$ T$=
   LOAD$ want ASSERT-UNIQUE
   s" structures-effects.f" ASSERT-ABSENT ;

: CAPTURE-LOAD ( -- n )
   s" : PFX-LOAD-CHECKER-FILES" s" : PFX-PATH-CHECKER-FILES"
   MODE-FORTH s" PFX-LOAD-ROW" [: LOAD+ ;] CAPTURE ;

: CAPTURE-PATH ( -- n )
   s" : PFX-PATH-CHECKER-FILES" s" : PFX-PROVIDE-CHECKER-FILES"
   MODE-FORTH s" PFX-PATH-ROW" [: PATH+ ;] CAPTURE ;

: CAPTURE-PROVIDE ( -- n )
   s" : PFX-PROVIDE-CHECKER-FILES" s" : C-SOURCE-PIPE"
   MODE-FORTH s" PFX-PROVIDE-ROW" [: PROVIDE+ ;] CAPTURE ;

: CAPTURE-PREFIX ( -- )
   0 LOAD-U !  0 PATH-U !  0 PROVIDE-U !
   CAPTURE-LOAD SCAN-N !
   CAPTURE-PATH SCAN-N @ T=
   CAPTURE-PROVIDE SCAN-N @ T= ;

: NATIVE-ROWS ( -- )
   CAPTURE-PREFIX
   EXPECT-NATIVE
   37 ASSERT-EQUAL
   s" src/core/structures-effects.f" BCG-MUST-LACK
   s" LPSTRUCTEFF" BCG-MUST-LACK ;

: GFORTH-ROWS ( -- )
   CAPTURE-PREFIX
   EXPECT-GFORTH
   34 ASSERT-EQUAL
   s" src/core/structures-effects.f" BCG-MUST-LACK
   s" LPSTRUCTEFF" BCG-MUST-LACK ;

: EXPECT-FILE ( ptr u8 n -- )
   EXPECT+  LF 1 EXPECT+ ;

: EXPECT-RECOVERY ( -- )
   0 EXPECT-U !
   s" src/core/util.f" EXPECT-FILE
   s" src/core/cell.f" EXPECT-FILE
   s" src/core/pointer-storage.f" EXPECT-FILE
   s" src/core/engine-error.f" EXPECT-FILE
   s" src/core/checker.f" EXPECT-FILE
   s" src/core/engine-error-effects.f" EXPECT-FILE
   s" src/core/lower-cert-base.f" EXPECT-FILE
   s" src/core/type-schema.f" EXPECT-FILE
   s" src/core/type-family.f" EXPECT-FILE
   s" src/core/render.f" EXPECT-FILE
   s" src/core/sumtype.f" EXPECT-FILE
   s" src/core/layout-buffer.f" EXPECT-FILE
   s" src/core/layout-valid.f" EXPECT-FILE
   s" src/core/check-hook.f" EXPECT-FILE
   s" src/core/cell-effects.f" EXPECT-FILE
   s" src/core/pointer-storage-effects.f" EXPECT-FILE
   s" src/core/structures.f" EXPECT-FILE ;

: EXPECT-RECOVERY-COMMON ( -- )
   0 EXPECT-U !
   s" src/core/roles.f" EXPECT-FILE
   s" $OS_TARGET" EXPECT-FILE
   s" src/arch/arm64/asm.f" EXPECT-FILE
   s" src/arch/arm64/icode.f" EXPECT-FILE
   s" src/arch/arm64/mnem.f" EXPECT-FILE
   s" $OS_LAYOUT" EXPECT-FILE
   s" $OS_SYS" EXPECT-FILE
   s" src/habu/layout.f" EXPECT-FILE
   s" src/os/env-base.f" EXPECT-FILE
   s" src/os/script-argv.f" EXPECT-FILE
   s" src/core/enums.f" EXPECT-FILE
   s" src/core/exec-vector.f" EXPECT-FILE
   s" src/core/sha256.f" EXPECT-FILE
   s" src/core/type-family-sha.f" EXPECT-FILE
   s" src/core/combinators.f" EXPECT-FILE
   s" src/habu/treeshake.f" EXPECT-FILE
   s" src/habu/rt.f" EXPECT-FILE
   s" src/habu/crash.f" EXPECT-FILE
   s" src/os/image-bytes.f" EXPECT-FILE
   s" $OS_IMAGE" EXPECT-FILE
   s" $OS_SIGN" EXPECT-FILE
   s" src/habu/habu1.f" EXPECT-FILE
   s" src/habu/prof.f" EXPECT-FILE
   s" src/habu/regalloc.f" EXPECT-FILE
   s" src/habu/jit.f" EXPECT-FILE
   s" src/habu/habu2.f" EXPECT-FILE
   s" src/habu/xref.f" EXPECT-FILE
   s" src/habu/owner-wid-emit-seal.f" EXPECT-FILE
   s" src/core/layout-buffer-seal.f" EXPECT-FILE
   s" src/core/lower-cert-seal.f" EXPECT-FILE ;

: EXPECT-FIXPOINT-COMMON ( -- )
   0 EXPECT-U !
   s" BF-APPEND-ROLES" EXPECT-FILE
   s" BF-APPEND-CORE-BYTES" EXPECT-FILE
   s" BF-APPEND-TARGET-FLAG" EXPECT-FILE
   s" src/arch/arm64/asm.f" EXPECT-FILE
   s" src/arch/arm64/icode.f" EXPECT-FILE
   s" src/arch/arm64/mnem.f" EXPECT-FILE
   s" BF-APPEND-TARGET-LAYOUT" EXPECT-FILE
   s" BF-APPEND-TARGET-SYS" EXPECT-FILE
   s" BF-APPEND-HABU-LAYOUT" EXPECT-FILE
   s" BF-APPEND-ENV-BASE" EXPECT-FILE
   s" BF-APPEND-SCRIPT-ARGV" EXPECT-FILE
   s" BF-APPEND-ENUMS" EXPECT-FILE
   s" BF-APPEND-EXEC-VECTOR" EXPECT-FILE
   s" src/core/sha256.f" EXPECT-FILE
   s" src/core/type-family-sha.f" EXPECT-FILE
   s" BF-APPEND-COMBINATORS" EXPECT-FILE
   s" src/habu/treeshake.f" EXPECT-FILE
   s" src/habu/rt.f" EXPECT-FILE
   s" src/habu/crash.f" EXPECT-FILE
   s" BF-APPEND-IMAGE-BYTES" EXPECT-FILE
   s" BF-APPEND-TARGET-IMAGE" EXPECT-FILE
   s" src/habu/habu1.f" EXPECT-FILE
   s" BUILD-EXT:APPEND" EXPECT-FILE
   s" src/habu/prof.f" EXPECT-FILE
   s" src/habu/regalloc.f" EXPECT-FILE
   s" src/habu/jit.f" EXPECT-FILE
   s" src/habu/habu2.f" EXPECT-FILE
   s" src/habu/xref.f" EXPECT-FILE
   s" src/habu/owner-wid-emit-seal.f" EXPECT-FILE
   s" src/core/layout-buffer-seal.f" EXPECT-FILE
   s" src/core/lower-cert-seal.f" EXPECT-FILE ;

variable COUNT-I
variable COUNT-N

\ typed-local-lint: allow-bare-local - source/needle remain byte spans.
: COUNT-SUB ( ptr u8 n ptr u8 n -- n ) {: src su:n needle nu:n :}
   0 COUNT-I !  0 COUNT-N !
   nu 0= if 0 exit then
   begin COUNT-I @ su nu - <= while
      src COUNT-I @ + nu needle nu STR= if
         COUNT-I @ nu + COUNT-I !
         1 COUNT-N +!
      else
         1 COUNT-I +!
      then
   repeat
   COUNT-N @ ;

\ typed-local-lint: allow-bare-local - scope/needle remain source byte spans.
: SCOPE-N ( ptr u8 n ptr u8 n ptr u8 n -- n )
   {: first fu:n after au:n needle nu:n :}
   first fu after au RANGE$ needle nu COUNT-SUB ;

\ typed-local-lint: allow-bare-local - source/needles remain byte spans.
: BEFORE ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: src su:n left lu:n right ru:n :}
   src su left lu BCG-FIND MATCH option
      none OF STR-FALSE TTRUE -1 ENDOF
      some OF IDX>N ENDOF
   ;MATCH
   src su right ru BCG-FIND MATCH option
      none OF STR-FALSE TTRUE -1 ENDOF
      some OF IDX>N ENDOF
   ;MATCH < TTRUE ;

\ typed-local-lint: allow-bare-local - scope/needles remain byte spans.
: SCOPE-BEFORE ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: first fu:n after au:n left lu:n right ru:n :}
   first fu after au RANGE$ left lu right ru BEFORE ;

: SCOPE-ONE ( ptr u8 n ptr u8 n ptr u8 n -- )
   SCOPE-N 1 T= ;

\ typed-local-lint: allow-bare-local - scope/markers remain byte spans.
: SCOPE-BETWEEN ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: first:ptr fu:n after:ptr au:n left:ptr lu:n item:ptr iu:n right:ptr ru:n :}
   first fu after au left lu item iu SCOPE-BEFORE
   first fu after au item iu right ru SCOPE-BEFORE ;

\ typed-local-lint: allow-bare-local - scope/calls remain byte spans.
: CALL-CHAIN ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: first:ptr fu:n after:ptr au:n a:ptr au2:n b:ptr bu:n c:ptr cu:n :}
   first fu after au a au2 SCOPE-ONE
   first fu after au b bu SCOPE-ONE
   first fu after au c cu SCOPE-ONE
   first fu after au a au2 b bu SCOPE-BEFORE
   first fu after au b bu c cu SCOPE-BEFORE ;

: PREFIX-CALLS ( -- )
   s" : PFX-LOAD-BASE-FILES" s" : PFX-LOAD-SCRIPT-ARGV ( -- )"
      s" PFX-LOAD-CHECKER-FILES" s" PFX-LOAD-DECL-FILES"
      s" PFX-LOAD-CORE-FILES" CALL-CHAIN
   s" : PFX-PATH-FILES" s" : EMIT-HOST-LOAD-PREFIX"
      s" PFX-PATH-CHECKER-FILES" s" PFX-PATH-DECL-FILES"
      s" PFX-PATH-CORE-FILES" CALL-CHAIN
   s" : PFX-PROVIDE-FILES" s" : C-SOURCE-PIPE"
      s" PFX-PROVIDE-CHECKER-FILES" s" PFX-PROVIDE-DECL-FILES"
      s" PFX-PROVIDE-CORE-FILES" CALL-CHAIN ;

: RECOVERY-TARGETS ( -- )
   S\" case \"$HABU_TARGET\" in" s" PROBE=" s" OS_" SCOPE-N 10 T=
   S\" case \"$HABU_TARGET\" in" s" PROBE="
      s" OS_TARGET=src/os/macos/target.f" SCOPE-ONE
   S\" case \"$HABU_TARGET\" in" s" PROBE="
      s" OS_LAYOUT=src/os/macos/layout.f" SCOPE-ONE
   S\" case \"$HABU_TARGET\" in" s" PROBE="
      s" OS_SYS=src/os/macos/sys.f" SCOPE-ONE
   S\" case \"$HABU_TARGET\" in" s" PROBE="
      s" OS_IMAGE=src/os/macos/macho.f" SCOPE-ONE
   S\" case \"$HABU_TARGET\" in" s" PROBE="
      s" OS_SIGN=src/os/macos/sign2.f" SCOPE-ONE
   S\" case \"$HABU_TARGET\" in" s" PROBE="
      s" OS_TARGET=src/os/linux/target.f" SCOPE-ONE
   S\" case \"$HABU_TARGET\" in" s" PROBE="
      s" OS_LAYOUT=src/os/linux/layout.f" SCOPE-ONE
   S\" case \"$HABU_TARGET\" in" s" PROBE="
      s" OS_SYS=src/os/linux/sys.f" SCOPE-ONE
   S\" case \"$HABU_TARGET\" in" s" PROBE="
      s" OS_IMAGE=src/os/linux/elf.f" SCOPE-ONE
   S\" case \"$HABU_TARGET\" in" s" PROBE="
      s" OS_SIGN=src/os/linux/sign.f" SCOPE-ONE
   S\" case \"$HABU_TARGET\" in" s" PROBE=" s" macos-aarch64)"
      s" OS_TARGET=src/os/macos/target.f" s" linux-aarch64)" SCOPE-BETWEEN
   S\" case \"$HABU_TARGET\" in" s" PROBE=" s" macos-aarch64)"
      s" OS_LAYOUT=src/os/macos/layout.f" s" linux-aarch64)" SCOPE-BETWEEN
   S\" case \"$HABU_TARGET\" in" s" PROBE=" s" macos-aarch64)"
      s" OS_SYS=src/os/macos/sys.f" s" linux-aarch64)" SCOPE-BETWEEN
   S\" case \"$HABU_TARGET\" in" s" PROBE=" s" macos-aarch64)"
      s" OS_IMAGE=src/os/macos/macho.f" s" linux-aarch64)" SCOPE-BETWEEN
   S\" case \"$HABU_TARGET\" in" s" PROBE=" s" macos-aarch64)"
      s" OS_SIGN=src/os/macos/sign2.f" s" linux-aarch64)" SCOPE-BETWEEN
   S\" case \"$HABU_TARGET\" in" s" PROBE=" s" linux-aarch64)"
      s" OS_TARGET=src/os/linux/target.f" s" *)" SCOPE-BETWEEN
   S\" case \"$HABU_TARGET\" in" s" PROBE=" s" linux-aarch64)"
      s" OS_LAYOUT=src/os/linux/layout.f" s" *)" SCOPE-BETWEEN
   S\" case \"$HABU_TARGET\" in" s" PROBE=" s" linux-aarch64)"
      s" OS_SYS=src/os/linux/sys.f" s" *)" SCOPE-BETWEEN
   S\" case \"$HABU_TARGET\" in" s" PROBE=" s" linux-aarch64)"
      s" OS_IMAGE=src/os/linux/elf.f" s" *)" SCOPE-BETWEEN
   S\" case \"$HABU_TARGET\" in" s" PROBE=" s" linux-aarch64)"
      s" OS_SIGN=src/os/linux/sign.f" s" *)" SCOPE-BETWEEN ;

: FIXPOINT-TARGETS ( -- )
   s" : BF-APPEND-TARGET-LAYOUT" s" : BF-APPEND-TARGET-SYS"
      s" src/os/" SCOPE-N 2 T=
   s" : BF-APPEND-TARGET-LAYOUT" s" : BF-APPEND-TARGET-SYS"
      s" src/os/linux/layout.f" SCOPE-ONE
   s" : BF-APPEND-TARGET-LAYOUT" s" : BF-APPEND-TARGET-SYS"
      s" src/os/macos/layout.f" SCOPE-ONE
   s" : BF-APPEND-TARGET-LAYOUT" s" : BF-APPEND-TARGET-SYS"
      s" HB-TARGET-LINUX?" s" src/os/linux/layout.f" SCOPE-BEFORE
   s" : BF-APPEND-TARGET-LAYOUT" s" : BF-APPEND-TARGET-SYS"
      s" src/os/linux/layout.f" s" HB-TARGET-MACOS?" SCOPE-BEFORE
   s" : BF-APPEND-TARGET-LAYOUT" s" : BF-APPEND-TARGET-SYS"
      s" HB-TARGET-MACOS?" s" src/os/macos/layout.f" SCOPE-BEFORE
   s" : BF-APPEND-TARGET-SYS" s" : BF-APPEND-TARGET-FLAG"
      s" src/os/" SCOPE-N 2 T=
   s" : BF-APPEND-TARGET-SYS" s" : BF-APPEND-TARGET-FLAG"
      s" src/os/linux/sys.f" SCOPE-ONE
   s" : BF-APPEND-TARGET-SYS" s" : BF-APPEND-TARGET-FLAG"
      s" src/os/macos/sys.f" SCOPE-ONE
   s" : BF-APPEND-TARGET-SYS" s" : BF-APPEND-TARGET-FLAG"
      s" HB-TARGET-LINUX?" s" src/os/linux/sys.f" SCOPE-BEFORE
   s" : BF-APPEND-TARGET-SYS" s" : BF-APPEND-TARGET-FLAG"
      s" src/os/linux/sys.f" s" HB-TARGET-MACOS?" SCOPE-BEFORE
   s" : BF-APPEND-TARGET-SYS" s" : BF-APPEND-TARGET-FLAG"
      s" HB-TARGET-MACOS?" s" src/os/macos/sys.f" SCOPE-BEFORE
   s" : BF-APPEND-TARGET-FLAG" s" : BF-APPEND-IMAGE-BYTES"
      s" src/os/" SCOPE-N 2 T=
   s" : BF-APPEND-TARGET-FLAG" s" : BF-APPEND-IMAGE-BYTES"
      s" src/os/linux/target.f" SCOPE-ONE
   s" : BF-APPEND-TARGET-FLAG" s" : BF-APPEND-IMAGE-BYTES"
      s" src/os/macos/target.f" SCOPE-ONE
   s" : BF-APPEND-TARGET-FLAG" s" : BF-APPEND-IMAGE-BYTES"
      s" HB-TARGET-LINUX?" s" src/os/linux/target.f" SCOPE-BEFORE
   s" : BF-APPEND-TARGET-FLAG" s" : BF-APPEND-IMAGE-BYTES"
      s" src/os/linux/target.f" s" HB-TARGET-MACOS?" SCOPE-BEFORE
   s" : BF-APPEND-TARGET-FLAG" s" : BF-APPEND-IMAGE-BYTES"
      s" HB-TARGET-MACOS?" s" src/os/macos/target.f" SCOPE-BEFORE
   s" : BF-APPEND-TARGET-IMAGE" s" : BF-APPEND-ROLES"
      s" src/os/" SCOPE-N 4 T=
   s" : BF-APPEND-TARGET-IMAGE" s" : BF-APPEND-ROLES"
      s" src/os/linux/elf.f" SCOPE-ONE
   s" : BF-APPEND-TARGET-IMAGE" s" : BF-APPEND-ROLES"
      s" src/os/linux/sign.f" SCOPE-ONE
   s" : BF-APPEND-TARGET-IMAGE" s" : BF-APPEND-ROLES"
      s" src/os/macos/macho.f" SCOPE-ONE
   s" : BF-APPEND-TARGET-IMAGE" s" : BF-APPEND-ROLES"
      s" src/os/macos/sign2.f" SCOPE-ONE
   s" : BF-APPEND-TARGET-IMAGE" s" : BF-APPEND-ROLES"
      s" HB-TARGET-LINUX?" s" src/os/linux/elf.f" SCOPE-BEFORE
   s" : BF-APPEND-TARGET-IMAGE" s" : BF-APPEND-ROLES"
      s" src/os/linux/elf.f" s" src/os/linux/sign.f" SCOPE-BEFORE
   s" : BF-APPEND-TARGET-IMAGE" s" : BF-APPEND-ROLES"
      s" src/os/linux/sign.f" s" HB-TARGET-MACOS?" SCOPE-BEFORE
   s" : BF-APPEND-TARGET-IMAGE" s" : BF-APPEND-ROLES"
      s" HB-TARGET-MACOS?" s" src/os/macos/macho.f" SCOPE-BEFORE
   s" : BF-APPEND-TARGET-IMAGE" s" : BF-APPEND-ROLES"
      s" src/os/macos/macho.f" s" src/os/macos/sign2.f" SCOPE-BEFORE ;

public

: NATIVE ( -- )
   NATIVE-ROWS
   PREFIX-CALLS ;

: GFORTH ( -- )
   GFORTH-ROWS
   PREFIX-CALLS ;

: RECOVERY ( -- )
   0 RECOVERY-U !
   s" emit_src() {" s"   local f" MODE-CAT s" cat"
   [: RECOVERY+ ;] CAPTURE 17 T=
   EXPECT-RECOVERY
   RECOVERY$ EXPECT$ T$=
   RECOVERY$ 17 ASSERT-UNIQUE
   s" emit_decl_src() {" s" emit_src() {" s" cat src/" SCOPE-N 0 T=
   s" emit_src() {" s"   local f" S\" emit_decl_src \"$out\"" SCOPE-N 1 T=
   s" emit_src() {" s"   local f" s" LOWER-CERT-HOOK:INSTALL" SCOPE-N 1 T=
   s" emit_src() {" s"   local f"
      s" src/core/pointer-storage-effects.f" S\" emit_decl_src \"$out\"" SCOPE-BEFORE
   s" emit_src() {" s"   local f"
      S\" emit_decl_src \"$out\"" s" src/core/structures.f" SCOPE-BEFORE
   s" emit_src() {" s"   local f"
      s" src/core/structures.f" s" LOWER-CERT-HOOK:INSTALL" SCOPE-BEFORE
   0 RECOVERY-U !
   s" SRC_COMMON=(" s" emit_boot_hide() {" MODE-ARRAY s" "
      [: RECOVERY+ ;] CAPTURE 30 T=
   EXPECT-RECOVERY-COMMON
   RECOVERY$ EXPECT$ T$=
   RECOVERY$ 30 ASSERT-UNIQUE
   RECOVERY-TARGETS ;

: FIXPOINT ( -- )
   0 FIXPOINT-U !
   s" : BF-APPEND-CHECKER-BOOT" s" : BF-APPEND-CORE-BYTES"
   MODE-SOURCE s" BF-APPEND-SOURCE" [: FIXPOINT+ ;] CAPTURE 17 T=
   EXPECT-RECOVERY
   FIXPOINT$ EXPECT$ T$=
   FIXPOINT$ 17 ASSERT-UNIQUE
   s" : BF-APPEND-DECL-FILES" s" : BF-APPEND-CORE-FILES"
   s" BF-APPEND-SOURCE" SCOPE-N 0 T=
   s" : BF-APPEND-RUN-PRELUDE" s" : BF-APPEND-STDIN-RUN-PRELUDE"
   s" BF-APPEND-CHECKER-BOOT" SCOPE-N 1 T=
   s" : BF-APPEND-RUN-PRELUDE" s" : BF-APPEND-STDIN-RUN-PRELUDE"
   s" BF-APPEND-DECL-FILES" SCOPE-N 1 T=
   s" : BF-APPEND-RUN-PRELUDE" s" : BF-APPEND-STDIN-RUN-PRELUDE"
   s" BF-APPEND-CORE-FILES" SCOPE-N 1 T=
   s" : BF-APPEND-RUN-PRELUDE" s" : BF-APPEND-STDIN-RUN-PRELUDE"
   s" LOWER-CERT-HOOK:INSTALL" SCOPE-N 1 T=
   s" : BF-APPEND-RUN-PRELUDE" s" : BF-APPEND-STDIN-RUN-PRELUDE"
      s" BF-APPEND-CHECKER-BOOT" s" BF-APPEND-DECL-FILES" SCOPE-BEFORE
   s" : BF-APPEND-RUN-PRELUDE" s" : BF-APPEND-STDIN-RUN-PRELUDE"
      s" BF-APPEND-DECL-FILES" s" BF-APPEND-CORE-FILES" SCOPE-BEFORE
   s" : BF-APPEND-RUN-PRELUDE" s" : BF-APPEND-STDIN-RUN-PRELUDE"
      s" BF-APPEND-CORE-FILES" s" LOWER-CERT-HOOK:INSTALL" SCOPE-BEFORE
   0 FIXPOINT-U !
   s" : BF-APPEND-COMMON" s" : BF-APPEND-DRIVER-IO" MODE-COMMON s" "
      [: FIXPOINT+ ;] CAPTURE 31 T=
   EXPECT-FIXPOINT-COMMON
   FIXPOINT$ EXPECT$ T$=
   FIXPOINT$ 31 ASSERT-UNIQUE
   FIXPOINT-TARGETS ;

;package

: BCG-TEST-INSTALL-FAIL-CLOSED ( -- )
   s" bootstrap/cg/install.fs" BCG-LOAD
   s" : BODY-ARITY ( -- n )  ['] TRY-ARITY CG-CATCH ;" BCG-MUST-HAVE
   s" ['] TRY-EFFECT CG-CATCH" BCG-MUST-HAVE
   s" catch if 1" BCG-MUST-LACK
   s" catch if 0" BCG-MUST-LACK
   s" NM@ CAP$ BODY-ARITY EFFECT-FLAGS CG-RECORD" BCG-MUST-HAVE ;

: BCG-TEST-FORTH-SDQ-COMMENT ( -- )
   s" bootstrap/cg/forth.fs" BCG-LOAD
   s" C-ADR PC-relative" BCG-MUST-HAVE
   s" push abs-addr" BCG-MUST-LACK
   s" absolute address is known" BCG-MUST-LACK ;

: BCG-TEST-PREFIX-LIST-COMMON ( -- )
   s" PFX-LOAD-FILES" BCG-MUST-HAVE
   s" PFX-PATH-FILES" BCG-MUST-HAVE
   s" PFX-FILES" BCG-MUST-LACK
   s" PFX-ROW" BCG-MUST-LACK
   s" PFX-LINUX  LPLINUXTARGET" BCG-MUST-HAVE
   s" PFX-MACOS  LPMACOSTARGET" BCG-MUST-HAVE
   s" a u ZBYTES," BCG-MUST-HAVE
   s" LPUTIL @ ADR" BCG-MUST-LACK
   s" LSRCRD @ BL then" BCG-MUST-LACK
   s" a u ZBYTES ;" BCG-MUST-LACK
   s" LPLINUXTARGET @ LBL, s" BCG-MUST-LACK ;

: BCG-TEST-PREFIX-LIST-BOOTSTRAP ( -- )
   s" bootstrap/cg/forth.fs" BCG-LOAD
   BCG-TEST-PREFIX-LIST-COMMON
   BCG-MANIFEST:GFORTH
   s" LSRCRD @ BL," BCG-MUST-HAVE
   s" LSRCRD LABEL@ BL," BCG-MUST-LACK ;

: BCG-TEST-PREFIX-LIST-NATIVE ( -- )
   s" src/habu/habu2.f" BCG-LOAD
   BCG-TEST-PREFIX-LIST-COMMON
   BCG-MANIFEST:NATIVE
   s" LSRCRD LABEL@ BL," BCG-MUST-HAVE ;

: BCG-TEST-PREFIX-LIST ( -- )
   BCG-TEST-PREFIX-LIST-BOOTSTRAP
   BCG-TEST-PREFIX-LIST-NATIVE ;

: BCG-TEST-TOK-IMM-MIRROR ( -- )
   s" bootstrap/cg/forth.fs" BCG-LOAD
   s" : BTOKIMM ( -- )" BCG-MUST-HAVE
   s" LFIND @ BL," BCG-MUST-HAVE
   s" 9 13 2 ANDI," BCG-MUST-HAVE
   s" ['] BTOKIMM FPRIM" BCG-MUST-HAVE
   s" src/habu/habu2.f" BCG-LOAD
   s" : BTOKIMM ( -- )" BCG-MUST-HAVE
   s" LFIND LABEL@ BL," BCG-MUST-HAVE
   s" 9 13 2 ANDI," BCG-MUST-HAVE
   s" ['] BTOKIMM 2 GDEREF-F" BCG-MUST-HAVE ;

: BCG-TEST-CELL-RUNTIME ( -- )
   CELL-WIDTH-CHECK
   CELL 1 cells T= ;

: BCG-TEST-ENGINE-ERROR ( -- )
   s" src/core/engine-error.f" BCG-LOAD
   s" package ENGINE-ERROR" BCG-MUST-HAVE
   s" 83 constant SEAL-VIOLATION" BCG-MUST-HAVE
   s" 84 constant SEAL-PACKAGE" BCG-MUST-HAVE
   s" 85 constant BAD-TAG" BCG-MUST-HAVE
   s" 86 constant CALLABLE-ABI" BCG-MUST-HAVE
   s" 87 constant CATCH-STACK" BCG-MUST-HAVE
   s" 88 constant CODE-CERT" BCG-MUST-HAVE
   s" constant E-SEAL-VIOLATION" BCG-MUST-LACK
   s" bootstrap/cg/forth.fs" BCG-LOAD
   s" 83 constant ENGINE-ERROR:SEAL-VIOLATION" BCG-MUST-HAVE
   s" 84 constant ENGINE-ERROR:SEAL-PACKAGE" BCG-MUST-HAVE
   s" 85 constant ENGINE-ERROR:BAD-TAG" BCG-MUST-HAVE
   s" 86 constant ENGINE-ERROR:CALLABLE-ABI" BCG-MUST-HAVE
   s" 87 constant ENGINE-ERROR:CATCH-STACK" BCG-MUST-HAVE
   s" 88 constant ENGINE-ERROR:CODE-CERT" BCG-MUST-HAVE
   s" : C-P2-FIND-GLOBAL?" BCG-MUST-HAVE
   s" : C-P2-FIND-CHECKER" BCG-MUST-HAVE
   s" FRIEND-LATCH-CELL LDR,  9 done CBZ," BCG-MUST-HAVE
   s" src/habu/habu2.f" BCG-LOAD
   s" : C-FIND-GLOBAL?" BCG-MUST-HAVE
   s" : C-FIND-CHECKER" BCG-MUST-HAVE
   s" FRIEND-LATCH-CELL LDR,  9 done CBZ," BCG-MUST-HAVE
   s" src/core/engine-error-effects.f" BCG-LOAD
   s" package ENGINE-ERROR" BCG-MUST-HAVE
   S\" s\" SEAL-VIOLATION\" s\" -- n\" TRUST" BCG-MUST-HAVE
   S\" s\" CODE-CERT\" s\" -- n\" TRUST" BCG-MUST-HAVE
   s" tools/bootstrap.sh" BCG-LOAD
   s" test/engine-error-package.f" BCG-MUST-HAVE ;

: BCG-TEST-CELL-SOURCE ( -- )
   s" src/core/cell.f" BCG-LOAD
   s" $8 constant CELL" BCG-MUST-HAVE
   s" $4C constant CORE-LAYOUT-RC" BCG-MUST-HAVE
   s" 1 cells CELL <>" BCG-MUST-HAVE
   s" CORE-LAYOUT-RC die" BCG-MUST-HAVE ;

: BCG-TEST-CELL-BOOTSTRAP ( -- )
   s" tools/bootstrap.sh" BCG-LOAD
   BCG-MANIFEST:RECOVERY
   s" cat src/core/structures-effects.f" BCG-MUST-LACK ;

: BCG-TEST-CELL-FIXPOINT ( -- )
   s" tools/build-fixpoint.f" BCG-LOAD
   BCG-MANIFEST:FIXPOINT
   s" src/core/structures-effects.f" BCG-MUST-LACK ;

: BCG-TEST-CELL-PARITY ( -- )
   BCG-TEST-CELL-RUNTIME
   BCG-TEST-CELL-SOURCE
   BCG-TEST-CELL-BOOTSTRAP
   BCG-TEST-CELL-FIXPOINT ;

: BCG-TEST-BAKED-SOURCE-PREFIX-CURRENT ( -- )
   s" : C-SOURCE-BAKED" BCG-POS-FOUND {: start :}
   start s" : EMIT-SOURCE" BCG-AFTER-FOUND {: end:n :}
   start end s" EMIT-COLD-PREFIX" BCG-MUST-FIND-BEFORE ;

: BCG-TEST-BAKED-SOURCE-PREFIX ( -- )
   s" bootstrap/cg/forth.fs" BCG-LOAD
   BCG-TEST-BAKED-SOURCE-PREFIX-CURRENT
   s" src/habu/habu2.f" BCG-LOAD
   BCG-TEST-BAKED-SOURCE-PREFIX-CURRENT ;

: BCG-TEST-TRUST-CALLS-CURRENT ( -- )
   s" : C-PUSH-DATA-CELL ( n -- )" BCG-MUST-HAVE
   s" : C-PUSH-TRUST-SIG ( n n -- )" BCG-MUST-HAVE
   s" : C-CALL-X11-SAVED ( -- )" BCG-MUST-HAVE
   s" CRSIG-A-CELL CRSIG-U-CELL C-PUSH-TRUST-SIG" BCG-MUST-HAVE
   s" 9 DATA CRSIG-A-CELL LDR,  9 G-PUSH" BCG-MUST-LACK
   s" 9 DATA CRSIG-U-CELL LDR,  9 G-PUSH" BCG-MUST-LACK ;

: BCG-TEST-TRUST-CALLS ( -- )
   s" bootstrap/cg/forth.fs" BCG-LOAD
   BCG-TEST-TRUST-CALLS-CURRENT
   s" src/habu/habu2.f" BCG-LOAD
   BCG-TEST-TRUST-CALLS-CURRENT
   s" TSIG-A-CELL TSIG-U-CELL C-PUSH-TRUST-SIG" BCG-MUST-HAVE
   s" 9 DATA TSIG-A-CELL LDR,  9 G-PUSH" BCG-MUST-LACK
   s" 9 DATA TSIG-U-CELL LDR,  9 G-PUSH" BCG-MUST-LACK ;

: BCG-TEST-IMAGE-BUFFER-CURRENT ( -- )
   s" require image.fs" BCG-MUST-HAVE
   s" $90000 constant MSIZE" BCG-MUST-LACK
   s" create MBUF MSIZE allot" BCG-MUST-LACK
   s" variable MP" BCG-MUST-LACK
   s" variable MLEN" BCG-MUST-LACK
   s" : M8" BCG-MUST-LACK
   s" : M16" BCG-MUST-LACK
   s" : M32" BCG-MUST-LACK
   s" : M64" BCG-MUST-LACK
   s" SCODE CODELEN @ M-BYTES" BCG-MUST-HAVE ;

: BCG-TEST-IMAGE-BUFFER ( -- )
   s" bootstrap/cg/image.fs" BCG-LOAD
   s" create MBUF MSIZE allot" BCG-MUST-HAVE
   s" : M-BYTES ( addr u -- )" BCG-MUST-HAVE
   s" : M-NAME16 ( addr u -- )" BCG-MUST-HAVE
   s" bootstrap/cg/elf.fs" BCG-LOAD
   BCG-TEST-IMAGE-BUFFER-CURRENT
   s" bootstrap/cg/macho.fs" BCG-LOAD
   BCG-TEST-IMAGE-BUFFER-CURRENT ;

: BCG-TEST-ASM-CHECKED ( -- )
   s" bootstrap/cg/asm-checked.fs" BCG-LOAD
   s" : A-RRR16 ( reg reg n n -- n )" BCG-MUST-HAVE
   s" : A-RRI10 ( reg reg n n -- n )" BCG-MUST-HAVE
   s" : A-MOVW ( reg n n n -- n )" BCG-MUST-HAVE
   s" : A-LS-UOFF ( reg reg off n -- n )" BCG-MUST-HAVE
   s" 2332033024 A-RRR16" BCG-MUST-HAVE
   s" $9AC00C00 A-RRR16" BCG-MUST-HAVE
   s" $D63F0000 A-R1-5" BCG-MUST-HAVE
   s" 16 lshift swap 5 lshift or swap or" BCG-MUST-LACK
   s" 10 lshift swap 5 lshift or swap or" BCG-MUST-LACK ;

: BCG-TEST-GFORTH-LOCALS ( -- )
   s" bootstrap/cg/forth.fs" BCG-LOAD
   s" done:label" BCG-MUST-LACK
   s" qexit:label" BCG-MUST-LACK
   s" qlok:label" BCG-MUST-LACK ;

: BCG-TEST-GFORTH-LOCAL-CAPTURE ( -- )
   s" bootstrap/cg/forth.fs" BCG-LOAD
   s" : EMIT-COMPILE-LOCAL" BCG-POS-FOUND {: start:n :}
   start s" : EMIT-COMPILE-LITERAL" BCG-AFTER-FOUND {: end:n :}
   start end s" LBCAP @ BL" BCG-MUST-FIND-BEFORE
   start end s" QPATCH-CELL" BCG-MUST-FIND-BEFORE
   start end s" LVRALLOC" BCG-MUST-FIND-BEFORE ;

: BCG-TEST-LINUX-SPAWN-SCOPED-LABELS ( -- )
   s" src/habu/habu1.f" BCG-LOAD
   s" : LINUX-SPAWN-PREP-W" BCG-POS-FOUND {: start:n :}
   start s" : BRUNRC" BCG-AFTER-FOUND {: end:n :}
   start end s" LNX-DONE LABEL@ B" BCG-MUST-NOT-FIND-BEFORE
   start end s" LNX-DONE LABEL@ LBL" BCG-MUST-NOT-FIND-BEFORE
   start end s" LNX-FAIL LABEL@ B" BCG-MUST-NOT-FIND-BEFORE
   start end s" LNX-FAIL LABEL@ LBL" BCG-MUST-NOT-FIND-BEFORE
   start end s" LNX-OK LABEL@ B" BCG-MUST-NOT-FIND-BEFORE
   start end s" LNX-OK LABEL@ LBL" BCG-MUST-NOT-FIND-BEFORE
   start end s" child:label" BCG-MUST-FIND-BEFORE
   start end s" done:label" BCG-MUST-FIND-BEFORE ;

: BCG-TEST-BOOTSTRAP-DATA-SIZE ( -- )
   s" bootstrap/cg/forth.fs" BCG-LOAD
   s" $2000000 constant DATA-SIZE" BCG-MUST-HAVE
   s" $300000 constant DATA-SIZE" BCG-MUST-LACK
   s" src/os/linux/layout.f" BCG-LOAD
   s" $2000000 constant DATA-SIZE" BCG-MUST-HAVE
   s" $300000 constant DATA-SIZE" BCG-MUST-LACK ;

: BCG-TEST-PROF-CNT-HIGH ( -- )
   s" bootstrap/cg/prof.fs" BCG-LOAD
   s" DATA-SIZE $10000 - constant PROF-CNT" BCG-MUST-HAVE
   s" $1F0000 constant PROF-CNT" BCG-MUST-LACK
   s" src/habu/prof.f" BCG-LOAD
   s" DATA-SIZE $10000 - constant PROF-CNT" BCG-MUST-HAVE
   s" $1F0000 constant PROF-CNT" BCG-MUST-LACK ;

: BCG-TEST-PUBLISH-HOOK-SPLIT ( -- )
   s" bootstrap/cg/forth.fs" BCG-LOAD
   s" : EMIT-COMPILE-PUBLISH-TRUSTED" BCG-MUST-HAVE
   s" : EMIT-COMPILE-PUBLISH-HOOKED" BCG-MUST-HAVE
   s" : EMIT-COMPILE-PUBLISH ( n -- )" BCG-MUST-HAVE
   s" BODYBUF-OFF ADDI,  10 G-PUSH" BCG-MUST-HAVE
   s" C-CALL-TRUST-PEND-MAYBE" BCG-MUST-LACK ;

: BCG-TEST-BOOTSTRAP-LOCAL-SHADOW ( -- )
   s" bootstrap/cg/forth.fs" BCG-LOAD
   s" lmain EMIT-COMPILE-LOCAL" s" lmain EMIT-COMPILE-KEYWORDS" BCG-MUST-BEFORE
   s" : J-CASE ( -- )" BCG-MUST-HAVE
   s" : J-OF ( -- )" BCG-MUST-HAVE
   s" : J-ENDOF ( -- )" BCG-MUST-HAVE
   s" : J-ENDCASE ( -- )" BCG-MUST-HAVE
   s" : J-MATCH ( -- )" BCG-MUST-HAVE
   s" : C-DIE-BAD-TAG ( -- )" BCG-MUST-HAVE ;

: BCG-TEST-BOOTSTRAP-HIDE-PRELUDE ( -- )
   s" tools/bootstrap.sh" BCG-LOAD
   s" BOOT-USIGS-RESET" BCG-MUST-HAVE
   s" SEQ" BCG-MUST-HAVE
   s" IMK-NDICT0" BCG-MUST-HAVE                \ replay hides from util.f's FIRST record (the int-mark watermark), mirroring BFR-HIDE-DICT-FROM-EARLIEST
   s" BOOT-HIDE-DICT-FROM-EARLIEST" BCG-MUST-HAVE
   s" T-CON" BCG-MUST-LACK ;

\ --- earliest-marker hide behavior ---
\ tools/bootstrap.sh's BOOT-* hide prelude mirrors src/habu/hide.f's BFR-*
\ words, so the native mirror is the executable spec and is driven directly
\ below; no shell is spawned (that would add host-glue surface), so the script
\ body itself stays pinned by the substring assertions above. hide.f is baked
\ into the engine prelude and truncated away after use, so `require` would be
\ skipped as already provided; include reloads the BFR-* words here.
include src/habu/hide.f

variable BCGH-MID                            \ ndict watermark between the duplicate fixture records

package BCGH-EARLY
public
: BCGH-DUP-MARK ( -- ) ;
;package

ndict@ BCGH-MID !

package BCGH-LATE
public
: BCGH-DUP-MARK ( -- ) ;
;package

: BCGH-FIND ( ptr u8 n -- n )
   BFR-FIND-FIRST-INDEX ;

: BCGH-IMK ( -- n )
   s" IMK-NDICT0" BCGH-FIND ;

: BCGH-SEQ ( -- n )
   s" SEQ" BCGH-FIND ;

\ The production markers exist in the live dictionary with IMK-NDICT0 (util.f's
\ first record) earlier than SEQ; the hide index must pick the earlier record
\ in either argument order.
: BCG-TEST-HIDE-EARLIEST-MARKER ( -- )
   BCGH-IMK 0 >= TTRUE
   BCGH-SEQ 0 >= TTRUE
   BCGH-IMK BCGH-SEQ < TTRUE
   s" IMK-NDICT0" s" SEQ" BFR-MARKER-INDEX BCGH-IMK T=
   s" SEQ" s" IMK-NDICT0" BFR-MARKER-INDEX BCGH-IMK T= ;

\ Earliest-hide depends on FIND-FIRST returning the FIRST record of a name:
\ the duplicate fixture record defined before the BCGH-MID watermark must win,
\ and the match must fold case like the shell's BOOT-XREF-STR=CI.
: BCG-TEST-HIDE-FIRST-RECORD ( -- )
   s" BCGH-DUP-MARK" BCGH-FIND 0 >= TTRUE
   s" BCGH-DUP-MARK" BCGH-FIND BCGH-MID @ < TTRUE
   s" bcgh-dup-mark" BCGH-FIND s" BCGH-DUP-MARK" BCGH-FIND T= ;

\ One marker missing falls back to the found one; both missing is asserted at
\ the component level (FIND -> NOT-FOUND, MIN-FOUND keeps NOT-FOUND) because
\ BFR-MARKER-INDEX's both-missing path is a process exit (die 76) by design.
: BCG-TEST-HIDE-MISSING-FALLBACK ( -- )
   s" IMK-NDICT0" s" BCGH-ABSENT-MARKER" BFR-MARKER-INDEX BCGH-IMK T=
   s" BCGH-ABSENT-MARKER" s" IMK-NDICT0" BFR-MARKER-INDEX BCGH-IMK T=
   s" BCGH-ABSENT-MARKER" BCGH-FIND BFR-NOT-FOUND T=
   BFR-NOT-FOUND BFR-NOT-FOUND BFR-MIN-FOUND BFR-NOT-FOUND T=
   5 BFR-REQUIRE-INDEX 5 T= ;

: BCG-TEST-BOOTSTRAP-SMALL-BIN ( -- )
   s" tools/bootstrap.sh" BCG-LOAD
   s" hb-new" BCG-MUST-LACK
   s" hb-snap-src" BCG-MUST-LACK
   s" hb-snap0" BCG-MUST-LACK
   s" bootstrap check OK: %s/hb-stdin" BCG-MUST-HAVE
   s" mv " BCG-MUST-HAVE
   s" bin/hb" BCG-MUST-HAVE ;

: BCG-TEST-OWNER-PERSIST ( -- )
   s" bootstrap/cg/forth.fs" BCG-LOAD
   s" 3 constant SNAP-FORMAT-VERSION" BCG-MUST-HAVE
   s" 1 constant OWNER-API-PUB-WID" BCG-MUST-HAVE
   s" 2 constant OWNER-API-PRI-WID" BCG-MUST-HAVE
   s" 3 constant FIRST-DYNAMIC-WID" BCG-MUST-HAVE
   s" 256 constant RSTK-CELLS" BCG-MUST-HAVE
   s" $47C0 constant OWNER-WID-N-CELL" BCG-MUST-HAVE
   s" create PWID PRIM-CAP cells allot" BCG-MUST-HAVE
   S\" s\" FINALIZE\" ['] BOWNERFINALIZE OWNER-API-PUB-WID FPRIM-WID" BCG-MUST-HAVE
   s" LNCOUNT @ LBL,  #PL @ 1+ DCQ," BCG-MUST-HAVE
   s" OWNER-API-PUB-WID DCQ," BCG-MUST-HAVE
   s" OWNER-API-PRI-WID DCQ," BCG-MUST-HAVE
   s" : EMIT-SNAPSHOT-VALIDATE-WIDS" BCG-MUST-HAVE
   s" 13 9 40 LDR,  14 0 MOVN,  13 14 CMP,  C-EQ sds2 BCOND," BCG-MUST-HAVE
   s" 22 22 48 SUBI," BCG-MUST-HAVE
   s" 14 5 CMP,  C-NE snbadver BCOND," BCG-MUST-HAVE
   s" 6 FIRST-DYNAMIC-WID CMPI,  C-LT bad BCOND," BCG-MUST-HAVE
   s" 13 LSRC @ ADR,  14 13 25 SUB," BCG-MUST-HAVE
   s" C-GT snpresent BCOND," BCG-MUST-HAVE
   s" 4 4 DBASE SUB,  4 4 25 ADD," BCG-MUST-HAVE
   s" 9 FIRST-DYNAMIC-WID MOVZ,  9 DATA WIDN-CELL STR," BCG-MUST-HAVE ;

: BCG-TEST-OWNER-PUBLISH ( -- )
   s" src/habu/habu1.f" BCG-LOAD
   s" 14 15 LDAR," BCG-MUST-HAVE
   s" 14 15 STLR," BCG-MUST-HAVE
   s" src/habu/habu2.f" BCG-LOAD
   s" 11 5 STLR," BCG-MUST-HAVE
   s" src/habu/aot-capture.f" BCG-LOAD
   s" AOT-LIVE-DATA PROT-WID-N-CELL + atomic@" BCG-MUST-HAVE
   s" AOT-LIVE-DATA PROT-WID-N-CELL + AOT-CELL@" BCG-MUST-LACK
   s" AOT-LIVE-DATA OWNER-WID-N-CELL + atomic@" BCG-MUST-HAVE
   s" variable OWNER-PACKAGE-K" BCG-MUST-HAVE
   s" variable OWNER-PACKAGE-REC" BCG-MUST-LACK
   s" tools/build-fixpoint.f" BCG-LOAD
   s" PTR-VARIABLE KEEP-A" BCG-MUST-HAVE
   s" variable KEEP-A" BCG-MUST-LACK ;

: BCG-MAIN ( -- )
   T-RESET
   BCG-TEST-INSTALL-FAIL-CLOSED
   BCG-TEST-FORTH-SDQ-COMMENT
   BCG-TEST-PREFIX-LIST
   BCG-TEST-TOK-IMM-MIRROR
   BCG-TEST-ENGINE-ERROR
   BCG-TEST-CELL-PARITY
   BCG-CAP:TEST
   BCG-TEST-BAKED-SOURCE-PREFIX
   BCG-TEST-TRUST-CALLS
   BCG-TEST-IMAGE-BUFFER
   BCG-TEST-ASM-CHECKED
   BCG-TEST-GFORTH-LOCALS
   BCG-TEST-GFORTH-LOCAL-CAPTURE
   BCG-TEST-LINUX-SPAWN-SCOPED-LABELS
   BCG-TEST-BOOTSTRAP-DATA-SIZE
   BCG-TEST-PROF-CNT-HIGH
   BCG-TEST-PUBLISH-HOOK-SPLIT
   BCG-TEST-BOOTSTRAP-LOCAL-SHADOW
   BCG-TEST-BOOTSTRAP-HIDE-PRELUDE
   BCG-TEST-HIDE-EARLIEST-MARKER
   BCG-TEST-HIDE-FIRST-RECORD
   BCG-TEST-HIDE-MISSING-FALLBACK
   BCG-TEST-OWNER-PERSIST
   BCG-TEST-OWNER-PUBLISH
   BCG-TEST-BOOTSTRAP-SMALL-BIN
   T-REPORT
   s" bootstrap-codegen-test: ok" type cr ;

BCG-MAIN
