\ bootstrap-codegen-test.f - native source regression for bootstrap codegen hard cutover.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f tools/bootstrap-codegen-test.f
\
\ Package layout.  BCG is short for "bootstrap codegen", the subject of this
\ regression.  Package BCG owns the source-under-test buffer, the substring
\ assertion vocabulary its public section exports, and the individual checks plus
\ the MAIN entry the file runs at the end.  The narrower sub-concerns keep the
\ sibling packages they already had - BCG-CAP for the shared arena constants,
\ BCG-MANIFEST for the prefix/manifest row capture, BCG-PREFLIGHT for the compile
\ preflight hook, BCG-USING for the `using` band and keywords, and BCG-HIDE for
\ the earliest-marker hide behaviour - and each of them imports BCG's assertion
\ words once with `using BCG`.

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

\ typed STR:FIND-SUB boundary: route byte-lengths through the STR: role surface,
\ project the option<CAD-NUM:index> result back to the switchover option<idx>.
package CAD-NUM
public
: BCG-IX>N ( CAD-NUM:index -- n ) INDEX>N ;
;package

package BCG
private

\ habu2.f is 262,867 bytes on the ENGINE-ERROR cutover tree; 25% headroom is
\ 328,584 bytes, so the next power-of-two arena is $80000.
$80000 constant SRC-CAP

create SRC-BUF SRC-CAP allot
variable SRC-LEN

public

: SRC ( -- ptr u8 n )
   SRC-BUF SRC-LEN @ ;

: LOAD ( ptr u8 n -- )
   SRC-BUF SRC-CAP READ-ALL SRC-LEN ! ;

: HAS? ( ptr u8 n -- bool )
   SRC 2swap CONTAINS? ;

: MUST-HAVE ( ptr u8 n -- )
   HAS? TTRUE ;

: MUST-LACK ( ptr u8 n -- )
   HAS? 0= TTRUE ;

: FIND-IN ( ptr u8 n ptr u8 n -- option<idx> ) {: a:ptr u:n b:ptr v:n :}
   a u STR:LENGTH b v STR:LENGTH STR:FIND-SUB MATCH option
     none OF OPTION:NONE ENDOF
     some OF CAD-NUM:BCG-IX>N >IDX OPTION:SOME ENDOF
   ;MATCH ;

: POS ( ptr u8 n -- option<idx> )
   SRC 2swap FIND-IN ;

: POS-FOUND ( ptr u8 n -- n )
   POS MATCH option
     none OF STR-FALSE TTRUE -1 ENDOF
     some OF STR-TRUE TTRUE IDX>N ENDOF
   ;MATCH ;

: FIND-AFTER ( n ptr u8 n -- option<idx> ) {: start:n needle:ptr nu:n :}
   SRC {: src:ptr srcu:n :}
   start 0 < if OPTION:NONE exit then
   start srcu >= if OPTION:NONE exit then
   src start + srcu start - needle nu FIND-IN MATCH option
     none OF OPTION:NONE ENDOF
     some OF IDX>N start + >IDX OPTION:SOME ENDOF
   ;MATCH ;

private

: MUST-BEFORE ( ptr u8 n ptr u8 n -- ) {: earlier:ptr earlieru:n later:ptr lateru:n :}
   earlier earlieru POS-FOUND
   later lateru POS-FOUND
   < TTRUE ;

: AFTER-FOUND ( n ptr u8 n -- n )                  \ assert found after start; found index
   FIND-AFTER MATCH option
     none OF STR-FALSE TTRUE -1 ENDOF
     some OF STR-TRUE TTRUE IDX>N ENDOF
   ;MATCH ;

: MUST-NOT-FIND-BEFORE ( n n ptr u8 n -- ) {: start:n end:n needle:ptr nu:n :}
   start needle nu FIND-AFTER MATCH option
     none OF exit ENDOF
     some OF IDX>N ENDOF
   ;MATCH {: pos:n :}
   pos end >= TTRUE ;

: MUST-FIND-BEFORE ( n n ptr u8 n -- )
   {: start:n end:n needle:ptr nu:n :}
   start needle nu FIND-AFTER MATCH option
     none OF STR-FALSE TTRUE ENDOF
     some OF STR-TRUE TTRUE IDX>N end < TTRUE ENDOF
   ;MATCH ;

;package

\ The shared arena constants: the emitters, the fixpoint driver and the maker must
\ all name the same source-arena capacity and headroom percentage.
package BCG-CAP
using BCG

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
   idx LINT-LEX:TOKEN a u LINT-STR=CI ;

: DEF? ( n ptr u8 n -- bool ) {: idx:n name:ptr nameu:n :}
   idx 0 <= if 0 0= 0= exit then
   idx 1 + LINT-LEX:COUNT >= if 0 0= 0= exit then
   idx 1 - LINT-LEX:KIND@ LINT-LEX:WORD <> if 0 0= 0= exit then
   idx LINT-LEX:KIND@ LINT-LEX:WORD <> if 0 0= 0= exit then
   idx 1 + LINT-LEX:KIND@ LINT-LEX:WORD <> if 0 0= 0= exit then
   idx s" constant" TOK=CI 0= if 0 0= 0= exit then
   idx 1 + name nameu TOK=CI ;

: DEF-SCAN ( ptr u8 n -- ) {: name:ptr nameu:n :}
   SRC LINT-LEX:SOURCE
   -1 DEF-I !
   0 DEF-N !
   0 begin dup LINT-LEX:COUNT < while
      dup name nameu DEF? if
         dup DEF-I !
         DEF-N @ 1 + DEF-N !
      then
      1+
   repeat drop ;

: DEF-VALUE ( ptr u8 n -- ptr u8 n )
   DEF-SCAN
   DEF-N @ 1 = dup TTRUE 0= if s" " exit then
   DEF-I @ 1 - LINT-LEX:TOKEN ;

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
   s" src/habu/layout.f" LOAD
   OWNER
   SAVE-TOKENS
   s" bootstrap/cg/forth.fs" LOAD
   OWNER
   CHECK-TOKENS
   s" src/habu/stage2.f" LOAD
   s" S2-SOURCE-CAP" DEF-VALUE s" SOURCE-ARENA-CAP" T$=
   s" src/habu/maker.f" LOAD
   s" MK-SOURCE-CAP" DEF-VALUE s" SOURCE-ARENA-CAP" T$= ;

;package

\ The prefix/manifest row capture: both emitters, the recovery script and the
\ fixpoint driver must list exactly the same source files in the same order.
package BCG-MANIFEST
using BCG

$4000 constant CAP
0 constant MODE-FORTH
1 constant MODE-SOURCE
2 constant MODE-CAT
3 constant MODE-ARRAY
4 constant MODE-COMMON
$24 constant DOLLAR

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
create SEP $7C c,
create LF $0A c,

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
   needle nu POS-FOUND {: pos:n :}
   pos 1+ needle nu FIND-AFTER MATCH option
     none OF ENDOF
     some OF drop STR-FALSE TTRUE ENDOF
   ;MATCH
   pos ;

\ typed-local-lint: allow-bare-local - markers and source preserve ptr u8 roles.
: RANGE$ ( ptr u8 n ptr u8 n -- ptr u8 n ) {: first fu:n after au:n :}
   first fu UNIQUE-POS {: start:n :}
   after au UNIQUE-POS {: end:n :}
   start end < TTRUE
   SRC {: src:ptr srcu:n :}
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
   s" PFX-COMMON" s" LPEXECVECTOR" s" src/core/exec-vector.f" EXPECT-ROW
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
   s" PFX-COMMON" s" LPPTRSTORAGEEFF" s" src/core/pointer-storage-effects.f" EXPECT-ROW
   s" PFX-COMMON" s" LPDECLTXN" s" src/core/declaration-transaction.f" EXPECT-ROW
   s" PFX-COMMON" s" LPGENDECL" s" src/core/generated-declaration.f" EXPECT-ROW ;

: EXPECT-DECL ( -- )
   s" PFX-COMMON" s" LPDECLEVENT" s" src/core/decl-event.f" EXPECT-ROW
   s" PFX-COMMON" s" LPSTRUCTMAKE" s" src/core/structure-make.f" EXPECT-ROW
   s" PFX-COMMON" s" LPSTRUCTDECL" s" src/core/structure-decl.f" EXPECT-ROW
   s" PFX-COMMON" s" LPENUMDECL" s" src/core/enum-decl.f" EXPECT-ROW ;

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
   s" PFX-COMMON" s" LPSHA256" s" src/core/sha256.f" EXPECT-ROW
   s" PFX-COMMON" s" LPTFAMSHA" s" src/core/type-family-sha.f" EXPECT-ROW
   s" PFX-COMMON" s" LPCOMBINATORS" s" src/core/combinators.f" EXPECT-ROW
   s" PFX-COMMON" s" LPXREF" s" src/habu/xref.f" EXPECT-ROW
   s" PFX-COMMON" s" LPGENDECLDICT" s" src/core/generated-declaration-dictionary.f" EXPECT-ROW
   s" PFX-COMMON" s" LPGENDECLPROT" s" src/core/generated-declaration-protection.f" EXPECT-ROW
   s" PFX-COMMON" s" LPLAYOUTSEAL" s" src/core/layout-buffer-seal.f" EXPECT-ROW ;

: EXPECT-NATIVE ( -- )
   0 EXPECT-U !
   EXPECT-CHECKER
   EXPECT-DECL
   EXPECT-CORE
   s" PFX-COMMON" s" LPLOWERCERTSEAL" s" src/core/lower-cert-seal.f" EXPECT-ROW
   s" PFX-COMMON" s" LPSCRIPTARGV" s" src/os/script-argv.f" EXPECT-ROW
   s" PFX-COMMON" s" LPINTMARK" s" src/core/internal-mark.f" EXPECT-ROW
   s" PFX-COMMON" s" LPTOPROW" s" src/core/top-row.f" EXPECT-ROW ;

: EXPECT-GFORTH ( -- )
   0 EXPECT-U !
   EXPECT-CHECKER
   EXPECT-DECL
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
   45 ASSERT-EQUAL
   s" src/core/structures-effects.f" MUST-LACK
   s" LPSTRUCTEFF" MUST-LACK ;

: GFORTH-ROWS ( -- )
   CAPTURE-PREFIX
   EXPECT-GFORTH
   42 ASSERT-EQUAL
   s" src/core/structures-effects.f" MUST-LACK
   s" LPSTRUCTEFF" MUST-LACK ;

: EXPECT-FILE ( ptr u8 n -- )
   EXPECT+  LF 1 EXPECT+ ;

: EXPECT-RECOVERY-CHECKER ( -- )
   0 EXPECT-U !
   s" src/core/util.f" EXPECT-FILE
   s" src/core/cell.f" EXPECT-FILE
   s" src/core/pointer-storage.f" EXPECT-FILE
   s" src/core/engine-error.f" EXPECT-FILE
   s" src/core/exec-vector.f" EXPECT-FILE
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
   s" src/core/declaration-transaction.f" EXPECT-FILE
   s" src/core/generated-declaration.f" EXPECT-FILE ;

\ RECOVERY emit_src() cats only the checker-boot span + structures; the DECL
\ files ride the separate emit_decl_src() helper (asserted below).
: EXPECT-RECOVERY ( -- )
   EXPECT-RECOVERY-CHECKER
   s" src/core/structures.f" EXPECT-FILE ;

\ FIXPOINT's BF-APPEND-SOURCE span spans BF-APPEND-CHECKER-BOOT .. -CORE-FILES,
\ so BF-APPEND-DECL-FILES's decl-event.f is inline in the captured range.
: EXPECT-FIXPOINT-SRC ( -- )
   EXPECT-RECOVERY-CHECKER
   s" src/core/decl-event.f" EXPECT-FILE
   s" src/core/structure-make.f" EXPECT-FILE
   s" src/core/structure-decl.f" EXPECT-FILE
   s" src/core/enum-decl.f" EXPECT-FILE
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
   s" src/core/sha256.f" EXPECT-FILE
   s" src/core/type-family-sha.f" EXPECT-FILE
   s" src/core/combinators.f" EXPECT-FILE
   s" src/habu/treeshake.f" EXPECT-FILE
   s" src/habu/rt.f" EXPECT-FILE
   s" src/habu/crash.f" EXPECT-FILE
   s" src/os/image-bytes.f" EXPECT-FILE
   s" $OS_IMAGE" EXPECT-FILE
   s" $OS_SIGN" EXPECT-FILE
   s" $OS_PROCWATCH" EXPECT-FILE
   s" $OS_PROCCONTROL" EXPECT-FILE
   s" src/habu/habu1.f" EXPECT-FILE
   s" src/habu/prof.f" EXPECT-FILE
   s" src/habu/regalloc.f" EXPECT-FILE
   s" src/habu/jit.f" EXPECT-FILE
   s" src/habu/engine-size.f" EXPECT-FILE
   s" src/habu/habu2.f" EXPECT-FILE
   s" src/habu/xref.f" EXPECT-FILE
   s" src/core/generated-declaration-dictionary.f" EXPECT-FILE
   s" src/core/generated-declaration-protection.f" EXPECT-FILE
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
   s" src/core/sha256.f" EXPECT-FILE
   s" src/core/type-family-sha.f" EXPECT-FILE
   s" BF-APPEND-COMBINATORS" EXPECT-FILE
   s" src/habu/treeshake.f" EXPECT-FILE
   s" src/habu/rt.f" EXPECT-FILE
   s" src/habu/crash.f" EXPECT-FILE
   s" BF-APPEND-IMAGE-BYTES" EXPECT-FILE
   s" BF-APPEND-TARGET-IMAGE" EXPECT-FILE
   s" BF-APPEND-TARGET-PROC-WATCH" EXPECT-FILE
   s" BF-APPEND-TARGET-PROC-CONTROL" EXPECT-FILE
   s" src/habu/habu1.f" EXPECT-FILE
   s" BUILD-EXT:APPEND" EXPECT-FILE
   s" src/habu/prof.f" EXPECT-FILE
   s" src/habu/regalloc.f" EXPECT-FILE
   s" src/habu/jit.f" EXPECT-FILE
   s" src/habu/engine-size.f" EXPECT-FILE
   s" src/habu/habu2.f" EXPECT-FILE
   s" src/habu/xref.f" EXPECT-FILE
   s" src/core/generated-declaration-dictionary.f" EXPECT-FILE
   s" src/core/generated-declaration-protection.f" EXPECT-FILE
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
   src su left lu FIND-IN MATCH option
      none OF STR-FALSE TTRUE -1 ENDOF
      some OF IDX>N ENDOF
   ;MATCH
   src su right ru FIND-IN MATCH option
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

: NATIVE-COLD-SCOPE ( -- ptr u8 n ptr u8 n )
   s" : EMIT-COLD-PREFIX-SHARED ( -- )"
   s" \ ---- control-flow JIT helpers ----" ;

: NATIVE-COLD-LOAD$ ( -- ptr u8 n )
   S\" \n      EMIT-COLD-PREFIX\n" ;

: NATIVE-OUTER-CALLS ( -- )
   NATIVE-COLD-SCOPE NATIVE-COLD-LOAD$ SCOPE-ONE
   NATIVE-COLD-SCOPE
      s" PFX-APPEND-ENGINE-SNAP-HOOK-BUILD" SCOPE-ONE
   NATIVE-COLD-SCOPE
      s" PFX-LOAD-SCRIPT-ARGV-COLD" SCOPE-ONE
   NATIVE-COLD-SCOPE
      s" PFX-PROVIDE-FILES" SCOPE-ONE
   NATIVE-COLD-SCOPE
      s" PFX-LOAD-INTMARK-COLD" SCOPE-ONE
   NATIVE-COLD-SCOPE
      s" PFX-LOAD-TOPROW-COLD" SCOPE-ONE
   NATIVE-COLD-SCOPE
      s" EMIT-SEAL-CAPTURE-TOKEN" SCOPE-ONE
   NATIVE-COLD-SCOPE
      s" EMIT-SEAL-FRIEND-TOKEN" SCOPE-ONE
   NATIVE-COLD-SCOPE NATIVE-COLD-LOAD$
      s" PFX-APPEND-ENGINE-SNAP-HOOK-BUILD" SCOPE-BEFORE
   NATIVE-COLD-SCOPE
      s" PFX-APPEND-ENGINE-SNAP-HOOK-BUILD" s" PFX-LOAD-SCRIPT-ARGV-COLD" SCOPE-BEFORE
   NATIVE-COLD-SCOPE
      s" PFX-LOAD-SCRIPT-ARGV-COLD" s" PFX-PROVIDE-FILES" SCOPE-BEFORE
   NATIVE-COLD-SCOPE
      s" PFX-PROVIDE-FILES" s" PFX-LOAD-INTMARK-COLD" SCOPE-BEFORE
   NATIVE-COLD-SCOPE
      s" PFX-LOAD-INTMARK-COLD" s" PFX-LOAD-TOPROW-COLD" SCOPE-BEFORE
   NATIVE-COLD-SCOPE
      s" PFX-LOAD-TOPROW-COLD" s" EMIT-SEAL-CAPTURE-TOKEN" SCOPE-BEFORE
   NATIVE-COLD-SCOPE
      s" EMIT-SEAL-CAPTURE-TOKEN" s" EMIT-SEAL-FRIEND-TOKEN" SCOPE-BEFORE ;

: GFORTH-OUTER-CALLS ( -- )
   s" : C-SOURCE-PIPE ( -- )" s" : C-SOURCE-FILE-INIT ( -- )"
      s" EMIT-COLD-PREFIX" s" PFX-LOAD-SCRIPT-ARGV-COLD"
      s" PFX-PROVIDE-FILES" CALL-CHAIN
   s" : C-SOURCE-FILE-PREFIX ( -- )" s" SRC-FPLAIN @ LBL,"
      s" EMIT-COLD-PREFIX" s" PFX-LOAD-SCRIPT-ARGV-COLD"
      s" PFX-PROVIDE-FILES" CALL-CHAIN
   s" SRC-FPLAIN @ LBL," s" SRC-FREADY @ LBL,"
      s" EMIT-COLD-PREFIX" s" PFX-LOAD-SCRIPT-ARGV-COLD"
      s" PFX-PROVIDE-FILES" CALL-CHAIN
   s" : C-SOURCE-FAIL-REPL-DONE ( -- )" s" : C-SOURCE-FILE-LIST ( -- )"
      s" EMIT-COLD-PREFIX" s" PFX-LOAD-SCRIPT-ARGV-COLD"
      s" PFX-PROVIDE-FILES" CALL-CHAIN ;

: RECOVERY-TARGETS ( -- )
   S\" case \"$HABU_TARGET\" in" s" PROBE=" s" OS_" SCOPE-N 14 T=
   S\" case \"$HABU_TARGET\" in" s" PROBE="
      s" OS_TARGET=src/os/macos/target.f" SCOPE-ONE
   S\" case \"$HABU_TARGET\" in" s" PROBE="
      s" OS_LAYOUT=src/os/macos/layout.f" SCOPE-ONE
   S\" case \"$HABU_TARGET\" in" s" PROBE="
      s" OS_SYS=src/os/macos/sys.f" SCOPE-ONE
   S\" case \"$HABU_TARGET\" in" s" PROBE="
      s" OS_PROCWATCH=src/os/macos/proc-watch.f" SCOPE-ONE
   S\" case \"$HABU_TARGET\" in" s" PROBE="
      s" OS_PROCCONTROL=src/os/macos/proc-control.f" SCOPE-ONE
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
      s" OS_PROCWATCH=src/os/linux/proc-watch.f" SCOPE-ONE
   S\" case \"$HABU_TARGET\" in" s" PROBE="
      s" OS_PROCCONTROL=src/os/linux/proc-control.f" SCOPE-ONE
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
      s" OS_PROCWATCH=src/os/macos/proc-watch.f" s" linux-aarch64)" SCOPE-BETWEEN
   S\" case \"$HABU_TARGET\" in" s" PROBE=" s" macos-aarch64)"
      s" OS_PROCCONTROL=src/os/macos/proc-control.f" s" linux-aarch64)" SCOPE-BETWEEN
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
      s" OS_PROCWATCH=src/os/linux/proc-watch.f" s" *)" SCOPE-BETWEEN
   S\" case \"$HABU_TARGET\" in" s" PROBE=" s" linux-aarch64)"
      s" OS_PROCCONTROL=src/os/linux/proc-control.f" s" *)" SCOPE-BETWEEN
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
   s" : BF-APPEND-TARGET-SYS" s" : BF-APPEND-TARGET-PROC-WATCH"
      s" src/os/" SCOPE-N 2 T=
   s" : BF-APPEND-TARGET-SYS" s" : BF-APPEND-TARGET-PROC-WATCH"
      s" src/os/linux/sys.f" SCOPE-ONE
   s" : BF-APPEND-TARGET-SYS" s" : BF-APPEND-TARGET-PROC-WATCH"
      s" src/os/macos/sys.f" SCOPE-ONE
   s" : BF-APPEND-TARGET-SYS" s" : BF-APPEND-TARGET-PROC-WATCH"
      s" HB-TARGET-LINUX?" s" src/os/linux/sys.f" SCOPE-BEFORE
   s" : BF-APPEND-TARGET-SYS" s" : BF-APPEND-TARGET-PROC-WATCH"
      s" src/os/linux/sys.f" s" HB-TARGET-MACOS?" SCOPE-BEFORE
   s" : BF-APPEND-TARGET-SYS" s" : BF-APPEND-TARGET-PROC-WATCH"
      s" HB-TARGET-MACOS?" s" src/os/macos/sys.f" SCOPE-BEFORE
   s" : BF-APPEND-TARGET-PROC-WATCH" s" : BF-APPEND-TARGET-PROC-CONTROL"
      s" src/os/" SCOPE-N 2 T=
   s" : BF-APPEND-TARGET-PROC-WATCH" s" : BF-APPEND-TARGET-PROC-CONTROL"
      s" src/os/linux/proc-watch.f" SCOPE-ONE
   s" : BF-APPEND-TARGET-PROC-WATCH" s" : BF-APPEND-TARGET-PROC-CONTROL"
      s" src/os/macos/proc-watch.f" SCOPE-ONE
   s" : BF-APPEND-TARGET-PROC-WATCH" s" : BF-APPEND-TARGET-PROC-CONTROL"
      s" HB-TARGET-LINUX?" s" src/os/linux/proc-watch.f" SCOPE-BEFORE
   s" : BF-APPEND-TARGET-PROC-WATCH" s" : BF-APPEND-TARGET-PROC-CONTROL"
      s" src/os/linux/proc-watch.f" s" HB-TARGET-MACOS?" SCOPE-BEFORE
   s" : BF-APPEND-TARGET-PROC-WATCH" s" : BF-APPEND-TARGET-PROC-CONTROL"
      s" HB-TARGET-MACOS?" s" src/os/macos/proc-watch.f" SCOPE-BEFORE
   s" : BF-APPEND-TARGET-PROC-CONTROL" s" : BF-APPEND-TARGET-FLAG"
      s" src/os/" SCOPE-N 2 T=
   s" : BF-APPEND-TARGET-PROC-CONTROL" s" : BF-APPEND-TARGET-FLAG"
      s" src/os/linux/proc-control.f" SCOPE-ONE
   s" : BF-APPEND-TARGET-PROC-CONTROL" s" : BF-APPEND-TARGET-FLAG"
      s" src/os/macos/proc-control.f" SCOPE-ONE
   s" : BF-APPEND-TARGET-PROC-CONTROL" s" : BF-APPEND-TARGET-FLAG"
      s" HB-TARGET-LINUX?" s" src/os/linux/proc-control.f" SCOPE-BEFORE
   s" : BF-APPEND-TARGET-PROC-CONTROL" s" : BF-APPEND-TARGET-FLAG"
      s" src/os/linux/proc-control.f" s" HB-TARGET-MACOS?" SCOPE-BEFORE
   s" : BF-APPEND-TARGET-PROC-CONTROL" s" : BF-APPEND-TARGET-FLAG"
      s" HB-TARGET-MACOS?" s" src/os/macos/proc-control.f" SCOPE-BEFORE
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
   PREFIX-CALLS
   NATIVE-OUTER-CALLS ;

: GFORTH ( -- )
   GFORTH-ROWS
   PREFIX-CALLS
   GFORTH-OUTER-CALLS ;

: RECOVERY ( -- )
   0 RECOVERY-U !
   s" emit_src() {" s"   local f" MODE-CAT s" cat"
   [: RECOVERY+ ;] CAPTURE 20 T=
   EXPECT-RECOVERY
   RECOVERY$ EXPECT$ T$=
   RECOVERY$ 20 ASSERT-UNIQUE
   s" emit_decl_src() {" s" emit_src() {" s" cat src/" SCOPE-N 4 T=
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
      [: RECOVERY+ ;] CAPTURE 34 T=
   EXPECT-RECOVERY-COMMON
   RECOVERY$ EXPECT$ T$=
   RECOVERY$ 34 ASSERT-UNIQUE
   RECOVERY-TARGETS ;

: FIXPOINT ( -- )
   0 FIXPOINT-U !
   s" : BF-APPEND-CHECKER-BOOT" s" : BF-APPEND-CORE-BYTES"
   MODE-SOURCE s" BF-APPEND-SOURCE" [: FIXPOINT+ ;] CAPTURE 24 T=
   EXPECT-FIXPOINT-SRC
   FIXPOINT$ EXPECT$ T$=
   FIXPOINT$ 24 ASSERT-UNIQUE
   s" : BF-APPEND-DECL-FILES" s" : BF-APPEND-CORE-FILES"
   s" BF-APPEND-SOURCE" SCOPE-N 4 T=
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
      [: FIXPOINT+ ;] CAPTURE 35 T=
   EXPECT-FIXPOINT-COMMON
   FIXPOINT$ EXPECT$ T$=
   FIXPOINT$ 35 ASSERT-UNIQUE
   FIXPOINT-TARGETS ;

;package

\ The individual checks. They are BCG's own private bodies, so they call the
\ assertion words above by their bare names; MAIN at the end of the file is the
\ only export. The block reopens twice below because the hide fixtures and the
\ narrower sibling packages have to be declared at genuine top level.
package BCG
private

: INSTALL-FAIL-CLOSED ( -- )
   s" bootstrap/cg/install.fs" LOAD
   s" : BODY-ARITY ( -- n )  ['] TRY-ARITY CG-CATCH ;" MUST-HAVE
   s" ['] TRY-EFFECT CG-CATCH" MUST-HAVE
   s" catch if 1" MUST-LACK
   s" catch if 0" MUST-LACK
   s" NM@ CAP$ BODY-ARITY EFFECT-FLAGS CG-RECORD" MUST-HAVE ;

: FORTH-SDQ-COMMENT ( -- )
   s" bootstrap/cg/forth.fs" LOAD
   s" C-ADR PC-relative" MUST-HAVE
   s" push abs-addr" MUST-LACK
   s" absolute address is known" MUST-LACK ;

: PREFIX-LIST-COMMON ( -- )
   s" PFX-LOAD-FILES" MUST-HAVE
   s" PFX-PATH-FILES" MUST-HAVE
   s" PFX-FILES" MUST-LACK
   s" PFX-ROW" MUST-LACK
   s" PFX-LINUX  LPLINUXTARGET" MUST-HAVE
   s" PFX-MACOS  LPMACOSTARGET" MUST-HAVE
   s" a u ZBYTES," MUST-HAVE
   s" LPUTIL @ ADR" MUST-LACK
   s" LSRCRD @ BL then" MUST-LACK
   s" a u ZBYTES ;" MUST-LACK
   s" LPLINUXTARGET @ LBL, s" MUST-LACK ;

: PREFIX-LIST-BOOTSTRAP ( -- )
   s" bootstrap/cg/forth.fs" LOAD
   PREFIX-LIST-COMMON
   BCG-MANIFEST:GFORTH
   s" LSRCRD @ BL," MUST-HAVE
   s" LSRCRD LABEL@ BL," MUST-LACK ;

: PREFIX-LIST-NATIVE ( -- )
   s" src/habu/habu2.f" LOAD
   PREFIX-LIST-COMMON
   BCG-MANIFEST:NATIVE
   s" LSRCRD LABEL@ BL," MUST-HAVE ;

: PREFIX-LIST ( -- )
   PREFIX-LIST-BOOTSTRAP
   PREFIX-LIST-NATIVE ;

: TOK-IMM-MIRROR ( -- )
   s" bootstrap/cg/forth.fs" LOAD
   s" : BTOKIMM ( -- )" MUST-HAVE
   s" LFIND @ BL," MUST-HAVE
   s" 9 13 2 ANDI," MUST-HAVE
   s" ['] BTOKIMM FPRIM" MUST-HAVE
   s" src/habu/habu2.f" LOAD
   s" : BTOKIMM ( -- )" MUST-HAVE
   s" LFIND LABEL@ BL," MUST-HAVE
   s" 9 13 2 ANDI," MUST-HAVE
   s" ['] BTOKIMM 2 GDEREF-F" MUST-HAVE ;

: PROTWID-LEAFNESS ( -- )
   s" bootstrap/cg/forth.fs" LOAD
   s" ['] BPROTWIDADD FPRIM-L" MUST-LACK
   s" ['] BPROTWIDADD FPRIM" MUST-HAVE ;

: CELL-RUNTIME ( -- )
   CELL-WIDTH-CHECK
   CELL 1 cells T= ;

: ENGINE-ERROR-CODES ( -- )
   s" src/core/engine-error.f" LOAD
   s" package ENGINE-ERROR" MUST-HAVE
   s" 83 constant SEAL-VIOLATION" MUST-HAVE
   s" 84 constant SEAL-PACKAGE" MUST-HAVE
   s" 85 constant BAD-TAG" MUST-HAVE
   s" 86 constant CALLABLE-ABI" MUST-HAVE
   s" 87 constant CATCH-STACK" MUST-HAVE
   s" 88 constant CODE-CERT" MUST-HAVE
   s" constant E-SEAL-VIOLATION" MUST-LACK
   s" bootstrap/cg/forth.fs" LOAD
   s" 83 constant ENGINE-ERROR:SEAL-VIOLATION" MUST-HAVE
   s" 84 constant ENGINE-ERROR:SEAL-PACKAGE" MUST-HAVE
   s" 85 constant ENGINE-ERROR:BAD-TAG" MUST-HAVE
   s" 86 constant ENGINE-ERROR:CALLABLE-ABI" MUST-HAVE
   s" 87 constant ENGINE-ERROR:CATCH-STACK" MUST-HAVE
   s" 88 constant ENGINE-ERROR:CODE-CERT" MUST-HAVE
   s" : C-P2-FIND-GLOBAL?" MUST-HAVE
   s" : C-P2-FIND-CHECKER" MUST-HAVE
   s" FRIEND-LATCH-CELL LDR,  9 done CBZ," MUST-HAVE
   s" src/habu/habu2.f" LOAD
   s" : C-FIND-GLOBAL?" MUST-HAVE
   s" : C-FIND-CHECKER" MUST-HAVE
   s" FRIEND-LATCH-CELL LDR,  9 done CBZ," MUST-HAVE
   s" src/core/engine-error-effects.f" LOAD
   s" package ENGINE-ERROR" MUST-HAVE
   S\" s\" SEAL-VIOLATION\" s\" -- n\" TRUST" MUST-HAVE
   S\" s\" CODE-CERT\" s\" -- n\" TRUST" MUST-HAVE
   s" tools/bootstrap.sh" LOAD
   s" test/engine-error-package.f" MUST-HAVE ;

: CELL-SOURCE ( -- )
   s" src/core/cell.f" LOAD
   s" $8 constant CELL" MUST-HAVE
   s" $4C constant CORE-LAYOUT-RC" MUST-HAVE
   s" 1 cells CELL <>" MUST-HAVE
   s" CORE-LAYOUT-RC die" MUST-HAVE ;

: CELL-BOOTSTRAP ( -- )
   s" tools/bootstrap.sh" LOAD
   BCG-MANIFEST:RECOVERY
   s" cat src/core/structures-effects.f" MUST-LACK ;

: CELL-FIXPOINT ( -- )
   s" tools/build-fixpoint.f" LOAD
   BCG-MANIFEST:FIXPOINT
   s" src/core/structures-effects.f" MUST-LACK ;

: CELL-PARITY ( -- )
   CELL-RUNTIME
   CELL-SOURCE
   CELL-BOOTSTRAP
   CELL-FIXPOINT ;

: BAKED-PREFIX-CURRENT ( -- )
   s" : C-SOURCE-BAKED" POS-FOUND {: start:n :}
   start s" : EMIT-SOURCE" AFTER-FOUND {: end:n :}
   start end s" EMIT-COLD-PREFIX" MUST-FIND-BEFORE ;

: BAKED-PREFIX ( -- )
   s" bootstrap/cg/forth.fs" LOAD
   BAKED-PREFIX-CURRENT
   s" src/habu/habu2.f" LOAD
   BAKED-PREFIX-CURRENT ;

: TRUST-CALLS-CURRENT ( -- )
   s" : C-PUSH-DATA-CELL ( n -- )" MUST-HAVE
   s" : C-PUSH-TRUST-SIG ( n n -- )" MUST-HAVE
   s" : C-CALL-X11-SAVED ( -- )" MUST-HAVE
   s" CRSIG-A-CELL CRSIG-U-CELL C-PUSH-TRUST-SIG" MUST-HAVE
   s" 9 DATA CRSIG-A-CELL LDR,  9 G-PUSH" MUST-LACK
   s" 9 DATA CRSIG-U-CELL LDR,  9 G-PUSH" MUST-LACK ;

: TRUST-CALLS ( -- )
   s" bootstrap/cg/forth.fs" LOAD
   TRUST-CALLS-CURRENT
   s" src/habu/habu2.f" LOAD
   TRUST-CALLS-CURRENT
   s" TSIG-A-CELL TSIG-U-CELL C-PUSH-TRUST-SIG" MUST-HAVE
   s" 9 DATA TSIG-A-CELL LDR,  9 G-PUSH" MUST-LACK
   s" 9 DATA TSIG-U-CELL LDR,  9 G-PUSH" MUST-LACK ;

: IMAGE-BUFFER-CURRENT ( -- )
   s" require image.fs" MUST-HAVE
   s" $90000 constant MSIZE" MUST-LACK
   s" create MBUF MSIZE allot" MUST-LACK
   s" variable MP" MUST-LACK
   s" variable MLEN" MUST-LACK
   s" : M8" MUST-LACK
   s" : M16" MUST-LACK
   s" : M32" MUST-LACK
   s" : M64" MUST-LACK
   s" SCODE CODELEN @ M-BYTES" MUST-HAVE ;

: IMAGE-BUFFER ( -- )
   s" bootstrap/cg/image.fs" LOAD
   s" create MBUF MSIZE allot" MUST-HAVE
   s" : M-BYTES ( addr u -- )" MUST-HAVE
   s" : M-NAME16 ( addr u -- )" MUST-HAVE
   s" bootstrap/cg/elf.fs" LOAD
   IMAGE-BUFFER-CURRENT
   s" bootstrap/cg/macho.fs" LOAD
   IMAGE-BUFFER-CURRENT ;

: ASM-CHECKED ( -- )
   s" bootstrap/cg/asm-checked.fs" LOAD
   s" : A-RRR16 ( reg reg n n -- n )" MUST-HAVE
   s" : A-RRI10 ( reg reg n n -- n )" MUST-HAVE
   s" : A-MOVW ( reg n n n -- n )" MUST-HAVE
   s" : A-LS-UOFF ( reg reg off n -- n )" MUST-HAVE
   s" 2332033024 A-RRR16" MUST-HAVE
   s" $9AC00C00 A-RRR16" MUST-HAVE
   s" $D63F0000 A-R1-5" MUST-HAVE
   s" 16 lshift swap 5 lshift or swap or" MUST-LACK
   s" 10 lshift swap 5 lshift or swap or" MUST-LACK ;

: GFORTH-LOCALS ( -- )
   s" bootstrap/cg/forth.fs" LOAD
   s" done:label" MUST-LACK
   s" qexit:label" MUST-LACK
   s" qlok:label" MUST-LACK ;

: GFORTH-LOCAL-CAPTURE ( -- )
   s" bootstrap/cg/forth.fs" LOAD
   s" : EMIT-COMPILE-LOCAL" POS-FOUND {: start:n :}
   start s" : EMIT-COMPILE-LITERAL" AFTER-FOUND {: end:n :}
   start end s" LBCAP @ BL" MUST-FIND-BEFORE
   start end s" QPATCH-CELL" MUST-FIND-BEFORE
   start end s" LVRALLOC" MUST-FIND-BEFORE ;

: SPAWN-SCOPED-LABELS ( -- )
   s" src/habu/habu1.f" LOAD
   s" : LINUX-SPAWN-PREP-W" POS-FOUND {: start:n :}
   start s" : BRUNRC" AFTER-FOUND {: end:n :}
   start end s" LNX-DONE LABEL@ B" MUST-NOT-FIND-BEFORE
   start end s" LNX-DONE LABEL@ LBL" MUST-NOT-FIND-BEFORE
   start end s" LNX-FAIL LABEL@ B" MUST-NOT-FIND-BEFORE
   start end s" LNX-FAIL LABEL@ LBL" MUST-NOT-FIND-BEFORE
   start end s" LNX-OK LABEL@ B" MUST-NOT-FIND-BEFORE
   start end s" LNX-OK LABEL@ LBL" MUST-NOT-FIND-BEFORE
   start end s" child:label" MUST-FIND-BEFORE
   start end s" done:label" MUST-FIND-BEFORE ;

: DATA-SIZE-MIRROR ( -- )
   s" bootstrap/cg/forth.fs" LOAD
   s" $2000000 constant DATA-SIZE" MUST-HAVE
   s" $300000 constant DATA-SIZE" MUST-LACK
   s" src/os/linux/layout.f" LOAD
   s" $2000000 constant DATA-SIZE" MUST-HAVE
   s" $300000 constant DATA-SIZE" MUST-LACK ;

\ The profiler counter band is sized from DICT-CAP (one 64-bit counter per dict
\ slot) and reserved high in the DATA region: PROF-CNT-BYTES = DICT-CAP cells, and
\ PROF-CNT = DATA-SIZE - PROF-CNT-BYTES. Neither the old magic byte count
\ ($10000 - ...) nor a hardcoded absolute base ($1F0000) may return, so the band
\ can never fall short of the NDICT<=DICT-CAP slots BPROF-ON zeroes and EMIT-PROF
\ indexes (dot habu-bound-profiler-counter-235c5f48). Native and bootstrap mirror.
: PROF-CNT-HIGH ( -- )
   s" bootstrap/cg/forth.fs" LOAD
   s" DICT-CAP cells constant PROF-CNT-BYTES" MUST-HAVE
   s" bootstrap/cg/prof.fs" LOAD
   s" DATA-SIZE PROF-CNT-BYTES - constant PROF-CNT" MUST-HAVE
   s" DATA-SIZE $10000 - constant PROF-CNT" MUST-LACK
   s" $1F0000 constant PROF-CNT" MUST-LACK
   s" src/habu/layout.f" LOAD
   s" DICT-CAP cells constant PROF-CNT-BYTES" MUST-HAVE
   s" src/habu/prof.f" LOAD
   s" DATA-SIZE PROF-CNT-BYTES - constant PROF-CNT" MUST-HAVE
   s" DATA-SIZE $10000 - constant PROF-CNT" MUST-LACK
   s" $1F0000 constant PROF-CNT" MUST-LACK ;

\ DP-CHECK (habu1.f + bootstrap mirror) caps the user heap below the profiler counter
\ band: the high bound is DATA-SIZE - PROF-CNT-BYTES, not the bare DATA-SIZE region top,
\ so a large allot + prof-on can never overlap the counters (dot
\ habu-bound-profiler-counter-235c5f48). Red-first: the unfixed DP-CHECK loads a bare
\ `5 DATA-SIZE LIT64,` high bound; the fix subtracts PROF-CNT-BYTES. Native and bootstrap
\ mirror the same cap.
: DP-CHECK-BAND-CAP ( -- )
   s" src/habu/habu1.f" LOAD
   s" : DP-CHECK" POS-FOUND {: hstart:n :}
   hstart s" : BALLOT" AFTER-FOUND {: hend:n :}
   hstart hend s" DATA-SIZE PROF-CNT-BYTES - LIT64," MUST-FIND-BEFORE
   hstart hend s" 5 DATA-SIZE LIT64," MUST-NOT-FIND-BEFORE
   s" bootstrap/cg/forth.fs" LOAD
   s" : DP-CHECK" POS-FOUND {: bstart:n :}
   bstart s" : BALLOT" AFTER-FOUND {: bend:n :}
   bstart bend s" DATA-SIZE PROF-CNT-BYTES - LIT64," MUST-FIND-BEFORE
   bstart bend s" 5 DATA-SIZE LIT64," MUST-NOT-FIND-BEFORE ;

: PUBLISH-HOOK-SPLIT ( -- )
   s" bootstrap/cg/forth.fs" LOAD
   s" : EMIT-COMPILE-PUBLISH-TRUSTED" MUST-HAVE
   s" : EMIT-COMPILE-PUBLISH-HOOKED" MUST-HAVE
   s" : EMIT-COMPILE-PUBLISH ( n -- )" MUST-HAVE
   s" BODYBUF-OFF ADDI,  10 G-PUSH" MUST-HAVE
   s" C-CALL-TRUST-PEND-MAYBE" MUST-LACK ;

: LOCAL-SHADOW ( -- )
   s" bootstrap/cg/forth.fs" LOAD
   s" lmain EMIT-COMPILE-LOCAL" s" lmain EMIT-COMPILE-KEYWORDS" MUST-BEFORE
   s" : J-CASE ( -- )" MUST-HAVE
   s" : J-OF ( -- )" MUST-HAVE
   s" : J-ENDOF ( -- )" MUST-HAVE
   s" : J-ENDCASE ( -- )" MUST-HAVE
   s" : J-MATCH ( -- )" MUST-HAVE
   s" : C-DIE-BAD-TAG ( -- )" MUST-HAVE ;

: HIDE-PRELUDE ( -- )
   s" tools/bootstrap.sh" LOAD
   s" BOOT-USIGS-RESET" MUST-HAVE
   s" SEQ" MUST-HAVE
   s" IMK-NDICT0" MUST-HAVE                \ replay hides from util.f's FIRST record (the int-mark watermark), mirroring BFR-HIDE-DICT-FROM-EARLIEST
   s" BOOT-HIDE-DICT-FROM-EARLIEST" MUST-HAVE
   s" T-CON" MUST-LACK ;

\ Every engine built from the emitted compiler source loads the boot prefix from
\ disk when it starts and then interprets that source, which loads the prefix a
\ second time. The hide prelude above is what stops the second load from
\ inheriting the first load's words. Without it the startup load's `trust` and
\ `checker-defer` stay resolvable while src/core/checker.f is being re-read, so a
\ `defer` declared before that file's own `: TRUST` publishes its effect row and
\ its defer row into the checker being replaced, the pending pre-trust defer
\ table is never filled, DRAIN-PRETRUST replays nothing, and the first checked
\ `is` on such a defer (src/habu/xref.f INSTALL, `is PKG-LIVE-XT`) cannot
\ certify: `hook: non-certified definition: install at 'is'`, exit 70. That is
\ how the whole no-binary recovery path died while the prelude was emitted for
\ the stage builds only and the Gforth-compiled recovery seed went without it
\ (dot habu-fix-stage0-pre-88a4297e).
\
\ So the prelude is not merely present in the script, it is emitted by every
\ caller of emit_src and it comes before the first source file. This checks the
\ structure rather than counting words: the prelude call must sit inside
\ emit_src's own head, that head must contain no conditional that could gate it
\ again, no mode variable may survive, and no call site may ask for a mode.
: PROLOGUE-UNCONDITIONAL ( -- )
   s" tools/bootstrap.sh" LOAD
   s" emit_src() {" POS-FOUND {: head:n :}
   head S\" printf \"0 set-check\\n\" >> \"$out\"" AFTER-FOUND {: body:n :}
   head body S\" emit_boot_hide \"$out\"" MUST-FIND-BEFORE
   head body s" if [[" MUST-NOT-FIND-BEFORE
   s" local mode=" MUST-LACK
   S\" emit_boot_hide \"$out\"" s" cat src/core/util.f" MUST-BEFORE
   s" src/habu/stage2.f native" MUST-LACK
   s" src/habu/stdin.f native" MUST-LACK ;

;package

\ --- earliest-marker hide behavior ---
\ tools/bootstrap.sh's BOOT-* hide prelude mirrors src/habu/hide.f's BFR-*
\ words, so the native mirror is the executable spec and is driven directly
\ below; no shell is spawned (that would add host-glue surface), so the script
\ body itself stays pinned by the substring assertions above. hide.f is baked
\ into the engine prelude and truncated away after use, so `require` would be
\ skipped as already provided; include reloads the BFR-* words here. The include
\ publishes those BFR-* words globally, so it stays outside every package.
include src/habu/hide.f

\ The watermark has to be read at top level BETWEEN the two duplicate fixture
\ records, and packages do not nest, so BCG-HIDE opens once to publish the
\ recording word and reopens below for the checks that read it.
package BCG-HIDE
private

variable MID                            \ ndict watermark between the duplicate fixture records

public

: MARK-MID ( -- )
   ndict@ MID ! ;

;package

\ Two packages export the same tail on purpose: the earlier record must win.
package BCG-DUP-EARLY
public
: DUP-MARK ( -- ) ;
;package

BCG-HIDE:MARK-MID

package BCG-DUP-LATE
public
: DUP-MARK ( -- ) ;
;package

package BCG-HIDE
private

: REC ( ptr u8 n -- n )
   BFR-FIND-FIRST-INDEX ;

: IMK-REC ( -- n )
   s" IMK-NDICT0" REC ;

: SEQ-REC ( -- n )
   s" SEQ" REC ;

\ The production markers exist in the live dictionary with IMK-NDICT0 (util.f's
\ first record) earlier than SEQ; the hide index must pick the earlier record
\ in either argument order.
: EARLIEST-MARKER ( -- )
   IMK-REC 0 >= TTRUE
   SEQ-REC 0 >= TTRUE
   IMK-REC SEQ-REC < TTRUE
   s" IMK-NDICT0" s" SEQ" BFR-MARKER-INDEX IMK-REC T=
   s" SEQ" s" IMK-NDICT0" BFR-MARKER-INDEX IMK-REC T= ;

\ Earliest-hide depends on FIND-FIRST returning the FIRST record of a name: the
\ duplicate fixture record published before the MID watermark must win, and the
\ match must fold case like the shell's BOOT-XREF-STR=CI. The dictionary record
\ of a package word stores its bare tail, so the searched name is `DUP-MARK`.
\ Naming both fixture words keeps the duplicate load-bearing: if either package
\ stopped publishing the tail, the file would fail to load here instead of
\ leaving the index assertions below trivially satisfiable by a single record.
: FIRST-RECORD ( -- )
   BCG-DUP-EARLY:DUP-MARK
   BCG-DUP-LATE:DUP-MARK
   s" DUP-MARK" REC 0 >= TTRUE
   s" DUP-MARK" REC MID @ < TTRUE
   s" dup-mark" REC s" DUP-MARK" REC T= ;

\ One marker missing falls back to the found one; both missing is asserted at
\ the component level (FIND -> NOT-FOUND, MIN-FOUND keeps NOT-FOUND) because
\ BFR-MARKER-INDEX's both-missing path is a process exit (die 76) by design.
: MISSING-FALLBACK ( -- )
   s" IMK-NDICT0" s" BCG-NO-SUCH-MARKER" BFR-MARKER-INDEX IMK-REC T=
   s" BCG-NO-SUCH-MARKER" s" IMK-NDICT0" BFR-MARKER-INDEX IMK-REC T=
   s" BCG-NO-SUCH-MARKER" REC BFR-NOT-FOUND T=
   BFR-NOT-FOUND BFR-NOT-FOUND BFR-MIN-FOUND BFR-NOT-FOUND T=
   5 BFR-REQUIRE-INDEX 5 T= ;

public

: TEST ( -- )
   EARLIEST-MARKER
   FIRST-RECORD
   MISSING-FALLBACK ;

;package

\ Back into BCG for the remaining checks.
package BCG
private

: SMALL-BIN ( -- )
   s" tools/bootstrap.sh" LOAD
   s" hb-new" MUST-LACK
   s" hb-snap-src" MUST-LACK
   s" hb-snap0" MUST-LACK
   s" bootstrap check OK: %s/hb-stdin" MUST-HAVE
   S\" env HABU_UNDER_TEST=\q$T/hb-stdin\q \q$T/hb-stdin\q --load test/top-row-hook-test.f" MUST-HAVE
   s" mv " MUST-HAVE
   s" bin/hb" MUST-HAVE ;

: OWNER-PERSIST ( -- )
   s" bootstrap/cg/forth.fs" LOAD
   s" 3 constant SNAP-FORMAT-VERSION" MUST-HAVE
   s" 1 constant OWNER-API-PUB-WID" MUST-HAVE
   s" 2 constant OWNER-API-PRI-WID" MUST-HAVE
   s" 3 constant FIRST-DYNAMIC-WID" MUST-HAVE
   s" 256 constant RSTK-CELLS" MUST-HAVE
   s" $47C0 constant OWNER-WID-N-CELL" MUST-HAVE
   s" create PWID PRIM-CAP cells allot" MUST-HAVE
   S\" s\" FINALIZE\" ['] BOWNERFINALIZE OWNER-API-PUB-WID FPRIM-WID" MUST-HAVE
   s" LNCOUNT @ LBL,  #PL @ 1+ DCQ," MUST-HAVE
   s" OWNER-API-PUB-WID DCQ," MUST-HAVE
   s" OWNER-API-PRI-WID DCQ," MUST-HAVE
   s" : EMIT-SNAPSHOT-VALIDATE-WIDS" MUST-HAVE
   s" 13 9 40 LDR,  14 0 MOVN,  13 14 CMP,  C-EQ sds2 BCOND," MUST-HAVE
   s" 22 22 48 SUBI," MUST-HAVE
   s" 14 5 CMP,  C-NE snbadver BCOND," MUST-HAVE
   s" 6 FIRST-DYNAMIC-WID CMPI,  C-LT bad BCOND," MUST-HAVE
   s" 13 LSRC @ ADR,  14 13 25 SUB," MUST-HAVE
   s" C-GT snpresent BCOND," MUST-HAVE
   s" 4 4 DBASE SUB,  4 4 25 ADD," MUST-HAVE
   s" 9 FIRST-DYNAMIC-WID MOVZ,  9 DATA WIDN-CELL STR," MUST-HAVE ;

: OWNER-PUBLISH ( -- )
   s" src/habu/habu1.f" LOAD
   \ prot-wid-add publishes a set bit with acquire-load / release-store on the band
   \ word that holds it; the AOT restore release-publishes the shape tag only after
   \ the whole bitmap is copied. Both are the ordering the readers depend on.
   s" 16 15 LDAR," MUST-HAVE
   s" 16 15 STLR," MUST-HAVE
   s" src/habu/habu2.f" LOAD
   s" 4 5 STLR," MUST-HAVE
   s" src/habu/aot-capture.f" LOAD
   s" AOT-LIVE-DATA PROT-REG-TAG-CELL + atomic@" MUST-HAVE
   s" AOT-LIVE-DATA PROT-REG-TAG-CELL + AOT-CELL@" MUST-LACK
   s" AOT-LIVE-DATA OWNER-WID-N-CELL + atomic@" MUST-HAVE
   s" variable OWNER-PACKAGE-K" MUST-HAVE
   s" variable OWNER-PACKAGE-REC" MUST-LACK
   s" tools/build-fixpoint.f" LOAD
   s" PTR-VARIABLE KEEP-A" MUST-HAVE
   s" variable KEEP-A" MUST-LACK ;

\ Fixed-VA region mmap failures at boot must NOT be silent (dot
\ habu-diagnose-fixed-va-ed649528). A forced fixed-VA collision is not reliably
\ forcible from checked Habu on either host (no setrlimit spawn primitive, and
\ MAP_FIXED replaces existing mappings so pre-mapping cannot force ENOMEM), so
\ the regression pins the diagnostic BYTES in BOTH emitters: the message text is
\ present, the native length constants are exact, and each failure path names the
\ fault on fd 2 before its exit 78. The bytes live in the loaded __text image, so
\ the write is valid even though the region being mapped does not exist yet.
: MMAP-DIAG ( -- )
   s" src/habu/habu2.f" LOAD
   s" hb: cannot map fixed code region" MUST-HAVE
   s" hb: cannot map fixed data region" MUST-HAVE
   s" 33 constant MMAPCODE-MSG-LEN" MUST-HAVE
   s" 33 constant MMAPDATA-MSG-LEN" MUST-HAVE
   s" : EM-MMAP-CODE-REGION" POS-FOUND {: ncstart:n :}
   ncstart s" rvok LBL, ;" AFTER-FOUND {: ncend:n :}
   ncstart ncend s" LMMAPCODE LABEL@ ADR" MUST-FIND-BEFORE
   ncstart ncend s" MMAPCODE-MSG-LEN MOVZ" MUST-FIND-BEFORE
   ncstart ncend s" 78 MOVZ" MUST-FIND-BEFORE
   s" : EM-MMAP-DATA-REGION" POS-FOUND {: ndstart:n :}
   ndstart s" dvok LBL, ;" AFTER-FOUND {: ndend:n :}
   ndstart ndend s" LMMAPDATA LABEL@ ADR" MUST-FIND-BEFORE
   ndstart ndend s" MMAPDATA-MSG-LEN MOVZ" MUST-FIND-BEFORE
   ndstart ndend s" 78 MOVZ" MUST-FIND-BEFORE
   s" bootstrap/cg/forth.fs" LOAD
   s" hb: cannot map fixed code region" MUST-HAVE
   s" hb: cannot map fixed data region" MUST-HAVE
   s" : EMIT-MMAP-CODE-REGION" POS-FOUND {: mcstart:n :}
   mcstart s" rvok LBL," AFTER-FOUND {: mcend:n :}
   mcstart mcend s" hb: cannot map fixed code region" MUST-FIND-BEFORE
   mcstart mcend s" C-EXIT-DIAG" MUST-FIND-BEFORE
   s" : EMIT-MMAP-DATA-REGION" POS-FOUND {: mdstart:n :}
   mdstart s" dvok LBL, ;" AFTER-FOUND {: mdend:n :}
   mdstart mdend s" hb: cannot map fixed data region" MUST-FIND-BEFORE
   mdstart mdend s" C-EXIT-DIAG" MUST-FIND-BEFORE ;

;package

\ The compile preflight hook: both emitters must carry the missing-hook
\ diagnostic and the package resynchronisation path.
package BCG-PREFLIGHT
using BCG

: RECOVERY ( -- )
   s" bootstrap/cg/forth.fs" LOAD
   s" 35 constant PREFMISSMSG-LEN" MUST-HAVE
   S\" LPREFMISSMSG @ LBL, S\\\" hb: compile preflight hook missing\\n\" BYTES," MUST-HAVE
   s" 9 DATA COMPILE-PREFLIGHT-CELL LDR,  9 LPREFMISS @ CBZ" MUST-HAVE
   s" 2 PREFMISSMSG-LEN MOVZ" MUST-HAVE
   s" : EMIT-PREFMISS-RECOVER" MUST-HAVE
   s" 12 PKGSNAP-OFF LIT64," MUST-HAVE
   s" $27C0 constant PKGRESYNC-CELL" MUST-HAVE
   s" : EMIT-PKG-RESYNC" MUST-HAVE
   s" 9 DATA PKGRESYNC-CELL LDR," MUST-HAVE
   s" LCHKENDPKG 19 C-PACKAGE-CHECK-CALL" MUST-HAVE
   s" 12 1 MOVZ,  12 DATA PKGRESYNC-CELL STR," MUST-HAVE
   s" LTHROWDISPATCH @ B," MUST-HAVE ;

: NATIVE ( -- )
   s" src/habu/habu2.f" LOAD
   s" 35 constant PREFMISSMSG-LEN" MUST-HAVE
   S\" LPREFMISSMSG LABEL@ LBL, S\\\" hb: compile preflight hook missing\\n\" BYTES," MUST-HAVE
   s" 9 DATA COMPILE-PREFLIGHT-CELL LDR,  9 LPREFMISS LABEL@ CBZ" MUST-HAVE
   s" 2 PREFMISSMSG-LEN MOVZ" MUST-HAVE ;

public

: TEST ( -- )
   RECOVERY
   NATIVE ;

;package

\ `using NAME` / `;using` must exist in BOTH engines with the same data band, the
\ same emitters and the same recovery fixtures. The Gforth stage0 engine compiles
\ src/core/checker.f during recovery, and the checker reads the live using depth
\ through a hard-coded DATA offset (CK-USE-DEPTH-OFF), so the band offsets are a
\ cross-file contract, not a local layout choice.
package BCG-USING
using BCG

: BAND ( -- )   \ the four band offsets, shared by both engines and the checker
   s" 40 constant PKGSNAP-USE" MUST-HAVE
   s" 16 constant USE-MAX" MUST-HAVE
   s" PKGSNAP-END constant USE-BAND-OFF" MUST-HAVE
   s" constant USE-DEPTH-CELL" MUST-HAVE
   s" constant USE-PKG-SAVE-CELL" MUST-HAVE
   s" constant USE-WIDS-OFF" MUST-HAVE
   s" USE-WIDS-OFF USE-MAX cells + constant USE-BAND-END" MUST-HAVE ;

\ The two engines legitimately diverge at the band that consumes USE-BAND-END.
\ The native engine chains through the snapshot relocation bands (the call map
\ anchors on USE-BAND-END and DATA-START derives from the relocation band end),
\ while the Gforth-built stage0 engine deliberately carries no relocation bands
\ - each build path keeps its own snapshot wire format - so its DATA-START
\ still anchors directly on USE-BAND-END. Each engine pins its own consumer;
\ when either chain changes, its pin changes with it.
: BAND-RECOVERY-END ( -- )
   s" USE-BAND-END constant DATA-START" MUST-HAVE ;

: BAND-NATIVE-END ( -- )
   s" USE-BAND-END constant CALLMAP-OFF" MUST-HAVE ;

: KEYWORDS ( -- )   \ the emitters both engines carry
   s" : C-USING-NAME-GUARD" MUST-HAVE
   s" : C-USING-WID" MUST-HAVE
   s" : C-USING-PUSH" MUST-HAVE
   s" : C-USING (" MUST-HAVE
   s" : C-END-USING" MUST-HAVE
   s" : EMIT-FIND-USED" MUST-HAVE
   s" hb: using: missing package name" MUST-HAVE
   s" hb: using: unknown package: " MUST-HAVE
   s" hb: using: too many concurrent usings: " MUST-HAVE
   s" hb: ;using without an open using" MUST-HAVE
   s" hb: ambiguous bare word resolves in multiple used packages: " MUST-HAVE ;

: RECOVERY ( -- )
   s" bootstrap/cg/forth.fs" LOAD
   BAND
   BAND-RECOVERY-END
   KEYWORDS
   s" LKWUSING @ LBL," MUST-HAVE
   s" LCHKUSING @ LBL," MUST-HAVE
   s" LBL LKWUSING !" MUST-HAVE
   s" LBL LFINDUSED !" MUST-HAVE
   s" lmain LKWUSING 5 " MUST-HAVE
   s" lmain LKWSEMIUSING 6 " MUST-HAVE
   s" LFINDUSED @ BL," MUST-HAVE
   s" 89 constant ENGINE-ERROR:USING-NO-NAME" MUST-HAVE
   s" 94 constant ENGINE-ERROR:USING-AMBIGUOUS" MUST-HAVE
   \ stage0 takes no REPL-line package/using snapshot, so it must not claim the
   \ native cell that restores one; the offset stays reserved instead.
   s" USE-RPKG-SAVE-CELL" MUST-LACK ;

: NATIVE ( -- )
   s" src/habu/habu2.f" LOAD
   KEYWORDS
   s" LKWUSING LABEL@ LBL," MUST-HAVE
   s" LFINDUSED LABEL@ BL," MUST-HAVE
   s" src/habu/layout.f" LOAD
   BAND
   BAND-NATIVE-END
   s" src/core/engine-error.f" LOAD
   s" 89 constant USING-NO-NAME" MUST-HAVE
   s" 94 constant USING-AMBIGUOUS" MUST-HAVE
   s" src/core/checker.f" LOAD
   s" $9C08 constant CK-USE-DEPTH-OFF" MUST-HAVE ;

: FIXTURES ( -- )   \ the recovery run executes stage0 on real using sources
   s" tools/bootstrap.sh" LOAD
   s" bootstrap_using_gate" MUST-HAVE
   s" bootstrap_using_case bootstrap-using 0" MUST-HAVE
   s" bootstrap_using_case bootstrap-using-unknown 91" MUST-HAVE
   s" bootstrap_using_case bootstrap-using-ambiguous 94" MUST-HAVE
   s" bootstrap_using_case bootstrap-using-scope 70" MUST-HAVE
   s" bootstrap_using_case bootstrap-using-checker-hook 0" MUST-HAVE ;

public

: TEST ( -- )
   RECOVERY
   NATIVE
   FIXTURES ;

;package

\ The entry point, and the only word this file exports to top level.
package BCG
public

: MAIN ( -- )
   T-RESET
   MMAP-DIAG
   BCG-PREFLIGHT:TEST
   BCG-USING:TEST
   INSTALL-FAIL-CLOSED
   FORTH-SDQ-COMMENT
   PREFIX-LIST
   TOK-IMM-MIRROR
   PROTWID-LEAFNESS
   ENGINE-ERROR-CODES
   CELL-PARITY
   BCG-CAP:TEST
   BAKED-PREFIX
   TRUST-CALLS
   IMAGE-BUFFER
   ASM-CHECKED
   GFORTH-LOCALS
   GFORTH-LOCAL-CAPTURE
   SPAWN-SCOPED-LABELS
   DATA-SIZE-MIRROR
   PROF-CNT-HIGH
   DP-CHECK-BAND-CAP
   PUBLISH-HOOK-SPLIT
   LOCAL-SHADOW
   HIDE-PRELUDE
   PROLOGUE-UNCONDITIONAL
   BCG-HIDE:TEST
   OWNER-PERSIST
   OWNER-PUBLISH
   SMALL-BIN
   T-REPORT
   s" bootstrap-codegen-test: ok" type cr ;

;package

BCG:MAIN
