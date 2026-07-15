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

: BCG-TEST-PREFIX-LOAD-CALLS ( -- )
   s" : PFX-LOAD-BASE-FILES" BCG-POS-FOUND {: base:n :}
   base s" PFX-LOAD-CHECKER-FILES" BCG-AFTER-FOUND {: check:n :}
   check s" PFX-LOAD-DECL-FILES" BCG-AFTER-FOUND {: decl:n :}
   decl s" PFX-LOAD-CORE-FILES" BCG-AFTER-FOUND {: core:n :}
   base check < TTRUE  check decl < TTRUE  decl core < TTRUE ;

: BCG-TEST-PREFIX-PATH-CALLS ( -- )
   s" : PFX-PATH-FILES" BCG-POS-FOUND {: base:n :}
   base s" PFX-PATH-CHECKER-FILES" BCG-AFTER-FOUND {: check:n :}
   check s" PFX-PATH-DECL-FILES" BCG-AFTER-FOUND {: decl:n :}
   decl s" PFX-PATH-CORE-FILES" BCG-AFTER-FOUND {: core:n :}
   base check < TTRUE  check decl < TTRUE  decl core < TTRUE ;

: BCG-TEST-PREFIX-PROVIDE-CALLS ( -- )
   s" : PFX-PROVIDE-FILES" BCG-POS-FOUND {: base:n :}
   base s" PFX-PROVIDE-CHECKER-FILES" BCG-AFTER-FOUND {: check:n :}
   check s" PFX-PROVIDE-DECL-FILES" BCG-AFTER-FOUND {: decl:n :}
   decl s" PFX-PROVIDE-CORE-FILES" BCG-AFTER-FOUND {: core:n :}
   base check < TTRUE  check decl < TTRUE  decl core < TTRUE ;

: BCG-TEST-PREFIX-PHASE-CALLS ( -- )
   BCG-TEST-PREFIX-LOAD-CALLS
   BCG-TEST-PREFIX-PATH-CALLS
   BCG-TEST-PREFIX-PROVIDE-CALLS ;

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
   s" LPLINUXTARGET @ LBL, s" BCG-MUST-LACK
   s" PFX-COMMON LPUTIL" s" PFX-COMMON LPCELL" BCG-MUST-BEFORE
   s" PFX-COMMON LPCELL" s" PFX-COMMON LPPTRSTORAGE" BCG-MUST-BEFORE
   s" PFX-COMMON LPPTRSTORAGE" s" PFX-COMMON LPENGINEERROR" BCG-MUST-BEFORE
   s" PFX-COMMON LPENGINEERROR" s" PFX-COMMON LPCHECKER" BCG-MUST-BEFORE
   s" PFX-COMMON LPCHECKER" s" PFX-COMMON LPENGINEERROREFFECTS" BCG-MUST-BEFORE
   s" PFX-COMMON LPENGINEERROREFFECTS" s" PFX-COMMON LPLOWERCERTBASE" BCG-MUST-BEFORE
   s" PFX-COMMON LPLOWERCERTBASE" s" PFX-COMMON LPTYPESCHEMA" BCG-MUST-BEFORE
   s" PFX-COMMON LPTYPESCHEMA" s" PFX-COMMON LPTYPEFAM" BCG-MUST-BEFORE
   s" PFX-COMMON LPTYPEFAM" s" PFX-COMMON LPRENDER" BCG-MUST-BEFORE
   s" PFX-COMMON LPRENDER" s" PFX-COMMON LPSUMTYPE" BCG-MUST-BEFORE
   s" PFX-COMMON LPSUMTYPE" s" PFX-COMMON LPLAYOUTBUF" BCG-MUST-BEFORE
   s" PFX-COMMON LPLAYOUTBUF" s" PFX-COMMON LPLAYOUTVALID" BCG-MUST-BEFORE
   s" PFX-COMMON LPLAYOUTVALID" s" PFX-COMMON LPHOOK" BCG-MUST-BEFORE
   BCG-TEST-PREFIX-PHASE-CALLS
   s" PFX-COMMON LPCHECKER" s" PFX-LINUX  LPLINUXTARGET" BCG-MUST-BEFORE
   s" PFX-COMMON LPCHECKER" s" PFX-MACOS  LPMACOSTARGET" BCG-MUST-BEFORE
   s" PFX-COMMON LPHOOK" s" PFX-LINUX  LPLINUXTARGET" BCG-MUST-BEFORE
   s" PFX-COMMON LPHOOK" s" PFX-MACOS  LPMACOSTARGET" BCG-MUST-BEFORE
   s" PFX-COMMON LPHOOK" s" PFX-COMMON LPCELLEFF" BCG-MUST-BEFORE
   s" PFX-COMMON LPCELLEFF" s" PFX-COMMON LPPTRSTORAGEEFF" BCG-MUST-BEFORE
   s" PFX-COMMON LPPTRSTORAGEEFF" s" PFX-COMMON LPSTRUCTURES" BCG-MUST-BEFORE
   s" PFX-COMMON LPSTRUCTURES" s" PFX-COMMON LPROLES" BCG-MUST-BEFORE
   s" PFX-COMMON LPROLES" s" PFX-COMMON LPBYTES" BCG-MUST-BEFORE
   s" PFX-COMMON LPBYTES" s" PFX-LINUX  LPLINUXTARGET" BCG-MUST-BEFORE
   s" PFX-COMMON LPBYTES" s" PFX-MACOS  LPMACOSTARGET" BCG-MUST-BEFORE
   s" PFX-COMMON LPSTRUCTURES" s" PFX-COMMON LPENUMS" BCG-MUST-BEFORE
   s" PFX-COMMON LPENUMS" s" PFX-COMMON LPEXECVECTOR" BCG-MUST-BEFORE
   s" PFX-COMMON LPEXECVECTOR" s" PFX-COMMON LPSHA256" BCG-MUST-BEFORE
   s" PFX-COMMON LPSHA256" s" PFX-COMMON LPCOMBINATORS" BCG-MUST-BEFORE ;

: BCG-TEST-PREFIX-LIST-BOOTSTRAP ( -- )
   s" bootstrap/cg/forth.fs" BCG-LOAD
   BCG-TEST-PREFIX-LIST-COMMON
   s" PFX-LOAD-DECL-FILES" BCG-MUST-HAVE
   s" PFX-PATH-DECL-FILES" BCG-MUST-HAVE
   s" PFX-PROVIDE-DECL-FILES" BCG-MUST-HAVE
   s" src/core/structures-effects.f" BCG-MUST-LACK
   s" LPSTRUCTEFF" BCG-MUST-LACK
   s" PFX-COMMON LPROLES" s" PFX-COMMON LPINCLUDE" BCG-MUST-BEFORE
   s" PFX-COMMON LPSTRUCTURES" s" PFX-COMMON LPINCLUDE" BCG-MUST-BEFORE
   s" LSRCRD @ BL," BCG-MUST-HAVE
   s" LSRCRD LABEL@ BL," BCG-MUST-LACK ;

: BCG-TEST-PREFIX-LIST-NATIVE ( -- )
   s" src/habu/habu2.f" BCG-LOAD
   BCG-TEST-PREFIX-LIST-COMMON
   s" PFX-LOAD-DECL-FILES" BCG-MUST-HAVE
   s" PFX-PATH-DECL-FILES" BCG-MUST-HAVE
   s" PFX-PROVIDE-DECL-FILES" BCG-MUST-HAVE
   s" src/core/structures-effects.f" BCG-MUST-LACK
   s" LPSTRUCTEFF" BCG-MUST-LACK
   s" PFX-COMMON LPROLES" s" PFX-COMMON LPINCLUDE" BCG-MUST-BEFORE
   s" PFX-COMMON LPSTRUCTURES" s" PFX-COMMON LPINCLUDE" BCG-MUST-BEFORE
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

: BCG-TEST-BOOTSTRAP-SEAM ( -- )
   s" emit_src()" BCG-POS-FOUND {: base:n :}
   base s" cat src/core/pointer-storage-effects.f" BCG-AFTER-FOUND {: check:n :}
   check s" emit_decl_src" BCG-AFTER-FOUND {: decl:n :}
   decl s" cat src/core/structures.f" BCG-AFTER-FOUND {: core:n :}
   core s" LOWER-CERT-HOOK:INSTALL" BCG-AFTER-FOUND {: install:n :}
   base check < TTRUE  check decl < TTRUE
   decl core < TTRUE  core install < TTRUE ;

: BCG-TEST-CELL-BOOTSTRAP ( -- )
   s" tools/bootstrap.sh" BCG-LOAD
   s" cat src/core/util.f" s" cat src/core/cell.f" BCG-MUST-BEFORE
   s" cat src/core/cell.f" s" cat src/core/pointer-storage.f" BCG-MUST-BEFORE
   s" cat src/core/pointer-storage.f" s" cat src/core/engine-error.f" BCG-MUST-BEFORE
   s" cat src/core/engine-error.f" s" cat src/core/checker.f" BCG-MUST-BEFORE
   s" cat src/core/checker.f" s" cat src/core/engine-error-effects.f" BCG-MUST-BEFORE
   s" cat src/core/type-family.f" s" cat src/core/render.f" BCG-MUST-BEFORE
   s" cat src/core/render.f" s" cat src/core/sumtype.f" BCG-MUST-BEFORE
   s" cat src/core/layout-valid.f" s" cat src/core/check-hook.f" BCG-MUST-BEFORE
   s" cat src/core/check-hook.f" s" cat src/core/cell-effects.f" BCG-MUST-BEFORE
   s" cat src/core/cell-effects.f" s" cat src/core/pointer-storage-effects.f" BCG-MUST-BEFORE
   s" cat src/core/pointer-storage-effects.f" s" cat src/core/structures.f" BCG-MUST-BEFORE
   s" cat src/core/structures-effects.f" BCG-MUST-LACK
   BCG-TEST-BOOTSTRAP-SEAM ;

: BCG-TEST-FIXPOINT-SEAM ( -- )
   s" : BF-APPEND-RUN-PRELUDE" BCG-POS-FOUND {: base:n :}
   base s" BF-APPEND-CHECKER-BOOT" BCG-AFTER-FOUND {: check:n :}
   check s" BF-APPEND-DECL-FILES" BCG-AFTER-FOUND {: decl:n :}
   decl s" BF-APPEND-CORE-FILES" BCG-AFTER-FOUND {: core:n :}
   core s" LOWER-CERT-HOOK:INSTALL" BCG-AFTER-FOUND {: install:n :}
   base check < TTRUE  check decl < TTRUE
   decl core < TTRUE  core install < TTRUE ;

: BCG-TEST-CELL-FIXPOINT ( -- )
   s" tools/build-fixpoint.f" BCG-LOAD
   s" src/core/util.f" s" src/core/cell.f" BCG-MUST-BEFORE
   s" src/core/cell.f" s" src/core/pointer-storage.f" BCG-MUST-BEFORE
   s" src/core/pointer-storage.f" s" src/core/engine-error.f" BCG-MUST-BEFORE
   s" src/core/engine-error.f" s" src/core/checker.f" BCG-MUST-BEFORE
   s" src/core/checker.f" s" src/core/engine-error-effects.f" BCG-MUST-BEFORE
   s" src/core/type-family.f" s" src/core/render.f" BCG-MUST-BEFORE
   s" src/core/render.f" s" src/core/sumtype.f" BCG-MUST-BEFORE
   s" src/core/layout-valid.f" s" src/core/check-hook.f" BCG-MUST-BEFORE
   s" src/core/check-hook.f" s" src/core/cell-effects.f" BCG-MUST-BEFORE
   s" src/core/cell-effects.f" s" src/core/pointer-storage-effects.f" BCG-MUST-BEFORE
   s" src/core/pointer-storage-effects.f" s" src/core/structures.f" BCG-MUST-BEFORE
   s" src/core/structures-effects.f" BCG-MUST-LACK
   BCG-TEST-FIXPOINT-SEAM ;

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
