\ bootstrap-codegen-test.f - native source regression for bootstrap codegen hard cutover.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f tools/bootstrap-codegen-test.f

require lib/errors.f
require lib/string.f
require lib/adt/option.f                 \ option<idx> FIND-SUB consumer (switchover wave A)
require lib/test.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/source-lex.f

$40000 constant BCG-CAP

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

: BCG-POS ( ptr u8 n -- option<idx> )
   BCG-SOURCE 2swap FIND-SUB ;

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
   src start + srcu start - needle nu FIND-SUB MATCH option
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
   s" PFX-COMMON LPCELL" s" PFX-COMMON LPSTRUCTURES" BCG-MUST-BEFORE
   s" PFX-COMMON LPSTRUCTURES" s" PFX-COMMON LPCHECKER" BCG-MUST-BEFORE
   s" PFX-COMMON LPCHECKER" s" PFX-LINUX  LPLINUXTARGET" BCG-MUST-BEFORE
   s" PFX-COMMON LPCHECKER" s" PFX-MACOS  LPMACOSTARGET" BCG-MUST-BEFORE
   s" PFX-COMMON LPHOOK" s" PFX-LINUX  LPLINUXTARGET" BCG-MUST-BEFORE
   s" PFX-COMMON LPHOOK" s" PFX-MACOS  LPMACOSTARGET" BCG-MUST-BEFORE
   s" PFX-COMMON LPHOOK" s" PFX-COMMON LPCELLEFF" BCG-MUST-BEFORE
   s" PFX-COMMON LPCELLEFF" s" PFX-COMMON LPSTRUCTEFF" BCG-MUST-BEFORE
   s" PFX-COMMON LPSTRUCTEFF" s" PFX-COMMON LPROLES" BCG-MUST-BEFORE
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
   s" PFX-COMMON LPROLES" s" PFX-COMMON LPINCLUDE" BCG-MUST-BEFORE
   s" PFX-COMMON LPSTRUCTURES" s" PFX-COMMON LPINCLUDE" BCG-MUST-BEFORE
   s" LSRCRD @ BL," BCG-MUST-HAVE
   s" LSRCRD LABEL@ BL," BCG-MUST-LACK ;

: BCG-TEST-PREFIX-LIST-NATIVE ( -- )
   s" src/habu/habu2.f" BCG-LOAD
   BCG-TEST-PREFIX-LIST-COMMON
   s" PFX-COMMON LPROLES" s" PFX-COMMON LPINCLUDE" BCG-MUST-BEFORE
   s" PFX-COMMON LPSTRUCTURES" s" PFX-COMMON LPINCLUDE" BCG-MUST-BEFORE
   s" LSRCRD LABEL@ BL," BCG-MUST-HAVE ;

: BCG-TEST-PREFIX-LIST ( -- )
   BCG-TEST-PREFIX-LIST-BOOTSTRAP
   BCG-TEST-PREFIX-LIST-NATIVE ;

: BCG-TEST-CELL-RUNTIME ( -- )
   CELL-WIDTH-CHECK
   CELL 1 cells T= ;

: BCG-TEST-CELL-SOURCE ( -- )
   s" src/core/cell.f" BCG-LOAD
   s" $8 constant CELL" BCG-MUST-HAVE
   s" $4C constant CORE-LAYOUT-RC" BCG-MUST-HAVE
   s" 1 cells CELL <>" BCG-MUST-HAVE
   s" CORE-LAYOUT-RC die" BCG-MUST-HAVE ;

: BCG-TEST-CELL-BOOTSTRAP ( -- )
   s" tools/bootstrap.sh" BCG-LOAD
   s" cat src/core/util.f" s" cat src/core/cell.f" BCG-MUST-BEFORE
   s" cat src/core/cell.f" s" cat src/core/structures.f" BCG-MUST-BEFORE
   s" cat src/core/check-hook.f" s" cat src/core/cell-effects.f" BCG-MUST-BEFORE
   s" cat src/core/cell-effects.f" s" cat src/core/structures-effects.f" BCG-MUST-BEFORE ;

: BCG-TEST-CELL-FIXPOINT ( -- )
   s" tools/build-fixpoint.f" BCG-LOAD
   s" src/core/util.f" s" src/core/cell.f" BCG-MUST-BEFORE
   s" src/core/cell.f" s" src/core/structures.f" BCG-MUST-BEFORE
   s" src/core/check-hook.f" s" src/core/cell-effects.f" BCG-MUST-BEFORE
   s" src/core/cell-effects.f" s" src/core/structures-effects.f" BCG-MUST-BEFORE ;

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

: BCG-MAIN ( -- )
   T-RESET
   BCG-TEST-INSTALL-FAIL-CLOSED
   BCG-TEST-FORTH-SDQ-COMMENT
   BCG-TEST-PREFIX-LIST
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
   BCG-TEST-BOOTSTRAP-SMALL-BIN
   T-REPORT
   s" bootstrap-codegen-test: ok" type cr ;

BCG-MAIN
