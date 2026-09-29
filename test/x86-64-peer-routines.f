\ x86-64-peer-routines.f - every HIR fixture of test/compiler/x64-emit.f that
\ the x86-64 rows emit, and the pressure fixtures of test/compiler/x64-chain.f
\ whose spills the rows lower into a frame, cross-built into an executable for an
\ x86-64 peer. Each is driven the way src/compiler/native/compiler.f drives a
\ definition - declare, select, prune, fixpoint, emit at the image's own
\ address, retire - and the routine is wrapped in test/x86-64-peer-harness.f's
\ checking entry with the answers its definition must give. Each fixture writes
\ one positive image into $HB_TMP/x64-routines; diff also writes a negative
\ harness image whose first case expects a wrong answer. The manifest lists each
\ file with its expected status; docs/bootstrap.md gives the peer's comparison.
\
\ Two fixtures have no image. The rows refuse BUILD-ADDRESSED with
\ E-IR-VERIFY-OPTYPE: it takes its memory order as an argument, which a
\ data-stack contract has no cell for. The last check, after every image is
\ written, asserts that refusal. BUILD-SELFCALLER is RECURSE with no base case,
\ so it never returns.
require lib/test.f
require lib/string.f
require lib/fmt.f
require lib/byte-buffer.f
require lib/fs.f
require lib/fs-mutate.f
require src/compiler/native/backend.f
require src/arch/x86-64/passes.f
require test/compiler/x64-emit-fixture.f
require test/compiler/x64-chain-fixture.f
require test/x86-64-peer-harness.f

\ The fixtures stay in the package that stages them; this adds each one's trip
\ through the rows to the harness's next address.
package X64EMIT-TEST
private
variable CALLEE                      \ the entry the wordcall site names

: ROWS, ( n n NBACK:linkage -- ) {: in:n out:n l:NBACK:linkage :}
   CC in out l NBACK:DECLARE
   CC BB NBACK:SELECT {: m0:IR-BUILD:module :}
   CC m0 NBACK:PRUNE {: m1:IR-BUILD:module :}
   CC m1 NBACK:FIXPOINT {: m:IR-BUILD:module :}
   CC m X64HARNESS:POSITION NBACK:EMIT
   X64EMIT:BYTES X64EMIT:SIZE X64HARNESS:APPEND-ROUTINE
   CC NBACK:RETIRE
   CC NBACK:RELEASE ;

: DIFF-BODY ( IR-CTX:ctx -- )     HIR-MOD BUILD-DIFF 2 1 NBACK:L-NONE ROWS, ;
: SQUARE-BODY ( IR-CTX:ctx -- )   HIR-MOD BUILD-SQUARE 1 1 NBACK:L-NONE ROWS, ;
\ The byte oracle in x64-emit-fixture.f pins BUILD-CHAIN's four tied binaries.
\ Its value is always zero, so the peer uses a toggled low bit for the OR.
\ With a=11 and b=2, the answer is 16; replacing AND with its first operand
\ answers 18, and omitting MUL answers 8.
: BUILD-CHAIN-PEER ( -- )
   2 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   ARG+ {: b:IR-ID:ir-value-id :}
   HIR-OPCODE:XOR a 1 CONSTOP BINOP {: toggled:IR-ID:ir-value-id :}
   HIR-OPCODE:AND a b BINOP {: common:IR-ID:ir-value-id :}
   HIR-OPCODE:OR common toggled BINOP {: both:IR-ID:ir-value-id :}
   HIR-OPCODE:XOR both b BINOP {: rest:IR-ID:ir-value-id :}
   HIR-OPCODE:MUL rest b BINOP RET1
   CLOSE-FUN ;

: CHAIN-BODY ( IR-CTX:ctx -- )    HIR-MOD BUILD-CHAIN-PEER 2 1 NBACK:L-NONE ROWS, ;
: IMMS-BODY ( IR-CTX:ctx -- )     HIR-MOD BUILD-IMMS 2 1 NBACK:L-NONE ROWS, ;
: SHIFTS-BODY ( IR-CTX:ctx -- )   HIR-MOD BUILD-SHIFTS 1 1 NBACK:L-NONE ROWS, ;
: NOT-BODY ( IR-CTX:ctx -- )      HIR-MOD BUILD-NOT 1 1 NBACK:L-NONE ROWS, ;
: CMPSET-BODY ( IR-CTX:ctx -- )   HIR-MOD BUILD-CMPSET 2 1 NBACK:L-NONE ROWS, ;
: CMPSETI-BODY ( IR-CTX:ctx -- )  HIR-MOD BUILD-CMPSETI 2 1 NBACK:L-NONE ROWS, ;
: MOVI-BODY ( IR-CTX:ctx -- )     HIR-MOD BUILD-MOVI 1 1 NBACK:L-NONE ROWS, ;
: DADDR-BODY ( IR-CTX:ctx -- )
   HIR-MOD BUILD-DADDRESSED 1 1 NBACK:L-NONE ROWS, ;
: LOOP-BODY ( IR-CTX:ctx -- )     HIR-MOD BUILD-LOOP 2 1 NBACK:L-NONE ROWS, ;
: WORDCALL-BODY ( IR-CTX:ctx -- )
   HIR-MOD CALLEE @ BUILD-WORDCALLER 1 1 NBACK:L-CALLED ROWS, ;

\ A refused definition ends the way src/compiler/native/compiler.f ends one:
\ what the refusal left bound is released, then the emission retired.
: ADDRESSED-REFUSED ( IR-CTX:ctx -- )
   HIR-MOD BUILD-ADDRESSED
   s" the rows refuse the addressed fixture: a data-stack contract has no cell for the memory order it takes as an argument" T-LABEL
   [: 2 1 NBACK:L-NONE ROWS, ;] E-IR-VERIFY-OPTYPE TTHROWSQ
   CC NBACK:RELEASE
   CC NBACK:RETIRE ;

public
: DIFF-ROUTINE ( -- )     WBND [: DIFF-BODY ;] IR-CTX:WITH-CONTEXT ;
: SQUARE-ROUTINE ( -- )   WBND [: SQUARE-BODY ;] IR-CTX:WITH-CONTEXT ;
: CHAIN-ROUTINE ( -- )    WBND [: CHAIN-BODY ;] IR-CTX:WITH-CONTEXT ;
: IMMS-ROUTINE ( -- )     WBND [: IMMS-BODY ;] IR-CTX:WITH-CONTEXT ;
: SHIFTS-ROUTINE ( -- )   WBND [: SHIFTS-BODY ;] IR-CTX:WITH-CONTEXT ;
: NOT-ROUTINE ( -- )      WBND [: NOT-BODY ;] IR-CTX:WITH-CONTEXT ;
: CMPSET-ROUTINE ( -- )   WBND [: CMPSET-BODY ;] IR-CTX:WITH-CONTEXT ;
: CMPSETI-ROUTINE ( -- )  WBND [: CMPSETI-BODY ;] IR-CTX:WITH-CONTEXT ;
: MOVI-ROUTINE ( -- )     WBND [: MOVI-BODY ;] IR-CTX:WITH-CONTEXT ;
: DADDR-ROUTINE ( -- )    WBND [: DADDR-BODY ;] IR-CTX:WITH-CONTEXT ;
: LOOP-ROUTINE ( -- )     WBND [: LOOP-BODY ;] IR-CTX:WITH-CONTEXT ;
: WORDCALL-ROUTINE ( n -- )
   CALLEE !  WBND [: WORDCALL-BODY ;] IR-CTX:WITH-CONTEXT ;
: ADDRESSED-REFUSAL ( -- ) WBND [: ADDRESSED-REFUSED ;] IR-CTX:WITH-CONTEXT ;
;package

\ The same trip for the fixtures the chain suite stages. Twelve values live at
\ once do not fit the nine registers, so the fixpoint puts some away in a frame
\ the routine reserves on rsp: the harness's own balance check is what says the
\ frame was given back exactly.
package X64CHAIN-TEST
private
variable CALLEE                      \ the entry the caller's site names

: ROWS, ( n n NBACK:linkage -- )
   CHAIN-LINKED {: m:IR-BUILD:module :}
   CC m X64HARNESS:POSITION NBACK:EMIT
   X64EMIT:BYTES X64EMIT:SIZE X64HARNESS:APPEND-ROUTINE
   CC NBACK:RETIRE
   CC NBACK:RELEASE ;

: PRESS-ROWS ( IR-CTX:ctx -- )    HIR-MOD BUILD-PRESSURE 1 1 NBACK:L-NONE ROWS, ;
: PBRANCH-ROWS ( IR-CTX:ctx -- )  HIR-MOD BUILD-PBRANCH 2 1 NBACK:L-NONE ROWS, ;
: PLOOP-ROWS ( IR-CTX:ctx -- )    HIR-MOD BUILD-PLOOP 2 1 NBACK:L-NONE ROWS, ;
: PCALLER-ROWS ( IR-CTX:ctx -- )
   HIR-MOD CALLEE @ BUILD-PCALLER 1 1 NBACK:L-CALLED ROWS, ;

public
: PRESSURE-ROUTINE ( -- )  WBND [: PRESS-ROWS ;] IR-CTX:WITH-CONTEXT ;
: PBRANCH-ROUTINE ( -- )   WBND [: PBRANCH-ROWS ;] IR-CTX:WITH-CONTEXT ;
: PLOOP-ROUTINE ( -- )     WBND [: PLOOP-ROWS ;] IR-CTX:WITH-CONTEXT ;
: PCALLER-ROUTINE ( n -- )
   CALLEE !  WBND [: PCALLER-ROWS ;] IR-CTX:WITH-CONTEXT ;
;package

package X64ROUTINES
using X64HARNESS
private

create MANIFEST BUF:HDR-BYTES allot

: DIR$ ( -- ptr u8 n ) s" x64-routines" ;

: MANIFEST+ ( ptr u8 n -- ) BUF:N>BLEN MANIFEST BUF:APPEND-SPAN ;

\ Write the staged image into the directory as `name`, or `name-negative`, and
\ list it in the manifest with the status the peer must see it exit with.
: WRITE-IMAGE ( ptr u8 n bool -- ) {: a:ptr u:n negative:bool :}
   SB-RESET DIR$ SB-APPEND s" /" SB-APPEND a u SB-APPEND
   negative if s" -negative" SB-APPEND then
   SB$ {: rel:ptr relu:n :}
   DIR$ nip 1+ {: skip:n :}
   rel skip + relu skip - MANIFEST+
   STR-SPACE MANIFEST BUF:APPEND-BYTE
   rel relu TMP-PATH {: path:ptr pathu:n :}
   SB-RESET negative if FIRST-CASE else 0 then FMT:SB-U SB$ MANIFEST+
   STR-LF MANIFEST BUF:APPEND-BYTE
   path pathu WRITE-ELF ;

: DIFF-IMAGE ( bool -- ) {: negative:bool :}
   negative OPEN,
   20 7 13 CASE2,
   MIN-CELL 1 MAX-CELL CASE2,
   MAX-CELL -1 MIN-CELL CASE2,
   CLOSE, ENTRY, X64EMIT-TEST:DIFF-ROUTINE
   s" diff" negative WRITE-IMAGE ;

: SQUARE-IMAGE ( -- )
   false OPEN,
   21 42 CASE1,
   -21 -42 CASE1,
   MAX-CELL -2 CASE1,
   MIN-CELL 0 CASE1,
   CLOSE, ENTRY, X64EMIT-TEST:SQUARE-ROUTINE
   s" square" false WRITE-IMAGE ;

: CHAIN-IMAGE ( -- )
   false OPEN,
   11 2 16 CASE2,
   5 2 12 CASE2,
   -1 2 -8 CASE2,
   CLOSE, ENTRY, X64EMIT-TEST:CHAIN-ROUTINE
   s" chain" false WRITE-IMAGE ;

\ `((b and a) + 1000 - 2000) and 4095 or 61440 xor 255`.
: IMMS-IMAGE ( -- )
   false OPEN,
   -1 5 64738 CASE2,
   0 0 64743 CASE2,
   -1 -1 64744 CASE2,
   CLOSE, ENTRY, X64EMIT-TEST:IMMS-ROUTINE
   s" imms" false WRITE-IMAGE ;

\ `3 lshift 5 rshift`: the right shift is logical.
: SHIFTS-IMAGE ( -- )
   false OPEN,
   1000 250 CASE1,
   -1 $07FFFFFFFFFFFFFF CASE1,
   MIN-CELL 0 CASE1,
   CLOSE, ENTRY, X64EMIT-TEST:SHIFTS-ROUTINE
   s" shifts" false WRITE-IMAGE ;

: NOT-IMAGE ( -- )
   false OPEN,
   0 -1 CASE1,
   5 -6 CASE1,
   MIN-CELL MAX-CELL CASE1,
   CLOSE, ENTRY, X64EMIT-TEST:NOT-ROUTINE
   s" not" false WRITE-IMAGE ;

\ A true flag is all ones, as the ARM64 emitter's CSETM makes it.
: CMPSET-IMAGE ( -- )
   false OPEN,
   5 2 0 CASE2,
   2 5 -1 CASE2,
   MIN-CELL MAX-CELL -1 CASE2,
   -1 -1 0 CASE2,
   CLOSE, ENTRY, X64EMIT-TEST:CMPSET-ROUTINE
   s" cmpset" false WRITE-IMAGE ;

\ `b a - 1000 <`.
: CMPSETI-IMAGE ( -- )
   false OPEN,
   0 5000 0 CASE2,
   10 500 -1 CASE2,
   1000 0 -1 CASE2,
   CLOSE, ENTRY, X64EMIT-TEST:CMPSETI-ROUTINE
   s" cmpseti" false WRITE-IMAGE ;

: MOVI-IMAGE ( -- )
   false OPEN,
   1 4294967297 CASE1,
   -4294967296 0 CASE1,
   MAX-CELL MAX-CELL 4294967296 + CASE1,
   CLOSE, ENTRY, X64EMIT-TEST:MOVI-ROUTINE
   s" movi" false WRITE-IMAGE ;

\ The cell and byte loads and stores at one address answer its low byte, zero
\ extended, and leave the cell as it was.
: DADDR-IMAGE ( -- )
   false OPEN,
   $1122334455667788 $88 $1122334455667788 CELL-CASE,
   -1 255 -1 CELL-CASE,
   CLOSE, ENTRY, X64EMIT-TEST:DADDR-ROUTINE
   s" daddressed" false WRITE-IMAGE ;

\ `c = c0; begin t = x + c; t while c = t repeat t`: zero whenever it returns.
\ Inverting the branch returns the first nonzero sum, so the one-turn case
\ tells the two apart.
: LOOP-IMAGE ( -- )
   false OPEN,
   5 -5 0 CASE2,
   1 -5 0 CASE2,
   -2 10 0 CASE2,
   CLOSE, ENTRY, X64EMIT-TEST:LOOP-ROUTINE
   s" loop" false WRITE-IMAGE ;

\ The callee is the squaring fixture, placed first; the call site names its
\ absolute entry, so the answers are the callee's.
: WORDCALL-IMAGE ( -- )
   false OPEN,
   21 42 CASE1,
   -7 -14 CASE1,
   MAX-CELL -2 CASE1,
   CLOSE,
   POSITION {: callee:n :}
   X64EMIT-TEST:SQUARE-ROUTINE
   ALIGN, ENTRY,
   callee X64EMIT-TEST:WORDCALL-ROUTINE
   s" wordcaller" false WRITE-IMAGE ;

\ Twelve doublings summed: `24 a *`, over four frame slots.
: PRESSURE-IMAGE ( -- )
   false OPEN,
   1 24 CASE1,
   -5 -120 CASE1,
   MAX-CELL -24 CASE1,
   CLOSE, ENTRY, X64CHAIN-TEST:PRESSURE-ROUTINE
   s" pressure" false WRITE-IMAGE ;

\ `24 a *` where `b` is zero and `24 a * b +` where it is not: what was put away
\ before the branch comes back on both arms.
: PBRANCH-IMAGE ( -- )
   false OPEN,
   1 0 24 CASE2,
   1 5 29 CASE2,
   -2 0 -48 CASE2,
   MAX-CELL -1 -25 CASE2,
   CLOSE, ENTRY, X64CHAIN-TEST:PBRANCH-ROUTINE
   s" pbranch" false WRITE-IMAGE ;

\ `a 24 a * n * +`: no turn, one, and several.
: PLOOP-IMAGE ( -- )
   false OPEN,
   1 0 1 CASE2,
   5 1 125 CASE2,
   1 3 73 CASE2,
   -2 2 -98 CASE2,
   CLOSE, ENTRY, X64CHAIN-TEST:PLOOP-ROUTINE
   s" ploop" false WRITE-IMAGE ;

\ The callee is the pressure fixture, placed first, so its frame is reserved and
\ given back below the caller's while the caller's is held across the call:
\ `24 a *`, then the callee's `24 *`, then `24 *` again - `13824 a *`.
: PCALLER-IMAGE ( -- )
   false OPEN,
   1 13824 CASE1,
   -1 -13824 CASE1,
   3 41472 CASE1,
   CLOSE,
   POSITION {: callee:n :}
   X64CHAIN-TEST:PRESSURE-ROUTINE
   ALIGN, ENTRY,
   callee X64CHAIN-TEST:PCALLER-ROUTINE
   s" pcaller" false WRITE-IMAGE ;

public
: RUN ( -- )
   T-RESET
   DIR$ TMP-PATH MAKE-DIRS
   MANIFEST 512 BUF:N>BLEN BUF:INIT
   INIT
   false DIFF-IMAGE      true DIFF-IMAGE
   SQUARE-IMAGE
   CHAIN-IMAGE
   IMMS-IMAGE
   SHIFTS-IMAGE
   NOT-IMAGE
   CMPSET-IMAGE
   CMPSETI-IMAGE
   MOVI-IMAGE
   DADDR-IMAGE
   LOOP-IMAGE
   WORDCALL-IMAGE
   PRESSURE-IMAGE
   PBRANCH-IMAGE
   PLOOP-IMAGE
   PCALLER-IMAGE
   SB-RESET DIR$ SB-APPEND s" /manifest" SB-APPEND SB$ TMP-PATH
   MANIFEST BUF:SPAN$ BUF:BLEN>N WRITE-ALL
   X64EMIT-TEST:ADDRESSED-REFUSAL
   DISPOSE
   MANIFEST BUF:DISPOSE
   T-REPORT ;
;package

X64ROUTINES:RUN
