\ x86-64-peer-routines.f - every HIR fixture of test/compiler/x64-emit.f that
\ the x86-64 rows emit, and the pressure fixtures of test/compiler/x64-chain.f
\ whose spills the rows lower into a frame, cross-built into an executable for an
\ x86-64 peer. Each is driven the way src/compiler/native/compiler.f drives a
\ definition - declare, select, prune, fixpoint, emit at the image's own
\ address, retire - and the routine is wrapped in test/x86-64-peer-harness.f's
\ checking entry with the answers its definition must give. Each fixture writes
\ one positive image into $HB_TMP/x64-routines, and each compare fixture one per
\ relation; diff also writes a negative harness image whose first case expects
\ a wrong answer. The manifest lists each file with its expected status;
\ docs/bootstrap.md gives the peer's comparison.
\
\ The same directory takes the signal image, which installs a handler through
\ src/habu/boot-x64.f, raises a real SIGUSR1 and checks the frame the kernel
\ hands the handler and the context the kernel resumes. boot-x64.f loads the
\ x86-64 seam globally, so it comes before the harness, which would otherwise
\ load the seam into its own private wordlist.
\
\ A routine whose bytes depend on where it is written - a call, a tail branch,
\ a function's address - has a second image, `-moved`: the same emission,
\ placed where the first image has it, lands MOVE-N bytes further on past a
\ ud2 pad, and every such field is written again from the emission's rows
\ alone (X64HARNESS:LINK-CALL, LINK-CODE). Both must run the same, which is
\ what says the rows name every site and name it right.
\
\ The divide routines are the three x64-emit.f stages in the machine dialect,
\ which have no source for the rows to select: the quotient, the remainder and
\ a zero divisor. Each cold side calls a callee its image carries in place of
\ `throw`: a stand-in in the zero-divisor image, a ud2 pad no case reaches in
\ the others. Each routine is allocated, accepted and emitted at the image's own
\ address directly (M-ROWS,). The two-counts routine is staged too, but its
\ allocation plans something, so it is declared and goes through the driver's
\ fixpoint and emit rows like a selected routine.
\
\ The select routines are staged and emitted the same way, since nothing selects
\ `x64.cmpsel` or `x64.selz` yet: one per aliasing case, each an IR identity -
\ the result tied to a compared operand, a compared operand moved in, both
\ sources one value - beside a general case of each.
\
\ Four fixtures have no image. The rows refuse BUILD-ADDRESSED with
\ E-IR-VERIFY-OPTYPE: it takes its memory order as an argument, which a
\ data-stack contract has no cell for. The last check, after every image is
\ written, asserts that refusal. BUILD-SELFCALLER is RECURSE with no base case,
\ so it never returns. BUILD-TRAP calls `die`, and BUILD-DIV's cold side
\ `throw`, each at its address in the host engine's dictionary (select-x64.f
\ TRAP-ENTRY, THROW-ENTRY), which is no address of an x86 image; the terminal
\ fixture here renders the same `x64.trap`, and the divide routines the same
\ `x64.idiv`, to a callee the image carries. BUILD-DIV's bytes are pinned by
\ test/compiler/x64-emit.f.
require lib/test.f
require lib/string.f
require lib/fmt.f
require lib/byte-buffer.f
require lib/fs.f
require lib/fs-mutate.f
require src/compiler/native/backend.f
require src/compiler/session/emission.f
require src/arch/x86-64/passes.f
require test/compiler/x64-emit-fixture.f
require test/compiler/x64-chain-fixture.f
require src/habu/boot-x64.f
require test/x86-64-peer-harness.f
require src/habu/arith-abi.f
require lib/ieee754.f

\ The fixtures stay in the package that stages them; this adds each one's trip
\ through the rows to the harness's next address.
package X64EMIT-TEST
private
variable CALLEE                      \ the entry a call site names
TYPED-VARIABLE REL-OP HIR:opcode     \ the relation the compare sites stage
variable MOVED                       \ how far past its placement the routine lands

public
\ The cell the terminal fixture computes and hands its callee.
$0123456789ABCDEF constant TERMINAL-CELL
private

: ROWS-EMIT ( n n NBACK:linkage -- ) {: in:n out:n l:NBACK:linkage :}
   X64CHAIN-TEST:SESSION in out l NBACK:DECLARE
   X64CHAIN-TEST:SESSION BB NBACK:FREEZE {: hm:IR-BUILD:module :}
   X64CHAIN-TEST:SESSION hm NBACK:SELECT {: m0:IR-BUILD:module :}
   hm IR-BUILD:RETIRE
   X64CHAIN-TEST:SESSION m0 NBACK:PRUNE {: m1:IR-BUILD:module :}
   X64CHAIN-TEST:SESSION m1 NBACK:FIXPOINT {: m:IR-BUILD:module :}
   X64CHAIN-TEST:SESSION m X64HARNESS:POSITION MOVED @ - NBACK:EMIT ;

\ A routine that landed where it was placed keeps the fields its emission wrote.
\ One that landed MOVED bytes further on has each of them written again from a
\ row: a call or tail branch at its absolute target, a CODE literal moved.
: LINK-ROWS ( NART:emission n -- ) {: e:NART:emission at:n :}
   e NART:CALL-SITES 0 ?do
      at e i NART:CALL-SITE@ + e i NART:CALL-TARGET@ X64HARNESS:LINK-CALL
   loop
   e NART:ADDR-SITES 0 ?do
      e i NART:ADDR-SITE-KIND@ X64IR:ADDR-CODE = if
         at e i NART:ADDR-SITE@ + MOVED @ X64HARNESS:LINK-CODE
      then
   loop ;

: LINKED ( NART:emission n -- ) {: e:NART:emission at:n :}
   MOVED @ 0<> if e at LINK-ROWS then ;

: ROWS-DONE ( -- )
   0 MOVED ! ;

: ROWS, ( n n NBACK:linkage -- )
   ROWS-EMIT
   X64CHAIN-TEST:SESSION NART:COPY {: e:NART:emission :}
   X64HARNESS:POSITION {: at:n :}
   e NART:BYTES e NART:SIZE X64HARNESS:APPEND-ROUTINE
   e at LINKED
   ROWS-DONE ;

\ The quoting fixture answers the address of its second function, so the label
\ its case expects is bound where the emission laid that function.
: QUOTING-ROWS, ( -- )
   0 1 NBACK:L-NONE ROWS-EMIT
   X64CHAIN-TEST:SESSION NART:COPY {: e:NART:emission :}
   X64HARNESS:POSITION {: at:n :}
   e NART:BYTES e NART:SIZE e 1 NART:FUNCTION-OFFSET@
   X64HARNESS:APPEND-QUOTING
   e at LINKED
   ROWS-DONE ;

\ `: LEAF ( n -- ) TERMINAL-CELL CALLEE ;` where CALLEE ends the process: the
\ terminal call hands over the cell the routine was entered with and a
\ computed one, publishing both where the callee reads its arguments, the shape
\ test/compiler/x64-select.f selects.
: BUILD-TERMINAL ( n -- )
   {: e:n :}
   1 0 OPEN-FUN
   ARG+ {: arg:IR-ID:ir-value-id :}
   MEM0 {: tok:IR-ID:ir-value-id :}
   TERMINAL-CELL CONSTOP {: extra:IR-ID:ir-value-id :}
   HIR-OPCODE:TERMINAL BODY-ST BODY-LN OPEN-OP
   CC BB tok IR-BUILD:ADD-OPERAND
   CC BB arg IR-BUILD:ADD-OPERAND
   CC BB extra IR-BUILD:ADD-OPERAND
   CC BB  CC BB HIR:KEY-ENTRY  CC BB e IR-BUILD:INTERN-INT-ATTR
   IR-BUILD:ADD-ATTR
   CC BB IR-BUILD:END-OP drop
   CLOSE-FUN ;

\ A definition control never comes back from is declared dead, and called
\ because it makes a call: src/arch/x86-64/passes.f ROUTINE composes
\ X64ABI:NORET-FRAMED from the two.
: NORET ( -- NBACK:linkage )
   NBACK:L-DEAD NBACK:L-CALLED NBACK:WITH ;

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
: SHL-BODY ( IR-CTX:ctx -- )      HIR-MOD BUILD-SHL 2 1 NBACK:L-NONE ROWS, ;
: SHR-BODY ( IR-CTX:ctx -- )      HIR-MOD BUILD-SHR 2 1 NBACK:L-NONE ROWS, ;
: SHL-ADD-BODY ( IR-CTX:ctx -- )  HIR-MOD BUILD-SHL-ADD 3 1 NBACK:L-NONE ROWS, ;
: SHL-CROSS-BODY ( IR-CTX:ctx -- ) HIR-MOD BUILD-SHL-CROSS 2 1 NBACK:L-NONE ROWS, ;
: SUM-SHL-BODY ( IR-CTX:ctx -- )
   HIR-MOD HIR-OPCODE:LSHIFT BUILD-SUM-SHIFT 3 1 NBACK:L-NONE ROWS, ;
: SUM-SHR-BODY ( IR-CTX:ctx -- )
   HIR-MOD HIR-OPCODE:RSHIFT BUILD-SUM-SHIFT 3 1 NBACK:L-NONE ROWS, ;
: NOT-BODY ( IR-CTX:ctx -- )      HIR-MOD BUILD-NOT 1 1 NBACK:L-NONE ROWS, ;
: RELATION-BODY ( HIR:opcode IR-CTX:ctx -- )
   {: op:HIR:opcode c:IR-CTX:ctx :}
   op REL-OP ! c HIR-MOD REL-OP @ BUILD-RELATION 2 1 NBACK:L-NONE ROWS, ;
: RELATIONI-BODY ( HIR:opcode IR-CTX:ctx -- )
   {: op:HIR:opcode c:IR-CTX:ctx :}
   op REL-OP ! c HIR-MOD REL-OP @ BUILD-RELATIONI 2 1 NBACK:L-NONE ROWS, ;
: MOVI-BODY ( IR-CTX:ctx -- )     HIR-MOD BUILD-MOVI 1 1 NBACK:L-NONE ROWS, ;
: DADDR-BODY ( IR-CTX:ctx -- )
   HIR-MOD BUILD-DADDRESSED 1 1 NBACK:L-NONE ROWS, ;
: LOOP-BODY ( IR-CTX:ctx -- )     HIR-MOD BUILD-LOOP 2 1 NBACK:L-NONE ROWS, ;
: WORDCALL-BODY ( n n IR-CTX:ctx -- )
   {: callee:n moved:n c:IR-CTX:ctx :}
   moved MOVED ! callee CALLEE !
   c HIR-MOD CALLEE @ BUILD-WORDCALLER 1 1 NBACK:L-CALLED ROWS, ;
\ The same site in tail position: the routine leaves through its callee, whose
\ return comes back to the case that called the routine.
: TAILCALL-BODY ( n n IR-CTX:ctx -- )
   {: callee:n moved:n c:IR-CTX:ctx :}
   moved MOVED ! callee CALLEE !
   c HIR-MOD CALLEE @ BUILD-WORDCALLER 1 1 NBACK:L-TAIL ROWS, ;
: TERMINAL-BODY ( n n IR-CTX:ctx -- )
   {: callee:n moved:n c:IR-CTX:ctx :}
   moved MOVED ! callee CALLEE !
   c HIR-MOD CALLEE @ BUILD-TERMINAL 1 0 NORET ROWS, ;
: QUOTER-BODY ( n IR-CTX:ctx -- )
   {: moved:n c:IR-CTX:ctx :}
   moved MOVED ! c HIR-MOD BUILD-QUOTER QUOTING-ROWS, ;
: FBIN-BODY ( HIR:opcode IR-CTX:ctx -- )
   {: op:HIR:opcode c:IR-CTX:ctx :}
   op F-OP ! c HIR-MOD BUILD-FBIN 2 1 NBACK:L-NONE ROWS, ;
: FKEEP-BODY ( IR-CTX:ctx -- )    HIR-MOD BUILD-FKEEP 2 1 NBACK:L-NONE ROWS, ;
: FUN1-BODY ( HIR:opcode IR-CTX:ctx -- )
   {: op:HIR:opcode c:IR-CTX:ctx :}
   op F-OP ! c HIR-MOD BUILD-FUN1 1 1 NBACK:L-NONE ROWS, ;
: FCMP-BODY ( HIR:opcode IR-CTX:ctx -- )
   {: op:HIR:opcode c:IR-CTX:ctx :}
   op F-OP ! c HIR-MOD BUILD-FCMP 2 1 NBACK:L-NONE ROWS, ;
: FCMP0-BODY ( HIR:opcode IR-CTX:ctx -- )
   {: op:HIR:opcode c:IR-CTX:ctx :}
   op F-OP ! c HIR-MOD BUILD-FCMP0 1 1 NBACK:L-NONE ROWS, ;
: INTREAL-BODY ( IR-CTX:ctx -- )  HIR-MOD BUILD-INTREAL 1 1 NBACK:L-NONE ROWS, ;
: REALINT-BODY ( IR-CTX:ctx -- )  HIR-MOD BUILD-REALINT 1 1 NBACK:L-NONE ROWS, ;
: BITS-BODY ( IR-CTX:ctx -- )     HIR-MOD BUILD-BITS 1 1 NBACK:L-NONE ROWS, ;
: FCONST-BODY ( IR-CTX:ctx -- )   HIR-MOD BUILD-FCONST 1 1 NBACK:L-NONE ROWS, ;
: FCALL-BODY ( n IR-CTX:ctx -- )
   {: callee:n c:IR-CTX:ctx :}
   callee CALLEE ! c HIR-MOD CALLEE @ BUILD-FCALL 2 1 NBACK:L-CALLED ROWS, ;

\ A routine staged in the dialect has no source operation for the rows to
\ select, so it is allocated, accepted and emitted the way the emitter's own
\ cases are, at the image's next address.
: M-ROWS, ( IR-BUILD:module n n -- )
   M-DALLOCATED X64HARNESS:POSITION PLACED
   X64EMIT:BYTES X64EMIT:SIZE X64HARNESS:APPEND-ROUTINE
   X64EMIT:RETIRE ;

\ Each cold side calls the callee its image carries in place of `throw`.
: QUOTIENT-BODY ( n IR-CTX:ctx -- )
   {: callee:n c:IR-CTX:ctx :}
   callee CALLEE ! c 0 W-CTX ! CALLEE @ BUILD-QUOTIENT 2 1 M-ROWS, ;
: REMAINDER-BODY ( n IR-CTX:ctx -- )
   {: callee:n c:IR-CTX:ctx :}
   callee CALLEE ! c 0 W-CTX ! CALLEE @ BUILD-REMAINDER 3 1 M-ROWS, ;
: DIVZERO-BODY ( n IR-CTX:ctx -- )
   {: callee:n c:IR-CTX:ctx :}
   callee CALLEE ! c 0 W-CTX ! CALLEE @ BUILD-DIVZERO 0 1 M-ROWS, ;

: M-SHIFT ( X64IR:opcode IR-ID:ir-value-id IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: o:X64IR:opcode x:IR-ID:ir-value-id n:IR-ID:ir-value-id :}
   o M-OPEN
   x M-OPERAND+
   n M-OPERAND+
   M-RESULT+
   M-CLOSE-VALUE ;

\ `( v a b -- n )` answering `v a lshift b rshift`, both counts loaded before
\ either shift: test/compiler/x64-regalloc.f TWO-COUNTS behind the data-stack
\ boundary, its second shift a right one so that counts given in the wrong order
\ answer wrongly. Each count is fixed to rcx at its shift and b is live where
\ the first shift reads a there, so neither keeps rcx from its load to its shift
\ (regalloc.f MB-PIN), and the fixpoint lowers each into a count of its own in
\ front of its shift. The spill pass is bound to the builder the routine is
\ staged in, as selection binds it to the one it writes.
: BUILD-TWO-COUNTS ( -- IR-BUILD:module )
   M-MOD
   M-BIND-MACHINE
   CC MB  CC MB X64IR:LOWERING  [: X64IR:ENSURE-NAMED ;] A64SPILL:BIND-DIALECT
   TERNARY-SIGN M-FUN
   24 M-DTAKE {: t0:IR-ID:ir-value-id :}
   t0 0 M-DLOAD {: v:IR-ID:ir-value-id t1:IR-ID:ir-value-id :}
   t1 8 M-DLOAD {: a:IR-ID:ir-value-id t2:IR-ID:ir-value-id :}
   t2 16 M-DLOAD {: b:IR-ID:ir-value-id t3:IR-ID:ir-value-id :}
   X64IR-OPCODE:SHL v a M-SHIFT {: s:IR-ID:ir-value-id :}
   X64IR-OPCODE:SHR s b M-SHIFT {: x:IR-ID:ir-value-id :}
   x t3 0 M-DSTORE {: t4:IR-ID:ir-value-id :}
   t4 8 M-DPUBLISH
   M-RET0
   M-CLOSE ;

\ A routine staged in the dialect whose allocation plans something takes the
\ driver's rows from the fixpoint on: declared, lowered until its plan is empty,
\ and emitted at the image's next address.
: TWO-COUNTS-BODY ( IR-CTX:ctx -- )
   0 W-CTX !
   X64CHAIN-TEST:SESSION 3 1 NBACK:L-NONE NBACK:DECLARE
   X64CHAIN-TEST:SESSION BUILD-TWO-COUNTS NBACK:FIXPOINT {: m:IR-BUILD:module :}
   X64HARNESS:POSITION {: at:n :}
   X64CHAIN-TEST:SESSION m at NBACK:EMIT
   X64CHAIN-TEST:SESSION NART:COPY {: e:NART:emission :}
   e NART:BYTES e NART:SIZE X64HARNESS:APPEND-ROUTINE
   ROWS-DONE ;

\ A select shape of the fixture's, taking `in` cells.
TYPED-VARIABLE SEL-SHAPE [ -- IR-BUILD:module ]
variable SEL-IN

: SEL-BODY ( [ -- IR-BUILD:module ] n IR-CTX:ctx -- )
   {: shape in:n c:IR-CTX:ctx :}
   shape SEL-SHAPE ! in SEL-IN !
   c 0 W-CTX ! SEL-SHAPE @ execute SEL-IN @ 1 M-ROWS, ;

: SEL-ROUTINE ( [ -- IR-BUILD:module ] n -- )
   [: SEL-BODY ;] X64CHAIN-TEST:WITH-CASE ;

\ A refused definition ends the way src/compiler/native/compiler.f ends one:
\ what the refusal left bound is released, then the emission retired.
: ADDRESSED-REFUSED ( IR-CTX:ctx -- )
   HIR-MOD BUILD-ADDRESSED
   s" the rows refuse the addressed fixture: a data-stack contract has no cell for the memory order it takes as an argument" T-LABEL
   [: 2 1 NBACK:L-NONE ROWS, ;] E-IR-VERIFY-OPTYPE TTHROWSQ ;

public
: DIFF-ROUTINE ( -- )     [: DIFF-BODY ;] X64CHAIN-TEST:WITH-CASE ;
: SQUARE-ROUTINE ( -- )   [: SQUARE-BODY ;] X64CHAIN-TEST:WITH-CASE ;
: CHAIN-ROUTINE ( -- )    [: CHAIN-BODY ;] X64CHAIN-TEST:WITH-CASE ;
: IMMS-ROUTINE ( -- )     [: IMMS-BODY ;] X64CHAIN-TEST:WITH-CASE ;
: SHIFTS-ROUTINE ( -- )   [: SHIFTS-BODY ;] X64CHAIN-TEST:WITH-CASE ;
: SHL-ROUTINE ( -- )      [: SHL-BODY ;] X64CHAIN-TEST:WITH-CASE ;
: SHR-ROUTINE ( -- )      [: SHR-BODY ;] X64CHAIN-TEST:WITH-CASE ;
: SHL-ADD-ROUTINE ( -- )  [: SHL-ADD-BODY ;] X64CHAIN-TEST:WITH-CASE ;
: SHL-CROSS-ROUTINE ( -- ) [: SHL-CROSS-BODY ;] X64CHAIN-TEST:WITH-CASE ;
: SUM-SHL-ROUTINE ( -- )  [: SUM-SHL-BODY ;] X64CHAIN-TEST:WITH-CASE ;
: SUM-SHR-ROUTINE ( -- )  [: SUM-SHR-BODY ;] X64CHAIN-TEST:WITH-CASE ;
: NOT-ROUTINE ( -- )      [: NOT-BODY ;] X64CHAIN-TEST:WITH-CASE ;
: RELATION-ROUTINE ( HIR:opcode -- )
   [: RELATION-BODY ;] X64CHAIN-TEST:WITH-CASE ;
: RELATIONI-ROUTINE ( HIR:opcode -- )
   [: RELATIONI-BODY ;] X64CHAIN-TEST:WITH-CASE ;
: MOVI-ROUTINE ( -- )     [: MOVI-BODY ;] X64CHAIN-TEST:WITH-CASE ;
: DADDR-ROUTINE ( -- )    [: DADDR-BODY ;] X64CHAIN-TEST:WITH-CASE ;
: LOOP-ROUTINE ( -- )     [: LOOP-BODY ;] X64CHAIN-TEST:WITH-CASE ;
\ These take how far past its placement the routine lands as well.
: WORDCALL-ROUTINE ( n n -- )
   [: WORDCALL-BODY ;] X64CHAIN-TEST:WITH-CASE ;
: TAILCALL-ROUTINE ( n n -- )
   [: TAILCALL-BODY ;] X64CHAIN-TEST:WITH-CASE ;
: TERMINAL-ROUTINE ( n n -- )
   [: TERMINAL-BODY ;] X64CHAIN-TEST:WITH-CASE ;
: QUOTER-ROUTINE ( n -- )
   [: QUOTER-BODY ;] X64CHAIN-TEST:WITH-CASE ;

: QUOTIENT-ROUTINE ( n -- )
   [: QUOTIENT-BODY ;] X64CHAIN-TEST:WITH-CASE ;
: REMAINDER-ROUTINE ( n -- )
   [: REMAINDER-BODY ;] X64CHAIN-TEST:WITH-CASE ;
: DIVZERO-ROUTINE ( n -- )
   [: DIVZERO-BODY ;] X64CHAIN-TEST:WITH-CASE ;
: TWO-COUNTS-ROUTINE ( -- )
   [: TWO-COUNTS-BODY ;] X64CHAIN-TEST:WITH-CASE ;

: CMPSEL-ROUTINE ( -- )      [: BUILD-CMPSEL ;] 3 SEL-ROUTINE ;
: CMPSEL-FA-ROUTINE ( -- )   [: BUILD-CMPSEL-FA ;] 3 SEL-ROUTINE ;
: CMPSEL-HB-ROUTINE ( -- )   [: BUILD-CMPSEL-HB ;] 3 SEL-ROUTINE ;
: CMPSEL-HA-ROUTINE ( -- )   [: BUILD-CMPSEL-HA ;] 3 SEL-ROUTINE ;
: CMPSEL-SAME-ROUTINE ( -- ) [: BUILD-CMPSEL-SAME ;] 3 SEL-ROUTINE ;
: SELZ-ROUTINE ( -- )        [: BUILD-SELZ ;] 3 SEL-ROUTINE ;
: SELZ-NV-ROUTINE ( -- )     [: BUILD-SELZ-NV ;] 2 SEL-ROUTINE ;
: SELZ-ZV-ROUTINE ( -- )     [: BUILD-SELZ-ZV ;] 2 SEL-ROUTINE ;
: SELZ-SAME-ROUTINE ( -- )   [: BUILD-SELZ-SAME ;] 2 SEL-ROUTINE ;

: ADDRESSED-REFUSAL ( -- ) [: ADDRESSED-REFUSED ;] X64CHAIN-TEST:WITH-CASE ;

\ The doubles: the source operation, where a fixture stages several, first.
: FBIN-ROUTINE ( HIR:opcode -- )
   [: FBIN-BODY ;] X64CHAIN-TEST:WITH-CASE ;
: FKEEP-ROUTINE ( -- )    [: FKEEP-BODY ;] X64CHAIN-TEST:WITH-CASE ;
: FUN1-ROUTINE ( HIR:opcode -- )
   [: FUN1-BODY ;] X64CHAIN-TEST:WITH-CASE ;
: FCMP-ROUTINE ( HIR:opcode -- )
   [: FCMP-BODY ;] X64CHAIN-TEST:WITH-CASE ;
: FCMP0-ROUTINE ( HIR:opcode -- )
   [: FCMP0-BODY ;] X64CHAIN-TEST:WITH-CASE ;
: INTREAL-ROUTINE ( -- )  [: INTREAL-BODY ;] X64CHAIN-TEST:WITH-CASE ;
: REALINT-ROUTINE ( -- )  [: REALINT-BODY ;] X64CHAIN-TEST:WITH-CASE ;
: BITS-ROUTINE ( -- )     [: BITS-BODY ;] X64CHAIN-TEST:WITH-CASE ;
: FCONST-ROUTINE ( -- )   [: FCONST-BODY ;] X64CHAIN-TEST:WITH-CASE ;
: FCALL-ROUTINE ( n -- )
   [: FCALL-BODY ;] X64CHAIN-TEST:WITH-CASE ;
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
   NS m X64HARNESS:POSITION NBACK:EMIT
   NS NART:COPY {: e:NART:emission :}
   e NART:BYTES e NART:SIZE X64HARNESS:APPEND-ROUTINE ;

: PRESS-ROWS ( IR-CTX:ctx -- )    HIR-MOD BUILD-PRESSURE 1 1 NBACK:L-NONE ROWS, ;
: PBRANCH-ROWS ( IR-CTX:ctx -- )  HIR-MOD BUILD-PBRANCH 2 1 NBACK:L-NONE ROWS, ;
: PLOOP-ROWS ( IR-CTX:ctx -- )    HIR-MOD BUILD-PLOOP 2 1 NBACK:L-NONE ROWS, ;
: PCALLER-ROWS ( n IR-CTX:ctx -- )
   {: callee:n c:IR-CTX:ctx :}
   callee CALLEE !
   c HIR-MOD CALLEE @ BUILD-PCALLER 1 1 NBACK:L-CALLED ROWS, ;

\ `( a b c d e f g h i j -- n )` reading the ten in base two, `a 2* b + 2* c +
\ ... 2* j +`, so a value lost or two exchanged answer wrongly. The entry loads
\ every argument in one run (select-x64.f OPEN-DARGS): ten loads for nine
\ registers, so the allocator divides the run, storing a value to the frame right
\ after its own load (regalloc.f MB-DIVIDE).
10 constant ARGS-N

: BUILD-ARGS ( -- )
   ARGS-N 1 OPEN-FUN
   ARGS-N 0 ?do ARG+ i ARGV ! loop
   0 ARGV @
   ARGS-N 1 ?do
      HIR-OPCODE:ADD swap dup BINOP
      HIR-OPCODE:ADD swap i ARGV @ BINOP
   loop
   RET1
   CLOSE-FUN ;

: ARGS-ROWS ( IR-CTX:ctx -- )     HIR-MOD BUILD-ARGS ARGS-N 1 NBACK:L-NONE ROWS, ;

public
: ARGS-ROUTINE ( -- )      [: ARGS-ROWS ;] X64CHAIN-TEST:WITH-CASE ;
: PRESSURE-ROUTINE ( -- )  [: PRESS-ROWS ;] X64CHAIN-TEST:WITH-CASE ;
: PBRANCH-ROUTINE ( -- )   [: PBRANCH-ROWS ;] X64CHAIN-TEST:WITH-CASE ;
: PLOOP-ROUTINE ( -- )     [: PLOOP-ROWS ;] X64CHAIN-TEST:WITH-CASE ;
: PCALLER-ROUTINE ( n -- )
   [: PCALLER-ROWS ;] X64CHAIN-TEST:WITH-CASE ;
;package

\ The signal case, staged in the harness's package because its checks are the
\ harness's own. The handler reads what the kernel saved of the interrupted
\ context and then writes a new one, which the kernel resumes: UC-RIP and every
\ UC-GREG slot but rdi's and r11's are pinned by the handler's reads, and UC-RIP
\ and every slot but rsp's by the resume.
package X64HARNESS
using X64ASM
using X64CODE
using X64BOOT
private

10 constant SIGUSR1

\ The cell the case leaves at the top of the machine stack for the raise: the
\ saved rsp must point at it.
$FEEDFACECAFEBEEF constant STACK-MARK

\ How far below the interrupted rsp the kernel builds a handler's frame at
\ most: the red zone, the siginfo, the ucontext and the extended register
\ state, a few KiB with AVX-512 and more with AMX.
$10000 constant FRAME-REACH

: SAME? ( r64 r64 -- bool ) R64>N swap R64>N = ;

\ The raise decides these registers: rax holds kill's number and then its
\ result, rdi and rsi its arguments, the syscall writes rcx and r11, and rsp is
\ the stack. The case loads every other register with its PATTERN.
: RAISED? ( r64 -- bool ) {: r:r64 :}
   r RAX SAME?  r RCX SAME? or  r RSP SAME? or
   r RSI SAME? or  r RDI SAME? or  r R11 SAME? or ;

\ The register's number in the low four bits of every byte and $A in the high
\ four: no two registers share a value, and the top byte makes it no address.
: PATTERN ( r64 -- n ) R64>N $0101010101010101 * $A0A0A0A0A0A0A0A0 or ;

\ What the handler writes into a register's slot: an imm32 the case compares
\ the resumed register with directly.
: FRESH ( r64 -- n ) R64>N $10101 * $5A000000 + ;

: LOAD-PATTERN, ( n -- ) >R64 {: r:r64 :}
   r RAISED? 0= if r r PATTERN IMM then ;

\ rax = the register's slot in the ucontext rdx points at.
: SAVED, ( r64 -- ) {: r:r64 :}
   RAX RDX r UC-GREG MEM-OFF ASM-SINK ENC-MOV-RM ;

: SAVED=, ( r64 n n -- ) {: r:r64 want:n s:n :}
   r SAVED,  RCX want IMM  RAX RCX ASM-SINK ENC-CMP-RR  s ASSERT-EQ ;

: PATTERN=, ( n n -- ) {: ix:n s:n :}
   ix >R64 {: r:r64 :}
   r RAISED? 0= if r r PATTERN s SAVED=, then ;

\ The saved rsp lies above the handler's frame, within FRAME-REACH, and points
\ at the mark.
: SAVED-RSP, ( n -- ) {: s:n :}
   RSP SAVED,
   RCX RAX ASM-SINK ENC-MOV-RR  RCX RSP ASM-SINK ENC-SUB-RR
   RCX FRAME-REACH >IMM32 ASM-SINK ENC-CMP-RI32  C-AE s FAIL-IF
   RAX RAX MEM-AT ASM-SINK ENC-MOV-RM
   RCX STACK-MARK IMM  RAX RCX ASM-SINK ENC-CMP-RR  s ASSERT-EQ ;

: WRITE-FRESH, ( n -- ) >R64 {: r:r64 :}
   r RSP SAME? 0= if
      RAX r FRESH IMM  RAX RDX r UC-GREG MEM-OFF ASM-SINK ENC-MOV-MR
   then ;

: FRESH=, ( r64 n -- ) {: r:r64 s:n :}
   r r FRESH >IMM32 ASM-SINK ENC-CMP-RI32  s ASSERT-EQ ;

: OTHER-FRESH=, ( n n -- ) {: ix:n s:n :}
   ix >R64 {: r:r64 :}
   r RSP SAME? r RDI SAME? or 0= if r s FRESH=, then ;

\ Every register but rsp holds what the handler wrote. A failure loads rdi with
\ its status first, so rdi is checked before the others.
: RESUMED, ( n -- ) {: s:n :}
   RDI s FRESH=,
   16 0 ?do i s OTHER-FRESH=, loop ;

\ Entered with the signal number in rdi, the siginfo in rsi and the ucontext in
\ rdx: check the three, then write the context to resume, the fresh registers
\ at `resumed`.
: HANDLER, ( label label -- ) {: raised:label resumed:label :}
   RAX RDI ASM-SINK ENC-MOV-RR  SIGUSR1 EXPECT,
   0 >R32 RSI MEM-AT ASM-SINK ENC-MOV32-RM  SIGUSR1 EXPECT,
   RAX RDX UC-RIP MEM-OFF ASM-SINK ENC-MOV-RM  RCX raised MOVABS,  EXPECT-RCX,
   STATUS SAVED-RSP,
   STATUS {: s:n :}
   16 0 ?do i s PATTERN=, loop
   RAX 0 s SAVED=,
   RSI SIGUSR1 s SAVED=,
   RCX SAVED,  RCX raised MOVABS,  RAX RCX ASM-SINK ENC-CMP-RR  s ASSERT-EQ
   16 0 ?do i WRITE-FRESH, loop
   RAX resumed MOVABS,  RAX RDX UC-RIP MEM-OFF ASM-SINK ENC-MOV-MR
   ASM-SINK ENC-RET ;

public

\ Install the handler for SIGUSR1, check the install gave rsp back and kept the
\ harness's reserved registers, and that one for SIGKILL is refused with the
\ carry set. Leave the mark on the machine stack, pattern every register the
\ raise leaves free, raise the signal with kill on the image's own pid and
\ check the resumed context. Control coming back to the
\ instruction after the syscall fails: the handler moved the resume to
\ `resumed`. It leaves rdi 0 for the exit.
: SIGNAL-CASE, ( -- )
   LBL LBL LBL LBL LBL
   {: handler:label rest:label past:label raised:label resumed:label :}
   SIGUSR1 SA-SIGINFO handler rest SIGACTION,  C-B STATUS FAIL-IF
   STATUS {: kept:n :}                          \ the installer's own contract
   RSP RBP ASM-SINK ENC-CMP-RR  kept ASSERT-EQ
   RBX $22334455 kept RESERVED,  R13 $33445566 kept RESERVED,
   R14 $44556677 kept RESERVED,  R15 $55667788 kept RESERVED,
   9 SA-SIGINFO handler rest SIGACTION,  C-AE STATUS FAIL-IF   \ SIGKILL: refused
   past JMP,
   handler LBL,  raised resumed HANDLER,
   rest RESTORER,
   past LBL,
   RSP CELL 2 * >IMM8 ASM-SINK ENC-SUB-RI8
   RAX STACK-MARK IMM  RAX RSP MEM-AT ASM-SINK ENC-MOV-MR
   NR-GETPID SYS,  RDI RAX ASM-SINK ENC-MOV-RR
   16 0 ?do i LOAD-PATTERN, loop
   RSI SIGUSR1 IMM
   0 >R32 NR-KILL >IMM32 ASM-SINK ENC-MOV32-RI32  ASM-SINK ENC-SYSCALL
   raised LBL,
   RDI STATUS IMM  EXIT-LBL JMP,
   resumed LBL,
   STATUS RESUMED,
   RDI ZERO-REG, ;

\ A callee `( n -- n )` that answers its cell doubled and writes every XMM
\ register on the way, which a call is allowed to: every contract of this
\ machine destroys the whole file (src/arch/x86-64/abi.f). A double the caller
\ kept in a register across the call reads this pattern back, a signalling NaN
\ no case computes.
$7FF4DEADBEEF0000 constant XMM-JUNK

: XMM-CLOBBER, ( -- )
   RAX XMM-JUNK IMM
   16 0 ?do  i >XMM RAX ASM-SINK ENC-MOVQ-XR  loop
   RAX R12 -8 MEM-OFF ASM-SINK ENC-MOV-RM
   RAX RAX ASM-SINK ENC-ADD-RR
   RAX R12 -8 MEM-OFF ASM-SINK ENC-MOV-MR
   ASM-SINK ENC-RET ;

;using
;using
;using
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

\ `over swap lshift xor`, the count in cl: `shl r64, cl` takes the count modulo
\ 64 as Habu's `lshift` does, so a count of 64 shifts by nothing and the answer
\ is zero where a count honoured whole would leave a alone.
: SHL-IMAGE ( -- )
   false OPEN,
   3 0 0 CASE2,
   3 1 5 CASE2,
   3 63 MIN-CELL 3 + CASE2,
   3 64 0 CASE2,
   CLOSE, ENTRY, X64EMIT-TEST:SHL-ROUTINE
   s" shl" false WRITE-IMAGE ;

\ `over swap rshift xor`: the right shift is logical, so -4 shifted by one is
\ MAX-CELL less one where an arithmetic shift would answer -2.
: SHR-IMAGE ( -- )
   false OPEN,
   -4 0 0 CASE2,
   -4 1 MIN-CELL 2 + CASE2,
   -4 63 -3 CASE2,
   -4 64 0 CASE2,
   CLOSE, ENTRY, X64EMIT-TEST:SHR-ROUTINE
   s" shr" false WRITE-IMAGE ;

\ `lshift +`, the value shifted the tied destination: a + b shifted by n, with b
\ kept out of rcx where the count's copy is. A b in cl would be shifted by
\ itself.
: SHL-ADD-IMAGE ( -- )
   false OPEN,
   5 3 0 8 CASE3,
   5 3 1 11 CASE3,
   5 3 63 MIN-CELL 5 + CASE3,
   5 3 64 8 CASE3,
   CLOSE, ENTRY, X64EMIT-TEST:SHL-ADD-ROUTINE
   s" shl-add" false WRITE-IMAGE ;

\ `2dup lshift rot xor +`: n + ((a shifted by n) xor a), with a and n both live
\ across the shift and so both out of rcx.
: SHL-CROSS-IMAGE ( -- )
   false OPEN,
   3 0 0 CASE2,
   3 1 6 CASE2,
   3 63 MIN-CELL 66 + CASE2,
   3 64 64 CASE2,
   CLOSE, ENTRY, X64EMIT-TEST:SHL-CROSS-ROUTINE
   s" shl-cross" false WRITE-IMAGE ;

\ `-rot + swap lshift`: a + b shifted by n, with the count loaded straight into
\ rcx and b, dead at the add, kept out of it. A sum shifted by a or by b answers
\ 256 or 64 for the second case.
: SUM-SHL-IMAGE ( -- )
   false OPEN,
   5 3 0 8 CASE3,
   5 3 1 16 CASE3,
   -1 2 63 MIN-CELL CASE3,
   5 3 64 8 CASE3,
   CLOSE, ENTRY, X64EMIT-TEST:SUM-SHL-ROUTINE
   s" sum-shl" false WRITE-IMAGE ;

\ `-rot + swap rshift`: the same shape shifted right, which is logical, so -4
\ shifted by one is MAX-CELL less one and -2 by 63 is one.
: SUM-SHR-IMAGE ( -- )
   false OPEN,
   5 3 0 8 CASE3,
   5 3 1 4 CASE3,
   -4 0 1 MAX-CELL 1 - CASE3,
   -1 -1 63 1 CASE3,
   5 3 64 8 CASE3,
   CLOSE, ENTRY, X64EMIT-TEST:SUM-SHR-ROUTINE
   s" sum-shr" false WRITE-IMAGE ;

: NOT-IMAGE ( -- )
   false OPEN,
   0 -1 CASE1,
   5 -6 CASE1,
   MIN-CELL MAX-CELL CASE1,
   CLOSE, ENTRY, X64EMIT-TEST:NOT-ROUTINE
   s" not" false WRITE-IMAGE ;

\ Every relation the selector compares with, in both forms, expecting the flag
\ Habu's own word for it answers. Each form is staged below the boundary, at it
\ and above it - above by 2^32, which a 32-bit compare would call level - and at
\ the ends of the signed order, MIN-CELL and MAX-CELL. Where the difference a
\ compare takes overflows - MIN-CELL against MAX-CELL either way round, and
\ MIN-CELL less 1000 - an ordering condition that reads the sign alone or the
\ unsigned order answers wrong. A sixth case would run the entry past the
\ harness's ROUTINE-OFF.
TYPED-VARIABLE ANSWER-KEY [ n n -- bool ]

\ The flag the relation answers for `a b`: all ones for true.
: ANSWER ( n n -- n ) ANSWER-KEY @ execute if -1 else 0 then ;

: REL-CASE, ( n n -- ) {: a:n b:n :}  a b  a b ANSWER CASE2, ;

\ `b a - 1000 rel`.
: RELI-CASE, ( n n -- ) {: a:n b:n :}  a b  b a - 1000 ANSWER CASE2, ;

: REL-IMAGE ( HIR:opcode ptr u8 n -- )
   {: o:HIR:opcode name:ptr u:n :}
   false OPEN,
   2 5 REL-CASE,
   5 5 REL-CASE,
   4294967296 0 REL-CASE,
   MIN-CELL MAX-CELL REL-CASE,
   MAX-CELL MIN-CELL REL-CASE,
   CLOSE, ENTRY, o X64EMIT-TEST:RELATION-ROUTINE
   name u false WRITE-IMAGE ;

: RELI-IMAGE ( HIR:opcode ptr u8 n -- )
   {: o:HIR:opcode name:ptr u:n :}
   false OPEN,
   10 500 RELI-CASE,
   24 1024 RELI-CASE,
   0 4294968296 RELI-CASE,
   0 MIN-CELL RELI-CASE,
   0 MAX-CELL RELI-CASE,
   CLOSE, ENTRY, o X64EMIT-TEST:RELATIONI-ROUTINE
   name u false WRITE-IMAGE ;

\ One relation's two images: the register form and the folded-immediate form.
: REL-IMAGES ( ptr u8 n ptr u8 n HIR:opcode [ n n -- bool ] -- )
   ANSWER-KEY !
   {: reg:ptr regu:n imm:ptr immu:n o:HIR:opcode :}
   o reg regu REL-IMAGE
   o imm immu RELI-IMAGE ;

: RELATION-IMAGES ( -- )
   s" cmpset-lt" s" cmpseti-lt" HIR-OPCODE:LT    [: < ;]  REL-IMAGES
   s" cmpset-le" s" cmpseti-le" HIR-OPCODE:LE    [: <= ;] REL-IMAGES
   s" cmpset-gt" s" cmpseti-gt" HIR-OPCODE:GT    [: > ;]  REL-IMAGES
   s" cmpset-ge" s" cmpseti-ge" HIR-OPCODE:GE    [: >= ;] REL-IMAGES
   s" cmpset-eq" s" cmpseti-eq" HIR-OPCODE:EQUAL [: = ;]  REL-IMAGES
   s" cmpset-ne" s" cmpseti-ne" HIR-OPCODE:NE    [: <> ;] REL-IMAGES ;

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

\ How far a `-moved` image puts its routine past the first image's: a whole
\ number of slots, and longer than any callee staged before one.
256 constant MOVE-N

\ The callee is the squaring fixture, placed first; the call site names its
\ absolute entry, so the answers are the callee's.
: WORDCALL-IMAGE ( n ptr u8 n -- ) {: moved:n name:ptr u:n :}
   false OPEN,
   21 42 CASE1,
   -7 -14 CASE1,
   MAX-CELL -2 CASE1,
   CLOSE,
   POSITION {: callee:n :}
   X64EMIT-TEST:SQUARE-ROUTINE
   ALIGN, moved SKIP, ENTRY,
   callee moved X64EMIT-TEST:WORDCALL-ROUTINE
   name u false WRITE-IMAGE ;

\ The same answers through a tail branch: the callee returns to the case.
: TAILCALL-IMAGE ( n ptr u8 n -- ) {: moved:n name:ptr u:n :}
   false OPEN,
   21 42 CASE1,
   -7 -14 CASE1,
   MAX-CELL -2 CASE1,
   CLOSE,
   POSITION {: callee:n :}
   X64EMIT-TEST:SQUARE-ROUTINE
   ALIGN, moved SKIP, ENTRY,
   callee moved X64EMIT-TEST:TAILCALL-ROUTINE
   name u false WRITE-IMAGE ;

\ The routine never comes back: its terminal call leaves through a stand-in for
\ the callee, placed first, which checks the argument and the computed cell the
\ routine published below the data-stack pointer and exits 0.
: TERMINAL-IMAGE ( n ptr u8 n -- ) {: moved:n name:ptr u:n :}
   false OPEN,
   MIN-CELL TERMINAL-CASE,
   CLOSE,
   POSITION {: callee:n :}
   MIN-CELL X64EMIT-TEST:TERMINAL-CELL STAND-IN,
   ALIGN, moved SKIP, ENTRY,
   callee moved X64EMIT-TEST:TERMINAL-ROUTINE
   name u false WRITE-IMAGE ;

\ The answer is the address the emission laid the second function at, which is
\ where the harness bound the label the case compares it with.
: QUOTER-IMAGE ( n ptr u8 n -- ) {: moved:n name:ptr u:n :}
   false OPEN,
   QUOTE-CASE,
   CLOSE, moved SKIP, ENTRY, moved X64EMIT-TEST:QUOTER-ROUTINE
   name u false WRITE-IMAGE ;

\ Each routine with a site of that kind, where the rows placed it and moved.
: SITE-IMAGES ( -- )
   0 s" wordcaller" WORDCALL-IMAGE
   MOVE-N s" wordcaller-moved" WORDCALL-IMAGE
   0 s" tailcaller" TAILCALL-IMAGE
   MOVE-N s" tailcaller-moved" TAILCALL-IMAGE
   0 s" terminal" TERMINAL-IMAGE
   MOVE-N s" terminal-moved" TERMINAL-IMAGE
   0 s" quoter" QUOTER-IMAGE
   MOVE-N s" quoter-moved" QUOTER-IMAGE ;

\ Twelve doublings summed: `24 a *`, over four frame slots.
: PRESSURE-IMAGE ( -- )
   false OPEN,
   1 24 CASE1,
   -5 -120 CASE1,
   MAX-CELL -24 CASE1,
   CLOSE, ENTRY, X64CHAIN-TEST:PRESSURE-ROUTINE
   s" pressure" false WRITE-IMAGE ;

\ Ten arguments read in base two, one more than the registers the entry's run of
\ loads can hold. A third case of ten staged cells would not fit before the
\ harness's ROUTINE-OFF.
: ARGS-IMAGE ( -- )
   false OPEN,
   1 2 3 4 5 6 7 8 9 10 2036 CASE10,
   10 9 8 7 6 5 4 3 2 1 9217 CASE10,
   CLOSE, ENTRY, X64CHAIN-TEST:ARGS-ROUTINE
   s" args" false WRITE-IMAGE ;

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

\ A real SIGUSR1 through the handler boot-x64.f installs, with no routine of the
\ rows: the case ends the image itself.
: SIGNAL-IMAGE ( -- )
   false OPEN,
   SIGNAL-CASE,
   s" signal" false WRITE-IMAGE ;

\ The callee of a divide whose image never divides by zero, in place of
\ `throw`: a ud2 pad the image carries, placed first, so a cold side that ran
\ would stop on SIGILL instead of exiting with the manifest's status.
: COLD-PAD, ( -- n )
   POSITION  X64IR:SP-ALIGN SKIP, ;

\ `/` truncates toward zero over the four sign pairs, and MIN-CELL -1 / wraps
\ to MIN-CELL where `idiv` raises #DE.
: DIV-IMAGE ( -- )
   false OPEN,
   7 2 3 CASE2,
   -7 2 -3 CASE2,
   7 -2 -3 CASE2,
   -7 -2 3 CASE2,
   MIN-CELL -1 MIN-CELL CASE2,
   CLOSE,
   COLD-PAD, {: cold:n :}
   ENTRY, cold X64EMIT-TEST:QUOTIENT-ROUTINE
   s" div" false WRITE-IMAGE ;

\ Minus one divides by negating: the same routine, 7 and -7 each over -1.
: DIVNEG-IMAGE ( -- )
   false OPEN,
   7 -1 -7 CASE2,
   -7 -1 7 CASE2,
   CLOSE,
   COLD-PAD, {: cold:n :}
   ENTRY, cold X64EMIT-TEST:QUOTIENT-ROUTINE
   s" divneg" false WRITE-IMAGE ;

\ `a b mod 1000 +`: the remainder takes the dividend's sign, and MIN-CELL -1
\ mod is zero. A divisor sharing rdx with `cqo` would fault on a positive
\ dividend and answer 1000 for a negative one.
: REMAINDER-IMAGE ( -- )
   false OPEN,
   1000 7 2 1001 CASE3,
   1000 -7 2 999 CASE3,
   1000 7 -2 1001 CASE3,
   1000 -7 -2 999 CASE3,
   1000 MIN-CELL -1 1000 CASE3,
   CLOSE,
   COLD-PAD, {: cold:n :}
   ENTRY, cold X64EMIT-TEST:REMAINDER-ROUTINE
   s" remainder" false WRITE-IMAGE ;

\ A zero divisor: the cold side leaves through a stand-in for `throw`, placed
\ first, which checks the cell the case staged and the code pushed above it,
\ and exits 0.
: DIVZERO-IMAGE ( -- )
   false OPEN,
   MIN-CELL TERMINAL-CASE,
   CLOSE,
   POSITION {: callee:n :}
   MIN-CELL ARITH-ABI:E-DIV-ZERO STAND-IN,
   ALIGN, ENTRY,
   callee X64EMIT-TEST:DIVZERO-ROUTINE
   s" divzero" false WRITE-IMAGE ;

\ `v a lshift b rshift` with two counts that cannot keep rcx: a count lost to
\ the other, or the two taken in the wrong order, answers wrongly here.
: TWO-COUNTS-IMAGE ( -- )
   false OPEN,
   $FF 4 8 $F CASE3,
   $FF 8 4 $FF0 CASE3,
   1 63 63 1 CASE3,
   -1 1 60 15 CASE3,
   CLOSE, ENTRY, X64EMIT-TEST:TWO-COUNTS-ROUTINE
   s" two-counts" false WRITE-IMAGE ;

\ ---- the selects ---------------------------------------------------------------
\ Each aliasing case of the two selects is its own routine, the IR identity
\ test/compiler/x64-emit-fixture.f stages. Each case expects what this engine
\ answers over the same cells picked into the same operands, which is ARM64's
\ `csel`: cmpsel's holding answer when `a b <` and its failing one when not,
\ selz's nonzero answer when `v 0<>` and its zero one when not.
TYPED-VARIABLE SEL3-KEY [ n n n -- n ]
TYPED-VARIABLE SEL2-KEY [ n n -- n ]
TYPED-VARIABLE SEL-RUN [ -- ]

: CMPSEL-ANSWER ( n n n n -- n ) {: a:n b:n f:n h:n :}
   a b < if h else f then ;

: SELZ-ANSWER ( n n n -- n ) {: v:n nz:n z:n :}
   v 0<> if nz else z then ;

: SEL3-CASE, ( n n n -- ) {: a:n b:n c:n :}
   a b c  a b c SEL3-KEY @ execute  CASE3, ;

: SEL2-CASE, ( n n -- ) {: a:n b:n :}
   a b  a b SEL2-KEY @ execute  CASE2, ;

\ Signed less-than holding, level, and holding for 0 against 2^32, which a
\ 32-bit compare calls level; then the ends of the signed order both ways
\ round, whose difference overflows.
: CMPSEL-IMAGE ( ptr u8 n [ n n n -- n ] [ -- ] -- )
   SEL-RUN !  SEL3-KEY !  {: name:ptr u:n :}
   false OPEN,
   2 5 9 SEL3-CASE,
   5 5 9 SEL3-CASE,
   0 4294967296 9 SEL3-CASE,
   MIN-CELL MAX-CELL 9 SEL3-CASE,
   MAX-CELL MIN-CELL 9 SEL3-CASE,
   CLOSE, ENTRY, SEL-RUN @ execute
   name u false WRITE-IMAGE ;

\ Zero, then values a test of fewer than sixty-four bits calls zero or misses:
\ 2^32, and MIN-CELL, whose only set bit is the sign.
: SELZ3-IMAGE ( ptr u8 n [ n n n -- n ] [ -- ] -- )
   SEL-RUN !  SEL3-KEY !  {: name:ptr u:n :}
   false OPEN,
   0 7 9 SEL3-CASE,
   1 7 9 SEL3-CASE,
   -1 7 9 SEL3-CASE,
   4294967296 7 9 SEL3-CASE,
   MIN-CELL 7 9 SEL3-CASE,
   CLOSE, ENTRY, SEL-RUN @ execute
   name u false WRITE-IMAGE ;

: SELZ2-IMAGE ( ptr u8 n [ n n -- n ] [ -- ] -- )
   SEL-RUN !  SEL2-KEY !  {: name:ptr u:n :}
   false OPEN,
   0 9 SEL2-CASE,
   5 9 SEL2-CASE,
   -1 9 SEL2-CASE,
   4294967296 9 SEL2-CASE,
   MIN-CELL 9 SEL2-CASE,
   CLOSE, ENTRY, SEL-RUN @ execute
   name u false WRITE-IMAGE ;

\ Each key picks the cells into the operands as its routine's shape does.
: SELECT-IMAGES ( -- )
   s" cmpsel"      [: X64EMIT-TEST:SEL-LIT CMPSEL-ANSWER ;]
                   [: X64EMIT-TEST:CMPSEL-ROUTINE ;] CMPSEL-IMAGE
   s" cmpsel-fa"   [: >r over r> CMPSEL-ANSWER ;]
                   [: X64EMIT-TEST:CMPSEL-FA-ROUTINE ;] CMPSEL-IMAGE
   s" cmpsel-hb"   [: over CMPSEL-ANSWER ;]
                   [: X64EMIT-TEST:CMPSEL-HB-ROUTINE ;] CMPSEL-IMAGE
   s" cmpsel-ha"   [: >r over r> swap CMPSEL-ANSWER ;]
                   [: X64EMIT-TEST:CMPSEL-HA-ROUTINE ;] CMPSEL-IMAGE
   s" cmpsel-same" [: dup CMPSEL-ANSWER ;]
                   [: X64EMIT-TEST:CMPSEL-SAME-ROUTINE ;] CMPSEL-IMAGE
   s" selz"        [: SELZ-ANSWER ;]
                   [: X64EMIT-TEST:SELZ-ROUTINE ;] SELZ3-IMAGE
   s" selz-nv"     [: >r dup r> SELZ-ANSWER ;]
                   [: X64EMIT-TEST:SELZ-NV-ROUTINE ;] SELZ2-IMAGE
   s" selz-zv"     [: over SELZ-ANSWER ;]
                   [: X64EMIT-TEST:SELZ-ZV-ROUTINE ;] SELZ2-IMAGE
   s" selz-same"   [: dup SELZ-ANSWER ;]
                   [: X64EMIT-TEST:SELZ-SAME-ROUTINE ;] SELZ2-IMAGE ;

\ ---- the doubles ---------------------------------------------------------------
\ A case hands the routine bit patterns and checks the bits it answers. Where
\ IEEE 754 fixes the answer, the bits a case expects are the ones this engine's
\ own word leaves over the same bits, so the image holds the x86-64 render to
\ the engine that stages it; none of those cases makes a NaN, whose bits Habu
\ states and test/prim-float-cases.f pins on both machines. Where Habu states
\ the answer - `f>s` at the ends and on a NaN, a comparison with a NaN on
\ either side - the case names it and this engine's own word is held to the
\ same answer first.
$3FF0000000000000 constant F-ONE
$4000000000000000 constant F-TWO
$4008000000000000 constant F-THREE
$3FF8000000000000 constant F-HALF3               \ 1.5
$3FD0000000000000 constant F-QUARTER
$3FB999999999999A constant F-TENTH               \ 0.1, rounded
$3FC999999999999A constant F-FIFTH               \ 0.2, rounded
$C00599999999999A constant F-NEG-TWO-SEVEN       \ -2.7
$8000000000000000 constant F-NEGZERO
$7FF0000000000000 constant F-INF
$FFF0000000000000 constant F-NEGINF
$7FEFFFFFFFFFFFFF constant F-MAX                 \ the largest finite double
1 constant F-TINY                                \ the smallest subnormal
$7FF8000000000000 constant F-NAN
$FFF8000000000000 constant F-NEGNAN
$43E0000000000000 constant F-TWO63               \ 2^63, one past MAX-CELL
$C3E0000000000000 constant F-NEGTWO63            \ -2^63, MIN-CELL exactly
$43DFFFFFFFFFFFFF constant F-BELOW63             \ the largest double below 2^63

TYPED-VARIABLE FBIN-KEY [ r r -- r ]
TYPED-VARIABLE FUN1-KEY [ r -- r ]
TYPED-VARIABLE FCMP-KEY [ r r -- bool ]
TYPED-VARIABLE FCMP0-KEY [ r -- bool ]

: FBIN-CASE, ( n n -- ) {: a:n b:n :}
   a b  a IEEE754:BITS>F64 b IEEE754:BITS>F64 FBIN-KEY @ execute IEEE754:F64>BITS
   CASE2, ;

\ No case divides zero by zero or subtracts infinities: each answer is a number,
\ an infinity or a signed zero, rounded to nearest - 0.1 and 0.2 add to the
\ double above 0.3, the largest double overflows, the smallest subnormal halves
\ to zero, and one over minus zero is minus infinity.
: FBIN-CASES, ( [ r r -- r ] -- )
   FBIN-KEY !
   false OPEN,
   F-TENTH F-FIFTH FBIN-CASE,
   F-ONE F-THREE FBIN-CASE,
   F-MAX F-MAX FBIN-CASE,
   F-TINY F-TWO FBIN-CASE,
   F-ONE F-NEGZERO FBIN-CASE,
   CLOSE, ENTRY, ;

: FBIN-IMAGE ( HIR:opcode ptr u8 n [ r r -- r ] -- )
   FBIN-CASES,
   {: o:HIR:opcode name:ptr u:n :}
   o X64EMIT-TEST:FBIN-ROUTINE
   name u false WRITE-IMAGE ;

\ The same cases through `(a + b) - a`, whose `a` is copied before the add
\ destroys it. The largest double's sum overflows to an infinity the
\ subtraction keeps, so no case makes a NaN either.
: FKEEP-IMAGE ( -- )
   [: over f+ swap f- ;] FBIN-CASES,
   X64EMIT-TEST:FKEEP-ROUTINE
   s" fkeep" false WRITE-IMAGE ;

: FUN1-CASE, ( n -- ) {: a:n :}
   a  a IEEE754:BITS>F64 FUN1-KEY @ execute IEEE754:F64>BITS  CASE1, ;

: FUN1-CLOSE, ( HIR:opcode ptr u8 n -- ) {: o:HIR:opcode name:ptr u:n :}
   CLOSE, ENTRY, o X64EMIT-TEST:FUN1-ROUTINE
   name u false WRITE-IMAGE ;

\ The sign and the magnitude are bit operations, a NaN's sign included.
: FNEG-IMAGE ( -- )
   [: fnegate ;] FUN1-KEY !
   false OPEN,
   F-HALF3 FUN1-CASE,
   0 FUN1-CASE,
   F-NEGINF FUN1-CASE,
   F-NAN 1 + FUN1-CASE,
   HIR-OPCODE:FNEG s" fnegate" FUN1-CLOSE, ;

: FABS-IMAGE ( -- )
   [: fabs ;] FUN1-KEY !
   false OPEN,
   F-HALF3 F-NEGZERO or FUN1-CASE,
   F-NEGZERO FUN1-CASE,
   F-NEGINF FUN1-CASE,
   F-NEGNAN 1 + FUN1-CASE,
   HIR-OPCODE:FABS s" fabs" FUN1-CLOSE, ;

\ No negative operand: the square root of one is a NaN.
: FSQRT-IMAGE ( -- )
   [: fsqrt ;] FUN1-KEY !
   false OPEN,
   F-TWO FUN1-CASE,
   F-QUARTER FUN1-CASE,
   F-NEGZERO FUN1-CASE,
   F-INF FUN1-CASE,
   F-TINY FUN1-CASE,
   HIR-OPCODE:FSQRT s" fsqrt" FUN1-CLOSE, ;

: FCMP-CASE, ( n n n -- ) {: a:n b:n want:n :}
   a IEEE754:BITS>F64 b IEEE754:BITS>F64 FCMP-KEY @ execute
   if -1 else 0 then  want T=
   a b want CASE2, ;

\ The relation's flag for 1 against 2, 2 against 1 and minus zero against zero,
\ then a NaN on each side, which every relation answers false.
: FCMP-IMAGE ( HIR:opcode ptr u8 n n n n [ r r -- bool ] -- )
   FCMP-KEY !
   {: o:HIR:opcode name:ptr u:n lt:n gt:n eq:n :}
   s" this engine's own comparison answers each case's flag, false with a NaN on either side" T-LABEL
   false OPEN,
   F-ONE F-TWO lt FCMP-CASE,
   F-TWO F-ONE gt FCMP-CASE,
   F-NEGZERO 0 eq FCMP-CASE,
   F-NAN F-ONE 0 FCMP-CASE,
   F-ONE F-NAN 0 FCMP-CASE,
   CLOSE, ENTRY, o X64EMIT-TEST:FCMP-ROUTINE
   name u false WRITE-IMAGE ;

: FCMP0-CASE, ( n n -- ) {: a:n want:n :}
   a IEEE754:BITS>F64 FCMP0-KEY @ execute  if -1 else 0 then  want T=
   a want CASE1, ;

\ Minus one, one and minus zero, then a NaN of each sign: a negative NaN is
\ not below zero.
: FCMP0-IMAGE ( HIR:opcode ptr u8 n n n [ r -- bool ] -- )
   FCMP0-KEY !
   {: o:HIR:opcode name:ptr u:n neg:n zero:n :}
   s" this engine's own comparison with zero answers each case's flag, false on a NaN of either sign" T-LABEL
   false OPEN,
   F-ONE F-NEGZERO or neg FCMP0-CASE,
   F-ONE 0 FCMP0-CASE,
   F-NEGZERO zero FCMP0-CASE,
   F-NAN 0 FCMP0-CASE,
   F-NEGNAN 0 FCMP0-CASE,
   CLOSE, ENTRY, o X64EMIT-TEST:FCMP0-ROUTINE
   name u false WRITE-IMAGE ;

: FCMP-IMAGES ( -- )
   HIR-OPCODE:FLT s" flt" -1 0 0 [: f< ;] FCMP-IMAGE
   HIR-OPCODE:FGT s" fgt" 0 -1 0 [: f> ;] FCMP-IMAGE
   HIR-OPCODE:FEQ s" feq" 0 0 -1 [: f= ;] FCMP-IMAGE
   HIR-OPCODE:FLTZ s" fltz" -1 0 [: f0< ;] FCMP0-IMAGE
   HIR-OPCODE:FEQZ s" feqz" 0 -1 [: f0= ;] FCMP0-IMAGE ;

: INTREAL-CASE, ( n -- ) {: a:n :}
   a  a s>f IEEE754:F64>BITS  CASE1, ;

\ The ends of the cell and 2^53 + 1, a tie that rounds to the even 2^53.
: INTREAL-IMAGE ( -- )
   false OPEN,
   3 INTREAL-CASE,
   -7 INTREAL-CASE,
   MAX-CELL INTREAL-CASE,
   MIN-CELL INTREAL-CASE,
   9007199254740993 INTREAL-CASE,
   CLOSE, ENTRY, X64EMIT-TEST:INTREAL-ROUTINE
   s" intreal" false WRITE-IMAGE ;

: REALINT-CASE, ( n n -- ) {: a:n want:n :}
   a IEEE754:BITS>F64 f>s want T=
   a want CASE1, ;

\ `f>s` truncates toward zero, saturates at both ends and answers zero for a
\ NaN, where `cvttsd2si` alone answers MIN-CELL for all three.
: REALINT-IMAGE ( -- )
   s" this engine's own f>s answers each case's cell: saturated at the ends, zero for a NaN" T-LABEL
   false OPEN,
   F-NAN 0 REALINT-CASE,
   F-TWO63 MAX-CELL REALINT-CASE,
   F-NEGTWO63 MIN-CELL REALINT-CASE,
   F-INF MAX-CELL REALINT-CASE,
   F-NEGINF MIN-CELL REALINT-CASE,
   F-NEG-TWO-SEVEN -2 REALINT-CASE,
   CLOSE, ENTRY, X64EMIT-TEST:REALINT-ROUTINE
   s" realint" false WRITE-IMAGE ;

\ Next to the bounds: the largest double below 2^63 is a cell, the next double
\ below -2^63 is not, a NaN with its sign set is still zero, and 1.5 truncates.
: REALINT-NEAR-IMAGE ( -- )
   false OPEN,
   F-BELOW63 $7FFFFFFFFFFFFC00 REALINT-CASE,
   F-NEGTWO63 1 + MIN-CELL REALINT-CASE,
   F-NEGNAN 0 REALINT-CASE,
   F-HALF3 1 REALINT-CASE,
   CLOSE, ENTRY, X64EMIT-TEST:REALINT-ROUTINE
   s" realint-near" false WRITE-IMAGE ;

\ The eight bytes come back as they went: a signalling NaN is not quieted.
: BITS-IMAGE ( -- )
   false OPEN,
   $7FF0000000000001 dup CASE1,
   F-NEGZERO dup CASE1,
   -1 dup CASE1,
   F-TINY dup CASE1,
   CLOSE, ENTRY, X64EMIT-TEST:BITS-ROUTINE
   s" bits" false WRITE-IMAGE ;

: FCONST-IMAGE ( -- )
   false OPEN,
   0 X64EMIT-TEST:FCONST-BITS CASE1,
   5 X64EMIT-TEST:FCONST-BITS 5 + CASE1,
   CLOSE, ENTRY, X64EMIT-TEST:FCONST-ROUTINE
   s" fconst" false WRITE-IMAGE ;

: FCALL-CASE, ( n n -- ) {: a:n b:n :}
   a b  a 2 *  a s>f b s>f f- IEEE754:F64>BITS +  CASE2, ;

\ The callee, placed first, writes every XMM register and doubles the first
\ cell, so the answer is that plus the bits of `a - b`, both doubles made before
\ the call. No case has a equal to b: two doubles put away in one slot come back
\ as one, and their difference is zero.
: FCALL-IMAGE ( -- )
   false OPEN,
   1 3 FCALL-CASE,
   -3 5 FCALL-CASE,
   MAX-CELL -1 FCALL-CASE,
   CLOSE,
   POSITION {: callee:n :}
   XMM-CLOBBER,
   ALIGN, ENTRY,
   callee X64EMIT-TEST:FCALL-ROUTINE
   s" fcall" false WRITE-IMAGE ;

: FLOAT-IMAGES ( -- )
   HIR-OPCODE:FADD s" fadd" [: f+ ;] FBIN-IMAGE
   HIR-OPCODE:FSUB s" fsub" [: f- ;] FBIN-IMAGE
   HIR-OPCODE:FMUL s" fmul" [: f* ;] FBIN-IMAGE
   HIR-OPCODE:FDIV s" fdiv" [: f/ ;] FBIN-IMAGE
   FKEEP-IMAGE
   FNEG-IMAGE
   FABS-IMAGE
   FSQRT-IMAGE
   FCMP-IMAGES
   INTREAL-IMAGE
   REALINT-IMAGE
   REALINT-NEAR-IMAGE
   BITS-IMAGE
   FCONST-IMAGE
   FCALL-IMAGE ;
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
   SHL-IMAGE
   SHR-IMAGE
   SHL-ADD-IMAGE
   SHL-CROSS-IMAGE
   SUM-SHL-IMAGE
   SUM-SHR-IMAGE
   NOT-IMAGE
   RELATION-IMAGES
   MOVI-IMAGE
   DADDR-IMAGE
   LOOP-IMAGE
   SITE-IMAGES
   PRESSURE-IMAGE
   ARGS-IMAGE
   PBRANCH-IMAGE
   PLOOP-IMAGE
   PCALLER-IMAGE
   SIGNAL-IMAGE
   DIV-IMAGE
   DIVNEG-IMAGE
   REMAINDER-IMAGE
   DIVZERO-IMAGE
   TWO-COUNTS-IMAGE
   SELECT-IMAGES
   FLOAT-IMAGES
   SB-RESET DIR$ SB-APPEND s" /manifest" SB-APPEND SB$ TMP-PATH
   MANIFEST BUF:SPAN$ BUF:BLEN>N WRITE-ALL
   X64EMIT-TEST:ADDRESSED-REFUSAL
   DISPOSE
   MANIFEST BUF:DISPOSE
   T-REPORT ;
;package

X64ROUTINES:RUN
