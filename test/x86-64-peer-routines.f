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
\ Three fixtures have no image. The rows refuse BUILD-ADDRESSED with
\ E-IR-VERIFY-OPTYPE: it takes its memory order as an argument, which a
\ data-stack contract has no cell for. The last check, after every image is
\ written, asserts that refusal. BUILD-SELFCALLER is RECURSE with no base case,
\ so it never returns. BUILD-TRAP calls `die` at its address in the host
\ engine's dictionary (select-x64.f TRAP-ENTRY), which is no address of an x86
\ image; the terminal fixture here renders the same `x64.trap` to a callee the
\ image carries.
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
require src/habu/boot-x64.f
require test/x86-64-peer-harness.f

\ The fixtures stay in the package that stages them; this adds each one's trip
\ through the rows to the harness's next address.
package X64EMIT-TEST
private
variable CALLEE                      \ the entry the wordcall site names
TYPED-VARIABLE REL-OP HIR:opcode     \ the relation the compare sites stage

public
\ The cell the terminal fixture computes and hands its callee.
$0123456789ABCDEF constant TERMINAL-CELL
private

: ROWS-EMIT ( n n NBACK:linkage -- ) {: in:n out:n l:NBACK:linkage :}
   CC in out l NBACK:DECLARE
   CC BB NBACK:SELECT {: m0:IR-BUILD:module :}
   CC m0 NBACK:PRUNE {: m1:IR-BUILD:module :}
   CC m1 NBACK:FIXPOINT {: m:IR-BUILD:module :}
   CC m X64HARNESS:POSITION NBACK:EMIT ;

: ROWS-DONE ( -- )
   CC NBACK:RETIRE
   CC NBACK:RELEASE ;

: ROWS, ( n n NBACK:linkage -- )
   ROWS-EMIT
   X64EMIT:BYTES X64EMIT:SIZE X64HARNESS:APPEND-ROUTINE
   ROWS-DONE ;

\ The quoting fixture answers the address of its second function, so the label
\ its case expects is bound where the emission laid that function.
: QUOTING-ROWS, ( -- )
   0 1 NBACK:L-NONE ROWS-EMIT
   X64EMIT:BYTES X64EMIT:SIZE  1 X64EMIT:FUNCTION-OFFSET@
   X64HARNESS:APPEND-QUOTING
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
: NOT-BODY ( IR-CTX:ctx -- )      HIR-MOD BUILD-NOT 1 1 NBACK:L-NONE ROWS, ;
: RELATION-BODY ( IR-CTX:ctx -- )
   HIR-MOD REL-OP @ BUILD-RELATION 2 1 NBACK:L-NONE ROWS, ;
: RELATIONI-BODY ( IR-CTX:ctx -- )
   HIR-MOD REL-OP @ BUILD-RELATIONI 2 1 NBACK:L-NONE ROWS, ;
: MOVI-BODY ( IR-CTX:ctx -- )     HIR-MOD BUILD-MOVI 1 1 NBACK:L-NONE ROWS, ;
: DADDR-BODY ( IR-CTX:ctx -- )
   HIR-MOD BUILD-DADDRESSED 1 1 NBACK:L-NONE ROWS, ;
: LOOP-BODY ( IR-CTX:ctx -- )     HIR-MOD BUILD-LOOP 2 1 NBACK:L-NONE ROWS, ;
: WORDCALL-BODY ( IR-CTX:ctx -- )
   HIR-MOD CALLEE @ BUILD-WORDCALLER 1 1 NBACK:L-CALLED ROWS, ;
: TERMINAL-BODY ( IR-CTX:ctx -- )
   HIR-MOD CALLEE @ BUILD-TERMINAL 1 0 NORET ROWS, ;
: QUOTER-BODY ( IR-CTX:ctx -- )   HIR-MOD BUILD-QUOTER QUOTING-ROWS, ;

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
: SHL-ROUTINE ( -- )      WBND [: SHL-BODY ;] IR-CTX:WITH-CONTEXT ;
: SHR-ROUTINE ( -- )      WBND [: SHR-BODY ;] IR-CTX:WITH-CONTEXT ;
: SHL-ADD-ROUTINE ( -- )  WBND [: SHL-ADD-BODY ;] IR-CTX:WITH-CONTEXT ;
: SHL-CROSS-ROUTINE ( -- ) WBND [: SHL-CROSS-BODY ;] IR-CTX:WITH-CONTEXT ;
: NOT-ROUTINE ( -- )      WBND [: NOT-BODY ;] IR-CTX:WITH-CONTEXT ;
: RELATION-ROUTINE ( HIR:opcode -- )
   REL-OP !  WBND [: RELATION-BODY ;] IR-CTX:WITH-CONTEXT ;
: RELATIONI-ROUTINE ( HIR:opcode -- )
   REL-OP !  WBND [: RELATIONI-BODY ;] IR-CTX:WITH-CONTEXT ;
: MOVI-ROUTINE ( -- )     WBND [: MOVI-BODY ;] IR-CTX:WITH-CONTEXT ;
: DADDR-ROUTINE ( -- )    WBND [: DADDR-BODY ;] IR-CTX:WITH-CONTEXT ;
: LOOP-ROUTINE ( -- )     WBND [: LOOP-BODY ;] IR-CTX:WITH-CONTEXT ;
: WORDCALL-ROUTINE ( n -- )
   CALLEE !  WBND [: WORDCALL-BODY ;] IR-CTX:WITH-CONTEXT ;
: TERMINAL-ROUTINE ( n -- )
   CALLEE !  WBND [: TERMINAL-BODY ;] IR-CTX:WITH-CONTEXT ;
: QUOTER-ROUTINE ( -- )   WBND [: QUOTER-BODY ;] IR-CTX:WITH-CONTEXT ;
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

\ The routine never comes back: its terminal call leaves through a stand-in for
\ the callee, placed first, which checks the argument and the computed cell the
\ routine published below the data-stack pointer and exits 0.
: TERMINAL-IMAGE ( -- )
   false OPEN,
   MIN-CELL TERMINAL-CASE,
   CLOSE,
   POSITION {: callee:n :}
   MIN-CELL X64EMIT-TEST:TERMINAL-CELL STAND-IN,
   ALIGN, ENTRY,
   callee X64EMIT-TEST:TERMINAL-ROUTINE
   s" terminal" false WRITE-IMAGE ;

\ The answer is the address the emission laid the second function at, which is
\ where the harness bound the label the case compares it with.
: QUOTER-IMAGE ( -- )
   false OPEN,
   QUOTE-CASE,
   CLOSE, ENTRY, X64EMIT-TEST:QUOTER-ROUTINE
   s" quoter" false WRITE-IMAGE ;

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

\ A real SIGUSR1 through the handler boot-x64.f installs, with no routine of the
\ rows: the case ends the image itself.
: SIGNAL-IMAGE ( -- )
   false OPEN,
   SIGNAL-CASE,
   s" signal" false WRITE-IMAGE ;

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
   NOT-IMAGE
   RELATION-IMAGES
   MOVI-IMAGE
   DADDR-IMAGE
   LOOP-IMAGE
   WORDCALL-IMAGE
   TERMINAL-IMAGE
   QUOTER-IMAGE
   PRESSURE-IMAGE
   PBRANCH-IMAGE
   PLOOP-IMAGE
   PCALLER-IMAGE
   SIGNAL-IMAGE
   SB-RESET DIR$ SB-APPEND s" /manifest" SB-APPEND SB$ TMP-PATH
   MANIFEST BUF:SPAN$ BUF:BLEN>N WRITE-ALL
   X64EMIT-TEST:ADDRESSED-REFUSAL
   DISPOSE
   MANIFEST BUF:DISPOSE
   T-REPORT ;
;package

X64ROUTINES:RUN
