\ x86-64-peer-harness.f - the executable an x86-64 peer runs around routines
\ the real pass rows emitted. An image is a checking entry, the routines, and
\ the one exit syscall, staged in this order:
\
\    OPEN,  cases  CLOSE,  [ callee  ALIGN, ]  [ n SKIP, ]  ENTRY,  routine  WRITE-ELF
\
\ A routine staged past a SKIP, lands n bytes beyond where its emission placed
\ it; LINK-CALL and LINK-CODE write its placement-dependent fields again from
\ the emission's rows.
\
\ The entry calls the routine bound at ENTRY, once per case, and checks its
\ answer, the machine and data stacks around the call, the registers a routine
\ must keep, the OS seam and a linked label. The first failed check exits with
\ its own status; an image whose checks all hold exits 0. Case checks exit
\ FIRST-CASE upward in the order they are staged, below the harness's own
\ statuses (31 and up). A negative image expects a wrong answer from its first
\ case check, so it must exit FIRST-CASE.
\
\ A routine control never comes back from is called by the one TERMINAL-CASE,
\ of its image and leaves through a STAND-IN, staged as its callee: the stand-in
\ checks what the routine published and exits 0 itself, and a routine that
\ returns fails the case. A routine answering the address of one of its own
\ functions is appended with APPEND-QUOTING, which binds the label its
\ QUOTE-CASE, compares the answer with where that function landed.
\
\ An image with an entry of its own (test/x86-64-skel-image.f) skips the
\ checking entry: it stages its stream after ASM-RESET and ends with WRITE.
require lib/test.f
require lib/byte-buffer.f
require src/habu/fdio.f
require src/arch/x86-64/icode.f
require src/compiler/native/x64ir.f

\ The production image writer loads at top level, as every production path
\ loads it: elf.f requires src/os/linux-x86-64/target-layout.f, which opens
\ package X64LAYOUT, and packages do not nest.
\ src/os/image-bytes.f sizes MSIZE from a bare CODE-CAP-BYTES at load, so it
\ loads under `using X64CODE`.
using X64CODE
require src/os/image-bytes.f
;using
require src/os/linux-x86-64/target-layout.f
require src/os/linux-x86-64/elf.f

package X64HARNESS
using X64ASM
using X64CODE
public

$8000000000000000 constant MIN-CELL
$7FFFFFFFFFFFFFFF constant MAX-CELL
21 constant FIRST-CASE

private

1024 constant ROUTINE-OFF
31 constant CASE-END                 \ where the harness's own statuses start
512 constant SCRATCH-OFF             \ a reserved cell no data stack reaches
\ The labels every assertion and case reaches, made fresh for each image.
variable EXIT-CELL
variable ROUTINE-CELL
variable QUOTED-CELL                 \ where a quoting routine's function lands
variable CASE-NEXT                   \ the status the next case check exits with
variable WRONG-AT                    \ the check a negative image fails, or 0

: EXIT-LBL ( -- label ) EXIT-CELL @ >LABEL ;
: ROUTINE-LBL ( -- label ) ROUTINE-CELL @ >LABEL ;
: QUOTED-LBL ( -- label ) QUOTED-CELL @ >LABEL ;

\ The signer, the image driver and the OS seam over package X64CODE's byte
\ stream. They load into this package because the x86-64 sys.f spells the host
\ seam's syscall-number words, which must not become globals here.
s" src/os/linux-x86-64/sign.f" required
s" src/habu/driver-io.f" required
s" src/os/linux-x86-64/sys.f" required
using X64LAYOUT   \ the guard: a bare layout name refuses (target-layout.f)

: IMM ( r64 n -- ) >IMM64 ASM-SINK ENC-MOV-RI64 ;

: PAD-TO ( n -- ) {: target:n :}
   target ASM-LEN < if E-BUF-BOUNDS throw then
   target ASM-LEN - 0 ?do ASM-SINK ENC-NOP loop ;

\ MOV preserves the flags being tested. A mismatch branches to the one exit
\ syscall with the assertion's nonzero status in edi.
: FAIL-IF ( condition n -- ) {: cond:condition failure:n :}
   RDI failure IMM
   cond EXIT-LBL JCC, ;

: ASSERT-EQ ( n -- ) C-NE swap FAIL-IF ;

: STATUS ( -- n )
   CASE-NEXT @ {: s:n :}
   s CASE-END >= if E-BUF-BOUNDS throw then
   s 1+ CASE-NEXT !
   s ;

\ Compare rax with what the next case check expects, which rcx holds: one less
\ in the check a negative image gets wrong.
: EXPECT-RCX, ( -- )
   STATUS {: s:n :}
   s WRONG-AT @ = if RCX 1 >IMM32 ASM-SINK ENC-SUB-RI32 then
   RAX RCX ASM-SINK ENC-CMP-RR s ASSERT-EQ ;

: EXPECT, ( n -- ) {: want:n :}  RCX want IMM EXPECT-RCX, ;

\ The data stack starts at rbp and grows up; a case stages argument `idx` in
\ cell `idx` of it.
: ARG, ( n n -- ) {: v:n idx:n :}
   RAX v IMM RAX R12 idx 8 * MEM-OFF ASM-SINK ENC-MOV-MR ;

\ Move the data-stack pointer past the `in` staged arguments and call the
\ routine.
: CALLED, ( n -- ) {: in:n :}
   R12 in 8 * >IMM8 ASM-SINK ENC-ADD-RI8
   ROUTINE-LBL CALL, ;

\ Check the one answer the routine left in rax, that the machine stack came
\ back balanced, and that the data stack holds that answer and nothing else.
: ANSWERED, ( -- )
   RAX R12 -8 MEM-OFF ASM-SINK ENC-MOV-RM
   EXPECT-RCX,
   RSP RBP ASM-SINK ENC-CMP-RR 31 ASSERT-EQ
   RAX RBP ASM-SINK ENC-MOV-RR RAX 8 >IMM8 ASM-SINK ENC-ADD-RI8
   R12 RAX ASM-SINK ENC-CMP-RR 32 ASSERT-EQ ;

: INVOKE, ( n n -- ) {: in:n want:n :}
   in CALLED,
   RCX want IMM ANSWERED, ;

: RESERVED, ( r64 n n -- ) {: reg:r64 value:n failure:n :}
   RAX value IMM reg RAX ASM-SINK ENC-CMP-RR failure ASSERT-EQ ;

\ A movabs label site holds the address the kernel loaded its label at: lea with
\ a zero rip displacement reads the address of the instruction after it, and the
\ label is bound there.
: LINKED-ADDRESS, ( -- )
   LBL {: at:label :}
   RCX 0 MEM-RIP ASM-SINK ENC-LEA
   at LBL,
   RAX at MOVABS,
   RAX RCX ASM-SINK ENC-CMP-RR 51 ASSERT-EQ ;

\ exit_group ends every thread: a booted image's task thread that outlives a
\ thread-local exit(2) would end the process with its own status instead.
: EXIT, ( -- )
   EXIT-LBL LBL,
   0 >R32 NR-EXIT-GROUP >IMM32 ASM-SINK ENC-MOV32-RI32
   ASM-SINK ENC-SYSCALL ;

public

\ The entry's head: reserve the region the stacks and the scratch cell live in,
\ start the data stack at its base, and load the registers a routine must keep.
: OPEN, ( bool -- ) {: negative:bool :}
   ASM-RESET
   LBL EXIT-CELL !  LBL ROUTINE-CELL !  LBL QUOTED-CELL !
   FIRST-CASE CASE-NEXT !
   negative if FIRST-CASE else 0 then WRONG-AT !
   RSP 1024 >IMM32 ASM-SINK ENC-SUB-RI32
   RBP RSP ASM-SINK ENC-MOV-RR
   RBX $22334455 IMM R13 $33445566 IMM
   R14 $44556677 IMM R15 $55667788 IMM ;

\ One cell in and one out.
: CASE1, ( n n -- ) {: a:n want:n :}
   R12 RBP ASM-SINK ENC-MOV-RR
   a 0 ARG,
   1 want INVOKE, ;

\ Two cells in and one out.
: CASE2, ( n n n -- ) {: a:n b:n want:n :}
   R12 RBP ASM-SINK ENC-MOV-RR
   a 0 ARG,  b 1 ARG,
   2 want INVOKE, ;

\ Three cells in and one out.
: CASE3, ( n n n n -- ) {: a:n b:n c:n want:n :}
   R12 RBP ASM-SINK ENC-MOV-RR
   a 0 ARG,  b 1 ARG,  c 2 ARG,
   3 want INVOKE, ;

\ One cell in, the address of the scratch cell once it holds `content`, and one
\ out; then a second check, of what the cell holds after the call.
: CELL-CASE, ( n n n -- ) {: content:n want:n after:n :}
   RAX content IMM RAX RBP SCRATCH-OFF MEM-OFF ASM-SINK ENC-MOV-MR
   R12 RBP ASM-SINK ENC-MOV-RR
   RAX RBP SCRATCH-OFF MEM-OFF ASM-SINK ENC-LEA
   RAX R12 MEM-AT ASM-SINK ENC-MOV-MR
   1 want INVOKE,
   RAX RBP SCRATCH-OFF MEM-OFF ASM-SINK ENC-MOV-RM
   after EXPECT, ;

\ One cell in and no return: the routine leaves through the STAND-IN, staged as
\ its callee, so control coming back here is itself the failure, 37. It is the
\ image's one case, because nothing after it runs.
: TERMINAL-CASE, ( n -- ) {: a:n :}
   R12 RBP ASM-SINK ENC-MOV-RR
   a 0 ARG,
   1 CALLED,
   RDI 37 IMM EXIT-LBL JMP, ;

\ No cell in and one out, the address of the routine's function APPEND-QUOTING
\ bound QUOTED-LBL at.
: QUOTE-CASE, ( -- )
   R12 RBP ASM-SINK ENC-MOV-RR
   0 CALLED,
   RCX QUOTED-LBL MOVABS, ANSWERED, ;

\ The entry's tail: exercise the real OS seam's success/error carry polarity,
\ not a byte model; check the kept registers and a linked label; then exit 0.
\ The routines start at ROUTINE-OFF.
: CLOSE, ( -- )
   NR-GETPID SYS, C-B 41 FAIL-IF
   RAX 0 >IMM8 ASM-SINK ENC-CMP-RI8 C-LE 42 FAIL-IF
   RDI -1 IMM NR-CLOSE SYS, C-AE 43 FAIL-IF
   RAX -9 >IMM8 ASM-SINK ENC-CMP-RI8 44 ASSERT-EQ
   RBX $22334455 33 RESERVED,
   R13 $33445566 34 RESERVED,
   R14 $44556677 35 RESERVED,
   R15 $55667788 36 RESERVED,
   LINKED-ADDRESS,
   RDI 0 IMM
   EXIT-LBL JMP,
   ROUTINE-OFF PAD-TO ;

\ The address the next routine appended lands at, so the one it is emitted at.
: POSITION ( -- n ) VMBASE X64LAYOUT:CODE-OFF + ASM-LEN + ;

\ Pad to the unit a code region hands slots out in, where a second routine can
\ be placed.
: ALIGN, ( -- )
   ASM-LEN X64IR:SP-ALIGN + 1- {: end:n :}
   end end X64IR:SP-ALIGN mod - PAD-TO ;

\ The routine every case calls starts here.
: ENTRY, ( -- ) ROUTINE-LBL LBL, ;

\ The callee a TERMINAL-CASE,'s routine leaves through, standing in for the one
\ that ends the process: it checks the two cells published below r12 and that
\ they end two cells above where the case started the data stack, then exits 0.
: STAND-IN, ( n n -- ) {: a:n b:n :}
   RAX R12 -16 MEM-OFF ASM-SINK ENC-MOV-RM  a EXPECT,
   RAX R12 -8 MEM-OFF ASM-SINK ENC-MOV-RM  b EXPECT,
   RAX R12 ASM-SINK ENC-MOV-RR  RAX RBP ASM-SINK ENC-SUB-RR  16 EXPECT,
   RDI 0 IMM EXIT-LBL JMP, ;

: APPEND-ROUTINE ( ptr u8 n -- ) {: a:ptr u:n :}
   ASM-LEN {: at:n :}
   u 0 > TTRUE
   a u BUF:N>BLEN ASM-SINK BUF:APPEND-SPAN
   s" the executable carries the compiler's exact routine bytes" T-LABEL
   CODE at + u a u T$= ;

\ The same for a routine whose answer is the address of its own function `off`
\ bytes in: its bytes go in two parts, and QUOTED-LBL is bound between them.
: APPEND-QUOTING ( ptr u8 n n -- ) {: a:ptr u:n off:n :}
   a off APPEND-ROUTINE
   QUOTED-LBL LBL,
   a off +  u off -  APPEND-ROUTINE ;

\ Pad `n` bytes with ud2, so the routine appended next lands that much further
\ on. A call its rows did not write again still reaches `n` bytes past a callee
\ staged just before it, which is inside this pad and on the first byte of a
\ ud2, so it stops with SIGILL rather than sliding through padding into code.
: SKIP, ( n -- ) {: n:n :}
   ASM-LEN n + {: target:n :}
   begin ASM-LEN target < while ASM-SINK ENC-UD2 repeat
   ASM-LEN target <> if E-BUF-BOUNDS throw then ;

private

\ ---- a routine written where its emission did not place it -------------------
\ The emission's rows say which of its fields depend on where it is written and
\ what goes in them, so these write them again the way a linker writes a
\ routine it did not place. A call or tail-branch row names its instruction's
\ first byte and an absolute target: the rel32 follows the one opcode byte of
\ `call` and `jmp` and counts from the end of the instruction (docs/x86-64.md
\ "Live-region sites"). A CODE literal is the placement plus a function's
\ offset, so it moves by what the routine moved.
4 constant REL32-N
8 constant IMM64-N

\ The `u` staged bytes at absolute address `at`, all of them inside the stream.
: STAGED ( n n -- ptr u8 ) {: at:n u:n :}
   at VMBASE X64LAYOUT:CODE-OFF + - {: off:n :}
   off 0 <  off u + ASM-LEN >  or if E-BUF-BOUNDS throw then
   CODE off + ;

: LE! ( n ptr u8 n -- ) {: v:n p:ptr u:n :}
   u 0 ?do  v i 8 * rshift $FF and  p i + c!  loop ;

: LE@ ( ptr u8 n -- n ) {: p:ptr u:n :}
   0  u 0 ?do  p i + c@  i 8 * lshift or  loop ;

public

\ The rel32 of the `call` or `jmp` at address `site`, for the absolute `target`
\ its row names.
: LINK-CALL ( n n -- ) {: site:n target:n :}
   site CALL-REL32-OFF + {: field:n :}
   target  field REL32-N +  -  {: rel:n :}
   rel X64IR:IMM-LIMIT negate <  rel X64IR:IMM-LIMIT >=  or
   if E-X64EMIT-REACH throw then
   rel  field REL32-N STAGED  REL32-N LE! ;

\ The immediate of the CODE literal whose `mov r64, imm64` is at address
\ `site`, moved by `moved` bytes.
: LINK-CODE ( n n -- ) {: site:n moved:n :}
   site MOV-RI64-IMM-OFF + IMM64-N STAGED {: p:ptr :}
   p IMM64-N LE@ moved +  p IMM64-N LE! ;

\ Write the staged stream as it stands: byte 0 is the ELF entry.
: WRITE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   ASM-CODE BUILD-IMAGE
   s" x64-peer" SET-SIGID CODESIG2
   path pathu DRV-WRITE-IMAGE
   s" ELF names x86-64 and enters this fixture's code" T-LABEL
   $12 M-OFF M-LE32@ $FFFF and 62 T=
   $18 M-OFF M-LE32@ VMBASE X64LAYOUT:CODE-OFF + T=
   $1C M-OFF M-LE32@ 0 T= ;

\ Close a peer image with the exit syscall every check reaches, then write it.
: WRITE-ELF ( ptr u8 n -- ) EXIT, WRITE ;

\ The sink every image is staged in, held across the images of one run.
: INIT ( -- ) ASM-SINK CODE-CAP-BYTES BUF:N>BLEN BUF:INIT ;
: DISPOSE ( -- ) ASM-SINK BUF:DISPOSE ;
;using   \ X64LAYOUT
;package
