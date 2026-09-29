\ Cross-build the real x64 pass-chain fixture into an executable for the peer.
\ The host checks the image; the peer must run hb-x64-peer with status 0 and
\ hb-x64-peer-negative with status 21. Neither image is a Habu engine.
require test/compiler/x64-chain.f
require src/habu/fdio.f
require src/arch/x86-64/icode.f

package X64PEER
using X64ASM
using X64CODE
private

1024 constant ROUTINE-OFF
$8000000000000000 constant MIN-CELL
$7FFFFFFFFFFFFFFF constant MAX-CELL
\ The two labels every assertion and case reaches, made fresh for each image.
variable EXIT-CELL
variable ROUTINE-CELL

: EXIT-LBL ( -- label ) EXIT-CELL @ >LABEL ;
: ROUTINE-LBL ( -- label ) ROUTINE-CELL @ >LABEL ;

\ These are the production image writers and OS seam over package X64CODE's
\ byte stream. They load into this package because the x86-64 sys.f spells the
\ host seam's syscall-number words, which must not become globals here.
s" src/os/image-bytes.f" required
s" src/os/linux-x86-64/elf.f" required
s" src/os/linux-x86-64/sign.f" required
s" src/habu/driver-io.f" required
s" src/os/linux-x86-64/sys.f" required

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

: CASE, ( n n n n -- ) {: a:n b:n expected:n failure:n :}
   R12 RBP ASM-SINK ENC-MOV-RR
   RAX a IMM RAX R12 MEM-AT ASM-SINK ENC-MOV-MR
   RAX b IMM RAX R12 8 MEM-OFF ASM-SINK ENC-MOV-MR
   R12 16 >IMM8 ASM-SINK ENC-ADD-RI8
   ROUTINE-LBL CALL,
   RAX R12 -8 MEM-OFF ASM-SINK ENC-MOV-RM
   RCX expected IMM RAX RCX ASM-SINK ENC-CMP-RR failure ASSERT-EQ
   RSP RBP ASM-SINK ENC-CMP-RR 31 ASSERT-EQ
   RAX RBP ASM-SINK ENC-MOV-RR RAX 8 >IMM8 ASM-SINK ENC-ADD-RI8
   R12 RAX ASM-SINK ENC-CMP-RR 32 ASSERT-EQ ;

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

: ENTRY, ( bool -- ) {: negative:bool :}
   ASM-RESET
   LBL EXIT-CELL !  LBL ROUTINE-CELL !
   RSP 1024 >IMM32 ASM-SINK ENC-SUB-RI32
   RBP RSP ASM-SINK ENC-MOV-RR
   RBX $22334455 IMM R13 $33445566 IMM
   R14 $44556677 IMM R15 $55667788 IMM
   20 7 negative if 12 else 13 then 21 CASE,
   7 20 -13 22 CASE,
   -20 -7 -13 23 CASE,
   MIN-CELL 1 MAX-CELL 24 CASE,
   MAX-CELL -1 MIN-CELL 25 CASE,
   \ Exercise the real OS seam's success/error carry polarity, not a byte model.
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
   ROUTINE-OFF PAD-TO
   ROUTINE-LBL LBL, ;

public
: POSITION ( -- n ) VMBASE CODE-OFF + ROUTINE-OFF + ;
: APPEND-ROUTINE ( ptr u8 n -- ) {: a:ptr u:n :}
   ASM-LEN ROUTINE-OFF T=
   u 0 > TTRUE
   a u BUF:N>BLEN ASM-SINK BUF:APPEND-SPAN
   s" the executable carries the compiler's exact routine bytes" T-LABEL
   CODE ROUTINE-OFF + u a u T$= ;
;package

\ Reuse the same HIR and pass-row driver as the host suite. This adds execution
\ of its result; no second copy of the compiler fixture or private driver exists.
package X64CHAIN-TEST
private
: PEER-BODY ( IR-CTX:ctx -- )
   HIR-MOD BUILD-DIFF
   2 1 CHAIN {: m:IR-BUILD:module :}
   CC m X64PEER:POSITION NBACK:EMIT
   X64EMIT:BYTES X64EMIT:SIZE X64PEER:APPEND-ROUTINE
   CC NBACK:RETIRE
   CC NBACK:RELEASE ;
public
: PEER-ROUTINE ( -- ) WBND [: PEER-BODY ;] IR-CTX:WITH-CONTEXT ;
;package

package X64PEER
using X64ASM
using X64CODE
private
: EXIT, ( -- )
   EXIT-LBL LBL,
   0 >R32 NR-EXIT >IMM32 ASM-SINK ENC-MOV32-RI32
   ASM-SINK ENC-SYSCALL ;

: BUILD ( bool ptr u8 n -- ) {: negative:bool path:ptr pathu:n :}
   negative ENTRY,
   X64CHAIN-TEST:PEER-ROUTINE
   EXIT,
   ASM-CODE BUILD-IMAGE
   s" x64-peer" SET-SIGID CODESIG2
   path pathu DRV-WRITE-IMAGE
   s" ELF names x86-64 and enters this fixture's code" T-LABEL
   $12 M-OFF M-LE32@ $FFFF and 62 T=
   $18 M-OFF M-LE32@ VMBASE CODE-OFF + T=
   $1C M-OFF M-LE32@ 0 T= ;

public
: RUN ( -- )
   T-RESET
   ASM-SINK CODE-CAP-BYTES BUF:N>BLEN BUF:INIT
   false s" hb-x64-peer" TMP-PATH BUILD
   true s" hb-x64-peer-negative" TMP-PATH BUILD
   ASM-SINK BUF:DISPOSE
   T-REPORT ;
;package

X64PEER:RUN
