\ Cross-build the real x64 pass-chain fixture into an executable for the peer.
\ The host checks the image; the peer must run hb-x64-peer with status 0 and
\ hb-x64-peer-negative with status 21. Neither image is a Habu engine.
require test/compiler/x64-chain.f
require src/habu/fdio.f

package X64PEER
using X64ASM
private

4096 constant CODE-CAP-BYTES
1024 constant ROUTINE-OFF
2048 constant EXIT-OFF
$8000000000000000 constant MIN-CELL
$7FFFFFFFFFFFFFFF constant MAX-CELL
create SINK BUF:HDR-BYTES allot

: CODE ( -- ptr u8 ) SINK BUF:SPAN$ drop ;
: ASM-LEN ( -- n ) SINK BUF:LEN@ BUF:BLEN>N ;
: ASM-SINK ( -- ptr u8 ) SINK ;

\ These are the production image writers over this fixture's byte stream.
\ Keeping them in this package avoids loading the ARM64 word-count assembler.
s" src/os/image-bytes.f" required
s" src/os/linux-x86-64/elf.f" required
s" src/os/linux-x86-64/sign.f" required
s" src/habu/driver-io.f" required
s" src/os/linux-x86-64/sys.f" required

: IMM ( r64 n -- ) >IMM64 SINK ENC-MOV-RI64 ;

: PAD-TO ( n -- ) {: target:n :}
   target ASM-LEN < if E-BUF-BOUNDS throw then
   target ASM-LEN - 0 ?do SINK ENC-NOP loop ;

\ MOV preserves the flags being tested. A mismatch branches to the one exit
\ syscall with the assertion's nonzero status in edi; Jcc rel32 is six bytes.
: FAIL-IF ( condition n -- ) {: cond:condition failure:n :}
   RDI failure IMM
   cond EXIT-OFF ASM-LEN 6 + - >REL SINK ENC-JCC-REL32 ;

: ASSERT-EQ ( n -- ) C-NE swap FAIL-IF ;

: CASE, ( n n n n -- ) {: a:n b:n expected:n failure:n :}
   R12 RBP SINK ENC-MOV-RR
   RAX a IMM RAX R12 MEM-AT SINK ENC-MOV-MR
   RAX b IMM RAX R12 8 MEM-OFF SINK ENC-MOV-MR
   R12 16 >IMM8 SINK ENC-ADD-RI8
   ROUTINE-OFF ASM-LEN 5 + - >REL SINK ENC-CALL-REL32
   RAX R12 -8 MEM-OFF SINK ENC-MOV-RM
   RCX expected IMM RAX RCX SINK ENC-CMP-RR failure ASSERT-EQ
   RSP RBP SINK ENC-CMP-RR 31 ASSERT-EQ
   RAX RBP SINK ENC-MOV-RR RAX 8 >IMM8 SINK ENC-ADD-RI8
   R12 RAX SINK ENC-CMP-RR 32 ASSERT-EQ ;

: RESERVED, ( r64 n n -- ) {: reg:r64 value:n failure:n :}
   RAX value IMM reg RAX SINK ENC-CMP-RR failure ASSERT-EQ ;

: ENTRY, ( bool -- ) {: negative:bool :}
   SINK BUF:CLEAR
   RSP 1024 >IMM32 SINK ENC-SUB-RI32
   RBP RSP SINK ENC-MOV-RR
   RBX $22334455 IMM R13 $33445566 IMM
   R14 $44556677 IMM R15 $55667788 IMM
   20 7 negative if 12 else 13 then 21 CASE,
   7 20 -13 22 CASE,
   -20 -7 -13 23 CASE,
   MIN-CELL 1 MAX-CELL 24 CASE,
   MAX-CELL -1 MIN-CELL 25 CASE,
   \ Exercise the real OS seam's success/error carry polarity, not a byte model.
   NR-GETPID SYS, C-B 41 FAIL-IF
   RAX 0 >IMM8 SINK ENC-CMP-RI8 C-LE 42 FAIL-IF
   RDI -1 IMM NR-CLOSE SYS, C-AE 43 FAIL-IF
   RAX -9 >IMM8 SINK ENC-CMP-RI8 44 ASSERT-EQ
   RBX $22334455 33 RESERVED,
   R13 $33445566 34 RESERVED,
   R14 $44556677 35 RESERVED,
   R15 $55667788 36 RESERVED,
   RDI 0 IMM
   EXIT-OFF ASM-LEN 5 + - >REL SINK ENC-JMP-REL32
   ROUTINE-OFF PAD-TO ;

public
: POSITION ( -- n ) VMBASE CODE-OFF + ROUTINE-OFF + ;
: APPEND-ROUTINE ( ptr u8 n -- ) {: a:ptr u:n :}
   ASM-LEN ROUTINE-OFF T=
   u 0 > TTRUE
   a u BUF:N>BLEN SINK BUF:APPEND-SPAN
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
private
: EXIT, ( -- )
   EXIT-OFF PAD-TO
   0 >R32 NR-EXIT >IMM32 SINK ENC-MOV32-RI32
   SINK ENC-SYSCALL ;

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
   SINK CODE-CAP-BYTES BUF:N>BLEN BUF:INIT
   false s" hb-x64-peer" TMP-PATH BUILD
   true s" hb-x64-peer-negative" TMP-PATH BUILD
   SINK BUF:DISPOSE
   T-REPORT ;
;package

X64PEER:RUN
