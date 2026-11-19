split-str
\ x86-64-peer-harness.f - the checks, exit and ELF writer of the images an
\ x86-64 peer runs: an image stages after ASM-RESET and ends with WRITE.
require src/lib/test.f
require src/lib/byte-buffer.f
require src/habu/fdio.f
require src/habu/sign-id.f
require src/arch/x86-64/icode.f
require src/os/linux-flags.f

\ The production image writer loads at top level, as every production path loads it.
require src/os/linux/layout.f
require src/os/linux/elf.f
require src/os/linux/sign.f

package X64HARNESS
using X64ASM
using X64CODE
using IMAGE-BYTES
public

$8000000000000000 constant MIN-CELL
$7FFFFFFFFFFFFFFF constant MAX-CELL
\ Each check exits with its own status, FIRST-CASE upward in staging order; a
\ negative image fails its first check, so it exits FIRST-CASE.
21 constant FIRST-CASE

private

31 constant CASE-END                 \ one past the last check's status
variable EXIT-CELL                   \ the label every check exits through
variable CASE-NEXT                   \ the status the next case check exits with
variable WRONG-AT                    \ the check a negative image fails, or 0

: EXIT-LBL ( -- label ) EXIT-CELL @ >LABEL ;

\ The image driver and the OS seam over package X64CODE's byte stream. They
\ load into this package because the x86-64 sys.f spells the host seam's
\ syscall-number words, which must not become globals here. The driver's
\ packaged dependencies, fdio.f and sign-id.f, and the seam's,
\ src/os/linux-flags.f, load at top level above, because a file required
\ inside a package cannot open one.
s" src/habu/driver-io.f" required
using DRIVER
s" src/os/linux-x86-64/sys.f" required
using LINUX-LAYOUT   \ the guard

: IMM ( r64 n -- ) >IMM64 ASM-SINK ENC-MOV-RI64 ;

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

\ exit_group ends every thread: a booted image's task thread that outlives a
\ thread-local exit(2) would end the process with its own status instead.
: EXIT, ( -- )
   EXIT-LBL LBL,
   0 >R32 NR-EXIT-GROUP >IMM32 ASM-SINK ENC-MOV32-RI32
   ASM-SINK ENC-SYSCALL ;

: TEXT-VA ( -- n ) LINUX-LAYOUT:IMAGE-VA LINUX-LAYOUT:CODE-OFF + ;

\ The engine's import names, each ending in a NUL: the kernel reaches its C
\ functions through their GOT slots, so every image binds all of them.
create IMPORT-BUF BUF:HDR-BYTES allot

: IMPORTS$ ( -- str ) IMPORT-BUF BUF:SPAN$ BUF:BLEN>N ;

: IMPORTS! ( -- )
   IMPORT-BUF 64 BUF:N>BLEN BUF:INIT
   ELF:IMPORTS 0 ?do
      i ELF:IMPORT$ BUF:N>BLEN IMPORT-BUF BUF:APPEND-SPAN
      0 IMPORT-BUF BUF:APPEND-BYTE
   loop ;

public

\ The address the next routine appended lands at, so the one it is emitted at.
: POSITION ( -- n ) TEXT-VA ASM-LEN + ;

\ Write the staged stream as it stands: byte 0 is the ELF entry.
: WRITE ( ^u8 n -- ) {: path:^ pathu:n :}
   TEXT-VA ASM-LINK
   CODE ASM-LEN TEXT-VA IMPORTS$ s" " ELF:BUILD-X64   \ no DATA, so an empty carrier
   s" x64-peer" ELF:SET-SIGID ELF:CODESIG2
   path pathu DRV-WRITE-IMAGE
   s" ELF names x86-64 and enters this fixture's code" T-LABEL
   $12 M-OFF M-LE32@ $FFFF and 62 T=
   $18 M-OFF M-LE32@ TEXT-VA T=
   $1C M-OFF M-LE32@ 0 T= ;

\ The sink every image is staged in and the import names, held across the
\ images of one run.
: INIT ( -- ) ASM-SINK CODE-CAP-BYTES BUF:N>BLEN BUF:INIT  IMPORTS! ;
: DISPOSE ( -- ) ASM-SINK BUF:DISPOSE  IMPORT-BUF BUF:DISPOSE ;
;using   \ LINUX-LAYOUT
;package
