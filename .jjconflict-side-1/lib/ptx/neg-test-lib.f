\ neg-test-lib.f - in-process PTX negative checker assertions.

require lib/ptx/test-prelude.f

$10000 constant PTXN-BUF-CAP

variable PTXN-SRC-A
variable PTXN-SRC-U
variable PTXN-RC
variable PTXN-VERD

create PTXN-DIAG PTXN-BUF-CAP allot

: PTXN-SRC-A-FIELD ( -- ptr ptr u8 )
   PTXN-SRC-A 0 ptr-field ;

: PTXN-SRC-A@ ( -- ptr u8 )
   PTXN-SRC-A-FIELD @ ;

: PTXN-SRC-A! ( ptr u8 -- )
   PTXN-SRC-A-FIELD ! ;

: PTXN-SRC! ( ptr u8 n -- ) {: a:ptr u:n :}
   a PTXN-SRC-A!
   u PTXN-SRC-U ! ;

: PTXN-SRC$ ( -- ptr u8 n )
   PTXN-SRC-A@ PTXN-SRC-U @ ;

: PTXN-CHECK-ACT ( -- )
   PTXN-SRC$ CHECK-CANDIDATE! PTXN-VERD ! ;

: PTXN-CHECK ( ptr u8 n -- n n n ) {: src:ptr srcu:n :}
   src srcu PTXN-SRC!
   -1 PTXN-VERD !
   PTXN-DIAG PTXN-BUF-CAP DIAG-BUFFER!
   [: PTXN-CHECK-ACT ;] catch PTXN-RC !
   DIAG-BUFFER$ nip
   DIAG-BUFFER-OFF
   PTXN-VERD @ PTXN-RC @ ;

: PTXN-REJECTS ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n needle:ptr needleu:n label:ptr labelu:n :}
   src srcu PTXN-CHECK {: diag:n verdict:n rc:n :}
   label labelu T-LABEL
   rc 0 T=
   label labelu T-LABEL
   verdict 0 T=
   label labelu T-LABEL
   PTXN-DIAG diag needle needleu CONTAINS? TTRUE ;
