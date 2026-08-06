\ runtime-subject.f - exact-candidate resident source capture and parity.

require lib/test/subject.f

package RUNTIME-SUBJECT

using GATE
using GATE-COMMON

64 constant DIGEST-U

create SRC GE-SRC-CAP allot
create DIRECT-OUT GT-OUT-CAP allot
create DIRECT-ERR GT-ERR-CAP allot
create SRC-DIGEST 32 allot
create SRC-HEX DIGEST-U allot
create OUT-DIGEST 32 allot
create OUT-HEX DIGEST-U allot
create ERR-DIGEST 32 allot
create ERR-HEX DIGEST-U allot

variable SRC-U
variable DIRECT-OUT-U
variable DIRECT-ERR-U
variable DIRECT-EXITED
variable DIRECT-TIMED
variable DIRECT-CODE
variable RUN-ID

: SOURCE! ( ptr u8 n -- ) {: src:ptr srcu:n :}
   srcu GE-SRC-CAP > if E-STR-CAPACITY throw then
   src SRC srcu BYTE-COPY
   srcu SRC-U ! ;

: CAPTURE ( ptr u8 n -- ) {: src:ptr srcu:n :}
   src srcu
   GT-OUT-SINK
   GT-ERR-SINK
   GE-TIMEOUT-MS >MS SUBJECT:RUN
   GT-STORE-RUN ;

\ The runner's outcome, spread into the three values this file compares and
\ logs.  It is read from GT-OUTCOME@ rather than GT-RC@ because the parity
\ check has to see a capture timeout and a SIGKILL death as different runs, and
\ rc flattens both to 137.
: OUTCOME-CELLS ( -- bool bool n )
   GT-OUTCOME@ MATCH outcome
     exited OF 0 0= 0 0= 0= rot ENDOF
     signaled OF 0 0= 0= 0 0= 0= rot ENDOF
     timeout OF 0 0= 0= 0 0= 0 ENDOF
   ;MATCH ;

: SAVE-DIRECT ( -- )
   GT-OUT$ DIRECT-OUT swap dup DIRECT-OUT-U ! BYTE-COPY
   GT-ERR$ DIRECT-ERR swap dup DIRECT-ERR-U ! BYTE-COPY
   OUTCOME-CELLS
   DIRECT-CODE !
   DIRECT-TIMED !
   DIRECT-EXITED ! ;

: CHECK-SOURCE ( ptr u8 n -- ) {: src:ptr srcu:n :}
   SRC SRC-U @ src srcu STR= 0= if
      s" runtime parity source mutation" GE-FAIL
   then ;

\ A live flag against the cell SAVE-DIRECT wrote for the direct run.
: FLAG-AGREES? ( bool n -- bool ) {: live:bool saved:n :}
   live if saved 0<> exit then
   saved 0= ;

: CHECK-OUTCOME ( bool bool n -- ) {: exited:bool timed:bool code:n :}
   exited DIRECT-EXITED @ FLAG-AGREES? 0= if s" runtime parity outcome" GE-FAIL then
   timed DIRECT-TIMED @ FLAG-AGREES? 0= if s" runtime parity outcome" GE-FAIL then
   code DIRECT-CODE @ <> if s" runtime parity outcome" GE-FAIL then ;

: CHECK-DIRECT ( ptr u8 n -- )
   CHECK-SOURCE
   OUTCOME-CELLS CHECK-OUTCOME
   GT-OUT$ DIRECT-OUT DIRECT-OUT-U @ STR= 0= if s" runtime parity stdout" GE-FAIL then
   GT-ERR$ DIRECT-ERR DIRECT-ERR-U @ STR= 0= if s" runtime parity stderr" GE-FAIL then ;

: DIGESTS! ( ptr u8 n -- ) {: src:ptr srcu:n :}
   src srcu SRC-DIGEST SHA256
   SRC-DIGEST SRC-HEX SHA256>HEX
   GT-OUT$ OUT-DIGEST SHA256
   OUT-DIGEST OUT-HEX SHA256>HEX
   GT-ERR$ ERR-DIGEST SHA256
   ERR-DIGEST ERR-HEX SHA256>HEX ;

: LOG ( ptr u8 n bool -- ) {: src:ptr srcu:n parity:bool :}
   src srcu DIGESTS!
   RUN-ID @ 1+ dup RUN-ID !
   parity SRC-HEX DIGEST-U
   OUTCOME-CELLS
   GT-OUT$ nip OUT-HEX DIGEST-U
   GT-ERR$ nip ERR-HEX DIGEST-U
   GS-RUNTIME-SUBJECT ;

public

: RESET ( -- )
   0 RUN-ID ! ;

: RUN ( ptr u8 n -- ) {: src:ptr srcu:n :}
   src srcu CAPTURE
   src srcu 0 0= 0= LOG ;

: PARITY ( ptr u8 n -- ) {: src:ptr srcu:n :}
   src srcu SOURCE!
   GE-HB-RESET
   SRC SRC-U @ RUNTIME-DIRECT:PARITY
   SAVE-DIRECT
   SRC SRC-U @ CAPTURE
   src srcu CHECK-DIRECT
   SRC SRC-U @ 0 0= LOG ;

;package
