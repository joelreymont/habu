\ Grown aio records and a grown group row across a capture's cleanup. Run as
\ its own process by test/aio-capture-sweep.f, because IMAGE-LIFECYCLE:PREPARE
\ runs every capture cleanup this process registered. The cleanup admits a
\ capture with nothing in flight and the group empty, and the loop starts
\ again after it: the second round grows the records and the group row anew
\ and fills them, and a ticket kept from a grown record of the first round is
\ refused. Whether the chunks went back to the system is not observed here.
require src/lib/errors.f
require src/lib/test.f
require src/lib/num-types.f
require src/lib/image-lifecycle.f
require src/lib/aio.f

package AIO-CAPTURE-SWEEP-SUBJECT
private

$50 constant OPS-N                  \ past the first record chunk and group row, $40 each

OPS-N TYPED-BUFFER TICKETS AIO:ticket
TYPED-VARIABLE KEPT AIO:ticket
AIO:GROUP SWEPT
variable P-R
variable P-W
variable READY-N
create POKE-BYTE $41 c,

: READY? ( AIO:outcome -- bool )
   MATCH AIO:outcome
      ready OF drop true ENDOF
      timed-out OF false ENDOF
      cancelled OF false ENDOF
      refused OF drop false ENDOF
   ;MATCH ;

: PIPE-OPEN ( -- )
   pipe {: r w rc :}
   rc 0 <> if E-AIO-SETUP throw then
   r P-R ! w P-W ! ;

: PIPE-CLOSE ( -- )
   P-R @ close-rc drop
   P-W @ close-rc drop ;

\ One byte makes every poll on the pipe ready.
: POKE ( -- )
   P-W @ POKE-BYTE 1 write drop ;

\ OPS-N polls on the pipe, all in flight at once and all in the group.
: FILL ( -- )
   OPS-N 0 ?do
      P-R @ >FD AIO:READABLE -1 >MS AIO:POLL dup i TICKETS ! SWEPT AIO:GROUP+
   loop
   SWEPT AIO:GROUP-COUNT OPS-N T= ;

: DRAIN ( -- )
   0 READY-N !
   OPS-N 0 ?do
      SWEPT AIO:AWAIT-ANY READY? if 1 READY-N +! then drop
   loop
   READY-N @ OPS-N T=
   SWEPT AIO:GROUP-COUNT 0 T= ;

public

: RUN ( -- )
   T-RESET
   AIO:START
   PIPE-OPEN FILL
   OPS-N 1 - TICKETS @ KEPT !
   POKE DRAIN PIPE-CLOSE
   AIO:STOP
   IMAGE-LIFECYCLE:PREPARE
   AIO:START
   PIPE-OPEN FILL POKE
   [: KEPT @ AIO:AWAIT READY? drop ;] E-AIO-STATE TTHROWSQ
   DRAIN PIPE-CLOSE
   AIO:STOP
   T-REPORT ;

;package

AIO-CAPTURE-SWEEP-SUBJECT:RUN
