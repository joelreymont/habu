\ A ticket AWAITed directly while its group still holds it: the group carries a
\ handle into the capture, which the capture cleanup refuses by name. Run as its
\ own process by test/aio-capture-sweep.f, because the refusal ends the process.
require src/lib/errors.f
require src/lib/num-types.f
require src/lib/image-lifecycle.f
require src/lib/aio.f

package AIO-CAPTURE-HELD-SUBJECT
private

TYPED-VARIABLE HELD-TICKET AIO:ticket
AIO:GROUP HELD
create POKE-BYTE $41 c,

: OUTCOME-DROP ( AIO:outcome -- )
   MATCH AIO:outcome
      ready OF drop ENDOF
      timed-out OF ENDOF
      cancelled OF ENDOF
      refused OF drop ENDOF
   ;MATCH ;

public

: RUN ( -- )
   AIO:START
   pipe {: r w rc :}
   rc 0 <> if E-AIO-SETUP throw then
   r >FD AIO:READABLE -1 >MS AIO:POLL dup HELD-TICKET ! HELD AIO:GROUP+
   w POKE-BYTE 1 write drop
   HELD-TICKET @ AIO:AWAIT OUTCOME-DROP
   r close-rc drop
   w close-rc drop
   AIO:STOP
   IMAGE-LIFECYCLE:PREPARE ;

;package

AIO-CAPTURE-HELD-SUBJECT:RUN
