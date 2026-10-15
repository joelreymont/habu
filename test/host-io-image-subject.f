\ The subject test/host-io-image.f saves and restores: a UDP worker and a
\ serial worker, each a task, exchanging one byte with the row's peers per RUN.
\ Both transports wait on the AIO loop, so RUN starts it before the workers and
\ stops it after they end: a saved image holds no live task, and the next RUN's
\ START needs the loop down.
require src/lib/task.f
require src/lib/aio.f
require src/lib/net/udp4.f
require src/lib/serial.f

package HOST-IO-IMAGE-SUBJECT
private

CAST: BLEN>N ( NUM:byte-len -- n )
variable ENTERED
variable DONE
variable PATH-SIZE
variable PEER-PORT
create PATH 64 allot
create UDP-BYTE 1 allot
create SERIAL-BYTE 1 allot
TASK:MIN-STACK TASK:TASK UDP-WORKER
TASK:MIN-STACK TASK:TASK SERIAL-WORKER

: GATE ( -- )
   1 ENTERED atomic-add drop
   begin ENTERED atomic@ 2 < while TASK:PAUSE repeat ;


: UDP-OK ( UDP4:status -- )
   MATCH UDP4:status
      ok OF ENDOF failed OF UDP4:ERRNO>N throw ENDOF
   ;MATCH ;


: UDP-RUN ( -- )
   GATE
   $7F000001 UDP4:ADDRESS 0 UDP4:PORT UDP4:BIND MATCH UDP4:open-result
      opened OF ENDOF failed OF UDP4:ERRNO>N throw ENDOF
   ;MATCH {: handle:UDP4:socket :}
   $19 UDP-BYTE c!
   handle $7F000001 UDP4:ADDRESS PEER-PORT @ UDP4:PORT
   UDP-BYTE 1 UDP4:PAYLOAD-BYTES UDP4:SEND UDP-OK
   handle UDP-BYTE 1 UDP4:PAYLOAD-BYTES 2000 >MS UDP4:RECEIVE
   MATCH UDP4:receive-result
      packet OF
         {: size:NUM:byte-len address:UDP4:address port:UDP4:port :}
         size BLEN>N 1 <> if -1 throw then
         address UDP4:ADDRESS>N $7F000001 <> if -1 throw then
         port UDP4:PORT>N PEER-PORT @ <> if -1 throw then
         UDP-BYTE c@ $E6 <> if -1 throw then
      ENDOF
      truncated OF drop drop drop -1 throw ENDOF
      timeout OF -1 throw ENDOF
      failed OF UDP4:ERRNO>N throw ENDOF
   ;MATCH
   handle UDP4:CLOSE UDP-OK
   1 DONE atomic-add drop ;


: SERIAL-ONE ( SERIAL:io-result -- )
   MATCH SERIAL:io-result
      transferred OF BLEN>N 1 <> if -1 throw then ENDOF
      timeout OF -1 throw ENDOF closed OF -1 throw ENDOF
      failed OF SERIAL:ERRNO>N throw ENDOF
   ;MATCH ;


: SERIAL-RUN ( -- )
   GATE
   PATH PATH-SIZE @ 115200 SERIAL:BAUD SERIAL:OPEN8N1 MATCH SERIAL:open-result
      opened OF ENDOF failed OF SERIAL:ERRNO>N throw ENDOF
      unsupported OF -1 throw ENDOF
   ;MATCH {: handle:SERIAL:handle :}
   $A5 SERIAL-BYTE c!
   handle SERIAL-BYTE 1 SERIAL:BYTES 2000 >MS SERIAL:WRITE SERIAL-ONE
   handle SERIAL-BYTE 1 SERIAL:BYTES 2000 >MS SERIAL:READ SERIAL-ONE
   SERIAL-BYTE c@ $5A <> if -1 throw then
   handle SERIAL:CLOSE MATCH SERIAL:status
      ok OF ENDOF failed OF SERIAL:ERRNO>N throw ENDOF
   ;MATCH
   1 DONE atomic-add drop ;


: WAIT-DONE ( ptr n -- ) {: worker:ptr :}
   begin worker TASK:DONE? 0= while TASK:PAUSE repeat ;

public


: RUN ( ptr u8 n UDP4:port -- ) {: text size:n port:UDP4:port :}
   size 1 < size 64 > or if -1 throw then
   text PATH size BYTE-COPY size PATH-SIZE ! port UDP4:PORT>N PEER-PORT !
   0 ENTERED atomic! 0 DONE atomic!
   AIO:START
   ['] UDP-RUN UDP-WORKER TASK:ACTIVATE
   ['] SERIAL-RUN SERIAL-WORKER TASK:ACTIVATE
   UDP-WORKER WAIT-DONE SERIAL-WORKER WAIT-DONE
   UDP-WORKER TASK:KILL SERIAL-WORKER TASK:KILL
   AIO:STOP
   DONE atomic@ 2 <> if -1 throw then
   s" io=ok" type cr ;

;package
