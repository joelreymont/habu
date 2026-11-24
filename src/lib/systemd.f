split-str
\ systemd.f - tells systemd's service manager a service is ready, or that its
\ start or stop needs longer, by the sd_notify(3) protocol: the datagram
\ READY=1 or EXTEND_TIMEOUT_USEC=<usec> to the unix datagram socket
\ NOTIFY_SOCKET names, a leading @ naming an abstract socket. With the variable
\ unset or empty, or off Linux, nothing is sent; no other message is spoken.
\ STORAGE CLASS. None of its own; EXTEND_TIMEOUT_USEC's text is assembled in
\ the task's SB.
require src/lib/error.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/type/deftype.f
require src/os/env-base.f
require src/lib/net/udp.f

package SYSTEMD
public

DEFTYPE ERRNO

SUMTYPE status
   VARIANT ok ;VARIANT
   VARIANT failed errno ;VARIANT
;SUMTYPE

private

\ Linux's sun_path.
108 constant PATH-BYTES
22 constant EINVAL

: INVALID ( -- status )
   EINVAL >ERRNO SYSTEMD-STATUS:FAILED ;

\ libsystemd's bounds: at least two bytes, and room for a NUL after them
\ in sun_path.
: NAME? ( str -- bool )
   {: name:^ size:n :}
   size 2 <  size PATH-BYTES >=  or if false exit then
   name c@ $2F =  name c@ $40 =  or ;

: >STATUS ( UDP:status -- status )
   MATCH UDP:status
      ok OF SYSTEMD-STATUS:OK ENDOF
      failed OF UDP:ERRNO>N >ERRNO SYSTEMD-STATUS:FAILED ENDOF
   ;MATCH ;

\ The notice went or did not: a failed close changes neither.
: SENT ( UDP:socket str str -- status )
   {: socket:UDP:socket name:^ size:n msg:^ msgu:n :}
   socket name size msg msgu UDP:SEND >STATUS
   socket UDP:CLOSE drop ;

\ Off Linux no service manager speaks this protocol.
: SOCKET$ ( -- str )
   HB-TARGET-LINUX-KERNEL? 0= if s" " exit then
   s" NOTIFY_SOCKET" GETENV ;

: NOTIFY ( str -- status )
   {: msg:^ msgu:n :}
   SOCKET$ {: name:^ size:n :}
   size 0= if SYSTEMD-STATUS:OK exit then
   name size NAME? 0= if INVALID exit then
   UDP:OPEN MATCH UDP:open-result
      opened OF name size msg msgu SENT ENDOF
      failed OF UDP:ERRNO>N >ERRNO SYSTEMD-STATUS:FAILED ENDOF
   ;MATCH ;

public

\ Once the service serves.
: NOTIFY-READY ( -- status )
   s" READY=1" NOTIFY ;

\ systemd's timeout for the current start, run or stop ends no sooner than ms
\ from now. A negative ms, or one past a cell in microseconds, answers EINVAL.
: NOTIFY-EXTEND-TIMEOUT ( ms -- status )
   MS>N {: t:n :}
   t 0<  t STR:MAX-I64 1000 / >  or if INVALID exit then
   SB-RESET s" EXTEND_TIMEOUT_USEC=" SB-APPEND  t 1000 * FMT:SB-INT
   SB$ NOTIFY ;

;package
