\ systemd.f - tells systemd's service manager a service is ready, by the
\ sd_notify(3) protocol: the datagram READY=1 to the unix datagram socket
\ NOTIFY_SOCKET names, a leading @ naming an abstract socket. With the variable
\ unset or empty, or off Linux, nothing is sent; no other message is spoken.
\ STORAGE CLASS. None of its own.
require src/lib/error.f
require src/lib/type/deftype.f
require src/lib/string-roles.f
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
: NAME? ( ^u8 n -- bool )
   {: name:^ size:n :}
   size 2 <  size PATH-BYTES >=  or if false exit then
   name c@ $2F =  name c@ $40 =  or ;

: >STATUS ( UDP:status -- status )
   MATCH UDP:status
      ok OF SYSTEMD-STATUS:OK ENDOF
      failed OF UDP:ERRNO>N >ERRNO SYSTEMD-STATUS:FAILED ENDOF
   ;MATCH ;

\ The notice went or did not: a failed close changes neither.
: SENT ( UDP:socket ^u8 n -- status )
   {: socket:UDP:socket name:^ size:n :}
   socket name size s" READY=1" STR:LENGTH UDP:SEND >STATUS
   socket UDP:CLOSE drop ;

: NOTIFY ( ^u8 n -- status )
   {: name:^ size:n :}
   UDP:OPEN MATCH UDP:open-result
      opened OF name size SENT ENDOF
      failed OF UDP:ERRNO>N >ERRNO SYSTEMD-STATUS:FAILED ENDOF
   ;MATCH ;

public

\ Once the service serves. Off Linux no service manager speaks this protocol.
: NOTIFY-READY ( -- status )
   HB-TARGET-LINUX-KERNEL? 0= if SYSTEMD-STATUS:OK exit then
   s" NOTIFY_SOCKET" GETENV {: name:^ size:n :}
   size 0= if SYSTEMD-STATUS:OK exit then
   name size NAME? 0= if INVALID exit then
   name size NOTIFY ;

;package
