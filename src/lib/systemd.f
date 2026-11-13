\ systemd.f - tells systemd's service manager a service is ready, by the
\ sd_notify(3) protocol: the datagram READY=1 to the unix datagram socket
\ NOTIFY_SOCKET names, a leading @ naming an abstract socket. With the variable
\ unset or empty, or off Linux, nothing is sent; no other message is spoken.
\ STORAGE CLASS. TASK-LOCAL: the socket address is one TASK:+USERS row.
require src/lib/error.f
require src/lib/ffi-abi.f
require src/lib/type/deftype.f
require src/lib/task.f
require src/lib/le.f
require src/os/env-base.f

package SYSTEMD
public

DEFTYPE ERRNO

SUMTYPE status
   VARIANT ok ;VARIANT
   VARIANT failed errno ;VARIANT
;SUMTYPE

private

\ Linux sockaddr_un: a u16 sun_family, then sun_path.
110 constant SOCKADDR-BYTES
108 constant PATH-BYTES
1 constant AF-UNIX
\ SOCK_DGRAM | SOCK_CLOEXEC
$80002 constant SOCKET-FLAGS
$4000 constant MSG-NOSIGNAL
22 constant EINVAL

SOCKADDR-BYTES TASK:+USERS ADDRESS-ROW u8
: SOCKADDR ( -- ptr u8 ) 0 ADDRESS-ROW ;
: SUN-PATH ( -- ptr u8 ) SOCKADDR 2 + ;

PROCESS-SYMBOLS

FUNCTION: SOCKET-CALL socket ( n n n -- i32 ) ;FUNCTION
FUNCTION: CLOSE-CALL close ( n -- i32 ) ;FUNCTION
FUNCTION: SEND-CALL sendto ( n ptr u8 n n ptr u8 n -- n ) ;FUNCTION

: LAST-ERROR ( -- errno )
   FFI:ERRNO >ERRNO ;

: INVALID ( -- status )
   EINVAL >ERRNO SYSTEMD-STATUS:failed ;

\ libsystemd's bounds: at least two bytes, and room for a NUL after them
\ in sun_path.
: NAME? ( ptr u8 n -- bool )
   {: name:ptr size:n :}
   size 2 <  size PATH-BYTES >=  or if false exit then
   name c@ $2F =  name c@ $40 =  or ;

\ Answers the address length. A path ends in a NUL, which the length counts;
\ an abstract name's @ becomes the NUL that marks it.
: ADDRESS! ( ptr u8 n -- n )
   {: name:ptr size:n :}
   AF-UNIX SOCKADDR LE:U16!
   name SUN-PATH size BYTE-COPY
   name c@ $40 = if 0 SUN-PATH c! size 2 + exit then
   0 SUN-PATH size + c! size 3 + ;

\ Reads errno before the caller's close can change it.
: SEND ( n n -- status )
   {: fd:n size:n :}
   fd s" READY=1" MSG-NOSIGNAL SOCKADDR size SEND-CALL
   0 < if LAST-ERROR SYSTEMD-STATUS:failed exit then
   SYSTEMD-STATUS:ok ;

: NOTIFY ( n -- status )
   {: size:n :}
   AF-UNIX SOCKET-FLAGS 0 SOCKET-CALL {: fd:n :}
   fd 0 < if LAST-ERROR SYSTEMD-STATUS:failed exit then
   fd size SEND fd CLOSE-CALL drop ;

public

\ Once the service serves. Off Linux no service manager speaks this protocol.
: NOTIFY-READY ( -- status )
   HB-TARGET-LINUX-KERNEL? 0= if SYSTEMD-STATUS:ok exit then
   s" NOTIFY_SOCKET" GETENV {: name:ptr size:n :}
   size 0= if SYSTEMD-STATUS:ok exit then
   name size NAME? 0= if INVALID exit then
   name size ADDRESS! NOTIFY ;

;package
