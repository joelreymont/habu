\ udp.f - unix-domain datagram sockets named by a path or, on Linux, an
\ abstract name written @name. Linux and macOS. CLOSE does not remove a
\ path's socket file.
\ A name sun_path cannot hold throws E-SPAN-CAPACITY.
\ STORAGE CLASS. TASK-LOCAL: the socket address is one TASK:+USERS row; the
\ data buffers are caller-owned.
require src/lib/ffi-abi.f
require src/lib/span.f
require src/lib/type/deftype.f
require src/lib/num-types.f
require src/lib/task.f
require src/lib/le.f
require src/lib/net/datagram.f

package UDP
public

DEFTYPE SOCKET
DEFTYPE ERRNO

SUMTYPE open-result
   VARIANT opened socket ;VARIANT
   VARIANT failed errno ;VARIANT
;SUMTYPE

SUMTYPE status
   VARIANT ok ;VARIANT
   VARIANT failed errno ;VARIANT
;SUMTYPE

SUMTYPE receive-result
   VARIANT packet NUM:byte-len ;VARIANT
   VARIANT truncated NUM:byte-len ;VARIANT
   VARIANT timeout ;VARIANT
   VARIANT failed errno ;VARIANT
;SUMTYPE

private

1 constant AF-UNIX
$40 constant AT-SIGN


HB-TARGET-LINUX-KERNEL? [if]
\ sockaddr_un: a u16 sun_family, then sun_path.
110 constant SOCKADDR-BYTES

: HEADER! ( n ^u8 -- )
   nip AF-UNIX swap LE:U16! ;
[then]


HB-TARGET-MACOS? [if]
\ sockaddr_un: a u8 sun_len, a u8 sun_family, then sun_path.
106 constant SOCKADDR-BYTES

: HEADER! ( n ^u8 -- ) {: length:n row :}
   length row c! AF-UNIX row 1+ c! ;
[then]


SOCKADDR-BYTES TASK:+USERS ADDRESS-ROW u8
: SOCKADDR ( -- ^u8 ) 0 ADDRESS-ROW ;


\ sun_path from byte skip on.
: PATH-SPAN ( n -- SPAN:span<u8> ) {: skip:n :}
   SOCKADDR 2 + skip + SOCKADDR-BYTES 2 - skip - SPAN:MAKE ;


\ A C string; the length counts its NUL.
: PATH! ( ^u8 n -- n ) {: name size:n :}
   name size 0 PATH-SPAN FFI:CSTR drop size 3 + ;


\ A NUL, then the name after its @, and no NUL that the length counts: Linux
\ reads an abstract name, macOS answers ENOENT.
: ABSTRACT! ( ^u8 n -- n ) {: name size:n :}
   name 1+ size 1- 1 PATH-SPAN SPAN:COPY
   0 SOCKADDR 2 + c! size 2 + ;


: NAME! ( ^u8 n -- n ) {: name size:n :}
   size 0= if 2 exit then
   name c@ AT-SIGN = if name size ABSTRACT! exit then
   name size PATH! ;


\ Answers the address length; the empty name is the header alone.
: ADDRESS! ( ^u8 n -- n )
   NAME! dup SOCKADDR HEADER! ;


: >OPEN-RESULT ( DATAGRAM:open-result -- open-result )
   MATCH DATAGRAM:open-result
      opened OF >SOCKET UDP-OPEN--RESULT:OPENED ENDOF
      failed OF DATAGRAM:ERRNO>N >ERRNO UDP-OPEN--RESULT:FAILED ENDOF
   ;MATCH ;


: >STATUS ( DATAGRAM:status -- status )
   MATCH DATAGRAM:status
      ok OF UDP-STATUS:OK ENDOF
      failed OF DATAGRAM:ERRNO>N >ERRNO UDP-STATUS:FAILED ENDOF
   ;MATCH ;


: >RECEIVE-RESULT ( DATAGRAM:receive-result -- receive-result )
   MATCH DATAGRAM:receive-result
      packet OF UDP-RECEIVE--RESULT:PACKET ENDOF
      truncated OF UDP-RECEIVE--RESULT:TRUNCATED ENDOF
      timeout OF UDP-RECEIVE--RESULT:TIMEOUT ENDOF
      failed OF DATAGRAM:ERRNO>N >ERRNO UDP-RECEIVE--RESULT:FAILED ENDOF
   ;MATCH ;


public


\ An unbound socket for sending; the caller closes it exactly once.
: OPEN ( -- open-result )
   AF-UNIX DATAGRAM:OPEN >OPEN-RESULT ;


\ A socket bound at the name; the caller closes it exactly once. A path
\ another socket file holds answers `failed` EADDRINUSE.
: BIND ( ^u8 n -- open-result )
   ADDRESS! {: length:n :}
   AF-UNIX SOCKADDR length DATAGRAM:BIND >OPEN-RESULT ;


\ Sends the payload to the name. On Linux it waits while the receiver's
\ queue is full; macOS answers `failed` ENOBUFS.
: SEND ( socket ^u8 n ^u8 NUM:byte-len -- status )
   {: socket:socket name size:n bytes length:NUM:byte-len :}
   name size ADDRESS! {: addrlen:n :}
   socket SOCKET>N SOCKADDR addrlen bytes length DATAGRAM:SEND >STATUS ;


\ `truncated` carries the datagram's own length. Timeout zero makes one try, a
\ negative one waits with no deadline; a wait needs AIO:START to have run.
: RECEIVE ( socket ^u8 NUM:byte-len ms -- receive-result )
   {: socket:socket bytes capacity:NUM:byte-len timeout:ms :}
   socket SOCKET>N bytes capacity timeout NULL-PTR NULL-PTR
   DATAGRAM:RECEIVE >RECEIVE-RESULT ;


: CLOSE ( socket -- status )
   SOCKET>N DATAGRAM:CLOSE >STATUS ;


;package
