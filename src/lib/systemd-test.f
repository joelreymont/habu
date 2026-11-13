\ systemd-test.f - SYSTEMD:NOTIFY-READY against unix datagram sockets this
\ test binds.
HB-TARGET-LINUX? ?TARGET

require src/lib/string.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/fmt.f
require src/lib/codegen.f
require src/lib/ffi-abi.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f

package SYSTEMD-TEST
private

$1000 constant IO-CAP
$40 constant GOT-CAP
110 constant SOCKADDR-BYTES
1 constant AF-UNIX
\ SOCK_DGRAM | SOCK_CLOEXEC
$80002 constant SOCKET-FLAGS
$40 constant MSG-DONTWAIT
2 constant ENOENT
22 constant EINVAL

$400 CODEGEN:BUFFER PROG
create SOCKETS PATH-CAP allot   variable SOCKETS-U
create BOUND PATH-CAP allot   variable BOUND-U
create MISSING PATH-CAP allot   variable MISSING-U
create ABSTRACT PATH-CAP allot   variable ABSTRACT-U
create ADDR SOCKADDR-BYTES allot
create OUT IO-CAP allot
create ERR IO-CAP allot
create GOT GOT-CAP allot
variable BOUND-FD
variable ABSTRACT-FD

: SOCKETS$ ( -- ptr u8 n ) SOCKETS SOCKETS-U @ ;
: BOUND$ ( -- ptr u8 n ) BOUND BOUND-U @ ;
: MISSING$ ( -- ptr u8 n ) MISSING MISSING-U @ ;
: ABSTRACT$ ( -- ptr u8 n ) ABSTRACT ABSTRACT-U @ ;

PROCESS-SYMBOLS

FUNCTION: SOCKET-CALL socket ( n n n -- i32 ) ;FUNCTION
FUNCTION: BIND-CALL bind ( n ptr u8 n -- i32 ) ;FUNCTION
FUNCTION: CLOSE-CALL close ( n -- i32 ) ;FUNCTION
FUNCTION: RECV-CALL recv ( n ptr u8 n n -- n )
   1 2 WRITES-ARG                         \ the datagram buffer
;FUNCTION

: P+ ( ptr u8 n -- )
   PROG CODEGEN:APPEND-STRING
   10 PROG CODEGEN:APPEND-BYTE ;

: CHILD-SOURCE ( -- )
   PROG CODEGEN:RESET
   s" require src/lib/systemd.f" P+
   s" require src/lib/fmt.f" P+
   s" : GO ( -- )" P+
   s"    SYSTEMD:NOTIFY-READY MATCH SYSTEMD:status" P+
   s"       ok OF 0 ENDOF" P+
   s"       failed OF SYSTEMD:ERRNO>N ENDOF" P+
   s"    ;MATCH FMT:INT$ type cr ;" P+
   s" GO" P+ ;

\ The address NOTIFY_SOCKET's value names; answers its length.
: ADDRESS! ( ptr u8 n -- n )
   {: name:ptr size:n :}
   SOCKADDR-BYTES 0 do 0 ADDR i + c! loop
   AF-UNIX ADDR c!
   name ADDR 2 + size BYTE-COPY
   name c@ $40 = if 0 ADDR 2 + c! size 2 + exit then
   size 3 + ;

: BIND-AT ( ptr u8 n -- n )
   {: name:ptr size:n :}
   AF-UNIX SOCKET-FLAGS 0 SOCKET-CALL {: fd:n :}
   fd 0 < if s" systemd-test: socket failed" T-EX-FAIL die then
   fd ADDR name size ADDRESS! BIND-CALL 0 < if
      s" systemd-test: bind failed" T-EX-FAIL die
   then fd ;

: NAMED ( ptr u8 n ptr u8 -- n )
   {: prefix:ptr prefixu:n dst:ptr :}
   SB-RESET prefix prefixu SB-APPEND getpid FMT:SB-INT
   SB$ {: a:ptr u:n :} a dst u BYTE-COPY u ;

: SOCKETS! ( ptr u8 n -- )
   {: a:ptr u:n :}
   u PATH-CAP > if E-FS-PATH throw then
   a SOCKETS u BYTE-COPY  u SOCKETS-U ! ;

: SETUP ( -- )
   FS:CLEANUP-RESET
   s" HB_SOCK_TMP" GETENV dup 0 > if
      SOCKETS!
   else
      2drop s" habu-sd" FS:MKTEMP-DIR SOCKETS!
      SOCKETS$ FS:CLEANUP-TREE+
   then
   s" habu-sd-" GOT NAMED {: u:n :}
   SOCKETS$ GOT u BOUND FS:JOIN-PATH BOUND-U !
   s" habu-sd-missing-" GOT NAMED {: v:n :}
   SOCKETS$ GOT v MISSING FS:JOIN-PATH MISSING-U !
   s" @habu-sd-" ABSTRACT NAMED ABSTRACT-U !
   BOUND$ BIND-AT BOUND-FD !
   ABSTRACT$ BIND-AT ABSTRACT-FD !
   CHILD-SOURCE ;

\ A child engine per case: GETENV reads the environment the process started with.
: RUN ( ptr u8 n -- n n )   \ outu rc
   {: value:ptr valueu:n :}
   ENGINE-CANDIDATE:PATH$ {: engine:ptr engineu:n :}
   PROG CODEGEN:CONTENTS {: prog:ptr progu:n :}
   PROC-ARGV-ENV-RESET
   s" NOTIFY_SOCKET" >LEN value valueu >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   engine engineu >LEN prog progu >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN PROCESS:WAIT-FOREVER
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROCESS:ENDING>RC RC>N
   {: outu:len erru:len rc:n :}
   rc 0 <> if ERR erru LEN>N type cr then
   outu LEN>N rc ;

: WANT$ ( n -- ptr u8 n )
   SB-RESET FMT:SB-INT 10 SB-APPEND-C SB$ ;

: ANSWERS ( ptr u8 n n -- )
   {: value:ptr valueu:n want:n :}
   value valueu RUN {: outu:n rc:n :}
   OUT outu want WANT$ T$=
   s" ... and the child engine exits cleanly" T-LABEL
   rc 0 T= ;

\ A unix datagram is queued before sendto returns, so nothing waits.
: RECEIVED$ ( n -- ptr u8 n )
   {: fd:n :}
   fd GOT GOT-CAP MSG-DONTWAIT RECV-CALL {: got:n :}
   GOT got 0 max ;

: PATH-CASE ( -- )
   s" a bound path socket: the child answers ok" T-LABEL
   BOUND$ 0 ANSWERS
   s" ... and the socket holds READY=1" T-LABEL
   BOUND-FD @ RECEIVED$ s" READY=1" T$= ;

: ABSTRACT-CASE ( -- )
   s" a bound abstract socket: the child answers ok" T-LABEL
   ABSTRACT$ 0 ANSWERS
   s" ... and the socket holds READY=1" T-LABEL
   ABSTRACT-FD @ RECEIVED$ s" READY=1" T$= ;

: EMPTY-CASE ( -- )
   s" an empty NOTIFY_SOCKET answers ok" T-LABEL
   s" " 0 ANSWERS ;

: RELATIVE-CASE ( -- )
   s" a value neither / nor @ starts answers EINVAL" T-LABEL
   s" notify" EINVAL ANSWERS ;

: ROOT-CASE ( -- )
   s" a lone / answers EINVAL" T-LABEL
   s" /" EINVAL ANSWERS ;

: MISSING-CASE ( -- )
   s" a path nothing is bound to answers ENOENT" T-LABEL
   MISSING$ ENOENT ANSWERS ;

: TEARDOWN ( -- )
   BOUND-FD @ CLOSE-CALL drop
   ABSTRACT-FD @ CLOSE-CALL drop
   BOUND$ FS:REMOVE-FILE
   FS:CLEANUP-RUN ;

public

: SYSTEMD-TEST-MAIN ( -- )
   T-RESET
   SETUP
   PATH-CASE
   ABSTRACT-CASE
   EMPTY-CASE
   RELATIVE-CASE
   ROOT-CASE
   MISSING-CASE
   TEARDOWN
   T-REPORT
   s" systemd: ok" type cr ;

;package

SYSTEMD-TEST:SYSTEMD-TEST-MAIN
