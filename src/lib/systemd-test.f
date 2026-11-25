\ systemd-test.f - SYSTEMD:NOTIFY-READY and SYSTEMD:NOTIFY-EXTEND-TIMEOUT
\ against unix datagram sockets this test binds.
HB-TARGET-LINUX? ?TARGET

require src/lib/string.f
require src/lib/test/assert.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/fmt.f
require src/lib/codegen.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require src/lib/num-types.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/net/udp.f
require src/core/exit-status.f
require src/lib/systemd.f

package SYSTEMD-TEST
private

$1000 constant IO-CAP
$40 constant GOT-CAP
2 constant ENOENT
22 constant EINVAL

: CAP-LEN ( n -- NUM:byte-len )
   NUM:BYTE-LEN MATCH option
      none OF
         s" systemd-test: negative capacity" EXIT-STATUS:FAILURE die
      ENDOF
      some OF ENDOF
   ;MATCH ;

CODEGEN:BUFFER PROG
create SOCKETS PATH-CAP allot   variable SOCKETS-U
create BOUND PATH-CAP allot   variable BOUND-U
create MISSING PATH-CAP allot   variable MISSING-U
create ABSTRACT PATH-CAP allot   variable ABSTRACT-U
create OUT IO-CAP allot
create ERR IO-CAP allot
create GOT GOT-CAP allot
variable BOUND-FD
variable ABSTRACT-FD

: SOCKETS$ ( -- str ) SOCKETS SOCKETS-U @ >str ;
: BOUND$ ( -- str ) BOUND BOUND-U @ >str ;
: MISSING$ ( -- str ) MISSING MISSING-U @ >str ;
: ABSTRACT$ ( -- str ) ABSTRACT ABSTRACT-U @ >str ;

: P+ ( str -- )
   PROG CODEGEN:APPEND-STRING
   10 PROG CODEGEN:APPEND-BYTE ;

: CHILD-SOURCE ( str -- )   \ the call that answers a SYSTEMD:status
   {: call:str :}
   PROG CODEGEN:RESET
   s" require src/lib/systemd.f" P+
   s" require src/lib/fmt.f" P+
   s" : GO ( -- )" P+
   call P+
   s"    MATCH SYSTEMD:status" P+
   s"       ok OF 0 ENDOF" P+
   s"       failed OF SYSTEMD:ERRNO>N ENDOF" P+
   s"    ;MATCH FMT:INT$ type cr ;" P+
   s" GO" P+ ;

: BIND-AT ( str -- n )
   UDP:BIND MATCH UDP:open-result
      opened OF UDP:SOCKET>N ENDOF
      failed OF
         drop s" systemd-test: bind failed" EXIT-STATUS:FAILURE die
      ENDOF
   ;MATCH ;

: NAMED ( str ^u8 -- n )
   {: prefix:str dst:^ :}
   SB-RESET prefix SB-APPEND getpid FMT:SB-INT
   SB$ {: a:str :} a STR:DATA dst a STR:LEN BYTE-COPY a STR:LEN ;

: SOCKETS! ( str -- )
   {: s:str :}
   s STR:LEN PATH-CAP > if E-FS-PATH throw then
   s STR:DATA SOCKETS s STR:LEN BYTE-COPY  s STR:LEN SOCKETS-U ! ;

: SETUP ( -- )
   FS:CLEANUP-RESET
   s" HB_SOCK_TMP" GETENV dup STR:LEN 0 > if
      SOCKETS!
   else
      drop s" habu-sd" FS:MKTEMP-DIR SOCKETS!
      SOCKETS$ FS:CLEANUP-TREE+
   then
   s" habu-sd-" GOT NAMED {: u:n :}
   SOCKETS$ GOT u >str BOUND FS:JOIN-PATH BOUND-U !
   s" habu-sd-missing-" GOT NAMED {: v:n :}
   SOCKETS$ GOT v >str MISSING FS:JOIN-PATH MISSING-U !
   s" @habu-sd-" ABSTRACT NAMED ABSTRACT-U !
   BOUND$ BIND-AT BOUND-FD !
   ABSTRACT$ BIND-AT ABSTRACT-FD !
   s"    SYSTEMD:NOTIFY-READY" CHILD-SOURCE ;

\ A child engine per case: GETENV reads the environment the process started with.
: RUN ( str -- n n )   \ outu rc
   {: value:str :}
   ENGINE-CANDIDATE:PATH$ {: engine:str :}
   PROG CODEGEN:CONTENTS {: prog:str :}
   PROC-ARGV-ENV-RESET
   s" NOTIFY_SOCKET" value PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   engine prog
   OUT IO-CAP >LEN ERR IO-CAP >LEN PROCESS:WAIT-FOREVER
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROCESS:ENDING>RC RC>N
   {: outu:len erru:len rc:n :}
   rc 0 <> if ERR erru LEN>N >str type cr then
   outu LEN>N rc ;

: WANT$ ( n -- str )
   SB-RESET FMT:SB-INT 10 SB-APPEND-C SB$ ;

: ANSWERS ( str n -- )
   {: value:str want:n :}
   value RUN {: outu:n rc:n :}
   OUT outu >str want WANT$ T$=
   s" ... and the child engine exits cleanly" T-LABEL
   rc 0 T= ;

\ A unix datagram is queued before sendto returns, so nothing waits.
: RECEIVE ( n ^VEC:vector<u8> -- )
   {: fd:n v:^ :}
   fd UDP:>SOCKET v GOT-CAP CAP-LEN 0 >MS UDP:RECEIVE
   MATCH UDP:receive-result
      packet OF drop ENDOF
      truncated OF drop ENDOF
      timeout OF ENDOF
      failed OF drop ENDOF
   ;MATCH ;

\ The socket's queued datagram is the str.
: HOLDS ( n str -- )
   {: fd:n want:str :}
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: v:^ :}
   fd v RECEIVE
   v VEC:BYTES want T$=
   v VEC:DELETE ;

: PATH-CASE ( -- )
   s" a bound path socket: the child answers ok" T-LABEL
   BOUND$ 0 ANSWERS
   s" ... and the socket holds READY=1" T-LABEL
   BOUND-FD @ s" READY=1" HOLDS ;

: ABSTRACT-CASE ( -- )
   s" a bound abstract socket: the child answers ok" T-LABEL
   ABSTRACT$ 0 ANSWERS
   s" ... and the socket holds READY=1" T-LABEL
   ABSTRACT-FD @ s" READY=1" HOLDS ;

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

: EXTEND-CASE ( -- )
   s"    90000 >MS SYSTEMD:NOTIFY-EXTEND-TIMEOUT" CHILD-SOURCE
   s" an extension to a bound path socket: the child answers ok" T-LABEL
   BOUND$ 0 ANSWERS
   s" ... and the socket holds EXTEND_TIMEOUT_USEC in microseconds" T-LABEL
   BOUND-FD @ s" EXTEND_TIMEOUT_USEC=90000000" HOLDS ;

\ In this process: the bound is checked before NOTIFY_SOCKET is read.
: NEGATIVE-CASE ( -- )
   s" a negative extension answers EINVAL" T-LABEL
   -1 >MS SYSTEMD:NOTIFY-EXTEND-TIMEOUT MATCH SYSTEMD:status
      ok OF 0 ENDOF
      failed OF SYSTEMD:ERRNO>N ENDOF
   ;MATCH EINVAL T= ;

: TEARDOWN ( -- )
   BOUND-FD @ UDP:>SOCKET UDP:CLOSE drop
   ABSTRACT-FD @ UDP:>SOCKET UDP:CLOSE drop
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
   EXTEND-CASE
   NEGATIVE-CASE
   TEARDOWN
   T-REPORT
   s" systemd: ok" type cr ;

;package

SYSTEMD-TEST:SYSTEMD-TEST-MAIN
