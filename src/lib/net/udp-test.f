\ udp-test.f - unix datagram sockets bound in the pool's short HB_SOCK_TMP,
\ else under TMPDIR, and abstract names.
\
\ A unix datagram is queued before sendto returns, so every receive is one
\ try with timeout zero and AIO never starts.
\ Run: bin/hb --load src/lib/net/udp-test.f
require src/lib/test/assert.f
require src/lib/string.f
require src/lib/num-types.f
require src/lib/span.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/fmt.f
require src/os/env-base.f
require src/lib/net/udp.f
require src/core/exit-status.f

package UDP-TEST

CAST: BLEN>N ( NUM:byte-len -- n )

: CAP-LEN ( n -- NUM:byte-len )
   NUM:BYTE-LEN MATCH option
      none OF
         s" udp-test: negative capacity" EXIT-STATUS:FAILURE die
      ENDOF
      some OF ENDOF
   ;MATCH ;

$40 constant PAYLOAD-N             \ the datagram every case sends
$10 constant SMALL-CAP             \ the capacity the truncating case offers
$80 constant BUF-CAP
$80 constant LONG-N                \ a file name longer than either sun_path
$40 constant NAME-CAP
2 constant ENOENT

create SEND-BUF BUF-CAP allot
create LONG LONG-N allot
create DIR PATH-CAP allot   variable DIR-U
create PATH PATH-CAP allot   variable PATH-U
create NAME NAME-CAP allot   variable NAME-U
variable SOCK-R
variable SOCK-S

: DIR$ ( -- str ) DIR DIR-U @ >str ;
: PATH$ ( -- str ) PATH PATH-U @ >str ;
: NAME$ ( -- str ) NAME NAME-U @ >str ;


\ ---- result inspectors -------------------------------------------------------

: STATUS-ERRNO ( UDP:status -- n )
   MATCH UDP:status
      ok OF 0 ENDOF
      failed OF UDP:ERRNO>N ENDOF
   ;MATCH ;


: SOCKET-OF ( UDP:open-result -- n )
   MATCH UDP:open-result
      opened OF UDP:SOCKET>N ENDOF
      failed OF UDP:ERRNO>N drop s" open failed" T-FAIL-AS -1 ENDOF
   ;MATCH ;


: WANT-PACKET ( UDP:receive-result n -- ) {: want:n :}
   MATCH UDP:receive-result
      packet OF BLEN>N want T= ENDOF
      truncated OF drop s" receive: unexpectedly truncated" T-FAIL-AS ENDOF
      timeout OF s" receive: nothing queued" T-FAIL-AS ENDOF
      failed OF UDP:ERRNO>N drop s" receive: failed" T-FAIL-AS ENDOF
   ;MATCH ;


: WANT-TRUNCATED ( UDP:receive-result n -- ) {: want:n :}
   MATCH UDP:receive-result
      packet OF drop s" receive: unexpectedly whole" T-FAIL-AS ENDOF
      truncated OF BLEN>N want T= ENDOF
      timeout OF s" receive: nothing queued" T-FAIL-AS ENDOF
      failed OF UDP:ERRNO>N drop s" receive: failed" T-FAIL-AS ENDOF
   ;MATCH ;


\ ---- the sockets the cases share ---------------------------------------------

: PATH! ( str -- )
   {: name:str :}
   DIR$ name PATH FS:JOIN-PATH PATH-U ! ;


\ An abstract name of this process: the namespace is the host's.
: NAME! ( -- )
   SB-RESET s" @habu-udp-" SB-APPEND getpid FMT:SB-INT
   SB$ {: s:str :}
   s STR:DATA NAME s STR:LEN BYTE-COPY s STR:LEN NAME-U ! ;


: BIND-RECEIVER ( str -- )
   UDP:BIND SOCKET-OF dup 0 >= TTRUE SOCK-R ! ;


: OPEN-SENDER ( -- )
   UDP:OPEN SOCKET-OF dup 0 >= TTRUE SOCK-S ! ;


: CLOSE-BOTH ( -- )
   SOCK-R @ UDP:>SOCKET UDP:CLOSE STATUS-ERRNO 0 T=
   SOCK-S @ UDP:>SOCKET UDP:CLOSE STATUS-ERRNO 0 T= ;


\ A datagram whose every byte says where in the datagram it was, so a truncated
\ answer can be checked to be the datagram's first bytes.
: PAT-FILL ( -- )
   PAYLOAD-N 0 do i 7 * 3 + $FF and SEND-BUF i + c! loop ;


: PAT? ( str -- bool ) {: got:str :}
   got STR:LEN 0 ?do
      got STR:DATA i + c@ i 7 * 3 + $FF and <> if unloop false exit then
   loop true ;


\ The bytes from n on hold the datagram's first bytes, and nothing follows.
: PAT-CHECK ( ^VEC:vector<u8> n n -- bool )
   {: v:^ skip:n count:n :}
   v VEC:COUNT skip count + = if
      v VEC:SPAN skip SPAN:SKIP SPAN:$ PAT? exit
   then false ;


: FRESH ( -- ^VEC:vector<u8> ) HEAP:ALLOCATOR 0 [type] u8 VEC:NEW ;


: SEND-TO ( str -- ) {: name:str :}
   SOCK-S @ UDP:>SOCKET name SEND-BUF PAYLOAD-N >str UDP:SEND
   STATUS-ERRNO 0 T= ;


: RECEIVE-R ( ^VEC:vector<u8> n -- UDP:receive-result )
   {: v:^ most:n :}
   SOCK-R @ UDP:>SOCKET v most CAP-LEN 0 >MS UDP:RECEIVE ;


\ ---- cases -------------------------------------------------------------------

: T-PATH ( -- )
   s" a datagram sent to a bound path arrives with its length" T-LABEL
   PAT-FILL
   s" path" PATH!
   PATH$ BIND-RECEIVER OPEN-SENDER
   PATH$ SEND-TO
   FRESH {: v:^ :}
   v BUF-CAP RECEIVE-R PAYLOAD-N WANT-PACKET
   s" and the bytes delivered are the bytes that were sent" T-LABEL
   v 0 PAYLOAD-N PAT-CHECK TTRUE
   v VEC:DELETE
   CLOSE-BOTH
   PATH$ FS:REMOVE-FILE ;


: T-APPENDS ( -- )
   s" a datagram is appended after the bytes a vector holds" T-LABEL
   PAT-FILL
   s" appends" PATH!
   PATH$ BIND-RECEIVER OPEN-SENDER
   PATH$ SEND-TO
   FRESH {: v:^ :}
   s" held" v VEC:APPEND
   v BUF-CAP RECEIVE-R PAYLOAD-N WANT-PACKET
   v 4 PAYLOAD-N PAT-CHECK TTRUE
   s" and the bytes it held are intact" T-LABEL
   v VEC:SPAN 4 SPAN:TAKE SPAN:$ s" held" T$=
   v VEC:DELETE
   CLOSE-BOTH
   PATH$ FS:REMOVE-FILE ;


: T-TRUNCATED ( -- )
   s" a datagram longer than the most appended is truncated, carrying its own length" T-LABEL
   PAT-FILL
   s" truncated" PATH!
   PATH$ BIND-RECEIVER OPEN-SENDER
   PATH$ SEND-TO
   FRESH {: v:^ :}
   v SMALL-CAP RECEIVE-R PAYLOAD-N WANT-TRUNCATED
   s" and the bytes appended are the datagram's first ones" T-LABEL
   v 0 SMALL-CAP PAT-CHECK TTRUE
   v VEC:DELETE
   CLOSE-BOTH
   PATH$ FS:REMOVE-FILE ;


HB-TARGET-LINUX? [if]
: T-ABSTRACT ( -- )
   s" a datagram sent to a bound abstract name arrives" T-LABEL
   PAT-FILL
   NAME!
   NAME$ BIND-RECEIVER OPEN-SENDER
   NAME$ SEND-TO
   FRESH {: v:^ :}
   v BUF-CAP RECEIVE-R PAYLOAD-N WANT-PACKET
   v 0 PAYLOAD-N PAT-CHECK TTRUE
   v VEC:DELETE
   CLOSE-BOTH
   s" and binding the name made no file" T-LABEL
   NAME$ FS:EXISTS? TFALSE ;
[then]


HB-TARGET-MACOS? [if]
: BIND-ERRNO ( UDP:open-result -- n )
   MATCH UDP:open-result
      opened OF UDP:CLOSE drop 0 ENDOF
      failed OF UDP:ERRNO>N ENDOF
   ;MATCH ;


: T-ABSTRACT ( -- )
   s" macOS has no abstract names: binding @name answers ENOENT" T-LABEL
   NAME!
   NAME$ UDP:BIND BIND-ERRNO ENOENT T= ;
[then]


: BIND-LONG ( -- )
   PATH$ UDP:BIND drop ;


: T-LONG ( -- )
   s" a path longer than sun_path throws E-SPAN-CAPACITY" T-LABEL
   $61 LONG LONG-N SPAN:MAKE SPAN:FILL
   LONG LONG-N >str PATH!
   [: BIND-LONG ;] E-SPAN-CAPACITY TTHROWSQ ;


: DIR! ( str -- ) {: s:str :}
   s STR:DATA DIR s STR:LEN BYTE-COPY s STR:LEN DIR-U ! ;


\ A gate test's TMPDIR leaves sun_path no room; HB_SOCK_TMP is short.
: SETUP ( -- )
   FS:CLEANUP-RESET
   s" HB_SOCK_TMP" GETENV dup STR:LEN 0 > if DIR! exit then
   drop s" habu-udp" FS:MKTEMP-DIR DIR!
   DIR$ FS:CLEANUP-TREE+ ;


: RUN ( -- )
   T-RESET
   SETUP
   T-PATH
   T-APPENDS
   T-TRUNCATED
   T-ABSTRACT
   T-LONG
   FS:CLEANUP-RUN
   T-REPORT
   s" udp-test: ok" type cr ;

RUN

;package
