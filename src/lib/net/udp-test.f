split-str
\ udp-test.f - unix datagram sockets bound in the pool's short HB_SOCK_TMP,
\ else under TMPDIR, and abstract names.
\
\ A unix datagram is queued before sendto returns, so every receive is one
\ try with timeout zero and the AIO loop never starts.
\ Run: bin/hb --load src/lib/net/udp-test.f
require src/lib/test.f
require src/lib/string.f
require src/lib/num-types.f
require src/lib/span.f
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
create RECV-BUF BUF-CAP allot
create LONG LONG-N allot
create DIR PATH-CAP allot   variable DIR-U
create PATH PATH-CAP allot   variable PATH-U
create NAME NAME-CAP allot   variable NAME-U
variable SOCK-R
variable SOCK-S

: DIR$ ( -- str ) DIR DIR-U @ ;
: PATH$ ( -- str ) PATH PATH-U @ ;
: NAME$ ( -- str ) NAME NAME-U @ ;


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
   {: name:^ size:n :}
   DIR$ name size PATH FS:JOIN-PATH PATH-U ! ;


\ An abstract name of this process: the namespace is the host's.
: NAME! ( -- )
   SB-RESET s" @habu-udp-" SB-APPEND getpid FMT:SB-INT
   SB$ {: a:^ u:n :} a NAME u BYTE-COPY u NAME-U ! ;


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


: PAT-CHECK ( n -- bool ) {: count:n :}
   count 0 do
      RECV-BUF i + c@ i 7 * 3 + $FF and <> if unloop false exit then
   loop true ;


: SEND-TO ( str -- ) {: name:^ size:n :}
   SOCK-S @ UDP:>SOCKET name size SEND-BUF PAYLOAD-N UDP:SEND
   STATUS-ERRNO 0 T= ;


: RECEIVE-R ( n -- UDP:receive-result ) {: capacity:n :}
   SOCK-R @ UDP:>SOCKET RECV-BUF capacity CAP-LEN 0 >MS UDP:RECEIVE ;


\ ---- cases -------------------------------------------------------------------

: T-PATH ( -- )
   s" a datagram sent to a bound path arrives with its length" T-LABEL
   PAT-FILL
   s" path" PATH!
   PATH$ BIND-RECEIVER OPEN-SENDER
   PATH$ SEND-TO
   BUF-CAP RECEIVE-R PAYLOAD-N WANT-PACKET
   s" and the bytes delivered are the bytes that were sent" T-LABEL
   PAYLOAD-N PAT-CHECK TTRUE
   CLOSE-BOTH
   PATH$ FS:REMOVE-FILE ;


: T-TRUNCATED ( -- )
   s" a datagram larger than the capacity is truncated, carrying its own length" T-LABEL
   PAT-FILL
   s" truncated" PATH!
   PATH$ BIND-RECEIVER OPEN-SENDER
   PATH$ SEND-TO
   SMALL-CAP RECEIVE-R PAYLOAD-N WANT-TRUNCATED
   s" and the capacity it filled holds the datagram's first bytes" T-LABEL
   SMALL-CAP PAT-CHECK TTRUE
   CLOSE-BOTH
   PATH$ FS:REMOVE-FILE ;


HB-TARGET-LINUX-KERNEL? [if]
: T-ABSTRACT ( -- )
   s" a datagram sent to a bound abstract name arrives" T-LABEL
   PAT-FILL
   NAME!
   NAME$ BIND-RECEIVER OPEN-SENDER
   NAME$ SEND-TO
   BUF-CAP RECEIVE-R PAYLOAD-N WANT-PACKET
   PAYLOAD-N PAT-CHECK TTRUE
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
   LONG LONG-N PATH!
   [: BIND-LONG ;] E-SPAN-CAPACITY TTHROWSQ ;


: DIR! ( str -- ) {: a:^ u:n :}
   a DIR u BYTE-COPY u DIR-U ! ;


\ A gate test's TMPDIR leaves sun_path no room; HB_SOCK_TMP is short.
: SETUP ( -- )
   FS:CLEANUP-RESET
   s" HB_SOCK_TMP" GETENV dup 0 > if DIR! exit then
   2drop s" habu-udp" FS:MKTEMP-DIR DIR!
   DIR$ FS:CLEANUP-TREE+ ;


: RUN ( -- )
   T-RESET
   SETUP
   T-PATH
   T-TRUNCATED
   T-ABSTRACT
   T-LONG
   FS:CLEANUP-RUN
   T-REPORT
   s" udp-test: ok" type cr ;

RUN

;package
