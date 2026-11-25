\ udp4-test.f - loopback sockets: bind, the bound endpoint and a send the
\ OS rejects.
\ Run: bin/hb --load src/lib/net/udp4-test.f
require src/lib/test/assert.f
require src/lib/net/udp4.f

package UDP4-TEST

$7F000001 constant LOOPBACK
$FFE4 constant OVERSIZE-N          \ one byte past the largest IPv4 UDP payload

: EMSGSIZE ( -- n ) HB-TARGET-MACOS? if 40 else 90 then ;

variable SOCK-A
variable SOCK-B
variable PORT-A
variable PORT-B
create OVERSIZE-BUF OVERSIZE-N allot


\ ---- result inspectors -------------------------------------------------------

: STATUS-ERRNO ( UDP4:status -- n )
   MATCH UDP4:status
      ok OF 0 ENDOF
      failed OF UDP4:ERRNO>N ENDOF
   ;MATCH ;


\ ---- the pair the cases share ------------------------------------------------

: OPEN-SOCKET ( -- n )
   LOOPBACK UDP4:ADDRESS 0 UDP4:PORT UDP4:BIND
   MATCH UDP4:open-result
      opened OF UDP4:SOCKET>N ENDOF
      failed OF UDP4:ERRNO>N drop s" bind failed" T-FAIL-AS -1 ENDOF
   ;MATCH ;


: PORT-OF ( n -- n )
   UDP4:>SOCKET UDP4:LOCAL
   MATCH UDP4:endpoint-result
      endpoint OF UDP4:PORT>N swap UDP4:ADDRESS>N drop ENDOF
      failed OF UDP4:ERRNO>N drop s" local failed" T-FAIL-AS 0 ENDOF
   ;MATCH ;


: OPEN-PAIR ( -- )
   OPEN-SOCKET dup 0 >= TTRUE SOCK-A !
   OPEN-SOCKET dup 0 >= TTRUE SOCK-B !
   SOCK-A @ PORT-OF dup 0 > TTRUE PORT-A !
   SOCK-B @ PORT-OF dup 0 > TTRUE PORT-B ! ;


: CLOSE-PAIR ( -- )
   SOCK-A @ UDP4:>SOCKET UDP4:CLOSE STATUS-ERRNO 0 T=
   SOCK-B @ UDP4:>SOCKET UDP4:CLOSE STATUS-ERRNO 0 T= ;


\ ---- cases -------------------------------------------------------------------

: T-OVERSIZE ( -- )
   s" a datagram past the largest payload answers failed with EMSGSIZE" T-LABEL
   OPEN-PAIR
   SOCK-A @ UDP4:>SOCKET LOOPBACK UDP4:ADDRESS PORT-B @ UDP4:PORT
   OVERSIZE-BUF OVERSIZE-N >str UDP4:SEND STATUS-ERRNO EMSGSIZE T=
   CLOSE-PAIR ;


: RUN ( -- )
   T-RESET
   T-OVERSIZE
   T-REPORT
   s" udp4-test: ok" type cr ;

RUN

;package
