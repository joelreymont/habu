split-str
\ http-basic.f - HTTP Basic credentials (RFC 7617), the value of an
\ `Authorization` field. Input is checked and output measured before a byte is
\ written. Memory Habu owns, dead stack cells aside, never holds a copy of the
\ password or of user`:`password: every byte is encoded straight from the
\ caller's spans, except the one three-byte group that straddles the `:`, which
\ is laid at its own output position in the destination and encoded there in
\ place. The caller wipes its span when done.
\ STORAGE CLASS. CALLER-OWNED: the module keeps no state.

require src/lib/error.f
require src/lib/span.f
require src/lib/base64.f

package HTTP
private

$3A constant USER-END            \ ':' ends the user-id

: BASIC-PREFIX$ ( -- str ) s" Basic " ;

: CONTROL? ( n -- bool ) dup $20 < swap $7F = or ;   \ RFC 5234 CTL.

: USER-CHECKED ( str -- ) {: a:^ u:n :}
   u 0 ?do
      a i + c@ dup USER-END = swap CONTROL? or if E-HTTP-BASIC-USER throw then
   loop ;

: PASS-CHECKED ( str -- ) {: a:^ p:n :}
   p 0 ?do a i + c@ CONTROL? if E-HTTP-BASIC-PASSWORD throw then loop ;

\ The input is compared with what the span holds past the prefix, three bytes
\ to four of reach, so no length is scaled up and none wraps.
: ROOM-CHECKED ( n n SPAN:span<u8> -- ) {: u:n p:n s :}
   s SPAN:LEN BASIC-PREFIX$ nip - {: room:n :}
   room 0 < if E-SPAN-CAPACITY throw then
   room 4 / 3 * {: most:n :}
   p most u - 1- > if E-SPAN-CAPACITY throw then ;

public

E-HTTP-BASIC-USER constant E-BASIC-USER
E-HTTP-BASIC-PASSWORD constant E-BASIC-PASSWORD

\ Writes `Basic ` and the RFC 4648 standard base64, padded, of user `:`
\ password; answers the length written. A negative length is E-SPAN-LENGTH, a
\ user holding `:` or a control character E-BASIC-USER, a password holding a
\ control character E-BASIC-PASSWORD, a span shorter than
\ 6 + 4 * ceil((u + 1 + p) / 3) E-SPAN-CAPACITY.
: BASIC-CREDENTIALS ( str ^u8 n SPAN:span<u8> -- n )
   {: user:^ u:n pass:^ p:n s :}
   u 0 < p 0 < or if E-SPAN-LENGTH throw then
   user u USER-CHECKED
   pass p PASS-CHECKED
   u p s ROOM-CHECKED
   BASIC-PREFIX$ s SPAN:COPY
   s BASIC-PREFIX$ nip SPAN:SKIP {: head :}
   u 3 / 3 * {: whole:n :}
   user whole head BASE64:ENCODE {: hn:n :}
   head hn SPAN:SKIP {: mid :}
   u whole - {: r:n :}
   2 r - p min {: k:n :}
   user whole + r mid SPAN:COPY
   USER-END mid r SPAN:U8!
   pass k mid r 1+ SPAN:SKIP SPAN:COPY
   mid SPAN:$ drop r 1+ k + mid BASE64:ENCODE {: mn:n :}
   pass k + p k - mid mn SPAN:SKIP BASE64:ENCODE {: tn:n :}
   BASIC-PREFIX$ nip hn + mn + tn + ;

;package
