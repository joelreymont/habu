split-str
\ http-basic.f - HTTP Basic credentials (RFC 7617), the value of an
\ `Authorization` field, appended to a byte vector. Input is checked and room
\ for the whole value reserved before a byte is written, so the vector's block
\ moves, if at all, before the first byte lands. Memory Habu owns, dead stack
\ cells aside, never holds a copy of the password or of user`:`password: every
\ byte is encoded straight from the caller's strings, except the one three-byte
\ group that straddles the `:`, which is laid at its own output position in the
\ vector and encoded there in place. The caller reserves room for what it
\ appends after the value, since growth leaves the old block unwiped, and wipes
\ the vector before VEC:DELETE.
\ STORAGE CLASS. CALLER-OWNED: the module keeps no state.

require src/lib/error.f
require src/lib/span.f
require src/lib/base64.f
require src/lib/vector.f

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

\ The bytes of the value: the prefix and the base64 of u + 1 + p bytes.
: VALUE-LEN ( n n -- n ) + 3 + 3 / 4 * BASIC-PREFIX$ nip + ;

\ Writes the value into s, which holds exactly VALUE-LEN bytes.
: WRITE-VALUE ( str str SPAN:span<u8> -- )
   {: user:^ u:n pass:^ p:n s :}
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
   pass k + p k - mid mn SPAN:SKIP BASE64:ENCODE drop ;

public

E-HTTP-BASIC-USER constant E-BASIC-USER
E-HTTP-BASIC-PASSWORD constant E-BASIC-PASSWORD

\ Appends `Basic ` and the RFC 4648 standard base64, padded, of user `:`
\ password, neither of which may lie in the vector: its block may move. A
\ negative length is E-SPAN-LENGTH, a user holding `:` or a control character
\ E-BASIC-USER, a password holding a control character E-BASIC-PASSWORD; a
\ rejection leaves the vector as it was.
: BASIC-CREDENTIALS ( str str ^VEC:vector<u8> -- )
   {: user:^ u:n pass:^ p:n v:^ :}
   u 0 < p 0 < or if E-SPAN-LENGTH throw then
   user u USER-CHECKED
   pass p PASS-CHECKED
   v VEC:COUNT {: old:n :}
   old u p VALUE-LEN + {: new:n :}
   v new VEC:RESERVE
   v new VEC:ENSURE
   user u pass p v VEC:SPAN old SPAN:SKIP WRITE-VALUE ;

;package
