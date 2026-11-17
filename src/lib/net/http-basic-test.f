\ http-basic-test.f - HTTP:BASIC-CREDENTIALS: RFC 7617's example, every place
\ the `:` can fall in a base64 group, the user and capacity rejections, and no
\ copy of the password left in the span.
\ Run: bin/hb --load src/lib/net/http-basic-test.f

require src/lib/string.f
require src/lib/test.f
require src/lib/span.f
require src/lib/base64.f
require src/lib/net/http-basic.f

package HTTP-BASIC-TEST

$A5 constant BASIC-CANARY
64 SPAN-BUFFER: BASIC-OUT
64 SPAN-BUFFER: BASIC-WANT
8 SPAN-BUFFER: BASIC-JOINED

: OUT$ ( n -- ^u8 n )
   BASIC-OUT SPAN:$ drop swap ;

: CANARY-FROM? ( n -- bool ) {: from:n :}
   BASIC-OUT SPAN:LEN from ?do
      BASIC-OUT i SPAN:U8@ BASIC-CANARY <> if false unloop exit then
   loop true ;

\ The example of RFC 7617 section 2, into a span of exactly its length.
: EXAMPLE ( -- )
   s" Aladdin" s" open sesame" BASIC-OUT 34 SPAN:TAKE HTTP:BASIC-CREDENTIALS
   {: k:n :}
   s" RFC 7617's Aladdin example" T-LABEL
   k OUT$ s" Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==" T$= ;

: WANT$ ( ^u8 n ^u8 n -- ^u8 n ) {: user:^ u:n pass:^ p:n :}
   user u BASIC-JOINED SPAN:COPY
   [char] : BASIC-JOINED u SPAN:U8!
   pass p BASIC-JOINED u 1+ SPAN:SKIP SPAN:COPY
   s" Basic " BASIC-WANT SPAN:COPY
   BASIC-JOINED SPAN:$ drop u 1+ p +
      BASIC-WANT 6 SPAN:SKIP BASE64:ENCODE {: n:n :}
   BASIC-WANT SPAN:$ drop n 6 + ;

\ Every user length 0..3 with every password length 0..3 puts the `:` at each
\ place in its group and the password's bytes in, and after, that group; the
\ canary past the credentials shows no laid byte is left.
: GROUP ( n n -- ) {: u:n p:n :}
   BASIC-CANARY BASIC-OUT SPAN:FILL
   s" abc" drop u s" xyz" drop p BASIC-OUT HTTP:BASIC-CREDENTIALS {: k:n :}
   s" user and password lengths 0..3" T-LABEL
   k OUT$ s" abc" drop u s" xyz" drop p WANT$ T$=
   s" the bytes past the credentials keep the canary" T-LABEL
   k CANARY-FROM? TTRUE ;

: GROUPS ( -- )
   4 0 do 4 0 do j i GROUP loop loop ;

\ A rejection writes nothing.
: REJECTIONS ( -- )
   BASIC-CANARY BASIC-OUT SPAN:FILL
   s" a user-id holding `:`" T-LABEL
   [: s" a:b" s" c" BASIC-OUT HTTP:BASIC-CREDENTIALS drop ;]
      HTTP:E-BASIC-USER TTHROWSQ
   0 CANARY-FROM? TTRUE
   s" a span one byte short" T-LABEL
   [: s" Aladdin" s" open sesame" BASIC-OUT 33 SPAN:TAKE
      HTTP:BASIC-CREDENTIALS drop ;] E-SPAN-CAPACITY TTHROWSQ
   0 CANARY-FROM? TTRUE ;

public

: RUN ( -- )
   T-RESET
   EXAMPLE GROUPS REJECTIONS ;

;package

HTTP-BASIC-TEST:RUN
T-REPORT
