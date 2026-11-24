\ http-basic-test.f - HTTP:BASIC-CREDENTIALS: RFC 7617's example, a UTF-8
\ password, every place the `:` can fall in a base64 group, the user and
\ password rejections, an append after held bytes into room reserved up front,
\ a block moved before the value's first byte lands, and no laid byte left past
\ the value.
\ Run: bin/hb --load src/lib/net/http-basic-test.f

require src/lib/string.f
require src/lib/test.f
require src/lib/span.f
require src/lib/base64.f
require src/lib/vector.f
require src/lib/arena.f
require src/lib/net/http-basic.f

package HTTP-BASIC-TEST

64 SPAN-BUFFER: BASIC-WANT
8 SPAN-BUFFER: BASIC-JOINED

: OUT$ ( ^VEC:vector<u8> -- str ) VEC:SPAN SPAN:$ ;

: DATA ( ^VEC:vector<u8> -- ^u8 ) OUT$ STR:DATA ;

: EXAMPLE ( ^ALLOC:allocator -- )
   {: al:^ :}
   al 0 [type] u8 VEC:NEW {: v:^ :}
   s" Aladdin" s" open sesame" v HTTP:BASIC-CREDENTIALS
   s" RFC 7617's Aladdin example" T-LABEL
   v OUT$ s" Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==" T$=
   v VEC:DELETE ;

: UTF8-PASSWORD ( ^ALLOC:allocator -- )
   {: al:^ :}
   al 0 [type] u8 VEC:NEW {: v:^ :}
   s" a" s\" \xC3\xA9" v HTTP:BASIC-CREDENTIALS
   s" a UTF-8 password is no control character" T-LABEL
   v OUT$ s" Basic YTrDqQ==" T$=
   v VEC:DELETE ;

\ The value follows the bytes the vector holds, in room reserved for both up
\ front, which is not grown.
: RESERVED ( ^ALLOC:allocator -- )
   {: al:^ :}
   al 0 [type] u8 VEC:NEW {: v:^ :}
   s" x: " v VEC:APPEND
   v 37 VEC:RESERVE
   s" Aladdin" s" open sesame" v HTTP:BASIC-CREDENTIALS
   s" the value is appended after the bytes the vector holds" T-LABEL
   v OUT$ s" x: Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==" T$=
   s" room reserved up front for the value is not grown" T-LABEL
   v VEC:CAP 37 = TTRUE
   v VEC:DELETE ;

\ The vector allocated after v makes growth move v's block; the arena leaves
\ the old bytes in place, so zero past `x: ` shows nothing landed before.
: MOVED ( ^ALLOC:allocator -- )
   {: al:^ :}
   al 0 [type] u8 VEC:NEW {: v:^ :}
   s" x: " v VEC:APPEND
   v 23 VEC:RESERVE
   al 1 [type] u8 VEC:NEW {: after:^ :}
   v DATA {: old:^ :}
   s" Aladdin" s" open sesame" v HTTP:BASIC-CREDENTIALS
   s" the value is appended after the bytes a moved block held" T-LABEL
   v OUT$ s" x: Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==" T$=
   s" the block moves before the first byte of the value lands" T-LABEL
   true 23 3 do old i + c@ 0<> if drop false leave then loop TTRUE
   after VEC:DELETE
   v VEC:DELETE ;

: WANT$ ( str str -- str ) {: user:str pass:str :}
   user BASIC-JOINED SPAN:COPY
   [char] : BASIC-JOINED user STR:LEN SPAN:U8!
   pass BASIC-JOINED user STR:LEN 1+ SPAN:SKIP SPAN:COPY
   s" Basic " BASIC-WANT SPAN:COPY
   BASIC-JOINED SPAN:$ STR:DATA user STR:LEN 1+ pass STR:LEN + >str
      BASIC-WANT 6 SPAN:SKIP BASE64:ENCODE {: n:n :}
   BASIC-WANT SPAN:$ STR:DATA n 6 + >str ;

: ZERO-PAST? ( ^VEC:vector<u8> -- bool )
   {: v:^ :}
   v DATA {: d:^ :}
   v VEC:CAP v VEC:COUNT ?do
      d i + c@ 0<> if false unloop exit then
   loop true ;

\ Every user length 0..3 with every password length 0..3 puts the `:` at each
\ place in its group and the password's bytes in, and after, that group; the
\ zero bytes past the value show no laid byte is left.
: GROUP ( ^ALLOC:allocator n n -- )
   {: al:^ u:n p:n :}
   al 0 [type] u8 VEC:NEW {: v:^ :}
   v 64 VEC:RESERVE
   s" abc" STR:DATA u >str {: user:str :}
   s" xyz" STR:DATA p >str {: pass:str :}
   user pass v HTTP:BASIC-CREDENTIALS
   s" user and password lengths 0..3" T-LABEL
   v OUT$ user pass WANT$ T$=
   s" the bytes past the credentials stay zero" T-LABEL
   v ZERO-PAST? TTRUE
   v VEC:DELETE ;

: GROUPS ( ^ALLOC:allocator -- )
   {: al:^ :}
   4 0 do 4 0 do al j i GROUP loop loop ;

: UNTOUCHED? ( ^VEC:vector<u8> -- bool )
   {: v:^ :}
   v VEC:COUNT 0 =  v VEC:CAP 0 =  and ;

\ A rejection neither grows nor writes the vector.
: REJECTIONS ( ^ALLOC:allocator -- )
   {: al:^ :}
   al 0 [type] u8 VEC:NEW {: v:^ :}
   s" a user-id holding `:`" T-LABEL
   [: s" a:b" s" c" v HTTP:BASIC-CREDENTIALS ;]
      HTTP:E-BASIC-USER TTHROWSQ
   v UNTOUCHED? TTRUE
   s" a user-id holding a tab" T-LABEL
   [: s\" a\tb" s" c" v HTTP:BASIC-CREDENTIALS ;]
      HTTP:E-BASIC-USER TTHROWSQ
   v UNTOUCHED? TTRUE
   s" a password ending in a line feed" T-LABEL
   [: s" a" s\" pw\n" v HTTP:BASIC-CREDENTIALS ;]
      HTTP:E-BASIC-PASSWORD TTHROWSQ
   v UNTOUCHED? TTRUE
   s" a password holding $1F" T-LABEL
   [: s" a" s\" p\x1Fw" v HTTP:BASIC-CREDENTIALS ;]
      HTTP:E-BASIC-PASSWORD TTHROWSQ
   v UNTOUCHED? TTRUE
   s" a password holding $7F" T-LABEL
   [: s" a" s\" p\x7Fw" v HTTP:BASIC-CREDENTIALS ;]
      HTTP:E-BASIC-PASSWORD TTHROWSQ
   v UNTOUCHED? TTRUE
   v VEC:DELETE ;

public

: RUN ( -- )
   T-RESET
   ARENA:NEW {: a:^ :}
   a ARENA:ALLOCATOR {: al:^ :}
   al EXAMPLE  al UTF8-PASSWORD  al RESERVED  al MOVED  al GROUPS
   al REJECTIONS
   a ARENA:RELEASE ;

;package

HTTP-BASIC-TEST:RUN
T-REPORT
