\ uri-test.f - file URIs decoded to paths, paths encoded to file URIs, bytes
\ encoded as a web form or a query component and decoded from a form or a
\ fragment, request paths checked for a dot segment, and http(s) URLs
\ resolved, compared, sliced and checked for an origin alone, through
\ src/lib/uri.f.
\ Run: bin/hb --load src/lib/uri-test.f

require src/lib/error.f
require src/lib/string.f
require src/lib/test/assert.f
require src/lib/span.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/uri.f
require src/lib/test/counting-allocator.f

package URI-TEST

64 SPAN-BUFFER: DST
64 SPAN-BUFFER: URI-DST
128 SPAN-BUFFER: URL-DST
2 BUFFER: ONE                     \ `/` and the byte a round trip takes
3 BUFFER: WANT                    \ one byte's form encoding
$A5 constant CANARY
TYPED-VARIABLE DECODED ^VEC:vector<u8>

: PATH$ ( str -- str )
   DST URI:FILE>PATH {: u:n :}
   DST SPAN:$ STR:DATA u >str ;

: PATH= ( str str -- )
   {: a:str b:str :}
   a PATH$ b T$= ;

\ An empty or `localhost` authority, the scheme and the host in any case.
: FORMS ( -- )
   s" file:///home/u/a.f" s" /home/u/a.f" PATH=
   s" file://localhost/home/u/a.f" s" /home/u/a.f" PATH=
   s" FILE://LocalHost/x" s" /x" PATH=
   s" File:///x" s" /x" PATH=
   s" file:///" s" /" PATH= ;

\ Each escape is one byte, in either hex case, decoded once: a decoded `%`
\ starts nothing. `%2F`, `%00`, `%3F` and `%23` are bytes like any other; a
\ byte a client left unescaped is kept as it is.
: ESCAPES ( -- )
   s" file:///a%20b/%C3%A9.f" s\" /a b/\xC3\xA9.f" PATH=
   s" file:///%c3%a9" s\" /\xC3\xA9" PATH=
   s" file:///a%2Fb" s" /a/b" PATH=
   s" file:///a%00b" s\" /a\x00b" PATH=
   s" file:///%2541" s" /%41" PATH=
   s" file:///a%3Fb%23c" s" /a?b#c" PATH=
   s\" file:///\xC3\xA9" s\" /\xC3\xA9" PATH= ;

: DECODE-DROP ( str -- )
   DST URI:FILE>PATH drop ;

: SCHEMES ( -- )
   [: s" http://localhost/x" DECODE-DROP ;] E-URI-SCHEME TTHROWSQ
   [: s" untitled:Untitled-1" DECODE-DROP ;] E-URI-SCHEME TTHROWSQ
   [: s" /home/u/a.f" DECODE-DROP ;] E-URI-SCHEME TTHROWSQ
   [: s" files:///x" DECODE-DROP ;] E-URI-SCHEME TTHROWSQ
   [: s" fil:///x" DECODE-DROP ;] E-URI-SCHEME TTHROWSQ
   [: s" " DECODE-DROP ;] E-URI-SCHEME TTHROWSQ ;

\ A remote host, a port, user information, a missing `//`, and an authority
\ with no path after it.
: AUTHORITIES ( -- )
   [: s" file://host/x" DECODE-DROP ;] E-URI-AUTHORITY TTHROWSQ
   [: s" file://localhost:80/x" DECODE-DROP ;] E-URI-AUTHORITY TTHROWSQ
   [: s" file://user@localhost/x" DECODE-DROP ;] E-URI-AUTHORITY TTHROWSQ
   [: s" file:/x" DECODE-DROP ;] E-URI-AUTHORITY TTHROWSQ
   [: s" file:x" DECODE-DROP ;] E-URI-AUTHORITY TTHROWSQ
   [: s" file:" DECODE-DROP ;] E-URI-AUTHORITY TTHROWSQ
   [: s" file://" DECODE-DROP ;] E-URI-AUTHORITY TTHROWSQ
   [: s" file://localhost" DECODE-DROP ;] E-URI-AUTHORITY TTHROWSQ ;

\ A `%` at the end, with one digit, with a non-hex digit in either place, and a
\ bare `?` or `#`, which would open a query or fragment a file URI cannot have.
: ESCAPE-REJECTIONS ( -- )
   [: s" file:///a%" DECODE-DROP ;] E-URI-ESCAPE TTHROWSQ
   [: s" file:///a%4" DECODE-DROP ;] E-URI-ESCAPE TTHROWSQ
   [: s" file:///a%G1" DECODE-DROP ;] E-URI-ESCAPE TTHROWSQ
   [: s" file:///a%1G" DECODE-DROP ;] E-URI-ESCAPE TTHROWSQ
   [: s" file:///a?b" DECODE-DROP ;] E-URI-ESCAPE TTHROWSQ
   [: s" file:///a#b" DECODE-DROP ;] E-URI-ESCAPE TTHROWSQ ;

: UNTOUCHED ( n -- )
   {: n:n :}
   n 0 ?do DST i SPAN:U8@ CANARY T= loop ;

: FOUR-SHORT ( -- )
   s" file:///abcd" DST 4 SPAN:TAKE URI:FILE>PATH drop ;

: LATE-ESCAPE ( -- )
   s" file:///abcd%G" DECODE-DROP ;

: NEGATIVE-LENGTH ( -- )
   s" file:///x" STR:DATA -1 >str DECODE-DROP ;

\ A rejection writes nothing: the whole URI is checked, and the path's length
\ measured against the span, before the first byte lands. A span of exactly the
\ path's length is enough.
: ALL-OR-NOTHING ( -- )
   CANARY DST SPAN:FILL
   [: FOUR-SHORT ;] E-SPAN-CAPACITY TTHROWSQ
   5 UNTOUCHED
   [: LATE-ESCAPE ;] E-URI-ESCAPE TTHROWSQ
   5 UNTOUCHED
   [: NEGATIVE-LENGTH ;] E-SPAN-LENGTH TTHROWSQ
   s" file:///abcd" DST 5 SPAN:TAKE URI:FILE>PATH 5 T=
   DST SPAN:$ STR:DATA 5 >str s" /abcd" T$= ;

: URI$ ( str -- str )
   URI-DST URI:PATH>FILE {: u:n :}
   URI-DST SPAN:$ STR:DATA u >str ;

: URI= ( str str -- )
   {: a:str b:str :}
   a URI$ b T$= ;

\ `file://` and the path, whose `/`, letters, digits and `-._~` stay as they
\ are.
: ENCODE-FORMS ( -- )
   s" /home/u/a.f" s" file:///home/u/a.f" URI=
   s" /" s" file:///" URI=
   s" /AZaz09-._~/x" s" file:///AZaz09-._~/x" URI= ;

\ Every other byte is `%XX` in upper-case hex: a space, each byte of a UTF-8
\ character, the delimiters a URI gives meaning to, and NUL.
: ENCODE-ESCAPES ( -- )
   s" /a b" s" file:///a%20b" URI=
   s\" /\xC3\xA9.f" s" file:///%C3%A9.f" URI=
   s" /%?#:@+()" s" file:///%25%3F%23%3A%40%2B%28%29" URI=
   s\" /a\x00b\xFF" s" file:///a%00b%FF" URI= ;

: ENCODE-DROP ( str -- )
   URI-DST URI:PATH>FILE drop ;

\ A path that is not absolute: relative, empty, or a URI already.
: RELATIVES ( -- )
   [: s" a/b" ENCODE-DROP ;] E-URI-RELATIVE TTHROWSQ
   [: s" ./a" ENCODE-DROP ;] E-URI-RELATIVE TTHROWSQ
   [: s" " ENCODE-DROP ;] E-URI-RELATIVE TTHROWSQ
   [: s" file:///a" ENCODE-DROP ;] E-URI-RELATIVE TTHROWSQ ;

: ENCODE-SHORT ( -- )   \ `file:///a%20b` is 13 bytes
   s" /a b" DST 12 SPAN:TAKE URI:PATH>FILE drop ;

: ENCODE-RELATIVE ( -- )
   s" a b" DST URI:PATH>FILE drop ;

: ENCODE-NEGATIVE ( -- )
   s" /x" STR:DATA -1 >str DST URI:PATH>FILE drop ;

\ An encoding rejected writes nothing, and a span of exactly the URI's length
\ is enough.
: ENCODE-ALL-OR-NOTHING ( -- )
   CANARY DST SPAN:FILL
   [: ENCODE-SHORT ;] E-SPAN-CAPACITY TTHROWSQ
   13 UNTOUCHED
   [: ENCODE-RELATIVE ;] E-URI-RELATIVE TTHROWSQ
   13 UNTOUCHED
   [: ENCODE-NEGATIVE ;] E-SPAN-LENGTH TTHROWSQ
   13 UNTOUCHED
   s" /a b" DST 13 SPAN:TAKE URI:PATH>FILE 13 T=
   DST SPAN:$ STR:DATA 13 >str s" file:///a%20b" T$= ;

: TRIP ( n -- )
   {: b:n :}
   [char] / ONE c!
   b ONE 1+ c!
   ONE 2 >str URI$ PATH$ ONE 2 >str T$= ;

\ Every byte comes back from its URI as it went in.
: ROUND-TRIP ( -- )
   256 0 do i TRIP loop ;

: FORM$ ( str -- str )
   URI-DST URI:FORM-ENCODE {: u:n :}
   URI-DST SPAN:$ STR:DATA u >str ;

: FORM-SAFE$ ( -- str )
   s" ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789*-._" ;

: HEX$ ( -- str ) s" 0123456789ABCDEF" ;

: FORM-SAFE? ( n -- bool )
   {: c:n :}
   FORM-SAFE$ {: s:str :}
   false s STR:LEN 0 ?do s STR:DATA i + c@ c = or loop ;

: FORM-WANT ( n -- str )
   {: b:n :}
   b FORM-SAFE? if b WANT c! WANT 1 >str exit then
   b STR:SPACE = if [char] + WANT c! WANT 1 >str exit then
   [char] % WANT c!
   HEX$ STR:DATA b 4 rshift + c@ WANT 1+ c!
   HEX$ STR:DATA b $F and + c@ WANT 2 + c!
   WANT 3 >str ;

: FORM-BYTES ( -- )
   256 0 do i ONE c! ONE 1 >str FORM$ i FORM-WANT T$= loop ;

: DECODED$ ( str -- str )
   DECODED @ 0 VEC:RESIZE
   DECODED @ URI:FORM-DECODE+
   DECODED @ VEC:BYTES ;

\ Every byte comes back from its form encoding as it went in.
: FORM-TRIP ( -- )
   256 0 do i ONE c! ONE 1 >str FORM$ DECODED$ ONE 1 >str T$= loop ;

\ A `%` at the end, with one digit, or with a non-hex digit in either place
\ decodes to itself.
: FORM-BARE-PERCENT ( -- )
   s" %" DECODED$ s" %" T$=
   s" a%4" DECODED$ s" a%4" T$=
   s" %G1%1G" DECODED$ s" %G1%1G" T$= ;

: FRAGMENT$ ( str -- str )
   DECODED @ 0 VEC:RESIZE
   DECODED @ URI:FRAGMENT-DECODE+
   DECODED @ VEC:BYTES ;

\ Every byte a fragment holds as itself is kept, and each escape, in either
\ hex case, is its byte: `%2F` is a `/`. An empty text is empty.
: FRAGMENTS ( -- )
   s" aZ09-._~!$&'()*+,;=:@/?" FRAGMENT$
   s" aZ09-._~!$&'()*+,;=:@/?" T$=
   s" a%2Fb%2fc%7E%7e%41" FRAGMENT$ s" a/b/c~~A" T$=
   s" " FRAGMENT$ STR:LEN 0 T= ;

\ The text appended to `x`; a rejected text leaves `x` alone.
: FRAGMENT-REJECTED ( str -- )
   {: s:str :}
   DECODED @ 0 VEC:RESIZE
   s" x" DECODED @ VEC:APPEND
   [: s DECODED @ URI:FRAGMENT-DECODE+ ;] E-URI-ESCAPE TTHROWSQ
   DECODED @ VEC:BYTES s" x" T$= ;

\ A raw space or `#`, a `%` at the end, before a non-hex digit or with one
\ digit is rejected whole, the bytes before it included.
: FRAGMENT-REJECTIONS ( -- )
   s" a b" FRAGMENT-REJECTED
   s" a#b" FRAGMENT-REJECTED
   s" a%" FRAGMENT-REJECTED
   s" a%G0" FRAGMENT-REJECTED
   s" a%4" FRAGMENT-REJECTED ;

\ `~` and `/`, which a path keeps, are escaped in a form.
: FORM-MIXED ( -- )
   s" a b*-._~/" FORM$ s" a+b*-._%7E%2F" T$= ;

: FORM-SHORT ( -- )   \ `a+%7E` is 5 bytes
   s" a ~" DST 4 SPAN:TAKE URI:FORM-ENCODE drop ;

: FORM-NEGATIVE ( -- )
   s" a" STR:DATA -1 >str DST URI:FORM-ENCODE drop ;

\ A form encoding rejected writes nothing, and a span of exactly its length
\ is enough.
: FORM-ALL-OR-NOTHING ( -- )
   CANARY DST SPAN:FILL
   [: FORM-SHORT ;] E-SPAN-CAPACITY TTHROWSQ
   5 UNTOUCHED
   [: FORM-NEGATIVE ;] E-SPAN-LENGTH TTHROWSQ
   5 UNTOUCHED
   s" a ~" DST 5 SPAN:TAKE URI:FORM-ENCODE 5 T=
   DST SPAN:$ STR:DATA 5 >str s" a+%7E" T$= ;

\ A dot segment in each reading, raw or decoded once, and none decoded twice.
: DOT-SEGMENTS ( -- )
   s" /a/../b" URI:DOT-SEGMENT? TTRUE
   s" /a/./b" URI:DOT-SEGMENT? TTRUE
   s" /a/%2e%2E/b" URI:DOT-SEGMENT? TTRUE
   s" /a/..%2fb" URI:DOT-SEGMENT? TTRUE
   s" /a/..%5Cb" URI:DOT-SEGMENT? TTRUE
   s" /a\..\b" URI:DOT-SEGMENT? TTRUE
   s" /a/..;v=1/b" URI:DOT-SEGMENT? TTRUE
   s" /a/..%3bv/b" URI:DOT-SEGMENT? TTRUE
   s" /a/b..c/d" URI:DOT-SEGMENT? TFALSE
   s" /a/.../b" URI:DOT-SEGMENT? TFALSE
   s" /a/x?$filter=../y" URI:DOT-SEGMENT? TFALSE
   s" /sap/opu/odata/sap/API_PRODUCT_SRV/A_Product('0000020701_R001')"
   URI:DOT-SEGMENT? TFALSE
   s" /sap/opu/odata/..#x" URI:DOT-SEGMENT? TTRUE
   s" /sap/opu/odata/..%2f..%2fbc%2fping" URI:DOT-SEGMENT? TTRUE
   s" /sap/opu/odata/..%2F.." URI:DOT-SEGMENT? TTRUE
   s" /sap/opu/odata/..%5c..%5cbc%5cping" URI:DOT-SEGMENT? TTRUE
   s" /sap/opu/odata/..%3bx/..%3bx/bc/ping" URI:DOT-SEGMENT? TTRUE
   s" /a/..%3fx" URI:DOT-SEGMENT? TTRUE
   s" /a%3f/../b" URI:DOT-SEGMENT? TTRUE
   s" /a/..%23x" URI:DOT-SEGMENT? TTRUE
   s" /a/..%2e" STR:DATA 5 >str URI:DOT-SEGMENT? TTRUE
   s" /a/%2e" STR:DATA 5 >str URI:DOT-SEGMENT? TFALSE
   s" /a/.." URI:DOT-SEGMENT? TTRUE
   s" .." URI:DOT-SEGMENT? TTRUE
   s" /a/%252e%252e/b" URI:DOT-SEGMENT? TFALSE
   s" " URI:DOT-SEGMENT? TFALSE ;

: RESOLVED$ ( str str -- str )
   URL-DST URI:RESOLVE {: u:n :}
   URL-DST SPAN:$ STR:DATA u >str ;

: RESOLVE= ( str str str -- )   \ base, reference, target
   {: b:str r:str t:str :}
   b r RESOLVED$ t T$= ;

: CATALOG$ ( -- str )
   s" https://h/sap/opu/odata4/iwfnd/config/default/iwfnd/catalog/0002/ServiceGroups?$expand=X&$top=3" ;

: SERVICE$ ( -- str )
   s" https://h/sap/opu/odata/iwfnd/CATALOGSERVICE;v=0002/ServiceCollection?$skiptoken=2" ;

: NEXT$ ( -- str )
   s" https://h/sap/opu/odata4/iwfnd/config/default/iwfnd/catalog/0002/ServiceGroups?$expand=X&$skiptoken=100" ;

\ The four pages an OData server linked: relative, absolute with a segment
\ parameter kept, absolute path, and another host.
: PAGES ( -- )
   CATALOG$ s" ServiceGroups?$expand=X&$skiptoken=100" NEXT$ RESOLVE=
   s" https://h/sap/opu/odata/iwfnd/CATALOGSERVICE;v=0002/ServiceCollection?$top=2"
   SERVICE$ SERVICE$ RESOLVE=
   CATALOG$ s" /sap/opu/odata/sap/X/A?$skiptoken=5"
   s" https://h/sap/opu/odata/sap/X/A?$skiptoken=5" RESOLVE=
   CATALOG$ s" https://other/x" s" https://other/x" RESOLVE= ;

: RFC= ( str str -- )   \ reference, target
   {: r:str t:str :}
   s" http://a/b/c/d;p?q" r t RESOLVE= ;

: RFC-DROP ( str -- )
   {: r:str :}
   s" http://a/b/c/d;p?q" r URL-DST URI:RESOLVE drop ;

\ RFC 3986 section 5.4.1, a scheme other than http(s) rejected.
: RFC-NORMAL ( -- )
   [: s" g:h" RFC-DROP ;] E-URI-HTTP TTHROWSQ
   s" g" s" http://a/b/c/g" RFC=
   s" ./g" s" http://a/b/c/g" RFC=
   s" g/" s" http://a/b/c/g/" RFC=
   s" /g" s" http://a/g" RFC=
   s" //g" s" http://g" RFC=
   s" ?y" s" http://a/b/c/d;p?y" RFC=
   s" g?y" s" http://a/b/c/g?y" RFC=
   s" #s" s" http://a/b/c/d;p?q#s" RFC=
   s" g#s" s" http://a/b/c/g#s" RFC=
   s" g?y#s" s" http://a/b/c/g?y#s" RFC=
   s" ;x" s" http://a/b/c/;x" RFC=
   s" g;x" s" http://a/b/c/g;x" RFC=
   s" g;x?y#s" s" http://a/b/c/g;x?y#s" RFC=
   s" " s" http://a/b/c/d;p?q" RFC=
   s" ." s" http://a/b/c/" RFC=
   s" ./" s" http://a/b/c/" RFC=
   s" .." s" http://a/b/" RFC=
   s" ../" s" http://a/b/" RFC=
   s" ../g" s" http://a/b/g" RFC=
   s" ../.." s" http://a/" RFC=
   s" ../../" s" http://a/" RFC=
   s" ../../g" s" http://a/g" RFC= ;

\ RFC 3986 section 5.4.2, a scheme without `//` rejected.
: RFC-ABNORMAL ( -- )
   s" ../../../g" s" http://a/g" RFC=
   s" ../../../../g" s" http://a/g" RFC=
   s" /./g" s" http://a/g" RFC=
   s" /../g" s" http://a/g" RFC=
   s" g." s" http://a/b/c/g." RFC=
   s" .g" s" http://a/b/c/.g" RFC=
   s" g.." s" http://a/b/c/g.." RFC=
   s" ..g" s" http://a/b/c/..g" RFC=
   s" ./../g" s" http://a/b/g" RFC=
   s" ./g/." s" http://a/b/c/g/" RFC=
   s" g/./h" s" http://a/b/c/g/h" RFC=
   s" g/../h" s" http://a/b/c/h" RFC=
   s" g;x=1/./y" s" http://a/b/c/g;x=1/y" RFC=
   s" g;x=1/../y" s" http://a/b/c/y" RFC=
   s" g?y/./x" s" http://a/b/c/g?y/./x" RFC=
   s" g?y/../x" s" http://a/b/c/g?y/../x" RFC=
   s" g#s/./x" s" http://a/b/c/g#s/./x" RFC=
   s" g#s/../x" s" http://a/b/c/g#s/../x" RFC=
   [: s" http:g" RFC-DROP ;] E-URI-HTTP TTHROWSQ ;

: BASE-DROP ( str -- )
   s" g" URL-DST URI:RESOLVE drop ;

\ User information, a `\` before it, and a port over 65535 or an unclosed
\ bracket in the base; user information in a reference.
: BAD-AUTHORITIES ( -- )
   [: s" https://u@h/x" BASE-DROP ;] E-URI-HOST TTHROWSQ
   [: s" https://h\@evil/x" BASE-DROP ;] E-URI-HOST TTHROWSQ
   [: s" https://h:99999/" BASE-DROP ;] E-URI-HOST TTHROWSQ
   [: s" https://[::1/" BASE-DROP ;] E-URI-HOST TTHROWSQ
   [: s" //u@evil/x" RFC-DROP ;] E-URI-HOST TTHROWSQ
   [: s" https://h\@evil/x" RFC-DROP ;] E-URI-HOST TTHROWSQ ;

\ A first segment whose bytes before its `:` do not form a scheme is a relative path.
: COLON-PATHS ( -- )
   s" A(D=datetime'2020-01-01T00:00')?$skiptoken=1"
   s" http://a/b/c/A(D=datetime'2020-01-01T00:00')?$skiptoken=1" RFC=
   s" 1a:b" s" http://a/b/c/1a:b" RFC= ;

: RESOLVE-SHORT ( -- )   \ `http://a/b/c/g` is 14 bytes
   s" http://a/b/c/d;p?q" s" g" DST 13 SPAN:TAKE URI:RESOLVE drop ;

\ A span short of the target writes nothing; one of exactly its length is
\ enough.
: RESOLVE-ALL-OR-NOTHING ( -- )
   CANARY DST SPAN:FILL
   [: RESOLVE-SHORT ;] E-SPAN-CAPACITY TTHROWSQ
   14 UNTOUCHED
   s" http://a/b/c/d;p?q" s" g" DST 14 SPAN:TAKE URI:RESOLVE 14 T=
   DST SPAN:$ STR:DATA 14 >str s" http://a/b/c/g" T$= ;

\ A reference resolved against an http(s) base: a server's pages, RFC 3986's
\ examples, paths holding `:`, authorities rejected and a short span.
: RESOLVES ( -- )
   PAGES
   RFC-NORMAL
   RFC-ABNORMAL
   COLON-PATHS
   BAD-AUTHORITIES
   RESOLVE-ALL-OR-NOTHING ;

\ The default port, scheme and host in any case, an empty port, a bracketed
\ host; another port, scheme or host.
: SAME-ORIGINS ( -- )
   s" https://h/a" s" HTTPS://H:443/b" URI:SAME-ORIGIN? TTRUE
   s" https://h:/" s" https://h/" URI:SAME-ORIGIN? TTRUE
   s" https://[::1]:443/" s" https://[::1]/" URI:SAME-ORIGIN? TTRUE
   s" https://h/a" s" https://h:8443/" URI:SAME-ORIGIN? TFALSE
   s" https://h/a" s" http://h/" URI:SAME-ORIGIN? TFALSE
   s" https://h/a" s" https://other/x" URI:SAME-ORIGIN? TFALSE ;

\ An origin alone in each form; a path, a fragment, user information, another
\ scheme or none, no host, a bad or unclosed address, and a bad port.
: ORIGINS-ALONE ( -- )
   s" https://h" URI:ORIGIN? TTRUE
   s" HTTP://h:8080" URI:ORIGIN? TTRUE
   s" https://[::1]:443" URI:ORIGIN? TTRUE
   s" https://h:" URI:ORIGIN? TTRUE
   s" https://h/" URI:ORIGIN? TFALSE
   s" https://h#f" URI:ORIGIN? TFALSE
   s" https://u@h" URI:ORIGIN? TFALSE
   s" ftp://h" URI:ORIGIN? TFALSE
   s" https://" URI:ORIGIN? TFALSE
   s" https://[::1" URI:ORIGIN? TFALSE
   s" https://[::1g]" URI:ORIGIN? TFALSE
   s" https://h:65536" URI:ORIGIN? TFALSE
   s" https://h:x8" URI:ORIGIN? TFALSE
   s" " URI:ORIGIN? TFALSE ;

\ The path and query without the fragment; none after a bare authority.
: REQUEST-TARGETS ( -- )
   s" https://h/sap/X/A?$skiptoken=5#f" URI:REQUEST-TARGET$
   s" /sap/X/A?$skiptoken=5" T$=
   s" https://h" URI:REQUEST-TARGET$ STR:LEN 0 T= ;

\ A space as `%20`, quotes and the delimiters a query gives meaning to
\ escaped; a second encoding appended after the first.
: QUERY-ENCODES ( -- )
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: v:^ :}
   s" a+b&c=d/" v URI:QUERY-ENCODE
   v VEC:SPAN SPAN:$ s" a%2Bb%26c%3Dd%2F" T$=
   s" MaterialDocument eq '4900037654'" v URI:QUERY-ENCODE
   v VEC:SPAN SPAN:$
   s" a%2Bb%26c%3Dd%2FMaterialDocument%20eq%20%274900037654%27" T$=
   v VEC:DELETE ;

\ The vector's own bytes encoded into it while its block moves.
: QUERY-OWN ( -- )
   ALLOC-TEST:MOVING 0 [type] u8 VEC:NEW {: v:^ :}
   s" a b/" v VEC:APPEND
   v VEC:SPAN SPAN:$ v URI:QUERY-ENCODE
   v VEC:SPAN SPAN:$ s" a b/a%20b%2F" T$=
   v VEC:DELETE ;

: MAIN ( -- )
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW DECODED !
   T-RESET
   FORMS
   ESCAPES
   SCHEMES
   AUTHORITIES
   ESCAPE-REJECTIONS
   ALL-OR-NOTHING
   ENCODE-FORMS
   ENCODE-ESCAPES
   RELATIVES
   ENCODE-ALL-OR-NOTHING
   ROUND-TRIP
   FORM-BYTES
   FORM-TRIP
   FORM-BARE-PERCENT
   FORM-MIXED
   FORM-ALL-OR-NOTHING
   FRAGMENTS
   FRAGMENT-REJECTIONS
   DOT-SEGMENTS
   RESOLVES
   SAME-ORIGINS
   ORIGINS-ALONE
   REQUEST-TARGETS
   QUERY-ENCODES
   QUERY-OWN
   T-REPORT
   DECODED @ VEC:DELETE
   s" uri-test: ok" type cr ;

MAIN

;package
