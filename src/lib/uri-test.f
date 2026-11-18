split-str
\ uri-test.f - file URIs decoded to paths, paths encoded to file URIs, bytes
\ encoded as a web form or a query component and decoded from a form, request
\ paths checked for a dot segment, and http(s) URLs resolved, compared and
\ sliced, through src/lib/uri.f.
\ Run: bin/hb --load src/lib/uri-test.f

require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
require src/lib/span.f
require src/lib/byte-buffer.f
require src/lib/uri.f

package URI-TEST
using SPAN

64 SPAN-BUFFER: DST
64 SPAN-BUFFER: URI-DST
128 SPAN-BUFFER: URL-DST
2 BUFFER: ONE                     \ `/` and the byte a round trip takes
3 BUFFER: WANT                    \ one byte's form encoding
$A5 constant CANARY
create DECODED BUF:HDR-BYTES allot

: PATH$ ( ^u8 n -- ^u8 n )
   DST URI:FILE>PATH {: u:n :}
   DST $ drop u ;

: PATH= ( ^u8 n ^u8 n -- )
   {: a:^ u:n b:^ v:n :}
   a u PATH$ b v T$= ;

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
: ESCAPE-REFUSALS ( -- )
   [: s" file:///a%" DECODE-DROP ;] E-URI-ESCAPE TTHROWSQ
   [: s" file:///a%4" DECODE-DROP ;] E-URI-ESCAPE TTHROWSQ
   [: s" file:///a%G1" DECODE-DROP ;] E-URI-ESCAPE TTHROWSQ
   [: s" file:///a%1G" DECODE-DROP ;] E-URI-ESCAPE TTHROWSQ
   [: s" file:///a?b" DECODE-DROP ;] E-URI-ESCAPE TTHROWSQ
   [: s" file:///a#b" DECODE-DROP ;] E-URI-ESCAPE TTHROWSQ ;

: UNTOUCHED ( n -- )
   {: n:n :}
   n 0 ?do DST i U8@ CANARY T= loop ;

: FOUR-SHORT ( -- )
   s" file:///abcd" DST 4 TAKE URI:FILE>PATH drop ;

: LATE-ESCAPE ( -- )
   s" file:///abcd%G" DECODE-DROP ;

: NEGATIVE-LENGTH ( -- )
   s" file:///x" drop -1 DECODE-DROP ;

\ A refusal writes nothing: the whole URI is checked, and the path's length
\ measured against the span, before the first byte lands. A span of exactly the
\ path's length is enough.
: ALL-OR-NOTHING ( -- )
   CANARY DST FILL
   [: FOUR-SHORT ;] E-SPAN-CAPACITY TTHROWSQ
   5 UNTOUCHED
   [: LATE-ESCAPE ;] E-URI-ESCAPE TTHROWSQ
   5 UNTOUCHED
   [: NEGATIVE-LENGTH ;] E-SPAN-LENGTH TTHROWSQ
   s" file:///abcd" DST 5 TAKE URI:FILE>PATH 5 T=
   DST $ drop 5 s" /abcd" T$= ;

: URI$ ( str -- str )
   URI-DST URI:PATH>FILE {: u:n :}
   URI-DST $ drop u ;

: URI= ( str str -- )
   {: a:^ u:n b:^ v:n :}
   a u URI$ b v T$= ;

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
   s" /a b" DST 12 TAKE URI:PATH>FILE drop ;

: ENCODE-RELATIVE ( -- )
   s" a b" DST URI:PATH>FILE drop ;

: ENCODE-NEGATIVE ( -- )
   s" /x" drop -1 DST URI:PATH>FILE drop ;

\ An encoding refused writes nothing, and a span of exactly the URI's length
\ is enough.
: ENCODE-ALL-OR-NOTHING ( -- )
   CANARY DST FILL
   [: ENCODE-SHORT ;] E-SPAN-CAPACITY TTHROWSQ
   13 UNTOUCHED
   [: ENCODE-RELATIVE ;] E-URI-RELATIVE TTHROWSQ
   13 UNTOUCHED
   [: ENCODE-NEGATIVE ;] E-SPAN-LENGTH TTHROWSQ
   13 UNTOUCHED
   s" /a b" DST 13 TAKE URI:PATH>FILE 13 T=
   DST $ drop 13 s" file:///a%20b" T$= ;

: TRIP ( n -- )
   {: b:n :}
   [char] / ONE c!
   b ONE 1+ c!
   ONE 2 URI$ PATH$ ONE 2 T$= ;

\ Every byte comes back from its URI as it went in.
: ROUND-TRIP ( -- )
   256 0 do i TRIP loop ;

: FORM$ ( str -- str )
   URI-DST URI:FORM-ENCODE {: u:n :}
   URI-DST $ drop u ;

: FORM-SAFE$ ( -- str )
   s" ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789*-._" ;

: HEX$ ( -- ^u8 n ) s" 0123456789ABCDEF" ;

: FORM-SAFE? ( n -- bool )
   {: c:n :}
   FORM-SAFE$ {: a:^ u:n :}
   false u 0 ?do a i + c@ c = or loop ;

: FORM-WANT ( n -- str )
   {: b:n :}
   b FORM-SAFE? if b WANT c! WANT 1 exit then
   b STR-SPACE = if [char] + WANT c! WANT 1 exit then
   [char] % WANT c!
   HEX$ drop b 4 rshift + c@ WANT 1+ c!
   HEX$ drop b $F and + c@ WANT 2 + c!
   WANT 3 ;

: FORM-BYTES ( -- )
   256 0 do i ONE c! ONE 1 FORM$ i FORM-WANT T$= loop ;

: DECODED$ ( str -- str )
   DECODED BUF:CLEAR
   DECODED URI:FORM-DECODE+
   DECODED BUF:SPAN$ BUF:BLEN>N ;

\ Every byte comes back from its form encoding as it went in.
: FORM-TRIP ( -- )
   256 0 do i ONE c! ONE 1 FORM$ DECODED$ ONE 1 T$= loop ;

\ A `%` at the end, with one digit, or with a non-hex digit in either place
\ decodes to itself.
: FORM-BARE-PERCENT ( -- )
   s" %" DECODED$ s" %" T$=
   s" a%4" DECODED$ s" a%4" T$=
   s" %G1%1G" DECODED$ s" %G1%1G" T$= ;

\ `~` and `/`, which a path keeps, are escaped in a form.
: FORM-MIXED ( -- )
   s" a b*-._~/" FORM$ s" a+b*-._%7E%2F" T$= ;

: FORM-SHORT ( -- )   \ `a+%7E` is 5 bytes
   s" a ~" DST 4 TAKE URI:FORM-ENCODE drop ;

: FORM-NEGATIVE ( -- )
   s" a" drop -1 DST URI:FORM-ENCODE drop ;

\ A form encoding rejected writes nothing, and a span of exactly its length
\ is enough.
: FORM-ALL-OR-NOTHING ( -- )
   CANARY DST FILL
   [: FORM-SHORT ;] E-SPAN-CAPACITY TTHROWSQ
   5 UNTOUCHED
   [: FORM-NEGATIVE ;] E-SPAN-LENGTH TTHROWSQ
   5 UNTOUCHED
   s" a ~" DST 5 TAKE URI:FORM-ENCODE 5 T=
   DST $ drop 5 s" a+%7E" T$= ;

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
   s" /a/..%2e" drop 5 URI:DOT-SEGMENT? TTRUE
   s" /a/%2e" drop 5 URI:DOT-SEGMENT? TFALSE
   s" /a/.." URI:DOT-SEGMENT? TTRUE
   s" .." URI:DOT-SEGMENT? TTRUE
   s" /a/%252e%252e/b" URI:DOT-SEGMENT? TFALSE
   s" " URI:DOT-SEGMENT? TFALSE ;

: RESOLVED$ ( str str -- str )
   URL-DST URI:RESOLVE {: u:n :}
   URL-DST $ drop u ;

: RESOLVE= ( str str str -- )   \ base, reference, target
   {: b:^ bu:n r:^ ru:n t:^ tu:n :}
   b bu r ru RESOLVED$ t tu T$= ;

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
   {: r:^ ru:n t:^ tu:n :}
   s" http://a/b/c/d;p?q" r ru t tu RESOLVE= ;

: RFC-DROP ( str -- )
   {: r:^ ru:n :}
   s" http://a/b/c/d;p?q" r ru URL-DST URI:RESOLVE drop ;

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
   s" http://a/b/c/d;p?q" s" g" DST 13 TAKE URI:RESOLVE drop ;

\ A span short of the target writes nothing; one of exactly its length is
\ enough.
: RESOLVE-ALL-OR-NOTHING ( -- )
   CANARY DST FILL
   [: RESOLVE-SHORT ;] E-SPAN-CAPACITY TTHROWSQ
   14 UNTOUCHED
   s" http://a/b/c/d;p?q" s" g" DST 14 TAKE URI:RESOLVE 14 T=
   DST $ drop 14 s" http://a/b/c/g" T$= ;

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

\ The path and query without the fragment; none after a bare authority.
: REQUEST-TARGETS ( -- )
   s" https://h/sap/X/A?$skiptoken=5#f" URI:REQUEST-TARGET$
   s" /sap/X/A?$skiptoken=5" T$=
   s" https://h" URI:REQUEST-TARGET$ nip 0 T= ;

: QUERY$ ( ^u8 n -- ^u8 n )
   URI-DST URI:QUERY-ENCODE {: u:n :}
   URI-DST $ drop u ;

: QUERY-SHORT ( -- )   \ `a%20b` is 5 bytes
   s" a b" DST 4 TAKE URI:QUERY-ENCODE drop ;

\ A space as `%20`, quotes and the delimiters a query gives meaning to
\ escaped; a short span writes nothing.
: QUERY-ENCODES ( -- )
   s" MaterialDocument eq '4900037654'" QUERY$
   s" MaterialDocument%20eq%20%274900037654%27" T$=
   s" a+b&c=d/" QUERY$ s" a%2Bb%26c%3Dd%2F" T$=
   CANARY DST FILL
   [: QUERY-SHORT ;] E-SPAN-CAPACITY TTHROWSQ
   5 UNTOUCHED ;

: MAIN ( -- )
   DECODED 16 BUF:N>BLEN BUF:INIT
   T-RESET
   FORMS
   ESCAPES
   SCHEMES
   AUTHORITIES
   ESCAPE-REFUSALS
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
   DOT-SEGMENTS
   RESOLVES
   SAME-ORIGINS
   REQUEST-TARGETS
   QUERY-ENCODES
   T-REPORT
   DECODED BUF:DISPOSE
   s" uri-test: ok" type cr ;

MAIN

;using
;package
