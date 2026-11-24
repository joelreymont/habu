split-str
\ uri.f - file URIs (RFC 8089) decoded to the paths they name, paths encoded,
\ bytes encoded as a web form or a query component and decoded from a form,
\ whether a request path holds a dot segment, and http(s) URLs read: one's
\ origin, whether a text is an origin alone, a reference resolved against
\ one, two compared by origin, and the request target sliced out. A file path
\ is bytes, as the file system takes them: a decoded NUL or `/` is kept.
\ Input is checked and output measured before a byte is written, so no
\ rejection leaves a byte behind. RESOLVE removes dot segments in place, after
\ it writes the target: the removal never lengthens the path.
\ STORAGE CLASS. CALLER-OWNED: the module keeps no state.

require src/lib/error.f
require src/lib/string.f
require src/lib/string-option.f
require src/lib/span.f
require src/lib/adt/option.f
require src/lib/vector.f

package URI
private

$25 constant PERCENT
$2F constant SLASH
$3F constant QUESTION
$23 constant HASH
$5C constant BACKSLASH
$3B constant SEMICOLON
$5B constant OPEN-BRACKET
$5D constant CLOSE-BRACKET
3 constant ESCAPE-WIDTH          \ `%` and two hex digits
4 constant NIBBLE-BITS
3 constant NOT-DOT               \ a segment's text that is no `.` or `..`
80 constant HTTP-PORT
443 constant HTTPS-PORT
65535 constant PORT-MAX
-1 constant NO-PORT
-2 constant BAD-PORT             \ not a host and an optional port

: SCHEME$ ( -- str ) s" file:" ;
: MARKER$ ( -- str ) s" //" ;
: PREFIX$ ( -- str ) s" file://" ;
: LOCALHOST$ ( -- str ) s" localhost" ;
: HEX-DIGITS ( -- str ) s" 0123456789ABCDEF" ;
: HTTP$ ( -- str ) s" http" ;
: HTTPS$ ( -- str ) s" https" ;
: SEPARATOR$ ( -- str ) s" ://" ;

: LOCAL? ( str -- bool )
   {: a:^ u:n :}   \ an authority naming this machine
   u 0= if true exit then
   a u LOCALHOST$ STR:=CI ;

: PATH-AT ( str -- n )
   {: a:^ u:n :}
   SCHEME$ nip {: s:n :}
   u s < if E-URI-SCHEME throw then
   a s SCHEME$ STR:=CI 0= if E-URI-SCHEME throw then
   a s + u s - MARKER$ STR:STARTS-WITH? 0= if E-URI-AUTHORITY throw then
   s MARKER$ nip + {: from:n :}
   a from + u from - SLASH STR:INDEX-OF MATCH option
      none OF E-URI-AUTHORITY throw ENDOF
      some OF IDX>N ENDOF
   ;MATCH {: len:n :}
   a from + len LOCAL? 0= if E-URI-AUTHORITY throw then
   from len + ;

: NIBBLE ( n -- n )
   {: c:n :}
   HEX-DIGITS c STR:ASCII-UPPER STR:INDEX-OF MATCH option
      none OF E-URI-ESCAPE throw ENDOF
      some OF IDX>N ENDOF
   ;MATCH ;

: BYTE-AT ( n str -- n n )
   {: i:n a:^ u:n :}
   a i + c@ {: c:n :}
   c QUESTION = c HASH = or if E-URI-ESCAPE throw then
   c PERCENT <> if c i 1+ exit then
   i ESCAPE-WIDTH + u > if E-URI-ESCAPE throw then
   a i + 1+ c@ NIBBLE NIBBLE-BITS lshift
   a i + 2 + c@ NIBBLE or
   i ESCAPE-WIDTH + ;

: MEASURE ( str -- n )
   {: a:^ u:n :}
   0 0 begin dup u < while
      a u BYTE-AT nip swap 1+ swap
   repeat drop ;

: PUT ( n n str ^u8 -- n n )
   {: at:n i:n a:^ u:n out:^ :}
   i a u BYTE-AT {: b:n next:n :}
   b out at + c!
   at 1+ next ;

: DECODE ( str ^u8 -- )
   {: a:^ u:n out:^ :}
   0 0 begin dup u < while a u out PUT repeat 2drop ;

: ALNUM? ( n -- bool )
   {: c:n :}
   c STR:ASCII-UPPER {: up:n :}
   up [char] A >= up [char] Z <= and
   c [char] 0 >= c [char] 9 <= and or ;

\ RFC 3986's unreserved bytes: letters, digits and `-._~`.
: UNRESERVED? ( n -- bool )
   {: c:n :}
   c ALNUM?
   c [char] - = or  c [char] . = or  c [char] _ = or  c [char] ~ = or ;

: KEPT? ( n -- bool )
   {: c:n :}
   c UNRESERVED? c SLASH = or ;

: ENCODED-LEN ( str [ n -- bool ] -- n )
   {: a:^ u:n kept :}
   0 u 0 ?do a i + c@ kept execute if 1 else ESCAPE-WIDTH then + loop ;

: ESCAPE ( n ^u8 n -- n )
   {: c:n out:^ at:n :}
   PERCENT out at + c!
   HEX-DIGITS drop c NIBBLE-BITS rshift + c@ out at + 1+ c!
   HEX-DIGITS drop c $F and + c@ out at + 2 + c!
   at ESCAPE-WIDTH + ;

: ENCODE-BYTE ( n ^u8 n [ n -- bool ] -- n )
   {: c:n out:^ at:n kept :}
   c kept execute if c out at + c! at 1+ exit then
   c out at ESCAPE ;

: FORM-KEPT? ( n -- bool )
   {: c:n :}
   c ALNUM?
   c [char] * = or  c [char] - = or  c [char] . = or  c [char] _ = or ;

: FORM-WIDTH ( n -- n )
   {: c:n :}
   c FORM-KEPT? c STR:SPACE = or if 1 else ESCAPE-WIDTH then ;

: FORM-BYTE ( n ^u8 n -- n )
   {: c:n out:^ at:n :}
   c FORM-KEPT? if c out at + c! at 1+ exit then
   c STR:SPACE = if [char] + out at + c! at 1+ exit then
   c out at ESCAPE ;

\ The byte at that offset with one `%XX` decoded, and the offset after it; a
\ `%` without two hex digits after it is itself (WHATWG URL 1.3).
: PERCENT-AT ( str n -- n n )
   {: a:^ u:n at:n :}
   a at + c@ {: c:n :}
   c PERCENT <> if c at 1+ exit then
   at ESCAPE-WIDTH + u > if c at 1+ exit then
   a at + 1+ c@ {: hi:n :}
   a at + 2 + c@ {: lo:n :}
   hi STR:HEX-DIGIT? lo STR:HEX-DIGIT? and 0= if c at 1+ exit then
   hi NIBBLE NIBBLE-BITS lshift lo NIBBLE or at ESCAPE-WIDTH + ;

\ The byte form-encoded text spells at that offset, and the offset after it:
\ `+` is a space.
: DECODED-AT ( str n -- n n )
   {: a:^ u:n at:n :}
   a at + c@ [char] + = if STR:SPACE at 1+ exit then
   a u at PERCENT-AT ;

: DOTS? ( n -- bool )
   {: dots:n :}
   dots 1 = dots 2 = or ;

: SEPARATOR? ( n -- bool )
   {: c:n :}
   c SLASH = c BACKSLASH = or ;

\ The segment text's dot count after the byte or escape at that offset (NOT-DOT
\ once it holds anything else), the offset after it, and whether a text it
\ closed was `.` or `..`; a raw `?` or `#` answers the path's end, closing none.
: SEGMENT-STEP ( n n str -- n n bool )
   {: dots:n at:n a:^ u:n :}
   a at + c@ {: raw:n :}
   raw QUESTION = raw HASH = or if dots u false exit then
   a u at PERCENT-AT {: c:n next:n :}
   c SEPARATOR? if 0 next dots DOTS? exit then
   c SEMICOLON = c QUESTION = or c HASH = or if
      NOT-DOT next dots DOTS? exit then
   c [char] . = if dots 1+ NOT-DOT min next false exit then
   NOT-DOT next false ;

\ The first offset below end whose byte answers true to test, or end.
: SCAN ( str [ n -- bool ] -- n )
   {: p:^ end:n test :}
   0 begin dup end < if dup p + c@ test execute 0= else false then
   while 1+ repeat ;

: QUERY-OR-HASH? ( n -- bool )
   {: c:n :}
   c QUESTION = c HASH = or ;

: AUTHORITY-END? ( n -- bool )
   {: c:n :}
   c SLASH = c QUERY-OR-HASH? or ;

\ The offset of the first `?` or `#` at or after from, or the end.
: PATH-END ( str n -- n )
   {: a:^ u:n from:n :}
   a from + u from - [: QUERY-OR-HASH? ;] SCAN from + ;

: HASH-END ( str -- n )
   [: HASH = ;] SCAN ;

: ADDRESS-BYTE? ( n -- bool )
   {: c:n :}
   c STR:HEX-DIGIT? c [char] : = or  c [char] . = or ;

\ The length of a bracketed address, brackets included, or 0 for none.
: BRACKETED ( str -- n )
   {: a:^ u:n :}
   a 1+ u 1- [: ADDRESS-BYTE? 0= ;] SCAN 1+ {: close:n :}
   close 1 = close u = or if 0 exit then
   a close + c@ CLOSE-BRACKET <> if 0 exit then
   close 1+ ;

\ The length of the host an authority starts with.
: HOST-LEN ( str -- n )
   {: a:^ u:n :}
   u 0 > if a c@ OPEN-BRACKET = if a u BRACKETED exit then then
   a u [: UNRESERVED? 0= ;] SCAN ;

: PORT-DIGIT ( n n -- n )
   {: acc:n c:n :}
   acc BAD-PORT = if BAD-PORT exit then
   c STR:DIGIT? 0= if BAD-PORT exit then
   acc 10 * c STR:DIGIT-VALUE + {: v:n :}
   v PORT-MAX > if BAD-PORT exit then
   v ;

\ The port the bytes after the host give: none, `:` alone or `:` and up to
\ PORT-MAX in digits; BAD-PORT for anything else.
: PORT ( str -- n )
   {: a:^ u:n :}
   u 0= if NO-PORT exit then
   a c@ [char] : <> if BAD-PORT exit then
   u 1 = if NO-PORT exit then
   0 u 1 ?do a i + c@ PORT-DIGIT loop ;

\ The host, the port and the length of the authority the bytes after `//`
\ start with, which ends at the first `/`, `?` or `#`, or at the end.
: AUTHORITY ( str -- str n n )
   {: a:^ u:n :}
   a u [: AUTHORITY-END? ;] SCAN {: end:n :}
   a end HOST-LEN {: h:n :}
   h 0= if a 0 BAD-PORT end exit then
   a h  a h + end h - PORT  end ;

: SCHEME-IS? ( str str -- bool )
   {: a:^ u:n s:^ v:n :}
   SEPARATOR$ {: m:^ w:n :}
   v w + u > if false exit then
   a v s v STR:=CI  a v + w m w STR:= and ;

\ The length of the scheme, `http` or `https` in any case, before `://`, or
\ 0 for another.
: SCHEME-LEN ( str -- n )
   {: a:^ u:n :}
   a u HTTP$ SCHEME-IS? if HTTP$ nip exit then
   a u HTTPS$ SCHEME-IS? 0= if 0 exit then
   HTTPS$ nip ;

: DEFAULT-PORT ( n n -- n )   \ a scheme's length, a port
   {: s:n p:n :}
   p NO-PORT <> if p exit then
   s HTTP$ nip = if HTTP-PORT else HTTPS-PORT then ;

\ An http(s) URL's scheme length, host, port (the scheme's default when absent
\ or empty) and authority end, then a code: 0, E-URI-HTTP or E-URI-HOST.
: HTTP-READ ( str -- n str n n n )
   {: a:^ u:n :}
   a u SCHEME-LEN {: s:n :}
   s 0= if 0 a 0 0 0 E-URI-HTTP exit then
   s SEPARATOR$ nip + {: from:n :}
   a from + u from - AUTHORITY {: h:^ hu:n p:n end:n :}
   p BAD-PORT = if 0 a 0 0 0 E-URI-HOST exit then
   s h hu  s p DEFAULT-PORT  from end +  0 ;

\ HTTP-READ's answers but its code, which is thrown unless 0.
: HTTP-AT ( str -- n str n n )
   HTTP-READ {: code:n :}
   code 0<> if code throw then ;

: HTTP-END ( str -- n )
   HTTP-AT {: s:n h:^ hu:n p:n end:n :} end ;

\ The offset of the last `/` below w, or 0.
: LAST-SLASH ( str -- n )
   {: a:^ w:n :}
   w begin dup 0 > if 1- dup a + c@ SLASH = else true then until ;

: SLASH-AT ( str -- n )
   {: a:^ w:n :}
   SLASH a w + c!  w 1+ ;

\ Moves the segment at r, its `/` and the bytes before the next `/`, to w.
: SEGMENT ( ^u8 n n n -- n n )
   {: a:^ u:n w:n r:n :}
   a r 1+ + u r 1+ - [: SLASH = ;] SCAN r 1+ + {: e:n :}
   a r + a w + e r - BYTE-COPY
   w e r - +  e ;

\ One step of RFC 3986 section 5.2.4 over the input at r, the output below w.
\ The input always starts with `/`, so rules A and D never apply.
: DOT-STEP ( str n n -- n n )
   {: a:^ u:n w:n r:n :}
   a r + u r - {: in:^ iu:n :}
   in iu s" /./" STR:STARTS-WITH? if w r 2 + exit then
   in iu s" /." STR:= if a w SLASH-AT u exit then
   in iu s" /../" STR:STARTS-WITH? if a w LAST-SLASH r 3 + exit then
   in iu s" /.." STR:= if a  a w LAST-SLASH  SLASH-AT u exit then
   a u w r SEGMENT ;

\ Removes the dot segments of a path that is empty or starts with `/`, in
\ place; answers its new length.
: DOT-SEGMENTS ( str -- n )
   {: a:^ u:n :}
   0 0 begin dup u < while a u 2swap DOT-STEP repeat drop ;

: SCHEME-BYTE? ( n -- bool )
   {: c:n :}
   c ALNUM? c [char] + = or  c [char] - = or  c [char] . = or ;

\ Whether a reference has a scheme: an ASCII letter, then letters, digits,
\ `+`, `-` or `.`, then `:` (RFC 3986 section 3.1).
: SCHEME? ( str -- bool )
   {: a:^ u:n :}
   a u [: SCHEME-BYTE? 0= ;] SCAN {: at:n :}
   at 0= at u = or if false exit then
   a c@ ALNUM? a c@ STR:DIGIT? 0= and  a at + c@ [char] : = and ;

: STARTS? ( str n -- bool )   \ whether the first byte is the byte
   {: a:^ u:n c:n :}
   u 0 > if a c@ c = exit then
   false ;

\ Each reference form below answers the target's pieces: the number of the
\ base's bytes it starts with, 1 when a `/` follows them, else 0, then the
\ offsets where the target's path starts and ends.

: ABSOLUTE ( str -- n n n n )
   {: r:^ ru:n :}
   r ru HTTP-END {: e:n :}
   0 0 e  r ru e PATH-END ;

\ `//` and an authority: the base's scheme and `:`, then the reference.
: NETWORK ( str str -- n n n n )
   {: b:^ bu:n r:^ ru:n :}
   b bu SCHEME-LEN 1+ {: al:n :}
   MARKER$ nip {: m:n :}
   r m + ru m - AUTHORITY {: h:^ hu:n p:n end:n :}
   p BAD-PORT = if E-URI-HOST throw then
   al 0  al m + end +  r ru m end + PATH-END al + ;

: ABSOLUTE-PATH ( str n -- n n n n )
   {: r:^ ru:n be:n :}
   be 0 be  r ru 0 PATH-END be + ;

: QUERY-ONLY ( str n -- n n n n )
   {: b:^ bu:n be:n :}
   b bu be PATH-END {: bp:n :}
   bp 0 be bp ;

\ Empty, or a fragment alone: the base up to its fragment.
: SAME-DOCUMENT ( str n -- n n n n )
   {: b:^ bu:n be:n :}
   b bu be PATH-END {: bp:n :}
   b bu HASH-END 0 be bp ;

\ A relative path, after the base path's last `/`, or after `/` for an empty
\ base path (RFC 3986 section 5.2.3).
: MERGE ( str n str -- n n n n )
   {: b:^ bu:n be:n r:^ ru:n :}
   b bu be PATH-END {: bp:n :}
   r ru 0 PATH-END {: rp:n :}
   bp be = if be 1 be be 1+ rp + exit then
   b bp LAST-SLASH 1+ {: al:n :}
   al 0 be al rp + ;

: PIECES ( str str -- n n n n )
   {: b:^ bu:n r:^ ru:n :}
   b bu HTTP-END {: be:n :}
   r ru SCHEME? if r ru ABSOLUTE exit then
   r ru MARKER$ STR:STARTS-WITH? if b bu r ru NETWORK exit then
   r ru SLASH STARTS? if r ru be ABSOLUTE-PATH exit then
   r ru QUESTION STARTS? if b bu be QUERY-ONLY exit then
   ru 0= r ru HASH STARTS? or if b bu be SAME-DOCUMENT exit then
   b bu be r ru MERGE ;

\ Removes the dot segments of the path between from and to, then moves the
\ bytes after it up; answers the target's length.
: SQUEEZE ( str n n -- n )
   {: out:^ len:n from:n to:n :}
   out from + to from - DOT-SEGMENTS from + {: pe:n :}
   out to + out pe + len to - BYTE-COPY
   pe len to - + ;

public

\ Writes `file://`, an empty authority, then the path with every byte but `/`
\ and RFC 3986's unreserved ones (letters, digits, `-._~`) escaped as
\ upper-case `%XX`, so FILE>PATH gives back any path. A relative path is
\ E-URI-RELATIVE, a short span E-SPAN-CAPACITY, a negative length E-SPAN-LENGTH.
: PATH>FILE ( str SPAN:span<u8> -- n )
   {: a:^ u:n s :}
   u 0 < if E-SPAN-LENGTH throw then
   u 0= if E-URI-RELATIVE throw then
   a c@ SLASH <> if E-URI-RELATIVE throw then
   PREFIX$ {: f:^ fu:n :}
   fu a u [: KEPT? ;] ENCODED-LEN + {: len:n :}
   s SPAN:$ len < if E-SPAN-CAPACITY throw then {: out:^ :}
   f out fu BYTE-COPY
   fu u 0 ?do a i + c@ out rot [: KEPT? ;] ENCODE-BYTE loop drop
   len ;

\ The number of bytes FORM-ENCODE writes for the bytes.
: FORM-LEN ( str -- n )
   {: a:^ u:n :}
   0 u 0 ?do a i + c@ FORM-WIDTH + loop ;

\ Writes the bytes as application/x-www-form-urlencoded: ASCII letters, digits
\ and `*-._` as they are, a space as `+`, every other byte as upper-case `%XX`.
: FORM-ENCODE ( str SPAN:span<u8> -- n )
   {: a:^ u:n s :}
   u 0 < if E-SPAN-LENGTH throw then
   a u FORM-LEN {: len:n :}
   s SPAN:$ len < if E-SPAN-CAPACITY throw then {: out:^ :}
   0 u 0 ?do a i + c@ out rot FORM-BYTE loop drop
   len ;

\ Appends the bytes application/x-www-form-urlencoded text decodes to
\ (DECODED-AT). The text must not lie in the vector: a push may move its block.
: FORM-DECODE+ ( str ^VEC:vector<u8> -- )
   {: a:^ u:n v:^ :}
   0 begin dup u < while
      a u rot DECODED-AT swap v VEC:PUSH c!
   repeat drop ;

\ Whether form-encoded text decodes to exactly the name; recurses once per
\ byte of the name.
: FORM= ( str str -- bool )
   {: a:^ u:n b:^ v:n :}
   u 0= if v 0= exit then
   v 0= if false exit then
   a u 0 DECODED-AT {: c:n next:n :}
   c b c@ <> if false exit then
   a next + u next - b 1+ v 1- RECURSE ;

\ Whether any segment of the path, up to a raw `?` or `#`, is `.` or `..` in a
\ reading a client or server may make: raw or percent-decoded once, `/` or `\`
\ separating segments, a segment's text ending at its first `;`, or decoded `?`
\ or `#`. One decoded pass covers the raw reading: no separator, `;`, `?` or
\ `#` is a hex digit, so decoding never joins or hides one. `%252e` is no dot:
\ RFC 3986 section 2.4 forbids decoding twice.
: DOT-SEGMENT? ( str -- bool )
   {: a:^ u:n :}
   0 0 begin dup u < while
      a u SEGMENT-STEP if 2drop true exit then
   repeat drop DOTS? ;

\ Appends the bytes as an RFC 3986 query component: unreserved bytes as they
\ are, every other byte, a space included, as upper-case `%XX`.
: QUERY-ENCODE ( str ^VEC:vector<u8> -- )
   {: a:^ u:n v:^ :}
   u 0 < if E-SPAN-LENGTH throw then
   a u [: UNRESERVED? ;] ENCODED-LEN {: len:n :}
   v VEC:COUNT {: at:n :}
   v at len + a VEC:MAKE-ROOM {: src:^ :}
   v at len + VEC:ENSURE
   v VEC:SPAN SPAN:$ drop {: out:^ :}
   at u 0 ?do src i + c@ out rot [: UNRESERVED? ;] ENCODE-BYTE loop drop ;

\ The path and query of an http(s) URL: its bytes from the authority's end
\ to the first `#` or the end; no copy. An empty path stays empty, where
\ RFC 9112's origin-form sends `/`. Rejected as HTTP-AT rejects.
: REQUEST-TARGET$ ( str -- str )
   {: a:^ u:n :}
   a u HTTP-END {: from:n :}
   a u HASH-END {: to:n :}
   a from + to from - ;

\ The scheme, the host, brackets kept, and the port HTTP-AT reads from an
\ http(s) URL. Rejected as HTTP-AT rejects.
: ORIGIN ( str -- str str n )
   {: a:^ u:n :}
   a u HTTP-AT {: s:n h:^ hu:n p:n end:n :}
   a s h hu p ;

\ Whether the text is an http(s) URL's origin alone: `http://` or `https://`, a
\ host and an optional port as HTTP-READ reads them, and no byte after.
: ORIGIN? ( str -- bool )
   {: a:^ u:n :}
   a u HTTP-READ {: s:n h:^ hu:n p:n end:n code:n :}
   code 0= end u = and ;

\ Whether two http(s) URLs share an origin: schemes equal without case, hosts
\ without ASCII case, ports as numbers, defaults applied. Rejected as HTTP-AT
\ rejects.
: SAME-ORIGIN? ( str str -- bool )
   {: a:^ u:n b:^ v:n :}
   a u ORIGIN {: s:^ su:n h:^ hu:n p:n :}
   b v ORIGIN {: t:^ tu:n k:^ ku:n q:n :}
   s su t tu STR:=CI  h hu k ku STR:=CI and  p q = and ;

\ Writes the URL a reference names against an http(s) base (RFC 3986 section
\ 5.2.2, strict): a reference with a scheme must itself be an http(s) URL,
\ and the target's path loses its dot segments; nothing else is normalised.
\ A base or reference rejected as HTTP-AT rejects throws that, a span short
\ of the target before dot-segment removal E-SPAN-CAPACITY, a negative
\ reference length E-SPAN-LENGTH.
: RESOLVE ( str str SPAN:span<u8> -- n )
   {: b:^ bu:n r:^ ru:n s :}
   ru 0 < if E-SPAN-LENGTH throw then
   b bu r ru PIECES {: al:n ml:n from:n to:n :}
   al ml + ru + {: len:n :}
   s SPAN:$ len < if E-SPAN-CAPACITY throw then {: out:^ :}
   b out al BYTE-COPY
   ml 0 > if SLASH out al + c! then
   r out al + ml + ru BYTE-COPY
   out len from to SQUEEZE ;

\ The scheme is `file` in any case, then `//` and an authority that is empty or
\ `localhost` in any case, ending at the path's `/`: every answer is absolute.
\ Each `%XX` decodes once, in either hex case; any other byte is copied, so a
\ client that left a byte unescaped still names its file. A bare `?` or `#` is
\ refused: RFC 8089 has no query or fragment, and a generic parser would split
\ the path there and name a different file. Another scheme is E-URI-SCHEME,
\ anything else before the path E-URI-AUTHORITY, a `%` without two hex digits
\ or a bare `?` or `#` E-URI-ESCAPE, a short span E-SPAN-CAPACITY and a
\ negative length E-SPAN-LENGTH.
: FILE>PATH ( str SPAN:span<u8> -- n )
   {: a:^ u:n s :}
   u 0 < if E-SPAN-LENGTH throw then
   a u PATH-AT {: at:n :}
   a at + u at - {: p:^ pu:n :}
   p pu MEASURE {: len:n :}
   s SPAN:$ len < if E-SPAN-CAPACITY throw then {: out:^ :}
   p pu out DECODE
   len ;

;package
