\ uri.f - file URIs (RFC 8089) decoded to the paths they name, paths encoded,
\ bytes encoded as a web form or a query component and decoded from a form
\ or a fragment (RFC 3986 section 3.5, without its `#`),
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
   {: s:str :}   \ an authority naming this machine
   s STR:LEN 0= if true exit then
   s LOCALHOST$ STR:=CI ;

: PATH-AT ( str -- n )
   {: t:str :}
   SCHEME$ STR:LEN {: s:n :}
   t STR:LEN s < if E-URI-SCHEME throw then
   t STR:DATA s >str SCHEME$ STR:=CI 0= if E-URI-SCHEME throw then
   t STR:DATA s + t STR:LEN s - >str MARKER$ STR:STARTS-WITH? 0= if E-URI-AUTHORITY throw then
   s MARKER$ STR:LEN + {: from:n :}
   t STR:DATA from + t STR:LEN from - >str SLASH STR:INDEX-OF MATCH option
      none OF E-URI-AUTHORITY throw ENDOF
      some OF IDX>N ENDOF
   ;MATCH {: len:n :}
   t STR:DATA from + len >str LOCAL? 0= if E-URI-AUTHORITY throw then
   from len + ;

: NIBBLE ( n -- n )
   {: c:n :}
   HEX-DIGITS c STR:ASCII-UPPER STR:INDEX-OF MATCH option
      none OF E-URI-ESCAPE throw ENDOF
      some OF IDX>N ENDOF
   ;MATCH ;

: BYTE-AT ( n str -- n n )
   {: i:n s:str :}
   s STR:DATA i + c@ {: c:n :}
   c QUESTION = c HASH = or if E-URI-ESCAPE throw then
   c PERCENT <> if c i 1+ exit then
   i ESCAPE-WIDTH + s STR:LEN > if E-URI-ESCAPE throw then
   s STR:DATA i + 1+ c@ NIBBLE NIBBLE-BITS lshift
   s STR:DATA i + 2 + c@ NIBBLE or
   i ESCAPE-WIDTH + ;

: MEASURE ( str -- n )
   {: s:str :}
   0 0 begin dup s STR:LEN < while
      s BYTE-AT nip swap 1+ swap
   repeat drop ;

: PUT ( n n str ^u8 -- n n )
   {: at:n i:n s:str out:^ :}
   i s BYTE-AT {: b:n next:n :}
   b out at + c!
   at 1+ next ;

: DECODE ( str ^u8 -- )
   {: s:str out:^ :}
   0 0 begin dup s STR:LEN < while s out PUT repeat 2drop ;

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
   {: s:str kept :}
   0 s STR:LEN 0 ?do s STR:DATA i + c@ kept execute if 1 else ESCAPE-WIDTH then + loop ;

: ESCAPE ( n ^u8 n -- n )
   {: c:n out:^ at:n :}
   PERCENT out at + c!
   HEX-DIGITS STR:DATA c NIBBLE-BITS rshift + c@ out at + 1+ c!
   HEX-DIGITS STR:DATA c $F and + c@ out at + 2 + c!
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
   {: s:str at:n :}
   s STR:DATA at + c@ {: c:n :}
   c PERCENT <> if c at 1+ exit then
   at ESCAPE-WIDTH + s STR:LEN > if c at 1+ exit then
   s STR:DATA at + 1+ c@ {: hi:n :}
   s STR:DATA at + 2 + c@ {: lo:n :}
   hi STR:HEX-DIGIT? lo STR:HEX-DIGIT? and 0= if c at 1+ exit then
   hi NIBBLE NIBBLE-BITS lshift lo NIBBLE or at ESCAPE-WIDTH + ;

\ The byte form-encoded text spells at that offset, and the offset after it:
\ `+` is a space.
: DECODED-AT ( str n -- n n )
   {: s:str at:n :}
   s STR:DATA at + c@ [char] + = if STR:SPACE at 1+ exit then
   s at PERCENT-AT ;

: SUB-DELIM? ( n -- bool )   \ RFC 3986's `!$&'()*+,;=`
   {: c:n :}
   s" !$&'()*+,;=" c STR:INDEX-OF MATCH option
      none OF false ENDOF
      some OF drop true ENDOF
   ;MATCH ;

\ The bytes a fragment holds as themselves (RFC 3986 section 3.5): unreserved,
\ sub-delims, `:`, `@`, `/` and `?`.
: FRAGMENT-BYTE? ( n -- bool )
   {: c:n :}
   c UNRESERVED? c SUB-DELIM? or  c [char] : = or  c [char] @ = or
   c SLASH = or  c QUESTION = or ;

\ The byte a fragment's text spells at that offset, and the offset after it;
\ E-URI-ESCAPE for a byte a fragment does not hold as itself, or a `%`
\ without two hex digits after it, which PERCENT-AT answers as itself.
: FRAGMENT-AT ( str n -- n n )
   {: s:str at:n :}
   s at PERCENT-AT {: b:n next:n :}
   next at - ESCAPE-WIDTH =  s STR:DATA at + c@ FRAGMENT-BYTE?  or
   0= if E-URI-ESCAPE throw then
   b next ;

: FRAGMENT-CHECK ( str -- )
   {: s:str :}
   0 begin dup s STR:LEN < while s swap FRAGMENT-AT nip repeat drop ;

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
   {: dots:n at:n s:str :}
   s STR:DATA at + c@ {: raw:n :}
   raw QUESTION = raw HASH = or if dots s STR:LEN false exit then
   s at PERCENT-AT {: c:n next:n :}
   c SEPARATOR? if 0 next dots DOTS? exit then
   c SEMICOLON = c QUESTION = or c HASH = or if
      NOT-DOT next dots DOTS? exit then
   c [char] . = if dots 1+ NOT-DOT min next false exit then
   NOT-DOT next false ;

\ The first offset below end whose byte answers true to test, or end.
: SCAN ( str [ n -- bool ] -- n )
   {: s:str test :}
   0 begin dup s STR:LEN < if dup s STR:DATA + c@ test execute 0= else false then
   while 1+ repeat ;

: QUERY-OR-HASH? ( n -- bool )
   {: c:n :}
   c QUESTION = c HASH = or ;

: AUTHORITY-END? ( n -- bool )
   {: c:n :}
   c SLASH = c QUERY-OR-HASH? or ;

\ The offset of the first `?` or `#` at or after from, or the end.
: PATH-END ( str n -- n )
   {: s:str from:n :}
   s STR:DATA from + s STR:LEN from - >str [: QUERY-OR-HASH? ;] SCAN from + ;

: HASH-END ( str -- n )
   [: HASH = ;] SCAN ;

: ADDRESS-BYTE? ( n -- bool )
   {: c:n :}
   c STR:HEX-DIGIT? c [char] : = or  c [char] . = or ;

\ The length of a bracketed address, brackets included, or 0 for none.
: BRACKETED ( str -- n )
   {: s:str :}
   s STR:DATA 1+ s STR:LEN 1- >str [: ADDRESS-BYTE? 0= ;] SCAN 1+ {: close:n :}
   close 1 = close s STR:LEN = or if 0 exit then
   s STR:DATA close + c@ CLOSE-BRACKET <> if 0 exit then
   close 1+ ;

\ The length of the host an authority starts with.
: HOST-LEN ( str -- n )
   {: s:str :}
   s STR:LEN 0 > if s STR:DATA c@ OPEN-BRACKET = if s BRACKETED exit then then
   s [: UNRESERVED? 0= ;] SCAN ;

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
   {: s:str :}
   s STR:LEN 0= if NO-PORT exit then
   s STR:DATA c@ [char] : <> if BAD-PORT exit then
   s STR:LEN 1 = if NO-PORT exit then
   0 s STR:LEN 1 ?do s STR:DATA i + c@ PORT-DIGIT loop ;

\ The host, the port and the length of the authority the bytes after `//`
\ start with, which ends at the first `/`, `?` or `#`, or at the end.
: AUTHORITY ( str -- str n n )
   {: s:str :}
   s [: AUTHORITY-END? ;] SCAN {: end:n :}
   s STR:DATA end >str HOST-LEN {: h:n :}
   h 0= if s STR:DATA 0 >str BAD-PORT end exit then
   s STR:DATA h >str  s STR:DATA h + end h - >str PORT  end ;

: SCHEME-IS? ( str str -- bool )
   {: t:str s:str :}
   SEPARATOR$ {: m:str :}
   s STR:LEN m STR:LEN + t STR:LEN > if false exit then
   t STR:DATA s STR:LEN >str s STR:=CI  t STR:DATA s STR:LEN + m STR:LEN >str m STR:= and ;

\ The length of the scheme, `http` or `https` in any case, before `://`, or
\ 0 for another.
: SCHEME-LEN ( str -- n )
   {: s:str :}
   s HTTP$ SCHEME-IS? if HTTP$ STR:LEN exit then
   s HTTPS$ SCHEME-IS? 0= if 0 exit then
   HTTPS$ STR:LEN ;

: DEFAULT-PORT ( n n -- n )   \ a scheme's length, a port
   {: s:n p:n :}
   p NO-PORT <> if p exit then
   s HTTP$ STR:LEN = if HTTP-PORT else HTTPS-PORT then ;

\ An http(s) URL's scheme length, host, port (the scheme's default when absent
\ or empty) and authority end, then a code: 0, E-URI-HTTP or E-URI-HOST.
: HTTP-READ ( str -- n str n n n )
   {: t:str :}
   t SCHEME-LEN {: s:n :}
   s 0= if 0 t STR:DATA 0 >str 0 0 E-URI-HTTP exit then
   s SEPARATOR$ STR:LEN + {: from:n :}
   t STR:DATA from + t STR:LEN from - >str AUTHORITY {: h:str p:n end:n :}
   p BAD-PORT = if 0 t STR:DATA 0 >str 0 0 E-URI-HOST exit then
   s h  s p DEFAULT-PORT  from end +  0 ;

\ HTTP-READ's answers but its code, which is thrown unless 0.
: HTTP-AT ( str -- n str n n )
   HTTP-READ {: code:n :}
   code 0<> if code throw then ;

: HTTP-END ( str -- n )
   HTTP-AT {: s:n h:str p:n end:n :} end ;

\ The offset of the last `/` below w, or 0.
: LAST-SLASH ( str -- n )
   {: s:str :}
   s STR:LEN begin dup 0 > if 1- dup s STR:DATA + c@ SLASH = else true then until ;

: SLASH-AT ( str -- n )
   {: s:str :}
   SLASH s STR:DATA s STR:LEN + c!  s STR:LEN 1+ ;

\ Moves the segment at r, its `/` and the bytes before the next `/`, to w.
: SEGMENT ( ^u8 n n n -- n n )
   {: a:^ u:n w:n r:n :}
   a r 1+ + u r 1+ - >str [: SLASH = ;] SCAN r 1+ + {: e:n :}
   a r + a w + e r - BYTE-COPY
   w e r - +  e ;

\ One step of RFC 3986 section 5.2.4 over the input at r, the output below w.
\ The input always starts with `/`, so rules A and D never apply.
: DOT-STEP ( str n n -- n n )
   {: s:str w:n r:n :}
   s STR:DATA r + s STR:LEN r - >str {: in:str :}
   in s" /./" STR:STARTS-WITH? if w r 2 + exit then
   in s" /." STR:= if s STR:DATA w >str SLASH-AT s STR:LEN exit then
   in s" /../" STR:STARTS-WITH? if s STR:DATA w >str LAST-SLASH r 3 + exit then
   in s" /.." STR:= if s STR:DATA  s STR:DATA w >str LAST-SLASH >str  SLASH-AT s STR:LEN exit then
   s STR:DATA s STR:LEN w r SEGMENT ;

\ Removes the dot segments of a path that is empty or starts with `/`, in
\ place; answers its new length.
: DOT-SEGMENTS ( str -- n )
   {: s:str :}
   0 0 begin dup s STR:LEN < while s -rot DOT-STEP repeat drop ;

: SCHEME-BYTE? ( n -- bool )
   {: c:n :}
   c ALNUM? c [char] + = or  c [char] - = or  c [char] . = or ;

\ Whether a reference has a scheme: an ASCII letter, then letters, digits,
\ `+`, `-` or `.`, then `:` (RFC 3986 section 3.1).
: SCHEME? ( str -- bool )
   {: s:str :}
   s [: SCHEME-BYTE? 0= ;] SCAN {: at:n :}
   at 0= at s STR:LEN = or if false exit then
   s STR:DATA c@ ALNUM? s STR:DATA c@ STR:DIGIT? 0= and  s STR:DATA at + c@ [char] : = and ;

: STARTS? ( str n -- bool )   \ whether the first byte is the byte
   {: s:str c:n :}
   s STR:LEN 0 > if s STR:DATA c@ c = exit then
   false ;

\ Each reference form below answers the target's pieces: the number of the
\ base's bytes it starts with, 1 when a `/` follows them, else 0, then the
\ offsets where the target's path starts and ends.

: ABSOLUTE ( str -- n n n n )
   {: r:str :}
   r HTTP-END {: e:n :}
   0 0 e  r e PATH-END ;

\ `//` and an authority: the base's scheme and `:`, then the reference.
: NETWORK ( str str -- n n n n )
   {: b:str r:str :}
   b SCHEME-LEN 1+ {: al:n :}
   MARKER$ STR:LEN {: m:n :}
   r STR:DATA m + r STR:LEN m - >str AUTHORITY {: h:str p:n end:n :}
   p BAD-PORT = if E-URI-HOST throw then
   al 0  al m + end +  r m end + PATH-END al + ;

: ABSOLUTE-PATH ( str n -- n n n n )
   {: r:str be:n :}
   be 0 be  r 0 PATH-END be + ;

: QUERY-ONLY ( str n -- n n n n )
   {: b:str be:n :}
   b be PATH-END {: bp:n :}
   bp 0 be bp ;

\ Empty, or a fragment alone: the base up to its fragment.
: SAME-DOCUMENT ( str n -- n n n n )
   {: b:str be:n :}
   b be PATH-END {: bp:n :}
   b HASH-END 0 be bp ;

\ A relative path, after the base path's last `/`, or after `/` for an empty
\ base path (RFC 3986 section 5.2.3).
: MERGE ( str n str -- n n n n )
   {: b:str be:n r:str :}
   b be PATH-END {: bp:n :}
   r 0 PATH-END {: rp:n :}
   bp be = if be 1 be be 1+ rp + exit then
   b STR:DATA bp >str LAST-SLASH 1+ {: al:n :}
   al 0 be al rp + ;

: PIECES ( str str -- n n n n )
   {: b:str r:str :}
   b HTTP-END {: be:n :}
   r SCHEME? if r ABSOLUTE exit then
   r MARKER$ STR:STARTS-WITH? if b r NETWORK exit then
   r SLASH STARTS? if r be ABSOLUTE-PATH exit then
   r QUESTION STARTS? if b be QUERY-ONLY exit then
   r STR:LEN 0= r HASH STARTS? or if b be SAME-DOCUMENT exit then
   b be r MERGE ;

\ Removes the dot segments of the path between from and to, then moves the
\ bytes after it up; answers the target's length.
: SQUEEZE ( str n n -- n )
   {: out:str from:n to:n :}
   out STR:DATA from + to from - >str DOT-SEGMENTS from + {: pe:n :}
   out STR:DATA to + out STR:DATA pe + out STR:LEN to - BYTE-COPY
   pe out STR:LEN to - + ;

public

\ Writes `file://`, an empty authority, then the path with every byte but `/`
\ and RFC 3986's unreserved ones (letters, digits, `-._~`) escaped as
\ upper-case `%XX`, so FILE>PATH gives back any path. A relative path is
\ E-URI-RELATIVE, a short span E-SPAN-CAPACITY, a negative length E-SPAN-LENGTH.
: PATH>FILE ( str SPAN:span<u8> -- n )
   {: p:str s :}
   p STR:LEN 0 < if E-SPAN-LENGTH throw then
   p STR:LEN 0= if E-URI-RELATIVE throw then
   p STR:DATA c@ SLASH <> if E-URI-RELATIVE throw then
   PREFIX$ {: f:str :}
   f STR:LEN p [: KEPT? ;] ENCODED-LEN + {: len:n :}
   s SPAN:$ dup STR:LEN len < if E-SPAN-CAPACITY throw then STR:DATA {: out:^ :}
   f STR:DATA out f STR:LEN BYTE-COPY
   f STR:LEN p STR:LEN 0 ?do p STR:DATA i + c@ out rot [: KEPT? ;] ENCODE-BYTE loop drop
   len ;

\ The number of bytes FORM-ENCODE writes for the bytes.
: FORM-LEN ( str -- n )
   {: s:str :}
   0 s STR:LEN 0 ?do s STR:DATA i + c@ FORM-WIDTH + loop ;

\ Writes the bytes as application/x-www-form-urlencoded: ASCII letters, digits
\ and `*-._` as they are, a space as `+`, every other byte as upper-case `%XX`.
: FORM-ENCODE ( str SPAN:span<u8> -- n )
   {: t:str s :}
   t STR:LEN 0 < if E-SPAN-LENGTH throw then
   t FORM-LEN {: len:n :}
   s SPAN:$ dup STR:LEN len < if E-SPAN-CAPACITY throw then STR:DATA {: out:^ :}
   0 t STR:LEN 0 ?do t STR:DATA i + c@ out rot FORM-BYTE loop drop
   len ;

\ Appends the bytes application/x-www-form-urlencoded text decodes to
\ (DECODED-AT). The text must not lie in the vector: a push may move its block.
: FORM-DECODE+ ( str ^VEC:vector<u8> -- )
   {: s:str v:^ :}
   0 begin dup s STR:LEN < while
      s swap DECODED-AT swap v VEC:PUSH c!
   repeat drop ;

\ Appends the bytes a URI fragment's text, without its `#`, spells (RFC 3986
\ section 3.5): each byte a fragment holds as itself as it is, each `%XX`, in
\ either hex case, as its byte. Any other byte, or a `%` without two hex
\ digits, is E-URI-ESCAPE, before a byte is appended. The text must not lie
\ in the vector: a push may move its block.
: FRAGMENT-DECODE+ ( str ^VEC:vector<u8> -- )
   {: s:str v:^ :}
   s FRAGMENT-CHECK
   0 begin dup s STR:LEN < while
      s swap PERCENT-AT swap v VEC:PUSH c!
   repeat drop ;

\ Whether form-encoded text decodes to exactly the name; recurses once per
\ byte of the name.
: FORM= ( str str -- bool )
   {: a:str b:str :}
   a STR:LEN 0= if b STR:LEN 0= exit then
   b STR:LEN 0= if false exit then
   a 0 DECODED-AT {: c:n next:n :}
   c b STR:DATA c@ <> if false exit then
   a STR:DATA next + a STR:LEN next - >str b STR:DATA 1+ b STR:LEN 1- >str RECURSE ;

\ Whether any segment of the path, up to a raw `?` or `#`, is `.` or `..` in a
\ reading a client or server may make: raw or percent-decoded once, `/` or `\`
\ separating segments, a segment's text ending at its first `;`, or decoded `?`
\ or `#`. One decoded pass covers the raw reading: no separator, `;`, `?` or
\ `#` is a hex digit, so decoding never joins or hides one. `%252e` is no dot:
\ RFC 3986 section 2.4 forbids decoding twice.
: DOT-SEGMENT? ( str -- bool )
   {: s:str :}
   0 0 begin dup s STR:LEN < while
      s SEGMENT-STEP if 2drop true exit then
   repeat drop DOTS? ;

\ Appends the bytes as an RFC 3986 query component: unreserved bytes as they
\ are, every other byte, a space included, as upper-case `%XX`.
: QUERY-ENCODE ( str ^VEC:vector<u8> -- )
   {: s:str v:^ :}
   s STR:LEN 0 < if E-SPAN-LENGTH throw then
   s [: UNRESERVED? ;] ENCODED-LEN {: len:n :}
   v VEC:COUNT {: at:n :}
   v at len + s STR:DATA VEC:MAKE-ROOM {: src:^ :}
   v at len + VEC:ENSURE
   v VEC:BYTES STR:DATA {: out:^ :}
   at s STR:LEN 0 ?do src i + c@ out rot [: UNRESERVED? ;] ENCODE-BYTE loop drop ;

\ The path and query of an http(s) URL: its bytes from the authority's end
\ to the first `#` or the end; no copy. An empty path stays empty, where
\ RFC 9112's origin-form sends `/`. Rejected as HTTP-AT rejects.
: REQUEST-TARGET$ ( str -- str )
   {: s:str :}
   s HTTP-END {: from:n :}
   s HASH-END {: to:n :}
   s STR:DATA from + to from - >str ;

\ The scheme, the host, brackets kept, and the port HTTP-AT reads from an
\ http(s) URL. Rejected as HTTP-AT rejects.
: ORIGIN ( str -- str str n )
   {: t:str :}
   t HTTP-AT {: s:n h:str p:n end:n :}
   t STR:DATA s >str h p ;

\ Whether the text is an http(s) URL's origin alone: `http://` or `https://`, a
\ host and an optional port as HTTP-READ reads them, and no byte after.
: ORIGIN? ( str -- bool )
   {: t:str :}
   t HTTP-READ {: s:n h:str p:n end:n code:n :}
   code 0= end t STR:LEN = and ;

\ Whether two http(s) URLs share an origin: schemes equal without case, hosts
\ without ASCII case, ports as numbers, defaults applied. Rejected as HTTP-AT
\ rejects.
: SAME-ORIGIN? ( str str -- bool )
   {: a:str b:str :}
   a ORIGIN {: s:str h:str p:n :}
   b ORIGIN {: t:str k:str q:n :}
   s t STR:=CI  h k STR:=CI and  p q = and ;

\ Writes the URL a reference names against an http(s) base (RFC 3986 section
\ 5.2.2, strict): a reference with a scheme must itself be an http(s) URL,
\ and the target's path loses its dot segments; nothing else is normalised.
\ A base or reference rejected as HTTP-AT rejects throws that, a span short
\ of the target before dot-segment removal E-SPAN-CAPACITY, a negative
\ reference length E-SPAN-LENGTH.
: RESOLVE ( str str SPAN:span<u8> -- n )
   {: b:str r:str s :}
   r STR:LEN 0 < if E-SPAN-LENGTH throw then
   b r PIECES {: al:n ml:n from:n to:n :}
   al ml + r STR:LEN + {: len:n :}
   s SPAN:$ dup STR:LEN len < if E-SPAN-CAPACITY throw then STR:DATA {: out:^ :}
   b STR:DATA out al BYTE-COPY
   ml 0 > if SLASH out al + c! then
   r STR:DATA out al + ml + r STR:LEN BYTE-COPY
   out len >str from to SQUEEZE ;

\ The scheme is `file` in any case, then `//` and an authority that is empty or
\ `localhost` in any case, ending at the path's `/`: every answer is absolute.
\ Each `%XX` decodes once, in either hex case; any other byte is copied, so a
\ client that left a byte unescaped still names its file. A bare `?` or `#` is
\ rejected: RFC 8089 has no query or fragment, and a generic parser would split
\ the path there and name a different file. Another scheme is E-URI-SCHEME,
\ anything else before the path E-URI-AUTHORITY, a `%` without two hex digits
\ or a bare `?` or `#` E-URI-ESCAPE, a short span E-SPAN-CAPACITY and a
\ negative length E-SPAN-LENGTH.
: FILE>PATH ( str SPAN:span<u8> -- n )
   {: t:str s :}
   t STR:LEN 0 < if E-SPAN-LENGTH throw then
   t PATH-AT {: at:n :}
   t STR:DATA at + t STR:LEN at - >str {: p:str :}
   p MEASURE {: len:n :}
   s SPAN:$ dup STR:LEN len < if E-SPAN-CAPACITY throw then STR:DATA {: out:^ :}
   p out DECODE
   len ;

private

: DOT ( str n -- n )
   {: s:str from:n :}
   s STR:LEN from ?do s STR:DATA i + c@ [char] . = if i unloop exit then loop s STR:LEN ;

: V6-LOOPBACK ( -- str ) s" [::1]" ;

\ Decimal 0 to 255 without a leading zero, which a resolver may read as
\ octal.
: OCTET? ( str -- bool )
   {: s:str :}
   s STR:LEN 1 < s STR:LEN 3 > or if false exit then
   s STR:DIGITS? 0= if false exit then
   s STR:LEN 1 > s STR:DATA c@ [char] 0 = and if false exit then
   s s" 255" STR:DIGITS<= ;

\ A dotted-quad IPv4 literal in 127.0.0.0/8.
: LOOPBACK-V4? ( str -- bool )
   {: h:str :}
   h s" 127." STR:STARTS-WITH? 0= if false exit then
   h 4 DOT {: d1:n :}
   h d1 1+ DOT {: d2:n :}
   d2 h STR:LEN >= if false exit then
   h d2 1+ DOT h STR:LEN <> if false exit then
   h STR:DATA 4 + d1 4 - >str OCTET?
   h STR:DATA d1 + 1+ d2 d1 - 1- >str OCTET? and
   h STR:DATA d2 + 1+ h STR:LEN d2 - 1- >str OCTET? and ;

\ True for an https URL, or an http one whose host is a loopback IP literal;
\ ORIGIN's rejection for one it cannot read.
: SECURE? ( str -- bool )
   ORIGIN drop {: s:str h:str :}
   s s" https" STR:=CI if true exit then
   h V6-LOOPBACK STR:= if true exit then
   h LOOPBACK-V4? ;

;package
