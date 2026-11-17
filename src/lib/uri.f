\ uri.f - file URIs (RFC 8089) decoded to the paths they name, paths encoded,
\ bytes encoded as a web form and decoded from one, and whether a request path
\ holds a dot segment. A file path is bytes, as the file system takes them: a
\ decoded NUL or `/` is kept.
\ Input is checked and output measured before a byte is written, so no refusal
\ leaves a byte behind.
\ STORAGE CLASS. CALLER-OWNED: the module keeps no state.

require src/lib/error.f
require src/lib/string.f
require src/lib/span.f
require src/lib/adt/option.f
require src/lib/byte-buffer.f

package URI
private

$25 constant PERCENT
$2F constant SLASH
$3F constant QUESTION
$23 constant HASH
$5C constant BACKSLASH
$3B constant SEMICOLON
3 constant ESCAPE-WIDTH          \ `%` and two hex digits
4 constant NIBBLE-BITS
3 constant NOT-DOT               \ a segment's text that is no `.` or `..`

: SCHEME$ ( -- ^u8 n ) s" file:" ;
: MARKER$ ( -- ^u8 n ) s" //" ;
: PREFIX$ ( -- ^u8 n ) s" file://" ;
: LOCALHOST$ ( -- ^u8 n ) s" localhost" ;
: HEX-DIGITS ( -- ^u8 n ) s" 0123456789ABCDEF" ;

: LOCAL? ( ^u8 n -- bool )
   {: a:^ u:n :}   \ an authority naming this machine
   u 0= if true exit then
   a u LOCALHOST$ STR=CI ;

: PATH-AT ( ^u8 n -- n )
   {: a:^ u:n :}
   SCHEME$ nip {: s:n :}
   u s < if E-URI-SCHEME throw then
   a s SCHEME$ STR=CI 0= if E-URI-SCHEME throw then
   a s + u s - MARKER$ STARTS-WITH? 0= if E-URI-AUTHORITY throw then
   s MARKER$ nip + {: from:n :}
   a from + u from - SLASH INDEX-OF MATCH option
      none OF E-URI-AUTHORITY throw ENDOF
      some OF IDX>N ENDOF
   ;MATCH {: len:n :}
   a from + len LOCAL? 0= if E-URI-AUTHORITY throw then
   from len + ;

: NIBBLE ( n -- n )
   {: c:n :}
   HEX-DIGITS c ASCII-UPPER INDEX-OF MATCH option
      none OF E-URI-ESCAPE throw ENDOF
      some OF IDX>N ENDOF
   ;MATCH ;

: BYTE-AT ( n ^u8 n -- n n )
   {: i:n a:^ u:n :}
   a i + c@ {: c:n :}
   c QUESTION = c HASH = or if E-URI-ESCAPE throw then
   c PERCENT <> if c i 1+ exit then
   i ESCAPE-WIDTH + u > if E-URI-ESCAPE throw then
   a i + 1+ c@ NIBBLE NIBBLE-BITS lshift
   a i + 2 + c@ NIBBLE or
   i ESCAPE-WIDTH + ;

: MEASURE ( ^u8 n -- n )
   {: a:^ u:n :}
   0 0 begin dup u < while
      a u BYTE-AT nip swap 1+ swap
   repeat drop ;

: PUT ( n n ^u8 n ^u8 -- n n )
   {: at:n i:n a:^ u:n out:^ :}
   i a u BYTE-AT {: b:n next:n :}
   b out at + c!
   at 1+ next ;

: DECODE ( ^u8 n ^u8 -- )
   {: a:^ u:n out:^ :}
   0 0 begin dup u < while a u out PUT repeat 2drop ;

: ALNUM? ( n -- bool )
   {: c:n :}
   c ASCII-UPPER {: up:n :}
   up [char] A >= up [char] Z <= and
   c [char] 0 >= c [char] 9 <= and or ;

: KEPT? ( n -- bool )
   {: c:n :}
   c ALNUM?
   c [char] - = or  c [char] . = or  c [char] _ = or  c [char] ~ = or
   c SLASH = or ;

: WIDTH ( n -- n )
   KEPT? if 1 else ESCAPE-WIDTH then ;

: ENCODED-LEN ( ^u8 n -- n )
   {: a:^ u:n :}
   0 u 0 ?do a i + c@ WIDTH + loop ;

: ESCAPE ( n ^u8 n -- n )
   {: c:n out:^ at:n :}
   PERCENT out at + c!
   HEX-DIGITS drop c NIBBLE-BITS rshift + c@ out at + 1+ c!
   HEX-DIGITS drop c $F and + c@ out at + 2 + c!
   at ESCAPE-WIDTH + ;

: ENCODE-BYTE ( n ^u8 n -- n )
   {: c:n out:^ at:n :}
   c KEPT? if c out at + c! at 1+ exit then
   c out at ESCAPE ;

: FORM-KEPT? ( n -- bool )
   {: c:n :}
   c ALNUM?
   c [char] * = or  c [char] - = or  c [char] . = or  c [char] _ = or ;

: FORM-WIDTH ( n -- n )
   {: c:n :}
   c FORM-KEPT? c STR-SPACE = or if 1 else ESCAPE-WIDTH then ;

: FORM-BYTE ( n ^u8 n -- n )
   {: c:n out:^ at:n :}
   c FORM-KEPT? if c out at + c! at 1+ exit then
   c STR-SPACE = if [char] + out at + c! at 1+ exit then
   c out at ESCAPE ;

\ The byte at that offset with one `%XX` decoded, and the offset after it; a
\ `%` without two hex digits after it is itself (WHATWG URL 1.3).
: PERCENT-AT ( ^u8 n n -- n n )
   {: a:^ u:n at:n :}
   a at + c@ {: c:n :}
   c PERCENT <> if c at 1+ exit then
   at ESCAPE-WIDTH + u > if c at 1+ exit then
   a at + 1+ c@ {: hi:n :}
   a at + 2 + c@ {: lo:n :}
   hi STR-HEX-DIGIT? lo STR-HEX-DIGIT? and 0= if c at 1+ exit then
   hi NIBBLE NIBBLE-BITS lshift lo NIBBLE or at ESCAPE-WIDTH + ;

\ The byte form-encoded text spells at that offset, and the offset after it:
\ `+` is a space.
: DECODED-AT ( ^u8 n n -- n n )
   {: a:^ u:n at:n :}
   a at + c@ [char] + = if STR-SPACE at 1+ exit then
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
: SEGMENT-STEP ( n n ^u8 n -- n n bool )
   {: dots:n at:n a:^ u:n :}
   a at + c@ {: raw:n :}
   raw QUESTION = raw HASH = or if dots u false exit then
   a u at PERCENT-AT {: c:n next:n :}
   c SEPARATOR? if 0 next dots DOTS? exit then
   c SEMICOLON = c QUESTION = or c HASH = or if
      NOT-DOT next dots DOTS? exit then
   c [char] . = if dots 1+ NOT-DOT min next false exit then
   NOT-DOT next false ;

public

\ Writes `file://`, an empty authority, then the path with every byte but `/`
\ and RFC 3986's unreserved ones (letters, digits, `-._~`) escaped as
\ upper-case `%XX`, so FILE>PATH gives back any path. A relative path is
\ E-URI-RELATIVE, a short span E-SPAN-CAPACITY, a negative length E-SPAN-LENGTH.
: PATH>FILE ( ^u8 n SPAN:span<u8> -- n )
   {: a:^ u:n s :}
   u 0 < if E-SPAN-LENGTH throw then
   u 0= if E-URI-RELATIVE throw then
   a c@ SLASH <> if E-URI-RELATIVE throw then
   PREFIX$ {: f:^ fu:n :}
   fu a u ENCODED-LEN + {: len:n :}
   s SPAN:$ len < if E-SPAN-CAPACITY throw then {: out:^ :}
   f out fu BYTE-COPY
   fu u 0 ?do a i + c@ out rot ENCODE-BYTE loop drop
   len ;

\ The number of bytes FORM-ENCODE writes for the bytes.
: FORM-LEN ( ^u8 n -- n )
   {: a:^ u:n :}
   0 u 0 ?do a i + c@ FORM-WIDTH + loop ;

\ Writes the bytes as application/x-www-form-urlencoded: ASCII letters, digits
\ and `*-._` as they are, a space as `+`, every other byte as upper-case `%XX`.
: FORM-ENCODE ( ^u8 n SPAN:span<u8> -- n )
   {: a:^ u:n s :}
   u 0 < if E-SPAN-LENGTH throw then
   a u FORM-LEN {: len:n :}
   s SPAN:$ len < if E-SPAN-CAPACITY throw then {: out:^ :}
   0 u 0 ?do a i + c@ out rot FORM-BYTE loop drop
   len ;

\ Appends to the BUF the bytes application/x-www-form-urlencoded text decodes
\ to (DECODED-AT).
: FORM-DECODE+ ( ^u8 n ^a -- )
   {: a:^ u:n buf:^ :}
   0 begin dup u < while
      a u rot DECODED-AT swap buf BUF:APPEND-BYTE
   repeat drop ;

\ Whether form-encoded text decodes to exactly the name; recurses once per
\ byte of the name.
: FORM= ( ^u8 n ^u8 n -- bool )
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
: DOT-SEGMENT? ( ^u8 n -- bool )
   {: a:^ u:n :}
   0 0 begin dup u < while
      a u SEGMENT-STEP if 2drop true exit then
   repeat drop DOTS? ;

\ The scheme is `file` in any case, then `//` and an authority that is empty or
\ `localhost` in any case, ending at the path's `/`: every answer is absolute.
\ Each `%XX` decodes once, in either hex case; any other byte is copied, so a
\ client that left a byte unescaped still names its file. A bare `?` or `#` is
\ refused: RFC 8089 has no query or fragment, and a generic parser would split
\ the path there and name a different file. Another scheme is E-URI-SCHEME,
\ anything else before the path E-URI-AUTHORITY, a `%` without two hex digits
\ or a bare `?` or `#` E-URI-ESCAPE, a short span E-SPAN-CAPACITY and a
\ negative length E-SPAN-LENGTH.
: FILE>PATH ( ^u8 n SPAN:span<u8> -- n )
   {: a:^ u:n s :}
   u 0 < if E-SPAN-LENGTH throw then
   a u PATH-AT {: at:n :}
   a at + u at - {: p:^ pu:n :}
   p pu MEASURE {: len:n :}
   s SPAN:$ len < if E-SPAN-CAPACITY throw then {: out:^ :}
   p pu out DECODE
   len ;

;package
