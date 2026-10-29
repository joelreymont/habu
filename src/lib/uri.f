\ uri.f - file URIs (RFC 8089) decoded to the paths they name, and paths
\ encoded. A path is bytes, as the file system takes them: a decoded NUL or `/`
\ is kept, and src/lib/fs.f refuses a NUL where it opens a path (E-FFI-NUL).
\ Input is checked and output measured before a byte is written, so no refusal
\ leaves a byte behind.
\ STORAGE CLASS. CALLER-OWNED: the module keeps no state.

require src/lib/errors.f
require src/lib/string.f
require src/lib/span.f
require src/lib/adt/option.f

package URI
private

$25 constant PERCENT
$2F constant SLASH
$3F constant QUESTION
$23 constant HASH
3 constant ESCAPE-WIDTH          \ `%` and two hex digits
4 constant NIBBLE-BITS

: SCHEME$ ( -- ptr u8 n ) s" file:" ;
: MARKER$ ( -- ptr u8 n ) s" //" ;
: PREFIX$ ( -- ptr u8 n ) s" file://" ;
: LOCALHOST$ ( -- ptr u8 n ) s" localhost" ;
: HEX-DIGITS ( -- ptr u8 n ) s" 0123456789ABCDEF" ;

: LOCAL? ( ptr u8 n -- bool )
   {: a:ptr u:n :}   \ an authority naming this machine
   u 0= if true exit then
   a u LOCALHOST$ STR=CI ;

: PATH-AT ( ptr u8 n -- n )
   {: a:ptr u:n :}
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

: BYTE-AT ( n ptr u8 n -- n n )
   {: i:n a:ptr u:n :}
   a i + c@ {: c:n :}
   c QUESTION = c HASH = or if E-URI-ESCAPE throw then
   c PERCENT <> if c i 1+ exit then
   i ESCAPE-WIDTH + u > if E-URI-ESCAPE throw then
   a i + 1+ c@ NIBBLE NIBBLE-BITS lshift
   a i + 2 + c@ NIBBLE or
   i ESCAPE-WIDTH + ;

: MEASURE ( ptr u8 n -- n )
   {: a:ptr u:n :}
   0 0 begin dup u < while
      a u BYTE-AT nip swap 1+ swap
   repeat drop ;

: PUT ( n n ptr u8 n ptr u8 -- n n )
   {: at:n i:n a:ptr u:n out:ptr :}
   i a u BYTE-AT {: b:n next:n :}
   b out at + c!
   at 1+ next ;

: DECODE ( ptr u8 n ptr u8 -- )
   {: a:ptr u:n out:ptr :}
   0 0 begin dup u < while a u out PUT repeat 2drop ;

: KEPT? ( n -- bool )
   {: c:n :}
   c ASCII-UPPER {: up:n :}
   up [char] A >= up [char] Z <= and
   c [char] 0 >= c [char] 9 <= and or
   c [char] - = or  c [char] . = or  c [char] _ = or  c [char] ~ = or
   c SLASH = or ;

: WIDTH ( n -- n )
   KEPT? if 1 else ESCAPE-WIDTH then ;

: ENCODED-LEN ( ptr u8 n -- n )
   {: a:ptr u:n :}
   0 u 0 ?do a i + c@ WIDTH + loop ;

: ENCODE-BYTE ( n ptr u8 n -- n )
   {: c:n out:ptr at:n :}
   c KEPT? if c out at + c! at 1+ exit then
   PERCENT out at + c!
   HEX-DIGITS drop c NIBBLE-BITS rshift + c@ out at + 1+ c!
   HEX-DIGITS drop c $F and + c@ out at + 2 + c!
   at ESCAPE-WIDTH + ;

public

\ Writes `file://`, an empty authority, then the path with every byte but `/`
\ and RFC 3986's unreserved ones (letters, digits, `-._~`) escaped as
\ upper-case `%XX`, so FILE>PATH gives back any path. A relative path is
\ E-URI-RELATIVE, a short span E-SPAN-CAPACITY, a negative length E-SPAN-LENGTH.
: PATH>FILE ( ptr u8 n SPAN:span<u8> -- n )
   {: a:ptr u:n s :}
   u 0 < if E-SPAN-LENGTH throw then
   u 0= if E-URI-RELATIVE throw then
   a c@ SLASH <> if E-URI-RELATIVE throw then
   PREFIX$ {: f:ptr fu:n :}
   fu a u ENCODED-LEN + {: len:n :}
   s SPAN:$ len < if E-SPAN-CAPACITY throw then {: out:ptr :}
   f out fu BYTE-COPY
   fu u 0 ?do a i + c@ out rot ENCODE-BYTE loop drop
   len ;

\ The scheme is `file` in any case, then `//` and an authority that is empty or
\ `localhost` in any case, ending at the path's `/`: every answer is absolute.
\ Each `%XX` decodes once, in either hex case; any other byte is copied, so a
\ client that left a byte unescaped still names its file. A bare `?` or `#` is
\ refused: RFC 8089 has no query or fragment, and a generic parser would split
\ the path there and name a different file. Another scheme is E-URI-SCHEME,
\ anything else before the path E-URI-AUTHORITY, a `%` without two hex digits
\ or a bare `?` or `#` E-URI-ESCAPE, a short span E-SPAN-CAPACITY and a
\ negative length E-SPAN-LENGTH.
: FILE>PATH ( ptr u8 n SPAN:span<u8> -- n )
   {: a:ptr u:n s :}
   u 0 < if E-SPAN-LENGTH throw then
   a u PATH-AT {: at:n :}
   a at + u at - {: p:ptr pu:n :}
   p pu MEASURE {: len:n :}
   s SPAN:$ len < if E-SPAN-CAPACITY throw then {: out:ptr :}
   p pu out DECODE
   len ;

;package
