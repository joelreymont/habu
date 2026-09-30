\ uri.f - file URIs, decoded to the paths they name.
\
\ FILE>PATH decodes a `file:` URI that names a file on this machine into the
\ caller's span and answers the path's length. The scheme is `file` in any
\ case; `//` and an authority that is empty or `localhost`, in any case, must
\ follow it, and the authority ends at the `/` that starts the path, so every
\ answer is an absolute path. Each `%XX` decodes, once, to the byte it names, in
\ either hex case, and every other byte is copied as it is, so a client that
\ left a byte unescaped still names its file. A bare `?` or `#` is refused
\ rather than copied: RFC 8089's file URI has no query or fragment, and a
\ generic parser would split the path there, so the two readings would name
\ different files.
\
\ A scheme other than `file` is E-URI-SCHEME, anything else between `file:` and
\ the path is E-URI-AUTHORITY, and a `%` without two hex digits or a bare `?`
\ or `#` is E-URI-ESCAPE. The whole URI is checked, and the path measured,
\ before a byte is written: a span too small for the path throws
\ E-SPAN-CAPACITY and a negative URI length E-SPAN-LENGTH, so no refusal leaves
\ a byte behind. The path is bytes, as the file system takes them: a decoded
\ NUL or `/` is kept, and lib/fs.f refuses a path holding a NUL where it opens
\ one (E-FS-PATH-UNSAFE).
\
\ STORAGE CLASS. CALLER-OWNED: the module keeps no state.

require lib/errors.f
require lib/string.f
require lib/span.f
require lib/adt/option.f

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
: LOCALHOST$ ( -- ptr u8 n ) s" localhost" ;
: HEX-DIGITS ( -- ptr u8 n ) s" 0123456789abcdef" ;

: LOCAL? ( ptr u8 n -- bool )
   {: a:ptr u:n :}   \ an authority naming this machine
   u 0= if true exit then
   a u LOCALHOST$ STR=CI ;

\ The index of the path's leading `/`, once the scheme and the authority before
\ it are those of a local file.
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
   {: c:n :}    \ a hex digit's value; any other byte is E-URI-ESCAPE
   HEX-DIGITS c ASCII-LOWER INDEX-OF MATCH option
      none OF E-URI-ESCAPE throw ENDOF
      some OF IDX>N ENDOF
   ;MATCH ;

\ The path byte at cursor i, and the cursor past the URI bytes it took.
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
   {: a:ptr u:n :}   \ the decoded length, every byte checked
   0 0 begin dup u < while
      a u BYTE-AT nip swap 1+ swap
   repeat drop ;

\ Writes the path byte at cursor i to out at `at`, and answers both past it.
: PUT ( n n ptr u8 n ptr u8 -- n n )
   {: at:n i:n a:ptr u:n out:ptr :}
   i a u BYTE-AT {: b:n next:n :}
   b out at + c!
   at 1+ next ;

: DECODE ( ptr u8 n ptr u8 -- )
   {: a:ptr u:n out:ptr :}
   0 0 begin dup u < while a u out PUT repeat 2drop ;

public

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
