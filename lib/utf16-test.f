\ utf16-test.f - UTF-16 code units of UTF-8 spans, and the byte back from a
\ unit, through lib/utf16.f.
\ Run: bin/hb --load lib/utf16-test.f
\
\ Every expected count and byte is written from the definitions, not computed:
\ a scalar below U+10000 is one unit, one at or above it is a surrogate pair,
\ and each byte UTF8:NEXT answers as `raw-byte` is one unit. The column cases
\ are the language server's use: the units before a byte offset on one line.

require lib/errors.f
require lib/test.f
require lib/utf16.f

package UTF16-TEST

: UNITS= ( ptr u8 n n -- )
   {: a:ptr u:n want:n :}
   a u UTF16:UNITS want T= ;

: OFFSET= ( ptr u8 n n n -- )
   {: a:ptr u:n unit:n want:n :}
   a u unit UTF16:OFFSET want T= ;

\ One unit per scalar below U+10000: the empty span, ASCII, U+FFFF on the near
\ side of the one boundary UNITS draws, and a two-byte scalar in a word.
: BMP ( -- )
   s" " 0 UNITS=
   s" abc" 3 UNITS=
   s\" \xEF\xBF\xBF" 1 UNITS=            \ U+FFFF, the last BMP scalar
   s\" caf\xC3\xA9" 4 UNITS= ;

: SUPPLEMENTARY ( -- )
   s\" \xF0\x90\x80\x80" 2 UNITS=        \ U+10000, the first surrogate pair
   s\" \xF4\x8F\xBF\xBF" 2 UNITS=        \ U+10FFFF, the last scalar
   s\" \xF0\x9F\x98\x80" 2 UNITS=        \ U+1F600
   s\" a\xF0\x9F\x98\x80b\xF0\x9F\x98\x80" 6 UNITS= ;

\ The byte offset of `x` against its UTF-16 column: 5 bytes are 3 units after
\ one pair, and 9 bytes are 4 units after a two-byte, a three-byte and a
\ four-byte scalar.
: COLUMNS ( -- )
   s\" \xF0\x9F\x98\x80 x" drop 5 3 UNITS=
   s\" \xC3\xA9\xE2\x82\xAC\xF0\x9F\x98\x80x" drop 9 4 UNITS= ;

\ Invalid UTF-8 counts as UTF8:NEXT reads it: one unit for every byte it answers
\ as `raw-byte`, the width of the U+FFFD that stands for that byte. A truncated
\ sequence is one unit per byte, where Unicode's maximal-subpart practice would
\ count one for its whole prefix.
: INVALID ( -- )
   s\" \x80" 1 UNITS=                    \ a lone continuation byte
   s\" \xFF" 1 UNITS=                    \ a byte no sequence starts with
   s\" \xC0\x80" 2 UNITS=                \ U+0000 in two bytes, overlong
   s\" \xED\xA0\x80" 3 UNITS=            \ U+D800, an encoded surrogate
   s\" \xF4\x90\x80\x80" 4 UNITS=        \ past U+10FFFF
   s\" \xE2\x82A" 3 UNITS=               \ a three-byte prefix cut by an A
   s\" \xF0\x9F\x98" 3 UNITS=            \ a four-byte prefix cut by the span's end
   s\" \x80\xF0\x9F\x98\x80" 3 UNITS= ;  \ a stray byte, then a pair

\ Index 0 is byte 0, and an empty span is byte 0 at any nonnegative index.
: OFFSET-EDGES ( -- )
   s" abc" 0 0 OFFSET=
   s\" \xF0\x9F\x98\x80" 0 0 OFFSET=
   s" " 0 0 OFFSET=
   s" " 1 0 OFFSET=
   s" " 9 0 OFFSET= ;

\ A scalar below U+10000 is one unit, so an index lands on its scalar's first
\ byte; an index at the span's units or past them is the span's length.
: OFFSET-BMP ( -- )
   s" abc" 1 1 OFFSET=
   s" abc" 2 2 OFFSET=
   s" abc" 3 3 OFFSET=                   \ at the end
   s" abc" 7 3 OFFSET=                   \ past the end
   s\" caf\xC3\xA9x" 4 5 OFFSET=          \ after a two-byte scalar
   s\" \xE2\x82\xACx" 1 3 OFFSET=         \ after U+20AC, three bytes
   s\" \xEF\xBF\xBFx" 1 3 OFFSET= ;       \ after U+FFFF, still one unit

\ A scalar at or above U+10000 is two units: the first starts the scalar, and
\ the second, inside the pair, names no byte and rounds to the byte after it.
: OFFSET-PAIR ( -- )
   s\" a\xF0\x9F\x98\x80b" 1 1 OFFSET=     \ the pair's first unit
   s\" a\xF0\x9F\x98\x80b" 2 5 OFFSET=     \ inside the pair
   s\" a\xF0\x9F\x98\x80b" 3 5 OFFSET=     \ the b after it
   s\" a\xF0\x9F\x98\x80b" 4 6 OFFSET=     \ at the end
   s\" \xF0\x9F\x98\x80" 1 4 OFFSET=       \ inside a pair that ends the span
   s\" \xF0\x9F\x98\x80" 2 4 OFFSET= ;     \ at the end

\ Malformed UTF-8 follows UTF8:NEXT: each byte it answers as `raw-byte` is one
\ byte and one unit, a truncated prefix included.
: OFFSET-INVALID ( -- )
   s\" \x80x" 1 1 OFFSET=                  \ after a lone continuation byte
   s\" \xC0\x80x" 1 1 OFFSET=              \ within an overlong U+0000
   s\" \xC0\x80x" 2 2 OFFSET=
   s\" \xE2\x82A" 2 2 OFFSET=              \ a three-byte prefix cut by an A
   s\" \xE2\x82A" 3 3 OFFSET=
   s\" \xF0\x9F\x98" 2 2 OFFSET=           \ a four-byte prefix cut by the end
   s\" \xF0\x9F\x98" 3 3 OFFSET=
   s\" \x80\xF0\x9F\x98\x80" 1 1 OFFSET=   \ a stray byte, then a pair
   s\" \x80\xF0\x9F\x98\x80" 2 5 OFFSET= ;

\ A, two-byte, three-byte and four-byte scalars, a raw byte, then x: scalar
\ boundaries at bytes 0 1 3 6 10 11 12, which are units 0 1 2 3 5 6 7. Unit 4
\ is inside the pair, the one index that does not come back.
: MIXED ( -- ptr u8 n )
   s\" a\xC3\xA9\xE2\x82\xAC\xF0\x9F\x98\x80\x80x" ;

\ A boundary byte comes back through its units.
: BYTE-TRIP ( n -- )
   {: b:n :}
   MIXED MIXED drop b UTF16:UNITS UTF16:OFFSET b T= ;

\ A unit not inside a pair comes back through its byte.
: UNIT-TRIP ( n -- )
   {: unit:n :}
   MIXED drop MIXED unit UTF16:OFFSET UTF16:UNITS unit T= ;

\ Where OFFSET is an exact inverse of UNITS: at every scalar boundary.
: ROUND-TRIPS ( -- )
   0 BYTE-TRIP 1 BYTE-TRIP 3 BYTE-TRIP 6 BYTE-TRIP
   10 BYTE-TRIP 11 BYTE-TRIP 12 BYTE-TRIP
   0 UNIT-TRIP 1 UNIT-TRIP 2 UNIT-TRIP 3 UNIT-TRIP
   5 UNIT-TRIP 6 UNIT-TRIP 7 UNIT-TRIP ;

: NEGATIVE-LENGTH ( -- )
   s" a" drop -1 UTF16:UNITS drop ;

: OFFSET-NEGATIVE-LENGTH ( -- )
   s" a" drop -1 0 UTF16:OFFSET drop ;

: OFFSET-NEGATIVE-INDEX ( -- )
   s" a" -1 UTF16:OFFSET drop ;

: OFFSET-EMPTY-NEGATIVE ( -- )
   s" " -1 UTF16:OFFSET drop ;

: REFUSALS ( -- )
   [: NEGATIVE-LENGTH ;] E-STR-BOUNDS TTHROWSQ
   [: OFFSET-NEGATIVE-LENGTH ;] E-STR-BOUNDS TTHROWSQ
   [: OFFSET-NEGATIVE-INDEX ;] E-STR-BOUNDS TTHROWSQ
   [: OFFSET-EMPTY-NEGATIVE ;] E-STR-BOUNDS TTHROWSQ ;

: MAIN ( -- )
   T-RESET
   BMP
   SUPPLEMENTARY
   COLUMNS
   INVALID
   OFFSET-EDGES
   OFFSET-BMP
   OFFSET-PAIR
   OFFSET-INVALID
   ROUND-TRIPS
   REFUSALS
   T-REPORT
   s" utf16-test: ok" type cr ;

MAIN

;package
