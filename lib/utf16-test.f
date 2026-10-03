\ utf16-test.f - UTF-16 code units of UTF-8 spans, through lib/utf16.f.
\ Run: bin/hb --load lib/utf16-test.f
\
\ Every expected count is written from the definitions, not computed: a scalar
\ below U+10000 is one unit, one at or above it is a surrogate pair, and each
\ byte UTF8:NEXT answers as `raw-byte` is one unit. The column cases are the
\ language server's use: the units before a byte offset on one line.

require lib/errors.f
require lib/test.f
require lib/utf16.f

package UTF16-TEST

: UNITS= ( ptr u8 n n -- )
   {: a:ptr u:n want:n :}
   a u UTF16:UNITS want T= ;

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

: NEGATIVE-LENGTH ( -- )
   s" a" drop -1 UTF16:UNITS drop ;

: REFUSALS ( -- )
   [: NEGATIVE-LENGTH ;] E-STR-BOUNDS TTHROWSQ ;

: MAIN ( -- )
   T-RESET
   BMP
   SUPPLEMENTARY
   COLUMNS
   INVALID
   REFUSALS
   T-REPORT
   s" utf16-test: ok" type cr ;

MAIN

;package
