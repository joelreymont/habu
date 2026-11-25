\ utf16-test.f - UTF-16 code units of UTF-8 spans, the byte back from a unit,
\ and strict UTF-16LE conversion, through src/lib/utf16.f.
\ Run: bin/hb --load src/lib/utf16-test.f
\ Expected counts and bytes are written from the definitions, not computed; each
\ byte UTF8:NEXT answers as `raw-byte` is one unit. The column cases are the
\ language server's use: the units before a byte offset on one line.

require src/lib/error.f
require src/lib/heap.f
require src/lib/span.f
require src/lib/test/assert.f
require src/lib/utf16.f
require src/lib/vector.f

package UTF16-TEST

\ Each cell holds a vector, NULL-PTR while it is not made.
TYPED-VARIABLE LE-BUF ^VEC:vector<u8>
TYPED-VARIABLE LE-OUT ^VEC:vector<u8>

: RELEASE ( ^^VEC:vector<u8> -- )
   {: c:^ :}
   c @ VEC:DELETE  NULL-PTR c ! ;

\ NULL-PTR goes in before NEW: a rejected NEW leaves no vector to delete twice.
: FRESH ( ^^VEC:vector<u8> -- )
   {: c:^ :}
   c RELEASE  HEAP:ALLOCATOR 0 [type] u8 VEC:NEW c ! ;

: GOT$ ( ^VEC:vector<u8> -- str )  VEC:BYTES ;

: UNITS= ( str n -- )
   {: s:str want:n :}
   s UTF16:UNITS want T= ;

: OFFSET= ( str n n -- )
   {: s:str unit:n want:n :}
   s unit UTF16:OFFSET want T= ;

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

: COLUMNS ( -- )
   s\" \xF0\x9F\x98\x80 x" STR:DATA 5 >str 3 UNITS=
   s\" \xC3\xA9\xE2\x82\xAC\xF0\x9F\x98\x80x" STR:DATA 9 >str 4 UNITS= ;

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
   s" abc" 3 3 OFFSET=
   s" abc" 7 3 OFFSET=
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
: MIXED ( -- str )
   s\" a\xC3\xA9\xE2\x82\xAC\xF0\x9F\x98\x80\x80x" ;

: BYTE-TRIP ( n -- )
   {: b:n :}
   MIXED MIXED STR:DATA b >str UTF16:UNITS UTF16:OFFSET b T= ;

: UNIT-TRIP ( n -- )
   {: unit:n :}
   MIXED STR:DATA MIXED unit UTF16:OFFSET >str UTF16:UNITS unit T= ;

: ROUND-TRIPS ( -- )
   0 BYTE-TRIP 1 BYTE-TRIP 3 BYTE-TRIP 6 BYTE-TRIP
   10 BYTE-TRIP 11 BYTE-TRIP 12 BYTE-TRIP
   0 UNIT-TRIP 1 UNIT-TRIP 2 UNIT-TRIP 3 UNIT-TRIP
   5 UNIT-TRIP 6 UNIT-TRIP 7 UNIT-TRIP ;

: NEGATIVE-LENGTH ( -- )
   s" a" STR:DATA -1 >str UTF16:UNITS drop ;

: OFFSET-NEGATIVE-LENGTH ( -- )
   s" a" STR:DATA -1 >str 0 UTF16:OFFSET drop ;

: OFFSET-NEGATIVE-INDEX ( -- )
   s" a" -1 UTF16:OFFSET drop ;

: OFFSET-EMPTY-NEGATIVE ( -- )
   s" " -1 UTF16:OFFSET drop ;

: REJECTIONS ( -- )
   [: NEGATIVE-LENGTH ;] E-STR-BOUNDS TTHROWSQ
   [: OFFSET-NEGATIVE-LENGTH ;] E-STR-BOUNDS TTHROWSQ
   [: OFFSET-NEGATIVE-INDEX ;] E-STR-BOUNDS TTHROWSQ
   [: OFFSET-EMPTY-NEGATIVE ;] E-STR-BOUNDS TTHROWSQ ;

: LE>UTF8= ( str str -- )
   {: le:str want:str :}
   LE-BUF @ 0 VEC:RESIZE
   le LE-BUF @ UTF16:APPEND-LE>UTF8
   LE-BUF @ GOT$ want T$= ;

: UTF8>LE= ( str str -- )
   {: text:str want:str :}
   LE-BUF @ 0 VEC:RESIZE
   text LE-BUF @ UTF16:APPEND-UTF8>LE
   LE-BUF @ GOT$ want T$= ;

: BOTH= ( str str -- )
   {: le:str text:str :}
   le text LE>UTF8=
   text le UTF8>LE= ;


: VECTORS ( -- )
   s" " s" " BOTH=
   s\" \x44\x00\x65\x00\x66\x00\x61\x00\x75\x00\x6C\x00\x74\x00" s" Default" BOTH=
   s\" a\x00\x00\x00b\x00" s\" a\x00b" BOTH=          \ an embedded NUL
   s\" \xE9\x00" s\" \xC3\xA9" BOTH=                  \ U+00E9
   s\" \xFF\xD7" s\" \xED\x9F\xBF" BOTH=              \ U+D7FF, just below the surrogates
   s\" \x00\xE0" s\" \xEE\x80\x80" BOTH=              \ U+E000, just above them
   s\" \xFF\xFF" s\" \xEF\xBF\xBF" BOTH=              \ U+FFFF, the last one-unit scalar
   s\" \x00\xD8\x00\xDC" s\" \xF0\x90\x80\x80" BOTH=  \ U+10000, the first pair
   s\" \xFF\xDB\xFF\xDF" s\" \xF4\x8F\xBF\xBF" BOTH=  \ U+10FFFF, the last pair
   s\" \x3D\xD8\x00\xDE" s\" \xF0\x9F\x98\x80" BOTH= ; \ U+1F600, a surrogate pair


: ROUND-TRIP ( -- )
   s\" a\x00\xC3\xA9\xEF\xBF\xBF\xF0\x9F\x98\x80z" {: text:str :}
   LE-BUF @ 0 VEC:RESIZE  LE-OUT @ 0 VEC:RESIZE
   text LE-BUF @ UTF16:APPEND-UTF8>LE
   LE-BUF @ GOT$ LE-OUT @ UTF16:APPEND-LE>UTF8
   LE-OUT @ GOT$ text T$= ;


: APPENDS ( -- )
   LE-BUF @ 0 VEC:RESIZE
   s\" a\x00b\x00" LE-BUF @ UTF16:APPEND-LE>UTF8
   s\" \xE9\x00" LE-BUF @ UTF16:APPEND-LE>UTF8
   LE-BUF @ GOT$ s\" ab\xC3\xA9" T$=
   LE-BUF @ 0 VEC:RESIZE
   s" a" LE-BUF @ UTF16:APPEND-UTF8>LE
   s\" \xC3\xA9" LE-BUF @ UTF16:APPEND-UTF8>LE
   LE-BUF @ GOT$ s\" a\x00\xE9\x00" T$= ;


: LE-REJECTIONS ( -- )
   [: s\" \x44" LE-BUF @ UTF16:APPEND-LE>UTF8 ;] UTF16:E-UTF16 TTHROWSQ
   [: s\" \x44\x00\x65" LE-BUF @ UTF16:APPEND-LE>UTF8 ;] UTF16:E-UTF16 TTHROWSQ
   [: s\" \x3D\xD8a\x00" LE-BUF @ UTF16:APPEND-LE>UTF8 ;] UTF16:E-UTF16 TTHROWSQ      \ high, then a
   [: s\" \x3D\xD8\x3D\xD8\x00\xDE" LE-BUF @ UTF16:APPEND-LE>UTF8 ;] UTF16:E-UTF16 TTHROWSQ \ high, then a pair
   [: s\" a\x00\x3D\xD8" LE-BUF @ UTF16:APPEND-LE>UTF8 ;] UTF16:E-UTF16 TTHROWSQ      \ high at the end
   [: s\" \x00\xDEa\x00" LE-BUF @ UTF16:APPEND-LE>UTF8 ;] UTF16:E-UTF16 TTHROWSQ      \ low first
   [: s\" a\x00\x00\xDE" LE-BUF @ UTF16:APPEND-LE>UTF8 ;] UTF16:E-UTF16 TTHROWSQ      \ low after a
   [: s\" \x00\xD8" LE-BUF @ UTF16:APPEND-LE>UTF8 ;] UTF16:E-UTF16 TTHROWSQ          \ D800 alone
   [: s\" \xFF\xDB" LE-BUF @ UTF16:APPEND-LE>UTF8 ;] UTF16:E-UTF16 TTHROWSQ          \ DBFF alone
   [: s\" \x00\xDC" LE-BUF @ UTF16:APPEND-LE>UTF8 ;] UTF16:E-UTF16 TTHROWSQ          \ DC00 alone
   [: s\" \xFF\xDF" LE-BUF @ UTF16:APPEND-LE>UTF8 ;] UTF16:E-UTF16 TTHROWSQ          \ DFFF alone
   [: s\" \x00\xD8\x00\xE0" LE-BUF @ UTF16:APPEND-LE>UTF8 ;] UTF16:E-UTF16 TTHROWSQ  \ high, then E000
   [: s\" \x00\xD8\x00\xDC" STR:DATA 2 >str LE-BUF @ UTF16:APPEND-LE>UTF8 ;] UTF16:E-UTF16 TTHROWSQ \ the span ends before its low
   [: s" a" STR:DATA -2 >str LE-BUF @ UTF16:APPEND-LE>UTF8 ;] E-STR-BOUNDS TTHROWSQ ;


: UTF8-REJECTIONS ( -- )
   [: s\" \x80" LE-BUF @ UTF16:APPEND-UTF8>LE ;] UTF16:E-UTF8 TTHROWSQ               \ a lone continuation
   [: s\" \xC0\x80" LE-BUF @ UTF16:APPEND-UTF8>LE ;] UTF16:E-UTF8 TTHROWSQ           \ U+0000, overlong
   [: s\" \xED\xA0\x80" LE-BUF @ UTF16:APPEND-UTF8>LE ;] UTF16:E-UTF8 TTHROWSQ       \ U+D800, a surrogate
   [: s\" \xF4\x90\x80\x80" LE-BUF @ UTF16:APPEND-UTF8>LE ;] UTF16:E-UTF8 TTHROWSQ   \ past U+10FFFF
   [: s\" \xF0\x9F\x98" LE-BUF @ UTF16:APPEND-UTF8>LE ;] UTF16:E-UTF8 TTHROWSQ       \ truncated at the end
   [: s\" a\xF0\x9F\x98b" LE-BUF @ UTF16:APPEND-UTF8>LE ;] UTF16:E-UTF8 TTHROWSQ     \ truncated by b
   [: s" a" STR:DATA -1 >str LE-BUF @ UTF16:APPEND-UTF8>LE ;] E-STR-BOUNDS TTHROWSQ ;

: CONVERSIONS ( -- )
   LE-BUF FRESH  LE-OUT FRESH
   VECTORS
   ROUND-TRIP
   APPENDS
   LE-REJECTIONS
   UTF8-REJECTIONS
   LE-BUF RELEASE  LE-OUT RELEASE ;

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
   REJECTIONS
   CONVERSIONS
   T-REPORT
   s" utf16-test: ok" type cr ;

MAIN

;package
