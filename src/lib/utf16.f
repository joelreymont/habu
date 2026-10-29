\ utf16.f - UTF-16 code units of UTF-8 text (UNITS and OFFSET, the measure LSP
\ positions take by default) and strict UTF-16LE conversion. UTF-8 is read with
\ UTF8:NEXT (src/lib/utf8-scalar.f): each byte it answers as `raw-byte` is one
\ unit, the width of its U+FFFD. A negative length or unit index is
\ E-STR-BOUNDS, as UTF8:NEXT refuses a negative length.
\ STORAGE CLASS. CALLER-OWNED: the module keeps no state.

require src/lib/errors.f
require src/lib/le.f
require src/lib/byte-buffer.f
require src/lib/utf8-scalar.f

package UTF16
public

-9530 constant E-FIRST
-9539 constant E-LAST
-9530 constant E-UTF16      \ UTF-16LE of odd byte length or with an unpaired surrogate
-9531 constant E-UTF8       \ UTF-8 that UTF8:NEXT reads as a raw byte

private

$10000 constant PAIR-FIRST        \ the first scalar UTF-16 writes as a surrogate pair
$D800 constant HIGH-FIRST         \ the first high (leading) surrogate
$DC00 constant LOW-FIRST          \ the first low (trailing) surrogate
$E000 constant LOW-END            \ the first unit past the low surrogates
10 constant PAIR-SHIFT            \ the scalar bits each surrogate carries
$3FF constant PAIR-MASK
2 constant UNIT-BYTES
8 constant BYTE-SHIFT
$FF constant BYTE-MASK

$80 constant ONE-LIMIT            \ the first scalar UTF-8 writes in two bytes
$800 constant TWO-LIMIT           \ the first scalar UTF-8 writes in three bytes
$C0 constant TWO-LEAD
$E0 constant THREE-LEAD
$F0 constant FOUR-LEAD
$80 constant CONT-TAG
$3F constant CONT-MASK
6 constant CONT-SHIFT

: SCALAR-UNITS ( n -- n )
   PAIR-FIRST < if 1 else 2 then ;

: STEP ( n n ptr u8 n -- n n )
   {: units:n cursor:n a:ptr u:n :}
   a u cursor UTF8:NEXT MATCH UTF8:scalar-step
      scalar OF swap SCALAR-UNITS swap ENDOF
      raw-byte OF nip 1 swap ENDOF
   ;MATCH
   swap units + swap ;

public

\ A scalar below U+10000 is one unit, one at or above it a surrogate pair. One
\ unit per raw byte departs from Unicode's maximal-subpart practice (chapter 3,
\ "U+FFFD Substitution of Maximal Subparts") only for a truncated sequence that
\ starts well (E2 82 then `A`, or F0 9F 98 at the span's end), which it counts
\ as one unit.
: UNITS ( ptr u8 n -- n )
   {: a:ptr u:n :}
   u 0 < if E-STR-BOUNDS throw then
   0 0 begin dup u < while a u STEP repeat
   drop ;

\ The byte where a unit index starts: index 0 is byte 0, an index at or past the
\ span's units is its length, and an index inside a surrogate pair rounds to the
\ byte after that scalar. It inverts UNITS only at scalar boundaries.
: OFFSET ( ptr u8 n n -- n )
   {: a:ptr u:n unit:n :}
   u 0 < if E-STR-BOUNDS throw then
   unit 0 < if E-STR-BOUNDS throw then
   0 0 begin over unit < over u < and while a u STEP repeat
   nip ;

private

: HIGH? ( n -- bool )
   {: unit:n :}
   unit HIGH-FIRST >= unit LOW-FIRST < and ;

: LOW? ( n -- bool )
   {: unit:n :}
   unit LOW-FIRST >= unit LOW-END < and ;

: PAIR>SCALAR ( n n -- n )
   {: high:n low:n :}
   high HIGH-FIRST - PAIR-SHIFT lshift  low LOW-FIRST - or  PAIR-FIRST + ;


\ A lead byte holding the scalar's high bits, then TAIL continuation bytes of
\ six bits each, highest first.
: APPEND-ENCODED ( n n n ptr a -- )
   {: scalar:n lead:n tail:n buf:ptr :}
   scalar tail CONT-SHIFT * rshift lead or buf BUF:APPEND-BYTE
   tail 0 ?do
      scalar tail 1- i - CONT-SHIFT * rshift CONT-MASK and CONT-TAG or
      buf BUF:APPEND-BYTE
   loop ;


: APPEND-SCALAR ( n ptr a -- )
   {: scalar:n buf:ptr :}
   scalar ONE-LIMIT < if scalar buf BUF:APPEND-BYTE exit then
   scalar TWO-LIMIT < if scalar TWO-LEAD 1 buf APPEND-ENCODED exit then
   scalar PAIR-FIRST < if scalar THREE-LEAD 2 buf APPEND-ENCODED exit then
   scalar FOUR-LEAD 3 buf APPEND-ENCODED ;


: APPEND-PAIR ( n n ptr u8 n ptr a -- n )
   {: high:n at:n a:ptr u:n buf:ptr :}
   at UNIT-BYTES + u >= if E-UTF16 throw then
   a at UNIT-BYTES + + LE:U16@ {: low:n :}
   low LOW? 0= if E-UTF16 throw then
   high low PAIR>SCALAR buf APPEND-SCALAR
   at UNIT-BYTES 2 * + ;


: DECODE-STEP ( n ptr u8 n ptr a -- n )
   {: at:n a:ptr u:n buf:ptr :}
   a at + LE:U16@ {: unit:n :}
   unit HIGH? if unit at a u buf APPEND-PAIR exit then
   unit LOW? if E-UTF16 throw then
   unit buf APPEND-SCALAR
   at UNIT-BYTES + ;

: APPEND-UNIT ( n ptr a -- )
   {: unit:n buf:ptr :}
   unit BYTE-MASK and buf BUF:APPEND-BYTE
   unit BYTE-SHIFT rshift buf BUF:APPEND-BYTE ;


: APPEND-SCALAR-LE ( n ptr a -- )
   {: scalar:n buf:ptr :}
   scalar PAIR-FIRST < if scalar buf APPEND-UNIT exit then
   scalar PAIR-FIRST - {: bits:n :}
   bits PAIR-SHIFT rshift HIGH-FIRST + buf APPEND-UNIT
   bits PAIR-MASK and LOW-FIRST + buf APPEND-UNIT ;


: ENCODE-STEP ( n ptr u8 n ptr a -- n )
   {: at:n a:ptr u:n buf:ptr :}
   a u at UTF8:NEXT MATCH UTF8:scalar-step
      scalar OF swap buf APPEND-SCALAR-LE ENDOF
      raw-byte OF 2drop E-UTF8 throw ENDOF
   ;MATCH ;

public

\ Both conversions append to an initialised BUF without clearing it, strictly:
\ no replacement character, BOM, terminator or normalisation; NUL is ordinary.
\ A negative length is E-STR-BOUNDS and input overlapping the destination's
\ storage, which an append may move, BUF's E-BUF-BOUNDS, both before a byte
\ moves. A throw while converting may leave part of the append; the caller
\ discards it.
\ Two-byte little-endian units; a high-then-low pair is one scalar. An odd byte
\ length or an unpaired surrogate, high (at the span's end included) or low, is
\ E-UTF16.
: APPEND-LE>UTF8 ( ptr u8 n ptr a -- )
   {: a:ptr u:n buf:ptr :}
   u 0 < if E-STR-BOUNDS throw then
   a u buf BUF:BORROWED? if E-BUF-BOUNDS throw then
   u UNIT-BYTES mod 0<> if E-UTF16 throw then
   0 begin dup u < while a u buf DECODE-STEP repeat
   drop ;

\ A scalar at or above U+10000 becomes a surrogate pair; every raw byte -
\ malformed, overlong, an encoded surrogate, past U+10FFFF or truncated - is
\ E-UTF8.
: APPEND-UTF8>LE ( ptr u8 n ptr a -- )
   {: a:ptr u:n buf:ptr :}
   u 0 < if E-STR-BOUNDS throw then
   a u buf BUF:BORROWED? if E-BUF-BOUNDS throw then
   0 begin dup u < while a u buf ENCODE-STEP repeat
   drop ;

;package
