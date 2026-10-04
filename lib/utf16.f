\ utf16.f - UTF-16 code units of UTF-8 text.
\
\ UNITS answers how many UTF-16 code units a UTF-8 byte span encodes to, the
\ measure LSP positions take by default. It reads the span with UTF8:NEXT
\ (lib/utf8-scalar.f): a scalar below U+10000 is one unit, and one at or above
\ it is two, a surrogate pair.
\
\ OFFSET goes back: the byte where a unit index starts, reading the span the
\ same way. Index 0 is byte 0, an index at or past the span's units is its
\ length, and an index inside a surrogate pair names no byte, so it rounds to
\ the byte after that scalar. It is the exact inverse of UNITS only at scalar
\ boundaries.
\
\ Invalid UTF-8 counts as UTF8:NEXT reads it: every byte it answers as
\ `raw-byte` is one unit, the width of the U+FFFD that stands for that byte.
\ That is one replacement per byte. Unicode's maximal-subpart practice (chapter
\ 3, "U+FFFD Substitution of Maximal Subparts") agrees on every ill-formed byte
\ except a truncated sequence that starts well - E2 82 then an `A`, or F0 9F 98
\ at the end of the span - which it counts as one unit and this counts as one
\ per byte. A span that ends inside a scalar is such a truncation. A negative
\ length, or a negative unit index, is E-STR-BOUNDS, as UTF8:NEXT refuses a
\ negative length.
\
\ STORAGE CLASS. CALLER-OWNED: the module keeps no state.

require lib/errors.f
require lib/utf8-scalar.f

package UTF16
private

$10000 constant PAIR-FIRST        \ the first scalar UTF-16 writes as a surrogate pair

: SCALAR-UNITS ( n -- n )
   PAIR-FIRST < if 1 else 2 then ;

\ The count and cursor after the scalar or raw byte at the cursor.
: STEP ( n n ptr u8 n -- n n )
   {: units:n cursor:n a:ptr u:n :}
   a u cursor UTF8:NEXT MATCH UTF8:scalar-step
      scalar OF swap SCALAR-UNITS swap ENDOF
      raw-byte OF nip 1 swap ENDOF
   ;MATCH
   swap units + swap ;

public

: UNITS ( ptr u8 n -- n )
   {: a:ptr u:n :}
   u 0 < if E-STR-BOUNDS throw then
   0 0 begin dup u < while a u STEP repeat
   drop ;

\ The byte where unit UNIT starts: steps whole scalars until UNIT units are
\ behind, so a unit inside a pair ends after the pair, and the span's end
\ stops an index past it.
: OFFSET ( ptr u8 n n -- n )
   {: a:ptr u:n unit:n :}
   u 0 < if E-STR-BOUNDS throw then
   unit 0 < if E-STR-BOUNDS throw then
   0 0 begin over unit < over u < and while a u STEP repeat
   nip ;

;package
