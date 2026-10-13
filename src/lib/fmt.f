\ fmt.f - checked number formatting.
\
\ STORAGE CLASS. TASK-LOCAL. The integer render buffer (FMT-NUM-BUF, FMT-NUM-U)
\ and the POW10I/SB-FRAC scratch cells (FMT-IX, FMT-FR, FMT-DV) live in the
\ FMT-ABI band of the per-task DATA region, and the appenders write into
\ src/lib/string.f's SB, which is task-local too, so two tasks formatting at once
\ share nothing. See docs/threads.md.
\
\ The module lives in `package FMT`. External callers reach it through the
\ qualified public API: FMT:SB-U, FMT:SB-HEX, FMT:SB-INT, and FMT:SB-SCALED
\ append unsigned decimal, unsigned hexadecimal, signed decimal, or an exact
\ scaled integer to the shared
\ src/lib/string.f builder (open it with SB-RESET, read it
\ back with SB$), FMT:SB-FIX appends a fixed-decimal float rounded half-up, and
\ the direct printers FMT:.U / FMT:.INT / FMT:F.N type the formatted text. The
\ word tails stay recognizable (SB-U, SB-INT, F.N, ...) because they read
\ across hundreds of call sites; only the FMT: package qualifier is added.
\
\ Integers render through a package-private buffer that the appenders and the
\ direct printers share, so FMT:.U and FMT:.INT leave the shared builder
\ alone: a caller part-way through building a string can print a number
\ without losing what it built. FMT:F.N is the exception. A fixed-decimal
\ fraction has no bounded width, so its text is assembled in the shared
\ builder and FMT:F.N resets SB like any other builder client.
\
\ Every appender is fail-closed on its domain. SB-U / .U are unsigned only: a
\ negative value throws E-FMT-DOMAIN rather than rendering a byte below '0'.
\ SB-HEX has the same domain and emits uppercase digits without a prefix.
\ SB-INT covers the whole signed i64 range, including STR-MIN-I64
\ ($8000000000000000), whose magnitude has no positive i64 form and so is
\ emitted from the canonical STR-MIN-I64$ digit table instead of a negate that
\ would overflow. Fixed-decimal floats are exact while the rounded scaled value
\ (|x| * 10^k) fits an i64; beyond that boundary SB-FIX throws E-FMT-OVERFLOW
\ instead of letting f>s saturate to a wrong number. The power-of-ten helper
\ POW10I, the fits-i64 guard FIT-I64, the zero-padded fraction helper SB-FRAC,
\ the integer renderer, and every buffer and constant are package-private.
\ Depends on src/lib/float.f (POW10) and src/lib/string.f (SB builder).

require src/lib/float.f                        \ POW10 (SB-FIX float scaling)
require src/lib/string.f                       \ SB-RESET / SB-APPEND-C / SB$ / STR-MINUS

package FMT

48 constant FMT-ZERO
65 constant FMT-A
46 constant FMT-DOT

\ Every byte and cell fmt keeps is TASK-LOCAL, in the FMT-ABI band of the
\ per-task DATA region: each accessor reads `data-base`, which is the RUNNING
\ task's region, so two tasks formatting at once share nothing. The band is
\ declared in src/habu/layout.f beside STRING-ABI and asserted there against
\ every other DATA claim. It is a declared band and not TASK:+USER rows because
\ src/habu/habu2.f requires this module for number text, so fmt is inside the
\ engine's own closure and a `require src/lib/task.f` here would pull pthread, mmap
\ and the FFI staging tables into the base image for 52 bytes of scratch. A
\ region is a fresh zeroed mapping, so a new task's cells start at zero exactly
\ as the old `variable`s did.
: FMT-IX ( -- ptr n )
   data-base FMT-ABI:IX-OFF + ;

: FMT-FR ( -- ptr n )
   data-base FMT-ABI:FR-OFF + ;

: FMT-DV ( -- ptr n )
   data-base FMT-ABI:DV-OFF + ;

\ ---- integer render buffer ------------------------------------------------
\ An i64 never renders wider than one sign byte plus the STR-I64-DIGITS digits
\ of its magnitude, so the buffer is exact by construction and the renderer
\ needs no capacity check. Every integer path ends here, which is what keeps
\ the direct printers off the shared builder.

STR-I64-DIGITS 1+ constant FMT-NUM-CAP
\ src/habu/layout.f is loaded before src/lib/string.f and cannot see
\ STR-I64-DIGITS, so FMT-ABI:NUM-BUF-BYTES states the width there and this
\ executes the agreement once at load, the way src/habu/rt.f executes
\ RT:DSTACK-AGREE. A narrower band would let the renderer overrun the band's
\ top into STRING-ABI; a wider one would reserve bytes no renderer uses.
: BAND-AGREE ( -- )
   FMT-NUM-CAP FMT-ABI:NUM-BUF-BYTES <> if E-FMT-BAND throw then ;
BAND-AGREE

: FMT-NUM-BUF ( -- ptr u8 )
   data-base FMT-ABI:NUM-BUF-OFF + BYTE-VIEW ;

: FMT-NUM-U ( -- ptr n )
   data-base FMT-ABI:NUM-U-OFF + ;

: NUM-C+ ( n -- )
   FMT-NUM-BUF FMT-NUM-U @ + c!
   FMT-NUM-U @ 1+ FMT-NUM-U ! ;

: NUM$ ( -- ptr u8 n )
   FMT-NUM-BUF FMT-NUM-U @ ;

: DIGITS>NUM ( n -- )                      \ nonnegative magnitude, most significant digit first
   dup 10 < if FMT-ZERO + NUM-C+ exit then
   dup 10 / RECURSE  10 mod FMT-ZERO + NUM-C+ ;

: U>NUM ( n -- )                           \ unsigned; negative -> E-FMT-DOMAIN before any byte lands
   dup 0 < if E-FMT-DOMAIN throw then
   0 FMT-NUM-U !  DIGITS>NUM ;

: HEX-C+ ( n -- )
   dup 10 < if FMT-ZERO + else 10 - FMT-A + then NUM-C+ ;

: HEX-DIGITS>NUM ( n -- )
   dup 16 < if HEX-C+ exit then
   dup 16 / RECURSE  16 mod HEX-C+ ;

: HEX>NUM ( n -- )
   dup 0 < if E-FMT-DOMAIN throw then
   0 FMT-NUM-U !  HEX-DIGITS>NUM ;

: INT>NUM ( n -- )                         \ signed (STR-MIN-I64 has no positive magnitude)
   0 FMT-NUM-U !
   dup STR-MIN-I64 = if
      drop STR-MINUS NUM-C+
      STR-MIN-I64$ FMT-NUM-BUF FMT-NUM-U @ + STR-I64-DIGITS BYTE-COPY
      FMT-NUM-U @ STR-I64-DIGITS + FMT-NUM-U ! exit
   then
   dup 0 < if STR-MINUS NUM-C+ negate then DIGITS>NUM ;

\ ---- string-builder appenders ---------------------------------------------
public

: SB-U ( n -- )                            \ unsigned int, no separators; negative -> E-FMT-DOMAIN
   U>NUM NUM$ SB-APPEND ;
: SB-HEX ( n -- )                          \ uppercase, no prefix; negative -> E-FMT-DOMAIN
   HEX>NUM NUM$ SB-APPEND ;
: SB-INT ( n -- )                          \ signed int (STR-MIN-I64 has no positive magnitude)
   INT>NUM NUM$ SB-APPEND ;

\ Interpret value as value / 10^places. The integer renderer supplies all
\ digits, including STR-MIN-I64, without taking a magnitude that could overflow.
\ Append the point and fraction separately so FMT-NUM-CAP stays unchanged.
: SB-SCALED ( n n -- )
   {: value:n places:n :}
   places 0 < places 18 > or if E-FMT-DOMAIN throw then
   value INT>NUM
   places 0= if NUM$ SB-APPEND exit then
   value 0= if FMT-ZERO SB-APPEND-C exit then

   NUM$ {: num-p:ptr width:n :}
   value 0 < if 1 else 0 then {: sign:n :}
   num-p sign + {: p:ptr :}
   width sign - {: digits:n :}
   0 FMT-IX !
   begin
      FMT-IX @ places < FMT-IX @ digits < and
      if p digits 1- + FMT-IX @ - c@ FMT-ZERO = else false then
   while
      1 FMT-IX +!
   repeat

   sign 0<> if STR-MINUS SB-APPEND-C then
   digits places > if
      p digits places - SB-APPEND
   else
      FMT-ZERO SB-APPEND-C
   then
   places FMT-IX @ - {: frac:n :}
   frac 0= if exit then
   FMT-DOT SB-APPEND-C
   digits places < if
      places digits - 0 ?do FMT-ZERO SB-APPEND-C loop
      p digits FMT-IX @ - SB-APPEND
   else
      p digits places - + frac SB-APPEND
   then ;

private

: POW10I ( n -- n ) {: k :}                \ 10^k as an integer
   1  0 FMT-IX !
   begin FMT-IX @ k < while
      10 *  FMT-IX @ 1+ FMT-IX !
   repeat ;

\ append frac (0 .. 10^k-1) as exactly k zero-padded digits
: SB-FRAC ( n n -- ) {: frac k :}
   k 0= if exit then
   frac FMT-FR !  k 1- POW10I FMT-DV !
   begin FMT-DV @ 0 > while
      FMT-FR @ FMT-DV @ / FMT-ZERO + SB-APPEND-C
      FMT-FR @ FMT-DV @ mod FMT-FR !
      FMT-DV @ 10 / FMT-DV !
   repeat ;

\ Fits-i64 guard: a nonnegative rounded magnitude is representable iff it stays
\ below 2^63 (STR-MAX-I64 s>f rounds to 2^63, the first double f>s cannot hold).
\ Beyond that, f>s would saturate to STR-MAX-I64 and print wrong digits.
: FIT-I64 ( r -- )
   STR-MAX-I64 s>f f< 0= if E-FMT-OVERFLOW throw then ;

\ append a float with exactly k decimal places (k=0 omits the point)
public

: SB-FIX ( r n -- ) {: k :}
   dup f0< if STR-MINUS SB-APPEND-C fnegate then
   k POW10 f*  0.5 f+                       \ rounded scaled magnitude ( r >= 0 )
   dup FIT-I64                              \ fail closed at the fits-i64 boundary
   f>s {: scaled:n :}
   k POW10I {: ps :}
   scaled ps / SB-U
   k 0 > if FMT-DOT SB-APPEND-C  scaled ps mod k SB-FRAC then ;

\ ---- direct printers ------------------------------------------------------
: .U ( n -- )     U>NUM   NUM$ type ;          \ unsigned, no trailing space, SB untouched
: .INT ( n -- )   INT>NUM NUM$ type ;          \ signed, no trailing space, SB untouched
: F.N ( r n -- )  SB-RESET SB-FIX SB$ type ;   \ unbounded width: assembled in the shared builder

;package
