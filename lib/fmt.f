\ fmt.f - checked number formatting into the string builder.
\
\ The module lives in `package FMT`. External callers reach it through the
\ qualified public API: FMT:SB-U and FMT:SB-INT append an unsigned or signed
\ integer to the shared lib/string.f builder (open it with SB-RESET, read it
\ back with SB$), FMT:SB-FIX appends a fixed-decimal float rounded half-up, and
\ the direct printers FMT:.U / FMT:.INT / FMT:F.N reset the builder, format, and
\ type the result. The word tails stay recognizable (SB-U, SB-INT, F.N, ...)
\ because they read across hundreds of call sites; only the FMT: package
\ qualifier is added. Fixed-decimal floats are exact while the scaled value
\ (|x| * 10^k) fits an i64. The power-of-ten helper POW10I, the zero-padded
\ fraction helper SB-FRAC, and every buffer and constant are package-private.
\ Depends on lib/float.f (POW10) and lib/string.f (SB builder).

require lib/float.f                        \ POW10 (SB-FIX float scaling)
require lib/string.f                       \ SB-RESET / SB-APPEND-C / SB$ / STR-MINUS

package FMT

48 constant FMT-ZERO
46 constant FMT-DOT

variable FMT-IX
variable FMT-FR
variable FMT-DV

\ ---- string-builder appenders ---------------------------------------------
public

: SB-U ( n -- )                            \ unsigned int, no separators
   dup 10 < if FMT-ZERO + SB-APPEND-C exit then
   dup 10 / RECURSE  10 mod FMT-ZERO + SB-APPEND-C ;
: SB-INT ( n -- )                          \ signed int
   dup 0 < if STR-MINUS SB-APPEND-C negate then SB-U ;

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

\ append a float with exactly k decimal places (k=0 omits the point)
public

: SB-FIX ( r n -- ) {: k :}
   dup f0< if STR-MINUS SB-APPEND-C fnegate then
   k POW10 f*  0.5 f+  f>s {: scaled :}
   k POW10I {: ps :}
   scaled ps / SB-U
   k 0 > if FMT-DOT SB-APPEND-C  scaled ps mod k SB-FRAC then ;

\ ---- direct printers (reset SB, build, emit) ------------------------------
: .U ( n -- )     SB-RESET SB-U   SB$ type ;   \ unsigned, no trailing space
: .INT ( n -- )   SB-RESET SB-INT SB$ type ;   \ signed, no trailing space
: F.N ( r n -- )  SB-RESET SB-FIX SB$ type ;

;package
