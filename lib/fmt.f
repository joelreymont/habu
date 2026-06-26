\ fmt.f - checked number formatting into the string builder.
\
\ Builds compact numeric text in the lib/string.f SB builder (SB-RESET / SB$),
\ which is what the analyzers append CSV/JSON/Markdown fields into, then a thin
\ direct-print wrapper for standalone use. Fixed-decimal floats round half-up and
\ are exact while the scaled value (|x| * 10^k) fits an i64. Depends on
\ lib/float.f (POW10) and lib/string.f (SB builder).

48 constant FMT-ZERO
46 constant FMT-DOT

variable FMT-IX
variable FMT-FR
variable FMT-DV

\ ---- string-builder appenders ---------------------------------------------
: SB-U ( n -- )                            \ unsigned int, no separators
   dup 10 < if FMT-ZERO + SB-APPEND-C exit then
   dup 10 / RECURSE  10 mod FMT-ZERO + SB-APPEND-C ;
: SB-INT ( n -- )                          \ signed int
   dup 0 < if STR-MINUS SB-APPEND-C negate then SB-U ;

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
: SB-FIX ( r n -- ) {: k :}
   dup f0< if STR-MINUS SB-APPEND-C fnegate then
   k POW10 f*  0.5 f+  f>s {: scaled :}
   k POW10I {: ps :}
   scaled ps / SB-U
   k 0 > if FMT-DOT SB-APPEND-C  scaled ps mod k SB-FRAC then ;

\ ---- direct printers (reset SB, build, emit) ------------------------------
: U.0 ( n -- )    SB-RESET SB-U   SB$ type ;
: .0 ( n -- )     SB-RESET SB-INT SB$ type ;
: F.N ( r n -- )  SB-RESET SB-FIX SB$ type ;
