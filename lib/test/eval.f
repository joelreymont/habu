\ eval.f - typed values and throw codes out of evaluated test text.

package TEST-EVAL

PTR-VARIABLE SRC-A
variable SRC-U
variable N-CELL

: SRC! ( ptr u8 n -- )
   SRC-U !
   SRC-A ! ;

public

\ SRC$ and N! are the hooks of N's closed text below, which runs in the
\ caller's scope and so reaches them only qualified. Callers use N, FLAG, RC.
: SRC$ ( -- ptr u8 n )
   SRC-A @ SRC-U @ ;

: N! ( n -- )
   N-CELL ! ;

\ The constant text is closed and the caller's text runs inside it under plain
\ `evaluate`, at the top level where `evaluate` is admitted: the closed floor
\ sits under the caller's text and N! takes the one cell it must leave. No cell
\ is N!'s underdepth, 70; two or more is the closed residue, E-EVAL-RESIDUE.
\ SRC$ reads the text before it runs and N! stores after it, so N nests inside
\ its own text.
: N ( ptr u8 n -- n )
   SRC!
   s" TEST-EVAL:SRC$ evaluate TEST-EVAL:N!" evaluate-closed
   N-CELL @ ;

: FLAG ( ptr u8 n -- bool )
   N 0<> ;

\ The text's throw code, 0 when it loaded as a closed program.
: RC ( ptr u8 n -- n )
   SRC!
   [: SRC$ evaluate-closed ;] catch ;

;package
