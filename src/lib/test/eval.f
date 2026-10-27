\ eval.f - typed values and throw codes out of evaluated test text, in package
\ TEST-EVAL, loaded by src/lib/test.f.
\
\ N, FLAG and RC take one cell, a flag or the throw code out of a source text
\ under `evaluate-closed`, so a test needs no `evaluate` wrapper of its own.
\ The measured rules are in docs/forth.md "Rules learned by refusal".
\
\ STORAGE CLASS. PROCESS-WIDE: the source and result cells are static; one
\ task evaluates at a time.

package TEST-EVAL

PTR-VARIABLE SRC-A
variable SRC-U
variable N-CELL
TYPED-VARIABLE FLAG-CELL bool

: SRC! ( ptr u8 n -- )
   SRC-U !
   SRC-A ! ;

public

\ SRC$, N! and FLAG! are the hooks of the closed texts below, which run in the
\ caller's scope and so reach them only qualified. Callers use N, FLAG, RC.
: SRC$ ( -- ptr u8 n )
   SRC-A @ SRC-U @ ;

: N! ( n -- )
   N-CELL ! ;

: FLAG! ( bool -- )
   FLAG-CELL ! ;

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

\ FLAG's text leaves the flag itself, which FLAG! takes as N! takes N's cell.
: FLAG ( ptr u8 n -- bool )
   SRC!
   s" TEST-EVAL:SRC$ evaluate TEST-EVAL:FLAG!" evaluate-closed
   FLAG-CELL @ ;

\ The text's throw code, 0 when it loaded as a closed program.
: RC ( ptr u8 n -- n )
   SRC!
   [: SRC$ evaluate-closed ;] catch ;

;package
