split-str
\ eval.f - typed values and throw codes out of evaluated test text, package
\ TEST-EVAL, loaded by src/lib/test.f. N, FLAG and RC take one cell, a flag or the
\ throw code out of a source text under `evaluate-closed`, so a test needs no
\ `evaluate` wrapper of its own; THROWN-HEAD$ is the record line a throw nothing
\ caught ends with. The measured rules are in docs/forth.md "Rules learned by
\ refusal".
\ STORAGE CLASS. PROCESS-WIDE: static source, result and head cells; one task at
\ a time.

require src/lib/error.f
require src/lib/string.f
require src/lib/fmt.f

package TEST-EVAL

PTR-VARIABLE SRC-A
variable SRC-U
variable N-CELL
TYPED-VARIABLE FLAG-CELL bool

: SRC! ( str -- )
   SRC-U !
   SRC-A ! ;

\ A code's name is at most 255 bytes; a longer text throws E-STR-CAPACITY.
$400 constant HEAD-CAP
HEAD-CAP BUFFER: HEAD-BUF
variable HEAD-LEN

: HEAD+ ( str -- )
   HEAD-BUF HEAD-CAP HEAD-LEN BUF-APPEND ;

public

\ SRC$, N! and FLAG! are the hooks of the closed texts below, which run in the
\ caller's scope and so reach them only qualified. Callers use N, FLAG, RC.
: SRC$ ( -- str )
   SRC-A @ SRC-U @ ;

: N! ( n -- )
   N-CELL ! ;

: FLAG! ( bool -- )
   FLAG-CELL ! ;

\ The constant text is closed and the caller's text runs inside it under plain
\ `evaluate`, at the top level where `evaluate` is admitted: the closed floor
\ sits under the caller's text and N! takes the one cell it must leave. No cell
\ is N!'s underdepth; two or more is the closed residue, a record: both are 70.
\ SRC$ reads the text before it runs and N! stores after it, so N nests inside
\ its own text.
: N ( str -- n )
   SRC!
   s" TEST-EVAL:SRC$ evaluate TEST-EVAL:N!" evaluate-closed
   N-CELL @ ;

\ FLAG's text leaves the flag itself, which FLAG! takes as N! takes N's cell.
: FLAG ( str -- bool )
   SRC!
   s" TEST-EVAL:SRC$ evaluate TEST-EVAL:FLAG!" evaluate-closed
   FLAG-CELL @ ;

\ The text's throw code, 0 when it loaded as a closed program.
: RC ( str -- n )
   SRC!
   [: SRC$ evaluate-closed ;] catch ;

\ The head line, without its line feed, of the record the engine prints for a
\ throw of code c that nothing caught. It is built in a buffer of its own, so a
\ caller's SB survives it; the next call overwrites it.
: THROWN-HEAD$ ( n -- str )
   {: c:n :}
   c ERROR:NAME$ {: name:^ nameu:n :}
   c ERROR:TEXT$ {: text:^ textu:n :}
   HEAD-LEN BUF-RESET
   s" error[" HEAD+
   nameu 0= if s" E-THROW" HEAD+ else name nameu HEAD+ then
   s" ]: " HEAD+
   textu 0= if
      s" nothing caught a throw of code " HEAD+  c FMT:INT$ HEAD+
   else text textu HEAD+ then
   HEAD-BUF HEAD-LEN BUF-LEN@ ;

;package
