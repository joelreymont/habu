\ codegen-compare-corpus.f - the pinned word corpus the codegen comparison
\ measures. One concern: the subject words themselves.
\
\ These are ordinary checked Habu definitions. When this file is loaded, the
\ engine compiles every one of them through exactly the path it uses for any
\ other library word, so the machine code the harness then measures is the real
\ output of the real code generator - the harness never reimplements a compiler.
\
\ Why the corpus is pinned here instead of pointing at live library words. The
\ measurement only means something if a change in the numbers means a change in
\ the compiler. If the subjects were live library words, every unrelated edit to
\ those libraries would move the recorded sizes and timings, and an old-vs-new
\ code generator difference would be indistinguishable from ordinary source
\ drift. So the corpus is frozen: do not "improve" a word here. Adding a word is
\ fine - it adds a row - but changing an existing body invalidates the recorded
\ baseline for that word and must be done deliberately, in the same change that
\ regenerates the baseline, with the reason written down.
\
\ Why these eleven words. Each one is the smallest honest example of a distinct
\ shape the code generator has to handle, so the table covers the generator
\ rather than one corner of it:
\
\   NOOP         an empty call - the calibration row every timing is measured
\                against, and the floor of what a call can cost
\   ADD3         straight-line integer arithmetic, no branches, no frame
\   SQUARE-SUM   arithmetic that needs the stack shuffled around a multiply
\   MAX2         a two-way branch (if / then) and a conditional swap
\   LERP         a three-slot typed locals frame
\   SUM-TO       a counted ?do loop with the loop index
\   COUNT-DOWN   the other loop form: begin / until with the test at the end
\   FACT         recursion, which is also the plain word-call-and-return shape
\   CELL-BUMP    cell-width memory: a store, a load, and a store again
\   BYTE-SUM     byte-width memory: c@ over a caller-supplied byte span
\   BYTE-FIND    a byte scan with an early exit out of the middle of a loop
\
\ Arithmetic, a loop word, a memory word and a string (byte-span) word are all
\ present, which is the coverage the comparison harness was asked for.

require lib/prelude.f

package CODEGEN-CORPUS

\ CELL-BUMP needs one cell of storage of its own, and it is the corpus's: no
\ other word in the process writes it except the routine the new code generator
\ compiles for the very same body, which is the point - both columns of the
\ comparison bump THIS cell, so the harness verifies one piece of memory instead
\ of two that merely look alike. That is safe against the timing runs for the
\ reason the body itself gives: CELL-BUMP stores its argument before it reads
\ anything, so what it answers never depends on what the cell held before.
create BUMP-CELL 1 cells allot

public

\ The cell's address as the number a code generator has to materialise, and its
\ contents. The address is a named unchecked boundary and the only one in this
\ file: the checker gives a `create`d word the type `ptr a`, deliberately, so
\ that a pointer cannot be done arithmetic on by accident, and there is no
\ checked way to say "the number that pointer is" - which is exactly what a
\ compiler needs of a data word. Dot habu-resolve-a-data-a1c8067f is the
\ capability that retires it: when the chain can ask the engine what a data word
\ is, the harness stops naming the address at all. The reader beside it is
\ ordinary checked Habu, and it is what both columns verify the bump through.
TRUSTED: BUMP-ADDR ( -- n ) BUMP-CELL ;

: BUMP-CELL@ ( -- n )
   BUMP-CELL @ ;

: NOOP ( -- )
   ;

: ADD3 ( n n n -- n )
   + + ;

: SQUARE-SUM ( n n -- n )
   dup * swap dup * + ;

: MAX2 ( n n -- n )
   2dup < if swap then drop ;

: LERP ( n n n -- n ) {: a:n b:n t:n :}
   b a - t * 100 / a + ;

: SUM-TO ( n -- n )
   0 swap 0 ?do i + loop ;

: COUNT-DOWN ( n -- n )
   begin 1- dup 0 <= until ;

: FACT ( n -- n )
   dup 1 <= if drop 1 exit then
   dup 1- RECURSE * ;

\ Deterministic whatever the cell held before: the input is stored first, so the
\ result depends only on the argument.
: CELL-BUMP ( n -- n )
   BUMP-CELL !
   BUMP-CELL @ 1+ dup BUMP-CELL ! ;

: BYTE-SUM ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 u 0 ?do i a + c@ + loop ;

: BYTE-FIND ( ptr u8 n n -- n ) {: a:ptr u:n c:n :}
   u 0 ?do
      i a + c@ c = if i unloop exit then
   loop
   -1 ;

;package
