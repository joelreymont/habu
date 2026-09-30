\ undefine-binding.f - an operator spelling redefined after `undefine` is the
\ new word at every operand, and the checker certifies that word: the one the
\ compiler calls.
\
\ `undefine dup` retires the engine's `dup`; the `: dup` after it is a global
\ record of its own. The JIT's operator rows lowered a token by its spelling
\ whenever the lookup bound a global record, so `5 dup` ran the engine's `dup`
\ and left two cells where the checker certified the new word's one, and
\ `5 3 +` folded to 8 where the certified word answers 2. A row now claims a
\ token only when the lookup binds that row's own seeded primitive
\ (src/habu/habu2.f C-OP-ROW-GATE). The same rows run under the optimizing
\ compiler in the `-aot` twin.
\
\ `undefine` is global for the process, so these rows live in a file of their
\ own: nothing else compiles against the redefined spellings.
\
\ Run: bin/hb --load test/undefine-binding.f

require lib/test.f

T-RESET

package UNDEFINE-BINDING-TEST

private

\ Compiled while `dup` and `+` are the engine's words.
: EARLY-PAIR ( -- n n ) 5 dup ;
: EARLY-SUM ( -- n ) 5 3 + ;

;package

undefine dup
: dup ( n -- n ) 100 + ;
undefine +
: + ( n n -- n ) - ;

package UNDEFINE-BINDING-TEST

private

s" a redefined operator spelling is checked as the new word" T-LABEL
s" UB-NEW-DUP ( -- n ) 5 dup" CHECK-QUIET-CANDIDATE! -1 T=
s" UB-OLD-DUP ( -- n n ) 5 dup" CHECK-QUIET-CANDIDATE! 0 T=

\ A constant operand, a caller's operand and a local each reach the new word:
\ where the operands sit does not choose the binding.
: UB-DUP ( -- n ) 5 dup ;
: UB-DUP-ARG ( n -- n ) dup ;
: UB-DUP-LOCAL ( n -- n ) {: a:n :} a dup ;
: UB-SUM ( -- n ) 5 3 + ;
: UB-SUM-ARGS ( n n -- n ) + ;
: UB-SUM-LOCALS ( n n -- n ) {: a:n b:n :} a b + ;

: RUN ( -- )
   s" a body compiled before the redefinition keeps the engine word" T-LABEL
   EARLY-PAIR 5 T= 5 T=
   EARLY-SUM 8 T=
   s" a redefined operator spelling is the new word at every operand" T-LABEL
   UB-DUP 105 T=
   5 UB-DUP-ARG 105 T=
   5 UB-DUP-LOCAL 105 T=
   UB-SUM 2 T=
   5 3 UB-SUM-ARGS 2 T=
   5 3 UB-SUM-LOCALS 2 T= ;

RUN

;package

T-REPORT
