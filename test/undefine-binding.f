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
\ The checker's rules for engine words it types by what they do - `@`, `!`,
\ `?dup`, `record-at` - follow the identity the engine registered on the word's
\ symbol (src/core/checker.f CTL-INTRINSIC), never the spelling: a redefinition
\ after `undefine` is a symbol with none, so it is judged by its own declared
\ effect, and the compilers call it.
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

STRUCTURE ubrec 0 FIELD v n ;STRUCTURE
1 TYPED-BUFFER UB-RECS ubrec

\ Redefined while `dup` and `+` are still the engine's words.
undefine @
: @ ( n -- n n ) dup ;
undefine !
: ! ( n n -- n ) + ;
undefine ?dup
: ?dup ( n -- n n ) dup ;
undefine record-at
: record-at ( ptr ubrec n -- n ) 2drop 42 ;

undefine dup
: dup ( n -- n ) 100 + ;
undefine +
: + ( n n -- n ) - ;

package UNDEFINE-BINDING-TEST

private

s" a redefined operator spelling is checked as the new word" T-LABEL
s" UB-NEW-DUP ( -- n ) 5 dup" CHECK-QUIET-CANDIDATE! -1 T=
s" UB-OLD-DUP ( -- n n ) 5 dup" CHECK-QUIET-CANDIDATE! 0 T=

s" a redefined engine word is checked by its own effect, not the engine rule" T-LABEL
s" UB-FETCH ( -- n n ) 5 @" CHECK-QUIET-CANDIDATE! -1 T=
s" UB-STORE ( -- n ) 2 3 !" CHECK-QUIET-CANDIDATE! -1 T=
s" UB-QDUP ( -- n n ) 5 ?dup" CHECK-QUIET-CANDIDATE! -1 T=
s" UB-REC ( ptr ubrec -- n ) 0 record-at" CHECK-QUIET-CANDIDATE! -1 T=
s" UB-REC-STEP ( ptr ubrec -- ptr ubrec ) 0 record-at" CHECK-QUIET-CANDIDATE! 0 T=

\ A constant operand, a caller's operand and a local each reach the new word:
\ where the operands sit does not choose the binding.
: UB-DUP ( -- n ) 5 dup ;
: UB-DUP-ARG ( n -- n ) dup ;
: UB-DUP-LOCAL ( n -- n ) {: a:n :} a dup ;
: UB-SUM ( -- n ) 5 3 + ;
: UB-SUM-ARGS ( n n -- n ) + ;
: UB-SUM-LOCALS ( n n -- n ) {: a:n b:n :} a b + ;
: UB-FETCH ( -- n n ) 5 @ ;
: UB-STORE ( -- n ) 2 3 ! ;
: UB-QDUP ( -- n n ) 5 ?dup ;
: UB-REC ( ptr ubrec -- n ) 0 record-at ;

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
   5 3 UB-SUM-LOCALS 2 T=
   s" a redefined engine word runs as the new word" T-LABEL
   UB-FETCH 5 T= 5 T=
   UB-STORE 5 T=
   UB-QDUP 5 T= 5 T=
   0 UB-RECS UB-REC 42 T= ;

RUN

;package

T-REPORT
