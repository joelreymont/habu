\ reopen-binding.f - a bare tail names the word its scope holds, and the checker
\ certifies that word: the one the compiler calls.
\
\ Package REOPEN-BIND (test/reopen-binding-lib.f) publishes `@`, `dup` and `+`,
\ each with an effect the engine's word of that spelling does not have. This
\ file reopens the package and names the tails bare. Before the fix the checker
\ typed a bare `@` by the engine's fetch rule while the compiler called the
\ package word - `REC @` certified ( -- n ) and ran into `hb: stack bounds
\ exceeded (data)`, exit 102 - and the JIT lowered `5 dup` and `5 3 +` as the
\ engine's operators while the checker certified the package's. The same rows
\ run under the optimizing compiler in the `-aot` twin.
\
\ Run: bin/hb --load test/reopen-binding.f

require lib/test.f
require test/reopen-binding-lib.f
require test/reopen-binding-early.f
require test/reopen-binding-late.f

T-RESET

package REOPEN-BIND

public

s" a bare tail is checked against the word the open package holds" T-LABEL
s" RE-CORE-FETCH ( -- n ) REC @" CHECK-QUIET-CANDIDATE! 0 T=
s" RE-CORE-DUP ( -- n n ) 5 dup" CHECK-QUIET-CANDIDATE! 0 T=
s" RE-PKG-FETCH ( -- n ) REC 2 @" CHECK-QUIET-CANDIDATE! -1 T=
s" RE-PKG-DUP ( -- n ) 5 dup" CHECK-QUIET-CANDIDATE! -1 T=
s" RE-CONTROL ( -- n ) REC 2 NTH" CHECK-QUIET-CANDIDATE! -1 T=

\ The tail in a comment and in a string binds nothing; the bare one after them
\ is the package fetch.
: RE-GET ( -- n )
   REC          \ not yet a fetch: @ in a comment is no token
   s" @ !" 2drop
   2 @ ;

\ Bare and qualified in one body are one word.
: RE-BOTH ( -- n n ) REC 2 @  REC 2 REOPEN-BIND:@ ;

: RE-NTH ( -- n ) REC 2 NTH ;

\ A constant operand, a caller's operand and a local each reach the package
\ word: where the operands sit does not choose the binding.
: RE-DUP ( -- n ) 5 dup ;
: RE-DUP-ARG ( n -- n ) dup ;
: RE-SUM ( -- n ) 5 3 + ;
: RE-SUM-ARGS ( n n -- n ) + ;
: RE-SUM-LOCALS ( n n -- n ) {: a:n b:n :} a b + ;

;package

\ A candidate is checked in the scope it is asked in: here, the reopen of
\ REOPEN-ORDER that owns `@`.
package REOPEN-ORDER

s" a reopen that adds the tail binds it for every body after it" T-LABEL
s" RO-CORE-FETCH ( -- n ) SLOT @" CHECK-QUIET-CANDIDATE! 0 T=
s" RO-PKG-FETCH ( -- n ) SLOT 0 @" CHECK-QUIET-CANDIDATE! -1 T=

;package

package REOPEN-BIND-TEST

private

variable HOLD

\ Outside the package every spelling is the engine's again.
: CORE-FETCH ( -- n ) HOLD @ ;
: CORE-PAIR ( -- n n ) 5 dup ;
: CORE-SUM ( -- n ) 5 3 + ;

: RUN ( -- )
   s" a reopened body runs the package word it was certified against" T-LABEL
   REOPEN-BIND:SET
   REOPEN-BIND:GET 7 T=
   REOPEN-BIND:RE-GET 7 T=
   REOPEN-BIND:RE-BOTH 7 T= 7 T=
   REOPEN-BIND:RE-NTH 7 T=
   s" an operator spelling the package owns is the package word at every operand" T-LABEL
   REOPEN-BIND:RE-DUP 6 T=
   5 REOPEN-BIND:RE-DUP-ARG 6 T=
   REOPEN-BIND:RE-SUM 2 T=
   5 3 REOPEN-BIND:RE-SUM-ARGS 2 T=
   5 3 REOPEN-BIND:RE-SUM-LOCALS 2 T=
   s" a body compiled before the package owns the tail keeps the engine word" T-LABEL
   41 REOPEN-ORDER:PUT
   REOPEN-ORDER:EARLY 41 T=
   REOPEN-ORDER:LATE 41 T=
   s" the engine words are unchanged outside the package" T-LABEL
   9 HOLD !
   CORE-FETCH 9 T=
   CORE-PAIR 5 T= 5 T=
   CORE-SUM 8 T= ;

RUN

;package

T-REPORT
