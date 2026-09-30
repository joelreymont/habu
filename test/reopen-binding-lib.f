\ reopen-binding-lib.f - the defining file of package REOPEN-BIND, whose public
\ tails spell engine words. test/reopen-binding.f reopens the package from its
\ own file and names each of those tails bare.

package REOPEN-BIND

private

create REC 4 cells allot

public

\ Compiled before the package owns `!` and `@`: the engine's cell store and fetch.
: SET ( -- ) 7 REC 2 cells + ! ;
: GET ( -- n ) REC 2 cells + @ ;

\ The control: the same indexed fetch under a tail no engine word spells.
: NTH ( ptr n n -- n ) {: base:ptr off:n :} base off cells + @ ;

\ `@` is a spelling the checker types by a rule of its own; `dup` and `+` are
\ spellings the JIT lowers inline. Each is defined after its last use as the
\ engine's word, so the bodies above and `dup`'s own keep the engine meaning.
: @ ( ptr n n -- n ) NTH ;
: dup ( n -- n ) 1 + ;
: + ( n n -- n ) - ;

;package
