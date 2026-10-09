\ A local is a name only inside the structure that declared it, checked and
\ trusted alike: inside, the latest local of a spelling; after, the outer one.
\ A case arm's local is the arm's, does> follows a block that closed its local,
\ a loop past an exit still sees the definition's locals, and a local answers
\ only to its own bytes, a word to any case.
: S ( n n -- n n ) {: a b :} a 0 > if b {: a :} a 10 * else 0 then a ;
trusted: ST ( n n -- n n ) {: a b :} a 0 > if b {: a :} a 10 * else 0 then a ;
: C ( n -- n ) case 1 of 10 {: a :} a endof 2 of 20 endof 0 swap endcase ;
trusted: CT ( n -- n ) 7 {: x :} case 1 of x endof 0 swap endcase x + ;
: U ( n -- n ) {: a :} 0 begin 1 + dup {: a :} a 3 = until a + ;
trusted: UT ( n -- n ) {: a :} 0 begin 1 + dup {: a :} a 3 = until a + ;
trusted: XB ( n -- n ) {: a :} a exit begin a 0= until a ;
trusted: XD ( n -- n ) {: a :} a exit 3 0 do a drop loop a ;
: K ( -- n ) 100 ;
: KC ( n -- n n ) {: k :} k K ;
trusted: KT ( n -- n n ) {: k :} k K ;
: MK ( n -- ) dup 0 > if dup {: a :} a drop then create , does> ( -- n ) @ 1 + ;
trusted: MT ( n -- ) dup 0 > if dup {: a :} a drop then create , does> ( -- n ) @ 2 + ;
: MC ( n -- ) create , does> ( -- n ) {: p :} p @ 3 + ;
7 MK X  7 MT Y  7 MC Z
: MAIN ( -- )
   3 4 S . .  3 4 ST . . cr
   1 C . 2 C . 3 C . 1 CT . cr
   10 U . 10 UT . 5 XB . 5 XD . cr
   1 KC . .  2 KT . . cr
   X . Y . Z . cr ;
MAIN
