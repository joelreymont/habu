\ run-attempts-lib.f - checked candidate enumeration for attempt runners.
\
\ Load after lib/errors.f, lib/string.f, and lib/fs.f.

64 constant RA-ROUND-MAX
20 constant RA-NUM-CAP
46 constant RA-DOT
102 constant RA-F

-3240 constant E-RA-CAPACITY
-3241 constant E-RA-MISSING

create RA-ROUND-PATHS RA-ROUND-MAX FS-PATH-CAP * allot
create RA-ROUND-US RA-ROUND-MAX cells allot
create RA-TMP-PATH FS-PATH-CAP allot
create RA-NAME-BUF FS-PATH-CAP allot
create RA-NUM-BUF RA-NUM-CAP allot

variable RA-ROUND#
variable RA-TMP-U
variable RA-NAME-U
variable RA-NUM-I
variable RA-I

: RA-CHECK-ROUND ( n -- ) {: idx :}
   idx 0 < if E-RA-CAPACITY throw then
   idx RA-ROUND-MAX >= if E-RA-CAPACITY throw then ;

: RA-ROUND-SLOT ( n -- ptr u8 ) {: idx :}
   idx RA-CHECK-ROUND
   RA-ROUND-PATHS idx FS-PATH-CAP * + ;

: RA-ROUND-U-PTR ( n -- ptr n ) {: idx :}
   idx RA-CHECK-ROUND
   RA-ROUND-US idx cells + ;

: RA-ROUND$ ( n -- ptr u8 n ) {: idx :}
   idx RA-ROUND-SLOT idx RA-ROUND-U-PTR @ ;

: RA-CHECK-PATH-U ( n -- ) {: u :}
   u 0 < if E-RA-CAPACITY throw then
   u FS-PATH-CAP > if E-RA-CAPACITY throw then ;

: RA-RESET ( -- )
   0 RA-ROUND# !
   0 RA-TMP-U !
   0 RA-NAME-U ! ;

: RA-NAME-ROOM ( n -- ) {: add :}
   add 0 < if E-RA-CAPACITY throw then
   add FS-PATH-CAP RA-NAME-U @ - > if E-RA-CAPACITY throw then ;

: RA-NAME+ ( ptr u8 n -- ) {: a:ptr u :}
   u RA-NAME-ROOM
   a RA-NAME-BUF RA-NAME-U @ + u BYTE-COPY
   RA-NAME-U @ u + RA-NAME-U ! ;

: RA-NAME-C ( n -- ) {: c :}
   1 RA-NAME-ROOM
   c RA-NAME-BUF RA-NAME-U @ + c!
   RA-NAME-U @ 1+ RA-NAME-U ! ;

: RA-NAME-U+ ( n -- ) {: u :}
   u 0 < if E-RA-CAPACITY throw then
   RA-NUM-CAP RA-NUM-I !
   u 0= if s" 0" RA-NAME+ exit then
   u begin dup 0 > while
      dup 10 mod 48 +
      RA-NUM-I @ 1- RA-NUM-I !
      RA-NUM-BUF RA-NUM-I @ + c!
      10 /
   repeat drop
   RA-NUM-BUF RA-NUM-I @ + RA-NUM-CAP RA-NUM-I @ - RA-NAME+ ;

: RA-NAME$ ( -- ptr u8 n )
   RA-NAME-BUF RA-NAME-U @ ;

: RA-ROUND-NAME! ( n -- ) {: round :}
   round 0 <= if E-RA-CAPACITY throw then
   0 RA-NAME-U !
   round RA-NAME-U+
   RA-DOT RA-NAME-C
   RA-F RA-NAME-C ;

: RA-SUFFIX-F-PATH ( ptr u8 n ptr u8 -- n ) {: a:ptr u dst:ptr :}
   u 2 + RA-CHECK-PATH-U
   a dst u BYTE-COPY
   RA-DOT dst u + c!
   RA-F dst u 1+ + c!
   u 2 + ;

: RA-TASK-DIR! ( ptr u8 n ptr u8 n -- ) {: root:ptr rootu id:ptr idu :}
   root rootu id idu RA-TMP-PATH JOIN-PATH RA-TMP-U ! ;

: RA-TASK-DIR$ ( -- ptr u8 n )
   RA-TMP-PATH RA-TMP-U @ ;

: RA-SINGLE-PATH! ( ptr u8 n ptr u8 n n -- ) {: root:ptr rootu id:ptr idu idx :}
   root rootu id idu RA-TMP-PATH JOIN-PATH {: baseu :}
   RA-TMP-PATH baseu idx RA-ROUND-SLOT RA-SUFFIX-F-PATH
   idx RA-ROUND-U-PTR ! ;

: RA-ROUND-PATH! ( ptr u8 n n n -- ) {: dir:ptr diru round idx :}
   round RA-ROUND-NAME!
   dir diru RA-NAME$ idx RA-ROUND-SLOT JOIN-PATH
   idx RA-ROUND-U-PTR ! ;

: RA-MAYBE-ADD-ROUND ( ptr u8 n n -- ) {: dir:ptr diru round :}
   RA-ROUND# @ {: idx :}
   idx RA-CHECK-ROUND
   dir diru round idx RA-ROUND-PATH!
   idx RA-ROUND$ FILE? if
      idx 1+ RA-ROUND# !
   then ;

: RA-REQUIRE-ROUNDS ( -- )
   RA-ROUND# @ 0= if E-RA-MISSING throw then ;

: RA-REQUIRE-SINGLE ( -- )
   0 RA-ROUND$ FILE? 0= if E-RA-MISSING throw then ;

: RA-ENUM-ROUND-DIR ( ptr u8 n -- n ) {: dir:ptr diru :}
   1 RA-I !
   begin RA-I @ RA-ROUND-MAX <= while
      dir diru RA-I @ RA-MAYBE-ADD-ROUND
      RA-I @ 1+ RA-I !
   repeat
   RA-REQUIRE-ROUNDS
   RA-ROUND# @ ;

: RA-CANDIDATES ( ptr u8 n ptr u8 n -- n ) {: root:ptr rootu id:ptr idu :}
   RA-RESET
   root rootu id idu RA-TASK-DIR!
   RA-TASK-DIR$ DIR? if
      RA-TASK-DIR$ RA-ENUM-ROUND-DIR exit
   then
   root rootu id idu 0 RA-SINGLE-PATH!
   RA-REQUIRE-SINGLE
   1 RA-ROUND# !
   1 ;
