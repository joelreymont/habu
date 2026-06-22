\ set-test.f — focused tests for tools/lint/lib.f intern/set helpers.
\ Run: cat tools/lint/lib.f tools/lint/set-test.f | bin/hb

variable TEST-N
: ASSERT  ( bool -- )
   0= if
      s" set-test failed at assertion " type TEST-N @ . cr
      s" set-test failed" 1 die
   then
   TEST-N @ 1+ TEST-N ! ;
: ASSERT=  ( n n -- )  = ASSERT ;
: ASSERT$  ( ptr u8 n ptr u8 n -- )  STR= ASSERT ;

create MUT-BUF 3 allot
create KEY-BUF 2 allot
variable SET-I
$40 constant WALK-LIMIT

: MUT!  {: c :}  ( -- )
   c MUT-BUF c!
   98 MUT-BUF 1+ c!
   99 MUT-BUF 2 + c! ;
: KEY$  {: n :}  ( -- a u )
   n $FF and KEY-BUF c!
   n 8 rshift $FF and KEY-BUF 1+ c!
   KEY-BUF 2 ;

: TEST-DUPLICATES  ( -- )
   INTERN-RESET
   s" alpha" INTERN 0 ASSERT=
   s" beta" INTERN 1 ASSERT=
   s" alpha" INTERN 0 ASSERT=
   INTERN# 2 ASSERT=
   0 INTERN$ s" alpha" ASSERT$
   1 INTERN$ s" beta" ASSERT$ ;

: TEST-STORAGE  ( -- )
   INTERN-RESET
   97 MUT!  MUT-BUF 3 INTERN 0 ASSERT=
   120 MUT!
   0 INTERN$ s" abc" ASSERT$
   MUT-BUF 3 s" xbc" ASSERT$ ;

: TEST-CASE  ( -- )
   INTERN-RESET
   s" Alpha" INTERN 0 ASSERT=
   s" alpha" INTERN 1 ASSERT=
   s" Alpha" INTERN-FIND 0 ASSERT=
   s" ALPHA" INTERN-FIND -1 ASSERT=
   s" ALPHA" INTERN? 0= ASSERT
   INTERN-RESET
   s" Alpha" INTERN-FOLD 0 ASSERT=
   s" alpha" INTERN-FOLD 0 ASSERT=
   s" ALPHA" INTERN-FOLD? ASSERT
   0 INTERN$ s" alpha" ASSERT$ ;

: TEST-MEMBERSHIP  ( -- )
   INTERN-RESET
   s" one" INTERN drop
   s" two" INTERN drop
   s" one" INTERN? ASSERT
   s" two" INTERN-FIND 1 ASSERT=
   s" nope" INTERN? 0= ASSERT
   s" nope" INTERN-FIND -1 ASSERT= ;

: FILL-WALK  {: limit :}  ( -- )
   INTERN-RESET  0 SET-I !
   begin SET-I @ limit < while
      SET-I @ KEY$ INTERN SET-I @ ASSERT=
      SET-I @ 1+ SET-I !
   repeat ;
: CHECK-WALK  {: limit :}  ( -- )
   0 SET-I !
   begin SET-I @ limit < while
      SET-I @ KEY$ INTERN-FIND SET-I @ ASSERT=
      SET-I @ 1+ SET-I !
   repeat ;
: TEST-WALK  ( -- )
   WALK-LIMIT FILL-WALK
   INTERN# WALK-LIMIT ASSERT=
   WALK-LIMIT 1- KEY$ INTERN? ASSERT
   WALK-LIMIT KEY$ INTERN? 0= ASSERT
   WALK-LIMIT CHECK-WALK ;

: FILL-CAP  ( -- )
   INTERN-RESET  0 SET-I !
   begin SET-I @ INTERN-MAX < while
      SET-I @ KEY$ INTERN SET-I @ ASSERT=
      SET-I @ 1+ SET-I !
   repeat
   INTERN# INTERN-MAX ASSERT= ;
: COUNT-OVERFLOW  ( -- )  FILL-CAP  INTERN-MAX KEY$ INTERN drop ;
: TEST-CAPACITY  ( -- )
   [: COUNT-OVERFLOW ;] catch E-LINT-INTERN-CAP ASSERT= ;

: SET-TEST  ( -- )
   1 TEST-N !
   TEST-DUPLICATES
   TEST-STORAGE
   TEST-CASE
   TEST-MEMBERSHIP
   TEST-WALK
   TEST-CAPACITY
   INTERN-RESET
   s" set-test: ok (" type TEST-N @ 1- . s"  assertions)" type cr ;

SET-TEST
