\ set-test.f — focused tests for tools/lint/intern.f helpers and the
\ LINT-MAIN attributed-failure seam they throw into.

require lib/errors.f
require lib/memory.f
require lib/vector.f
require tools/lint/text.f
require tools/lint/intern.f
require tools/lint/token.f
require tools/lint/lib.f

package LINT-INTERN
private

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
   LINT-INTERN:RESET
   s" alpha" LINT-INTERN:ADD 0 ASSERT=
   s" beta" LINT-INTERN:ADD 1 ASSERT=
   s" alpha" LINT-INTERN:ADD 0 ASSERT=
   LINT-INTERN:COUNT 2 ASSERT=
   0 LINT-INTERN:TEXT s" alpha" ASSERT$
   1 LINT-INTERN:TEXT s" beta" ASSERT$ ;

: TEST-STORAGE  ( -- )
   LINT-INTERN:RESET
   97 MUT!  MUT-BUF 3 LINT-INTERN:ADD 0 ASSERT=
   120 MUT!
   0 LINT-INTERN:TEXT s" abc" ASSERT$
   MUT-BUF 3 s" xbc" ASSERT$ ;

: TEST-CASE  ( -- )
   LINT-INTERN:RESET
   s" Alpha" LINT-INTERN:ADD 0 ASSERT=
   s" alpha" LINT-INTERN:ADD 1 ASSERT=
   s" Alpha" LINT-INTERN:FIND 0 ASSERT=
   s" ALPHA" LINT-INTERN:FIND -1 ASSERT=
   s" ALPHA" LINT-INTERN:HAS? 0= ASSERT
   LINT-INTERN:RESET
   s" Alpha" LINT-INTERN:ADD-FOLD 0 ASSERT=
   s" alpha" LINT-INTERN:ADD-FOLD 0 ASSERT=
   s" ALPHA" LINT-INTERN:HAS-FOLD? ASSERT
   0 LINT-INTERN:TEXT s" alpha" ASSERT$ ;

: TEST-MEMBERSHIP  ( -- )
   LINT-INTERN:RESET
   s" one" LINT-INTERN:ADD drop
   s" two" LINT-INTERN:ADD drop
   s" one" LINT-INTERN:HAS? ASSERT
   s" two" LINT-INTERN:FIND 1 ASSERT=
   s" nope" LINT-INTERN:HAS? 0= ASSERT
   s" nope" LINT-INTERN:FIND -1 ASSERT= ;

: FILL-WALK  {: limit :}  ( -- )
   LINT-INTERN:RESET  0 SET-I !
   begin SET-I @ limit < while
      SET-I @ KEY$ LINT-INTERN:ADD SET-I @ ASSERT=
      SET-I @ 1+ SET-I !
   repeat ;
: CHECK-WALK  {: limit :}  ( -- )
   0 SET-I !
   begin SET-I @ limit < while
      SET-I @ KEY$ LINT-INTERN:FIND SET-I @ ASSERT=
      SET-I @ 1+ SET-I !
   repeat ;
: TEST-WALK  ( -- )
   WALK-LIMIT FILL-WALK
   LINT-INTERN:COUNT WALK-LIMIT ASSERT=
   WALK-LIMIT 1- KEY$ LINT-INTERN:HAS? ASSERT
   WALK-LIMIT KEY$ LINT-INTERN:HAS? 0= ASSERT
   WALK-LIMIT CHECK-WALK ;

: FILL-CAP  ( -- )
   LINT-INTERN:RESET  0 SET-I !
   begin SET-I @ MAX < while
      SET-I @ KEY$ LINT-INTERN:ADD SET-I @ ASSERT=
      SET-I @ 1+ SET-I !
   repeat
   LINT-INTERN:COUNT MAX ASSERT= ;
: COUNT-OVERFLOW  ( -- )  FILL-CAP  MAX KEY$ LINT-INTERN:ADD drop ;
: TEST-CAPACITY  ( -- )
   [: COUNT-OVERFLOW ;] catch LINT-INTERN:E-CAP ASSERT= ;

\ a cap throw routed through LINT-MAIN prints an attribution line naming the
\ tool and the code, and re-throws the same code (never a silent rc-only death)
$100 constant ATTR-CAP
create ATTR-BUF ATTR-CAP allot

: ATTR-THROW  ( -- )
   s" set-test" LINT-INTERN:E-CAP LINT-MAIN ;
: TEST-ATTRIBUTION  ( -- )
   ATTR-BUF ATTR-CAP LINT-OUT-BUFFER!
   [: ATTR-THROW ;] catch
   LINT-OUT-BUFFER-OFF
   LINT-INTERN:E-CAP ASSERT=
   LINT-OUT$ s" set-test: threw " LINT-STARTS-WITH? ASSERT
   LINT-OUT$ s" (E-LINT-INTERN-CAP)" LINT-CONTAINS? ASSERT
   LINT-OUT$ 1- + c@ 10 = ASSERT ;
: TEST-ATTRIBUTION-OK  ( -- )
   ATTR-BUF ATTR-CAP LINT-OUT-BUFFER!
   s" set-test" 0 LINT-MAIN
   LINT-OUT-BUFFER-OFF
   LINT-OUT$ nip 0 ASSERT= ;

: SET-TEST  ( -- )
   1 TEST-N !
   TEST-DUPLICATES
   TEST-STORAGE
   TEST-CASE
   TEST-MEMBERSHIP
   TEST-WALK
   TEST-CAPACITY
   TEST-ATTRIBUTION
   TEST-ATTRIBUTION-OK
   LINT-INTERN:RESET
   s" set-test: ok (" type TEST-N @ 1- . s"  assertions)" type cr ;

SET-TEST

;package
