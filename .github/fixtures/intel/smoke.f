require .github/fixtures/intel/include.f
require .github/fixtures/intel/../intel/include.f
require lib/string.f

package INTELTEST
private

: ASSERT= ( n n -- )
   {: got:n want:n :}
   got want <> if s" assert mismatch" 70 die then ;


: ASSERT-TRUE ( bool -- )
   0= if s" expected true" 70 die then ;


: INC ( n -- n ) 1 + ;


: SUM-RANGE ( n -- n )
   0 swap 0 ?do i + loop ;


: FIB ( n -- n )
   dup 2 < if exit then
   dup 1- recurse swap 2 - recurse + ;

here 7 and 8 swap - 7 and allot
variable ATOM
create DOT-PATH 46 c, 0 c,
create PATH-OUT 4096 allot


: ATOMICS ( -- )
   0 ATOM !
   5 ATOM atomic-add 0 ASSERT=
   ATOM atomic@ 5 ASSERT=
   7 ATOM atomic!
   7 9 ATOM atomic-cas 7 ASSERT=
   50 99 ATOM atomic-cas 9 ASSERT=
   ATOM atomic@ 9 ASSERT=
   fence ;


: PATHS ( -- )
   123 PATH-OUT c!
   DOT-PATH PATH-OUT 1 realpath -2 ASSERT=
   PATH-OUT c@ 123 ASSERT=
   DOT-PATH PATH-OUT 0 realpath -2 ASSERT=
   DOT-PATH PATH-OUT 4096 realpath 0 > ASSERT-TRUE
   PATH-OUT c@ 47 ASSERT= ;

public

: RUN ( -- )
   41 INC 42 ASSERT=
   10 SUM-RANGE 45 ASSERT=
   10 FIB 55 ASSERT=
   ATOMICS PATHS
   40 INTELINC:ADD-TWO 42 ASSERT=
   s" same" s" same" STR= ASSERT-TRUE
   s" real-checker-and-runtime: ok" type cr ;

;package

INTELTEST:RUN
