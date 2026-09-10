\ Large local frames and empty groups through the native load path.
require lib/test.f

package MANY-LOCALS-TEST

: BUMP ( n -- n ) 7 * 3 + ;


: ACROSS-CALL ( -- n )
   1 2 3 4 5 6 7 8
   9 10 11 12 13 14 15 16
   17 18 19 20 21 22 23 24
   25 26 27 28 29 30 31 32
   {: a01:n a02:n a03:n a04:n a05:n a06:n a07:n a08:n
      a09:n a10:n a11:n a12:n a13:n a14:n a15:n a16:n
      a17:n a18:n a19:n a20:n a21:n a22:n a23:n a24:n
      a25:n a26:n a27:n a28:n a29:n a30:n a31:n a32:n :}
   a01 BUMP a32 + ;


: ACROSS-LOOP ( -- n )
   1 2 3 4 5 6 7 8
   9 10 11 12 13 14 15 16
   17 18 19 20 21 22 23 24
   25 26 27 28 29 30 31 32
   {: a01:n a02:n a03:n a04:n a05:n a06:n a07:n a08:n
      a09:n a10:n a11:n a12:n a13:n a14:n a15:n a16:n
      a17:n a18:n a19:n a20:n a21:n a22:n a23:n a24:n
      a25:n a26:n a27:n a28:n a29:n a30:n a31:n a32:n :}
   0 3 0 ?do a32 + BUMP loop ;


: MANY-GROUPS ( -- n )
   {: :} {: :} {: :} {: :} {: :} {: :}
   {: :} {: :} {: :} {: :} {: :} {: :}
   {: :} {: :} {: :} {: :} {: :} {: :}
   42 ;


: RUN ( -- )
   T-RESET
   ACROSS-CALL 42 T=
   ACROSS-LOOP 12939 T=
   MANY-GROUPS 42 T=
   T-REPORT ;

RUN
;package
