\ The clause's created effect reaches its definer's record (checker.f
\ DOES-EFF-TAKE), so a generates: row that restates it is refused.
: MK ( n -- ) create , does> ( -- n ) @ ;
generates: MK ( -- n )
." after" cr
