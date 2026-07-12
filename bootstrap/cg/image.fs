\ image.fs -- shared executable image buffer for bootstrap target writers.

$220000 constant MSIZE
create MBUF MSIZE allot
variable MP
variable MLEN

: M-RESET ( -- )  MBUF MP ! ;

: M8  ( b -- )  MP @ c!  1 MP +! ;

: M16 ( h -- )  dup M8  8 rshift M8 ;

: M32 ( w -- )  dup M16  16 rshift M16 ;

: M64 ( x -- )  dup M32  32 rshift M32 ;

: M-HERE ( -- off )  MP @ MBUF - ;

: M-ZEROS ( n -- )  0 max 0 ?do 0 M8 loop ;

: M-BYTES ( addr u -- )  dup >r  MP @ swap move  r> MP +! ;

: M-NAME16 ( addr u -- )  dup >r  M-BYTES  16 r> - M-ZEROS ;

: M-PAD ( off -- )  M-HERE - M-ZEROS ;
