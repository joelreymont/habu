\ Native image layout baseline at the retained host-reader boundary. Read the
\ rows from NATIVE-LAYOUT, the owner the build uses to translate fixed slots.
\ These values pin the retained source layout before target data separates.

require lib/test.f
require tools/native-layout.f

package CTARGET-LAYOUT-TEST
private

\ Ordered (DATA offset, kind) pairs: kind 0 is an execution token, 1 is DATA.
\ A shift in one of these rows requires a capture compatibility review.
create GOLDEN
   $38 ,   0 ,   \ HOOK-CELL
   $27E8 , 0 ,   \ COMPILE-PREFLIGHT-CELL
   $27F0 , 0 ,   \ TOP-HOOK-CELL
   $2818 , 0 ,   \ EXIT-HOOK-CELL
   $2CF0 , 0 ,   \ NBACK-OBSERVE-CELL
   $2CF8 , 0 ,   \ NCOMP-PUBLISHED-CELL
   $2D00 , 0 ,   \ CODE-INVALIDATE-CELL
   $358 ,  0 ,   \ NCOMP-DISPATCH:XT-CELL
   $2D18 , 0 ,   \ NCOMP-DISPATCH:FIXED-SHADOW-CELL
   $2D20 , 0 ,   \ NCOMP-DISPATCH:DOES-SHADOW-CELL
   $2D08 , 1 ,   \ TASK-CHAIN-CELL
   $360 ,  1 ,   \ NCOMP-DISPATCH:DECL-CELL
   $368 ,  1 ,   \ NCOMP-DISPATCH:TARGET-DECL-CELL
   $3810 , 0 ,   \ PROVIDED-XT:EVALUATE-CELL
   $3808 , 0 ,   \ ENGINE-MAIN:XT-CELL
   $43A0 , 0 ,   \ APP-ENTRY:XT-CELL
   $3640 , 0 ,   \ REPLH-CELL
   $37E8 , 1 ,   \ BPWBASE-CELL
   $560 ,  0 ,   \ LASTC-CELL

19 constant ROWS

public

: RUN ( -- )
   T-RESET
   s" native image fixed-slot offsets and kinds" T-LABEL
   NATIVE-LAYOUT:CURRENT {: actual:ptr count:n :}
   count ROWS T=
   ROWS 2 * 0 ?do
      actual i cells + @ GOLDEN i cells + @ T=
   loop
   T-REPORT ;

;package

CTARGET-LAYOUT-TEST:RUN
