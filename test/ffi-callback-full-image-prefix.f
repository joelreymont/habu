\ A declared native C entry is baked into a full saved engine image.
1 set-tier
require src/lib/ffi-callback.f

CALLBACK: SAVED-COMPARE ( ptr u8 ptr u8 -- i32 ) 0 FALLBACK ;CALLBACK

: SAVED-ORDER ( ptr u8 ptr u8 -- n ) {: a b :}
   a CELL-VIEW @ b CELL-VIEW @ - ;

' SAVED-ORDER SAVED-COMPARE-BODY !
