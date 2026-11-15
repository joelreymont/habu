package ENTRY-GUARD-READ-ONLY

: ARG ( ^u8 n -- ) 2drop ;
: METADATA ( -- )
   s" test/gate-entry-guard-target.f" 2drop
   s" --load" ARG ;

;package
