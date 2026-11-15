package ENTRY-GUARD-DIAGNOSTICS

: DIAGNOSTIC-WB ( ^u8 n ^u8 n -- ) 2drop 2drop ;
: CHECK ( -- )
   s" test/gate-entry-guard-target.f" s" expected refusal" DIAGNOSTIC-WB ;

;package
