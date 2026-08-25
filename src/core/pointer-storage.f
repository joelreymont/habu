\ pointer-storage.f - pointer-valued global storage definer.

: PTR-VARIABLE ( -- )
   create 0 , does> ( -- ptr ptr a ) 0 ptr-field ;

\ Persistence is explicit: scratch pointer slots use PTR-VARIABLE and never join
\ the relocation table; only a slot whose value must survive an image/capture uses
\ this definer.
: PERSISTED-PTR-VARIABLE ( -- )
   create here ptr-cell-mark 0 , does> ( -- ptr ptr a ) 0 ptr-field ;
