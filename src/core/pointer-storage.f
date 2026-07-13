\ pointer-storage.f - pointer-valued global storage definer.

: PTR-VARIABLE ( -- )
   create 0 , does> ( -- ptr ptr a ) 0 ptr-field ;
