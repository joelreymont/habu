\ Loaded after the native window's checked store replaces its bootstrap store.
\ The preceding check-hook/roles definitions recorded calls into CWIN-BOOT.
package WINDOW-CALL-STORE

\ The retired header must hold no transient pointer. The replacement owns a
\ live allocation, and its normal call-fact readers still compile this body.
: ADOPTED ( -- )
   CWIN-STATE CWIN-BOOT = if 77 throw then
   CWIN-BOOT @ 0= 0= if 77 throw then
   CWIN-BOOT CELL + BYTE-VIEW CELL-VIEW @ 0 <> if 77 throw then
   CWIN-BOOT 2 cells + BYTE-VIEW CELL-VIEW @ 0 <> if 77 throw then
   CWIN-STATE @ 0= if 77 throw then ;
ADOPTED

;package
