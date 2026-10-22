\ Loaded after the native window's checked store replaces its bootstrap store.
\ The preceding check-hook/roles definitions recorded calls into CWIN-BOOT.
package WINDOW-CALL-STORE

\ The retired header must hold no transient pointer. The replacement owns a
\ live allocation and its capacity, and its normal call-fact readers still
\ compile this body. A lost capacity would skip the old mapping's munmap.
: ADOPTED ( -- )
   CWIN-STATE drop drop CWIN-BOOT = if 77 throw then
   CWIN-BOOT @ 0= 0= if 77 throw then
   CWIN-BOOT-N @ 0 <> if 77 throw then
   CWIN-BOOT-CAP @ 0 <> if 77 throw then
   CWIN-STATE drop drop @ 0= if 77 throw then
   CWIN-STATE nip nip @ 0= if 77 throw then ;
ADOPTED

;package
