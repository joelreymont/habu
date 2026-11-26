\ call-window.f - the checked call-window store.

\ checker.f reads the store through CWIN-STATE; this package only installs it.
package CHECKER-CALLS

PTR-VARIABLE STATE  variable STATE-N  variable STATE-CAP

EXPORT CHECK:CWIN-STATE
\ The owner can record calls before this checked header is loaded. Transfer
\ that allocation with its rows, then clear the retired header so capture has
\ only one live store to prepare. Reinstalling the current header is harmless.
: INSTALL ( -- )
   CWIN-STATE {: prior:^ prior-n:^ prior-cap:^ :}
   prior STATE = if exit then
   prior @ STATE !
   prior-n @ STATE-N !
   prior-cap @ STATE-CAP !
   NULL-PTR prior !
   0 prior-n !
   0 prior-cap !
   [: STATE STATE-N STATE-CAP ;] is CWIN-STATE ;
INSTALL

seal-package
;package
