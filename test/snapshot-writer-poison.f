\ Plant and verify canaries in the live return-stack band before APP-IMAGE:SAVE.
\ The parent reads the saved DATA bytes and requires the entire band to be zero.

package SNAP-WRITER-POISON

$5253544B4C4F0001 constant LO-CANARY   \ "RSTKLO" + 1
$5253544B48490002 constant HI-CANARY   \ "RSTKHI" + 2

: PLANT ( -- )
   LO-CANARY data-base RSTK-OFF + !
   HI-CANARY data-base RSTK-END 8 - + ! ;

: PROVE-PLANTED ( -- )
   data-base RSTK-OFF + @ LO-CANARY <> if
      s" snapshot writer low return-stack poison failed" 70 die
   then
   data-base RSTK-END 8 - + @ HI-CANARY <> if
      s" snapshot writer high return-stack poison failed" 70 die
   then ;

: POISON ( -- )
   PLANT
   PROVE-PLANTED ;

POISON

;package
