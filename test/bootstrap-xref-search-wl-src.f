\ Recovery uses its shared linear scan and must return the record, not the xt.
0 set-check

package BOOT-RECORD
public
: ANSWER ( -- n ) 42 ;
;package

package BOOT-RECORD-TEST

TRUSTED: RECORD ( ptr u8 n n -- ptr n ) xref-search-wl ;

: FAIL ( -- ) s" bootstrap compiler record lookup" 1 die ;

: RUN ( -- )
   s" BOOT-RECORD" -1 RECORD dup 0= if FAIL then
   @ {: wid:n :}
   s" aNsWeR" wid RECORD dup 0= if FAIL then
   {: rec:ptr :}
   rec @ rec = if FAIL then
   rec @ s" ANSWER" wid search-wl <> if FAIL then
   s" BOOT-RECORD-ABSENT" 0 RECORD 0= 0= if FAIL then
   s" ok" type cr ;

RUN
;package
