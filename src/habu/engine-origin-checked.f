\ Observe checked definitions between the hook installation and the full
\ engine-origin module's load. The prefix capture owner is already present.

package ENGINE-ORIGIN-EARLY

: FIRST-TOKEN ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   0 begin dup u < if a over + c@ 32 > else 0 0= 0= then while 1+ repeat
   a swap ;

: CHECKED-HOOK ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u LOWER-CERT-HOOK:HOOK dup -1 = if a u FIRST-TOKEN CHECKED-TAKE then ;

public

TRUSTED: INSTALL ( -- ) ['] CHECKED-HOOK set-check ;

;package
