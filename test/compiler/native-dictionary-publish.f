\ Publication must make each completed native record immediately searchable.
require lib/test.f

\ Tier 1 first: a native record is what the optimizing compiler publishes, so
\ the index this file reads is only filled by it.
1 set-tier

package NDICT-PUBLISH-TEST

TRUSTED: RECORD ( ptr u8 n n -- ptr n ) xref-search-wl ;
: EV ( ptr u8 n -- ) INCLUDE-EVALUATE ;

: ORDINARY ( -- )
   ndict@ {: first:n :}
   s" : NDP-INCR ( n -- n ) 1 + ; : NDP-CALL ( n -- n ) NDP-INCR ; 40 NDP-CALL 41 T=" EV
   ndict@ first 2 + T=
   s" NDP-INCR" 0 RECORD first XREF-REC = TTRUE
   s" ndp-call" 0 RECORD first 1+ XREF-REC = TTRUE ;

: DOES-COMPANION ( -- )
   ndict@ {: first:n :}
   s" TRUSTED: NDP-MAKE ( n -- ) create , does> ( -- n ) @ ; 42 NDP-MAKE NDP-ITEM : NDP-GET ( -- n ) NDP-ITEM ; NDP-GET 42 T=" EV
   ndict@ first 4 + T=
   s" NDP-MAKE" 0 RECORD first XREF-REC = TTRUE
   s" NDP-MAKE;does" 0 RECORD first 1+ XREF-REC = TTRUE
   s" NDP-ITEM" 0 RECORD first 2 + XREF-REC = TTRUE ;

: SCOPED-COLLISIONS ( -- )
   s" package NDP-SCOPE : HIDDEN ( -- n ) 7 ; public : NDB-COLLIDE-548 ( -- n ) HIDDEN ; : NDB-COLLIDE-1022 ( -- n ) 9 ; ;package" EV
   s" NDP-SCOPE:NDB-COLLIDE-548 7 T= ndp-scope:ndb-collide-1022 9 T=" EV
   s" NDP-SCOPE:HIDDEN" NDICT:SPELL-START 0 T=
   s" HIDDEN" NDICT:SPELL-START 0 T=
   s" undefine NDP-SCOPE:NDB-COLLIDE-548" EV
   s" NDP-SCOPE:NDB-COLLIDE-548" NDICT:SPELL-START 0 T=
   s" NDP-SCOPE:NDB-COLLIDE-1022 9 T=" EV
   s" package NDP-SCOPE public : NDB-COLLIDE-548 ( -- n ) 11 ; ;package NDP-SCOPE:NDB-COLLIDE-548 11 T=" EV ;

: FAIL-FRAME ( -- )
   s" : NDP-ROLLED ( -- n ) 13 ; NDP-ROLLED 13 T= 73 throw" EV ;

: ROLLBACK ( -- )
   ndict@ {: first:n :}
   ['] FAIL-FRAME 73 TTHROWS
   ndict@ first T=
   s" NDP-ROLLED" NDICT:SPELL-START 0 T=
   s" : NDP-REGROWN ( -- n ) 17 ; NDP-REGROWN 17 T=" EV
   s" NDP-REGROWN" 0 RECORD first XREF-REC = TTRUE
   s" NDP-ROLLED" NDICT:SPELL-START 0 T= ;

: RESTORE ( -- )
   ndict@ {: first:n :}
   s" : NDP-RESTORED ( -- n ) 19 ;" EV
   first ndict!
   s" NDP-RESTORED" NDICT:SPELL-START 0 T=
   first 1+ ndict!
   s" NDP-RESTORED" 0 RECORD first XREF-REC = TTRUE
   s" NDP-RESTORED 19 T=" EV ;

: RUN ( -- )
   T-RESET
   s" ordinary native publication is immediately indexed" T-LABEL ORDINARY T-NEXT
   s" native DOES parent and companion keep their record identities" T-LABEL DOES-COMPANION T-NEXT
   s" qualified visibility, collision chains and retirement survive append" T-LABEL SCOPED-COLLISIONS T-NEXT
   s" failed evaluation reuses the same record slot" T-LABEL ROLLBACK T-NEXT
   s" general ndict! restore still rebuilds the live index" T-LABEL RESTORE T-NEXT
   T-REPORT ;

' RUN
;package
execute
