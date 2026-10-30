\ cold-load-sources.f - a copy of every source tools/cold-load.f reloads, in
\ load order, kept until tools/cold-load-roles.f hashes it. cold-load.f loads
\ this file after src/core/include.f, before SHA-256 and DYNAMIC-STORAGE load,
\ so it requires nothing and maps its own log; a load frees its bytes on return.
package COLD-SOURCES
private

1 constant SOURCE     \ a path, its root and the bytes the load read
2 constant CHECKER    \ the checker has loaded
3 constant DONE       \ the load of a path and root returned
4 constant HEAD-CELLS \ kind, path, root and byte counts

\ The first mapping's bytes; a full log moves to one twice its size.
$100000 constant ROOM-MIN

PTR-VARIABLE LOG
variable CAP
variable USED
PTR-VARIABLE ANCHOR
TYPED-VARIABLE PRIOR [ ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- ]
TYPED-VARIABLE ON-SOURCE [ ptr u8 n ptr u8 n ptr u8 n -- ]
TYPED-VARIABLE ON-CHECKER [ -- ]
TYPED-VARIABLE ON-DONE [ ptr u8 n ptr u8 n -- ]

: CELL-PAD ( n -- n ) CELL 1 - + CELL negate and ;
: LOG-AT ( n -- ptr u8 ) LOG @ swap + ;
: CELL, ( n -- ) USED @ LOG-AT CELL-VIEW !  CELL USED +! ;


: UNMAP ( -- )
   CAP @ 0= if exit then
   LOG @ CAP @ munmap 0 < if s" cold-load: cannot release the source log" 76 die then
   0 CAP !  0 USED ! ;


: ROOM ( n -- )
   {: more:n :}
   USED @ more + {: need:n :}
   need CAP @ <= if exit then
   CAP @ 2 * need max ROOM-MIN max {: cap:n :}
   cap map-anon 0 <> if drop s" cold-load: cannot map the source log" 76 die then
   {: fresh:ptr :}
   USED @ {: used:n :}
   used 0 > if LOG @ fresh used BYTE-COPY then
   UNMAP
   fresh LOG !  cap CAP !  used USED ! ;


: SPAN, ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0 > if a USED @ LOG-AT u BYTE-COPY then
   u CELL-PAD USED +! ;


: RECORD ( n ptr u8 n ptr u8 n ptr u8 n -- )
   {: kind:n path:ptr pathu:n root:ptr rootu:n bytes:ptr size:n :}
   HEAD-CELLS CELL * pathu CELL-PAD + rootu CELL-PAD + size CELL-PAD + ROOM
   kind CELL,  pathu CELL,  rootu CELL,  size CELL,
   path pathu SPAN,  root rootu SPAN,  bytes size SPAN, ;


: HOOK ( ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- )
   {: path:ptr pathu:n root:ptr rootu:n bytes:ptr size:n q :}
   SOURCE path pathu root rootu bytes size RECORD
   path pathu root rootu bytes size q PRIOR @ execute
   DONE path pathu root rootu bytes 0 RECORD ;


\ Replay the record at offset OFF; answer the next record's offset.
: STEP ( n -- n )
   {: off:n :}
   off LOG-AT {: head:ptr :}
   head CELL-VIEW @ {: kind:n :}
   head CELL + CELL-VIEW @ {: pathu:n :}
   head 2 CELL * + CELL-VIEW @ {: rootu:n :}
   head 3 CELL * + CELL-VIEW @ {: size:n :}
   head HEAD-CELLS CELL * + {: path:ptr :}
   path pathu CELL-PAD + {: root:ptr :}
   root rootu CELL-PAD + {: bytes:ptr :}
   kind SOURCE = if path pathu root rootu bytes size ON-SOURCE @ execute then
   kind CHECKER = if ON-CHECKER @ execute then
   kind DONE = if path pathu root rootu ON-DONE @ execute then
   bytes size CELL-PAD + LOG @ - ;

public

\ The staged token after this word anchors STARTUP's distances.
: TEXT-AT ( -- ) parse-name drop ANCHOR ! ;
parses: TEXT-AT 1

\ A startup file cold-load.f staged as text: its path, relative to the engine
\ root, starts PATH-BACK bytes before the anchor, and its bytes BACK bytes before.
: STARTUP ( n n n n -- )
   {: path-back:n pathu:n back:n size:n :}
   SOURCE ANCHOR @ path-back - pathu SOURCE-ROOT:ENGINE$
      ANCHOR @ back - size RECORD ;

: CHECKER-LOADED ( -- ) CHECKER NULL$ NULL$ NULL$ RECORD ;

\ Load a file, recording each source the load reads and each load's return.
: LOAD ( ptr u8 n -- )
   SOURCE-UNIT:CURRENT PRIOR !
   [: HOOK ;] SOURCE-UNIT:USE
   [: included ;] [: PRIOR @ SOURCE-UNIT:USE ;] finally ;

\ Hand each record to its action in the order recorded, then free the log.
: REPLAY ( [ ptr u8 n ptr u8 n ptr u8 n -- ] [ -- ] [ ptr u8 n ptr u8 n -- ] -- )
   ON-DONE !  ON-CHECKER !  ON-SOURCE !
   0 begin dup USED @ < while STEP repeat drop
   UNMAP ;

;package
