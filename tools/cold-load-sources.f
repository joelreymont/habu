\ cold-load-sources.f - every file tools/cold-load.f reads, in load order: its
\ bytes until REPLAY hands them to tools/cold-load-roles.f, then its path, root
\ and SHA-256 until DRAIN. cold-load.f stages this file before SHA-256 and
\ DYNAMIC-STORAGE load, so it requires nothing and maps its own log; a load
\ frees its bytes on return.
package COLD-SOURCES
private

1 constant SOURCE     \ a producer role's path, its root and the bytes read
2 constant CHECKER    \ the checker has loaded
3 constant DONE       \ the load of a path and root returned
4 constant DRIVER     \ a cold load driver file's path, root and bytes read
5 constant KEPT       \ a path, its root and the SHA-256 of the bytes read
4 constant HEAD-CELLS \ kind, path, root and byte counts

\ The first mapping's bytes; a full log moves to one twice its size.
$100000 constant ROOM-MIN

PTR-VARIABLE LOG
variable CAP
variable USED
PTR-VARIABLE ANCHOR
\ The kind STARTUP and HOOK record: SOURCE until LOAD returns, then DRIVER.
variable READ-KIND
create DIGEST-OUT 32 allot
TYPED-VARIABLE PRIOR [ ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- ]
TYPED-VARIABLE ON-SOURCE [ ptr u8 n ptr u8 n ptr u8 n -- ]
TYPED-VARIABLE ON-CHECKER [ -- ]
TYPED-VARIABLE ON-DONE [ ptr u8 n ptr u8 n -- ]
TYPED-VARIABLE DIGEST [ ptr u8 n ptr u8 -- ]

SOURCE READ-KIND !

: CELL-PAD ( n -- n ) CELL 1 - + CELL negate and ;
: LOG-AT ( n -- ptr u8 ) LOG @ swap + ;
: CELL, ( n -- ) USED @ LOG-AT CELL-VIEW !  CELL USED +! ;


: RELEASE ( ptr u8 n -- )
   {: log:ptr cap:n :}
   cap 0= if exit then
   log cap munmap 0 < if s" cold-load: cannot release the source log" 76 die then ;


: UNMAP ( -- ) LOG @ CAP @ RELEASE  0 CAP !  0 USED ! ;


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
   READ-KIND @ {: kind:n :}
   kind path pathu root rootu bytes size RECORD
   path pathu root rootu bytes size q PRIOR @ execute
   kind SOURCE = if DONE path pathu root rootu bytes 0 RECORD then ;


\ The record at offset OFF of the log at BASE: its kind, path, root, bytes and
\ the next record's offset.
: ROW ( ptr u8 n -- n ptr u8 n ptr u8 n ptr u8 n n )
   {: base:ptr off:n :}
   base off + {: head:ptr :}
   head CELL-VIEW @ {: kind:n :}
   head CELL + CELL-VIEW @ {: pathu:n :}
   head 2 CELL * + CELL-VIEW @ {: rootu:n :}
   head 3 CELL * + CELL-VIEW @ {: size:n :}
   head HEAD-CELLS CELL * + {: path:ptr :}
   path pathu CELL-PAD + {: root:ptr :}
   root rootu CELL-PAD + {: bytes:ptr :}
   kind path pathu root rootu bytes size
   bytes size CELL-PAD + base - ;


\ Replay the record at offset OFF of the log at BASE into the actions and the
\ log of kept rows; answer the next record's offset.
: STEP ( ptr u8 n -- n )
   ROW {: kind:n path:ptr pathu:n root:ptr rootu:n bytes:ptr size:n next:n :}
   kind SOURCE = kind DRIVER = or if
      bytes size DIGEST-OUT DIGEST @ execute
      KEPT path pathu root rootu DIGEST-OUT 32 RECORD
   then
   kind SOURCE = if path pathu root rootu bytes size ON-SOURCE @ execute then
   kind CHECKER = if ON-CHECKER @ execute then
   kind DONE = if path pathu root rootu ON-DONE @ execute then
   next ;

public

\ The staged token after this word anchors STARTUP's distances.
: TEXT-AT ( -- ) parse-name drop ANCHOR ! ;
parses: TEXT-AT 1

\ A file cold-load.f staged as text: its absolute path starts PATH-BACK bytes
\ before the anchor, and its bytes BACK bytes before.
: STARTUP ( n n n n -- )
   {: path-back:n pathu:n back:n size:n :}
   READ-KIND @ ANCHOR @ path-back - pathu SOURCE-ROOT:ENGINE$
      ANCHOR @ back - size RECORD ;

: CHECKER-LOADED ( -- ) CHECKER NULL$ NULL$ NULL$ RECORD ;

\ Load a file, recording each source the load reads and each load's return.
\ The hook stays installed until REPLAY, recording later reads as DRIVER.
: LOAD ( ptr u8 n -- )
   SOURCE-UNIT:CURRENT PRIOR !
   [: HOOK ;] SOURCE-UNIT:USE
   [: included ;] [: DRIVER READ-KIND ! ;] finally ;

\ Restore the loader LOAD replaced, hand each record to its action in the order
\ recorded and keep each read's path, root and DIGEST of its bytes.
: REPLAY ( [ ptr u8 n ptr u8 n ptr u8 n -- ] [ -- ] [ ptr u8 n ptr u8 n -- ] [ ptr u8 n ptr u8 -- ] -- )
   DIGEST !  ON-DONE !  ON-CHECKER !  ON-SOURCE !
   PRIOR @ SOURCE-UNIT:USE
   LOG @ CAP @ USED @ {: old:ptr cap:n used:n :}
   0 CAP !  0 USED !
   0 begin dup used < while old swap STEP repeat drop
   old cap RELEASE ;

\ Hand each kept row to Q as path, root and digest, then free them. A process
\ that never drains holds them until it exits.
: DRAIN ( [ ptr u8 n ptr u8 n ptr u8 -- ] -- )
   {: q :}
   0 begin dup USED @ < while
      LOG @ swap ROW {: kind:n path:ptr pathu:n root:ptr rootu:n digest:ptr size:n next:n :}
      path pathu root rootu digest q execute
      next
   repeat drop
   UNMAP ;

;package
