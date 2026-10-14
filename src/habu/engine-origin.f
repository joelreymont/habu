\ Source names accepted by the check hook while an image is being built.
\ Hook-time rows live in mmap scratch: a definition may arrive while a
\ DYNAMIC-BUFFER definer still owns HERE. Capture copies the offset-linked
\ rows into image DATA, then releases and clears the process-local map.
\ Newer rows win, including unlocated definitions that erase an older source.

require src/core/include.f
require src/core/sha256.f
require src/habu/engine-origin-early.f

package ENGINE-ORIGIN

0 constant O-NEXT
1 constant O-SYM
2 constant O-XT
3 constant O-START
4 constant O-END
5 constant O-PATH-U
6 constant O-NAME-U
7 constant O-CELLS
32 constant HASH-U

PTR-VARIABLE SCR-P
variable SCR-U
variable SCR-CAP
variable SCR-HEAD
PERSISTED-PTR-VARIABLE ROW-P
variable ROW-U
variable ROW-HEAD
variable ARMED
create HASH-CTX SHA256-CTX-BYTES allot
create HASH-OUT HASH-U allot
variable HASH-GEN
PTR-VARIABLE HASH-SRC
PTR-VARIABLE HASH-END
create ZERO-HASH 0 , 0 , 0 , 0 ,
NULL-PTR SCR-P !
NULL-PTR ROW-P !
-1 HASH-GEN !

SRCLOC:INB-CELL RESERVED-PTR-U8-CELL SOURCE-A
SRCLOC:PATH-CELL RESERVED-PTR-U8-CELL PATH-A
INE-CELL RESERVED-PTR-U8-CELL SOURCE-END
DEF-TKA-CELL RESERVED-PTR-U8-CELL DEF-NAME-A
PENDTKA-CELL RESERVED-PTR-U8-CELL OPEN-A

CAST: SYM-XT ( n -- [ ptr u8 n -- n ] )
CAST: REC-XT ( n -- [ n -- ptr n ] )

: RECORD-SYM ( ptr u8 n -- n )
   data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @
   ENGINE-ORIGIN-EARLY:RECORD-SYM-OFF + CELL-VIEW @
   SYM-XT execute ;

: CELL-AT ( ptr n n -- ptr n ) cells + ;
: ROW ( ptr u8 n -- ptr n ) 1 - + CELL-VIEW ;
: PATH$ ( ptr n -- ptr u8 n ) {: row:ptr :}
   row O-CELLS cells + BYTE-VIEW row O-PATH-U CELL-AT @ ;
: NAME$ ( ptr n -- ptr u8 n ) {: row:ptr :}
   row O-CELLS cells + row O-PATH-U CELL-AT @ + BYTE-VIEW
   row O-NAME-U CELL-AT @ ;
: HASH$ ( ptr n -- ptr u8 n ) {: row:ptr :}
   row O-CELLS cells + row O-PATH-U CELL-AT @ +
   row O-NAME-U CELL-AT @ + BYTE-VIEW HASH-U ;

: ROOT-REL ( ptr u8 n -- ptr u8 n bool )
   {: path:ptr pathu:n :}
   SOURCE-ROOT:ENGINE$ {: root:ptr rootu:n :}
   pathu rootu 1 + <= if NULL$ INCLUDE-FALSE exit then
   path rootu root rootu CORE-STR= 0= if NULL$ INCLUDE-FALSE exit then
   path rootu + c@ 47 <> if NULL$ INCLUDE-FALSE exit then
   path rootu 1 + + pathu rootu 1 + - INCLUDE-TRUE ;

\ The saved name token must still be inside this loaded file's input buffer.
: SOURCE-NAME ( -- ptr u8 )
   data-base PEND-CELL + @ 0= if DEF-NAME-A @ exit then
   OPEN-A @ ;

: LOCATION ( ptr u8 n -- ptr u8 n n n bool )
   {: name:ptr nameu:n :}
   PATH-A @ data-base SRCLOC:PATHLEN-CELL + @ ROOT-REL
   {: path:ptr pathu:n in-root:bool :}
   in-root 0= if NULL$ 0 0 INCLUDE-FALSE exit then
   SOURCE-NAME {: source-name:ptr :}
   source-name SOURCE-A @ - {: at:n :}
   SOURCE-END @ SOURCE-A @ - {: size:n :}
   at 0 < nameu 0 <= or at size > or if NULL$ 0 0 INCLUDE-FALSE exit then
   nameu size at - > if NULL$ 0 0 INCLUDE-FALSE exit then
   source-name nameu name nameu CORE-STR= 0= if NULL$ 0 0 INCLUDE-FALSE exit then
   path pathu at at nameu + INCLUDE-TRUE ;

: DEF-XT ( -- n )
   data-base PEND-CELL + @ dup 0= if drop data-base LASTC-CELL + @ then
   dup 0= if exit then
   XREF-ADDR>REC XREF-START ;

: ROUND-CELL ( n -- n ) CELL 1 - + CELL / CELL * ;

: SCR-ENSURE ( n -- ) {: need:n :}
   need SCR-CAP @ <= if exit then
   need SCR-CAP @ 2 * max 4096 max {: cap:n :}
   cap map-anon 0 <> if drop s" engine origin: mmap failed" 76 die then
   {: fresh:ptr :}
   SCR-U @ 0 > if
      SCR-P @ BYTE-VIEW fresh BYTE-VIEW SCR-U @ BYTE-COPY
      SCR-P @ SCR-CAP @ munmap 0 <> if
         s" engine origin: munmap failed" 76 die
      then
   then
   fresh SCR-P !  cap SCR-CAP ! ;

: APPEND ( n n ptr u8 n n n ptr u8 n ptr u8 -- )
   {: sym:n xt:n path:ptr pathu:n at:n end:n name:ptr nameu:n hash:ptr :}
   SCR-U @ ROUND-CELL {: off:n :}
   off O-CELLS cells + pathu + nameu + HASH-U + {: used:n :}
   used SCR-ENSURE
   SCR-P @ BYTE-VIEW off + CELL-VIEW {: row:ptr :}
   SCR-HEAD @ row O-NEXT CELL-AT !
   sym row O-SYM CELL-AT !
   xt row O-XT CELL-AT !
   at row O-START CELL-AT !  end row O-END CELL-AT !
   pathu row O-PATH-U CELL-AT !  nameu row O-NAME-U CELL-AT !
   path row O-CELLS cells + BYTE-VIEW pathu BYTE-COPY
   name row O-CELLS cells + pathu + BYTE-VIEW nameu BYTE-COPY
   hash row O-CELLS cells + pathu + nameu + BYTE-VIEW HASH-U BYTE-COPY
   off 1 + SCR-HEAD !  used SCR-U ! ;

: TAKE ( ptr u8 n -- )
   {: name:ptr nameu:n :}
   ARMED @ 0= if exit then
   nameu 0 <= if exit then
   name nameu RECORD-SYM {: sym:n :}
   sym 0= if exit then
   DEF-XT {: xt:n :}
   xt 0= if exit then
   name nameu LOCATION {: path:ptr pathu:n at:n end:n found:bool :}
   found 0= if sym xt NULL$ 0 0 name nameu ZERO-HASH APPEND exit then
   SOURCE-UNIT:GEN @ HASH-GEN @ <>
   SOURCE-A @ HASH-SRC @ <> or
   SOURCE-END @ HASH-END @ <> or if
      HASH-CTX SOURCE-A @ SOURCE-END @ SOURCE-A @ - HASH-OUT SHA256-IN
      SOURCE-UNIT:GEN @ HASH-GEN !
      SOURCE-A @ HASH-SRC !
      SOURCE-END @ HASH-END !
   then
   sym xt path pathu at end name nameu HASH-OUT APPEND ;

\ The early observer saw accepted definitions before this module's loader,
\ digest and XREF dependencies existed. Resolve its saved dictionary records
\ and hash the exact source copies now, in their original publication order.
: EARLY-ACCEPT ( ptr n -- ) {: row:ptr :}
   row ENGINE-ORIGIN-EARLY:E-REC cells + @
   XREF-ADDR>REC XREF-START {: xt:n :}
   row ENGINE-ORIGIN-EARLY:E-SYM cells + @ {: sym:n :}
   row ENGINE-ORIGIN-EARLY:E-START cells + @ {: at:n :}
   row ENGINE-ORIGIN-EARLY:E-END cells + @ {: end:n :}
   row ENGINE-ORIGIN-EARLY:E-PATH-U cells + @ {: pathu:n :}
   row ENGINE-ORIGIN-EARLY:E-NAME-U cells + @ {: nameu:n :}
   row ENGINE-ORIGIN-EARLY:E-SOURCE-U cells + @ {: sourceu:n :}
   row ENGINE-ORIGIN-EARLY:E-CELLS cells + BYTE-VIEW {: path:ptr :}
   path pathu + {: name:ptr :}
   name nameu + {: source:ptr :}
   path pathu ROOT-REL {: rel:ptr relu:n in-root:bool :}
   in-root sourceu 0 > and at 0 >= and end at > and end sourceu <= and if
      source at + end at - name nameu CORE-STR= if
         HASH-CTX source sourceu HASH-OUT SHA256-IN
         sym xt rel relu at end name nameu HASH-OUT APPEND
         exit
      then
   then
   sym xt NULL$ 0 0 name nameu ZERO-HASH APPEND ;

: DRAIN-EARLY ( -- )
   ENGINE-ORIGIN-EARLY:ROWS {: base:ptr used:n :}
   0 begin dup used < while
      ROUND-CELL {: off:n :}
      base off + CELL-VIEW {: row:ptr :}
      row EARLY-ACCEPT
      off ENGINE-ORIGIN-EARLY:E-CELLS cells +
      row ENGINE-ORIGIN-EARLY:E-PATH-U cells + @ +
      row ENGINE-ORIGIN-EARLY:E-NAME-U cells + @ +
      row ENGINE-ORIGIN-EARLY:E-SOURCE-U cells + @ +
   repeat drop
   ENGINE-ORIGIN-EARLY:RELEASE ;

: FIND-ROW ( ptr u8 n n -- ptr n bool )
   {: base:ptr head:n sym:n :}
   head begin dup 0= 0= while
      base over ROW {: row:ptr :}
      row O-SYM CELL-AT @ sym = if drop row INCLUDE-TRUE exit then
      row O-NEXT CELL-AT @ swap drop
   repeat drop NULL-PTR INCLUDE-FALSE ;

: FIND-SYM ( n -- ptr n bool ) {: sym:n :}
   sym 0= if NULL-PTR INCLUDE-FALSE exit then
   SCR-P @ BYTE-VIEW SCR-HEAD @ sym FIND-ROW
   dup if exit then drop drop
   ROW-P @ BYTE-VIEW ROW-HEAD @ sym FIND-ROW ;

: ROW-LIVE? ( ptr n -- bool ) {: row:ptr :}
   row O-SYM CELL-AT @ 0= if INCLUDE-FALSE exit then
   data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @
   ENGINE-ORIGIN-EARLY:ORIGIN-REC-OFF + CELL-VIEW @
   row O-SYM CELL-AT @ swap REC-XT execute
   dup XREF-FOUND? if
      XREF-START row O-XT CELL-AT @ =
   else drop INCLUDE-FALSE then ;

: ROW-TARGET ( ptr n -- ptr u8 n n n ptr u8 n bool )
   {: row:ptr :}
   row PATH$ row O-START CELL-AT @ row O-END CELL-AT @
   row NAME$
   row O-PATH-U CELL-AT @ 0 > if row ROW-LIVE? else INCLUDE-FALSE then ;

public

: ARM ( -- ) 1 ARMED ! ;
: DISARM ( -- ) 0 ARMED ! ;

\ Move every hook-time row in one copy before image lifecycle preparation.
\ The map pointer is cleared before capture, so no transient address leaks.
: CAPTURE-PREPARE ( -- )
   DISARM
   SCR-U @ 0 > if
      ROW-U @ ROUND-CELL {: old:n :}
      old SCR-U @ + {: used:n :}
      align
      here {: rows:ptr :}
      used allot
      ROW-U @ 0 > if
         ROW-P @ BYTE-VIEW rows BYTE-VIEW ROW-U @ BYTE-COPY
      then
      SCR-P @ BYTE-VIEW rows old + BYTE-VIEW SCR-U @ BYTE-COPY
      SCR-HEAD @ begin dup 0= 0= while
         rows old + BYTE-VIEW over ROW {: row:ptr :}
         row O-NEXT CELL-AT @ {: prev:n :}
         prev 0= if ROW-HEAD @ else old prev + then row O-NEXT CELL-AT !
         prev swap drop
      repeat drop
      old SCR-HEAD @ + ROW-HEAD !
      used ROW-U !  rows ROW-P !
      ROW-HEAD @ begin dup 0= 0= while
         rows BYTE-VIEW over ROW {: row:ptr :}
         row ROW-LIVE? if
            row O-XT CELL-AT @ row O-XT CELL-AT xt!
         else
            \ FORGET can retire a hook-time row before this capture. A stale
            \ row must neither declare reclaimed code nor hide an older origin.
            0 row O-SYM CELL-AT !  0 row O-XT CELL-AT !
            0 row O-PATH-U CELL-AT !
         then
         row O-NEXT CELL-AT @ swap drop
      repeat drop
   then
   SCR-CAP @ 0 > if
      SCR-P @ SCR-CAP @ munmap 0 <> if
         s" engine origin: munmap failed" 76 die
      then
   then
   NULL-PTR SCR-P !  0 SCR-U !  0 SCR-CAP !  0 SCR-HEAD !
   -1 HASH-GEN !  NULL-PTR HASH-SRC !  NULL-PTR HASH-END !
   \ The generation only invalidates this now-disarmed hash cache.
   0 SOURCE-UNIT:GEN ! ;

\ Relative path, exact name token and byte span of the latest build-time
\ definition for SYM. A zero path is a deliberate supersession tombstone.
: TARGET ( n -- ptr u8 n n n ptr u8 n bool )
   FIND-SYM if ROW-TARGET exit then
   drop NULL$ 0 0 NULL$ INCLUDE-FALSE ;

\ The exact accepted source buffer's digest, for a still-current definition.
\ A caller can hash disk or supplied bytes and compare before using a span.
: SOURCE-HASH ( n -- ptr u8 n bool )
   FIND-SYM if
      dup ROW-TARGET {: path:ptr pathu:n at:n end:n name:ptr nameu:n live:bool :}
      live if HASH$ INCLUDE-TRUE exit then
      drop NULL$ INCLUDE-FALSE exit
   then
   drop NULL$ INCLUDE-FALSE ;

: FIND-AT ( ptr u8 n ptr u8 n n -- n bool )
   {: base:ptr head:n path:ptr pathu:n byte:n :}
   head begin dup 0= 0= while
      base over ROW {: row:ptr :}
      row PATH$ path pathu CORE-STR=
      byte row O-START CELL-AT @ >=
      byte row O-END CELL-AT @ < and and if
         row O-SYM CELL-AT @ TARGET
         {: live:ptr liveu:n start:n end:n name:ptr nameu:n found:bool :}
         found if
            live liveu path pathu CORE-STR=
            byte start >= byte end < and and if
               drop row O-SYM CELL-AT @ INCLUDE-TRUE exit
            then
         then
      then
      row O-NEXT CELL-AT @ swap drop
   repeat drop 0 INCLUDE-FALSE ;

\ Reverse selection for a declaration in engine source. A superseded row
\ cannot identify the old symbol because TARGET checks its live definition.
: SYMBOL-AT ( ptr u8 n n -- n bool )
   {: full:ptr fullu:n byte:n :}
   full fullu ROOT-REL {: path:ptr pathu:n in-root:bool :}
   in-root 0= if 0 INCLUDE-FALSE exit then
   SCR-P @ BYTE-VIEW SCR-HEAD @ path pathu byte FIND-AT
   dup if exit then drop drop
   ROW-P @ BYTE-VIEW ROW-HEAD @ path pathu byte FIND-AT ;

: FIRST-TOKEN ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   0 begin dup u < if a over + c@ 32 > else 0 0= 0= then while 1 + repeat
   a swap ;

: CHECKED-HOOK ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u LOWER-CERT-HOOK:HOOK dup -1 = if a u FIRST-TOKEN TAKE then ;

TRUSTED: INSTALL ( -- ) ['] CHECKED-HOOK set-check ;

DRAIN-EARLY

;package
