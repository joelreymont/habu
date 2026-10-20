\ Source names accepted by the check hook while an image is being built.
\ Hook-time rows live in mmap scratch: a definition may arrive while a
\ DYNAMIC-BUFFER definer still owns HERE. Capture copies the offset-linked
\ rows into image DATA, then releases and clears the process-local map.
\ Newer rows win, including unlocated definitions that erase an older source.

require src/core/include.f
require src/habu/native-body-abi.f
require src/habu/native-body.f
require src/habu/engine-origin-early.f
require src/habu/engine-origin-checked.f

package ENGINE-ORIGIN

0 constant O-NEXT
1 constant O-SYM
2 constant O-XT
3 constant O-START
4 constant O-END
5 constant O-PATH-U
6 constant O-NAME-U
7 constant O-SERIAL
8 constant O-CELLS

PTR-VARIABLE SCR-P
variable SCR-U
variable SCR-CAP
variable SCR-HEAD
PERSISTED-PTR-VARIABLE ROW-P
variable ROW-U
variable ROW-HEAD
PTR-VARIABLE INDEX-P
variable INDEX-CAP
variable ARMED
variable FILE-ACCEPT-XT
PTR-VARIABLE FILE-VISIT-P
variable FILE-VISIT-U
variable FILE-VISIT-N
NULL-PTR SCR-P !
NULL-PTR ROW-P !
NULL-PTR INDEX-P !
NULL-PTR FILE-VISIT-P !

SRCLOC:OWNED-CELL RESERVED-PTR-U8-CELL SOURCE-A
SRCLOC:PATH-CELL RESERVED-PTR-U8-CELL PATH-A
DEF-TKA-CELL RESERVED-PTR-U8-CELL DEF-NAME-A
PENDTKA-CELL RESERVED-PTR-U8-CELL OPEN-A

CAST: SYM-XT ( n -- [ ptr u8 n -- n ] )
CAST: REC-XT ( n -- [ n -- ptr n ] )
CAST: HOOK-N ( [ ptr u8 n -- n ] -- n )
CAST: TAKE-N ( [ ptr u8 n -- ] -- n )
CAST: MARK-N ( [ -- n n bool ] -- n )
CAST: ROWS-N ( [ n [ n n n n ptr u8 n ptr u8 n n n -- ] -- ] -- n )
CAST: ACCEPT-N ( [ n n -- ] -- n )
CAST: N-ACCEPT ( n -- [ n n -- ] )

: NOTIFY-ACCEPT ( n n -- )
   FILE-ACCEPT-XT @ dup 0= if drop 2drop exit then
   N-ACCEPT execute ;

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

: LOCATION ( ptr u8 n ptr u8 -- ptr u8 n n n bool )
   {: name:ptr nameu:n source-name:ptr :}
   PATH-A @ data-base SRCLOC:PATHLEN-CELL + @ ROOT-REL
   {: path:ptr pathu:n in-root:bool :}
   in-root 0= if NULL$ 0 0 INCLUDE-FALSE exit then
   source-name SOURCE-A @ - {: at:n :}
   data-base SRCLOC:OWNEDLEN-CELL + @ {: size:n :}
   at 0 < nameu 0 <= or at size > or if NULL$ 0 0 INCLUDE-FALSE exit then
   nameu size at - > if NULL$ 0 0 INCLUDE-FALSE exit then
   source-name nameu name nameu CORE-STR= 0= if NULL$ 0 0 INCLUDE-FALSE exit then
   path pathu at at nameu + INCLUDE-TRUE ;

: DEF-ADDR ( -- n )
   data-base PEND-CELL + @ dup 0= if drop data-base LASTC-CELL + @ then ;

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

\ Checker symbol IDs are dense. Keep the latest journal offset per symbol:
\ positive for scratch, negative for captured rows, zero for absent. Offsets
\ survive scratch growth; capture releases the table before moving rows.
: INDEX-SLOT ( n -- ptr n ) cells INDEX-P @ BYTE-VIEW + CELL-VIEW ;

: INDEX-ENSURE ( n -- )
   {: need:n :}
   need INDEX-CAP @ <= if exit then
   need INDEX-CAP @ 2 * max 512 max {: cap:n :}
   cap cells map-anon 0 <> if
      drop s" engine origin: lookup mmap failed" 76 die
   then {: fresh:ptr :}
   INDEX-CAP @ 0 > if
      INDEX-P @ BYTE-VIEW fresh BYTE-VIEW INDEX-CAP @ cells BYTE-COPY
      INDEX-P @ INDEX-CAP @ cells munmap 0 <> if
         s" engine origin: lookup munmap failed" 76 die
      then
   then
   fresh INDEX-P ! cap INDEX-CAP ! ;

: INDEX-ROWS ( ptr u8 n n -- )
   {: base:ptr head:n sign:n :}
   head begin dup 0= 0= while
      {: off:n :}
      base off ROW {: row:ptr :}
      row O-SYM CELL-AT @ {: sym:n :}
      sym 0 > if
         sym 1+ INDEX-ENSURE
         sym INDEX-SLOT @ 0= if off sign * sym INDEX-SLOT ! then
      then
      row O-NEXT CELL-AT @
   repeat drop ;

: INDEX-BUILD ( -- )
   1 INDEX-ENSURE
   SCR-P @ BYTE-VIEW SCR-HEAD @ 1 INDEX-ROWS
   ROW-P @ BYTE-VIEW ROW-HEAD @ -1 INDEX-ROWS ;

: INDEX-RELEASE ( -- )
   INDEX-CAP @ 0 > if
      INDEX-P @ INDEX-CAP @ cells munmap 0 <> if
         s" engine origin: lookup munmap failed" 76 die
      then
   then
   NULL-PTR INDEX-P ! 0 INDEX-CAP ! ;

: APPEND ( n n n ptr u8 n n n ptr u8 n -- )
   {: serial:n sym:n xt:n path:ptr pathu:n at:n end:n name:ptr nameu:n :}
   SCR-U @ ROUND-CELL {: off:n :}
   off O-CELLS cells + pathu + nameu + {: used:n :}
   used SCR-ENSURE
   SCR-P @ BYTE-VIEW off + CELL-VIEW {: row:ptr :}
   SCR-HEAD @ row O-NEXT CELL-AT !
   sym row O-SYM CELL-AT !
   xt row O-XT CELL-AT !
   serial row O-SERIAL CELL-AT !
   at row O-START CELL-AT !  end row O-END CELL-AT !
   pathu row O-PATH-U CELL-AT !  nameu row O-NAME-U CELL-AT !
   path row O-CELLS cells + BYTE-VIEW pathu BYTE-COPY
   name row O-CELLS cells + pathu + BYTE-VIEW nameu BYTE-COPY
   off 1 + SCR-HEAD !  used SCR-U !
   INDEX-CAP @ 0 > sym 0 > and if
      sym 1+ INDEX-ENSURE
      off 1+ sym INDEX-SLOT !
   then ;

: TAKE-REC-AT ( ptr u8 n n ptr u8 -- )
   {: name:ptr nameu:n addr:n token:ptr :}
   ARMED @ 0= if exit then
   nameu 0 <= if exit then
   name nameu RECORD-SYM {: sym:n :}
   sym 0= if exit then
   addr 0= if exit then
   addr XREF-ADDR>REC XREF-START {: xt:n :}
   xt 0= if exit then
   addr XREF-ADDR>INDEX {: ord:n :}
   name nameu token LOCATION {: path:ptr pathu:n at:n end:n found:bool :}
   found 0= if
      ENGINE-ORIGIN-EARLY:FILE-NEXT-SERIAL {: serial:n :}
      serial sym xt NULL$ 0 0 name nameu APPEND
      serial ord NOTIFY-ACCEPT exit
   then
   ENGINE-ORIGIN-EARLY:FILE-NEXT-SERIAL {: serial:n :}
   serial sym xt path pathu at end name nameu APPEND
   serial ord NOTIFY-ACCEPT ;

: TAKE-AT ( ptr u8 n ptr u8 -- )
   {: name:ptr nameu:n token:ptr :}
   name nameu DEF-ADDR token TAKE-REC-AT ;

: TAKE ( ptr u8 n -- ) SOURCE-NAME TAKE-AT ;

: EXPORT-ACCEPT ( ptr u8 n n -- )
   {: operand:ptr operand-u:n ord:n :}
   ord XREF-REC XREF-NAME-LEN {: name-u:n :}
   operand operand-u name-u - + {: name:ptr :}
   name name-u ord XREF-REC-ADDR name TAKE-REC-AT ;

: OBSERVE-CAST ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n sig:ptr sigu:n :}
   name nameu sig sigu ENGINE-ORIGIN-EARLY:CERTIFY-CAST
   name nameu DEF-NAME-A @ TAKE-AT ;

\ The early observer saw accepted definitions before this module's loader,
\ and XREF dependencies existed. Resolve its saved dictionary records in
\ their original publication order.
: EARLY-ACCEPT ( ptr n -- ) {: row:ptr :}
   row ENGINE-ORIGIN-EARLY:E-REC cells + @
   XREF-REC XREF-START {: xt:n :}
   row ENGINE-ORIGIN-EARLY:E-SERIAL cells + @ {: serial:n :}
   row ENGINE-ORIGIN-EARLY:E-START cells + @ {: at:n :}
   row ENGINE-ORIGIN-EARLY:E-END cells + @ {: end:n :}
   row ENGINE-ORIGIN-EARLY:E-PATH-U cells + @ {: pathu:n :}
   row ENGINE-ORIGIN-EARLY:E-NAME-U cells + @ {: nameu:n :}
   row ENGINE-ORIGIN-EARLY:E-CELLS cells + BYTE-VIEW {: path:ptr :}
   path pathu + {: name:ptr :}
   row ENGINE-ORIGIN-EARLY:E-SYM cells + @ dup 0= if
      drop name nameu RECORD-SYM
   then {: sym:n :}
   path pathu ROOT-REL {: rel:ptr relu:n in-root:bool :}
   in-root at 0 >= and end at > and end at - nameu = and if
      serial sym xt rel relu at end name nameu APPEND
      exit
   then
   serial sym xt NULL$ 0 0 name nameu APPEND ;

: DRAIN-EARLY ( -- )
   ENGINE-ORIGIN-EARLY:ROWS {: base:ptr used:n :}
   0 begin dup used < while
      ROUND-CELL {: off:n :}
      base off + CELL-VIEW {: row:ptr :}
      row EARLY-ACCEPT
      off ENGINE-ORIGIN-EARLY:E-CELLS cells +
      row ENGINE-ORIGIN-EARLY:E-PATH-U cells + @ +
      row ENGINE-ORIGIN-EARLY:E-NAME-U cells + @ +
   repeat drop
   ENGINE-ORIGIN-EARLY:RELEASE ;

: FIND-SYM ( n -- ptr n bool )
   {: sym:n :}
   sym 0 <= if NULL-PTR INCLUDE-FALSE exit then
   INDEX-CAP @ 0= if INDEX-BUILD then
   sym INDEX-CAP @ >= if NULL-PTR INCLUDE-FALSE exit then
   sym INDEX-SLOT @ {: off:n :}
   off 0= if NULL-PTR INCLUDE-FALSE exit then
   off 0 > if SCR-P @ BYTE-VIEW off
   else ROW-P @ BYTE-VIEW off negate then
   ROW INCLUDE-TRUE ;

: ROW-RECORD ( ptr n -- ptr n ) {: row:ptr :}
   row O-SYM CELL-AT @ 0= if XREF-NULL exit then
   data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @
   ENGINE-ORIGIN-EARLY:ORIGIN-REC-OFF + CELL-VIEW @
   row O-SYM CELL-AT @ swap REC-XT execute
      {: rec:ptr :}
   rec XREF-FOUND? if
      rec XREF-START row O-XT CELL-AT @ = if rec exit then
   then
   XREF-NULL ;

: ROW-LIVE? ( ptr n -- bool ) ROW-RECORD XREF-FOUND? ;

: ROW-TARGET ( ptr n -- ptr u8 n n n ptr u8 n bool )
   {: row:ptr :}
   row PATH$ row O-START CELL-AT @ row O-END CELL-AT @
   row NAME$
   row O-PATH-U CELL-AT @ 0 > if row ROW-LIVE? else INCLUDE-FALSE then ;

public

: FILE-ACCEPT-USE ( [ n n -- ] -- ) ACCEPT-N FILE-ACCEPT-XT ! ;
: FILE-ACCEPT-CLEAR ( -- ) 0 FILE-ACCEPT-XT ! ;

: ARM ( -- ) 1 ARMED ! ENGINE-ORIGIN-EARLY:FILE-ARM ;
: DISARM ( -- ) 0 ARMED ! ENGINE-ORIGIN-EARLY:FILE-DISARM ;
: FILE-RESTORE ( bool -- )
   if 1 else 0 then ARMED ! ;
: FILE-DRAIN ( -- ) DRAIN-EARLY ;

: FILE-STATE-SPANS ( [ ptr u8 n -- ] -- ) {: visit :}
   SCR-P BYTE-VIEW CELL visit execute
   SCR-U BYTE-VIEW CELL visit execute
   SCR-CAP BYTE-VIEW CELL visit execute
   SCR-HEAD BYTE-VIEW CELL visit execute
   ROW-P BYTE-VIEW CELL visit execute
   ROW-U BYTE-VIEW CELL visit execute
   ROW-HEAD BYTE-VIEW CELL visit execute
   INDEX-P BYTE-VIEW CELL visit execute
   INDEX-CAP BYTE-VIEW CELL visit execute
   ARMED BYTE-VIEW CELL visit execute
   FILE-VISIT-P BYTE-VIEW CELL visit execute
   FILE-VISIT-U BYTE-VIEW CELL visit execute
   FILE-VISIT-N BYTE-VIEW CELL visit execute ;

private

: FILE-ROW ( ptr n [ n n n n ptr u8 n ptr u8 n n n -- ] -- )
   {: row:ptr visit :}
   row O-SERIAL CELL-AT @ 1 row O-XT CELL-AT @ row O-SYM CELL-AT @
   row PATH$ row NAME$
   row O-START CELL-AT @ row O-END CELL-AT @
   visit execute ;

\ ROW-P holds batches joined by links. Their DATA extents can have gaps, so
\ walking apparent row sizes as a byte stream would read padding as a header.
: FILE-COUNT ( ptr u8 n n -- n )
   {: base:ptr head:n mark:n :}
   0 head begin dup 0= 0= while
      base over ROW {: row:ptr :}
      row O-SERIAL CELL-AT @ mark <= if drop exit then
      row O-NEXT CELL-AT @ swap drop
      swap 1+ swap
   repeat drop ;

: FILE-VISIT-ENSURE ( n -- )
   {: count:n :}
   count 0= if exit then
   count cells dup FILE-VISIT-U !
   map-anon 0 <> if drop s" engine origin: file index mmap failed" 76 die then
   FILE-VISIT-P ! ;

: FILE-VISIT-RELEASE ( -- )
   FILE-VISIT-U @ 0 > if
      FILE-VISIT-P @ FILE-VISIT-U @ munmap 0 <> if
         s" engine origin: file index munmap failed" 76 die
      then
   then
   NULL-PTR FILE-VISIT-P !  0 FILE-VISIT-U !  0 FILE-VISIT-N ! ;

: FILE-COLLECT ( ptr u8 n n -- )
   {: base:ptr head:n mark:n :}
   head begin dup 0= 0= while
      base over ROW {: row:ptr :}
      row O-SERIAL CELL-AT @ mark <= if drop exit then
      dup FILE-VISIT-P @ BYTE-VIEW FILE-VISIT-N @ cells + CELL-VIEW !
      1 FILE-VISIT-N +!
      row O-NEXT CELL-AT @ swap drop
   repeat drop ;

: FILE-EMIT ( ptr u8 [ n n n n ptr u8 n ptr u8 n n n -- ] -- )
   {: base:ptr visit :}
   FILE-VISIT-N @ begin dup 0 > while
      1- dup FILE-VISIT-P @ BYTE-VIEW swap cells + CELL-VIEW @
      base swap ROW visit FILE-ROW
   repeat drop ;

: FILE-ROWS-BODY ( n [ n n n n ptr u8 n ptr u8 n n n -- ] -- )
   {: mark:n visit :}
   ROW-P @ BYTE-VIEW ROW-HEAD @ mark FILE-COUNT
   SCR-P @ BYTE-VIEW SCR-HEAD @ mark FILE-COUNT max FILE-VISIT-ENSURE
   0 FILE-VISIT-N !
   ROW-P @ BYTE-VIEW ROW-HEAD @ mark FILE-COLLECT
   ROW-P @ BYTE-VIEW visit FILE-EMIT
   0 FILE-VISIT-N !
   SCR-P @ BYTE-VIEW SCR-HEAD @ mark FILE-COLLECT
   SCR-P @ BYTE-VIEW visit FILE-EMIT ;

public

: FILE-ROWS ( n [ n n n n ptr u8 n ptr u8 n n n -- ] -- )
   FILE-VISIT-P @ NULL-PTR <> if
      s" engine origin: nested file row visit" 76 die
   then
   ['] FILE-ROWS-BODY ['] FILE-VISIT-RELEASE finally ;

\ The full journal was compiled against this exact early serial boundary.
: FILE-BOUNDARY ( -- n n )
   ['] ENGINE-ORIGIN-EARLY:FILE-MARK MARK-N ['] FILE-ROWS ROWS-N ;

\ Import order is acceptance order in this engine; producer serials do not
\ travel with the FILE section. The decoder supplies resolved local identities.
: FILE-IMPORT ( n n ptr u8 n ptr u8 n n n -- )
   {: sym:n ord:n path:ptr pathu:n name:ptr nameu:n at:n end:n :}
   ENGINE-ORIGIN-EARLY:FILE-NEXT-SERIAL {: serial:n :}
   serial sym ord XREF-REC XREF-START path pathu at end name nameu APPEND
   serial ord NOTIFY-ACCEPT ;

\ Move every hook-time row in one copy before process resource cleanup.
\ The map pointer is cleared before capture, so no transient address leaks.
: CAPTURE-PREPARE ( -- )
   FILE-ACCEPT-CLEAR
   DISARM
   FILE-VISIT-RELEASE
   INDEX-RELEASE
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
   NULL-PTR SCR-P !  0 SCR-U !  0 SCR-CAP !  0 SCR-HEAD ! ;

\ Capture cleanup can query the captured journal and rebuild its index.
\ Drop that process-local mapping after the callbacks have finished.
: CAPTURE-END ( -- ) INDEX-RELEASE ;

\ Relative path, exact name token and byte span of the latest build-time
\ definition for SYM. A zero path is a deliberate supersession tombstone.
: TARGET ( n -- ptr u8 n n n ptr u8 n bool )
   FIND-SYM if ROW-TARGET exit then
   drop NULL$ 0 0 NULL$ INCLUDE-FALSE ;

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

private
\ Scan both journals; only the latest row for a live binding is visible.
: EACH-ROWS ( ptr u8 n ptr u8 n [ n -- ] -- )
   {: base:ptr head:n path:ptr pathu:n action :}
   head begin dup 0= 0= while
      base over ROW {: row:ptr :}
      row PATH$ path pathu CORE-STR= if
         row O-SYM CELL-AT @ FIND-SYM {: latest:ptr found:bool :}
         found if latest row = row ROW-LIVE? and if
            row O-SYM CELL-AT @ action execute
         then then
      then
      row O-NEXT CELL-AT @ swap drop
   repeat drop ;

\ Native build reads this journal once before stripping the checker. Select
\ exact live records: EXPORT can share an xt with a different source record.
: MAP-ROWS ( ptr u8 n [ ptr n ptr u8 n -- ] -- )
   {: base:ptr head:n visit :}
   head begin dup 0= 0= while
      base over ROW {: row:ptr :}
      row ROW-RECORD {: rec:ptr :}
      rec XREF-FOUND? if
         rec row PATH$ visit execute
      then
      row O-NEXT CELL-AT @ swap drop
   repeat drop ;

\ Newest first, including empty paths that supersede an older source origin.
\ The caller copies each record's first borrowed path before capture changes it.
: MAP-SOURCES ( [ ptr n ptr u8 n -- ] -- ) {: visit :}
   SCR-P @ BYTE-VIEW SCR-HEAD @ visit MAP-ROWS
   ROW-P @ BYTE-VIEW ROW-HEAD @ visit MAP-ROWS ;

public

\ Completion receives the captured body; TAKE identifies the name token.
: FIRST-TOKEN ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   0 begin dup u < if a over + c@ 32 > else 0 0= 0= then while 1 + repeat
   a swap ;

\ A definition restored at the native dispatch boundary still belongs to the
\ current interpreter source. Record that acceptance with the same source
\ reader and path checks as a freshly compiled definition.
: ACCEPT-PENDING ( ptr u8 n -- ) FIRST-TOKEN TAKE ;

\ Reverse selection for a declaration in engine source. A superseded row
\ cannot identify the old symbol because TARGET checks its live definition.
: SYMBOL-AT ( ptr u8 n n -- n bool )
   {: full:ptr fullu:n byte:n :}
   full fullu ROOT-REL {: path:ptr pathu:n in-root:bool :}
   in-root 0= if 0 INCLUDE-FALSE exit then
   SCR-P @ BYTE-VIEW SCR-HEAD @ path pathu byte FIND-AT
   dup if exit then drop drop
   ROW-P @ BYTE-VIEW ROW-HEAD @ path pathu byte FIND-AT ;

\ Visit live latest origins in one engine-relative file.
: EACH-IN ( ptr u8 n [ n -- ] -- )
   {: path:ptr pathu:n action :}
   pathu 0= if exit then
   SCR-P @ BYTE-VIEW SCR-HEAD @ path pathu action EACH-ROWS
   ROW-P @ BYTE-VIEW ROW-HEAD @ path pathu action EACH-ROWS ;

: CHECKED-HOOK ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u LOWER-CERT-HOOK:HOOK dup -1 = if
      a u FIRST-TOKEN {: name:ptr nameu:n :}
      check@ ['] ACCEPT-PENDING TAKE-N
         ENGINE-ORIGIN-EARLY:NATIVE-ARM 0= if name nameu TAKE then
   then ;

: FILE-STATE ( -- bool bool ) ARMED @ 0= 0= check@ ['] CHECKED-HOOK HOOK-N = ;
TRUSTED: INSTALL ( -- )
   ['] CHECKED-HOOK set-check
   ['] OBSERVE-CAST ENGINE-ORIGIN-EARLY:CAST-ENTRY
   CHECKER-REG:DECLARATIONS CHECKER-OWNER-ABI:CAST-OFF + CELL-VIEW xt!
   ['] EXPORT-ACCEPT ENGINE-ORIGIN-EARLY:EXPORT-ENTRY
   ENGINE-ORIGIN-EARLY:BODY-CAPABILITY dup NULL-PTR = if drop exit then
   {: record:ptr :}
   ['] CHECKED-HOOK HOOK-N
      record NATIVE-BODY-ABI:ORIGIN-HOOK cells + CELL-VIEW !
   ['] ACCEPT-PENDING TAKE-N
      record NATIVE-BODY-ABI:ORIGIN-TAKE cells + CELL-VIEW ! ;

\ Resolve prefix globals after closing this package: their checker symbols
\ were handed over after capture, and RECORD-SYM reads the current scope.
' DRAIN-EARLY
;package
execute
ENGINE-ORIGIN-EARLY:FILE-FULL
