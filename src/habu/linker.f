\ linker.f - the cross-build's data space: WITH maps a space beside hb's DATA,
\ points DP, the wordlists and the image base at it, switches the checker's
\ registered stores to the build's (CHECK:OPEN), routes every file load to the
\ build's runner (SOURCE-UNIT:USE), runs the body there with no package of
\ hb's open and closes it. The close undoes exactly the open steps that ran,
\ in reverse.

require src/lib/prelude.f
require src/lib/errors.f
require src/lib/string.f
require src/lib/memory.f
require src/habu/layout.f
require src/habu/boot-cells.f
require src/habu/xref.f
require src/habu/address-cells.f
require src/habu/interpret.f
require src/habu/definers.f
require src/habu/intern.f
require src/core/include.f
require src/compiler/native/abi.f
require src/compiler/native/shadow.f

package LINKER
private

\ The open's progress: each step names the last one that completed.
ENUM step none mapped shadowed moved checked required routed open ;ENUM
TYPED-VARIABLE PROGRESS step

TYPED-VARIABLE ENGINE? bool          \ the mode is "engine"
variable BASE                        \ the space's address, its distance from NULL-PTR
variable FIRST-REC                   \ ndict@ at the open: records from here are the space's
TYPED-VARIABLE BODY [ -- ]
variable COMPILED-N                  \ files the last build compiled
TYPED-VARIABLE PRIOR-RUNNER [ ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- ]

\ The host cells a build changes; RESTORE puts them back.
variable SAVED-DP
variable SAVED-FLOOR
variable SAVED-CEILING
variable SAVED-DPHW
variable SAVED-DATA-FLOOR
variable SAVED-GLOBAL-WL
variable SAVED-NAMESPACE-WL
variable SAVED-COMMA-RUN
variable SAVED-COMMA-END
variable SAVED-COMMA-BASE
variable SAVED-LASTC
variable SAVED-CURRENT
variable SAVED-SIG-POOL
variable SAVED-SIG-LEN
variable SAVED-FLAGS                 \ hb's newest record's flags (NEWEST-FLAGS)

$40 constant NORESERVE               \ macOS MAP_NORESERVE

: HOST ( n -- ptr n )
   {: off:n :}
   data-base BYTE-VIEW off + CELL-VIEW ;

: HOST@ ( n -- n ) HOST @ ;
: HOST! ( n n -- ) HOST ! ;
: DATA-N ( -- n ) data-base NULL-PTR - ;

: SPACE-AT ( n -- ptr n )
   {: off:n :}
   NULL-PTR BYTE-VIEW BASE @ + off + CELL-VIEW ;

: UNMAP ( -- )
   NULL-PTR BYTE-VIEW BASE @ + DATA-SIZE munmap
   0<> if E-MEM-UNMAP throw then ;

\ Above hb's DATA: below it CHECKER-OWNER-GUARD refuses with E-COMP-OWNER.
: HINT ( -- n ) DATA-N DATA-SIZE + ;

\ ---- the open ------------------------------------------------------------------
\ Installed while IMAGE-BASE-CELL is 0, so its xt! stores reach hb's own cells.
\ Installing again adds no address-cell row: EMIT-MARK keeps one row per cell.
: INSTALL-LOOP ( -- )
   OUTER:INSTALL-EVALUATE
   OUTER:INSTALL-CREATE ;

\ The address-cell header the boot lays (habu2.f), so EMIT-MARK records the
\ space's address cells in the space.
: LAY-ADDRESS-HEADER ( -- )
   SNAP-RELOC:XTCELL-N-CELL {: h:n :}
   0 h SPACE-AT !
   ADDRESS-CELLS:MAGIC h ADDRESS-CELLS:MAGIC-FIELD + SPACE-AT !
   ADDRESS-CELLS:BOOT-OFF h ADDRESS-CELLS:BASE-FIELD + SPACE-AT !
   ADDRESS-CELLS:BOOT-CAP h ADDRESS-CELLS:CAP-FIELD + SPACE-AT !
   0 h ADDRESS-CELLS:MODE-FIELD + SPACE-AT ! ;

: MAP ( -- )
   HINT DATA-SIZE MEM-PROT-RW MEM-MAP-PRIVATE-ANON NORESERVE or
   MEM-ANON-FD MEM-OFF-ZERO mmap {: at:n :}
   at 0 < if E-MEM-MAP throw then
   at BASE !
   at HINT < if UNMAP E-MEM-MAP throw then
   construct step mapped PROGRESS !
   LAY-ADDRESS-HEADER ;

: SHADOW ( -- )
   NABI:BINDING NSHADOW:OPEN-NATIVE
   construct step shadowed PROGRESS ! ;

\ The flags cell of hb's newest record, through the alias as habu2.f C-IMMEDIATE
\ writes it: an `immediate` before the space's first definition sets it.
: NEWEST-FLAGS ( -- ptr n )
   FIRST-REC @ 1- XREF-REC-ADDR XREF-FLAGS-SLOT cells +
   REGION-ALIAS:DELTA-CELL HOST@ + {: at:n :}
   NULL-PTR BYTE-VIEW at + CELL-VIEW ;

: SAVE ( -- )
   NEWEST-FLAGS @ SAVED-FLAGS !
   DP-CELL HOST@ SAVED-DP !
   BOOT:DP-FLOOR-CELL HOST@ SAVED-FLOOR !
   BOOT:DP-CEILING-CELL HOST@ SAVED-CEILING !
   DPHW-CELL HOST@ SAVED-DPHW !
   DATA-FLOOR-CELL HOST@ SAVED-DATA-FLOOR !
   BOOT:GLOBAL-WL-CELL HOST@ SAVED-GLOBAL-WL !
   BOOT:NAMESPACE-WL-CELL HOST@ SAVED-NAMESPACE-WL !
   COMMA-RUN-CELL HOST@ SAVED-COMMA-RUN !
   COMMA-END-CELL HOST@ SAVED-COMMA-END !
   COMMA-BASE-CELL HOST@ SAVED-COMMA-BASE !
   LASTC-CELL HOST@ SAVED-LASTC !
   AOT-CELLS:SIG-POOL-CELL HOST@ SAVED-SIG-POOL !
   AOT-CELLS:SIG-LEN-CELL HOST@ SAVED-SIG-LEN !
   get-current SAVED-CURRENT ! ;

\ DATA-FLOOR-CELL is an offset from hb's data-base (layout.f). The wordlists
\ come before any host cell moves: `wordlist` throws OVERLAY-OPEN in a replay.
: MOVE-IN ( -- )
   wordlist wordlist negate {: global:n ns:n :}
   ndict@ FIRST-REC !
   SAVE
   BASE @ DATA-START + {: start:n :}
   start DP-CELL HOST!
   start BOOT:DP-FLOOR-CELL HOST!
   BASE @ DATA-SIZE + PROF-CNT-BYTES - BOOT:DP-CEILING-CELL HOST!
   0 DPHW-CELL HOST!
   0 LASTC-CELL HOST!
   start DATA-N - DATA-FLOOR-CELL HOST!
   0 AOT-CELLS:SIG-POOL-CELL HOST!    \ a build has no signature pool
   0 AOT-CELLS:SIG-LEN-CELL HOST!
   global BOOT:GLOBAL-WL-CELL HOST!
   ns BOOT:NAMESPACE-WL-CELL HOST!
   global set-current
   construct step moved PROGRESS ! ;

: CHECK-IN ( -- )
   CHECK:OPEN
   construct step checked PROGRESS ! ;

: REQUIRE-FRESH ( -- )
   REQUIRE-SNAPSHOT
   construct step required PROGRESS ! ;

\ The build's file runner: each file a load reaches compiles, and is counted
\ once its compile returns.
: RUN-FILE ( ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- )
   {: path:ptr pathu:n root:ptr rootu:n source:ptr sourceu:n q :}
   0 sourceu q execute
   1 COMPILED-N +! ;

: ROUTE ( -- )
   SOURCE-UNIT:CURRENT PRIOR-RUNNER !
   [: RUN-FILE ;] SOURCE-UNIT:USE
   construct step routed PROGRESS ! ;

\ Last: from here header stores go to the space (habu2.f ROUTE-HEADER).
: IMAGE-IN ( -- )
   BASE @ BOOT:IMAGE-BASE-CELL HOST!
   construct step open PROGRESS ! ;

: OPEN ( -- )
   0 COMPILED-N !
   INSTALL-LOOP
   MAP SHADOW MOVE-IN CHECK-IN REQUIRE-FRESH ROUTE IMAGE-IN ;

\ ---- the close -----------------------------------------------------------------
: RESTORE ( -- )
   SAVED-CURRENT @ set-current
   SAVED-DP @ DP-CELL HOST!
   SAVED-FLOOR @ BOOT:DP-FLOOR-CELL HOST!
   SAVED-CEILING @ BOOT:DP-CEILING-CELL HOST!
   SAVED-DPHW @ DPHW-CELL HOST!
   SAVED-DATA-FLOOR @ DATA-FLOOR-CELL HOST!
   SAVED-GLOBAL-WL @ BOOT:GLOBAL-WL-CELL HOST!
   SAVED-NAMESPACE-WL @ BOOT:NAMESPACE-WL-CELL HOST!
   SAVED-COMMA-RUN @ COMMA-RUN-CELL HOST!
   SAVED-COMMA-END @ COMMA-END-CELL HOST!
   SAVED-COMMA-BASE @ COMMA-BASE-CELL HOST!
   SAVED-LASTC @ LASTC-CELL HOST!
   SAVED-SIG-POOL @ AOT-CELLS:SIG-POOL-CELL HOST!
   SAVED-SIG-LEN @ AOT-CELLS:SIG-LEN-CELL HOST!
   SAVED-FLAGS @ NEWEST-FLAGS ! ;

\ Records still below the count would name bodies in the unmapped space; their
\ code goes back as FORGET-DEFS-FROM gives it.
: FORGET-SPACE ( -- )
   FIRST-REC @ CODE-RECLAIM:FLOOR-FROM {: floor:n :}
   FIRST-REC @ ndict!
   floor CODE-RECLAIM:TRUNCATE ;

\ Undo the last step that ran; false once none is left.
: UNDO ( -- bool )
   PROGRESS @
   MATCH step
      none OF false ENDOF
      mapped OF
         UNMAP construct step none PROGRESS ! true
      ENDOF
      shadowed OF
         NSHADOW:CLOSE construct step mapped PROGRESS ! true
      ENDOF
      moved OF
         RESTORE FORGET-SPACE construct step shadowed PROGRESS ! true
      ENDOF
      checked OF
         CHECK:CLOSE construct step moved PROGRESS ! true
      ENDOF
      required OF
         REQUIRE-RESTORE construct step checked PROGRESS ! true
      ENDOF
      routed OF
         PRIOR-RUNNER @ SOURCE-UNIT:USE
         construct step required PROGRESS ! true
      ENDOF
      open OF
         0 BOOT:IMAGE-BASE-CELL HOST!  INTERN:CLEANUP
         construct step routed PROGRESS ! true
      ENDOF
   ;MATCH ;

: CLOSE ( -- )
   begin UNDO 0= until ;

: RUN ( -- ) OPEN BODY @ execute ;

: IDLE? ( -- bool )
   PROGRESS @
   MATCH step
      none OF true ENDOF
      mapped OF false ENDOF
      shadowed OF false ENDOF
      moved OF false ENDOF
      checked OF false ENDOF
      required OF false ENDOF
      routed OF false ENDOF
      open OF false ENDOF
   ;MATCH ;

public

\ Run the body in a build's space with no package of hb's open, close on every
\ exit, reopen the caller's package, rethrow; E-COMP-STATE while a build is open.
: WITH ( ptr u8 n [ -- ] -- )
   {: mode:ptr modeu:n body :}
   IDLE? 0= if E-COMP-STATE throw then
   mode modeu s" engine" STR= ENGINE? !
   body BODY !
   [: ['] RUN ['] CLOSE finally ;] OUTER:WITHOUT-PACKAGE ;

\ The files the last build compiled, from its open until the next open.
: COMPILED ( -- n ) COMPILED-N @ ;

;package
