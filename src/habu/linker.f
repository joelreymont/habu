\ linker.f - the cross-build's data space: WITH maps a space beside hb's DATA,
\ points DP, the wordlists and the image base at it, switches the checker's
\ registered stores to the build's (CHECK:OPEN), routes every file load to the
\ build's runner (SOURCE-UNIT:USE), runs the body there with no package of
\ hb's open and closes it. The close undoes exactly the open steps that ran,
\ in reverse. The runner writes each compiled file's object under the out root.

require src/lib/prelude.f
require src/lib/error.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/byte-buffer.f
require src/lib/engine-id.f
require src/lib/build-out.f
require src/core/sha256.f
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
ENUM step none mapped shadowed moved checked required objects routed open ;ENUM
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

\ ---- the object ----------------------------------------------------------------
\ LOADERS stacks the key of each file whose compile is running; LOADS holds a
\ row per load: the loader's key, zeros for the build's body, then the SHA-256
\ of the path under its root.
32 constant KEY-BYTES
KEY-BYTES 2 * constant ROW-BYTES
create LOADERS BUF:HDR-BYTES allot
create LOADS BUF:HDR-BYTES allot
create OBJECT BUF:HDR-BYTES allot    \ a key's fields, then an object's bytes
SHA256-CTX-BYTES BUFFER: SHA-CTX
KEY-BYTES BUFFER: FILE-KEY
ROW-BYTES BUFFER: ROW
KEY-BYTES BUFFER: INPUT-SUM
KEY-BYTES 2 * BUFFER: KEY-HEX

\ The magic, the key, and an offset from the file's start and a length for each
\ of INPUT FILES REQUESTS NAMES PIECES, 0 0 while empty.
8 KEY-BYTES + 5 2 * 8 * + constant HEADER-BYTES
8 KEY-BYTES + constant INPUT-BYTES   \ the source's size, then its SHA-256

: DISPOSE-OBJECTS ( -- )
   OBJECT BUF:DISPOSE  LOADS BUF:DISPOSE  LOADERS BUF:DISPOSE ;

: INIT-BUFFERS ( -- )
   LOADERS 64 BUF:N>BLEN BUF:INIT
   LOADS 256 BUF:N>BLEN BUF:INIT
   OBJECT 256 BUF:N>BLEN BUF:INIT ;

\ All or none: disposing a header never set up, or disposed, is a no-op.
: INIT-OBJECTS ( -- )
   ['] INIT-BUFFERS catch {: rc:n :}
   rc 0<> if DISPOSE-OBJECTS rc throw then ;

: BYTES$ ( ptr a -- ptr u8 n ) BUF:SPAN$ BUF:BLEN>N ;

: SPAN+ ( ptr u8 n ptr a -- ) {: a:ptr u:n buf:ptr :}
   a u BUF:N>BLEN buf BUF:APPEND-SPAN ;

\ n as eight bytes, low byte first.
: U64+ ( n ptr a -- ) {: v:n buf:ptr :}
   8 0 ?do v i 8 * rshift $FF and buf BUF:APPEND-BYTE loop ;

\ A key field: its length as a U64+, then its bytes.
: FIELD+ ( ptr u8 n ptr a -- ) {: a:ptr u:n buf:ptr :}
   u buf U64+  a u buf SPAN+ ;

\ The path with the root and the slash after it removed when it starts there.
: UNDER-ROOT ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: path:ptr pathu:n root:ptr rootu:n :}
   pathu rootu > if
      path pathu root rootu STARTS-WITH?  path rootu + c@ FS:SLASH =  and if
         path rootu + 1 +  pathu rootu - 1 -  exit
      then
   then
   path pathu ;

\ The key of the file whose compile is running, empty for the build's body.
: LOADER$ ( -- ptr u8 n )
   LOADERS BYTES$ {: a:ptr u:n :}
   u 0= if a 0 exit then
   a u KEY-BYTES - +  KEY-BYTES ;

\ The loads already recorded with ROW's loader and path.
: SEEN ( -- n )
   LOADS BYTES$ {: rows:ptr rowsu:n :}
   0  rowsu ROW-BYTES / 0 ?do
      rows i ROW-BYTES * +  ROW-BYTES  ROW ROW-BYTES  STR= if 1 + then
   loop ;

\ FILE-KEY for a load of the path under its root, recorded in LOADS.
: KEY! ( ptr u8 n -- ) {: rel:ptr relu:n :}
   KEY-BYTES 0 ?do 0 ROW i + c! loop
   LOADER$ {: ld:ptr ldu:n :}
   ld ROW ldu BYTE-COPY
   SHA-CTX rel relu ROW KEY-BYTES + SHA256-IN
   SEEN {: n:n :}
   OBJECT BUF:CLEAR
   ENGINE-ID:KEY$ OBJECT FIELD+
   ENGINE? @ if s" engine" else s" app" then OBJECT FIELD+
   s" arm64" OBJECT FIELD+
   rel relu OBJECT FIELD+
   ld ldu OBJECT FIELD+
   8 OBJECT U64+  n OBJECT U64+
   SHA-CTX OBJECT BYTES$ FILE-KEY SHA256-IN
   ROW ROW-BYTES LOADS SPAN+ ;

: PUSH-KEY ( -- ) FILE-KEY KEY-BYTES LOADERS SPAN+ ;

\ The stack's top back into FILE-KEY, and off the stack.
: POP-KEY ( -- )
   LOADER$ {: top:ptr topu:n :}
   top FILE-KEY topu BYTE-COPY
   LOADERS BYTES$ KEY-BYTES - BUF:N>BLEN LOADERS BUF:REPLACE ;

\ FILE-KEY's object; INPUT sums the bytes the runner gets (`#!` read as `\ `).
: WRITE-OBJECT ( ptr u8 n -- ) {: src:ptr srcu:n :}
   SHA-CTX src srcu INPUT-SUM SHA256-IN
   OBJECT BUF:CLEAR
   s" HABUFOBJ" OBJECT SPAN+
   FILE-KEY KEY-BYTES OBJECT SPAN+
   HEADER-BYTES OBJECT U64+  INPUT-BYTES OBJECT U64+
   8 0 ?do 0 OBJECT U64+ loop          \ FILES REQUESTS NAMES PIECES, empty
   srcu OBJECT U64+
   INPUT-SUM KEY-BYTES OBJECT SPAN+
   FILE-KEY KEY-HEX SHA256>HEX
   KEY-HEX KEY-BYTES 2 * BUILD-OUT:OBJ$  OBJECT BYTES$  FS:ATOMIC-WRITE-FILE ;

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

\ `[: execute ;]` here is E-EXEC-OPAQUE-XT; a quotation parameter is typed.
: RUN-UNIT ( n n [ n n -- ] -- ) execute ;

\ The build's file runner: each file a load reaches compiles, and is counted
\ and its object written once its compile returns; a throw writes none.
: RUN-FILE ( ptr u8 n ptr u8 n ptr u8 n [ n n -- ] -- )
   {: path:ptr pathu:n root:ptr rootu:n source:ptr sourceu:n q :}
   path pathu root rootu UNDER-ROOT KEY!
   PUSH-KEY
   0 sourceu q ['] RUN-UNIT ['] POP-KEY finally
   1 COMPILED-N +!
   source sourceu WRITE-OBJECT ;

: OBJECTS-IN ( -- )
   INIT-OBJECTS
   construct step objects PROGRESS ! ;

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
   MAP SHADOW MOVE-IN CHECK-IN REQUIRE-FRESH OBJECTS-IN ROUTE IMAGE-IN ;

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
      objects OF
         DISPOSE-OBJECTS construct step required PROGRESS ! true
      ENDOF
      routed OF
         PRIOR-RUNNER @ SOURCE-UNIT:USE construct step objects PROGRESS ! true
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
      objects OF false ENDOF
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
