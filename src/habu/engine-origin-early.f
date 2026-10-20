\ Capture prefix and checked definitions before the source loader and XREF
\ load. The full origin module consumes these rows before image capture.
\ Validate the borrowed source at acceptance; retain only its coordinates.
\ The boot prefix includes native-body-abi.f before this file; require is
\ unavailable here until include.f loads. native-body.f loads after the hook.
\ The native builder calls PREFIX-TAKE for each accepted global definition in
\ its checked prefix after this module loads, before the hook is installed.
\ engine-origin-checked.f installs the later check-hook observer.

package ENGINE-ORIGIN-EARLY
public

0 constant E-REC
1 constant E-SYM
2 constant E-START
3 constant E-END
4 constant E-PATH-U
5 constant E-NAME-U
6 constant E-SERIAL
7 constant E-CELLS

\ Partial cold images retain the owner but can omit this unused constant name.
\ The installed callback cell is fixed by checker-owner-abi.f.
$270 constant RECORD-SYM-OFF
\ Carl's prepared-unit callbacks occupy $3E8..$400 in the owner; the origin
\ callback follows them in a full image and is never called by a cold scanner.
$408 constant ORIGIN-REC-OFF
\ layout.f has not loaded yet; the prefix dictionary record is 48 bytes.
48 constant RECORD-BYTES

private

$2800 constant PATH-OFF
$2808 constant PATHLEN-OFF
$2D68 constant SOURCE-OFF
$2D70 constant SIZE-OFF
$3688 constant PEND-OFF
$2850 constant PENDNAME-OFF
$250 constant DEFNAME-OFF
$560 constant LAST-OFF
\ src/habu/layout.f NCOMP-DISPATCH:DECL-CELL; layout loads after this observer.
$360 constant OWNER-OFF
$368 constant TARGET-OFF
$00007FFFFFFFFFFF constant RECORD-LEN-MASK
PATH-OFF RESERVED-PTR-U8-CELL PATH-A
SOURCE-OFF RESERVED-PTR-U8-CELL SOURCE-A
PENDNAME-OFF RESERVED-PTR-U8-CELL OPEN-A
DEFNAME-OFF RESERVED-PTR-U8-CELL DEF-NAME-A

PTR-VARIABLE SCR-P
variable SCR-U
variable SCR-CAP
variable FILE-SERIAL
variable FILE-PHASE
variable FILE-ARMED
variable CAST-REG
NULL-PTR SCR-P !
1 FILE-ARMED !

CAST: SYM-XT ( n -- [ ptr u8 n -- n ] )
CAST: CAST-XT ( n -- [ ptr u8 n ptr u8 n -- ] )
CAST: CAST>N ( [ ptr u8 n ptr u8 n -- ] -- n )
CAST: EXPORT>N ( [ ptr u8 n n -- ] -- n )
CAST: RECORD-PTR ( n -- ptr n )

\ Byte 16 of the published record holds its name length and flags.
: RECORD-NAME-U ( n -- n )
   RECORD-BYTES * dbase@ + RECORD-PTR
   16 + @ RECORD-LEN-MASK and ;

: RECORD-SYM ( ptr u8 n -- n )
   data-base OWNER-OFF + 0 ptr-field @
   RECORD-SYM-OFF + CELL-VIEW @
   SYM-XT execute ;

: ROUND-CELL ( n -- n ) CELL 1 - + CELL / CELL * ;

: COPY ( ptr u8 ptr u8 n -- ) {: src:ptr dest:ptr len:n :}
   len 0 ?do src i + c@ dest i + c! loop ;

: SCR-ENSURE ( n -- ) {: need:n :}
   need SCR-CAP @ <= if exit then
   need SCR-CAP @ 2 * max 4096 max {: cap:n :}
   cap map-anon 0 <> if drop s" engine origin: early mmap failed" 76 die then
   {: fresh:ptr :}
   SCR-U @ 0 > if
      SCR-P @ BYTE-VIEW fresh BYTE-VIEW SCR-U @ COPY
      SCR-P @ SCR-CAP @ munmap 0 <> if
         s" engine origin: early munmap failed" 76 die
      then
   then
   fresh SCR-P !  cap SCR-CAP ! ;

: APPEND-ORD ( n n n n ptr u8 n ptr u8 n -- )
   {: rec:n sym:n at:n end:n path:ptr pathu:n name:ptr nameu:n :}
   SCR-U @ ROUND-CELL {: off:n :}
   off E-CELLS cells + pathu + nameu + {: used:n :}
   used SCR-ENSURE
   SCR-P @ BYTE-VIEW off + CELL-VIEW {: row:ptr :}
   rec row E-REC cells + !
   sym row E-SYM cells + !
   at row E-START cells + !  end row E-END cells + !
   pathu row E-PATH-U cells + !  nameu row E-NAME-U cells + !
   1 FILE-SERIAL +!
   FILE-SERIAL @ row E-SERIAL cells + !
   path row E-CELLS cells + BYTE-VIEW pathu COPY
   name row E-CELLS cells + pathu + BYTE-VIEW nameu COPY
   used SCR-U ! ;

: SOURCE-NAME ( -- ptr u8 )
   data-base PEND-OFF + @ 0= if DEF-NAME-A @ exit then
   OPEN-A @ ;

: TAKE-REC-AT ( ptr u8 n n n ptr u8 -- )
   {: name:ptr nameu:n rec:n sym:n token:ptr :}
   nameu 0 <= if exit then
   rec 0= if exit then
   rec dbase@ - RECORD-BYTES / {: ord:n :}
   PATH-A @ {: path:ptr :}
   data-base PATHLEN-OFF + @ {: pathu:n :}
   SOURCE-A @ {: source:ptr :}
   data-base SIZE-OFF + @ {: sourceu:n :}
   token source - {: at:n :}
   pathu 0 > sourceu 0 > and
   at 0 >= at sourceu <= and and
   nameu sourceu at - <= and if
      token nameu name nameu CORE-STR= if
         ord sym at at nameu + path pathu name nameu APPEND-ORD
         exit
      then
   then
   ord sym 0 0 NULL-PTR 0 name nameu APPEND-ORD ;

: TAKE-REC ( ptr u8 n n n -- ) SOURCE-NAME TAKE-REC-AT ;

: TAKE ( ptr u8 n -- )
   {: name:ptr nameu:n :}
   name nameu RECORD-SYM {: sym:n :}
   sym 0= if exit then
   data-base PEND-OFF + @ dup 0= if drop data-base LAST-OFF + @ then
   {: rec:n :}
   name nameu rec sym TAKE-REC ;

: OBSERVE-CAST ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n sig:ptr sigu:n :}
   name nameu sig sigu CAST-REG @ CAST-XT execute
   name nameu RECORD-SYM {: sym:n :}
   data-base PEND-OFF + @ {: rec:n :}
   name nameu rec sym DEF-NAME-A @ TAKE-REC-AT ;

: EXPORT-ACCEPT ( ptr u8 n n -- )
   {: operand:ptr operand-u:n ord:n :}
   FILE-ARMED @ 0= if exit then
   ord RECORD-NAME-U {: name-u:n :}
   operand operand-u name-u - + {: name:ptr :}
   name name-u RECORD-SYM {: sym:n :}
   sym 0= if exit then
   name name-u dbase@ ord RECORD-BYTES * + sym name TAKE-REC-AT ;

public

: EXPORT-ENTRY ( [ ptr u8 n n -- ] -- )
   {: accept :}
   accept EXPORT>N data-base TARGET-OFF + 0 ptr-field @
      CHECKER-OWNER-ABI:EXPORT-ACCEPT-OFF + CELL-VIEW xt! ;

\ A mark names the last semantic acceptance, independent of the scratch map.
\ Phase 0 is the prefix/early observer; phase 1 is the full origin observer.
: FILE-MARK ( -- n n bool )
   FILE-SERIAL @ FILE-PHASE @ FILE-ARMED @ 0= 0= ;

\ Imported rows get receiver serials; only observer state crosses a FILE.
: FILE-RESTORE ( n bool -- ) {: phase:n armed:bool :}
   phase FILE-PHASE !
   armed if 1 else 0 then FILE-ARMED ! ;
: FILE-PENDING? ( -- bool ) SCR-U @ 0 > ;

: FILE-STATE-SPANS ( [ ptr u8 n -- ] -- ) {: visit :}
   SCR-P BYTE-VIEW CELL visit execute
   SCR-U BYTE-VIEW CELL visit execute
   SCR-CAP BYTE-VIEW CELL visit execute
   FILE-SERIAL BYTE-VIEW CELL visit execute
   FILE-PHASE BYTE-VIEW CELL visit execute
   FILE-ARMED BYTE-VIEW CELL visit execute ;

: FILE-NEXT-SERIAL ( -- n ) 1 FILE-SERIAL +! FILE-SERIAL @ ;
: FILE-FULL ( -- ) 1 FILE-PHASE ! ;
: FILE-ARM ( -- ) 1 FILE-ARMED ! ;
: FILE-DISARM ( -- ) 0 FILE-ARMED ! ;
: ROWS ( -- ptr u8 n ) SCR-P @ BYTE-VIEW SCR-U @ ;

: EARLY-FILE-ROW ( ptr n [ n n n n ptr u8 n ptr u8 n n n -- ] -- )
   {: row:ptr visit :}
   row E-CELLS cells + BYTE-VIEW {: path:ptr :}
   row E-SERIAL cells + @ 0 row E-REC cells + @ row E-SYM cells + @
   path row E-PATH-U cells + @
   path row E-PATH-U cells + @ + row E-NAME-U cells + @
   row E-START cells + @ row E-END cells + @ visit execute ;

: FILE-ROWS ( n [ n n n n ptr u8 n ptr u8 n n n -- ] -- )
   {: mark:n visit :}
   ROWS {: base:ptr used:n :}
   0 begin dup used < while
      ROUND-CELL {: off:n :}
      base off + CELL-VIEW {: row:ptr :}
      row E-SERIAL cells + @ mark > if row visit EARLY-FILE-ROW then
      off E-CELLS cells +
      row E-PATH-U cells + @ + row E-NAME-U cells + @ +
   repeat drop ;

\ The borrowed compiler has not handed its symbols to this target checker yet.
\ Save the pending global record and resolve its symbol at the later drain.
: PREFIX-TAKE ( ptr u8 n n -- ) 0 TAKE-REC ;

\ The FILE decoder resolves the record and any checked symbol. A prefix
\ global has no symbol yet and keeps zero for resolution during the drain.
\ Imported rows take fresh receiver serials, like locally accepted words.
: FILE-IMPORT ( n n n n ptr u8 n ptr u8 n -- )
   {: ord:n sym:n at:n end:n path:ptr pathu:n name:ptr nameu:n :}
   ord sym at end path pathu name nameu APPEND-ORD ;

: RELEASE ( -- )
   SCR-CAP @ 0 > if
      SCR-P @ SCR-CAP @ munmap 0 <> if
         s" engine origin: early munmap failed" 76 die
      then
   then
   NULL-PTR SCR-P !  0 SCR-U !  0 SCR-CAP ! ;

: CHECKED-TAKE ( ptr u8 n -- ) TAKE ;

: CERTIFY-CAST ( ptr u8 n ptr u8 n -- )
   CAST-REG @ CAST-XT execute ;

: CAST-ENTRY ( [ ptr u8 n ptr u8 n -- ] -- n ) CAST>N ;

;package
