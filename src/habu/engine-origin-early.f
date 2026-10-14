\ Capture checked definitions before the source loader, SHA256 and XREF load.
\ The full origin module consumes these process-local rows before image capture.
\ Source bytes are copied at acceptance because the loader may release them.
\ This observer begins after check-hook.f installs the checker. Definitions
\ before that boundary, and definers that bypass the hook, have no row here.

package ENGINE-ORIGIN-EARLY
public

0 constant E-REC
1 constant E-SYM
2 constant E-START
3 constant E-END
4 constant E-PATH-U
5 constant E-NAME-U
6 constant E-SOURCE-U
7 constant E-CELLS

\ Partial cold images retain the owner but can omit this unused constant name.
\ The installed callback cell is fixed by checker-owner-abi.f.
$270 constant RECORD-SYM-OFF
\ Carl's prepared-unit callbacks occupy $3E8..$400 in the owner; the origin
\ callback follows them in a full image and is never called by a cold scanner.
$408 constant ORIGIN-REC-OFF

private

$2800 constant PATH-OFF
$2808 constant PATHLEN-OFF
$2810 constant SOURCE-OFF
$36A8 constant END-OFF
$3688 constant PEND-OFF
$2850 constant PENDNAME-OFF
$250 constant DEFNAME-OFF
$560 constant LAST-OFF
\ src/habu/layout.f NCOMP-DISPATCH:DECL-CELL; layout loads after this observer.
$360 constant OWNER-OFF
PATH-OFF RESERVED-PTR-U8-CELL PATH-A
SOURCE-OFF RESERVED-PTR-U8-CELL SOURCE-A
END-OFF RESERVED-PTR-U8-CELL SOURCE-END
PENDNAME-OFF RESERVED-PTR-U8-CELL OPEN-A
DEFNAME-OFF RESERVED-PTR-U8-CELL DEF-NAME-A

PTR-VARIABLE SCR-P
variable SCR-U
variable SCR-CAP
NULL-PTR SCR-P !

CAST: SYM-XT ( n -- [ ptr u8 n -- n ] )

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

: APPEND ( n n n n ptr u8 n ptr u8 n ptr u8 n -- )
   {: rec:n sym:n at:n end:n path:ptr pathu:n name:ptr nameu:n source:ptr sourceu:n :}
   SCR-U @ ROUND-CELL {: off:n :}
   off E-CELLS cells + pathu + nameu + sourceu + {: used:n :}
   used SCR-ENSURE
   SCR-P @ BYTE-VIEW off + CELL-VIEW {: row:ptr :}
   rec row E-REC cells + !  sym row E-SYM cells + !
   at row E-START cells + !  end row E-END cells + !
   pathu row E-PATH-U cells + !  nameu row E-NAME-U cells + !
   sourceu row E-SOURCE-U cells + !
   path row E-CELLS cells + BYTE-VIEW pathu COPY
   name row E-CELLS cells + pathu + BYTE-VIEW nameu COPY
   source row E-CELLS cells + pathu + nameu + BYTE-VIEW sourceu COPY
   used SCR-U ! ;

: SOURCE-NAME ( -- ptr u8 )
   data-base PEND-OFF + @ 0= if DEF-NAME-A @ exit then
   OPEN-A @ ;

: TAKE ( ptr u8 n -- )
   {: name:ptr nameu:n :}
   nameu 0 <= if exit then
   name nameu RECORD-SYM {: sym:n :}
   sym 0= if exit then
   data-base PEND-OFF + @ dup 0= if drop data-base LAST-OFF + @ then
   {: rec:n :}
   rec 0= if exit then
   PATH-A @ {: path:ptr :}
   data-base PATHLEN-OFF + @ {: pathu:n :}
   SOURCE-A @ {: source:ptr :}
   SOURCE-END @ source - {: sourceu:n :}
   SOURCE-NAME {: token:ptr :}
   token source - {: at:n :}
   pathu 0 > sourceu 0 > and
   at 0 >= at sourceu <= and and
   nameu sourceu at - <= and if
      token nameu name nameu CORE-STR= if
         rec sym at at nameu + path pathu name nameu source sourceu APPEND
         exit
      then
   then
   rec sym 0 0 NULL-PTR 0 name nameu NULL-PTR 0 APPEND ;

: FIRST-TOKEN ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   0 begin dup u < if a over + c@ 32 > else 0 0= 0= then while 1 + repeat
   a swap ;

: CHECKED-HOOK ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u LOWER-CERT-HOOK:HOOK dup -1 = if a u FIRST-TOKEN TAKE then ;

public

: ROWS ( -- ptr u8 n ) SCR-P @ BYTE-VIEW SCR-U @ ;

: RELEASE ( -- )
   SCR-CAP @ 0 > if
      SCR-P @ SCR-CAP @ munmap 0 <> if
         s" engine origin: early munmap failed" 76 die
      then
   then
   NULL-PTR SCR-P !  0 SCR-U !  0 SCR-CAP ! ;

TRUSTED: INSTALL ( -- ) ['] CHECKED-HOOK set-check ;

;package
