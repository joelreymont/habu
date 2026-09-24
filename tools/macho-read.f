\ Read the container extents of Habu's ARM64 Mach-O executables. Offsets come
\ from load commands and section records, never from the inspecting host.
require lib/le.f
require lib/string.f

package MACHO-READ

PTR-VARIABLE IMAGE
variable BYTES
variable CMD-END
variable TEXT-CMD
variable DATA-CMD
variable LINK-CMD
variable FIX-CMD
variable SIGN-CMD
variable CURSOR

: CHECK ( bool -- )
   0= if s" macho-read: invalid Habu Mach-O container" 74 die then ;

: AT ( n n -- ptr u8 ) {: off:n size:n :}
   off 0 >= size 0 >= and off BYTES @ <= and CHECK
   size BYTES @ off - <= CHECK
   IMAGE @ off + ;

: U32@ ( n -- n ) 4 AT LE:U32@ ;
: U64@ ( n -- n ) 8 AT LE:U64@ ;

: NAME? ( n ptr u8 n -- bool ) {: off:n name:ptr size:n :}
   off 16 AT size name size STR= 0= if false exit then
   size 16 < if off size + 1 AT c@ 0= else true then ;

: UNIQUE! ( n ptr n -- ) {: off:n slot:ptr :}
   slot @ 0= CHECK off slot ! ;

: SEGMENT ( n n -- ) {: off:n size:n :}
   size 72 >= CHECK
   off 64 + U32@ 80 * 72 + size = CHECK
   off 8 + s" __TEXT" NAME? if off TEXT-CMD UNIQUE! exit then
   off 8 + s" __DATA_CONST" NAME? if off DATA-CMD UNIQUE! exit then
   off 8 + s" __LINKEDIT" NAME? if off LINK-CMD UNIQUE! exit then
   off 8 + s" __PAGEZERO" NAME? CHECK
   off 40 + U64@ 0= off 48 + U64@ 0= and CHECK ;

: COMMAND ( n n -- ) {: off:n size:n :}
   off U32@ {: kind:n :}
   kind $19 = if off size SEGMENT exit then
   kind $80000034 = if size 16 = CHECK off FIX-CMD UNIQUE! exit then
   kind $1D = if size 16 = CHECK off SIGN-CMD UNIQUE! then ;

public

: MAGIC? ( ptr u8 n -- bool ) {: image:ptr size:n :}
   size 32 < if false exit then
   image LE:U32@ $FEEDFACF = ;

: COMMANDS-END ( -- n ) CMD-END @ ;
: TEXT-OFF ( -- n ) TEXT-CMD @ 120 + U32@ ;
: TEXT-END ( -- n ) TEXT-OFF TEXT-CMD @ 112 + U64@ + ;
: TEXT-LIMIT ( -- n ) TEXT-CMD @ 48 + U64@ ;
: TEXT-VA ( -- n ) TEXT-CMD @ 104 + U64@ ;
: DATA-OFF ( -- n ) DATA-CMD @ 40 + U64@ ;
: DATA-BYTES ( -- n ) DATA-CMD @ 48 + U64@ ;
: GOT-BYTES ( -- n ) DATA-CMD @ 112 + U64@ ;
: LINK-OFF ( -- n ) LINK-CMD @ 40 + U64@ ;
: LINK-BYTES ( -- n ) LINK-CMD @ 48 + U64@ ;
: FIXUPS-OFF ( -- n ) FIX-CMD @ 8 + U32@ ;
: FIXUPS-BYTES ( -- n ) FIX-CMD @ 12 + U32@ ;
: SIGN-OFF ( -- n ) SIGN-CMD @ 8 + U32@ ;
: SIGN-BYTES ( -- n ) SIGN-CMD @ 12 + U32@ ;

private

: CHECK-TEXT ( -- )
   TEXT-CMD @ 4 + U32@ 152 = CHECK
   TEXT-CMD @ 72 + s" __text" NAME? CHECK
   TEXT-CMD @ 88 + s" __TEXT" NAME? CHECK
   TEXT-CMD @ 40 + U64@ 0= CHECK
   TEXT-OFF $1000 = CHECK
   TEXT-CMD @ 24 + U64@ TEXT-OFF + TEXT-VA = CHECK
   TEXT-CMD @ 112 + U64@ 0 >= CHECK
   TEXT-END TEXT-OFF >= TEXT-END TEXT-LIMIT <= and CHECK
   0 TEXT-LIMIT AT drop ;

: CHECK-DATA ( -- )
   DATA-CMD @ 4 + U32@ 152 = CHECK
   DATA-CMD @ 72 + s" __got" NAME? CHECK
   DATA-CMD @ 88 + s" __DATA_CONST" NAME? CHECK
   DATA-CMD @ 120 + U32@ DATA-OFF = CHECK
   GOT-BYTES 16 = CHECK
   DATA-OFF TEXT-LIMIT = CHECK
   DATA-BYTES GOT-BYTES >= CHECK
   DATA-OFF DATA-BYTES AT drop ;

: CHECK-LINK ( -- )
   LINK-CMD @ 4 + U32@ 72 = CHECK
   LINK-OFF DATA-OFF DATA-BYTES + = CHECK
   LINK-OFF LINK-BYTES + BYTES @ = CHECK
   LINK-OFF LINK-BYTES AT drop
   FIXUPS-OFF LINK-OFF = CHECK
   FIXUPS-OFF FIXUPS-BYTES AT drop
   SIGN-OFF FIXUPS-OFF FIXUPS-BYTES + >= CHECK
   SIGN-OFF SIGN-BYTES + BYTES @ = CHECK
   SIGN-OFF SIGN-BYTES AT drop ;

public

: OPEN ( ptr u8 n -- ) {: image:ptr size:n :}
   image size MAGIC? CHECK
   image IMAGE ! size BYTES !
   4 U32@ $100000C = 12 U32@ 2 = and CHECK
   0 TEXT-CMD ! 0 DATA-CMD ! 0 LINK-CMD ! 0 FIX-CMD ! 0 SIGN-CMD !
   20 U32@ 32 + CMD-END !
   CMD-END @ $1000 <= CHECK
   32 CURSOR !
   16 U32@ $1000 8 / <= CHECK
   16 U32@ 0 ?do
      CURSOR @ {: off:n :}
      off 8 + CMD-END @ <= CHECK
      off 4 + U32@ {: width:n :}
      width 8 >= width 8 mod 0= and CHECK
      width CMD-END @ off - <= CHECK
      off width COMMAND
      off width + CURSOR !
   loop
   CURSOR @ CMD-END @ = CHECK
   TEXT-CMD @ 0 > DATA-CMD @ 0 > and LINK-CMD @ 0 > and
      FIX-CMD @ 0 > and SIGN-CMD @ 0 > and CHECK
   CHECK-TEXT CHECK-DATA CHECK-LINK ;

;package
