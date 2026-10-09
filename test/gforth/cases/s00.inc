\ What the s cases require beside them: the ends of a protected band, read
\ from src/habu/layout.f's DATA-BANDS table by row as DATA offsets, and one
\ write per span-guarded sink at a DATA offset (habu1.f:295 GUARD-SPAN; the
\ host's src/host/gforth/prims.fs SPAN-GUARD). Each write leaves the bytes it
\ spans as they were, so a write the guard passes changes nothing.
require src/habu/layout.f
package SG
private
: AT ( n -- ptr u8 ) data-base BYTE-VIEW + ;
: CELL-AT ( n -- ptr n ) data-base + ;
: ADDR ( n -- n ) data-base BYTE-VIEW NULL-PTR BYTE-VIEW - + ;
create EMPTY-PATH 0 c,                \ "", which resolves to nothing
public
\ Row ix's first byte and its end, the byte after its last.
: FIRST ( n -- n ) DATA-BANDS:OFF ;
: END ( n -- n ) dup DATA-BANDS:OFF swap DATA-BANDS:LEN + ;
\ The sinks, each writing from the offset the width it names.
: VIA-C! ( n -- ) AT dup c@ swap c! ;                  \ 1 byte
: VIA-! ( n -- ) CELL-AT dup @ swap ! ;                \ 8 bytes
: VIA-+! ( n -- ) CELL-AT 0 swap +! ;                  \ 8 bytes
: VIA-XT! ( n -- ) CELL-AT dup @ swap xt! ;            \ 8 bytes
\ 4 bytes. Only where the guard refuses it: past the guard native flips the
\ target's pages for an instruction write, which a DATA page does not survive.
TRUSTED: VIA-PATCH32 ( n -- )
   dup CELL-AT @ $FFFFFFFF and swap ADDR patch32 ;
: VIA-READ ( n -- ) -1 swap AT 8 read drop ;           \ 8 bytes, from no fd
: VIA-REALPATH ( n -- ) EMPTY-PATH swap AT 8 realpath drop ;   \ 8 bytes
\ 8 bytes. No case gives it a page-aligned offset, so an unmap the guard
\ passes fails and frees nothing.
: VIA-MUNMAP ( n -- ) CELL-AT 8 munmap drop ;
;package
