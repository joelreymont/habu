\ storage.f - WSTORE, DYNAMIC-BUFFER in a generated module: test/wasm/storage-test.f
\ builds each public word as a module's entry, runs it and checks every line it
\ prints. Each entry starts in a fresh module, its free list empty and its
\ memory ending at its image's last page, P below.
\
\ BUFFER is one buffer's life as on the native target. 100 bytes hold index 103
\ and not 104: the capacity is the request rounded up to a cell. Growing to
\ 70000 bytes maps two pages at P+1, copies the old page's bytes and frees it.
\ A refused reserve throws its code and keeps the mapping and its capacity: a
\ negative count, a count whose bytes overflow, 4 GiB, which memory.grow
\ refuses, and 2^48 bytes, more pages than memory32 has. A released buffer
\ refuses every index; reserving it again takes its two pages back, cleared.
\
\ HOLES frees three one-page neighbours A, B, C as A, C, B: B joins both into
\ one run of three pages, which a three-page buffer then takes whole. Freed
\ again, a one-page buffer takes its first page and a two-page one the rest.
\
\ TWICE frees a mapped page twice, and INSIDE the second of two neighbouring
\ pages again after both joined one run. Each run then overlaps a free one, so
\ its second free traps, after the lines of the maps and frees before it.
\
\ REFUSED frees a mapped page with a zero length, at an address inside it, with
\ a negative length, and at the address 2^32 past it, which no memory offset
\ is and which wraps to it in 32 bits: each answers -1 and leaves the free list
\ as it was, so the page then frees, maps back whole and frees again.
package WSTORE
private

DYNAMIC-BUFFER BYTES u8
DYNAMIC-BUFFER CELLS n
DYNAMIC-BUFFER PTRS ptr u8
DYNAMIC-BUFFER A u8
DYNAMIC-BUFFER B u8
DYNAMIC-BUFFER C u8
PTR-VARIABLE FIRST
PTR-VARIABLE SECOND

: SAME ( ptr a ptr a -- n )
   = if 1 else 0 then ;

: PAST-CAP ( -- )  104 BYTES drop ;
: AT-CAP ( -- )  70000 BYTES drop ;
: NEGATIVE-IDX ( -- )  -1 BYTES drop ;
: RELEASED ( -- )  0 BYTES drop ;
: NEGATIVE-COUNT ( -- )  -1 BYTES-RESERVE ;
: WIDE-COUNT ( -- )  $7FFFFFFFFFFFFFFF PTRS-RESERVE ;
: PAST-MEMORY ( -- )  $100000000 BYTES-RESERVE ;
: PAST-MEMORY32 ( -- )  $1000000000000 BYTES-RESERVE ;

: GROW ( -- )
   100 BYTES-RESERVE
   77 103 BYTES c!
   ['] PAST-CAP catch .
   70000 BYTES-RESERVE
   103 BYTES c@ .
   88 69999 BYTES c!
   69999 BYTES c@ .
   3 CELLS-RESERVE
   312 2 CELLS !
   2 CELLS @ .
   2 PTRS-RESERVE
   0 BYTES 1 PTRS !
   1 PTRS @ 0 BYTES SAME . ;

: REFUSE ( -- )
   ['] NEGATIVE-COUNT catch .
   ['] NEGATIVE-IDX catch .
   ['] WIDE-COUNT catch .
   ['] PAST-MEMORY catch .
   ['] PAST-MEMORY32 catch .
   103 BYTES c@ .
   69999 BYTES c@ .
   ['] AT-CAP catch . ;

: REUSE ( -- )
   0 BYTES FIRST !
   BYTES-RELEASE
   ['] RELEASED catch .
   BYTES-RELEASE
   70000 BYTES-RESERVE
   0 BYTES FIRST @ SAME .
   103 BYTES c@ .
   69999 BYTES c@ . ;

public

: BUFFER ( -- )
   GROW REFUSE REUSE
   BYTES-RELEASE CELLS-RELEASE PTRS-RELEASE ;

: HOLES ( -- )
   65536 A-RESERVE  65536 B-RESERVE  65536 C-RESERVE
   0 A FIRST !  0 B SECOND !
   5 100 B c!
   A-RELEASE  C-RELEASE  B-RELEASE
   196608 A-RESERVE
   0 A FIRST @ SAME .
   65636 A c@ .
   A-RELEASE
   65536 B-RESERVE
   0 B FIRST @ SAME .
   131072 C-RESERVE
   0 C SECOND @ SAME .
   B-RELEASE  C-RELEASE ;

: TWICE ( -- )
   65536 map-anon . {: p:ptr :}
   p 65536 munmap .
   p 65536 munmap . ;

: INSIDE ( -- )
   65536 map-anon . {: p:ptr :}
   65536 map-anon . {: q:ptr :}
   q byte-view p byte-view - .
   p 65536 munmap .
   q 65536 munmap .
   q 65536 munmap . ;

: REFUSED ( -- )
   65536 map-anon . {: p:ptr :}
   p 0 munmap .
   p byte-view 100 + 65536 munmap .
   p -65536 munmap .
   p $100000001 munmap .
   p byte-view $100000000 + 65536 munmap .
   p 65536 munmap .
   65536 map-anon . {: q:ptr :}
   q p SAME .
   q 65536 munmap . ;

;package
