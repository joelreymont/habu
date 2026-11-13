\ storage.f - WSTORE, DYNAMIC-BUFFER in a generated module: test/wasm/storage-test.f
\ builds each public word as a module's entry, runs it and checks every line it
\ prints. Each entry starts in a fresh module, its free list empty and its
\ memory ending at its image's last page, P below.
package WSTORE
private

DYNAMIC-BUFFER BYTES u8
DYNAMIC-BUFFER CELLS n
DYNAMIC-BUFFER PTRS ptr u8
DYNAMIC-BUFFER A u8
DYNAMIC-BUFFER B u8
DYNAMIC-BUFFER C u8
STRUCTURE triple DERIVE addr FIELD a n FIELD b n FIELD c n ;STRUCTURE
DYNAMIC-BUFFER WIDE triple
PTR-VARIABLE FIRST
PTR-VARIABLE SECOND

STRUCTURE sample OPAQUE DERIVE addr
   FIELD number n
   FIELD value r
;STRUCTURE
2 TYPED-BUFFER ROWS sample

: SAME ( ptr a ptr a -- n )
   = if 1 else 0 then ;

: PAST-MEMORY ( -- )  $100000000 BYTES-RESERVE ;
: PAST-MEMORY32 ( -- )  $1000000000000 BYTES-RESERVE ;

: GROW ( -- )
   100 BYTES-RESERVE
   77 103 BYTES c!
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
   ['] PAST-MEMORY catch .
   ['] PAST-MEMORY32 catch .
   103 BYTES c@ .
   69999 BYTES c@ . ;

: REUSE ( -- )
   0 BYTES FIRST !
   BYTES-RELEASE
   BYTES-RELEASE
   70000 BYTES-RESERVE
   0 BYTES FIRST @ SAME .
   103 BYTES c@ .
   69999 BYTES c@ . ;

public

: FRACTIONAL ( -- )
   0 WIDE-RESERVE
   1 WIDE-RESERVE
   7 0 WIDE TRIPLE-A byte-view 63 + c!
   1 WIDE drop
   3 WIDE-RESERVE
   0 WIDE TRIPLE-A byte-view 63 + c@ .
   4 WIDE drop ;

: RECORD ( -- )
   23 0 ROWS SAMPLE-NUMBER !  1.25 0 ROWS SAMPLE-VALUE !
   47 1 ROWS SAMPLE-NUMBER !  -2.5 1 ROWS SAMPLE-VALUE !
   0 ROWS SAMPLE-NUMBER @ .  0 ROWS SAMPLE-VALUE @ f.
   1 ROWS SAMPLE-NUMBER @ .  1 ROWS SAMPLE-VALUE @ f. ;

\ One buffer's life as on the native target. 100 bytes hold index 103: the
\ capacity is the request rounded up to a cell. Growing to 70000 bytes maps two
\ pages at P+1, copies the old page's bytes and frees it. A refused reserve
\ throws its code and keeps the mapping and its capacity: 4 GiB, which
\ memory.grow refuses, and 2^48 bytes, more pages than memory32 has. Reserving
\ a released buffer again takes its two pages back, cleared.
: BUFFER ( -- )
   GROW REFUSE REUSE
   BYTES-RELEASE CELLS-RELEASE PTRS-RELEASE ;

\ One-page neighbours A, B, C, freed as A, C, B, join into one run. BYTES is held first:
\ a first reserve maps the registry's page after its own (src/core/dynamic-storage.f REG-ROOM).
: HOLES ( -- )
   1 BYTES-RESERVE
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
   B-RELEASE  C-RELEASE  BYTES-RELEASE ;

\ TWICE frees a mapped page twice, and INSIDE the second of two neighbouring
\ pages again after both joined one run. Each run then overlaps a free one, so
\ its second free traps, after the lines of the maps and frees before it.
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

\ Frees a mapped page with a zero length, at an address inside it, with a
\ negative length, and at the address 2^32 past it, which no memory offset is
\ and which wraps to it in 32 bits: each answers -1 and leaves the free list as
\ it was, so the page then frees, maps back whole and frees again.
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

false [if] \ off until dot habu-add-the-checks-a49d054d
: PAST-CAP ( -- )  104 BYTES drop ;
: AT-CAP ( -- )  70000 BYTES drop ;
: NEGATIVE-IDX ( -- )  -1 BYTES drop ;
: RELEASED ( -- )  0 BYTES drop ;
: NEGATIVE-COUNT ( -- )  -1 BYTES-RESERVE ;
: WIDE-COUNT ( -- )  $7FFFFFFFFFFFFFFF PTRS-RESERVE ;
: WIDE-PAST ( -- ) 2 WIDE drop ;
: WIDE-PAST-GROWN ( -- ) 5 WIDE drop ;

\ A buffer refuses an index past its capacity, a negative one and any after
\ release; a reserve refuses a negative count and one whose bytes overflow.
\ The accessor bounds by its declared width.
: BOUNDS ( -- )
   100 BYTES-RESERVE
   ['] PAST-CAP catch .
   ['] NEGATIVE-COUNT catch .
   ['] NEGATIVE-IDX catch .
   ['] WIDE-COUNT catch .
   70000 BYTES-RESERVE
   ['] AT-CAP catch .
   BYTES-RELEASE
   ['] RELEASED catch .
   0 WIDE-RESERVE
   ['] WIDE-PAST catch .
   1 WIDE-RESERVE
   ['] WIDE-PAST catch .
   3 WIDE-RESERVE
   ['] WIDE-PAST-GROWN catch .
   WIDE-RELEASE ;
[then]

;package
