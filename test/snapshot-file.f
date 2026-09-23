\ Doctor decoded snapshot DATA, then rebuild the same outer format. Corrupt
\ address/WID headers must reach their validator, not fail in the cell codec.
require src/habu/app-image.f
require src/habu/snapshot-data.f

package SNAPSHOT-FILE
DYNAMIC-BUFFER IMAGE n
DYNAMIC-BUFFER DATA n
DYNAMIC-BUFFER FLAT n
DYNAMIC-BUFFER ENCODED n
variable IMAGE-U
variable DATA-U
variable DATA-OFF
variable TRAILER
variable VERSION
variable PAYLOAD-U
create NEW-TRAILER SNAP-TRL-BYTES allot

: IMAGE@ ( -- ptr u8 ) 0 IMAGE BYTE-VIEW ;
: DATA@ ( -- ptr u8 ) 0 DATA BYTE-VIEW ;
: ENCODED@ ( -- ptr u8 ) 0 ENCODED BYTE-VIEW ;
: IMG-CELL@ ( n -- n ) IMAGE@ + CELL-VIEW @ ;
: CHECK ( bool -- ) 0= if s" snapshot fixture: invalid image" 74 die then ;
: CELLS-FOR ( n -- n ) CELL 1- + CELL / ;

public
: READ-IMAGE ( ptr u8 n -- ) {: path:ptr size:n :}
   path size FILE-SIZE dup IMAGE-U ! CELLS-FOR IMAGE-RESERVE
   path size IMAGE@ IMAGE-U @ READ-ALL IMAGE-U @ = CHECK
   IMAGE-TEXT-SIZE-OFF IMG-CELL@ IMAGE-TEXT-TRAILER-ADJ + SNAP-TRL-BYTES -
      TRAILER !
   TRAILER @ 0 >= TRAILER @ SNAP-TRL-BYTES + IMAGE-U @ <= and CHECK
   TRAILER @ IMG-CELL@ SNAP-MAGIC = CHECK
   TRAILER @ SNAP-TRL-VERSION + IMG-CELL@ VERSION !
   TRAILER @ SNAP-TRL-DATALEN + IMG-CELL@ {: stored:n :}
   stored 0 > stored TRAILER @ <= and CHECK
   TRAILER @ stored - DATA-OFF !
   IMAGE@ DATA-OFF @ + stored VERSION @ SNAPSHOT-DATA:EXTENT
      dup 0 > CHECK dup DATA-U ! CELLS-FOR DATA-RESERVE
   IMAGE@ DATA-OFF @ + stored VERSION @ DATA@ DATA-U @ SNAPSHOT-DATA:READ
      dup 0 > CHECK PAYLOAD-U ! ;

: SIZE ( -- n ) DATA-U @ ;
: FORMAT ( -- n ) VERSION @ ;
: BYTE@ ( n -- n ) DATA@ + c@ ;
: BYTE! ( n n -- ) {: off:n value:n :} value DATA@ off + c! ;
: CELL@ ( n -- n ) DATA@ + CELL-VIEW @ ;
: CELL! ( n n -- ) {: off:n value:n :} value DATA@ off + CELL-VIEW ! ;

private
: PACK ( -- )
   DATA-U @ 2 * IMAGE-CELLS:GROUP-BYTES 32 + + PROT-PAGE-MAX +
      CELLS-FOR ENCODED-RESERVE
   VERSION @ ADDRESS-CELLS:SNAPSHOT-VERSION = if
      DATA@ ENCODED@ DATA-U @ BYTE-COPY DATA-U @ PAYLOAD-U ! exit
   then
   DATA-U @ IMAGE-CELLS:BM-BYTE-SPAN 1- + IMAGE-CELLS:BM-BYTE-SPAN /
      CELLS-FOR FLAT-RESERVE
   DATA@ DATA-U @ 0 FLAT BYTE-VIEW IMAGE-CELLS:BITMAP! {: bmu:n values:n :}
   0 FLAT BYTE-VIEW bmu ENCODED@ 16 + IMAGE-CELLS:BM! {: groups:n stored:n :}
   DATA-U @ ENCODED@ CELL-VIEW !
   groups ENCODED@ 8 + IMAGE-CELLS:U32!
   stored ENCODED@ 12 + IMAGE-CELLS:U32!
   16 groups IMAGE-CELLS:PMAP-BYTES + stored + {: at:n :}
   DATA@ DATA-U @ ENCODED@ at + IMAGE-CELLS:VALUES! values = CHECK
   at values + PAYLOAD-U ! ;

public
: WRITE ( ptr u8 n -- ) {: path:ptr size:n :}
   PACK
   DATA-OFF @ CODE-OFF - PAYLOAD-U @ + SNAP-TRL-BYTES + {: body:n :}
   body BUILD-SNAP-HDR {: text:n :} SNAP-DROP
   text CODE-OFF - body - {: pad:n :}
   pad 0 >= pad PROT-PAGE-MAX < and CHECK
   pad 0 ?do 0 ENCODED@ PAYLOAD-U @ + i + c! loop
   IMAGE@ TRAILER @ + NEW-TRAILER SNAP-TRL-BYTES BYTE-COPY
   PAYLOAD-U @ pad + NEW-TRAILER SNAP-TRL-DATALEN + CELL-VIEW !
   VERSION @ NEW-TRAILER SNAP-TRL-VERSION + CELL-VIEW !
   path size PATH0 1537 493 open {: fd:n :} fd 0 >= CHECK
   fd MBUF CODE-OFF FDIO:WALL
   fd IMAGE@ CODE-OFF + DATA-OFF @ CODE-OFF - FDIO:WALL
   fd ENCODED@ PAYLOAD-U @ pad + FDIO:WALL
   fd NEW-TRAILER SNAP-TRL-BYTES FDIO:WALL
   fd SNAP-EXTRA-PTR SNAP-EXTRA-SIZE FDIO:WALL
   fd close-rc 0= CHECK
   path size CODESIGN:FORCE ;

: RELEASE ( -- )
   IMAGE-RELEASE DATA-RELEASE FLAT-RELEASE ENCODED-RELEASE ;
;package
