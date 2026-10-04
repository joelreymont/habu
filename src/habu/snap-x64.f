\ Persist a fixed-address x86-64 snapshot using SNAP's canonical copies and
\ publication lifecycle. Loaded only on the x86-64 target after snap-lib.f.
require src/habu/image-x64.f

package SNAP

\ RX retains the original engine text, then holds the encoded heap and trailer.
\ REGION and DATA are populated PT_LOADs; the heap is decoded by snapshot boot.
: X64-FRAME ( -- )
   STSZ @ SHL @ + SNAP-TRL-BYTES + {: bytes:n :}
   X64LAYOUT:CODE-OFF bytes + PROT-PAGE-MAX 1- +
      PROT-PAGE-MAX 1- invert and
      X64LAYOUT:CODE-OFF bytes + - SPAD !
   DATA-START SHL @ + SPAD @ + SDW !
   bytes SPAD @ + SNL ! ;

TRUSTED: SXT-PTR ( -- ptr u8 ) SXT-N @ ;

: X64-TEXT ( -- )
   SNL @ SCRATCH SXT-N !
   STB@ SXT-PTR STSZ @ BYTE-COPY
   SHF @ SNAPSHOT-FORMAT:HEAP-GRID = if SGR-PTR else SND-PTR DATA-START + then
      SXT-PTR STSZ @ + SHL @ BYTE-COPY
   SPAD @ 0 ?do 0 SXT-PTR STSZ @ SHL @ + i + + c! loop
   TRL SXT-PTR SNL @ SNAP-TRL-BYTES - + SNAP-TRL-BYTES BYTE-COPY ;

: WRITE-X64 ( -- )
   X64-FRAME
   FILL-TRL
   X64-TEXT
   STAGE
   SXT-PTR SNL @ SNC-PTR SCL @ SND-PTR DATA-START SFD @ X64IMAGE:WRITE-FD
   SFD @ BEFORE-CLOSE
   SFD @ close-rc 0 <> if s" snap: output close failed" 74 die then ;

: INSTALL-X64 ( -- ) [: WRITE-X64 ;] is WRITE-TARGET ;
INSTALL-X64

;package
