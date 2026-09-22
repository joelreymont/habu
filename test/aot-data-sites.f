\ Exercise the actual packed-site appenders beyond their former 16,384 rows.
\ No generated code is executed: the buffer contains tagged metadata cells and
\ correctly shaped address chains, then survives file and owned-value transfer.
require lib/test.f
require src/os/script-argv.f
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f
require src/habu/aot-ident.f
require src/habu/fdio.f
require src/habu/aot-owned.f

package AOT-CAPTURE
public
: SITE-TEST-DATA+ ( n -- ) ACAP-ADD-DSITE ;
: SITE-TEST-CODE+ ( n -- ) ACAP-ADD-CSITE ;
: SITE-TEST-NORMALIZE ( -- ) ACAP-NORMALIZE-DSITES ;
: SITE-TEST-HELD? ( n -- bool ) ACAP-DSITE-HELD? ;
\ ACAP-DEFER-SITE's last line, over the two words it composes. The word itself
\ takes a live dictionary record and reads the defer trailer at its body, which a
\ blob this fixture builds cannot stand in for; what is pinned here is the guard.
: SITE-TEST-DEFER+ ( n -- ) {: site:n :}
   site ACAP-DSITE-HELD? 0= if site ACAP-ADD-DSITE then ;
;package

package AOT-FILE
public

\ Exercise the actual overlapping tail move used by MERGE. Incoming DATA rows
\ make a gap before 20,001 existing CODE rows; reserve must precede the move.
: SITE-TEST-GAP ( -- )
   LATCH-HOST
   S-DSITES ROW-OFF@ 12 S-DSITES ROW!
   S-CSITES ROW-OFF@ 0 S-CSITES ROW!
   BASES-AFTER-HOST OPEN-CSITE-GAP ;

\ The smallest aggregate budget this format can be given: a blob filling the
\ runtime code band, plus the fixed framing every artifact carries around it.
\ Spelled from AOT-FILE's own constants rather than repeated, because the point
\ is that src/arch/arm64/icode.f's AOT-SECTION-CAP has to cover exactly these.
: SITE-TEST-BAND-BYTES ( -- n )
   AOT-BUF:AOT-BLOB-CAP HDR-BYTES + SEC-N ROW-BYTES * + SCAL-BYTES + CLOSURE-CAP + ;

\ Keep total payload length and contiguous table offsets valid. Reassign some
\ blob bytes to DATA/CODE sections whose combined length is four bytes too big.
: SITE-TEST-FORGE ( AOT-OWNED:capture -- AOT-OWNED:capture )
   dup AOT-OWNED:BYTES$ drop {: dst:ptr :}
   AOT-BUF:AOT-DSITE-MAX 4 * 4 +
   S-DSITES ROW-LEN@ S-CSITES ROW-LEN@ + - {: extra:n :}
   S-BLOB ROW-OFF@ S-BLOB ROW-LEN@ extra - S-BLOB ROW!
   S-DSITES ROW-OFF@ AOT-BUF:AOT-DSITE-MAX 4 * 4 - S-DSITES ROW!
   S-CSITES ROW-OFF@ 8 S-CSITES ROW!
   SEC-N ROW-BYTES * CUR !
   SEC-N 0 ?do
      CUR @ i ROW-LEN@ i ROW!
      CUR @ i ROW-LEN@ + CUR !
   loop
   TBL dst SEC-N ROW-BYTES * BYTE-COPY ;
;package

package AOT-DATA-SITES-TEST
using AOT-BUF
using AOT-WINDOW

20001 constant ROWS
create KEY 32 allot

: DATA-OFF ( n -- n ) 16 * 16 + ;
: CODE-OFF ( n -- n ) ROWS + 16 * 8 + ;
: U32@ ( ptr u8 -- n ) {: p:ptr :}
   p c@ p 1+ c@ 8 lshift or p 2 + c@ 16 lshift or p 3 + c@ 24 lshift or ;
: U32! ( n ptr u8 -- ) {: value:n p:ptr :}
   4 0 ?do value i 8 * rshift p i + c! loop ;
: ROW@ ( n -- n ) 4 * AOT-DSITE-BUF@ + U32@ ;

: CLEAR-COUNTS ( -- )
   0 AOT-BLOB-LEN ! 0 AOT-REC-N ! 0 AOT-SITE-N ! 0 AOT-NAMES-LEN !
   0 AOT-DSITE-N ! 0 AOT-CSITE-N !
   0 AOT-CODE-B0 ! 0 AOT-DATA-D0 ! 0 AOT-DATA-SIZE !
   0 AOT-WID-W0 ! 0 AOT-WID-SPAN !
   WINDOW-RESET 0 XTOFF-N !
   0 AOT-XTSITE:N ! 0 AOT-BOOTRUN-LEN ! 0 AOT-PWIN-N !
   0 AOT-SIG-N ! 0 AOT-SIG-STR-LEN ! 0 AOT-REG-LEN ! ;

: SOURCE ( -- )
   CLEAR-COUNTS AOT-IDENT:RESET
   s" src/habu/aot-decl.f" AOT-IDENT:PATH+
   AOT-BLOB-CAP AOT-BLOB-LEN !
   $1000 AOT-DATA-D0 ! 8 AOT-DATA-SIZE !
   $D65F03C0 AOT-BLOB-BUF@ U32!
   ROWS 0 ?do
      AOT-BLOB-BUF@ i DATA-OFF + {: addr:ptr :}
      DEFER-MAGIC addr 8 - CELL-VIEW !
      $1000 addr CELL-VIEW !
      i DATA-OFF AOT-DSITE-CELL or AOT-CAPTURE:SITE-TEST-DATA+
   loop
   ROWS 0 ?do
      AOT-BLOB-BUF@ i CODE-OFF + {: addr:ptr :}
      $D2800009 addr U32!
      $F2A00009 addr 4 + U32!
      $F2C00009 addr 8 + U32!
      $F2E00009 addr 12 + U32!
      i CODE-OFF AOT-CAPTURE:SITE-TEST-CODE+
   loop ;

: CHECK ( -- )
   AOT-DSITE-N @ ROWS T= AOT-CSITE-N @ ROWS T=
   ROWS 0 ?do
      i ROW@ i DATA-OFF AOT-DSITE-CELL or T=
      ROWS i + ROW@ i CODE-OFF T=
   loop
   AOT-BLOB-BUF@ ROWS 1- DATA-OFF + CELL-VIEW @ $1000 T=
   AOT-BLOB-BUF@ ROWS 1- CODE-OFF + SNAP-RELOC:CHAINV 0 T= ;

: RELEASE ( -- ) CLEAR-COUNTS DSITE-STORAGE-RELEASE ;

\ Regrowing the runtime region past the aggregate byte budget makes AOT-BLOB-CAP
\ and the shared DATA/CODE site buffer unreachable and kills this file's own
\ shared-overflow refusal, which the budget would then pre-empt. REGION grew on
\ 2026-09-14 and AOT-SECTION-CAP did not follow; assert the inequality here, where
\ icode.f, layout.f and aot-decl.f are all loaded, so the next growth says so.
: ?BAND-BUDGET ( -- )
   s" the section budget admits a full code band" T-LABEL
   AOT-SECTION-CAP AOT-FILE:SITE-TEST-BAND-BYTES >= TTRUE ;

\ --- the window-relative DATA coordinate --------------------------------------
\ ACAP-NORMALIZE-DSITES rewrites every listed DATA site into the coordinate the
\ seed restores: stored = value - d0 + (d0 and 7), and the capture base is left
\ holding that residue alone. The restore side is EM-AOT-RELOC-DATA in
\ src/habu/habu2.f: it advances the seeded DP to the next address carrying the
\ capture base's own residue and adds one delta to every site, so
\ restored = stored + delta with delta a multiple of 8 - which is what keeps each
\ captured cell as aligned as it was captured. Both site kinds and all eight
\ residues are fed known values here, rather than re-derived from live memory as
\ test/aot-artifact-rows.f does.
$1000 constant NORM-BASE      \ 8-aligned, so d0's residue is exactly the one added
3 constant NORM-CELLS
48 constant NORM-CHAIN-OFF    \ above the cells, 4-byte aligned, 16 bytes wide
3 constant NORM-CHAIN-K       \ the chain carries d0 + 8 * this
: NORM-CELL-OFF ( n -- n ) 8 * 16 + ;

\ Cell k holds d0 + 8k (k = 0 included: a value AT the base), and the chain holds
\ d0 + 8 * NORM-CHAIN-K, so every site is a whole number of cells above the base.
\ A cell site carries AOT-DSITE-CELL, a chain site does not - the flag the DATA
\ sweep gives each kind (ACAP-SCAN-DSITES, ACAP-DEFER-SITE).
: NORM-SITES ( n -- ) {: r:n :}
   CLEAR-COUNTS
   NORM-BASE r + AOT-DATA-D0 !  64 AOT-DATA-SIZE !
   NORM-CELLS 0 ?do
      NORM-BASE r + i 8 * +  AOT-BLOB-BUF@ i NORM-CELL-OFF + CELL-VIEW !
      i NORM-CELL-OFF AOT-DSITE-CELL or AOT-CAPTURE:SITE-TEST-DATA+
   loop
   AOT-BLOB-BUF@ NORM-CHAIN-OFF + {: p:ptr :}
   $D2800009 p U32!        $F2A00009 p 4 + U32!
   $F2C00009 p 8 + U32!    $F2E00009 p 12 + U32!
   p NORM-BASE r + NORM-CHAIN-K 8 * + SNAP-RELOC:SET-CHAIN
   NORM-CHAIN-OFF AOT-CAPTURE:SITE-TEST-DATA+ ;

: NORM-SHIFTED ( n -- ) {: r:n :}
   s" cell site: stored = value - d0 + (d0 and 7)" T-LABEL
   NORM-CELLS 0 ?do
      AOT-BLOB-BUF@ i NORM-CELL-OFF + CELL-VIEW @  i 8 * r +  T=
   loop
   s" chain site: the same coordinate, re-encoded into the chain" T-LABEL
   AOT-BLOB-BUF@ NORM-CHAIN-OFF + SNAP-RELOC:CHAINV  NORM-CHAIN-K 8 * r +  T=
   s" the capture base keeps its 8-residue, so the seed's delta stays a multiple of 8" T-LABEL
   AOT-DATA-D0 @ r T= ;

: ?NORMALIZE ( -- )
   8 0 ?do  i NORM-SITES  AOT-CAPTURE:SITE-TEST-NORMALIZE  i NORM-SHIFTED  loop ;

\ A deferred word's trailer is one DATA address reached once per alias, and
\ ACAP-NORMALIZE-DSITES shifts every row it is given - so the second sighting
\ must not add a row. A row is the offset AND its kind bit, so a listed cell's
\ offset asked as the other kind is not held.
: ?HELD-DUPLICATE ( -- )
   5 NORM-SITES
   0 NORM-CELL-OFF AOT-DSITE-CELL or {: site:n :}
   s" a listed cell site is held" T-LABEL
   site AOT-CAPTURE:SITE-TEST-HELD? TTRUE
   s" an unlisted offset is not held" T-LABEL
   NORM-CELLS NORM-CELL-OFF AOT-DSITE-CELL or AOT-CAPTURE:SITE-TEST-HELD? TFALSE
   s" the kind bit belongs to the row" T-LABEL
   0 NORM-CELL-OFF AOT-CAPTURE:SITE-TEST-HELD? TFALSE
   AOT-DSITE-N @ {: rows:n :}
   site AOT-CAPTURE:SITE-TEST-DEFER+
   s" an aliased trailer is listed once" T-LABEL
   AOT-DSITE-N @ rows T=
   AOT-CAPTURE:SITE-TEST-NORMALIZE
   5 NORM-SHIFTED                   \ ... and is therefore shifted once, not twice
   RELEASE ;

: TRANSFER ( AOT-OWNED:capture -- )
   RELEASE
   dup AOT-FILE:IMPORT CHECK
   AOT-OWNED:CLOSE ;

: GAP ( -- )
   AOT-FILE:OWN
   AOT-FILE:SITE-TEST-GAP
   ROWS 0 ?do
      i ROW@ i DATA-OFF AOT-DSITE-CELL or T=
      ROWS 3 + i + ROW@ i CODE-OFF T=
   loop
   TRANSFER ;

: SPAN-VALUE ( -- n )
   1 SCRIPT-ARGV$ {: a:ptr u:n :}
   a u s" span-negative" STR= if -1 exit then
   a u s" span-min" STR= if $8000000000000000 exit then
   a u s" span-zero" STR= if 0 exit then
   a u s" span-cap" STR= if SPAN-CAP exit then
   SPAN-CAP 1+ ;

: SPAN-CASE ( -- )
   CLEAR-COUNTS AOT-IDENT:RESET
   s" src/habu/aot-decl.f" AOT-IDENT:PATH+
   SPAN-VALUE AOT-DATA-SIZE !
   2 SCRIPT-ARGV$ s" owned" STR= if
      AOT-FILE:OWN
      CLEAR-COUNTS
      dup AOT-FILE:IMPORT AOT-OWNED:CLOSE
   else
      KEY 0 SCRIPT-ARGV$ AOT-FILE:WRITE
      CLEAR-COUNTS
      2 SCRIPT-ARGV$ s" merge" STR= if
         \ As in the address-row merge fixture, one zeroed host record marks
         \ a captured host. The positive host span must not hide source -1.
         \ One cell of host span is under a whole bitmap byte, so the merge
         \ rounds it up to BM-BYTE-SPAN before it places the artifact's cells
         \ (src/habu/aot-file.f PLACE-WDATA) - which is what the merged span
         \ below is counted from.
         1 AOT-REC-N ! 8 AOT-DATA-SIZE !
         KEY 0 SCRIPT-ARGV$ AOT-FILE:MERGE
      else
         KEY 0 SCRIPT-ARGV$ AOT-FILE:READ
      then
   then
   AOT-DATA-SIZE @ SPAN-VALUE
   2 SCRIPT-ARGV$ s" merge" STR= if BM-BYTE-SPAN + then T=
   T-REPORT
   s" aot-data-sites: span ok" type cr ;

: RUN ( -- )
   T-RESET
   ?BAND-BUDGET
   1 SCRIPT-ARGV$ 5 min s" span-" STR= if SPAN-CASE exit then
   1 SCRIPT-ARGV$ s" reserve-overflow" STR= if
      $7FFFFFFFFFFFFFFF AOT-DSITE-RESERVE exit then
   1 SCRIPT-ARGV$ s" reserve-limit" STR= if
      AOT-DSITE-MAX 1+ AOT-DSITE-RESERVE exit then
   1 SCRIPT-ARGV$ s" reserve-negative" STR= if
      -1 AOT-DSITE-RESERVE exit then
   SOURCE CHECK
   1 SCRIPT-ARGV$ s" bad-order" STR= if
      0 AOT-CAPTURE:SITE-TEST-DATA+ exit then
   1 SCRIPT-ARGV$ s" shared-overflow" STR= if
      AOT-FILE:OWN AOT-FILE:SITE-TEST-FORGE AOT-FILE:IMPORT exit then
   AOT-FILE:OWN TRANSFER
   KEY 0 SCRIPT-ARGV$ AOT-FILE:WRITE
   RELEASE
   KEY 0 SCRIPT-ARGV$ AOT-FILE:READ CHECK
   GAP
   AOT-DSITE-MAX AOT-DSITE-RESERVE CHECK
   DSITE-STORAGE-RELEASE
   \ Last, because both build their own site list over the capture state the
   \ transfers above read.
   ?NORMALIZE
   ?HELD-DUPLICATE
   T-REPORT
   s" aot-data-sites: ok" type cr ;

RUN
;using
;using
;package
