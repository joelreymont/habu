\ aot-file-merge.f — capture a window, merge a real artifact onto it, and check
\ that every coordinate the merge had to move actually moved.
\
\ WHAT IT IS FOR. src/habu/aot-file.f MERGE appends a second capture to one that
\ already happened, in the first one's coordinates. The metabuild proves that by
\ booting; this proves it in a second, and it proves it where a boot cannot look -
\ at the rows themselves. Nothing here re-implements the merge: the capture is
\ AOT-CAPTURE:CAPTURE and the merge is AOT-FILE:MERGE, out of the production
\ files, and every assertion below reads the live buffers those two filled.
\
\ WHY IT CAPTURES A WINDOW OF ITS OWN FIRST. Merging onto empty buffers would set
\ every shift to zero, and a shift of zero is indistinguishable from a shift that
\ was never applied - the one bug this file exists to catch. So the window below
\ is built to carry a nonzero population of EVERY axis the merge moves: code and
\ records and names, a call site, a DATA literal, a code literal, a declared
\ address cell, a wordlist of its own, a protected wordlist, and a boot-run entry.
\ ?HOST asserts each of those is really nonzero, so the day one of them stops
\ being produced this file fails instead of quietly testing nothing.
\
\ THE ONE AXIS WITH NO POPULATION is the named code site: the compiler chain's
\ capture records none (xtsites=0) and neither does this window, so the merge's
\ shift of those two fields is carried by the code and by no measurement here.
\
\ HOW IT KNOWS A SHIFT WAS THE RIGHT SIZE. A range is not enough and this file
\ was built with ranges first, which is how that was found out: the host window is
\ small beside a compiler chain, so "inside the merged blob" is nearly the same
\ band as "inside the artifact's blob" and four of the eight shifts could be
\ deleted with every range still satisfied (measured). So the artifact is READ
\ ONCE ON ITS OWN first and each row family's SUM is kept; the capture then wipes
\ the buffers, the merge fills them again, and the merged sum has to be the
\ artifact's plus the row count times the shift. That pins every shift to its
\ exact quantity over every row, and no mutation of one survives it.
\
\ Run:  bin/hb --load test/aot-file-merge.f -- <artifact>
\ Prints `aot-file-merge: merged` and a census, exits 0, or refuses by name.

package AFM
public
ndict@ here
variable PRE-R  variable PRE-D
PRE-D !  PRE-R !
;package

require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/habu/layout.f
require src/habu/aot-decl.f
require src/habu/aot-arm.f
require src/habu/aot-capture.f
require src/habu/aot-ident.f
require src/habu/fdio.f
require src/habu/aot-file.f
require lib/engine-id.f

package AFM
using AOT-BUF
public

$4A constant REFUSE-RC

variable B0  variable B1  variable R0  variable R1
variable D0  variable D1  variable W0  variable W1

\ What the host held before the merge, read back afterwards to say what moved.
variable H-BLOB   variable H-REC    variable H-SITE  variable H-NAMES
variable H-DSITE  variable H-CSITE  variable H-XTOFF variable H-DATA
variable H-BOOT   variable H-PWIN   variable H-SPAN
variable BAD

\ The artifact on its own: its counts, its two window bases, and one sum per row
\ family. Every one of these is read through AOT-FILE:READ before the host window
\ is captured over the top of it.
variable A-REC    variable A-SITE   variable A-DSITE variable A-CSITE
variable A-XTOFF  variable A-PWIN   variable A-D0    variable A-W0
variable A-ORD                       \ ordinary (non-package) records
variable A-WIDN                      \ record wid fields that are not the global 0
variable A-RBLOB  variable A-RNAME  variable A-RWID
variable A-SBLOB  variable A-SNAME
variable A-DROW   variable A-CROW   variable A-XROW  variable A-PROW
variable A-DVAL   variable A-CVAL
variable SUM      variable SUM2     variable CNT

: DIE ( ptr u8 n -- ) REFUSE-RC die ;

\ This file's own reader of a packed u32 row field, owner-prefixed like every
\ other reader of that shape in the tree (tools/jitdump-core.f says why).
: W32@ ( ptr u8 -- n ) {: p:ptr :}
   p c@  p 1+ c@ 8 lshift or  p 2 + c@ 16 lshift or  p 3 + c@ 24 lshift or ;

: OPEN ( -- )
   cp@ B0 !  ndict@ R0 !  here D0 !  AOT-ARM:WIDN W0 !
   B0 @ D0 @ AOT-ARM:OPEN ;

: CLOSE ( -- )
   cp@ B1 !  ndict@ R1 !  here D1 !  AOT-ARM:WIDN W1 ! ;

;package

AFM:OPEN

\ ---- the window: one live producer of every axis the merge moves -------------
\ A package of its own, so the capture has a wordlist to rebase; protected, so it
\ has one to carry in the window's own seal table.

package AFMW
get-current prot-wid-add
public

create BUF 32 allot                  \ DATA content, and the address a literal holds
defer HOOK ( -- )                    \ a declared address cell

\ Long enough that the engine's compile-mode inliner emits a call rather than
\ copying the body: the call site is the point.
: CALLEE ( n -- n ) {: v:n :}
   v 1 +  v 2 * +  v 3 * +  v 5 * +  v 7 * +  v 11 * +  v 13 * +
   v 17 * +  v 19 * +  v 23 * +  v 29 * +  v 31 * + ;

: USER ( n -- n ) CALLEE CALLEE ;

: TOUCH ( -- ) 7 BUF c! ;            \ a DATA literal: BUF's address in this window

: NONE ( -- ) ;

: INSTALL ( -- ) [: NONE ;] is HOOK ;   \ a code literal: the quotation's entry

;package

AFM:CLOSE

package AFM
using AOT-BUF
public

create KEY 32 allot

: ?ARGS ( -- )
   SCRIPT-ARGC 0 > if exit then
   s" aot-file-merge: usage: --load test/aot-file-merge.f -- <artifact>" DIE ;

: KEY! ( -- )
   ENGINE-ID:PATH$ KEY SHA256-FILE 0 <> if
      s" aot-file-merge: cannot hash the engine that is running" DIE
   then ;

: CAPTURE-HOST ( -- )
   PRE-R @ PRE-D @ AOT-CAPTURE:PRELUDE-MARK
   W0 @ W1 @ AOT-CAPTURE:WID-SPAN
   B0 @ B1 @  R0 @ R1 @  D0 @ D1 @  AOT-CAPTURE:CAPTURE
   s" AFMW:INSTALL" AOT-CAPTURE:BOOTRUN+ ;

: LATCH ( -- )
   AOT-BLOB-LEN @ H-BLOB !          AOT-REC-N @ H-REC !
   AOT-SITE-N @ H-SITE !            AOT-NAMES-LEN @ H-NAMES !
   AOT-DSITE-N @ H-DSITE !          AOT-CSITE-N @ H-CSITE !
   AOT-WINDOW:XTOFF-N @ H-XTOFF !   AOT-DATA-SIZE @ H-DATA !
   AOT-BOOTRUN-LEN @ H-BOOT !       AOT-PWIN-N @ H-PWIN !
   AOT-WID-SPAN @ H-SPAN ! ;

: ?NONZERO ( n ptr u8 n -- ) {: v:n a:ptr u:n :}
   v 0 > if exit then
   s" aot-file-merge: the window produced no " type a u type cr
   s" aot-file-merge: the host window lost an axis this file exists to shift" DIE ;

\ Every axis the merge moves has a nonzero host population, or this file is
\ testing a shift of zero and would pass with the shift deleted.
: ?HOST ( -- )
   H-BLOB @ s" code" ?NONZERO           H-REC @ s" records" ?NONZERO
   H-SITE @ s" call sites" ?NONZERO     H-NAMES @ s" names" ?NONZERO
   H-DSITE @ s" DATA sites" ?NONZERO    H-CSITE @ s" CODE sites" ?NONZERO
   H-XTOFF @ s" address cells" ?NONZERO H-DATA @ s" DATA bytes" ?NONZERO
   H-BOOT @ s" boot-run names" ?NONZERO H-PWIN @ s" protected window WIDs" ?NONZERO
   H-SPAN @ s" wordlists" ?NONZERO ;

\ ---- what every merged coordinate has to satisfy -----------------------------
\ Each check is a RANGE over a live population, so a shift that is dropped, or
\ applied with the wrong quantity, lands a row outside its band and is named.

: ?IN ( n n n ptr u8 n -- ) {: v:n lo:n hi:n a:ptr u:n :}
   v lo >= v hi < and if exit then
   s" aot-file-merge: " type a u type
   s"  is " type v .
   s" outside [" type lo .  s" ," type hi .  s" )" type cr
   1 BAD +! ;

: CREC ( n -- ptr u8 ) AOT-CREC-ROW * AOT-REC-MAX 48 * + AOT-REC-BUF@ swap + ;

\ ---- one sum per row family, taken twice -------------------------------------

: SUM-ROWS ( ptr u8 n n n -- n ) {: p:ptr n:n row:n off:n :}
   0 SUM !
   n 0 ?do  SUM @ p i row * + off + W32@ +  SUM !  loop
   SUM @ ;

\ The values the chains at those blob offsets hold, which is the other half of a
\ site: the row says where the literal is and this says what it came to.
: SUM-CHAINS ( ptr u8 n -- n ) {: p:ptr n:n :}
   0 SUM2 !
   n 0 ?do
      SUM2 @  AOT-BLOB-BUF@ p i 4 * + W32@ +  SNAP-RELOC:CHAINV  +  SUM2 !
   loop
   SUM2 @ ;

variable T-RBLOB  variable T-RNAME  variable T-RWID
variable T-ORD    variable T-WIDN

: WID+ ( n -- ) {: w:n :}
   w 0= if exit then
   T-RWID @ w + T-RWID !  T-WIDN @ 1+ T-WIDN ! ;

\ Records need their own walk: a package record's [0] and [4] are wordlist ids
\ where an ordinary one's are a blob offset and a code length, so the two do not
\ feed the same sum. Counting the ordinary ones is part of the answer - the blob
\ shift applies once per ordinary record and not once per record.
: WALK-RECS ( n n -- ) {: base:n n:n :}
   0 T-RBLOB !  0 T-RNAME !  0 T-RWID !  0 T-ORD !  0 T-WIDN !
   n 0 ?do
      base i + CREC {: c:ptr :}
      T-RNAME @ c 8 + W32@ + T-RNAME !
      c 16 + W32@ {: w:n :}
      w $FFFFFFFF = if
         c W32@ WID+  c 4 + W32@ WID+
      else
         T-RBLOB @ c W32@ + T-RBLOB !
         T-ORD @ 1+ T-ORD !
         w WID+
      then
   loop ;

\ The artifact read on its own, before the host window is captured over the top
\ of it. AOT-CAPTURE:CAPTURE resets every buffer, so nothing of this survives
\ except the numbers.
: SNAP-ARTIFACT ( -- )
   KEY 0 SCRIPT-ARGV$ AOT-FILE:READ
   AOT-REC-N @ A-REC !                 AOT-SITE-N @ A-SITE !
   AOT-DSITE-N @ A-DSITE !             AOT-CSITE-N @ A-CSITE !
   AOT-WINDOW:XTOFF-N @ A-XTOFF !      AOT-PWIN-N @ A-PWIN !
   AOT-DATA-D0 @ A-D0 !                AOT-WID-W0 @ A-W0 !
   0 AOT-REC-N @ WALK-RECS
   T-RBLOB @ A-RBLOB !  T-RNAME @ A-RNAME !  T-RWID @ A-RWID !
   T-ORD @ A-ORD !      T-WIDN @ A-WIDN !
   AOT-SITE-BUF@ AOT-SITE-N @ 8 0 SUM-ROWS A-SBLOB !
   AOT-SITE-BUF@ AOT-SITE-N @ 8 4 SUM-ROWS A-SNAME !
   AOT-DSITE-BUF@ AOT-DSITE-N @ 4 0 SUM-ROWS A-DROW !
   AOT-DSITE-BUF@ AOT-DSITE-N @ 4 * + AOT-CSITE-N @ 4 0 SUM-ROWS A-CROW !
   AOT-WINDOW:XTOFF-BUF@ AOT-WINDOW:XTOFF-N @ 4 0 SUM-ROWS A-XROW !
   AOT-PWIN-BUF@ AOT-PWIN-N @ 4 0 SUM-ROWS A-PROW !
   AOT-DSITE-BUF@ AOT-DSITE-N @ SUM-CHAINS A-DVAL !
   AOT-DSITE-BUF@ AOT-DSITE-N @ 4 * + AOT-CSITE-N @ SUM-CHAINS A-CVAL ! ;

: MERGED-WID? ( n -- ) {: w:n :}
   w 0= if exit then
   w  AOT-WID-W0 @ H-SPAN @ +  AOT-WID-W0 @ AOT-WID-SPAN @ +  s" a merged record wid" ?IN ;

: HOST-WID? ( n -- ) {: w:n :}
   w 0= if exit then
   w  AOT-WID-W0 @  AOT-WID-W0 @ H-SPAN @ +  s" a host record wid" ?IN ;

\ The host's own rows must be where they were: a merge that shifted everything
\ instead of only what it read would still put the chain in range.
: ?HOST-RECS ( -- )
   H-REC @ 0 ?do
      i CREC {: c:ptr :}
      c 8 + W32@ 0 H-NAMES @ s" a host record name offset" ?IN
      c 16 + W32@ {: w:n :}
      w $FFFFFFFF = if
         c W32@ HOST-WID?  c 4 + W32@ HOST-WID?
      else
         c W32@ 0 H-BLOB @ s" a host record blob offset" ?IN
         w HOST-WID?
      then
   loop ;

: ?MERGED-RECS ( -- )
   AOT-REC-N @ H-REC @ ?do
      i CREC {: c:ptr :}
      c 8 + W32@ H-NAMES @ AOT-NAMES-LEN @ s" a merged record name offset" ?IN
      c 16 + W32@ {: w:n :}
      w $FFFFFFFF = if
         c W32@ MERGED-WID?  c 4 + W32@ MERGED-WID?
      else
         c W32@ H-BLOB @ AOT-BLOB-LEN @ s" a merged record blob offset" ?IN
         w MERGED-WID?
      then
   loop ;

: ?SITES ( -- )
   H-SITE @ 0 ?do
      AOT-SITE-BUF@ i 8 * + {: r:ptr :}
      r W32@ 0 H-BLOB @ s" a host call site blob offset" ?IN
      r 4 + W32@ 0 H-NAMES @ s" a host call site name offset" ?IN
   loop
   AOT-SITE-N @ H-SITE @ ?do
      AOT-SITE-BUF@ i 8 * + {: r:ptr :}
      r W32@ H-BLOB @ AOT-BLOB-LEN @ s" a merged call site blob offset" ?IN
      r 4 + W32@ H-NAMES @ AOT-NAMES-LEN @ s" a merged call site name offset" ?IN
   loop ;

\ The DATA half: the row moved with the blob, and the chain it points at now
\ holds an address inside the merged window's own continuation of the host's.
: ?DSITES ( -- )
   AOT-DSITE-N @ H-DSITE @ ?do
      AOT-DSITE-BUF@ i 4 * + W32@ {: boff:n :}
      boff H-BLOB @ AOT-BLOB-LEN @ s" a merged DATA site blob offset" ?IN
      AOT-BLOB-BUF@ boff + SNAP-RELOC:CHAINV
      AOT-DATA-D0 @ H-DATA @ +
      AOT-DATA-D0 @ AOT-DATA-SIZE @ +
      s" a merged DATA literal" ?IN
   loop
   H-DSITE @ 0 ?do
      AOT-DSITE-BUF@ i 4 * + W32@ 0 H-BLOB @ s" a host DATA site blob offset" ?IN
   loop ;

\ The CODE half: the same two questions, against the blob rather than the window.
: ?CSITES ( -- )
   AOT-CSITE-N @ H-CSITE @ ?do
      AOT-DSITE-BUF@ AOT-DSITE-N @ i + 4 * + W32@ {: boff:n :}
      boff H-BLOB @ AOT-BLOB-LEN @ s" a merged CODE site blob offset" ?IN
      AOT-BLOB-BUF@ boff + SNAP-RELOC:CHAINV
      H-BLOB @ AOT-BLOB-LEN @ s" a merged CODE literal" ?IN
   loop
   H-CSITE @ 0 ?do
      AOT-DSITE-BUF@ AOT-DSITE-N @ i + 4 * + W32@
      0 H-BLOB @ s" a host CODE site blob offset" ?IN
   loop ;

: ?XTOFFS ( -- )
   AOT-WINDOW:XTOFF-N @ H-XTOFF @ ?do
      AOT-WINDOW:XTOFF-BUF@ i 4 * + W32@
      H-DATA @ AOT-DATA-SIZE @ s" a merged address cell offset" ?IN
   loop
   H-XTOFF @ 0 ?do
      AOT-WINDOW:XTOFF-BUF@ i 4 * + W32@ 0 H-DATA @ s" a host address cell offset" ?IN
   loop ;

: ?PWIN ( -- )
   AOT-PWIN-N @ H-PWIN @ ?do
      AOT-PWIN-BUF@ i 4 * + W32@
      H-SPAN @ AOT-WID-SPAN @ s" a merged protected window WID" ?IN
   loop
   H-PWIN @ 0 ?do
      AOT-PWIN-BUF@ i 4 * + W32@ 0 H-SPAN @ s" a host protected window WID" ?IN
   loop ;

\ ---- and the same sums after the merge, each shifted by exactly one quantity --

: ?SUM ( n n ptr u8 n -- ) {: got:n want:n a:ptr u:n :}
   got want = if exit then
   s" aot-file-merge: " type a u type
   s"  sum is " type got .
   s" where the artifact's plus its own shift is " type want . cr
   1 BAD +! ;

: ?EXACT ( -- )
   AOT-REC-N @ H-REC @ - A-REC @ = 0= if
      s" aot-file-merge: the merge did not add the artifact's records" DIE
   then
   H-REC @ A-REC @ WALK-RECS
   T-ORD @ A-ORD @ = 0= if
      s" aot-file-merge: a merged record changed between package and ordinary" DIE
   then
   T-RBLOB @  A-RBLOB @ A-ORD @ H-BLOB @ * +
      s" merged record blob offset" ?SUM
   T-RNAME @  A-RNAME @ A-REC @ H-NAMES @ * +
      s" merged record name offset" ?SUM
   T-RWID @   A-RWID @ A-WIDN @ AOT-WID-W0 @ H-SPAN @ + A-W0 @ - * +
      s" merged record wid" ?SUM
   AOT-SITE-BUF@ H-SITE @ 8 * + A-SITE @ 8 0 SUM-ROWS
      A-SBLOB @ A-SITE @ H-BLOB @ * +
      s" merged call site blob offset" ?SUM
   AOT-SITE-BUF@ H-SITE @ 8 * + A-SITE @ 8 4 SUM-ROWS
      A-SNAME @ A-SITE @ H-NAMES @ * +
      s" merged call site name offset" ?SUM
   AOT-DSITE-BUF@ H-DSITE @ 4 * + A-DSITE @ 4 0 SUM-ROWS
      A-DROW @ A-DSITE @ H-BLOB @ * +
      s" merged DATA site row" ?SUM
   AOT-DSITE-BUF@ AOT-DSITE-N @ H-CSITE @ + 4 * + A-CSITE @ 4 0 SUM-ROWS
      A-CROW @ A-CSITE @ H-BLOB @ * +
      s" merged CODE site row" ?SUM
   AOT-WINDOW:XTOFF-BUF@ H-XTOFF @ 4 * + A-XTOFF @ 4 0 SUM-ROWS
      A-XROW @ A-XTOFF @ H-DATA @ * +
      s" merged address cell offset" ?SUM
   AOT-PWIN-BUF@ H-PWIN @ 4 * + A-PWIN @ 4 0 SUM-ROWS
      A-PROW @ A-PWIN @ H-SPAN @ * +
      s" merged protected window WID" ?SUM
   AOT-DSITE-BUF@ H-DSITE @ 4 * + A-DSITE @ SUM-CHAINS
      A-DVAL @ A-DSITE @ AOT-DATA-D0 @ H-DATA @ + A-D0 @ - * +
      s" merged DATA literal" ?SUM
   AOT-DSITE-BUF@ AOT-DSITE-N @ H-CSITE @ + 4 * + A-CSITE @ SUM-CHAINS
      A-CVAL @ A-CSITE @ H-BLOB @ * +
      s" merged CODE literal" ?SUM ;

\ The host's list is still in front, and the merged one is still terminated.
: ?BOOTRUN ( -- )
   AOT-BOOTRUN-LEN @ H-BOOT @ >= 0= if
      s" aot-file-merge: the boot-run list got shorter" DIE
   then
   AOT-BOOTRUN-BUF@ AOT-BOOTRUN-LEN @ + c@ 0= if exit then
   s" aot-file-merge: the merged boot-run list is not terminated" DIE ;

\ The three coordinates the merge must NOT move, because the artifact was just
\ expressed in them.
: ?KEPT ( -- )
   AOT-CODE-B0 @ 0= 0= if s" aot-file-merge: the code base moved" DIE then
   AOT-DATA-SIZE @ H-DATA @ > 0= if s" aot-file-merge: the DATA window did not grow" DIE then
   AOT-WID-SPAN @ H-SPAN @ > 0= if s" aot-file-merge: the wordlist window did not grow" DIE then ;

: ?BAD ( -- )
   BAD @ 0= if exit then
   s" aot-file-merge: out-of-range coordinates=" type BAD @ . cr
   s" aot-file-merge: the merge left a coordinate outside its own window" DIE ;

: CENSUS. ( -- )
   s" aot-file-merge: merged" type cr
   s" hostrecs=" type H-REC @ .
   s" mergedrecs=" type AOT-REC-N @ .
   s" hostsites=" type H-SITE @ .
   s" mergedsites=" type AOT-SITE-N @ .
   s" hostblob=" type H-BLOB @ .
   s" mergedblob=" type AOT-BLOB-LEN @ . cr
   s" hostnames=" type H-NAMES @ .
   s" mergednames=" type AOT-NAMES-LEN @ .
   s" hostdsites=" type H-DSITE @ .
   s" mergeddsites=" type AOT-DSITE-N @ .
   s" hostcsites=" type H-CSITE @ .
   s" mergedcsites=" type AOT-CSITE-N @ . cr
   s" hostdata=" type H-DATA @ .
   s" mergeddatasz=" type AOT-DATA-SIZE @ .
   s" hostxtoff=" type H-XTOFF @ .
   s" mergedxtoff=" type AOT-WINDOW:XTOFF-N @ .
   s" hostspan=" type H-SPAN @ .
   s" mergedwidspan=" type AOT-WID-SPAN @ .
   s" hostpwin=" type H-PWIN @ .
   s" mergedpwin=" type AOT-PWIN-N @ . cr ;

: MAIN ( -- )
   ?ARGS
   KEY!
   SNAP-ARTIFACT
   CAPTURE-HOST
   LATCH
   ?HOST
   KEY 0 SCRIPT-ARGV$ AOT-FILE:MERGE
   ?KEPT
   ?HOST-RECS  ?MERGED-RECS
   ?SITES  ?DSITES  ?CSITES  ?XTOFFS  ?PWIN
   ?EXACT
   ?BOOTRUN
   ?BAD
   CENSUS. ;

;package

AFM:MAIN
