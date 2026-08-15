\ aot-file.f — the AOT capture artifact: what a capture writes, and what a
\ metabuild refuses to read.
\
\ WHY AN ARTIFACT EXISTS AT ALL. A capture has to run inside a booted bin/hb,
\ because the metabuild host's dictionary is not the target's and a host-captured
\ name does not exist in the engine being built (three ordered deaths measured, on
\ src/habu/aot-capture.f's header). The metabuild is a different process, so the
\ capture has to cross to it as bytes. This file is those bytes, and the reader is
\ the only thing standing between a stale or corrupt file and a baked engine.
\
\ ONE FILE, TWO PROCESSES, the way src/habu/aot-decl.f serves two fillers: the
\ booted capture writes and the metabuild host reads, so both compile this source
\ and neither can hold a private idea of the layout. That is also why it uses
\ ENGINE PRIMITIVES ONLY (open/read/write/close, PATH0, SHA256*, the AOT-BUF
\ buffers, FDIO:WALL) - the host carries no lib/, and the capture cannot load
\ src/habu/driver-io.f at all.
\
\ IDENTITY IS NOT THE FORMAT'S. WRITE takes the producer key and READ takes the
\ key it will accept; neither asks where the caller got it. A capture running in a
\ booted engine reads lib/engine-id.f's content key for the binary it is running;
\ the metabuild recomputes SHA-256 over the capture host it just emitted. One
\ fact, two independent readings, compared for hard equality here.
\
\ THE THREE DIGESTS, and what each one catches that the others do not:
\   PRODUCER  - sha256 of the engine binary that produced the capture. Refuses an
\               artifact some other engine made. A header field, so it is checked
\               before a single payload byte is trusted.
\   CHAIN     - sha256 over the ordered bytes of the closure the capture compiled.
\               The artifact carries the FILE LIST and the reader RE-DERIVES the
\               digest from disk rather than trusting the stored one, so a match
\               means "this came from the chain that is on disk NOW". A mismatch
\               means the sources moved since capture; a recapture is the cure.
\   PAYLOAD   - sha256 over everything after the header, verified BEFORE any
\               section is parsed. The header's size arithmetic only catches
\               truncation; this is what lets the reader say "corrupt" instead of
\               "the sizes add up".
\
\ THE LAYOUT. All scalars are u64 little-endian and nothing is padded.
\
\   header, 136 bytes, and the only part not covered by the payload digest:
\     0    MAGIC          u64   "HABUAOT\0" read as LE bytes
\     8    VERSION        u64   hard equality; the format is baked into an engine
\     16   TARGET         u64   hard equality against the reading process
\     24   SECTION-COUNT  u64   hard equality; names a wrong table before it is read
\     32   PAYLOAD-LEN    u64
\     40   PRODUCER-SHA   32B
\     72   CHAIN-SHA      32B
\     104  PAYLOAD-SHA    32B
\
\   payload, offsets relative to its own start so the header is not in the sums:
\     0                    section table, SECTION-COUNT rows of (offset u64,
\                          length u64), in the fixed order below
\     table-end onwards    the sections themselves, contiguous, in that order
\
\ THE SECTION ORDER IS habu2.f EMIT-AOT-SEED'S ORDER, because that is the word
\ that consumes every one of them, and a format that lists them in some other
\ order invites a reader that fills the wrong buffer.
\
\ COUNTS ARE NOT STORED. AOT-REC-N is the record section's length divided by
\ AOT-CREC-ROW, the site count is its length over 8, AOT-DATA-SIZE is the window
\ DATA section's length, and so on. A stored count would be a second authority for
\ a number the length already fixes, and two authorities disagree eventually. A
\ length that is not a whole number of rows is refused by name instead. Four
\ numbers are genuine scalars - the capture-time DATA base, the canonical code
\ base, and the window's wordlist base and span - and they travel in a section of
\ their own.
\
\ WHAT THE READER REFUSES, each by name and each fail-closed: a wrong magic, a
\ wrong version, a wrong target, a wrong section count, a producer that is not the
\ engine this build made, a file whose length disagrees with its header, a payload
\ whose digest disagrees, a section table that is not contiguous or steps outside
\ the payload, a section longer than the buffer it fills, a section length that is
\ not a whole number of rows, a closure list that does not walk to its own end, and
\ a chain digest that does not re-derive from the files on disk.

package AOT-FILE
using AOT-BUF

$00544F4155424148 constant MAGIC     \ "HABUAOT\0" in LE byte order, readable in a dump
2 constant VERSION   \ 2 added the window's wordlist span and its protected WIDs
1 constant TARGET-MACOS
2 constant TARGET-LINUX

136 constant HDR-BYTES
0 constant O-MAGIC
8 constant O-VERSION
16 constant O-TARGET
24 constant O-SECTIONS
32 constant O-PAYLEN
40 constant O-PRODUCER
72 constant O-CHAIN
104 constant O-PAYSHA
32 constant SHA-BYTES

14 constant SEC-N
16 constant ROW-BYTES                \ one section-table row: offset u64 + length u64

0 constant S-SCALARS
1 constant S-BLOB
2 constant S-RECS
3 constant S-SITES
4 constant S-NAMES
5 constant S-DSITES
6 constant S-CSITES
7 constant S-XTOFFS
8 constant S-WDATA
9 constant S-XTSITES
10 constant S-BOOTRUN
11 constant S-PWID
12 constant S-PWIN
13 constant S-CLOSURE

\ The four genuine scalars: the capture-time DATA base, the canonical code base,
\ and the window's wordlist base and span. Everything else is a length.
32 constant SCAL-BYTES
\ AOT-IDENT holds at most 256 paths of at most 256 bytes, so the list cannot
\ exceed 8 + 256 * (8 + 256) = 67592 bytes. The cap is the next round number above
\ it and the overflow is refused rather than truncated.
$20000 constant CLOSURE-CAP
$10000 constant CHUNK                \ read granularity for the verify pass

$4B constant REFUSE-RC

create HDR HDR-BYTES allot           \ the header as written, or as read in pass 1
create HDR2 HDR-BYTES allot          \ ... and as re-read in pass 2, compared byte for byte
create TBL SEC-N ROW-BYTES * allot
create SCAL SCAL-BYTES allot
create CBUF CLOSURE-CAP allot        \ the closure list, assembled or read back
create CHUNK-BUF CHUNK allot
create PAYSHA SHA-BYTES allot        \ the payload digest this process computed
create FILESHA SHA-BYTES allot       \ sha256 of the whole file WRITE just wrote
create DERIVED SHA-BYTES allot       \ the chain digest re-derived from disk

variable CLEN                        \ the closure section's length: staged on the way
                                     \ out, taken from the table on the way in
variable FD
variable RD
variable GOT                         \ GET's own cursor: it runs inside loops that use CUR
variable LEFT
variable PAYLEN
variable CUR

: DIE ( ptr u8 n -- ) REFUSE-RC die ;

: U64! ( n ptr u8 -- ) {: v:n p:ptr :}
   8 0 ?do  v i 8 * rshift $FF and  p i + c!  loop ;

\ Most significant byte first into the accumulator, so the shift is the running
\ value's and not the byte's.
: U64@ ( ptr u8 -- n ) {: p:ptr :}
   0 8 0 ?do  8 lshift  p 7 i - + c@ or  loop ;

\ Counted rather than short-circuited, and that is not a style choice: this file
\ compiles in the stdin metabuild host, where src/habu/hide.f has already retired
\ `true` and `false` (measured - the host build died E-UNDEFINED: false). Every
\ other host-side AOT source is written the same way, without a boolean literal.
: BYTES= ( ptr u8 ptr u8 n -- bool ) {: a:ptr b:ptr n:n :}
   0  n 0 ?do  a i + c@ b i + c@ <> if 1+ then  loop  0= ;

: TARGET-ID ( -- n )
   HB-TARGET-MACOS? if TARGET-MACOS exit then
   HB-TARGET-LINUX? if TARGET-LINUX exit then
   s" aot-file: unknown target" DIE  0 ;

: ROW-OFF@ ( n -- n ) ROW-BYTES * TBL + U64@ ;
: ROW-LEN@ ( n -- n ) ROW-BYTES * TBL + 8 + U64@ ;
: ROW! ( n n n -- ) {: off:n len:n k:n :}
   off  k ROW-BYTES * TBL +      U64!
   len  k ROW-BYTES * TBL + 8 +  U64! ;

\ THE TABLE IS THE ONE AUTHORITY FOR EVERY LENGTH, in both directions. The writer
\ derives it from the live buffers before it needs a pointer, so asking the table
\ here answers for a write; the reader has it before it fills anything, so the
\ same question answers for a read against buffers still holding the last
\ capture. Reading the live count variables instead would have made the reader's
\ CODE-site pointer depend on whatever AOT-DSITE-N happened to hold.
: DSITE-BYTES ( -- n ) S-DSITES ROW-LEN@ ;

\ ---- the section table, read off the live buffers ---------------------------
\ Both directions ask these two words, so a section's source and its size cannot
\ come apart. The two assembled sections (the scalars and the closure list) are
\ staged into buffers of their own first, which is what keeps the pair uniform.

: SEC-PTR ( n -- ptr u8 ) {: k:n :}
   k S-SCALARS = if SCAL exit then
   k S-BLOB    = if AOT-BLOB-BUF@ exit then
   k S-RECS    = if AOT-REC-BUF@ AOT-REC-MAX 48 * + exit then
   k S-SITES   = if AOT-SITE-BUF@ exit then
   k S-NAMES   = if AOT-NAMES-BUF@ exit then
   k S-DSITES  = if AOT-DSITE-BUF@ exit then
   k S-CSITES  = if AOT-DSITE-BUF@ DSITE-BYTES + exit then
   k S-XTOFFS  = if AOT-WINDOW:XTOFF-BUF@ exit then
   k S-WDATA   = if AOT-WINDOW:DATA-BUF@ exit then
   k S-XTSITES = if AOT-XTSITE:BUF@ exit then
   k S-BOOTRUN = if AOT-BOOTRUN-BUF@ exit then
   k S-PWID    = if AOT-PWID-BUF@ exit then
   k S-PWIN    = if AOT-PWIN-BUF@ exit then
   CBUF ;

: SEC-LEN ( n -- n ) {: k:n :}
   k S-SCALARS = if SCAL-BYTES exit then
   k S-BLOB    = if AOT-BLOB-LEN @ exit then
   k S-RECS    = if AOT-REC-N @ AOT-CREC-ROW * exit then
   k S-SITES   = if AOT-SITE-N @ 8 * exit then
   k S-NAMES   = if AOT-NAMES-LEN @ exit then
   k S-DSITES  = if AOT-DSITE-N @ 4 * exit then
   k S-CSITES  = if AOT-CSITE-N @ 4 * exit then
   k S-XTOFFS  = if AOT-WINDOW:XTOFF-N @ 4 * exit then
   k S-WDATA   = if AOT-DATA-SIZE @ exit then
   k S-XTSITES = if AOT-XTSITE:N @ 8 * exit then
   k S-BOOTRUN = if AOT-BOOTRUN-LEN @ exit then
   k S-PWID    = if PROT-BITS-BYTES exit then
   k S-PWIN    = if AOT-PWIN-N @ 4 * exit then
   CLEN @ ;

\ How many bytes one row of a section is, so a length that is not a whole number
\ of them is refused rather than silently rounded. 1 means the section is bytes.
: SEC-ROW ( n -- n ) {: k:n :}
   k S-SCALARS = if 8 exit then
   k S-RECS    = if AOT-CREC-ROW exit then
   k S-SITES   = if 8 exit then
   k S-DSITES  = if 4 exit then
   k S-CSITES  = if 4 exit then
   k S-XTOFFS  = if 4 exit then
   k S-XTSITES = if 8 exit then
   k S-PWIN    = if 4 exit then
   1 ;

\ The buffer each section is read back into, and how much of it there is. The
\ CODE-literal sites share the DATA-site buffer's tail, so their room is what the
\ DATA sites left - which is why the table's order puts the DATA sites first and
\ the reader fills them in that order.
: SEC-CAP ( n -- n ) {: k:n :}
   k S-SCALARS = if SCAL-BYTES exit then
   k S-BLOB    = if AOT-BLOB-CAP exit then
   k S-RECS    = if AOT-REC-MAX AOT-CREC-ROW * exit then
   k S-SITES   = if AOT-SITE-MAX 8 * exit then
   k S-NAMES   = if AOT-NAMES-CAP exit then
   k S-DSITES  = if AOT-DSITE-MAX 4 * exit then
   k S-CSITES  = if AOT-DSITE-MAX 4 * DSITE-BYTES - exit then
   k S-XTOFFS  = if AOT-WINDOW:XTOFF-MAX 4 * exit then
   k S-WDATA   = if AOT-WINDOW:DATA-CAP exit then
   k S-XTSITES = if AOT-XTSITE:MAX 8 * exit then
   k S-BOOTRUN = if AOT-BOOTRUN-CAP exit then
   k S-PWID    = if PROT-BITS-BYTES exit then
   k S-PWIN    = if AOT-PWIN-MAX 4 * exit then
   CLOSURE-CAP ;

: SEC-NAME ( n -- ptr u8 n ) {: k:n :}
   k S-SCALARS = if s" scalars" exit then
   k S-BLOB    = if s" blob" exit then
   k S-RECS    = if s" records" exit then
   k S-SITES   = if s" call sites" exit then
   k S-NAMES   = if s" name pool" exit then
   k S-DSITES  = if s" DATA sites" exit then
   k S-CSITES  = if s" CODE sites" exit then
   k S-XTOFFS  = if s" address cells" exit then
   k S-WDATA   = if s" window DATA" exit then
   k S-XTSITES = if s" named code sites" exit then
   k S-BOOTRUN = if s" boot-run list" exit then
   k S-PWID    = if s" protected-WID bitmap" exit then
   k S-PWIN    = if s" protected window WIDs" exit then
   s" closure list" ;

\ ---- staging the two assembled sections -------------------------------------

: STAGE-SCALARS ( -- )
   AOT-DATA-D0 @ SCAL U64!
   AOT-CODE-B0 @ SCAL 8 + U64!
   AOT-WID-W0 @ SCAL 16 + U64!
   AOT-WID-SPAN @ SCAL 24 + U64! ;

: CB! ( n n -- ) {: v:n at:n :} v CBUF at + U64! ;

: STAGE-CLOSURE ( -- )
   AOT-IDENT:COUNT CBUF U64!
   8 CUR !
   AOT-IDENT:COUNT 0 ?do
      i AOT-IDENT:PATH$ {: pa:ptr pu:n :}
      CUR @ 8 + pu + CLOSURE-CAP > if
         s" aot-file: closure list exceeds the artifact's list buffer" DIE
      then
      pu CUR @ CB!
      pa  CBUF CUR @ 8 + +  pu BYTE-COPY
      CUR @ 8 + pu + CUR !
   loop
   CUR @ CLEN ! ;

: STAGE ( -- ) STAGE-SCALARS STAGE-CLOSURE ;

\ ---- the payload's shape, computed from the live buffers ---------------------

: BUILD-TABLE ( -- )
   SEC-N ROW-BYTES * CUR !
   SEC-N 0 ?do
      CUR @  i SEC-LEN  i ROW!
      CUR @ i SEC-LEN + CUR !
   loop
   CUR @ PAYLEN ! ;

\ The payload digest, over exactly the bytes the write is about to hand over and
\ in exactly that order: the table, then every section.
: PAYLOAD-DIGEST ( -- )
   SHA256-RESET
   TBL SEC-N ROW-BYTES * SHA256-UPDATE
   SEC-N 0 ?do
      i SEC-LEN 0 > if i SEC-PTR i SEC-LEN SHA256-UPDATE then
   loop
   PAYSHA SHA256-FINAL ;

: BUILD-HEADER ( ptr u8 -- ) {: prod:ptr :}
   HDR-BYTES 0 ?do 0 HDR i + c! loop
   MAGIC      HDR O-MAGIC +    U64!
   VERSION    HDR O-VERSION +  U64!
   TARGET-ID  HDR O-TARGET +   U64!
   SEC-N      HDR O-SECTIONS + U64!
   PAYLEN @   HDR O-PAYLEN +   U64!
   prod  HDR O-PRODUCER +  SHA-BYTES BYTE-COPY
   DERIVED  HDR O-CHAIN +  SHA-BYTES BYTE-COPY
   PAYSHA  HDR O-PAYSHA +  SHA-BYTES BYTE-COPY ;

\ ---- writing ----------------------------------------------------------------
\ Every span goes out through FDIO:WALL, which loops until the descriptor has
\ taken all of it, and the same spans feed a running SHA-256 so the caller learns
\ the digest of the file that was actually written - which is what the round-trip
\ acceptance compares and what the fixpoint loop compares between generations.

: PUT ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= if exit then
   a u SHA256-UPDATE
   FD @ a u FDIO:WALL ;

: WRITE-BODY ( -- )
   SHA256-RESET
   HDR HDR-BYTES PUT
   TBL SEC-N ROW-BYTES * PUT
   SEC-N 0 ?do i SEC-PTR i SEC-LEN PUT loop
   FILESHA SHA256-FINAL ;

public

\ The digest of the artifact WRITE last wrote, header and payload together.
: SHA$ ( -- ptr u8 n ) FILESHA SHA-BYTES ;

\ Serialize the live capture buffers, the closure AOT-IDENT holds, and the three
\ digests to `path`. `prod` is the 32-byte content key of the engine that produced
\ the capture; this file does not ask where it came from.
: WRITE ( ptr u8 ptr u8 n -- ) {: prod:ptr path:ptr pathu:n :}
   STAGE
   BUILD-TABLE
   PAYLOAD-DIGEST
   DERIVED AOT-IDENT:CHAIN-DIGEST
   prod BUILD-HEADER
   path pathu PATH0 1537 493 open FD !
   FD @ 0 < if s" aot-file: cannot open the artifact for writing" DIE then
   WRITE-BODY
   FD @ close ;

private

\ ---- reading ----------------------------------------------------------------

: OPEN-RD ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu PATH0 0 0 open FD !
   FD @ 0 < if s" aot-file: cannot open the artifact" DIE then ;

\ Exactly n bytes into dst, however many reads that takes. Short is the kernel's
\ right; short at end of file is a truncated artifact and is refused.
: GET ( ptr u8 n -- ) {: dst:ptr n:n :}
   0 GOT !
   begin GOT @ n < while
      FD @ dst GOT @ + n GOT @ - read RD !
      RD @ 0 <= if
         FD @ close
         s" aot-file: the artifact ends before its header says it should" DIE
      then
      GOT @ RD @ + GOT !
   repeat ;

: ?HDR-FIELD ( n n ptr u8 n -- ) {: got:n want:n na:ptr nu:n :}
   got want = if exit then
   s" aot-file: " type na nu type s" =" type got .
   s" aot-file: expected " type want . cr
   s" aot-file: the artifact is not one this engine can read" DIE ;

: CHECK-HEADER ( ptr u8 -- ) {: want:ptr :}
   HDR O-MAGIC + U64@     MAGIC     s" magic" ?HDR-FIELD
   HDR O-VERSION + U64@   VERSION   s" version" ?HDR-FIELD
   HDR O-TARGET + U64@    TARGET-ID s" target" ?HDR-FIELD
   HDR O-SECTIONS + U64@  SEC-N     s" section count" ?HDR-FIELD
   HDR O-PRODUCER + want SHA-BYTES BYTES= 0= if
      FD @ close
      s" aot-file: the artifact was produced by a different engine" DIE
   then
   HDR O-PAYLEN + U64@ PAYLEN !
   PAYLEN @ SEC-N ROW-BYTES * < if
      FD @ close
      s" aot-file: the payload is shorter than its own section table" DIE
   then ;

\ Pass one: stream the payload through SHA-256 without touching a destination
\ buffer, and refuse before any section is parsed. A file with bytes left over
\ after the payload is refused too - the header's length is the whole story.
: VERIFY-PAYLOAD ( -- )
   SHA256-RESET
   PAYLEN @ LEFT !
   begin LEFT @ 0 > while
      LEFT @ CHUNK < if LEFT @ else CHUNK then {: want:n :}
      CHUNK-BUF want GET
      CHUNK-BUF want SHA256-UPDATE
      LEFT @ want - LEFT !
   repeat
   PAYSHA SHA256-FINAL
   FD @ CHUNK-BUF 1 read RD !
   RD @ 0 > if
      FD @ close
      s" aot-file: the artifact carries bytes past the end of its payload" DIE
   then
   PAYSHA HDR O-PAYSHA + SHA-BYTES BYTES= 0= if
      FD @ close
      s" aot-file: the artifact's payload does not match its payload digest" DIE
   then ;

\ Pass two re-reads the header and requires it byte for byte. The file changing
\ between the two passes is then a refusal rather than an assumption, and the
\ running digest below closes the same hole over the payload.
: ?SAME-HEADER ( -- )
   HDR HDR2 HDR-BYTES BYTES= if exit then
   FD @ close
   s" aot-file: the artifact changed while it was being read" DIE ;

: ?TABLE ( -- )
   SEC-N ROW-BYTES * CUR !
   SEC-N 0 ?do
      i ROW-OFF@ CUR @ <> if
         FD @ close
         s" aot-file: section " type i SEC-NAME type
         s"  does not start where the table says the last one ended" DIE
      then
      i ROW-LEN@ {: len:n :}
      len 0 < if
         FD @ close
         s" aot-file: section " type i SEC-NAME type s"  has a negative length" DIE
      then
      len i SEC-ROW mod 0 <> if
         FD @ close
         s" aot-file: section " type i SEC-NAME type
         s"  is not a whole number of rows" DIE
      then
      CUR @ len + CUR !
      CUR @ PAYLEN @ > if
         FD @ close
         s" aot-file: section " type i SEC-NAME type s"  runs past the payload" DIE
      then
   loop
   CUR @ PAYLEN @ <> if
      FD @ close
      s" aot-file: the sections do not fill the payload" DIE
   then ;

: ?ROOM ( n -- ) {: k:n :}
   k ROW-LEN@ k SEC-CAP <= if exit then
   FD @ close
   s" aot-file: section " type k SEC-NAME type s"  is larger than the buffer it fills" DIE ;

\ The two fixed-width sections say their own size, so a short one is a different
\ shape rather than a smaller one and cannot be filled in part.
: ?EXACT ( n n -- ) {: k:n want:n :}
   k ROW-LEN@ want = if exit then
   FD @ close
   s" aot-file: section " type k SEC-NAME type s"  is not its fixed width" DIE ;

: LOAD-SECTION ( n -- ) {: k:n :}
   k ?ROOM
   k ROW-LEN@ 0= if exit then
   k SEC-PTR k ROW-LEN@ GET
   k SEC-PTR k ROW-LEN@ SHA256-UPDATE ;

\ Every count is the length the table gave, divided by the row the format fixes.
\ Reading them back in the same order the writer derived them from is what makes
\ the round trip an identity rather than a resemblance.
: RESTORE-COUNTS ( -- )
   SCAL U64@ AOT-DATA-D0 !
   SCAL 8 + U64@ AOT-CODE-B0 !
   SCAL 16 + U64@ AOT-WID-W0 !
   SCAL 24 + U64@ AOT-WID-SPAN !
   S-BLOB ROW-LEN@ AOT-BLOB-LEN !
   S-RECS ROW-LEN@ AOT-CREC-ROW / AOT-REC-N !
   S-SITES ROW-LEN@ 8 / AOT-SITE-N !
   S-NAMES ROW-LEN@ AOT-NAMES-LEN !
   S-DSITES ROW-LEN@ 4 / AOT-DSITE-N !
   S-CSITES ROW-LEN@ 4 / AOT-CSITE-N !
   S-XTOFFS ROW-LEN@ 4 / AOT-WINDOW:XTOFF-N !
   S-WDATA ROW-LEN@ AOT-DATA-SIZE !
   S-XTSITES ROW-LEN@ 8 / AOT-XTSITE:N !
   S-BOOTRUN ROW-LEN@ AOT-BOOTRUN-LEN !
   S-PWIN ROW-LEN@ 4 / AOT-PWIN-N !
   0 AOT-BOOTRUN-BUF@ AOT-BOOTRUN-LEN @ + c! ;   \ the live terminator, uncounted

\ The list walks to its own end or the artifact is refused: a count that promises
\ more entries than the bytes hold, or bytes left after the last entry, are both a
\ list this reader will not guess at.
: RESTORE-CLOSURE ( -- )
   AOT-IDENT:RESET
   CBUF U64@ {: n:n :}
   8 CUR !
   n 0 ?do
      CUR @ 8 + CLEN @ > if
         FD @ close
         s" aot-file: the closure list ends inside an entry" DIE
      then
      CBUF CUR @ + U64@ {: pu:n :}
      CUR @ 8 + pu + CLEN @ > if
         FD @ close
         s" aot-file: the closure list ends inside a path" DIE
      then
      CBUF CUR @ 8 + +  pu  AOT-IDENT:PATH+
      CUR @ 8 + pu + CUR !
   loop
   CUR @ CLEN @ <> if
      FD @ close
      s" aot-file: the closure list does not fill its section" DIE
   then ;

\ The stored chain digest is never trusted: it is recomputed from the files the
\ artifact names, as they are on disk right now. A mismatch means the chain moved
\ since the capture, and the cure is a recapture, not a weaker check.
: ?CHAIN ( -- )
   DERIVED AOT-IDENT:CHAIN-DIGEST
   DERIVED HDR O-CHAIN + SHA-BYTES BYTES= if exit then
   s" aot-file: the chain sources have changed since this capture" DIE ;

\ The sibling of the header byte-identity check, over the payload: pass two
\ accumulates what it actually read into the buffers and it has to come to the
\ digest pass one verified. That closes two holes at once - the file changing
\ between the passes, and a reader that filled less than it claimed the table
\ said. Skipping one section reds here (measured).
: ?PAYLOAD-AGAIN ( -- )
   PAYSHA SHA256-FINAL
   PAYSHA HDR O-PAYSHA + SHA-BYTES BYTES= if exit then
   s" aot-file: the second pass did not read the payload the first pass verified" DIE ;

public

\ Fill the live capture buffers from `path`, refusing anything this build must not
\ bake. `want` is the 32-byte producer key this reader accepts and nothing else.
: READ ( ptr u8 ptr u8 n -- ) {: want:ptr path:ptr pathu:n :}
   path pathu OPEN-RD
   HDR HDR-BYTES GET
   want CHECK-HEADER
   VERIFY-PAYLOAD
   FD @ close
   path pathu OPEN-RD
   HDR2 HDR-BYTES GET
   ?SAME-HEADER
   SHA256-RESET
   TBL SEC-N ROW-BYTES * GET
   TBL SEC-N ROW-BYTES * SHA256-UPDATE
   ?TABLE
   S-CLOSURE ROW-LEN@ CLEN !
   S-SCALARS SCAL-BYTES ?EXACT
   S-PWID PROT-BITS-BYTES ?EXACT
   SEC-N 0 ?do i LOAD-SECTION loop
   FD @ close
   ?PAYLOAD-AGAIN
   RESTORE-COUNTS
   RESTORE-CLOSURE
   ?CHAIN ;

;package
