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
\
\ TWO ENTRY POINTS OVER ONE MACHINERY. READ fills the buffers with the artifact,
\ bases and all, and is what the round trip and the fixpoint compare. MERGE
\ APPENDS the artifact to a capture that already happened, in that capture's
\ coordinates, because the engine bakes ONE of everything; the section that says
\ what they disagree about is at the merge itself.

package AOT-FILE
using AOT-BUF

$00544F4155424148 constant MAGIC     \ "HABUAOT\0" in LE byte order, readable in a dump
3 constant VERSION   \ 3 widened a call-site row with the callee's SCOPE
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

\ The packed u32 every row field of this format is. Owner-prefixed by its
\ package like every other reader of that shape in the tree (AOT-W32@,
\ ACAP-W32@, SNAP-RELOC's instruction word; see tools/jitdump-core.f).
: U32@ ( ptr u8 -- n ) {: p:ptr :}
   p c@  p 1+ c@ 8 lshift or  p 2 + c@ 16 lshift or  p 3 + c@ 24 lshift or ;

: U32! ( n ptr u8 -- ) {: v:n p:ptr :}
   v p c!  v 8 rshift p 1+ c!  v 16 rshift p 2 + c!  v 24 rshift p 3 + c! ;

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

\ ---- the section table, read off the live buffers ---------------------------
\ Both directions ask these two words, so a section's source and its size cannot
\ come apart. The two assembled sections (the scalars and the closure list) are
\ staged into buffers of their own first, which is what keeps the pair uniform.
\ SEC-PTR answers a BUFFER, never a position inside one: where a section's bytes
\ sit in that buffer is the BASE table below, which is the one thing READ and
\ MERGE disagree about.

: SEC-PTR ( n -- ptr u8 ) {: k:n :}
   k S-SCALARS = if SCAL exit then
   k S-BLOB    = if AOT-BLOB-BUF@ exit then
   k S-RECS    = if AOT-REC-BUF@ AOT-REC-MAX 48 * + exit then
   k S-SITES   = if AOT-SITE-BUF@ exit then
   k S-NAMES   = if AOT-NAMES-BUF@ exit then
   k S-DSITES  = if AOT-DSITE-BUF@ exit then
   k S-CSITES  = if AOT-DSITE-BUF@ exit then
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
   k S-SITES   = if AOT-SITE-N @ SITE-ROW * exit then
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
   k S-SITES   = if SITE-ROW exit then
   k S-DSITES  = if 4 exit then
   k S-CSITES  = if 4 exit then
   k S-XTOFFS  = if 4 exit then
   k S-XTSITES = if 8 exit then
   k S-PWIN    = if 4 exit then
   1 ;

\ How much of each buffer there is. The CODE-literal sites share the DATA-site
\ buffer, so both name the whole of it and their BASEs are what keeps them apart
\ - which is also what makes the room check `base + length` rather than length.
: SEC-CAP ( n -- n ) {: k:n :}
   k S-SCALARS = if SCAL-BYTES exit then
   k S-BLOB    = if AOT-BLOB-CAP exit then
   k S-RECS    = if AOT-REC-MAX AOT-CREC-ROW * exit then
   k S-SITES   = if AOT-SITE-MAX SITE-ROW * exit then
   k S-NAMES   = if AOT-NAMES-CAP exit then
   k S-DSITES  = if AOT-DSITE-MAX 4 * exit then
   k S-CSITES  = if AOT-DSITE-MAX 4 * exit then
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

\ ---- where each section's bytes sit in its buffer ----------------------------
\ THE ONE THING READ AND MERGE DISAGREE ABOUT. Every other question - which
\ buffer, how long, how wide a row, what it is called - has one answer for both,
\ so the table below is the whole of the parameterization and the loading,
\ digesting and room-checking machinery is shared.

create BASE SEC-N cells allot
: BASE@ ( n -- n ) cells BASE + @ ;
: BASE! ( n n -- ) cells BASE + ! ;

\ The artifact IS the buffers: every section at its buffer's own start, except
\ the CODE sites, which live behind the DATA sites in the buffer they share.
\ THE TABLE IS THE ONE AUTHORITY FOR THAT OFFSET, in both directions. The writer
\ derives the table from the live buffers before it needs a position, so asking
\ the table here answers for a write; the reader has the table before it fills
\ anything, so the same question answers for a read against buffers still
\ holding the last capture. Reading the live count variables instead would have
\ made the reader's CODE-site position depend on whatever AOT-DSITE-N held.
: BASES-ALONE ( -- )
   SEC-N 0 ?do 0 i BASE! loop
   S-DSITES ROW-LEN@ S-CSITES BASE! ;

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
      i SEC-LEN 0 > if i SEC-PTR i BASE@ + i SEC-LEN SHA256-UPDATE then
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
   SEC-N 0 ?do i SEC-PTR i BASE@ + i SEC-LEN PUT loop
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
   BASES-ALONE
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
   k BASE@ k ROW-LEN@ + k SEC-CAP <= if exit then
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
   k SEC-PTR k BASE@ + k ROW-LEN@ GET
   k SEC-PTR k BASE@ + k ROW-LEN@ SHA256-UPDATE ;

\ Read a section past without keeping it. The bytes still feed the running
\ digest, so a section this build has no use for is still one the second pass
\ proves it read. CHUNK-BUF is free here: the verify pass that owns it has
\ finished, and a section too big for it is refused rather than partly read.
: SKIP-SECTION ( n -- ) {: k:n :}
   k ROW-LEN@ 0= if exit then
   k ROW-LEN@ CHUNK > if
      FD @ close
      s" aot-file: section " type k SEC-NAME type s"  is too large to read past" DIE
   then
   CHUNK-BUF k ROW-LEN@ GET
   CHUNK-BUF k ROW-LEN@ SHA256-UPDATE ;

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
   S-SITES ROW-LEN@ SITE-ROW / AOT-SITE-N !
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

\ Everything both entry points do before a single byte lands anywhere: verify the
\ header, verify the payload against its own digest, reopen, prove the header did
\ not move, and take the section table. What follows differs only in where the
\ sections go and what is done to them once they are there.
: LOAD-PASS ( ptr u8 ptr u8 n -- ) {: want:ptr path:ptr pathu:n :}
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
   S-PWID PROT-BITS-BYTES ?EXACT ;

public

\ Fill the live capture buffers from `path`, refusing anything this build must not
\ bake. `want` is the 32-byte producer key this reader accepts and nothing else.
\ The artifact REPLACES what the buffers held: it brings its own DATA base, its
\ own code base and its own wordlist window, and the round trip lives here.
: READ ( ptr u8 ptr u8 n -- )
   LOAD-PASS
   BASES-ALONE
   SEC-N 0 ?do i LOAD-SECTION loop
   FD @ close
   ?PAYLOAD-AGAIN
   RESTORE-COUNTS
   RESTORE-CLOSURE
   ?CHAIN ;

private

\ ---- merging an artifact into a capture that already happened ----------------
\
\ WHY MERGE IS NOT READ WITH DIFFERENT NUMBERS. The metabuild host captures its
\ own window first and then takes the chain's from an artifact, and habu2.f
\ EMIT-AOT-SEED bakes ONE blob, ONE name pool, ONE (DATA base, size) pair and ONE
\ wordlist window. So the second capture has to be expressed in the first's
\ coordinates: every offset it carries moves by what the host already holds, and
\ every VALUE it carries - a DATA address in its own window, a code address in
\ its own blob, a wordlist id in its own window - has to be recomputed for the
\ merged one. Loading into zeroed buffers is not a degenerate case of this: a
\ host DATA base of 0 would bake a DATA base of 0.
\
\ THE SIX THINGS THAT MOVE, and nothing else does:
\   blob offsets    ordinary records' [0], call sites' [0], named code sites'
\                   [0], and both site tables' own u32 rows: + the host's blob.
\   pool offsets    records' [8], call sites' [4], named code sites' [4]: + the
\                   host's pool, which is appended to and never deduplicated
\                   against (the remap would buy under a kilobyte).
\   window offsets  the declared address cells: + the host's DATA span.
\   DATA values     each recorded chain holds an address in the ARTIFACT's DATA
\                   window; the merged window continues the host's, so it moves
\                   to the same place inside that.
\   CODE values     each holds an offset into the artifact's own blob, which is
\                   now that far into the merged blob.
\   wordlist ids    records' wids and the window's own protected ids are window
\                   coordinates, so they continue the host's window rather than
\                   restarting it, and the merged span is the two added.
\ The host's PRE-window protected bitmap is NOT one of them: the artifact's is
\ the capture engine's own band, numbered in a wordlist space this host does not
\ share, and the host's band already covers what this engine protects. It is read
\ past, so the payload digest still proves it arrived.

variable H-BLOB   variable H-REC     variable H-SITE   variable H-NAMES
variable H-DSITE  variable H-CSITE   variable H-XTOFF  variable H-DATA
variable H-XTSITE variable H-BOOTRUN variable H-PWIN   variable H-SPAN
variable A-D0     variable A-B0      variable A-W0     variable A-SPAN

: LATCH-HOST ( -- )
   AOT-BLOB-LEN @ H-BLOB !            AOT-REC-N @ H-REC !
   AOT-SITE-N @ H-SITE !              AOT-NAMES-LEN @ H-NAMES !
   AOT-DSITE-N @ H-DSITE !            AOT-CSITE-N @ H-CSITE !
   AOT-WINDOW:XTOFF-N @ H-XTOFF !     AOT-DATA-SIZE @ H-DATA !
   AOT-XTSITE:N @ H-XTSITE !          AOT-BOOTRUN-LEN @ H-BOOTRUN !
   AOT-PWIN-N @ H-PWIN !              AOT-WID-SPAN @ H-SPAN ! ;

\ A merge appends, so there has to be something to append to. A host that has not
\ captured yet would take every shift below as zero and bake the artifact alone -
\ an engine with the chain and without the REPL, which boots and is wrong.
: ?HOST-CAPTURED ( -- )
   AOT-REC-N @ 0 > if exit then
   s" aot-file: nothing has been captured for this artifact to be merged into" DIE ;

\ Each section behind what the host already holds. The CODE sites are the one
\ base that is not the host's own count: they sit behind the host's DATA rows,
\ the artifact's DATA rows AND the host's CODE rows, because the two tables share
\ one buffer with every DATA row ahead of every CODE row.
: BASES-AFTER-HOST ( -- )
   SEC-N 0 ?do 0 i BASE! loop
   H-BLOB @                    S-BLOB BASE!
   H-REC @ AOT-CREC-ROW *      S-RECS BASE!
   H-SITE @ SITE-ROW *     S-SITES BASE!
   H-NAMES @                   S-NAMES BASE!
   H-DSITE @ 4 *               S-DSITES BASE!
   H-DSITE @ 4 * S-DSITES ROW-LEN@ + H-CSITE @ 4 * +   S-CSITES BASE!
   H-XTOFF @ 4 *               S-XTOFFS BASE!
   H-DATA @                    S-WDATA BASE!
   H-XTSITE @ 8 *              S-XTSITES BASE!
   H-BOOTRUN @                 S-BOOTRUN BASE!
   H-PWIN @ 4 *                S-PWIN BASE! ;

\ The host's CODE rows move up by the artifact's DATA rows before anything is
\ read into the gap they leave. Copied from the top down, because the source and
\ the destination overlap.
: OPEN-CSITE-GAP ( -- )
   H-DSITE @ S-DSITES ROW-LEN@ 4 / + H-CSITE @ + S-CSITES ROW-LEN@ 4 / +
   AOT-DSITE-MAX > if
      FD @ close
      s" aot-file: the merged DATA and CODE site tables do not fit one buffer" DIE
   then
   S-DSITES ROW-LEN@ {: d:n :}
   d 0= if exit then
   H-CSITE @ 0 ?do
      H-CSITE @ 1 - i - {: k:n :}
      AOT-DSITE-BUF@ H-DSITE @ k + 4 * +  U32@
      AOT-DSITE-BUF@ H-DSITE @ 4 * + d + k 4 * +  U32!
   loop ;

: SEC-AT ( n -- ptr u8 ) {: k:n :} k SEC-PTR k BASE@ + ;
: SEC-ROWS ( n -- n ) {: k:n :} k ROW-LEN@ k SEC-ROW / ;

: SCALARS@ ( -- )
   SCAL U64@ A-D0 !
   SCAL 8 + U64@ A-B0 !
   SCAL 16 + U64@ A-W0 !
   SCAL 24 + U64@ A-SPAN ! ;

\ Both captures canonicalize their code literals against base 0 (aot-capture.f
\ ACAP-SCAN-CSITES), which is what lets a merge move one into the other's blob by
\ shifting an offset. A capture that did not is a shape this cannot rebase.
: ?BASES ( -- )
   AOT-CODE-B0 @ 0= A-B0 @ 0= and if exit then
   s" aot-file: a code base is not the canonical zero this merge shifts against" DIE ;

\ Raise one u32 field of every row: `n` rows of `row` bytes from `p`, the field
\ at byte `off` in each.
: FIELD+ ( ptr u8 n n n n -- ) {: p:ptr n:n row:n off:n d:n :}
   n 0 ?do
      p i row * + off + {: q:ptr :}
      q U32@ d +  q U32!
   loop ;

: NAME. ( n -- ) {: noff:n :}
   AOT-NAMES-BUF@ noff 1+ +  AOT-NAMES-BUF@ noff + c@  type ;

\ A captured wid is a window coordinate: 0 is the global wordlist and means the
\ same thing in every process, and an in-window id continues the host's window
\ instead of restarting it. An id the artifact's own window did not create has no
\ counterpart here, so it is refused by name rather than rebased into whatever
\ this host keeps at that number.
: REWID ( n n -- n ) {: noff:n w:n :}
   w 0= if 0 exit then
   w A-W0 @ >= w A-W0 @ A-SPAN @ + < and if
      AOT-WID-W0 @ H-SPAN @ + w + A-W0 @ - exit
   then
   s" aot-file: merged record " type noff NAME.
   s"  names wordlist " type w .
   s" , which its own window did not create; that window allocated [" type
   A-W0 @ .  s" ," type A-W0 @ A-SPAN @ + .  s" )" type cr
   s" aot-file: a merged record names a wid its window did not create" DIE
   0 ;

\ A record's name offset always moves with the pool. Its [0] and [4] are a blob
\ offset and a code length for an ordinary word, and a package's PUBLIC and
\ PRIVATE wordlist ids for a package record - which take the wid rebase and never
\ the blob shift.
: MERGE-RECS ( -- )
   S-RECS SEC-AT {: c0:ptr :}
   S-RECS SEC-ROWS 0 ?do
      c0 i AOT-CREC-ROW * + {: c:ptr :}
      c 8 + U32@ H-NAMES @ +  c 8 + U32!
      c 8 + U32@ {: noff:n :}
      c 16 + U32@ {: w:n :}
      w $FFFFFFFF = if
         noff c U32@ REWID      c U32!
         noff c 4 + U32@ REWID  c 4 + U32!
      else
         c U32@ H-BLOB @ +      c U32!
         noff w REWID           c 16 + U32!
      then
   loop ;

\ A call site's third field is a SCOPE, and only one of its four kinds moves: a
\ window coordinate continues the host's window, exactly as a record's wid does.
\ A layout constant and the QUALIFIED marker are the same number in every engine
\ and in every merge, so REWID's own pass-through is what leaves them alone.
: SCOPE-FIXED? ( n -- bool ) {: w:n :}      \ the same number in every engine, so no merge moves it
   w WID-QUAL = if 0 0= exit then
   w FIRST-DYNAMIC-WID < ;

: MERGE-SITE-SCOPE ( -- )
   S-SITES SEC-AT {: p:ptr :}
   S-SITES SEC-ROWS 0 ?do
      p i SITE-ROW * + {: r:ptr :}
      r 8 + U32@ {: w:n :}
      w SCOPE-FIXED? 0= if  r 4 + U32@ w REWID  r 8 + U32!  then
   loop ;

: MERGE-ROWS ( -- )
   S-SITES SEC-AT    S-SITES SEC-ROWS    SITE-ROW 0 H-BLOB @ FIELD+
   S-SITES SEC-AT    S-SITES SEC-ROWS    SITE-ROW 4 H-NAMES @ FIELD+
   MERGE-SITE-SCOPE
   S-XTSITES SEC-AT  S-XTSITES SEC-ROWS  8 0 H-BLOB @ FIELD+
   S-XTSITES SEC-AT  S-XTSITES SEC-ROWS  8 4 H-NAMES @ FIELD+
   S-XTOFFS SEC-AT   S-XTOFFS SEC-ROWS   4 0 H-DATA @ FIELD+
   S-PWIN SEC-AT     S-PWIN SEC-ROWS     4 0 H-SPAN @ FIELD+ ;

: ?DVALUE ( n n -- ) {: boff:n v:n :}
   s" aot-file: the chain at merged blob offset " type boff H-BLOB @ + .
   s" holds " type v .
   s" which its own window's DATA span does not contain" type cr
   s" aot-file: a merged DATA literal is outside the artifact's DATA window" DIE ;

\ Each recorded chain holds an address in the ARTIFACT's DATA window; the merged
\ window continues the host's, so the address moves to the same place inside it.
\ The site's own blob offset moves with the blob, and it is read before it moves
\ because it is what locates the chain.
: MERGE-DSITES ( -- )
   S-DSITES SEC-AT {: p:ptr :}
   S-WDATA ROW-LEN@ {: dlen:n :}
   S-DSITES SEC-ROWS 0 ?do
      p i 4 * + {: q:ptr :}
      q U32@ {: boff:n :}
      AOT-BLOB-BUF@ H-BLOB @ + boff + {: ch:ptr :}
      ch SNAP-RELOC:CHAINV {: v:n :}
      v A-D0 @ >= v A-D0 @ dlen + < and 0= if boff v ?DVALUE then
      ch  v A-D0 @ -  AOT-DATA-D0 @ +  H-DATA @ +  SNAP-RELOC:SET-CHAIN
      boff H-BLOB @ +  q U32!
   loop ;

: ?CVALUE ( n n -- ) {: boff:n v:n :}
   s" aot-file: the chain at merged blob offset " type boff H-BLOB @ + .
   s" holds code offset " type v .
   s" which its own blob does not contain" type cr
   s" aot-file: a merged CODE literal is outside the artifact's blob" DIE ;

\ Each holds an offset into the artifact's own blob, canonicalized against code
\ base 0, and that blob now starts that far into the merged one.
: MERGE-CSITES ( -- )
   S-CSITES SEC-AT {: p:ptr :}
   S-BLOB ROW-LEN@ {: blen:n :}
   S-CSITES SEC-ROWS 0 ?do
      p i 4 * + {: q:ptr :}
      q U32@ {: boff:n :}
      AOT-BLOB-BUF@ H-BLOB @ + boff + {: ch:ptr :}
      ch SNAP-RELOC:CHAINV {: v:n :}
      v blen < 0= if boff v ?CVALUE then
      ch  v H-BLOB @ +  SNAP-RELOC:SET-CHAIN
      boff H-BLOB @ +  q U32!
   loop ;

\ Every count is the host's plus the length the table gave, divided by the row
\ the format fixes - the same one authority per number the plain read uses. The
\ two bases and the window's own base do not move: they are the host's, and the
\ artifact was just expressed in them.
: MERGE-COUNTS ( -- )
   H-BLOB @ S-BLOB ROW-LEN@ + AOT-BLOB-LEN !
   H-REC @ S-RECS SEC-ROWS + AOT-REC-N !
   H-SITE @ S-SITES SEC-ROWS + AOT-SITE-N !
   H-NAMES @ S-NAMES ROW-LEN@ + AOT-NAMES-LEN !
   H-DSITE @ S-DSITES SEC-ROWS + AOT-DSITE-N !
   H-CSITE @ S-CSITES SEC-ROWS + AOT-CSITE-N !
   H-XTOFF @ S-XTOFFS SEC-ROWS + AOT-WINDOW:XTOFF-N !
   H-DATA @ S-WDATA ROW-LEN@ + AOT-DATA-SIZE !
   H-XTSITE @ S-XTSITES SEC-ROWS + AOT-XTSITE:N !
   H-BOOTRUN @ S-BOOTRUN ROW-LEN@ + AOT-BOOTRUN-LEN !
   H-PWIN @ S-PWIN SEC-ROWS + AOT-PWIN-N !
   H-SPAN @ A-SPAN @ + AOT-WID-SPAN !
   0 AOT-BOOTRUN-BUF@ AOT-BOOTRUN-LEN @ + c! ;   \ the live terminator, uncounted

public

\ Append the artifact at `path` to the capture already in the live buffers,
\ expressed in that capture's coordinates. `want` is the 32-byte producer key
\ this reader accepts and nothing else. Every refusal READ can raise, this one
\ raises too - it is the same header, payload and table machinery - plus the ones
\ a second capture can fail on its own.
: MERGE ( ptr u8 ptr u8 n -- )
   ?HOST-CAPTURED
   LATCH-HOST
   LOAD-PASS
   BASES-AFTER-HOST
   OPEN-CSITE-GAP
   SEC-N 0 ?do
      i S-PWID = if i SKIP-SECTION else i LOAD-SECTION then
   loop
   FD @ close
   ?PAYLOAD-AGAIN
   SCALARS@
   ?BASES
   MERGE-RECS
   MERGE-ROWS
   MERGE-DSITES
   MERGE-CSITES
   MERGE-COUNTS
   RESTORE-CLOSURE
   ?CHAIN ;

;package
