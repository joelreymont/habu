\ content-key.f - manifest-hashed content cache keys.
\
\ Requires SHA256 words; native bin/hb already carries src/core/sha256.f.
\ Needs SORT:SORT! for the path-ordered lookup index and RENAME-FILE for the
\ atomic compacting writer, so both modules are pulled in explicitly.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/sort.f
require lib/fs-mutate.f

$40000 constant CK-CAP
$100000 constant CK-CACHE-CAP
FS-PATH-CAP 160 + constant CK-ROW-CAP
64 constant CK-HEX-LEN
$54 constant CK-TEXT-TAG
$46 constant CK-FILE-TAG
$44 constant CK-DIGEST-TAG
$41 constant CK-HEX-UP-A
$47 constant CK-HEX-UP-G
$61 constant CK-HEX-LOW-A
$67 constant CK-HEX-LOW-G
$2E constant CK-DOT
2 constant CK-STDERR-FD

\ A cache row is never shorter than one path byte, five tab-separated decimal
\ metadata fields (>=1 digit each), a tab, the 64-hex digest, and a newline.
\ CK-CACHE-MAX-ROWS bounds the offset index from the buffer capacity: no row can
\ be smaller, so the loaded rows can never exceed this count.
77 constant CK-MIN-ROW
CK-CACHE-CAP CK-MIN-ROW / constant CK-CACHE-MAX-ROWS

create CK-BUF CK-CAP allot
create CK-DG 40 allot
create CK-FILE-DG 40 allot
create CK-FILE-HEX 80 allot
create CK-CACHE-PATH-BUF FS-PATH-CAP allot
create CK-ROW-BUF CK-ROW-CAP allot
create CK-CACHE-TMP-BUF FS-PATH-CAP allot

variable CK-U
variable CK-CACHE-BUF-A
variable CK-CACHE-OUT-A
variable CK-IDX-BASE
variable CK-CACHE-U
variable CK-CACHE-OUT-U
variable CK-CACHE-PATH-U
variable CK-CACHE-TMP-U
variable CK-ROW-U
variable CK-PREFIX-U
variable CK-CACHE-LOADED
variable CK-CACHE-DIRTY
variable CK-CACHE-DISABLED
variable CK-IDX-N
variable CK-IDX-END
variable CK-KEEP-N
variable CK-LO
variable CK-HI
variable CK-MID
variable CK-FIND-I
variable CK-DUP-I
variable CK-EVICT-TOTAL
variable CK-EMIT-OFF
variable CK-EMIT-LEN
variable CK-TMP-TRY

: CK-TRUE ( -- bool )
   0 0= ;

: CK-FALSE ( -- bool )
   CK-TRUE 0= ;

: CK-RESET ( -- )
   0 CK-U ! ;

: CK-CAP-CHECK ( n -- ) {: n:n :}
   n 0 < if E-STR-BOUNDS throw then
   CK-U @ n + CK-CAP > if E-STR-CAPACITY throw then ;

: CK-U8+ ( n -- ) {: c:n :}
   1 CK-CAP-CHECK
   c 0 < if E-STR-BOUNDS throw then
   c STR-BYTE-MAX > if E-STR-BOUNDS throw then
   c CK-BUF CK-U @ + c!
   CK-U @ 1+ CK-U ! ;

: CK-BYTES+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u CK-CAP-CHECK
   a CK-BUF CK-U @ + u BYTE-COPY
   CK-U @ u + CK-U ! ;

: CK-FRAG+ ( n ptr u8 n -- ) {: tag:n a:ptr u:n :}
   u 0 < if E-STR-BOUNDS throw then
   u STR-BYTE-MAX > if E-STR-BOUNDS throw then
   tag CK-U8+
   u CK-U8+
   a u CK-BYTES+ ;

: CK-TEXT+ ( ptr u8 n -- )
   CK-TEXT-TAG -rot CK-FRAG+ ;

: CK-DIGEST+ ( ptr u8 -- )
   CK-DIGEST-TAG CK-U8+
   32 CK-U8+
   32 CK-BYTES+ ;

\ One plain-English line to stderr; the visible-degradation channel the cache
\ uses instead of silently disabling itself.
: CK-STDERR-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   CK-STDERR-FD a u write drop
   CK-STDERR-FD S\" \n" write drop ;

: CK-CACHE-CLEAR! ( -- )
   0 CK-CACHE-PATH-U !
   0 CK-CACHE-LOADED !
   0 CK-CACHE-DIRTY !
   0 CK-CACHE-DISABLED ! ;

: CK-CACHE-BUF-FIELD ( -- ptr ptr u8 )
   CK-CACHE-BUF-A 0 ptr-field ;

: CK-CACHE-BUF@ ( -- ptr u8 )
   CK-CACHE-BUF-FIELD @ ;

: CK-CACHE-BUF! ( ptr u8 -- )
   CK-CACHE-BUF-FIELD ! ;

\ CK-CACHE-CAP is a positive library constant: MEM:BYTES-ALLOC-LEN narrows the raw
\ size to the validated alloc role before MEM:ALLOC-BYTES, throwing E-MEM-SIZE on
\ any refusal (unreachable for the constant).
: CK-CACHE-BUF ( -- ptr u8 )
   CK-CACHE-BUF@ 0= if
      CK-CACHE-CAP MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop CK-CACHE-BUF!
   then
   CK-CACHE-BUF@ ;

\ Second cap-sized buffer used only to assemble the compacted image before the
\ atomic write; kept off the static image the same way as the load buffer.
: CK-CACHE-OUT-FIELD ( -- ptr ptr u8 )
   CK-CACHE-OUT-A 0 ptr-field ;

: CK-CACHE-OUT@ ( -- ptr u8 )
   CK-CACHE-OUT-FIELD @ ;

: CK-CACHE-OUT! ( ptr u8 -- )
   CK-CACHE-OUT-FIELD ! ;

: CK-CACHE-OUT ( -- ptr u8 )
   CK-CACHE-OUT@ 0= if
      CK-CACHE-CAP MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop CK-CACHE-OUT!
   then
   CK-CACHE-OUT@ ;

\ Path-ordered offset index over the loaded rows: one cell per row, bounded by
\ CK-CACHE-MAX-ROWS, allocated once through the checked MEM: cell surface.
: CK-IDX-FIELD ( -- ptr ptr a )
   CK-IDX-BASE 0 ptr-field ;

: CK-IDX ( -- ptr a )
   CK-IDX-FIELD @ 0= if
      CK-CACHE-MAX-ROWS MEM:CELLS-ALLOC-COUNT MEM:ALLOC-CELLS CK-IDX-FIELD !
   then
   CK-IDX-FIELD @ ;

: CK-IDX-AT@ ( n -- n ) {: i:n :}
   CK-IDX i cells + @ ;

: CK-IDX-AT! ( n n -- ) {: off:n i:n :}
   off CK-IDX i cells + ! ;

: CK-CACHE-PATH$ ( -- ptr u8 n )
   CK-CACHE-PATH-BUF CK-CACHE-PATH-U @ ;

: CK-CACHE-PATH? ( -- bool )
   CK-CACHE-PATH-U @ 0 > ;

: CK-CACHE-PATH! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 < if E-FS-PATH throw then
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   a CK-CACHE-PATH-BUF u BYTE-COPY
   u CK-CACHE-PATH-U !
   0 CK-CACHE-LOADED ! ;

: CK-CACHE-ROOT! ( ptr u8 n -- ) {: a:ptr u:n :}
   a u s" content-key.cache" CK-CACHE-PATH-BUF JOIN-PATH CK-CACHE-PATH-U !
   0 CK-CACHE-LOADED ! ;

: CK-CACHE-AUTO? ( -- bool )
   CK-CACHE-DISABLED @ 0 <> if CK-FALSE exit then
   CK-CACHE-PATH? if CK-TRUE exit then
   CK-FALSE ;

: CK-ROW-RESET ( -- )
   0 CK-ROW-U ! ;

: CK-ROW-CHECK ( n -- ) {: n:n :}
   n 0 < if E-STR-BOUNDS throw then
   CK-ROW-U @ n + CK-ROW-CAP > if E-STR-CAPACITY throw then ;

: CK-ROW-C+ ( n -- ) {: c:n :}
   1 CK-ROW-CHECK
   c 0 < if E-STR-BOUNDS throw then
   c STR-BYTE-MAX > if E-STR-BOUNDS throw then
   c CK-ROW-BUF CK-ROW-U @ + c!
   CK-ROW-U @ 1+ CK-ROW-U ! ;

: CK-ROW+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u CK-ROW-CHECK
   a CK-ROW-BUF CK-ROW-U @ + u BYTE-COPY
   CK-ROW-U @ u + CK-ROW-U ! ;

: CK-ROW-N+ ( n -- ) {: n:n :}
   n 0 < if E-STR-BOUNDS throw then
   n 10 >= if n 10 / RECURSE then
   n 10 mod STR-ZERO + CK-ROW-C+ ;

: CK-ROW-FILE-PREFIX ( ptr u8 n n n n n n -- )
   {: a:ptr u:n sz:n mt:n mn:n ct:n cn:n :}
   CK-ROW-RESET
   a u CK-ROW+
   STR-TAB CK-ROW-C+
   sz CK-ROW-N+
   STR-TAB CK-ROW-C+
   mt CK-ROW-N+
   STR-TAB CK-ROW-C+
   mn CK-ROW-N+
   STR-TAB CK-ROW-C+
   ct CK-ROW-N+
   STR-TAB CK-ROW-C+
   cn CK-ROW-N+
   STR-TAB CK-ROW-C+
   CK-ROW-U @ CK-PREFIX-U ! ;

: CK-HEX-NIB ( n -- n ) {: c:n :}
   c STR-ZERO >= c STR-ZERO 10 + < and if c STR-ZERO - exit then
   c CK-HEX-LOW-A >= c CK-HEX-LOW-G < and if c 87 - exit then
   c CK-HEX-UP-A >= c CK-HEX-UP-G < and if c 55 - exit then
   E-STR-BOUNDS throw ;

: CK-HEX-BYTE@ ( ptr u8 -- n ) {: a:ptr :}
   a c@ CK-HEX-NIB 4 lshift
   a 1 + c@ CK-HEX-NIB or ;

: CK-HEX>DIGEST ( ptr u8 -- ) {: a:ptr :}
   32 0 DO
      a i 2 * + CK-HEX-BYTE@ CK-FILE-DG i + c!
   LOOP ;

: CK-LINE-END ( n -- n ) {: off:n :}
   off begin dup CK-CACHE-U @ < while
      CK-CACHE-BUF over + c@ STR-LF = if exit then
      1+
   repeat ;

: CK-CACHE-LINE? ( n -- bool ) {: off:n :}
   off CK-LINE-END {: ed:n :}
   ed off - CK-PREFIX-U @ CK-HEX-LEN + <> if CK-FALSE exit then
   CK-CACHE-BUF off + CK-PREFIX-U @ CK-ROW-BUF CK-PREFIX-U @ STR= 0= if CK-FALSE exit then
   CK-CACHE-BUF off + CK-PREFIX-U @ + CK-HEX>DIGEST
   CK-TRUE ;

\ ---- path fields + ordering over cache rows ----------------------------------

: CK-ROW-PATH-END ( n -- n ) {: off:n :}
   off begin dup CK-CACHE-U @ < while
      CK-CACHE-BUF over + c@ STR-TAB = if exit then
      1+
   repeat ;

: CK-ROW-PATH$ ( n -- ptr u8 n ) {: off:n :}
   CK-CACHE-BUF off +  off CK-ROW-PATH-END off - ;

: CK-QUERY-PATH-END ( -- n )
   0 begin dup CK-PREFIX-U @ < while
      CK-ROW-BUF over + c@ STR-TAB = if exit then
      1+
   repeat ;

: CK-QUERY-PATH$ ( -- ptr u8 n )
   CK-ROW-BUF CK-QUERY-PATH-END ;

: CK-PATH<? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr au:n b:ptr bu:n :}
   0 begin dup au < over bu < and while
      dup a + c@ over b + c@ 2dup <> if < nip exit then 2drop 1+
   repeat drop au bu < ;

\ SORT:SORT! comparator: order rows by path, then by buffer offset so equal-path rows
\ stay in append (chronological) order and the last of each run is the newest.
: CK-OFF-LESS? ( n n -- bool ) {: oa:n ob:n :}
   oa CK-ROW-PATH$ ob CK-ROW-PATH$ STR= if oa ob < exit then
   oa CK-ROW-PATH$ ob CK-ROW-PATH$ CK-PATH<? ;

\ ---- index build over a byte region of the cache buffer ----------------------

: CK-CACHE-INDEX-REGION ( n n -- ) {: lo:n hi:n :}
   0 CK-IDX-N !
   lo begin dup hi < while
      CK-IDX-N @ CK-CACHE-MAX-ROWS >= if E-STR-CAPACITY throw then
      dup CK-IDX-N @ CK-IDX-AT!
      CK-IDX-N @ 1+ CK-IDX-N !
      CK-LINE-END 1+
   repeat drop ;

: CK-CACHE-BUILD-IDX ( -- )
   0 CK-CACHE-U @ CK-CACHE-INDEX-REGION
   CK-IDX CK-IDX-N @ [: CK-OFF-LESS? ;] SORT:SORT!
   CK-CACHE-U @ CK-IDX-END ! ;

: CK-CACHE-HAS-DUPS? ( -- bool )
   1 CK-DUP-I !
   begin CK-DUP-I @ CK-IDX-N @ < while
      CK-DUP-I @ 1- CK-IDX-AT@ CK-ROW-PATH$
      CK-DUP-I @ CK-IDX-AT@ CK-ROW-PATH$
      STR= if CK-TRUE exit then
      CK-DUP-I @ 1+ CK-DUP-I !
   repeat CK-FALSE ;

\ ---- compaction: keep the newest row per path, atomically ---------------------

: CK-ROW-BYTES ( n -- n ) {: off:n :}
   off CK-LINE-END off - 1+ ;

: CK-IDX-LAST-OF-RUN? ( n -- bool ) {: i:n :}
   i 1+ CK-IDX-N @ >= if CK-TRUE exit then
   i CK-IDX-AT@ CK-ROW-PATH$  i 1+ CK-IDX-AT@ CK-ROW-PATH$  STR= 0= ;

\ Collect kept offsets into the front of CK-IDX. Safe in place: the write slot
\ CK-KEEP-N is never greater than the loop index, and every future read is of a
\ strictly higher slot, so no not-yet-read entry is overwritten.
: CK-KEEP+ ( n -- )
   CK-KEEP-N @ CK-IDX-AT!
   CK-KEEP-N @ 1+ CK-KEEP-N ! ;

: CK-CACHE-KEEP-NEWEST ( -- )
   0 CK-KEEP-N !
   0 begin dup CK-IDX-N @ < while
      dup CK-IDX-LAST-OF-RUN? if
         dup CK-IDX-AT@ CK-KEEP+
      then
      1+
   repeat drop ;

: CK-CACHE-TOTAL-BYTES ( -- n )
   0 0 begin dup CK-KEEP-N @ < while
      dup CK-IDX-AT@ CK-ROW-BYTES rot + swap
      1+
   repeat drop ;

\ Drop the oldest kept rows (lowest offsets, sorted to the front) until the
\ surviving image fits under the cap; returns the first row index to emit.
: CK-CACHE-EVICT-START ( -- n )
   CK-CACHE-TOTAL-BYTES CK-EVICT-TOTAL !
   0 begin CK-EVICT-TOTAL @ CK-CACHE-CAP > while
      dup CK-KEEP-N @ >= if exit then
      dup CK-IDX-AT@ CK-ROW-BYTES CK-EVICT-TOTAL @ swap - CK-EVICT-TOTAL !
      1+
   repeat ;

: CK-CACHE-EMIT ( -- )
   0 CK-CACHE-OUT-U !
   CK-CACHE-EVICT-START
   begin dup CK-KEEP-N @ < while
      dup CK-IDX-AT@ CK-EMIT-OFF !
      CK-EMIT-OFF @ CK-ROW-BYTES CK-EMIT-LEN !
      CK-CACHE-BUF CK-EMIT-OFF @ +  CK-CACHE-OUT CK-CACHE-OUT-U @ +  CK-EMIT-LEN @ BYTE-COPY
      CK-CACHE-OUT-U @ CK-EMIT-LEN @ + CK-CACHE-OUT-U !
      1+
   repeat drop ;

: CK-CACHE-COMPACT ( -- )
   CK-CACHE-OUT drop
   0 CK-CACHE-U @ CK-CACHE-INDEX-REGION
   CK-IDX CK-IDX-N @ [: CK-OFF-LESS? ;] SORT:SORT!
   CK-CACHE-KEEP-NEWEST
   CK-IDX CK-KEEP-N @ [: < ;] SORT:SORT!
   CK-CACHE-EMIT ;

\ ---- atomic writer: unique sibling temp then rename (last-writer-wins) --------

: CK-TMP-C+ ( n -- ) {: c:n :}
   CK-CACHE-TMP-U @ FS-PATH-CAP >= if E-FS-CAPACITY throw then
   c CK-CACHE-TMP-BUF CK-CACHE-TMP-U @ + c!
   CK-CACHE-TMP-U @ 1+ CK-CACHE-TMP-U ! ;

: CK-TMP-BYTES+ ( ptr u8 n -- ) {: a:ptr u:n :}
   CK-CACHE-TMP-U @ u + FS-PATH-CAP > if E-FS-CAPACITY throw then
   a CK-CACHE-TMP-BUF CK-CACHE-TMP-U @ + u BYTE-COPY
   CK-CACHE-TMP-U @ u + CK-CACHE-TMP-U ! ;

: CK-TMP-N+ ( n -- ) {: n:n :}
   n 0 < if E-STR-BOUNDS throw then
   n 10 >= if n 10 / RECURSE then
   n 10 mod STR-ZERO + CK-TMP-C+ ;

: CK-CACHE-BUILD-TMP ( -- ptr u8 n )
   0 CK-CACHE-TMP-U !
   CK-CACHE-PATH$ CK-TMP-BYTES+
   CK-DOT CK-TMP-C+
   mono-ns CK-TMP-N+
   s" .tmp" CK-TMP-BYTES+
   CK-CACHE-TMP-BUF CK-CACHE-TMP-U @ ;

: CK-CACHE-UNIQUE-TMP ( -- ptr u8 n )
   0 CK-TMP-TRY !
   begin
      CK-CACHE-BUILD-TMP 2drop
      CK-CACHE-TMP-BUF CK-CACHE-TMP-U @ EXISTS? 0=
      CK-TMP-TRY @ 1+ dup CK-TMP-TRY ! 64 >= if E-FS-IO throw then
   until
   CK-CACHE-TMP-BUF CK-CACHE-TMP-U @ ;

: CK-CACHE-WRITE ( -- )
   CK-CACHE-UNIQUE-TMP CK-CACHE-OUT CK-CACHE-OUT-U @ WRITE-ALL
   CK-CACHE-TMP-BUF CK-CACHE-TMP-U @ CK-CACHE-PATH$ RENAME-FILE ;

\ Best-effort temp removal after a failed write; there is nowhere left to report
\ a cleanup failure during error handling, so it is intentionally swallowed.
: CK-CACHE-CLEAN-TMP ( -- )
   CK-CACHE-TMP-BUF CK-CACHE-TMP-U @ EXISTS? if
      [: CK-CACHE-TMP-BUF CK-CACHE-TMP-U @ REMOVE-FILE ;] catch drop
   then ;

\ A cache the process cannot persist is announced once on stderr and then left
\ disabled so the build continues uncached (visible degradation, never silent).
: CK-CACHE-PERSIST ( -- )
   [: CK-CACHE-WRITE ;] catch 0 <> if
      CK-CACHE-CLEAN-TMP
      s" hb: content-key cache write failed; continuing without cache" CK-STDERR-LINE
      -1 CK-CACHE-DISABLED !
   then ;

: CK-CACHE-OVERCAP ( -- )
   s" hb: content-key cache over capacity; rebuilding compacted" CK-STDERR-LINE ;

: CK-CACHE-INIT ( -- )
   CK-CACHE-BUF drop
   0 CK-CACHE-U !
   CK-CACHE-PATH$ FILE? if
      CK-CACHE-PATH$ FILE-SIZE CK-CACHE-CAP > if
         CK-CACHE-OVERCAP
      else
         CK-CACHE-PATH$ CK-CACHE-BUF CK-CACHE-CAP READ-ALL CK-CACHE-U !
      then
   then
   CK-CACHE-BUILD-IDX
   CK-CACHE-HAS-DUPS? if -1 CK-CACHE-DIRTY ! then ;

: CK-CACHE-LOAD? ( -- bool )
   CK-CACHE-AUTO? 0= if CK-FALSE exit then
   CK-CACHE-LOADED @ 0 <> if CK-TRUE exit then
   CK-CACHE-INIT
   -1 CK-CACHE-LOADED !
   CK-TRUE ;

\ ---- lookup: binary search the index, then scan the appended tail ------------

: CK-IDX-LOWER ( -- n )
   0 CK-LO !  CK-IDX-N @ CK-HI !
   begin CK-LO @ CK-HI @ < while
      CK-LO @ CK-HI @ + 2 / CK-MID !
      CK-MID @ CK-IDX-AT@ CK-ROW-PATH$ CK-QUERY-PATH$ CK-PATH<? if
         CK-MID @ 1+ CK-LO !
      else
         CK-MID @ CK-HI !
      then
   repeat
   CK-LO @ ;

: CK-IDX-FIND? ( -- bool )
   CK-IDX-N @ 0= if CK-FALSE exit then
   CK-IDX-LOWER CK-FIND-I !
   begin CK-FIND-I @ CK-IDX-N @ < while
      CK-FIND-I @ CK-IDX-AT@ CK-ROW-PATH$ CK-QUERY-PATH$ STR= 0= if CK-FALSE exit then
      CK-FIND-I @ CK-IDX-AT@ CK-CACHE-LINE? if CK-TRUE exit then
      CK-FIND-I @ 1+ CK-FIND-I !
   repeat CK-FALSE ;

: CK-CACHE-TAIL-FIND? ( -- bool )
   CK-IDX-END @ begin dup CK-CACHE-U @ < while
      dup CK-CACHE-LINE? if drop CK-TRUE exit then
      CK-LINE-END 1+
   repeat drop CK-FALSE ;

: CK-CACHE-FIND? ( -- bool )
   CK-IDX-FIND? if CK-TRUE exit then
   CK-CACHE-TAIL-FIND? ;

\ ---- save: compact the newest-per-path image and persist it atomically -------

: CK-CACHE-SAVE ( -- )
   CK-CACHE-AUTO? 0= if exit then
   CK-CACHE-DIRTY @ 0= if exit then
   CK-CACHE-COMPACT
   CK-CACHE-PERSIST
   CK-CACHE-DISABLED @ 0= if
      CK-CACHE-OUT CK-CACHE-BUF CK-CACHE-OUT-U @ BYTE-COPY
      CK-CACHE-OUT-U @ CK-CACHE-U !
      CK-CACHE-BUILD-IDX
   then
   0 CK-CACHE-DIRTY ! ;

: CK-ROW-DIGEST+ ( -- )
   CK-FILE-DG CK-FILE-HEX SHA256>HEX
   CK-FILE-HEX CK-HEX-LEN CK-ROW+
   STR-LF CK-ROW-C+ ;

\ Newly computed rows land in the in-memory buffer and mark the cache dirty; the
\ file is written by CK-CACHE-SAVE at key finalize, not per row. Compact first if
\ the row would not fit so the buffer stays under the cap.
: CK-CACHE-APPEND ( -- )
   CK-CACHE-AUTO? 0= if exit then
   CK-ROW-DIGEST+
   CK-CACHE-U @ CK-ROW-U @ + CK-CACHE-CAP > if CK-CACHE-SAVE then
   CK-ROW-BUF CK-ROW-U @ STR:LENGTH CK-CACHE-BUF CK-CACHE-CAP STR:LENGTH CK-CACHE-U STR:BUF-APPEND
   -1 CK-CACHE-DIRTY ! ;

: CK-FILE-DIGEST! ( ptr u8 n -- ) {: a:ptr u:n :}
   a u CK-FILE-DG SHA256-FILE dup 0 <> if throw then drop ;

: CK-FILE+ ( ptr u8 n -- ) {: a:ptr u:n :}
   CK-FILE-TAG a u CK-FRAG+
   a u FILE-META {: sz:n mt:n mn:n ct:n cn:n :}
   a u sz mt mn ct cn CK-ROW-FILE-PREFIX
   CK-CACHE-LOAD? if
      CK-CACHE-FIND? if CK-FILE-DG CK-DIGEST+ exit then
   then
   a u CK-FILE-DIGEST!
   CK-FILE-DG CK-DIGEST+
   CK-CACHE-APPEND ;

\ Finalizing a key is the batch boundary: flush the accumulated cache rows to
\ disk once, compacted, so a run against a bloated cache self-heals on save.
: CK-FINAL ( ptr u8 -- ) {: dst:ptr :}
   CK-BUF CK-U @ dst SHA256
   CK-CACHE-SAVE ;

: CK-FINAL-HEX ( ptr u8 -- ) {: hex:ptr :}
   CK-DG CK-FINAL
   CK-DG hex SHA256>HEX ;
