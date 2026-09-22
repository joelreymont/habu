\ image-size-lib.f - where every byte of a Habu image goes. The measurement
\ half, with no entry of its own: tools/engine-size.f is the command line and
\ tools/hb-build-lib.f requires this file to size what a build just wrote.
\
\ WHY IT EXISTS. "Why is bin/hb 5.8 MB?" had no answer a measurement could
\ give. The size of the parts was folklore -- the dictionary was blamed for
\ megabytes it does not cost -- because nothing walked the file. This does:
\ it attributes EVERY byte of an image to a class and an owner, and refuses to
\ report at all unless the classes sum to the file's own length. "Why is this
\ application 24 MB?" was the same question one image class later, and the
\ answer turned out to be neither code nor dictionary: see WHAT AN APPLICATION
\ IMAGE IS below.
\
\ WHAT IT WALKS. A baked engine (no snapshot trailer) is four things on disk:
\ the ELF header page, the engine's own emitted code, the boot-seeded primitive
\ dictionary, and the AOT payload src/habu/habu2.f EMIT-AOT-SEED bakes last.
\ The payload is self-describing -- every count precedes its rows -- so the walk
\ needs no symbol table: it finds the payload by trying each 4-byte-aligned
\ offset and keeping the one whose framing walks from its first cell to the end
\ of the image content. A walk that lands anywhere else is refused, which is
\ what makes a single self-consistent answer evidence rather than a guess.
\ The section format mirrors EMIT-AOT-SEED and the record format mirrors
\ src/habu/xref.f, the same way tools/imgdump.f mirrors them; an emitter change
\ that this file does not follow makes the walk fail to land, loudly.
\
\ THE REACHABILITY CENSUS answers the second half: a stripped application gets
\ the closure walk (src/habu/aot-closure.f), the engine does not, so every word
\ the build ever compiled ships. The census marks from roots over the same call
\ graph the closure walker uses -- direct B/BL edges in the baked code, plus the
\ code addresses the payload's own relocation tables name -- and reports what
\ nothing reaches. Two root sets, because "dead" means two different things:
\   dictionary  the language surface is the API: every global and package-PUBLIC
\               word is a root, since a program can name it. What this leaves
\               unreachable is private code no public word can reach.
\   entry       only the engine's own entry points are roots (its boot-run entry
\               words and the code addresses its DATA cells hold). This is a
\               LOWER BOUND on what a tree-shaken engine could keep, not a strip
\               list: the interpreter resolves user tokens by name, so a public
\               word outside this closure is still callable.
\ Both over-approximate reachability -- gaps outside the record and stripped-span
\ indices are root regions, and an address the payload records is a root --
\ so a word this reports
\ as unreachable is unreachable, and the report is a floor, never a wish.
\ NEITHER ROOT SET MEANS ANYTHING FOR AN APPLICATION IMAGE, so the census does
\ not run on one: a stripped image already had the closure walk run against it
\ at build time, and a --repl image ships the interpreter, which resolves user
\ tokens by name, so every word it carries is reachable by construction.
\
\ WHAT AN APPLICATION IMAGE IS. tools/hb-build.f writes two other classes and
\ this file walks both, because "where is the size going" was asked of them and
\ not of the engine.
\   --repl   a SNAPSHOT: the whole donor engine's text, then the live region
\            (the dictionary slot array and the code band) and the DATA window
\            copied verbatim, then the 48-byte trailer that says how long the
\            two payloads are (src/habu/snap-lib.f writes it, src/habu/habu2.f
\            EM-SNAPSHOT-RESTORE reads it, src/habu/layout.f owns its geometry).
\            The engine half is bit-for-bit a baked engine, so the walkers above
\            measure it unchanged; only the text end moves, from TEXT-SIZE to
\            where the region payload starts (ETEXT-END).
\   stripped no dictionary, no compiler, no trailer: an entry, the closure of
\            MAIN, the crash handlers, the DATA window as sparse non-zero runs,
\            and one 8-byte relocation row per declared address cell.
\ VERBATIM IS THE WHOLE STORY. A snapshot writes its zero bytes -- the unused
\ dictionary slots, the untouched tail of every table -- and they are most of
\ the file, so every class here carries a ZERO column beside its byte count and
\ the two are reported apart. A stripped image carries no zero byte at all: its
\ runs describe a span far larger than the file.
\
\ Run: <engine> --load tools/engine-size.f -- <image>

require lib/fmt.f
require lib/fs.f
require lib/sort.f
require src/habu/code-span.f
require tools/aot-startup-shape.f        \ the startup's instruction shapes, named once
require tools/image-names.f

\ The engine's own layout is already in the cold prefix; the target executable
\ layout (CODE-OFF, IMAGE-TEXT-SIZE-OFF) is not. tools/imgdump.f loads it the
\ same way and for the same reason.
: ES-LOAD-TARGET-LAYOUT ( -- )
   s" DATA-SIZE" XREF-FIND 0= if
      HB-TARGET-LINUX? if s" src/os/linux/layout.f" included exit then
      HB-TARGET-MACOS? if s" src/os/macos/layout.f" included exit then
      HB-TARGET-LINUX-X86-64? if
         s" src/os/linux-x86-64/layout.f" included exit then
      s" image-size: unknown target" 74 die
   then ;

ES-LOAD-TARGET-LAYOUT
undefine ES-LOAD-TARGET-LAYOUT

package IMAGE-SIZE

74 constant RC

\ The walk's own refusal. Thrown, never died: the payload search probes many
\ offsets and every wrong one must come back as a failed walk, not as an exit.
\ This tool's own throw range: -9240..-9249 (the -9180 decade is lib/errors.f TCP4).
-9240 constant E-ES-FIRST
-9249 constant E-ES-LAST
-9240 constant E-ES-WALK

\ ---- image intake -------------------------------------------------------------
DYNAMIC-BUFFER IMG n
variable ILEN

: IMG@ ( -- ptr u8 )
   0 IMG BYTE-VIEW ;

: IN-IMAGE? ( n n -- bool ) {: off:n len:n :}
   off 0 < len 0 < or if false exit then
   off len + ILEN @ <= ;

: ?RANGE ( n n -- )
   IN-IMAGE? 0= if E-ES-WALK throw then ;

: U8@ ( n -- n ) {: off:n :}
   off 1 ?RANGE  IMG@ off + c@ ;

: U32@ ( n -- n ) {: off:n :}
   off 4 ?RANGE
   off U8@  off 1+ U8@ 8 lshift or
   off 2 + U8@ 16 lshift or  off 3 + U8@ 24 lshift or ;

: U64@ ( n -- n ) {: off:n :}
   off U32@  off 4 + U32@ 32 lshift or ;

\ How many bytes of a span are zero. An application image writes its DATA window
\ verbatim, zeros included, so this is the difference between what an image
\ CARRIES and what it SAYS, and every class is reported with it beside the byte
\ count rather than folded into one number.
variable ZACC

: ZEROS ( n n -- n ) {: at:n len:n :}
   at len ?RANGE
   0 ZACC !
   len 0 ?do  at i + U8@ 0= if ZACC @ 1+ ZACC ! then  loop
   ZACC @ ;

\ A class that is one contiguous span answers both its numbers at once.
: SPAN ( n n -- n n ) {: at:n len:n :}
   len  at len ZEROS ;

: READ-IMAGE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu FILE-SIZE {: size:n :}
   size CELL < if s" image-size: image too small" RC die then
   size CELL 1- + CELL / IMG-RESERVE
   size ILEN !
   path pathu IMG@ size READ-ALL size <> if
      s" image-size: short read" RC die
   then ;

\ ---- ELF header page -----------------------------------------------------------
\ Header field offsets, the same evidence tools/imgdump.f reads: the class of the
\ file in hand is a property of that file, never of this tool's own build target.
$00 constant ELF-MAG-OFF
$464C457F constant ELF-MAG
$04 constant ELF-CLASS-OFF
2 constant ELF-CLASS-64
$10 constant ELF-TYPE-OFF
2 constant ELF-ET-EXEC
$12 constant ELF-MACHINE-OFF
\ The size map reads an ARM64 engine: BAKED-ELF? admits that machine and no
\ other, so an x86_64 image is refused by name rather than measured with the
\ wrong startup in mind (dot habu-cross-build-the-d25a959d).
183 constant ELF-EM-AARCH64
$20 constant ELF-PHOFF-OFF
$34 constant ELF-EHSIZE-OFF
$36 constant ELF-PHENTSIZE-OFF
$38 constant ELF-PHNUM-OFF
$40 constant ELF-EHDR-BYTES

: U16@ ( n -- n ) {: off:n :}
   off U32@ $FFFF and ;

: BAKED-ELF? ( -- bool )
   ILEN @ ELF-EHDR-BYTES < if false exit then
   ELF-MAG-OFF U32@ ELF-MAG <> if false exit then
   ELF-CLASS-OFF U8@ ELF-CLASS-64 <> if false exit then
   ELF-MACHINE-OFF U16@ ELF-EM-AARCH64 <> if false exit then
   ELF-TYPE-OFF U16@ ELF-ET-EXEC = ;

: TEXT-SIZE ( -- n )
   IMAGE-TEXT-SIZE-OFF U64@ ;

\ Where the ENGINE's own text content ends, which is not the same number as the
\ file's text extent once an application image appends payloads of its own. A
\ baked engine ends its text where the file's text segment ends; a snapshot ends
\ it where the region payload begins, and every walker below -- the seeded
\ dictionary's bound, the AOT payload's framing bounds, the tail-pad test --
\ means THIS end, never the file's. Set once, before any walk.
variable ETEXT-N

: ETEXT-END ( -- n )
   ETEXT-N @ ;

\ The file is the RX segment (which begins at offset 0 and contains the header
\ page) followed by the RW segment the second program header names. Checking
\ that identity first is what makes every later offset a real one: a truncated
\ or padded file is refused here instead of failing as a walk that does not land.
: PHDR2 ( -- n )
   ELF-EHDR-BYTES ELF-PHENTSIZE-OFF U16@ + ;

: RW-OFF ( -- n ) PHDR2 8 + U64@ ;
: RW-BYTES ( -- n ) PHDR2 32 + U64@ ;

: CHECK-SEGMENTS ( -- )
   ELF-PHNUM-OFF U16@ 2 < if
      s" image-size: image has no RW segment" RC die
   then
   ELF-PHOFF-OFF U64@ ELF-EHDR-BYTES <> if
      s" image-size: program headers are not where an engine image puts them" RC die
   then
   RW-OFF TEXT-SIZE <> if
      s" image-size: the RW segment does not follow the text segment" RC die
   then
   TEXT-SIZE RW-BYTES + ILEN @ <> if
      s" image-size: file length is not its two segments" RC die
   then ;

: PHDR-END ( -- n )
   ELF-EHDR-BYTES  ELF-PHNUM-OFF U16@ ELF-PHENTSIZE-OFF U16@ * + ;

\ The header page's tail is zero padding. Its live extent is read out of the file
\ rather than out of the builder's constants: this tool reports what an image IS.
: LAST-NONZERO ( n n -- n ) {: from:n to:n :}
   to begin dup from > while
      dup 1- U8@ 0<> if exit then
      1-
   repeat ;


\ ---- the boot-seeded primitive dictionary --------------------------------------
\ src/habu/habu1.f ENGINE-EMIT:EMIT-DICT bakes, in this order: the long names of
\ the primitives whose name does not fit a record, then LNCOUNT (the primitive
\ count, one cell), then LDICT (that many 48-byte records). It is the LAST thing
\ in the engine's code half, so the AOT payload starts after it.
\ The run is found the way tools/imgdump.f finds it -- the longest run of
\ plausible records -- and then PROVED by the count cell in front of it.
DREC constant PREC
variable PDICT                                    \ file offset of the first record
variable PDICT-N                                  \ records in the seeded table
variable PNAME-BYTES                              \ padded long-name bytes before it

: PREC-START ( n -- n ) U64@ ;
: PREC-END ( n -- n ) 8 + U64@ ;
: PREC-FLAGS ( n -- n ) 16 + U64@ ;
: PREC-NAME-LEN ( n -- n ) PREC-FLAGS DNAME-LEN-MASK and ;
: PREC-EXT? ( n -- bool ) PREC-FLAGS DNAME-EXT and 0<> ;
: PREC-WID ( n -- n ) 40 + U64@ ;

\ A record's name bytes: inline at [24], or at a text-content-relative offset the
\ boot rebases by the image base (src/habu/habu2.f EM-SEED-DICT).
: PREC-NAME ( n -- n n ) {: r:n :}
   r PREC-EXT? if CODE-OFF r 24 + U64@ + else r 24 + then
   r PREC-NAME-LEN ;

: PRINTABLE? ( n n -- bool ) {: at:n len:n :}
   len 0 ?do
      at i + U8@ {: c:n :}
      c 33 < c 126 > or if false unloop exit then
   loop
   true ;

: PREC-OK? ( n -- bool ) {: r:n :}
   r PREC IN-IMAGE? 0= if false exit then
   r PREC-START 1 < if false exit then
   r PREC-END r PREC-START < if false exit then
   r PREC-END ETEXT-END > if false exit then
   r PREC-WID 0 < r PREC-WID $FFFF > or if false exit then
   r PREC-NAME-LEN 1 < if false exit then
   r PREC-EXT? 0= r PREC-NAME-LEN DNAME-INL > and if false exit then
   r PREC-NAME 2dup IN-IMAGE? 0= if 2drop false exit then
   PRINTABLE? ;

: PREC-RUN ( n -- n ) {: r:n :}
   0 begin
      r over PREC * + PREC-OK? while
      1+
   repeat ;

\ A run is only the seeded table when the cell in front of it says so: the count
\ cell is the emitter's own statement of the record count, so an accidental run
\ of record-shaped bytes inside the payload cannot be mistaken for the table.
: COUNTED-RUN ( n -- n ) {: at:n :}
   at 8 < if 0 exit then
   at PREC-RUN {: run:n :}
   run 0= if 0 exit then
   at 8 - U64@ run <> if 0 exit then
   run ;

\ Answers PDICT-N 0 when the image has no such table, which is how an image
\ class is told apart: a stripped application carries no seeded dictionary at
\ all, so its absence is evidence and not a failure.
: PRIM-DICT-SCAN ( n -- ) {: limit:n :}
   0 PDICT !  0 PDICT-N !
   CODE-OFF begin dup limit < while
      dup PREC-OK? if
         dup COUNTED-RUN {: run:n :}
         run PDICT-N @ > if dup PDICT !  run PDICT-N ! then
         run 0 > if run PREC * + else 4 + then
      else 4 + then
   repeat drop ;

: PRIM-DICT-FIND ( n -- ) {: limit:n :}
   limit PRIM-DICT-SCAN
   PDICT-N @ 0= if
      s" image-size: no counted seeded dictionary in the image" RC die
   then ;

: PAD4 ( n -- n ) negate 3 and ;

\ The long-name blob sits immediately below the count cell, one padded run per
\ out-of-line name, in record order. Proved by its own arithmetic: the first
\ such name must start exactly that many bytes below the count cell.
variable NFIRST

: PRIM-NAMES-MEASURE ( -- )
   0 PNAME-BYTES !  0 NFIRST !
   PDICT-N @ 0 ?do
      PDICT @ i PREC * + {: r:n :}
      r PREC-EXT? if
         NFIRST @ 0= if CODE-OFF r 24 + U64@ + NFIRST ! then
         PNAME-BYTES @ r PREC-NAME-LEN dup PAD4 + + PNAME-BYTES !
      then
   loop
   PNAME-BYTES @ 0 > if
      NFIRST @ PNAME-BYTES @ + PDICT @ 8 - <> if
         s" image-size: seeded long-name blob does not meet its count cell" RC die
      then
   then ;

\ ---- the AOT payload -----------------------------------------------------------
\ One walk of src/habu/habu2.f EMIT-AOT-SEED's emission order. Every count cell
\ precedes its rows, two runs carry no count of their own (the DATA run bytes are
\ the sum of the run lengths; the boot-run list is 0-terminated), and the whole
\ payload is followed only by the zero pad that rounds the text segment up.
variable AOT0        variable AOT-END     variable CUR        variable FRAME-CELLS
variable FRAME-ZERO
variable BLOB-OFF    variable BLOB-LEN
variable REC0        variable REC-N
variable SITE0       variable SITE-N
variable NAMES0      variable NAMES-LEN
variable DATA-SPAN   variable DATA-D0
variable DSITE0      variable DSITE-N
variable XTOFF0      variable XTOFF-N
variable RUN0        variable RUN-BYTES   variable RUN-N
variable RUN-AT      variable RUN-PREV
variable RBYTES0     variable RBYTES-LEN
variable CODE-B0
variable CSITE0      variable CSITE-N
variable XTSITE0     variable XTSITE-N
variable SPAN0       variable SPAN-N
variable BOOTRUN0    variable BOOTRUN-LEN
variable WID-W0      variable WID-SPAN
variable PWIN0       variable PWIN-N
variable SIG-LEN     variable SIG0        variable SIGNAME0   variable SIGNAME-LEN
variable CAND        variable ACC

\ Row widths belong to src/habu/aot-decl.f (SITE-ROW, AOT-WINDOW:XTOFF-ROW) and
\ to src/habu/layout.f (AOT-CREC-ROW, which this engine does carry). The two from
\ aot-decl.f are mirrored here because that file is a build-side source a booted
\ engine cannot load, the same reason tools/data-table-census.f mirrors the
\ payload's 8-byte run row. A width this file gets wrong cannot pass unnoticed:
\ the walk stops landing on the image's own content end.
12 constant SITE-ROW                              \ blob-off u32, target u32, callee scope u32
$80000000 constant SITE-NAME-TAG                  \ target is a name-pool offset, not an index
8 constant XTOFF-ROW                              \ location u32, typed target u32
8 constant XTSITE-ROW                             \ blob-off u32, name-off u32
8 constant SPAN-ROW                               \ blob-off u32, raw code span u32
AOT-NAMES-CAP constant NAMES-CAP
\ The window's cells have no row width to mirror: the payload is a presence
\ bitmap, one bit a cell, and one unsigned LEB128 per present cell. This decode
\ mirrors src/habu/aot-decl.f AOT-WINDOW:CELL-V@ for the same reason the widths
\ above are mirrored, and it is what counts the present cells and sums their
\ value bytes - the payload states neither.
8 constant CELL-BYTES                             \ the DATA cell grid the bitmap covers
8 constant CELL-BITS                              \ cells one bitmap byte covers
10 constant CELL-VMAX                             \ an unsigned LEB128 of a cell is at most ten bytes

: RUN-VW ( n n -- n ) {: at:n avail:n :}
   avail CELL-VMAX min 0 ?do
      at i + U8@ $80 and 0= if
         i 1+ {: w:n :}
         w 1 > at w 1- + U8@ 0= and if 0 unloop exit then
         w unloop exit
      then
   loop
   0 ;

: RUN-VV ( n n -- n ) {: at:n w:n :}
   0 w 0 ?do  at i + U8@ $7F and  i 7 * lshift or  loop ;

\ Value and width, and a walk that cannot decode a row is a walk that did not
\ land - the refusal every other malformation here raises.
: RUN-V@ ( n n -- n n ) {: at:n avail:n :}
   at avail RUN-VW {: w:n :}
   w 0= if E-ES-WALK throw then
   w CELL-VMAX = at CELL-VMAX 1- + U8@ 1 > and if E-ES-WALK throw then
   at w RUN-VV {: v:n :}
   v w ;

\ The count cells are the payload's only class that is not one contiguous span,
\ so their zero bytes are summed here, as they are read, rather than rescanned.
: TAKE-CELL ( -- n )
   CUR @ 8 ?RANGE
   CUR @ 8 ZEROS FRAME-ZERO @ + FRAME-ZERO !
   CUR @ U64@  CUR @ 8 + CUR !
   FRAME-CELLS @ 1+ FRAME-CELLS ! ;

: ?BOUND ( n n -- n ) {: v:n cap:n :}
   v 0 < v cap > or if E-ES-WALK throw then
   v ;

: TAKE-RUN ( n -- n ) {: len:n :}
   len 0 < if E-ES-WALK throw then
   CUR @ {: at:n :}
   at len ?RANGE
   at len + len PAD4 + CUR !
   at ;

: TAKE-ROWS ( n n -- n ) {: count:n width:n :}
   count ETEXT-END ?BOUND drop
   count width * TAKE-RUN ;

\ The pool is [len][bytes] entries that exactly fill their declared length; the
\ boot pass validates it the same way before it reads an entry (EM-AOT-VALIDATE).
: POOL-CHECK ( n n -- ) {: at:n len:n :}
   0 ACC !
   begin ACC @ len < while
      at ACC @ + U8@ 1+ ACC @ + ACC !
   repeat
   ACC @ len <> if E-ES-WALK throw then ;

\ The bitmap states no count of present cells, so walking it is what counts
\ them, and their value widths are what the value section that follows is long.
: RUN-BYTES-MEASURE ( -- n )
   0 ACC !  0 RUN-N !  0 RUN-PREV !
   RUN-BYTES @ CELL-BITS * 0 ?do
      RUN0 @ i CELL-BITS / + U8@  i CELL-BITS mod rshift  1 and 0<> if
         i 1+ CELL-BYTES * {: end:n :}
         end DATA-SPAN @ > if E-ES-WALK throw then
         RBYTES0 @ ACC @ +  ETEXT-END RBYTES0 @ ACC @ + -  RUN-V@ {: v:n w:n :}
         ACC @ w + ACC !
         end RUN-PREV !
         RUN-N @ 1+ RUN-N !
      then
   loop
   ACC @ ;

: BOOTRUN-MEASURE ( n -- n ) {: at:n :}
   0 ACC !
   begin
      at ACC @ + U8@ {: len:n :}
      ACC @ 1+ len + ACC !
      len 0=
   until
   ACC @ ;

: TAIL-PAD? ( n -- bool ) {: at:n :}
   at ETEXT-END > if false exit then
   ETEXT-END at - PROT-PAGE-MAX >= if false exit then
   at ETEXT-END LAST-NONZERO at = ;

\ The sidecar's trailing name, mirrored from src/habu/habu2.f EMIT-AOT-SEED
\ (AOT-SIG:INSTALL-NAME$) because it is the payload's only length-free run. The
\ mirror is checked against the image's own bytes, so a renamed installer makes
\ the walk refuse rather than silently mismeasure.
: SIGNAME$ ( -- ptr u8 n ) s" CK-AOT-REG-INSTALL" ;

: IMAGE-AT= ( n n n -- bool ) {: a:n b:n len:n :}
   len 0 ?do
      a i + U8@  b i + U8@ <> if false unloop exit then
   loop
   true ;

: IMAGE-STR= ( n n ptr u8 n -- bool ) {: at:n len:n a:ptr u:n :}
   len u <> if false exit then
   len 0 ?do
      at i + U8@  a i + c@ <> if false unloop exit then
   loop
   true ;

: WALK-AT ( n -- n ) {: start:n :}
   start CUR !  0 FRAME-CELLS !  0 FRAME-ZERO !
   TAKE-CELL ETEXT-END ?BOUND BLOB-LEN !
   BLOB-LEN @ TAKE-RUN BLOB-OFF !
   TAKE-CELL DICT-CAP ?BOUND REC-N !
   REC-N @ AOT-CREC-ROW TAKE-ROWS REC0 !
   TAKE-CELL ETEXT-END ?BOUND SITE-N !
   SITE-N @ SITE-ROW TAKE-ROWS SITE0 !
   TAKE-CELL NAMES-CAP ?BOUND NAMES-LEN !
   NAMES-LEN @ TAKE-RUN NAMES0 !
   NAMES0 @ NAMES-LEN @ POOL-CHECK
   TAKE-CELL DATA-SIZE ?BOUND DATA-SPAN !
   TAKE-CELL DATA-D0 !
   TAKE-CELL ETEXT-END ?BOUND DSITE-N !
   DSITE-N @ 4 TAKE-ROWS DSITE0 !
   TAKE-CELL ETEXT-END ?BOUND XTOFF-N !
   XTOFF-N @ XTOFF-ROW TAKE-ROWS XTOFF0 !
   TAKE-CELL ETEXT-END ?BOUND RUN-BYTES !
   RUN-BYTES @ TAKE-RUN RUN0 !
   CUR @ RBYTES0 !                                \ the values begin where the bitmap ended
   RUN-BYTES-MEASURE RBYTES-LEN !
   RBYTES-LEN @ TAKE-RUN drop
   TAKE-CELL CODE-B0 !
   TAKE-CELL ETEXT-END ?BOUND CSITE-N !
   CSITE-N @ 4 TAKE-ROWS CSITE0 !
   TAKE-CELL ETEXT-END ?BOUND XTSITE-N !
   XTSITE-N @ XTSITE-ROW TAKE-ROWS XTSITE0 !
   TAKE-CELL DICT-CAP ?BOUND SPAN-N !
   SPAN-N @ SPAN-ROW TAKE-ROWS SPAN0 !
   CUR @ BOOTRUN-MEASURE BOOTRUN-LEN !
   BOOTRUN-LEN @ TAKE-RUN BOOTRUN0 !
   TAKE-CELL WID-W0 !
   TAKE-CELL WID-SPAN !
   TAKE-CELL DICT-CAP ?BOUND PWIN-N !
   PWIN-N @ 4 TAKE-ROWS PWIN0 !
   0 SIG-LEN !  0 SIGNAME-LEN !  0 SIG0 !  0 SIGNAME0 !
   CUR @ TAIL-PAD? 0= if
      TAKE-CELL ETEXT-END ?BOUND SIG-LEN !
      SIG-LEN @ TAKE-RUN SIG0 !
      SIGNAME$ nip SIGNAME-LEN !
      SIGNAME-LEN @ TAKE-RUN SIGNAME0 !
      SIGNAME0 @ SIGNAME-LEN @ SIGNAME$ IMAGE-STR= 0= if E-ES-WALK throw then
   then
   CUR @ TAIL-PAD? 0= if E-ES-WALK throw then
   CUR @ ;

: TRY-AT ( n -- bool )
   CAND !
   [: CAND @ WALK-AT AOT-END ! ;] catch 0= ;

\ Cheap refusal before the walk: the first cell must be a blob length that fits.
: CANDIDATE? ( n -- bool ) {: at:n :}
   at 8 + ETEXT-END > if false exit then
   at U64@ {: len:n :}
   len 0 < len ETEXT-END > or if false exit then
   at 8 + len + ETEXT-END <= ;

: FIND-AOT ( n -- ) {: from:n :}
   from begin dup ETEXT-END < while
      dup CANDIDATE? over TRY-AT and if AOT0 ! exit then
      4 +
   repeat drop
   s" image-size: no AOT payload in the image" RC die ;



\ ---- the byte budget -----------------------------------------------------------
\ Every row is (bytes, zero bytes). A baked engine's table prints three columns,
\ exactly as it always has; an application image's prints the zero column too,
\ because for a snapshot that column is the answer. Every row still lands in
\ TOTAL, and no table is printed at all unless TOTAL is the file's own length.
variable TOTAL       variable TOTAL-ZERO
variable ZCOL        \ non-zero while the table carries the zero column
\ The table is what fills the six summary numbers and what proves they sum, so
\ a caller that wants only the summary runs the same rows with the printing
\ suppressed rather than a second, quieter arithmetic that could disagree.
variable QUIET

\ THE SIX NUMBERS THE ONE-LINE SUMMARY CARRIES, and the seventh that makes the
\ line an identity rather than a selection. Every class below declares which one
\ it feeds, so the line and the table can never drift: `other` is whatever no
\ row claimed, and the table itemises it.
0 constant B-OTHER
1 constant B-CODE
2 constant B-NAMES
3 constant B-DATA        \ split by the row's own zero count
4 constant B-PAD
variable BK-CODE   variable BK-NAMES   variable BK-DATA
variable BK-DZERO  variable BK-PAD

: BUCKETS-RESET ( -- )
   0 BK-CODE !  0 BK-NAMES !  0 BK-DATA !  0 BK-DZERO !  0 BK-PAD ! ;

: BUCKET+ ( n n n -- ) {: bytes:n zero:n bucket:n :}
   bucket B-CODE  = if BK-CODE  @ bytes + BK-CODE  ! exit then
   bucket B-NAMES = if BK-NAMES @ bytes + BK-NAMES ! exit then
   bucket B-PAD   = if BK-PAD   @ bytes + BK-PAD   ! exit then
   bucket B-DATA  = if
      BK-DATA @ bytes zero - + BK-DATA !
      BK-DZERO @ zero + BK-DZERO !
   then ;

: BK-OTHER ( -- n )
   TOTAL @ BK-CODE @ - BK-NAMES @ - BK-DATA @ - BK-DZERO @ - BK-PAD @ - ;

: TAB ( -- ) 9 emit ;

\ Tenths of a percent, so a 0.1% row is still visible beside a 35% one.
: SHARE ( n -- ) {: bytes:n :}
   bytes 1000 * ILEN @ / {: tenths:n :}
   tenths 10 / FMT:.U 46 emit  tenths 10 mod FMT:.U ;

: ROW ( ptr u8 n n n n -- ) {: name:ptr nameu:n bytes:n zero:n bucket:n :}
   bytes 0 < if s" image-size: negative section" RC die then
   zero 0 < zero bytes > or if s" image-size: impossible zero count" RC die then
   QUIET @ 0= if
      name nameu type TAB  bytes FMT:.U TAB
      ZCOL @ if zero FMT:.U TAB then
      bytes SHARE cr
   then
   TOTAL @ bytes + TOTAL !
   TOTAL-ZERO @ zero + TOTAL-ZERO !
   bytes zero bucket BUCKET+ ;

: HEADINGS ( -- )
   QUIET @ if exit then
   s" class" type TAB s" bytes" type TAB
   ZCOL @ if s" zero" type TAB then
   s" percent" type cr ;

: TOTAL-ROW ( -- )
   QUIET @ if exit then
   s" total" type TAB TOTAL @ FMT:.U TAB
   ZCOL @ if TOTAL-ZERO @ FMT:.U TAB then
   TOTAL @ SHARE cr ;

: SUMS? ( -- )
   TOTAL @ ILEN @ <> if
      s" image-size: classes do not sum to the file length" RC die
   then ;

: BUDGET-BEGIN ( -- )
   0 TOTAL !  0 TOTAL-ZERO !  BUCKETS-RESET  HEADINGS ;

: PADDED ( n -- n ) dup PAD4 + ;

: DICT-END ( -- n ) PDICT @ PDICT-N @ PREC * + ;
: NAMES-START ( -- n ) PDICT @ 8 - PNAME-BYTES @ - ;

\ The ELF header page, which every image class begins with.
: ELF-ROWS ( -- )
   s" elf/header" 0 ELF-EHDR-BYTES SPAN B-OTHER ROW
   s" elf/program-headers" ELF-EHDR-BYTES PHDR-END ELF-EHDR-BYTES - SPAN B-OTHER ROW
   PHDR-END CODE-OFF LAST-NONZERO {: meta:n :}
   s" elf/dynamic-metadata" PHDR-END meta PHDR-END - SPAN B-OTHER ROW
   s" elf/header-pad" meta CODE-OFF meta - SPAN B-PAD ROW ;

\ The engine's own text: identical in a baked engine and in the engine half a
\ snapshot copies in front of its payloads, which is why ETEXT-END and not
\ TEXT-SIZE bounds the walk that produced these numbers.
: ENGINE-ROWS ( -- )
   s" engine/code" CODE-OFF NAMES-START CODE-OFF - SPAN B-CODE ROW
   s" engine/primitive-names" NAMES-START PNAME-BYTES @ SPAN B-NAMES ROW
   s" engine/primitive-count" PDICT @ 8 - 8 SPAN B-OTHER ROW
   s" engine/primitive-records" PDICT @ PDICT-N @ PREC * SPAN B-NAMES ROW
   s" source/baked" DICT-END AOT0 @ DICT-END - SPAN B-OTHER ROW
   s" aot/framing-cells" FRAME-CELLS @ 8 * FRAME-ZERO @ B-OTHER ROW
   s" aot/code-blob" BLOB-OFF @ BLOB-LEN @ PADDED SPAN B-CODE ROW
   s" aot/dictionary-records" REC0 @ REC-N @ AOT-CREC-ROW * PADDED SPAN B-NAMES ROW
   s" aot/call-sites" SITE0 @ SITE-N @ SITE-ROW * PADDED SPAN B-OTHER ROW
   s" aot/name-pool" NAMES0 @ NAMES-LEN @ PADDED SPAN B-NAMES ROW
   s" aot/data-sites" DSITE0 @ DSITE-N @ 4 * PADDED SPAN B-OTHER ROW
   s" aot/address-cells" XTOFF0 @ XTOFF-N @ XTOFF-ROW * PADDED SPAN B-OTHER ROW
   s" aot/data-cell-bitmap" RUN0 @ RUN-BYTES @ PADDED SPAN B-OTHER ROW
   s" aot/data-cell-values" RBYTES0 @ RBYTES-LEN @ PADDED SPAN B-DATA ROW
   s" aot/code-sites" CSITE0 @ CSITE-N @ 4 * PADDED SPAN B-OTHER ROW
   s" aot/named-code-sites" XTSITE0 @ XTSITE-N @ XTSITE-ROW * PADDED SPAN B-OTHER ROW
   s" aot/code-spans" SPAN0 @ SPAN-N @ SPAN-ROW * PADDED SPAN B-CODE ROW
   s" aot/boot-run-entries" BOOTRUN0 @ BOOTRUN-LEN @ PADDED SPAN B-OTHER ROW
   s" aot/protected-wordlists" PWIN0 @ PWIN-N @ 4 * PADDED SPAN B-OTHER ROW
   \ The sidecar's own count cell is one of the cells above, so this row is the
   \ two runs it frames and nothing else. Adding the cell here as well made the
   \ classes over-count by eight bytes on any image that carries a sidecar, and
   \ the sum-to-length refusal turned that into a walk that would not report.
   s" aot/checker-sidecar" SIG-LEN @ 0 > SIGNAME-LEN @ 0 > or
      if SIG0 @  SIG-LEN @ PADDED SIGNAME-LEN @ PADDED +
      else AOT-END @ 0 then SPAN B-OTHER ROW ;

: RW-ROW ( -- )
   s" container/rw-segment" TEXT-SIZE ILEN @ TEXT-SIZE - SPAN B-OTHER ROW ;

: BUDGET ( -- )
   0 ZCOL !  BUDGET-BEGIN
   ELF-ROWS
   ENGINE-ROWS
   s" image/text-pad" AOT-END @ ETEXT-END AOT-END @ - SPAN B-PAD ROW
   RW-ROW
   SUMS?
   TOTAL-ROW ;

\ ---- the snapshot trailer and its two payloads ---------------------------------
\ src/habu/layout.f owns the trailer's size and every field offset, and is the
\ only place that may state them; src/habu/snap-lib.f SNAP:WRITE-BYTES writes
\ the stream this reads back, src/habu/habu2.f EM-SNAPSHOT-RESTORE restores it
\ and tools/imgdump.f reads the same six fields for its own report. The stream
\ is: the new ELF header page, the donor engine's text content, the region
\ payload (the dictionary slot array then the code band), the DATA window, the
\ trailer, and the RW segment.
\ It is read here, above the dictionary walkers, because the records a snapshot
\ carries are inside that region payload: the cursor below needs its offsets.
variable TRL-OFF     variable NDICT-N
variable REG-LEN     variable DAT-LEN
variable REG-OFF     variable DAT-OFF
variable TBASE                                    \ the writing run's text base

: TRAILER-OFF ( -- n )
   TEXT-SIZE IMAGE-TEXT-TRAILER-ADJ + SNAP-TRL-BYTES - ;

\ The trailer is the last thing inside the authenticated text extent, so a
\ snapshot announces itself at a fixed place and nothing has to be searched for.
: SNAPSHOT? ( -- bool )
   TRAILER-OFF {: trl:n :}
   trl 0 < if false exit then
   trl SNAP-TRL-BYTES IN-IMAGE? 0= if false exit then
   trl U64@ SNAP-MAGIC = ;

: ?TRL ( bool ptr u8 n -- ) {: ok:bool a:ptr u:n :}
   ok if exit then
   a u RC die ;

: READ-TRAILER ( -- )
   TRAILER-OFF TRL-OFF !
   TRL-OFF @ SNAP-TRL-VERSION + U64@ SNAP-FORMAT-VERSION =
      s" image-size: snapshot format version is not the one this engine writes" ?TRL
   TRL-OFF @ SNAP-TRL-TBASE + U64@ TBASE !
   TRL-OFF @ SNAP-TRL-NDICT + U64@ NDICT-N !
   TRL-OFF @ SNAP-TRL-REGLEN + U64@ REG-LEN !
   TRL-OFF @ SNAP-TRL-DATALEN + U64@ DAT-LEN !
   NDICT-N @ 1 >= NDICT-N @ DICT-CAP <= and
      s" image-size: snapshot record count is outside the dictionary" ?TRL
   NDICT-N @ DREC * CFSTK-OFF <=
      s" image-size: snapshot records overflow the dictionary slot array" ?TRL
   \ The bands are this engine's constants, so an image whose region was written
   \ against a different DICT-SIZE is refused by name rather than split at an
   \ offset that means nothing in it.
   REG-LEN @ DICT-SIZE > REG-LEN @ REGION <= and
      s" image-size: region payload does not hold this engine's dictionary band and a code band" ?TRL
   DAT-LEN @ 1 >= DAT-LEN @ DATA-SIZE <= and
      s" image-size: snapshot DATA payload is outside the DATA window" ?TRL
   TRL-OFF @ DAT-LEN @ - REG-LEN @ - {: r:n :}
   r CODE-OFF >=
      s" image-size: snapshot payloads do not fit in front of the trailer" ?TRL
   \ The payloads begin where the DONOR engine's text ended, and an engine
   \ image's text is rounded up to PROT-PAGE-MAX -- which is what turns a
   \ trailer whose lengths were edited into a refusal. Without this, moving
   \ REGLEN by a page slides the boundary through the donor's zero text pad,
   \ where the classes still sum and the table is still wrong.
   r PROT-PAGE-MAX mod 0=
      s" image-size: the snapshot's payloads do not begin on the donor engine's text boundary" ?TRL
   r REG-OFF !
   r REG-LEN @ + DAT-OFF !
   \ The engine half ends where the region payload begins: every baked walker
   \ above is bounded by this and not by the file's own text extent.
   r ETEXT-N ! ;

\ ---- which of the three images is in hand -------------------------------------
\ Read out of the file, never out of this tool's own build: a snapshot says so
\ at a fixed offset, a baked engine carries a counted boot-seeded dictionary,
\ and what has neither is a stripped application, which is then proved by its
\ own blob rather than accepted by elimination. CLASSIFY, which fills this in,
\ is at the bottom of the file with the walkers it chooses between; the class is
\ named here because the record cursor below dispatches on it.
ENUM image-class engine snapshot stripped ;ENUM

1 LAYOUT-BUFFER CLASS-BUF image-class

: CLASS-PTR ( -- ptr image-class ) 0 CLASS-BUF ;

: CLASS! ( image-class -- ) CLASS-PTR ! ;

: SNAPSHOT-CLASS? ( -- bool )
   CLASS-PTR @ MATCH image-class
      engine OF false ENDOF
      snapshot OF true ENDOF
      stripped OF false ENDOF
   ;MATCH ;

\ ---- the shipped dictionary ----------------------------------------------------
\ Compact records, 20 bytes each (src/habu/aot-capture.f ACAP-COMPACT-RECS;
\ src/habu/habu2.f EM-AOT-REGISTER-RECS expands them to 48-byte dictionary
\ records at boot). A package's own row carries its two wordlist ids in the code
\ fields and $FFFFFFFF in the wid field, so the rows also carry the map from a
\ wordlist id to the package that owns it, and to its role.
$FFFFFFFF constant PKG-ROW
0 constant ROLE-GLOBAL
1 constant ROLE-PUBLIC
2 constant ROLE-PRIVATE
3 constant ROLE-UNMAPPED
4 constant ROLE-N

: CREC ( n -- n ) AOT-CREC-ROW * REC0 @ + ;
: CREC-START ( n -- n ) CREC U32@ ;
: CREC-RAW-LEN ( n -- n ) CREC 4 + U32@ ;
: CREC-NAME-OFF ( n -- n ) CREC 8 + U32@ ;
: CREC-META ( n -- n ) CREC 12 + U32@ ;
: CREC-WID ( n -- n ) CREC 16 + U32@ ;
: CREC-PKG? ( n -- bool ) CREC-WID PKG-ROW = ;
: CREC-BYTES ( n -- n ) {: k:n :}
   k CREC-PKG? if 0 exit then
   k CREC-RAW-LEN CODE-SPAN:BYTES ;
: CREC-ADDR? ( n -- bool )
   CREC-META 16 rshift 3 and  DKIND:ADDR 50 rshift = ;

\ A pool entry is [len][bytes] at NAMES0 + the record's name offset.
: POOL-LEN ( n -- n ) NAMES0 @ + U8@ ;
: POOL-TEXT ( n -- n n ) {: off:n :}
   NAMES0 @ off + 1+  off POOL-LEN ;
: POOL-BYTES ( n -- n ) POOL-LEN 1+ ;
: CREC-NAME ( n -- n n ) CREC-NAME-OFF POOL-TEXT ;

\ ---- the live dictionary a snapshot carries ------------------------------------
\ 48-byte records (DREC, src/habu/layout.f), the ones EM-SNAPSHOT-RESTORE maps
\ straight back into the region: slot 0 the code start, slot 1 the raw CODE-SPAN
\ length, [16] the flags cell, [24] the name bytes or a pointer to them, slot 5
\ the wordlist id. src/habu/xref.f names the same slots for a running engine and
\ tools/imgdump.f reads them out of a file exactly like this.
\ A namespace record (wid XREF-NAMESPACE-WL) has no code: it spends slots 0 and 1
\ on the two wordlist ids its package publishes, which is what makes the live
\ records their own wid-to-package map.
: PTR>OFF ( n -- n ) {: p:n :}
   p RBASE-VA >= p RBASE-VA REG-LEN @ + < and if p RBASE-VA - REG-OFF @ + exit then
   p TBASE @ >= p TBASE @ REG-OFF @ CODE-OFF - + < and if
      p TBASE @ - CODE-OFF + exit then
   -1 ;

: LREC ( n -- n ) DREC * REG-OFF @ + ;
: LREC-START ( n -- n ) LREC U64@ ;
: LREC-RAW-LEN ( n -- n ) LREC 8 + U64@ ;
: LREC-FLAGS ( n -- n ) LREC 16 + U64@ ;
: LREC-WID ( n -- n ) LREC 40 + U64@ ;
: LREC-PKG? ( n -- bool ) LREC-WID XREF-NAMESPACE-WL = ;
: LREC-ADDR? ( n -- bool ) LREC-FLAGS DKIND:MASK and DKIND:ADDR = ;

\ An out-of-line name (DNAME-EXT) is a canonical pointer like a code start, so it
\ is found the same way; an inline one is the record's own [24] bytes.
: LREC-NAME ( n -- n n ) {: k:n :}
   k LREC-FLAGS DNAME-EXT and 0<> if k LREC 24 + U64@ PTR>OFF else k LREC 24 + then
   k LREC-FLAGS DNAME-LEN-MASK and ;

\ A record's code start is a pointer, and 0 is not one: it is what an untouched
\ dictionary slot holds, so a record whose start is zero is not a record at all
\ (tools/imgdump.f ENT? refuses the same slot for the same reason). Answering
\ -1 here is what turns a trailer whose NDICT was raised into a refusal, instead
\ of a walk that reads empty slots and charges the engine's header page for them.
: LREC-CODE ( n -- n ) {: k:n :}
   k LREC-PKG? if -1 exit then
   k LREC-START 0= if -1 exit then
   k LREC-START PTR>OFF ;

: LREC-BYTES ( n -- n ) {: k:n :}
   k LREC-PKG? if 0 exit then
   k LREC-RAW-LEN CODE-SPAN:VALID? 0= if
      s" image-size: a live dictionary record carries no code length" RC die
   then
   k LREC-RAW-LEN CODE-SPAN:BYTES ;

\ The highest wordlist id the live records mention. The payload states its span;
\ a region does not, so it is read off the records themselves.
: LWIDS ( -- n )
   0 ACC !
   NDICT-N @ 0 ?do
      i LREC-WID {: w:n :}
      w ACC @ > if w ACC ! then
   loop
   ACC @ 1+ ;

\ ---- one cursor over the two record formats ------------------------------------
\ Everything below reads records through these words and nothing else, so the
\ wid map, the roles, the owner collection and the rankings are written once and
\ answer for a baked engine's compact rows and for a snapshot's live ones alike.
\ Code is answered as a FILE OFFSET by both, which is what lets one CHAIN-VALUE
\ decode an address literal wherever the body lives.
: NRECS ( -- n )
   SNAPSHOT-CLASS? if NDICT-N @ exit then  REC-N @ ;

: REC-PKG? ( n -- bool )
   SNAPSHOT-CLASS? if LREC-PKG? exit then  CREC-PKG? ;

: REC-WID ( n -- n )
   SNAPSHOT-CLASS? if LREC-WID exit then  CREC-WID ;

: REC-PUB-WID ( n -- n )
   SNAPSHOT-CLASS? if LREC-START exit then  CREC-START ;

: REC-PRI-WID ( n -- n )
   SNAPSHOT-CLASS? if LREC-RAW-LEN exit then  CREC-RAW-LEN ;

: NWIDS ( -- n )
   SNAPSHOT-CLASS? if LWIDS exit then  WID-W0 @ WID-SPAN @ + 1+ ;

: REC-CODE ( n -- n ) {: k:n :}
   SNAPSHOT-CLASS? if k LREC-CODE exit then
   k CREC-PKG? if -1 exit then
   BLOB-OFF @ k CREC-START + ;

: REC-BYTES ( n -- n )
   SNAPSHOT-CLASS? if LREC-BYTES exit then  CREC-BYTES ;

: REC-ADDR? ( n -- bool )
   SNAPSHOT-CLASS? if LREC-ADDR? exit then  CREC-ADDR? ;

: REC-NAME ( n -- n n )
   SNAPSHOT-CLASS? if LREC-NAME exit then  CREC-NAME ;

\ The address a DKIND:ADDR body's chain holds for DATA offset 0, and how far the
\ image's DATA reaches. A snapshot's DATA window is mapped at the fixed DATA-VA
\ (src/habu/habu2.f EM-MMAP-DATA-REGION refuses a boot the kernel answered
\ elsewhere), so a live address literal is its offset plus that base; the AOT
\ payload records the base of the run that captured it.
: DATA-AT0 ( -- n )
   SNAPSHOT-CLASS? if DATA-VA exit then  DATA-D0 @ ;

: DATA-REACH ( -- n )
   SNAPSHOT-CLASS? if DAT-LEN @ exit then  DATA-SPAN @ ;

: .NAME ( n -- ) {: k:n :}
   k REC-NAME {: at:n len:n :}
   at 0 < if s" ?" type exit then
   len 0 ?do at i + U8@ emit loop ;

DYNAMIC-BUFFER WROLE n                            \ role of each wordlist id
DYNAMIC-BUFFER WPKG n                             \ the package row that owns it
variable WID-CAP

: WID-OK? ( n -- bool ) {: w:n :}
   w 0 >= w WID-CAP @ < and ;

: BUILD-WID-MAP ( -- )
   NWIDS WID-CAP !
   WID-CAP @ DICT-CAP > if s" image-size: wordlist span outside the dictionary" RC die then
   WID-CAP @ WROLE-RESERVE  WID-CAP @ WPKG-RESERVE
   WID-CAP @ 0 ?do  ROLE-GLOBAL i WROLE !  0 i WPKG !  loop
   NRECS 0 ?do
      i REC-PKG? if
         i REC-PUB-WID {: pub:n :}
         i REC-PRI-WID {: priv:n :}
         \ A generated package (ENUM, SUMTYPE, the type-family products) carries
         \ 0 as its private role: it has no private wordlist. Zero is the GLOBAL
         \ wordlist and belongs to no package, so neither role may claim it.
         pub 0 <> pub WID-OK? and if ROLE-PUBLIC pub WROLE !  i pub WPKG ! then
         priv 0 <> priv WID-OK? and if ROLE-PRIVATE priv WROLE !  i priv WPKG ! then
      then
   loop ;

: REC-ROLE ( n -- n ) {: k:n :}
   k REC-WID {: w:n :}
   w 0= if ROLE-GLOBAL exit then
   w WID-OK? 0= if ROLE-UNMAPPED exit then
   w WROLE @ {: role:n :}
   role ROLE-GLOBAL = if ROLE-UNMAPPED exit then
   role ;

: ROLE-NAME ( n -- ptr u8 n ) {: role:n :}
   role ROLE-GLOBAL = if s" global" exit then
   role ROLE-PUBLIC = if s" package-public" exit then
   role ROLE-PRIVATE = if s" package-private" exit then
   s" unmapped-wordlist" ;

\ Pool entries are deduplicated, so a name's bytes belong to the set of roles
\ that reference it. A call site no longer references one: the build binds its
\ callee to a dictionary index (src/habu/habu2.f EMIT-AOT-SITES), so the names
\ the capture pooled for those callees are carried by nothing and land in the
\ unreferenced row. The bytes that would leave the image with the private
\ records are the entries NOTHING ELSE references, so the mask is per entry.
DYNAMIC-BUFFER PMASK n
1 constant M-GLOBAL
2 constant M-PUBLIC
4 constant M-PRIVATE
8 constant M-SITE
16 constant M-OTHER

: PMASK+ ( n n -- ) {: off:n bit:n :}
   off 0 < off NAMES-LEN @ >= or if exit then
   off PMASK @ bit or  off PMASK ! ;

: ROLE-BIT ( n -- n ) {: role:n :}
   role ROLE-PUBLIC = if M-PUBLIC exit then
   role ROLE-PRIVATE = if M-PRIVATE exit then
   role ROLE-GLOBAL = if M-GLOBAL exit then
   M-OTHER ;

: BUILD-POOL-MASK ( -- )
   NAMES-LEN @ 1+ PMASK-RESERVE
   NAMES-LEN @ 1+ 0 ?do 0 i PMASK ! loop
   REC-N @ 0 ?do
      i CREC-PKG? if i CREC-NAME-OFF M-OTHER PMASK+
      else i CREC-NAME-OFF i REC-ROLE ROLE-BIT PMASK+ then
   loop
   XTSITE-N @ 0 ?do  XTSITE0 @ i XTSITE-ROW * + 4 + U32@ M-SITE PMASK+  loop ;

: POOL-ONLY-BYTES ( n -- n ) {: mask:n :}
   0 ACC !
   0 begin dup NAMES-LEN @ < while
      dup PMASK @ mask = if dup POOL-BYTES ACC @ + ACC ! then
      dup POOL-BYTES +
   repeat drop
   ACC @ ;

\ ---- record census -------------------------------------------------------------
DYNAMIC-BUFFER RCOUNT n
DYNAMIC-BUFFER RCODE n
DYNAMIC-BUFFER RNAME n

: CENSUS-RECORDS ( -- )
   ROLE-N 1+ RCOUNT-RESERVE  ROLE-N 1+ RCODE-RESERVE  ROLE-N 1+ RNAME-RESERVE
   ROLE-N 1+ 0 ?do 0 i RCOUNT !  0 i RCODE !  0 i RNAME ! loop
   REC-N @ 0 ?do
      i CREC-PKG? if ROLE-N else i REC-ROLE then {: slot:n :}
      slot RCOUNT @ 1+ slot RCOUNT !
      slot RCODE @ i CREC-BYTES + slot RCODE !
      slot RNAME @ i CREC-NAME-OFF POOL-BYTES + slot RNAME !
   loop ;

: .CLASS-ROW ( ptr u8 n n -- ) {: name:ptr nameu:n slot:n :}
   name nameu type TAB
   slot RCOUNT @ FMT:.U TAB
   slot RCOUNT @ AOT-CREC-ROW * FMT:.U TAB
   slot RNAME @ FMT:.U TAB
   slot RCODE @ FMT:.U cr ;

: REPORT-RECORDS ( -- )
   cr s" dictionary the image ships" type cr
   s" class" type TAB s" records" type TAB s" record bytes" type TAB
   s" name bytes" type TAB s" code bytes" type cr
   s" global" ROLE-GLOBAL .CLASS-ROW
   s" package-public" ROLE-PUBLIC .CLASS-ROW
   s" package-private" ROLE-PRIVATE .CLASS-ROW
   s" unmapped-wordlist" ROLE-UNMAPPED .CLASS-ROW
   s" package rows" ROLE-N .CLASS-ROW
   s" name pool entries reachable only from private records, bytes " type
   M-PRIVATE POOL-ONLY-BYTES FMT:.U cr
   s" name pool entries reachable only from named code sites, bytes " type
   M-SITE POOL-ONLY-BYTES FMT:.U cr
   s" name pool entries nothing in the image references, bytes " type
   0 POOL-ONLY-BYTES FMT:.U cr ;

\ ---- what the baked call sites bind to ----------------------------------------
\ Every site is (blob offset, target, callee scope). A target with the name tag
\ clear is the callee's index in the dictionary the boot builds - primitives
\ first, then the payload's records - and the boot loads dict[k][0]. A target
\ with the tag set is a name-pool offset the boot resolves in the scope beside
\ it, which is what a partial capture needs for a callee its own payload does
\ not carry (src/habu/habu2.f EMIT-AOT-SITES binds, EM-AOT-PATCH-SITES
\ relocates).
DYNAMIC-BUFFER SMASK n
variable SITE-PRIMS    variable SITE-RECS
variable SITE-NAMED    variable SITE-CALLEES  variable SITE-BAD

: CENSUS-SITES ( -- )
   REC-N @ PDICT-N @ + 1+ SMASK-RESERVE
   REC-N @ PDICT-N @ + 1+ 0 ?do 0 i SMASK ! loop
   0 SITE-PRIMS !  0 SITE-RECS !  0 SITE-NAMED !  0 SITE-CALLEES !  0 SITE-BAD !
   SITE-N @ 0 ?do
      SITE0 @ i SITE-ROW * + 4 + U32@ {: tgt:n :}
      tgt SITE-NAME-TAG and 0<> if
         SITE-NAMED @ 1+ SITE-NAMED !
      else
         tgt {: k:n :}
         k 0 < k REC-N @ PDICT-N @ + >= or if
            SITE-BAD @ 1+ SITE-BAD !
         else
            k PDICT-N @ < if SITE-PRIMS @ 1+ SITE-PRIMS !
            else SITE-RECS @ 1+ SITE-RECS ! then
            k SMASK @ 0= if
               1 k SMASK !  SITE-CALLEES @ 1+ SITE-CALLEES !
            then
         then
      then
   loop ;

: REPORT-SITES ( -- )
   CENSUS-SITES
   SITE-BAD @ 0 > if
      s" image-size: a bound call site names no record in this image" RC die
   then
   cr s" baked call sites" type cr
   s"   sites " type SITE-N @ FMT:.U
   s" , bound to seeded primitives " type SITE-PRIMS @ FMT:.U
   s" , to payload records " type SITE-RECS @ FMT:.U
   s" , left as names " type SITE-NAMED @ FMT:.U cr
   s"   distinct bound callees " type SITE-CALLEES @ FMT:.U cr ;

\ ---- reachability --------------------------------------------------------------
\ The same call graph src/habu/aot-closure.f walks for a stripped application:
\ direct B/BL edges between baked routines, plus the code addresses the payload's
\ own relocation tables name. Each aot/code-spans row is an anonymous graph node,
\ so its edges are followed without making every stripped span a root. Bytes in
\ gaps outside both indices remain root regions, so an edge out of a true gap is
\ never lost and the answer stays a floor.
$7C000000 constant BR-MASK
$14000000 constant BR-OP                          \ B and BL differ only in the link bit
$3FFFFFF constant BR-IMM
$2000000 constant BR-SIGN
$C0000000 constant XTOFF-KIND                     \ 00 CODE, 01 named CODE, 10 DATA
$3FFFFFFF constant XTOFF-VALUE

DYNAMIC-BUFFER RSTART n
DYNAMIC-BUFFER REND n
DYNAMIC-BUFFER RIDX n
DYNAMIC-BUFFER MARK n
DYNAMIC-BUFFER WORK n
variable CODE-N     variable WORK-N     variable REACH-N    variable REACH-CODE
variable REACH-SN   variable REACH-SCODE
variable LO         variable HI         variable IDXV       variable GAP

: BLOB-W32@ ( n -- n ) BLOB-OFF @ + U32@ ;
: SPAN-START ( n -- n ) SPAN-ROW * SPAN0 @ + U32@ ;
: SPAN-BYTES ( n -- n ) SPAN-ROW * SPAN0 @ + 4 + U32@ CODE-SPAN:BYTES ;

\ A graph member is either a dictionary record or an anonymous span row. The
\ latter's ID follows the records. Sorting the combined index puts every body
\ behind the same binary lookup and worklist; neither kind is an implicit root.
: NODE-START ( n -- n ) {: k:n :}
   k REC-N @ < if k CREC-START else k REC-N @ - SPAN-START then ;
: NODE-BYTES ( n -- n ) {: k:n :}
   k REC-N @ < if k CREC-BYTES else k REC-N @ - SPAN-BYTES then ;

: INDEX-ADD ( n -- ) {: k:n :}
   k NODE-START {: at:n :}
   at BLOB-LEN @ > if s" image-size: code starts past the blob" RC die then
   k NODE-BYTES BLOB-LEN @ at - > if
      s" image-size: a code span runs past the blob" RC die
   then
   at 32 lshift k or CODE-N @ RIDX !
   CODE-N @ 1+ CODE-N ! ;

: BUILD-CODE-INDEX ( -- )
   REC-N @ SPAN-N @ + 1+ {: cap:n :}
   cap RSTART-RESERVE cap REND-RESERVE cap RIDX-RESERVE
   cap MARK-RESERVE cap WORK-RESERVE
   0 CODE-N !
   REC-N @ 0 ?do i CREC-PKG? 0= if i INDEX-ADD then loop
   SPAN-N @ 0 ?do REC-N @ i + INDEX-ADD loop
   0 RIDX CODE-N @ [: < ;] SORT:SORT!
   CODE-N @ 0 ?do
      i RIDX @ $FFFFFFFF and {: k:n :}
      k i RIDX !
      k NODE-START dup i RSTART !
      k NODE-BYTES + i REND !
   loop ;

\ The lowest code index whose start is not below off.
: LOWER-BOUND ( n -- n ) {: off:n :}
   0 LO ! CODE-N @ HI !
   begin LO @ HI @ < while
      LO @ HI @ + 2 / {: mid:n :}
      mid RSTART @ off < if mid 1+ LO ! else mid HI ! then
   repeat LO @ ;

: ENTRY-INDEX ( n -- n ) {: off:n :}
   off LOWER-BOUND {: j:n :}
   j CODE-N @ >= if -1 exit then
   j RSTART @ off <> if -1 exit then j ;

: MARK-ONE ( n -- ) {: j:n :}
   j RIDX @ {: k:n :}
   k MARK @ 0<> if exit then
   1 k MARK !
   j WORK-N @ WORK ! WORK-N @ 1+ WORK-N !
   k REC-N @ < if
      REACH-N @ 1+ REACH-N !
      REACH-CODE @ k NODE-BYTES + REACH-CODE !
   else
      REACH-SN @ 1+ REACH-SN !
      REACH-SCODE @ k NODE-BYTES + REACH-SCODE !
   then ;

: MORE-HERE? ( n -- bool ) {: off:n :}
   IDXV @ CODE-N @ >= if false exit then
   IDXV @ RSTART @ off = ;

\ EXPORT aliases can name the same entry, including a record/span pair. Mark
\ all rows at that entry so a stripped alias never looks dead beside live code.
: MARK-ENTRY ( n -- ) {: off:n :}
   off ENTRY-INDEX {: j:n :}
   j 0 < if exit then
   j IDXV !
   begin off MORE-HERE? while IDXV @ MARK-ONE IDXV @ 1+ IDXV ! repeat ;

\ A declared DATA cell or a code literal can point inside a body (quotation,
\ does> clause). Mark every overlapping owner, conservatively; alias spans can
\ have different ends. Direct branches below still resolve exact entries.
: MARK-SPAN ( n -- ) {: off:n :}
   off ENTRY-INDEX 0 >= if off MARK-ENTRY exit then
   off LOWER-BOUND 0 ?do
      off i REND @ < if i RSTART @ MARK-ENTRY then
   loop ;

: SCAN-AT ( n -- ) {: at:n :}
   at BLOB-W32@ {: w:n :}
   w BR-MASK and BR-OP <> if exit then
   w BR-IMM and {: raw:n :}
   raw BR-SIGN and 0<> if raw BR-SIGN 2 * - else raw then {: rel:n :}
   at rel 4 * + {: tgt:n :}
   tgt 0 >= tgt BLOB-LEN @ < and if tgt MARK-ENTRY then ;

: SCAN-SPAN ( n n -- ) {: from:n to:n :}
   from begin dup to < while dup SCAN-AT 4 + repeat drop ;

: SWEEP ( -- )
   begin WORK-N @ 0 > while
      WORK-N @ 1- WORK-N !
      WORK-N @ WORK @ {: j:n :}
      j RSTART @ j REND @ SCAN-SPAN
   repeat ;

\ A four-instruction MOVZ/MOVK chain, the one form an address literal takes
\ (src/habu/aot-closure.f ADDRESS-CHAIN?, src/habu/layout.f SNAP-RELOC, whose
\ two relocation passes rewrite exactly these four immediates). It is read at a
\ FILE OFFSET, so the same decode answers for a chain in the baked code blob and
\ for one in a snapshot's region.
: CHAIN? ( n -- bool ) {: at:n :}
   at 16 IN-IMAGE? 0= if false exit then
   at U32@ $FF800000 and $D2800000 <> if false exit then     \ MOVZ x?, #imm16
   at U32@ $1F and {: rd:n :}
   4 1 ?do
      at i 4 * + U32@ $FF800000 and $F2800000 <> if false unloop exit then
      at i 4 * + U32@ $1F and rd <> if false unloop exit then \ MOVK x?, #imm16, LSL k
   loop
   true ;

: CHAIN-VALUE ( n -- n ) {: at:n :}
   0 ACC !
   4 0 ?do
      at i 4 * + U32@ {: w:n :}
      ACC @  w 5 rshift $FFFF and  w 21 rshift 3 and 16 * lshift  or ACC !
   loop
   ACC @ ;

: ROOT-NAME ( n n -- ) {: at:n len:n :}
   REC-N @ 0 ?do
      i CREC-PKG? 0= if
         i CREC-NAME {: rat:n rlen:n :}
         rlen len = if
            rat at len IMAGE-AT= if i CREC-START MARK-ENTRY then
         then
      then
   loop ;

\ Only bytes outside both the record and stripped-span indices are roots.
\ Scanning an unreachable span here would root all of its callees by mistake.
: SCAN-UNOWNED ( -- )
   0 GAP !
   CODE-N @ 0 ?do
      i RSTART @ {: at:n :}
      at GAP @ > if GAP @ at SCAN-SPAN then
      i REND @ GAP @ max GAP !
   loop
   GAP @ BLOB-LEN @ < if GAP @ BLOB-LEN @ SCAN-SPAN then ;

: ROOT-XTOFF ( n -- ) {: row:n :}
   row 4 + U32@ {: tgt:n :}
   tgt XTOFF-KIND and 0<> if exit then
   tgt XTOFF-VALUE and {: v:n :}
   v 0 > if v 1- MARK-SPAN then ;

: ROOT-CSITE ( n -- ) {: off:n :}
   BLOB-OFF @ off + CHAIN-VALUE CODE-B0 @ - {: at:n :}
   at 0 >= at BLOB-LEN @ < and if at MARK-SPAN then ;

: ROOT-BOOTRUN ( -- )
   0 GAP !
   begin GAP @ BOOTRUN-LEN @ 1- < while
      BOOTRUN0 @ GAP @ + U8@ {: len:n :}
      len 0 > if BOOTRUN0 @ GAP @ + 1+ len ROOT-NAME then
      GAP @ 1+ len + GAP !
   repeat ;

\ The engine's own entry points: the code addresses its DATA cells hold, the
\ code literals inside the blob, the named code sites, and the boot-run entry
\ words the seed calls after relocation.
: ROOTS-ENTRY ( -- )
   XTOFF-N @ 0 ?do XTOFF0 @ i XTOFF-ROW * + ROOT-XTOFF loop
   CSITE-N @ 0 ?do CSITE0 @ i 4 * + U32@ ROOT-CSITE loop
   XTSITE-N @ 0 ?do XTSITE0 @ i XTSITE-ROW * + 4 + U32@ POOL-TEXT ROOT-NAME loop
   ROOT-BOOTRUN
   SCAN-UNOWNED ;

\ The language surface: a program can name any global or package-public word.
: ROOTS-SURFACE ( -- )
   REC-N @ 0 ?do
      i CREC-PKG? 0= if
         i REC-ROLE ROLE-PRIVATE <> if i CREC-START MARK-ENTRY then
      then
   loop ;

: REACH-RESET ( -- )
   REC-N @ SPAN-N @ + 0 ?do 0 i MARK ! loop
   0 WORK-N ! 0 REACH-N ! 0 REACH-CODE !
   0 REACH-SN ! 0 REACH-SCODE ! ;

\ ---- what nothing reaches ------------------------------------------------------
DYNAMIC-BUFFER DEAD-N n                           \ unreachable records per package row
DYNAMIC-BUFFER DEAD-CODE n                        \ their code bytes
DYNAMIC-BUFFER DMASK n                            \ per pool entry: 1 dead ref, 2 live ref
DYNAMIC-BUFFER PROW n                             \ sortable (bytes, package row)
variable PROW-N     variable DROLE-N   variable DROLE-CODE
variable DEAD-TOTAL variable DEAD-BYTES variable DEAD-NAMES
variable DEAD-SN variable DEAD-SCODE
DYNAMIC-BUFFER SPROW n                            \ sortable (bytes, stripped span)
variable SPROW-N
16 constant TOP-ROWS
$FFFF constant ROW-MASK

: DEAD? ( n -- bool ) MARK @ 0= ;

: PKG-ROW-OF ( n -- n ) {: k:n :}
   k REC-WID {: w:n :}
   w WID-OK? 0= if -1 exit then
   w WROLE @ ROLE-GLOBAL = if -1 exit then
   w WPKG @ ;

: DMASK+ ( n n -- ) {: off:n bit:n :}
   off 0 < off NAMES-LEN @ >= or if exit then
   off DMASK @ bit or  off DMASK ! ;

: DEAD-NAME-BYTES ( -- n )
   NAMES-LEN @ 1+ DMASK-RESERVE
   NAMES-LEN @ 1+ 0 ?do 0 i DMASK ! loop
   REC-N @ 0 ?do
      i CREC-PKG? if i CREC-NAME-OFF 2 DMASK+
      else i CREC-NAME-OFF i DEAD? if 1 else 2 then DMASK+ then
   loop
   SITE-N @ 0 ?do SITE0 @ i SITE-ROW * + 4 + U32@ 2 DMASK+ loop
   XTSITE-N @ 0 ?do XTSITE0 @ i XTSITE-ROW * + 4 + U32@ 2 DMASK+ loop
   0 ACC !
   0 begin dup NAMES-LEN @ < while
      dup DMASK @ 1 = if dup POOL-BYTES ACC @ + ACC ! then
      dup POOL-BYTES +
   repeat drop
   ACC @ ;

: COLLECT-DEAD ( -- )
   REC-N @ 1+ DEAD-N-RESERVE  REC-N @ 1+ DEAD-CODE-RESERVE
   REC-N @ 1+ 0 ?do 0 i DEAD-N !  0 i DEAD-CODE ! loop
   0 DEAD-TOTAL !  0 DEAD-BYTES !
   REC-N @ 0 ?do
      i CREC-PKG? 0= i DEAD? and if
         DEAD-TOTAL @ 1+ DEAD-TOTAL !
         DEAD-BYTES @ i CREC-BYTES + DEAD-BYTES !
         i PKG-ROW-OF {: row:n :}
         row 0 >= if
            row DEAD-N @ 1+ row DEAD-N !
            row DEAD-CODE @ i CREC-BYTES + row DEAD-CODE !
         then
      then
   loop ;

: COLLECT-DEAD-SPANS ( -- )
   0 DEAD-SN ! 0 DEAD-SCODE !
   SPAN-N @ 0 ?do
      REC-N @ i + DEAD? if
         DEAD-SN @ 1+ DEAD-SN !
         DEAD-SCODE @ i SPAN-BYTES + DEAD-SCODE !
      then
   loop ;

: BUILD-SPROWS ( -- )
   SPAN-N @ 1+ SPROW-RESERVE 0 SPROW-N !
   SPAN-N @ 0 ?do
      REC-N @ i + DEAD? if
         i SPAN-BYTES 32 lshift i or SPROW-N @ SPROW !
         SPROW-N @ 1+ SPROW-N !
      then
   loop
   0 SPROW SPROW-N @ [: > ;] SORT:SORT! ;

: .SPAN-ROWS ( -- )
   COLLECT-DEAD-SPANS
   s"   spans reachable" type TAB REACH-SN @ FMT:.U TAB
   REACH-SCODE @ FMT:.U s"  code bytes" type cr
   s"   spans unreachable" type TAB DEAD-SN @ FMT:.U TAB
   DEAD-SCODE @ FMT:.U s"  code bytes" type cr
   BUILD-SPROWS
   SPROW-N @ 0= if exit then
   s"   largest unreachable spans (blob offset, bytes, optional sidecar name)" type cr
   SPROW-N @ TOP-ROWS min 0 ?do
      i SPROW @ $FFFFFFFF and {: k:n :}
      s"     " type k SPAN-START FMT:.U TAB k SPAN-BYTES FMT:.U
      k SPAN-START k SPAN-BYTES IMAGE-NAMES:SPAN-NAME$ dup 0 > if TAB type else 2drop then cr
   loop ;

: DEAD-ROLE ( n -- ) {: role:n :}
   0 DROLE-N !  0 DROLE-CODE !
   REC-N @ 0 ?do
      i CREC-PKG? 0= i DEAD? and if
         i REC-ROLE role = if
            DROLE-N @ 1+ DROLE-N !
            DROLE-CODE @ i CREC-BYTES + DROLE-CODE !
         then
      then
   loop ;

: .ROLE-DEAD ( n -- ) {: role:n :}
   role DEAD-ROLE
   DROLE-N @ 0= if exit then
   s"   " type role ROLE-NAME type TAB
   DROLE-N @ FMT:.U TAB  DROLE-N @ AOT-CREC-ROW * FMT:.U TAB
   DROLE-CODE @ FMT:.U cr ;

: BUILD-PROWS ( -- )
   REC-N @ 1+ PROW-RESERVE
   0 PROW-N !
   REC-N @ 0 ?do
      i DEAD-N @ 0 > if
         i DEAD-CODE @ 16 lshift i or  PROW-N @ PROW !
         PROW-N @ 1+ PROW-N !
      then
   loop
   0 PROW PROW-N @ [: > ;] SORT:SORT! ;

: .TOP-PACKAGES ( -- )
   BUILD-PROWS
   PROW-N @ 0= if exit then
   s"   package" type TAB s" records" type TAB s" code bytes" type cr
   PROW-N @ TOP-ROWS min 0 ?do
      i PROW @ {: row:n :}
      row ROW-MASK and {: pkg:n :}
      s"   " type pkg .NAME TAB
      pkg DEAD-N @ FMT:.U TAB  pkg DEAD-CODE @ FMT:.U cr
   loop
   PROW-N @ TOP-ROWS > if
      s"   (" type PROW-N @ TOP-ROWS - FMT:.U s"  more packages)" type cr
   then ;

: REPORT-REACH ( ptr u8 n -- ) {: label:ptr labelu:n :}
   COLLECT-DEAD
   DEAD-NAME-BYTES DEAD-NAMES !
   cr s" reachability from the " type label labelu type s"  roots" type cr
   s"   reachable" type TAB REACH-N @ FMT:.U TAB REACH-CODE @ FMT:.U s"  code bytes" type cr
   s"   unreachable" type TAB DEAD-TOTAL @ FMT:.U TAB DEAD-BYTES @ FMT:.U
   s"  code bytes, " type DEAD-TOTAL @ AOT-CREC-ROW * FMT:.U s"  record bytes, " type
   DEAD-NAMES @ FMT:.U s"  name bytes" type cr
   ROLE-GLOBAL .ROLE-DEAD
   ROLE-PUBLIC .ROLE-DEAD
   ROLE-PRIVATE .ROLE-DEAD
   ROLE-UNMAPPED .ROLE-DEAD
   .TOP-PACKAGES
   .SPAN-ROWS ;

: CENSUS-SURFACE ( -- )
   REACH-RESET  ROOTS-SURFACE  ROOTS-ENTRY  SWEEP
   s" dictionary-surface" REPORT-REACH ;

: CENSUS-ENTRY ( -- )
   REACH-RESET  ROOTS-ENTRY  SWEEP
   s" engine-entry" REPORT-REACH ;

\ ---- who owns the DATA ---------------------------------------------------------
\ Shared by the two image classes that carry DATA at all. A baked engine carries
\ the window as the payload's non-zero runs, each with an (offset, length)
\ header, so a table of cells holding small numbers costs MORE in header rows
\ than in bytes; a snapshot carries the window verbatim, where a cell costs its
\ bytes whatever it holds. tools/data-table-census.f asks this question of a
\ LIVE heap; this asks it of the shipped image, where the cost is real.
\ An owner is a record the definer stamped DKIND:ADDR (create/variable): no
\ other record kind owns DATA. Its address is the MOVZ/MOVK chain its body
\ pushes, read with the same decode the relocation pass uses.
\ THE STAMP IS THE CLAIM AND THE BODY MUST BACK IT. DKIND:ADDR says the body
\ pushes its DATA address and nothing else, and `does>` CLEARS the stamp in the
\ same window it patches the body (src/habu/habu2.f DOESPATCH:EMIT), so a
\ stamped record whose body is not that chain, or whose chain names an address
\ outside this image's DATA, is a record this walk cannot read. Both are refused
\ by name: skipping one silently would charge its table to the owner below it
\ and the table would still add up.
\ AN OWNER IS CHARGED UP TO THE NEXT OWNER, the rule tools/data-table-census.f
\ states for the live heap and for the same reason: `allot` only moves DP, so
\ consecutive bases partition the span exactly. A block allotted after a
\ variable -- a grown arena a pointer cell reaches, heap above the last
\ `create` -- therefore shows up under the name below it, which is a locator,
\ not an accusation.
DYNAMIC-BUFFER DOWNER n                           \ (DATA offset, record) packed, ascending
DYNAMIC-BUFFER DBYTES n                           \ content bytes charged to each owner
DYNAMIC-BUFFER DRUNS n                            \ run rows charged to each owner (engine)
\ What the image spends on the owner BESIDE its content, so that DBYTES + DOVER
\ is the owner's image cost in either class: the bytes the engine's run rows
\ encode to, and the zero bytes a snapshot's window carries verbatim.
DYNAMIC-BUFFER DOVER n
DYNAMIC-BUFFER DCOST n                            \ (cost, owner) packed, for the ranking
variable DOWN-N     variable DCOST-N
variable UNOWNED-BYTES  variable UNOWNED-RUNS  variable UNOWNED-ROWB
16 constant OWNER-SHIFT

: DOWN-OFF ( n -- n ) DOWNER @ OWNER-SHIFT rshift ;
: DOWN-REC ( n -- n ) DOWNER @ ROW-MASK and ;

: COLLECT-OWNERS ( -- )
   NRECS 1+ DOWNER-RESERVE
   0 DOWN-N !
   NRECS 0 ?do
      i REC-PKG? 0= i REC-ADDR? and i REC-BYTES 16 >= and if
         i REC-CODE {: at:n :}
         at 0 < at CHAIN? 0= or if
            s" image-size: a DKIND:ADDR record's body is not the address chain its definer emits" RC die
         then
         at CHAIN-VALUE DATA-AT0 - {: off:n :}
         off 0 < off DATA-REACH >= or if
            s" image-size: a DKIND:ADDR record names an address outside this image's DATA" RC die
         then
         off OWNER-SHIFT lshift i or  DOWN-N @ DOWNER !
         DOWN-N @ 1+ DOWN-N !
      then
   loop
   0 DOWNER DOWN-N @ [: < ;] SORT:SORT!
   DOWN-N @ 1+ DBYTES-RESERVE  DOWN-N @ 1+ DRUNS-RESERVE
   DOWN-N @ 1+ DOVER-RESERVE
   DOWN-N @ 1+ 0 ?do 0 i DBYTES !  0 i DRUNS !  0 i DOVER ! loop ;

\ The last owner at or below off, or -1 when the run starts below every owner.
: OWNER-AT ( n -- n ) {: off:n :}
   0 LO !  DOWN-N @ HI !
   begin LO @ HI @ < while
      LO @ HI @ + 2 / {: mid:n :}
      mid DOWN-OFF off > if mid HI ! else mid 1+ LO ! then
   repeat
   LO @ 1- ;

: OWNER-END ( n -- n ) {: k:n :}
   k 1+ DOWN-N @ >= if DATA-REACH exit then
   k 1+ DOWN-OFF ;

\ A CELL BELONGS TO ONE OWNER, the one its own offset lands on, and its value
\ bytes go there whole: a cell is eight bytes and an owner is a `create` base,
\ so no cell straddles two of them.
: CHARGE-CELL ( n n -- ) {: off:n w:n :}
   off OWNER-AT {: k:n :}
   k 0 < if
      UNOWNED-RUNS @ 1+ UNOWNED-RUNS !  UNOWNED-BYTES @ w + UNOWNED-BYTES !
      exit
   then
   k DRUNS @ 1+ k DRUNS !
   k DBYTES @ w + k DBYTES ! ;

\ A BITMAP BYTE IS CHARGED WHOLE, to the owner its first cell lands on: the
\ eight cells it covers cost one byte however many of them are present, and
\ splitting that byte eight ways would report bits as bytes.
: CHARGE-BM-BYTE ( n n -- ) {: off:n bytes:n :}
   off OWNER-AT {: k:n :}
   k 0 < if UNOWNED-ROWB @ bytes + UNOWNED-ROWB ! exit then
   k DOVER @ bytes + k DOVER ! ;

: CHARGE-RUNS ( -- )
   0 UNOWNED-BYTES !  0 UNOWNED-RUNS !  0 UNOWNED-ROWB !
   0 ACC !
   RUN-BYTES @ CELL-BITS * 0 ?do
      i CELL-BITS mod 0= if i CELL-BYTES * 1 CHARGE-BM-BYTE then
      RUN0 @ i CELL-BITS / + U8@  i CELL-BITS mod rshift  1 and 0<> if
         RBYTES0 @ ACC @ +  ETEXT-END RBYTES0 @ ACC @ + -  RUN-V@ {: v:n w:n :}
         i CELL-BYTES * w CHARGE-CELL
         ACC @ w + ACC !
      then
   loop ;

: OWNER-COST ( n -- n ) {: k:n :}
   k DBYTES @  k DOVER @ + ;

: RANK-OWNERS ( -- )
   DOWN-N @ 1+ DCOST-RESERVE
   0 DCOST-N !
   DOWN-N @ 0 ?do
      i OWNER-COST {: cost:n :}
      cost 0 > if
         cost OWNER-SHIFT lshift i or  DCOST-N @ DCOST !
         DCOST-N @ 1+ DCOST-N !
      then
   loop
   0 DCOST DCOST-N @ [: > ;] SORT:SORT! ;

\ Every run byte, every row and every byte those rows encode to lands on exactly
\ one owner or on the unowned head, so the charges add up to the payload's own
\ three numbers.
: CHECK-CHARGES ( -- )
   0 ACC !
   DOWN-N @ 0 ?do ACC @ i DBYTES @ + ACC ! loop
   ACC @ UNOWNED-BYTES @ + RBYTES-LEN @ <> if
      s" image-size: DATA cell values do not add up" RC die
   then
   0 ACC !
   DOWN-N @ 0 ?do ACC @ i DRUNS @ + ACC ! loop
   ACC @ UNOWNED-RUNS @ + RUN-N @ <> if
      s" image-size: DATA present cells do not add up" RC die
   then
   0 ACC !
   DOWN-N @ 0 ?do ACC @ i DOVER @ + ACC ! loop
   ACC @ UNOWNED-ROWB @ + RUN-BYTES @ <> if
      s" image-size: DATA bitmap bytes do not add up" RC die
   then ;

: REPORT-DATA ( -- )
   COLLECT-OWNERS  CHARGE-RUNS  CHECK-CHARGES  RANK-OWNERS
   cr s" captured DATA heap: " type DATA-SPAN @ FMT:.U
   s"  bytes of span, " type RUN-N @ FMT:.U s"  present cells in " type
   RUN-BYTES @ FMT:.U s"  bitmap bytes, " type
   RUN-BYTES @ RBYTES-LEN @ + FMT:.U s"  bytes of image" type cr
   s"   owners " type DOWN-N @ FMT:.U
   s" , unowned value bytes " type UNOWNED-BYTES @ FMT:.U
   s" , unowned cells " type UNOWNED-RUNS @ FMT:.U cr
   s"   owner" type TAB s" offset" type TAB s" extent" type TAB s" cells" type TAB
   s" bytes" type TAB s" image cost" type cr
   DCOST-N @ TOP-ROWS min 0 ?do
      i DCOST @ ROW-MASK and {: k:n :}
      s"   " type k DOWN-REC .NAME TAB
      k DOWN-OFF FMT:.U TAB  k OWNER-END k DOWN-OFF - FMT:.U TAB
      k DRUNS @ FMT:.U TAB  k DBYTES @ FMT:.U TAB  k OWNER-COST FMT:.U cr
   loop
   DCOST-N @ TOP-ROWS > if
      s"   (" type DCOST-N @ TOP-ROWS - FMT:.U s"  more owners)" type cr
   then ;


\ ---- a --repl application: the code band, by package ---------------------------
\ The band holds two different things: the code of every word the image compiled
\ into its region, and the out-of-line bytes of every name too long to sit in a
\ record (src/habu/habu2.f C-STORE-NAME and DOES-REC:COPY-NAME both write theirs
\ at CP, padded to a word), so the two are separated before either is attributed.
\ EVERY BAND BYTE IS CHARGED AT MOST ONCE. The spans are sorted and walked in
\ order, and a record is charged the bytes its own span covers THAT NO EARLIER
\ SPAN ALREADY COVERED: an EXPORT alias and a `does>` clause put a second record
\ over ground the first one already answers for, and charging both would report
\ more code than the band holds. What no span covers is the `unowned` row --
\ stored quotation bodies, hidden bodies, and the code a definition abandoned
\ where it stood (src/habu/snap-lib.f says the retained region carries them).
DYNAMIC-BUFFER BSPAN n                            \ (band offset, kind, record), ascending
DYNAMIC-BUFFER PKGB n                             \ band code bytes per package row
DYNAMIC-BUFFER PKGN n                             \ records charged to it
DYNAMIC-BUFFER PCOST n                            \ (bytes, package row), for the ranking
variable BSPAN-N    variable PCOST-N   variable COVER
variable BAND-CODE  variable BAND-CZERO
variable BAND-NAMES variable BAND-NZERO
variable BAND-FREE  variable BAND-FZERO
variable PKG-GLOBAL variable PKG-UNMAPPED
variable SHARED-N   variable SHARED-BYTES
variable TEXT-RECS  variable TEXT-NAMES variable BAND-RECS
17 constant BAND-SHIFT                            \ a band offset fits above the two tag fields
$10000 constant BAND-NAME                         \ the row is a name, not code

: BAND0 ( -- n ) REG-OFF @ DICT-SIZE + ;
: BAND-END ( -- n ) REG-OFF @ REG-LEN @ + ;
: BAND-BYTES ( -- n ) BAND-END BAND0 - ;
: IN-BAND? ( n -- bool ) {: at:n :} at BAND0 >= at BAND-END < and ;

: BSPAN-OFF ( n -- n ) BSPAN @ BAND-SHIFT rshift ;
: BSPAN-REC ( n -- n ) BSPAN @ ROW-MASK and ;
: BSPAN-NAME? ( n -- bool ) BSPAN @ BAND-NAME and 0<> ;

: BSPAN+ ( n n n -- ) {: at:n k:n kind:n :}
   at BAND0 - BAND-SHIFT lshift kind or k or  BSPAN-N @ BSPAN !
   BSPAN-N @ 1+ BSPAN-N ! ;

: BSPAN-LEN ( n -- n ) {: j:n :}
   j BSPAN-REC {: k:n :}
   j BSPAN-NAME? if k REC-NAME nip PADDED exit then
   k REC-BYTES ;

: ?FITS ( n n ptr u8 n -- ) {: at:n len:n a:ptr u:n :}
   at len + BAND-END > if a u RC die then ;

\ A record's code and its name each live in the band, in the donor engine's own
\ text, or -- if the pointer maps nowhere at all -- in an image this walk will
\ not report on.
: COLLECT-BAND ( -- )
   NRECS 2 * 2 + BSPAN-RESERVE
   0 BSPAN-N !  0 TEXT-RECS !  0 TEXT-NAMES !  0 BAND-RECS !
   NRECS 0 ?do
      i REC-PKG? 0= if
         i REC-CODE {: at:n :}
         at 0 < if
            s" image-size: a dictionary record's code is not in this image" RC die
         then
         at IN-BAND? if
            BAND-RECS @ 1+ BAND-RECS !
            at i REC-BYTES s" image-size: a dictionary record's code runs past the region payload" ?FITS
            i REC-BYTES 0 > if at i 0 BSPAN+ then
         else TEXT-RECS @ 1+ TEXT-RECS ! then
      then
      i REC-NAME {: nat:n nlen:n :}
      nat 0 < if
         s" image-size: a dictionary record's name is not in this image" RC die
      then
      nat IN-BAND? if
         nat nlen PADDED s" image-size: a record name runs past the region payload" ?FITS
         nlen 0 > if nat i BAND-NAME BSPAN+ then
      else nat CODE-OFF >= nat REG-OFF @ < and if TEXT-NAMES @ 1+ TEXT-NAMES ! then then
   loop
   0 BSPAN BSPAN-N @ [: < ;] SORT:SORT! ;

: CHARGE-PKG ( n n -- ) {: k:n got:n :}
   k REC-ROLE {: role:n :}
   role ROLE-GLOBAL = if PKG-GLOBAL @ got + PKG-GLOBAL ! exit then
   role ROLE-UNMAPPED = if PKG-UNMAPPED @ got + PKG-UNMAPPED ! exit then
   k PKG-ROW-OF {: row:n :}
   row 0 < if PKG-UNMAPPED @ got + PKG-UNMAPPED ! exit then
   row PKGB @ got + row PKGB !
   row PKGN @ 1+ row PKGN ! ;

: CHARGE-GAP ( n n -- ) {: from:n to:n :}
   to from <= if exit then
   BAND-FREE @ to from - + BAND-FREE !
   BAND-FZERO @ BAND0 from + to from - ZEROS + BAND-FZERO ! ;

: CHARGE-SPAN ( n -- ) {: j:n :}
   j BSPAN-OFF {: off:n :}
   j BSPAN-LEN {: len:n :}
   off COVER @ max {: from:n :}
   off len + {: end:n :}
   end from <= if
      SHARED-N @ 1+ SHARED-N !  SHARED-BYTES @ len + SHARED-BYTES !  exit
   then
   end from - {: got:n :}
   got len < if SHARED-N @ 1+ SHARED-N !  SHARED-BYTES @ len got - + SHARED-BYTES ! then
   j BSPAN-NAME? if
      BAND-NAMES @ got + BAND-NAMES !
      BAND-NZERO @ BAND0 from + got ZEROS + BAND-NZERO !
   else
      BAND-CODE @ got + BAND-CODE !
      BAND-CZERO @ BAND0 from + got ZEROS + BAND-CZERO !
      j BSPAN-REC got CHARGE-PKG
   then
   end COVER ! ;

: CHARGE-BAND ( -- )
   NRECS 1+ PKGB-RESERVE  NRECS 1+ PKGN-RESERVE
   NRECS 1+ 0 ?do 0 i PKGB !  0 i PKGN ! loop
   0 BAND-CODE !  0 BAND-CZERO !  0 BAND-NAMES !  0 BAND-NZERO !
   0 BAND-FREE !  0 BAND-FZERO !  0 PKG-GLOBAL !  0 PKG-UNMAPPED !
   0 SHARED-N !  0 SHARED-BYTES !  0 COVER !
   BSPAN-N @ 0 ?do
      COVER @ i BSPAN-OFF CHARGE-GAP
      i CHARGE-SPAN
   loop
   COVER @ BAND-BYTES CHARGE-GAP ;

\ The three classes partition the band, and so do their zero bytes: both are
\ checked against the band's own length and its own zero count, so a span this
\ walk placed twice or missed cannot reach the table.
: CHECK-BAND ( -- )
   0 ACC !
   NRECS 0 ?do ACC @ i PKGB @ + ACC ! loop
   ACC @ PKG-GLOBAL @ + PKG-UNMAPPED @ + BAND-CODE @ <> if
      s" image-size: band code does not add up to its packages" RC die
   then
   BAND-CODE @ BAND-NAMES @ + BAND-FREE @ + BAND-BYTES <> if
      s" image-size: region band charges do not add up" RC die
   then
   BAND-CZERO @ BAND-NZERO @ + BAND-FZERO @ + BAND0 BAND-BYTES ZEROS <> if
      s" image-size: region band zero bytes do not add up" RC die
   then ;

: RANK-PKGS ( -- )
   NRECS 1+ PCOST-RESERVE
   0 PCOST-N !
   NRECS 0 ?do
      i PKGB @ 0 > if
         i PKGB @ BAND-SHIFT lshift i or  PCOST-N @ PCOST !
         PCOST-N @ 1+ PCOST-N !
      then
   loop
   0 PCOST PCOST-N @ [: > ;] SORT:SORT! ;

: REPORT-BAND ( -- )
   cr s" the region code band by package" type cr
   s"   band " type BAND-BYTES FMT:.U s"  bytes: code " type BAND-CODE @ FMT:.U
   s" , out-of-line names " type BAND-NAMES @ FMT:.U
   s" , unowned " type BAND-FREE @ FMT:.U cr
   s"   records with code in the band " type BAND-RECS @ FMT:.U
   s" , in the donor engine's text " type TEXT-RECS @ FMT:.U
   s" , names there " type TEXT-NAMES @ FMT:.U cr
   s"   package" type TAB s" records" type TAB s" code bytes" type cr
   PCOST-N @ TOP-ROWS min 0 ?do
      i PCOST @ ROW-MASK and {: row:n :}
      s"   " type row .NAME TAB  row PKGN @ FMT:.U TAB  row PKGB @ FMT:.U cr
   loop
   PCOST-N @ TOP-ROWS > if
      s"   (" type PCOST-N @ TOP-ROWS - FMT:.U s"  more packages)" type cr
   then
   s"   global-wordlist words " type PKG-GLOBAL @ FMT:.U s"  bytes" type
   PKG-UNMAPPED @ 0 > if
      s" , unmapped wordlists " type PKG-UNMAPPED @ FMT:.U s"  bytes" type
   then cr
   SHARED-N @ 0 > if
      s"   " type SHARED-N @ FMT:.U
      s"  spans (" type SHARED-BYTES @ FMT:.U
      s"  bytes) were already covered by the record below them" type cr
   then ;

\ ---- a --repl application: the DATA window, by owner ---------------------------
\ Verbatim is what makes this simple: the window is written as it stood, so an
\ owner's image cost IS its extent, and the only question left is how much of
\ that extent carries anything. No run rows to charge, and the engine's
\ three-way sum becomes a two-way one.
variable HEAD-W     variable HEAD-Z

: CHARGE-WINDOW ( -- )
   DOWN-N @ 0 > if 0 DOWN-OFF else DAT-LEN @ then {: head:n :}
   DAT-OFF @ head ZEROS {: hz:n :}
   hz HEAD-Z !  head hz - HEAD-W !
   DOWN-N @ 0 ?do
      i OWNER-END i DOWN-OFF - {: ext:n :}
      ext 0 < if s" image-size: DATA owners do not advance" RC die then
      DAT-OFF @ i DOWN-OFF + ext ZEROS {: z:n :}
      ext z - i DBYTES !  z i DOVER !
   loop ;

: CHECK-WINDOW ( -- )
   0 ACC !
   DOWN-N @ 0 ?do ACC @ i DBYTES @ + i DOVER @ + ACC ! loop
   ACC @ HEAD-W @ + HEAD-Z @ + DAT-LEN @ <> if
      s" image-size: DATA owner extents do not add up" RC die
   then ;

: .OWNER-PKG ( n -- ) {: k:n :}
   k REC-ROLE ROLE-GLOBAL = if s" -" type exit then
   k PKG-ROW-OF {: row:n :}
   row 0 < if s" -" type exit then
   row .NAME ;

: REPORT-WINDOW ( -- )
   cr s" the DATA window by owner" type cr
   s"   owners " type DOWN-N @ FMT:.U
   s" , below the first one " type HEAD-W @ HEAD-Z @ + FMT:.U s"  bytes (" type
   HEAD-Z @ FMT:.U s"  zero)" type cr
   s"   owner" type TAB s" package" type TAB s" offset" type TAB s" extent" type TAB
   s" written" type TAB s" zero" type cr
   DCOST-N @ TOP-ROWS min 0 ?do
      i DCOST @ ROW-MASK and {: k:n :}
      s"   " type k DOWN-REC .NAME TAB  k DOWN-REC .OWNER-PKG TAB
      k DOWN-OFF FMT:.U TAB  k OWNER-END k DOWN-OFF - FMT:.U TAB
      k DBYTES @ FMT:.U TAB  k DOVER @ FMT:.U cr
   loop
   DCOST-N @ TOP-ROWS > if
      s"   (" type DCOST-N @ TOP-ROWS - FMT:.U s"  more owners)" type cr
   then ;

\ A class that has no region and no DATA owners must not answer with the last
\ image's numbers: one process measures several images (tools/hb-build-test.f
\ measures two), so every accumulator this section publishes starts at zero for
\ each of them.
: ATTRIB-RESET ( -- )
   0 BAND-CODE !  0 BAND-CZERO !  0 BAND-NAMES !  0 BAND-NZERO !
   0 BAND-FREE !  0 BAND-FZERO !  0 PKG-GLOBAL !  0 PKG-UNMAPPED !
   0 SHARED-N !  0 SHARED-BYTES !  0 TEXT-RECS !  0 TEXT-NAMES !
   0 BAND-RECS !  0 BSPAN-N !  0 PCOST-N !
   0 DOWN-N !  0 DCOST-N !  0 HEAD-W !  0 HEAD-Z ! ;

\ Both attributions, run before the table because the band's rows are their
\ result; the two reports below print what this computed.
: SNAP-ATTRIBUTE ( -- )
   BUILD-WID-MAP
   COLLECT-BAND  CHARGE-BAND  CHECK-BAND  RANK-PKGS
   COLLECT-OWNERS  CHARGE-WINDOW  CHECK-WINDOW  RANK-OWNERS ;

: REGION-ROWS ( -- )
   NDICT-N @ DREC * {: recs:n :}
   s" region/dict-records" REG-OFF @ recs SPAN B-NAMES ROW
   s" region/dict-unused" REG-OFF @ recs + CFSTK-OFF recs - SPAN B-NAMES ROW
   s" region/cf-stack" REG-OFF @ CFSTK-OFF + DICT-SIZE CFSTK-OFF - SPAN B-OTHER ROW
   s" region/record-names" BAND-NAMES @ BAND-NZERO @ B-NAMES ROW
   s" region/code-band" BAND-CODE @ BAND-CZERO @ B-CODE ROW
   s" region/code-unowned" BAND-FREE @ BAND-FZERO @ B-CODE ROW ;

: SNAP-BUDGET ( -- )
   -1 ZCOL !  BUDGET-BEGIN
   ELF-ROWS
   ENGINE-ROWS
   s" engine/text-pad" AOT-END @ ETEXT-END AOT-END @ - SPAN B-PAD ROW
   REGION-ROWS
   s" data/window" DAT-OFF @ DAT-LEN @ SPAN B-DATA ROW
   s" snapshot/trailer" TRL-OFF @ SNAP-TRL-BYTES SPAN B-OTHER ROW
   RW-ROW
   SUMS?
   TOTAL-ROW ;

: SNAP-NOTES ( -- )
   cr s" the application's own half: " type
   REG-LEN @ DAT-LEN @ + SNAP-TRL-BYTES + FMT:.U s"  bytes, " type
   NDICT-N @ FMT:.U s"  dictionary records" type cr
   REPORT-BAND
   REPORT-WINDOW ;

\ ---- a stripped application ---------------------------------------------------
\ src/habu/aot-lib.f LINK emits, in this order: the startup entry, the closure
\ of MAIN, the crash and signal handlers, the sparse DATA blob, and one 8-byte
\ relocation row per declared address cell. Nothing in the file frames that last
\ pair, so the walk takes the emitter's own two statements about them.
\
\ THE BLOB is named by the startup's single code-base-relative address into x9
\ (EMIT-DATA-COPY loads the sparse header there through src/habu/aot-lib.f
\ TEXT-ADR,; test/gate-aot-image.f already admits exactly one such sequence in
\ the startup), and the blob then frames itself: a u32 of encoded row bytes,
\ that many varint (gap, length) rows, then the bytes those rows describe. The
\ format is the AOT-WINDOW row (src/habu/aot-decl.f), so RUN-V@ above decodes it.
\
\ THE ROW COUNT is the MOVZ/MOVK chain EMIT-XT-CELLS loads into x11, found
\ behind the three-instruction idiom that rounds the byte cursor up to the rows'
\ four-byte boundary and confirmed by the `ADR x12` that follows it and must
\ name this image's own code base.
\ DO NOT REPLACE THIS WITH THE LAST NON-ZERO BYTE. A row is (u32 location, u32
\ target) and a target below 65,536 leaves the row's last two bytes zero: the
\ fixture image in docs/engine-size.md has exactly one row, target 1,252, and a
\ last-non-zero scan ends the content two bytes early and loses eight bytes of
\ the file to the pad. The count is read, never inferred.
variable BLOB-AT     variable BLOB-ROWB   variable BLOB-RUNS
variable BLOB-CARRIED  variable BLOB-SPAN variable BLOB-STOP
variable RELOC-AT    variable RELOC-N     variable APP-END
variable BLOBADR-N   variable BLOBADR-PRE
variable IP          variable MOVACC

4 constant INSN

: INSN@ ( n -- n ) U32@ ;

\ Every instruction shape this file reads is named in tools/aot-startup-shape.f,
\ which writes them with the emitter's own encoders; this file finds them in an
\ image, and keeps its own bounds check in front of every fetch.
: ADR-AT? ( n n -- bool ) {: at:n rd:n :}
   at INSN IN-IMAGE? 0= if false exit then
   at INSN@ rd AOT-STARTUP-SHAPE:ADR-RD? ;

\ Does a sparse blob start here? Its own header and bitmap have to walk to a
\ value payload that ends inside the text, which no run of code bytes does by
\ accident.
variable ROWS-END

\ One pass of the AOT-WINDOW bitmap, throwing the walk's own refusal on a cell
\ the format cannot express, so a candidate offset that is not a blob comes back
\ as a failed walk rather than as an exit.
: BLOB-ROWS-WALK ( -- )
   0 ACC !  0 RUN-N !  0 RUN-PREV !
   ROWS-END @ RUN-AT @ - {: bm:n :}
   bm CELL-BITS * 0 ?do
      RUN-AT @ i CELL-BITS / + U8@  i CELL-BITS mod rshift  1 and 0<> if
         i 1+ CELL-BYTES * {: end:n :}
         end DATA-SIZE > if E-ES-WALK throw then
         ROWS-END @ ACC @ +  TEXT-SIZE ROWS-END @ ACC @ + -  RUN-V@ {: v:n w:n :}
         ACC @ w + ACC !
         end RUN-PREV !
         RUN-N @ 1+ RUN-N !
      then
   loop ;

: BLOB-AT? ( n -- bool ) {: at:n :}
   at 4 IN-IMAGE? 0= if false exit then
   at U32@ {: rowb:n :}
   rowb 0 < rowb TEXT-SIZE > or if false exit then
   at 4 + rowb + {: rows-end:n :}
   rows-end TEXT-SIZE > if false exit then
   rows-end ROWS-END !  at 4 + RUN-AT !
   [: BLOB-ROWS-WALK ;] catch 0<> if false exit then
   rows-end ACC @ + TEXT-SIZE > if false exit then
   rowb BLOB-ROWB !  ACC @ BLOB-CARRIED !  RUN-N @ BLOB-RUNS !
   RUN-PREV @ BLOB-SPAN !
   rows-end ACC @ + BLOB-STOP !
   true ;

\ THE BLOB'S ADDRESS IS FOUR WORDS, not one ADR: the startup sits at text offset
\ zero and the blob is placed after all code, which ADR's +-1 MiB cannot reach in
\ a large program, so src/habu/aot-lib.f TEXT-ADR, emits the label's byte offset
\ from the code base in a movz/movk pair, `adr x12` to the base itself (LTEXT,
\ bound at text offset zero = CODE-OFF) and the add that joins them. All four are
\ matched because the movz alone is also how EMIT-OWNED-CELLS opens a DATA-offset
\ literal; only the whole sequence names a label. EMIT-DATA-COPY loads the blob
\ header into x9, and that is the only site of the four that uses it.
: TEXT-ADR9? ( n -- bool ) {: at:n :}
   at 4 INSN * IN-IMAGE? 0= if false exit then
   at INSN@  at INSN + INSN@  at 2 INSN * + INSN@  at 3 INSN * + INSN@
   at CODE-OFF 9 AOT-STARTUP-SHAPE:TEXT-ADR-SEQ? ;

: TEXT-ADR9-TARGET ( n -- n ) {: at:n :}
   at INSN@  at INSN + INSN@ AOT-STARTUP-SHAPE:TEXT-ADR-OFFSET  CODE-OFF + ;

\ The first such sequence whose target walks as a blob is the startup's, because
\ the startup is the first thing emitted. Every one of them is counted as well,
\ so an image that HAS one and whose blob does not walk is refused instead of
\ falling back to the code-only shape and quietly absorbing the blob into
\ app/code: a fallback that still sums is a wrong answer wearing the identity's
\ clothes.
: FIND-BLOB ( -- )
   -1 BLOB-AT !  0 BLOBADR-N !
   CODE-OFF IP !
   begin IP @ INSN + TEXT-SIZE <= while
      IP @ TEXT-ADR9? if
         BLOBADR-N @ 1+ BLOBADR-N !
         BLOB-AT @ 0 < if
            IP @ TEXT-ADR9-TARGET {: t:n :}
            t IP @ > t 3 and 0= and t BLOB-AT? and if t BLOB-AT ! then
         then
      then
      IP @ INSN + IP !
   repeat
   BLOB-AT @ 0 < if
      BLOBADR-N @ 0 > if
         s" image-size: this image's code base + offset names no sparse DATA blob" RC die
      then
      exit
   then
   \ ... and in the startup it is the ONLY one, which is what makes the first
   \ one the answer rather than the first of several candidates. The other three
   \ TEXT-ADR, sites load x11, not x9.
   0 BLOBADR-PRE !
   CODE-OFF IP !
   begin IP @ INSN + BLOB-AT @ <= while
      IP @ TEXT-ADR9? if BLOBADR-PRE @ 1+ BLOBADR-PRE ! then
      IP @ INSN + IP !
   repeat
   BLOBADR-PRE @ 1 <> if
      s" image-size: stripped image startup does not hold exactly one blob address" RC die
   then ;


\ ADD x10,x10,#3 / LSR x10,x10,#2 / LSL x10,x10,#2: the byte cursor rounded up
\ to the four-byte boundary the rows sit on (src/habu/aot-lib.f EMIT-XT-CELLS).
$91000D4A constant XTC-ADD3
$D342FD4A constant XTC-LSR2
$D37EF54A constant XTC-LSL2

: XTC-SIG? ( n -- bool ) {: at:n :}
   at 3 INSN * IN-IMAGE? 0= if false exit then
   at INSN@ XTC-ADD3 =
   at INSN + INSN@ XTC-LSR2 = and
   at 2 INSN * + INSN@ XTC-LSL2 = and ;

\ The LIT64, chain by which src/habu/aot-lib.f EMIT-XT-CELLS puts the row count
\ in x11, then the ADR x12 that must follow it and name this image's code base
\ (EMIT-XT-CELLS' own scratch, a plain ADR, and not a TEXT-ADR, site): two
\ independent statements about the same place, so a coincidental three-word
\ match cannot be read as a row count.
: XTC-COUNT ( n -- n ) {: at:n :}
   at 3 INSN * + IP !
   IP @ INSN@ 11 AOT-STARTUP-SHAPE:MOVZ-RD? 0= if -1 exit then
   IP @ INSN@ AOT-STARTUP-SHAPE:MOVW-CHUNK MOVACC !
   IP @ INSN + IP !
   begin IP @ INSN IN-IMAGE?
         IP @ INSN@ 11 AOT-STARTUP-SHAPE:MOVK-RD? and while
      MOVACC @ IP @ INSN@ AOT-STARTUP-SHAPE:MOVW-CHUNK or MOVACC !
      IP @ INSN + IP !
   repeat
   IP @ 12 ADR-AT? 0= if -1 exit then
   IP @ INSN@ IP @ AOT-STARTUP-SHAPE:ADR-TARGET CODE-OFF <> if -1 exit then
   MOVACC @ ;

\ A match that does not decode is not the block -- three instructions can occur
\ anywhere -- so the scan goes on rather than refusing. An image whose real
\ block this misses does not report a wrong number either: its rows are
\ non-zero bytes past where the walk then ends the content, and STRIP-WALK's
\ zero-tail check refuses by name.
: FIND-RELOC ( -- )
   0 RELOC-N !
   CODE-OFF IP !
   begin IP @ 3 INSN * + BLOB-AT @ <= while
      IP @ XTC-SIG? if
         IP @ XTC-COUNT {: n:n :}
         n 0 > if n RELOC-N ! exit then
      then
      IP @ INSN + IP !
   repeat ;

: ?ROWS ( -- )
   RELOC-N @ 0= if exit then
   BLOB-AT @ CODE-OFF - {: codelen:n :}
   RELOC-N @ 0 ?do
      RELOC-AT @ i XTOFF-ROW * + {: r:n :}
      r U32@ {: loc:n :}
      r 4 + U32@ {: tgt:n :}
      loc 1 < loc DATA-SIZE >= or loc 7 and 0<> or if
         s" image-size: a relocation row names no DATA cell" RC die
      then
      tgt 1 < tgt codelen >= or tgt 3 and 0<> or if
         s" image-size: a relocation row names no code in this image" RC die
      then
   loop ;

\ A program whose capture window is empty emits no blob and therefore no blob
\ address, and EMIT-XT-CELLS refuses declared address cells without one, so such an image
\ is code and nothing else. Here -- and ONLY here -- the last non-zero byte is a
\ sound content end: what it ends is an A64 instruction word, and no A64
\ encoding has a zero top byte, so rounding it up to the instruction boundary
\ recovers exactly the trailing zero bytes of the final instruction. The same
\ reasoning does not hold one line further down, where the content ends in a
\ relocation row whose target really can leave two zero bytes behind it.
: STRIP-CODE-ONLY ( -- )
   CODE-OFF TEXT-SIZE LAST-NONZERO dup PAD4 + {: end:n :}
   end APP-END !  end BLOB-AT !  end BLOB-STOP !  end RELOC-AT !
   0 BLOB-ROWB !  0 BLOB-CARRIED !  0 BLOB-RUNS !  0 BLOB-SPAN !
   0 RELOC-N ! ;

: STRIP-WALK ( -- )
   FIND-BLOB
   BLOB-AT @ 0 < if STRIP-CODE-ONLY exit then
   BLOB-STOP @ dup PAD4 + RELOC-AT !
   FIND-RELOC
   ?ROWS
   RELOC-AT @ RELOC-N @ XTOFF-ROW * + APP-END !
   APP-END @ TEXT-SIZE > if
      s" image-size: the relocation rows run past the text segment" RC die
   then
   APP-END @ TEXT-SIZE APP-END @ - ZEROS  TEXT-SIZE APP-END @ - <> if
      s" image-size: the stripped image's tail is not zero pad" RC die
   then ;

: STRIP-BUDGET ( -- )
   -1 ZCOL !  BUDGET-BEGIN
   ELF-ROWS
   s" app/code" CODE-OFF BLOB-AT @ CODE-OFF - SPAN B-CODE ROW
   s" app/data-cell-bitmap" BLOB-AT @ BLOB-RUNS @ 0 > if 4 BLOB-ROWB @ + else 0 then
      SPAN B-OTHER ROW
   s" app/data-cell-values" BLOB-STOP @ BLOB-CARRIED @ - BLOB-CARRIED @ SPAN B-DATA ROW
   s" app/row-align-pad" BLOB-STOP @ RELOC-AT @ BLOB-STOP @ - SPAN B-PAD ROW
   s" app/relocation-rows" RELOC-AT @ RELOC-N @ XTOFF-ROW * SPAN B-OTHER ROW
   s" image/text-pad" APP-END @ TEXT-SIZE APP-END @ - SPAN B-PAD ROW
   RW-ROW
   SUMS?
   TOTAL-ROW ;

\ What a stripped image does NOT carry is the interesting half of its DATA: the
\ cells restore a window far larger than the file, because a zero cell costs one
\ bit and never travels as bytes. Reported beside the table rather than as a class, since none of those
\ bytes is in the file to attribute.
: STRIP-NOTES ( -- )
   BLOB-RUNS @ 0= if
      cr s" this image's capture window is empty: it carries code and nothing else" type cr
      exit
   then
   cr s" restored DATA window: " type BLOB-SPAN @ FMT:.U
   s"  bytes from " type BLOB-CARRIED @ FMT:.U s"  carried in " type
   BLOB-RUNS @ FMT:.U s"  cells; " type
   BLOB-SPAN @ BLOB-CARRIED @ - FMT:.U s"  bytes do not travel" type cr
   s"   relocation rows " type RELOC-N @ FMT:.U
   s" , declared address cells this image rebinds at startup" type cr ;

\ ---- which of the three images is in hand -------------------------------------
: SEEDED-DICT? ( -- bool )
   ETEXT-END PRIM-DICT-SCAN
   PDICT-N @ 0 > ;

: CLASSIFY ( -- )
   TEXT-SIZE ETEXT-N !
   ATTRIB-RESET
   SNAPSHOT? if
      READ-TRAILER
      construct image-class snapshot CLASS!
      exit
   then
   SEEDED-DICT? if construct image-class engine CLASS! exit then
   construct image-class stripped CLASS! ;

: WALK ( -- )
   CLASS-PTR @ MATCH image-class
      engine OF
         PRIM-NAMES-MEASURE
         DICT-END FIND-AOT
      ENDOF
      snapshot OF
         ETEXT-END PRIM-DICT-FIND
         PRIM-NAMES-MEASURE
         DICT-END FIND-AOT
         SNAP-ATTRIBUTE
      ENDOF
      stripped OF STRIP-WALK ENDOF
   ;MATCH ;

: TABLE ( -- )
   CLASS-PTR @ MATCH image-class
      engine OF BUDGET ENDOF
      snapshot OF SNAP-BUDGET ENDOF
      stripped OF STRIP-BUDGET ENDOF
   ;MATCH ;

\ The engine's censuses answer questions only a baked engine has: which records
\ it ships, what its baked call sites bind to, what nothing reaches, and where
\ its captured DATA heap goes. None of them is a question about an application
\ image -- a --repl image ships the interpreter, so every word it carries is
\ reachable by name, and a stripped image already had the closure walk run
\ against it at build time -- so they run for one class only.
: ENGINE-CENSUS ( -- )
   BUILD-WID-MAP
   BUILD-POOL-MASK
   CENSUS-RECORDS
   REPORT-RECORDS
   REPORT-SITES
   BUILD-CODE-INDEX
   CENSUS-SURFACE
   CENSUS-ENTRY
   REPORT-DATA ;

: NOTES ( -- )
   CLASS-PTR @ MATCH image-class
      engine OF ENGINE-CENSUS ENDOF
      snapshot OF SNAP-NOTES ENDOF
      stripped OF STRIP-NOTES ENDOF
   ;MATCH ;

public

\ ---- the surface tools/engine-size.f and tools/hb-build-lib.f call ------------

: CLASS$ ( -- ptr u8 n )
   CLASS-PTR @ MATCH image-class
      engine OF s" engine" ENDOF
      snapshot OF s" repl-snapshot" ENDOF
      stripped OF s" stripped" ENDOF
   ;MATCH ;

\ Read an image and attribute every one of its bytes. Prints nothing; MEASURE
\ dies by name on an image whose classes do not sum to its length, because the
\ table is written to a counting row printer first.
: MEASURE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu READ-IMAGE
   BAKED-ELF? 0= if s" image-size: not a fixed-base arm64 image" RC die then
   CHECK-SEGMENTS
   CLASSIFY
   WALK
   path pathu IMAGE-NAMES:LOAD
   -1 QUIET !  TABLE  0 QUIET ! ;

: TOTAL-BYTES ( -- n ) TOTAL @ ;
: CODE-BYTES ( -- n ) BK-CODE @ ;
: NAME-BYTES ( -- n ) BK-NAMES @ ;
: DATA-WRITTEN ( -- n ) BK-DATA @ ;
: DATA-ZERO ( -- n ) BK-DZERO @ ;
: DATA-SPAN-BYTES ( -- n ) DATA-SPAN @ ;
: DATA-CAPACITY-BYTES ( -- n ) DATA-SIZE ;
: PAD-BYTES ( -- n ) BK-PAD @ ;
: OTHER-BYTES ( -- n ) BK-OTHER ;

\ What the region attribution found, for a caller that wants the numbers rather
\ than the two tables. Zero on the two classes that carry no region.
: REGION-CODE ( -- n ) BAND-CODE @ ;
: REGION-NAMES ( -- n ) BAND-NAMES @ ;
: REGION-UNOWNED ( -- n ) BAND-FREE @ ;
: DATA-OWNERS ( -- n ) DOWN-N @ ;

\ File coordinates of the captured code, matching the start/len columns in
\ native-build's .names sidecar. Valid after MEASURE on a baked engine.
: CODE-BLOB-RANGE ( -- n n ) BLOB-OFF @ BLOB-LEN @ ;

\ The same rows again, printed this time, with the class's own notes under them.
: REPORT ( -- )
   TABLE
   NOTES ;

\ The summary line, in the one place that owns its wording. `other` is what no
\ class claimed, so the six terms and it add up to the file's own length.
: .SUMMARY ( ptr u8 n -- ) {: name:ptr nameu:n :}
   s" size: " type name nameu type
   s"  " type TOTAL-BYTES FMT:.U s"  bytes = code " type CODE-BYTES FMT:.U
   s" , names " type NAME-BYTES FMT:.U
   s" , data " type DATA-WRITTEN FMT:.U s"  written + " type DATA-ZERO FMT:.U
   s"  zero, padding " type PAD-BYTES FMT:.U
   s" , other " type OTHER-BYTES FMT:.U
   s"  (" type CLASS$ type s" )" type cr ;

: RUN ( -- )
   SCRIPT-ARGC 1 <> if
      s" usage: <engine> --load tools/engine-size.f -- <image>" 64 die
   then
   0 SCRIPT-ARGV$ MEASURE
   REPORT ;

;package
