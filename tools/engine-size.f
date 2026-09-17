\ engine-size.f - where every byte of a baked engine image goes.
\
\ WHY IT EXISTS. "Why is bin/hb 5.8 MB?" had no answer a measurement could
\ give. The size of the parts was folklore -- the dictionary was blamed for
\ megabytes it does not cost -- because nothing walked the file. This does:
\ it attributes EVERY byte of an engine image to a class and an owner, and
\ refuses to report at all unless the classes sum to the file's own length.
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
\ Both over-approximate reachability -- code no record owns is scanned as a root
\ region, and an address the payload records is a root -- so a word this reports
\ as unreachable is unreachable, and the report is a floor, never a wish.
\
\ Run: <engine> --load tools/engine-size.f -- <image>

require lib/fmt.f
require lib/fs.f
require lib/sort.f
require src/habu/code-span.f

\ The engine's own layout is already in the cold prefix; the target executable
\ layout (CODE-OFF, IMAGE-TEXT-SIZE-OFF) is not. tools/imgdump.f loads it the
\ same way and for the same reason.
: ES-LOAD-TARGET-LAYOUT ( -- )
   s" DATA-SIZE" XREF-FIND 0= if
      HB-TARGET-LINUX? if s" src/os/linux/layout.f" included exit then
      HB-TARGET-MACOS? if s" src/os/macos/layout.f" included exit then
      s" engine-size: unknown target" 74 die
   then ;

ES-LOAD-TARGET-LAYOUT
undefine ES-LOAD-TARGET-LAYOUT

package ENGINE-SIZE

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

: READ-IMAGE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu FILE-SIZE {: size:n :}
   size CELL < if s" engine-size: image too small" RC die then
   size CELL 1- + CELL / IMG-RESERVE
   size ILEN !
   path pathu IMG@ size READ-ALL size <> if
      s" engine-size: short read" RC die
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
      s" engine-size: image has no RW segment" RC die
   then
   ELF-PHOFF-OFF U64@ ELF-EHDR-BYTES <> if
      s" engine-size: program headers are not where an engine image puts them" RC die
   then
   RW-OFF TEXT-SIZE <> if
      s" engine-size: the RW segment does not follow the text segment" RC die
   then
   TEXT-SIZE RW-BYTES + ILEN @ <> if
      s" engine-size: file length is not its two segments" RC die
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
   r PREC-END TEXT-SIZE > if false exit then
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

: PRIM-DICT-FIND ( n -- ) {: limit:n :}
   0 PDICT !  0 PDICT-N !
   CODE-OFF begin dup limit < while
      dup PREC-OK? if
         dup COUNTED-RUN {: run:n :}
         run PDICT-N @ > if dup PDICT !  run PDICT-N ! then
         run 0 > if run PREC * + else 4 + then
      else 4 + then
   repeat drop
   PDICT-N @ 0= if
      s" engine-size: no counted seeded dictionary in the image" RC die
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
         s" engine-size: seeded long-name blob does not meet its count cell" RC die
      then
   then ;

\ ---- the AOT payload -----------------------------------------------------------
\ One walk of src/habu/habu2.f EMIT-AOT-SEED's emission order. Every count cell
\ precedes its rows, two runs carry no count of their own (the DATA run bytes are
\ the sum of the run lengths; the boot-run list is 0-terminated), and the whole
\ payload is followed only by the zero pad that rounds the text segment up.
variable AOT0        variable AOT-END     variable CUR        variable FRAME-CELLS
variable BLOB-OFF    variable BLOB-LEN
variable REC0        variable REC-N
variable SITE0       variable SITE-N
variable NAMES0      variable NAMES-LEN
variable DATA-SPAN   variable DATA-D0
variable DSITE0      variable DSITE-N
variable XTOFF0      variable XTOFF-N
variable RUN0        variable RUN-N       variable RBYTES0    variable RBYTES-LEN
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
8 constant RUN-ROW                                \ window offset u32, length u32
8 constant XTSITE-ROW                             \ blob-off u32, name-off u32
8 constant SPAN-ROW                               \ blob-off u32, raw code span u32
AOT-NAMES-CAP constant NAMES-CAP

: TAKE-CELL ( -- n )
   CUR @ 8 ?RANGE
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
   count TEXT-SIZE ?BOUND drop
   count width * TAKE-RUN ;

\ The pool is [len][bytes] entries that exactly fill their declared length; the
\ boot pass validates it the same way before it reads an entry (EM-AOT-VALIDATE).
: POOL-CHECK ( n n -- ) {: at:n len:n :}
   0 ACC !
   begin ACC @ len < while
      at ACC @ + U8@ 1+ ACC @ + ACC !
   repeat
   ACC @ len <> if E-ES-WALK throw then ;

: RUN-BYTES-MEASURE ( -- n )
   0 ACC !
   RUN-N @ 0 ?do
      RUN0 @ i RUN-ROW * + {: row:n :}
      row U32@ {: off:n :}
      row 4 + U32@ {: len:n :}
      len 1 < off 0 < or  off len + DATA-SPAN @ > or if E-ES-WALK throw then
      ACC @ len + ACC !
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
   at TEXT-SIZE > if false exit then
   TEXT-SIZE at - PROT-PAGE-MAX >= if false exit then
   at TEXT-SIZE LAST-NONZERO at = ;

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
   start CUR !  0 FRAME-CELLS !
   TAKE-CELL TEXT-SIZE ?BOUND BLOB-LEN !
   BLOB-LEN @ TAKE-RUN BLOB-OFF !
   TAKE-CELL DICT-CAP ?BOUND REC-N !
   REC-N @ AOT-CREC-ROW TAKE-ROWS REC0 !
   TAKE-CELL TEXT-SIZE ?BOUND SITE-N !
   SITE-N @ SITE-ROW TAKE-ROWS SITE0 !
   TAKE-CELL NAMES-CAP ?BOUND NAMES-LEN !
   NAMES-LEN @ TAKE-RUN NAMES0 !
   NAMES0 @ NAMES-LEN @ POOL-CHECK
   TAKE-CELL DATA-SIZE ?BOUND DATA-SPAN !
   TAKE-CELL DATA-D0 !
   TAKE-CELL TEXT-SIZE ?BOUND DSITE-N !
   DSITE-N @ 4 TAKE-ROWS DSITE0 !
   TAKE-CELL TEXT-SIZE ?BOUND XTOFF-N !
   XTOFF-N @ XTOFF-ROW TAKE-ROWS XTOFF0 !
   TAKE-CELL TEXT-SIZE ?BOUND RUN-N !
   RUN-N @ RUN-ROW TAKE-ROWS RUN0 !
   RUN-BYTES-MEASURE RBYTES-LEN !
   RBYTES-LEN @ TAKE-RUN RBYTES0 !
   TAKE-CELL CODE-B0 !
   TAKE-CELL TEXT-SIZE ?BOUND CSITE-N !
   CSITE-N @ 4 TAKE-ROWS CSITE0 !
   TAKE-CELL TEXT-SIZE ?BOUND XTSITE-N !
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
      TAKE-CELL TEXT-SIZE ?BOUND SIG-LEN !
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
   at 8 + TEXT-SIZE > if false exit then
   at U64@ {: len:n :}
   len 0 < len TEXT-SIZE > or if false exit then
   at 8 + len + TEXT-SIZE <= ;

: FIND-AOT ( n -- ) {: from:n :}
   from begin dup TEXT-SIZE < while
      dup CANDIDATE? over TRY-AT and if AOT0 ! exit then
      4 +
   repeat drop
   s" engine-size: no AOT payload in the image" RC die ;



\ ---- the byte budget -----------------------------------------------------------
variable TOTAL

: TAB ( -- ) 9 emit ;

\ Tenths of a percent, so a 0.1% row is still visible beside a 35% one.
: SHARE ( n -- ) {: bytes:n :}
   bytes 1000 * ILEN @ / {: tenths:n :}
   tenths 10 / FMT:.U 46 emit  tenths 10 mod FMT:.U ;

: ROW ( ptr u8 n n -- ) {: name:ptr nameu:n bytes:n :}
   bytes 0 < if s" engine-size: negative section" RC die then
   name nameu type TAB  bytes FMT:.U TAB  bytes SHARE cr
   TOTAL @ bytes + TOTAL ! ;

: PADDED ( n -- n ) dup PAD4 + ;

: DICT-END ( -- n ) PDICT @ PDICT-N @ PREC * + ;
: NAMES-START ( -- n ) PDICT @ 8 - PNAME-BYTES @ - ;

: BUDGET ( -- )
   0 TOTAL !
   s" class" type TAB s" bytes" type TAB s" percent" type cr
   s" elf/header" type TAB ELF-EHDR-BYTES FMT:.U TAB ELF-EHDR-BYTES SHARE cr
   TOTAL @ ELF-EHDR-BYTES + TOTAL !
   s" elf/program-headers" PHDR-END ELF-EHDR-BYTES - ROW
   PHDR-END CODE-OFF LAST-NONZERO {: meta:n :}
   s" elf/dynamic-metadata" meta PHDR-END - ROW
   s" elf/header-pad" CODE-OFF meta - ROW
   s" engine/code" NAMES-START CODE-OFF - ROW
   s" engine/primitive-names" PNAME-BYTES @ ROW
   s" engine/primitive-count" 8 ROW
   s" engine/primitive-records" PDICT-N @ PREC * ROW
   s" source/baked" AOT0 @ DICT-END - ROW
   s" aot/framing-cells" FRAME-CELLS @ 8 * ROW
   s" aot/code-blob" BLOB-LEN @ PADDED ROW
   s" aot/dictionary-records" REC-N @ AOT-CREC-ROW * PADDED ROW
   s" aot/call-sites" SITE-N @ SITE-ROW * PADDED ROW
   s" aot/name-pool" NAMES-LEN @ PADDED ROW
   s" aot/data-sites" DSITE-N @ 4 * PADDED ROW
   s" aot/address-cells" XTOFF-N @ XTOFF-ROW * PADDED ROW
   s" aot/data-run-rows" RUN-N @ RUN-ROW * PADDED ROW
   s" aot/data-run-bytes" RBYTES-LEN @ PADDED ROW
   s" aot/code-sites" CSITE-N @ 4 * PADDED ROW
   s" aot/named-code-sites" XTSITE-N @ XTSITE-ROW * PADDED ROW
   s" aot/code-spans" SPAN-N @ SPAN-ROW * PADDED ROW
   s" aot/boot-run-entries" BOOTRUN-LEN @ PADDED ROW
   s" aot/protected-wordlists" PWIN-N @ 4 * PADDED ROW
   s" aot/checker-sidecar" SIG-LEN @ 0 > SIGNAME-LEN @ 0 > or
      if 8 SIG-LEN @ PADDED + SIGNAME-LEN @ PADDED + else 0 then ROW
   s" image/text-pad" TEXT-SIZE AOT-END @ - ROW
   s" container/rw-segment" ILEN @ TEXT-SIZE - ROW
   TOTAL @ ILEN @ <> if
      s" engine-size: classes do not sum to the file length" RC die
   then
   s" total" type TAB TOTAL @ FMT:.U TAB TOTAL @ SHARE cr ;

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

: .NAME ( n -- ) {: k:n :}
   k CREC-NAME {: at:n len:n :}
   len 0 ?do at i + U8@ emit loop ;

DYNAMIC-BUFFER WROLE n                            \ role of each wordlist id
DYNAMIC-BUFFER WPKG n                             \ the package row that owns it
variable WID-CAP

: WID-OK? ( n -- bool ) {: w:n :}
   w 0 >= w WID-CAP @ < and ;

: BUILD-WID-MAP ( -- )
   WID-W0 @ WID-SPAN @ + 1+ WID-CAP !
   WID-CAP @ DICT-CAP > if s" engine-size: wordlist span outside the dictionary" RC die then
   WID-CAP @ WROLE-RESERVE  WID-CAP @ WPKG-RESERVE
   WID-CAP @ 0 ?do  ROLE-GLOBAL i WROLE !  0 i WPKG !  loop
   REC-N @ 0 ?do
      i CREC-PKG? if
         i CREC-START {: pub:n :}
         i CREC-RAW-LEN {: priv:n :}
         \ A generated package (ENUM, SUMTYPE, the type-family products) carries
         \ 0 as its private role: it has no private wordlist. Zero is the GLOBAL
         \ wordlist and belongs to no package, so neither role may claim it.
         pub 0 <> pub WID-OK? and if ROLE-PUBLIC pub WROLE !  i pub WPKG ! then
         priv 0 <> priv WID-OK? and if ROLE-PRIVATE priv WROLE !  i priv WPKG ! then
      then
   loop ;

: CREC-ROLE ( n -- n ) {: k:n :}
   k CREC-WID {: w:n :}
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
      else i CREC-NAME-OFF i CREC-ROLE ROLE-BIT PMASK+ then
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
      i CREC-PKG? if ROLE-N else i CREC-ROLE then {: slot:n :}
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
      s" engine-size: a bound call site names no record in this image" RC die
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
\ own relocation tables name. Code inside the blob that no record owns -- a
\ quotation body, padding between routines -- is scanned as a root region, so an
\ edge out of it is never lost and the answer stays a floor.
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
variable LO         variable HI         variable IDXV       variable GAP

: BLOB-W32@ ( n -- n ) BLOB-OFF @ + U32@ ;

: BUILD-CODE-INDEX ( -- )
   REC-N @ 1+ RSTART-RESERVE  REC-N @ 1+ REND-RESERVE  REC-N @ 1+ RIDX-RESERVE
   REC-N @ 1+ MARK-RESERVE    REC-N @ 1+ WORK-RESERVE
   0 CODE-N !
   REC-N @ 0 ?do
      i CREC-PKG? 0= if
         i CREC-START {: at:n :}
         at i CREC-BYTES + BLOB-LEN @ > if
            s" engine-size: a baked record runs past the code blob" RC die
         then
         CODE-N @ 0 > if
            at CODE-N @ 1- RSTART @ < if
               s" engine-size: baked records are not in code order" RC die
            then
         then
         at CODE-N @ RSTART !
         at i CREC-BYTES + CODE-N @ REND !
         i CODE-N @ RIDX !
         CODE-N @ 1+ CODE-N !
      then
   loop ;

\ The lowest code index whose start is not below off.
: LOWER-BOUND ( n -- n ) {: off:n :}
   0 LO !  CODE-N @ HI !
   begin LO @ HI @ < while
      LO @ HI @ + 2 / {: mid:n :}
      mid RSTART @ off < if mid 1+ LO ! else mid HI ! then
   repeat
   LO @ ;

: ENTRY-INDEX ( n -- n ) {: off:n :}
   off LOWER-BOUND {: j:n :}
   j CODE-N @ >= if -1 exit then
   j RSTART @ off <> if -1 exit then
   j ;

: MARK-ONE ( n -- ) {: j:n :}
   j RIDX @ {: k:n :}
   k MARK @ 0<> if exit then
   1 k MARK !
   k WORK-N @ WORK !  WORK-N @ 1+ WORK-N !
   REACH-N @ 1+ REACH-N !
   REACH-CODE @ k CREC-BYTES + REACH-CODE ! ;

: MORE-BELOW? ( n -- bool ) {: off:n :}
   IDXV @ 0 <= if false exit then
   IDXV @ 1- RSTART @ off = ;

: MORE-HERE? ( n -- bool ) {: off:n :}
   IDXV @ CODE-N @ >= if false exit then
   IDXV @ RSTART @ off = ;

\ An EXPORT alias puts a second record over one routine, so an entry marks every
\ record that starts there, not the first one the search lands on.
: MARK-ENTRY ( n -- ) {: off:n :}
   off ENTRY-INDEX {: j:n :}
   j 0 < if exit then
   j IDXV !
   begin off MORE-BELOW? while IDXV @ 1- IDXV ! repeat
   begin off MORE-HERE? while IDXV @ MARK-ONE  IDXV @ 1+ IDXV ! repeat ;

\ A DATA cell or a code literal may hold an address INSIDE a routine -- a does>
\ clause, a quotation body -- and that routine runs when the address is used, so
\ a recorded address marks the record whose span contains it. A branch is the
\ other case: it enters a word at its entry, and a target in the middle of
\ another routine is that routine's own control flow (src/habu/aot-closure.f
\ follows entries for the same reason).
: MARK-SPAN ( n -- ) {: off:n :}
   off ENTRY-INDEX 0 >= if off MARK-ENTRY exit then
   off LOWER-BOUND {: j:n :}
   j 0 <= if exit then
   j 1- {: prev:n :}
   off prev REND @ < if prev RSTART @ MARK-ENTRY then ;

: SCAN-AT ( n -- ) {: at:n :}
   at BLOB-W32@ {: w:n :}
   w BR-MASK and BR-OP <> if exit then
   w BR-IMM and {: raw:n :}
   raw BR-SIGN and 0<> if raw BR-SIGN 2 * - else raw then {: rel:n :}
   at rel 4 * + {: tgt:n :}
   tgt 0 >= tgt BLOB-LEN @ < and if tgt MARK-ENTRY then ;

: SCAN-SPAN ( n n -- ) {: from:n to:n :}
   from begin dup to < while
      dup SCAN-AT  4 +
   repeat drop ;

: SWEEP ( -- )
   begin WORK-N @ 0 > while
      WORK-N @ 1- WORK-N !
      WORK-N @ WORK @ {: k:n :}
      k CREC-START dup k CREC-BYTES + SCAN-SPAN
   repeat ;

\ A four-instruction MOVZ/MOVK chain, the one form a code literal takes
\ (src/habu/aot-closure.f ADDRESS-CHAIN?). Only the immediates are read.
: CHAIN-VALUE ( n -- n ) {: at:n :}
   0 ACC !
   4 0 ?do
      at i 4 * + BLOB-W32@ {: w:n :}
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

\ Every span of the blob no record owns is a root region.
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
   off CHAIN-VALUE CODE-B0 @ - {: at:n :}
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
         i CREC-ROLE ROLE-PRIVATE <> if i CREC-START MARK-ENTRY then
      then
   loop ;

: REACH-RESET ( -- )
   REC-N @ 0 ?do 0 i MARK ! loop
   0 WORK-N !  0 REACH-N !  0 REACH-CODE ! ;

\ ---- what nothing reaches ------------------------------------------------------
DYNAMIC-BUFFER DEAD-N n                           \ unreachable records per package row
DYNAMIC-BUFFER DEAD-CODE n                        \ their code bytes
DYNAMIC-BUFFER DMASK n                            \ per pool entry: 1 dead ref, 2 live ref
DYNAMIC-BUFFER PROW n                             \ sortable (bytes, package row)
variable PROW-N     variable DROLE-N   variable DROLE-CODE
variable DEAD-TOTAL variable DEAD-BYTES variable DEAD-NAMES
16 constant TOP-ROWS
$FFFF constant ROW-MASK

: DEAD? ( n -- bool ) MARK @ 0= ;

: PKG-ROW-OF ( n -- n ) {: k:n :}
   k CREC-WID {: w:n :}
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

: DEAD-ROLE ( n -- ) {: role:n :}
   0 DROLE-N !  0 DROLE-CODE !
   REC-N @ 0 ?do
      i CREC-PKG? 0= i DEAD? and if
         i CREC-ROLE role = if
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
   .TOP-PACKAGES ;

: CENSUS-SURFACE ( -- )
   REACH-RESET  ROOTS-SURFACE  ROOTS-ENTRY  SWEEP
   s" dictionary-surface" REPORT-REACH ;

: CENSUS-ENTRY ( -- )
   REACH-RESET  ROOTS-ENTRY  SWEEP
   s" engine-entry" REPORT-REACH ;

\ ---- the captured DATA heap ----------------------------------------------------
\ The payload carries the window's DATA as its non-zero runs, each with an
\ (offset, length) header, so a table of cells holding small numbers costs MORE
\ in header rows than in bytes. tools/data-table-census.f asks this question of
\ a LIVE heap; this asks it of the shipped image, where the cost is real.
\ An owner is a record the definer stamped DKIND:ADDR (create/variable): no
\ other record kind owns DATA. Its address is the MOVZ/MOVK chain its body
\ pushes, read with the same decode the relocation pass uses.
\ AN OWNER IS CHARGED UP TO THE NEXT OWNER, the rule tools/data-table-census.f
\ states for the live heap and for the same reason: `allot` only moves DP, so
\ consecutive bases partition the span exactly. A block allotted after a
\ variable -- a grown arena a pointer cell reaches, heap above the last
\ `create` -- therefore shows up under the name below it, which is a locator,
\ not an accusation.
DYNAMIC-BUFFER DOWNER n                           \ (DATA offset, record) packed, ascending
DYNAMIC-BUFFER DBYTES n                           \ run bytes charged to each owner
DYNAMIC-BUFFER DRUNS n                            \ run headers charged to each owner
DYNAMIC-BUFFER DCOST n                            \ (cost, owner) packed, for the ranking
variable DOWN-N     variable DCOST-N   variable CRP
variable UNOWNED-BYTES  variable UNOWNED-RUNS
16 constant OWNER-SHIFT

: DOWN-OFF ( n -- n ) DOWNER @ OWNER-SHIFT rshift ;
: DOWN-REC ( n -- n ) DOWNER @ ROW-MASK and ;

: COLLECT-OWNERS ( -- )
   REC-N @ 1+ DOWNER-RESERVE
   0 DOWN-N !
   REC-N @ 0 ?do
      i CREC-PKG? 0= i CREC-ADDR? and i CREC-BYTES 16 >= and if
         i CREC-START CHAIN-VALUE DATA-D0 @ - {: off:n :}
         off 0 >= off DATA-SPAN @ < and if
            off OWNER-SHIFT lshift i or  DOWN-N @ DOWNER !
            DOWN-N @ 1+ DOWN-N !
         then
      then
   loop
   0 DOWNER DOWN-N @ [: < ;] SORT:SORT!
   DOWN-N @ 1+ DBYTES-RESERVE  DOWN-N @ 1+ DRUNS-RESERVE
   DOWN-N @ 1+ 0 ?do 0 i DBYTES !  0 i DRUNS ! loop ;

\ The last owner at or below off, or -1 when the run starts below every owner.
: OWNER-AT ( n -- n ) {: off:n :}
   0 LO !  DOWN-N @ HI !
   begin LO @ HI @ < while
      LO @ HI @ + 2 / {: mid:n :}
      mid DOWN-OFF off > if mid HI ! else mid 1+ LO ! then
   repeat
   LO @ 1- ;

: OWNER-END ( n -- n ) {: k:n :}
   k 1+ DOWN-N @ >= if DATA-SPAN @ exit then
   k 1+ DOWN-OFF ;

\ A run is a maximal non-zero extent and may cross into the next owner's table,
\ so its bytes are split; the header row is charged where the run starts.
\ Where the owner at index k ends; -1 is the head below the first owner.
: OWNER-LIMIT ( n -- n ) {: k:n :}
   k 0 < if
      DOWN-N @ 0 > if 0 DOWN-OFF else DATA-SPAN @ then exit
   then
   k OWNER-END ;

: CHARGE-BYTES ( n n n -- ) {: k:n from:n to:n :}
   k 0 < if UNOWNED-BYTES @ to from - + UNOWNED-BYTES ! exit then
   k DBYTES @ to from - + k DBYTES ! ;

\ A run is a maximal non-zero extent and may cross into the next owner's table,
\ so its bytes are split; the header row is charged where the run starts.
: CHARGE-RUN ( n n -- ) {: off:n len:n :}
   off OWNER-AT {: k:n :}
   k 0 < if UNOWNED-RUNS @ 1+ UNOWNED-RUNS ! else k DRUNS @ 1+ k DRUNS ! then
   off len + {: end:n :}
   off CRP !
   begin CRP @ end < while
      CRP @ OWNER-AT {: cur:n :}
      cur OWNER-LIMIT end min {: cut:n :}
      cut CRP @ <= if s" engine-size: DATA owners do not advance" RC die then
      cur CRP @ cut CHARGE-BYTES
      cut CRP !
   repeat ;

: CHARGE-RUNS ( -- )
   0 UNOWNED-BYTES !  0 UNOWNED-RUNS !
   RUN-N @ 0 ?do
      RUN0 @ i RUN-ROW * + {: row:n :}
      row U32@  row 4 + U32@ CHARGE-RUN
   loop ;

: OWNER-COST ( n -- n ) {: k:n :}
   k DBYTES @  k DRUNS @ RUN-ROW * + ;

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

\ Every run byte and every run header lands on exactly one owner or on the
\ unowned head, so the charges add up to the payload's own two numbers.
: CHECK-CHARGES ( -- )
   0 ACC !
   DOWN-N @ 0 ?do ACC @ i DBYTES @ + ACC ! loop
   ACC @ UNOWNED-BYTES @ + RBYTES-LEN @ <> if
      s" engine-size: DATA run bytes do not add up" RC die
   then
   0 ACC !
   DOWN-N @ 0 ?do ACC @ i DRUNS @ + ACC ! loop
   ACC @ UNOWNED-RUNS @ + RUN-N @ <> if
      s" engine-size: DATA run rows do not add up" RC die
   then ;

: REPORT-DATA ( -- )
   COLLECT-OWNERS  CHARGE-RUNS  CHECK-CHARGES  RANK-OWNERS
   cr s" captured DATA heap: " type DATA-SPAN @ FMT:.U
   s"  bytes of span, " type RUN-N @ FMT:.U s"  runs, " type
   RUN-N @ RUN-ROW * RBYTES-LEN @ + FMT:.U s"  bytes of image" type cr
   s"   owners " type DOWN-N @ FMT:.U
   s" , unowned run bytes " type UNOWNED-BYTES @ FMT:.U
   s" , unowned run rows " type UNOWNED-RUNS @ FMT:.U cr
   s"   owner" type TAB s" offset" type TAB s" extent" type TAB s" runs" type TAB
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

\ A snapshot image carries its dictionary and DATA verbatim behind a trailer
\ (src/habu/snap-lib.f), which is a different budget with a different owner.
\ Refuse it by name rather than walk it as if it were baked.
: SNAPSHOT? ( -- bool )
   TEXT-SIZE IMAGE-TEXT-TRAILER-ADJ + SNAP-TRL-BYTES - {: trl:n :}
   trl 0 < if false exit then
   trl SNAP-TRL-BYTES IN-IMAGE? 0= if false exit then
   trl U64@ SNAP-MAGIC = ;

public
: RUN ( -- )
   SCRIPT-ARGC 1 <> if
      s" usage: <engine> --load tools/engine-size.f -- <image>" 64 die
   then
   0 SCRIPT-ARGV$ READ-IMAGE
   BAKED-ELF? 0= if s" engine-size: not a fixed-base arm64 engine image" RC die then
   CHECK-SEGMENTS
   SNAPSHOT? if s" engine-size: image carries a snapshot trailer, not a baked payload" RC die then
   TEXT-SIZE PRIM-DICT-FIND
   PRIM-NAMES-MEASURE
   DICT-END FIND-AOT
   BUDGET
   BUILD-WID-MAP
   BUILD-POOL-MASK
   CENSUS-RECORDS
   REPORT-RECORDS
   REPORT-SITES
   BUILD-CODE-INDEX
   CENSUS-SURFACE
   CENSUS-ENTRY
   REPORT-DATA ;

;package

\ Loading with no argument defines the tool without measuring anything, so a
\ test can drive it; tools/imgdump.f has the same entry shape.
: ENGINE-SIZE-MAIN? ( -- )
   SCRIPT-ARGC 0 > if ENGINE-SIZE:RUN then ;

ENGINE-SIZE-MAIN?
undefine ENGINE-SIZE-MAIN?
