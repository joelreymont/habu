\ A stripped image cannot restore a mapping made by its builder. AOT and FILE
\ admission classify live cells here. Automatic FILE caching declines unsafe
\ storage; strict export and the final AOT link name the cell and refuse it.
require src/habu/layout.f
require src/habu/xref.f
require src/habu/address-carrier.f
require src/habu/proc-maps.f
require src/lib/sort.f

\ The checker does not publish this diagnostic-mode cell to later checked loads.
s" JSON-DIAGS" s" -- ptr a" TRUST

package AOT-CELL
using SNAP-RELOC
using ADDRESS-CARRIER

: DBASE-N ( -- n ) dbase@ ;
: DBASE@ dbase@ ;
s" DBASE@" s" -- ptr a" TRUST

: DATA-ADDRESS? ( n -- bool ) {: v:n :}
   v DATA-VA VA>N >= v DATA-VA VA>N DATA-SIZE + <= and ;

: REGION-ADDRESS? ( n -- bool ) {: v:n :}
   v DBASE-N >= v DBASE-N REGION + < and ;

: DICT-PTR? ( n -- bool ) {: v:n :}
   v DBASE-N >= v DBASE-N ndict@ DREC * + < and ;
: CODE-PTR? ( n -- bool ) {: v:n :}
   v DBASE-N DICT-SIZE + >= v cp@ < and ;
: TEXT-PTR? ( n -- bool ) {: v:n :}
   v DICT-PTR? if true exit then v CODE-PTR? ;

\ A scan span describes the storage producer's cell grid, byte extent, or
\ allocation start. A start is a point: only windows crossing it are excluded.
\ Declared spans are gathered once for a FILE admission or final AOT scan;
\ undeclared CREATE bytes retain the full bytewide scan.
0 constant CELL-SPAN
1 constant BYTE-SPAN
2 constant ALLOC-START
3 constant SPAN-CELLS
DYNAMIC-BUFFER SPAN-ROWS n
DYNAMIC-BUFFER SPAN-ORDER n
variable SPAN-N
variable SPAN-CURSOR
variable SPAN-LAST
variable SPAN-FLOOR

: SPAN-BASE@ ( n -- n ) SPAN-CELLS * SPAN-ROWS @ ;
: SPAN-END@ ( n -- n ) SPAN-CELLS * 1+ SPAN-ROWS @ ;
: SPAN-KIND@ ( n -- n ) SPAN-CELLS * 2 + SPAN-ROWS @ ;

: SPAN+ ( ptr u8 n n -- )
   {: p:ptr size:n kind:n :}
   kind ALLOC-START = if
      size 0<> if s" aot-cell: allocation start has a size" 74 die then
   else
      size 0= if exit then
   then
   p NULL-PTR BYTE-VIEW - {: base:n :}
   base DATA-VA VA>N < size 0 < or
   base DATA-VA VA>N DATA-SIZE + > or
   size DATA-VA VA>N DATA-SIZE + base - > or if
      s" aot-cell: scan span outside DATA" 74 die
   then
   kind CELL-SPAN = kind ALLOC-START = or if
      base CELL mod 0<> size CELL mod 0<> or if
         s" aot-cell: unaligned cell or allocation start" 74 die
      then
   then
   SPAN-N @ {: row:n :}
   row 1+ SPAN-CELLS * SPAN-ROWS-RESERVE
   row 1+ SPAN-ORDER-RESERVE
   base row SPAN-CELLS * SPAN-ROWS !
   base size + row SPAN-CELLS * 1+ SPAN-ROWS !
   kind row SPAN-CELLS * 2 + SPAN-ROWS !
   row row SPAN-ORDER !
   1 SPAN-N +! ;

: CELLS+ ( ptr u8 n -- ) CELL-SPAN SPAN+ ;
: BYTES+ ( ptr u8 n -- ) BYTE-SPAN SPAN+ ;
: START+ ( ptr u8 -- ) 0 ALLOC-START SPAN+ ;
: SPAN-LESS? ( n n -- bool )
   {: a:n b:n :}
   a SPAN-BASE@ b SPAN-BASE@ < ;

CAST: N>U8 ( n -- ptr u8 )
CAST: CELL-PRODUCER-XT ( n -- [ [ ptr u8 n -- ] -- ] )
CAST: MIXED-PRODUCER-XT ( n -- [ [ ptr u8 n -- ] [ ptr u8 n -- ] -- ] )

: REFUSE-PRODUCER ( ptr u8 n -- )
   {: a:ptr u:n :}
   s" aot-cell: span producer " 2 -rot write drop
   2 a u write drop
   s"  is missing or stale" 74 die ;

: TARGET-XT? ( ptr u8 n bool -- n )
   {: a:ptr u:n required:bool :}
   a u XREF-FIND dup XREF-FOUND? 0= if
      drop
      required if a u REFUSE-PRODUCER then
      0 exit
   then
   XREF-START dup SPAN-FLOOR @ < over cp@ >= or if
      drop a u REFUSE-PRODUCER
   then ;

: TARGET-XT ( ptr u8 n -- n ) true TARGET-XT? ;
names: TARGET-XT
: OPTIONAL-XT ( ptr u8 n -- n ) false TARGET-XT? ;
names: OPTIONAL-XT

: SCALAR-REC? ( ptr n -- bool ) {: rec:ptr :}
   rec XREF-RETIRED? if false exit then
   rec XREF-WORDLIST XREF-NAMESPACE-WL = if false exit then
   rec XREF-FLAGS {: flags:n :}
   flags DNAME-CELL and 0<>
   flags DKIND:MASK and DKIND:ADDR = and ;

: SCALAR-BASE ( ptr n -- n ) {: rec:ptr :}
   rec XREF-START-SLOT XREF-PTR@ {: p:ptr :}
   p p rec XREF-CODE-BYTES + CHAIN-SIZE {: size:n :}
   size 0= if s" aot-cell: malformed variable address carrier" 74 die then
   p size CHAIN-VALUE {: base:n :}
   base DATA-VA VA>N <
   base DATA-VA VA>N DATA-SIZE CELL - + > or
   base CELL mod 0<> or if
      s" aot-cell: variable cell outside DATA" 74 die
   then
   base ;

: ADD-SCALARS ( -- )
   ndict@ 0 ?do
      i XREF-REC {: rec:ptr :}
      rec SCALAR-REC? if
         rec SCALAR-BASE N>U8 CELL CELLS+
      then
   loop ;

: ADDRESS-SITE? ( ptr u8 -- bool ) {: p:ptr :}
   p DBASE@ BYTE-VIEW - {: off:n :}
   off DICT-SIZE < off REGION >= or if false exit then
   data-base ADDRMAP-OFF + off 5 rshift + c@
   off 2 rshift 7 and rshift 1 and 0<> ;

: ALLOC-REC? ( ptr n -- bool ) {: rec:ptr :}
   rec XREF-RETIRED? if false exit then
   rec XREF-WORDLIST XREF-NAMESPACE-WL = if false exit then
   rec XREF-FLAGS DNAME-ALLOC and 0<> ;

: ALLOC-BASE ( ptr n -- n ) {: rec:ptr :}
   rec XREF-START-SLOT XREF-PTR@ {: p:ptr :}
   p ADDRESS-SITE? 0= if
      s" aot-cell: allocation start has no recorded address site" 74 die
   then
   p p rec XREF-CODE-BYTES + CHAIN-SIZE {: size:n :}
   size ADDRESS-CARRIER:DATA-CHAIN-BYTES <> if
      s" aot-cell: malformed allocation address carrier" 74 die
   then
   p size CHAIN-VALUE {: base:n :}
   base DATA-VA VA>N <
   base DATA-VA VA>N DATA-SIZE + > or
   base CELL mod 0<> or if
      s" aot-cell: allocation start outside DATA" 74 die
   then
   base ;

: ADD-ALLOC-STARTS ( -- )
   ndict@ 0 ?do
      i XREF-REC {: rec:ptr :}
      rec ALLOC-REC? if
         rec XREF-FLAGS DNAME-CELL and 0= if
            rec ALLOC-BASE N>U8 START+
         then
      then
   loop ;

: PREPARE ( n -- )
   SPAN-FLOOR !
   0 SPAN-N ! 0 SPAN-CURSOR ! -1 SPAN-LAST !
   ADD-SCALARS
   ADD-ALLOC-STARTS
   [: CELLS+ ;] s" CHECKER-STORAGE-CELLS"
      TARGET-XT CELL-PRODUCER-XT execute
   [: CELLS+ ;] s" NSTR:CELL-SPANS"
      TARGET-XT CELL-PRODUCER-XT execute
   [: CELLS+ ;] [: BYTES+ ;] s" FFI:SCAN-SPANS"
      TARGET-XT MIXED-PRODUCER-XT execute
   [: CELLS+ ;] [: BYTES+ ;] s" FFI-DECL:SCAN-SPANS"
      TARGET-XT MIXED-PRODUCER-XT execute
   \ TASK owns callback controls below the FILE range and the live row table.
   \ Its declaration visitor supplies their exact cell extents even when the
   \ baked engine no longer retains the private variable records.
   s" TASK:OWNED-CALLBACK-ROWS" OPTIONAL-XT {: task:n :}
   task 0<> if
      [: CELLS+ ;] task CELL-PRODUCER-XT execute
   then
   s" FFI-CB:SCAN-SPANS" OPTIONAL-XT {: callback:n :}
   callback 0<> if
      [: CELLS+ ;] [: BYTES+ ;]
      callback MIXED-PRODUCER-XT execute
   then
   0 SPAN-ORDER SPAN-N @ [: SPAN-LESS? ;] SORT:SORT! ;

: RESET ( -- )
   0 SPAN-N !  0 SPAN-CURSOR !  0 SPAN-LAST !  0 SPAN-FLOOR ! ;

\ A complete declared cell still receives the ordinary value check. A shifted
\ window touching any cell grid, or any window touching byte storage, has no
\ independent cell value.
: NONCELL-WINDOW? ( n -- bool ) {: at:n :}
   at SPAN-LAST @ < if 0 SPAN-CURSOR ! then
   at SPAN-LAST !
   begin
      SPAN-CURSOR @ SPAN-N @ <
      if SPAN-CURSOR @ SPAN-ORDER @ SPAN-END@ at <= else false then
   while
      1 SPAN-CURSOR +!
   repeat
   SPAN-CURSOR @
   begin dup SPAN-N @ < while
      dup SPAN-ORDER @ {: row:n :}
      row SPAN-BASE@ at CELL + >= if drop false exit then
      row SPAN-END@ at > if
         row SPAN-KIND@ ALLOC-START = if drop true exit then
         row SPAN-KIND@ BYTE-SPAN = if drop true exit then
         at row SPAN-BASE@ <
         at CELL + row SPAN-END@ > or
         at row SPAN-BASE@ - CELL mod 0<> or if
            drop true exit
         then
      then
      1+
   repeat drop false ;

\ The caller supplies its target's literal row query. A compiler host must not
\ ask its own retired pool whether target DATA bytes are literal text.
: LITERAL-OVERLAP? ( n [ n -- bool ] -- bool ) {: at:n row? :}
   CELL 0 ?do
      at i + row? execute if true unloop exit then
   loop false ;

\ Ask the kernel first: almost every stored value misses its mapped extents.
\ DATA is MAP_FIXED in both processes, and the whole code/dictionary region is
\ classified by the linker's live extents instead. Linux's fixed executable
\ band catches ordinary small integers too; its own path tag exempts that band.
\ The brk [heap] band likewise catches integers, and Habu allocates from mmap,
\ so no Habu allocation can point into it. Mach-O has neither exemption.
\ Other mapped areas belong to the builder's process, not to the image.
: MAPPED? ( n -- bool ) {: v:n :}
   v PROC-MAPS:MAPPED? 0= IF false EXIT THEN
   v DATA-ADDRESS? IF false EXIT THEN
   v REGION-ADDRESS? IF false EXIT THEN
   v PROC-MAPS:SELF-IMAGE? IF false EXIT THEN
   v PROC-MAPS:HEAP? 0= ;

\ A word owns a cell only when a recorded address carrier spells that cell's
\ exact address. Neighbouring data and a coincidentally close record do not.
: REC-CELL-SITE? ( ptr n n -- bool ) {: r:ptr v:n :}
   r XREF-START N>U8 {: p:ptr :}
   r XREF-CODE-BYTES ADDRESS-CARRIER:DATA-CHAIN-BYTES - 4 / 1+ 0 max 0 ?do
      p i 4 * + ADDRESS-SITE? if
         p i 4 * + p r XREF-CODE-BYTES + CHAIN-SIZE {: size:n :}
         size 0<> if
            p i 4 * + size CHAIN-VALUE v = if true unloop exit then
         then
      then
   loop false ;

: OWNER ( n -- ptr n ) {: cell:n :}
   ndict@ 0 ?do
      i XREF-REC {: r:ptr :}
      r XREF-WORDLIST -1 <> if
         r cell REC-CELL-SITE? if r unloop exit then
      then
   loop XREF-NULL ;

create AECH 1 allot
: AE1 ( n -- ) {: c:n :} c AECH c! 2 AECH 1 write drop ;
: AETXT ( ptr u8 n -- ) {: a:ptr u:n :} 2 a u write drop ;
: AEREC-TXT ( ptr n -- ) {: r:ptr :}
   r XREF-FOUND? IF r XREF-NAME$ AETXT ELSE s" <unknown>" AETXT THEN ;
: AEJCHAR ( n -- ) {: c:n :}
   c 10 = IF 92 AE1 110 AE1 EXIT THEN
   c 13 = IF 92 AE1 114 AE1 EXIT THEN
   c 9 = IF 92 AE1 116 AE1 EXIT THEN
   c 34 = c 92 = or IF 92 AE1 THEN c AE1 ;
: AEJSTR ( ptr u8 n -- ) {: a:ptr u:n :}
   34 AE1 0 BEGIN dup u < WHILE dup a + c@ AEJCHAR 1 + REPEAT drop 34 AE1 ;
: AEJKEY ( ptr u8 n -- ) {: a:ptr u:n :} a u AEJSTR 58 AE1 ;
: AEJREC ( ptr n -- ) {: r:ptr :}
   r XREF-FOUND? IF r XREF-NAME$ AEJSTR ELSE s" <unknown>" AEJSTR THEN ;
create AENB 20 allot variable AENV variable AENN
: AEJNUM ( n -- )
   AENV ! 0 AENN !
   AENV @ 0= IF 48 AE1 EXIT THEN
   BEGIN AENV @ 0 > WHILE
      AENV @ 10 mod 48 + AENB AENN @ + c!
      AENN @ 1 + AENN !
      AENV @ 10 / AENV !
   REPEAT
   AENN @ BEGIN dup 0 > WHILE 1 - dup AENB + c@ AE1 REPEAT drop ;

: DATA-CELL-JSON ( n n ptr u8 n ptr u8 n -- ) {: cell:n v:n ra:ptr ru:n sa:ptr su:n :}
   123 AE1
   s" schema_version" AEJKEY 1 AEJNUM 44 AE1
   s" code" AEJKEY s" E-AOT-UNSUPPORTED" AEJSTR 44 AE1
   s" verdict" AEJKEY s" rejected" AEJSTR 44 AE1
   s" word" AEJKEY cell OWNER AEJREC 44 AE1
   s" data_off" AEJKEY cell DATA-VA VA>N - AEJNUM 44 AE1
   s" value" AEJKEY v AEJNUM 44 AE1
   s" reason" AEJKEY ra ru AEJSTR 44 AE1
   s" suggestion" AEJKEY sa su AEJSTR
   125 AE1 10 AE1 ;

: DATA-CELL-PROSE ( n n ptr u8 n ptr u8 n -- ) {: cell:n v:n ra:ptr ru:n sa:ptr su:n :}
   s" hb-build: " AETXT ra ru AETXT
   s"  word=" AETXT cell OWNER AEREC-TXT
   s"  data-off=" AETXT cell DATA-VA VA>N - AEJNUM
   s"  value=" AETXT v AEJNUM
   s"  suggestion='" AETXT sa su AETXT 39 AE1
   10 AE1 ;

: REFUSE ( n n ptr u8 n ptr u8 n -- ) {: cell:n v:n ra:ptr ru:n sa:ptr su:n :}
   JSON-DIAGS @ IF cell v ra ru sa su DATA-CELL-JSON
      ELSE cell v ra ru sa su DATA-CELL-PROSE THEN
   s" hb-build: AOT unsupported persistent data" 70 die ;

: REFUSE-MAPPED ( n n -- ) {: cell:n v:n :}
   cell v
   s" stripped AOT persistent data holds a pointer into memory the build mapped"
   s" the image restores no mapping the build made; allocate at run time (in MAIN or an IMAGE-LIFECYCLE hook) and store the pointer then, or use --repl"
   REFUSE ;

: REFUSE-UNDECLARED ( n n -- ) {: cell:n v:n :}
   cell v
   s" stripped AOT persistent data holds an undeclared code/dict pointer"
   s" a stripped image relocates only DECLARED xt cells (defer/is/xt!); ' word , declares nothing - bind the cell with a defer or xt!, or use --repl"
   REFUSE ;

public
EXPORT DATA-ADDRESS?
EXPORT DICT-PTR?
EXPORT CODE-PTR?
EXPORT TEXT-PTR?
EXPORT PREPARE
EXPORT RESET
EXPORT NONCELL-WINDOW?
EXPORT LITERAL-OVERLAP?
EXPORT MAPPED?
EXPORT OWNER
EXPORT AE1
EXPORT AETXT
EXPORT AEREC-TXT
EXPORT AEJSTR
EXPORT AEJKEY
EXPORT AEJREC
EXPORT AEJNUM
EXPORT REFUSE
EXPORT REFUSE-MAPPED
EXPORT REFUSE-UNDECLARED

;using
;using
;package
