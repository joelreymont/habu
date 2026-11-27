\ link-kernel.f - the native kernel a build's link adopts from the kernel object
\ a kernel build writes, the map from the running hb's kernel primitive entries
\ to the object's, and the cut that keeps only the kernel code a program
\ reaches. The two kernels bake different inputs, so the map goes
\ by name and wordlist.
require src/core/exit-status.f
require src/lib/error.f
require src/lib/le.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/sort.f
require src/core/bytes.f
require src/core/sha256.f
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/arch/arm64/mnem.f
require src/habu/link-code.f
require src/habu/layout.f
require src/habu/code-bytes.f
require src/arch/x86-64/icode.f
require src/lib/vector.f
require src/lib/heap.f

package LINKER
private

\ The linker's error lines go whole to the error output.
: SAY ( str -- ) ERROR-TEXT:TYPE ;

: SAY-INT ( n -- )
   dup 0< if s" -" SAY then
   ERROR-TEXT:DIGITS$ SAY ;

: SAY-CR ( -- ) s\" \n" SAY ;

\ An object hb writes, a file or the kernel's, opens with an 8-byte magic and then
\ the SHA-256 of its bytes from 40 to its end. The span holds at least 40 bytes.
SHA256-CTX-BYTES BUFFER: SHA-CTX
32 BUFFER: SUM

: SUM-OF ( str -- ) {: s:str :}
   SHA-CTX  s STR:DATA 40 +  s STR:LEN 40 - >str  SUM SHA256-IN ;

: SEALED? ( str -- bool ) {: s:str :}
   s SUM-OF  SUM 32 >str  s STR:DATA 8 + 32 >str  STR:= ;

: SEAL ( str -- ) {: s:str :}
   s SUM-OF  SUM  s STR:DATA 8 +  32 BYTE-COPY ;

: DAMAGE-END ( str -- )
   s" cause" REJECTION:NOTE  REJECTION:EMIT-STR
   REJECTION:HELP  s" delete it and build again" REJECTION:EMIT-STR
   EXIT-STATUS:DATAERR REJECTION:STOP ;

\ Stops the build with status DATAERR: the object at the path is not as hb
\ wrote it, for the cause.
: DAMAGED ( str str -- ) {: p:str c:str :}
   s" E-DAMAGED-OBJECT" REJECTION:OPEN
   p REJECTION:EMIT-STR  s"  is not the file hb wrote" REJECTION:EMIT-STR
   c DAMAGE-END ;

\ Stops the build with status DATAERR: the object at the path cannot be
\ read, for the cause.
: UNREADABLE ( str str -- ) {: p:str c:str :}
   s" E-DAMAGED-OBJECT" REJECTION:OPEN
   p REJECTION:EMIT-STR  s"  cannot be read" REJECTION:EMIT-STR
   c DAMAGE-END ;

\ Stops the build as UNREADABLE for a code caught opening, sizing, reading or
\ mapping the object at the path. The caller throws any other code on itself,
\ which keeps the caught record.
: READ-FAILED ( str n -- ) {: p:str code:n :}
   code E-FS-OPEN =  code E-FS-STAT = or  code E-FS-IO = or  code E-MEM-MAP = or  if
      p  code ERROR:NAME$  UNREADABLE
   then ;

EXPORT BOOT:data-base
EXPORT CHECK:PRIM-NAME
EXPORT CHECK:PRIM-OPEN
EXPORT CHECK:PRIM-ATOM
EXPORT CHECK:PRIM-CLOSE
EXPORT CHECK:PRIM-CELLS
EXPORT CHECK:PRIMS-END

\ Every row source, hb's own table and the bytes STATE-PRIMS reads, reaches the checker through these STATE- words into ROWS-CTX; each
\ tag fixes the fields after it and a text carries its length, so equal rows digest alike and different rows apart.
SHA256-CTX-BYTES BUFFER: ROWS-CTX
8 BUFFER: ROWS-NUM

: ROWS-BEGIN ( -- ) ROWS-CTX SHA256-BEGIN ;
: ROW-N+ ( n -- ) ROWS-NUM LE:U64!  ROWS-CTX ROWS-NUM 8 >str SHA256-FEED ;
: ROW-STR+ ( str -- ) {: s:str :}  s STR:LEN ROW-N+  ROWS-CTX s SHA256-FEED ;
: ROW-FLAG+ ( bool -- ) if 1 else 0 then ROW-N+ ;

: STATE-NAME ( str -- ) dup 0 ROW-N+ ROW-STR+  PRIM-NAME ;
: STATE-OPEN ( str -- ) dup 1 ROW-N+ ROW-STR+  PRIM-OPEN ;
: STATE-ATOM ( n -- ) dup 2 ROW-N+ ROW-N+  PRIM-ATOM ;

: STATE-CLOSE ( str bool bool -- )
   {: pkg:str trusted:bool fixed:bool :}
   3 ROW-N+  pkg ROW-STR+  trusted ROW-FLAG+  fixed ROW-FLAG+
   pkg trusted fixed PRIM-CLOSE ;

package NATIVE

\ The host-entry table, ascending by host entry: HOSTS[i] is where a running
\ hb's kernel primitive enters, ROWS[i] the product kernel row of that primitive.
DYNAMIC-BUFFER HOSTS n
DYNAMIC-BUFFER ROWS n
variable N-HOSTS


\ CHECK passes only an object as hb wrote it, and BAD stops one whose layout
\ this hb does not read, so the readers index it unchecked.

TYPED-VARIABLE KOBJ-CELL ^VEC:vector<u8> SCRATCH-ONLY
PTR-VARIABLE PARSED                  \ the bytes NEXT parses: READ's object, or the rows STATE-PRIMS states
variable PARSED-U                    \ their length
variable AT                          \ NEXT's offset in them
variable CODE-AT
variable CODE-U
variable LABELS-AT
variable LABELS-N
variable REFS-AT
variable REFS-N
variable SPANS-AT
variable SPANS-N
variable FIXUPS-AT
variable FIXUPS-N
DYNAMIC-BUFFER PRIM-AT n             \ primitive row k's offset in KOBJ
variable PRIMS-N
variable SCOPES-AT
variable SCOPES-N
variable SYMS-AT
variable SYMS-N
variable KTEXT-VA                    \ the values of the object's TEXT-VA and DATA-VA symbols, which READ fills
variable KDATA-VA
variable PRESETS-AT
variable PRESETS-N
variable MACHO-AT                    \ the Mach-O import names' bytes in KOBJ
variable MACHO-U
variable ELF-AT                      \ the ELF import names' bytes in KOBJ
variable ELF-U
variable CHECK-ROWS-AT               \ the checker rows' bytes in KOBJ
variable CHECK-ROWS-U
DYNAMIC-BUFFER OWNED-AT n            \ each package-private row's package, then name: offset in PARSED, length
variable OWNED-ROWS
PATH-CAP BUFFER: PATH                \ READ's path, for its error lines
variable PATH-U


: SAY-OBJECT ( -- ) s" link: kernel object " SAY  PATH PATH-U @ >str SAY  SAY-CR ;

: BAD ( -- )
   SAY-OBJECT
   s" link: this hb does not read the kernel object's layout: build its tree with a stage (docs/bootstrap.md)" EXIT-STATUS:DATAERR die ;

: KOBJ-VEC ( -- ^VEC:vector<u8> )
   KOBJ-CELL @ NULL-PTR = if
      HEAP:ALLOCATOR 0 [type] u8 VEC:NEW KOBJ-CELL !
   then
   KOBJ-CELL @ ;

\ The address of the object's byte n; KOBJ-VEC's COUNT is its length, 0
\ before READ.
: KOBJ ( n -- ^u8 ) KOBJ-VEC swap VEC:AT ;

: KOBJ@ ( n -- n ) 0 KOBJ swap + LE:U64@ ;

: PARSED-BASE ( -- ^u8 ) PARSED @ ;

: NEXT ( -- n )
   AT @ 8 + PARSED-U @ > if BAD then
   PARSED-BASE AT @ + LE:U64@  8 AT +! ;

: SKIP ( n -- )
   {: u:n :}
   u 0<  u PARSED-U @ > or if BAD then
   u 7 + -8 and AT +!
   AT @ PARSED-U @ > if BAD then ;

\ A section of entries of n cells each: its first entry's offset and its count.
: TABLE ( n -- n n )
   {: each:n :}
   NEXT {: count:n :}
   count 0<  count PARSED-U @ > or if BAD then
   AT @  count each * 8 * SKIP  count ;

\ Each primitive row's offset: three cells, then its name.
: PRIM-ROWS ( -- )
   NEXT {: count:n :}
   count 0<  count PARSED-U @ > or if BAD then
   count PRIMS-N !
   count 1 max PRIM-AT-RESERVE
   count 0 ?do  AT @ i PRIM-AT !  24 SKIP  NEXT SKIP  loop ;

\ Each symbol: its name, then its value.
: SYMBOLS ( -- )
   NEXT {: count:n :}
   count 0<  count PARSED-U @ > or if BAD then
   AT @ SYMS-AT !  count SYMS-N !
   count 0 ?do  NEXT SKIP  8 SKIP  loop ;

\ An import table's offset in KOBJ and its length: names each ending in a NUL,
\ so the last byte is one.
: IMPORTS ( -- n n )
   NEXT {: u:n :}
   AT @ {: off:n :}
   u SKIP
   u 0 > if off u + 1 - PARSED-BASE swap + c@ 0<> if BAD then then
   off u ;

: SECTIONS ( -- )
   40 AT !
   NEXT {: codeu:n :}
   AT @ CODE-AT !  codeu CODE-U !  codeu SKIP
   X64? 0= codeu 3 and 0<> and if BAD then
   1 TABLE LABELS-N ! LABELS-AT !
   3 TABLE REFS-N ! REFS-AT !
   2 TABLE SPANS-N ! SPANS-AT !
   3 TABLE FIXUPS-N ! FIXUPS-AT !
   PRIM-ROWS
   1 TABLE SCOPES-N ! SCOPES-AT !
   SYMBOLS
   2 TABLE PRESETS-N ! PRESETS-AT !
   IMPORTS MACHO-U ! MACHO-AT !
   IMPORTS ELF-U ! ELF-AT !
   NEXT {: rowsu:n :}
   AT @ CHECK-ROWS-AT !  rowsu CHECK-ROWS-U !  rowsu SKIP
   AT @ PARSED-U @ <> if BAD then ;

: CHECK ( -- )
   KOBJ-VEC VEC:COUNT 40 < if PATH PATH-U @ >str s" checksum" DAMAGED then
   0 KOBJ 8 >str s" HBKERNX6" STR:= X64!
   X64? 0= if
      0 KOBJ 8 >str s" HBKERNEL" STR:= 0= if PATH PATH-U @ >str s" checksum" DAMAGED then
   then
   KOBJ-VEC VEC:BYTES SEALED? 0= if PATH PATH-U @ >str s" checksum" DAMAGED then ;

: LABEL-AT ( n -- n ) 8 * LABELS-AT @ + KOBJ@ ;

\ The name at AT: its length, then its bytes padded to 8.
: NAME-NEXT ( -- str )
   NEXT {: u:n :}
   AT @ {: at:n :}
   u SKIP
   PARSED-BASE at +  u >str ;

\ Cells 0-3 hold hb's row's cells, 4-7 the tree's: data in, data out, return
\ in, return out.
8 TYPED-BUFFER COUNTS n

: COUNTS! ( n n n n n -- ) {: din:n dout:n rin:n rout:n at:n :}
   din at COUNTS !  dout at 1 + COUNTS !
   rin at 2 + COUNTS !  rout at 3 + COUNTS ! ;

: SAME-COUNTS? ( -- bool )
   true  4 0 ?do  i COUNTS @  i 4 + COUNTS @ =  and  loop ;

: EMIT-COUNTS ( n str -- ) {: at:n whose:str :}
   s" data " REJECTION:EMIT-STR  at COUNTS @ REJECTION:EMIT-U
   s"  -- " REJECTION:EMIT-STR  at 1 + COUNTS @ REJECTION:EMIT-U
   s" , return " REJECTION:EMIT-STR  at 2 + COUNTS @ REJECTION:EMIT-U
   s"  -- " REJECTION:EMIT-STR  at 3 + COUNTS @ REJECTION:EMIT-U
   whose REJECTION:EMIT-STR ;

: CELLS-DIFFER ( str -- ) {: name:str :}
   s" E-PRIM-CELLS" REJECTION:OPEN
   s" the row of primitive `" REJECTION:EMIT-STR  name REJECTION:EMIT-STR
   s" ` in src/habu/prims.f takes other cells than the running hb's body" REJECTION:EMIT-STR
   s" expected" REJECTION:NOTE  0 s" , the running hb's row" EMIT-COUNTS
   s" found" REJECTION:NOTE  4 s" , the tree's row" EMIT-COUNTS
   REJECTION:HELP
   s" give the new cells to a new primitive first, build with it, then move the callers" REJECTION:EMIT-STR
   EXIT-STATUS:DATAERR REJECTION:STOP ;

: HB-CELLS ( n -- n n n n ) {: k:n :}
   k PRIM-SPEC:NAME$ PRIM-OPEN
   k PRIM-SPEC:CODE-LEN@ 0 ?do  k i PRIM-SPEC:CODE@ PRIM-ATOM  loop
   PRIM-CELLS ;

\ The cells of the tree's row of the name, whose n atoms start at AT, left there.
: TREE-CELLS ( str n -- n n n n ) {: name:str atoms:n :}
   AT @ {: at:n :}
   name PRIM-OPEN  atoms 0 ?do NEXT PRIM-ATOM loop  PRIM-CELLS
   at AT ! ;

\ Stops the build when hb's row of the name takes other cells than the tree's, whose n atoms start at AT: build-time code
\ typed by the tree's row runs hb's body. A name hb lacks is a new primitive; a row hb elaborates has no cells to compare.
: SAME-CELLS ( str n -- ) {: name:str atoms:n :}
   name PRIM-SPEC:FIND {: k:n :}
   k 0< if exit then
   k PRIM-SPEC:KIND@ PRIM-SPEC:K-ELAB = if exit then
   k HB-CELLS 0 COUNTS!
   name atoms TREE-CELLS 4 COUNTS!
   SAME-COUNTS? 0= if name CELLS-DIFFER then ;

\ One row, stated by kind: a package's private row on its package's private
\ symbol, kept for the record linker.f OWNED+ lays of it.
: STATE-ROW ( -- )
   NEXT NEXT NEXT {: kind:n trusted:n fixed:n :}
   AT @ 8 + {: name-at:n :}
   NAME-NEXT {: name:str :}
   AT @ 8 + {: pkg-at:n :}
   NAME-NEXT {: pkg:str :}
   NEXT {: atoms:n :}
   atoms 0<  atoms PARSED-U @ > or if BAD then
   kind PRIM-SPEC:K-PKG-PRIVATE = {: owned:bool :}
   owned if
      OWNED-ROWS @ 4 * {: o:n :}
      o 4 + OWNED-AT-RESERVE
      pkg-at o OWNED-AT !  pkg STR:LEN o 1 + OWNED-AT !
      name-at o 2 + OWNED-AT !  name STR:LEN o 3 + OWNED-AT !
      1 OWNED-ROWS +!
   then
   kind PRIM-SPEC:K-PRIM =  owned or if
      name atoms SAME-CELLS
      name STATE-OPEN
      atoms 0 ?do NEXT STATE-ATOM loop
      pkg  trusted 0<>  fixed 0<>  STATE-CLOSE
      exit
   then
   atoms 8 * SKIP
   kind PRIM-SPEC:K-ELAB = if name STATE-NAME exit then
   BAD ;


public

\ True once READ read an object.
: READ? ( -- bool ) KOBJ-VEC VEC:COUNT 0<> ;

: SYM ( str -- n )
   {: s:str :}
   SYMS-AT @
   SYMS-N @ 0 ?do
      {: at:n :}
      at KOBJ@ {: len:n :}
      at 8 +  len 7 + -8 and + {: value-at:n :}
      0 KOBJ at 8 + +  len >str  s  STR:= if value-at KOBJ@ unloop exit then
      value-at 8 +
   loop
   drop
   SAY-OBJECT
   s" link: the kernel object has no symbol " SAY s SAY SAY-CR
   s" link: the kernel object lacks a symbol the link reads" EXIT-STATUS:DATAERR die ;

: LABEL-REF ( n -- label )
   X64? if X64CODE:LABEL-REF else >LABEL then ;

: SYM-LABEL ( str -- label ) SYM LABEL-REF ;

\ The address the target links its code at, from the kernel object.
: TEXT-VA ( -- n ) KTEXT-VA @ ;

\ The address the target maps its DATA at, from the kernel object.
: DATA-VA ( -- n ) KDATA-VA @ ;

\ The object's primitive rows, as the tree's writer registered them.
: ROW-N ( -- n ) PRIMS-N @ ;
: ROW-FIRST ( n -- label ) PRIM-AT @ KOBJ@ LABEL-REF ;
: ROW-LAST ( n -- label ) PRIM-AT @ 8 + KOBJ@ LABEL-REF ;
: ROW-WID ( n -- n ) PRIM-AT @ 16 + KOBJ@ ;

: ROW-NAME$ ( n -- str )
   PRIM-AT @ 24 + {: at:n :}
   0 KOBJ at 8 + +  at KOBJ@ >str ;

\ The package-private rows STATE-PRIMS stated: row k's package and name.
: OWNED-N ( -- n ) OWNED-ROWS @ ;
: OWNED-PKG$ ( n -- str ) 4 * {: o:n :}  PARSED-BASE o OWNED-AT @ +  o 1 + OWNED-AT @ >str ;
: OWNED-NAME$ ( n -- str ) 4 * {: o:n :}  PARSED-BASE o 2 + OWNED-AT @ +  o 3 + OWNED-AT @ >str ;

\ The label of the chain holding the entry scope-kind? answers kind k for, k
\ from 2 to 8.
: SCOPE-SITE ( n -- label )
   2 - {: j:n :}
   j 0<  j SCOPES-N @ >= or if
      s" link: the kernel object has no scope site of that kind" EXIT-STATUS:DATAERR die
   then
   j 8 * SCOPES-AT @ + KOBJ@ LABEL-REF ;

\ The Mach-O and the ELF import sections' bytes.
: MACHO-IMPORTS$ ( -- str ) 0 KOBJ MACHO-AT @ +  MACHO-U @ >str ;
: ELF-IMPORTS$ ( -- str ) 0 KOBJ ELF-AT @ +  ELF-U @ >str ;

\ The header cells the object gives a value: preset k's value and offset.
: PRESET-N ( -- n ) PRESETS-N @ ;

: PRESET@ ( n -- n n )
   16 * PRESETS-AT @ + {: at:n :}
   at 8 + KOBJ@  at KOBJ@ ;

private

\ The file at the path into KOBJ-VEC, which is empty; false when it is missing.
: KOBJ-FILL ( str -- bool )
   {: path:str :}
   path FS:OPEN-SIZED {: file:fd size:n found:bool :}
   found 0= if false exit then
   file KOBJ-VEC size FS:READ-SIZED
   true ;

public

\ The kernel object at the path, which the links that follow adopt; one whose
\ DATA-START is above hb's dies: hb routes only its own header's stores.
: READ ( str -- )
   {: path:str :}
   KOBJ-VEC 0 VEC:RESIZE
   path STR:LEN PATH-CAP min PATH-U !
   path STR:DATA PATH PATH-U @ BYTE-COPY
   [: path KOBJ-FILL ;] catch {: found code:n :}
   path code READ-FAILED
   code 0<> if code throw then
   found 0= if
      s" link: no kernel object at " SAY path SAY SAY-CR
      s" link: the kernel object is missing" EXIT-STATUS:NOINPUT die
   then
   CHECK
   0 KOBJ PARSED !  KOBJ-VEC VEC:COUNT PARSED-U !
   SECTIONS
   s" DATA-START" SYM {: theirs:n :}
   theirs DATA-START > if
      SAY-OBJECT
      s" link: its DATA-START is " SAY theirs SAY-INT
      s" , hb's is " SAY DATA-START SAY-INT SAY-CR
      s" link: a target may not raise DATA-START above the running hb's" EXIT-STATUS:DATAERR die
   then
   s" TEXT-VA" SYM KTEXT-VA !  s" DATA-VA" SYM KDATA-VA ! ;

\ READ's object's checker rows: the count, then the rows.
: CHECK-ROWS$ ( -- str ) 0 KOBJ CHECK-ROWS-AT @ +  CHECK-ROWS-U @ >str ;

\ States the rows in the bytes, a count then the rows, to the open build's checker in table order, which overloads
\ keep. OWNED-PKG$ and OWNED-NAME$ read the bytes, so the caller keeps them until it has read those.
: STATE-PRIMS ( str -- )
   {: s:str :}
   s STR:DATA PARSED !  s STR:LEN PARSED-U !  0 AT !
   NEXT {: count:n :}
   count 0<  count PARSED-U @ > or if BAD then
   0 OWNED-ROWS !
   count 0 ?do STATE-ROW loop
   AT @ PARSED-U @ <> if BAD then
   PRIMS-END ;

: HOST-KERNEL? ( n -- bool )
   {: a:n :}
   a rbase >= a data-base CODE-END-CELL + @ < and ;

private

: SHIFT-UP ( n -- )
   {: j:n :}
   j 1 - HOSTS @ j HOSTS !
   j 1 - ROWS @ j ROWS ! ;


: INSERT ( n n -- )
   {: host:n row:n :}
   N-HOSTS @ {: k:n :}
   k 1 + HOSTS-RESERVE
   k 1 + ROWS-RESERVE
   k begin dup 0 > if dup 1 - HOSTS @ host > else false then while
      dup SHIFT-UP 1 -
   repeat {: at:n :}
   host at HOSTS !
   row at ROWS !
   k 1 + N-HOSTS ! ;


\ The object's row of the primitive with the name, or -1. Seed names are
\ unique across wordlists, and an owned row's wordlist differs between hbs.
: ROW-OF ( str -- n )
   {: s:str :}
   ROW-N 0 ?do
      i ROW-NAME$ s STR:=CI if i unloop exit then
   loop
   -1 ;


\ The running hb's primitive table; its row count is the cell before it.
: HOST-TABLE ( -- n )
   data-base PRIM-TABLE-CELL + @ {: off:n :}
   rbase off + ;

: HOST-ROW ( n n -- ^u8 )
   {: table:n k:n :}
   table k DREC * +  DREC CODE-BYTES:AT STR:DATA ;


\ A table row enters at rbase plus [0] and keeps an out-of-line name at rbase
\ plus [24]; a package row, wordlist -1, holds its wid
\ in [0], never code.
: KEEP ( ^u8 -- )
   {: rec:^ :}
   rec 40 + LE:U64@ {: wid:n :}
   wid -1 = if exit then
   rec 16 + LE:U64@ {: flags:n :}
   flags DNAME-LEN-MASK and {: len:n :}
   flags DNAME-EXT and 0<> if
      rbase rec 24 + LE:U64@ +  len CODE-BYTES:AT
   else
      rec 24 +  len >str
   then
   ROW-OF {: row:n :}
   row 0 < if exit then
   rbase rec LE:U64@ +  row INSERT ;


: HOST-ENTRIES ( -- )
   0 N-HOSTS !
   HOST-TABLE {: table:n :}
   table 8 - 8 CODE-BYTES:AT STR:DATA LE:U64@
   0 ?do table i HOST-ROW KEEP loop ;


\ The table index whose host entry is a, or -1.
: FIND ( n -- n )
   {: a:n :}
   0 N-HOSTS @
   [: HOSTS @ a < ;] SORT:PARTITION-POINT {: i:n :}
   i N-HOSTS @ <  if i HOSTS @ a = if i exit then then
   -1 ;


\ A position is an instruction word of the ARM64 code buffer, a byte of the
\ x86-64 one. K0 is the kernel's first. Nothing emits code between ADOPT and
\ CUT, so the region CUT edits is [K0, REGION-END).
variable K0

\ Labels whose kernel code the program reaches besides the start-up's
\ references, as LABEL>N.
DYNAMIC-BUFFER ROOTS n
variable N-ROOTS

\ A node runs from K0 or a label bound in the region to the next one or
\ REGION-END; NODE-OF holds each position's node, STARTS each node's first, KEPT
\ [start, end).
DYNAMIC-BUFFER NODE-OF n
DYNAMIC-BUFFER STARTS n
DYNAMIC-BUFFER REACHED n
DYNAMIC-BUFFER WORK n
DYNAMIC-BUFFER KEPT n
variable N-NODES
variable N-WORK
variable N-KEPT


: REGION-END ( -- n )
   X64? if X64CODE:ASM-LEN exit then
   A64ICODE:ASM-CP @ ;

: IN-REGION? ( n -- bool )
   {: w:n :}
   w K0 @ >=  w REGION-END < and ;

: LABEL-COUNT ( -- n )
   X64? if X64CODE:LABEL-COUNT exit then
   A64ICODE:NLBL @ ;

\ Label number k's position, negative while it is unbound.
: LABEL-K ( n -- n )
   X64? if X64CODE:LABEL-POS exit then
   A64ICODE:LBLP @ ;

: LABEL-POS ( label -- n )
   X64? if X64CODE:LABEL-INDEX else LABEL>N then LABEL-K ;

\ Reference records ascend by site.
: REF-COUNT ( -- n )
   X64? if X64CODE:SITES exit then
   A64ICODE:REFS ;

: REF-SITE ( n -- n )
   X64? if X64CODE:REF-SITE exit then
   A64ICODE:REF-SITE ;

\ The position of record k's label.
: REF-POS ( n -- n )
   X64? if X64CODE:REF-LABEL LABEL-K exit then
   A64ICODE:REF-LABEL LABEL-POS ;


\ NODE-OF holds 1 at each node's start and 0 elsewhere until NUMBER.
: MARK-STARTS ( -- )
   REGION-END K0 @ - {: n:n :}
   n NODE-OF-RESERVE
   n 0 ?do 0 i NODE-OF ! loop
   1 0 NODE-OF !
   LABEL-COUNT 0 ?do
      i LABEL-K dup IN-REGION? if K0 @ - 1 swap NODE-OF ! else drop then
   loop ;


: NUMBER ( -- )
   -1
   REGION-END K0 @ - 0 ?do
      i NODE-OF @ 0<> if
         1 +  dup 1 + STARTS-RESERVE  i K0 @ + over STARTS !
      then
      dup i NODE-OF !
   loop
   1 + N-NODES ! ;


: NODE-END ( n -- n )
   1 + dup N-NODES @ < if STARTS @ exit then
   drop REGION-END ;


\ Position p reaches the node holding it, once; a position outside the
\ region, an unbound label's -1 among them, reaches nothing.
: REACH ( n -- )
   {: p:n :}
   p IN-REGION? 0= if exit then
   p K0 @ - NODE-OF @ {: node:n :}
   node REACHED @ 0<> if exit then
   1 node REACHED !
   N-WORK @ 1 + WORK-RESERVE
   node N-WORK @ WORK !
   1 N-WORK +! ;


\ The instruction word at word position w, little-endian.
: WORD@ ( n -- n ) A64ICODE:CW@ LE:U32@ ;


\ ret, brk, or b or br whatever its field holds: control never passes on.
: TERMINAL? ( n -- bool )
   {: w:n :}
   w A64ASM:ENC-RET =
   w A64ASM:ENC-BRK = or
   w  -4 A64ASM:ENC-B 0 A64ASM:ENC-B xor invert and  0 A64ASM:ENC-B = or
   w  31 A64ASM:ENC-BR 0 A64ASM:ENC-BR xor invert and  0 A64ASM:ENC-BR = or ;


\ The first reference record whose site is at or after position a.
: FIRST-REF ( n -- n )
   {: a:n :}
   0 REF-COUNT
   [: REF-SITE a < ;] SORT:PARTITION-POINT ;


\ The first data span ending after word position a.
: FIRST-SPAN ( n -- n )
   {: a:n :}
   0 A64ICODE:DATA-SPANS
   [: A64ICODE:DATA-SPAN-END a <= ;] SORT:PARTITION-POINT ;

\ True when word position a lies in a data span.
: DATA? ( n -- bool )
   {: a:n :}
   a FIRST-SPAN {: k:n :}
   k A64ICODE:DATA-SPANS < if k A64ICODE:DATA-SPAN-FIRST a <= else false then ;

\ Control passes on from ARM64 word position a: it is code that is not terminal.
: ARM-FALLS? ( n -- bool )
   {: a:n :}
   a DATA? if false exit then
   a WORD@ TERMINAL? 0= ;

\ Control passes on past x86-64 position b: the kernel's writer put no end there.
: X64-FALLS? ( n -- bool )
   {: b:n :}
   0 X64CODE:ENDS [: X64CODE:END-AT b < ;] SORT:PARTITION-POINT {: k:n :}
   k X64CODE:ENDS < if k X64CODE:END-AT b <> else true then ;

\ Control passes on from the end of a node ending at position b.
: FALLS? ( n -- bool )
   X64? if X64-FALLS? exit then
   1 - ARM-FALLS? ;


\ A node reaches the labels its sites name and, when control passes on from its
\ end, the node after it: a call of a word that never returns counts as
\ falling through, so reach is an upper bound.
: VISIT ( n -- )
   {: node:n :}
   node STARTS @ {: a:n :}
   node NODE-END {: b:n :}
   a FIRST-REF
   begin dup REF-COUNT < if dup REF-SITE b < else false then while
      dup REF-POS REACH  1 +
   repeat drop
   b REGION-END < if b FALLS? if b REACH then then ;


\ The start-up's references, the records sited below K0, root the walk; its last
\ code passes control nowhere, so nothing falls into node 0.
: ROOT-STARTUP ( -- )
   K0 @ FIRST-REF 0 ?do i REF-POS REACH loop ;


: ROOT-LABELS ( -- )
   N-ROOTS @ 0 ?do i ROOTS @ >LABEL LABEL-POS REACH loop ;


: WALK ( -- )
   MARK-STARTS NUMBER
   N-NODES @ REACHED-RESERVE
   N-NODES @ 0 ?do 0 i REACHED ! loop
   0 N-WORK !
   ROOT-STARTUP ROOT-LABELS
   begin N-WORK @ 0 > while
      -1 N-WORK +!  N-WORK @ WORK @ VISIT
   repeat ;


: KEEP-NODE ( n -- )
   {: node:n :}
   node STARTS @ {: a:n :}
   node NODE-END {: b:n :}
   N-KEPT @ 0 > if
      N-KEPT @ 2 * 1 - KEPT @ a = if b N-KEPT @ 2 * 1 - KEPT ! exit then
   then
   N-KEPT @ 1 + 2 * KEPT-RESERVE
   a N-KEPT @ 2 * KEPT !
   b N-KEPT @ 2 * 1 + KEPT !
   1 N-KEPT +! ;


public

\ A label's byte offset in the code buffer, negative while it is unbound.
: OFFSET ( label -- n )
   LABEL-POS  X64? if exit then  4 * ;


\ The entry of a product kernel row the link kept, in bytes from the start of the
\ code buffer; negative for a row CUT removed.
: PRIM-OFFSET ( n -- n ) ROW-FIRST OFFSET ;


private

\ The object READ read as the code buffer's start: the start-up, then the
\ kernel, with their labels, references, data spans and pending fixups as the
\ writer left them; the product's own code follows. No C2 entry is bound.
: ARM-ADOPT ( -- )
   A64ICODE:ASM-INIT
   CODE-U @ 4 / {: words:n :}
   words A64ICODE:ASM-CP?
   0 KOBJ CODE-AT @ +  A64ICODE:CODE  CODE-U @  BYTE-COPY
   words A64ICODE:ASM-CP !
   LABELS-N @ 0 ?do  A64ICODE:LBL drop  i LABEL-AT  i A64ICODE:LBLP !  loop
   REFS-N @ 0 ?do
      i 24 * REFS-AT @ + {: at:n :}
      at KOBJ@  at 8 + KOBJ@ >LABEL  at 16 + KOBJ@  A64ICODE:REF+
   loop
   SPANS-N @ 0 ?do
      i 16 * SPANS-AT @ + {: at:n :}
      at KOBJ@  at 8 + KOBJ@  A64ICODE:DATA-SPAN+
   loop
   FIXUPS-N @ 0 ?do
      i 24 * FIXUPS-AT @ + {: at:n :}
      at KOBJ@  at 8 + KOBJ@ >LABEL  at 16 + KOBJ@  A64ICODE:FX+
   loop
   s" K0" SYM K0 !  0 N-ROOTS !
   HOST-ENTRIES ;

: X64-ADOPT ( -- )
   X64CODE:SINK-OPEN
   0 KOBJ CODE-AT @ + CODE-U @ >str X64CODE:ASM-SINK VEC:APPEND
   LABELS-N @ 0 ?do
      X64CODE:LBL drop i LABEL-AT i X64CODE:LABEL-POS!
   loop
   REFS-N @ 0 ?do
      i 24 * REFS-AT @ + {: at:n :}
      at KOBJ@ at 8 + KOBJ@ at 16 + KOBJ@ X64CODE:REF+
   loop
   SPANS-N @ 0 ?do
      i 16 * SPANS-AT @ + {: at:n :}
      at KOBJ@ at 8 + KOBJ@ X64CODE:END+
   loop
   s" K0" SYM K0 ! 0 N-ROOTS !
   HOST-ENTRIES ;

public

: ADOPT ( -- )
   READ? 0= if s" link: no kernel object was read" EXIT-STATUS:INTERNAL die then
   X64? if X64-ADOPT else ARM-ADOPT then ;


\ The kernel's first byte, where the start-up ends; CUT keeps the start-up whole.
: KERNEL-AT ( -- n ) K0 @ X64? if exit then 4 * ;


\ The product kernel row of the primitive that enters the running hb at the
\ host address, or -1 when no host-entry row enters exactly there.
: KERNEL-ROW ( n -- n )
   FIND dup 0 < if exit then
   ROWS @ ;

\ That primitive's product kernel offset, or -1.
: KERNEL-ENTRY ( n -- n )
   KERNEL-ROW dup 0 < if exit then
   PRIM-OFFSET ;


\ Keeps the kernel code at a label; one bound outside the kernel keeps nothing.
: ROOT-LABEL ( label -- )
   N-ROOTS @ 1 + ROOTS-RESERVE
   LABEL>N N-ROOTS @ ROOTS !
   1 N-ROOTS +! ;


\ True when host address a is a kernel primitive's entry, whose code it then
\ keeps.
: ROOT-ENTRY ( n -- bool )
   FIND dup 0 < if drop false exit then
   ROWS @ ROW-FIRST ROOT-LABEL true ;


\ Removes the kernel nodes WALK does not reach and moves the rest down.
: CUT ( -- )
   WALK
   0 N-KEPT !  2 KEPT-RESERVE
   N-NODES @ 0 ?do i REACHED @ 0<> if i KEEP-NODE then loop
   0 KEPT N-KEPT @ K0 @  X64? if X64CODE:CUT else A64ICODE:CUT then
   NODE-OF-RELEASE STARTS-RELEASE REACHED-RELEASE WORK-RELEASE KEPT-RELEASE ;

;package
;package
