\ link-kernel.f - the arm64 kernel a build's link adopts from the kernel object
\ a kernel build writes, the map from the running hb's kernel primitive entries
\ to the object's, and the cut that keeps only the kernel code a program
\ reaches. The two kernels bake different inputs, so the map goes
\ by name and wordlist.
require src/lib/le.f
require src/lib/string.f
require src/lib/fs.f
require src/core/bytes.f
require src/core/sha256.f
require src/arch/arm64/asm.f
require src/arch/arm64/icode.f
require src/arch/arm64/mnem.f
require src/habu/layout.f
require src/habu/code-bytes.f

package LINKER
private

\ The linker's error lines go whole to the error output.
: SAY ( ptr u8 n -- ) ERROR-TEXT:TYPE ;

: SAY-INT ( n -- )
   dup 0< if s" -" SAY then
   ERROR-TEXT:DIGITS$ SAY ;

: SAY-CR ( -- ) s\" \n" SAY ;

package ARM64

\ The host-entry table, ascending by host entry: HOSTS[i] is where a running
\ hb's kernel primitive enters, ROWS[i] the product kernel row of that primitive.
DYNAMIC-BUFFER HOSTS n
DYNAMIC-BUFFER ROWS n
variable N-HOSTS


\ READ checks the whole object and every section's extent once, so the readers
\ after it index it unchecked.

DYNAMIC-BUFFER KOBJ u8
variable KOBJ-U                      \ its length, 0 before READ
variable AT                          \ READ's offset in it
variable CODE-AT
variable CODE-U
variable LABELS-AT
variable LABELS-N
variable REFS-AT
variable REFS-N
variable FIXUPS-AT
variable FIXUPS-N
DYNAMIC-BUFFER PRIM-AT n             \ primitive row k's offset in KOBJ
variable PRIMS-N
variable SCOPES-AT
variable SCOPES-N
variable SYMS-AT
variable SYMS-N
variable PRESETS-AT
variable PRESETS-N
variable CHECK-ROWS-AT               \ the checker rows' bytes in KOBJ
variable CHECK-ROWS-U
DYNAMIC-BUFFER OWNED-AT n            \ each package-private row's package, then name: offset in KOBJ, length
variable OWNED-ROWS
SHA256-CTX-BYTES BUFFER: SHA-CTX
32 BUFFER: SUM
PATH-CAP BUFFER: PATH                \ READ's path, for its error lines
variable PATH-U


: SAY-OBJECT ( -- ) s" link: kernel object " SAY  PATH PATH-U @ SAY  SAY-CR ;

: BAD ( -- )
   SAY-OBJECT
   s" link: the kernel object is cut short or malformed" 74 die ;

: SAME ( n n ptr u8 n -- )
   {: mine:n theirs:n a:ptr u:n :}
   mine theirs = if exit then
   SAY-OBJECT
   s" link: its " SAY a u SAY s"  is " SAY theirs SAY-INT
   s" , hb's is " SAY mine SAY-INT SAY-CR
   s" link: the kernel object's code or data layout is not hb's" 74 die ;

: KOBJ@ ( n -- n ) 0 KOBJ swap + LE:U64@ ;

: NEXT ( -- n )
   AT @ 8 + KOBJ-U @ > if BAD then
   AT @ KOBJ@  8 AT +! ;

: SKIP ( n -- )
   {: u:n :}
   u 0<  u KOBJ-U @ > or if BAD then
   u 7 + -8 and AT +!
   AT @ KOBJ-U @ > if BAD then ;

\ A section of entries of n cells each: its first entry's offset and its count.
: TABLE ( n -- n n )
   {: each:n :}
   NEXT {: count:n :}
   count 0<  count KOBJ-U @ > or if BAD then
   AT @  count each * 8 * SKIP  count ;

\ Each primitive row's offset: three cells, then its name.
: PRIM-ROWS ( -- )
   NEXT {: count:n :}
   count 0<  count KOBJ-U @ > or if BAD then
   count PRIMS-N !
   count 1 max PRIM-AT-RESERVE
   count 0 ?do  AT @ i PRIM-AT !  24 SKIP  NEXT SKIP  loop ;

\ Each symbol: its name, then its value.
: SYMBOLS ( -- )
   NEXT {: count:n :}
   count 0<  count KOBJ-U @ > or if BAD then
   AT @ SYMS-AT !  count SYMS-N !
   count 0 ?do  NEXT SKIP  8 SKIP  loop ;

: SECTIONS ( -- )
   40 AT !
   NEXT {: codeu:n :}
   AT @ CODE-AT !  codeu CODE-U !  codeu SKIP
   codeu 3 and 0<> if BAD then
   1 TABLE LABELS-N ! LABELS-AT !
   3 TABLE REFS-N ! REFS-AT !
   3 TABLE FIXUPS-N ! FIXUPS-AT !
   PRIM-ROWS
   1 TABLE SCOPES-N ! SCOPES-AT !
   SYMBOLS
   2 TABLE PRESETS-N ! PRESETS-AT !
   AT @ KOBJ-U @ = if
      SAY-OBJECT
      s" link: the kernel object has no checker rows: its tree is older than this hb" 74 die
   then
   NEXT {: rowsu:n :}
   AT @ CHECK-ROWS-AT !  rowsu CHECK-ROWS-U !  rowsu SKIP
   AT @ KOBJ-U @ <> if BAD then ;

: CHECK ( -- )
   KOBJ-U @ 40 < if BAD then
   0 KOBJ 8 s" HBKERNEL" STR= 0= if BAD then
   SHA-CTX  0 KOBJ 40 +  KOBJ-U @ 40 -  SUM SHA256-IN
   SUM 32  0 KOBJ 8 + 32  STR= 0= if
      SAY-OBJECT
      s" link: the kernel object fails its checksum" 74 die
   then ;

: LABEL-AT ( n -- n ) 8 * LABELS-AT @ + KOBJ@ ;

\ The name at AT: its length, then its bytes padded to 8.
: NAME-NEXT ( -- ptr u8 n )
   NEXT {: u:n :}
   AT @ {: at:n :}
   u SKIP
   0 KOBJ at +  u ;

\ One row, stated by kind: a package's private row on its package's private
\ symbol, kept for the record linker.f OWNED+ lays of it.
: STATE-ROW ( -- )
   NEXT NEXT NEXT NEXT {: kind:n trusted:n fixed:n stow:n :}
   AT @ 8 + {: name-at:n :}
   NAME-NEXT {: name:ptr nameu:n :}
   AT @ 8 + {: pkg-at:n :}
   NAME-NEXT {: pkg:ptr pkgu:n :}
   NEXT {: atoms:n :}
   atoms 0<  atoms KOBJ-U @ > or if BAD then
   kind PRIM-SPEC:K-PKG-PRIVATE = {: owned:bool :}
   owned if
      OWNED-ROWS @ 4 * {: o:n :}
      o 4 + OWNED-AT-RESERVE
      pkg-at o OWNED-AT !  pkgu o 1 + OWNED-AT !
      name-at o 2 + OWNED-AT !  nameu o 3 + OWNED-AT !
      1 OWNED-ROWS +!
   then
   kind PRIM-SPEC:K-PRIM =  owned or if
      name nameu CHECK:PRIM-OPEN
      atoms 0 ?do NEXT CHECK:PRIM-ATOM loop
      pkg  pkgu  trusted 0<>  fixed 0<>  stow CHECK:PRIM-CLOSE
      exit
   then
   atoms 8 * SKIP
   kind PRIM-SPEC:K-ELAB = if name nameu CHECK:PRIM-NAME exit then
   kind PRIM-SPEC:K-UNROWED = if exit then
   BAD ;


public

\ True once READ read an object.
: READ? ( -- bool ) KOBJ-U @ 0<> ;

: SYM ( ptr u8 n -- n )
   {: a:ptr u:n :}
   SYMS-AT @
   SYMS-N @ 0 ?do
      {: at:n :}
      at KOBJ@ {: len:n :}
      at 8 +  len 7 + -8 and + {: value-at:n :}
      0 KOBJ at 8 + +  len  a u  STR= if value-at KOBJ@ unloop exit then
      value-at 8 +
   loop
   drop
   SAY-OBJECT
   s" link: the kernel object has no symbol " SAY a u SAY SAY-CR
   s" link: the kernel object lacks a symbol the link reads" 74 die ;

: SYM-LABEL ( ptr u8 n -- label ) SYM >LABEL ;

\ The address the target links its code at, from the kernel object.
: TEXT-VA ( -- n ) s" TEXT-VA" SYM ;

\ The address the target maps its DATA at, from the kernel object.
: DATA-VA ( -- n ) s" DATA-VA" SYM ;

\ The object's primitive rows, as the tree's writer registered them.
: ROW-N ( -- n ) PRIMS-N @ ;
: ROW-FIRST ( n -- label ) PRIM-AT @ KOBJ@ >LABEL ;
: ROW-LAST ( n -- label ) PRIM-AT @ 8 + KOBJ@ >LABEL ;
: ROW-WID ( n -- n ) PRIM-AT @ 16 + KOBJ@ ;

: ROW-NAME$ ( n -- ptr u8 n )
   PRIM-AT @ 24 + {: at:n :}
   0 KOBJ at 8 + +  at KOBJ@ ;

\ The package-private rows STATE-PRIMS stated: row k's package and name.
: OWNED-N ( -- n ) OWNED-ROWS @ ;
: OWNED-PKG$ ( n -- ptr u8 n ) 4 * {: o:n :}  0 KOBJ o OWNED-AT @ +  o 1 + OWNED-AT @ ;
: OWNED-NAME$ ( n -- ptr u8 n ) 4 * {: o:n :}  0 KOBJ o 2 + OWNED-AT @ +  o 3 + OWNED-AT @ ;

\ The label of the chain holding the entry scope-kind? answers kind k for, k
\ from 2 to 8.
: SCOPE-SITE ( n -- label )
   2 - {: j:n :}
   j 0<  j SCOPES-N @ >= or if s" link: the kernel object has no scope site of that kind" 74 die then
   j 8 * SCOPES-AT @ + KOBJ@ >LABEL ;

\ The header cells the object gives a value: preset k's value and offset.
: PRESET-N ( -- n ) PRESETS-N @ ;

: PRESET@ ( n -- n n )
   16 * PRESETS-AT @ + {: at:n :}
   at 8 + KOBJ@  at KOBJ@ ;

\ The kernel object at the path, which the links that follow adopt; one whose
\ DATA-START is not hb's dies.
: READ ( ptr u8 n -- )
   {: path:ptr pathu:n :}
   0 KOBJ-U !
   pathu PATH-CAP min PATH-U !
   path PATH PATH-U @ BYTE-COPY
   path pathu FS:OPEN-SIZED {: file:fd size:n found:bool :}
   found 0= if
      s" link: no kernel object at " SAY path pathu SAY SAY-CR
      s" link: the kernel object is missing" 74 die
   then
   size 1 max KOBJ-RESERVE
   file 0 KOBJ size FS:READ-SIZED
   size KOBJ-U !
   CHECK SECTIONS
   DATA-START  s" DATA-START" SYM  s" DATA-START" SAME ;

\ States READ's object's primitive rows to the open build's checker in table
\ order, which overloads keep; the section ends the object, which NEXT bounds.
: STATE-PRIMS ( -- )
   CHECK-ROWS-AT @ AT !
   CHECK-ROWS-AT @ CHECK-ROWS-U @ + {: end:n :}
   NEXT {: count:n :}
   count 0<  count KOBJ-U @ > or if BAD then
   0 OWNED-ROWS !
   count 0 ?do STATE-ROW loop
   AT @ end <> if BAD then
   CHECK:PRIMS-END ;

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


\ The object's row of the primitive with the name in wordlist w, or -1.
: ROW-OF ( ptr u8 n n -- n )
   {: a:ptr u:n w:n :}
   ROW-N 0 ?do
      i ROW-WID w = if
         i ROW-NAME$ a u CORE-STR=CI if i unloop exit then
      then
   loop
   -1 ;


\ The running hb's primitive table; its row count is the cell before it.
: HOST-TABLE ( -- n )
   data-base PRIM-TABLE-CELL + @ {: off:n :}
   off 0= if s" link: this hb has no primitive table" 74 die then
   rbase off + ;

: HOST-ROW ( n n -- ptr u8 )
   {: table:n k:n :}
   table k DREC * +  DREC CODE-BYTES:AT drop ;


\ A table row enters at rbase plus [0] and keeps an out-of-line name at rbase
\ plus [24]; a package row, wordlist -1, holds its wid
\ in [0], never code.
: KEEP ( ptr u8 -- )
   {: rec:ptr :}
   rec 40 + LE:U64@ {: wid:n :}
   wid -1 = if exit then
   rec 16 + LE:U64@ {: flags:n :}
   flags DNAME-LEN-MASK and {: len:n :}
   flags DNAME-EXT and 0<> if
      rbase rec 24 + LE:U64@ +  len CODE-BYTES:AT
   else
      rec 24 +  len
   then
   wid ROW-OF {: row:n :}
   row 0 < if exit then
   rbase rec LE:U64@ +  row INSERT ;


: HOST-ENTRIES ( -- )
   0 N-HOSTS !
   HOST-TABLE {: table:n :}
   table 8 - 8 CODE-BYTES:AT drop LE:U64@
   0 ?do table i HOST-ROW KEEP loop ;


\ The table index whose host entry is a, or -1.
: FIND ( n -- n )
   {: a:n :}
   0 N-HOSTS @
   begin 2dup < while
      2dup + 2 /
      dup HOSTS @ a = if nip nip exit then
      dup HOSTS @ a < if rot drop 1 + swap else nip then
   repeat
   2drop -1 ;


\ The kernel's first word position. Nothing emits code between ADOPT and CUT,
\ so the region CUT edits is [K0, ASM-CP).
variable K0

\ Labels whose kernel code the program reaches besides the start-up's
\ references, as LABEL>N.
DYNAMIC-BUFFER ROOTS n
variable N-ROOTS

\ A node runs from K0 or a label bound in the region to the next one or ASM-CP;
\ NODE-OF holds each word's node, STARTS each node's first, KEPT [start, end).
DYNAMIC-BUFFER NODE-OF n
DYNAMIC-BUFFER STARTS n
DYNAMIC-BUFFER REACHED n
DYNAMIC-BUFFER WORK n
DYNAMIC-BUFFER KEPT n
variable N-NODES
variable N-WORK
variable N-KEPT


: REGION-END ( -- n ) A64ICODE:ASM-CP @ ;

: IN-REGION? ( n -- bool )
   {: w:n :}
   w K0 @ >=  w REGION-END < and ;

: LABEL-POS ( label -- n ) LABEL>N A64ICODE:LBLP @ ;


\ NODE-OF holds 1 at each node's start and 0 elsewhere until NUMBER.
: MARK-STARTS ( -- )
   REGION-END K0 @ - {: n:n :}
   n NODE-OF-RESERVE
   n 0 ?do 0 i NODE-OF ! loop
   1 0 NODE-OF !
   A64ICODE:NLBL @ 0 ?do
      i A64ICODE:LBLP @ dup IN-REGION? if K0 @ - 1 swap NODE-OF ! else drop then
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


\ Word position p reaches the node holding it, once; a position outside the
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


\ The first reference record whose site is at or after word position a; records
\ ascend by site.
: FIRST-REF ( n -- n )
   {: a:n :}
   0 A64ICODE:REFS
   begin 2dup < while
      2dup + 2 /
      dup A64ICODE:REF-SITE a < if rot drop 1 + swap else nip then
   repeat
   drop ;


\ A node reaches the labels its sites name and, unless its last word is
\ terminal, the node after it: a bl to a word that never returns counts as
\ falling through, so reach is an upper bound.
: VISIT ( n -- )
   {: node:n :}
   node STARTS @ {: a:n :}
   node NODE-END {: b:n :}
   a FIRST-REF
   begin dup A64ICODE:REFS < if dup A64ICODE:REF-SITE b < else false then while
      dup A64ICODE:REF-LABEL LABEL-POS REACH  1 +
   repeat drop
   b REGION-END < if b 1 - WORD@ TERMINAL? 0= if b REACH then then ;


\ The start-up's references, the records sited below K0, root the walk; START,
\ ends in an exit syscall, so nothing falls into node 0.
: ROOT-STARTUP ( -- )
   K0 @ FIRST-REF 0 ?do i A64ICODE:REF-LABEL LABEL-POS REACH loop ;


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

\ A bound label's byte offset in the code buffer.
: OFFSET ( label -- n )
   {: l:label :}
   l A64ICODE:LBL-BOUND? 0= if
      s" link: a label the link reads is unbound" 74 die
   then
   l LABEL-POS 4 * ;


\ A product kernel row's entry, in bytes from the start of the code buffer; a
\ row whose code CUT removed dies in OFFSET.
: PRIM-OFFSET ( n -- n ) ROW-FIRST OFFSET ;


\ The object READ read as the code buffer's start: the start-up, then the
\ kernel, with their labels, references and pending fixups as the writer left
\ them; the product's own code follows. No C2 entry is bound.
: ADOPT ( -- )
   READ? 0= if s" link: no kernel object was read for this link" 74 die then
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
   FIXUPS-N @ 0 ?do
      i 24 * FIXUPS-AT @ + {: at:n :}
      at KOBJ@  at 8 + KOBJ@ >LABEL  at 16 + KOBJ@  A64ICODE:FX+
   loop
   s" K0" SYM K0 !  0 N-ROOTS !
   HOST-ENTRIES ;


\ The kernel's first byte, where the start-up ends; CUT keeps the start-up whole.
: KERNEL-AT ( -- n ) K0 @ 4 * ;


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
   0 KEPT N-KEPT @ K0 @ A64ICODE:CUT
   NODE-OF-RELEASE STARTS-RELEASE REACHED-RELEASE WORK-RELEASE KEPT-RELEASE ;

;package
;package
