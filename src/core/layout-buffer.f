\ layout-buffer.f: the generative storage definers LAYOUT-BUFFER (the only
\ introduction form for `^layout`), DEFER-LAYOUT-BUFFER, TYPED-VARIABLE and
\ TYPED-BUFFER, each making its words as typed children and laying each child's
\ block at the address its body pushes. A ptr variable never gains layout
\ identity by unification. The words live in package LBUF, and the four
\ definers are its global words.

using TYPE-DECL

7122 constant E-LAYOUT-BOUNDS

\ The bytes a stored type takes, for a container's own arithmetic.
CAST: width>n ( width<t> -- n )

package LBUF
EXPORT CHECK:ARENA-BYTES-ENSURE
EXPORT CHECK:REJECT-DUPLICATE
EXPORT CHECK:TYPE-TEXT
EXPORT TFAM:EFFECT-TYPE
EXPORT TFAM:HAS-ADDRESS?
EXPORT TFAM:SUMV-PAY-N
EXPORT TFAM:TFAM-FLD-COUNT@
EXPORT TFAM:TFAM-FLD-START@
EXPORT TFAM:TFAM-NICHE?
EXPORT TFAM:TFAM-STORAGE-FIELD
EXPORT TFAM:TFAM-STORAGE-PAY
EXPORT TFAM:TFAM-STORAGE-PRODUCT?
EXPORT TFAM:TFAM-STORAGE-QUOT?
EXPORT TFAM:TFAM-STORAGE-SUM?
EXPORT TFAM:TFAM-VAR-COUNT@
EXPORT TFAM:TFAM-VAR-START@

$7FFFFFFFFFFFFFFF constant LBUF-N-MAX
0 constant LBUF-FALSE
-1 constant LBUF-TRUE

PTR-VARIABLE LBUF-GEN-P
SCRATCH-VARIABLE LBUF-GEN-CAP
variable LBUF-GEN-U
variable LBUF-I
variable LBUF-N
variable LBUF-W
variable LBUF-BYTES

: LBUF-CLEAR ( -- )
   0 LBUF-GEN-U ! ;

: LBUF-GEN ( -- ^u8 ) LBUF-GEN-P @ ;

: LBUF-C, ( n -- ) {: c:n :}
   LBUF-GEN-P LBUF-GEN-CAP LBUF-GEN-U @ 1 + ARENA-BYTES-ENSURE
   c LBUF-GEN LBUF-GEN-U @ + c!
   LBUF-GEN-U @ 1 + LBUF-GEN-U ! ;

: LBUF-APP ( str -- ) {: s:str :}
   0 LBUF-I !
   begin LBUF-I @ s STR:LEN < while
      s STR:DATA LBUF-I @ + c@ LBUF-C,
      LBUF-I @ 1 + LBUF-I !
   repeat ;

: LBUF-EXTENT? ( n n -- n bool ) {: count:n width:n :}
   count 0 <= width 0 <= or if 0 LBUF-FALSE exit then
   count LBUF-N-MAX width / > if 0 LBUF-FALSE exit then
   count width * {: cellsn:n :}
   cellsn LBUF-N-MAX CELL / > if 0 LBUF-FALSE exit then
   cellsn cells LBUF-TRUE ;

EXPORT BOOT:data-base
EXPORT CHECK:CHECKER-DEFINED-HERE?
EXPORT CHECK:CHECKER-LAYOUT-INFO
EXPORT CHECK:CHECKER-LAYOUT-TYPE-REJECT
EXPORT CHECK:CHECKER-LBUF-NAME-OK?
EXPORT CHECK:CHECKER-STORAGE-BIND
EXPORT CHECK:CHECKER-STORAGE-COUNT-REJECT
EXPORT CHECK:CHECKER-STORAGE-DEFER
EXPORT CHECK:CHECKER-STORAGE-INFO
EXPORT CHECK:CHECKER-STORAGE-NAME-REJECT
EXPORT CHECK:CHECKER-STORAGE-REJECT
EXPORT CHECK:CHECKER-STORAGE-TERM
EXPORT CHECK:CHECKER-STORAGE-TYPE-REJECT
EXPORT CHECK:CHECKER-UNSEEN-PAIR
EXPORT CHECK:CHECKER-TYPE-SPAN-BREAK?
EXPORT CHECK:CHECKER-TYPE-SPAN-STEP
EXPORT CHECK:CODE-SPAN
EXPORT CHECK:INTRINSIC
EXPORT CHECK:INTRINSIC-DECLARATION
EXPORT CHECK:LBI-BAD
EXPORT CHECK:LBI-CELLS
EXPORT CHECK:LBI-T
EXPORT CHECK:LBI-TERM
EXPORT CHECK:STG-NO-NAME
EXPORT CHECK:STGR-END
EXPORT CHECK:STORAGE-RESET
EXPORT CHECK:T-RES
EXPORT CHECK:T-WIDTH
EXPORT CHECK:TAG
EXPORT TYPE-DECL:TDECL-STORED-CHILD
EXPORT TYPE-DECL:TDECL-NODE-CHILD
: LBUF-VALIDATE ( n str str -- bool )
   {: count:n name:str type:str :}
   type CHECKER-LAYOUT-INFO 0= if
      2drop name type CHECKER-LAYOUT-TYPE-REJECT LBUF-FALSE exit
   then
   LBUF-W ! drop
   count LBUF-N !
   count LBUF-W @ LBUF-EXTENT? 0= if
      drop name count CHECKER-STORAGE-COUNT-REJECT LBUF-FALSE exit
   then
   LBUF-BYTES ! LBUF-TRUE ;

CAST: AS-SEAL-NAME ( n -- [ str -- ] )

public

: SEAL-NAME ( str -- )
   data-base PROVIDED-XT:SEAL-CELL + @ AS-SEAL-NAME execute ;

\ A name a sealed package would take is rejected before a taken one, which is
\ rejected at t, the name as the source holds it.
: CLAIM-NAME ( str str -- ) {: name:str t:str :}
   name SEAL-NAME
   name CHECKER-DEFINED-HERE? if name t REJECT-DUPLICATE then
   name get-current search-wl 0 <> if name t REJECT-DUPLICATE then ;

\ A name the checker rejects answers false, and the definer defines nothing:
\ the checker has reported it.
: LBUF-NAME-OK? ( str str -- bool ) {: name:str t:str :}
   name CHECKER-LBUF-NAME-OK? 0= if LBUF-FALSE exit then
   name t CLAIM-NAME
   LBUF-TRUE ;

private

: LBUF-ZERO ( ^n n -- ) {: base:^ bytes:n :}
   0 LBUF-I !
   begin LBUF-I @ bytes < while
      0 base LBUF-I @ + !
      LBUF-I @ CELL + LBUF-I !
   repeat ;

\ bytes is a whole number of cells; src and dst are disjoint, so a forward copy
\ is safe.
: LBUF-COPY ( ^a ^a n -- ) {: src:^ dst:^ bytes:n :}
   0 LBUF-I !
   begin LBUF-I @ bytes < while
      src LBUF-I @ + @  dst LBUF-I @ + !
      LBUF-I @ CELL + LBUF-I !
   repeat ;

public

\ The answer stays in LBUF-GEN until the next LBUF-CLEAR.
: LBUF-SUFFIXED ( str str -- str )
   {: name:str sfx:str :}
   LBUF-CLEAR  name LBUF-APP  sfx LBUF-APP
   LBUF-GEN LBUF-GEN-U @ >str ;

\ Every derived name (NAME-BIND, NAME-GROW, NAME-RESERVE, NAME-RELEASE) is
\ guarded like the declared one, so a collision fails loudly instead of
\ rebinding a word.
: LBUF-SUFFIX-OK? ( str str -- bool ) {: name:str sfx:str :}
   name sfx LBUF-SUFFIXED name LBUF-NAME-OK? ;

private

PTR-VARIABLE STGT-A
variable STGT-U
PTR-VARIABLE STGT-START

\ Every region here holds cells, so DP is rounded to a cell before a definer
\ measures it.
: LBUF-ALIGN ( -- )
   align ;

\ A cell's class: the kinds its variants put in it, one bit each. A sum's store
\ zeroes the cells past its payload, so a mark holds for every variant stored.
0 constant CELL-ABSENT
1 constant CELL-SCALAR
2 constant CELL-PTR
4 constant CELL-CODE

PTR-VARIABLE CLS-P
SCRATCH-VARIABLE CLS-CAP

: CLS-BASE ( -- ^u8 ) CLS-P @ ;
: CLS ( n -- ^u8 ) {: at:n :} CLS-BASE at + ;

: CLS-PUT ( n n -- ) {: at:n class:n :}
   at CLS c@ class or at CLS c! ;

: CLS-SPAN ( n n n -- ) {: at:n width:n class:n :}
   width 0 ?do at i + class CLS-PUT loop ;

\ Classes the cells of term from cell at, every sum variant over the same
\ cells; a tag and every cell of a byte buffer are numbers.
: CLASSIFY ( n n n -- ) {: at:n width:n term:n :}
   term TFAM-STORAGE-QUOT? if at CELL-CODE CLS-PUT exit then
   term T-RES TAG T-PTR = if at CELL-PTR CLS-PUT exit then
   term TFAM-STORAGE-PRODUCT? if
      {: fam:n :}
      at
      fam TFAM-FLD-COUNT@ 0 ?do
         fam TFAM-FLD-START@ i + {: field:n :}
         term field TFAM-STORAGE-FIELD {: child:n cw:n :}
         dup cw child recurse
         cw +
      loop drop
      exit
   then drop
   term TFAM-STORAGE-SUM? if
      {: fam:n :}
      fam TFAM-NICHE? if at CELL-PTR CLS-PUT exit then
      at width + 1 - CELL-SCALAR CLS-PUT
      fam TFAM-VAR-COUNT@ 0 ?do
         fam TFAM-VAR-START@ i + {: vid:n :}
         at
         vid SUMV-PAY-N 0 ?do
            term vid i TFAM-STORAGE-PAY {: child:n cw:n :}
            dup cw child recurse
            cw +
         loop drop
      loop
      exit
   then drop
   at width CELL-SCALAR CLS-SPAN ;

\ Classifies the cells of an element of term and answers their count.
: CLASSIFY-ELEMENT ( n -- n ) {: term:n :}
   term T-WIDTH {: width:n :}
   CLS-P CLS-CAP width 1 max ARENA-BYTES-ENSURE
   width 0 ?do CELL-ABSENT i CLS c! loop
   0 width term CLASSIFY
   width ;

\ The first of the width cells of the element that holds more than one kind,
\ else -1.
: MIXED-CELL ( n -- n ) {: width:n :}
   width 0 ?do
      i CLS c@ {: class:n :}
      class class 1 - and 0= 0= if i unloop exit then
   loop -1 ;

: KINDS$ ( n -- str ) {: class:n :}
   class CELL-SCALAR CELL-PTR or CELL-CODE or = if
      s" a number, an address and a quotation" exit
   then
   class CELL-SCALAR CELL-PTR or = if s" an address and a number" exit then
   class CELL-SCALAR CELL-CODE or = if s" a quotation and a number" exit then
   s" an address and a quotation" ;

\ The storage name, of type text, whose element's cell at holds the kinds of
\ its class, each in another variant.
: MIXED-REJECT ( str str n -- ) {: name:str type:str at:n :}
   at CLS c@ {: class:n :}
   s" E-BAD-STORAGE" REJECTION:OPEN
   s" the type " REJECTION:EMIT-STR  type CODE-SPAN
   s"  of " REJECTION:EMIT-STR  name CODE-SPAN
   s"  holds " REJECTION:EMIT-STR  class KINDS$ REJECTION:EMIT-STR
   s"  in cell " REJECTION:EMIT-STR  at REJECTION:EMIT-N
   s"  of an element, each in another variant" REJECTION:EMIT-STR
   type REJECTION:AT
   s" a saved cell must hold one kind in every variant" REJECTION:EMIT-STR
   REJECTION:HELP  s" keep the " REJECTION:EMIT-STR
   class CELL-PTR and 0= 0= if s" address" else s" quotation" then REJECTION:EMIT-STR
   s"  in a variant cell no other variant uses as " REJECTION:EMIT-STR
   class CELL-SCALAR and 0= 0= if s" a number" else s" a quotation" then REJECTION:EMIT-STR
   STGR-END ;

\ Rejects the storage name, of type text and term, when two variants put
\ different kinds in one cell of its element.
: MIXED-HELD? ( str str n -- bool ) {: name:str type:str term:n :}
   term CLASSIFY-ELEMENT MIXED-CELL {: at:n :}
   at 0< if LBUF-FALSE exit then
   name type at MIXED-REJECT LBUF-TRUE ;

: MARK-CELLS ( ^n str -- ) {: base:^ kinds:str :}
   kinds STR:LEN 0 ?do
      kinds STR:DATA i + c@ {: class:n :}
      base i cells + {: cell:^ :}
      class CELL-PTR = if cell ptr-cell-mark then
      class CELL-CODE = if cell @ cell xt! then
   loop ;

: STORAGE-MARK-FIXED ( ^n n -- ) {: base:^ term:n :}
   term CLASSIFY-ELEMENT {: width:n :}
   LBUF-N @ 0 ?do
      base i width * cells + CLS-BASE width >str MARK-CELLS
   loop ;

\ A type node is read where its text would be parsed (STORAGE-PARSE?), into
\ LBI-T, live as long; a rejection shows the type as a diagnostic renders it.
: NODE-READ ( n -- ) {: node:n :}
   STORAGE-RESET
   node EFFECT-TYPE T-RES LBI-T !  0 LBI-BAD ! ;

public

: NODE-TYPE-REJECT ( str -- ) {: name:str :}
   LBUF-CLEAR  LBI-T @ TYPE-TEXT LBUF-APP
   name LBUF-GEN LBUF-GEN-U @ >str CHECKER-STORAGE-TYPE-REJECT ;

: NODE-STORAGE-TERM ( n -- n bool ) NODE-READ LBI-TERM ;

\ Rejects the storage name, of type text, when the type puts an address and
\ another kind in one cell; a type the checker cannot read answers false.
: MIXED-SAVED? ( str str -- bool ) {: name:str type:str :}
   type CHECKER-STORAGE-TERM 0= if drop LBUF-FALSE exit then
   {: term:n :}
   term HAS-ADDRESS? 0= if LBUF-FALSE exit then
   name type term MIXED-HELD? ;

private

\ The definer's validation took the type or node, so its term is taken too.
: TYPE-TERM ( str -- n ) CHECKER-STORAGE-TERM drop ;

: NODE-TERM ( n -- n ) NODE-STORAGE-TERM drop ;

: STORAGE-HAS-ADDRESS? ( str -- bool ) TYPE-TERM HAS-ADDRESS? ;

: NODE-HAS-ADDRESS? ( n -- bool ) NODE-TERM HAS-ADDRESS? ;

\ Quotation cells belong to the image even while null; the term walk declares
\ each code and address cell of these DATA allocations for capture.
: STORAGE-BIND ( n ^n n str bool -- )
   {: handle:n base:^ count:n type:str hasaddr:bool :}
   hasaddr if
      base type TYPE-TERM STORAGE-MARK-FIXED
   then
   handle base count CHECKER-STORAGE-BIND ;

: NODE-BIND ( n ^n n n bool -- )
   {: handle:n base:^ count:n node:n hasaddr:bool :}
   hasaddr if
      base node NODE-TERM STORAGE-MARK-FIXED
   then
   handle base count CHECKER-STORAGE-BIND ;

\ ---- the stored type --------------------------------------------------------
\ Every definer reads its type here, so pointer chains, family applications and
\ spaced quotations reach the checker whole; it rejects one empty or unclosed.

public

\ Leaves a token parse-name read to the next statement.
: STORAGE-UNREAD ( ^u8 -- )
   NULL-PTR - data-base INP-CELL + ! ;

\ The next token, as parse-name reads one, when it stands on the cursor's line:
\ the bytes the read skips hold no line feed (CHECKER-TYPE-SPAN-BREAK?). For a
\ token on a later line it reads none and leaves the cursor where it found it.
: STORAGE-LINE-TOKEN ( -- str )
   data-base INP-CELL + @ {: at:n :}
   parse-name {: s:str :}
   s STR:DATA NULL-PTR - at - {: gap:n :}
   s STR:DATA gap - gap >str CHECKER-TYPE-SPAN-BREAK? if
      at data-base INP-CELL + !  s STR:DATA 0 >str exit
   then
   s ;

\ Whether the next token on the line is the SCRATCH-ONLY clause, consumed; any
\ other is left unread.
: SCRATCH-CLAUSE? ( -- bool )
   STORAGE-LINE-TOKEN {: ts:str :}
   ts STR:LEN 0= IF 0 0= 0= EXIT THEN
   ts s" scratch-only" STR:=CI IF 0 0= EXIT THEN
   ts STR:DATA STORAGE-UNREAD  0 0= 0= ;

private

: STORAGE-MORE? ( n bool -- n bool ) {: depth:n ended:bool :}
   ended if depth LBUF-FALSE exit then
   depth 0 > if parse-name else STORAGE-LINE-TOKEN then {: s:str :}
   s STR:LEN 0= if depth LBUF-FALSE exit then
   s STR:DATA STGT-A !  s STR:LEN STGT-U !  depth LBUF-TRUE ;

public

\ The whole spelling from a first token on the name's line, ended where the
\ checker ends one or, while no `<` or `[` is open, at the end of its line.
: STORAGE-PARSE-TYPE ( -- str )
   STORAGE-LINE-TOKEN dup STR:LEN STGT-U !  STR:DATA STGT-A !
   STGT-A @ STGT-START !
   0 begin STGT-A @ STGT-U @ >str CHECKER-TYPE-SPAN-STEP STORAGE-MORE? 0= until drop
   STGT-START @  STGT-A @ STGT-U @ + STGT-START @ - >str ;

private

\ ---- LBUF: the block -----------------------------------------------------------
\ A buffer block's header cells; an unbound block reads 0 in all but STRIDE.
public

0 constant OFF        \ fixed: the elements' byte offset from the block
1 constant CAP        \ allotted elements
2 constant CNT        \ live elements
3 constant STRIDE     \ an element's width in cells
4 constant HEAD-CELLS

private

0 constant REGION     \ deferred: the region's address, marked; a build lays it apart
4 constant KINDS      \ deferred: the count of class bytes after it, 0 or STRIDE

: REGION@ ( ^n -- ^n ) REGION ptr-field @ ;
: REGION! ( ^n ^n -- ) REGION ptr-field ! ;

\ Declares each cell of the count elements from base by the block's class
\ bytes, so a build relocates a saved address and keeps a saved quotation.
: MARK-FRESH ( ^n n ^n -- ) {: base:^ count:n cb:^ :}
   cb KINDS cells + @ {: wc:n :}
   wc 0= if exit then
   cb KINDS 1 + cells + BYTE-VIEW {: kinds:^ :}
   count 0 ?do base i wc * cells + kinds wc >str MARK-CELLS loop ;

\ ---- DEFER-LAYOUT-BUFFER: a column sized by the model at build time ----------
\ The accessor reads the region cell on every call, so an index survives
\ NAME-BIND and NAME-GROW; a pointer an accessor answered dangles across them.

\ Retiring a moved column clears its old cells' declarations and bytes after
\ the new region is published.
: LDEFER-RETIRE ( ^n n n ^n -- ) {: oldbase:^ oldcap:n wc:n cb:^ :}
   oldcap 0= if exit then
   oldbase cb REGION@ = if exit then
   oldbase BYTE-VIEW data-base BYTE-VIEW - oldcap wc * cells ADDRESS-CELLS:REMOVE-SPAN
   oldbase oldcap wc * cells LBUF-ZERO ;

public

: LDEFER-BIND ( n ^n n -- )   \ count cbase wc
   {: count:n cb:^ wc:n :}
   count wc * {: need:n :}
   cb REGION@ {: oldbase:^ :} cb CAP cells + @ {: oldcap:n :}
   count oldcap > if
      LBUF-ALIGN
      here {: base:^ :}
      need cells allot
      base count cb MARK-FRESH
      base cb REGION!
      count  cb CAP cells +  !
   then
   count  cb CNT cells +  !
   oldbase oldcap wc cb LDEFER-RETIRE ;

\ A new region's cells are declared before the live ones are copied in. Within
\ capacity [old-live, count) is zeroed: a prior shrink may have left it dirty.
: LDEFER-GROW ( n ^n n -- )   \ count cbase wc
   {: count:n cb:^ wc:n :}
   cb REGION@ {: oldbase:^ :} cb CAP cells + @ {: oldcap:n :}
   cb CNT cells + @ {: live:n :}
   count oldcap > if
      oldcap 2 *  count max {: newcap:n :}
      LBUF-ALIGN
      here {: nbase:^ :}
      newcap wc * cells allot                                \ allot hands the fresh region out zeroed
      nbase newcap cb MARK-FRESH
      oldbase nbase  live wc * cells  LBUF-COPY
      nbase cb REGION!
      newcap  cb CAP cells +  !
   else count live > if
      oldbase live wc * cells +
      count live -  wc * cells  LBUF-ZERO
   then then
   count  cb CNT cells +  !
   oldbase oldcap wc cb LDEFER-RETIRE ;

\ A companion's one cell, laid after it: it holds the block v and keeps the
\ address across a save.
: LDEFER-CELL ( ^n -- ) {: v:^ :}
   here cell-view {: at:^ :}
   here ptr-cell-mark 0 ,
   v at 0 ptr-field ! ;

private

\ Element i of the block.
: ELEMENT ( n ^n -- ^n )
   {: i:n v:^ :}
   v  v OFF cells + @ +  i v STRIDE cells + @ * cells + ;

\ Element i of the deferred block.
: DEFERRED-ELEMENT ( n ^n -- ^n )
   {: i:n v:^ :}
   v REGION@  i v STRIDE cells + @ * cells + ;

\ Binds the block in the cell to count elements, as LDEFER-BIND does.
: BIND ( n ^^n -- )
   {: count:n b:^ :}
   b @ {: v:^ :}
   count v  v STRIDE cells + @  LDEFER-BIND ;

\ Grows the block in the cell to count elements, as LDEFER-GROW does.
: GROW ( n ^^n -- )
   {: count:n b:^ :}
   b @ {: v:^ :}
   count v  v STRIDE cells + @  LDEFER-GROW ;

\ A buffer's children are made over these routines' entries.
CAST: AS-ENTRY ( [ n ^n -- ^n ] -- n )
CAST: AS-RESIZE-ENTRY ( [ n ^^n -- ] -- n )

CAST: AS-CHILD-RUN ( n -- [ str n n str -- n ] )

: CHILD ( str n n str -- n )
   data-base PROVIDED-XT:CHILD-CELL + @ AS-CHILD-RUN execute ;

\ Makes the buffer child NAME over TYPE and answers its effect handle.
: BUFFER-CHILD ( str str -- n )
   {: name:str type:str :}
   name CHILD-ABI:BUFFER ['] ELEMENT AS-ENTRY
   type CHILD ;

\ Lays a deferred block's KINDS cell and class bytes, classified once here for
\ every bind to mark by.
: LAY-KINDS ( str bool -- ) {: type:str hasaddr:bool :}
   hasaddr 0= if 0 , exit then
   type TYPE-TERM CLASSIFY-ELEMENT {: width:n :}
   width ,
   width 0 ?do i CLS c@ c, loop
   LBUF-ALIGN ;

\ Lays NAME's -BIND and -GROW companions over the block v.
: COMPANIONS ( str ^n -- ) {: name:str v:^ :}
   name s" -BIND" LBUF-SUFFIXED CHILD-ABI:BIND
   ['] BIND AS-RESIZE-ENTRY s" " CHILD drop  v LDEFER-CELL
   name s" -GROW" LBUF-SUFFIXED CHILD-ABI:GROW
   ['] GROW AS-RESIZE-ENTRY s" " CHILD drop  v LDEFER-CELL ;

\ ---- LAYOUT-BUFFER ------------------------------------------------------------
\ Makes the buffer child NAME and lays its block: the header, bound to LBUF-N
\ elements of LBUF-W cells, then the elements, which the checker binds.
: LBUF-CHILD ( str str bool bool -- )
   {: name:str type:str hasaddr:bool scratch:bool :}
   name type BUFFER-CHILD {: handle:n :}
   here {: v:^ :}
   HEAD-CELLS cells ,  LBUF-N @ ,  LBUF-N @ ,  LBUF-W @ ,
   scratch if LBUF-BYTES @ SCRATCH-ALLOT else LBUF-BYTES @ allot then
   v HEAD-CELLS cells + {: base:^ :}
   handle base LBUF-N @ type hasaddr STORAGE-BIND ;

global

: LAYOUT-BUFFER ( n -- ) {: count:n :}
   SRCLOC:TOKEN$ >str {: run:str :}
   STORAGE-LINE-TOKEN {: name:str :}
   name STR:LEN 0= if
      s" LAYOUT-BUFFER" run STG-NO-NAME CHECKER-STORAGE-REJECT exit
   then
   run name REJECTION:NOT-A-NAME
   STORAGE-PARSE-TYPE {: type:str :}
   name name LBUF-NAME-OK? 0= if exit then
   count name type LBUF-VALIDATE 0= if exit then
   name type MIXED-SAVED? if exit then
   type STORAGE-HAS-ADDRESS? {: hasaddr:bool :}
   name type hasaddr LBUF-FALSE LBUF-CHILD ;
INTRINSIC-DECLARATION INTRINSIC

: DEFER-LAYOUT-BUFFER ( -- )
   SRCLOC:TOKEN$ >str {: run:str :}
   STORAGE-LINE-TOKEN {: name:str :}
   name STR:LEN 0= if
      s" DEFER-LAYOUT-BUFFER" run STG-NO-NAME CHECKER-STORAGE-REJECT exit
   then
   run name REJECTION:NOT-A-NAME
   STORAGE-PARSE-TYPE {: type:str :}
   name name LBUF-NAME-OK? 0= if exit then
   name s" -BIND" LBUF-SUFFIX-OK? 0= if exit then
   name s" -GROW" LBUF-SUFFIX-OK? 0= if exit then
   type CHECKER-LAYOUT-INFO 0= if
      2drop
      name s" -BIND" s" -GROW" CHECKER-UNSEEN-PAIR
      name type CHECKER-LAYOUT-TYPE-REJECT exit
   then
   LBUF-W !  drop
   name type MIXED-SAVED? if exit then
   type STORAGE-HAS-ADDRESS? {: hasaddr:bool :}
   name CHILD-ABI:DEFERRED ['] DEFERRED-ELEMENT AS-ENTRY
   type CHILD {: handle:n :}
   here {: v:^ :}
   here ptr-cell-mark 0 ,  0 ,  0 ,  LBUF-W @ ,
   type hasaddr LAY-KINDS
   handle v CHECKER-STORAGE-DEFER
   name v COMPANIONS ;
INTRINSIC-DECLARATION INTRINSIC

private

\ ---- TYPED-VARIABLE / TYPED-BUFFER --------------------------------------------
\ LAYOUT-BUFFER's machinery gated by CHECKER-STORAGE-INFO, so closed typed
\ pointers (`^TARGET`, `^res<n,n>`) and spaced quotations are expressible.

\ Sizes COUNT elements of W cells each; false once a count outside the extent
\ is rejected.
: STORAGE-SIZE ( n n str -- bool ) {: count:n w:n name:str :}
   w LBUF-W !
   count LBUF-N !
   count LBUF-W @ LBUF-EXTENT? 0= if
      drop name count CHECKER-STORAGE-COUNT-REJECT LBUF-FALSE exit
   then
   LBUF-BYTES ! LBUF-TRUE ;

: STORAGE-VALIDATE ( n str str -- bool )
   {: count:n name:str type:str :}
   type CHECKER-STORAGE-INFO 0= if
      drop name type CHECKER-STORAGE-TYPE-REJECT LBUF-FALSE exit
   then
   count swap name STORAGE-SIZE ;

: NODE-VALIDATE ( n str n -- bool )
   {: count:n name:str node:n :}
   node NODE-READ LBI-CELLS 0= if
      drop name NODE-TYPE-REJECT LBUF-FALSE exit
   then
   count swap name STORAGE-SIZE ;

global

: TYPED-BUFFER ( n -- ) {: count:n :}
   SRCLOC:TOKEN$ >str {: run:str :}
   STORAGE-LINE-TOKEN {: name:str :}
   name STR:LEN 0= if
      s" TYPED-BUFFER" run STG-NO-NAME CHECKER-STORAGE-REJECT exit
   then
   run name REJECTION:NOT-A-NAME
   STORAGE-PARSE-TYPE {: type:str :}
   SCRATCH-CLAUSE? {: scratch:bool :}
   name name LBUF-NAME-OK? 0= if exit then
   count name type STORAGE-VALIDATE 0= if exit then
   scratch 0= if
      name type MIXED-SAVED? if exit then
   then
   scratch if LBUF-FALSE else type STORAGE-HAS-ADDRESS? then {: hasaddr:bool :}
   name type hasaddr scratch LBUF-CHILD ;
INTRINSIC-DECLARATION INTRINSIC

\ A missing name is rejected at the token that ran the definer, which the
\ message names.
: TYPED-VARIABLE ( -- )
   SRCLOC:TOKEN$ >str {: run:str :}
   STORAGE-LINE-TOKEN {: name:str :}
   name STR:LEN 0= if
      s" TYPED-VARIABLE" run STG-NO-NAME CHECKER-STORAGE-REJECT exit
   then
   run name REJECTION:NOT-A-NAME
   STORAGE-PARSE-TYPE {: type:str :}
   SCRATCH-CLAUSE? {: scratch:bool :}
   name name LBUF-NAME-OK? 0= if exit then
   1 name type STORAGE-VALIDATE 0= if exit then
   scratch 0= if
      name type MIXED-SAVED? if exit then
   then
   scratch if LBUF-FALSE else type STORAGE-HAS-ADDRESS? then {: hasaddr:bool :}
   name type TDECL-STORED-CHILD {: handle:n :}
   here {: v:^ :}
   scratch if LBUF-BYTES @ SCRATCH-ALLOT else LBUF-BYTES @ allot then
   handle v 1 type hasaddr STORAGE-BIND ;
INTRINSIC-DECLARATION INTRINSIC

public

\ TYPED-VARIABLE's run over the name and the type node, which no text spells:
\ a definer's variable (EFFECT:VARIABLE-OF), through PROVIDED-XT:VARIABLE-CELL.
: TYPED-VARIABLE-NODE ( str n -- ) {: name:str node:n :}
   name STR:LEN 0= if s" EFFECT:VARIABLE-OF" CHECKER-STORAGE-NAME-REJECT exit then
   name name LBUF-NAME-OK? 0= if exit then
   1 name node NODE-VALIDATE 0= if exit then
   node NODE-HAS-ADDRESS? {: hasaddr:bool :}
   hasaddr if
      node NODE-TERM {: term:n :}
      LBUF-CLEAR  LBI-T @ TYPE-TEXT LBUF-APP
      name LBUF-GEN LBUF-GEN-U @ >str term MIXED-HELD? if exit then
   then
   name node TDECL-NODE-CHILD {: handle:n :}
   here {: v:^ :}
   LBUF-BYTES @ allot
   handle v 1 node hasaddr NODE-BIND ;

;package

;using
