split-str
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

: LBUF-APP ( str -- ) {: a:^ u:n :}
   0 LBUF-I !
   begin LBUF-I @ u < while
      a LBUF-I @ + c@ LBUF-C,
      LBUF-I @ 1 + LBUF-I !
   repeat ;

: LBUF-EXTENT? ( n n -- n bool ) {: count:n width:n :}
   count 0 <= width 0 <= or if 0 LBUF-FALSE exit then
   count LBUF-N-MAX width / > if 0 LBUF-FALSE exit then
   count width * {: cellsn:n :}
   cellsn LBUF-N-MAX CELL / > if 0 LBUF-FALSE exit then
   cellsn cells LBUF-TRUE ;

EXPORT CHECK:CHECKER-DEFINED-HERE?
EXPORT CHECK:CHECKER-LAYOUT-INFO
EXPORT CHECK:CHECKER-LAYOUT-TYPE-REJECT
EXPORT CHECK:CHECKER-LBUF-NAME-OK?
EXPORT CHECK:CHECKER-STORAGE-BIND
EXPORT CHECK:CHECKER-STORAGE-COUNT-REJECT
EXPORT CHECK:CHECKER-STORAGE-DEFER
EXPORT CHECK:CHECKER-STORAGE-INFO
EXPORT CHECK:CHECKER-STORAGE-NAME-REFUSE
EXPORT CHECK:CHECKER-STORAGE-REJECT
EXPORT CHECK:CHECKER-STORAGE-TERM
EXPORT CHECK:CHECKER-STORAGE-TYPE-REJECT
EXPORT CHECK:CHECKER-TYPE-SPAN-BREAK?
EXPORT CHECK:CHECKER-TYPE-SPAN-STEP
EXPORT CHECK:INTRINSIC
EXPORT CHECK:INTRINSIC-DECLARATION
EXPORT CHECK:LBI-BAD
EXPORT CHECK:LBI-CELLS
EXPORT CHECK:LBI-T
EXPORT CHECK:LBI-TERM
EXPORT CHECK:STG-NO-NAME
EXPORT CHECK:STG-SCRATCH-SAVED
EXPORT CHECK:STG-SCRATCH-TYPE
EXPORT CHECK:STORAGE-RESET
EXPORT CHECK:T-RES
EXPORT CHECK:T-WIDTH
EXPORT CHECK:TAG
EXPORT TYPE-DECL:TDECL-STORED-CHILD
EXPORT TYPE-DECL:TDECL-NODE-CHILD
: LBUF-VALIDATE ( n str str -- bool )
   {: count:n name:^ nameu:n type:^ typeu:n :}
   type typeu CHECKER-LAYOUT-INFO 0= if
      2drop name nameu type typeu CHECKER-LAYOUT-TYPE-REJECT LBUF-FALSE exit
   then
   LBUF-W ! drop
   count LBUF-N !
   count LBUF-W @ LBUF-EXTENT? 0= if
      drop name nameu count CHECKER-STORAGE-COUNT-REJECT LBUF-FALSE exit
   then
   LBUF-BYTES ! LBUF-TRUE ;

CAST: AS-SEAL-NAME ( n -- [ ^u8 n -- ] )

public

: SEAL-NAME ( str -- )
   data-base PROVIDED-XT:SEAL-CELL + @ AS-SEAL-NAME execute ;

\ A name a sealed package would take is rejected before a taken one, which is
\ rejected at t tu, the name as the source holds it.
: CLAIM-NAME ( str str -- ) {: name:^ nameu:n t:^ tu:n :}
   name nameu SEAL-NAME
   name nameu CHECKER-DEFINED-HERE? if name nameu t tu REJECT-DUPLICATE then
   name nameu get-current search-wl 0 <> if name nameu t tu REJECT-DUPLICATE then ;

\ A name the checker rejects answers false, and the definer defines nothing:
\ the checker has reported it.
: LBUF-NAME-OK? ( str str -- bool ) {: name:^ nameu:n t:^ tu:n :}
   name nameu CHECKER-LBUF-NAME-OK? 0= if LBUF-FALSE exit then
   name nameu t tu CLAIM-NAME
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
   {: name:^ nameu:n sfx:^ sfxu:n :}
   LBUF-CLEAR  name nameu LBUF-APP  sfx sfxu LBUF-APP
   LBUF-GEN LBUF-GEN-U @ ;

\ Every derived name (NAME-BIND, NAME-GROW, NAME-RESERVE, NAME-RELEASE) is
\ guarded like the declared one, so a collision fails loudly instead of
\ rebinding a word.
: LBUF-SUFFIX-OK? ( str str -- bool ) {: name:^ nameu:n sfx:^ sfxu:n :}
   name nameu sfx sfxu LBUF-SUFFIXED name nameu LBUF-NAME-OK? ;

private

PTR-VARIABLE STGT-A
variable STGT-U
PTR-VARIABLE STGT-START

\ Every region here holds cells, so DP is rounded to a cell before a definer
\ measures it.
: LBUF-ALIGN ( -- )
   align ;

\ A sum is marked by its active tag: a shared payload slot is a code
\ cell while it holds a quotation and an address cell while it holds a pointer.
\ A niche value has no tag: its one cell is an address cell, zero or not.
: STORAGE-MARK-TERM ( ^n n -- ) {: base:^ term:n :}
   term TFAM:TFAM-STORAGE-QUOT? if base @ base xt! exit then
   term T-RES TAG T-PTR = if base ptr-cell-mark exit then
   term TFAM:TFAM-STORAGE-PRODUCT? if
      {: fam:n :}
      0
      fam TFAM:TFAM-FLD-COUNT@ 0 ?do
         fam TFAM:TFAM-FLD-START@ i + {: field:n :}
         term field TFAM:TFAM-STORAGE-FIELD {: child:n width:n :}
         base over cells + child recurse
         width +
      loop drop
      exit
   then drop
   term TFAM:TFAM-STORAGE-SUM? if
      {: fam:n :}
      fam TFAM:TFAM-NICHE? if base ptr-cell-mark exit then
      \ The block is fresh, so its tag is 0, which every sum's first variant has.
      base term T-WIDTH 1- cells + @
      fam swap TFAM:TFAM-STORAGE-VARIANT drop {: vid:n :}
      0
      vid TFAM:SUMV-PAY-N 0 ?do
         term vid i TFAM:TFAM-STORAGE-PAY {: child:n width:n :}
         base over cells + child recurse
         width +
      loop drop
   else drop then ;

: STORAGE-MARK-FIXED ( ^n n -- ) {: base:^ term:n :}
   LBUF-N @ 0 ?do
      base i LBUF-W @ * cells + term STORAGE-MARK-TERM
   loop ;

\ A type node is read where its text would be parsed (STORAGE-PARSE?), into
\ LBI-T, live as long; a rejection shows the type as a diagnostic renders it.
: NODE-READ ( n -- ) {: node:n :}
   STORAGE-RESET
   node TFAM:EFFECT-TYPE T-RES LBI-T !  0 LBI-BAD ! ;

public

: NODE-TYPE-REFUSE ( str -- ) {: name:^ nameu:n :}
   LBUF-CLEAR  LBI-T @ TYPE-TEXT LBUF-APP
   name nameu LBUF-GEN LBUF-GEN-U @ CHECKER-STORAGE-TYPE-REJECT ;

: NODE-STORAGE-TERM ( n -- n bool ) NODE-READ LBI-TERM ;

private

\ The definer's validation took the type or node, so its term is taken too.
: TYPE-TERM ( str -- n ) CHECKER-STORAGE-TERM drop ;

: NODE-TERM ( n -- n ) NODE-STORAGE-TERM drop ;

: STORAGE-HAS-ADDRESS? ( str -- bool ) TYPE-TERM TFAM:HAS-ADDRESS? ;

: NODE-HAS-ADDRESS? ( n -- bool ) NODE-TERM TFAM:HAS-ADDRESS? ;

: NODE-SCRATCH-REJECT ( str -- ) {: name:^ nameu:n :}
   LBUF-CLEAR  LBI-T @ TYPE-TEXT LBUF-APP
   name nameu LBUF-GEN LBUF-GEN-U @ STG-SCRATCH-SAVED CHECKER-STORAGE-REJECT ;

\ Quotation cells belong to the image even while null; the term walk declares
\ each code and address cell of these DATA allocations for capture.
: STORAGE-BIND ( n ^n n str bool -- )
   {: handle:n base:^ count:n type:^ typeu:n hasaddr:bool :}
   hasaddr if
      base type typeu TYPE-TERM STORAGE-MARK-FIXED
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
   parse-name {: a:^ u:n :}
   a NULL-PTR - at - {: gap:n :}
   a gap - gap CHECKER-TYPE-SPAN-BREAK? if
      at data-base INP-CELL + !  a 0 exit
   then
   a u ;

\ Whether the next token on the line is the SCRATCH-ONLY clause, consumed; any
\ other is left unread.
: SCRATCH-CLAUSE? ( -- bool )
   STORAGE-LINE-TOKEN {: ta:^ tu:n :}
   tu 0= IF 0 0= 0= EXIT THEN
   ta tu s" scratch-only" CORE-STR=CI IF 0 0= EXIT THEN
   ta STORAGE-UNREAD  0 0= 0= ;

\ Rejects the storage name, of type text, with the STG- reason why when the
\ type holds a SCRATCH-ONLY family; a type the checker cannot read answers false.
: SCRATCH-HELD? ( str str n -- bool )
   {: name:^ nameu:n type:^ typeu:n why:n :}
   type typeu CHECKER-STORAGE-TERM 0= if drop LBUF-FALSE exit then
   TFAM:HOLDS-SCRATCH? 0= if LBUF-FALSE exit then
   name nameu type typeu why CHECKER-STORAGE-REJECT LBUF-TRUE ;

private

: STORAGE-MORE? ( n bool -- n bool ) {: depth:n ended:bool :}
   ended if depth LBUF-FALSE exit then
   depth 0 > if parse-name else STORAGE-LINE-TOKEN then {: a:^ u:n :}
   u 0= if depth LBUF-FALSE exit then
   a STGT-A !  u STGT-U !  depth LBUF-TRUE ;

public

\ The whole spelling from a first token on the name's line, ended where the
\ checker ends one or, while no `<` or `[` is open, at the end of its line.
: STORAGE-PARSE-TYPE ( -- str )
   STORAGE-LINE-TOKEN STGT-U !  STGT-A !
   STGT-A @ STGT-START !
   0 begin STGT-A @ STGT-U @ CHECKER-TYPE-SPAN-STEP STORAGE-MORE? 0= until drop
   STGT-START @  STGT-A @ STGT-U @ + STGT-START @ - ;

private

\ ---- DEFER-LAYOUT-BUFFER: a column sized by the model at build time ----------
\ An unbound buffer block whose NAME-BIND ( count -- ) allots count*width cells.
\ NAME-BIND reuses the region when the count fits, else allots a larger one and
\ retires the old; its cells are fresh and zeroed. NAME-GROW carries the live
\ cells over. The accessor reads the offset cell on every call, so indices
\ survive a grow; a raw pointer derived from an accessor dangles across
\ NAME-GROW.

\ The region offset is measured from the block, not from `data-base`, so it
\ relocates with the block. Retiring a moved column clears its old cells'
\ declarations and bytes after the new region is published.
: LDEFER-RETIRE ( n n n ^n -- ) {: oldoff:n oldcap:n wc:n cb:^ :}
   oldcap 0= oldoff cb @ = or if exit then
   cb oldoff + {: oldbase:^ :}
   oldbase BYTE-VIEW data-base BYTE-VIEW - oldcap wc * cells ADDRESS-CELLS:REMOVE-SPAN
   oldbase oldcap wc * cells LBUF-ZERO ;

public

: LDEFER-BIND ( n ^n n -- )   \ count cbase wc
   {: count:n cb:^ wc:n :}
   count wc * {: need:n :}
   cb @ {: oldoff:n :} cb CELL + @ {: oldcap:n :}
   count  cb CELL + @  > if
      LBUF-ALIGN
      here {: base:^ :}
      need cells allot
      base cb -  cb  !                                  \ off-cell = new region offset from cb
      count  cb CELL +  !                               \ cap-cell = new allotted capacity
   then
   count  cb 2 CELL * +  !                             \ cnt-cell = live bound
   oldoff oldcap wc cb LDEFER-RETIRE ;

\ NAME-GROW extends a column, preserving written cells. Past capacity the
\ region becomes max(2*capacity, count). A within-capacity grow
\ zeroes [old-live, count): a prior shrink may have left it dirty.
: LDEFER-GROW ( n ^n n -- )   \ count cbase wc
   {: count:n cb:^ wc:n :}
   cb @ {: oldoff:n :} cb CELL + @ {: oldcap:n :}
   count  cb CELL + @  > if
      cb 2 CELL * + @ {: live:n :}
      cb  cb @  + {: obase:^ :}
      cb CELL + @ 2 *  count max {: newcap:n :}
      LBUF-ALIGN
      here {: nbase:^ :}
      newcap wc * cells allot                                \ allot hands the fresh region out zeroed
      obase nbase  live wc * cells  LBUF-COPY
      nbase cb -  cb  !
      newcap  cb CELL +  !
   else count  cb 2 CELL * + @  > if
      cb  cb @  +
      cb 2 CELL * + @ wc * cells +
      count  cb 2 CELL * + @  -  wc * cells  LBUF-ZERO
   then then
   count  cb 2 CELL * +  !
   oldoff oldcap wc cb LDEFER-RETIRE ;

\ A companion's one cell, laid after it: it holds the block v and keeps the
\ address across a save.
: LDEFER-CELL ( ^n -- ) {: v:^ :}
   here cell-view {: at:^ :}
   here ptr-cell-mark 0 ,
   v at 0 ptr-field ! ;

private

\ ---- LBUF: the routines a buffer child's body calls --------------------------
\ A buffer block starts with HEAD-CELLS header cells, indexed by OFF (the
\ elements' byte offset from the block), CAP and CNT (allotted and live element
\ counts) and STRIDE (an element's width in cells). An unbound block reads 0
\ in all but STRIDE. Each routine takes the block, or the cell holding it, last.
public

0 constant OFF
1 constant CAP
2 constant CNT
3 constant STRIDE
4 constant HEAD-CELLS

private

\ Element i of the block.
: ELEMENT ( n ^n -- ^n )
   {: i:n v:^ :}
   v  v OFF cells + @ +  i v STRIDE cells + @ * cells + ;

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

CAST: AS-CHILD-RUN ( n -- [ ^u8 n n n ^u8 n -- n ] )

: CHILD ( ^u8 n n n ^u8 n -- n )
   data-base PROVIDED-XT:CHILD-CELL + @ AS-CHILD-RUN execute ;

\ Makes the buffer child NAME over TYPE and answers its effect handle.
: BUFFER-CHILD ( ^u8 n ^u8 n -- n )
   {: name:^ nameu:n type:^ typeu:n :}
   name nameu CHILD-ABI:BUFFER ['] ELEMENT AS-ENTRY
   type typeu CHILD ;

\ Lays NAME's -BIND and -GROW companions over the block v.
: COMPANIONS ( str ^n -- ) {: name:^ nameu:n v:^ :}
   name nameu s" -BIND" LBUF-SUFFIXED CHILD-ABI:BIND
   ['] BIND AS-RESIZE-ENTRY s" " CHILD drop  v LDEFER-CELL
   name nameu s" -GROW" LBUF-SUFFIXED CHILD-ABI:GROW
   ['] GROW AS-RESIZE-ENTRY s" " CHILD drop  v LDEFER-CELL ;

\ ---- LAYOUT-BUFFER ------------------------------------------------------------
\ Makes the buffer child NAME and lays its block: the header, bound to LBUF-N
\ elements of LBUF-W cells, then the elements, which the checker binds.
: LBUF-CHILD ( str str bool bool -- )
   {: name:^ nameu:n type:^ typeu:n hasaddr:bool scratch:bool :}
   name nameu type typeu BUFFER-CHILD {: handle:n :}
   here {: v:^ :}
   HEAD-CELLS cells ,  LBUF-N @ ,  LBUF-N @ ,  LBUF-W @ ,
   scratch if LBUF-BYTES @ SCRATCH-ALLOT else LBUF-BYTES @ allot then
   v HEAD-CELLS cells + {: base:^ :}
   handle base LBUF-N @ type typeu hasaddr STORAGE-BIND ;

global

: LAYOUT-BUFFER ( n -- ) {: count:n :}
   SRCLOC:TOKEN$ {: run:^ runu:n :}
   STORAGE-LINE-TOKEN {: name:^ nameu:n :}
   nameu 0= if
      s" LAYOUT-BUFFER" run runu STG-NO-NAME CHECKER-STORAGE-REJECT exit
   then
   run runu name nameu REJECTION:NOT-A-NAME
   STORAGE-PARSE-TYPE {: type:^ typeu:n :}
   name nameu name nameu LBUF-NAME-OK? 0= if exit then
   count name nameu type typeu LBUF-VALIDATE 0= if exit then
   name nameu type typeu STG-SCRATCH-SAVED SCRATCH-HELD? if exit then
   type typeu STORAGE-HAS-ADDRESS? {: hasaddr:bool :}
   name nameu type typeu hasaddr LBUF-FALSE LBUF-CHILD ;
INTRINSIC-DECLARATION INTRINSIC

: DEFER-LAYOUT-BUFFER ( -- )
   SRCLOC:TOKEN$ {: run:^ runu:n :}
   STORAGE-LINE-TOKEN {: name:^ nameu:n :}
   nameu 0= if
      s" DEFER-LAYOUT-BUFFER" run runu STG-NO-NAME CHECKER-STORAGE-REJECT exit
   then
   run runu name nameu REJECTION:NOT-A-NAME
   STORAGE-PARSE-TYPE {: type:^ typeu:n :}
   name nameu name nameu LBUF-NAME-OK? 0= if exit then
   name nameu s" -BIND" LBUF-SUFFIX-OK? 0= if exit then
   name nameu s" -GROW" LBUF-SUFFIX-OK? 0= if exit then
   type typeu CHECKER-LAYOUT-INFO 0= if
      2drop name nameu type typeu CHECKER-LAYOUT-TYPE-REJECT exit
   then
   LBUF-W !  drop
   name nameu type typeu STG-SCRATCH-SAVED SCRATCH-HELD? if exit then
   name nameu type typeu BUFFER-CHILD {: handle:n :}
   here {: v:^ :}
   0 ,  0 ,  0 ,  LBUF-W @ ,
   handle v CHECKER-STORAGE-DEFER
   name nameu v COMPANIONS ;
INTRINSIC-DECLARATION INTRINSIC

private

\ ---- TYPED-VARIABLE / TYPED-BUFFER --------------------------------------------
\ LAYOUT-BUFFER's machinery gated by CHECKER-STORAGE-INFO, so closed typed
\ pointers (`^TARGET`, `^res<n,n>`) and spaced quotations are expressible.

\ Sizes COUNT elements of W cells each; false once a count outside the extent
\ is rejected.
: STORAGE-SIZE ( n n str -- bool ) {: count:n w:n name:^ nameu:n :}
   w LBUF-W !
   count LBUF-N !
   count LBUF-W @ LBUF-EXTENT? 0= if
      drop name nameu count CHECKER-STORAGE-COUNT-REJECT LBUF-FALSE exit
   then
   LBUF-BYTES ! LBUF-TRUE ;

: STORAGE-VALIDATE ( n str str -- bool )
   {: count:n name:^ nameu:n type:^ typeu:n :}
   type typeu CHECKER-STORAGE-INFO 0= if
      drop name nameu type typeu CHECKER-STORAGE-TYPE-REJECT LBUF-FALSE exit
   then
   count swap name nameu STORAGE-SIZE ;

: NODE-VALIDATE ( n str n -- bool )
   {: count:n name:^ nameu:n node:n :}
   node NODE-READ LBI-CELLS 0= if
      drop name nameu NODE-TYPE-REFUSE LBUF-FALSE exit
   then
   count swap name nameu STORAGE-SIZE ;

global

: TYPED-BUFFER ( n -- ) {: count:n :}
   SRCLOC:TOKEN$ {: run:^ runu:n :}
   STORAGE-LINE-TOKEN {: name:^ nameu:n :}
   nameu 0= if
      s" TYPED-BUFFER" run runu STG-NO-NAME CHECKER-STORAGE-REJECT exit
   then
   run runu name nameu REJECTION:NOT-A-NAME
   STORAGE-PARSE-TYPE {: type:^ typeu:n :}
   SCRATCH-CLAUSE? {: scratch:bool :}
   name nameu name nameu LBUF-NAME-OK? 0= if exit then
   count name nameu type typeu STORAGE-VALIDATE 0= if exit then
   scratch 0= if
      name nameu type typeu STG-SCRATCH-TYPE SCRATCH-HELD? if exit then
   then
   scratch if LBUF-FALSE else type typeu STORAGE-HAS-ADDRESS? then {: hasaddr:bool :}
   name nameu type typeu hasaddr scratch LBUF-CHILD ;
INTRINSIC-DECLARATION INTRINSIC

\ A missing name is rejected at the token that ran the definer, which the
\ message names.
: TYPED-VARIABLE ( -- )
   SRCLOC:TOKEN$ {: run:^ runu:n :}
   STORAGE-LINE-TOKEN {: name:^ nameu:n :}
   nameu 0= if
      s" TYPED-VARIABLE" run runu STG-NO-NAME CHECKER-STORAGE-REJECT exit
   then
   run runu name nameu REJECTION:NOT-A-NAME
   STORAGE-PARSE-TYPE {: type:^ typeu:n :}
   SCRATCH-CLAUSE? {: scratch:bool :}
   name nameu name nameu LBUF-NAME-OK? 0= if exit then
   1 name nameu type typeu STORAGE-VALIDATE 0= if exit then
   scratch 0= if
      name nameu type typeu STG-SCRATCH-TYPE SCRATCH-HELD? if exit then
   then
   scratch if LBUF-FALSE else type typeu STORAGE-HAS-ADDRESS? then {: hasaddr:bool :}
   name nameu type typeu TDECL-STORED-CHILD {: handle:n :}
   here {: v:^ :}
   scratch if LBUF-BYTES @ SCRATCH-ALLOT else LBUF-BYTES @ allot then
   handle v 1 type typeu hasaddr STORAGE-BIND ;
INTRINSIC-DECLARATION INTRINSIC

public

\ TYPED-VARIABLE's run over the name and the type node, which no text spells:
\ a definer's variable (EFFECT:VARIABLE-OF), through PROVIDED-XT:VARIABLE-CELL.
: TYPED-VARIABLE-NODE ( str n -- ) {: name:^ nameu:n node:n :}
   nameu 0= if s" EFFECT:VARIABLE-OF" CHECKER-STORAGE-NAME-REFUSE exit then
   name nameu name nameu LBUF-NAME-OK? 0= if exit then
   1 name nameu node NODE-VALIDATE 0= if exit then
   node NODE-TERM TFAM:HOLDS-SCRATCH? if name nameu NODE-SCRATCH-REJECT exit then
   node NODE-HAS-ADDRESS? {: hasaddr:bool :}
   name nameu node TDECL-NODE-CHILD {: handle:n :}
   here {: v:^ :}
   LBUF-BYTES @ allot
   handle v 1 node hasaddr NODE-BIND ;

;package

;using
