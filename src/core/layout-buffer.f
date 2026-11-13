\ layout-buffer.f: the generative storage definers LAYOUT-BUFFER (the only
\ introduction form for `ptr layout`), DEFER-LAYOUT-BUFFER, TYPED-VARIABLE and
\ TYPED-BUFFER, each making its words as typed children and laying each child's
\ block at the address its body pushes. LBUF-EVAL evaluates a generated accessor
\ the checker authorizes once, by name, in the armed window. A ptr variable
\ never gains layout identity by unification.

using TYPE-DECL

$1000 constant LBUF-GEN-CAP
$7FFFFFFFFFFFFFFF constant LBUF-N-MAX
E-CHECKER-LAYOUT-BUFFER constant E-LAYOUT-BUFFER   \ the checker's layout refusal, one code
7122 constant E-LAYOUT-BOUNDS
78 constant E-DUP-DEFINITION
0 constant LBUF-FALSE
-1 constant LBUF-TRUE

create LBUF-GEN LBUF-GEN-CAP allot
variable LBUF-GEN-U
variable LBUF-I
variable LBUF-N
variable LBUF-W
variable LBUF-BYTES

: LBUF-CLEAR ( -- )
   0 LBUF-GEN-U ! ;

: LBUF-C, ( n -- ) {: c:n :}
   LBUF-GEN-U @ LBUF-GEN-CAP >= if E-LAYOUT-BUFFER throw then
   c LBUF-GEN LBUF-GEN-U @ + c!
   LBUF-GEN-U @ 1 + LBUF-GEN-U ! ;

: LBUF-APP ( ptr u8 n -- ) {: a:ptr u:n :}
   0 LBUF-I !
   begin LBUF-I @ u < while
      a LBUF-I @ + c@ LBUF-C,
      LBUF-I @ 1 + LBUF-I !
   repeat ;

: LBUF-DEC, ( n -- ) {: n:n :}
   n 10 >= if n 10 / recurse then
   n 10 mod 48 + LBUF-C, ;

: LBUF-EXTENT? ( n n -- n bool ) {: count:n width:n :}
   count 0 <= width 0 <= or if 0 LBUF-FALSE exit then
   count LBUF-N-MAX width / > if 0 LBUF-FALSE exit then
   count width * {: cellsn:n :}
   cellsn LBUF-N-MAX CELL / > if 0 LBUF-FALSE exit then
   cellsn cells LBUF-TRUE ;

: LBUF-VALIDATE ( n ptr u8 n ptr u8 n -- bool )
   {: count:n name:ptr nameu:n type:ptr typeu:n :}
   type typeu CHECKER-LAYOUT-INFO 0= if
      2drop name nameu type typeu CHECKER-STORAGE-TYPE-REFUSE LBUF-FALSE exit
   then
   LBUF-W ! drop
   count LBUF-N !
   count LBUF-W @ LBUF-EXTENT? 0= if drop E-LAYOUT-BUFFER throw then
   LBUF-BYTES ! LBUF-TRUE ;

\ A name the checker refuses answers false, and the definer defines nothing:
\ the checker has reported it (src/core/checker.f CHECKER-STORAGE-REFUSE).
: LBUF-NAME-OK? ( ptr u8 n -- bool ) {: name:ptr nameu:n :}
   name nameu CHECKER-LBUF-NAME-OK? 0= if LBUF-FALSE exit then
   name nameu CHECKER-DEFINED-HERE? if name nameu CHECK:REFUSE-DUPLICATE then
   name nameu get-current search-wl 0 <> if name nameu CHECK:REFUSE-DUPLICATE then
   LBUF-TRUE ;

: LBUF-ZERO ( ptr n n -- ) {: base:ptr bytes:n :}
   0 LBUF-I !
   begin LBUF-I @ bytes < while
      0 base LBUF-I @ + !
      LBUF-I @ CELL + LBUF-I !
   repeat ;

\ bytes is a whole number of cells; src and dst are disjoint, so a forward copy
\ is safe.
: LBUF-COPY ( ptr a ptr a n -- ) {: src:ptr dst:ptr bytes:n :}
   0 LBUF-I !
   begin LBUF-I @ bytes < while
      src LBUF-I @ + @  dst LBUF-I @ + !
      LBUF-I @ CELL + LBUF-I !
   repeat ;

: LBUF-NAME, ( ptr u8 n -- ptr u8 n ) {: name:ptr nameu:n :}
   LBUF-GEN-U @ {: start:n :}
   name nameu LBUF-APP
   LBUF-GEN start + nameu ;

\ ---- how a generated accessor reaches its storage ----------------------------
\ Every generated accessor addresses its storage through a `create`d word, whose
\ DATA carrier habu2.f C-DATA-ADDR emits.
: LBUF-BASE$ ( -- ptr u8 n ) s" #base" ;

\ The answer stays in LBUF-GEN until the next LBUF-CLEAR.
: LBUF-SUFFIXED ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: name:ptr nameu:n sfx:ptr sfxu:n :}
   LBUF-CLEAR  name nameu LBUF-APP  sfx sfxu LBUF-APP
   LBUF-GEN LBUF-GEN-U @ ;

\ Every derived name (the storage word, NAME-BIND, NAME-GROW, NAME-RESERVE,
\ NAME-RELEASE) is guarded like the declared one, so a collision fails loudly
\ instead of rebinding a word. LBUF-NAMES-OK? guards the declared name and its
\ storage word.
: LBUF-SUFFIX-OK? ( ptr u8 n ptr u8 n -- bool ) {: name:ptr nameu:n sfx:ptr sfxu:n :}
   name nameu sfx sfxu LBUF-SUFFIXED LBUF-NAME-OK? ;

: LBUF-NAMES-OK? ( ptr u8 n -- bool ) {: name:ptr nameu:n :}
   name nameu LBUF-NAME-OK? 0= if LBUF-FALSE exit then
   name nameu LBUF-BASE$ LBUF-SUFFIX-OK? ;

: LBUF-BASE, ( ptr u8 n -- ) {: name:ptr nameu:n :}
   name nameu LBUF-APP  LBUF-BASE$ LBUF-APP ;

PTR-VARIABLE LBUF-EVAL-A
variable LBUF-EVAL-U
PTR-VARIABLE STGT-A
variable STGT-U
PTR-VARIABLE STGT-START

: LBUF-EVAL-RUN ( -- )
   LBUF-EVAL-A 0 ptr-field @ LBUF-EVAL-U @ TDECL-EVAL-XT ;

: LBUF-EVAL ( ptr u8 n ptr u8 n -- n n )
   {: src:ptr srcu:n name:ptr nameu:n :}
   TDECL-EVAL-ARMED @ 0= if E-LAYOUT-BUFFER throw then
   name nameu LBUF-PEND!
   src LBUF-EVAL-A 0 ptr-field !  srcu LBUF-EVAL-U !
   [: LBUF-EVAL-RUN ;] catch {: rc:n :}
   NULL-PTR LBUF-EVAL-A !  0 LBUF-EVAL-U !
   LBUF-EVAL-OFF @ {: handle:n :}
   0 LBUF-EVAL-OFF !
   LBUF-PEND-CLEAR
   handle rc ;

: LBUF-EVAL! ( ptr u8 n ptr u8 n -- n )
   LBUF-EVAL {: handle:n rc:n :}
   rc 0 <> if rc throw then
   handle ;

\ Every region here holds cells, so DP is rounded to a cell before a definer
\ measures it; `create` rounds the same way and leaves it in place.
: LBUF-ALIGN ( -- )
   align ;

\ A sum is marked by its active tag: a shared payload slot is a code
\ cell while it holds a quotation and an address cell while it holds a pointer.
\ A niche value has no tag: its one cell is an address cell, zero or not.
: STORAGE-MARK-TERM ( ptr n n -- ) {: base:ptr term:n :}
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
      base term T-WIDTH 1- cells + @
      fam swap TFAM:TFAM-STORAGE-VARIANT
      0= if drop E-LAYOUT-BUFFER throw then {: vid:n :}
      0
      vid TFAM:SUMV-PAY-N 0 ?do
         term vid i TFAM:TFAM-STORAGE-PAY {: child:n width:n :}
         base over cells + child recurse
         width +
      loop drop
   else drop then ;

: STORAGE-MARK-FIXED ( ptr n ptr u8 n -- ) {: base:ptr type:ptr typeu:n :}
   type typeu CHECKER-STORAGE-TERM 0= if drop E-LAYOUT-BUFFER throw then
   {: term:n :}
   LBUF-N @ 0 ?do
      base i LBUF-W @ * cells + term STORAGE-MARK-TERM
   loop ;

: STORAGE-HAS-ADDRESS? ( ptr u8 n -- bool )
   CHECKER-STORAGE-TERM 0= if drop E-LAYOUT-BUFFER throw then
   TFAM:HAS-ADDRESS? ;

defer STORAGE-CLEAR-XT ( n n -- )
: STORAGE-CLEAR-MISSING ( n n -- ) 2drop E-LAYOUT-BUFFER throw ;
: STORAGE-CLEAR-DEFAULT ( -- )
   ['] STORAGE-CLEAR-MISSING is STORAGE-CLEAR-XT ;
STORAGE-CLEAR-DEFAULT

\ Quotation cells belong to the image even while null; the term walk declares
\ each code and address cell of these DATA allocations for capture.
: STORAGE-BIND ( n ptr n n ptr u8 n bool -- )
   {: handle:n base:ptr count:n type:ptr typeu:n hasaddr:bool :}
   hasaddr if
      base type typeu STORAGE-MARK-FIXED
   then
   handle base count CHECKER-STORAGE-BIND ;

\ ---- the stored type --------------------------------------------------------
\ Every definer reads its type here, so pointer chains, family applications and
\ spaced quotations reach the checker whole. The name stands on the definer's
\ line and the type's first token on the name's (STORAGE-LINE-TOKEN); a missing
\ name is refused at the definer (CHECKER-STORAGE-NAME-REFUSE), an empty type by
\ the checker (CHECKER-STORAGE-TYPE-REFUSE).

\ Leaves a token parse-name read to the next statement.
: STORAGE-UNREAD ( ptr u8 -- )
   NULL-PTR - data-base INP-CELL + ! ;

\ The next token, as parse-name reads one, when it stands on the cursor's line:
\ the bytes the read skips hold no line feed (CHECKER-TYPE-SPAN-BREAK?). A token
\ on a later line goes back to the input, and none is read.
: STORAGE-LINE-TOKEN ( -- ptr u8 n )
   data-base INP-CELL + @ {: at:n :}
   parse-name {: a:ptr u:n :}
   a NULL-PTR - at - {: gap:n :}
   a gap - gap CHECKER-TYPE-SPAN-BREAK? if a STORAGE-UNREAD a 0 exit then
   a u ;

: STORAGE-MORE? ( bool -- bool ) {: ended:bool :}
   ended if LBUF-FALSE exit then
   STORAGE-LINE-TOKEN {: a:ptr u:n :}
   u 0= if LBUF-FALSE exit then
   a STGT-A !  u STGT-U !  LBUF-TRUE ;

\ The whole spelling, ended where the checker ends one (CHECKER-TYPE-SPAN-STEP)
\ or with its line.
: STORAGE-PARSE-TYPE ( -- ptr u8 n )
   STORAGE-LINE-TOKEN STGT-U !  STGT-A !
   STGT-A @ STGT-START !
   0 begin STGT-A @ STGT-U @ CHECKER-TYPE-SPAN-STEP STORAGE-MORE? 0= until drop
   STGT-START @  STGT-A @ STGT-U @ + STGT-START @ - ;

\ The bytes a stored type takes, for a container's own arithmetic.
CAST: width>n ( width<t> -- n )

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
: LDEFER-RETIRE ( n n n ptr n -- ) {: oldoff:n oldcap:n wc:n cb:ptr :}
   oldcap 0= oldoff cb @ = or if exit then
   cb oldoff + {: oldbase:ptr :}
   oldbase BYTE-VIEW data-base BYTE-VIEW - oldcap wc * cells STORAGE-CLEAR-XT
   oldbase oldcap wc * cells LBUF-ZERO ;

: LDEFER-BIND ( n ptr n n -- )   \ count cbase wc
   {: count:n cb:ptr wc:n :}
   count wc * {: need:n :}
   cb @ {: oldoff:n :} cb CELL + @ {: oldcap:n :}
   count  cb CELL + @  > if
      LBUF-ALIGN
      here {: base:ptr :}
      need cells allot
      base cb -  cb  !                                  \ off-cell = new region offset from cb
      count  cb CELL +  !                               \ cap-cell = new allotted capacity
   then
   count  cb 2 CELL * +  !                             \ cnt-cell = live bound
   oldoff oldcap wc cb LDEFER-RETIRE ;

\ The axiom keeps the shared binder executable after the seal pass; a raw-memory
\ surface like allot and !, so not UNSAFE-TOK?.
\ Effect: ( count cbase wc -- ).
PRIM: LDEFER-BIND PE-N PE-IN PE-PTR-N PE-IN PE-N PE-IN PRIM;

\ NAME-GROW extends a column, preserving written cells. Past capacity the
\ region becomes max(2*capacity, count). A within-capacity grow
\ zeroes [old-live, count): a prior shrink may have left it dirty.
: LDEFER-GROW ( n ptr n n -- )   \ count cbase wc
   {: count:n cb:ptr wc:n :}
   cb @ {: oldoff:n :} cb CELL + @ {: oldcap:n :}
   count  cb CELL + @  > if
      cb 2 CELL * + @ {: live:n :}
      cb  cb @  + {: obase:ptr :}
      cb CELL + @ 2 *  count max {: newcap:n :}
      LBUF-ALIGN
      here {: nbase:ptr :}
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

\ Same seal treatment as LDEFER-BIND.
\ Effect: ( count cbase wc -- ).
PRIM: LDEFER-GROW PE-N PE-IN PE-PTR-N PE-IN PE-N PE-IN PRIM;

\ A companion's one cell, laid after it: it holds the block v and keeps the
\ address across a save.
: LDEFER-CELL ( ptr n -- ) {: v:ptr :}
   here cell-view {: at:ptr :}
   here ptr-cell-mark 0 ,
   v at 0 ptr-field ! ;

\ ---- LBUF: the routines a buffer child's body calls --------------------------
\ A buffer block starts with HEAD-CELLS header cells, indexed by OFF (the
\ elements' byte offset from the block), CAP and CNT (allotted and live element
\ counts) and STRIDE (an element's width in cells). An unbound block reads 0
\ in all but STRIDE. Each routine takes the block, or the cell holding it, last.
package LBUF
public

0 constant OFF
1 constant CAP
2 constant CNT
3 constant STRIDE
4 constant HEAD-CELLS

private

\ Element i of the block.
: ELEMENT ( n ptr n -- ptr n )
   {: i:n v:ptr :}
   v  v OFF cells + @ +  i v STRIDE cells + @ * cells + ;

\ Binds the block in the cell to count elements, as LDEFER-BIND does.
: BIND ( n ptr ptr n -- )
   {: count:n b:ptr :}
   b @ {: v:ptr :}
   count v  v STRIDE cells + @  LDEFER-BIND ;

\ Grows the block in the cell to count elements, as LDEFER-GROW does.
: GROW ( n ptr ptr n -- )
   {: count:n b:ptr :}
   b @ {: v:ptr :}
   count v  v STRIDE cells + @  LDEFER-GROW ;

\ A buffer's children are made over these routines' entries.
CAST: AS-ENTRY ( [ n ptr n -- ptr n ] -- n )
CAST: AS-RESIZE-ENTRY ( [ n ptr ptr n -- ] -- n )

CAST: AS-CHILD-RUN ( n -- [ ptr u8 n n n ptr u8 n -- n ] )

: CHILD ( ptr u8 n n n ptr u8 n -- n )
   data-base PROVIDED-XT:CHILD-CELL + @ AS-CHILD-RUN execute ;

public

\ Makes the buffer child NAME over TYPE and answers its effect handle.
: BUFFER-CHILD ( ptr u8 n ptr u8 n -- n )
   {: name:ptr nameu:n type:ptr typeu:n :}
   name nameu CHILD-ABI:BUFFER ['] ELEMENT AS-ENTRY
   type typeu CHILD ;

\ Lays NAME's -BIND and -GROW companions over the block v.
: COMPANIONS ( ptr u8 n ptr n -- ) {: name:ptr nameu:n v:ptr :}
   name nameu s" -BIND" LBUF-SUFFIXED CHILD-ABI:BIND
   ['] BIND AS-RESIZE-ENTRY s" " CHILD drop  v LDEFER-CELL
   name nameu s" -GROW" LBUF-SUFFIXED CHILD-ABI:GROW
   ['] GROW AS-RESIZE-ENTRY s" " CHILD drop  v LDEFER-CELL ;

;package

\ ---- LAYOUT-BUFFER ------------------------------------------------------------
\ Makes the buffer child NAME and lays its block: the header, bound to LBUF-N
\ elements of LBUF-W cells, then the elements, which the checker binds.
: LBUF-CHILD ( ptr u8 n ptr u8 n bool -- )
   {: name:ptr nameu:n type:ptr typeu:n hasaddr:bool :}
   name nameu type typeu LBUF:BUFFER-CHILD {: handle:n :}
   here {: v:ptr :}
   LBUF:HEAD-CELLS cells ,  LBUF-N @ ,  LBUF-N @ ,  LBUF-W @ ,
   LBUF-BYTES @ allot
   v LBUF:HEAD-CELLS cells + {: base:ptr :}
   handle base LBUF-N @ type typeu hasaddr STORAGE-BIND ;

: LAYOUT-BUFFER ( n -- ) {: count:n :}
   STORAGE-LINE-TOKEN {: name:ptr nameu:n :}
   nameu 0= if s" LAYOUT-BUFFER" CHECKER-STORAGE-NAME-REFUSE exit then
   STORAGE-PARSE-TYPE {: type:ptr typeu:n :}
   name nameu LBUF-NAME-OK? 0= if exit then
   count name nameu type typeu LBUF-VALIDATE 0= if exit then
   type typeu STORAGE-HAS-ADDRESS? {: hasaddr:bool :}
   name nameu type typeu hasaddr LBUF-CHILD ;

\ The axiom keeps LAYOUT-BUFFER checker-known, so top level may run it;
\ UNSAFE-TOK? refuses it inside checked bodies (it declares a word), so the
\ axiom adds no checked-code capability.
PRIM: LAYOUT-BUFFER PE-N PE-IN PRIM;

: DEFER-LAYOUT-BUFFER ( -- )
   STORAGE-LINE-TOKEN {: name:ptr nameu:n :}
   nameu 0= if s" DEFER-LAYOUT-BUFFER" CHECKER-STORAGE-NAME-REFUSE exit then
   STORAGE-PARSE-TYPE {: type:ptr typeu:n :}
   name nameu LBUF-NAME-OK? 0= if exit then
   name nameu s" -BIND" LBUF-SUFFIX-OK? 0= if exit then
   name nameu s" -GROW" LBUF-SUFFIX-OK? 0= if exit then
   type typeu CHECKER-LAYOUT-INFO 0= if
      2drop name nameu type typeu CHECKER-STORAGE-TYPE-REFUSE exit
   then
   LBUF-W !  drop
   name nameu type typeu LBUF:BUFFER-CHILD {: handle:n :}
   here {: v:ptr :}
   0 ,  0 ,  0 ,  LBUF-W @ ,
   handle v CHECKER-STORAGE-DEFER
   name nameu v LBUF:COMPANIONS ;

\ The axiom keeps DEFER-LAYOUT-BUFFER executable at top level after the seal
\ pass; it takes nothing from the stack (the count arrives at bind).
PRIM: DEFER-LAYOUT-BUFFER PRIM;

\ ---- TYPED-VARIABLE / TYPED-BUFFER --------------------------------------------
\ LAYOUT-BUFFER's machinery gated by CHECKER-STORAGE-INFO, so closed typed
\ pointers (`ptr TARGET`, `ptr res<n,n>`) and spaced quotations are expressible.

: STORAGE-VALIDATE ( n ptr u8 n ptr u8 n -- bool )
   {: count:n name:ptr nameu:n type:ptr typeu:n :}
   type typeu CHECKER-STORAGE-INFO 0= if
      drop name nameu type typeu CHECKER-STORAGE-TYPE-REFUSE LBUF-FALSE exit
   then
   LBUF-W !
   count LBUF-N !
   count LBUF-W @ LBUF-EXTENT? 0= if drop E-LAYOUT-BUFFER throw then
   LBUF-BYTES ! LBUF-TRUE ;

: TYPED-BUFFER ( n -- ) {: count:n :}
   STORAGE-LINE-TOKEN {: name:ptr nameu:n :}
   nameu 0= if s" TYPED-BUFFER" CHECKER-STORAGE-NAME-REFUSE exit then
   STORAGE-PARSE-TYPE {: type:ptr typeu:n :}
   name nameu LBUF-NAME-OK? 0= if exit then
   count name nameu type typeu STORAGE-VALIDATE 0= if exit then
   type typeu STORAGE-HAS-ADDRESS? {: hasaddr:bool :}
   name nameu type typeu hasaddr LBUF-CHILD ;

: TYPED-VARIABLE ( -- )
   STORAGE-LINE-TOKEN {: name:ptr nameu:n :}
   nameu 0= if s" TYPED-VARIABLE" CHECKER-STORAGE-NAME-REFUSE exit then
   STORAGE-PARSE-TYPE {: type:ptr typeu:n :}
   name nameu LBUF-NAME-OK? 0= if exit then
   1 name nameu type typeu STORAGE-VALIDATE 0= if exit then
   type typeu STORAGE-HAS-ADDRESS? {: hasaddr:bool :}
   name nameu type typeu TDECL-STORED-CHILD {: handle:n :}
   here {: v:ptr :}
   LBUF-BYTES @ allot
   handle v 1 type typeu hasaddr STORAGE-BIND ;

\ As for LAYOUT-BUFFER: top-level executable after the seal pass, refused inside
\ checked bodies by UNSAFE-TOK?.
PRIM: TYPED-BUFFER PE-N PE-IN PRIM;
PRIM: TYPED-VARIABLE PRIM;

;using
