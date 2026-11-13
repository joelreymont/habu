\ layout-buffer.f: the generative storage definers LAYOUT-BUFFER (the only
\ introduction form for `ptr layout`), DEFER-LAYOUT-BUFFER, TYPED-VARIABLE,
\ TYPED-BUFFER and DYNAMIC-BUFFER. Each evaluates a generated accessor the checker
\ authorizes once, by name, in the armed window (LBUF-EVAL, checker.f LBUF-PEND!);
\ a ptr variable never gains layout identity by unification. Admission: LAYOUT-BUFFER
\ CHECKER-LAYOUT-INFO, TYPED-* CHECKER-STORAGE-INFO, DYNAMIC-BUFFER CHECKER-DYNAMIC-INFO.

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
\ Every generated accessor addresses its storage through a `create`d word: habu2.f
\ C-DATA-ADDR emits a recorded DATA carrier (RELOC-EMIT:MARK-SITE), and every pass
\ that moves persisted DATA (snapshot restore, aot-file.f MERGE)
\ rewrites those and nothing else; a scalar literal never reads as an
\ address. `data-base <baked offset> +` is invisible to them: it survives a
\ snapshot but not a capture, where a merged window lands at another offset
\ (measured: 152 bytes low, so BKEY's accessor wrote BMID's cells).
\ `data-base <engine-layout constant> +` stays correct: src/habu/layout.f cells do
\ not move with a window.
: LBUF-BASE$ ( -- ptr u8 n ) s" #base" ;

\ Every derived name (the storage word, NAME-BIND, NAME-GROW, NAME-RESERVE,
\ NAME-RELEASE) is guarded like the declared one, so a collision fails loudly
\ instead of rebinding a word. LBUF-NAMES-OK? guards the declared name and its
\ storage word.
: LBUF-SUFFIX-OK? ( ptr u8 n ptr u8 n -- bool ) {: name:ptr nameu:n sfx:ptr sfxu:n :}
   LBUF-CLEAR
   name nameu LBUF-APP  sfx sfxu LBUF-APP
   LBUF-GEN LBUF-GEN-U @ LBUF-NAME-OK? ;

: LBUF-NAMES-OK? ( ptr u8 n -- bool ) {: name:ptr nameu:n :}
   name nameu LBUF-NAME-OK? 0= if LBUF-FALSE exit then
   name nameu LBUF-BASE$ LBUF-SUFFIX-OK? ;

: LBUF-BASE, ( ptr u8 n -- ) {: name:ptr nameu:n :}
   name nameu LBUF-APP  LBUF-BASE$ LBUF-APP ;

: LBUF-ALLOT, ( n -- ) {: bytes:n :}
   s"  " LBUF-APP  bytes LBUF-DEC,  s"  allot" LBUF-APP ;

\ Every generated text allots its storage right after its `create`, so each
\ definition in the text, which may lay data at `here`, follows the storage.
: LBUF-SOURCE ( ptr u8 n ptr u8 n -- ptr u8 n ptr u8 n )
   {: name:ptr nameu:n type:ptr typeu:n :}
   LBUF-CLEAR
   s" create " LBUF-APP  name nameu LBUF-BASE,  LBUF-BYTES @ LBUF-ALLOT,
   s"  : " LBUF-APP
   name nameu LBUF-NAME, {: pna:ptr pnu:n :}
   s"  ( n -- ptr " LBUF-APP  type typeu LBUF-APP
   s"  ) " LBUF-APP
   name nameu LBUF-BASE,
   s"  swap " LBUF-APP
   LBUF-W @ cells LBUF-DEC,
   s"  * + ;" LBUF-APP
   LBUF-GEN LBUF-GEN-U @ pna pnu ;

PTR-VARIABLE LBUF-EVAL-A
variable LBUF-EVAL-U
PTR-VARIABLE STGT-A
variable STGT-U
PTR-VARIABLE STGT-START

: LBUF-CAPTURE-PREPARE ( -- )
   LBUF-EVAL-U @ 0 > LBUF-PEND-U @ 0 > or if E-LAYOUT-BUFFER throw then
   NULL-PTR STGT-A !  0 STGT-U !  NULL-PTR STGT-START !
   LBUF-GEN-CAP 0 ?do 0 LBUF-GEN i + c! loop
   0 LBUF-GEN-U ! 0 LBUF-I ! 0 LBUF-N ! 0 LBUF-W ! 0 LBUF-BYTES ! ;

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

: LAYOUT-BUFFER ( n -- ) {: count:n :}
   STORAGE-LINE-TOKEN {: name:ptr nameu:n :}
   nameu 0= if s" LAYOUT-BUFFER" CHECKER-STORAGE-NAME-REFUSE exit then
   STORAGE-PARSE-TYPE {: type:ptr typeu:n :}
   TDECL-EVAL-ARMED @ 0= if E-LAYOUT-BUFFER throw then
   name nameu LBUF-NAMES-OK? 0= if exit then
   count name nameu type typeu LBUF-VALIDATE 0= if exit then
   type typeu STORAGE-HAS-ADDRESS? {: hasaddr:bool :}
   LBUF-ALIGN
   here {: base:ptr :}
   name nameu type typeu LBUF-SOURCE {: src:ptr srcu:n pna:ptr pnu:n :}
   src srcu pna pnu LBUF-EVAL! {: handle:n :}
   handle base LBUF-N @ type typeu hasaddr STORAGE-BIND ;

\ The axiom keeps LAYOUT-BUFFER checker-known, so top level may run it; UNSAFE-TOK? refuses it inside checked bodies (it evaluates generated
\ source), so the axiom adds no checked-code capability.
PRIM: LAYOUT-BUFFER PE-N PE-IN PRIM;

\ ---- DEFER-LAYOUT-BUFFER: a column sized by the model at build time ----------
\ Three control cells (offset, capacity, live count; all 0 = unbound) and an
\ accessor that reads them; NAME-BIND ( count -- ) allots count*width cells. The
\ armed window keys on the accessor name and signature (checker.f
\ LBUF-PEND-MATCH?), not the body, so it mints the LAYOUT-BUFFER signature.
\ NAME-BIND reuses the region when the count fits, else allots a larger one and
\ retires the old; its cells are fresh and zeroed. NAME-GROW carries the live
\ cells over. The accessor reads the offset cell on every call, so indices
\ survive a grow; a raw pointer derived from an accessor dangles across
\ NAME-GROW.

\ Every generated NAME-BIND is `<NAME>#base <wc> LDEFER-BIND`. The region offset
\ is measured from the control cells, not from `data-base`, so it relocates with
\ them (the `create` rule above). Retiring a moved column clears its old cells'
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

\ The axiom keeps the shared binder executable for the generated NAME-BIND words
\ after the seal pass; a raw-memory surface like allot and !, so not UNSAFE-TOK?.
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

\ Same seal treatment as LDEFER-BIND, for the generated NAME-GROW words.
\ Effect: ( count cbase wc -- ).
PRIM: LDEFER-GROW PE-N PE-IN PE-PTR-N PE-IN PE-N PE-IN PRIM;

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

;package

\ The `create` comes first so the accessor and both binders can name the control
\ cells; it runs no CHECK, so the accessor is still the first definition the
\ armed window authorizes by name.
: LDEFER-SOURCE ( ptr u8 n ptr u8 n -- ptr u8 n ptr u8 n )
   {: name:ptr nameu:n type:ptr typeu:n :}
   LBUF-CLEAR
   s" create " LBUF-APP  name nameu LBUF-BASE,  LBUF-BYTES @ LBUF-ALLOT,
   s"  : " LBUF-APP
   name nameu LBUF-NAME, {: pna:ptr pnu:n :}
   s"  ( n -- ptr " LBUF-APP  type typeu LBUF-APP
   s"  ) " LBUF-APP
   name nameu LBUF-BASE,  s"  " LBUF-APP  name nameu LBUF-BASE,
   s"  @ + swap " LBUF-APP  LBUF-W @ cells LBUF-DEC,
   s"  * + ; : " LBUF-APP
   name nameu LBUF-APP  s" -BIND ( n -- ) " LBUF-APP
   name nameu LBUF-BASE,  s"  " LBUF-APP  LBUF-W @ LBUF-DEC,
   s"  LDEFER-BIND ; : " LBUF-APP
   name nameu LBUF-APP  s" -GROW ( n -- ) " LBUF-APP
   name nameu LBUF-BASE,  s"  " LBUF-APP  LBUF-W @ LBUF-DEC,
   s"  LDEFER-GROW ;" LBUF-APP
   LBUF-GEN LBUF-GEN-U @ pna pnu ;

: DEFER-LAYOUT-BUFFER ( -- )
   STORAGE-LINE-TOKEN {: name:ptr nameu:n :}
   nameu 0= if s" DEFER-LAYOUT-BUFFER" CHECKER-STORAGE-NAME-REFUSE exit then
   STORAGE-PARSE-TYPE {: type:ptr typeu:n :}
   TDECL-EVAL-ARMED @ 0= if E-LAYOUT-BUFFER throw then
   name nameu LBUF-NAMES-OK? 0= if exit then
   name nameu s" -BIND" LBUF-SUFFIX-OK? 0= if exit then
   name nameu s" -GROW" LBUF-SUFFIX-OK? 0= if exit then
   type typeu CHECKER-LAYOUT-INFO 0= if
      2drop name nameu type typeu CHECKER-STORAGE-TYPE-REFUSE exit
   then
   LBUF-W !  drop
   LDEFER-CTRL-CELLS cells LBUF-BYTES !
   LBUF-ALIGN
   here {: cbase:ptr :}
   name nameu type typeu LDEFER-SOURCE {: src:ptr srcu:n pna:ptr pnu:n :}
   src srcu pna pnu LBUF-EVAL! {: handle:n :}
   handle cbase CHECKER-STORAGE-DEFER ;

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

: TYPED-VAR-SOURCE ( ptr u8 n ptr u8 n -- ptr u8 n ptr u8 n )
   {: name:ptr nameu:n type:ptr typeu:n :}
   LBUF-CLEAR
   s" create " LBUF-APP  name nameu LBUF-BASE,  LBUF-BYTES @ LBUF-ALLOT,
   s"  : " LBUF-APP
   name nameu LBUF-NAME, {: pna:ptr pnu:n :}
   s"  ( -- ptr " LBUF-APP  type typeu LBUF-APP
   s"  ) " LBUF-APP
   name nameu LBUF-BASE,
   s"  ;" LBUF-APP
   LBUF-GEN LBUF-GEN-U @ pna pnu ;

: TYPED-BUFFER ( n -- ) {: count:n :}
   STORAGE-LINE-TOKEN {: name:ptr nameu:n :}
   nameu 0= if s" TYPED-BUFFER" CHECKER-STORAGE-NAME-REFUSE exit then
   STORAGE-PARSE-TYPE {: type:ptr typeu:n :}
   TDECL-EVAL-ARMED @ 0= if E-LAYOUT-BUFFER throw then
   name nameu LBUF-NAMES-OK? 0= if exit then
   count name nameu type typeu STORAGE-VALIDATE 0= if exit then
   type typeu STORAGE-HAS-ADDRESS? {: hasaddr:bool :}
   LBUF-ALIGN
   here {: base:ptr :}
   name nameu type typeu LBUF-SOURCE {: src:ptr srcu:n pna:ptr pnu:n :}
   src srcu pna pnu LBUF-EVAL! {: handle:n :}
   handle base LBUF-N @ type typeu hasaddr STORAGE-BIND ;

: TYPED-VARIABLE ( -- )
   STORAGE-LINE-TOKEN {: name:ptr nameu:n :}
   nameu 0= if s" TYPED-VARIABLE" CHECKER-STORAGE-NAME-REFUSE exit then
   STORAGE-PARSE-TYPE {: type:ptr typeu:n :}
   TDECL-EVAL-ARMED @ 0= if E-LAYOUT-BUFFER throw then
   name nameu LBUF-NAMES-OK? 0= if exit then
   1 name nameu type typeu STORAGE-VALIDATE 0= if exit then
   type typeu STORAGE-HAS-ADDRESS? {: hasaddr:bool :}
   LBUF-ALIGN
   here {: base:ptr :}
   name nameu type typeu TYPED-VAR-SOURCE {: src:ptr srcu:n pna:ptr pnu:n :}
   src srcu pna pnu LBUF-EVAL! {: handle:n :}
   handle base LBUF-N @ type typeu hasaddr STORAGE-BIND ;

\ As for LAYOUT-BUFFER: top-level executable after the seal pass, refused inside
\ checked bodies by UNSAFE-TOK?.
PRIM: TYPED-BUFFER PE-N PE-IN PRIM;
PRIM: TYPED-VARIABLE PRIM;

\ ---- transient typed buffers (DYNAMIC-BUFFER) --------------------------------
\ The control head owns an OS mapping; RESERVE preserves old cells and may move
\ it, so callers keep indices, not pointers. The registry handle is registered on
\ first allocation and unregistered by RELEASE, so a declaration stays reusable
\ after capture and restore. The accessor fetches the head on every call and
\ adds index times width, with no index check.
\ The head is a declared PTR-VARIABLE, not `create`d: a raw storage cell never
\ holds an address. NAME#ctl names the numeric cells, so DYNAMIC-STORAGE never
\ views the head's pointer cell as bytes.
\ The element width is bytes, where LBUF-W holds cells, so a byte element is
\ expressible only here (CHECKER-DYNAMIC-INFO over CHECKER-STORAGE-INFO).
variable DBUF-W
\ Every dynamic control record (src/core/dynamic-storage.f): the head cell, then
\ the control block's exact byte capacity and registry slot + 1.
3 cells constant DBUF-CTRL-BYTES
: DBUF-CTL$ ( -- ptr u8 n ) s" #ctl" ;

: DBUF-CTL, ( ptr u8 n -- ) {: name:ptr nameu:n :}
   name nameu LBUF-APP  DBUF-CTL$ LBUF-APP ;

\ The control cells describe the head's mapping, so a save or a file's object
\ writes them zero with the head (scratch-cell-mark): the record it holds is
\ released.
: DBUF-MARKS, ( -- )
   DBUF-CTRL-BYTES CELL / 1- 0 ?do
      s"  here " LBUF-APP  i cells LBUF-DEC,  s"  + scratch-cell-mark" LBUF-APP
   loop ;

: DBUF-RECORD, ( ptr u8 n -- ) {: name:ptr nameu:n :}
   name nameu LBUF-BASE,  s"  " LBUF-APP  name nameu DBUF-CTL, ;

: DBUF-VALIDATE ( ptr u8 n ptr u8 n -- bool ) {: name:ptr nameu:n type:ptr typeu:n :}
   type typeu CHECKER-DYNAMIC-INFO 0= if
      drop name nameu type typeu CHECKER-STORAGE-TYPE-REFUSE LBUF-FALSE exit
   then
   DBUF-W ! LBUF-TRUE ;

: DBUF-SOURCE ( ptr u8 n ptr u8 n -- ptr u8 n ptr u8 n )
   {: name:ptr nameu:n type:ptr typeu:n :}
   LBUF-CLEAR
   s" PTR-VARIABLE " LBUF-APP name nameu LBUF-BASE,
   s"  create " LBUF-APP name nameu DBUF-CTL,  DBUF-MARKS,  DBUF-CTRL-BYTES CELL - LBUF-ALLOT,
   s"  : " LBUF-APP name nameu LBUF-NAME, {: pna:ptr pnu:n :}
   s"  ( n -- ptr " LBUF-APP type typeu LBUF-APP
   s"  ) " LBUF-APP name nameu LBUF-BASE,
   s"  @ swap " LBUF-APP DBUF-W @ LBUF-DEC,
   s"  * + ; : " LBUF-APP name nameu LBUF-APP
   s" -RESERVE ( n -- ) " LBUF-APP name nameu DBUF-RECORD,
   s"  " LBUF-APP DBUF-W @ LBUF-DEC,
   s"  DYNAMIC-STORAGE:RESERVE ; : " LBUF-APP name nameu LBUF-APP
   s" -RELEASE ( -- ) " LBUF-APP name nameu DBUF-RECORD,
   s"  DYNAMIC-STORAGE:RELEASE ;" LBUF-APP
   LBUF-GEN LBUF-GEN-U @ pna pnu ;

: DYNAMIC-BUFFER ( -- )
   STORAGE-LINE-TOKEN {: name:ptr nameu:n :}
   nameu 0= if s" DYNAMIC-BUFFER" CHECKER-STORAGE-NAME-REFUSE exit then
   STORAGE-PARSE-TYPE {: type:ptr typeu:n :}
   TDECL-EVAL-ARMED @ 0= if E-LAYOUT-BUFFER throw then
   name nameu LBUF-NAMES-OK? 0= if exit then
   name nameu s" -RESERVE" LBUF-SUFFIX-OK? 0= if exit then
   name nameu s" -RELEASE" LBUF-SUFFIX-OK? 0= if exit then
   name nameu DBUF-CTL$ LBUF-SUFFIX-OK? 0= if exit then
   name nameu type typeu DBUF-VALIDATE 0= if exit then
   name nameu type typeu DBUF-SOURCE {: src:ptr srcu:n pna:ptr pnu:n :}
   src srcu pna pnu LBUF-EVAL! drop ;

PRIM: DYNAMIC-BUFFER PRIM;

;using
