\ layout-buffer.f — generative typed fixed-capacity storage.
\
\ LAYOUT-BUFFER is the only public introduction form for `ptr layout`. It owns
\ allocation, zero-image initialization, stride, and bounds; the checker arms a
\ single generated-accessor authorization instead of allowing ptr variables to
\ acquire layout identity through ordinary unification.
\
\ TYPED-VARIABLE and TYPED-BUFFER (dot habu-nominal-storage-typed) are the
\ convenience definers built on the SAME generative boundary: a single typed
\ cell, and a typed fixed-capacity buffer. They reuse LAYOUT-BUFFER's armed
\ generated-accessor window (LBUF-EVAL / LBUF-PEND) and admit a broader
\ CHECKER-STORAGE-INFO type surface — nominal scalars, closed non-linear layout
\ families, AND closed typed pointers — without weakening LAYOUT-BUFFER, whose
\ own narrower CHECKER-LAYOUT-INFO gate is unchanged.

\ TDECL-EVAL-XT and TDECL-EVAL-ARMED are package TYPE-DECL's since dot
\ habu-tfam-2b-sealed-1b77662c; imported bare here rather than qualified, per
\ docs/forth.md's consumer rule.
using TYPE-DECL

$1000 constant LBUF-GEN-CAP
$7FFFFFFFFFFFFFFF constant LBUF-N-MAX
7121 constant E-LAYOUT-BUFFER
7122 constant E-LAYOUT-BOUNDS
7123 constant E-LAYOUT-UNBOUND    \ deferred column accessed before its NAME-BIND
7124 constant E-LAYOUT-CEIL       \ deferred bind past the generous per-column sanity ceiling
$100000 constant LDEFER-CELL-MAX  \ per-column allot ceiling (cells); generous, well under the data-region floor
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

: LBUF-VALIDATE ( n ptr u8 n -- ) {: count:n type:ptr typeu:n :}
   type typeu CHECKER-LAYOUT-INFO 0= if 2drop E-LAYOUT-BUFFER throw then
   LBUF-W ! drop
   count LBUF-N !
   count LBUF-W @ LBUF-EXTENT? 0= if drop E-LAYOUT-BUFFER throw then
   LBUF-BYTES ! ;

: LBUF-NAME-GUARD ( ptr u8 n -- ) {: name:ptr nameu:n :}
   name nameu CHECKER-LBUF-NAME-GUARD
   name nameu CHECKER-DEFINED-HERE? if E-DUP-DEFINITION throw then
   name nameu get-current search-wl 0 <> if E-DUP-DEFINITION throw then ;

: LBUF-ZERO ( ptr a n -- ) {: base:ptr bytes:n :}
   0 LBUF-I !
   begin LBUF-I @ bytes < while
      0 base LBUF-I @ + !
      LBUF-I @ CELL + LBUF-I !
   repeat ;

\ Cell-wise move (bytes is a whole number of cells: live-count * width). src and
\ dst are the abandoned and fresh column regions — always disjoint — so a forward
\ copy is safe. Sibling of LBUF-ZERO, on the same @/! surface.
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
\ EVERY GENERATED ACCESSOR ADDRESSES ITS STORAGE THROUGH A `create`d WORD, and
\ that is a relocation statement, not a style choice.
\
\ The engine has exactly one relocatable form for a DATA address: the fixed
\ four-instruction movz/movk x9 chain that habu2.f C-DATA-ADDR emits for
\ `create`/`variable` and records in the address map (SNAP-RELOC:MARK-SITE).
\ Every pass that MOVES persisted DATA - the snapshot restore, the AOT seed's
\ EM-AOT-RELOC-DATA, and aot-file.f's MERGE - rewrites those chains by one delta
\ and touches nothing else. A scalar literal is emitted through a different path
\ on purpose (habu2.f's literal note: minimal chain into x16), precisely so that
\ a number which happens to land inside a DATA address range can never be taken
\ for an address.
\
\ So `data-base <baked offset> +` - which is what these accessors used to emit -
\ is a reference no relocation pass can see. It survives a snapshot, where the
\ whole region moves together and every offset from the base keeps its meaning,
\ and it does NOT survive a capture: an AOT window's DATA is copied to whatever
\ DP the booting engine has reached, and a merged window is placed after the
\ host's, so the window's bytes land at a DIFFERENT offset from `data-base` than
\ they were compiled against. Measured on the compiler chain (dot
\ habu-bmid-module-id-ec6c709b): the merged window sat 152 bytes lower than the
\ baked offsets said, so BKEY's accessor wrote into BMID's cells and
\ IR-BUILD:MODULE@ answered 5 where the source-loaded control answered 1.
\
\ `data-base <engine-layout constant> +` is a different thing and stays correct:
\ those offsets name cells the engine reserves at a fixed place in every image
\ (src/habu/layout.f), so they are not DP-derived and do not move with a window.
\ The rule is about DP-derived offsets, which is exactly what a definer bakes.
: LBUF-BASE$ ( -- ptr u8 n ) s" #base" ;

\ The storage word is a definition like any other, so its name is guarded the
\ same way NAME-BIND and NAME-GROW are: a collision fails loudly here instead of
\ silently rebinding somebody's word.
: LBUF-BASE-GUARD ( ptr u8 n -- ) {: name:ptr nameu:n :}
   LBUF-CLEAR
   name nameu LBUF-APP  LBUF-BASE$ LBUF-APP
   LBUF-GEN LBUF-GEN-U @ LBUF-NAME-GUARD ;

: LBUF-BASE, ( ptr u8 n -- ) {: name:ptr nameu:n :}
   name nameu LBUF-APP  LBUF-BASE$ LBUF-APP ;

: LBUF-SOURCE ( ptr u8 n ptr u8 n -- ptr u8 n ptr u8 n )
   {: name:ptr nameu:n type:ptr typeu:n :}
   LBUF-CLEAR
   s" create " LBUF-APP  name nameu LBUF-BASE,
   s"  : " LBUF-APP
   name nameu LBUF-NAME, {: pna:ptr pnu:n :}
   s"  ( n -- ptr " LBUF-APP  type typeu LBUF-APP
   s"  ) {: i:n :} i 0 < if " LBUF-APP
   E-LAYOUT-BOUNDS LBUF-DEC,
   s"  throw then i " LBUF-APP
   LBUF-N @ LBUF-DEC,
   s"  >= if " LBUF-APP
   E-LAYOUT-BOUNDS LBUF-DEC,
   s"  throw then " LBUF-APP
   name nameu LBUF-BASE,
   s"  i " LBUF-APP
   LBUF-W @ cells LBUF-DEC,
   s"  * + ;" LBUF-APP
   LBUF-GEN LBUF-GEN-U @ pna pnu ;

PTR-VARIABLE LBUF-EVAL-A
variable LBUF-EVAL-U

: LBUF-EVAL-RUN ( -- )
   LBUF-EVAL-A 0 ptr-field @ LBUF-EVAL-U @ TDECL-EVAL-XT ;

: LBUF-EVAL ( ptr u8 n ptr u8 n -- n )
   {: src:ptr srcu:n name:ptr nameu:n :}
   TDECL-EVAL-ARMED @ 0= if E-LAYOUT-BUFFER throw then
   name nameu LBUF-PEND!
   src LBUF-EVAL-A 0 ptr-field !  srcu LBUF-EVAL-U !
   [: LBUF-EVAL-RUN ;] catch
   LBUF-PEND-CLEAR ;

: LBUF-EVAL! ( ptr u8 n ptr u8 n -- )
   LBUF-EVAL {: rc:n :}
   rc 0 <> if rc throw then ;

\ The `create` in the generated source publishes the storage word at the DP the
\ definer measured, and the definer allots and zeroes against that same DP once
\ the accessor has compiled. If anything moved DP in between, the accessor and
\ the storage would name different places, so this refuses rather than allot
\ into the gap. Nothing is allotted before the eval, so a rejected accessor
\ leaves DP exactly where it found it and needs no rewind.
: LBUF-ALLOT ( ptr a -- ) {: base:ptr :}
   here base <> if E-LAYOUT-BUFFER throw then
   LBUF-BYTES @ allot
   base LBUF-BYTES @ LBUF-ZERO ;

: LAYOUT-BUFFER ( n -- ) {: count:n :}
   parse-name {: name:ptr nameu:n :}
   parse-name {: type:ptr typeu:n :}
   nameu 0= if E-LAYOUT-BUFFER throw then
   TDECL-EVAL-ARMED @ 0= if E-LAYOUT-BUFFER throw then
   name nameu LBUF-NAME-GUARD
   name nameu LBUF-BASE-GUARD
   count type typeu LBUF-VALIDATE
   here {: base:ptr :}
   name nameu type typeu LBUF-SOURCE {: src:ptr srcu:n pna:ptr pnu:n :}
   src srcu pna pnu LBUF-EVAL!
   base LBUF-ALLOT ;

\ LAYOUT-BUFFER is the public top-level introduction form: it consumes the
\ count operand and parses its own name + type tokens. The axiom keeps it
\ checker-known so the seal-time internal-word marking pass
\ (src/core/internal-mark.f) leaves it executable at top level (dot
\ habu-hb-crash-bare-c5be6634). UNSAFE-TOK? rejects `layout-buffer` inside
\ checked bodies (it evaluates generated accessor source via LBUF-EVAL), so
\ the axiom adds no checked-code capability.
PRIM: LAYOUT-BUFFER PE-N PE-IN PRIM;

\ ---- DEFER-LAYOUT-BUFFER: derive-from-model deferred-offset column -----------
\ Sibling of LAYOUT-BUFFER whose storage is NOT allotted at library load: the
\ definer reserves three control cells (offset, capacity, live-count; all 0 =
\ unbound) and emits an accessor that reads them, plus a published NAME-BIND
\ ( count -- ) that allots count*width cells at build time and stores the offset
\ + count. So the table SIZE derives from the model (bound once per build from
\ the counted need) instead of a compile-time constant.
\
\ The accessor body reads the rebindable offset/count cells; per the checker's
\ armed LAYOUT-INTRO window (keyed on the pending accessor NAME + declared
\ signature, checker.f:9004-9007, not the body's arithmetic form) it mints the
\ SAME `( n -- ptr type )` as the immediate LAYOUT-BUFFER accessor. An access
\ before the first NAME-BIND (count-cell 0) dies NAMED (E-LAYOUT-UNBOUND),
\ red-first — never a silent zero-offset read.
\
\ Bind policy is grow-to-largest reuse, mirroring the landed executor arena
\ (maki/executor.f EX-ARENA-ENSURE, stage 1): NAME-BIND reuses the current
\ region when count fits the allotted capacity (cap-cell), else allots a fresh
\ larger region and abandons the predecessor — leak bounded by the largest
\ model, no copy, no mid-build base move (the region only grows before its cells
\ are written). A bind past LDEFER-CELL-MAX dies NAMED (E-LAYOUT-CEIL) BEFORE
\ any allot or cell store, so a too-big model leaves the prior tables intact
\ (the transactional boundary).
\
\ NAME-GROW ( count -- ) is the copy-on-grow sibling of NAME-BIND for a two-phase
\ table (bound once to a first-phase count, then extended incrementally): it
\ carries the live cells into the fresh larger region before abandoning the old
\ one, so nodes written before the grow survive. NAME-BIND stays fresh/zeroed
\ (unchanged semantics); preservation is opt-in via NAME-GROW alone.
\
\ USAGE LAW: a deferred accessor reads the offset cell on EVERY call, so the
\ column base may move between two accessor calls with no hazard — an index read
\ before a grow and one after both resolve against the live base. The ONE unsafe
\ act is holding a RAW pointer derived from an accessor across a NAME-GROW: the
\ grow abandons the old region, so that pointer dangles. Re-derive through the
\ accessor after any grow; never cache an accessor result across an append.

\ Shared runtime binder: every generated NAME-BIND is `<NAME>#base <wc>
\ LDEFER-BIND`, so the per-column emitted code stays tiny. The three control
\ cells arrive as the address of the column's `create`d storage word - the one
\ relocatable DATA-address form (see the note above LBUF-SOURCE) - and the
\ region offset it stores is measured FROM THOSE CELLS, not from `data-base`,
\ so it means the same thing wherever the pair ends up. It carries a certified
\ signature, so the seal-time internal-word pass leaves it executable for the
\ generated NAME-BIND callers.
: LDEFER-BIND ( n ptr a n -- )   \ count cbase wc
   {: count:n cb:ptr wc:n :}
   count 0 < if E-LAYOUT-BUFFER throw then
   count wc * {: need:n :}
   need LDEFER-CELL-MAX > if E-LAYOUT-CEIL throw then    \ transactional: die before any mutation
   count  cb CELL + @  > if                             \ count > capacity: grow-to-largest
      here {: base:ptr :}
      need cells allot
      base need cells LBUF-ZERO
      base cb -  cb  !                                  \ off-cell = new region offset from cb
      count  cb CELL +  !                               \ cap-cell = new allotted capacity
   then
   count  cb 2 CELL * +  ! ;                             \ cnt-cell = live bound

\ The generated NAME-BIND words are user-level (minted at model load), so the
\ shared binder must survive the seal-time internal-word pass. The axiom keeps it
\ checker-known and top-level executable (LAYOUT-BUFFER parity); it is not a
\ source-evaluating opener, so it is not UNSAFE-TOK? (raw-memory surface, like
\ allot/!). Effect: ( count cbase wc -- ).
PRIM: LDEFER-BIND PE-N PE-IN PE-PTR-A PE-IN PE-N PE-IN PRIM;

\ Copy-on-grow binder: extends a column already bound by LDEFER-BIND to `count`
\ live cells, PRESERVING the cells written so far. Growing unbound (cnt-cell 0)
\ dies NAMED (E-LAYOUT-UNBOUND) — the caller must BIND first (the MIR binds the
\ forward count at capture-finish, then GROWs during backward-build). Grow-to-at-
\ least lives here so callers stay dumb: when count outgrows the capacity the new
\ region is `max(2*capacity, count)` cells (doubling floor, clamped to `count`
\ when doubling would trip the ceiling), the live cells are carried over, and the
\ tail past the old live count is zeroed (a within-capacity grow zeroes only the
\ newly exposed [old-live, count) slots — a prior shrink may have left them dirty).
\ Like LDEFER-BIND it dies NAMED past LDEFER-CELL-MAX BEFORE any allot or store,
\ so a too-big grow leaves the prior region and its live data intact.
: LDEFER-GROW ( n ptr a n -- )   \ count cbase wc
   {: count:n cb:ptr wc:n :}
   count 0 < if E-LAYOUT-BUFFER throw then
   cb 2 CELL * + @ 0= if E-LAYOUT-UNBOUND throw then        \ grow requires a prior BIND
   count wc * {: need:n :}
   need LDEFER-CELL-MAX > if E-LAYOUT-CEIL throw then       \ transactional: die before any mutation
   count  cb CELL + @  > if                                 \ count > capacity: copy-on-grow
      cb 2 CELL * + @ {: live:n :}                          \ live cells to carry to the fresh region
      cb  cb @  + {: obase:ptr :}                           \ current region base
      cb CELL + @ 2 *  count max {: dbl:n :}                \ grow-to-at-least: doubling floor
      dbl wc * LDEFER-CELL-MAX > if count else dbl then {: newcap:n :}   \ clamp so doubling never trips the ceiling
      here {: nbase:ptr :}
      newcap wc * cells allot
      nbase newcap wc * cells LBUF-ZERO                      \ fresh region zeroed (new cells read 0)
      obase nbase  live wc * cells  LBUF-COPY                \ carry the live cells forward
      nbase cb -  cb  !                                      \ off-cell = new region offset from cb
      newcap  cb CELL +  !                                   \ cap-cell = new capacity
   else count  cb 2 CELL * + @  > if                         \ fits capacity but exposes new slots
      cb  cb @  +                                            \ region base
      cb 2 CELL * + @ wc * cells +                           \ + old-live * width cells
      count  cb 2 CELL * + @  -  wc * cells  LBUF-ZERO        \ zero [old-live, count)
   then then
   count  cb 2 CELL * +  ! ;                                 \ cnt-cell = new live bound

\ Same seal treatment as LDEFER-BIND: the axiom keeps the shared grow binder
\ checker-known and top-level executable for the generated NAME-GROW callers; it
\ is a raw-memory surface (allot/!), not a source-evaluating opener, so it is not
\ UNSAFE-TOK?. Effect: ( count cbase wc -- ).
PRIM: LDEFER-GROW PE-N PE-IN PE-PTR-A PE-IN PE-N PE-IN PRIM;

\ Generate the deferred accessor plus its NAME-BIND and NAME-GROW into one source.
\ The `create` comes first so the accessor and both binders can name the control
\ cells; it runs no CHECK, so the accessor is still the first DEFINITION and
\ LBUF-EVAL's one-shot armed window authorizes it by name. NAME-BIND and
\ NAME-GROW are ordinary checked words (each pushes the control-cell address and
\ the width, then calls LDEFER-BIND / LDEFER-GROW).
: LDEFER-SOURCE ( ptr u8 n ptr u8 n -- ptr u8 n ptr u8 n )
   {: name:ptr nameu:n type:ptr typeu:n :}
   LBUF-CLEAR
   s" create " LBUF-APP  name nameu LBUF-BASE,
   s"  : " LBUF-APP
   name nameu LBUF-NAME, {: pna:ptr pnu:n :}
   s"  ( n -- ptr " LBUF-APP  type typeu LBUF-APP
   s"  ) {: i:n :} i 0 < if " LBUF-APP
   E-LAYOUT-BOUNDS LBUF-DEC,
   s"  throw then " LBUF-APP  name nameu LBUF-BASE,
   s"  " LBUF-APP  2 CELL * LBUF-DEC,
   s"  + @ {: c:n :} c 0= if " LBUF-APP
   E-LAYOUT-UNBOUND LBUF-DEC,
   s"  throw then i c >= if " LBUF-APP
   E-LAYOUT-BOUNDS LBUF-DEC,
   s"  throw then " LBUF-APP
   name nameu LBUF-BASE,  s"  " LBUF-APP  name nameu LBUF-BASE,
   s"  @ + i " LBUF-APP  LBUF-W @ cells LBUF-DEC,
   s"  * + ; : " LBUF-APP
   name nameu LBUF-APP  s" -BIND ( n -- ) " LBUF-APP
   name nameu LBUF-BASE,  s"  " LBUF-APP  LBUF-W @ LBUF-DEC,
   s"  LDEFER-BIND ; : " LBUF-APP
   name nameu LBUF-APP  s" -GROW ( n -- ) " LBUF-APP
   name nameu LBUF-BASE,  s"  " LBUF-APP  LBUF-W @ LBUF-DEC,
   s"  LDEFER-GROW ;" LBUF-APP
   LBUF-GEN LBUF-GEN-U @ pna pnu ;

3 constant LDEFER-CTRL-CELLS   \ off-cell, cap-cell, cnt-cell

: DEFER-LAYOUT-BUFFER ( -- )
   parse-name {: name:ptr nameu:n :}
   parse-name {: type:ptr typeu:n :}
   nameu 0= if E-LAYOUT-BUFFER throw then
   TDECL-EVAL-ARMED @ 0= if E-LAYOUT-BUFFER throw then
   name nameu LBUF-NAME-GUARD
   name nameu LBUF-BASE-GUARD                              \ guard the control-cell word too
   LBUF-CLEAR  name nameu LBUF-APP  s" -BIND" LBUF-APP     \ guard the published NAME-BIND too
   LBUF-GEN LBUF-GEN-U @ LBUF-NAME-GUARD
   LBUF-CLEAR  name nameu LBUF-APP  s" -GROW" LBUF-APP     \ guard the published NAME-GROW too
   LBUF-GEN LBUF-GEN-U @ LBUF-NAME-GUARD
   type typeu CHECKER-LAYOUT-INFO 0= if 2drop E-LAYOUT-BUFFER throw then
   LBUF-W !  drop                                          \ width (cells) from the layout family
   LDEFER-CTRL-CELLS cells LBUF-BYTES !                    \ the control cells this definer owns
   here {: cbase:ptr :}
   name nameu type typeu LDEFER-SOURCE {: src:ptr srcu:n pna:ptr pnu:n :}
   src srcu pna pnu LBUF-EVAL!
   cbase LBUF-ALLOT ;                                      \ all 0 = unbound

\ Like LAYOUT-BUFFER: the axiom keeps DEFER-LAYOUT-BUFFER checker-known so the
\ seal-time internal-word pass leaves it top-level executable; it parses its own
\ name + type and consumes nothing from the stack (the count arrives at bind).
PRIM: DEFER-LAYOUT-BUFFER PRIM;

\ ---- TYPED-VARIABLE / TYPED-BUFFER convenience definers ----------------------
\ Same generative machinery as LAYOUT-BUFFER (name guard, allocation, zero image,
\ generated-accessor evaluation under the armed window, transactional rollback),
\ gated by the broader CHECKER-STORAGE-INFO admissibility. TYPED-BUFFER reuses
\ LBUF-SOURCE (the indexed `( n -- ptr type )` accessor); TYPED-VARIABLE emits a
\ single-cell `( -- ptr type )` accessor. Both parse a `ptr* base` stored type so
\ closed typed pointers (`ptr TARGET`, `ptr res<n,n>`) are expressible.

variable STGT-A
variable STGT-U
variable STGT-START

: STORAGE-PTR-TOK? ( ptr u8 n -- bool )   \ token is the pointer constructor `ptr`
   s" ptr" CORE-STR= ;

: STORAGE-QUOT-OPEN? ( ptr u8 n -- bool )   \ token is the quotation opener `[`
   s" [" CORE-STR= ;

: STORAGE-QUOT-CLOSE? ( ptr u8 n -- bool )   \ token is the quotation closer `]`
   s" ]" CORE-STR= ;

\ Consume a spaced `[ in -- out ]` xt<effect> quotation type token by token, up
\ to and including the closer, so the returned span is the whole quotation.
: STORAGE-PARSE-QUOT ( -- )
   begin STGT-A @ STGT-U @ STORAGE-QUOT-CLOSE? 0= while
      parse-name STGT-U !  STGT-A !
      STGT-U @ 0= if E-LAYOUT-BUFFER throw then
   repeat ;

: STORAGE-PARSE-TYPE ( -- ptr u8 n )   \ capture a `ptr* base` or `[ in -- out ]` stored-type source span
   parse-name STGT-U !  STGT-A !
   STGT-U @ 0= if E-LAYOUT-BUFFER throw then
   STGT-A @ STGT-START !
   begin STGT-A @ STGT-U @ STORAGE-PTR-TOK? while
      parse-name STGT-U !  STGT-A !
      STGT-U @ 0= if E-LAYOUT-BUFFER throw then
   repeat
   STGT-A @ STGT-U @ STORAGE-QUOT-OPEN? if STORAGE-PARSE-QUOT then
   STGT-START @  STGT-A @ STGT-U @ + STGT-START @ - ;

: STORAGE-VALIDATE ( n ptr u8 n -- ) {: count:n type:ptr typeu:n :}
   type typeu CHECKER-STORAGE-INFO 0= if drop E-LAYOUT-BUFFER throw then
   LBUF-W !
   count LBUF-N !
   count LBUF-W @ LBUF-EXTENT? 0= if drop E-LAYOUT-BUFFER throw then
   LBUF-BYTES ! ;

: TYPED-VAR-SOURCE ( ptr u8 n ptr u8 n -- ptr u8 n ptr u8 n )
   {: name:ptr nameu:n type:ptr typeu:n :}
   LBUF-CLEAR
   s" create " LBUF-APP  name nameu LBUF-BASE,
   s"  : " LBUF-APP
   name nameu LBUF-NAME, {: pna:ptr pnu:n :}
   s"  ( -- ptr " LBUF-APP  type typeu LBUF-APP
   s"  ) " LBUF-APP
   name nameu LBUF-BASE,
   s"  ;" LBUF-APP
   LBUF-GEN LBUF-GEN-U @ pna pnu ;

: TYPED-BUFFER ( n -- ) {: count:n :}
   parse-name {: name:ptr nameu:n :}
   STORAGE-PARSE-TYPE {: type:ptr typeu:n :}
   nameu 0= if E-LAYOUT-BUFFER throw then
   TDECL-EVAL-ARMED @ 0= if E-LAYOUT-BUFFER throw then
   name nameu LBUF-NAME-GUARD
   name nameu LBUF-BASE-GUARD
   count type typeu STORAGE-VALIDATE
   here {: base:ptr :}
   name nameu type typeu LBUF-SOURCE {: src:ptr srcu:n pna:ptr pnu:n :}
   src srcu pna pnu LBUF-EVAL!
   base LBUF-ALLOT ;

: TYPED-VARIABLE ( -- )
   parse-name {: name:ptr nameu:n :}
   STORAGE-PARSE-TYPE {: type:ptr typeu:n :}
   nameu 0= if E-LAYOUT-BUFFER throw then
   TDECL-EVAL-ARMED @ 0= if E-LAYOUT-BUFFER throw then
   name nameu LBUF-NAME-GUARD
   name nameu LBUF-BASE-GUARD
   1 type typeu STORAGE-VALIDATE
   here {: base:ptr :}
   name nameu type typeu TYPED-VAR-SOURCE {: src:ptr srcu:n pna:ptr pnu:n :}
   src srcu pna pnu LBUF-EVAL!
   base LBUF-ALLOT ;

\ Axioms keep the two definers checker-known so the seal-time internal-word pass
\ leaves them executable at top level (like LAYOUT-BUFFER); UNSAFE-TOK? rejects
\ `typed-buffer`/`typed-variable` inside checked bodies (they evaluate generated
\ accessor source), so the axioms add no checked-code capability.
PRIM: TYPED-BUFFER PE-N PE-IN PRIM;
PRIM: TYPED-VARIABLE PRIM;

;using
