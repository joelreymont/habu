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
   name nameu CHECKER-DEFINED? if E-DUP-DEFINITION throw then
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

: LBUF-SOURCE ( ptr u8 n ptr u8 n n -- ptr u8 n ptr u8 n )
   {: name:ptr nameu:n type:ptr typeu:n off:n :}
   LBUF-CLEAR
   s" : " LBUF-APP
   name nameu LBUF-NAME, {: pna:ptr pnu:n :}
   s"  ( n -- ptr " LBUF-APP  type typeu LBUF-APP
   s"  ) {: i:n :} i 0 < if " LBUF-APP
   E-LAYOUT-BOUNDS LBUF-DEC,
   s"  throw then i " LBUF-APP
   LBUF-N @ LBUF-DEC,
   s"  >= if " LBUF-APP
   E-LAYOUT-BOUNDS LBUF-DEC,
   s"  throw then data-base " LBUF-APP
   off LBUF-DEC,
   s"  + i " LBUF-APP
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

: LBUF-ROLLBACK ( n -- ) {: rc:n :}
   LBUF-BYTES @ negate allot
   rc throw ;

: LAYOUT-BUFFER ( n -- ) {: count:n :}
   parse-name {: name:ptr nameu:n :}
   parse-name {: type:ptr typeu:n :}
   nameu 0= if E-LAYOUT-BUFFER throw then
   TDECL-EVAL-ARMED @ 0= if E-LAYOUT-BUFFER throw then
   name nameu LBUF-NAME-GUARD
   count type typeu LBUF-VALIDATE
   here {: base:ptr :}
   base data-base - {: off:n :}
   name nameu type typeu off LBUF-SOURCE {: src:ptr srcu:n pna:ptr pnu:n :}
   LBUF-BYTES @ allot
   base LBUF-BYTES @ LBUF-ZERO
   src srcu pna pnu LBUF-EVAL
   dup 0 <> if LBUF-ROLLBACK then
   drop ;

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

\ Shared runtime binder: every generated NAME-BIND is `<offo capo cnto wc>
\ LDEFER-BIND`, so the per-column emitted code stays tiny. Cell offsets are
\ data-base-relative (relocation-safe like the immediate accessor's baked
\ offset). It carries a certified signature, so the seal-time internal-word pass
\ leaves it executable for the generated NAME-BIND callers.
: LDEFER-BIND ( n n n n n -- )   \ count offo capo cnto wc
   {: count:n offo:n capo:n cnto:n wc:n :}
   count 0 < if E-LAYOUT-BUFFER throw then
   count wc * {: need:n :}
   need LDEFER-CELL-MAX > if E-LAYOUT-CEIL throw then    \ transactional: die before any mutation
   count  data-base capo + @  > if                      \ count > capacity: grow-to-largest
      here {: base:ptr :}
      need cells allot
      base need cells LBUF-ZERO
      base data-base -  offo data-base +  !             \ off-cell = new region offset
      count  capo data-base +  !                        \ cap-cell = new allotted capacity
   then
   count  cnto data-base +  ! ;                          \ cnt-cell = live bound

\ The generated NAME-BIND words are user-level (minted at model load), so the
\ shared binder must survive the seal-time internal-word pass. The axiom keeps it
\ checker-known and top-level executable (LAYOUT-BUFFER parity); it is not a
\ source-evaluating opener, so it is not UNSAFE-TOK? (raw-memory surface, like
\ allot/!). Effect: ( count offo capo cnto wc -- ).
PRIM: LDEFER-BIND PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN PRIM;

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
: LDEFER-GROW ( n n n n n -- )   \ count offo capo cnto wc
   {: count:n offo:n capo:n cnto:n wc:n :}
   count 0 < if E-LAYOUT-BUFFER throw then
   data-base cnto + @ 0= if E-LAYOUT-UNBOUND throw then    \ grow requires a prior BIND
   count wc * {: need:n :}
   need LDEFER-CELL-MAX > if E-LAYOUT-CEIL throw then       \ transactional: die before any mutation
   count  data-base capo + @  > if                          \ count > capacity: copy-on-grow
      data-base cnto + @ {: live:n :}                       \ live cells to carry to the fresh region
      data-base  data-base offo + @  + {: obase:ptr :}      \ current region base
      data-base capo + @ 2 *  count max {: dbl:n :}          \ grow-to-at-least: doubling floor
      dbl wc * LDEFER-CELL-MAX > if count else dbl then {: newcap:n :}   \ clamp so doubling never trips the ceiling
      here {: nbase:ptr :}
      newcap wc * cells allot
      nbase newcap wc * cells LBUF-ZERO                      \ fresh region zeroed (new cells read 0)
      obase nbase  live wc * cells  LBUF-COPY                \ carry the live cells forward
      nbase data-base -  offo data-base +  !                 \ off-cell = new region offset
      newcap  capo data-base +  !                            \ cap-cell = new capacity
   else count  data-base cnto + @  > if                      \ fits capacity but exposes new slots
      data-base  data-base offo + @  +                       \ region base
      data-base cnto + @ wc * cells +                        \ + old-live * width cells
      count  data-base cnto + @  -  wc * cells  LBUF-ZERO     \ zero [old-live, count)
   then then
   count  cnto data-base +  ! ;                              \ cnt-cell = new live bound

\ Same seal treatment as LDEFER-BIND: the axiom keeps the shared grow binder
\ checker-known and top-level executable for the generated NAME-GROW callers; it
\ is a raw-memory surface (allot/!), not a source-evaluating opener, so it is not
\ UNSAFE-TOK?. Effect: ( count offo capo cnto wc -- ).
PRIM: LDEFER-GROW PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN PRIM;

\ Generate the deferred accessor plus its NAME-BIND and NAME-GROW into one source.
\ The accessor is the FIRST definition so LBUF-EVAL's one-shot armed window
\ authorizes it by name; NAME-BIND and NAME-GROW are ordinary checked words (each
\ pushes the four baked literals and calls LDEFER-BIND / LDEFER-GROW).
: LDEFER-SOURCE ( ptr u8 n ptr u8 n n n n -- ptr u8 n ptr u8 n )
   {: name:ptr nameu:n type:ptr typeu:n offo:n capo:n cnto:n :}
   LBUF-CLEAR
   s" : " LBUF-APP
   name nameu LBUF-NAME, {: pna:ptr pnu:n :}
   s"  ( n -- ptr " LBUF-APP  type typeu LBUF-APP
   s"  ) {: i:n :} i 0 < if " LBUF-APP
   E-LAYOUT-BOUNDS LBUF-DEC,
   s"  throw then data-base " LBUF-APP  cnto LBUF-DEC,
   s"  + @ {: c:n :} c 0= if " LBUF-APP
   E-LAYOUT-UNBOUND LBUF-DEC,
   s"  throw then i c >= if " LBUF-APP
   E-LAYOUT-BOUNDS LBUF-DEC,
   s"  throw then data-base data-base " LBUF-APP  offo LBUF-DEC,
   s"  + @ + i " LBUF-APP  LBUF-W @ cells LBUF-DEC,
   s"  * + ; : " LBUF-APP
   name nameu LBUF-APP  s" -BIND ( n -- ) " LBUF-APP
   offo LBUF-DEC,  s"  " LBUF-APP  capo LBUF-DEC,  s"  " LBUF-APP
   cnto LBUF-DEC,  s"  " LBUF-APP  LBUF-W @ LBUF-DEC,
   s"  LDEFER-BIND ; : " LBUF-APP
   name nameu LBUF-APP  s" -GROW ( n -- ) " LBUF-APP
   offo LBUF-DEC,  s"  " LBUF-APP  capo LBUF-DEC,  s"  " LBUF-APP
   cnto LBUF-DEC,  s"  " LBUF-APP  LBUF-W @ LBUF-DEC,
   s"  LDEFER-GROW ;" LBUF-APP
   LBUF-GEN LBUF-GEN-U @ pna pnu ;

: DEFER-LAYOUT-BUFFER ( -- )
   parse-name {: name:ptr nameu:n :}
   parse-name {: type:ptr typeu:n :}
   nameu 0= if E-LAYOUT-BUFFER throw then
   TDECL-EVAL-ARMED @ 0= if E-LAYOUT-BUFFER throw then
   name nameu LBUF-NAME-GUARD
   LBUF-CLEAR  name nameu LBUF-APP  s" -BIND" LBUF-APP     \ guard the published NAME-BIND too
   LBUF-GEN LBUF-GEN-U @ LBUF-NAME-GUARD
   LBUF-CLEAR  name nameu LBUF-APP  s" -GROW" LBUF-APP     \ guard the published NAME-GROW too
   LBUF-GEN LBUF-GEN-U @ LBUF-NAME-GUARD
   type typeu CHECKER-LAYOUT-INFO 0= if 2drop E-LAYOUT-BUFFER throw then
   LBUF-W !  drop                                          \ width (cells) from the layout family
   here {: cbase:ptr :}
   3 cells allot                                           \ off-cell, cap-cell, cnt-cell
   cbase 3 cells LBUF-ZERO                                 \ all 0 = unbound
   cbase data-base - {: offo:n :}
   offo CELL + {: capo:n :}
   offo 2 CELL * + {: cnto:n :}
   name nameu type typeu offo capo cnto LDEFER-SOURCE {: src:ptr srcu:n pna:ptr pnu:n :}
   3 cells LBUF-BYTES !                                    \ rollback rewinds the control cells
   src srcu pna pnu LBUF-EVAL
   dup 0 <> if LBUF-ROLLBACK then
   drop ;

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

: TYPED-VAR-SOURCE ( ptr u8 n ptr u8 n n -- ptr u8 n ptr u8 n )
   {: name:ptr nameu:n type:ptr typeu:n off:n :}
   LBUF-CLEAR
   s" : " LBUF-APP
   name nameu LBUF-NAME, {: pna:ptr pnu:n :}
   s"  ( -- ptr " LBUF-APP  type typeu LBUF-APP
   s"  ) data-base " LBUF-APP
   off LBUF-DEC,
   s"  + ;" LBUF-APP
   LBUF-GEN LBUF-GEN-U @ pna pnu ;

: TYPED-BUFFER ( n -- ) {: count:n :}
   parse-name {: name:ptr nameu:n :}
   STORAGE-PARSE-TYPE {: type:ptr typeu:n :}
   nameu 0= if E-LAYOUT-BUFFER throw then
   TDECL-EVAL-ARMED @ 0= if E-LAYOUT-BUFFER throw then
   name nameu LBUF-NAME-GUARD
   count type typeu STORAGE-VALIDATE
   here {: base:ptr :}
   base data-base - {: off:n :}
   name nameu type typeu off LBUF-SOURCE {: src:ptr srcu:n pna:ptr pnu:n :}
   LBUF-BYTES @ allot
   base LBUF-BYTES @ LBUF-ZERO
   src srcu pna pnu LBUF-EVAL
   dup 0 <> if LBUF-ROLLBACK then
   drop ;

: TYPED-VARIABLE ( -- )
   parse-name {: name:ptr nameu:n :}
   STORAGE-PARSE-TYPE {: type:ptr typeu:n :}
   nameu 0= if E-LAYOUT-BUFFER throw then
   TDECL-EVAL-ARMED @ 0= if E-LAYOUT-BUFFER throw then
   name nameu LBUF-NAME-GUARD
   1 type typeu STORAGE-VALIDATE
   here {: base:ptr :}
   base data-base - {: off:n :}
   name nameu type typeu off TYPED-VAR-SOURCE {: src:ptr srcu:n pna:ptr pnu:n :}
   LBUF-BYTES @ allot
   base LBUF-BYTES @ LBUF-ZERO
   src srcu pna pnu LBUF-EVAL
   dup 0 <> if LBUF-ROLLBACK then
   drop ;

\ Axioms keep the two definers checker-known so the seal-time internal-word pass
\ leaves them executable at top level (like LAYOUT-BUFFER); UNSAFE-TOK? rejects
\ `typed-buffer`/`typed-variable` inside checked bodies (they evaluate generated
\ accessor source), so the axioms add no checked-code capability.
PRIM: TYPED-BUFFER PE-N PE-IN PRIM;
PRIM: TYPED-VARIABLE PRIM;
