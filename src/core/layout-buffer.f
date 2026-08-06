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
\ generated-accessor window (LAYOUT-BUF:EVAL / LBUF-PEND) and admit a broader
\ CHECKER-STORAGE-INFO type surface — nominal scalars, closed non-linear layout
\ families, AND closed typed pointers — without weakening LAYOUT-BUFFER, whose
\ own narrower CHECKER-LAYOUT-INFO gate is unchanged.
\
\ The machinery lives in package LAYOUT-BUF. The four declaration keywords stay
\ global one-line entries at the bottom, the package-first exception STRUCTURE
\ and ENUM already carry (src/core/structure-decl.f, src/core/enum-decl.f): a
\ user writes them bare at top level, so they cannot be qualified names. The
\ error codes stay global too, because every package that throws or catches them
\ has to name them (docs/forth.md § Packages, the lib/errors.f rule).

7121 constant E-LAYOUT-BUFFER
7122 constant E-LAYOUT-BOUNDS
7123 constant E-LAYOUT-UNBOUND    \ deferred column accessed before its NAME-BIND
7124 constant E-LAYOUT-CEIL       \ deferred bind past the generous per-column sanity ceiling
78 constant E-DUP-DEFINITION

\ Members whose plain stem-drop would collide keep a distinguishing name: a
\ package member is visible to every member defined after it, so a constant
\ named TRUE or FALSE, a variable named I, or a word named SOURCE or C, would
\ silently capture the core word these bodies call. YES/NO follows enum-decl.f.
package LAYOUT-BUF

$1000 constant GEN-CAP
$7FFFFFFFFFFFFFFF constant N-MAX
$100000 constant CELL-MAX         \ per-column allot ceiling (cells); generous, well under the data-region floor
0 constant NO
-1 constant YES

create GEN GEN-CAP allot
variable GEN-U
variable IDX
variable ROWS
variable CELLW
variable SIZE

: CLEAR ( -- )
   0 GEN-U ! ;

: PUT ( n -- ) {: c:n :}
   GEN-U @ GEN-CAP >= if E-LAYOUT-BUFFER throw then
   c GEN GEN-U @ + c!
   GEN-U @ 1 + GEN-U ! ;

: APP ( ptr u8 n -- ) {: a:ptr u:n :}
   0 IDX !
   begin IDX @ u < while
      a IDX @ + c@ PUT
      IDX @ 1 + IDX !
   repeat ;

: PUT-DEC ( n -- ) {: n:n :}
   n 10 >= if n 10 / recurse then
   n 10 mod 48 + PUT ;

: EXTENT? ( n n -- n bool ) {: count:n width:n :}
   count 0 <= width 0 <= or if 0 NO exit then
   count N-MAX width / > if 0 NO exit then
   count width * {: cellsn:n :}
   cellsn N-MAX CELL / > if 0 NO exit then
   cellsn cells YES ;

: VALIDATE-LAYOUT ( n ptr u8 n -- ) {: count:n type:ptr typeu:n :}
   type typeu CHECKER-LAYOUT-INFO 0= if 2drop E-LAYOUT-BUFFER throw then
   CELLW ! drop
   count ROWS !
   count CELLW @ EXTENT? 0= if drop E-LAYOUT-BUFFER throw then
   SIZE ! ;

: NAME-GUARD ( ptr u8 n -- ) {: name:ptr nameu:n :}
   name nameu CHECKER-LBUF-NAME-GUARD
   name nameu CHECKER-DEFINED? if E-DUP-DEFINITION throw then
   name nameu get-current search-wl 0 <> if E-DUP-DEFINITION throw then ;

: ZERO ( ptr n n -- ) {: base:ptr bytes:n :}
   0 IDX !
   begin IDX @ bytes < while
      0 base IDX @ + !
      IDX @ CELL + IDX !
   repeat ;

\ Cell-wise move (bytes is a whole number of cells: live-count * width). src and
\ dst are the abandoned and fresh column regions — always disjoint — so a forward
\ copy is safe. Sibling of ZERO, on the same @/! surface.
: COPY ( ptr a ptr a n -- ) {: src:ptr dst:ptr bytes:n :}
   0 IDX !
   begin IDX @ bytes < while
      src IDX @ + @  dst IDX @ + !
      IDX @ CELL + IDX !
   repeat ;

: PUT-NAME ( ptr u8 n -- ptr u8 n ) {: name:ptr nameu:n :}
   GEN-U @ {: start:n :}
   name nameu APP
   GEN start + nameu ;

: ACCESSOR-SRC ( ptr u8 n ptr u8 n n -- ptr u8 n ptr u8 n )
   {: name:ptr nameu:n type:ptr typeu:n off:n :}
   CLEAR
   s" : " APP
   name nameu PUT-NAME {: pna:ptr pnu:n :}
   s"  ( n -- ptr " APP  type typeu APP
   s"  ) {: i:n :} i 0 < if " APP
   E-LAYOUT-BOUNDS PUT-DEC
   s"  throw then i " APP
   ROWS @ PUT-DEC
   s"  >= if " APP
   E-LAYOUT-BOUNDS PUT-DEC
   s"  throw then data-base " APP
   off PUT-DEC
   s"  + i " APP
   CELLW @ cells PUT-DEC
   s"  * + ;" APP
   GEN GEN-U @ pna pnu ;

PTR-VARIABLE EVAL-A
variable EVAL-U

: EVAL-RUN ( -- )
   EVAL-A 0 ptr-field @ EVAL-U @ TDECL-EVAL-XT ;

: EVAL ( ptr u8 n ptr u8 n -- n )
   {: src:ptr srcu:n name:ptr nameu:n :}
   TDECL-EVAL-ARMED @ 0= if E-LAYOUT-BUFFER throw then
   name nameu LBUF-PEND!
   src EVAL-A 0 ptr-field !  srcu EVAL-U !
   [: EVAL-RUN ;] catch
   LBUF-PEND-CLEAR ;

: ROLLBACK ( n -- ) {: rc:n :}
   SIZE @ negate allot
   rc throw ;

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
\ are written). A bind past CELL-MAX dies NAMED (E-LAYOUT-CEIL) BEFORE
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

public

\ Shared runtime binder: every generated NAME-BIND is `<offo capo cnto wc>
\ LAYOUT-BUF:BIND`, so the per-column emitted code stays tiny. Cell offsets are
\ data-base-relative (relocation-safe like the immediate accessor's baked
\ offset). It is public and carries a PPRIM: axiom because the generated
\ NAME-BIND words are ordinary user-level checked definitions that call it by
\ its qualified name.
: BIND ( n n n n n -- )   \ count offo capo cnto wc
   {: count:n offo:n capo:n cnto:n wc:n :}
   count 0 < if E-LAYOUT-BUFFER throw then
   count wc * {: need:n :}
   need CELL-MAX > if E-LAYOUT-CEIL throw then           \ transactional: die before any mutation
   count  data-base capo + @  > if                      \ count > capacity: grow-to-largest
      here {: base:ptr :}
      need cells allot
      base need cells ZERO
      base data-base -  offo data-base +  !             \ off-cell = new region offset
      count  capo data-base +  !                        \ cap-cell = new allotted capacity
   then
   count  cnto data-base +  ! ;                          \ cnt-cell = live bound

\ Copy-on-grow binder: extends a column already bound by BIND to `count`
\ live cells, PRESERVING the cells written so far. Growing unbound (cnt-cell 0)
\ dies NAMED (E-LAYOUT-UNBOUND) — the caller must BIND first (the MIR binds the
\ forward count at capture-finish, then GROWs during backward-build). Grow-to-at-
\ least lives here so callers stay dumb: when count outgrows the capacity the new
\ region is `max(2*capacity, count)` cells (doubling floor, clamped to `count`
\ when doubling would trip the ceiling), the live cells are carried over, and the
\ tail past the old live count is zeroed (a within-capacity grow zeroes only the
\ newly exposed [old-live, count) slots — a prior shrink may have left them dirty).
\ Like BIND it dies NAMED past CELL-MAX BEFORE any allot or store,
\ so a too-big grow leaves the prior region and its live data intact.
: GROW ( n n n n n -- )   \ count offo capo cnto wc
   {: count:n offo:n capo:n cnto:n wc:n :}
   count 0 < if E-LAYOUT-BUFFER throw then
   data-base cnto + @ 0= if E-LAYOUT-UNBOUND throw then    \ grow requires a prior BIND
   count wc * {: need:n :}
   need CELL-MAX > if E-LAYOUT-CEIL throw then              \ transactional: die before any mutation
   count  data-base capo + @  > if                          \ count > capacity: copy-on-grow
      data-base cnto + @ {: live:n :}                       \ live cells to carry to the fresh region
      data-base  data-base offo + @  + {: obase:ptr :}      \ current region base
      data-base capo + @ 2 *  count max {: dbl:n :}          \ grow-to-at-least: doubling floor
      dbl wc * CELL-MAX > if count else dbl then {: newcap:n :}   \ clamp so doubling never trips the ceiling
      here {: nbase:ptr :}
      newcap wc * cells allot
      nbase newcap wc * cells ZERO                          \ fresh region zeroed (new cells read 0)
      obase nbase  live wc * cells  COPY                    \ carry the live cells forward
      nbase data-base -  offo data-base +  !                 \ off-cell = new region offset
      newcap  capo data-base +  !                            \ cap-cell = new capacity
   else count  data-base cnto + @  > if                      \ fits capacity but exposes new slots
      data-base  data-base offo + @  +                       \ region base
      data-base cnto + @ wc * cells +                        \ + old-live * width cells
      count  data-base cnto + @  -  wc * cells  ZERO          \ zero [old-live, count)
   then then
   count  cnto data-base +  ! ;                              \ cnt-cell = new live bound

private

\ Generate the deferred accessor plus its NAME-BIND and NAME-GROW into one source.
\ The accessor is the FIRST definition so EVAL's one-shot armed window
\ authorizes it by name; NAME-BIND and NAME-GROW are ordinary checked words (each
\ pushes the four baked literals and calls LAYOUT-BUF:BIND / LAYOUT-BUF:GROW by
\ the qualified name their package publishes).
: DEFER-SRC ( ptr u8 n ptr u8 n n n n -- ptr u8 n ptr u8 n )
   {: name:ptr nameu:n type:ptr typeu:n offo:n capo:n cnto:n :}
   CLEAR
   s" : " APP
   name nameu PUT-NAME {: pna:ptr pnu:n :}
   s"  ( n -- ptr " APP  type typeu APP
   s"  ) {: i:n :} i 0 < if " APP
   E-LAYOUT-BOUNDS PUT-DEC
   s"  throw then data-base " APP  cnto PUT-DEC
   s"  + @ {: c:n :} c 0= if " APP
   E-LAYOUT-UNBOUND PUT-DEC
   s"  throw then i c >= if " APP
   E-LAYOUT-BOUNDS PUT-DEC
   s"  throw then data-base data-base " APP  offo PUT-DEC
   s"  + @ + i " APP  CELLW @ cells PUT-DEC
   s"  * + ; : " APP
   name nameu APP  s" -BIND ( n -- ) " APP
   offo PUT-DEC  s"  " APP  capo PUT-DEC  s"  " APP
   cnto PUT-DEC  s"  " APP  CELLW @ PUT-DEC
   s"  LAYOUT-BUF:BIND ; : " APP
   name nameu APP  s" -GROW ( n -- ) " APP
   offo PUT-DEC  s"  " APP  capo PUT-DEC  s"  " APP
   cnto PUT-DEC  s"  " APP  CELLW @ PUT-DEC
   s"  LAYOUT-BUF:GROW ;" APP
   GEN GEN-U @ pna pnu ;

\ ---- TYPED-VARIABLE / TYPED-BUFFER convenience definers ----------------------
\ Same generative machinery as LAYOUT-BUFFER (name guard, allocation, zero image,
\ generated-accessor evaluation under the armed window, transactional rollback),
\ gated by the broader CHECKER-STORAGE-INFO admissibility. TYPED-BUFFER reuses
\ ACCESSOR-SRC (the indexed `( n -- ptr type )` accessor); TYPED-VARIABLE emits a
\ single-cell `( -- ptr type )` accessor. Both parse a `ptr* base` stored type so
\ closed typed pointers (`ptr TARGET`, `ptr res<n,n>`) are expressible.

variable TOK-A
variable TOK-U
variable TOK-START

: PTR-TOK? ( ptr u8 n -- bool )   \ token is the pointer constructor `ptr`
   s" ptr" CORE-STR= ;

: QUOT-OPEN? ( ptr u8 n -- bool )   \ token is the quotation opener `[`
   s" [" CORE-STR= ;

: QUOT-CLOSE? ( ptr u8 n -- bool )   \ token is the quotation closer `]`
   s" ]" CORE-STR= ;

\ Consume a spaced `[ in -- out ]` xt<effect> quotation type token by token, up
\ to and including the closer, so the returned span is the whole quotation.
: PARSE-QUOT ( -- )
   begin TOK-A @ TOK-U @ QUOT-CLOSE? 0= while
      parse-name TOK-U !  TOK-A !
      TOK-U @ 0= if E-LAYOUT-BUFFER throw then
   repeat ;

: PARSE-TYPE ( -- ptr u8 n )   \ capture a `ptr* base` or `[ in -- out ]` stored-type source span
   parse-name TOK-U !  TOK-A !
   TOK-U @ 0= if E-LAYOUT-BUFFER throw then
   TOK-A @ TOK-START !
   begin TOK-A @ TOK-U @ PTR-TOK? while
      parse-name TOK-U !  TOK-A !
      TOK-U @ 0= if E-LAYOUT-BUFFER throw then
   repeat
   TOK-A @ TOK-U @ QUOT-OPEN? if PARSE-QUOT then
   TOK-START @  TOK-A @ TOK-U @ + TOK-START @ - ;

: VALIDATE-STORAGE ( n ptr u8 n -- ) {: count:n type:ptr typeu:n :}
   type typeu CHECKER-STORAGE-INFO 0= if drop E-LAYOUT-BUFFER throw then
   CELLW !
   count ROWS !
   count CELLW @ EXTENT? 0= if drop E-LAYOUT-BUFFER throw then
   SIZE ! ;

: VAR-SRC ( ptr u8 n ptr u8 n n -- ptr u8 n ptr u8 n )
   {: name:ptr nameu:n type:ptr typeu:n off:n :}
   CLEAR
   s" : " APP
   name nameu PUT-NAME {: pna:ptr pnu:n :}
   s"  ( -- ptr " APP  type typeu APP
   s"  ) data-base " APP
   off PUT-DEC
   s"  + ;" APP
   GEN GEN-U @ pna pnu ;

public

\ The four declaration runners. Each carries every guard its global keyword had,
\ because the qualified name is reachable wherever the bare keyword is (the
\ lesson enum-decl.f records: a runner that drops a guard the bare token enforced
\ loses the reject).
: RUN ( n -- ) {: count:n :}
   parse-name {: name:ptr nameu:n :}
   parse-name {: type:ptr typeu:n :}
   nameu 0= if E-LAYOUT-BUFFER throw then
   TDECL-EVAL-ARMED @ 0= if E-LAYOUT-BUFFER throw then
   name nameu NAME-GUARD
   count type typeu VALIDATE-LAYOUT
   here {: base:ptr :}
   base data-base - {: off:n :}
   name nameu type typeu off ACCESSOR-SRC {: src:ptr srcu:n pna:ptr pnu:n :}
   SIZE @ allot
   base SIZE @ ZERO
   src srcu pna pnu EVAL
   dup 0 <> if ROLLBACK then
   drop ;

: RUN-DEFER ( -- )
   parse-name {: name:ptr nameu:n :}
   parse-name {: type:ptr typeu:n :}
   nameu 0= if E-LAYOUT-BUFFER throw then
   TDECL-EVAL-ARMED @ 0= if E-LAYOUT-BUFFER throw then
   name nameu NAME-GUARD
   CLEAR  name nameu APP  s" -BIND" APP                    \ guard the published NAME-BIND too
   GEN GEN-U @ NAME-GUARD
   CLEAR  name nameu APP  s" -GROW" APP                    \ guard the published NAME-GROW too
   GEN GEN-U @ NAME-GUARD
   type typeu CHECKER-LAYOUT-INFO 0= if 2drop E-LAYOUT-BUFFER throw then
   CELLW !  drop                                           \ width (cells) from the layout family
   here {: cbase:ptr :}
   3 cells allot                                           \ off-cell, cap-cell, cnt-cell
   cbase 3 cells ZERO                                      \ all 0 = unbound
   cbase data-base - {: offo:n :}
   offo CELL + {: capo:n :}
   offo 2 CELL * + {: cnto:n :}
   name nameu type typeu offo capo cnto DEFER-SRC {: src:ptr srcu:n pna:ptr pnu:n :}
   3 cells SIZE !                                          \ rollback rewinds the control cells
   src srcu pna pnu EVAL
   dup 0 <> if ROLLBACK then
   drop ;

: RUN-BUF ( n -- ) {: count:n :}
   parse-name {: name:ptr nameu:n :}
   PARSE-TYPE {: type:ptr typeu:n :}
   nameu 0= if E-LAYOUT-BUFFER throw then
   TDECL-EVAL-ARMED @ 0= if E-LAYOUT-BUFFER throw then
   name nameu NAME-GUARD
   count type typeu VALIDATE-STORAGE
   here {: base:ptr :}
   base data-base - {: off:n :}
   name nameu type typeu off ACCESSOR-SRC {: src:ptr srcu:n pna:ptr pnu:n :}
   SIZE @ allot
   base SIZE @ ZERO
   src srcu pna pnu EVAL
   dup 0 <> if ROLLBACK then
   drop ;

: RUN-VAR ( -- )
   parse-name {: name:ptr nameu:n :}
   PARSE-TYPE {: type:ptr typeu:n :}
   nameu 0= if E-LAYOUT-BUFFER throw then
   TDECL-EVAL-ARMED @ 0= if E-LAYOUT-BUFFER throw then
   name nameu NAME-GUARD
   1 type typeu VALIDATE-STORAGE
   here {: base:ptr :}
   base data-base - {: off:n :}
   name nameu type typeu off VAR-SRC {: src:ptr srcu:n pna:ptr pnu:n :}
   SIZE @ allot
   base SIZE @ ZERO
   src srcu pna pnu EVAL
   dup 0 <> if ROLLBACK then
   drop ;

;package

\ The four global declaration keywords. A user writes them bare at top level, so
\ they cannot be qualified names; each is the sole global entry to its runner,
\ exactly as `: STRUCTURE ( -- ) STRUCTURE-DECL:SD-RUN ;` and
\ `: ENUM ( -- ) ENUM-DECL:ED-RUN ;` are. The axioms keep them checker-known so
\ the seal-time internal-word marking pass (src/core/internal-mark.f) leaves them
\ executable at top level (dot habu-hb-crash-bare-c5be6634). UNSAFE-TOK? rejects
\ these four tokens inside checked bodies (they evaluate generated accessor
\ source), so the axioms add no checked-code capability.
\ LAYOUT-BUFFER count name type
: LAYOUT-BUFFER ( n -- ) LAYOUT-BUF:RUN ;
\ DEFER-LAYOUT-BUFFER name type
: DEFER-LAYOUT-BUFFER ( -- ) LAYOUT-BUF:RUN-DEFER ;
\ TYPED-BUFFER count name type
: TYPED-BUFFER ( n -- ) LAYOUT-BUF:RUN-BUF ;
\ TYPED-VARIABLE name type
: TYPED-VARIABLE ( -- ) LAYOUT-BUF:RUN-VAR ;

PRIM: LAYOUT-BUFFER PE-N PE-IN PRIM;
PRIM: DEFER-LAYOUT-BUFFER PRIM;
PRIM: TYPED-BUFFER PE-N PE-IN PRIM;
PRIM: TYPED-VARIABLE PRIM;

\ The generated NAME-BIND and NAME-GROW words are user-level (minted at model
\ load) and call the binders by their qualified names, so the package-aware axiom
\ registers the effect the checker needs and keeps them past the seal-time
\ internal-word pass. Neither is a source-evaluating opener, so neither is
\ UNSAFE-TOK? (raw-memory surface, like allot/!).
\ Effect for both: ( count offo capo cnto wc -- ).
PPRIM: LAYOUT-BUF BIND PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN PPRIM;
PPRIM: LAYOUT-BUF GROW PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN PE-N PE-IN PPRIM;
