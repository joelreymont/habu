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
E-CHECKER-LAYOUT-BUFFER constant E-LAYOUT-BUFFER   \ the checker's layout refusal, one code
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
   name nameu CHECKER-DEFINED-HERE? if E-DUP-DEFINITION throw then
   name nameu get-current search-wl 0 <> if E-DUP-DEFINITION throw then
   LBUF-TRUE ;

: LBUF-ZERO ( ptr n n -- ) {: base:ptr bytes:n :}
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
\ A DATA address uses the recorded carrier that habu2.f C-DATA-ADDR emits for
\ `create`/`variable` (RELOC-EMIT:MARK-SITE): a three-half shared DATA address,
\ or the full absolute chain on an older host.
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

\ Every name a definer derives from the declared one - the storage word, and
\ NAME-BIND, NAME-GROW, NAME-RESERVE, NAME-RELEASE - is a definition like any
\ other, so it is guarded the way the declared name is: a collision fails loudly
\ here instead of silently rebinding somebody's word. LBUF-NAMES-OK? guards the
\ declared name and its storage word, which every definer publishes.
: LBUF-SUFFIX-OK? ( ptr u8 n ptr u8 n -- bool ) {: name:ptr nameu:n sfx:ptr sfxu:n :}
   LBUF-CLEAR
   name nameu LBUF-APP  sfx sfxu LBUF-APP
   LBUF-GEN LBUF-GEN-U @ LBUF-NAME-OK? ;

: LBUF-NAMES-OK? ( ptr u8 n -- bool ) {: name:ptr nameu:n :}
   name nameu LBUF-NAME-OK? 0= if LBUF-FALSE exit then
   name nameu LBUF-BASE$ LBUF-SUFFIX-OK? ;

: LBUF-BASE, ( ptr u8 n -- ) {: name:ptr nameu:n :}
   name nameu LBUF-APP  LBUF-BASE$ LBUF-APP ;

: LBUF-SOURCE ( ptr u8 n ptr u8 n -- ptr u8 n ptr u8 n )
   {: name:ptr nameu:n type:ptr typeu:n :}
   LBUF-CLEAR
   s" create " LBUF-APP  name nameu LBUF-BASE,
   s"  : " LBUF-APP
   name nameu LBUF-NAME, {: pna:ptr pnu:n :}
   s"  ( n -- ptr " LBUF-APP  type typeu LBUF-APP
   s"  ) " LBUF-APP
   LBUF-N @ LBUF-DEC,
   s"  DYNAMIC-STORAGE:BOUNDS " LBUF-APP
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
   NULL-PTR STGT-A !  0 STGT-U !  NULL-PTR STGT-START ! ;

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

\ Every region this file allots holds cells, so it starts on a cell: LBUF-ALIGN
\ rounds the data pointer up before a definer measures it or a binder allots at
\ it. `create` rounds the same way, so the generated `create` finds an aligned
\ pointer and leaves it in place. The pad bytes are dictionary space nothing
\ names.
TRUSTED: LBUF-ALIGN-PAD ( -- n )  here CELL 1- and CELL swap - CELL 1- and ;

: LBUF-ALIGN ( -- )
   LBUF-ALIGN-PAD allot ;

\ The `create` in the generated source publishes the storage word at the DP the
\ definer measured, and the definer allots and zeroes against that same DP once
\ the accessor has compiled. If anything moved DP in between, the accessor and
\ the storage would name different places, so this refuses rather than allot
\ into the gap. Only the alignment pad is allotted before the eval, so a
\ rejected accessor leaves DP on the cell where it found it and needs no rewind.
: LBUF-ALLOT ( ptr n -- ) {: base:ptr :}
   here base <> if E-LAYOUT-BUFFER throw then
   LBUF-BYTES @ allot
   base LBUF-BYTES @ LBUF-ZERO ;

\ Whole-record stores move ordinary cells. Walk the admitted instantiated term
\ at capture, using the active sum tag so shared payload slots are classified
\ as code only while their active alternative holds a quotation.
TRUSTED: STORAGE-MARK-TERM ( ptr a n -- ) {: base:ptr term:n :}
   term TFAM:TFAM-STORAGE-QUOT? if base @ base xt! exit then
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

: STORAGE-MARK-FIXED ( ptr a ptr u8 n -- ) {: base:ptr type:ptr typeu:n :}
   type typeu CHECKER-STORAGE-TERM 0= if drop E-LAYOUT-BUFFER throw then
   {: term:n :}
   LBUF-N @ 0 ?do
      base i LBUF-W @ * cells + term STORAGE-MARK-TERM
   loop ;

: STORAGE-HAS-QUOT? ( ptr u8 n -- bool )
   CHECKER-STORAGE-TERM 0= if drop E-LAYOUT-BUFFER throw then
   TFAM:TFAM-STORAGE-HAS-QUOT? ;

defer STORAGE-CLEAR-XT ( n n -- )
: STORAGE-CLEAR-MISSING ( n n -- ) 2drop E-LAYOUT-BUFFER throw ;
: STORAGE-CLEAR-DEFAULT ( -- )
   ['] STORAGE-CLEAR-MISSING is STORAGE-CLEAR-XT ;
STORAGE-CLEAR-DEFAULT

TRUSTED: STORAGE-CAPTURE-WALK ( n n n -- ) {: off:n count:n term:n :}
   term T-WIDTH {: width:n :}
   count width LBUF-EXTENT?
   0= if drop E-LAYOUT-BUFFER throw then {: bytes:n :}
   off bytes STORAGE-CLEAR-XT
   data-base BYTE-VIEW off + CELL-VIEW {: base:ptr :}
   count 0 ?do
      base i width * cells + term STORAGE-MARK-TERM
   loop ;

: STORAGE-CAPTURE-INSTALL ( -- )
   [: STORAGE-CAPTURE-WALK ;] is CHECKER-STORAGE-WALK-XT ;
STORAGE-CAPTURE-INSTALL

\ Quotation cells belong to the image even while null, before either compiler
\ stores into them. These allocations are DATA; the term walk declares each
\ code cell for capture. Pointer-to-quotation elements stay pointer cells.
: STORAGE-ALLOT ( n ptr n n ptr u8 n bool -- )
   {: handle:n base:ptr count:n type:ptr typeu:n hasquot:bool :}
   base LBUF-ALLOT
   hasquot if
      base type typeu STORAGE-MARK-FIXED
      handle base count CHECKER-STORAGE-BIND
   then ;

\ ---- the stored type --------------------------------------------------------
\ Every storage definer reads its type here, so a pointer chain, a family
\ application and a spaced quotation or scheme reach the checker whole. The
\ declaration ends with its line: a definer reads its name on its own line and
\ the type's first token on the name's (STORAGE-LINE-TOKEN). With no name there
\ the definer is refused at its own spelling (CHECKER-STORAGE-NAME-REFUSE). With
\ no first token the span is empty, and the checker refuses that by name
\ (CHECKER-STORAGE-TYPE-REFUSE).

\ The engine's input cursor, src/habu/layout.f INP-CELL, spelled here because
\ that file loads after this one. Pointing it back at a token parse-name read
\ leaves that token to the next statement.
$36A0 constant STGT-INP-CELL
: STORAGE-UNREAD ( ptr u8 -- )
   NULL-PTR - data-base STGT-INP-CELL + ! ;

\ The next token, as parse-name reads one, when it stands on the cursor's line:
\ the bytes the read skips hold no line feed (CHECKER-TYPE-SPAN-BREAK?). A token
\ on a later line goes back to the input, and none is read.
: STORAGE-LINE-TOKEN ( -- ptr u8 n )
   data-base STGT-INP-CELL + @ {: at:n :}
   parse-name {: a:ptr u:n :}
   a NULL-PTR - at - {: gap:n :}
   a gap - gap CHECKER-TYPE-SPAN-BREAK? if a STORAGE-UNREAD a 0 exit then
   a u ;

\ Whether the spelling goes on past its last token: not when that token ended
\ it, and not past its line.
: STORAGE-MORE? ( bool -- bool ) {: ended:bool :}
   ended if LBUF-FALSE exit then
   STORAGE-LINE-TOKEN {: a:ptr u:n :}
   u 0= if LBUF-FALSE exit then
   a STGT-A !  u STGT-U !  LBUF-TRUE ;

\ The whole spelling, ended where the checker ends one (CHECKER-TYPE-SPAN-STEP)
\ or with its line.
: STORAGE-PARSE-TYPE ( -- ptr u8 n )   \ capture the stored type's source span
   STORAGE-LINE-TOKEN STGT-U !  STGT-A !
   STGT-A @ STGT-START !
   0 begin STGT-A @ STGT-U @ CHECKER-TYPE-SPAN-STEP STORAGE-MORE? 0= until drop
   STGT-START @  STGT-A @ STGT-U @ + STGT-START @ - ;

: LAYOUT-BUFFER ( n -- ) {: count:n :}
   STORAGE-LINE-TOKEN {: name:ptr nameu:n :}
   nameu 0= if s" LAYOUT-BUFFER" CHECKER-STORAGE-NAME-REFUSE exit then
   STORAGE-PARSE-TYPE {: type:ptr typeu:n :}
   TDECL-EVAL-ARMED @ 0= if E-LAYOUT-BUFFER throw then
   name nameu LBUF-NAMES-OK? 0= if exit then
   count name nameu type typeu LBUF-VALIDATE 0= if exit then
   type typeu STORAGE-HAS-QUOT? {: hasquot:bool :}
   LBUF-ALIGN
   here {: base:ptr :}
   name nameu type typeu LBUF-SOURCE {: src:ptr srcu:n pna:ptr pnu:n :}
   src srcu pna pnu LBUF-EVAL! {: handle:n :}
   handle base LBUF-N @ type typeu hasquot STORAGE-ALLOT ;

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
: LDEFER-BIND ( n ptr n n -- )   \ count cbase wc
   {: count:n cb:ptr wc:n :}
   count 0 < if E-LAYOUT-BUFFER throw then
   count wc * {: need:n :}
   need LDEFER-CELL-MAX > if E-LAYOUT-CEIL throw then    \ transactional: die before any mutation
   count  cb CELL + @  > if                             \ count > capacity: grow-to-largest
      LBUF-ALIGN
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
PRIM: LDEFER-BIND PE-N PE-IN PE-PTR-N PE-IN PE-N PE-IN PRIM;

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
: LDEFER-GROW ( n ptr n n -- )   \ count cbase wc
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
      LBUF-ALIGN
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
PRIM: LDEFER-GROW PE-N PE-IN PE-PTR-N PE-IN PE-N PE-IN PRIM;

\ A relocated quotation column leaves both its old bytes and their code-cell
\ declarations behind. Retire the entire former capacity after the binder has
\ published the new region; a failed bind leaves the old allocation untouched.
: LDEFER-RETIRE ( n n n ptr n -- ) {: oldoff:n oldcap:n wc:n cb:ptr :}
   oldcap 0= oldoff cb @ = or if exit then
   cb oldoff + {: oldbase:ptr :}
   oldbase BYTE-VIEW data-base BYTE-VIEW - oldcap wc * cells STORAGE-CLEAR-XT
   oldbase oldcap wc * cells LBUF-ZERO ;

: LDEFER-BIND-QUOT ( n ptr n n -- ) {: count:n cb:ptr wc:n :}
   cb @ {: oldoff:n :} cb CELL + @ {: oldcap:n :}
   count cb wc LDEFER-BIND
   oldoff oldcap wc cb LDEFER-RETIRE ;

: LDEFER-GROW-QUOT ( n ptr n n -- ) {: count:n cb:ptr wc:n :}
   cb @ {: oldoff:n :} cb CELL + @ {: oldcap:n :}
   count cb wc LDEFER-GROW
   oldoff oldcap wc cb LDEFER-RETIRE ;

PRIM: LDEFER-BIND-QUOT PE-N PE-IN PE-PTR-N PE-IN PE-N PE-IN PRIM;
PRIM: LDEFER-GROW-QUOT PE-N PE-IN PE-PTR-N PE-IN PE-N PE-IN PRIM;

\ Generate the deferred accessor plus its NAME-BIND and NAME-GROW into one source.
\ The `create` comes first so the accessor and both binders can name the control
\ cells; it runs no CHECK, so the accessor is still the first DEFINITION and
\ LBUF-EVAL's one-shot armed window authorizes it by name. NAME-BIND and
\ NAME-GROW are ordinary checked words (each pushes the control-cell address and
\ the width, then calls LDEFER-BIND / LDEFER-GROW).
: LDEFER-SOURCE ( ptr u8 n ptr u8 n bool -- ptr u8 n ptr u8 n )
   {: name:ptr nameu:n type:ptr typeu:n hasquot:bool :}
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
   hasquot if s"  LDEFER-BIND-QUOT ; : " else s"  LDEFER-BIND ; : " then LBUF-APP
   name nameu LBUF-APP  s" -GROW ( n -- ) " LBUF-APP
   name nameu LBUF-BASE,  s"  " LBUF-APP  LBUF-W @ LBUF-DEC,
   hasquot if s"  LDEFER-GROW-QUOT ;" else s"  LDEFER-GROW ;" then LBUF-APP
   LBUF-GEN LBUF-GEN-U @ pna pnu ;

3 constant LDEFER-CTRL-CELLS   \ off-cell, cap-cell, cnt-cell

: DEFER-LAYOUT-BUFFER ( -- )
   STORAGE-LINE-TOKEN {: name:ptr nameu:n :}
   nameu 0= if s" DEFER-LAYOUT-BUFFER" CHECKER-STORAGE-NAME-REFUSE exit then
   STORAGE-PARSE-TYPE {: type:ptr typeu:n :}
   TDECL-EVAL-ARMED @ 0= if E-LAYOUT-BUFFER throw then
   name nameu LBUF-NAMES-OK? 0= if exit then              \ the name and its control-cell word
   name nameu s" -BIND" LBUF-SUFFIX-OK? 0= if exit then   \ guard the published NAME-BIND too
   name nameu s" -GROW" LBUF-SUFFIX-OK? 0= if exit then   \ guard the published NAME-GROW too
   type typeu CHECKER-LAYOUT-INFO 0= if
      2drop name nameu type typeu CHECKER-STORAGE-TYPE-REFUSE exit
   then
   LBUF-W !  drop                                          \ width (cells) from the layout family
   type typeu STORAGE-HAS-QUOT? {: hasquot:bool :}
   LDEFER-CTRL-CELLS cells LBUF-BYTES !                    \ the control cells this definer owns
   LBUF-ALIGN
   here {: cbase:ptr :}
   name nameu type typeu hasquot LDEFER-SOURCE {: src:ptr srcu:n pna:ptr pnu:n :}
   src srcu pna pnu LBUF-EVAL! {: handle:n :}
   cbase LBUF-ALLOT                                        \ all 0 = unbound
   hasquot if handle cbase CHECKER-STORAGE-DEFER then ;

\ Like LAYOUT-BUFFER: the axiom keeps DEFER-LAYOUT-BUFFER checker-known so the
\ seal-time internal-word pass leaves it top-level executable; it parses its own
\ name + type and consumes nothing from the stack (the count arrives at bind).
PRIM: DEFER-LAYOUT-BUFFER PRIM;

\ ---- TYPED-VARIABLE / TYPED-BUFFER convenience definers ----------------------
\ Same generative machinery as LAYOUT-BUFFER (name guard, allocation, zero image,
\ generated-accessor evaluation under the armed window, transactional rollback),
\ gated by the broader CHECKER-STORAGE-INFO admissibility. TYPED-BUFFER reuses
\ LBUF-SOURCE (the indexed `( n -- ptr type )` accessor); TYPED-VARIABLE emits a
\ single-cell `( -- ptr type )` accessor. Both read a multi-token stored type, so
\ closed typed pointers (`ptr TARGET`, `ptr res<n,n>`) and spaced quotations are
\ expressible.

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
   s" create " LBUF-APP  name nameu LBUF-BASE,
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
   type typeu STORAGE-HAS-QUOT? {: hasquot:bool :}
   LBUF-ALIGN
   here {: base:ptr :}
   name nameu type typeu LBUF-SOURCE {: src:ptr srcu:n pna:ptr pnu:n :}
   src srcu pna pnu LBUF-EVAL! {: handle:n :}
   handle base LBUF-N @ type typeu hasquot STORAGE-ALLOT ;

: TYPED-VARIABLE ( -- )
   STORAGE-LINE-TOKEN {: name:ptr nameu:n :}
   nameu 0= if s" TYPED-VARIABLE" CHECKER-STORAGE-NAME-REFUSE exit then
   STORAGE-PARSE-TYPE {: type:ptr typeu:n :}
   TDECL-EVAL-ARMED @ 0= if E-LAYOUT-BUFFER throw then
   name nameu LBUF-NAMES-OK? 0= if exit then
   1 name nameu type typeu STORAGE-VALIDATE 0= if exit then
   type typeu STORAGE-HAS-QUOT? {: hasquot:bool :}
   LBUF-ALIGN
   here {: base:ptr :}
   name nameu type typeu TYPED-VAR-SOURCE {: src:ptr srcu:n pna:ptr pnu:n :}
   src srcu pna pnu LBUF-EVAL! {: handle:n :}
   handle base LBUF-N @ type typeu hasquot STORAGE-ALLOT ;

\ Axioms keep the two definers checker-known so the seal-time internal-word pass
\ leaves them executable at top level (like LAYOUT-BUFFER); UNSAFE-TOK? rejects
\ `typed-buffer`/`typed-variable` inside checked bodies (they evaluate generated
\ accessor source), so the axioms add no checked-code capability.
PRIM: TYPED-BUFFER PE-N PE-IN PRIM;
PRIM: TYPED-VARIABLE PRIM;

\ ---- transient typed buffers -------------------------------------------------
\ The control cells own an OS mapping and its byte capacity. The generated
\ accessor is the same checked storage introduction as TYPED-BUFFER. A reserve
\ preserves old cells and may move the mapping; callers retain indices across it.
\
\ The private third control cell is the live registry handle. RESERVE registers
\ on first allocation; RELEASE unregisters, so an existing declaration remains
\ reusable after capture and restore without replaying this generated source.
\
\ THE CONTROL HEAD IS DECLARED, NOT `create`d (dot habu-refuse-a-ptr-5ad2734e).
\ It holds the mapping's address, and a raw storage cell never holds an address:
\ `NAME#base 0 ptr-field @` fetched a typed pointer out of an undeclared cell,
\ which is exactly the launder the rule refuses, in every accessor this definer
\ has ever generated. `PTR-VARIABLE` declares the head, so the accessor reads the
\ mapping with a plain `@` and DYNAMIC-STORAGE receives `ptr ptr a`, a pointer to
\ that declared cell. The accessor's own pointee is minted from the declared
\ signature the way every generated accessor's is (LBUF-EVAL's armed window), so
\ a nominal or pointer element type reaches it exactly as it does through
\ LAYOUT-BUFFER and TYPED-BUFFER. The definer's other three generative forms need
\ no change at all: their accessors do pointer ARITHMETIC on the storage word and
\ load counts, and never fetch a pointer out of it.
\
\ The head's cell is committed by the generated `PTR-VARIABLE` itself, so the
\ definer allots the two count cells behind it (DBUF-ALLOT). Those counts are
\ numbers, so the accessor reads the capacity through an explicit cell view of
\ the head rather than through its pointer type — the same record layout, the
\ same compiled add-and-load, since both views are type-level only.
\ THE DYNAMIC ELEMENT WIDTH IS BYTES, where LBUF-W holds cells: this definer
\ allots no element storage, it scales the accessor's index and the reserve by
\ the width, and DYNAMIC-STORAGE:RESERVE has always taken its width in bytes.
\ A byte element is therefore expressible here and nowhere else, which is what
\ CHECKER-DYNAMIC-INFO admits over CHECKER-STORAGE-INFO; every element the cell
\ definers admit keeps the generated source it had, since DBUF-W is then
\ exactly LBUF-W cells.
variable DBUF-W

: DBUF-VALIDATE ( ptr u8 n ptr u8 n -- bool ) {: name:ptr nameu:n type:ptr typeu:n :}
   type typeu CHECKER-DYNAMIC-INFO 0= if
      drop name nameu type typeu CHECKER-STORAGE-TYPE-REFUSE LBUF-FALSE exit
   then
   DBUF-W ! LBUF-TRUE ;

: DBUF-SOURCE ( ptr u8 n ptr u8 n -- ptr u8 n ptr u8 n )
   {: name:ptr nameu:n type:ptr typeu:n :}
   LBUF-CLEAR
   s" PTR-VARIABLE " LBUF-APP name nameu LBUF-BASE,
   s"  : " LBUF-APP name nameu LBUF-NAME, {: pna:ptr pnu:n :}
   s"  ( n -- ptr " LBUF-APP type typeu LBUF-APP
   s"  ) " LBUF-APP name nameu LBUF-BASE,
   s"  " LBUF-APP DBUF-W @ LBUF-DEC,
   s"  DYNAMIC-STORAGE:OFFSET " LBUF-APP name nameu LBUF-BASE,
   s"  @ swap + ; : " LBUF-APP name nameu LBUF-APP
   s" -RESERVE ( n -- ) " LBUF-APP name nameu LBUF-BASE,
   s"  " LBUF-APP DBUF-W @ LBUF-DEC,
   s"  DYNAMIC-STORAGE:RESERVE ; : " LBUF-APP name nameu LBUF-APP
   s" -RELEASE ( -- ) " LBUF-APP name nameu LBUF-BASE,
   s"  DYNAMIC-STORAGE:RELEASE ;" LBUF-APP
   LBUF-GEN LBUF-GEN-U @ pna pnu ;

\ LBUF-ALLOT one cell on: the generated `PTR-VARIABLE` publishes the control head
\ AND commits its cell, so the definer allots the two count cells behind it and
\ zeroes the record it measured. The same transactional check — if anything else
\ moved DP between the measurement and here, the accessor and the storage would
\ name different places, so this refuses instead of allotting into the gap. A
\ rejected accessor leaves the head's cell allotted and unnamed, like the
\ alignment pad; the declaration itself fails, so nothing reads it.
: DBUF-ALLOT ( ptr n -- ) {: base:ptr :}
   here base CELL + <> if E-LAYOUT-BUFFER throw then
   LBUF-BYTES @ CELL - allot
   base LBUF-BYTES @ LBUF-ZERO ;

: DYNAMIC-BUFFER ( -- )
   STORAGE-LINE-TOKEN {: name:ptr nameu:n :}
   nameu 0= if s" DYNAMIC-BUFFER" CHECKER-STORAGE-NAME-REFUSE exit then
   STORAGE-PARSE-TYPE {: type:ptr typeu:n :}
   TDECL-EVAL-ARMED @ 0= if E-LAYOUT-BUFFER throw then
   name nameu LBUF-NAMES-OK? 0= if exit then
   name nameu s" -RESERVE" LBUF-SUFFIX-OK? 0= if exit then
   name nameu s" -RELEASE" LBUF-SUFFIX-OK? 0= if exit then
   name nameu type typeu DBUF-VALIDATE 0= if exit then
   3 cells LBUF-BYTES !
   LBUF-ALIGN
   here {: base:ptr :}
   name nameu type typeu DBUF-SOURCE {: src:ptr srcu:n pna:ptr pnu:n :}
   src srcu pna pnu LBUF-EVAL! drop
   base DBUF-ALLOT ;

PRIM: DYNAMIC-BUFFER PRIM;

;using
