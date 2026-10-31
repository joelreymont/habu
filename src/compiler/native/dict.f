\ dict.f - what the running engine's dictionary says a spelling denotes, and
\ what the chain may do with that record. The binding is the engine's own
\ `scope-find` (habu2.f LSCOPEREC). A `create`d or `constant` word's value is in
\ no record slot and its body may not be decoded, so the word is entered, and
\ only while its flags still carry its definer's stamp.

require src/lib/prelude.f
require src/lib/errors.f
require src/habu/layout.f
require src/habu/terminal-call.f
require src/compiler/native/checker-owner.f
require src/compiler/native/host.f
require src/compiler/workspace.f

package NDICT

public

\ ---- the scope every question below is asked in ------------------------------
\ Published because WHICH scope is open is a fact a caller may need to hold on
\ to. A zero private cell is the engine's own test for no package open.
: OPEN-PRI ( -- n )
   data-base PKG-PRI-CELL + @ ;

: OPEN-PUB ( -- n )
   data-base PKG-PUB-CELL + @ ;

private

\ ---- which record a spelling denotes -----------------------------------------
\ The raw indexed record stays inside this protected package. Visibility is
\ checked after namespace resolution. xref-search-wl's row is NDICT's own
\ (src/habu/prims.f).
: WL-RECORD ( ptr u8 n n -- ptr n ) xref-search-wl ;

1 constant SCOPE-SEEDED              \ LSCOPEREC flag: the bound record is a seeded primitive

\ Public search hides internal words; the compiler admits them only while a
\ TRUSTED: definition is being compiled, and a call to an owned one always
\ (CALL-BINDING). Re-read that authority on every call.
: VISIBLE-RECORD? ( ptr n -- bool )
   {: rec:ptr :}
   rec XREF-FLAGS DNAME-INT and 0<> if
      data-base TRUSTED-CELL + @ 0<> exit
   then
   rec XREF-WORDLIST OWNER-API-PRI-WID <>
   rec XREF-START 0<> and ;

public

\ A tick refuses an internal record outside a TRUSTED: body, and in every body a
\ scope entry, as outer.f SCOPE-ENTRY-GUARD and habu2.f C-SCOPE-ENTRY-GUARD do.
: TICK-REFUSED? ( ptr n -- bool )
   {: rec:ptr :}
   rec XREF-FLAGS DNAME-INT and 0<>  rec VISIBLE-RECORD? 0= and if
      true exit
   then
   rec XREF-START scope-kind? 0<> ;

\ The bound record whatever its flags, or none. Two used publics that both
\ export the tail, with nothing earlier binding it, are the ambiguity the engine
\ refuses.
: SCOPE-REC ( ptr u8 n -- ptr n )
   scope-find {: rec:ptr used:ptr used2:ptr flags:n :}
   rec XREF-FOUND? 0= flags SCOPE-FIND-AMBIGUOUS and 0<> and if E-USING-AMBIGUOUS throw then
   rec ;

\ Whether a bare reference to the spelling is a using refusal, so that it binds
\ no word: two used publics export the tail and nothing earlier binds it, or a
\ used public also exports the tail a global binds (E-USING-SHADOW-GLOBAL, or
\ E-USING-AMBIGUOUS when a second used public exports it too). The engine's
\ interpreters (habu2.f INTERP-EMIT:FIND-SHADOW, outer.f LOOKUP-GO) and the
\ checker's binder (checker.f LIVE-BIND) refuse the same two shapes.
: USING-REFUSED? ( ptr u8 n -- bool )
   scope-find {: rec:ptr used:ptr used2:ptr flags:n :}
   rec XREF-FOUND? 0= if flags SCOPE-FIND-AMBIGUOUS and 0<> exit then
   rec XREF-WORDLIST 0=  used XREF-FOUND?  and ;

\ Whether an operator row may stand for the spelling: the lookup binds the
\ seeded primitive of that spelling, or nothing at all (a keyword such as `>r`
\ has no record). A package word, an EXPORT alias and a redefinition after
\ `undefine` each bind a record of their own, and the call path reaches it.
: SPELL-PRIM? ( ptr u8 n -- bool )
   scope-find {: rec:ptr used:ptr used2:ptr flags:n :}
   flags SCOPE-FIND-AMBIGUOUS and 0<> if false exit then
   rec XREF-FOUND? 0= if true exit then
   flags SCOPE-SEEDED and 0<> ;

private

\ The record a compilation may name other than by a call: tick, enter or read.
\ One it may not is refused where it binds, so the spelling is never handed on
\ to a later scope. An internal record needs the TRUSTED: cell here even when
\ owned: only a call binds an owned primitive (CALL-BINDING).
: SPELL-REC ( ptr u8 n -- ptr n )
   SCOPE-REC {: rec:ptr :}
   rec XREF-FOUND? 0= if rec exit then
   rec VISIBLE-RECORD? 0= if XREF-NULL exit then
   rec ;

\ ---- entering the word the spelling denoted ----------------------------------
\ FIXED-VALUE admits only a live constant/create/variable record before entering
\ it. The definer stamp supplies the single-cell callable layout; the value's
\ number/address interpretation remains the caller's recorded kind.
CAST: AS-FIXED ( n -- [ -- n ] )
: RUN-WORD ( n -- n )
   AS-FIXED execute ;

\ A cell and not a local: a local bound AFTER the entered word ran would itself
\ read what that word left, which is the failure this check exists to refuse.
0 CWORK-VARIABLE FX-BASE n

public

: SPELL-START ( ptr u8 n -- n )
   {: a u:n :}
   a u SPELL-REC dup XREF-FOUND? 0= if drop 0 exit then
   XREF-START ;

\ ---- which definer made the record a spelling denotes -------------------------
\ A `create`d or `variable` address moves with the DATA region on a snapshot, so
\ it may not be compiled as an ordinary number. The stamp is the definer's own.
0 constant FIXED-NONE                \ nothing a mention of this name folds to
NCOMP-DISPATCH:FIXED-VAL-KIND constant FIXED-VAL \ `constant`: the body pushes a decided number
NCOMP-DISPATCH:FIXED-ADDR-KIND constant FIXED-ADDR \ `create`/`variable`: the body pushes a DATA address

\ ---- one walk answers both the record and its start --------------------------
\ The engine returned the record and its start together. Readers retain that
\ record only for this operation; later source may mutate its binding or slot.
private

\ Which definer stamped a record, retired or not: `undefine` removes a name,
\ while the record's storage and the code compiled against it stay.
: STAMP-FIXED ( ptr n -- n )
   {: rec:ptr :}
   rec XREF-FOUND? 0= if FIXED-NONE exit then
   rec XREF-START 0= if FIXED-NONE exit then
   rec XREF-FLAGS DKIND:MASK and {: kind:n :}
   kind DKIND:VAL = if FIXED-VAL exit then
   kind DKIND:ADDR = if FIXED-ADDR exit then
   FIXED-NONE ;

\ A caller already holding the record asks here instead of SPELL-FIXED.
: REC-FIXED ( ptr n -- n )
   {: rec:ptr :}
   rec XREF-FOUND? 0= if FIXED-NONE exit then
   rec XREF-RETIRED? if FIXED-NONE exit then
   rec STAMP-FIXED ;

public

: SPELL-FIXED ( ptr u8 n -- n )
   {: a u:n :}
   a u SPELL-REC REC-FIXED ;

private

: FIXED-ENTRY ( ptr n -- n )
   {: rec:ptr :}
   rec XREF-FOUND? 0= if E-NDICT-NAME throw then
   rec XREF-START {: start:n :}
   start 0= if E-NDICT-NAME throw then
   rec REC-FIXED FIXED-NONE = if E-NDICT-KIND throw then
   start ;

: READ-FIXED ( n -- n )
   {: entry:n :}
   depth FX-BASE !
   entry RUN-WORD
   depth FX-BASE @ 1+ <> if E-NDICT-VALUE throw then ;

public

\ A record no definer stamped is refused BEFORE the word is entered. The count
\ settles the arity but not the TYPE (habu-guard-an-executed-8a0f2f77). A
\ caller already holding the record, as a dictionary walk does, asks REC-VALUE.
: REC-VALUE ( ptr n -- n )
   {: rec:ptr :}
   rec FIXED-ENTRY drop
   rec XREF-START READ-FIXED ;

\ Called once by the checker at generated-word publication; later dependency
\ checks read the stored fact and never enter the word again.
: RETAIN-FIXED ( ptr n -- n n bool ) {: rec:ptr :}
   rec REC-FIXED {: kind:n :}
   kind FIXED-NONE = if 0 0 true exit then
   kind rec FIXED-ENTRY READ-FIXED true ;

: FIXED-VALUE ( ptr u8 n -- n ) SPELL-REC REC-VALUE ;

\ A fixed DATA literal keeps the exact declaration that produced its address.
\ -1 means the fixed value is a scalar and has no DATA owner.
: FIXED-OWNER ( ptr u8 n -- n )
   SPELL-REC {: rec:ptr :}
   rec REC-FIXED FIXED-ADDR <> if -1 exit then
   rec XREF-A>U8 NULL-PTR BYTE-VIEW - dbase@ - DREC / ;

\ Enter the exact fixed implementation the scan selected. The caller checked
\ its captured definer stamp; no spelling is resolved again here.
: BOUND-VALUE ( n n -- n ) {: entry:n kind:n :}
   entry 0< entry 0= or if E-NDICT-NAME throw then
   kind FIXED-VAL <> kind FIXED-ADDR <> and if E-NDICT-KIND throw then
   entry READ-FIXED ;

\ ---- and how many cells a call to it moves --------------------------------
\ CELLS and not terms: `ptr u8 n` is two terms and two cells, while one term of
\ a three-cell family is one term and three cells. The checker publishes cells.
: EFF-CELLS ( ptr u8 n -- n n )
   CHECKER-OWNER:QUERY if CHECKER-OWNER:DIN-CELLS CHECKER-OWNER:DOUT-CELLS else -1 -1 then ;

: EFF-COUNTS ( -- n n n n )        \ din terms, din cells, dout terms, dout cells
   CHECKER-OWNER:DIN-N CHECKER-OWNER:DIN-CELLS CHECKER-OWNER:DOUT-N CHECKER-OWNER:DOUT-CELLS ;

public
0 constant GLUE-NONE                 \ every cell of the row is a value of its own

\ ---- which cells of a row may not be separated from one another --------------
\ Bit i says the i-th cell from the BOTTOM of the row and the one below it are
\ cells of ONE value, so bit 0 is always clear and all-bits is an impossible row.
-1 constant GLUE-UNKNOWN
private

64 constant GLUE-MAX                 \ cells of one row a mask can describe

\ Shifting by the word size is reduced modulo it on this machine, so a run as
\ long as the word would come back as a run of ONE.
: GLUE-RUN-MASK ( n -- n ) {: cells:n :}
   cells 0 <= if GLUE-NONE exit then
   cells GLUE-MAX >= if -1 exit then
   1 cells lshift 1 - ;

\ Every cell above the bottom one is glued to the cell below it.
: GLUE-WHOLE ( n -- n )
   GLUE-RUN-MASK  1 invert and ;

\ Four rows carry per-cell value boundaries, a definition's two and a `does>`
\ clause's two; each is read by its own owner operation and walked by one rule.
0 constant RS-DIN
1 constant RS-DOUT
2 constant RS-DOES-IN
3 constant RS-DOES-OUT

: RG-SLOT ( n n -- n ) {: i:n src:n :}
   src RS-DIN = if i CHECKER-OWNER:DIN-SLOT exit then
   src RS-DOUT = if i CHECKER-OWNER:DOUT-SLOT exit then
   src RS-DOES-IN = if i CHECKER-OWNER:DOES-IN-SLOT exit then
   src RS-DOES-OUT = if i CHECKER-OWNER:DOES-OUT-SLOT exit then
   E-NCOMP-OWNER throw ;

\ The width of the value a term belongs to is readable from the top of each run:
\ the cells of one value carry slots W, W-1 ... 1 downwards. A run of one is not
\ a bundle, and a run reaching past the row's end answers not-known.
0 CWORK-VARIABLE RG-MASK n
0 CWORK-VARIABLE RG-I n
0 CWORK-VARIABLE RG-S n
0 CWORK-VARIABLE RG-BAD n

: RG-BIT ( n -- ) {: bit:n :}
   RG-MASK @  1 bit lshift or  RG-MASK ! ;

\ Term `top` is cell cells-1-top; a bit is about a cell and the one BELOW it, so
\ the inner boundaries are the upper width-1 cells and the bottom cell carries none.
: RG-RUN ( n n n -- ) {: cells:n top:n width:n :}
   width 1 -  0 ?do  cells 1 - top i + -  RG-BIT  loop ;

\ The row's term count and its cell count are the same number here.
: RG-STEP ( n n -- ) {: cells:n src:n :}
   RG-I @ src RG-SLOT RG-S !
   RG-S @ 2 < if
      RG-I @ 1 + RG-I !
      exit
   then
   RG-I @ RG-S @ + cells > if
      1 RG-BAD !
      cells RG-I !
      exit
   then
   cells RG-I @ RG-S @ RG-RUN
   RG-I @ RG-S @ + RG-I ! ;

: ROW-GLUE ( n n n -- n ) {: terms:n cells:n src:n :}
   cells 0 < if GLUE-UNKNOWN exit then
   cells 0 = if GLUE-NONE exit then
   cells GLUE-MAX > if GLUE-UNKNOWN exit then
   terms cells <> if
      terms 1 = if cells GLUE-WHOLE exit then
      GLUE-UNKNOWN exit
   then
   GLUE-NONE RG-MASK !
   0 RG-I !
   0 RG-BAD !
   begin RG-I @ terms < while
      cells src RG-STEP
   repeat
   RG-BAD @ 0<> if GLUE-UNKNOWN exit then
   RG-MASK @ ;

\ The entry a bound record offers a compiled reference and its definer kind, or
\ 0 0. A RETIRED record's start is code nothing can reach. An IMMEDIATE word
\ runs at compile time, so a CALL may not branch to it; a code-address literal
\ (a tick) may still name it without running it (CALLABLE false).
: REC-CODE ( ptr n bool -- n n )
   {: rec:ptr callable:bool :}
   rec XREF-FOUND? 0= if 0 0 exit then
   rec XREF-START {: start:n :}
   start 0= if 0 0 exit then
   rec XREF-RETIRED? if 0 0 exit then
   rec XREF-FLAGS {: f:n :}
   callable f DNAME-IMM and 0<> and if 0 0 exit then
   start f DKIND:MASK and ;

: REC-BINDING ( ptr n -- n n )
   true REC-CODE ;

public

\ Whether a compiled call may bind an internal record (DNAME-INT) with flag cell
\ f: an owned one (DNAME-OWNED) always, since the checker decides that call as
\ tier 0's C-COMPILE-CALL-GUARD does (src/habu/habu2.f); any other only while
\ the TRUSTED: compilation cell is armed. Asked by CALL-BINDING and by
\ src/compiler/native/hir-word.f RESOLVE-SITE.
: INT-CALL? ( n -- bool )
   DNAME-OWNED and 0<> if true exit then
   data-base TRUSTED-CELL + @ 0<> ;

\ Where a compiled CALL may branch to, or zero when it may not branch there at
\ all: an internal record as INT-CALL? admits it, any other when it is visible.
: CALL-BINDING ( ptr u8 n -- n n )
   SCOPE-REC {: rec:ptr :}
   rec XREF-FOUND? 0= if 0 0 exit then
   rec XREF-FLAGS {: f:n :}
   f DNAME-INT and 0<> if f INT-CALL? else rec VISIBLE-RECORD? then
   0= if 0 0 exit then
   rec REC-BINDING ;

\ A caller that needs definer semantics captures them with the same resolved
\ entry. Most call sites only need the target address.
: CALL-TARGET ( ptr u8 n -- n ) CALL-BINDING drop ;

\ Where a compiled `[']` may take its xt from, or zero. An xt runs wherever it
\ is handed, so a tick binds no record TICK-REFUSED? refuses, owned or not.
: TICK-TARGET ( ptr u8 n -- n )
   SPELL-REC {: rec:ptr :}
   rec XREF-FOUND? if rec TICK-REFUSED? if 0 exit then then
   rec false REC-CODE drop ;

\ A helper (src/habu/primitive-registry.f HELPER-REGISTER) sits in
\ OWNER-API-PRI-WID, which no spelling search enters, so it is looked up there
\ by exact name. Only an entry inside the engine's text answers
\ (TERMINAL-CALL:IN-ENGINE-TEXT?).
: HELPER-TARGET ( ptr u8 n -- n )
   OWNER-API-PRI-WID WL-RECORD {: rec:ptr :}
   rec XREF-FOUND? 0= if 0 exit then
   rec XREF-START {: start:n :}
   start TERMINAL-CALL:IN-ENGINE-TEXT? 0= if 0 exit then
   start ;

-1 constant ARITY-NONE

\ Absent is tested as "not a count", not as a particular number: a width is a
\ count of cells and never negative, whichever vocabulary spelled the absence.
: SPELL-ARITY ( ptr u8 n -- n n )
   EFF-CELLS {: din:n dout:n :}
   din 0 < if ARITY-NONE ARITY-NONE exit then
   dout 0 < if ARITY-NONE ARITY-NONE exit then
   din dout ;

\ GLUE-NONE for a name the checker holds no effect for - the same answer as
\ "every cell is its own value", and SPELL-ARITY refuses such a name first.
: SPELL-GLUE ( ptr u8 n -- n n )
   CHECKER-OWNER:QUERY 0= if GLUE-NONE GLUE-NONE exit then
   EFF-COUNTS {: dn:n dc:n on:n oc:n :}
   dn dc RS-DIN ROW-GLUE
   on oc RS-DOUT ROW-GLUE ;

\ The clause has no name until the created word exists, so its rows come from
\ the owner's does> group. The elaborator refuses GLUE-UNKNOWN at the definer.
: DOES-GLUE ( -- n n )
   CHECKER-OWNER:DOES-IN-N CHECKER-OWNER:DOES-IN RS-DOES-IN ROW-GLUE
   CHECKER-OWNER:DOES-OUT-N CHECKER-OWNER:DOES-OUT RS-DOES-OUT ROW-GLUE ;

\ ---- the callable facts a call site needs, from ONE resolution ---------------
\ One effect query, read both ways: the query resolves the spelling to a checker
\ symbol and latches its rows, so separate readers would resolve one name twice.
\ ARITY-NONE in the first answer refuses as SPELL-ARITY does and voids the rest.
\ No return-stack answer: no declaration moves a return-stack cell
\ (docs/forth.md "The return-stack clause"). Result glue only; a definition's
\ own frame uses SPELL-GLUE. Deadness stays apart: CTL-DEAD? reads the
\ control-flag table, a second query wherever it is asked.
: SPELL-CALL ( ptr u8 n -- n n n )  \ din cells, dout cells, result glue
   CHECKER-OWNER:QUERY 0= if ARITY-NONE ARITY-NONE GLUE-NONE exit then
   CHECKER-OWNER:DIN-CELLS {: din:n :}
   CHECKER-OWNER:DOUT-CELLS {: dout:n :}
   din 0 < dout 0 < or if ARITY-NONE ARITY-NONE GLUE-NONE exit then
   EFF-COUNTS {: dn:n dc:n on:n oc:n :}
   din dout  on oc RS-DOUT ROW-GLUE ;

\ ---- the quotation a term of one of those rows IS ----------------------------
public
-1 constant QUOT-NONE                \ that term is no quotation this chain may compile
private

\ Read while the latch is down, with the latch put back before either is
\ answered; declining and answering leave it in the same place.
: QUOT-CELLS ( -- n n )
   CHECKER-OWNER:QUOT-SIMPLE? {: simple:bool :}
   EFF-COUNTS {: dn:n dc:n on:n oc:n :}
   CHECKER-OWNER:QUOT-UP 0= if QUOT-NONE QUOT-NONE exit then
   simple 0= if QUOT-NONE QUOT-NONE exit then
   dc 0 < oc 0 < or if QUOT-NONE QUOT-NONE exit then
   dn dc <> on oc <> or if QUOT-NONE QUOT-NONE exit then
   dc oc ;

\ Terms are counted from the TOP and cells from the BOTTOM, so term i and cell i
\ are the same index only while every term of the row is one cell wide.
: ROW-INDEXABLE? ( bool -- bool )
   {: din:bool :}
   EFF-COUNTS {: dn:n dc:n on:n oc:n :}
   din if dn dc = dc 0 >= and exit then
   on oc = oc 0 >= and ;

public

\ QUOT-NONE twice when there is no such term, it is not a quotation, the row is
\ not indexable, or the body is not one a caller may branch to and come back from.
: SPELL-QUOT-DIN ( ptr u8 n n -- n n )
   {: a u:n i:n :}
   a u CHECKER-OWNER:QUERY 0= if QUOT-NONE QUOT-NONE exit then
   true ROW-INDEXABLE? 0= if QUOT-NONE QUOT-NONE exit then
   i CHECKER-OWNER:DIN-QUOT 0= if QUOT-NONE QUOT-NONE exit then
   QUOT-CELLS ;

: SPELL-QUOT-DOUT ( ptr u8 n n -- n n )
   {: a u:n i:n :}
   a u CHECKER-OWNER:QUERY 0= if QUOT-NONE QUOT-NONE exit then
   false ROW-INDEXABLE? 0= if QUOT-NONE QUOT-NONE exit then
   i CHECKER-OWNER:DOUT-QUOT 0= if QUOT-NONE QUOT-NONE exit then
   QUOT-CELLS ;

\ ---- and the window a catch site takes ---------------------------------------
public
-1 constant CATCH-NONE               \ no catch this chain may compile stands on that token

\ The catch window is certified per SITE, so it is keyed by the TAPE TOKEN the
\ catch stands on rather than by a name. A body that never returns has no
\ output row, which is what the absent second answer says.
: CATCH-CELLS ( n -- n n )
   CHECKER-OWNER:CATCH-CELLS {: in:n out:n :}
   in 0 < if CATCH-NONE CATCH-NONE exit then
   out 0 < if in CATCH-NONE exit then
   in out ;

: EXEC-CELLS ( n -- n n )
   CHECKER-OWNER:EXEC-CELLS ;


: FINALLY-CELLS ( n -- n n n )
   CHECKER-OWNER:FINALLY-CELLS ;

: CALL-CELLS ( n -- n n ) CHECKER-OWNER:CALL-CELLS ;
: INIT-LAYOUT ( n -- n n n ) CHECKER-OWNER:INIT-LAYOUT ;
: FIELD-SPAN ( n -- n n ) CHECKER-OWNER:FIELD-SPAN ;
: C2-STOW ( n -- n n ) CHECKER-OWNER:C2-STOW ;
: CALL-GLUE ( n -- n n ) CHECKER-OWNER:CALL-GLUE ;
: MATCH-PAYLOAD ( n -- n n ) CHECKER-OWNER:MATCH-PAYLOAD ;
: CALL-QUOT-IN ( n n -- n n ) CHECKER-OWNER:CALL-QUOT-IN ;
: CALL-QUOT-OUT ( n n -- n n ) CHECKER-OWNER:CALL-QUOT-OUT ;

\ ---- and how many cells a layout token really moves ---------------------------
-1 constant MATCH-NONE               \ no dispatch cell count was proved for that token

: MATCH-CELLS ( n -- n )
   CHECKER-OWNER:MATCH-CELLS {: w:n :}
   w 0 < if MATCH-NONE exit then
   w ;

\ ---- and how many cells a construction's instantiation ADDS -------------------
\ Absent is ZERO here rather than a refusal: this reader is asked about every
\ call, so absence is the ordinary case and means "adds nothing".
: CON-PADS ( n -- n )
   CHECKER-OWNER:MATCH-CELLS {: w:n :}
   w 0 < if 0 exit then
   w ;

\ ---- and the literal inputs a call takes on top of its checked site inputs ---
\ Both native tiers push them, deepest first, just before the call.
: CALL-EXTRAS ( n -- n ) {: ix:n :}
   ix INIT-LAYOUT 2drop 0 >= if 3 exit then
   ix FIELD-SPAN drop 0 >= if 2 exit then
   ix CON-PADS ;

: CALL-EXTRA ( n n -- n ) {: ix:n k:n :}
   ix INIT-LAYOUT {: width:n bytes:n align:n :}
   width 0 >= if
      k 0= if width exit then
      k 1 = if bytes exit then
      align exit
   then
   ix FIELD-SPAN {: off:n extent:n :}
   off 0 >= if
      k 0= if off exit then
      extent exit
   then
   0 ;

\ ---- and how many cells one memory access moves ------------------------------
\ Keyed by the token's OFFSET into the checked text, the same key the engine's
\ own pass 2 reads. Absent is one cell, which is the checker's own answer.
: OPERAND-CELLS ( n n -- n )
   CHECKER-OWNER:WIDTH-AT ;

: MEM-CELLS ( n -- n )
   0 OPERAND-CELLS ;

\ False for a name the checker holds no control flag for, which is every
\ ordinary word; SPELL-ARITY refuses an uncertified name before this is reached.
: SPELL-DEAD? ( ptr u8 n -- bool )
   CHECKER-OWNER:DEAD-TOKEN? ;

private

\ ---- and which cell a deferred word dispatches through ------------------------
\ The trailer is recognised by DEFER-MAGIC and never by its shape, because an
\ ordinary integer may hold any value.
CAST: >TRAILER-CELL ( n -- ptr n )
: TRAILER@ ( n -- n ) >TRAILER-CELL @ ;

public

\ A deferred word's code record carries the trailer's exact location, retired
\ or not: retirement leaves the cell and the code that dispatches through it.
\ A data word cannot qualify by putting the magic in its body, and a row with
\ no code bytes, a package row live or retired, has no trailer.
: TRAILER-CELL ( ptr n -- n )
   {: rec:ptr :}
   rec XREF-FOUND? 0= if 0 exit then
   rec XREF-START 0= if 0 exit then
   rec XREF-CODE-BYTES 0= if 0 exit then
   rec XREF-FLAGS DKIND:MASK and 0<> if 0 exit then
   rec XREF-START  rec XREF-LEN +  {: meta:n :}
   meta TRAILER@ DEFER-MAGIC <> if 0 exit then
   meta 8 + TRAILER@ ;

\ The selected live record's dispatch cell: a retired one no spelling names.
: REC-DEFER-CELL ( ptr n -- n )
   {: rec:ptr :}
   rec XREF-FOUND? 0= if 0 exit then
   rec XREF-RETIRED? if 0 exit then
   rec TRAILER-CELL ;

\ The DATA origin a record's definer stamped, retired or not: the address a
\ `create`d or `variable` body pushes, or a deferred word's dispatch cell.
\ A dependency walk reads storage by ordinal, and code compiled before an
\ `undefine` still reaches it.
: STAMP-ORIGIN ( ptr n -- n bool )
   {: rec:ptr :}
   rec STAMP-FIXED FIXED-ADDR = if rec XREF-START READ-FIXED true exit then
   rec TRAILER-CELL dup 0<> ;

\ Resolve once: the address and its owner must describe the same declaration.
: SPELL-DEFER-BINDING ( ptr u8 n -- n n )
   SPELL-REC {: rec:ptr :}
   rec REC-DEFER-CELL {: cell:n :}
   cell 0= if 0 -1 exit then
   cell rec XREF-A>U8 NULL-PTR BYTE-VIEW - dbase@ - DREC / ;

: SPELL-DEFER-CELL ( ptr u8 n -- n )
   SPELL-DEFER-BINDING drop ;

\ ---- and the engine word a does> body leaves through --------------------------
\ The engine's own, whatever the compiled source's package scope. Its tick is
\ trust-only: `['] does-patch` in a checked body is E-CAP-TRUSTED.
TRUSTED: DOES-PATCH-ENTRY ( -- n ) ['] does-patch ;

private

CAST: RECORD-FIXED-USE-XT ( n -- [ [ ptr n -- n n bool ] -- ] )

public

\ The source load and a restored partial compiler bind the same reader into
\ the retained checker, whose dispatch cell lies below the capture window.
: RETAIN-FIXED-INSTALL ( -- )
   s" CHECKER-REG:RECORD-FIXED-USE" XREF-FIND
   dup XREF-FOUND? 0= if drop exit then
   XREF-START RECORD-FIXED-USE-XT
   ['] RETAIN-FIXED swap execute ;
RETAIN-FIXED-INSTALL

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
