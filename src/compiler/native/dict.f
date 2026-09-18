\ dict.f - what the running engine's dictionary says a spelling denotes. One
\ concern: turning a name a program wrote into the fact the chain needs about it.
\
\ The lookup order is the engine's own (habu1.f EMIT-FIND): the open package's
\ private wordlist, then its public one, then the global one, then the live used
\ publics, plus a NAME:tail through the namespace record.
\
\ No record slot holds what a `create`d or `constant` word pushes and decoding
\ its body is forbidden, so the only honest answer is to ENTER the word - and
\ only a record whose flags still carry its definer's stamp is ever entered.

require lib/prelude.f
require lib/errors.f
require src/compiler/native/checker-owner.f

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
-2 constant QUAL-BAD

: USE-DEPTH ( -- n )
   data-base USE-DEPTH-CELL + @ ;

: USE-WID ( n -- n )
   cells data-base USE-WIDS-OFF + + @ ;

\ The raw indexed record stays inside this protected package. Visibility is
\ checked after namespace resolution. Retirement: habu-attr-and-remove-2b13e978.
TRUSTED: WL-RECORD ( ptr u8 n n -- ptr n ) xref-search-wl ;


\ Public search hides internal words; the compiler admits them only while a
\ TRUSTED: definition is being compiled. Re-read that authority on every call.
: VISIBLE-RECORD? ( ptr n -- bool )
   {: rec:ptr :}
   rec XREF-FLAGS DNAME-INT and 0<> if
      data-base TRUSTED-CELL + @ 0<> exit
   then
   rec XREF-WORDLIST OWNER-API-PRI-WID <>
   rec XREF-START 0<> and ;


: WL-CANDIDATE ( ptr u8 n n -- ptr n )
   WL-RECORD
   dup XREF-FOUND? 0= if exit then
   dup VISIBLE-RECORD? 0= if drop XREF-NULL then ;

\ Duplicate `using` of one package is one binding. Two distinct records are the
\ same ambiguity the engine refuses before it executes or compiles the token.
: USED-REC ( ptr u8 n -- ptr n )
   {: a:ptr u:n :}
   XREF-NULL
   USE-DEPTH 0 ?do
      a u i USE-WID WL-CANDIDATE
      dup XREF-FOUND? if
         over XREF-FOUND? if
            2dup <> if E-USING-AMBIGUOUS throw then
         then
         nip
      else
         drop
      then
   loop ;

\ ---- the engine's scope order, retaining its indexed record ------------------
: OPEN-REC ( ptr u8 n -- ptr n )
   {: a u:n :}
   OPEN-PRI 0= if XREF-NULL exit then
   a u OPEN-PRI WL-CANDIDATE {: pri:ptr :}
   pri XREF-FOUND? if pri exit then
   a u OPEN-PUB WL-CANDIDATE ;

: BARE-REC ( ptr u8 n -- ptr n )
   {: a u:n :}
   a u OPEN-REC {: open:ptr :}
   open XREF-FOUND? if open exit then
   a u 0 WL-CANDIDATE {: global:ptr :}
   global XREF-FOUND? if global exit then
   a u USED-REC ;


: QUALIFIED-REC ( ptr u8 n n -- ptr n )
   {: a u:n split:n :}
   a split XREF-NAMESPACE-WL WL-RECORD
   dup XREF-FOUND? 0= if exit then
   XREF-START {: wid:n :}
   a split 1+ ZPTR+ u split - 1- wid WL-CANDIDATE ;


: SPELL-REC ( ptr u8 n -- ptr n )
   {: a u:n :}
   a u XREF-QUAL-INDEX {: q:n :}
   q QUAL-BAD = if XREF-NULL exit then
   q 0 >= if a u q QUALIFIED-REC exit then
   a u BARE-REC ;

\ ---- entering the word the spelling denoted ----------------------------------
\ FIXED-VALUE admits only a live constant/create/variable record before entering
\ it. The definer stamp supplies the single-cell callable layout; the value's
\ number/address interpretation remains the caller's recorded kind.
TRUSTED: AS-FIXED ( n -- [ -- n ] ) ;
TRUSTED: RUN-WORD ( n -- n )
   AS-FIXED execute ;

\ A cell and not a local: a local bound AFTER the entered word ran would itself
\ read what that word left, which is the failure this check exists to refuse.
variable FX-BASE

public

: SPELL-START ( ptr u8 n -- n )
   {: a u:n :}
   a u SPELL-REC dup XREF-FOUND? 0= if drop 0 exit then
   XREF-START ;

\ ---- which definer made the record a spelling denotes -------------------------
\ A `create`d or `variable` address moves with the DATA region on a snapshot, so
\ it may not be compiled as an ordinary number. The stamp is the definer's own.
0 constant FIXED-NONE                \ nothing a mention of this name folds to
1 constant FIXED-VAL                 \ `constant`: the body pushes a decided number
2 constant FIXED-ADDR                \ `create`/`variable`: the body pushes a DATA address

\ ---- one walk answers both the record and its start --------------------------
\ The engine returned the record and its start together. Readers retain that
\ record only for this operation; later source may mutate its binding or slot.
private

\ Which definer stamped a record. The spelling-level SPELL-FIXED is this reader
\ plus the walk that finds the record, so a caller already holding one asks here.
: REC-FIXED ( ptr n -- n )
   {: rec:ptr :}
   rec XREF-FOUND? 0= if FIXED-NONE exit then
   rec XREF-START 0= if FIXED-NONE exit then
   rec XREF-RETIRED? if FIXED-NONE exit then
   rec XREF-FLAGS DKIND:MASK and {: kind:n :}
   kind DKIND:VAL = if FIXED-VAL exit then
   kind DKIND:ADDR = if FIXED-ADDR exit then
   FIXED-NONE ;

public

: SPELL-FIXED ( ptr u8 n -- n )
   {: a u:n :}
   a u SPELL-REC REC-FIXED ;

\ A record no definer stamped is refused BEFORE the word is entered. The count
\ settles the arity but not the TYPE (habu-guard-an-executed-8a0f2f77).
: FIXED-VALUE ( ptr u8 n -- n )
   {: a u:n :}
   a u SPELL-REC {: rec:ptr :}
   rec XREF-FOUND? 0= if E-NDICT-NAME throw then
   rec XREF-START {: start:n :}
   start 0= if E-NDICT-NAME throw then
   rec REC-FIXED FIXED-NONE = if E-NDICT-KIND throw then
   depth FX-BASE !
   start RUN-WORD
   depth FX-BASE @ 1+ <> if E-NDICT-VALUE throw then ;

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

\ WHICH ROW a glue question is about. Four rows carry per-cell value boundaries
\ - a definition's two and a `does>` clause's two - and each is read by its own
\ owner operation, so a caller names the row and this file asks the operation.
\ A does> clause's rows are rows like any other: same terms, same cells, same
\ slots, and therefore the same walk below rather than a second rule for them.
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
variable RG-MASK   variable RG-I   variable RG-S   variable RG-BAD

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

public

\ Where a compiled CALL may branch to, or zero when it may not branch there at
\ all. An IMMEDIATE word runs at compile time, and a RETIRED record's start is
\ code nothing can reach. A DNAME-INT call is resolved only while the existing
\ TRUSTED: compilation cell is armed; checked bodies cannot branch to internal
\ engine code even if a trusted-only primitive row supplies its real arity.
: CALL-BINDING ( ptr u8 n -- n n )
   {: a u:n :}
   a u SPELL-REC {: rec:ptr :}
   rec XREF-FOUND? 0= if 0 0 exit then
   rec XREF-START {: start:n :}
   start 0= if 0 0 exit then
   rec XREF-RETIRED? if 0 0 exit then
   rec XREF-FLAGS {: f:n :}
   f DNAME-INT and 0<> if
      data-base TRUSTED-CELL + @ 0= if 0 0 exit then
   then
   f DNAME-IMM and 0<> if 0 0 exit then
   start f DKIND:MASK and ;

\ A caller that needs definer semantics captures them with the same resolved
\ entry. Most call sites only need the target address.
: CALL-TARGET ( ptr u8 n -- n ) CALL-BINDING drop ;

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

\ The same two answers for the `does>` clause the compiler has just had checked.
\ The clause is not a name the effect store can be queried for - it has no name
\ until the created word exists - so its rows come from the owner's does> group
\ instead, and nothing else about the question changes. GLUE-UNKNOWN is the
\ honest answer for a row whose cells the checker could not separate, and the
\ elaborator refuses it at the definer rather than placing a guess.
: DOES-GLUE ( -- n n )
   CHECKER-OWNER:DOES-IN-N CHECKER-OWNER:DOES-IN RS-DOES-IN ROW-GLUE
   CHECKER-OWNER:DOES-OUT-N CHECKER-OWNER:DOES-OUT RS-DOES-OUT ROW-GLUE ;

\ ---- the callable facts a call site needs, from ONE resolution ---------------
\ EVERY READER ABOVE STARTS WITH THE SAME EFFECT-QUERY, and that query is not a
\ field read: it resolves the spelling to a checker symbol - a hash lookup plus,
\ inside an open package, up to two `search-wl` probes the checker's own tables
\ missed - and latches that symbol's rows. A caller deciding whether a name is
\ a call it can build wants the arity, the result glue and the return-stack
\ answer together, and asking for them one at a time resolved one name three
\ times and latched one set of rows three times.
\
\ So this asks once and reads the latch three ways. ARITY-NONE in the first
\ answer is the same refusal SPELL-ARITY gives, so a caller tests that one value
\ and the rest of the row is meaningless when it fires - which is what the
\ separate readers already meant by answering ARITY-NONE, GLUE-NONE and false.
\
\ IT ANSWERS THE RESULT GLUE ONLY. SPELL-GLUE's first answer is the ARGUMENT
\ row's glue, and a call site's concern is what the callee LEAVES; the two
\ consumers of that word that stand at a call site both drop the argument half.
\ The one consumer that wants both (a definition's own frame) keeps SPELL-GLUE.
\
\ DEADNESS IS DELIBERATELY NOT HERE. CTL-DEAD? reads the control-flag table and
\ not the effect record this latches, so it is a second query no matter where it
\ is asked from; folding it in would hide that behind a word that looks like one
\ lookup. What the two share is the name-to-symbol half, and sharing THAT is a
\ checker-side question, not one this file can answer.
: SPELL-CALL ( ptr u8 n -- n n n bool )  \ din cells, dout cells, result glue, returns?
   CHECKER-OWNER:QUERY 0= if ARITY-NONE ARITY-NONE GLUE-NONE false exit then
   CHECKER-OWNER:DIN-CELLS {: din:n :}
   CHECKER-OWNER:DOUT-CELLS {: dout:n :}
   din 0 < dout 0 < or if ARITY-NONE ARITY-NONE GLUE-NONE false exit then
   EFF-COUNTS {: dn:n dc:n on:n oc:n :}
   on oc RS-DOUT ROW-GLUE {: glue:n :}
   din dout glue CHECKER-OWNER:RET-NEUTRAL? ;

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

\ ---- and how many cells one memory access moves ------------------------------
\ Keyed by the token's OFFSET into the checked text, the same key the engine's
\ own pass 2 reads. Absent is one cell, which is the checker's own answer.
: MEM-CELLS ( n -- n )
   0 CHECKER-OWNER:WIDTH-AT ;

\ False for a name the checker holds no control flag for, which is every
\ ordinary word; SPELL-ARITY refuses an uncertified name before this is reached.
: SPELL-DEAD? ( ptr u8 n -- bool )
   CHECKER-OWNER:DEAD-TOKEN? ;

\ ---- and whether a call to it leaves the caller's return stack alone ----------
\ It asks what the ROWS say and not what the signature spells: a word with no
\ `|` clause records two empty rows because the balance check proved it moves none.
: SPELL-RET-NEUTRAL? ( ptr u8 n -- bool )
   CHECKER-OWNER:QUERY 0= if false exit then
   CHECKER-OWNER:RET-NEUTRAL? ;

private

\ ---- and which cell a deferred word dispatches through ------------------------
\ The trailer is recognised by DEFER-MAGIC and never by its shape, because an
\ ordinary integer may hold any value. Retires with habu-typed-xt-storage-ddad4af8.
TRUSTED: TRAILER@ ( n -- n )
   @ ;

public

\ The record carries the code's start and its length, and the trailer begins
\ where the code ends, so there is no second opinion about a word's extent.
: SPELL-DEFER-CELL ( ptr u8 n -- n )
   {: a u:n :}
   a u SPELL-REC {: rec:ptr :}
   rec XREF-FOUND? 0= if 0 exit then
   rec XREF-RETIRED? if 0 exit then
   rec XREF-START  rec XREF-LEN +  {: meta:n :}
   meta TRAILER@ DEFER-MAGIC <> if 0 exit then
   meta 8 + TRAILER@ ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
