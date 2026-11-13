\ dict.f - what the running engine's dictionary says a spelling denotes, and
\ what the chain may do with that record. The binding is the engine's own
\ `scope-find` (habu2.f LSCOPEREC). A `create`d or `constant` word's value is in
\ no record slot and its body may not be decoded, so the word is entered, and
\ only while its flags still carry its definer's stamp.

require src/lib/prelude.f
require src/lib/error.f
require src/habu/layout.f
require src/habu/wl.f
require src/compiler/fixed-body.f
require src/core/checker.f
require src/compiler/native/checker-owner.f
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
\ scope entry, as interpreter.f SCOPE-ENTRY-GUARD does.
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
\ interpreter (interpreter.f LOOKUP-GO) and the checker's binder (checker.f LIVE-BIND) refuse the same two shapes.
: USING-REFUSED? ( ptr u8 n -- bool )
   scope-find {: rec:ptr used:ptr used2:ptr flags:n :}
   rec XREF-FOUND? 0= if flags SCOPE-FIND-AMBIGUOUS and 0<> exit then
   rec XREF-WORDLIST WL:GLOBAL?  used XREF-FOUND?  and ;

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
\ it may not be compiled as an ordinary number. The stamp is the definer's own:
\ COMP-BODY:value for `constant`, COMP-BODY:address for `create`/`variable`,
\ and COMP-BODY:compiled when a mention of the name folds to nothing.
private

\ Which definer stamped a record, retired or not: `undefine` removes a name,
\ while the record's storage and the code compiled against it stay.
: STAMP-FIXED ( ptr n -- COMP:body )
   {: rec:ptr :}
   rec XREF-FOUND? 0= if COMP-BODY:compiled exit then
   rec XREF-START 0= if COMP-BODY:compiled exit then
   rec XREF-FLAGS DKIND:MASK and {: kind:n :}
   kind DKIND:VAL = if COMP-BODY:value exit then
   kind DKIND:ADDR = if COMP-BODY:address exit then
   COMP-BODY:compiled ;

\ A caller already holding the record asks here instead of SPELL-FIXED.
: REC-FIXED ( ptr n -- COMP:body )
   {: rec:ptr :}
   rec XREF-FOUND? 0= if COMP-BODY:compiled exit then
   rec XREF-RETIRED? if COMP-BODY:compiled exit then
   rec STAMP-FIXED ;

public

: SPELL-FIXED ( ptr u8 n -- COMP:body )
   {: a u:n :}
   a u SPELL-REC REC-FIXED ;

\ ---- one walk answers both the record and its start --------------------------
\ The engine returned the record and its start together. Readers retain that
\ record only for this operation; later source may mutate its binding or slot.
private

: FIXED-ENTRY ( ptr n -- n )
   {: rec:ptr :}
   rec XREF-FOUND? 0= if E-NDICT-NAME throw then
   rec XREF-START {: start:n :}
   start 0= if E-NDICT-NAME throw then
   rec REC-FIXED COMP-BODY:compiled COMP-BODY:EQ if E-NDICT-KIND throw then
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

: FIXED-VALUE ( ptr u8 n -- n ) SPELL-REC REC-VALUE ;

\ A fixed DATA literal keeps the exact declaration that produced its address.
\ -1 means the fixed value is a scalar and has no DATA owner.
: FIXED-OWNER ( ptr u8 n -- n )
   SPELL-REC {: rec:ptr :}
   rec REC-FIXED COMP-BODY:address COMP-BODY:EQ 0= if -1 exit then
   rec XREF-A>U8 NULL-PTR BYTE-VIEW - dbase@ - DREC / ;

\ Enter the exact fixed implementation the scan selected. The caller checked
\ its captured definer stamp; no spelling is resolved again here.
: BOUND-VALUE ( n COMP:body -- n ) {: entry:n kind:COMP:body :}
   entry 0< entry 0= or if E-NDICT-NAME throw then
   kind MATCH COMP:body
      compiled OF E-NDICT-KIND throw ENDOF
      value OF ENDOF
      address OF ENDOF
      defer OF E-NDICT-KIND throw ENDOF
      cast OF E-NDICT-KIND throw ENDOF
      offset OF E-NDICT-KIND throw ENDOF
      index OF E-NDICT-KIND throw ENDOF
      routine OF E-NDICT-KIND throw ENDOF
   ;MATCH
   entry READ-FIXED ;

\ ---- and how many cells a call to it moves --------------------------------
\ CELLS and not terms: `ptr u8 n` is two terms and two cells, while one term of
\ a three-cell family is one term and three cells. The checker publishes cells.
: EFF-CELLS ( ptr u8 n -- n n )
   EFFECT-QUERY if EFFECT-DIN-CELLS EFFECT-DOUT-CELLS else -1 -1 then ;

private
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
\ f: an owned one (DNAME-OWNED) always, since the checker decides that call;
\ any other only while the TRUSTED: compilation cell is armed. Asked by REC-CALL.
: INT-CALL? ( n -- bool )
   DNAME-OWNED and 0<> if true exit then
   data-base TRUSTED-CELL + @ 0<> ;

private

\ Where a compiled CALL to the bound record may branch, or zero: an internal
\ record as INT-CALL? admits it, any other when visible.
: REC-CALL ( ptr n -- n n )
   {: rec:ptr :}
   rec XREF-FOUND? 0= if 0 0 exit then
   rec XREF-FLAGS {: f:n :}
   f DNAME-INT and 0<> if f INT-CALL? else rec VISIBLE-RECORD? then
   0= if 0 0 exit then
   rec REC-BINDING ;

\ A `PACKAGE:WORD` spelling's record among the publics of the top-level package
\ BOOT:NAMESPACE-WL holds; any other spelling binds none.
: TOP-REC ( ptr u8 n -- ptr n )
   {: a:ptr u:n :}
   a u XREF-QUAL-INDEX {: q:n :}
   q 0 < if XREF-NULL exit then
   a q BOOT:NAMESPACE-WL WL-RECORD {: pkg:ptr :}
   pkg XREF-FOUND? 0= if XREF-NULL exit then
   a q 1+ +  u q - 1-  pkg XREF-PKG-PUBLIC WL-RECORD ;

public

: CALL-BINDING ( ptr u8 n -- n n ) SCOPE-REC REC-CALL ;

\ A caller that needs definer semantics captures them with the same resolved
\ entry. Most call sites only need the target address.
: CALL-TARGET ( ptr u8 n -- n ) CALL-BINDING drop ;

\ CALL-TARGET for a runtime word the compiler itself calls: bound at the top
\ level, so no part or using of the code being compiled can stand in for it.
: TOP-TARGET ( ptr u8 n -- n ) TOP-REC REC-CALL drop ;

\ Where a compiled `[']` may take its xt from, or zero. An xt runs wherever it
\ is handed, so a tick binds no record TICK-REFUSED? refuses, owned or not.
: TICK-TARGET ( ptr u8 n -- n )
   SPELL-REC {: rec:ptr :}
   rec XREF-FOUND? if rec TICK-REFUSED? if 0 exit then then
   rec false REC-CODE drop ;

-1 constant ARITY-NONE

\ Absent is tested as "not a count", not as a particular number: a width is a
\ count of cells and never negative, whichever vocabulary spelled the absence.
: SPELL-ARITY ( ptr u8 n -- n n )
   EFF-CELLS {: din:n dout:n :}
   din 0 < if ARITY-NONE ARITY-NONE exit then
   dout 0 < if ARITY-NONE ARITY-NONE exit then
   din dout ;

\ ---- and how many cells a layout token really moves ---------------------------
-1 constant MATCH-NONE               \ no dispatch cell count was proved for that token

: MATCH-CELLS ( n -- n )
   EFFECT-MATCH-CELLS {: w:n :}
   w 0 < if MATCH-NONE exit then
   w ;

\ ---- and how many cells a construction's instantiation ADDS -------------------
\ Absent is ZERO here rather than a refusal: this reader is asked about every
\ call, so absence is the ordinary case and means "adds nothing".
: CON-PADS ( n -- n )
   EFFECT-MATCH-CELLS {: w:n :}
   w 0 < if 0 exit then
   w ;

\ ---- and the literal inputs a call takes on top of its checked site inputs ---
\ Tier 0 pushes them, deepest first, just before the call.
: CALL-EXTRAS ( n -- n ) {: ix:n :}
   ix CHECKER-OWNER:INIT-LAYOUT 2drop 0 >= if 3 exit then
   ix CHECKER-OWNER:FIELD-SPAN drop 0 >= if 2 exit then
   ix CON-PADS ;

: CALL-EXTRA ( n n -- n ) {: ix:n k:n :}
   ix CHECKER-OWNER:INIT-LAYOUT {: width:n bytes:n align:n :}
   width 0 >= if
      k 0= if width exit then
      k 1 = if bytes exit then
      align exit
   then
   ix CHECKER-OWNER:FIELD-SPAN {: off:n extent:n :}
   off 0 >= if
      k 0= if off exit then
      extent exit
   then
   0 ;

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
   rec STAMP-FIXED COMP-BODY:address COMP-BODY:EQ if rec XREF-START READ-FIXED true exit then
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

seal-package

;package
