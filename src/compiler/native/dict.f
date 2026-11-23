split-str
\ dict.f - what the running engine's dictionary says a spelling denotes, and
\ what the chain may do with that record. The binding is the one `scope-find`
\ answers. A `create`d or `constant` word's value is in no record slot and its
\ body may not be decoded, so the word is entered, and only while its flags
\ still carry its definer's stamp.

require src/lib/prelude.f
require src/lib/error.f
require src/habu/layout.f
require src/habu/wl.f
require src/compiler/fixed-body.f
require src/core/checker.f
require src/compiler/native/checker-owner.f
require src/compiler/workspace.f

package NDICT
EXPORT BOOT:data-base

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

1 constant SCOPE-SEEDED              \ LSCOPEREC flag: the bound record is a seeded primitive

: VISIBLE-RECORD? ( ^n -- bool )
   {: rec:^ :}
   rec XREF-WORDLIST OWNER-API-PRI-WID <>
   rec XREF-START 0<> and ;

public

\ A tick refuses a scope entry in every body, as interpreter.f
\ SCOPE-ENTRY-GUARD does.
: TICK-REFUSED? ( ^n -- bool )
   {: rec:^ :}
   rec XREF-START CHECK:SCOPE-CODE-KIND 0<> ;

\ The bound record whatever its flags, or none. Two used publics that both
\ export the tail, with nothing earlier binding it, are the ambiguity the engine
\ refuses.
: SCOPE-REC ( str -- ^n )
   scope-find {: rec:^ used:^ used2:^ flags:n :}
   rec XREF-FOUND? 0= flags SCOPE-FIND-AMBIGUOUS and 0<> and if E-USING-AMBIGUOUS throw then
   rec ;

\ Whether a bare reference to the spelling is a using refusal, so that it binds
\ no word: two used publics export the tail and nothing earlier binds it, or a
\ used public also exports the tail a global binds (E-USING-SHADOW-GLOBAL, or
\ E-USING-AMBIGUOUS when a second used public exports it too). The engine's
\ lookup and the checker's binder refuse the same two shapes.
: USING-REFUSED? ( str -- bool )
   scope-find {: rec:^ used:^ used2:^ flags:n :}
   rec XREF-FOUND? 0= if flags SCOPE-FIND-AMBIGUOUS and 0<> exit then
   rec XREF-WORDLIST WL:GLOBAL?  used XREF-FOUND?  and ;

\ Whether an operator row may stand for the spelling: the lookup binds the
\ seeded primitive of that spelling, or nothing at all (a keyword such as `>r`
\ has no record). A package word, an EXPORT alias and a redefinition after
\ `undefine` each bind a record of their own, and the call path reaches it.
: SPELL-PRIM? ( str -- bool )
   scope-find {: rec:^ used:^ used2:^ flags:n :}
   flags SCOPE-FIND-AMBIGUOUS and 0<> if false exit then
   rec XREF-FOUND? 0= if true exit then
   flags SCOPE-SEEDED and 0<> ;

private

\ The record a compilation may name other than by a call: tick, enter or read.
\ One it may not is refused where it binds, so the spelling is never handed on
\ to a later scope.
: SPELL-REC ( str -- ^n )
   SCOPE-REC {: rec:^ :}
   rec XREF-FOUND? 0= if rec exit then
   rec VISIBLE-RECORD? 0= if XREF-NULL exit then
   rec ;

public

: SPELL-START ( ^u8 n -- n )
   {: a u:n :}
   a u SPELL-REC dup XREF-FOUND? 0= if drop 0 exit then
   XREF-START ;

\ ---- which definer made the record a spelling denotes -------------------------
\ The stamp is the definer's own: COMP-BODY:VALUE for `constant`,
\ COMP-BODY:ADDRESS for `create`/`variable`, and COMP-BODY:COMPILED when a
\ mention of the name folds to nothing.
private

\ Which definer stamped a record, retired or not: `undefine` removes a name,
\ while the record's storage and the code compiled against it stay.
: STAMP-FIXED ( ^n -- COMP:body )
   {: rec:^ :}
   rec XREF-FOUND? 0= if COMP-BODY:COMPILED exit then
   rec XREF-START 0= if COMP-BODY:COMPILED exit then
   rec XREF-FLAGS DKIND:MASK and {: kind:n :}
   kind DKIND:VAL = if COMP-BODY:VALUE exit then
   kind DKIND:ADDR = if COMP-BODY:ADDRESS exit then
   COMP-BODY:COMPILED ;

\ A caller already holding the record asks here instead of SPELL-FIXED.
: REC-FIXED ( ^n -- COMP:body )
   {: rec:^ :}
   rec XREF-FOUND? 0= if COMP-BODY:COMPILED exit then
   rec XREF-RETIRED? if COMP-BODY:COMPILED exit then
   rec STAMP-FIXED ;

public

: SPELL-FIXED ( str -- COMP:body )
   {: a u:n :}
   a u SPELL-REC REC-FIXED ;

\ ---- one walk answers both the record and its start --------------------------
\ The engine returned the record and its start together. Readers retain that
\ record only for this operation; later source may mutate its binding or slot.
private
EXPORT CHECKER-RESOLVE:CK-ENTRY-VALUE

: FIXED-ENTRY ( ^n -- n )
   {: rec:^ :}
   rec XREF-FOUND? 0= if E-NDICT-NAME throw then
   rec XREF-START {: start:n :}
   start 0= if E-NDICT-NAME throw then
   rec REC-FIXED COMP-BODY:COMPILED COMP-BODY:EQ if E-NDICT-KIND throw then
   start ;

public

\ A record no definer stamped is rejected before the word is entered: the
\ depth count settles the arity, never the type.
: REC-VALUE ( ^n -- n )
   {: rec:^ :}
   rec FIXED-ENTRY drop
   rec XREF-START CK-ENTRY-VALUE ;

: FIXED-VALUE ( ^u8 n -- n ) SPELL-REC REC-VALUE ;

\ A fixed DATA literal keeps the exact declaration that produced its address.
\ -1 means the fixed value is a scalar and has no DATA owner.
: FIXED-OWNER ( str -- n )
   SPELL-REC {: rec:^ :}
   rec REC-FIXED COMP-BODY:ADDRESS COMP-BODY:EQ 0= if -1 exit then
   rec XREF-A>U8 NULL-PTR BYTE-VIEW - dbase@ - DREC / ;

\ Enter the exact fixed implementation the scan selected. The caller checked
\ its captured definer stamp; no spelling is resolved again here.
: BOUND-VALUE ( n COMP:body -- n ) {: entry:n kind:COMP:body :}
   entry 0< entry 0= or if E-NDICT-NAME throw then
   kind MATCH COMP:body
      compiled OF E-NDICT-KIND throw ENDOF
      value OF ENDOF
      address OF ENDOF
      deferred OF E-NDICT-KIND throw ENDOF
      cast OF E-NDICT-KIND throw ENDOF
      offset OF E-NDICT-KIND throw ENDOF
      index OF E-NDICT-KIND throw ENDOF
      routine OF E-NDICT-KIND throw ENDOF
      getter OF E-NDICT-KIND throw ENDOF
      setter OF E-NDICT-KIND throw ENDOF
      bytes OF E-NDICT-KIND throw ENDOF
   ;MATCH
   entry CK-ENTRY-VALUE ;

private
EXPORT CHECK:EFFECT-DIN-CELLS
EXPORT CHECK:EFFECT-DOUT-CELLS
EXPORT CHECK:EFFECT-FIELD-SPAN
EXPORT CHECK:EFFECT-INIT-LAYOUT
EXPORT CHECK:EFFECT-MATCH-CELLS
EXPORT CHECK:EFFECT-MEMBER-OFFSET
EXPORT CHECK:EFFECT-MEMBER-SPAN
EXPORT CHECK:EFFECT-MEMBER-STRIDE
EXPORT CHECK:EFFECT-QUERY
public
\ ---- and how many cells a call to it moves --------------------------------
\ CELLS and not terms: `^u8 n` is two terms and two cells, while one term of
\ a three-cell family is one term and three cells. The checker publishes cells.
: EFF-CELLS ( str -- n n )
   EFFECT-QUERY if EFFECT-DIN-CELLS EFFECT-DOUT-CELLS else -1 -1 then ;

private
\ The entry a bound record offers a compiled reference and its definer kind, or
\ 0 0. A RETIRED record's start is code nothing can reach. An IMMEDIATE word
\ runs at compile time, so a CALL may not branch to it; a code-address literal
\ (a tick) may still name it without running it (CALLABLE false).
: REC-CODE ( ^n bool -- n n )
   {: rec:^ callable:bool :}
   rec XREF-FOUND? 0= if 0 0 exit then
   rec XREF-START {: start:n :}
   start 0= if 0 0 exit then
   rec XREF-RETIRED? if 0 0 exit then
   rec XREF-FLAGS {: f:n :}
   callable f DNAME-IMM and 0<> and if 0 0 exit then
   start f DKIND:MASK and ;

: REC-BINDING ( ^n -- n n )
   true REC-CODE ;

\ Where a compiled CALL to the bound record may branch, or zero when the record
\ is not visible.
: REC-CALL ( ^n -- n n )
   {: rec:^ :}
   rec XREF-FOUND? 0= if 0 0 exit then
   rec VISIBLE-RECORD? 0= if 0 0 exit then
   rec REC-BINDING ;

\ A `PACKAGE:WORD` spelling's record among the publics of the top-level package
\ BOOT:NAMESPACE-WL holds, whatever its visibility, which REC-CALL checks; any
\ other spelling binds none.
: TOP-REC ( str -- ^n )
   {: a:^ u:n :}
   a u XREF-QUAL-INDEX {: q:n :}
   q 0 < if XREF-NULL exit then
   a q BOOT:NAMESPACE-WL CHECKER-RESOLVE:WL-PROBE {: pkg:^ :}
   pkg XREF-FOUND? 0= if XREF-NULL exit then
   a q 1+ +  u q - 1-  pkg XREF-PKG-PUBLIC CHECKER-RESOLVE:WL-PROBE ;

public

: CALL-BINDING ( str -- n n ) SCOPE-REC REC-CALL ;

\ A caller that needs definer semantics captures them with the same resolved
\ entry. Most call sites only need the target address.
: CALL-TARGET ( str -- n ) CALL-BINDING drop ;

\ CALL-TARGET for a runtime word the compiler itself calls: bound at the top
\ level, so no part or using of the code being compiled can stand in for it.
: TOP-TARGET ( str -- n ) TOP-REC REC-CALL drop ;

\ Where a compiled `[']` may take its xt from, or zero. An xt runs wherever it
\ is handed, so a tick binds no record TICK-REFUSED? refuses, owned or not.
: TICK-TARGET ( str -- n )
   SPELL-REC {: rec:^ :}
   rec XREF-FOUND? if rec TICK-REFUSED? if 0 exit then then
   rec false REC-CODE drop ;

-1 constant ARITY-NONE

\ Absent is tested as "not a count", not as a particular number: a width is a
\ count of cells and never negative, whichever vocabulary spelled the absence.
: SPELL-ARITY ( str -- n n )
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

\ ---- and the number a generic member call adds for its receiver's instance --
\ Each is -1 when the checker did not know the instance: the call runs the word.
: MEMBER-OFFSET ( n -- n ) EFFECT-MEMBER-OFFSET ;
: MEMBER-SPAN ( n -- n n ) EFFECT-MEMBER-SPAN ;
: MEMBER-STRIDE ( n -- n ) EFFECT-MEMBER-STRIDE ;

\ ---- and the literal inputs a call takes on top of its checked site inputs ---
\ Tier 0 pushes them, deepest first, just before the call.
: CALL-EXTRAS ( n -- n ) {: ix:n :}
   ix EFFECT-INIT-LAYOUT 2drop 0 >= if 3 exit then
   ix EFFECT-FIELD-SPAN drop 0 >= if 2 exit then
   ix CON-PADS ;

: CALL-EXTRA ( n n -- n ) {: ix:n k:n :}
   ix EFFECT-INIT-LAYOUT {: width:n bytes:n align:n :}
   width 0 >= if
      k 0= if width exit then
      k 1 = if bytes exit then
      align exit
   then
   ix EFFECT-FIELD-SPAN {: off:n extent:n :}
   off 0 >= if
      k 0= if off exit then
      extent exit
   then
   0 ;

private

\ ---- and which cell a deferred word dispatches through ------------------------
\ The trailer is recognised by DEFER-MAGIC and never by its shape, because an
\ ordinary integer may hold any value.
CAST: >TRAILER-CELL ( n -- ^n )
: TRAILER@ ( n -- n ) >TRAILER-CELL @ ;

public

\ A deferred word's code record carries the trailer's exact location, retired
\ or not: retirement leaves the cell and the code that dispatches through it.
\ A data word cannot qualify by putting the magic in its body, and a row with
\ no code bytes, a package row live or retired, has no trailer.
: TRAILER-CELL ( ^n -- n )
   {: rec:^ :}
   rec XREF-FOUND? 0= if 0 exit then
   rec XREF-START 0= if 0 exit then
   rec XREF-CODE-BYTES 0= if 0 exit then
   rec XREF-FLAGS DKIND:MASK and 0<> if 0 exit then
   rec XREF-START  rec XREF-LEN +  {: meta:n :}
   meta TRAILER@ DEFER-MAGIC <> if 0 exit then
   meta 8 + TRAILER@ ;

\ The selected live record's dispatch cell: a retired one no spelling names.
: REC-DEFER-CELL ( ^n -- n )
   {: rec:^ :}
   rec XREF-FOUND? 0= if 0 exit then
   rec XREF-RETIRED? if 0 exit then
   rec TRAILER-CELL ;

\ ---- and the engine word a does> body leaves through --------------------------
private
CAST: PATCH>N ( [ n ^u8 n -- ] -- n )

public
\ The engine's own, whatever the compiled source's package scope.
: DOES-PATCH-ENTRY ( -- n ) ['] does-patch PATCH>N ;

seal-package

;package
