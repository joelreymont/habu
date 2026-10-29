\ t0.f - the tier 0 compiler's front end, package T0.
\
\ T0:COMPILE-TEXT walks one definition's body text as BODYBUF holds it at `;`
\ (src/habu/definers.f DEF-COMPILE hands the same text to the compiler entry):
\ the name, an optional `( ... )` signature it skips, then blank-separated
\ tokens. It emits through the seam src/habu/t0-emit.f (package T0-EMIT) to the
\ emitter selected there (src/habu/t0-x64.f for x86-64); the emitter leaves the
\ finished body in its sink, and the caller seals it into NEMIT
\ (T0-EMIT:SEAL or SEAL-UNPLACED).
\
\ T0:COMPILE is the runtime compiler entry: T0:INSTALL stores it in the cell
\ the tier 1 `:` dispatches through (T0:DISPATCH!), with no fallback to tier 1,
\ and stores NDEFINITION's CALLBACK-STUB! and CALLBACK-STUB@ in the cells
\ CALLBACK: (src/lib/ffi-callback.f) arms and reads its C entry through. The
\ definition after a CALLBACK-STUB! seals with NCBSTUB armed, so the emitter
\ appends the stub after its last unit (src/habu/t0-emit.f THE CALLBACK STUB)
\ and CALLBACK-STUB@ answers its placed address; that compile ends the pending
\ stub on every exit, as tier 1's does.
\ It compiles one pending definition through the step every native compiler
\ shares (src/compiler/native/definition.f, NDEFINITION): stage the unit, scan
\ and capture the body inside a recording unit whose observer keeps only the
\ verdict, enforce the verdict, check the name and arity, walk the body, seal
\ the emission at the next code slot and publish it; a refusal is reported and
\ retracts the rows the definition recorded, and a tick operand no record
\ answers is reported as the engine's tick reports it (E-UNDEFINED). The name
\ is the body text's first token. T0 classifies no body for the host yet: each one's reason is
\ UNKNOWN, and a quotation's arity is unknown (-1 -1), as tier 1 answers for
\ one it has none for. A definition while a target shadow is open is
\ E-T0-SHADOW.
\
\ A `does>` DEFINER is one text split at M-DOES, the byte just past its
\ `does> `, which the shared step checks (NDEFINITION:CHECK-DOES-HEAD). The
\ parent is unit 0 up to the `does>` token, where it calls `does-patch` with
\ the clause's address, 0 for an empty clause, and the clause's interned
\ signature, then returns. The clause is the unit after every quotation of
\ the parent text, nested ones included, as tier 1 numbers it, and its own
\ quotations follow it; an empty clause's unit is still built. The clause
\ binds locals of its own, read from its certificate at base M-DOES.
\
\ A TOKEN IS, IN THIS ORDER: a local in scope; a string opener (`s" c" ." s\"
\ c\" .\"`); a control keyword (if else then begin until while repeat again do
\ ?do loop +loop leave unloop i j case of endof endcase MATCH construct exit
\ recurse >r r> r@ ['] [char] char is {: [: ;]); an operation T0 inlines, when
\ the spelling binds the engine's seeded primitive (NDICT:SPELL-PRIM?): the
\ stack shuffles (dup drop swap over nip tuck rot -rot 2dup 2drop 2swap 2over
\ 2>r 2r> 2r@), + - * and or xor 1+ 1- negate invert lshift rshift, cells
\ cell+ ptr-field, / and mod by a literal in 1..T0-EMIT:DIV-LIT-MAX (DIV2), the
\ comparisons, @ and c@, and ! as a call to the primitive or one per cell (THE
\ WIDTHS); a word the dictionary binds (NDICT:CALL-BINDING), as a call, or as
\ the value it pushes when its definer fixed one (a constant's number, a
\ created word's or variable's DATA address; WORD-CALL); an immediate
\ (IMM-TOKEN says which pass); a number (OUTER:NUMBER), a float being one cell
\ of a double's bits. Anything else throws E-T0-UNSUPPORTED, and
\ UNSUPPORTED$ names the token until the next compile.
\
\ THE SPELLING. T0 compiles only bodies the checker certified, so it reads each
\ token as the checker does (src/core/checker.f DO-TOK1). A local answers to
\ its declared spelling byte for byte (LOC-REF?). Every other class is matched
\ with ASCII case folded: the checker folds the token (TOKFOLD) before it
\ compares it with a keyword, a string opener, `[char]` or `char`, and the
\ dictionary lookups behind the inlined operations and the calls fold case
\ themselves; T0 compares with STR=CI. So `IF`, `RECURSE`, `S"` and `DUP` are
\ `if`, `recurse`, `s"` and the primitive `dup`.
\
\ THE ORDINALS. The checker files what it proved about a token under that
\ token's ordinal in its report (src/core/checker.f SCAN-REPORT), and TOKEN
\ counts the same way: the name is 0, the signature reports nothing, each other
\ token is one, a string literal with its opener is one, `[char]` or `char`
\ with its operand is one, and a tick's or `is`'s operand is one after its
\ keyword. A quotation's first ordinal is recorded where it is skipped, and the
\ does> clause's follows its `does>`.
\
\ MATCH AND CONSTRUCT. `MATCH fam` reads its value as the cells the checker
\ proved at the family token, the tag on top, and opens a case over the tag.
\ Each arm, `variant of`, compares the tag with the variant's (NFAM:TAG); the
\ match drops the tag and the pads the checker proved at the `of`, leaving the
\ payload. `;MATCH` is the default arm: a tag no arm matched calls the trap
\ routine with the family's bad-tag message (src/compiler/native/trap.f), as
\ tier 1's MATCH-TRAP. The variant tokens, `of` and `;MATCH` are read in place,
\ never as locals or words. `construct fam v` pushes the variant's pads as
\ zeros, then its tag. A call to a word first pushes, as zeros, the pads the
\ checker proved the call's instantiation adds (NDICT:CON-PADS): OPTION:NONE
\ at option<pt>. A form whose facts are missing or disagree, as tier 1's
\ MATCH-SCAN refuses them, is E-T0-MATCH.
\
\ A STRING's payload starts one byte past its opener, as BODYBUF keeps it
\ (src/habu/definers.f DEF-TEXT), and src/habu/escape.f decodes an escaped
\ one. Its bytes are interned (NSTR:INTERN-SOURCE), so equal bodies share an
\ address, and its address row files the source origin interning answers.
\ `s"` pushes the address and length, `c"` the address of a count byte and
\ the bytes, `."` passes both to `type`. A payload with no closing quote or a
\ bad escape, and a counted one past COUNTED-MAX bytes, is E-T0-LITERAL.
\
\ `is W` stores the xt on the stack in deferred word W's cell: it pushes the
\ cell's address, whose row files the origin the dictionary answers with it
\ (NDICT:SPELL-DEFER-BINDING), and calls the engine's `xt!`, as tier 1's
\ (src/compiler/native/elaborate.f DO-IS). A W that is no deferred word, or
\ whose effect the dictionary holds no arity for, is E-T0-DEFER.
\
\ A QUOTATION `[: ... ;]` is a unit of its own in the same emission
\ (src/habu/t0-emit.f THE UNITS). At `[:` the body pushes the unit's address
\ and skips the text through the matching `;]`; once the definition is
\ compiled, each quotation is compiled from its recorded start, in the order
\ the `[:` tokens were met, nested ones after their parent. Each unit starts
\ with an empty virtual stack, no structure open, no loop level and no local,
\ as the checker reads a quotation: the enclosing loop's `i` and the enclosing
\ locals are out of its reach. A `{:` inside a quotation is E-T0-UNSUPPORTED.
\
\ THE VIRTUAL STACK. The top of the data stack lives in at most T0-EMIT:VREGS
\ entries, each a vreg or a constant; below them the stack is in memory. A
\ constant becomes code only when an operation needs it in a register or the
\ stack is spilled. The paths that meet at an `if`'s `else` and `then`, at a
\ `begin` loop's head and exits and at a counted loop's head and exit keep
\ their values in registers: `if` records the layout its skipped path carries,
\ `begin` the loop head's, `while` its exit's and `do` its level's (SNAPSHOT,
\ every entry settled into a vreg of its own), and the paths that join there
\ reconcile to it (RECONCILE): the arms at `else` and `then`, the back edge at
\ `repeat`, `until`, `again`, `loop` and `+loop`, the fallthrough at a `then`
\ that lands a `while` exit, and every `leave`. Every call spills the whole
\ virtual stack to memory, and so does every `case` and MATCH join, so those
\ paths meet with it empty. A push onto a full virtual
\ stack spills its bottom entry. Entries may share a vreg: a copy (`dup`,
\ `over`, a wide move's cell) is a second entry holding the same vreg, and a
\ vreg is free only once no entry holds it (RELEASE). An operation that writes
\ an entry's vreg in place takes it through OWN, which first copies a shared
\ vreg into a fresh one; one that overwrites it whole, reading the old value
\ only as an input, takes it through WRITE-TARGET, which relabels the entry to
\ a fresh vreg with no copy. A comparison writes no operand. Followed by `if`,
\ `while` or `until` it leaves its condition in the flags for the branch;
\ otherwise it pushes its flag into a fresh vreg.
\
\ THE WIDTHS. A stack word moves whole values: the checker measured each
\ operand's cells at the token, and the capture of each split's certificate
\ keeps them by the token's offset in the whole text (NFETCH:OPERAND-CELLS,
\ position 0 the top, from the return row for r> r@ 2r> 2r@), so a does>
\ definer's parent keeps its widths past the clause's check. An unmeasured
\ operand is one cell, as the checker files only layout values.
\ Over one-cell operands dup drop swap over nip tuck rot keep their entry
\ moves; -rot 2dup 2drop 2swap 2over, and those over a wider operand, go
\ through MOVE-WIDE, which copies each output cell on top from the entry or
\ memory cell it comes from, leaving in place the bottom operands the word
\ keeps, and then removes the cells it consumed from under the copies, and
\ 2>r 2r> 2r@ through their swap and >r composition. `>r`
\ and `r>` move every cell of the value one at a time, so a value's cells sit
\ on the return stack in reverse and `r>` restores their order; `r@` of a
\ wider value is `r>` and a copy pushed back. `@` and `!` move the cells the
\ checker certified at the token, as tier 1's WIDE-LOAD and WIDE-STORE
\ (src/compiler/native/elaborate.f): `@` loads from the address plus i cells for
\ each cell, and `!` calls the primitive, the engine's guarded store, once per
\ cell with the address kept under the copies. Before it loads, an `@` the
\ checker filed a layout fact for (NFETCH:AT) calls NFETCH-CHECK:TAGS with a
\ copy of the address and the fact's descriptor, as tier 1's VALIDATE-FETCH;
\ a fact whose width is not the token's is E-T0-BUNDLE. Any other operation
\ T0 inlines over a width other than one cell, and `@` or `!` under one cell,
\ is E-T0-BUNDLE, as tier 1's DO-OP refuses it E-NELAB-BUNDLE.
\
\ THE LOCALS. Each `{: ... :}` group binds its names once, left to right from
\ the deepest cell, into slots of the body's machine-stack frame (T0-EMIT:SLOTS,
\ SLOT-STORE, SLOT-LOAD); a read pushes every cell of the local. A local holds
\ as many cells as the checker certified for its bind (NFETCH:BIND-WIDTH), so
\ the caller captures the body's certificate (NFETCH:CAPTURE at base 0) before
\ COMPILE-TEXT, and T0 never reads a type annotation. A bind past the
\ certificate's count, and a body that binds fewer than it counts, is
\ E-T0-BINDS: that certificate is not of this text; a body that binds nothing
\ reads none, as tier 1's BINDS-CK. A local answers to its declared spelling
\ byte for byte, ahead of the keywords and the dictionary, from its group to
\ the closer of the structure it binds in (else, then, repeat, until, again,
\ loop, +loop, endof, endcase), as the checker's CF-LOC-REST ends it; `while`
\ keeps the loop's names, as tier 1's borrow does. A dead local's slots go to
\ the next group.
\
\ THE CONTROL STACK holds the kind of each open structure, with the locals in
\ scope where its block opened: an `if` or `else`; a `begin` and a `while`,
\ which `while` puts under its `begin` as Forth's orig/dest order has it, so
\ `begin ... while ... while ... repeat then` closes; a `do` level; a `case`,
\ each `endof` above it and the open `of`; a `MATCH`, with its family and the
\ cells the checker proved at the family token, tag on top (CS-FAM, CS-WID),
\ each `endof` above it and the open `OF`. A structure that does not close,
\ and a loop word outside the loops it needs, is E-T0-CONTROL, refused before
\ the emitter is called: T0-EMIT's structured words (src/habu/t0-emit.f
\ CONTROL) keep the branches and trust this nesting.
\
\ LOOPS. `do` and `?do` take the start and limit, push a runtime frame
\ (T0-EMIT LOOP-PUSH: the index's home is memory), record the rest of the
\ virtual stack as the level's layout and open the level (LOOP-OPEN), `?do`
\ with its entry test, which runs in the layout. `loop` and `+loop` reconcile
\ to the layout and branch back to the level's head, `+loop`'s step reconciled
\ into a vreg above it; `leave` reconciles to it and branches out. LOOP-CLOSE
\ lands the `leave` exits and the entry on the pop of the frame that follows,
\ so every way out leaves through that pop in the layout, where the code after
\ the loop continues.

require src/lib/prelude.f
require src/lib/string.f
require src/lib/source-syntax.f
require src/core/bytes.f
require src/habu/outer.f
require src/habu/escape.f
require src/compiler/native/checker-owner.f
require src/compiler/native/dict.f
require src/compiler/native/family.f
require src/compiler/native/trap.f
require src/compiler/native/fetch.f
require src/compiler/native/string.f
require src/compiler/native/publish.f
require src/compiler/native/definition.f
require src/compiler/native/callback-stub.f
require src/habu/t0-emit.f

package T0
public

\ Tier 0 compiler: -9600..-9609, the block src/lib/errors.f names.
-9600 constant E-FIRST
-9609 constant E-LAST
-9600 constant E-T0-UNSUPPORTED  \ a token T0 does not compile yet: UNSUPPORTED$
-9601 constant E-T0-CONTROL      \ a control word without its opener, or a body ending inside one
-9602 constant E-T0-LITERAL      \ a string with no closing quote, a bad escape, or a counted one too long
-9603 constant E-T0-IMMEDIATE    \ an immediate other than a parse-neutral one: UNSUPPORTED$
-9604 constant E-T0-BINDS        \ the body binds other than the certificate's count of locals
-9605 constant E-T0-SHADOW       \ a definition while a target shadow is open: no shadow path yet
-9606 constant E-T0-MATCH        \ a MATCH or construct the checker's facts do not bear out, as tier 1's E-NELAB-MATCH
-9607 constant E-T0-DEFER        \ `is` names no deferred word, or one with no arity
-9608 constant E-T0-BUNDLE       \ an operation over a width it cannot move, as tier 1's E-NELAB-BUNDLE

private

\ ---- the text ----------------------------------------------------------------
PTR-VARIABLE TXT
variable TXT-LEN
variable POS
PTR-VARIABLE BAD
variable BAD-LEN
variable MISSING                     \ nonzero: the refused token is a tick operand no record answers
variable ORD                         \ the checker's ordinal of the token TOKEN reads next
variable CUT                         \ the byte just past `does> `, or 0

: TXT$ ( -- ptr u8 ) TXT @ ;

: BLANK? ( n -- bool ) 32 <= ;

\ The next blank-delimited token, of length zero at the end of the text.
: AT-BLANK? ( -- bool )
   POS @ TXT-LEN @ < if TXT$ POS @ + c@ BLANK? exit then
   false ;

: IN-TOKEN? ( -- bool )
   POS @ TXT-LEN @ < if TXT$ POS @ + c@ BLANK? 0= exit then
   false ;

: TOKEN ( -- ptr u8 n )
   begin AT-BLANK? while POS @ 1+ POS ! repeat
   POS @ {: start:n :}
   begin IN-TOKEN? while POS @ 1+ POS ! repeat
   POS @ start - {: u:n :}
   u 0<> if 1 ORD +! then
   TXT$ start + u ;

\ The next token, read without consuming it: POS and ORD are restored.
: PEEK-TOKEN ( -- ptr u8 n )
   POS @ ORD @ {: saved:n ord:n :}
   TOKEN
   saved POS !  ord ORD ! ;

\ `[char]` and `char` file their operand under their own ordinal, as the
\ checker reports a character literal (src/core/checker.f PARSE-LIT?); a tick's
\ operand takes the next one.
: LIT-OPERAND ( -- ptr u8 n ) TOKEN -1 ORD +! ;

: LIT-KEYWORD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" [char]" STR=CI a u s" char" STR=CI or ;

\ Whether a token takes the next one as its operand: a parsing word, or `is`
\ naming the deferred word it stores into.
: OPERAND? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u SOURCE-SYNTAX:PARSING-KEYWORD?  a u s" is" STR=CI  or ;

\ Whether a token ends a quotation.
: QUOT-END? ( ptr u8 n -- bool ) s" ;]" STR= ;

\ Whether the unit ends here: no token follows, or the quotation's `;]` does.
: AT-END? ( -- bool )
   PEEK-TOKEN {: a:ptr u:n :}
   u 0= a u QUOT-END? or ;

\ Throw code, naming the token for UNSUPPORTED$.
: REFUSE ( ptr u8 n n -- ) {: a:ptr u:n code:n :}
   a BAD !  u BAD-LEN !
   code throw ;

: UNSUPPORTED ( ptr u8 n -- ) E-T0-UNSUPPORTED REFUSE ;

\ ---- the control stack -------------------------------------------------------
0 constant CS-IF
1 constant CS-ELSE
2 constant CS-BEGIN
3 constant CS-WHILE
4 constant CS-DO
5 constant CS-CASE
6 constant CS-OF
7 constant CS-ENDOF
8 constant CS-MATCH

DYNAMIC-BUFFER CS-KIND n
DYNAMIC-BUFFER CS-LOC n              \ locals in scope where the block opened
DYNAMIC-BUFFER CS-FAM n              \ a MATCH's family
DYNAMIC-BUFFER CS-WID n              \ a MATCH's cells, the tag on top
DYNAMIC-BUFFER CS-LAY n              \ layout k's vregs from k*VREGS, bottom first
DYNAMIC-BUFFER CS-LAY-N n            \ layout k's count, -1 unrecorded
variable CS-N
variable DEAD                        \ nonzero: the fallthrough is unreachable

\ Room for layouts 0 .. k.
: LAY-ROOM ( n -- ) {: k:n :}
   k 1+ T0-EMIT:VREGS * CS-LAY-RESERVE  k 1+ CS-LAY-N-RESERVE ;

: LAY@ ( n n -- n ) {: k:n j:n :} k T0-EMIT:VREGS * j + CS-LAY @ ;
: LAY! ( n n n -- ) {: v:n k:n j:n :} v k T0-EMIT:VREGS * j + CS-LAY ! ;

\ The innermost open structure with a recorded layout, or -1.
: STEERING ( -- n )
   CS-N @ 1- begin dup 0 >= if dup CS-LAY-N @ 0< else false then while 1- repeat ;

\ ---- the virtual stack -------------------------------------------------------
0 constant K-REG
1 constant K-CONST

DYNAMIC-BUFFER VS-KIND n            \ reserved to T0-EMIT:VREGS by RESET
DYNAMIC-BUFFER VS-VAL n
variable VS-N
variable FREE                       \ bit k set: no entry holds vreg k

: ALL-FREE ( -- n ) 1 T0-EMIT:VREGS lshift 1- ;

: TOP ( -- n ) VS-N @ 1- ;

: REG? ( n -- bool ) VS-KIND @ K-REG = ;

: VREG@ ( n -- T0-EMIT:vreg ) VS-VAL @ T0-EMIT:>VREG ;

-1 constant NO-ENTRY                \ an entry index HOLDERS skips none for

\ How many entries other than entry x hold vreg v.
: HOLDERS ( n n -- n ) {: v:n x:n :}
   0 VS-N @ 0 ?do i x <> i REG? and i VS-VAL @ v = and if 1+ then loop ;

\ Free a vreg once no entry holds it; one an entry still holds stays allocated.
: RELEASE ( T0-EMIT:vreg -- )
   T0-EMIT:VREG>N {: n:n :}
   n NO-ENTRY HOLDERS 0<> if exit then
   1 n lshift FREE @ or FREE ! ;

: ENTRY! ( n n n -- ) {: kind:n val:n k:n :}
   kind k VS-KIND !  val k VS-VAL ! ;

: COPY-ENTRY ( n n -- ) {: from:n to:n :}
   from VS-KIND @ from VS-VAL @ to ENTRY! ;

\ Drop entry k and close the gap above it, then release its vreg, which stays
\ allocated while another entry holds it.
: REMOVE ( n -- ) {: k:n :}
   k REG? k VS-VAL @ {: reg:bool v:n :}
   VS-N @ k 1+ ?do i i 1- COPY-ENTRY loop
   VS-N @ 1- VS-N !
   reg if v T0-EMIT:>VREG RELEASE then ;

\ Entry k written to the memory cell k above the stack pointer.
: SPILL-ENTRY ( n -- ) {: k:n :}
   k REG? if k VREG@ k T0-EMIT:SPILL exit then
   k VS-VAL @ k T0-EMIT:SPILL-LIT ;

\ Entry 0 written to the stack pointer's cell, which then moves up n cells.
: PUSH-ENTRY ( n -- ) {: n:n :}
   0 REG? if 0 VREG@ n T0-EMIT:PUSH exit then
   0 VS-VAL @ n T0-EMIT:PUSH-LIT ;

\ Spill the bottom entry to memory.
: EVICT ( -- )
   1 PUSH-ENTRY
   0 REMOVE ;

\ Make room for one more entry.
: ROOM ( -- )
   VS-N @ T0-EMIT:VREGS = if EVICT then ;

\ Vreg n, which no entry holds, allocated.
: CLAIM ( n -- T0-EMIT:vreg ) {: n:n :}
   1 n lshift invert FREE @ and FREE !
   n T0-EMIT:>VREG ;

\ The lowest vreg in the nonzero mask m, allocated.
: LOWEST ( n -- T0-EMIT:vreg ) {: m:n :}
   0 begin 1 over lshift m and 0= while 1+ repeat CLAIM ;

\ A vreg no entry or TAKE holds. A caller holding a taken vreg ALLOCs only for
\ an entry with none, at most VREGS-1 entries being left, so a vreg is free;
\ with none taken, entries holding every vreg hold one each, so EVICT frees the
\ bottom one's.
: ALLOC ( -- T0-EMIT:vreg )
   FREE @ 0= if EVICT then FREE @ LOWEST ;

: PUSH-REG ( T0-EMIT:vreg -- ) {: r:T0-EMIT:vreg :}
   K-REG r T0-EMIT:VREG>N VS-N @ ENTRY!
   VS-N @ 1+ VS-N ! ;

: PUSH-CONST ( n -- ) {: v:n :}
   ROOM
   K-CONST v VS-N @ ENTRY!
   VS-N @ 1+ VS-N ! ;

\ Write every entry to memory and empty the virtual stack. Entry 0 goes last,
\ by the push that moves the stack pointer past them all.
: SPILL-ALL ( -- )
   VS-N @ {: n:n :}
   n 1 ?do n i - SPILL-ENTRY loop
   n 0 > if n PUSH-ENTRY then
   0 VS-N !
   ALL-FREE FREE ! ;

\ The vreg layout s names for entry j, counted from the bottom as the JIT's
\ snapshot frame counts its slots, or -1 for none or an entry above it.
: AIM ( n n -- n ) {: s:n j:n :}
   s 0< if -1 exit then
   j s CS-LAY-N @ >= if -1 exit then
   s j LAY@ ;

\ The vregs layout s names for entries 0 .. j-1.
: HELD-BACK ( n n -- n ) {: s:n j:n :}
   0 j 0 ?do s i AIM dup 0< if drop else 1 swap lshift or then loop ;

\ A vreg for entry j, filled from memory, steered by layout s as the arm64
\ JIT's NEED-ALLOC, (src/habu/jit.f) steers a pull: the vreg s names for the
\ entry is taken when free, so a value filled after a call inside a loop or an
\ `if` lands where the back edge or `then` needs it. Otherwise the lowest
\ free vreg is taken that s names for none of entries 0 .. j-1, filled after
\ this one. ENSURE asks for at most VREGS entries, so with the held-back vregs
\ aside one is free for LOWEST and nothing is evicted.
: STEER ( n n -- T0-EMIT:vreg ) {: s:n j:n :}
   s j AIM {: g:n :}
   g 0< if false else 1 g lshift FREE @ and 0<> then if g CLAIM exit then
   s j HELD-BACK invert FREE @ and LOWEST ;

\ Entry k of m loaded from the memory cell k - m below the stack pointer into
\ the vreg layout s steers it to (STEER); entry 0, the last, by the pop that
\ moves the stack pointer down to it.
: FILL-ENTRY ( n n n -- ) {: s:n k:n m:n :}
   s k STEER {: r:T0-EMIT:vreg :}
   k 0= if r m T0-EMIT:POP else r k m - T0-EMIT:FILL then
   K-REG r T0-EMIT:VREG>N k ENTRY! ;

\ At least k entries: the missing ones are loaded from memory under the rest,
\ steered by the innermost layout an open structure recorded.
: ENSURE ( n -- ) {: k:n :}
   VS-N @ k >= if exit then
   k VS-N @ - {: m:n :}
   VS-N @ 0 ?do VS-N @ 1- i - dup m + COPY-ENTRY loop
   STEERING {: s:n :}
   m 0 ?do s m 1- i - m FILL-ENTRY loop
   VS-N @ m + VS-N ! ;

\ Entry k in a vreg. A constant entry means a vreg is free, so k stays put.
: MAT ( n -- T0-EMIT:vreg ) {: k:n :}
   k REG? if k VREG@ exit then
   ALLOC {: r:T0-EMIT:vreg :}
   k VS-VAL @ r T0-EMIT:LIT
   K-REG r T0-EMIT:VREG>N k ENTRY!
   r ;

\ The top entry in a vreg, taken off the virtual stack. The vreg stays allocated
\ until the caller's next SPILL-ALL or RELEASE, and an entry below may still
\ hold it: the caller writes it only once no entry holds it.
: TAKE ( -- T0-EMIT:vreg )
   1 ENSURE
   TOP MAT {: r:T0-EMIT:vreg :}
   VS-N @ 1- VS-N !
   r ;

\ Whether an entry other than k and x holds vreg entry k's vreg.
: SHARED? ( n n -- bool ) {: k:n x:n :} k VS-VAL @ x HOLDERS 1 > ;

\ Entry k relabelled to a fresh vreg, returned. A shared entry means a vreg is
\ free, so ALLOC evicts nothing and k stays put.
: RELABEL ( n -- T0-EMIT:vreg ) {: k:n :}
   ALLOC {: d:T0-EMIT:vreg :}
   K-REG d T0-EMIT:VREG>N k ENTRY!
   d ;

\ Entry k in a vreg no entry other than k and x holds, for an operation that
\ writes it in place (d op= s) and consumes entry x, NO-ENTRY when it consumes
\ none: a vreg shared with a third entry is copied into a fresh one first.
: OWN ( n n -- T0-EMIT:vreg ) {: k:n x:n :}
   k MAT {: r:T0-EMIT:vreg :}
   k x SHARED? 0= if r exit then
   k RELABEL {: d:T0-EMIT:vreg :}
   d r T0-EMIT:MOV
   d ;

\ Entry k's vreg for an operation that overwrites it whole and reads the old
\ value only as an input register: a shared entry gets a fresh vreg, and
\ nothing is copied into it.
: WRITE-TARGET ( n -- T0-EMIT:vreg ) {: k:n :}
   k MAT {: r:T0-EMIT:vreg :}
   k NO-ENTRY SHARED? 0= if r exit then
   k RELABEL ;

\ ---- inline operations -------------------------------------------------------
: CONST? ( n -- bool ) VS-KIND @ K-CONST = ;

: IMM? ( n -- bool ) {: k:n :}
   k CONST? if k VS-VAL @ T0-EMIT:IMM? exit then
   false ;

\ Replace the two top entries with one constant. A local cannot hold a
\ quotation, so the folding words take theirs on the stack, under the rest.
: FOLD2 ( [ n n -- n ] -- )
   TOP 1- VS-VAL @  TOP VS-VAL @  rot execute {: v:n :}
   VS-N @ 2 - VS-N !
   v PUSH-CONST ;

\ Whether the two top entries are constants.
: CONST2? ( -- bool ) TOP 1- CONST? TOP CONST? and ;

\ The second entry, in a vreg no entry but the top one holds, holds the result;
\ the top entry, which may share that vreg (`dup *`), goes.
: INTO-SECOND ( -- T0-EMIT:vreg T0-EMIT:vreg )
   TOP 1- TOP OWN  TOP MAT ;

: ALU2 ( [ n n -- n ] T0-EMIT:alu bool -- ) {: o:T0-EMIT:alu comm:bool :}
   2 ENSURE
   CONST2? if FOLD2 exit then
   drop
   TOP IMM? if
      TOP 1- NO-ENTRY OWN TOP VS-VAL @ o T0-EMIT:OP-LIT
      TOP REMOVE exit
   then
   comm TOP 1- IMM? and if
      TOP NO-ENTRY OWN TOP 1- VS-VAL @ o T0-EMIT:OP-LIT
      TOP 1- REMOVE exit
   then
   INTO-SECOND {: d:T0-EMIT:vreg s:T0-EMIT:vreg :}
   d s o T0-EMIT:OP
   TOP REMOVE ;

: MUL2 ( -- )
   2 ENSURE
   CONST2? if [: * ;] FOLD2 exit then
   TOP IMM? if
      TOP 1- NO-ENTRY OWN TOP VS-VAL @ T0-EMIT:MUL-LIT
      TOP REMOVE exit
   then
   TOP 1- IMM? if
      TOP NO-ENTRY OWN TOP 1- VS-VAL @ T0-EMIT:MUL-LIT
      TOP 1- REMOVE exit
   then
   INTO-SECOND {: d:T0-EMIT:vreg s:T0-EMIT:vreg :}
   d s T0-EMIT:MULX
   TOP REMOVE ;

\ Whether the top entry is a constant divisor DIV-LIT and MOD-LIT take.
: DIVISOR? ( -- bool )
   VS-N @ 0= if false exit then
   TOP CONST? 0= if false exit then
   TOP VS-VAL @ {: v:n :}
   v 1 >= v T0-EMIT:DIV-LIT-MAX <= and ;

\ `/` or `mod` by a literal divisor in 1..DIV-LIT-MAX, folded when the dividend
\ is a constant too and else divided in place by the seam word; whether it
\ claimed the token. Any other divisor is left to the call of the kernel row,
\ which screens a zero divisor and MIN-N -1. The fold runs the engine's own
\ `/` or `mod`, which truncates toward zero as every kernel's rows do.
: DIV2 ( [ n n -- n ] [ T0-EMIT:vreg n -- ] -- bool )
   DIVISOR? 0= if 2drop false exit then
   2 ENSURE
   CONST2? if drop FOLD2 true exit then
   nip TOP 1- NO-ENTRY OWN TOP VS-VAL @ rot execute
   TOP REMOVE true ;

: SHIFT2 ( [ n n -- n ] T0-EMIT:shifter -- ) {: o:T0-EMIT:shifter :}
   2 ENSURE
   CONST2? if FOLD2 exit then
   drop
   TOP CONST? if
      TOP 1- NO-ENTRY OWN TOP VS-VAL @ o T0-EMIT:SHIFT-LIT
      TOP REMOVE exit
   then
   INTO-SECOND {: d:T0-EMIT:vreg s:T0-EMIT:vreg :}
   d s o T0-EMIT:SHIFT
   TOP REMOVE ;

\ A cell is 8 bytes on every T0 target: `cells` shifts left by 3, and
\ `ptr-field` adds the shifted cell index to its base.
3 constant CELL-SHIFT

\ The top entry in cells, as bytes.
: CELLS-OP ( -- ) CELL-SHIFT PUSH-CONST [: lshift ;] T0-EMIT:SH-LEFT SHIFT2 ;

: REMOVE2 ( -- ) TOP REMOVE TOP REMOVE ;

\ Fold the top constant entry through a quotation.
: FOLD1 ( [ n -- n ] -- ) TOP VS-VAL @ swap execute TOP VS-VAL ! ;

: UNARY ( [ n -- n ] [ T0-EMIT:vreg -- ] -- )
   1 ENSURE
   TOP CONST? if drop FOLD1 exit then
   nip TOP NO-ENTRY OWN swap execute ;

: FETCH ( T0-EMIT:width -- ) {: w:T0-EMIT:width :}
   1 ENSURE
   TOP MAT {: a:T0-EMIT:vreg :}
   TOP WRITE-TARGET {: d:T0-EMIT:vreg :}
   d a w T0-EMIT:LOAD ;

\ ---- the stack shuffles ------------------------------------------------------
\ A copy of entry k on top: a constant is copied as itself, a vreg by a
\ second entry holding it.
: COPY ( n -- ) {: k:n :}
   k CONST? if k VS-VAL @ PUSH-CONST exit then
   VS-N @ T0-EMIT:VREGS = if EVICT k 1- else k then {: j:n :}
   j VREG@ PUSH-REG ;

: SWAP-AT ( n n -- ) {: a:n b:n :}
   a VS-KIND @ a VS-VAL @ {: ka:n va:n :}
   b a COPY-ENTRY
   ka va b ENTRY! ;

: DUP1 ( -- ) 1 ENSURE TOP COPY ;
: OVER1 ( -- ) 2 ENSURE TOP 1- COPY ;
: NIP1 ( -- ) 2 ENSURE TOP 1- REMOVE ;
: SWAP1 ( -- ) 2 ENSURE TOP 1- TOP SWAP-AT ;
: ROT1 ( -- ) 3 ENSURE TOP 2 - TOP 1- SWAP-AT TOP 1- TOP SWAP-AT ;
: TUCK1 ( -- ) SWAP1 OVER1 ;


\ A drop of a memory cell moves the stack pointer and loads nothing.
: DROP1 ( -- )
   VS-N @ 0= if -1 T0-EMIT:DSP+ exit then
   TOP REMOVE ;

\ ---- whole-width moves -------------------------------------------------------
DYNAMIC-BUFFER WS n                  \ the operands' cells, position 0 the top
variable WS-K                        \ operands the word moves

\ The cells of the k operands of the stack word at token a.
: WIDTHS ( ptr u8 n -- ) {: a:ptr k:n :}
   a TXT$ - {: at:n :}
   k WS-RESERVE
   k 0 ?do at i NFETCH:OPERAND-CELLS i WS ! loop
   k WS-K ! ;

: WIDE? ( -- bool )
   false WS-K @ 0 ?do i WS @ 1 > or loop ;

\ The depth of operand k's top cell: the cells of the operands above it.
: DEPTH ( n -- n ) 0 swap 0 ?do i WS @ + loop ;

\ The two top operands' widths exchanged, as their values were.
: WS-SWAP ( -- ) 0 WS @ 1 WS @ 0 WS ! 1 WS ! ;

\ A copy of the cell d below the top, pushed: from its entry or from memory.
: DEEP ( n -- ) {: d:n :}
   ROOM
   d VS-N @ < if TOP d - COPY exit then
   ALLOC {: r:T0-EMIT:vreg :}
   r d VS-N @ - 1+ negate T0-EMIT:FILL
   r PUSH-REG ;

\ Push every cell of operand k, with p cells pushed above the operands so far.
: PUT-OP ( n n -- n ) {: p:n k:n :}
   p k DEPTH + k WS @ + 1- {: d:n :}
   k WS @ 0 ?do d DEEP loop
   p k WS @ + ;

\ Remove the c cells under the top o: the entries among them, then the memory
\ cells right under the entries. With part of the top o in memory, the stack is
\ spilled and the o cells are moved down over the c.
: NIP-CELLS ( n n -- ) {: o:n c:n :}
   c 0= if exit then
   o VS-N @ <= if
      VS-N @ o - c min {: v:n :}
      v 0 ?do TOP o - REMOVE loop
      c v > if v c - T0-EMIT:DSP+ then
      exit
   then
   SPILL-ALL
   o 0 ?do
      ALLOC {: r:T0-EMIT:vreg :}
      r i o - T0-EMIT:FILL
      r i o - c - T0-EMIT:SPILL
      r RELEASE
   loop
   c negate T0-EMIT:DSP+ ;

\ How many leading outputs are the bottom operands in their order.
: KEPT ( ptr u8 n -- n ) {: outs:ptr u:n :}
   WS-K @ u min {: m:n :}
   m 0 ?do outs i + c@ $30 -  WS-K @ 1- i -  <> if i unloop exit then loop
   m ;

\ The WS-K operands replaced by outs, the operand positions of the outputs
\ from the bottom as digits: the kept bottom ones stay, the rest are copied.
: MOVE-WIDE ( ptr u8 n -- ) {: outs:ptr u:n :}
   outs u KEPT {: j:n :}
   WS-K @ j - DEPTH {: c:n :}
   0 u j ?do outs i + c@ $30 - PUT-OP loop {: o:n :}
   o c NIP-CELLS ;

\ A shuffle with its entry move over one-cell operands.
: SHUFFLE ( [ -- ] ptr u8 n ptr u8 n -- ) {: a:ptr k:n outs:ptr u:n :}
   a k WIDTHS
   WIDE? if drop outs u MOVE-WIDE exit then
   execute ;

: SWAP-OP ( -- ) WIDE? if s" 01" MOVE-WIDE exit then SWAP1 ;

\ ---- the locals --------------------------------------------------------------
DYNAMIC-BUFFER L-AT n                \ the name's offset in the text
DYNAMIC-BUFFER L-LEN n
DYNAMIC-BUFFER L-SLOT n              \ the first of its frame slots
DYNAMIC-BUFFER L-WIDTH n             \ its cells
variable L-N                         \ locals in scope
variable BINDS                       \ binds so far: the next one's certificate sequence
variable UNIT                        \ the unit being compiled: 0 the definition
variable CLAUSE                      \ a does> definer's clause unit, else -1

: L-NAME$ ( n -- ptr u8 n ) {: k:n :} TXT$ k L-AT @ +  k L-LEN @ ;

\ The latest local in scope spelled exactly so, or -1. A name of another
\ length is passed on its length alone, before its bytes are addressed.
: FIND-LOCAL ( ptr u8 n -- n ) {: a:ptr u:n :}
   L-N @ begin dup 0 > while
      1- dup L-LEN @ u = if dup L-NAME$ a u STR= if exit then then
   repeat
   1- ;

\ The frame slots the locals in scope hold.
: SLOTS-LIVE ( -- n )
   L-N @ 0= if 0 exit then
   L-N @ 1- {: k:n :}
   k L-SLOT @ k L-WIDTH @ + ;

\ Record local k, declared by the token of length u that ends at POS, on slot.
: DECLARE ( ptr u8 n n n -- n ) {: a:ptr u:n k:n slot:n :}
   POS @ u - {: at:n :}
   BINDS @ at NFETCH:BINDS >= if E-T0-BINDS throw then
   at BINDS @ NFETCH:BIND-WIDTH {: w:n :}
   BINDS @ 1+ BINDS !
   a u $3A INDEX-OF MATCH option         \ the name ends at the type's `:`
      none OF u ENDOF
      some OF IDX>N ENDOF
   ;MATCH {: len:n :}
   k 1+ L-AT-RESERVE  k 1+ L-LEN-RESERVE  k 1+ L-SLOT-RESERVE  k 1+ L-WIDTH-RESERVE
   at k L-AT !  len k L-LEN !  slot k L-SLOT !  w k L-WIDTH !
   w ;

\ Pop c cells into slots base .. base+c-1, the top cell into the last: first
\ the virtual stack's entries, then the cells in memory under them, the
\ deepest by the pop that moves the stack pointer down to it.
: BIND-CELLS ( n n -- ) {: base:n c:n :}
   VS-N @ c min {: v:n :}
   v 0 ?do
      TOP MAT  base c + 1- i -  T0-EMIT:SLOT-STORE
      TOP REMOVE
   loop
   c v ?do
      ALLOC {: r:T0-EMIT:vreg :}
      i v - 1+ {: d:n :}
      i c 1- = if r d T0-EMIT:POP else r d negate T0-EMIT:FILL then
      r  base c + 1- i -  T0-EMIT:SLOT-STORE
      r RELEASE
   loop ;

\ Declare the next name of a group whose earlier names hold cells; false at
\ the `:}`.
: GROUP-NAME ( n n n n -- n n bool ) {: first:n base:n names:n cells:n :}
   TOKEN {: a:ptr u:n :}
   u 0= if E-T0-CONTROL throw then
   a u s" :}" STR= if names cells false exit then
   a u first names + base cells + DECLARE {: w:n :}
   names 1+  cells w +  true ;

\ `{:` up to `:}`: the names come into scope once their cells are bound. A
\ quotation's group is not compiled yet; a does> clause's is.
: DO-GROUP ( ptr u8 n -- ) {: a:ptr u:n :}
   UNIT @ 0<> UNIT @ CLAUSE @ <> and if a u UNSUPPORTED then
   SLOTS-LIVE {: base:n :}
   L-N @ {: first:n :}
   0 0 begin first base 2swap GROUP-NAME 0= until {: names:n cells:n :}
   base cells + T0-EMIT:SLOTS
   base cells BIND-CELLS
   first names + L-N ! ;

\ Every cell of local k, pushed.
: DO-LOCAL ( n -- ) {: k:n :}
   k L-WIDTH @ 0 ?do
      ROOM ALLOC {: r:T0-EMIT:vreg :}
      r k L-SLOT @ i + T0-EMIT:SLOT-LOAD
      r PUSH-REG
   loop ;

\ ---- comparisons -------------------------------------------------------------
\ The flag of condition c, from the compare just emitted, pushed in a fresh
\ vreg. The compare's operands are removed, so a vreg is free and ALLOC emits
\ nothing between the compare and SET-FLAG, as T0-EMIT:CMP requires.
: PUSH-FLAG ( T0-EMIT:cc -- ) {: c:T0-EMIT:cc :}
   ALLOC {: d:T0-EMIT:vreg :}
   d c T0-EMIT:SET-FLAG
   d PUSH-REG ;

\ One more than the condition (T0-EMIT:CC>N) of a compare whose flag is still
\ in the condition codes, 0 none. A compare leaves it only when the next token
\ is `if`, `while` or `until`, whose DO-IF, DO-WHILE or DO-UNTIL takes it, so
\ no other word ever finds it set.
variable PENDING

\ Whether the next token is the keyword `if`, `while` or `until`, matched as
\ COMPILE-TOKEN matches it: a local of that name shadows it, then KW2 and KW5
\ compare it case-blind.
: BRANCH-NEXT? ( -- bool )
   PEEK-TOKEN {: a:ptr u:n :}
   a u FIND-LOCAL 0 >= if false exit then
   a u s" if" STR=CI  a u s" while" STR=CI or  a u s" until" STR=CI or ;

\ The flag of condition c from the compare just emitted: left in the condition
\ codes for the branch that follows, else pushed.
: CMP-FLAG ( T0-EMIT:cc -- ) {: c:T0-EMIT:cc :}
   BRANCH-NEXT? if c T0-EMIT:CC>N 1+ PENDING ! exit then
   c PUSH-FLAG ;

: CMP2 ( [ n n -- n ] T0-EMIT:cc -- ) {: c:T0-EMIT:cc :}
   2 ENSURE
   CONST2? if FOLD2 exit then
   drop
   TOP IMM? if
      TOP 1- MAT TOP VS-VAL @ T0-EMIT:CMP-LIT
      REMOVE2 c CMP-FLAG exit
   then
   TOP 1- IMM? if
      TOP MAT TOP 1- VS-VAL @ T0-EMIT:CMP-LIT
      REMOVE2 c T0-EMIT:CC-SWAP CMP-FLAG exit
   then
   TOP 1- MAT TOP MAT T0-EMIT:CMP
   REMOVE2 c CMP-FLAG ;

\ A comparison with zero.
: CMP0 ( [ n -- n ] T0-EMIT:cc -- ) {: c:T0-EMIT:cc :}
   1 ENSURE
   TOP CONST? if FOLD1 exit then
   drop
   TOP MAT 0 T0-EMIT:CMP-LIT
   TOP REMOVE c CMP-FLAG ;

\ ---- opening and closing structures ------------------------------------------
: CS-PUSH ( n n -- ) {: kind:n loc:n :}
   CS-N @ {: k:n :}
   k 1+ CS-KIND-RESERVE  k 1+ CS-LOC-RESERVE  k LAY-ROOM
   kind k CS-KIND !  loc k CS-LOC !  -1 k CS-LAY-N !
   k 1+ CS-N ! ;

\ Pop the top structure, which must be of kind a or b.
: CS-POP2 ( n n -- ) {: a:n b:n :}
   CS-N @ 0= if E-T0-CONTROL throw then
   CS-N @ 1- {: k:n :}
   k CS-KIND @ {: kind:n :}
   kind a <> kind b <> and if E-T0-CONTROL throw then
   k CS-N ! ;

: CS-POP ( n -- ) dup CS-POP2 ;

\ The kind of the top structure.
: CS-TOP-KIND ( -- n )
   CS-N @ 0= if E-T0-CONTROL throw then
   CS-N @ 1- CS-KIND @ ;

\ The locals in scope where the structure last popped opened.
: CS-LOC@ ( -- n ) CS-N @ CS-LOC @ ;

\ The names the popped structure's block bound go out of scope.
: SCOPE-END ( -- ) CS-LOC@ L-N ! ;

\ Whether a compare left its condition pending for this branch.
: CC? ( -- bool ) PENDING @ 0<> ;

\ The pending condition, taken.
: CC-TAKE ( -- T0-EMIT:cc )
   PENDING @ 1- T0-EMIT:>CC
   0 PENDING ! ;

\ A landing that spills: the emptied virtual stack agrees with every arrival.
: LAND ( -- ) SPILL-ALL 0 DEAD ! ;

\ ---- joins -------------------------------------------------------------------
\ A layout is the virtual stack as registers alone: count n and n distinct
\ vregs, the top n cells of the data stack bottom first, the rest in memory.
\ `if` records the layout its skipped path carries, `begin` its loop head's,
\ `while` its exit's and `do` its level's; the paths that meet there
\ reconcile to the layout recorded first.

\ Whether an entry below k holds k's vreg.
: HELD-BELOW? ( n -- bool ) {: k:n :}
   k VS-VAL @ {: v:n :}
   false k 0 ?do i REG? i VS-VAL @ v = and or loop ;

\ Every entry in a vreg of its own: a constant is materialised, and an entry
\ sharing a deeper entry's vreg gets a copy. FREE is exactly the vregs no entry
\ holds (ADOPT), less the flag `if` took off the stack, so the entries and that
\ flag, at most VREGS, leave ALLOC a free vreg each time and it evicts nothing.
\ LIT and MOV keep the condition codes for a fused branch.
: SETTLE ( -- )
   VS-N @ 0 ?do
      i CONST? if i MAT drop else
         i HELD-BELOW? if
            i VREG@ {: s:T0-EMIT:vreg :}
            i RELABEL s T0-EMIT:MOV
         then
      then
   loop ;

\ The virtual stack, settled, recorded as layout k.
: RECORD ( n -- ) {: k:n :}
   VS-N @ k CS-LAY-N !
   VS-N @ 0 ?do i VS-VAL @ k i LAY! loop ;

: SNAPSHOT ( n -- ) SETTLE RECORD ;

: LAY-COPY ( n n -- ) {: from:n to:n :}
   from CS-LAY-N @ to CS-LAY-N !
   T0-EMIT:VREGS 0 ?do from i LAY@ to i LAY! loop ;

\ The virtual stack becomes layout k, its vregs the only ones held. FREE is
\ rebuilt from the layout alone, so it is exactly the vregs no entry holds,
\ whatever the entries it replaces or RECONCILE's moves left allocated.
: ADOPT ( n -- ) {: k:n :}
   k CS-LAY-N @ VS-N !
   ALL-FREE FREE !
   VS-N @ 0 ?do
      k i LAY@ {: v:n :}
      K-REG v i ENTRY!
      1 v lshift invert FREE @ and FREE !
   loop
   0 DEAD ! ;

\ The vreg layout k gives entry j: the entries match the layout's top cells.
: GOAL ( n n -- n ) {: k:n j:n :}
   k  k CS-LAY-N @ VS-N @ - j +  LAY@ ;

\ Whether entry j is not yet in its goal vreg.
: AWAY? ( n n -- bool ) {: k:n j:n :}
   j REG? 0=  j VS-VAL @ k j GOAL <>  or ;

\ Whether an entry not yet in its goal holds vreg v.
: AWAY-HOLDS? ( n n -- bool ) {: k:n v:n :}
   false VS-N @ 0 ?do i REG? i VS-VAL @ v = and k i AWAY? and or loop ;

\ The first entry not yet in its goal, or -1.
: FIRST-AWAY ( n -- n ) {: k:n :}
   -1 VS-N @ 0 ?do dup 0< k i AWAY? and if drop i then loop ;

\ An entry not in its goal whose goal no such entry holds, or -1.
: READY ( n -- n ) {: k:n :}
   -1 VS-N @ 0 ?do
      dup 0< k i AWAY? and if
         k  k i GOAL  AWAY-HOLDS? 0= if drop i then
      then
   loop ;

\ Entry j copied into its goal vreg; a shared source stays for its others.
: MOVE-TO ( n n -- ) {: k:n j:n :}
   k j GOAL {: t:n :}
   j REG? if t T0-EMIT:>VREG j VREG@ T0-EMIT:MOV
   else j VS-VAL @ t T0-EMIT:>VREG T0-EMIT:LIT then
   K-REG t j ENTRY! ;

\ Vregs a and b exchanged in every entry that holds either.
: RENAME ( n n -- ) {: a:n b:n :}
   VS-N @ 0 ?do
      i REG? if
         i VS-VAL @ a = if b i VS-VAL ! else
            i VS-VAL @ b = if a i VS-VAL ! then
         then
      then
   loop ;

\ Entry j put in its goal by an exchange with the entry holding the goal.
\ No entry is ready only when the waiting entries hold their goals and
\ nothing else, one vreg each, a permutation of them.
: CYCLE ( n n -- ) {: k:n j:n :}
   k j GOAL  j VS-VAL @ {: t:n v:n :}
   t T0-EMIT:>VREG v T0-EMIT:>VREG T0-EMIT:EXCHANGE
   t v RENAME ;

\ The bottom d entries written to memory as SPILL-ALL writes them, and dropped.
: SINK ( n -- ) {: d:n :}
   d 1 ?do d i - SPILL-ENTRY loop
   d PUSH-ENTRY
   d 0 ?do 0 REMOVE loop ;

\ The bottom d cells of layout k loaded from memory straight into their
\ vregs, the deepest by the pop, as FILL-ENTRY loads them.
: RAISE ( n n -- ) {: k:n d:n :}
   d 0 ?do
      d 1- i - {: e:n :}
      k e LAY@ T0-EMIT:>VREG {: r:T0-EMIT:vreg :}
      e 0= if r d T0-EMIT:POP else r e d - T0-EMIT:FILL then
   loop ;

\ The fallthrough brought into layout k: entries past its count to memory,
\ the rest moved into their goal vregs as one parallel copy, then the cells
\ the layout holds above memory loaded. An exact layout emits nothing; an
\ unreachable fallthrough emits nothing and takes the layout. It writes only
\ the layout's own vregs: moves into goal vregs, exchanges within a permutation
\ of goals and loads into layout vregs, so a vreg outside the layout keeps its
\ value across it.
: RECONCILE ( n -- ) {: k:n :}
   DEAD @ if k ADOPT exit then
   VS-N @ k CS-LAY-N @ - {: d:n :}
   d 0 > if d SINK then
   begin k FIRST-AWAY 0 >= while
      k READY {: j:n :}
      j 0 >= if k j MOVE-TO else k k FIRST-AWAY CYCLE then
   repeat
   d 0 < if k d negate RAISE then
   k ADOPT ;

\ The arm runs in the layout recorded for the skipped path.
: DO-IF ( -- )
   CS-N @ {: k:n :}
   CS-IF L-N @ CS-PUSH
   CC? if
      CC-TAKE {: c:T0-EMIT:cc :}
      k SNAPSHOT
      c T0-EMIT:IF-CC, exit
   then
   TAKE {: f:T0-EMIT:vreg :}
   k SNAPSHOT
   f T0-EMIT:IF,
   f RELEASE ;

\ The first arm's end is the layout `then` reconciles to; the second arm
\ starts in the one `if` recorded. A dead first arm records none.
: DO-ELSE ( -- )
   CS-IF CS-POP
   SCOPE-END
   CS-N @ {: k:n :}
   k 1+ LAY-ROOM
   DEAD @ 0= {: live:bool :}
   live if k 1+ SNAPSHOT then
   T0-EMIT:ELSE,
   k ADOPT
   CS-ELSE L-N @ CS-PUSH
   live if k 1+ k LAY-COPY then ;

\ `then` reconciles to the layout its opener recorded: a `while`'s exit, or
\ an `if` or `else` path, and with none recorded every other arrival was
\ dead, so the virtual stack continues.
: DO-THEN ( -- )
   CS-TOP-KIND CS-WHILE = if CS-WHILE else CS-IF then
   CS-ELSE CS-POP2
   SCOPE-END
   CS-N @ {: k:n :}
   k CS-LAY-N @ 0 >= if k RECONCILE then
   T0-EMIT:THEN, ;

\ The loop head's layout is the one every back edge reconciles to.
: DO-BEGIN ( -- )
   CS-N @ {: k:n :}
   CS-BEGIN L-N @ CS-PUSH  k SNAPSHOT
   T0-EMIT:BEGIN, ;

\ The exit's layout is recorded under the head's, which moves up a slot; the
\ body continues in the exit's.
: DO-WHILE ( -- )
   CS-BEGIN CS-POP
   CS-LOC@ {: loc:n :}
   CS-N @ {: k:n :}
   k CS-LAY-N @ {: head:n :}
   k 1+ LAY-ROOM  k k 1+ LAY-COPY
   CS-WHILE loc CS-PUSH
   CS-BEGIN loc CS-PUSH  head k 1+ CS-LAY-N !
   CC? if
      CC-TAKE {: c:T0-EMIT:cc :}
      k SNAPSHOT
      c T0-EMIT:WHILE-CC, exit
   then
   TAKE {: f:T0-EMIT:vreg :}
   k SNAPSHOT
   f T0-EMIT:WHILE,
   f RELEASE ;

\ The back edge reconciles to the head; the exit continues in its layout.
: DO-REPEAT ( -- )
   CS-BEGIN CS-POP
   CS-N @ RECONCILE
   CS-WHILE CS-POP
   SCOPE-END
   T0-EMIT:REPEAT,
   CS-N @ ADOPT ;

\ Whether layout k holds vreg r.
: LAY-HOLDS? ( n T0-EMIT:vreg -- bool ) {: k:n r:T0-EMIT:vreg :}
   r T0-EMIT:VREG>N {: v:n :}
   false k CS-LAY-N @ 0 ?do k i LAY@ v = or loop ;

\ The back edge reconciles to the head, and the fallthrough continues in its
\ layout. RECONCILE keeps the condition codes, so the branch on a pending
\ compare follows it; a flag in a vreg the head holds is compared first, and
\ the loop goes back while it is zero, the compare's NE failing. A flag in a
\ vreg outside the head keeps its value across RECONCILE and is tested after.
: DO-UNTIL ( -- )
   CS-BEGIN CS-POP
   SCOPE-END
   CS-N @ {: k:n :}
   CC? if
      CC-TAKE {: c:T0-EMIT:cc :}
      k RECONCILE
      c T0-EMIT:UNTIL-CC, exit
   then
   TAKE {: f:T0-EMIT:vreg :}
   k f LAY-HOLDS? if
      f 0 T0-EMIT:CMP-LIT
      k RECONCILE
      T0-EMIT:CC-NE T0-EMIT:UNTIL-CC, exit
   then
   k RECONCILE
   f T0-EMIT:UNTIL, ;

: DO-AGAIN ( -- )
   CS-BEGIN CS-POP
   SCOPE-END
   CS-N @ RECONCILE
   T0-EMIT:AGAIN,
   1 DEAD ! ;

\ ---- loops -------------------------------------------------------------------
\ A `do` level's layout L is the virtual stack under the start and limit, so it
\ holds at most VREGS - 2 entries: the closer's step and scratch vregs lie
\ outside it, and the emitter's loop words touch no other vreg.
variable DO-N                       \ the open levels

\ Refuse unless at least k levels are open.
: LEVELS ( n -- ) DO-N @ > if E-T0-CONTROL throw then ;

\ Push the frame of `do` or `?do` from ( limit start ), record the rest of the
\ virtual stack as the level's layout, and open the level, `?do` with its entry
\ test when the flag is true. The body, the back edge, the entry's skip and
\ every `leave` meet in the layout.
: OPEN-LEVEL ( bool -- ) {: entry:bool :}
   TAKE {: s:T0-EMIT:vreg :}
   TAKE {: l:T0-EMIT:vreg :}
   s l T0-EMIT:LOOP-PUSH
   CS-N @ {: k:n :}
   CS-DO L-N @ CS-PUSH  k SNAPSHOT
   s l entry T0-EMIT:LOOP-OPEN
   s RELEASE  l RELEASE
   DO-N @ 1+ DO-N ! ;

: DO-DO ( -- ) false OPEN-LEVEL ;

: DO-?DO ( -- ) true OPEN-LEVEL ;

\ The innermost level, which `loop` or `+loop` closes, ending its names.
: CLOSING ( -- ) CS-DO CS-POP SCOPE-END ;

\ Close the innermost level and pop its frame.
: CLOSE-LEVEL ( -- )
   DO-N @ 1- DO-N !
   T0-EMIT:LOOP-CLOSE ;

\ The back edge reconciles to the level's layout, in which the code after the
\ loop continues.
: DO-LOOP ( -- )
   CLOSING
   CS-N @ RECONCILE
   ALLOC {: t:T0-EMIT:vreg :}
   t T0-EMIT:LOOP-NEXT
   t RELEASE
   CLOSE-LEVEL ;

\ The first vreg layout k does not hold.
: OUTSIDE ( n -- T0-EMIT:vreg ) {: k:n :}
   0 begin k over T0-EMIT:>VREG LAY-HOLDS? while 1+ repeat
   T0-EMIT:>VREG ;

\ Layout k with one more vreg on top, outside it, recorded as layout k + 1.
: STEP-LAYOUT ( n -- ) {: k:n :}
   k 1+ LAY-ROOM  k k 1+ LAY-COPY
   k CS-LAY-N @ {: n:n :}
   k OUTSIDE T0-EMIT:VREG>N  k 1+ n LAY!
   n 1+ k 1+ CS-LAY-N ! ;

\ The step reconciles with the layout as a vreg above it, so the layout's moves
\ leave it whole and LOOP-STEP may clobber it.
: DO-+LOOP ( -- )
   CLOSING
   CS-N @ {: k:n :}
   k STEP-LAYOUT
   k 1+ RECONCILE
   TAKE {: s:T0-EMIT:vreg :}
   ALLOC {: t:T0-EMIT:vreg :}
   s t T0-EMIT:LOOP-STEP
   s RELEASE  t RELEASE
   CLOSE-LEVEL ;

\ The innermost level's control stack entry.
: INNER-DO ( -- n )
   CS-N @ 1- begin dup CS-KIND @ CS-DO <> while 1- repeat ;

\ `leave` reconciles to the innermost level's layout, where its exit lands.
: DO-LEAVE ( -- )
   1 LEVELS
   INNER-DO RECONCILE
   T0-EMIT:LEAVE,
   1 DEAD ! ;

: DO-UNLOOP ( -- ) 1 LEVELS T0-EMIT:LOOP-POP ;

\ `i` and `j` push the index of the innermost or the next frame.
: DO-INDEX ( [ T0-EMIT:vreg -- ] n -- ) {: k:n :}
   k LEVELS
   ROOM ALLOC {: r:T0-EMIT:vreg :}
   r swap execute
   r PUSH-REG ;

\ ---- case --------------------------------------------------------------------
\ `of` compares the selector under the top with the top, which it drops; the
\ match drops the selector too, the mismatch branches past `endof` keeping it.
: OF-LIT ( -- )
   TOP VS-VAL @ {: v:n :}
   TOP REMOVE
   TOP MAT {: s:T0-EMIT:vreg :}
   SPILL-ALL
   s v T0-EMIT:OF-LIT ;

: OF-REG ( -- )
   TAKE {: v:T0-EMIT:vreg :}
   TOP MAT {: s:T0-EMIT:vreg :}
   SPILL-ALL
   s v T0-EMIT:OF, ;

: DO-CASE ( -- )
   T0-EMIT:CASE,
   CS-CASE L-N @ CS-PUSH ;

\ The open structure under the `endof`s on top: the case or MATCH whose arms
\ they close.
: ARMS-AT ( -- n )
   CS-N @ 1- begin dup 0 >= if dup CS-KIND @ CS-ENDOF = else false then while 1- repeat
   dup 0 < if E-T0-CONTROL throw then ;

\ Compare the selector under the top with the top and open the arm.
: OF-TEST ( -- )
   2 ENSURE
   TOP IMM? if OF-LIT else OF-REG then
   DROP1
   CS-OF L-N @ CS-PUSH ;

: DO-OF ( -- )
   ARMS-AT CS-KIND @ CS-CASE <> if E-T0-CONTROL throw then
   OF-TEST ;

: DO-ENDOF ( -- )
   CS-OF CS-POP
   SCOPE-END
   LAND
   T0-EMIT:ENDOF,
   CS-ENDOF L-N @ CS-PUSH ;

\ The default arm drops the selector, and every `endof` lands after the drop.
: DO-ENDCASE ( -- )
   DROP1
   LAND
   begin CS-TOP-KIND CS-ENDOF = while CS-ENDOF CS-POP repeat
   CS-CASE CS-POP
   T0-EMIT:ENDCASE,
   SCOPE-END ;

\ ---- the rest of the keywords ------------------------------------------------
\ True once the body has left through a tail call: nothing follows it.
variable TAILED

: DO-EXIT ( -- ) SPILL-ALL T0-EMIT:RETURN 1 DEAD ! ;
: DO-RECURSE ( -- ) SPILL-ALL T0-EMIT:CALL-SELF ;

: DO-TOR ( -- )
   TAKE {: r:T0-EMIT:vreg :}
   r T0-EMIT:TOR
   r RELEASE ;

: DO-RFROM ( -- )
   ROOM ALLOC {: r:T0-EMIT:vreg :}
   r T0-EMIT:RFROM
   r PUSH-REG ;

: DO-RFETCH ( -- )
   ROOM ALLOC {: r:T0-EMIT:vreg :}
   r T0-EMIT:RFETCH
   r PUSH-REG ;

: TOR-CELLS ( n -- ) 0 ?do DO-TOR loop ;
: RFROM-CELLS ( n -- ) 0 ?do DO-RFROM loop ;

\ The top cells of the w-cell value on the return stack, pushed back top first.
: RE-TOR ( n -- ) 0 ?do i DEEP DO-TOR loop ;

: DO-R@ ( ptr u8 -- ) {: a:ptr :}
   a 1 WIDTHS
   WIDE? 0= if DO-RFETCH exit then
   0 WS @ RFROM-CELLS  0 WS @ RE-TOR ;

\ `2>r` is `swap >r >r`, `2r>` is `r> r> swap`, `2r@` is `2r> 2dup 2>r`.
: 2TOR ( -- ) SWAP-OP  1 WS @ TOR-CELLS  0 WS @ TOR-CELLS ;
: 2RFROM ( -- ) 0 WS @ RFROM-CELLS  1 WS @ RFROM-CELLS  WS-SWAP SWAP-OP WS-SWAP ;

: DO-TICK ( -- )
   TOKEN {: a:ptr u:n :}
   a u NDICT:TICK-TARGET {: xt:n :}
   xt 0= if
      a u NDICT:SCOPE-REC XREF-FOUND? 0= if -1 MISSING ! then
      a u UNSUPPORTED
   then
   ROOM ALLOC {: r:T0-EMIT:vreg :}
   xt r T0-EMIT:ADDR-CODE
   r PUSH-REG ;

\ The operand's first byte, as tier 1 compiles it (the JIT compiles `[char]`
\ only): `[char] é` is the 195 that begins its UTF-8.
: DO-CHAR ( -- )
   LIT-OPERAND {: a:ptr u:n :}
   u 0= if a u UNSUPPORTED then
   a c@ PUSH-CONST ;

\ A call spills first; the last token of a body outside every structure leaves
\ through the callee when the callee lies in the code region, as tier 1's
\ (src/compiler/native/compiler.f LINKAGE): publication refuses a tail branch
\ out of it. loc is the calling token's offset in the text.
: DO-CALL ( n n -- ) {: to:n loc:n :}
   SPILL-ALL
   CS-N @ 0= AT-END? and to NPUB:IN-REGION? and if
      to loc T0-EMIT:TAIL
      1 TAILED ! exit
   then
   to loc T0-EMIT:CALL ;

\ The stack word at token a over k operands, moved as outs, MOVE-WIDE's digits.
: MOVE-OP ( ptr u8 n ptr u8 n -- ) {: a:ptr k:n outs:ptr u:n :}
   a k WIDTHS  outs u MOVE-WIDE ;

\ The two-operand return-stack word at token a, as the composition xt.
: PAIR-OP ( [ -- ] ptr u8 -- ) {: a:ptr :} a 2 WIDTHS execute ;

: 2RFETCH ( -- ) 2RFROM s" 1010" MOVE-WIDE 2TOR ;

\ ---- whole-width memory -------------------------------------------------------
\ The cells of the top operand of the operation at token a.
: CELLS0 ( ptr u8 -- n ) {: a:ptr :} a 1 WIDTHS 0 WS @ ;

\ The address on top advanced by k cells.
: ADDR+ ( n -- ) {: k:n :}
   k 0= if exit then
   k cells PUSH-CONST [: + ;] T0-EMIT:OP-ADD true ALU2 ;

\ Push data address v, whose row files origin o.
: PUSH-ADDR ( n n -- ) {: v:n o:n :}
   ROOM ALLOC {: r:T0-EMIT:vreg :}
   v o r T0-EMIT:ADDR-DATA
   r PUSH-REG ;

\ A fetch the checker filed a layout fact for calls NFETCH-CHECK:TAGS with a
\ copy of the address and the fact's descriptor before any cell is loaded, as
\ tier 1's VALIDATE-FETCH (src/compiler/native/elaborate.f), whose comment says
\ why the descriptor's address literal is the compiler's own trusted producer of
\ a frozen value. NFETCH:CAPTURE keys each fact by its offset in the whole text,
\ the clause's base added, so the key is the token's offset with no cut taken.
: VALIDATE ( ptr u8 n -- ) {: a:ptr w:n :}
   a TXT$ - {: at:n :}
   at NFETCH:AT {: address:n bytes:n width:n origin:n :}
   bytes 0= if exit then
   width w <> if E-T0-BUNDLE throw then
   s" NFETCH-CHECK:TAGS" NDICT:CALL-TARGET {: entry:n :}
   entry 0= if E-HIR-UNMODELED throw then
   0 DEEP
   address origin PUSH-ADDR
   SPILL-ALL  entry at T0-EMIT:CALL ;

: DO-FETCH ( ptr u8 -- ) {: a:ptr :}
   a CELLS0 {: w:n :}
   w 1 < if E-T0-BUNDLE throw then
   a w VALIDATE
   w 1 = if T0-EMIT:CELL-W FETCH exit then
   w 0 ?do i DEEP i ADDR+ T0-EMIT:CELL-W FETCH loop
   w 1 NIP-CELLS ;

\ The wide store's calls never leave as a tail: each stores a copy of one cell,
\ so the w cells and the address are still on the stack after the last.
: DO-STORE ( ptr u8 n -- ) {: a:ptr u:n :}
   a CELLS0 {: w:n :}
   w 1 < if E-T0-BUNDLE throw then
   a u NDICT:CALL-TARGET  a TXT$ - {: to:n loc:n :}
   w 1 = if to loc DO-CALL exit then
   w 0 ?do
      w i - DEEP  1 DEEP  i ADDR+
      SPILL-ALL  to loc T0-EMIT:CALL
   loop
   w 1+ negate T0-EMIT:DSP+ ;

\ ---- strings -------------------------------------------------------------------
0 constant STR-NONE
1 constant STR-PUSH                 \ s": the address and the length
2 constant STR-COUNTED              \ c": the address of the count byte
3 constant STR-PRINT                \ .": both, passed to `type`

$22 constant QUOTE
$5C constant BACKSLASH
255 constant COUNTED-MAX            \ a counted string's length is one byte

DYNAMIC-BUFFER STR-BUF u8           \ a decoded or counted body before interning

\ The kind of string a token opens; an escaped one is three bytes long.
: OPENER ( ptr u8 n -- n ) {: a:ptr u:n :}
   u 2 < u 3 > or if STR-NONE exit then
   a u + 1- c@ QUOTE <> if STR-NONE exit then
   u 3 = if a 1+ c@ BACKSLASH <> if STR-NONE exit then then
   a c@ {: c:n :}
   c [char] s = c [char] S = or if STR-PUSH exit then
   c [char] c = c [char] C = or if STR-COUNTED exit then
   c [char] . = if STR-PRINT exit then
   STR-NONE ;

\ The first quote from s on before e, the bytes before it, and whether one is.
: PLAIN-SPAN ( ptr u8 ptr u8 -- ptr u8 n bool ) {: s:ptr e:ptr :}
   s begin dup e < if dup c@ QUOTE <> else false then while 1+ repeat
   {: q:ptr :}
   q  q s -  q e < ;

\ The payload after an opener: its first byte, its closing quote and the count
\ of bytes it stands for. POS moves past the quote.
: SPAN ( bool -- ptr u8 ptr u8 n ) {: esc:bool :}
   TXT$ POS @ 1+ + {: s:ptr :}
   TXT$ TXT-LEN @ + {: e:ptr :}
   esc if s e ESCAPE:SPAN else s e PLAIN-SPAN then {: q:ptr u:n ok:bool :}
   ok 0= if E-T0-LITERAL throw then
   q TXT$ - 1+ POS !
   s q u ;

\ The payload's interned address, its source origin and its length. A plain or
\ empty body interns from the text; a decoded or counted one is built in
\ STR-BUF first.
: INTERN-BODY ( bool bool -- n n n ) {: esc:bool counted:bool :}
   esc SPAN {: s:ptr q:ptr u:n :}
   counted u COUNTED-MAX > and if E-T0-LITERAL throw then
   esc 0= u 0= or counted 0= and if s u NSTR:INTERN-SOURCE u exit then
   counted if 1 else 0 then {: at:n :}
   at u + STR-BUF-RESERVE
   esc if s q at STR-BUF ESCAPE:COPY else s at STR-BUF u BYTE-COPY then
   counted if u 0 STR-BUF c! then
   0 STR-BUF at u + NSTR:INTERN-SOURCE u ;

\ `."` calls the engine's `type` whatever the source's package scope, as tier
\ 1 does (src/compiler/native/elaborate.f TYPE-ENTRY).
TRUSTED: TYPE-ENTRY ( -- n ) ['] type ;

\ loc is the text's offset in the body, the call's for `."`, as tier 1's
\ (src/core/checker.f SPAY-B).
: DO-STRING ( n bool n -- ) {: kind:n esc:bool loc:n :}
   esc kind STR-COUNTED = INTERN-BODY {: v:n o:n u:n :}
   v o PUSH-ADDR
   kind STR-COUNTED = if exit then
   u PUSH-CONST
   kind STR-PRINT = if TYPE-ENTRY loc DO-CALL then ;

\ ---- is ------------------------------------------------------------------------
\ `is` stores through the engine's `xt!` whatever the source's package scope, as
\ tier 1 does (src/compiler/native/elaborate.f XT-STORE-ENTRY).
TRUSTED: XT-STORE-ENTRY ( -- n ) ['] xt! ;

\ `is W` at offset loc: the xt on the stack goes into W's cell.
: DO-IS ( n -- )
   {: loc:n :}
   TOKEN {: a:ptr u:n :}
   a u NDICT:SPELL-DEFER-BINDING {: cell:n origin:n :}
   cell 0= if E-T0-DEFER throw then
   a u NDICT:SPELL-ARITY {: din:n dout:n :}
   din NDICT:ARITY-NONE = dout NDICT:ARITY-NONE = or if E-T0-DEFER throw then
   cell origin PUSH-ADDR
   XT-STORE-ENTRY loc DO-CALL ;

\ ---- quotations ------------------------------------------------------------------
variable NQ                         \ units met so far, the definition's included
DYNAMIC-BUFFER QSTART n             \ where each quotation's body text starts
DYNAMIC-BUFFER QORD n               \ and the checker's ordinal of its first token
variable NESTED                     \ the `[:` tokens skipped inside quotations so far

\ How a token in a quotation being skipped moves the nesting depth. A string's
\ payload, a parsing word's operand and the word `is` names are skipped with it.
: SKIP-TOKEN ( ptr u8 n -- n ) {: a:ptr u:n :}
   u 0= if E-T0-CONTROL throw then
   a u s" [:" STR= if 1 NESTED +! 1 exit then
   a u QUOT-END? if -1 exit then
   a u OPENER STR-NONE <> if u 3 = SPAN drop 2drop 0 exit then
   a u OPERAND? if
      a u LIT-KEYWORD? if LIT-OPERAND else TOKEN then 2drop
   then
   0 ;

\ Past the `;]` that closes the quotation just opened.
: SKIP-QUOT ( -- )
   1 begin dup 0 > while TOKEN SKIP-TOKEN + repeat drop ;

\ Push unit k's address.
: PUSH-UNIT ( n -- ) {: k:n :}
   ROOM ALLOC {: r:T0-EMIT:vreg :}
   k r T0-EMIT:ADDR-QUOT
   r PUSH-REG ;

\ Push the next unit's address and skip its text, compiled after this unit.
: DO-QUOT ( -- )
   NQ @ {: k:n :}
   k 1+ QSTART-RESERVE  k 1+ QORD-RESERVE
   POS @ k QSTART !  ORD @ k QORD !
   k 1+ NQ !
   SKIP-QUOT
   k PUSH-UNIT ;

\ ---- the does> split -----------------------------------------------------------
variable EMPTY-CLAUSE               \ whether no token follows the cut: the clause is empty
variable CLAUSE-ORD                 \ the checker's ordinal of the clause's first token

\ The parent leaves through `does-patch` whatever the source's package scope,
\ as tier 1 does (src/compiler/native/elaborate.f DOES-PATCH-ENTRY).
TRUSTED: DOES-PATCH-ENTRY ( -- n ) ['] does-patch ;

\ Whether no token follows the cut: the clause is empty.
: CLAUSE-EMPTY? ( -- bool )
   CUT @ POS !
   TOKEN nip 0= ;

\ The parent's tail, at its `does>`: `does-patch` takes the clause's entry, 0
\ for an empty clause, and the clause's interned signature, as tier 1's
\ (src/compiler/native/elaborate.f STAGE-DOES-PATCH). The clause is the unit
\ after every quotation of the parent text, nested ones included.
: DOES-TAIL ( -- )
   NQ @ NESTED @ + CLAUSE !
   EMPTY-CLAUSE @ if 0 PUSH-CONST else CLAUSE @ PUSH-UNIT then
   NDEFINITION:M-DOES-SIG @ NDEFINITION:M-DOES-SIG-U @ NSTR:INTERN-SOURCE PUSH-ADDR
   NDEFINITION:M-DOES-SIG-U @ PUSH-CONST
   SPILL-ALL
   DOES-PATCH-ENTRY CUT @ NDEFINITION:DOES-SPAN - T0-EMIT:CALL ;

\ ---- MATCH and construct -----------------------------------------------------
\ The facts are the checker's, filed under the ordinal of the token that holds
\ them, as tier 1 reads them (src/compiler/native/elaborate.f MATCH-SCAN).
: MATCH-CK ( bool -- ) 0= if E-T0-MATCH throw then ;

\ `;MATCH` closes MATCH k: a tag no arm matched runs into the trap on the last
\ arm's mismatch edge, as tier 1's MATCH-TRAP, and every `endof` lands after
\ it. loc is the `;MATCH` token's offset in the text.
: CLOSE-MATCH ( n n -- ) {: loc:n k:n :}
   CS-TOP-KIND CS-MATCH <> MATCH-CK
   k CS-FAM @ NFAM:NAME$ NTRAP:BAD-TAG {: a:ptr u:n rc:n :}
   a u NSTR:INTERN-SOURCE PUSH-ADDR
   u PUSH-CONST  rc PUSH-CONST
   NTRAP:ROUTINE loc DO-CALL
   begin CS-TOP-KIND CS-ENDOF = while CS-ENDOF CS-POP repeat
   CS-MATCH CS-POP
   T0-EMIT:ENDCASE,
   SCOPE-END ;

\ The next arm's `variant of`, or the `;MATCH` that ends the arms. The arm
\ compares the tag with the variant's; the match drops the tag and the pads
\ the checker proved at the `of`, leaving the payload, and the mismatch keeps
\ the value for the next arm.
: ARM-HEAD ( -- )
   ARMS-AT {: k:n :}
   TOKEN {: a:ptr u:n :}
   a u s" ;MATCH" STR=CI if a TXT$ - k CLOSE-MATCH exit then
   a u k CS-FAM @ NFAM:VARIANT MATCH-CK {: vid:n :}
   ORD @ {: at:n :}
   TOKEN s" of" STR=CI MATCH-CK
   at NDICT:MATCH-CELLS {: pads:n :}
   pads NDICT:MATCH-NONE <> MATCH-CK
   k CS-WID @ 1- pads - {: pay:n :}
   pay 0 >= MATCH-CK
   at NDICT:MATCH-PAYLOAD drop pay = MATCH-CK
   vid NFAM:TAG PUSH-CONST
   OF-TEST
   pads 0 ?do DROP1 loop ;

\ `MATCH fam`: the value is the cells the checker proved at the family token,
\ at least the family's declared width.
: DO-MATCH ( -- )
   ORD @ {: at:n :}
   TOKEN NFAM:MATCH-FAM MATCH-CK {: fam:n :}
   at NDICT:MATCH-CELLS {: w:n :}
   w NDICT:MATCH-NONE <> MATCH-CK
   w fam NFAM:WIDTH >= MATCH-CK
   T0-EMIT:CASE,
   CS-N @ {: k:n :}
   CS-MATCH L-N @ CS-PUSH
   k 1+ CS-FAM-RESERVE  k 1+ CS-WID-RESERVE
   fam k CS-FAM !  w k CS-WID !
   ARM-HEAD ;

\ `endof`; a MATCH's arm reads the next `variant of` or `;MATCH` after it.
: ARM-END ( -- )
   DO-ENDOF
   ARMS-AT CS-KIND @ CS-MATCH = if ARM-HEAD then ;

\ `construct fam v`: the variant's pads, the family's and those the checker
\ proved the instantiation adds at the variant token, then its tag, as tier 1's
\ DO-MAKE-BUNDLE; the payload is already on the stack.
: DO-CONSTRUCT ( -- )
   TOKEN NFAM:CON-FAM MATCH-CK {: fam:n :}
   ORD @ {: at:n :}
   TOKEN fam NFAM:VARIANT MATCH-CK {: vid:n :}
   at NDICT:CALL-CELLS drop 0 >= MATCH-CK
   fam vid NFAM:PADS  at NDICT:CON-PADS +  0 ?do 0 PUSH-CONST loop
   vid NFAM:TAG PUSH-CONST ;

\ ---- the keyword dispatch ----------------------------------------------------
\ By length first, so a token compares only with the keywords of its length.
\ An operation spelling counts only when it binds the seeded primitive
\ (NDICT:SPELL-PRIM?); a package's own word of that name is a call.
: OP? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n b:ptr v:n :}
   a u b v STR=CI 0= if false exit then
   a u NDICT:SPELL-PRIM? ;

\ An operation over one cell: the spelling binds the primitive and the checker
\ certified no wider operand at the token.
: CELL-OP? ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n b:ptr v:n :}
   a u b v OP? 0= if false exit then
   a CELLS0 1 <> if E-T0-BUNDLE throw then
   true ;

: KW1 ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" i" STR=CI if [: T0-EMIT:LOOP-I ;] 1 DO-INDEX true exit then
   a u s" j" STR=CI if [: T0-EMIT:LOOP-J ;] 2 DO-INDEX true exit then
   a u s" +" CELL-OP? if [: + ;] T0-EMIT:OP-ADD true ALU2 true exit then
   a u s" -" CELL-OP? if [: - ;] T0-EMIT:OP-SUB false ALU2 true exit then
   a u s" *" CELL-OP? if MUL2 true exit then
   a u s" /" CELL-OP? if [: / ;] [: T0-EMIT:DIV-LIT ;] DIV2 exit then
   a u s" @" OP? if a DO-FETCH true exit then
   a u s" !" OP? if a u DO-STORE true exit then
   a u s" =" CELL-OP? if [: = bool>n ;] T0-EMIT:CC-EQ CMP2 true exit then
   a u s" <" CELL-OP? if [: < bool>n ;] T0-EMIT:CC-LT CMP2 true exit then
   a u s" >" CELL-OP? if [: > bool>n ;] T0-EMIT:CC-GT CMP2 true exit then
   false ;

: KW2 ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" if" STR=CI if DO-IF true exit then
   a u s" is" STR=CI if a TXT$ - DO-IS true exit then
   a u s" do" STR=CI if DO-DO true exit then
   a u s" of" STR=CI if DO-OF true exit then
   a u s" {:" STR=CI if a u DO-GROUP true exit then
   a u s" [:" STR=CI if DO-QUOT true exit then
   a u s" >r" STR=CI if a 1 WIDTHS 0 WS @ TOR-CELLS true exit then
   a u s" r>" STR=CI if a 1 WIDTHS 0 WS @ RFROM-CELLS true exit then
   a u s" r@" STR=CI if a DO-R@ true exit then
   a u s" or" CELL-OP? if [: or ;] T0-EMIT:OP-OR true ALU2 true exit then
   a u s" 1+" CELL-OP? if 1 PUSH-CONST [: + ;] T0-EMIT:OP-ADD true ALU2 true exit then
   a u s" 1-" CELL-OP? if 1 PUSH-CONST [: - ;] T0-EMIT:OP-SUB false ALU2 true exit then
   a u s" c@" CELL-OP? if T0-EMIT:BYTE-W FETCH true exit then
   a u s" 0=" CELL-OP? if [: 0= bool>n ;] T0-EMIT:CC-EQ CMP0 true exit then
   a u s" 0<" CELL-OP? if [: 0< bool>n ;] T0-EMIT:CC-LT CMP0 true exit then
   a u s" <>" CELL-OP? if [: <> bool>n ;] T0-EMIT:CC-NE CMP2 true exit then
   a u s" <=" CELL-OP? if [: <= bool>n ;] T0-EMIT:CC-LE CMP2 true exit then
   a u s" >=" CELL-OP? if [: >= bool>n ;] T0-EMIT:CC-GE CMP2 true exit then
   false ;

: KW3 ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" [']" STR=CI if DO-TICK true exit then
   a u s" ?do" STR=CI if DO-?DO true exit then
   a u s" dup" OP? if [: DUP1 ;] a 1 s" 00" SHUFFLE true exit then
   a u s" rot" OP? if [: ROT1 ;] a 3 s" 102" SHUFFLE true exit then
   a u s" nip" OP? if [: NIP1 ;] a 2 s" 0" SHUFFLE true exit then
   a u s" 2>r" OP? if [: 2TOR ;] a PAIR-OP true exit then
   a u s" 2r>" OP? if [: 2RFROM ;] a PAIR-OP true exit then
   a u s" 2r@" OP? if [: 2RFETCH ;] a PAIR-OP true exit then
   a u s" and" CELL-OP? if [: and ;] T0-EMIT:OP-AND true ALU2 true exit then
   a u s" mod" CELL-OP? if [: mod ;] [: T0-EMIT:MOD-LIT ;] DIV2 exit then
   a u s" xor" CELL-OP? if [: xor ;] T0-EMIT:OP-XOR true ALU2 true exit then
   false ;

: KW4 ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" then" STR=CI if DO-THEN true exit then
   a u s" else" STR=CI if DO-ELSE true exit then
   a u s" exit" STR=CI if DO-EXIT true exit then
   a u s" loop" STR=CI if DO-LOOP true exit then
   a u s" case" STR=CI if DO-CASE true exit then
   a u s" char" STR=CI if DO-CHAR true exit then
   a u s" drop" OP? if [: DROP1 ;] a 1 s" " SHUFFLE true exit then
   a u s" swap" OP? if [: SWAP1 ;] a 2 s" 01" SHUFFLE true exit then
   a u s" over" OP? if [: OVER1 ;] a 2 s" 101" SHUFFLE true exit then
   a u s" tuck" OP? if [: TUCK1 ;] a 2 s" 010" SHUFFLE true exit then
   a u s" -rot" OP? if a 3 s" 021" MOVE-OP true exit then
   a u s" 2dup" OP? if a 2 s" 1010" MOVE-OP true exit then
   false ;

: KW5 ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" begin" STR=CI if DO-BEGIN true exit then
   a u s" while" STR=CI if DO-WHILE true exit then
   a u s" until" STR=CI if DO-UNTIL true exit then
   a u s" again" STR=CI if DO-AGAIN true exit then
   a u s" +loop" STR=CI if DO-+LOOP true exit then
   a u s" leave" STR=CI if DO-LEAVE true exit then
   a u s" endof" STR=CI if ARM-END true exit then
   a u s" MATCH" STR=CI if DO-MATCH true exit then
   a u s" 2drop" OP? if a 2 s" " MOVE-OP true exit then
   a u s" 2swap" OP? if a 4 s" 1032" MOVE-OP true exit then
   a u s" 2over" OP? if a 4 s" 321032" MOVE-OP true exit then
   a u s" cells" CELL-OP? if CELLS-OP true exit then
   a u s" cell+" CELL-OP? if
      CELL PUSH-CONST [: + ;] T0-EMIT:OP-ADD true ALU2 true exit
   then
   false ;

: KW6 ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" repeat" STR=CI if DO-REPEAT true exit then
   a u s" [char]" STR=CI if DO-CHAR true exit then
   a u s" unloop" STR=CI if DO-UNLOOP true exit then
   a u s" negate" CELL-OP? if [: negate ;] [: T0-EMIT:NEG ;] UNARY true exit then
   a u s" invert" CELL-OP? if [: invert ;] [: T0-EMIT:NOT ;] UNARY true exit then
   a u s" lshift" CELL-OP? if [: lshift ;] T0-EMIT:SH-LEFT SHIFT2 true exit then
   a u s" rshift" CELL-OP? if [: rshift ;] T0-EMIT:SH-RIGHT SHIFT2 true exit then
   false ;

: KW7 ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" recurse" STR=CI if DO-RECURSE true exit then
   a u s" endcase" STR=CI if DO-ENDCASE true exit then
   false ;

: KW9 ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" construct" STR=CI if DO-CONSTRUCT true exit then
   a u s" ptr-field" CELL-OP? if
      CELLS-OP [: + ;] T0-EMIT:OP-ADD true ALU2 true exit
   then
   false ;

: KEYWORD ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 1 = if a u KW1 exit then
   u 2 = if a u KW2 exit then
   u 3 = if a u KW3 exit then
   u 4 = if a u KW4 exit then
   u 5 = if a u KW5 exit then
   u 6 = if a u KW6 exit then
   u 7 = if a u KW7 exit then
   u 9 = if a u KW9 exit then
   false ;

\ ---- one token ---------------------------------------------------------------
\ An immediate the capture holds. A parse-neutral one ran as the engine read it
\ (src/habu/definers.f DEF-IMMEDIATE) and left only its own token, its operands
\ read from the input and text it evaluated captured after it, so T0 passes
\ over the token as tier 1 does (src/compiler/native/elaborate.f WALK-SKIP?).
\ Any other is refused by name.
: IMM-TOKEN ( ptr u8 n -- ) {: a:ptr u:n :}
   a u CHECKER-OWNER:PARSE-IMM? if exit then
   a u E-T0-IMMEDIATE REFUSE ;

: NUMBER ( ptr u8 n -- ) {: a:ptr u:n :}
   a u OUTER:NUMBER {: v:n float:bool number:bool range:bool :}
   number 0= range or if a u UNSUPPORTED then
   v PUSH-CONST ;

\ A word the dictionary binds. One that builds a value of a wide instantiation
\ first pushes the cells the checker proved the instantiation adds at the
\ token (NDICT:CON-PADS), as tier 1's DO-CALL; the token TOKEN just read is
\ ordinal ORD - 1. A fixed word, one whose definer stamped what its body
\ pushes, is then that value, as tier 1 folds it (src/compiler/native/hir-word.f
\ RESOLVE-SITE): a constant's number, which CMP2 and the ALU take as an
\ immediate, or a created word's or variable's DATA address, filed with its
\ record's ordinal so publication relocates it. The stamp is that of the
\ record the call binds (NDICT:CALL-BINDING): a non-empty does> clause clears
\ it, so a does> child is called. Any other word is called.
: WORD-CALL ( ptr u8 n n n n -- ) {: a:ptr u:n to:n kind:n loc:n :}
   ORD @ 1- NDICT:CON-PADS 0 ?do 0 PUSH-CONST loop
   kind DKIND:VAL = if to NDICT:FIXED-VAL NDICT:BOUND-VALUE PUSH-CONST exit then
   kind DKIND:ADDR = if
      to NDICT:FIXED-ADDR NDICT:BOUND-VALUE  a u NDICT:FIXED-OWNER  PUSH-ADDR exit
   then
   to loc DO-CALL ;

: COMPILE-TOKEN ( ptr u8 n -- ) {: a:ptr u:n :}
   a TXT$ - {: loc:n :}
   a u FIND-LOCAL {: k:n :}
   k 0 >= if k DO-LOCAL exit then
   a u OPENER {: kind:n :}
   kind STR-NONE <> if kind u 3 = loc u + 1+ DO-STRING exit then
   a u KEYWORD if exit then
   a u NDICT:CALL-BINDING {: to:n kind:n :}
   to 0<> if a u to kind loc WORD-CALL exit then
   a u tok-imm? 0<> if a u IMM-TOKEN exit then
   a u NUMBER ;

$29 constant CLOSE-PAREN

\ Past the name and a signature, if one follows it. The checker reports the
\ name as ordinal 0 and no token of the signature. The signature ends at its
\ first `)` byte, as the engine's `(` reads it: `( n -- n)` ends after `n`.
: SKIP-HEAD ( -- )
   TOKEN 2drop
   POS @ {: after:n :}
   TOKEN s" (" STR= 0= if after POS ! 1 ORD ! exit then
   begin
      POS @ TXT-LEN @ < if TXT$ POS @ + c@ CLOSE-PAREN <> else false then
   while POS @ 1+ POS ! repeat
   POS @ 1+ POS !
   1 ORD ! ;

\ ---- one unit --------------------------------------------------------------------
\ Compile tokens to the end of the unit: whether a `;]` ended it rather than
\ the end of the text or the split's `does>`.
: BODY ( -- bool )
   begin
      TOKEN dup 0= if 2drop false exit then
      over TXT$ - CUT @ NDEFINITION:DOES-SPAN - = if 2drop false exit then
      2dup QUOT-END? if 2drop true exit then
      COMPILE-TOKEN
   again ;

\ Begin unit k with nothing open and no local in scope.
: RESET ( n -- )
   UNIT !
   T0-EMIT:VREGS VS-KIND-RESERVE  T0-EMIT:VREGS VS-VAL-RESERVE
   0 VS-N !  ALL-FREE FREE !  0 CS-N !  0 DO-N !  0 L-N !  0 TAILED !
   0 DEAD !  0 PENDING ! ;

: FINISH ( -- )
   CS-N @ 0<> if E-T0-CONTROL throw then
   TAILED @ 0= if SPILL-ALL T0-EMIT:RETURN then
   T0-EMIT:CLOSE ;

\ Quotation unit k, from its recorded start through its `;]`.
: QUOT ( n -- ) {: k:n :}
   k QSTART @ POS !  k QORD @ ORD !
   k RESET
   k T0-EMIT:QUOT-OPEN
   BODY 0= if E-T0-CONTROL throw then
   FINISH ;

\ A unit that binds must bind the count of locals the certificate captured at
\ base n holds, as tier 1's BINDS-CK requires (src/compiler/native/elaborate.f).
: BINDS-CK ( n -- ) {: base:n :}
   BINDS @ 0= if exit then
   BINDS @ base NFETCH:BINDS <> if E-T0-BINDS throw then ;

\ Quotation units from n until every unit met is compiled: the next unit.
: UNITS ( n -- n )
   begin dup NQ @ < while dup QUOT 1+ repeat ;

\ The does> clause, unit k, from the split to the end of the text, with its
\ own locals and its own certificate at base CUT. An empty clause's unit is
\ still built, with no address bound to it: the parent pushed 0.
: DO-CLAUSE ( n -- ) {: k:n :}
   CUT @ POS !  CLAUSE-ORD @ ORD !  k 1+ NQ !  k RESET  0 BINDS !
   EMPTY-CLAUSE @ if T0-EMIT:UNIT-OPEN else k T0-EMIT:QUOT-OPEN then
   BODY if E-T0-CONTROL throw then
   FINISH
   CUT @ BINDS-CK ;

\ Compile one body, split at cut when it is not 0, into the emitter's sink:
\ the definition, its quotations, then the clause and its quotations.
: WALK ( ptr u8 n n -- ) {: a:ptr u:n cut:n :}
   a TXT !  u TXT-LEN !  1 NQ !  0 BINDS !
   cut CUT !  -1 CLAUSE !  0 NESTED !
   cut 0<> if CLAUSE-EMPTY? else false then EMPTY-CLAUSE !
   0 POS !
   0 RESET
   T0-EMIT:OPEN
   SKIP-HEAD
   BODY if E-T0-CONTROL throw then
   ORD @ CLAUSE-ORD !
   cut 0<> if
      POS @ cut 1- <> if E-T0-CONTROL throw then
      DOES-TAIL
   then
   FINISH
   0 BINDS-CK
   1 UNITS
   cut 0= if drop exit then
   CLAUSE @ <> if E-T0-CONTROL throw then
   CLAUSE @ DO-CLAUSE
   CLAUSE @ 1+ UNITS drop ;

public

\ The token the last E-T0-UNSUPPORTED or E-T0-IMMEDIATE named.
: UNSUPPORTED$ ( -- ptr u8 n ) BAD @ BAD-LEN @ ;

\ Compile one body with no split, and then its quotations, into the emitter's
\ sink. The body's certificate is captured.
: COMPILE-TEXT ( ptr u8 n -- ) 0 WALK ;

using NDEFINITION
private

\ ---- the runtime compiler ------------------------------------------------------
\ The scan's recording unit latches the facts the capture and the call targets
\ read. T0 keeps no tape: its observer drops every event and keeps only the
\ verdict the scan reached, as tier 1's tape does (src/compiler/native/feed.f
\ ON-DONE).
variable OBSERVER-ID
variable SCAN-VERDICT

: OBSERVE ( -- )
   OBSERVER-ID BYTE-VIEW NULL-PTR BYTE-VIEW -
   [: 2drop ;] [: 2drop 2drop 2drop ;] [: SCAN-VERDICT ! 2drop ;]
   CHECKER-OWNER:TAPE-INSTALL ;

\ The definition's name, the body text's first token, read in place.
: KEEP-NAME ( -- )
   SRC$ TXT-LEN ! TXT !  0 POS !
   TOKEN NAME-U ! NAME-A ! ;

\ The body's scan and capture, or a does> definer's head and clause, the
\ clause named for a refusal by the definition's name.
: CHECK-UNIT ( -- n )
   M-DOES @ 0<> if
      CHECK-DOES-HEAD
      NAME$ CHECK-DOES-CLAUSE exit
   then
   CHECK-SOURCE ;

\ The scan's verdict stands over a hook's nonzero answer; a hook's zero answer
\ stands over the scan's, as tier 1's END-RECORDED has it.
: SCAN ( -- )
   OBSERVE
   0 SCAN-VERDICT !
   CHECKER-OWNER:TAPE-ARM
   [: CHECK-UNIT M-VERDICT ! ;] [: CHECKER-OWNER:TAPE-DISARM ;] finally
   M-VERDICT @ 0<> if SCAN-VERDICT @ M-VERDICT ! then
   DECLARE-ROW ;

: LOWER ( n -- ) {: before:n :}
   before SOURCE-PUBLICATION-CK
   RECORD-NAME-CK
   KEEP-ARITY
   SRC$ M-DOES @ WALK  CLAUSE @ M-DOES-FUN !
   NQ @ [: drop -1 -1 ;] HOST-ARITIES
   NHOST:SOURCE-FREEZE
   STUB-ARM
   NPUB:NEXT-SLOT T0-EMIT:SEAL
   STUB-TAKE
   PUBLISH-IT ;

\ The compile window, the emission and the armed stub end on every exit.
: SHUT ( -- )
   0 CHECKER-OWNER:WRITE-WINDOW
   NEMIT:CLEAR
   NCBSTUB:CLEAR ;

: RUN ( -- )
   NSHADOW:OPEN? if E-T0-SHADOW throw then
   ndict@ {: before:n :}
   SCAN
   VERDICT-CK
   E-NCOMP-STATE CHECKER-OWNER:WRITE-WINDOW
   before [: LOWER ;] [: SHUT ;] finally ;

\ The token a refusal names: T0's own for its refusals, else none.
: REFUSED$ ( n -- ptr u8 n ) {: rc:n :}
   rc E-T0-UNSUPPORTED = rc E-T0-IMMEDIATE = or if UNSUPPORTED$ exit then
   NULL-PTR 0 ;

public

\ Compile the pending definition whose body text the engine captured.
: COMPILE ( ptr u8 n -- )
   IDLE-CK
   false STAGE-UNIT
   KEEP-NAME
   NHOST:UNKNOWN 0 NHOST:SOURCE-REFUSE
   CHECKER-OWNER:RESET-REPORT
   0 MISSING !
   1 M-OPEN !
   [: RUN ;] catch {: rc:n :}
   0 M-OPEN !
   STUB-END
   NFETCH:RELEASE
   NHOST:SOURCE-ABANDON
   0 rc  rc E-T0-UNSUPPORTED = MISSING @ 0<> and  rc REFUSED$ SETTLE-UNIT ;

\ Store a compile entry in NCOMP-DISPATCH:XT-CELL, the image-ABI cell the
\ tier 1 `:` dispatches through.
: DISPATCH! ( [ ptr u8 n -- ] -- )
   data-base NCOMP-DISPATCH:XT-CELL + xt! ;

\ Store COMPILE and NDEFINITION's callback words in the image-ABI cells.
: INSTALL ( -- )
   ['] CALLBACK-STUB! data-base NCOMP-DISPATCH:CALLBACK-STUB-CELL + xt!
   ['] CALLBACK-STUB@ data-base NCOMP-DISPATCH:CALLBACK-ENTRY-CELL + xt!
   ['] COMPILE DISPATCH! ;

;using
;package
