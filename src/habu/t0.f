\ t0.f - the tier 0 compiler's front end, package T0.
\
\ T0 compiles one definition's body text as BODYBUF holds it at `;`
\ (src/habu/definers.f DEF-COMPILE hands the same text to the compiler entry),
\ read as the checker's scan reported it: one row per token the checker read,
\ the definition's name first (THE TAPE). It emits through the seam
\ src/habu/t0-emit.f (package T0-EMIT) to the emitter selected there
\ (src/habu/t0-x64.f for x86-64); the emitter leaves the finished body in its
\ sink, and the caller seals it into NEMIT (T0-EMIT:SEAL).
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
\ and capture the body inside a recording unit whose observer lays the tape
\ and keeps the verdict, enforce the verdict, check the name and arity, walk the
\ body, seal the emission at the next code slot and publish it; a refusal is
\ reported and retracts the rows the definition recorded, and a tick operand no
\ record answers is reported as the engine's tick reports it (E-UNDEFINED). The
\ name is the tape's first row. T0 classifies no body for the host yet: each
\ one's reason is UNKNOWN, and a quotation's arity is unknown (-1 -1), as tier 1
\ answers for one it has none for. A definition while a target shadow is open is
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
\ THE EVENTS. A token is what the checker filed it as (CHECKER-TAPE's kinds):
\ a string literal (`s" c" ." s\" c\" .\"`) or a character literal (`[char]
\ char`); an integer, its value, or a real, its bits as one cell; a control
\ word (if else then begin until while repeat again do ?do loop +loop leave
\ unloop i j case of endof endcase exit recurse >r r> r@ [: ;]), by its place
\ in the checker's list (DO-CONTROL); a local's declaration, its group's `:}`
\ or a local read, by its bind sequence (THE LOCALS); `[']` and `is`, whose
\ operand row follows; a MATCH's family, each arm's variant and `OF`, its
\ `;MATCH`, and `construct`'s variant, with the family, tag and pads the
\ checker filed; or an operand its keyword consumed (`{:`, `MATCH`,
\ `construct` and its family), which compiles nothing, or a loaned field's
\ name, whose call T0 refuses (WORD-CALL). A name is, by the
\ record the checker bound it to: an operation T0 inlines, when that record is
\ the engine's seeded primitive of the operation (THE INLINED OPERATIONS): the
\ stack shuffles (dup drop swap over nip tuck rot -rot 2dup 2drop 2swap 2over
\ 2>r 2r> 2r@), + - * and or xor 1+ 1- negate invert lshift rshift, cells cell+
\ ptr-field, / and mod by a literal neither 0 nor -1 (DIV2), the comparisons,
\ @ and c@, and ! as a call to the primitive or one per cell (THE WIDTHS); a
\ word, as a call, or as the value it pushes when its definer fixed one (a
\ constant's number, a created word's or variable's DATA address; WORD-CALL);
\ an immediate (IMM-TOKEN says which pass). Anything else, a name bound to no
\ record among them, throws E-T0-UNSUPPORTED, and UNSUPPORTED$ names the token
\ until the next compile.
\
\ THE RECORDS. A name, a tick's operand and `is`'s operand lower from the
\ record the checker bound (CHECKER-TAPE's K-NAME, K-TICK and K-IS a0, the
\ record's dictionary index), as tier 1 lowers a call from the checker's
\ binding (src/compiler/native/hir-word.f RESOLVE-SITE). A package's own word
\ named like a primitive binds a record of its own and is a call. Two lookups
\ by spelling remain: IMM-TOKEN asks CHECKER-OWNER:PARSE-IMM? whether an
\ immediate is parse-neutral, and VALIDATE finds NFETCH-CHECK:TAGS. Otherwise
\ the spelling is only what a refusal names.
\
\ THE ORDINALS. The checker files what it proved about a token under that
\ token's ordinal in its report (src/core/checker.f SCAN-REPORT), and the tape
\ holds one row per ordinal: the name is 0, the signature reports nothing, each
\ other token is one, a string literal with its opener is one, `[char]` or
\ `char` with its operand is one, and a tick's or `is`'s operand is one after
\ its keyword. A quotation's first ordinal is recorded where it is skipped, and
\ the does> clause's follows its `does>`.
\
\ MATCH AND CONSTRUCT. `MATCH fam` reads its value as the cells the checker
\ proved at the family token, the tag on top, and opens a case over the tag.
\ Each arm, `variant of`, compares the tag with the variant's; the match drops
\ the tag and the pads the checker proved at the `of`, leaving the payload.
\ `;MATCH` is the default arm: a tag no arm matched calls the trap routine with
\ the family's bad-tag message (src/compiler/native/trap.f), as tier 1's
\ MATCH-TRAP. The family and each variant's tag and pads are the events the
\ checker filed, so no name is looked up. `construct fam v` pushes the
\ variant's pads as zeros, then its tag. A call to a word first pushes, as
\ zeros, the pads the checker proved the call's instantiation adds
\ (NDICT:CON-PADS): OPTION:NONE at option<pt>. A form whose facts are missing or
\ disagree, as tier 1's MATCH-SCAN refuses them, is E-T0-MATCH.
\
\ A STRING is one row, of its kind, whose span is the payload in the text; the
\ checker reports its body, the payload itself for a plain literal and the
\ decoded bytes for an escaped one (src/core/checker.f CHECKER-TAPE:STRING),
\ which the tape copies. Its bytes are interned (NSTR:INTERN-SOURCE), so equal
\ bodies share an address, and its address row files the source origin
\ interning answers. `s"` pushes the address and length, `c"` the address of a
\ count byte and the bytes, `."` passes both to `type`. A counted one past
\ COUNTED-MAX bytes is E-T0-LITERAL. A CHARACTER literal is one row whose a0 is
\ its operand's first byte.
\
\ `is W` stores the xt on the stack in deferred word W's cell: it pushes the
\ cell's address (NDICT:REC-DEFER-CELL), whose row files W's record as its
\ origin, and calls the engine's `xt!`, as tier 1's
\ (src/compiler/native/elaborate.f DO-IS). A W that is no deferred word is
\ E-T0-DEFER. Every deferred word has an arity: `defer W` without an effect is
\ refused where it is defined, under `0 set-check` too.
\
\ A QUOTATION `[: ... ;]` is a unit of its own in the same emission
\ (src/habu/t0-emit.f THE UNITS). At `[:` the body pushes the unit's address
\ and skips the rows through the matching `;]`; once the definition is
\ compiled, each quotation is compiled from its recorded first row (QORD), in
\ the order the `[:` tokens were met, nested ones after their parent. Each
\ unit starts with an empty virtual stack, no structure open, no loop level
\ and no local, as the checker reads a quotation: the enclosing loop's `i` and
\ the enclosing locals are out of its reach. A local group inside a quotation
\ never reaches T0 as one: the checker refuses it in a checked body
\ (E-BAD-LOCAL-SHAPE), and a TRUSTED: quotation's group arrives as plain names;
\ the group is refused at the first name bound to no record, `:}` at the latest
\ (E-T0-UNSUPPORTED).
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
\ 2>r 2r> 2r@ through their swap and >r composition. `>r` and `r>` move a
\ value's cells as one cell each would, so its cells sit on the return stack in
\ reverse and `r>` restores their order, in one return-stack move per VREGS
\ cells; `r@` and `2r@` read the cells `r>` and `2r>` would pop. `@` and `!`
\ move the cells the checker certified at the token, as tier 1's WIDE-LOAD and
\ WIDE-STORE (src/compiler/native/elaborate.f): `@` loads from the address
\ plus i cells for each cell, and `!` calls the primitive, the engine's guarded
\ store, once per cell with the address kept under the copies. Before it
\ loads, an `@` the checker filed a layout fact for (NFETCH:AT) calls
\ NFETCH-CHECK:TAGS with a copy of the address and the fact's descriptor, as
\ tier 1's VALIDATE-FETCH; a fact whose width is not the token's is
\ E-T0-BUNDLE. Any other operation T0 inlines over a width other than one
\ cell, and `@` or `!` under one cell, is E-T0-BUNDLE, as tier 1's DO-OP
\ refuses it E-NELAB-BUNDLE.
\
\ THE LOCALS. Each `{: ... :}` group binds its names once, left to right from
\ the deepest cell, into slots of the body's machine-stack frame (T0-EMIT:SLOTS,
\ SLOT-STORE, SLOT-LOAD); a read pushes every cell of the local. A local holds
\ as many cells as the checker certified for its bind (NFETCH:BIND-WIDTH), so
\ the caller captures the body's certificate (NFETCH:CAPTURE at base 0) before
\ the walk, and T0 never reads a type annotation. A bind past the
\ certificate's count, and a body that binds fewer than it counts, is
\ E-T0-BINDS: that certificate is not of this text; a body that binds nothing
\ reads none, as tier 1's BINDS-CK. A local is its bind sequence, which the
\ checker files at its name and at each read, in scope from its group to the
\ closer of the structure it binds in (else, then, repeat, until, again,
\ loop, +loop, endof, endcase), as the checker's CF-LOC-REST ends it; `while`
\ keeps the loop's names, as tier 1's borrow does. A dead local's slots go to
\ the next group.
\
\ THE CONTROL STACK holds the kind of each open structure, with the frame slots
\ the locals in scope held where its block opened: an `if` or `else`; a
\ `begin` and a `while`, which `while` puts under its `begin` as Forth's
\ orig/dest order has it, so `begin ... while ... while ... repeat then`
\ closes; a `do` level; a `case`, each `endof` above it and the open `of`; a
\ `MATCH`, with its family and the cells the checker proved at the family token,
\ tag on top (CS-FAM, CS-WID), each `endof` above it and the open `OF`. A
\ structure that does not close, and a loop word outside the loops it needs, is
\ E-T0-CONTROL, refused before the emitter is called: T0-EMIT's structured words
\ (src/habu/t0-emit.f CONTROL) keep the branches and trust this nesting.
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
require src/core/bytes.f
require src/compiler/native/checker-owner.f
require src/compiler/native/dict.f
require src/core/prefix-boundary.f
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
-9602 constant E-T0-LITERAL      \ a counted string too long
-9603 constant E-T0-IMMEDIATE    \ an immediate other than a parse-neutral one: UNSUPPORTED$
-9604 constant E-T0-BINDS        \ the body binds other than the certificate's count of locals
-9605 constant E-T0-SHADOW       \ a definition while a target shadow is open: no shadow path yet
-9606 constant E-T0-MATCH        \ a MATCH or construct the checker's facts do not bear out, as tier 1's E-NELAB-MATCH
-9607 constant E-T0-DEFER        \ `is` names no deferred word
-9608 constant E-T0-BUNDLE       \ an operation over a width it cannot move, as tier 1's E-NELAB-BUNDLE

private

\ ---- the tape ----------------------------------------------------------------
\ THE TAPE. Row k is the token the checker's scan reported as ordinal k (the
\ observer lays it, THE RUNTIME COMPILER): its span, an offset and a length in
\ the definition's text, and the event the checker filed for it, a kind of
\ CHECKER-TAPE's and its two arguments. A string row keeps its body's length,
\ and an escaped one a copy of its decoded body in BODIES, since the checker's
\ buffer outlives no token.
PTR-VARIABLE TXT
DYNAMIC-BUFFER ROW-OFF n
DYNAMIC-BUFFER ROW-LEN n
DYNAMIC-BUFFER ROW-KIND n
DYNAMIC-BUFFER ROW-A0 n
DYNAMIC-BUFFER ROW-A1 n
DYNAMIC-BUFFER ROW-U n               \ a string's body length
DYNAMIC-BUFFER ROW-AT n              \ where an escaped string's body starts in BODIES, else -1
DYNAMIC-BUFFER BODIES u8
variable BODIES-N
variable ROWS                        \ the rows laid
variable SPLIT                       \ a does> definer's `does>` row, or -1
-1 constant K-NONE                   \ a row the checker filed no event for: the `does>`
0 constant CF-QUOT-OPEN              \ a K-CONTROL row's a0 for `[:` (src/core/checker.f CF-TOK?)
1 constant CF-QUOT-CLOSE             \ and for `;]`
2 constant CF-IF                     \ `if`, and so on in the checker's list
3 constant CF-ELSE
4 constant CF-THEN
5 constant CF-CASE
6 constant CF-OF
7 constant CF-ENDOF
8 constant CF-ENDCASE
10 constant CF-BEGIN
11 constant CF-UNTIL
12 constant CF-AGAIN
13 constant CF-WHILE
14 constant CF-REPEAT
15 constant CF-DO
16 constant CF-?DO
17 constant CF-LOOP
18 constant CF-+LOOP
19 constant CF-I
20 constant CF-J
21 constant CF-EXIT
22 constant CF-LEAVE
23 constant CF-UNLOOP
24 constant CF-RECURSE
25 constant CF-TOR                   \ `>r`, then `r>` and `r@` (RS-KEYWORD)
26 constant CF-RFROM
27 constant CF-RFETCH
PTR-VARIABLE BAD
variable BAD-LEN
variable MISSING                     \ nonzero: the refused token is a tick operand no record answers
variable ORD                         \ the ordinal of the row the walk reads next
variable CUT                         \ the byte just past `does> `, or 0

: TXT$ ( -- ptr u8 ) TXT @ ;

\ Row k's span, empty past the tape.
: ROW$ ( n -- ptr u8 n ) {: k:n :}
   k ROWS @ >= if TXT$ 0 exit then
   TXT$ k ROW-OFF @ +  k ROW-LEN @ ;

\ The row the walk reads next, which it then passes.
: NEXT$ ( -- ptr u8 n )
   ORD @ ROW$
   ORD @ ROWS @ < if 1 ORD +! then ;

\ Whether row k is the `;]` that ends a quotation.
: QUOT-END? ( n -- bool ) {: k:n :}
   k ROW-KIND @ CHECKER-TAPE:K-CONTROL =  k ROW-A0 @ CF-QUOT-CLOSE =  and ;

\ Whether the unit ends at the next row: the tape does, or a quotation's `;]`.
: AT-END? ( -- bool )
   ORD @ ROWS @ >= if true exit then
   ORD @ QUOT-END? ;

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
DYNAMIC-BUFFER CS-LOC n              \ the locals' slots in scope where the block opened
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
   v 0 <> v -1 <> and ;

\ `/` or `mod` by a literal divisor neither 0 nor -1 (t0-emit.f DIVISION),
\ folded when the dividend is a constant too and else divided in place by the
\ seam word; whether it claimed the token. Any other divisor is left to the
\ call of the kernel row. The fold runs the engine's own `/` or `mod`, which
\ truncates toward zero as every kernel's rows do.
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
\ A local is its bind sequence, the checker's number for it in the unit's
\ certificate: a K-LOCAL-DECL row names the next one, and a K-LOCAL-REF row the
\ one it reads.
DYNAMIC-BUFFER L-SLOT n              \ by sequence: the first of its frame slots
DYNAMIC-BUFFER L-WIDTH n             \ by sequence: its cells
variable L-N                         \ the frame slots the locals in scope hold
variable G-CELLS                     \ the cells the open group's names hold so far
variable BINDS                       \ binds so far: the next one's certificate sequence
variable CLAUSE                      \ a does> definer's clause unit, else -1

\ Name local seq of the open group, declared by the token at offset at, on the
\ slots after the group's earlier names.
: DECLARE ( n n -- ) {: seq:n at:n :}
   seq at NFETCH:BINDS >= if E-T0-BINDS throw then
   at seq NFETCH:BIND-WIDTH {: w:n :}
   seq 1+ BINDS !
   seq 1+ L-SLOT-RESERVE  seq 1+ L-WIDTH-RESERVE
   L-N @ G-CELLS @ +  seq L-SLOT !  w seq L-WIDTH !
   w G-CELLS +! ;

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

\ The `:}` that binds the open group: its names come into scope once their
\ cells are bound.
: CLOSE-GROUP ( -- )
   L-N @ {: base:n :}
   G-CELLS @ {: cells:n :}
   base cells + T0-EMIT:SLOTS
   base cells BIND-CELLS
   base cells + L-N !
   0 G-CELLS ! ;

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

\ Whether the checker filed the next row as the control word `if`, `while` or
\ `until`.
: BRANCH-NEXT? ( -- bool )
   ORD @ {: k:n :}
   k ROWS @ >= if false exit then
   k ROW-KIND @ CHECKER-TAPE:K-CONTROL <> if false exit then
   k ROW-A0 @ {: code:n :}
   code CF-IF =  code CF-WHILE = or  code CF-UNTIL = or ;

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

\ The locals' slots in scope where the structure last popped opened.
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

DYNAMIC-BUFFER RS-V T0-EMIT:vreg     \ reserved to T0-EMIT:VREGS by RESET: one
                                     \ return-stack move's vregs, the deepest
                                     \ slot first

\ How many of n cells one move carries: one per vreg.
: RS-PART ( n -- n ) T0-EMIT:VREGS min ;

\ The top c cells, the top into the deepest slot, pushed by one move.
: TOR-PART ( n -- ) {: c:n :}
   c 0 ?do TAKE i RS-V ! loop
   0 RS-V c T0-EMIT:TOR
   c 0 ?do i RS-V @ RELEASE loop ;

\ c free vregs in RS-V, with room for an entry each.
: RS-ALLOC ( n -- ) {: c:n :}
   begin VS-N @ c + T0-EMIT:VREGS > while EVICT repeat
   c 0 ?do ALLOC i RS-V ! loop ;

\ RS-V's c vregs pushed in the order `r>` leaves cells: the top slot's deepest.
: RS-PUSH ( n -- ) {: c:n :}
   c 0 ?do c 1- i - RS-V @ PUSH-REG loop ;

: RFROM-PART ( n -- ) {: c:n :}
   c RS-ALLOC  0 RS-V c T0-EMIT:RFROM  c RS-PUSH ;

\ The c cells k under the top, copied as RFROM-PART would pop them.
: RFETCH-PART ( n n -- ) {: c:n k:n :}
   c RS-ALLOC  0 RS-V c k T0-EMIT:RFETCH  c RS-PUSH ;

\ `>r` and `r>` of n cells, each cell as one `>r` or `r>`, a value's cells
\ landing on the return stack in reverse; a move per RS-PART cells.
: TOR-CELLS ( n -- )
   begin dup 0 > while dup RS-PART dup TOR-PART - repeat drop ;

: RFROM-CELLS ( n -- )
   begin dup 0 > while dup RS-PART dup RFROM-PART - repeat drop ;

\ The top n cells copied as RFROM-CELLS would pop them.
: RFETCH-CELLS ( n -- ) {: n:n :}
   0 begin dup n < while dup n swap - RS-PART  2dup swap RFETCH-PART  + repeat drop ;

\ The cells of the two operands.
: PAIR-CELLS ( -- n ) 0 WS @ 1 WS @ + ;

\ `2>r` is `swap >r >r`, `2r>` is `r> r> swap`, `2r@` is `2r> 2dup 2>r`: over
\ one-cell operands one move, the deeper operand in the deeper slot, as the
\ primitives order them (habu1.f B2TOR).
: 2TOR ( -- ) SWAP-OP PAIR-CELLS TOR-CELLS ;
: 2RFROM ( -- ) PAIR-CELLS RFROM-CELLS  WS-SWAP SWAP-OP WS-SWAP ;
: 2RFETCH ( -- ) PAIR-CELLS RFETCH-CELLS  WS-SWAP SWAP-OP WS-SWAP ;

\ The code record rec offers a compiled reference, or 0: a retired record's
\ code nothing reaches (src/compiler/native/dict.f REC-CODE).
: REC-START ( ptr n -- n ) {: rec:ptr :}
   rec XREF-RETIRED? if 0 exit then
   rec XREF-START ;

\ `[']` bound to record r: its operand's code address. The checker refuses a
\ tick of an internal word in a checked body (E-CAP-TRUSTED) and admits one
\ under TRUSTED:, as tier 1 does. Bound to none, the operand is
\ undefined (MISSING).
: DO-TICK ( n -- ) {: r:n :}
   NEXT$ {: a:ptr u:n :}
   r 0< if -1 MISSING !  a u UNSUPPORTED then
   r XREF-REC REC-START {: xt:n :}
   xt 0= if a u UNSUPPORTED then
   ROOM ALLOC {: r:T0-EMIT:vreg :}
   xt r T0-EMIT:ADDR-CODE
   r PUSH-REG ;

\ Character row k: its operand's first byte, the row's a0, as tier 1 compiles
\ it (the JIT compiles `[char]` only): `[char] é` is the 195 that begins its
\ UTF-8.
: DO-CHAR ( n -- ) ROW-A0 @ PUSH-CONST ;

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

\ ---- whole-width memory ------------------------------------------------------
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

\ `!` at a, whose primitive's entry is to. The wide store's calls never leave
\ as a tail: each stores a copy of one cell, so the w cells and the address are
\ still on the stack after the last.
: DO-STORE ( ptr u8 n -- ) {: a:ptr to:n :}
   a CELLS0 {: w:n :}
   w 1 < if E-T0-BUNDLE throw then
   a TXT$ - {: loc:n :}
   w 1 = if to loc DO-CALL exit then
   w 0 ?do
      w i - DEEP  1 DEEP  i ADDR+
      SPILL-ALL  to loc T0-EMIT:CALL
   loop
   w 1+ negate T0-EMIT:DSP+ ;

\ ---- strings -----------------------------------------------------------------
255 constant COUNTED-MAX            \ a counted string's length is one byte

DYNAMIC-BUFFER STR-BUF u8           \ a decoded or counted body before interning

\ Whether a row of this kind is a string literal.
: STRING-KIND? ( n -- bool ) {: kind:n :}
   kind CHECKER-TAPE:K-STRING =  kind CHECKER-TAPE:K-COUNTED-STRING = or
   kind CHECKER-TAPE:K-PRINTED-STRING = or ;

\ String row k's interned address, its source origin and its length. A plain or
\ empty body interns from the text; a decoded or counted one is built in
\ STR-BUF first.
: INTERN-BODY ( n bool -- n n n ) {: k:n counted:bool :}
   TXT$ k ROW-OFF @ + {: s:ptr :}
   k ROW-AT @ {: body:n :}
   k ROW-U @ {: u:n :}
   counted u COUNTED-MAX > and if E-T0-LITERAL throw then
   body 0< u 0= or counted 0= and if s u NSTR:INTERN-SOURCE u exit then
   counted if 1 else 0 then {: at:n :}
   at u + STR-BUF-RESERVE
   body 0< if s at STR-BUF u BYTE-COPY else body BODIES at STR-BUF u BYTE-COPY then
   counted if u 0 STR-BUF c! then
   0 STR-BUF at u + NSTR:INTERN-SOURCE u ;

\ `."` calls the engine's `type` whatever the source's package scope, as tier
\ 1 does (src/compiler/native/elaborate.f TYPE-ENTRY).
TRUSTED: TYPE-ENTRY ( -- n ) ['] type ;

\ String row k. The payload's offset is the call's for `."`, as tier 1's
\ (src/core/checker.f SPAY-B).
: DO-STRING ( n -- ) {: k:n :}
   k ROW-KIND @ {: kind:n :}
   kind CHECKER-TAPE:K-COUNTED-STRING = {: counted:bool :}
   k counted INTERN-BODY {: v:n o:n u:n :}
   v o PUSH-ADDR
   counted if exit then
   u PUSH-CONST
   kind CHECKER-TAPE:K-PRINTED-STRING = if TYPE-ENTRY k ROW-OFF @ DO-CALL then ;

\ ---- is ----------------------------------------------------------------------
\ `is` stores through the engine's `xt!` whatever the source's package scope, as
\ tier 1 does (src/compiler/native/elaborate.f XT-STORE-ENTRY).
TRUSTED: XT-STORE-ENTRY ( -- n ) ['] xt! ;

\ `is` bound to record r, at offset loc: the xt on the stack goes into r's cell.
: DO-IS ( n n -- ) {: r:n loc:n :}
   NEXT$ 2drop                       \ the operand
   r 0< if E-T0-DEFER throw then
   r XREF-REC NDICT:REC-DEFER-CELL {: cell:n :}
   cell 0= if E-T0-DEFER throw then
   cell r PUSH-ADDR
   XT-STORE-ENTRY loc DO-CALL ;

\ ---- quotations --------------------------------------------------------------
variable NQ                         \ units met so far, the definition's included
DYNAMIC-BUFFER QORD n               \ each quotation's first ordinal
variable NESTED                     \ the `[:` rows skipped inside quotations so far

\ How row k, in a quotation being skipped, moves the nesting depth.
: SKIP-ROW ( n -- n ) {: k:n :}
   k ROWS @ >= if E-T0-CONTROL throw then
   k ROW-KIND @ CHECKER-TAPE:K-CONTROL <> if 0 exit then
   k ROW-A0 @ CF-QUOT-OPEN = if 1 NESTED +! 1 exit then
   k QUOT-END? if -1 exit then
   0 ;

\ Past the `;]` that closes the quotation just opened.
: SKIP-QUOT ( -- )
   1 begin dup 0 > while ORD @ SKIP-ROW +  1 ORD +! repeat drop ;

\ Push unit k's address.
: PUSH-UNIT ( n -- ) {: k:n :}
   ROOM ALLOC {: r:T0-EMIT:vreg :}
   k r T0-EMIT:ADDR-QUOT
   r PUSH-REG ;

\ Push the next unit's address and skip its rows, compiled after this unit.
: DO-QUOT ( -- )
   NQ @ {: k:n :}
   k 1+ QORD-RESERVE
   ORD @ k QORD !
   k 1+ NQ !
   SKIP-QUOT
   k PUSH-UNIT ;

\ ---- the does> split ---------------------------------------------------------
variable EMPTY-CLAUSE               \ whether no row follows the split: the clause is empty
variable CLAUSE-ORD                 \ the checker's ordinal of the clause's first token

\ The parent leaves through `does-patch` whatever the source's package scope,
\ as tier 1 does (src/compiler/native/elaborate.f DOES-PATCH-ENTRY).
TRUSTED: DOES-PATCH-ENTRY ( -- n ) ['] does-patch ;

\ Whether no row follows the split: the clause is empty.
: CLAUSE-EMPTY? ( -- bool ) CLAUSE-ORD @ ROWS @ >= ;

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

\ A MATCH arm's `OF`, row at, after its variant pushed the tag it compares:
\ the match drops the tag and the pads the checker proved at the `OF`, leaving
\ the payload, and the mismatch keeps the value for the next arm.
: ARM-OF ( n -- ) {: at:n :}
   ARMS-AT {: k:n :}
   k CS-KIND @ CS-MATCH = MATCH-CK
   at NDICT:MATCH-CELLS {: pads:n :}
   pads NDICT:MATCH-NONE <> MATCH-CK
   k CS-WID @ 1- pads - {: pay:n :}
   pay 0 >= MATCH-CK
   at NDICT:MATCH-PAYLOAD drop pay = MATCH-CK
   OF-TEST
   pads 0 ?do DROP1 loop ;

\ `MATCH fam` at fam's row, at: the value is the cells the checker proved
\ there, at least the family's declared width.
: DO-MATCH ( n n -- ) {: fam:n at:n :}
   at NDICT:MATCH-CELLS {: w:n :}
   w NDICT:MATCH-NONE <> MATCH-CK
   w fam NFAM:WIDTH >= MATCH-CK
   T0-EMIT:CASE,
   CS-N @ {: k:n :}
   CS-MATCH L-N @ CS-PUSH
   k 1+ CS-FAM-RESERVE  k 1+ CS-WID-RESERVE
   fam k CS-FAM !  w k CS-WID ! ;

\ `construct fam v` at v's row, at: the variant's pads, the family's and those
\ the checker proved the instantiation adds there, then its tag, as tier 1's
\ DO-MAKE-BUNDLE; the payload is already on the stack.
: DO-CONSTRUCT ( n n n -- ) {: tag:n pads:n at:n :}
   at NDICT:CALL-CELLS drop 0 >= MATCH-CK
   pads at NDICT:CON-PADS +  0 ?do 0 PUSH-CONST loop
   tag PUSH-CONST ;

\ ---- the inlined operations --------------------------------------------------
\ THE INLINED OPERATIONS. Each operation T0 inlines is one of the engine's
\ seeded primitives, a record below the source prefix's first
\ (CORE-PREFIX:FIRST-RECORD), and OP-OF maps such a record's dictionary index to
\ the operation's code in INLINE, 0 for none. OPS-MAP fills it once, when T0
\ loads, by each operation's name; a record at or above the boundary is never
\ an operation, so a package's own word of that name is a call. OP-OF is
\ dictionary storage, so a saved image keeps it filled.
CORE-PREFIX:FIRST-RECORD constant OP-N   \ the records OP-OF holds
OP-N TYPED-BUFFER OP-OF n
variable OP-HITS                     \ the seeded records OP found by the name
variable OP-REC                      \ the last of them

\ End the load: operation a u names no single seeded record.
: OP-DIE ( ptr u8 n -- )
   {: a:ptr u:n :}
   s" : T0: an inlined operation names no single seeded record" {: m:ptr k:n :}
   u k + STR-BUF-RESERVE
   a 0 STR-BUF u BYTE-COPY
   m u STR-BUF k BYTE-COPY
   0 STR-BUF u k + 76 die ;

\ Map the one seeded record named a u to code.
: OP ( ptr u8 n n -- )
   {: a:ptr u:n code:n :}
   0 OP-HITS !
   OP-N 0 ?do
      i XREF-REC a u XREF-MATCH? if 1 OP-HITS +!  i OP-REC ! then
   loop
   OP-HITS @ 1 <> if a u OP-DIE then
   code OP-REC @ OP-OF ! ;

: OPS-MAP ( -- )
   s" +" 1 OP  s" -" 2 OP  s" *" 3 OP  s" /" 4 OP  s" @" 5 OP
   s" !" 6 OP  s" =" 7 OP  s" <" 8 OP  s" >" 9 OP  s" or" 10 OP
   s" 1+" 11 OP  s" 1-" 12 OP  s" c@" 13 OP  s" 0=" 14 OP
   s" 0<" 15 OP  s" <>" 16 OP  s" <=" 17 OP  s" >=" 18 OP
   s" dup" 19 OP  s" rot" 20 OP  s" nip" 21 OP  s" 2>r" 22 OP
   s" 2r>" 23 OP  s" 2r@" 24 OP  s" and" 25 OP  s" mod" 26 OP
   s" xor" 27 OP  s" drop" 28 OP  s" swap" 29 OP  s" over" 30 OP
   s" tuck" 31 OP  s" -rot" 32 OP  s" 2dup" 33 OP  s" 2drop" 34 OP
   s" 2swap" 35 OP  s" 2over" 36 OP  s" cells" 37 OP  s" cell+" 38 OP
   s" negate" 39 OP  s" invert" 40 OP  s" lshift" 41 OP
   s" rshift" 42 OP  s" ptr-field" 43 OP ;
OPS-MAP

\ The code of the operation record r is, or 0.
: OP-CODE ( n -- n )
   {: r:n :}
   r OP-N >= if 0 exit then
   r OP-OF @ ;

\ An operation over one cell: the checker certified no wider operand at a.
: CELL1 ( ptr u8 -- )
   {: a:ptr :}
   a CELLS0 1 <> if E-T0-BUNDLE throw then ;

\ Operation code at a, whose primitive's entry is to: false when it is not
\ inlined after all (DIV2), so the name is a call.
: INLINE ( ptr u8 n n -- bool )
   {: a:ptr code:n to:n :}
   code case
      1 of a CELL1 [: + ;] T0-EMIT:OP-ADD true ALU2 true endof
      2 of a CELL1 [: - ;] T0-EMIT:OP-SUB false ALU2 true endof
      3 of a CELL1 MUL2 true endof
      4 of a CELL1 [: / ;] [: T0-EMIT:DIV-LIT ;] DIV2 endof
      5 of a DO-FETCH true endof
      6 of a to DO-STORE true endof
      7 of a CELL1 [: = bool>n ;] T0-EMIT:CC-EQ CMP2 true endof
      8 of a CELL1 [: < bool>n ;] T0-EMIT:CC-LT CMP2 true endof
      9 of a CELL1 [: > bool>n ;] T0-EMIT:CC-GT CMP2 true endof
      10 of a CELL1 [: or ;] T0-EMIT:OP-OR true ALU2 true endof
      11 of a CELL1 1 PUSH-CONST [: + ;] T0-EMIT:OP-ADD true ALU2 true endof
      12 of a CELL1 1 PUSH-CONST [: - ;] T0-EMIT:OP-SUB false ALU2 true endof
      13 of a CELL1 T0-EMIT:BYTE-W FETCH true endof
      14 of a CELL1 [: 0= bool>n ;] T0-EMIT:CC-EQ CMP0 true endof
      15 of a CELL1 [: 0< bool>n ;] T0-EMIT:CC-LT CMP0 true endof
      16 of a CELL1 [: <> bool>n ;] T0-EMIT:CC-NE CMP2 true endof
      17 of a CELL1 [: <= bool>n ;] T0-EMIT:CC-LE CMP2 true endof
      18 of a CELL1 [: >= bool>n ;] T0-EMIT:CC-GE CMP2 true endof
      19 of [: DUP1 ;] a 1 s" 00" SHUFFLE true endof
      20 of [: ROT1 ;] a 3 s" 102" SHUFFLE true endof
      21 of [: NIP1 ;] a 2 s" 0" SHUFFLE true endof
      22 of a 2 WIDTHS 2TOR true endof
      23 of a 2 WIDTHS 2RFROM true endof
      24 of a 2 WIDTHS 2RFETCH true endof
      25 of a CELL1 [: and ;] T0-EMIT:OP-AND true ALU2 true endof
      26 of a CELL1 [: mod ;] [: T0-EMIT:MOD-LIT ;] DIV2 endof
      27 of a CELL1 [: xor ;] T0-EMIT:OP-XOR true ALU2 true endof
      28 of [: DROP1 ;] a 1 s" " SHUFFLE true endof
      29 of [: SWAP1 ;] a 2 s" 01" SHUFFLE true endof
      30 of [: OVER1 ;] a 2 s" 101" SHUFFLE true endof
      31 of [: TUCK1 ;] a 2 s" 010" SHUFFLE true endof
      32 of a 3 s" 021" MOVE-OP true endof
      33 of a 2 s" 1010" MOVE-OP true endof
      34 of a 2 s" " MOVE-OP true endof
      35 of a 4 s" 1032" MOVE-OP true endof
      36 of a 4 s" 321032" MOVE-OP true endof
      37 of a CELL1 CELLS-OP true endof
      38 of a CELL1 CELL PUSH-CONST [: + ;] T0-EMIT:OP-ADD true ALU2 true endof
      39 of a CELL1 [: negate ;] [: T0-EMIT:NEG ;] UNARY true endof
      40 of a CELL1 [: invert ;] [: T0-EMIT:NOT ;] UNARY true endof
      41 of a CELL1 [: lshift ;] T0-EMIT:SH-LEFT SHIFT2 true endof
      42 of a CELL1 [: rshift ;] T0-EMIT:SH-RIGHT SHIFT2 true endof
      43 of a CELL1 CELLS-OP [: + ;] T0-EMIT:OP-ADD true ALU2 true endof
      E-T0-UNSUPPORTED throw
   endcase ;

\ A K-CONTROL row at a: code is the marker's place in the checker's list
\ (src/core/checker.f CF-TOK?, then RS-KEYWORD's `>r` `r>` `r@`). The `;]`
\ ends its unit before it is compiled (BODY), and a stray `;MATCH` (9) never
\ certifies.
: DO-CONTROL ( ptr u8 n -- ) {: a:ptr code:n :}
   code case
      CF-QUOT-OPEN of DO-QUOT endof
      CF-IF of DO-IF endof
      CF-ELSE of DO-ELSE endof
      CF-THEN of DO-THEN endof
      CF-CASE of DO-CASE endof
      CF-OF of DO-OF endof
      CF-ENDOF of DO-ENDOF endof        \ a case's `endof` or a MATCH's
      CF-ENDCASE of DO-ENDCASE endof
      CF-BEGIN of DO-BEGIN endof
      CF-UNTIL of DO-UNTIL endof
      CF-AGAIN of DO-AGAIN endof
      CF-WHILE of DO-WHILE endof
      CF-REPEAT of DO-REPEAT endof
      CF-DO of DO-DO endof
      CF-?DO of DO-?DO endof
      CF-LOOP of DO-LOOP endof
      CF-+LOOP of DO-+LOOP endof
      CF-I of [: T0-EMIT:LOOP-I ;] 1 DO-INDEX endof
      CF-J of [: T0-EMIT:LOOP-J ;] 2 DO-INDEX endof
      CF-EXIT of DO-EXIT endof
      CF-LEAVE of DO-LEAVE endof
      CF-UNLOOP of DO-UNLOOP endof
      CF-RECURSE of DO-RECURSE endof
      CF-TOR of a CELLS0 TOR-CELLS endof
      CF-RFROM of a CELLS0 RFROM-CELLS endof
      CF-RFETCH of a CELLS0 RFETCH-CELLS endof
      E-T0-CONTROL throw
   endcase ;

\ ---- one token ---------------------------------------------------------------
\ An immediate the capture holds. A parse-neutral one ran as the engine read it
\ (src/habu/definers.f DEF-IMMEDIATE) and left only its own token, its operands
\ read from the input and text it evaluated captured after it, so T0 passes
\ over the token as tier 1 does (src/compiler/native/elaborate.f WALK-SKIP?).
\ Any other is refused by name.
: IMM-TOKEN ( ptr u8 n -- ) {: a:ptr u:n :}
   a u CHECKER-OWNER:PARSE-IMM? if exit then
   a u E-T0-IMMEDIATE REFUSE ;

\ A word bound to record r, whose entry is to and definer kind is kind, at
\ offset loc. One that builds a value of a wide instantiation first pushes the
\ cells the checker proved the instantiation adds at the token
\ (NDICT:CON-PADS), as tier 1's DO-CALL; the row just read is ordinal ORD - 1.
\ A fixed word, one whose definer stamped what its body pushes, is then that
\ value, as tier 1 folds it (src/compiler/native/hir-word.f RESOLVE-SITE): a
\ constant's number, which CMP2 and the ALU take as an immediate, or a created
\ word's or variable's DATA address, filed with r so publication relocates it.
\ A non-empty does> clause clears the stamp, so a does> child is called. Any
\ other word is called. A field-loan site, a call the checker filed a loaned
\ field's offset and extent for (NDICT:FIELD-SPAN), is refused: tier 1 pushes
\ both before the call (src/compiler/native/elaborate.f DO-CALL).
: WORD-CALL ( ptr u8 n n n n n -- )
   {: a:ptr u:n to:n kind:n r:n loc:n :}
   ORD @ 1- NDICT:FIELD-SPAN drop 0 >= if a u UNSUPPORTED then
   ORD @ 1- NDICT:CON-PADS 0 ?do 0 PUSH-CONST loop
   kind DKIND:VAL = if to NDICT:FIXED-VAL NDICT:BOUND-VALUE PUSH-CONST exit then
   kind DKIND:ADDR = if to NDICT:FIXED-ADDR NDICT:BOUND-VALUE  r PUSH-ADDR exit then
   to loc DO-CALL ;

\ A name a u bound to record r, at offset loc: an operation T0 inlines, an
\ immediate, or a word a compiled call may branch to (NDICT:INT-CALL? for an
\ internal one), as RESOLVE-SITE lowers one. Bound to none, it is refused.
: NAME-TOKEN ( ptr u8 n n n -- )
   {: a:ptr u:n r:n loc:n :}
   r 0< if a u UNSUPPORTED then
   r XREF-REC {: rec:ptr :}
   rec REC-START {: to:n :}
   r OP-CODE {: code:n :}
   code 0<> if a code to INLINE if exit then then
   rec XREF-FLAGS {: flags:n :}
   flags DNAME-IMM and 0<> if a u IMM-TOKEN exit then
   flags DNAME-INT and 0<> if flags NDICT:INT-CALL? 0= if a u UNSUPPORTED then then
   to 0= if a u UNSUPPORTED then
   a u to flags DKIND:MASK and r loc WORD-CALL ;

\ The row at the cursor, which the walk then passes, by the event the checker
\ filed for it (THE EVENTS). A string or character literal's span is its
\ payload; a name lowers from its record (THE RECORDS).
: COMPILE-TOKEN ( -- )
   ORD @ {: k:n :}
   k ROW-KIND @ {: event:n :}
   k ROW-A0 @ {: a0:n :}
   NEXT$ {: a:ptr u:n :}
   a TXT$ - {: loc:n :}
   event STRING-KIND? if k DO-STRING exit then
   event case
      CHECKER-TAPE:K-NAME of a u a0 loc NAME-TOKEN endof
      CHECKER-TAPE:K-INT of a0 PUSH-CONST endof
      CHECKER-TAPE:K-REAL of a0 PUSH-CONST endof
      CHECKER-TAPE:K-CHAR of k DO-CHAR endof
      CHECKER-TAPE:K-CONTROL of a a0 DO-CONTROL endof
      CHECKER-TAPE:K-LOCAL-DECL of a0 loc DECLARE endof
      CHECKER-TAPE:K-LOCAL-CLOSE of CLOSE-GROUP endof
      CHECKER-TAPE:K-LOCAL-REF of a0 DO-LOCAL endof
      CHECKER-TAPE:K-OPERAND of endof
      CHECKER-TAPE:K-TICK of a0 DO-TICK endof
      CHECKER-TAPE:K-IS of a0 loc DO-IS endof
      CHECKER-TAPE:K-MATCH of a0 k DO-MATCH endof
      CHECKER-TAPE:K-MATCH-ARM of a0 PUSH-CONST endof   \ the variant's tag
      CHECKER-TAPE:K-MATCH-OF of k ARM-OF endof
      CHECKER-TAPE:K-MATCH-END of loc ARMS-AT CLOSE-MATCH endof
      CHECKER-TAPE:K-CONSTRUCT of a0 k ROW-A1 @ k DO-CONSTRUCT endof
      a u UNSUPPORTED
   endcase ;

\ ---- one unit ----------------------------------------------------------------
\ Compile rows to the end of the unit: whether a `;]` ended it rather than
\ the end of the tape or the split's `does>`.
: BODY ( -- bool )
   begin
      ORD @ ROWS @ >= if false exit then
      ORD @ SPLIT @ = if false exit then
      ORD @ QUOT-END? if 1 ORD +! true exit then
      COMPILE-TOKEN
   again ;

\ Begin a unit with nothing open and no local in scope.
: RESET ( -- )
   T0-EMIT:VREGS VS-KIND-RESERVE  T0-EMIT:VREGS VS-VAL-RESERVE  T0-EMIT:VREGS RS-V-RESERVE
   0 VS-N !  ALL-FREE FREE !  0 CS-N !  0 DO-N !  0 L-N !  0 TAILED !
   0 DEAD !  0 PENDING !  0 G-CELLS ! ;

: FINISH ( -- )
   CS-N @ 0<> if E-T0-CONTROL throw then
   TAILED @ 0= if SPILL-ALL T0-EMIT:RETURN then
   T0-EMIT:CLOSE ;

\ Quotation unit k, from its first row through its `;]`.
: QUOT ( n -- ) {: k:n :}
   k QORD @ ORD !
   RESET
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

\ The does> clause, unit k, from the split to the end of the tape, with its
\ own locals and its own certificate at base CUT. An empty clause's unit is
\ still built, with no address bound to it: the parent pushed 0.
: DO-CLAUSE ( n -- ) {: k:n :}
   CLAUSE-ORD @ ORD !  k 1+ NQ !  RESET  0 BINDS !
   EMPTY-CLAUSE @ if T0-EMIT:UNIT-OPEN else k T0-EMIT:QUOT-OPEN then
   BODY if E-T0-CONTROL throw then
   FINISH
   CUT @ BINDS-CK ;

\ Compile the staged body from its tape, split at NDEFINITION:M-DOES when it
\ is not 0, into the emitter's sink: the definition, its quotations, then the
\ clause and its quotations. The name is row 0, so the body starts at row 1.
: WALK ( -- )
   NDEFINITION:SRC$ drop TXT !
   NDEFINITION:M-DOES @ {: cut:n :}
   1 NQ !  0 BINDS !
   cut CUT !  -1 CLAUSE !  0 NESTED !
   cut 0<> if CLAUSE-EMPTY? else false then EMPTY-CLAUSE !
   1 ORD !
   RESET
   T0-EMIT:OPEN
   BODY if E-T0-CONTROL throw then
   cut 0<> if
      ORD @ SPLIT @ <> if E-T0-CONTROL throw then
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

using NDEFINITION
private

\ ---- the runtime compiler ----------------------------------------------------
\ The scan's recording unit latches the facts the capture and the call targets
\ read. T0's observer keeps the verdict the scan reached, as tier 1's tape does
\ (src/compiler/native/feed.f ON-DONE), and lays the tape the walk reads (THE
\ TAPE): a row per token the scan reports, as it reports it, and at the scan's
\ end each row's event, which the checker answers only then.
\ A does> definer is scanned twice: its head (CHECK-DOES-HEAD), whose text
\ starts the definition's, then its clause (CHECK-DOES-CLAUSE), whose text
\ starts at M-DOES. The clause's ordinals go on from the head's, one past for
\ the `does>` between them, which the checker files nothing for
\ (CHECKER-OWNER:TAPE-ADVANCE), and its token offsets count from the clause's
\ start. So a scan of text at the definition's start begins the tape, and the
\ clause's scan first lays the `does>` row, the split, then its own rows at
\ offsets from M-DOES: one tape in text order whose ordinals are the checker's.
variable OBSERVER-ID
variable SCAN-VERDICT
PTR-VARIABLE SCAN-TXT                \ the text the scan reads
variable SCAN-BASE                   \ its offset in the definition's text
variable SCAN-LO                     \ the ordinal of its first row

\ Lay the next row, of no event yet, at offset off for u bytes: its ordinal.
: LAY-ROW ( n n -- n ) {: off:n u:n :}
   ROWS @ {: k:n :}
   k 1+ {: m:n :}
   m ROW-OFF-RESERVE  m ROW-LEN-RESERVE  m ROW-KIND-RESERVE  m ROW-A0-RESERVE
   m ROW-A1-RESERVE  m ROW-U-RESERVE  m ROW-AT-RESERVE
   off k ROW-OFF !  u k ROW-LEN !  K-NONE k ROW-KIND !  0 k ROW-A0 !  0 k ROW-A1 !
   0 k ROW-U !  -1 k ROW-AT !
   m ROWS !
   k ;

\ Copy an escaped string's body for row k.
: KEEP-BODY ( ptr u8 n n -- ) {: a:ptr u:n k:n :}
   BODIES-N @ {: at:n :}
   at u + BODIES-RESERVE
   a at BODIES u BYTE-COPY
   at k ROW-AT !
   at u + BODIES-N ! ;

: ON-SCAN ( ptr u8 n -- )
   drop {: a:ptr :}
   a SCAN-TXT !
   a SRC$ drop - {: base:n :}
   base SCAN-BASE !
   base 0= if
      0 ROWS !  0 BODIES-N !  -1 SPLIT !
   else
      base DOES-SPAN -  DOES-SPAN 1-  LAY-ROW SPLIT !
      ROWS @ CLAUSE-ORD !
   then
   ROWS @ SCAN-LO ! ;

\ A token's row. A string's body is copied when it is not the text at its
\ offset, which an escaped one's is not. Row 0, the definition's name, names
\ the unit from here on, as tier 1's KEEP-TAPE-NAME.
: ON-TOKEN ( ptr u8 n n n n n -- )
   drop {: a:ptr u:n off:n ru:n kind:n :}
   SCAN-TXT @ off + {: at:ptr :}
   SCAN-BASE @ off + ru LAY-ROW {: k:n :}
   kind STRING-KIND? if
      u k ROW-U !
      a at <> if a u k KEEP-BODY then
   then
   k 0= if at NAME-A !  ru NAME-U ! then ;

: ON-DONE ( ptr u8 n n -- )
   SCAN-VERDICT !  2drop
   ROWS @ SCAN-LO @ ?do
      i CHECKER-OWNER:TAPE-EVENT  i ROW-A1 !  i ROW-A0 !  i ROW-KIND !
   loop ;

: OBSERVE ( -- )
   OBSERVER-ID BYTE-VIEW NULL-PTR BYTE-VIEW -
   [: ON-SCAN ;] [: ON-TOKEN ;] [: ON-DONE ;]
   CHECKER-OWNER:TAPE-INSTALL ;

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
   WALK  CLAUSE @ M-DOES-FUN !
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
   ndict@ {: before:n :}
   SCAN
   VERDICT-CK
   NSHADOW:OPEN? if E-T0-SHADOW throw then
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
