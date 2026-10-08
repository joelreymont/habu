\ native-emit.f - checked ARM64 instruction-emission tests.
\
\ Proves the contract of src/compiler/native/emit.f: an accepted straight-line
\ A64IR module becomes exactly the ARM64 instructions its operations are, placed
\ little-endian in a buffer, with one source-map row per instruction tying its
\ byte offset back to the span of the operation it came from; and a module this
\ leaf cannot emit, a register assignment nobody accepted, one accepted for
\ another module, one a later allocation has replaced, a machine these
\ instructions are not for, and a reader asking about an emission that never
\ happened are each refused by name.
\
\ WHY THE BYTES ARE EXECUTED AND NOT ONLY COMPARED. A table of expected words is
\ necessary and not sufficient: it can only disagree with an emitter that changed,
\ never with one that was always wrong, because the expected words and the
\ emitter can be wrong in the same way. The shapes are therefore also published
\ into the engine's own code space and CALLED as leaf routines, with the
\ arguments the source-level arithmetic takes and its answer compared. Those
\ executing cases run in test/compiler/native-emit-run-child.f, a window child
\ this suite starts (RUN-CHILD-CASE below), which builds the same shapes from
\ test/compiler/native-emit-shapes.f and emits them with the emitter loaded from
\ the same sources inside the whitebox window; the sealed
\ product's baked emitter runs through the real load path, which compiles every
\ colon definition through src/compiler/native/compiler.f, whose ARM64 rows are
\ src/arch/arm64/passes.f over A64EMIT. The byte cases stay here, on the product
\ engine, where they exercise the emitter it bakes. The header of
\ test/compiler/native-run-fixture.f says why the byte offsets come from the
\ source map. The C-ABI cases other than division assert the allocated result
\ register; division asserts three arithmetic answers and the Habu square one
\ data-stack answer.
\
\ WHY ONE OF THE BYTE CASES ALLOCATES OUT OF A HIGH POOL. The low registers are
\ where an emitter that ignored the allocation entirely would put things anyway.
\ The three-argument shape is therefore emitted twice, once from a pool that
\ starts at register zero and once from a pool that starts at register four, and
\ the second one's expected words are different in every register field.
\
\ ONE FIXTURE PER CONTEXT. A module holds about seventeen arenas and the live
\ arena registry holds sixty-four, so a case that builds a source module and a
\ machine module is already close to full and a case that builds two machine
\ modules is too. Every case therefore runs in its own context, and a refusing
\ case runs inside an enclosing one because an abandoned context gives its
\ registry slots back only when a live enclosing context leaves normally.
\ Tier-neutral by design: the emitter is driven module by module through its own
\ API, so the bytes asserted come from those calls and not from the compiler
\ that compiled this file.

require lib/test.f
require lib/string.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f          \ the run child takes the unsealed engine
require lib/fs-mutate.f            \ CLEANUP-RUN - that engine's private copy
require test/whitebox-child.f
require test/suite-budget.f        \ CHILD-MS, the run child's hang guard
require test/compiler/native-emit-shapes.f

package A64EMIT-TEST
private

\ ---- the emitted bytes -------------------------------------------------------
\ `mul x0, x0, x0` then `ret`. The bytes are written out here rather than
\ recomputed from the encoders, so the expected value is independent of the
\ emitter and of the assembler both.
: SQUARE-BODY ( IR-CTX:ctx -- n n n n )
   HIR-MOD
   BUILD-SQUARE
   4 EMITTED
   A64EMIT:INSNS
   A64EMIT:SIZE
   0 A64EMIT:WORD@
   1 A64EMIT:WORD@ ;

: SQUARE-CASE ( -- )
   s" a multiply and a return emit as the two instructions they are" T-LABEL
   WBND [: SQUARE-BODY ;] IR-CTX:WITH-CONTEXT
   $D65F03C0 T= $9B007C00 T= 8 T= 2 T= ;

\ The same two instructions read one byte at a time: the buffer really holds the
\ little-endian placement the machine reads, and the map's offsets index it.
: BYTES-BODY ( IR-CTX:ctx -- n n n n n n )
   HIR-MOD
   BUILD-SQUARE
   4 EMITTED
   0 BYTE-AT
   1 BYTE-AT
   2 BYTE-AT
   3 BYTE-AT
   0 A64EMIT:MAP-OFFSET@
   1 A64EMIT:MAP-OFFSET@ ;

: BYTES-CASE ( -- )
   s" the instruction words are placed little-endian at the mapped offsets" T-LABEL
   WBND [: BYTES-BODY ;] IR-CTX:WITH-CONTEXT
   4 T= 0 T= $9B T= 0 T= $7C T= 0 T= ;

\ `sub x0, x0, x1` then `ret`.
: DIFF-BODY ( IR-CTX:ctx -- n n n n )
   HIR-MOD
   BUILD-DIFF
   4 EMITTED
   A64EMIT:INSNS
   0 A64EMIT:WORD@
   1 A64EMIT:WORD@
   RESULT-REG ;

: DIFF-CASE ( -- )
   s" a subtraction emits with its operands in the order the source has them" T-LABEL
   WBND [: DIFF-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= $D65F03C0 T= $CB010000 T= 2 T= ;

\ `cbnz x1, +2`, `bl (DIV-ZERO)`, `sdiv x0, x0, x1`, `ret`. The division is ONE
\ operation of the machine dialect and three instructions, and the two in front
\ of the divide are what make a compiled division agree with an interpreted one:
\ ARM64's Sdiv answers zero for a zero divisor, and the engine's own `/` branches
\ to the same sealed (DIV-ZERO) helper, which throws ARITH-ABI:E-DIV-ZERO
\ (src/habu/habu1.f BDIV0?). Deleting either, moving the guard's distance off
\ two, or branching anywhere else reddens here. The `bl` is measured from where
\ the bytes land, so the emission is placed at the free code slot first.
: DIV-BODY ( IR-CTX:ctx -- n n bool n n )
   HIR-MOD
   BUILD-DIV
   cp@ A64EMIT:PLACE-AT
   4 EMITTED
   A64EMIT:INSNS
   0 A64EMIT:WORD@
   1 BL-TARGET  DIV-ZERO-ENTRY =
   2 A64EMIT:WORD@
   3 A64EMIT:WORD@ ;

: DIV-CASE ( -- )
   s" a division emits the zero-divisor refusal the engine's own divide has" T-LABEL
   WBND [: DIV-BODY ;] IR-CTX:WITH-CONTEXT
   $D65F03C0 T= $9AC10C00 T= TTRUE $B5000041 T= 4 T= ;

\ `add x0, x0, x1`, `add x0, x0, x2`, `ret`.
: SUM3-BODY ( IR-CTX:ctx -- n n n n )
   HIR-MOD
   BUILD-SUM3
   4 EMITTED
   A64EMIT:INSNS
   0 A64EMIT:WORD@
   1 A64EMIT:WORD@
   2 A64EMIT:WORD@ ;

: SUM3-CASE ( -- )
   s" two additions emit with the registers the allocation gave them" T-LABEL
   WBND [: SUM3-BODY ;] IR-CTX:WITH-CONTEXT
   $D65F03C0 T= $8B020000 T= $8B010000 T= 3 T= ;

\ `add x1, x0, x1`, `add x0, x0, x1`, `ret`. The first addition writes a register
\ that is not the one it reads first, so an emitter that took the destination off
\ the first operand - or the first operand off the destination - is wrong here and
\ nowhere else in this file.
: REUSE-BODY ( IR-CTX:ctx -- n n n n )
   HIR-MOD
   BUILD-REUSE
   4 EMITTED
   A64EMIT:INSNS
   0 A64EMIT:WORD@
   1 A64EMIT:WORD@
   2 A64EMIT:WORD@ ;

: REUSE-CASE ( -- )
   s" an addition whose result outlives neither operand keeps all three fields apart" T-LABEL
   WBND [: REUSE-BODY ;] IR-CTX:WITH-CONTEXT
   $D65F03C0 T= $8B010000 T= $8B010001 T= 3 T= ;

\ The same shape out of a pool that starts at register four: `add x4, x4, x5`,
\ `add x4, x4, x6`, `ret`. Every register field differs from the case above.
: SUM3-HIGH-BODY ( IR-CTX:ctx -- n n n n )
   HIR-MOD
   BUILD-SUM3
   4 3 EMITTED-FROM
   A64EMIT:INSNS
   0 A64EMIT:WORD@
   1 A64EMIT:WORD@
   2 A64EMIT:WORD@ ;

: SUM3-HIGH-CASE ( -- )
   s" a pool that starts above register zero reaches every register field" T-LABEL
   WBND [: SUM3-HIGH-BODY ;] IR-CTX:WITH-CONTEXT
   $D65F03C0 T= $8B060084 T= $8B050084 T= 3 T= ;

\ `movz x0, #$5678` , `movk x0, #$1234, lsl 48`, `ret`. The half selector in the
\ encoding is three, and the dialect records the shift as forty-eight bits, so
\ this is also where that conversion is measured.
: WIDE-BODY ( IR-CTX:ctx -- n n n n )
   HIR-MOD
   BUILD-WIDE
   4 EMITTED
   A64EMIT:INSNS
   0 A64EMIT:WORD@
   1 A64EMIT:WORD@
   2 A64EMIT:WORD@ ;

: WIDE-CASE ( -- )
   s" a two-half literal emits as a move-wide and its overwrite" T-LABEL
   WBND [: WIDE-BODY ;] IR-CTX:WITH-CONTEXT
   $D65F03C0 T= $F2E24680 T= $D28ACF00 T= 3 T= ;

\ ---- the source map ----------------------------------------------------------
\ The literal's two instructions both answer for the body word the constant came
\ from, and the return answers for the closing `;`. One source, three offsets,
\ two distinct spans.
: MAP-BODY ( IR-CTX:ctx -- n n n n n n n n n n )
   HIR-MOD
   BUILD-WIDE
   4 EMITTED
   0 A64EMIT:MAP-OFFSET@
   1 A64EMIT:MAP-OFFSET@
   2 A64EMIT:MAP-OFFSET@
   0 SPAN-SRC-AT
   0 SPAN-START-AT
   0 SPAN-LEN-AT
   1 SPAN-START-AT
   1 SPAN-LEN-AT
   2 SPAN-START-AT
   2 SPAN-LEN-AT ;

: MAP-CASE ( -- )
   s" every emitted instruction maps to the span of the operation it came from" T-LABEL
   WBND [: MAP-BODY ;] IR-CTX:WITH-CONTEXT
   CLOSE-LN T= CLOSE-ST T=
   BODY-LN T= BODY-ST T=
   BODY-LN T= BODY-ST T=
   0 T= 8 T= 4 T= 0 T= ;

\ ---- machine modules built by hand -------------------------------------------
\ `movz x0, #7` then `ret`: a module built by hand emits exactly as one that came
\ through selection does.
: PLAIN-BODY ( IR-CTX:ctx -- n n n bool )
   PLAIN-EMITTED
   A64EMIT:INSNS
   0 A64EMIT:WORD@
   1 A64EMIT:WORD@
   A64EMIT:SEALED? ;

: PLAIN-CASE ( -- )
   s" a hand-built machine module emits the instructions it names" T-LABEL
   WBND [: PLAIN-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE $D65F03C0 T= $D28000E0 T= 2 T= ;

\ ---- the two addressed instructions, as the exact words they are -------------
\ The whole reason to pin these two words rather than only run the body: an
\ addressed store takes a value and an address, and the two are both registers,
\ so a routine that swapped them would write the address into whatever cell the
\ VALUE happens to name. Running it then fails by dying somewhere else, which
\ proves nothing about which field is which. The emitted word says it exactly:
\ the store's transfer field is the value's register and its base field is the
\ address's, and the load's transfer field is the loaded value's register. Both
\ offsets are zero, which is `[Xn]`, and a form that grew an offset it should not
\ have moves these numbers.
\
\ AND THE ROUTINE IS TEN INSTRUCTIONS AND NOT THIRTEEN, which is where the
\ positions below come from. It takes one cell and leaves one, so the place the
\ caller left the data-stack pointer and the place it expects it back are the
\ same place; the routine stands there, and the two adjustments that used to
\ bracket the body are distances of zero that no instruction is written for.
\ The eleventh instruction was the move-wide that put the literal 1 in a
\ register: selection folds a single-use constant into the addition that reads
\ it, so the increment is one `add x0, x0, #1` and there is no move to make.
: BUMP-BODY ( IR-CTX:ctx -- n n n n )
   HIR-MOD
   BUILD-BUMP
   6 1 1 EMITTED-HABU
   A64EMIT:INSNS
   2 A64EMIT:WORD@                   \ str x0, [x1] - the argument into the cell
   4 A64EMIT:WORD@                   \ ldr x0, [x0] - and back out of it
   7 A64EMIT:WORD@ ;                 \ str x0, [x1] - the bumped value in again

: BUMP-CASE ( -- )
   s" an addressed store and load emit through the registers they name" T-LABEL
   WBND [: BUMP-BODY ;] IR-CTX:WITH-CONTEXT
   $F9000020 T= $F9400000 T= $F9000020 T= 10 T= ;

\ ---- the two addressing modes a data-stack access is written in --------------
\ THE WHOLE OF WHAT THE PLACEMENT COSTS THE ENCODER. A routine stands where the
\ fewest pointer adjustments are needed, so the cell an access names can be UNDER
\ the pointer as easily as over it - and under it has no spelling in the scaled
\ unsigned Ldr and Str. It is the unscaled SIGNED pair, Ldur and Stur, and which
\ of the two an access is written in is decided by the sign of its offset and
\ nothing else.
\
\ WHY THE EXACT WORDS AND NOT THE MNEMONIC. The two forms differ in one bit of
\ the size field and hold their offsets in DIFFERENT fields - twelve scaled bits
\ at bit ten against nine signed bits at bit twelve - so an access written in the
\ wrong one reads a cell somewhere else entirely rather than failing to encode.
\ Squaring one cell is the smallest routine that has both: it stands at 8, so its
\ load and its store are both eight bytes under the pointer, and both are the
\ negative form. The routine is also RUN, over the same contract, in
\ test/compiler/native-emit-run-child.f - so a wrong field would answer something
\ other than forty-nine as well as read differently here.
: SQUARE-HABU-BODY ( IR-CTX:ctx -- n n n )
   HIR-MOD
   BUILD-SQUARE
   4 1 1 EMITTED-HABU
   A64EMIT:INSNS
   0 A64EMIT:WORD@                   \ ldur x0, [x19, #-8] - the argument's cell
   2 A64EMIT:WORD@ ;                 \ stur x0, [x19, #-8] - the result into it

: SQUARE-HABU-CASE ( -- )
   s" a cell under the pointer is written in the unscaled signed form" T-LABEL
   WBND [: SQUARE-HABU-BODY ;] IR-CTX:WITH-CONTEXT
   $F81F8260 T= $F85F8260 T= 4 T= ;

\ ---- a program that does not fit ---------------------------------------------
: SPILL-BODY ( IR-CTX:ctx -- n bool bool n )
   SPILL-EMITTED
   0 A64EMIT:WORD@                   \ sub sp, sp, #16 - the routine takes its frame
   $F90003E2 HAS-WORD?               \ str x2, [sp, #0] - the third value is put away
   $F94003E1 HAS-WORD?               \ ldr x1, [sp, #0] - and comes back for the sum
   A64EMIT:INSNS 2 - A64EMIT:WORD@ ; \ add sp, sp, #16 - the frame is returned

: SPILL-CASE ( -- )
   s" a block that does not fit reserves a frame and spills into it" T-LABEL
   WBND [: SPILL-BODY ;] IR-CTX:WITH-CONTEXT
   $910043FF T= TTRUE TTRUE $D10043FF T= ;

\ ---- the same program, written again instead of put away ---------------------
: REMAT-EMIT-BODY ( IR-CTX:ctx -- n n n )
   REMAT-EMITTED
   A64EMIT:INSNS
   0 A64EMIT:WORD@                   \ movz x0, #$11 - no stack adjustment at all
   NFIX:RESULT-REG ;

: REMAT-EMIT-CASE ( -- )
   s" a block that does not fit writes its constants again and takes no frame"
   T-LABEL
   WBND [: REMAT-EMIT-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= $D2800220 T= 12 T= ;

\ ---- a returned value put where the contract says it leaves ------------------
: SECOND-BODY ( IR-CTX:ctx -- n n n )
   SECOND-EMITTED
   A64EMIT:INSNS
   0 A64EMIT:WORD@                   \ mov x0, x1 - orr x0, xzr, x1
   1 A64EMIT:WORD@ ;

: SECOND-CASE ( -- )
   s" a returned value in the wrong register is copied into the right one" T-LABEL
   WBND [: SECOND-BODY ;] IR-CTX:WITH-CONTEXT
   $D65F03C0 T= $AA0103E0 T= 2 T= ;

\ ---- the emitted bytes, executed ---------------------------------------------
\ Publishing the bytes and calling them take two owner rows, code-publish and
\ the bounded FFI call, which a checked caller outside their owners is refused,
\ so the executing cases live in test/compiler/native-emit-run-child.f and call
\ through the public words test/mcode-window-prepare.f opens before the seal.
\ That makes the child a window child of test/native-window-owner-child.f, with
\ the dependency list lib/ffi-test.f FFI-T-STUB-ARGS gives its stub child.
\ Reopening the window is refused on the sealed product (`hb: internal engine
\ word`, exit 70), so the child runs on the engine test/whitebox-child.f names.
$4000 constant CHILD-CAP
create CHILD-OUT CHILD-CAP allot
create CHILD-ERR CHILD-CAP allot

: CHILD-ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

: CHILD-ARGS ( -- )
   PROC-ARGV-RESET
   s" --load" CHILD-ARG
   s" test/native-window-owner-child.f" CHILD-ARG
   s" --" CHILD-ARG
   s" test/compiler/native-emit-run-child.f" CHILD-ARG
   s" src/core/declaration-transaction.f" CHILD-ARG
   s" src/core/generated-declaration.f" CHILD-ARG
   s" src/core/decl-event.f" CHILD-ARG
   s" src/core/structure-make.f" CHILD-ARG
   s" src/core/structure-decl.f" CHILD-ARG
   s" src/core/enum-decl.f" CHILD-ARG
   s" src/core/structures.f" CHILD-ARG
   s" src/core/bytes.f" CHILD-ARG
   s" src/core/dynamic-storage.f" CHILD-ARG
   HB-TARGET-LINUX? if
      s" src/os/linux/target.f" CHILD-ARG
      s" src/os/linux/layout-constants.f" CHILD-ARG
      s" src/os/linux/layout.f" CHILD-ARG
   else
      s" src/os/macos/target.f" CHILD-ARG s" src/os/macos/layout.f" CHILD-ARG
   then
   s" src/habu/stack-abi.f" CHILD-ARG
   s" src/habu/layout.f" CHILD-ARG
   s" src/os/env-base.f" CHILD-ARG
   s" src/core/include.f" CHILD-ARG
   s" src/core/sha256.f" CHILD-ARG
   s" src/habu/code-span.f" CHILD-ARG
   s" test/mcode-window-prepare.f" CHILD-ARG
   WHITEBOX-CHILD:ENV! ;

: CHILD-RESULT ( -- )
   WHITEBOX-CHILD:ENGINE$ >LEN CHILD-OUT CHILD-CAP >LEN CHILD-ERR CHILD-CAP >LEN
   SUITE-BUDGET:CHILD-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   CHILD-OUT outu LEN>N S\" test: ok\nwindow: 0\n" STR= 0= rc 0 <> or
      if CHILD-OUT outu LEN>N type CHILD-ERR erru LEN>N type cr then
   s" the window child that runs the emitted bytes exits 0" T-LABEL
   rc 0 T=
   s" every executing case passes in the window child" T-LABEL
   CHILD-OUT outu LEN>N S\" test: ok\nwindow: 0\n" T$= ;

: RUN-CHILD-CASE ( -- )
   [: s" native-emit-run" WHITEBOX-CHILD:PROVIDE CHILD-ARGS CHILD-RESULT ;]
   [: CLEANUP-RUN ;] finally ;

\ ---- refusals ----------------------------------------------------------------
\ Nobody has accepted anything yet. This case runs FIRST in the suite, because an
\ acceptance is package state no later run takes back: once one allocation has
\ been accepted, the ways to be refused for it are that it is about another
\ module or that a later walk replaced it, and both have cases of their own.
: UNACCEPTED-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c A64-NEW
   BIND-EMIT
   BUILD-PLAIN
   M-FREEZE {: m:IR-BUILD:module :}
   c m A64EMIT:EMIT ;

\ A module the binding was not taken over: the binding is taken over the first
\ module and the second is the one presented.
: WRONG-MODULE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c A64-NEW
   BIND-EMIT
   BUILD-PLAIN
   M-FREEZE drop
   c A64-NEW
   BUILD-PLAIN
   M-FREEZE {: m2:IR-BUILD:module :}
   c m2 A64EMIT:EMIT ;

: NO-BIND-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c A64-NEW
   BUILD-PLAIN
   M-FREEZE {: m:IR-BUILD:module :}
   c m A64EMIT:EMIT ;

: TWICE-BIND-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   c A64IR:NEW-BUILDER {: b:IR-BUILD:builder :}
   c b A64EMIT:BIND-DIALECT
   c b A64EMIT:BIND-DIALECT ;

: WRONG-DIALECT-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   c HIR:NEW-BUILDER {: b:IR-BUILD:builder :}
   c b A64EMIT:BIND-DIALECT ;

\ BOTH FUNCTIONS' INSTRUCTIONS, IN ONE EMISSION, read back as the words they are.
\ The four instructions are the two functions end to end - each one's move-wide
\ and its return - and the literals say which is which, so an emitter that laid
\ the second function over the first, or wrote the first one twice, answers
\ different words here rather than a different count.
: TWO-FUNS-BODY ( IR-CTX:ctx -- n n n n n bool )
   {: c:IR-CTX:ctx :}
   c A64-NEW
   BIND-RA
   BIND-RAV
   BIND-EMIT
   BUILD-TWO-FUNS
   M-FREEZE {: m:IR-BUILD:module :}
   c m 0 4 NFIX:FINISH
   A64EMIT:INSNS
   0 A64EMIT:WORD@   1 A64EMIT:WORD@
   2 A64EMIT:WORD@   3 A64EMIT:WORD@
   A64EMIT:SEALED? ;

: TWO-FUNS-CASE ( -- )
   s" a module of two functions emits both of them, one after the other" T-LABEL
   WBND [: TWO-FUNS-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE
   $D65F03C0 T=  $D2800120 T=
   $D65F03C0 T=  $D28000E0 T=
   4 T= ;

: EXTRA-OPCODE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c A64-NEW
   BIND-EMIT
   BUILD-EXTRA
   M-FREEZE {: m:IR-BUILD:module :}
   c m A64EMIT:EMIT ;

\ These instructions belong to one architecture. The module is built under the
\ machine this dialect is for and presented under one it is not.
: WRONG-TARGET-INNER ( IR-BUILD:module IR-CTX:ctx -- )
   {: m:IR-BUILD:module c:IR-CTX:ctx :}
   c m A64EMIT:EMIT ;

: WRONG-TARGET-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c A64-NEW
   BIND-EMIT
   BUILD-PLAIN
   M-FREEZE {: m:IR-BUILD:module :}
   m PBND [: WRONG-TARGET-INNER ;] IR-CTX:WITH-CONTEXT ;

\ The same presentation to a machine whose architecture this backend does serve.
: UNSERVED-TARGET-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c A64-NEW
   BIND-EMIT
   BUILD-PLAIN
   M-FREEZE {: m:IR-BUILD:module :}
   m BEBND [: WRONG-TARGET-INNER ;] IR-CTX:WITH-CONTEXT ;

\ An acceptance about one module is not an answer about another: the first module
\ is allocated and accepted, the second is the one presented for emission.
: OTHER-ALLOC-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c A64-NEW
   BIND-RA
   BIND-RAV
   BUILD-PLAIN
   M-FREEZE {: m1:IR-BUILD:module :}
   c m1 4 NFIX:LEAF-N A64RA:ALLOCATE
   m1 4 NFIX:LEAF-N A64RAV:ACCEPT
   c A64-NEW
   BIND-EMIT
   BUILD-PLAIN
   M-FREEZE {: m2:IR-BUILD:module :}
   c m2 A64EMIT:EMIT ;

\ An accepted answer stops being one when a later walk replaces the allocation it
\ was about, and the emitter finds that out before it writes a byte.
: STALE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c A64-NEW
   BIND-RA
   BIND-RAV
   BIND-EMIT
   BUILD-PLAIN
   M-FREEZE {: m1:IR-BUILD:module :}
   c m1 4 NFIX:LEAF-N A64RA:ALLOCATE
   m1 4 NFIX:LEAF-N A64RAV:ACCEPT
   c A64-NEW
   BIND-RA
   BUILD-PLAIN
   M-FREEZE {: m2:IR-BUILD:module :}
   c m2 4 NFIX:LEAF-N A64RA:ALLOCATE
   c m1 A64EMIT:EMIT ;

\ An index outside a sealed emission.
: PAST-END-BODY ( IR-CTX:ctx -- )
   PLAIN-EMITTED
   A64EMIT:INSNS A64EMIT:WORD@ drop ;

: PAST-MAP-BODY ( IR-CTX:ctx -- )
   PLAIN-EMITTED
   -1 A64EMIT:MAP-OFFSET@ drop ;

\ ---- the three address-chain shapes the emitter refuses ----------------------
\ A MOVN CLAIMING AN ADDRESS. The relocation pass writes four plain immediates
\ over a chain; a movn builds its value out of ones, so a lane that was one would
\ have to be complemented and the pass does not look. Refused where the movn word
\ is encoded.
\
\ IT IS A FULL FOUR-LANE RUN IN ONE REGISTER, and that is the point of the
\ fixture rather than an incidental detail. A bare movn carrying the kind is a
\ ONE-lane run, which the run-length check below refuses on its own - so a case
\ built that way passes with the movn guard deleted and proves nothing about it.
\ Leading a genuine carrier with a movn is the only shape that reaches the movn
\ guard with every other check satisfied: deleting the guard then reds THIS case
\ and leaves the other two green.
: BUILD-MOVN-ADDR ( -- )
   s" MOVNADDR" 0 1 OPEN-FUN
   A64IR-OPCODE:MOVN 1 0 A64IR:ADDR-DATA M-WIDE
   2 16 A64IR:ADDR-DATA M-WIDE-K
   3 32 A64IR:ADDR-DATA M-WIDE-K
   4 48 A64IR:ADDR-DATA M-WIDE-K
   M-RET
   CLOSE-FUN ;

\ A RUN THAT IS NOT THE CARRIER'S WIDTH. Three lanes leave no room for a fourth
\ half that rebasing can make non-zero, so a three-lane run is not a site and is
\ not silently treated as one either.
: BUILD-SHORT-RUN ( -- )
   s" SHORTRUN" 0 1 OPEN-FUN
   A64IR-OPCODE:MOVZ 1 0 A64IR:ADDR-DATA M-WIDE
   2 16 A64IR:ADDR-DATA M-WIDE-K
   3 32 A64IR:ADDR-DATA M-WIDE-K
   M-RET
   CLOSE-FUN ;

\ FOUR LANES THAT DO NOT NAME ONE REGISTER. Four move-wides into two registers
\ spell out no address any site pushed, and the loader refuses exactly this shape
\ from the other end (src/habu/habu2.f EMIT-ADDRS). Two independent two-lane
\ chains are the way to build it: neither takes the other as an operand, so the
\ allocator has no reason to give them the same register.
: BUILD-SPLIT-RUN ( -- )
   s" SPLITRUN" 0 1 OPEN-FUN
   A64IR-OPCODE:MOVZ 1 0 A64IR:ADDR-DATA M-WIDE
   2 16 A64IR:ADDR-DATA M-WIDE-K {: a:IR-ID:ir-value-id :}
   A64IR-OPCODE:MOVZ 3 0 A64IR:ADDR-DATA M-WIDE
   4 16 A64IR:ADDR-DATA M-WIDE-K {: b:IR-ID:ir-value-id :}
   a b M-ADD M-RET
   CLOSE-FUN ;

: EMIT-BUILT ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   M-FREEZE {: m:IR-BUILD:module :}
   c m 0 4 NFIX:FINISH ;

: DATA-ADDR-BODY ( IR-CTX:ctx -- ) {: c:IR-CTX:ctx :}
   c A64-NEW BIND-RA BIND-RAV BIND-EMIT
   s" DATAADDR" 0 1 OPEN-FUN
   A64IR-OPCODE:DATAADDR M-OPEN M-RESULT+
   CC BB CC BB A64IR:KEY-DATA-OFFSET
   CC BB DATA-SIZE A64IR:DATA-OFFSET-ATTR IR-BUILD:ADD-ATTR
   CLOSE-VALUE M-RET CLOSE-FUN
   c EMIT-BUILT
   s" DATA addresses above 16 MiB keep a contiguous three-instruction carrier" T-LABEL
   A64EMIT:INSNS 4 T=
   A64EMIT:ADDR-SITES 1 T=
   0 A64EMIT:ADDR-SITE@ 0 T=
   0 A64EMIT:ADDR-SITE-KIND@ A64IR:ADDR-DATA T=
   0 A64EMIT:WORD@ 31 and {: rd:n :}
   DATA-VA VA>N DATA-SIZE + {: addr:n :}
   0 A64EMIT:WORD@ rd addr 32 rshift 2 A64ASM:MOVZHW T=
   1 A64EMIT:WORD@ rd addr 16 rshift $FFFF and 1 A64ASM:MOVKHW T=
   2 A64EMIT:WORD@ rd addr $FFFF and 0 A64ASM:MOVKHW T=
   3 A64EMIT:WORD@ A64ASM:ENC-RET T= ;

: DATA-ADDR-CASE ( -- )
   WBND [: DATA-ADDR-BODY ;] IR-CTX:WITH-CONTEXT
   [: -1 A64IR:DATA-OFFSET drop ;] E-A64IR-DATA TTHROWSQ
   [: DATA-SIZE 1+ A64IR:DATA-OFFSET drop ;] E-A64IR-DATA TTHROWSQ ;

: MOVN-ADDR-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c A64-NEW BIND-RA BIND-RAV BIND-EMIT BUILD-MOVN-ADDR c EMIT-BUILT ;

: SHORT-RUN-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c A64-NEW BIND-RA BIND-RAV BIND-EMIT BUILD-SHORT-RUN c EMIT-BUILT ;

: SPLIT-RUN-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c A64-NEW BIND-RA BIND-RAV BIND-EMIT BUILD-SPLIT-RUN c EMIT-BUILT ;

\ ---- refusal cases -----------------------------------------------------------
: UNACCEPTED ( -- )      WBND [: UNACCEPTED-BODY ;] IR-CTX:WITH-CONTEXT ;
: WRONG-MODULE ( -- )    WBND [: WRONG-MODULE-BODY ;] IR-CTX:WITH-CONTEXT ;
: NO-BIND ( -- )         WBND [: NO-BIND-BODY ;] IR-CTX:WITH-CONTEXT ;
: TWICE-BIND ( -- )      WBND [: TWICE-BIND-BODY ;] IR-CTX:WITH-CONTEXT ;
: WRONG-DIALECT ( -- )   WBND [: WRONG-DIALECT-BODY ;] IR-CTX:WITH-CONTEXT ;
: EXTRA-OPCODE ( -- )    WBND [: EXTRA-OPCODE-BODY ;] IR-CTX:WITH-CONTEXT ;
: WRONG-TARGET ( -- )    WBND [: WRONG-TARGET-BODY ;] IR-CTX:WITH-CONTEXT ;
: UNSERVED-TARGET ( -- ) WBND [: UNSERVED-TARGET-BODY ;] IR-CTX:WITH-CONTEXT ;
: OTHER-ALLOC ( -- )     WBND [: OTHER-ALLOC-BODY ;] IR-CTX:WITH-CONTEXT ;
: STALE ( -- )           WBND [: STALE-BODY ;] IR-CTX:WITH-CONTEXT ;
: PAST-END ( -- )        WBND [: PAST-END-BODY ;] IR-CTX:WITH-CONTEXT ;
: PAST-MAP ( -- )        WBND [: PAST-MAP-BODY ;] IR-CTX:WITH-CONTEXT ;
: MOVN-ADDR ( -- )       WBND [: MOVN-ADDR-BODY ;] IR-CTX:WITH-CONTEXT ;
: SHORT-RUN ( -- )       WBND [: SHORT-RUN-BODY ;] IR-CTX:WITH-CONTEXT ;
: SPLIT-RUN ( -- )       WBND [: SPLIT-RUN-BODY ;] IR-CTX:WITH-CONTEXT ;

: DROP-BINDING ( -- )
   A64EMIT:RELEASE ;

\ Each of the three names its own shape, so a guard that stopped refusing one of
\ them leaves the other two green and the case that reds says which.
: ADDR-REFUSE-CASES ( -- )
   s" a movn that claims to carry an address is refused" T-LABEL
   [: MOVN-ADDR ;] E-A64EMIT-ADDR TTHROWSQ
   s" an address run shorter than the carrier is refused" T-LABEL
   [: SHORT-RUN ;] E-A64EMIT-ADDR TTHROWSQ
   s" four address lanes that do not name one register are refused" T-LABEL
   [: SPLIT-RUN ;] E-A64EMIT-ADDR TTHROWSQ ;

: ALLOC-REFUSE-CASES ( -- )
   s" emitting from a register assignment nobody accepted is refused" T-LABEL
   [: UNACCEPTED ;] E-A64EMIT-ALLOC TTHROWSQ ;

: BIND-REFUSE-CASES ( -- )
   s" emitting without a binding is refused" T-LABEL
   [: NO-BIND ;] E-A64EMIT-BIND TTHROWSQ
   s" a second binding over a live one is refused" T-LABEL
   [: TWICE-BIND ;] E-A64EMIT-BIND TTHROWSQ
   DROP-BINDING ;

: MODULE-REFUSE-CASES ( -- )
   s" a frozen module the binding was not taken over is refused" T-LABEL
   [: WRONG-MODULE ;] E-A64EMIT-MODULE TTHROWSQ
   s" binding a builder of another dialect is refused" T-LABEL
   [: WRONG-DIALECT ;] E-A64EMIT-MODULE TTHROWSQ ;

: SHAPE-REFUSE-CASES ( -- )
   s" an operation of a form outside the dialect's family is refused" T-LABEL
   [: EXTRA-OPCODE ;] E-A64EMIT-OPCODE TTHROWSQ ;

: TARGET-REFUSE-CASES ( -- )
   s" emitting under a context bound to a foreign architecture refuses" T-LABEL
   [: WRONG-TARGET ;] E-A64EMIT-TARGET TTHROWSQ
   s" emitting under a context this backend does not serve is this stage's refusal" T-LABEL
   [: UNSERVED-TARGET ;] E-A64EMIT-TARGET TTHROWSQ ;

: OTHER-ALLOC-REFUSE-CASE ( -- )
   s" an acceptance made from another module is refused" T-LABEL
   [: OTHER-ALLOC ;] E-A64EMIT-ALLOC TTHROWSQ ;

\ Its own group: this fixture and the one above each abandon a context holding
\ two modules, and two of those at once run the arena registry dry.
: STALE-REFUSE-CASE ( -- )
   s" an acceptance a later allocation replaced stops answering" T-LABEL
   [: STALE ;] E-A64RAV-STATE TTHROWSQ ;

: BOUND-REFUSE-CASES ( -- )
   s" an instruction index past the emission is refused" T-LABEL
   [: PAST-END ;] E-A64EMIT-BOUND TTHROWSQ
   s" a source-map index below the emission is refused" T-LABEL
   [: PAST-MAP ;] E-A64EMIT-BOUND TTHROWSQ ;

\ ---- groups ------------------------------------------------------------------
: GROUP-ALLOC ( IR-CTX:ctx -- )   drop ALLOC-REFUSE-CASES ;
: GROUP-BIND ( IR-CTX:ctx -- )    drop BIND-REFUSE-CASES ;
: GROUP-MODULE ( IR-CTX:ctx -- )  drop MODULE-REFUSE-CASES ;
: GROUP-SHAPE ( IR-CTX:ctx -- )   drop SHAPE-REFUSE-CASES ;
: GROUP-TARGET ( IR-CTX:ctx -- )  drop TARGET-REFUSE-CASES ;
: GROUP-ACCEPT ( IR-CTX:ctx -- )  drop OTHER-ALLOC-REFUSE-CASE ;
: GROUP-STALE ( IR-CTX:ctx -- )   drop STALE-REFUSE-CASE ;
: GROUP-BOUND ( IR-CTX:ctx -- )   drop BOUND-REFUSE-CASES ;
: GROUP-ADDR ( IR-CTX:ctx -- )    drop ADDR-REFUSE-CASES ;

public

: RUN ( -- )
   T-RESET
   WBND [: GROUP-ALLOC ;] IR-CTX:WITH-CONTEXT
   SQUARE-CASE
   BYTES-CASE
   DIFF-CASE
   DIV-CASE
   SUM3-CASE
   REUSE-CASE
   SUM3-HIGH-CASE
   WIDE-CASE
   BUMP-CASE
   SQUARE-HABU-CASE
   MAP-CASE
   PLAIN-CASE
   TWO-FUNS-CASE
   DATA-ADDR-CASE
   SPILL-CASE
   REMAT-EMIT-CASE
   SECOND-CASE
   RUN-CHILD-CASE
   WBND [: GROUP-ADDR ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-BIND ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-MODULE ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-SHAPE ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-TARGET ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-ACCEPT ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-STALE ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-BOUND ;] IR-CTX:WITH-CONTEXT
   T-REPORT ;

;package

A64EMIT-TEST:RUN
