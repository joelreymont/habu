\ elaborate.f - the straight-line elaborator: it walks one sealed source tape and
\ builds the operations of a colon definition into a module under construction.
\
\ docs/compiler-ir-design.md section 7.2 with section 7.12's definition
\ transaction. This is the step between "capture source tape" and "freeze HIR":
\ everything before it records what the compiler read, everything after it reads
\ what the compiler built, and this file is the only place that turns one into
\ the other. It is the first pass of the native chain that translates a program.
\
\ WHAT IT TRANSLATES. One colon definition of the straight-line subset: an
\ opening `:`, the defined name, a body of integer literals, modeled arithmetic
\ words and compile-time stack renames, and a closing `;`. Nothing else, and no
\ second definition on the same tape - a tape with tokens after the `;` is
\ refused rather than half read.
\
\ THE COMPILE-TIME VALUE VECTOR IS THE WHOLE IDEA. Design section 7.3 keeps a
\ vector of the values the data stack holds at each point of the body, and says
\ that `DUP`, `DROP`, `SWAP` and `OVER` "produce no SIR operation and therefore
\ no runtime instruction". This file is where that stops being a plan: a rename
\ reorders the vector and stages nothing, so RENAME below does not even take a
\ builder. `: SQUARE ( n -- n ) dup * ;` becomes exactly two operations - one
\ multiply and one return - and the multiply's two operands are the same value.
\ That is the measured difference from the old emitter, which spends an
\ instruction on every stack word.
\
\ WHERE THE WORD'S INPUTS COME FROM. The entry block's arguments are the word's
\ declared inputs, one value each, and they are the vector's contents when the
\ body starts. At the `;` the vector must hold exactly the declared outputs, and
\ they become the operands of `hir.return` bottom first - the order the caller's
\ stack has them.
\
\ WHAT THIS PASS IS TOLD RATHER THAN READS. Two facts come in as arguments: how
\ many values the word takes and how many it leaves. They belong to the checker's
\ accepted stack effect, which section 7.2 requires the elaborated operations to
\ correspond to, and the frozen checker environment that will carry it is dot
\ habu-bind-checker-env-ed4f9f87. Until that lands the caller states the arity
\ and the elaborator checks the body against it, which is why a body that leaves
\ the wrong number of values is refused here rather than discovered later. Two
\ more facts are this file's own for the same reason: the definition frame is
\ found by the spellings `:` and `;`, and a definition compiles as exported. Dot
\ habu-bind-the-colon-ea509e61 takes all four off this file and onto the frozen
\ environment. Linkage and convention need no such dot: a colon definition is a
\ definition of this module and it is called the Habu way.
\
\ WHY IT ASKS FOUR AUTHORITIES AND OWNS NONE OF THEM. The tape says what the
\ tokens are; src/compiler/native/immediate.f says whether the two frame words
\ may be compiled at all; src/compiler/native/hir-word.f says what each body word
\ means, and for a rename says exactly which values it puts back;
\ src/compiler/ir/build.f's schema readers say how many operands an opcode takes
\ and how many results it defines. This file repeats none of those facts. It owns
\ the value vector, the shape of a definition on a tape, and the parser mode each
\ of its tokens must have been read in - and nothing else.
\
\ THE PARSER MODE IS CHECKED, NOT DECORATION. `:` runs from the outer interpreter
\ and parses the defined name before it switches the parser to compiling, so both
\ of the first two tokens were read while interpreting; every token of the body,
\ and the `;` that ends it, was read while compiling. The tape records the mode
\ each token was consumed in, so those are facts this pass can check instead of
\ assume, and a producer that disagrees is refused loudly rather than compiled
\ into something else.
\
\ WHAT A REFUSAL LEAVES BEHIND. A refused elaboration leaves the builder holding
\ whatever function, block and operation stages it had opened. That is the
\ builder's own fail-closed state: FREEZE refuses a module with an open stage
\ with E-IR-BUILD-OPEN, so nothing half-built can be published, and the caller
\ gives the module up with IR-BUILD:ABORT, which returns every stage to its
\ authority. Unwinding them here would need to catch and rethrow, and a checked
\ `catch` takes a stack-neutral quotation, so it could not carry the handles it
\ would need to unwind with.
\
\ ONE DEFINITION AT A TIME. The value vector is a fixed package-owned array
\ rather than a heap object, so this pass compiles one definition at a time -
\ which is the single-task compilation discipline the rest of the compiler
\ already keeps. It is not a staging protocol with a begin and an end: the whole
\ walk is one call, so the vector is emptied when that call starts and nothing a
\ refused call left behind can be read by the next one.

require lib/prelude.f
require lib/errors.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/arena.f
require src/compiler/ir/type.f
require src/compiler/ir/fun.f
require src/compiler/ir/build.f
require src/compiler/native/tape.f
require src/compiler/native/immediate.f
require src/compiler/native/hir.f
require src/compiler/native/hir-word.f

package NELAB
private

\ ---- the compile-time value vector -------------------------------------------
\ How deep the data stack may get inside one straight-line body. Sixty-four is
\ far past anything hand-written Forth reaches; a body that wants more is a
\ capability to raise here, not a ceiling to widen silently.
64 constant VMAX

here CELL 1- and CELL swap - CELL 1- and allot
variable VN                          \ how many values the vector holds
VMAX TYPED-BUFFER VSTK IR-ID:ir-value-id
VMAX TYPED-BUFFER VWIN IR-ID:ir-value-id

: VRESET ( -- )
   0 VN ! ;

: VPUSH ( IR-ID:ir-value-id -- )
   {: val:IR-ID:ir-value-id :}
   VN @ VMAX >= if E-NELAB-CAP throw then
   val VN @ VSTK !
   VN @ 1+ VN ! ;

\ The i-th value from the bottom. Every reader of the vector goes through here,
\ so an index outside what the vector holds is one refusal rather than several.
: VAT ( n -- IR-ID:ir-value-id )
   {: i:n :}
   i 0 < i VN @ >= or if E-NELAB-UNDER throw then
   i VSTK @ ;

: VDROP ( n -- )
   {: k:n :}
   k 0 < k VN @ > or if E-NELAB-UNDER throw then
   VN @ k - VN ! ;

\ ---- compile-time stack renames ----------------------------------------------
\ The consumed window is copied aside before the vector is shortened, because a
\ rename may put a value back below where it read it from: `swap` writes its
\ first pick over the cell its second pick reads. The window is as large as the
\ vector, so a rename that fits in the vector fits in the window, and there is no
\ second ceiling to keep in step with the word model's.
\
\ A pick names its input by depth in the consumed window with zero being the top,
\ so the value it names sits at window position in-1-depth. Picks are listed
\ bottom first, which is the order they are pushed.
: RENAME ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id -- )
   {: p:IR-ARENA:arena r:IR-ARENA:arena sym:IR-ID:ir-symbol-id :}
   r sym HIR-WORD:INPUTS@ {: in:n :}
   r sym HIR-WORD:PICKS {: picks:n :}
   in VN @ > if E-NELAB-UNDER throw then
   VN @ in - {: base:n :}
   in 0 ?do
      base i + VAT  i VWIN !
   loop
   in VDROP
   VN @ picks + VMAX > if E-NELAB-CAP throw then
   picks 0 ?do
      in 1- p r sym i HIR-WORD:PICK@ -  VWIN @  VPUSH
   loop ;

\ ---- staging one operation ---------------------------------------------------
\ The cell type of this subset: one signed 64-bit integer per stack value. The
\ dialect declares the same type in every schema it registers, and this is the
\ same interned identity because interning deduplicates - and if the two ever
\ disagreed, IR-OP:END-OP would refuse the first operation whose operand type is
\ not the one its schema declares, so the restatement is checked rather than
\ trusted.
: CELL-TYPE ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   IR--TYPE-WIDTH:W64 IR--TYPE-SIGN:SIGNED IR-BUILD:INTERN-INT ;

\ Every operation this pass stages carries the span of the token that produced
\ it, which is what makes a later diagnostic point at source the programmer
\ wrote. The span comes off the tape, so it is the real byte range and not a
\ placeholder.
: OPEN ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ID:ir-module-key n IR-ID:ir-symbol-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view key:IR-ID:ir-module-key
      ix:n op:IR-ID:ir-symbol-id :}
   c b op IR-BUILD:BEGIN-OP
   c b  v key ix NTAPE:SPAN@  IR-BUILD:SET-OP-SPAN ;

\ The operands the opcode's schema declares, taken off the vector. The deepest
\ one is the operation's first operand, so `-` on a stack holding a then b
\ subtracts b from a, exactly as the source reads. The count is the schema's
\ fixed operand list: no source word of this subset binds to an opcode with a
\ variadic tail, and a word model that bound one would be refused downstream by
\ name - IR-OP measures a staged operation against the same schema, and IR-FUN
\ refuses a terminator that is not the block's last operation.
: OPERANDS+ ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder op:IR-ID:ir-symbol-id :}
   c b op IR-BUILD:SCHEMA-OPERANDS {: k:n :}
   k VN @ > if E-NELAB-UNDER throw then
   VN @ k - {: base:n :}
   k 0 ?do
      c b  base i + VAT  IR-BUILD:ADD-OPERAND
   loop
   k VDROP ;

: RESULTS+ ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder op:IR-ID:ir-symbol-id :}
   c b op IR-BUILD:SCHEMA-RESULTS {: k:n :}
   k 0 ?do
      c b  c b CELL-TYPE  IR-BUILD:ADD-RESULT
   loop ;

\ Close the operation and push what it defined. The values are the operation's
\ own, read back off its row, so nothing here has to know which value ordinals
\ the store happened to mint.
: CLOSE ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-symbol-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder op:IR-ID:ir-symbol-id :}
   c b IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   c b op IR-BUILD:SCHEMA-RESULTS {: k:n :}
   k 0 ?do
      c b id i IR-BUILD:OP-RESULT@ VPUSH
   loop ;

\ ---- the three things a body token becomes -----------------------------------
\ An integer literal. The value is the whole content of a constant, so it rides
\ as the attribute the opcode's schema requires.
: EMIT-CONST ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view key:IR-ID:ir-module-key
      ix:n :}
   c b HIR-OPCODE:CONST HIR:OPCODE {: op:IR-ID:ir-symbol-id :}
   c b v key ix op OPEN
   c b op OPERANDS+
   c b op RESULTS+
   c b  c b HIR:KEY-VALUE  c b  v ix NTAPE:LIT@  IR-BUILD:INTERN-INT-ATTR
   IR-BUILD:ADD-ATTR
   c b op CLOSE ;

\ A word the dialect has an operation for. Which operation is the word model's
\ answer; how many operands and results it has is the schema table's.
: EMIT-OP ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ARENA:arena IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view r:IR-ARENA:arena
      key:IR-ID:ir-module-key ix:n :}
   r  v key ix NTAPE:SPELL@  HIR-WORD:OPCODE@ {: k:HIR:opcode :}
   c b k HIR:OPCODE {: op:IR-ID:ir-symbol-id :}
   c b v key ix op OPEN
   c b op OPERANDS+
   c b op RESULTS+
   c b op CLOSE ;

\ Leaving the word. The outputs are the whole vector, bottom first, and the
\ vector has to hold exactly as many as the word declares - one too few or one
\ too many is a body that does not match its effect, and it is refused here
\ rather than turned into a return of the wrong width. `hir.return` declares a
\ variadic operand tail, so the count is the word's and not the opcode's.
: EMIT-RETURN ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ID:ir-module-key n n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view key:IR-ID:ir-module-key
      ix:n out:n :}
   VN @ out <> if E-NELAB-ARITY throw then
   c b HIR-OPCODE:RETURN HIR:OPCODE {: op:IR-ID:ir-symbol-id :}
   c b v key ix op OPEN
   out 0 ?do
      c b  i VAT  IR-BUILD:ADD-OPERAND
   loop
   out VDROP
   c b IR-BUILD:END-OP drop ;

\ ---- reading the definition frame --------------------------------------------
: NAME-CK ( IR-ARENA:view n -- )
   NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if E-NELAB-SHAPE throw then ;

: MODE-CK ( IR-ARENA:view n NTAPE:mode -- )
   {: v:IR-ARENA:view ix:n want:NTAPE:mode :}
   v ix NTAPE:MODE@ want NTAPE-MODE:EQ 0= if E-NELAB-MODE throw then ;

\ Is this token spelled exactly these bytes? Byte equality against the module's
\ own interner, which appends nothing - asking the interner for the identity of
\ the bytes would mint a symbol when they are absent, and reading a tape must not
\ change the module it is read into.
: SPELLED? ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ID:ir-module-key n ptr u8 n -- bool )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view key:IR-ID:ir-module-key
      ix:n a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   c b  v key ix NTAPE:SPELL@  a u IR-BUILD:SYMBOL-IS? ;

\ A frame word must be a declared front-end intrinsic. `compile-time` is a
\ different contract - an immediate that may run during elaboration and reach the
\ program only through the builder - and opening or closing a definition is not
\ that, so it is refused by name rather than admitted as near enough.
: FRAME-CK ( IR-ARENA:view IR-ID:ir-module-key IR-ARENA:arena n -- )
   NIMM:ADMIT-TOKEN NIMM-CLASS:INTRINSIC NIMM-CLASS:EQ
   0= if E-NELAB-IMMEDIATE throw then ;

\ The token that ends the definition. A literal is never it, whatever it is
\ spelled, so the kind is asked before the spelling.
: END-TOKEN? ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ID:ir-module-key n -- bool )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view key:IR-ID:ir-module-key
      ix:n :}
   v ix NTAPE:KIND@ NTAPE-KIND:NAME NTAPE-KIND:EQ 0= if false exit then
   c b v key ix s" ;" SPELLED? ;

\ ---- the walk ----------------------------------------------------------------
variable IX                          \ the body token the walk stands on

\ One body token. The word model answers what it is; a literal and an operation
\ word each stage one operation, and a rename stages none. `unmodeled` never
\ reaches the match - HIR-WORD:ADMIT refuses it first - and the arm throws the
\ same refusal rather than inventing a second name for it.
: STEP ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view p:IR-ARENA:arena
      r:IR-ARENA:arena key:IR-ID:ir-module-key ix:n :}
   v ix NTAPE-MODE:COMPILING MODE-CK
   v key r ix HIR-WORD:ADMIT-TOKEN
   MATCH HIR:meaning
      literal   OF c b v key ix EMIT-CONST ENDOF
      op        OF c b v r key ix EMIT-OP ENDOF
      rename    OF p r  v key ix NTAPE:SPELL@  RENAME ENDOF
      unmodeled OF E-HIR-UNMODELED throw ENDOF
   ;MATCH ;

\ Walk the body until the closing `;`, and leave the walk standing on it. A tape
\ that runs out first is not a definition at all.
: WALK ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view p:IR-ARENA:arena
      r:IR-ARENA:arena key:IR-ID:ir-module-key n:n :}
   2 IX !
   begin
      IX @ n >= if E-NELAB-SHAPE throw then
      c b v key IX @ END-TOKEN? 0=
   while
      c b v p r key IX @ STEP
      IX @ 1+ IX !
   repeat ;

\ ---- opening the function ----------------------------------------------------
\ The word's declared effect as a code-reference type: one cell in per input and
\ one cell out per output.
: SIGNATURE ( IR-CTX:ctx IR-BUILD:builder n n -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder in:n out:n :}
   c b CELL-TYPE {: t:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN
   in 0 ?do t IR-TYPE:FN-PARAM loop
   out 0 ?do t IR-TYPE:FN-RESULT loop
   c b IR-BUILD:INTERN-CODE-REF ;

: ARITY-CK ( n n -- )
   {: in:n out:n :}
   in 0 < out 0 < or if E-NELAB-ARITY throw then
   in VMAX > out VMAX > or if E-NELAB-ARITY throw then ;

\ The defined word becomes a function whose span is the span of its name. A
\ colon definition is a definition in this module, so its linkage is defined and
\ its convention is Habu's.
: OPEN-FUN ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ID:ir-module-key n n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view key:IR-ID:ir-module-key
      in:n out:n :}
   c b  v key 1 NTAPE:SPELL@  IR-BUILD:BEGIN-FUN
   c b  c b in out SIGNATURE  IR-BUILD:SET-SIGNATURE
   c b IR--FUN-LINKAGE:DEFINED IR-BUILD:SET-LINKAGE
   c b IR--FUN-VISIBILITY:EXPORTED IR-BUILD:SET-VISIBILITY
   c b IR--FUN-CONVENTION:HABU IR-BUILD:SET-CONVENTION
   c b  v key 1 NTAPE:SPAN@  IR-BUILD:SET-FUN-SPAN ;

\ The entry block, whose arguments are the word's inputs and whose span is the
\ `:` that opened the definition. The arguments enter the value vector in
\ declaration order, so the first input is the deepest value, exactly as the
\ caller's stack has them.
: OPEN-BLOCK ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view key:IR-ID:ir-module-key
      in:n :}
   c b IR-BUILD:BEGIN-BLOCK
   c b  v key 0 NTAPE:SPAN@  IR-BUILD:SET-BLOCK-SPAN
   VRESET
   in 0 ?do
      c b  c b CELL-TYPE  IR-BUILD:ADD-BLOCK-ARG VPUSH
   loop ;

\ The two frame tokens, read before anything is built: the opener is the
\ intrinsic `:` read while interpreting, and the token after it is the name the
\ definition gives, read while interpreting too because `:` parses it before it
\ switches the parser to compiling.
: FRAME-READ ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ID:ir-module-key IR-ARENA:arena -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view key:IR-ID:ir-module-key
      im:IR-ARENA:arena :}
   v 0 NAME-CK
   v 0 NTAPE-MODE:INTERPRETING MODE-CK
   c b v key 0 s" :" SPELLED? 0= if E-NELAB-SHAPE throw then
   v key im 0 FRAME-CK
   v 1 NAME-CK
   v 1 NTAPE-MODE:INTERPRETING MODE-CK ;

\ The closing `;`: the same contract check the opener gets, then the return that
\ hands the word's outputs over, and then the rule that this tape held one
\ definition and nothing after it.
: FRAME-CLOSE ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ID:ir-module-key IR-ARENA:arena n n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view key:IR-ID:ir-module-key
      im:IR-ARENA:arena out:n n:n :}
   v IX @ NTAPE-MODE:COMPILING MODE-CK
   v key im IX @ FRAME-CK
   c b v key IX @ out EMIT-RETURN
   IX @ 1+ n <> if E-NELAB-SHAPE throw then ;

public

\ Elaborate the one colon definition this sealed tape holds, and answer the
\ function it became. The arenas are, in order, the tape's sealed view, the word
\ model's pick pool, the word model's rows, and the immediate-word contract
\ table; the two counts are the values the word takes and the values it leaves.
\ Every identity read off the tape is checked against this builder's module by
\ the table that owns it, so a tape of another module cannot be elaborated into
\ this one.
: COLON ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:view IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena n n -- IR-ID:ir-fun-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ARENA:view p:IR-ARENA:arena
      r:IR-ARENA:arena im:IR-ARENA:arena in:n out:n :}
   in out ARITY-CK
   b IR-BUILD:MODULE-KEY {: key:IR-ID:ir-module-key :}
   v NTAPE:TOKENS {: n:n :}
   n 3 < if E-NELAB-SHAPE throw then
   c b v key im FRAME-READ
   c b v key in out OPEN-FUN
   c b v key in OPEN-BLOCK
   c b v p r key n WALK
   c b v key im out n FRAME-CLOSE
   c b IR-BUILD:END-BLOCK drop
   c b IR-BUILD:END-FUN ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
