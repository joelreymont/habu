\ insn-proof.f - the instruction-encoding parity gate.
\
\ `formal/Common/Insn.v` states what the engine's ARM64 assembler is supposed to
\ emit: decoding an encoded well-formed instruction gives it back, two different
\ well-formed instructions never encode to the same word, no 32-bit word matches
\ two decoder rows, the opcode bits and the operand fields tile the whole word
\ without overlapping, the shipped 32-bit mask never truncates a well-formed
\ encoding, and - as findings rather than design - an out-of-range operand is
\ not refused but silently encodes a different instruction or none at all, and
\ four forms take an X register no reserved-register check reaches. Until this
\ gate exists those are theorems about a model, and `src/arch/arm64/asm.f` with
\ its layers in `src/arch/arm64/mnem.f` and `src/arch/arm64/icode.f` is a
\ separate description of the same encodings, with nothing stopping the two
\ drifting apart.
\
\ One artifact, two readers. `test/compiler/insn-schema.f` holds the encoding
\ vectors, the overflow vectors, the reserved-register rows and the
\ logical-immediate packings. `test/compiler/insn-cases.f` drives them through
\ the real emitter words into the real code buffer, and
\ `test/compiler/insn-obligations.f` asks Rocq about the same rows. Neither side
\ carries a copy.
\
\ What the gate refuses:
\
\   - a field that moved, a base opcode that changed, or a scale that stopped
\     dividing, through the emitted word of every row on the Habu side and the
\     generated Rocq obligation built from that same row;
\   - a reserved-register check that appeared or disappeared, through the child
\     engines that run the refusing rows and read their exit status, and through
\     the rows the shipped code lets through unrefused;
\   - a rewritten theorem statement, through the statement each manifest row
\     pins. The row's type is ascribed to a generated definition whose body is
\     the theorem, so Rocq itself has to accept the proved statement as the one
\     the committed manifest wrote down;
\   - an unbound theorem, a manifest row for a theorem that no longer exists, a
\     published result the file forgets to query, or an `Admitted`, through the
\     structural inventory of what `Insn.v` declares and what it queries;
\   - ANY assumption at all. The instruction proofs rest on nothing, so this
\     gate refuses an assumption twice over: the manifest is read with
\     assumption rows forbidden, so one cannot be written down, and every
\     statement must additionally report "Closed under the global context" with
\     no `Axioms:` header anywhere in what Rocq printed.
\
\ Focused command: `bin/hb --load test/compiler/insn-proof.f`. The gate compiles
\ the model itself, so nothing has to be built first.

require lib/prelude.f
require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/test.f
require lib/test/outcome.f
require test/compiler/proof-manifest.f
require test/compiler/rocq-run.f
require test/compiler/ir-id-source.f
require test/compiler/insn-cases.f
require test/compiler/insn-obligations.f

package COMPILER-INSN-PROOF-GATE
using COMPILER-INSN-PROOF
private

$100 constant PATH-CAP
8 constant MOD-MAX
$2E constant DOT

create VPATH PATH-CAP allot
create MOD-TOK MOD-MAX cells allot

variable VPATH-U
variable MOD-N
variable DECL-N
variable QUERY-N
variable ADMITTED-N

: MANIFEST-PATH$ ( -- ptr u8 n )
   s" test/compiler/insn-axioms.txt" ;

\ ---- the manifest, read with assumptions forbidden ---------------------------

\ A malformed manifest, or one that tries to claim an assumption, is refused
\ when it is read. That refusal is caught here only so the run says which rule
\ the committed file broke before it stops; the code is rethrown, because every
\ later phase reads the manifest and none of them can mean anything without it.
: READ-MANIFEST ( -- )
   false PROOF-MANIFEST:AXIOMS-ALLOWED!
   [: MANIFEST-PATH$ PROOF-MANIFEST:READ ;] catch {: rc:n :}
   s" the committed manifest is well formed and claims no assumption" T-LABEL
   rc 0 T=
   rc 0 <> if rc throw then ;

\ The grammar's shape rules belong to `package PROOF-MANIFEST` and are proved by
\ the identity gate. What this gate owns is the strength of its assumption
\ claim, so these fixtures are about that and nothing else.

: HOSTILE-AXIOM-ROW ( -- )
   s" an assumption row cannot be written into a manifest held at empty" T-LABEL
   [: false PROOF-MANIFEST:AXIOMS-ALLOWED!
      S\" theorem Habu.Common.Insn.decode_encode\ntype True\naxiom host_encoder : encode_impl"
      PROOF-MANIFEST:TEXT ;]
      E-CID-AXIOM TTHROWSQ ;

: HOSTILE-AXIOM-AFTER-CLOSED ( -- )
   s" an assumption row after a closed row is refused just the same" T-LABEL
   [: false PROOF-MANIFEST:AXIOMS-ALLOWED!
      S\" theorem Habu.Common.Insn.decode_encode\ntype True\nclosed\naxiom whatever : Prop"
      PROOF-MANIFEST:TEXT ;]
      E-CID-AXIOM TTHROWSQ ;

: MANIFEST-CLOSED-PAIR ( -- )
   false PROOF-MANIFEST:AXIOMS-ALLOWED!
   S\" theorem Habu.Common.Insn.decode_encode\n# a note\ntype True\nclosed"
   PROOF-MANIFEST:TEXT
   s" a manifest held at empty still reads a theorem and its statement" T-LABEL
   PROOF-MANIFEST:THEOREMS 1 T=
   s" the pinned statement is exactly the type row's text" T-LABEL
   0 PROOF-MANIFEST:TYPE$ s" True" T$= ;

public

: PHASE-MANIFEST ( -- )
   HOSTILE-AXIOM-ROW
   HOSTILE-AXIOM-AFTER-CLOSED
   MANIFEST-CLOSED-PAIR ;

private

\ ---- what the model file actually declares -----------------------------------
\ Read structurally through the shared source lexer. A Rocq name carries the
\ trailing period of the command it ends, and a name inside a `Module` is
\ qualified by it, so both are handled here rather than assumed away.

: STRIP-DOT$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   u 0= if a u exit then
   a u 1- + c@ DOT = if a u 1- exit then
   a u ;

: TOK$ ( n -- ptr u8 n )
   COMPILER-ID-SRC:TOKEN$ ;

: NAME-AT$ ( n -- ptr u8 n )
   TOK$ STRIP-DOT$ ;

: TOK-IS? ( n ptr u8 n -- bool ) {: k:n a:ptr u:n :}
   k TOK$ a u STR= ;

: MOD-PUSH ( n -- ) {: k:n :}
   MOD-N @ MOD-MAX >= if E-CIE-ROW throw then
   k MOD-TOK MOD-N @ cells + !
   MOD-N @ 1+ MOD-N ! ;

\ A `Section` closes with the same word a `Module` does, so a closer only pops
\ when it names the module actually open.
: MOD-POP ( n -- ) {: k:n :}
   MOD-N @ 0= if exit then
   MOD-TOK MOD-N @ 1- cells + @ NAME-AT$ {: a:ptr u:n :}
   k NAME-AT$ a u STR= 0= if exit then
   MOD-N @ 1- MOD-N ! ;

: MODEL-PREFIX$ ( -- ptr u8 n )
   s" Habu.Common.Insn." ;

\ The fully qualified name of something declared at this point in the walk.
: QUALIFIED$ ( n -- ptr u8 n ) {: k:n :}
   SB-RESET
   MODEL-PREFIX$ SB-APPEND
   MOD-N @ 0 ?do
      MOD-TOK i cells + @ NAME-AT$ SB-APPEND
      s" ." SB-APPEND
   loop
   k NAME-AT$ SB-APPEND
   SB$ ;

: DECL-HEAD? ( n -- bool ) {: k:n :}
   k s" Theorem" TOK-IS? if true exit then
   k s" Corollary" TOK-IS? ;

: ADMITTED-TOKEN? ( n -- bool ) {: k:n :}
   k s" Admitted" TOK-IS? if true exit then
   k s" Admitted." TOK-IS? if true exit then
   k s" admit" TOK-IS? ;

: QUERY-HEAD? ( n -- bool ) {: k:n :}
   k s" Print" TOK-IS? 0= if false exit then
   k 1+ s" Assumptions" TOK-IS? ;

: DECL-ROW ( n -- ) {: k:n :}
   s" every published result is bound by the committed manifest" T-LABEL
   DECL-N @ PROOF-MANIFEST:THEOREMS < TTRUE
   DECL-N @ PROOF-MANIFEST:THEOREMS < if
      s" the manifest binds that result, at that position" T-LABEL
      k 1+ QUALIFIED$ DECL-N @ PROOF-MANIFEST:THEOREM$ T$=
   then
   DECL-N @ 1+ DECL-N ! ;

\ The model file ends with one `Print Assumptions` per published result. That
\ list is what the gate reads the assumption set back out of, so a result the
\ file forgets to query would never be asked what it rests on.
: QUERY-ROW ( n -- ) {: k:n :}
   s" every assumption query is bound by the committed manifest" T-LABEL
   QUERY-N @ PROOF-MANIFEST:THEOREMS < TTRUE
   QUERY-N @ PROOF-MANIFEST:THEOREMS < if
      s" the manifest binds that query, at that position" T-LABEL
      SB-RESET MODEL-PREFIX$ SB-APPEND k 2 + NAME-AT$ SB-APPEND SB$
         QUERY-N @ PROOF-MANIFEST:THEOREM$ T$=
   then
   QUERY-N @ 1+ QUERY-N ! ;

: WALK-TOKEN ( n -- ) {: k:n :}
   k s" Module" TOK-IS? if k 1+ MOD-PUSH exit then
   k s" End" TOK-IS? if k 1+ MOD-POP exit then
   k ADMITTED-TOKEN? if ADMITTED-N @ 1+ ADMITTED-N ! then
   k DECL-HEAD? if k DECL-ROW exit then
   k QUERY-HEAD? if k QUERY-ROW then ;

public

: PHASE-DECLARATIONS ( -- )
   READ-MANIFEST
   MODEL-FILE$ COMPILER-ID-SRC:SCAN-FILE
   0 MOD-N !
   0 DECL-N !
   0 QUERY-N !
   0 ADMITTED-N !
   COMPILER-ID-SRC:TOKENS 0 ?do i WALK-TOKEN loop
   s" the manifest names every published result and no others" T-LABEL
   DECL-N @ PROOF-MANIFEST:THEOREMS T=
   s" every published result is queried for what it rests on" T-LABEL
   QUERY-N @ PROOF-MANIFEST:THEOREMS T=
   s" every named result also has its statement pinned" T-LABEL
   PROOF-MANIFEST:TYPES PROOF-MANIFEST:THEOREMS T=
   s" no statement in the instruction proofs is admitted" T-LABEL
   ADMITTED-N @ 0 T=
   s" every module the walk opened was closed again" T-LABEL
   MOD-N @ 0 T= ;

private

\ ---- making Rocq answer ------------------------------------------------------

: SCRATCH-DIR ( -- ptr u8 n )
   s" habu-insn-parity" TMPDIR-MKDIR ;

: OBLIGATION-ROW ( n -- ) {: k:n :}
   k PROOF-MANIFEST:THEOREM$ k PROOF-MANIFEST:TYPE$ COMPILER-INSN-ROCQ:STATEMENT+
   k PROOF-MANIFEST:THEOREM$ COMPILER-INSN-ROCQ:ASSUMPTION+ ;

: EMIT-OBLIGATIONS ( -- )
   COMPILER-INSN-ROCQ:START
   PROOF-MANIFEST:THEOREMS 0 ?do i OBLIGATION-ROW loop ;

: WRITE-OBLIGATIONS ( ptr u8 n -- ) {: dir:ptr diru:n :}
   dir diru s" InsnParity.v" VPATH JOIN-PATH VPATH-U !
   VPATH VPATH-U @ COMPILER-INSN-ROCQ:TEXT$ ATOMIC-WRITE-FILE ;

public

: PHASE-ROCQ ( -- )
   s" the instruction model compiles" T-LABEL
   MODEL-FILE$ ROCQ-CMD:COMPILE
   EMIT-OBLIGATIONS
   CLEANUP-RESET
   SCRATCH-DIR 2dup CLEANUP-TREE+ WRITE-OBLIGATIONS
   s" every generated instruction obligation is proved" T-LABEL
   VPATH VPATH-U @ ROCQ-CMD:COMPILE
   ROCQ-CMD:OUT$ PROOF-MANIFEST:RENDER
   CLEANUP-RUN ;

\ ---- the assumption set, held at empty ---------------------------------------

: PHASE-ASSUMPTIONS ( -- )
   s" the reported assumption set is the committed manifest, entire" T-LABEL
   PROOF-MANIFEST:GOT$ PROOF-MANIFEST:WANT$ T$=
   s" every published result reports closed under the global context" T-LABEL
   PROOF-MANIFEST:CLOSED PROOF-MANIFEST:THEOREMS T=
   s" no result rests on an external assumption" T-LABEL
   PROOF-MANIFEST:BEARING 0 T=
   s" the whole external assumption set is empty" T-LABEL
   PROOF-MANIFEST:AXIOM-COUNT 0 T= ;

: RUN ( -- )
   T-RESET
   COMPILER-INSN-CASES:HABU-SIDE
   COMPILER-INSN-CASES:REFUSAL-SIDE
   PHASE-MANIFEST
   PHASE-DECLARATIONS
   PHASE-ROCQ
   PHASE-ASSUMPTIONS
   T-REPORT ;

;using
;package

COMPILER-INSN-PROOF-GATE:RUN
