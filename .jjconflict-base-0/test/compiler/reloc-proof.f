\ reloc-proof.f - the snapshot relocation parity gate.
\
\ `formal/Common/Reloc.v` proves the one invariant a portable snapshot image
\ rests on: for every RECORDED site, writer-side canonicalization composed
\ with loader-side rebase is the IDENTITY, for any writer base and any loader
\ base within BL's reach; a site the map does not record is never touched; a
\ recorded site that does not hold what the map says it holds is refused with
\ the shipped exit status rather than rewritten; a declared address cell makes
\ the same round trip through the RBASE-VA sentinel, with a cleared cell staying
\ cleared; and an address literal compiled into region code as a
\ four-instruction MOVZ/MOVK chain makes it too, all four immediates together,
\ against the band the pass is given. Until
\ this gate exists those are theorems about a model, and
\ `src/habu/habu2.f` is a separate description of the same design, with nothing
\ stopping the two drifting apart. The costliest defects of this campaign - a
\ deleted BL relocation pass, a displacement lottery, stale persisted cells -
\ were each a way of breaking an invariant nobody had written down.
\
\ One artifact, two readers. `test/compiler/reloc-schema.f` holds the pinned
\ band constants, the frozen writer bodies and the vector rows.
\ `test/compiler/reloc-cases.f` asks the shipped passes about them and
\ `test/compiler/reloc-obligations.f` asks Rocq about the same rows. Neither
\ side carries a copy.
\
\ How the shipped side is reached. The three passes are EMITTED ASSEMBLY: Forth
\ words that write AArch64 instructions into the engine being built. No test can
\ call the machine code they produce. So `package RELOC-VM` decodes each
\ definition's own instruction sequence out of habu2.f, through the shared
\ source lexer, and RUNS it over a real region image and a real call-map band.
\ What the rows are asked of is therefore the arithmetic in habu2.f, operand for
\ operand - not a second copy of it written here. Measured: skewing the shift
\ constant, the field width, the opcode operand of the call check, the site
\ index shift, or the opcode-preserving write in EMIT-CALLS each turns this gate
\ red, and removing the call check makes the refusal row pass a data word
\ through. The same was measured for the address-literal pass: dropping any one
\ of its four immediate rewrites, dropping the band's lower-bound test, or
\ dropping the fourth scaffold check each turns this gate red, on the chain rows
\ and on nothing else.
\
\ What the gate refuses:
\
\   - a renumbered band constant - REGION-OFF, RBASE-VA, BL-REACH, REGION,
\     CALLMAP-RC or BL-OP-HI - through the pinned literal read out of the
\     shipped source;
\   - a changed address-cell body in the builder-only snapshot writer, through
\     the frozen token runs;
\   - a step of any call or address-cell row, through the shipped instruction
\     sequence run on the Habu side and the generated Rocq obligation built from
\     the same row;
\   - an instruction, condition code, operand spelling or memory access the
\     machine was never taught, and a pass that neither returns nor exits,
\     through the machine's own fail-closed refusals;
\   - a rewritten theorem statement, through the statement each manifest row
\     pins. The row's type is ascribed to a generated definition whose body is
\     the theorem, so Rocq itself has to accept the proved statement as the one
\     the committed manifest wrote down;
\   - an unbound theorem, a manifest row for a theorem that no longer exists, a
\     published result the file forgets to query, or an `Admitted`, through the
\     structural inventory of what `Reloc.v` declares and what it queries;
\   - ANY assumption at all. The relocation proofs rest on nothing, so this gate
\     refuses an assumption twice over: the manifest is read with assumption
\     rows forbidden, so one cannot be written down, and every statement must
\     additionally report "Closed under the global context" with no `Axioms:`
\     header anywhere in what Rocq printed.
\
\ Completeness, which the round trip alone does not give. "Every recorded site
\ survives" is vacuously true of an address class nobody records, and that is
\ how a JIT-region address baked into region code as a MOVZ/MOVK chain came to
\ crash a restored image. So the gate also enumerates the emit vocabulary that
\ can bake an address into region bytes and classifies every member, with the
\ classification a total function in the model - a producer added without a
\ class is a Rocq error - and the vocabulary rebuilt from src/habu/habu2.f
\ itself, not listed in a comment. Measured: a new word that calls the shared
\ MOVZ/MOVK carrier, a second hand-built copy of that chain, and C-CODE-ADDR
\ ceasing to record its site each turn this gate red. The model states, and this
\ gate holds, that a snapshot restore now replays the table of EVERY producer
\ whose bytes move with the region; classify one to a table the restore does not
\ walk and the model stops compiling.
\
\ What it does NOT prove. The machine reads mnemonics and operands, not the
\ encoded instruction words: the encoders in src/arch/arm64 sit between this
\ source and the bytes a CPU runs, and they have their own tests. So this gate
\ binds the model to the shipped INSTRUCTION SEQUENCE, one step short of the
\ shipped bytes. Closing that last step needs a real snapshot write-then-boot
\ under a skewed constant, and that measurement is still blocked: a restored
\ image cannot yet compile a definition, for a reason of a different class that
\ has its own dot, so there is no green write-then-boot baseline to break. The
\ dot records the measurements that were taken instead, including a full engine
\ rebuild in both directions around a deliberately skewed address pass.
\
\ Focused command: `bin/hb --load test/compiler/reloc-proof.f`. The gate
\ compiles the model itself, so nothing has to be built first.

require lib/prelude.f
require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/test.f
require lib/test/outcome.f
require test/compiler/proof-manifest.f
require test/compiler/rocq-run.f
require test/compiler/reloc-cases.f
require test/compiler/reloc-obligations.f

package RELOC-PROOF-GATE
using RELOC-PROOF
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
   s" test/compiler/reloc-axioms.txt" ;

\ ---- the manifest, read with assumptions forbidden ---------------------------

\ A malformed manifest, or one that tries to claim an assumption, is refused when
\ it is read. That refusal is caught here only so the run says which rule the
\ committed file broke before it stops; the code is rethrown, because every later
\ phase reads the manifest and none of them can mean anything without it.
: READ-MANIFEST ( -- )
   false PROOF-MANIFEST:AXIOMS-ALLOWED!
   [: MANIFEST-PATH$ PROOF-MANIFEST:READ ;] catch {: rc:n :}
   s" the committed manifest is well formed and claims no assumption" T-LABEL
   rc 0 T=
   rc 0 <> if rc throw then ;

\ The grammar's shape rules belong to `package PROOF-MANIFEST` and are proved by
\ the identity gate. What this gate owns is the strength of its assumption claim,
\ so these fixtures are about that and nothing else.

: HOSTILE-AXIOM-ROW ( -- )
   s" an assumption row cannot be written into a manifest held at empty" T-LABEL
   [: false PROOF-MANIFEST:AXIOMS-ALLOWED!
      S\" theorem Habu.Common.Reloc.image_round_trip\ntype True\naxiom host_relocation : reloc_impl"
      PROOF-MANIFEST:TEXT ;]
      E-CID-AXIOM TTHROWSQ ;

: HOSTILE-AXIOM-AFTER-CLOSED ( -- )
   s" an assumption row after a closed row is refused just the same" T-LABEL
   [: false PROOF-MANIFEST:AXIOMS-ALLOWED!
      S\" theorem Habu.Common.Reloc.image_round_trip\ntype True\nclosed\naxiom whatever : Prop"
      PROOF-MANIFEST:TEXT ;]
      E-CID-AXIOM TTHROWSQ ;

: MANIFEST-CLOSED-PAIR ( -- )
   false PROOF-MANIFEST:AXIOMS-ALLOWED!
   S\" theorem Habu.Common.Reloc.image_round_trip\n# a note\ntype True\nclosed"
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
   MOD-N @ MOD-MAX >= if E-CRL-STRUCT throw then
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
   s" Habu.Common.Reloc." ;

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
   s" no statement in the relocation proofs is admitted" T-LABEL
   ADMITTED-N @ 0 T=
   s" every module the walk opened was closed again" T-LABEL
   MOD-N @ 0 T= ;

private

\ ---- making Rocq answer ------------------------------------------------------

: SCRATCH-DIR ( -- ptr u8 n )
   s" habu-reloc-parity" TMPDIR-MKDIR ;

: OBLIGATION-ROW ( n -- ) {: k:n :}
   k PROOF-MANIFEST:THEOREM$ k PROOF-MANIFEST:TYPE$ RELOC-ROCQ:STATEMENT+
   k PROOF-MANIFEST:THEOREM$ RELOC-ROCQ:ASSUMPTION+ ;

: EMIT-OBLIGATIONS ( -- )
   RELOC-ROCQ:START
   PROOF-MANIFEST:THEOREMS 0 ?do i OBLIGATION-ROW loop ;

: WRITE-OBLIGATIONS ( ptr u8 n -- ) {: dir:ptr diru:n :}
   dir diru s" RelocParity.v" VPATH JOIN-PATH VPATH-U !
   VPATH VPATH-U @ RELOC-ROCQ:TEXT$ ATOMIC-WRITE-FILE ;

public

: PHASE-ROCQ ( -- )
   s" the relocation model compiles" T-LABEL
   MODEL-FILE$ ROCQ-CMD:COMPILE
   EMIT-OBLIGATIONS
   CLEANUP-RESET
   SCRATCH-DIR 2dup CLEANUP-TREE+ WRITE-OBLIGATIONS
   s" every generated relocation obligation is proved" T-LABEL
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
   RELOC-CASES:HABU-SIDE
   PHASE-MANIFEST
   PHASE-DECLARATIONS
   PHASE-ROCQ
   PHASE-ASSUMPTIONS
   T-REPORT ;

;using
;package

RELOC-PROOF-GATE:RUN
