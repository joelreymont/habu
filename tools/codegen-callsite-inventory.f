\ codegen-callsite-inventory.f - how many of a published routine's instructions
\ exist only to move arguments and results through the caller's data stack, and
\ WHERE they sit. One concern: counting those instructions in the code a routine
\ actually holds.
\
\ WHY THE POSITION IS THE WHOLE POINT. The register-calling-convention lane
\ (dot habu-pass-args-in-da01bd62) was scoped on the belief that a chain routine
\ calling a chain routine pays a data-stack round trip per call. Whether it does
\ is a question about WHICH data-stack instructions a routine holds, not how
\ many, because the two kinds are removed by different things and one of them
\ cannot be removed at all today:
\
\   site  an access in the run immediately beside a call - the marshalling of
\         THAT call. It goes when the caller and the callee agree to pass the
\         value in a register instead, which is what the convention would buy.
\   own   an access anywhere else: the routine reading its own arguments at
\         entry and publishing its own results at exit. It goes only when
\         NOTHING outside the chain can enter the routine - and every routine
\         the chain publishes is an ordinary dictionary record, so the engine can
\         always name it, call it, and copy it.
\
\ So `site` is the convention's prize and `own` is not. A report that added the
\ two together would have said the prize was three to five instructions a routine
\ when it is one or two a call site, which is the number the decision needs.
\
\ WHAT THE MEASUREMENT SAID WHEN THIS FILE WAS WRITTEN, because a reader deserves
\ the answer and not just the instrument. Across all forty-four migrated corpus
\ rows the `site` column totals ten instructions, three of them in one self-call;
\ the pure tail rows of corpus5 carry none at all, because a tail call passes its
\ arguments by leaving them exactly where the callee already reads them and costs
\ ONE branch. The shape that pays the most - a caller that COMPUTES both
\ arguments and calls a callee too big for either inliner, in a loop - was built
\ on purpose to be the best case and carries two. Read `site` as what a register
\ convention could remove and `own` as what it could not.
\
\ WHY IT READS THE CODE AND NOT THE SOURCE, and why it reads it through
\ tools/codegen-tail-probe.f's walk: the same two reasons its sibling inventories
\ give. A Habu body that calls does not necessarily emit a call - both compilers
\ copy a small callee into its caller - and the residency machinery elides an
\ access whose value is already where it needs to be, so only the emitted code
\ knows how many are left.
\
\ WHAT A CLASSIFIER HERE IS. Not a second table of bit layouts. Each one decodes
\ the fields out of a word through tools/codegen-combine-inventory.f's decoders
\ and asks src/arch/arm64/asm.f's own encoder to write the instruction again from
\ them: the word IS that form exactly when the encoder reproduces it bit for bit.
\ That is the discipline the combining inventory states and the reason it gives -
\ the layout has one authority, the one that emitted the code.
\
\ AND A CALL SITE IS NOT ONLY A Bl. A routine that leaves through a tail branch
\ hands its arguments to another routine just as a branch-with-link does, so the
\ run beside that branch is marshalling too and is counted as such. Asking only
\ about Bl would report every tail row as having no call site at all, which is
\ true of the opcode and false of the question.

require lib/prelude.f
require lib/errors.f
require lib/string.f
require src/arch/arm64/asm.f
require src/compiler/a64-effect.f
require src/compiler/native/branch.f
require tools/codegen-tail-probe.f
require tools/codegen-combine-inventory.f

package NSITEINV

private

18 constant RESERVED-REG           \ x18: no emitted routine holds it, and every
                                   \ X-register encoder ends the process on it

: ENCODABLE? ( n -- bool ) {: r:n :}
   r RESERVED-REG <> ;

\ The forms below name two registers; the offset is a field, not a register.
: R2-OK? ( n -- bool ) {: w:n :}
   w NCOMBINV:FRD ENCODABLE?
   w NCOMBINV:FRN ENCODABLE? and ;

: LDUR? ( n -- bool ) {: w:n :}
   w R2-OK? 0= if false exit then
   w NCOMBINV:FRD  w NCOMBINV:FRN  w NCOMBINV:FSIMM9  ENC-LDUR  w = ;

: STUR? ( n -- bool ) {: w:n :}
   w R2-OK? 0= if false exit then
   w NCOMBINV:FRD  w NCOMBINV:FRN  w NCOMBINV:FSIMM9  ENC-STUR  w = ;

\ The same two forms over the D file. A double reaches and leaves the caller's
\ data stack in the register file it lives in (src/compiler/native/select.f, the
\ placement), so `ldur d0, [x19,#-8]` is exactly the argument traffic
\ `ldur x0, [x19,#-8]` is and has to be counted with it - a classifier blind to
\ them would report a float row as carrying less marshalling than it does, which
\ is the silent wrong number this file exists to prevent.
\
\ AND THEY SCREEN ONE REGISTER AND NOT TWO. The base is an X register and no
\ emitted routine may hold x18; the transferred register is a D register and the
\ floating file has no reserved member, so d18 is one the allocator really hands
\ out. Screening both would answer false for every access through it.
: LDURD? ( n -- bool ) {: w:n :}
   w NCOMBINV:FRN ENCODABLE? 0= if false exit then
   w NCOMBINV:FRD  w NCOMBINV:FRN  w NCOMBINV:FSIMM9  ENC-LDURD  w = ;

: STURD? ( n -- bool ) {: w:n :}
   w NCOMBINV:FRN ENCODABLE? 0= if false exit then
   w NCOMBINV:FRD  w NCOMBINV:FRN  w NCOMBINV:FSIMM9  ENC-STURD  w = ;

: ADDI? ( n -- bool ) {: w:n :}
   w R2-OK? 0= if false exit then
   w NCOMBINV:FRD  w NCOMBINV:FRN  w NCOMBINV:FI12  ENC-ADDI  w = ;

: SUBI? ( n -- bool ) {: w:n :}
   w R2-OK? 0= if false exit then
   w NCOMBINV:FRD  w NCOMBINV:FRN  w NCOMBINV:FI12  ENC-SUBI  w = ;

: DS ( -- n ) A64EFF:DSTACK-GPR ;

public

\ A load or a store that reaches THROUGH the data-stack pointer: one argument
\ read, one result written, or one value put across a call. Four forms, because
\ the eight bytes travel in whichever register file the value lives in and the
\ instruction that moves them is the same instruction either way.
: DACCESS? ( n -- bool ) {: w:n :}
   w LDUR? w STUR? or  w LDURD? or  w STURD? or
   w NCOMBINV:FRN DS = and ;

\ An adjustment OF that pointer. The frame's own `sub sp, sp, #16` is the very
\ same form over register 31, so the destination and the base both have to be
\ asked or every framed routine reports a phantom adjustment.
: DADJUST? ( n -- bool ) {: w:n :}
   w ADDI? w SUBI? or
   w NCOMBINV:FRD DS = and
   w NCOMBINV:FRN DS = and ;

: DSTACK? ( n -- bool ) {: w:n :}
   w DACCESS? if true exit then
   w DADJUST? ;

private

\ ---- the routine under the walk ----------------------------------------------
\ The subject is held by tools/codegen-combine-inventory.f, which owns the walk
\ this file reads through; a second copy of the name here would be a second
\ subject that could disagree with the one the instructions come from.

variable SUBJ-A
variable SUBJ-U

: NAME ( -- ptr u8 n )
   SUBJ-A 0 ptr-field @  SUBJ-U @ ;

: AT ( n -- n ) {: k:n :}
   k NCOMBINV:INSN@ ;

: IN-RANGE? ( n -- bool ) {: k:n :}
   k 0 >= k NCOMBINV:INSNS < and ;

: MARKED? ( n -- bool ) {: k:n :}
   k IN-RANGE? 0= if false exit then
   k AT DSTACK? ;

\ A branch-with-link is a call site, and so is the tail branch a routine leaves
\ through - see the head of this file.
: CALL-AT? ( n -- bool ) {: k:n :}
   k IN-RANGE? 0= if false exit then
   k AT NBR:BL? if true exit then
   k NCOMBINV:INSNS 1- =
   NAME NTAILPROBE:TAIL-BRANCH? and ;

\ The maximal contiguous run of data-stack instructions this one belongs to. A
\ run is what the validator measures a call site as, so it is what decides here
\ too: one access of the run beside a call makes the whole run that call's.
: RUN-LO ( n -- n ) {: k:n :}
   k begin dup 1- MARKED? while 1- repeat ;

: RUN-HI ( n -- n ) {: k:n :}
   k begin dup 1+ MARKED? while 1+ repeat ;

: SITE? ( n -- bool ) {: k:n :}
   k RUN-LO 1- CALL-AT?
   k RUN-HI 1+ CALL-AT? or ;

variable N-SITE
variable N-OWN
variable N-CALL

: RESET ( -- )
   0 N-SITE ! 0 N-OWN ! 0 N-CALL ! ;

: SCAN ( -- )
   RESET
   NCOMBINV:INSNS 0 ?do
      i AT NBR:BL? if 1 N-CALL +! then
      i MARKED? if
         i SITE? if 1 N-SITE +! else 1 N-OWN +! then
      then
   loop ;

public

\ Name the routine to be counted. It sets the shared walk's subject too, so a
\ caller that reads NCOMBINV:INSNS after this one is reading the same routine.
: ROW! ( ptr u8 n -- ) {: a:ptr u:n :}
   a SUBJ-A !  u SUBJ-U !
   a u NCOMBINV:ROW!
   SCAN ;

\ What a register calling convention could remove: the marshalling beside a call.
: SITE ( -- n )
   N-SITE @ ;

\ What it could not: the routine's own crossing into the data stack, which is
\ there because something outside the chain can enter it.
: OWN ( -- n )
   N-OWN @ ;

: CALLS ( -- n )
   N-CALL @ ;

: TOTAL ( -- n )
   N-SITE @ N-OWN @ + ;

;package
