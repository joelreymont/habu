\ codegen-verify-dump.f - the machine module the register verifier judged,
\ printed at the moment it judges it. One concern: turning a data-stack refusal
\ into the operations it was about instead of a bare throw code.
\
\ WHY IT EXISTS. src/compiler/native/regalloc-verify.f refuses a data-stack
\ access the emission had no reason to make under one name, E-A64RAV-DKEEP, and
\ the name covers three different findings: a load into a slot that already holds
\ a named value, a load whose result nothing reads, and a store of the value the
\ slot already holds. Which one fired, and at which operation, is the whole of
\ what a diagnosis needs and none of what the caller sees. The 122-definition
\ residency defect (the emission and the planning disagreeing about what an
\ addressed store did to the map) was found by a scratch build that split the
\ code into three and printed the module; this is that instrument, kept.
\
\ WHAT EACH LINE MEANS.
\
\   verify rc            the code the migration threw, or 0
\   verify judged no     the run never reached the residency check, so there is
\                        no module to print - an earlier stage refused it, and
\                        `verify rc` is that stage's code
\   verify clause        which of the three findings, by number and by name, or
\                        `none` when the run reached the end without one
\   verify at            the block and position the refused operation stands at,
\                        found by walking the judged function rather than
\                        recorded, so a number that no longer names an operation
\                        of that function reads as `-1 -1` instead of as a
\                        position
\   verify blk           one block of the judged function, and how many
\                        operations it holds
\   verify op            one operation: its position, its shape, the data-stack
\                        slot it moves if it moves one, and its results with the
\                        USE COUNT the verifier itself computed. The use count is
\                        the column the dead-load finding is about, so a load
\                        printed with `u0` in front of a store that was elided is
\                        the defect this file was written for, on one line.
\
\ WHERE THE LINES COME FROM, because it decides how it is used. The arena the
\ module lives in is torn down with the run, so nothing can walk it afterwards -
\ A64RAV:DKEEP-HOOK is the door the check opens at the moment it finishes or
\ refuses, and every line above `verify rc` is printed through it. The rc is what
\ the run answered after that, which is why it comes last.
\
\ HOW TO USE IT.
\
\   bin/hb --load tools/codegen-verify-dump.f /tmp/dump-run.f
\
\ where the second file is one call on the source in question. A refusal is
\ caught either way, so a refused module is dumped exactly like an accepted one.
\
\   VERIFY-DUMP:REPORT          any body, whether it calls or not. It goes
\                               through NMIGRATE:MEASURE-HELD, the census entry
\                               that runs every stage and keeps nothing: a body
\                               that names another word has that name resolved
\                               off the dictionary like any other, so a call
\                               site needs nothing of the caller but the source.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/fmt.f
require src/compiler/native/frozen.f
require src/compiler/native/regalloc-verify.f
require src/compiler/native/migrate.f

package VERIFY-DUMP
using NFROZEN

private

PTR-VARIABLE SRC
variable SRC-U

\ The migration, caught. Nothing is published: MEASURE-HELD runs every stage the
\ publishing entries run and retracts everything on the way out, so a body can be
\ dumped as many times as it takes without filling the address-keyed records.
: RUN-RC ( -- n )
   [: SRC @ SRC-U @ NMIGRATE:MEASURE-HELD ;]
   catch ;

: LBL ( ptr u8 n -- )   type ;
: NUM ( n -- )          FMT:.INT ;
: NL ( -- )             s\" \n" type ;

: SAY-N ( ptr u8 n n -- )
   {: v:n :}
   LBL v NUM NL ;

\ ---- one operation's shape ---------------------------------------------------
\ Named from what the operation IS rather than from its opcode's spelling, which
\ is the same discipline every consumer of this dialect keeps: a call is a call
\ because the check says so, and a data-stack access is one because it carries a
\ slot.
: SHAPE$ ( IR-ID:ir-op-id -- ptr u8 n )
   {: id:IR-ID:ir-op-id :}
   id A64RAV:DKEEP-CALL? if s" call" exit then
   id A64RAV:DKEEP-DSLOT 0 < if s" -" exit then
   id A64RAV:DKEEP-STORES? if s" dstore" exit then
   s" dload" ;

: SAY-RESULTS ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id RESULTS-OF 0= if exit then
   s"  ->" LBL
   id RESULTS-OF 0 ?do
      s"  u" LBL  id i RESULT-AT A64RAV:DKEEP-USES NUM
   loop ;

: SAY-OP ( n IR-ID:ir-op-id -- )
   {: at:n id:IR-ID:ir-op-id :}
   s" verify op " LBL at NUM
   s"  " LBL id SHAPE$ LBL
   id A64RAV:DKEEP-DSLOT 0 >= if
      s"  dslot " LBL id A64RAV:DKEEP-DSLOT NUM
   then
   s"  in " LBL id OPERANDS-OF NUM
   id SAY-RESULTS
   NL ;

: SAY-BLOCK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   s" verify blk " LBL b NUM  s"  ops " LBL bk OP-COUNT NUM  NL
   bk OP-COUNT 0 ?do  i  bk i OP-AT  SAY-OP  loop ;

\ ---- where the refused operation stands --------------------------------------
\ The position is searched for rather than recorded, so a recorded operation that
\ is not in the judged function cannot be printed as a position in it. Identity
\ is the module-local ordinal, which is what names one operation of one module.
: SAME-OP? ( IR-ID:ir-op-id IR-ID:ir-op-id -- bool )
   {: a:IR-ID:ir-op-id b:IR-ID:ir-op-id :}
   a IR-ID:OP-LOCAL  b IR-ID:OP-LOCAL  = ;

variable AT-BLK
variable AT-POS

: FIND-IN-BLOCK ( IR-ID:ir-block-id IR-ID:ir-op-id n -- )
   {: bk:IR-ID:ir-block-id want:IR-ID:ir-op-id b:n :}
   bk OP-COUNT 0 ?do
      bk i OP-AT want SAME-OP? if
         b AT-BLK !  i AT-POS !  leave
      then
   loop ;

: SAY-AT ( -- )
   A64RAV:DKEEP-FUN {: f:IR-ID:ir-fun-id :}
   A64RAV:DKEEP-OP {: want:IR-ID:ir-op-id :}
   -1 AT-BLK !  -1 AT-POS !
   f BLOCK-COUNT 0 ?do
      AT-POS @ 0 < if  f i BLOCK-AT want i FIND-IN-BLOCK  then
   loop
   s" verify at blk " LBL AT-BLK @ NUM  s"  pos " LBL AT-POS @ NUM  NL ;

: SAY-FUN ( -- )
   A64RAV:DKEEP-FUN {: f:IR-ID:ir-fun-id :}
   f BLOCK-COUNT 0 ?do f i SAY-BLOCK loop ;

\ ---- the dump, run from inside the judgement ---------------------------------
\ The arena the module lives in is torn down with the run, so every walk above
\ happens here: at the moment the residency check finishes or refuses, with the
\ module still standing. A64RAV:DKEEP-HOOK is the door the check opens for it.
: DUMP ( -- )
   s" verify clause " LBL
   A64RAV:DKEEP-CLAUSE NUM
   s"  " LBL  A64RAV:DKEEP-CLAUSE A64RAV:DKEEP-CLAUSE$ LBL  NL
   A64RAV:DKEEP-CLAUSE 0<> if SAY-AT then
   SAY-FUN ;

: HOOK-ON ( -- )
   [: DUMP ;] is A64RAV:DKEEP-HOOK ;

public

\ Compile this source through the production migration entry and print what the
\ register verifier judged, whether it was accepted or refused. The source is the
\ whole of what a case says: the arity is the checker's and the pool is the
\ machine's. The module's lines come out BEFORE the rc, because they are printed
\ at the moment it was judged and the rc is what the run answered afterwards.
: REPORT ( ptr u8 n -- )
   {: a:ptr u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   a SRC ! u SRC-U !
   HOOK-ON
   s" verify rc " RUN-RC SAY-N
   A64RAV:DKEEP-HELD? 0= if s" verify judged no" LBL NL then ;

;package
