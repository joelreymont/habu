\ codegen-alloc-dump.f - what the register allocator decided, read back after
\ the run that used it. One concern: turning a refused migration into the
\ allocator's own answers instead of a bare throw code.
\
\ WHY IT EXISTS. When src/compiler/native/regalloc-verify.f refuses an
\ allocation, the caller sees one number and nothing else - and the numbers it
\ can see are about the wrong thing: E-A64RAV-REGISTER says "some value holds a
\ register the contract does not allow", which is also what it says when a value
\ holds NO register at all. Which value, whether the walk had decided to evict
\ it, and whether the lowering pass ever ran on the module being emitted are all
\ recorded in the allocator - and every one of them is already published, since
\ src/compiler/native/regalloc.f seals a walk and answers about it until the
\ next walk starts. Nothing resets that seal on the way out of a refusal, so the
\ answers are still there to be read; this file reads them and prints them.
\
\ WHAT IT SHOWS AND WHAT EACH LINE MEANS.
\
\   alloc rc           the code the migration threw, or 0
\   alloc gen          which walk is sealed, counted over the whole process. A
\                      migration whose first walk decides nothing runs one walk;
\                      one that lowers a plan runs a second over the rewritten
\                      module, so the number advances by one or by two per case
\                      and which it was IS the answer to "did the lowering pass
\                      run at all".
\   alloc first-slots  how many frame slots the FIRST walk handed out, and
\   alloc first-remats how many values it decided to write again instead. These
\                      two are read off the migration rather than off the seal,
\                      because the second walk's own answers replace the first
\                      walk's - so they are the only view of what was decided
\                      about the module the definition arrived as.
\   alloc spills       how many FRAME SLOTS the walk handed out. This is the
\                      number src/compiler/native/migrate.f reads to decide
\                      whether the lowering pass runs at all.
\   alloc plan         how many decisions the walk made, of every kind.
\   alloc plan-row     one decision: its kind, the value, the block and the
\                      position it stands in front of.
\   alloc value        one value the walk gave no register to: its claim (-1
\                      for none) and its slot (-1 for none). A value with
\                      neither is one the walk decided to deal with some other
\                      way, which is what the emitted module must not contain.
\
\ HOW TO USE IT. Stage a definition's source and run it; nothing is published,
\ because the run goes through NMIGRATE:MEASURE-HELD, the census entry that runs
\ every stage and keeps nothing.
\
\   bin/hb --load tools/codegen-alloc-dump.f tools/codegen-alloc-dump-run.f
\
\ where the second file is one ALLOC-DUMP:REPORT call on the source in question.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require lib/fmt.f
require src/compiler/native/regalloc.f
require src/compiler/native/migrate.f

package ALLOC-DUMP

private

PTR-VARIABLE SRC
variable SRC-U
variable SRC-IN
variable SRC-OUT
variable SRC-REGS

\ The migration, caught. Nothing is published: MEASURE-HELD runs every stage the
\ publishing entries run and retracts everything on the way out, so a body can be
\ dumped as many times as it takes without filling the address-keyed records.
: RUN-RC ( -- n )
   [: SRC @ SRC-U @ SRC-IN @ SRC-OUT @ SRC-REGS @ NMIGRATE:MEASURE-HELD ;]
   catch ;

: LBL ( ptr u8 n -- )
   type ;

: NUM ( n -- )
   FMT:.INT ;

: NL ( -- )
   s\" \n" type ;

\ ---- the three kinds of line -------------------------------------------------
: SAY-N ( ptr u8 n n -- )
   {: v:n :}
   LBL v NUM NL ;

\ Which decision this row is. The kinds are asked in the order the lowering pass
\ asks them, and the last is named by exclusion exactly as that dispatch names
\ it: a row that is none of the first three is a reload.
: KIND$ ( n -- ptr u8 n )
   {: j:n :}
   j A64RA:PLAN-STORE? if s" store" exit then
   j A64RA:PLAN-MOVE? if s" move" exit then
   j A64RA:PLAN-REMAT? if s" remat" exit then
   s" reload" ;

: SAY-ROW ( n -- )
   {: j:n :}
   s" alloc plan-row " LBL
   j KIND$ LBL
   s"  value " LBL j A64RA:PLAN-VALUE@ NUM
   s"  block " LBL j A64RA:PLAN-BLOCK@ NUM
   s"  pos " LBL j A64RA:PLAN-POS@ NUM
   NL ;

: SAY-VALUE ( n -- )
   {: k:n :}
   s" alloc value " LBL k NUM
   s"  claim " LBL k A64RA:CLAIM@ NUM
   s"  slot " LBL k A64RA:SLOT@ NUM
   NL ;

\ Only the values the walk gave no register to are printed. A module of any size
\ has far more values than decisions, and a value holding a register is not what
\ any question this file answers is about.
: SAY-VALUES ( -- )
   A64RA:VALUES 0 ?do
      i A64RA:CLAIM@ 0 < if i SAY-VALUE then
   loop ;

: SAY-PLAN ( -- )
   A64RA:PLAN-N 0 ?do i SAY-ROW loop ;

\ The whole of the sealed walk. A walk that is not sealed is said so rather than
\ asked, because every reader below would throw E-A64RA-STATE and the throw would
\ replace the answer the caller came for.
: SAY-SEALED ( -- )
   A64RA:SEALED? 0= if s" alloc sealed no" LBL NL exit then
   s" alloc gen " A64RA:GEN SAY-N
   s" alloc spills " A64RA:SPILLS SAY-N
   s" alloc plan " A64RA:PLAN-N SAY-N
   SAY-PLAN
   SAY-VALUES ;

public

\ Compile this source through the production migration entry and print what the
\ allocator decided, whether it was accepted or refused. The arities and the
\ register budget are the caller's, so a case can be dumped at exactly the
\ contract that refused it.
: REPORT ( ptr u8 n n n n -- )
   {: a:ptr u:n in:n out:n regs:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   a SRC ! u SRC-U ! in SRC-IN ! out SRC-OUT ! regs SRC-REGS !
   s" alloc rc " RUN-RC SAY-N
   s" alloc first-slots " NMIGRATE:SPILLS SAY-N
   s" alloc first-remats " NMIGRATE:REMATS SAY-N
   SAY-SEALED ;

;package
