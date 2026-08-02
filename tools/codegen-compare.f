\ codegen-compare.f - command line entry for the code generator comparison.
\ One concern: reading the command line and choosing the run.
\
\   bin/hb --load tools/codegen-compare.f
\       Measure both pinned corpora, print a report for each, and compare each
\       measurement with its own committed baseline table. Exits non-zero,
\       after naming every disagreement, if either comparison finds anything.
\
\   bin/hb --load tools/codegen-compare.f -- --update
\       Measure and print as above, then rewrite both committed baseline tables
\       from this measurement. Use it when a deliberate compiler change has
\       moved the numbers, and read the resulting diff before committing it.
\
\ TWO CORPORA, TWO TABLES, ONE RUN. tools/codegen-compare-corpus.f is the
\ original eleven words - one smallest honest example of each shape a code
\ generator has to handle - and tools/codegen-compare-corpus2.f is seven words
\ taken from the places this system really spends its time. They are separate
\ files with separate committed tables because the first table is a pinned
\ yardstick: adding a row to it would be a change to the artifact every compiler
\ change is read against. The runner measures them one after the other, in one
\ pass each, and adds the findings up.
\
\ THIS ENTRY IS THE TIMED CHECK, AND IT IS RUN BY HAND. It compares the cost
\ column with the committed table; no gate does. A cost is a measurement, and a
\ machine with every core busy - which is what a gate is - moves one further than
\ the tolerance band without any compiler having changed. The head of
\ tools/codegen-compare-baseline.f records the measurements that settle it. What
\ runs in the gates is tools/codegen-compare-test.f, which checks the same pass
\ over the same shared body with the cost column left out and says so in its
\ output. Run this entry on a quiet machine before and after a change that is
\ meant to move the numbers, and read what it prints.
\
\ The whole point of the harness is that the numbers come from the real engine:
\ the corpus words are compiled by bin/hb when tools/codegen-compare-corpus.f is
\ loaded, their machine code sizes are read from their own dictionary records,
\ and they are executed and timed as themselves. Nothing here models a compiler.

require lib/string.f
require tools/codegen-compare-cli.f

package CODEGEN-COMPARE-ENTRY

private

: UPDATE-FLAG$ ( -- ptr u8 n )
   s" --update" ;

: UPDATE-REQUESTED? ( -- bool )
   0 begin dup SCRIPT-ARGC < while
      dup SCRIPT-ARGV$ UPDATE-FLAG$ STR= if drop 0 0= exit then
      1+
   repeat drop 0 0= 0= ;

public

: MAIN ( -- )
   UPDATE-REQUESTED? if CODEGEN-COMPARE-CLI:UPDATE exit then
   CODEGEN-COMPARE-CLI:CHECK ;

;package

CODEGEN-COMPARE-ENTRY:MAIN
