\ codegen-compare.f - command line entry for the code generator comparison.
\ One concern: reading the command line and choosing the run.
\
\   bin/hb --load tools/codegen-compare.f
\       Measure every pinned corpus, print a report for each, and compare each
\       measurement with its own committed baseline table. Exits non-zero,
\       after naming every disagreement, if any comparison finds anything.
\
\   bin/hb --load tools/codegen-compare.f -- --update <corpus>
\       Measure that ONE corpus, print its report, and rewrite ITS committed
\       table from this measurement - and no other table. Use it when a
\       deliberate compiler change has moved that corpus's numbers, and read the
\       resulting diff before committing it. The corpus names are the ones each
\       case file declares: corpus, corpus2, corpus3, corpus4.
\
\   bin/hb --load tools/codegen-compare.f -- --update all
\       The same for every corpus, in one run.
\
\ WHY A BARE --update IS REFUSED RATHER THAN TAKEN AS "all". It used to mean
\ "all", and every regeneration therefore rewrote four pinned yardsticks when
\ one had moved. A cost is a measurement, so the three untouched tables came
\ back with fresh numbers in them, and three lanes have had to snapshot the
\ other three baselines and restore them by hand after regenerating one. The
\ tables are separate files precisely because each is a separate yardstick;
\ rewriting all four is still one command, but it is now a sentence somebody
\ had to type. A bare --update names the corpora it would have accepted and
\ exits with the usage status, having written nothing.
\
\ FOUR CORPORA, FOUR TABLES, ONE RUN. tools/codegen-compare-corpus.f is the
\ original eleven words - one smallest honest example of each shape a code
\ generator has to handle - tools/codegen-compare-corpus2.f is seven words taken
\ from the places this system really spends its time,
\ tools/codegen-compare-corpus3.f is ten float words, measured before the chain
\ has a single float capability so that the float campaign has a yardstick it
\ did not choose afterwards, and tools/codegen-compare-corpus4.f is twelve shapes
\ chosen the other way round - each one picked because somebody had a reason to
\ believe the new chain handles it WORSE than the engine's emitter, so that the
\ suite does not consist only of rows its subject wins. They are separate files
\ with separate committed tables because each table is a pinned yardstick:
\ adding a row to one would be a change to the artifact every compiler change is
\ read against. Each declares itself in tools/codegen-compare-corpora.f, the
\ runner measures whatever was declared one after the other, in one pass each,
\ and adds the findings up.
\
\ THIS ENTRY IS THE TIMED CHECK, AND IT IS RUN BY HAND. It compares the cost
\ column with the committed table; no gate does. A cost is a measurement, and a
\ machine with every core busy - which is what a gate is - moves one further than
\ the tolerance band without any compiler having changed. The head of
\ tools/codegen-compare-baseline.f records the measurements that settle it. What
\ runs in the gates is tools/codegen-compare-test.f, which checks the same pass
\ over the same shared body with the cost column left out and says so in its
\ output. The assertions that are about a cost rather than about emitted code
\ live beside this entry, in tools/codegen-compare-timed-test.f, and are run by
\ hand on the same quiet machine. Run this entry before and after a change that
\ is meant to move the numbers, and read what it prints.
\
\ The whole point of the harness is that the numbers come from the real engine:
\ the corpus words are compiled by bin/hb when tools/codegen-compare-corpus.f is
\ loaded, their machine code sizes are read from their own dictionary records,
\ and they are executed and timed as themselves. Nothing here models a compiler.

require lib/string.f
require tools/codegen-compare-cli.f

package CODEGEN-COMPARE-ENTRY

private

64 constant USAGE-RC               \ sysexits EX_USAGE: the command line was wrong

: UPDATE-FLAG$ ( -- ptr u8 n )
   s" --update" ;

: ALL$ ( -- ptr u8 n )
   s" all" ;

\ Where --update appears on the command line, or -1.
: UPDATE-AT ( -- n )
   0 begin dup SCRIPT-ARGC < while
      dup SCRIPT-ARGV$ UPDATE-FLAG$ STR= if exit then
      1+
   repeat drop -1 ;

\ What a caller who named no corpus, or a corpus nobody declared, is told: the
\ names that exist, each beside the source file it is the measurement of.
: SAY-CORPORA ( -- )
   s" codegen-compare: name the corpus whose table is to be rewritten:" type cr
   CODEGEN-CORPORA:NAMES.
   s"   all" type cr
   s" codegen-compare: nothing was written." type cr ;

\ --update with nothing after it. It used to mean all four tables; it now says
\ which names it would have taken and writes nothing.
: REFUSE-BARE ( -- )
   SAY-CORPORA
   S\" codegen-compare: --update needs a corpus name\n" USAGE-RC die ;

\ --update with a name no corpus was declared under. The offending word is
\ printed back, because the likeliest cause is a typo in it.
: REFUSE-NAME ( ptr u8 n -- )
   s" codegen-compare: no corpus is declared under the name " type type cr
   SAY-CORPORA
   S\" codegen-compare: --update was given a name no corpus goes by\n" USAGE-RC die ;

\ --update with the word that follows it.
: UPDATE-RUN ( n -- ) {: at:n :}
   at 1+ SCRIPT-ARGC >= if REFUSE-BARE then
   at 1+ SCRIPT-ARGV$ ALL$ STR= if CODEGEN-COMPARE-CLI:UPDATE-ALL exit then
   at 1+ SCRIPT-ARGV$ CODEGEN-COMPARE-CLI:UPDATE-NAMED if exit then
   at 1+ SCRIPT-ARGV$ REFUSE-NAME ;

public

: MAIN ( -- )
   UPDATE-AT dup 0 < if drop CODEGEN-COMPARE-CLI:CHECK exit then
   UPDATE-RUN ;

;package

CODEGEN-COMPARE-ENTRY:MAIN
