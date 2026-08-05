\ codegen-workload-rows.f - the timed rows of the compute workloads. One concern:
\ what gets measured against what.
\
\ THIS FILE IS LOADED BY tools/codegen-workload-run.f, LAST, and cannot be loaded
\ on its own: every word here names a driver that file publishes, and a driver
\ only exists after the choreography that compiles it. The split is by concern -
\ that file decides the ORDER things are put into the dictionary in, this one
\ decides what is timed against what - and the load order is one line at the
\ bottom of the other file.
\
\ FIVE WORKLOADS, AND WHAT EACH ONE IS FOR. The sixth, the compile-shaped one,
\ is measured in the other file because its arms straddle the migration.
\
\   scan   every byte of a buffer through a small folding word. The migration
\          can only reach the callee, and the caller's loop - which the old
\          emitter compiled in both arms - is most of the time. This is the low
\          end of what a migration can be worth.
\   count  one call per repetition into a word that scans the whole buffer
\          itself. Nearly all of the work is inside the migrated word, so this
\          row is not "what a migration is worth to a program" - it is the
\          migrated word's own speed-up, and it is the END of a curve.
\   mix66  the same buffer pass three times, two of them through the migrated
\   mix33  word and one through a publication nothing migrates - and the other
\          way round. The migration reaches exactly two thirds and one third of
\          the old arm's work, by construction, so these two rows are the middle
\          of that curve and the reader can check the endpoint against them.
\   term   every cell of a buffer through two words the engine copies into its
\          callers rather than calling. This measures whether a migration
\          reaches an inlined subject at all.
\
\ EVERY FAMILY CARRIES TWO NULL ROWS, and that is what makes its number
\ judgeable. A null row runs the same program on both arms, so whatever delta it
\ reports is manufactured by this harness rather than by a code generator, and
\ the report takes the larger of the two as the bar the family's real row has to
\ clear. They measure two different confounds:
\
\   -control    the same body compiled on either side of the migration, both
\               arms reaching HOT-FIXED. It holds everything the migration did to
\               the process that is not the subject's code: the code region grew,
\               later definitions moved, the caches hold different things.
\   -placement  the same body, five drivers compiled at the same moment, each
\               reaching a DIFFERENT publication of the same subject. It holds
\               what the address a callee landed at is worth - which on the scan
\               shape is ten times what the code generator is worth.
\
\ The placement row times all five publications woven through each other and
\ reports the widest gap between any two of them. It is five and not two because
\ the effect is not a wobble around a centre: on the scan shape the publications
\ fall into a fast group and a slow group tens of per cent apart, so a bar taken
\ from one named pair depends on which pair was named - and a bar taken that way
\ reported a workload as a REAL LOSS three runs running with nothing whatever
\ having slowed it down.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require tools/codegen-workload-time.f
require tools/codegen-workload-hot.f

package CODEGEN-RUN

public

\ ---- the scan workload ------------------------------------------------------
\ The answers are taken once, outside the timing, and the timing bodies drop
\ them: a timed run must do the workload and nothing else.

: SCAN-OLD-SUM ( -- n )
   CODEGEN-HOT:BYTES$ WORKLOAD:SCAN-OLD ;

: SCAN-NEW-SUM ( -- n )
   CODEGEN-HOT:BYTES$ WORKLOAD:SCAN-NEW ;

: SCAN-CTL-A-SUM ( -- n )
   CODEGEN-HOT:BYTES$ WORKLOAD:SCAN-CTL-A ;

: SCAN-CTL-B-SUM ( -- n )
   CODEGEN-HOT:BYTES$ WORKLOAD:SCAN-CTL-B ;

: SCAN-F1-SUM ( -- n )
   CODEGEN-HOT:BYTES$ WORKLOAD:SCAN-F1 ;

: SCAN-F2-SUM ( -- n )
   CODEGEN-HOT:BYTES$ WORKLOAD:SCAN-F2 ;

: SCAN-F3-SUM ( -- n )
   CODEGEN-HOT:BYTES$ WORKLOAD:SCAN-F3 ;

: SCAN-F4-SUM ( -- n )
   CODEGEN-HOT:BYTES$ WORKLOAD:SCAN-F4 ;

private

: SCAN-ROW ( -- )
   s" scan" s" scan" CODEGEN-CLOCK:OPEN-REAL
   SCAN-REPS ROUNDS SCAN-OLD-SUM SCAN-NEW-SUM
      [: CODEGEN-HOT:BYTES$ WORKLOAD:SCAN-OLD drop ;]
      [: CODEGEN-HOT:BYTES$ WORKLOAD:SCAN-NEW drop ;] CODEGEN-CLOCK:PAIR ;

: SCAN-CONTROL ( -- )
   s" scan-control" s" scan" CODEGEN-CLOCK:OPEN-NULL
   SCAN-REPS ROUNDS SCAN-CTL-A-SUM SCAN-CTL-B-SUM
      [: CODEGEN-HOT:BYTES$ WORKLOAD:SCAN-CTL-A drop ;]
      [: CODEGEN-HOT:BYTES$ WORKLOAD:SCAN-CTL-B drop ;] CODEGEN-CLOCK:PAIR ;

: SCAN-PLACE ( -- )
   s" scan-placement" s" scan" CODEGEN-CLOCK:OPEN-NULL
   SCAN-REPS ROUNDS SCAN-OLD-SUM
      [: CODEGEN-HOT:BYTES$ WORKLOAD:SCAN-OLD drop ;]
      [: CODEGEN-HOT:BYTES$ WORKLOAD:SCAN-F1 drop ;]
      [: CODEGEN-HOT:BYTES$ WORKLOAD:SCAN-F2 drop ;]
      [: CODEGEN-HOT:BYTES$ WORKLOAD:SCAN-F3 drop ;]
      [: CODEGEN-HOT:BYTES$ WORKLOAD:SCAN-F4 drop ;] CODEGEN-CLOCK:SWEEP ;

: SCAN-ROWS ( -- )
   SCAN-ROW SCAN-CONTROL SCAN-PLACE ;

public

\ ---- the count workload -----------------------------------------------------

: COUNT-OLD-SUM ( -- n )
   CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:COUNT-OLD ;

: COUNT-NEW-SUM ( -- n )
   CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:COUNT-NEW ;

: COUNT-CTL-A-SUM ( -- n )
   CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:COUNT-CTL-A ;

: COUNT-CTL-B-SUM ( -- n )
   CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:COUNT-CTL-B ;

: COUNT-F1-SUM ( -- n )
   CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:COUNT-F1 ;

: COUNT-F2-SUM ( -- n )
   CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:COUNT-F2 ;

: COUNT-F3-SUM ( -- n )
   CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:COUNT-F3 ;

: COUNT-F4-SUM ( -- n )
   CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:COUNT-F4 ;

private

: COUNT-ROW ( -- )
   s" count" s" count" CODEGEN-CLOCK:OPEN-REAL
   COUNT-REPS ROUNDS COUNT-OLD-SUM COUNT-NEW-SUM
      [: CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:COUNT-OLD drop ;]
      [: CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:COUNT-NEW drop ;] CODEGEN-CLOCK:PAIR ;

: COUNT-CONTROL ( -- )
   s" count-control" s" count" CODEGEN-CLOCK:OPEN-NULL
   COUNT-REPS ROUNDS COUNT-CTL-A-SUM COUNT-CTL-B-SUM
      [: CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:COUNT-CTL-A drop ;]
      [: CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:COUNT-CTL-B drop ;] CODEGEN-CLOCK:PAIR ;

: COUNT-PLACE ( -- )
   s" count-placement" s" count" CODEGEN-CLOCK:OPEN-NULL
   COUNT-REPS ROUNDS COUNT-OLD-SUM
      [: CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:COUNT-OLD drop ;]
      [: CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:COUNT-F1 drop ;]
      [: CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:COUNT-F2 drop ;]
      [: CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:COUNT-F3 drop ;]
      [: CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:COUNT-F4 drop ;] CODEGEN-CLOCK:SWEEP ;

: COUNT-ROWS ( -- )
   COUNT-ROW COUNT-CONTROL COUNT-PLACE ;

public

\ ---- the two mixed-coverage workloads ---------------------------------------
\ Same driver shape as count, three buffer passes instead of one, and a third of
\ the repetitions so a timed run costs about what a count run costs.

: MIX66-OLD-SUM ( -- n )
   CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:MIX66-OLD ;

: MIX66-NEW-SUM ( -- n )
   CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:MIX66-NEW ;

: MIX66-CTL-A-SUM ( -- n )
   CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:MIX66-CTL-A ;

: MIX66-CTL-B-SUM ( -- n )
   CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:MIX66-CTL-B ;

: MIX66-F1-SUM ( -- n )
   CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:MIX66-F1 ;

: MIX66-F2-SUM ( -- n )
   CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:MIX66-F2 ;

: MIX66-F3-SUM ( -- n )
   CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:MIX66-F3 ;

: MIX66-F4-SUM ( -- n )
   CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:MIX66-F4 ;

: MIX33-OLD-SUM ( -- n )
   CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:MIX33-OLD ;

: MIX33-NEW-SUM ( -- n )
   CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:MIX33-NEW ;

: MIX33-CTL-A-SUM ( -- n )
   CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:MIX33-CTL-A ;

: MIX33-CTL-B-SUM ( -- n )
   CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:MIX33-CTL-B ;

: MIX33-F1-SUM ( -- n )
   CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:MIX33-F1 ;

: MIX33-F2-SUM ( -- n )
   CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:MIX33-F2 ;

: MIX33-F3-SUM ( -- n )
   CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:MIX33-F3 ;

: MIX33-F4-SUM ( -- n )
   CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:MIX33-F4 ;

private

: MIX66-ROW ( -- )
   s" mix66" s" mix66" CODEGEN-CLOCK:OPEN-REAL
   MIX-REPS ROUNDS MIX66-OLD-SUM MIX66-NEW-SUM
      [: CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:MIX66-OLD drop ;]
      [: CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:MIX66-NEW drop ;] CODEGEN-CLOCK:PAIR ;

: MIX66-CONTROL ( -- )
   s" mix66-control" s" mix66" CODEGEN-CLOCK:OPEN-NULL
   MIX-REPS ROUNDS MIX66-CTL-A-SUM MIX66-CTL-B-SUM
      [: CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:MIX66-CTL-A drop ;]
      [: CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:MIX66-CTL-B drop ;] CODEGEN-CLOCK:PAIR ;

: MIX66-PLACE ( -- )
   s" mix66-placement" s" mix66" CODEGEN-CLOCK:OPEN-NULL
   MIX-REPS ROUNDS MIX66-OLD-SUM
      [: CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:MIX66-OLD drop ;]
      [: CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:MIX66-F1 drop ;]
      [: CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:MIX66-F2 drop ;]
      [: CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:MIX66-F3 drop ;]
      [: CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:MIX66-F4 drop ;] CODEGEN-CLOCK:SWEEP ;

: MIX66-ROWS ( -- )
   MIX66-ROW MIX66-CONTROL MIX66-PLACE ;

: MIX33-ROW ( -- )
   s" mix33" s" mix33" CODEGEN-CLOCK:OPEN-REAL
   MIX-REPS ROUNDS MIX33-OLD-SUM MIX33-NEW-SUM
      [: CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:MIX33-OLD drop ;]
      [: CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:MIX33-NEW drop ;] CODEGEN-CLOCK:PAIR ;

: MIX33-CONTROL ( -- )
   s" mix33-control" s" mix33" CODEGEN-CLOCK:OPEN-NULL
   MIX-REPS ROUNDS MIX33-CTL-A-SUM MIX33-CTL-B-SUM
      [: CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:MIX33-CTL-A drop ;]
      [: CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:MIX33-CTL-B drop ;] CODEGEN-CLOCK:PAIR ;

: MIX33-PLACE ( -- )
   s" mix33-placement" s" mix33" CODEGEN-CLOCK:OPEN-NULL
   MIX-REPS ROUNDS MIX33-OLD-SUM
      [: CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:MIX33-OLD drop ;]
      [: CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:MIX33-F1 drop ;]
      [: CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:MIX33-F2 drop ;]
      [: CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:MIX33-F3 drop ;]
      [: CODEGEN-HOT:BYTES$ COUNT-INNER WORKLOAD:MIX33-F4 drop ;] CODEGEN-CLOCK:SWEEP ;

: MIX33-ROWS ( -- )
   MIX33-ROW MIX33-CONTROL MIX33-PLACE ;

public

\ ---- the term workload ------------------------------------------------------

: TERM-OLD-SUM ( -- n )
   CODEGEN-HOT:TERMS$ WORKLOAD:TERM-OLD ;

: TERM-NEW-SUM ( -- n )
   CODEGEN-HOT:TERMS$ WORKLOAD:TERM-NEW ;

: TERM-CTL-A-SUM ( -- n )
   CODEGEN-HOT:TERMS$ WORKLOAD:TERM-CTL-A ;

: TERM-CTL-B-SUM ( -- n )
   CODEGEN-HOT:TERMS$ WORKLOAD:TERM-CTL-B ;

: TERM-F1-SUM ( -- n )
   CODEGEN-HOT:TERMS$ WORKLOAD:TERM-F1 ;

: TERM-F2-SUM ( -- n )
   CODEGEN-HOT:TERMS$ WORKLOAD:TERM-F2 ;

: TERM-F3-SUM ( -- n )
   CODEGEN-HOT:TERMS$ WORKLOAD:TERM-F3 ;

: TERM-F4-SUM ( -- n )
   CODEGEN-HOT:TERMS$ WORKLOAD:TERM-F4 ;

private

: TERM-ROW ( -- )
   s" term" s" term" CODEGEN-CLOCK:OPEN-REAL
   TERM-REPS ROUNDS TERM-OLD-SUM TERM-NEW-SUM
      [: CODEGEN-HOT:TERMS$ WORKLOAD:TERM-OLD drop ;]
      [: CODEGEN-HOT:TERMS$ WORKLOAD:TERM-NEW drop ;] CODEGEN-CLOCK:PAIR ;

: TERM-CONTROL ( -- )
   s" term-control" s" term" CODEGEN-CLOCK:OPEN-NULL
   TERM-REPS ROUNDS TERM-CTL-A-SUM TERM-CTL-B-SUM
      [: CODEGEN-HOT:TERMS$ WORKLOAD:TERM-CTL-A drop ;]
      [: CODEGEN-HOT:TERMS$ WORKLOAD:TERM-CTL-B drop ;] CODEGEN-CLOCK:PAIR ;

: TERM-PLACE ( -- )
   s" term-placement" s" term" CODEGEN-CLOCK:OPEN-NULL
   TERM-REPS ROUNDS TERM-OLD-SUM
      [: CODEGEN-HOT:TERMS$ WORKLOAD:TERM-OLD drop ;]
      [: CODEGEN-HOT:TERMS$ WORKLOAD:TERM-F1 drop ;]
      [: CODEGEN-HOT:TERMS$ WORKLOAD:TERM-F2 drop ;]
      [: CODEGEN-HOT:TERMS$ WORKLOAD:TERM-F3 drop ;]
      [: CODEGEN-HOT:TERMS$ WORKLOAD:TERM-F4 drop ;] CODEGEN-CLOCK:SWEEP ;

: TERM-ROWS ( -- )
   TERM-ROW TERM-CONTROL TERM-PLACE ;

public

\ Each family's real row is measured first and its null rows immediately after,
\ so the draws that judge a number are taken as close in time to it as the
\ harness can manage.
: MEASURE ( -- )
   SCAN-ROWS
   COUNT-ROWS
   MIX66-ROWS
   MIX33-ROWS
   TERM-ROWS ;

;package
