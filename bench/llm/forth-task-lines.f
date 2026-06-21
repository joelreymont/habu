\ forth-task-lines.f - CLI wrapper for harness=forth task-row emission.
\
\ Load after tools/argv.f and bench/llm/forth-task-lines-lib.f.

: FTL-USAGE ( -- )
   s" bench/llm/forth-task-lines.f TASKS.tsv OUT.tsv" ARGV-USAGE!
   ARGV-PARSE
   2 2 ARGV-EXPECT-POS ;

: FTL-MAIN ( -- )
   FTL-USAGE
   0 ARGV-POS$ 1 ARGV-POS$ FTL-WRITE-FILE ;

FTL-MAIN
