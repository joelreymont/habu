\ attempt-solutions.f - CLI wrapper for harness=forth reference extraction.
\
\ Load after tools/argv.f and bench/llm/attempt-solutions-lib.f.

: AS-USAGE ( -- )
   s" bench/llm/attempt-solutions.f TASKS.tsv SOLUTIONS.f OUTDIR" ARGV-USAGE!
   ARGV-PARSE
   3 3 ARGV-EXPECT-POS ;

: AS-MAIN ( -- )
   AS-USAGE
   0 ARGV-POS$ 1 ARGV-POS$ 2 ARGV-POS$ AS-EXTRACT-FILES ;

AS-MAIN
