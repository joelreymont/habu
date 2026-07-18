\ size-report-main.f - CLI entry for tools/size-report.f.
\ Load after tools/size-report.f:
\   bin/hb --load tools/size-report.f tools/size-report-main.f -- MAP-FILE [ENGINE]
\ MAP-FILE is a captured engine size map (HABU_ENGINE_SIZE_MAP=1 build output);
\ ENGINE is the built engine to size, default bin/hb.

74 constant SR-RC

: SR-ENGINE$ ( -- ptr u8 n )
   SCRIPT-ARGC 2 >= if 1 SCRIPT-ARGV$ exit then
   s" bin/hb" ;

: SR-MAIN ( -- )
   SCRIPT-ARGC 1 < if
      s" usage: bin/hb --load tools/size-report.f tools/size-report-main.f -- MAP-FILE [ENGINE]" type cr
      s" size-report: missing MAP-FILE arg" SR-RC die
   then
   0 SCRIPT-ARGV$ SIZE-REPORT:LOAD
   SR-ENGINE$ SIZE-REPORT:PRINT ;

SR-MAIN
