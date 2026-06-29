\ gate-aot-runner-entry.f - dispatch for the baked AOT gate runner.
\
\ Loaded by the AOT warm gate image after AOT support libraries have already
\ been baked. Keep this file side-effect-only.

64 constant GAR-USAGE-RC

: GAR-USAGE ( -- )
   s" usage: hb-aot-warm --load test/gate-aot-runner-entry.f -- PHASE" GAR-USAGE-RC die ;

: GAR-ARG0= ( ptr u8 n -- bool )
   0 SCRIPT-ARGV$ STR= ;

: GAR-DISPATCH ( -- )
   s" aot-pos" GAR-ARG0= if GAP-RUN exit then
   s" aot-neg" GAR-ARG0= if GAN-RUN exit then
   GAR-USAGE ;

: GAR-MAIN ( -- )
   SCRIPT-ARGC 1 <> if GAR-USAGE then
   GAR-DISPATCH ;

GAR-MAIN
