\ low-light-report-emit.f - emits the low-light CSV + manifest markdown to stdout for
\ verification against src/low_light.zig test-832 (markdown ohsnap) and the
\ renderLowLightCsv formula. Feeds the exact test fixture (one camera, one image).

package LOWLIGHT
private
: FEED ( -- )
   LE-RESET  2000 LE-ADD
   LI-RESET  1 LI-FRAMES !  4 LI-PIXELS !  400 LI-LUMSUM !
   100 4 LI-HIST!  5.0 LI-NOISE+  1.25 LI-EDGE+
   s" 100" s" cam_a0" 1 0 0 1 0 0 LC-SET
   \ manifest
   1 LM-FRECS !
   s" odin.capture.v1" LM-SCH-A LM-SCH-N SVAR
   s" dusk-auto" LM-SCEN-A LM-SCEN-N SVAR
   s" 2026-06-24T18:00:00Z" LM-CAP-A LM-CAP-N SVAR
   s" incident lux at rig" LM-LREF-A LM-LREF-N SVAR
   s" dusk sky" LM-LCOND-A LM-LCOND-N SVAR
   s" dusk" LM-TOD-A LM-TOD-N SVAR
   s" dark proxy" LM-TDESC-A LM-TDESC-N SVAR
   s" target/background samples" LM-CONTR-A LM-CONTR-N SVAR
   s" auto exposure" LM-EXPM-A LM-EXPM-N SVAR
   s" auto gain" LM-GAINM-A LM-GAINM-N SVAR
   s" record telemetry" LM-EXPP-A LM-EXPP-N SVAR
   s" field template" LM-NOTES-A LM-NOTES-N SVAR
   0.254 LM-TW F!  -1 LM-TWP !   0.152 LM-TH F!  -1 LM-THP !
   100.0 LM-TR F!  -1 LM-TRP !   2.54 LM-TAW F!  -1 LM-TAWP !
   5000 LM-WARM !  2000 LM-SETT !  3 LM-REP ! ;

: MARK ( ptr u8 n -- ) type 10 emit ;

FEED
s" <<<MD>>>"  MARK   LL-MD  type
s" <<<CSV>>>" MARK   LL-CSV type
s" <<<END>>>" MARK
end-package
