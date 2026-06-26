\ low-light-report.f - low-light metrics report model + renderers, ported from
\ renderLowLightCsv + renderLowLightMarkdownWithManifest in src/low_light.zig.
\ Holds one camera's low-light aggregate (exposure IntegerStats, image
\ ImageAggregate with a 256-bin luminance histogram + noise/edge FloatStats) and
\ the scenario manifest, then renders the per-camera CSV row and the manifest+table
\ markdown. meanLuminance = luminance_sum/pixels; percentileLuminance is the
\ histogram nearest-rank ceil((pixels*p+99)/100); snrProxy = meanLuminance/noise
\ mean. Floats render {d:.3} via RB-FFIX3. Single camera (the fixture); a multi-
\ camera report would index these as arrays. Depends on lib/errors.f lib/string.f
\ lib/float.f odin/float-cell.f lib/render.f.

package LOWLIGHT
private
: MAX2 ( n n -- n ) {: a:n b:n :} a b > if a else b then ;

\ --- exposure IntegerStats ---
variable LE-SAMP  variable LE-MIN  variable LE-MAX  variable LE-SUM
public
: LE-RESET ( -- ) 0 LE-SAMP !  0 LE-MIN !  0 LE-MAX !  0 LE-SUM ! ;
: LE-ADD ( n -- ) {: v:n :}
   LE-SAMP @ 0= if v LE-MIN !  v LE-MAX !
   else v LE-MIN @ < if v LE-MIN ! then  v LE-MAX @ > if v LE-MAX ! then then
   LE-SUM @ v + LE-SUM !  LE-SAMP @ 1+ LE-SAMP ! ;
private
: LE-MEAN ( -- n ) LE-SAMP @ 0= if 0 else LE-SUM @ LE-SAMP @ / then ;

\ --- image ImageAggregate ---
variable LI-FRAMES  variable LI-PIXELS  variable LI-LUMSUM  variable LI-BI
create LI-HIST 256 cells allot
variable LI-NSAMP  variable LI-NSUM     \ noise_stddev FloatStats (sum is a float cell)
variable LI-ESAMP  variable LI-ESUM     \ edge_density_pct FloatStats
public
: LI-RESET ( -- )
   0 LI-FRAMES !  0 LI-PIXELS !  0 LI-LUMSUM !  0 LI-NSAMP !  0 LI-ESAMP !
   0.0 LI-NSUM F!  0.0 LI-ESUM F!
   0 LI-BI ! begin LI-BI @ 256 < while  0 LI-HIST LI-BI @ cells + !  LI-BI @ 1+ LI-BI ! repeat ;
: LI-HIST! ( n n -- ) {: bin:n cnt:n :} cnt LI-HIST bin cells + ! ;     \ set a histogram bin
: LI-NOISE+ ( r -- ) LI-NSUM F@ f+ LI-NSUM F!  LI-NSAMP @ 1+ LI-NSAMP ! ;
: LI-EDGE+  ( r -- ) LI-ESUM F@ f+ LI-ESUM F!  LI-ESAMP @ 1+ LI-ESAMP ! ;
private
: LI-MEANLUM ( -- n ) LI-PIXELS @ 0= if 0 else LI-LUMSUM @ LI-PIXELS @ / then ;

\ percentileLuminance(p,100): nearest-rank over the histogram (rank ceil, clamp 1)
variable PL-TGT  variable PL-SEEN  variable PL-BI
: PCT-LUM ( n -- n ) {: num:n :}
   LI-PIXELS @ 0= if 0 exit then
   LI-PIXELS @ num * 99 + 100 /  1 MAX2  1-  PL-TGT !
   0 PL-SEEN !  0 PL-BI !
   begin PL-BI @ 256 < while
      PL-SEEN @ LI-HIST PL-BI @ cells + @ + PL-SEEN !
      PL-SEEN @ PL-TGT @ > if PL-BI @ exit then
      PL-BI @ 1+ PL-BI !
   repeat  255 ;
: LI-NOISE-MEAN ( -- r ) LI-NSAMP @ 0= if 0.0 else LI-NSUM F@ LI-NSAMP @ s>f f/ then ;
: LI-EDGE-MEAN  ( -- r ) LI-ESAMP @ 0= if 0.0 else LI-ESUM F@ LI-ESAMP @ s>f f/ then ;
\ snrProxy: meanLuminance / noise mean, or absent (flag 0) when no pixels/noise/<=0 noise
: LI-SNR ( -- r bool )
   LI-PIXELS @ 0= if 0.0 0 0= 0= exit then
   LI-NSAMP @ 0= if 0.0 0 0= 0= exit then
   LI-NOISE-MEAN 0.0 f> 0= if 0.0 0 0= 0= exit then
   LI-MEANLUM s>f LI-NOISE-MEAN f/  0 0= ;

\ --- per-camera low-light fields ---
variable LC-SER-A variable LC-SER-N  variable LC-LNA-A variable LC-LNA-N
variable LC-FRAMES variable LC-DROPPED variable LC-REGR
variable LC-IPR variable LC-IMISS variable LC-IDEC
public
: LC-SET ( ptr u8 n ptr u8 n n n n n n n -- )
   {: sa:ptr sn:n la:ptr ln:n frames:n dropped:n regr:n ipr:n imiss:n idec:n :}
   sa LC-SER-A !  sn LC-SER-N !  la LC-LNA-A !  ln LC-LNA-N !
   frames LC-FRAMES !  dropped LC-DROPPED !  regr LC-REGR !
   ipr LC-IPR !  imiss LC-IMISS !  idec LC-IDEC ! ;
private
: LC-SER@ ( -- ptr u8 n ) LC-SER-A @ LC-SER-N @ ;
: LC-LNA@ ( -- ptr u8 n ) LC-LNA-A @ LC-LNA-N @ ;

\ --- scenario manifest fields ---
variable LM-FRECS
variable LM-SCH-A variable LM-SCH-N
variable LM-SCEN-A variable LM-SCEN-N    variable LM-CAP-A variable LM-CAP-N
variable LM-LREF-A variable LM-LREF-N    variable LM-LCOND-A variable LM-LCOND-N
variable LM-TOD-A variable LM-TOD-N      variable LM-TDESC-A variable LM-TDESC-N
variable LM-CONTR-A variable LM-CONTR-N  variable LM-EXPM-A variable LM-EXPM-N
variable LM-GAINM-A variable LM-GAINM-N  variable LM-EXPP-A variable LM-EXPP-N
variable LM-NOTES-A variable LM-NOTES-N
variable LM-TW variable LM-TWP  variable LM-TH variable LM-THP        \ target floats + present flags
variable LM-TR variable LM-TRP  variable LM-TAW variable LM-TAWP
variable LM-WARM variable LM-SETT variable LM-REP
public
: SVAR ( ptr u8 n ptr a ptr b -- ) {: aa:ptr an:ptr :} an !  aa ! ;   \ store a string (ptr,len) into target vars (addr-a, addr-n)

\ manifest string accessors ( -- ptr u8 n )
private
: LM-SCH@   LM-SCH-A @ LM-SCH-N @ ;      : LM-SCEN@  LM-SCEN-A @ LM-SCEN-N @ ;
: LM-CAP@   LM-CAP-A @ LM-CAP-N @ ;      : LM-LREF@  LM-LREF-A @ LM-LREF-N @ ;
: LM-LCOND@ LM-LCOND-A @ LM-LCOND-N @ ;  : LM-TOD@   LM-TOD-A @ LM-TOD-N @ ;
: LM-TDESC@ LM-TDESC-A @ LM-TDESC-N @ ;  : LM-CONTR@ LM-CONTR-A @ LM-CONTR-N @ ;
: LM-EXPM@  LM-EXPM-A @ LM-EXPM-N @ ;    : LM-GAINM@ LM-GAINM-A @ LM-GAINM-N @ ;
: LM-EXPP@  LM-EXPP-A @ LM-EXPP-N @ ;    : LM-NOTES@ LM-NOTES-A @ LM-NOTES-N @ ;

\ (markdown bullet DSL MD-S/MD-N lives in lib/render.f)

\ optional target-proxy float: value if present flag set, else "n/a"
: OPTF ( bool ptr a -- ) {: present:bool rc:ptr :} present if rc F@ RB-FFIX3 else s" n/a" RB+ then ;

\ renderLowLightCsv: declare the 20 columns once; the engine emits the row. Cell
\ quotations ignore the row index (single camera) and read the LE/LI/LC aggregates.
: LL-CSV-COLS ( -- ) TBL-RESET
   s" serial" AL-L [: drop LC-SER@ RB+ ;] COL+
   s" logical_name" AL-L [: drop LC-LNA@ RB+ ;] COL+
   s" frames" AL-L [: drop LC-FRAMES @ RB# ;] COL+
   s" dropped_event_flags" AL-L [: drop LC-DROPPED @ RB# ;] COL+
   s" timestamp_regressions" AL-L [: drop LC-REGR @ RB# ;] COL+
   s" exposure_samples" AL-L [: drop LE-SAMP @ RB# ;] COL+
   s" exposure_min_us" AL-L [: drop LE-SAMP @ 0 > if LE-MIN @ RB# then ;] COL+
   s" exposure_max_us" AL-L [: drop LE-SAMP @ 0 > if LE-MAX @ RB# then ;] COL+
   s" exposure_mean_us" AL-L [: drop LE-SAMP @ 0 > if LE-MEAN RB# then ;] COL+
   s" image_path_records" AL-L [: drop LC-IPR @ RB# ;] COL+
   s" image_missing" AL-L [: drop LC-IMISS @ RB# ;] COL+
   s" image_decode_failures" AL-L [: drop LC-IDEC @ RB# ;] COL+
   s" image_frames" AL-L [: drop LI-FRAMES @ RB# ;] COL+
   s" mean_luminance" AL-L [: drop LI-MEANLUM RB# ;] COL+
   s" median_luminance" AL-L [: drop 50 PCT-LUM RB# ;] COL+
   s" p05_luminance" AL-L [: drop 5 PCT-LUM RB# ;] COL+
   s" p95_luminance" AL-L [: drop 95 PCT-LUM RB# ;] COL+
   s" noise_stddev_mean" AL-L [: drop LI-NSAMP @ 0 > if LI-NOISE-MEAN RB-FFIX3 then ;] COL+
   s" snr_proxy" AL-L [: drop LI-SNR if RB-FFIX3 else fdrop then ;] COL+
   s" edge_density_pct_mean" AL-L [: drop LI-ESAMP @ 0 > if LI-EDGE-MEAN RB-FFIX3 then ;] COL+ ;
public
: LL-CSV ( -- ptr u8 n ) LL-CSV-COLS  RB-RESET  1 TBL-CSV  RB$ ;

\ the camera table columns (markdown), declared once for TBL-MD
private
: LL-MD-COLS ( -- ) TBL-RESET
   s" camera" AL-L [: drop LC-LNA@ RB+ ;] COL+
   s" frames" AL-R [: drop LC-FRAMES @ RB# ;] COL+
   s" drops" AL-R [: drop LC-DROPPED @ RB# ;] COL+
   s" exposure samples" AL-R [: drop LE-SAMP @ RB# ;] COL+
   s" images" AL-R [: drop LI-FRAMES @ RB# ;] COL+
   s" mean luminance" AL-R [: drop LI-MEANLUM RB# ;] COL+
   s" p05/p50/p95" AL-L [: drop 5 PCT-LUM RB# 47 RB-C 50 PCT-LUM RB# 47 RB-C 95 PCT-LUM RB# ;] COL+
   s" noise stddev mean" AL-R [: drop LI-NSAMP @ 0 > if LI-NOISE-MEAN RB-FFIX3 then ;] COL+
   s" SNR proxy" AL-R [: drop LI-SNR if RB-FFIX3 else fdrop s" n/a" RB+ then ;] COL+
   s" edge density mean %" AL-R [: drop LI-ESAMP @ 0 > if LI-EDGE-MEAN RB-FFIX3 then ;] COL+ ;

\ renderLowLightMarkdownWithManifest: scenario prose via the MD-* bullet DSL, the
\ per-camera table via the report engine.
public
: LL-MD ( -- ptr u8 n )
   LL-MD-COLS  RB-RESET
   s" # Low-Light Metrics" RB+ RB-NL  RB-NL
   s" schema" LM-SCH@ MD-S
   s" frame records" LM-FRECS @ MD-N  RB-NL
   s" ## Scenario" RB+ RB-NL  RB-NL
   s" scenario" LM-SCEN@ MD-S
   s" captured at UTC" LM-CAP@ MD-S
   s" light reference" LM-LREF@ MD-S
   s" lighting condition" LM-LCOND@ MD-S
   s" time of day" LM-TOD@ MD-S
   s" - target proxy: " RB+ LM-TDESC@ RB+
   s" ; width_m=" RB+ LM-TWP @ LM-TW OPTF
   s" ; height_m=" RB+ LM-THP @ LM-TH OPTF
   s" ; range_m=" RB+ LM-TRP @ LM-TR OPTF
   s" ; angular_width_mrad=" RB+ LM-TAWP @ LM-TAW OPTF
   s" ; contrast=" RB+ LM-CONTR@ RB+ RB-NL
   s" - exposure/gain settings: " RB+ LM-EXPM@ RB+ s"  / " RB+ LM-GAINM@ RB+ RB-NL
   s" exposure plan" LM-EXPP@ MD-S
   s" - warmup/settling/repeats: " RB+ LM-WARM @ RB# s"  ms / " RB+ LM-SETT @ RB# s"  ms / " RB+ LM-REP @ RB# RB-NL
   s" notes" LM-NOTES@ MD-S  RB-NL
   1 TBL-MD
   RB$ ;
end-package
