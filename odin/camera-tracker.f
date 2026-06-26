\ camera-tracker.f - single-camera multi-object tracking at camera-frame rate,
\ ported from src/camera_tracker.zig (alpha-beta predictive fusion). A detection
\ associates to the nearest track within a pixel gate and updates it via an
\ alpha-beta filter (or spawns a track); a heartbeat drops stale tracks, predicts
\ every track forward, and accumulates update-latency / prediction-horizon /
\ detection-gap samples plus a per-matched-detection prediction-error sample.
\ FINISH sorts each series; percentiles use linear interpolation (PCTL-LIN),
\ matching the .zig. Float state is in float cells behind F@/F!; no mid-control
\ locals or bool `and` (the checker rejects both).
\
\ This is the internal `CAMTRACK` module package. Its own files (the emit driver,
\ any test) reopen `package CAMTRACK` and call these words unqualified; the public
\ ODIN interface or other packages reach it as CAMTRACK:RESET / :DET / :HB /
\ :FINISH / :MD. Depends on lib/errors.f lib/string.f lib/float.f lib/sort.f
\ odin/float-cell.f lib/render.f.

package CAMTRACK
private

64 constant MAX-TRACKS                   \ max simultaneous tracks
512 constant MAX-SAMPLES                 \ max stat samples per series
-6220 constant E-TRACKS-FULL  -6221 constant E-SAMPLES-FULL

variable ALPHA  variable BETA  variable GATE  variable COAST  variable CONFIRM
create X  MAX-TRACKS cells allot   create Y  MAX-TRACKS cells allot
create VX MAX-TRACKS cells allot   create VY MAX-TRACKS cells allot
create LM MAX-TRACKS cells allot
create HITS MAX-TRACKS cells allot  create CONF MAX-TRACKS cells allot  create ID MAX-TRACKS cells allot
variable N  variable NEXTID  variable LASTDET
variable DETS  variable HBS  variable EST  variable CREATED  variable CONFCNT

create LAT MAX-SAMPLES cells allot   variable LATN
create HOR MAX-SAMPLES cells allot   variable HORN
create ERR MAX-SAMPLES cells allot   variable ERRN
create GAP MAX-SAMPLES cells allot   variable GAPN

\ detection scratch + filter scratch (avoid mid-control locals)
variable DXV variable DYV variable TSV
variable DI variable BEST variable BDIST variable DV
variable DT variable PXV variable PYV variable RX variable RY variable LATV
variable IDXF variable LO variable HI variable FRAC

: NS>S ( i64 i64 -- r ) {: l:i64 e:i64 :} l e <= if 0.0 else l e - s>f 1000000000.0 f/ then ;
: NS>MS ( i64 i64 -- r ) {: l:i64 e:i64 :} l e <= if 0.0 else l e - s>f 1000000.0 f/ then ;
: SAMPLE+ ( r ptr a ptr a -- ) {: base:ptr nv:ptr :}
   nv @ MAX-SAMPLES >= if E-SAMPLES-FULL throw then  nv @ cells base + F!  nv @ 1+ nv ! ;
: DIST ( r r -- r ) {: rx:r ry:r :} rx rx f* ry ry f* f+ fsqrt ;
: PRED-X ( n -- r ) {: ix:n :} X ix cells + F@  VX ix cells + F@  TSV @ LM ix cells + @ NS>S f*  f+ ;
: PRED-Y ( n -- r ) {: ix:n :} Y ix cells + F@  VY ix cells + F@  TSV @ LM ix cells + @ NS>S f*  f+ ;

: SPAWN ( -- )
   N @ MAX-TRACKS >= if E-TRACKS-FULL throw then
   DXV F@ X N @ cells + F!   DYV F@ Y N @ cells + F!
   0.0 VX N @ cells + F!     0.0 VY N @ cells + F!
   TSV @ LM N @ cells + !    1 HITS N @ cells + !
   CONFIRM @ 1 <= if 1 else 0 then  CONF N @ cells + !
   NEXTID @ ID N @ cells + !  NEXTID @ 1+ NEXTID !
   CREATED @ 1+ CREATED !
   CONF N @ cells + @ 0 <> if CONFCNT @ 1+ CONFCNT ! then
   N @ 1+ N ! ;

: UPDATE ( n -- ) {: ix:n :}
   TSV @ LM ix cells + @ NS>S DT F!
   X ix cells + F@  VX ix cells + F@ DT F@ f* f+  PXV F!
   Y ix cells + F@  VY ix cells + F@ DT F@ f* f+  PYV F!
   DXV F@ PXV F@ f- RX F!   DYV F@ PYV F@ f- RY F!
   RX F@ RY F@ DIST  ERR ERRN SAMPLE+
   PXV F@ ALPHA F@ RX F@ f* f+  X ix cells + F!
   PYV F@ ALPHA F@ RY F@ f* f+  Y ix cells + F!
   DT F@ 0.0 f> if
      VX ix cells + F@  BETA F@ RX F@ f* DT F@ f/  f+  VX ix cells + F!
      VY ix cells + F@  BETA F@ RY F@ f* DT F@ f/  f+  VY ix cells + F!
   then
   TSV @ LM ix cells + !
   HITS ix cells + dup @ 1+ swap !
   CONF ix cells + @ 0= if
      HITS ix cells + @ CONFIRM @ >= if 1 CONF ix cells + !  CONFCNT @ 1+ CONFCNT ! then
   then ;

: FIND ( -- )       \ set BEST to nearest track within gate, or -1
   -1 BEST !  GATE F@ BDIST F!  0 DI !
   begin DI @ N @ < while
      DXV F@ DI @ PRED-X f-  DYV F@ DI @ PRED-Y f-  DIST  DV F!
      DV F@ BDIST F@ f> 0= if  DV F@ BDIST F!  DI @ BEST !  then
      DI @ 1+ DI !
   repeat ;

: COPY ( n n -- ) {: d:n s:n :}
   X s cells + F@ X d cells + F!  Y s cells + F@ Y d cells + F!
   VX s cells + F@ VX d cells + F!  VY s cells + F@ VY d cells + F!
   LM s cells + @ LM d cells + !  HITS s cells + @ HITS d cells + !
   CONF s cells + @ CONF d cells + !  ID s cells + @ ID d cells + ! ;

: CAMS ( -- n ) DETS @ HBS @ + 0 > if 1 else 0 then ;

\ linear-interpolation percentile over a sorted float array (percentile() in .zig)
: PCTL-LIN ( ptr a n r -- r ) {: base:ptr len:n q:r :}
   len 0= if 0.0 else
      q len 1- s>f f* IDXF F!
      IDXF F@ f>s LO !
      IDXF F@ LO @ s>f f- FRAC F!
      FRAC F@ 0.0 f> if LO @ 1+ else LO @ then HI !
      LO @ cells base + F@  1.0 FRAC F@ f- f*
      HI @ cells base + F@  FRAC F@ f*  f+
   then ;
: SERIES-PCTL ( ptr a ptr a r -- r ) {: base:ptr nv:ptr q:r :} base nv @ q PCTL-LIN ;
: SERIES-MAX ( ptr a ptr a -- r ) {: base:ptr nv:ptr :} nv @ 0= if 0.0 else nv @ 1- cells base + F@ then ;

\ read accessors (private; a future CAMTRACK test reopens the package to reach them)
: DETS@ ( -- n ) DETS @ ;   : HBS@ ( -- n ) HBS @ ;
: EST@ ( -- n ) EST @ ;     : CREATED@ ( -- n ) CREATED @ ;
: CONFCNT@ ( -- n ) CONFCNT @ ;

public

: RESET ( -- )
   0.6 ALPHA F!  0.3 BETA F!  80.0 GATE F!  200.0 COAST F!  2 CONFIRM !
   0 N !  1 NEXTID !  0 LASTDET !
   0 DETS !  0 HBS !  0 EST !  0 CREATED !  0 CONFCNT !
   0 LATN !  0 HORN !  0 ERRN !  0 GAPN ! ;

: DET ( r r i64 -- )     \ detection: x y ts
   TSV !  DYV F!  DXV F!
   DETS @ 1+ DETS !
   FIND
   BEST @ 0 < if SPAWN else BEST @ UPDATE then
   LASTDET @ 0 <> if TSV @ LASTDET @ NS>MS  GAP GAPN SAMPLE+ then
   TSV @ LASTDET ! ;

: HB ( i64 i64 -- ) {: ts:i64 proc:i64 :}
   HBS @ 1+ HBS !
   0 DI ! begin DI @ N @ < while
      ts LM DI @ cells + @ NS>MS COAST F@ f> if
         DI @ N @ 1- COPY  N @ 1- N !
      else DI @ 1+ DI ! then
   repeat
   proc ts NS>MS LATV F!
   0 DI ! begin DI @ N @ < while
      LATV F@ LAT LATN SAMPLE+
      ts LM DI @ cells + @ NS>MS  HOR HORN SAMPLE+
      EST @ 1+ EST !
      DI @ 1+ DI !
   repeat ;

: FINISH ( -- )
   LAT LATN @ FSORT!  HOR HORN @ FSORT!  ERR ERRN @ FSORT!  GAP GAPN @ FSORT! ;

\ renderSummaryMarkdown ("snap: camera-rate summary")
: MD ( -- ptr u8 n )
   RB-RESET
   s" # Camera-Rate Tracking Summary" RB+ RB-NL  RB-NL
   s" cameras" CAMS MD-N
   s" detections" DETS @ MD-N
   s" heartbeats" HBS @ MD-N
   s" estimates" EST @ MD-N
   s" tracks created" CREATED @ MD-N
   s" tracks confirmed" CONFCNT @ MD-N
   s" - update latency ms p50/p95/p99/max: " RB+
      LAT LATN 0.5 SERIES-PCTL RB-FFIX3 47 RB-C  LAT LATN 0.95 SERIES-PCTL RB-FFIX3 47 RB-C
      LAT LATN 0.99 SERIES-PCTL RB-FFIX3 47 RB-C  LAT LATN SERIES-MAX RB-FFIX3 RB-NL
   s" - prediction horizon ms p50/p95/max: " RB+
      HOR HORN 0.5 SERIES-PCTL RB-FFIX3 47 RB-C  HOR HORN 0.95 SERIES-PCTL RB-FFIX3 47 RB-C  HOR HORN SERIES-MAX RB-FFIX3 RB-NL
   s" - prediction error px p50/p95/max: " RB+
      ERR ERRN 0.5 SERIES-PCTL RB-FFIX3 47 RB-C  ERR ERRN 0.95 SERIES-PCTL RB-FFIX3 47 RB-C  ERR ERRN SERIES-MAX RB-FFIX3 RB-NL
   s" - detection gap ms p50/p95/max: " RB+
      GAP GAPN 0.5 SERIES-PCTL RB-FFIX3 47 RB-C  GAP GAPN 0.95 SERIES-PCTL RB-FFIX3 47 RB-C  GAP GAPN SERIES-MAX RB-FFIX3 RB-NL
   s" result" s" ready" MD-S
   RB$ ;

end-package
