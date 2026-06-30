\ perception-analyze.f - NDJSON file analyzer for Odin live perception rows.
\
\ This bridges SCHEMA's parsed JSON validation and ODREC's checked live-record
\ structures into the PERCEPTION report renderer. It matches
\ src/perception_latency.zig analyze(): tick latencies/queues override detection
\ latencies/queues when present, tracker ticks override older tracker-update
\ fields, and per-camera rates are reduced into report min/mean/max fields.
\ Load after lib/errors.f lib/string.f lib/memory.f lib/fs.f tools/json.f
\ tools/json-file.f odin/float-cell.f odin/capture-schema.f
\ odin/capture-schema-json.f odin/perception-latency.f
\ odin/perception-render.f odin/live-records.f.

package PERCEPTION
private

16 constant PA-CAM-MAX
8192 constant PA-SAMPLE-MAX
8192 constant PA-KEY-MAX
128 constant PA-TEXT-CAP

-6220 constant E-PA-FULL
-6221 constant E-PA-JSON
-6222 constant E-PA-SCHEMA
-6223 constant E-PA-MISSING-LATENCY

\ stable text slabs; JSON string pointers are only valid until the next parse.
create PA-CAM-SER-BUF PA-CAM-MAX PA-TEXT-CAP * allot
create PA-CAM-LNA-BUF PA-CAM-MAX PA-TEXT-CAP * allot
create PA-TGT-BUF     PA-KEY-MAX PA-TEXT-CAP * allot

create PA-CAM-SER-A PA-CAM-MAX cells allot
create PA-CAM-SER-N PA-CAM-MAX cells allot
create PA-CAM-LNA-A PA-CAM-MAX cells allot
create PA-CAM-LNA-N PA-CAM-MAX cells allot

\ per-camera counts, ranges, unique-frame counts, and rendered rates
create PA-CDET       PA-CAM-MAX cells allot
create PA-CDET-UNIQ  PA-CAM-MAX cells allot
create PA-CDET-FIRST PA-CAM-MAX cells allot
create PA-CDET-LAST  PA-CAM-MAX cells allot
create PA-CDET-RATE  PA-CAM-MAX cells allot
create PA-CDET-RATEP PA-CAM-MAX cells allot

create PA-CINF       PA-CAM-MAX cells allot
create PA-CINF-UNIQ  PA-CAM-MAX cells allot
create PA-CINF-FIRST PA-CAM-MAX cells allot
create PA-CINF-LAST  PA-CAM-MAX cells allot
create PA-CINF-RATE  PA-CAM-MAX cells allot
create PA-CINF-RATEP PA-CAM-MAX cells allot

create PA-CTRK       PA-CAM-MAX cells allot
create PA-CTRK-UNIQ  PA-CAM-MAX cells allot
create PA-CTRK-FIRST PA-CAM-MAX cells allot
create PA-CTRK-LAST  PA-CAM-MAX cells allot
create PA-CTRK-RATE  PA-CAM-MAX cells allot
create PA-CTRK-RATEP PA-CAM-MAX cells allot

\ unique SDK frame timestamp sets, flattened by camera.
create PA-DET-TS PA-CAM-MAX PA-SAMPLE-MAX * cells allot
create PA-INF-TS PA-CAM-MAX PA-SAMPLE-MAX * cells allot
create PA-TRK-TS PA-CAM-MAX PA-SAMPLE-MAX * cells allot

\ report sample arrays
create PA-DET-LAT PA-SAMPLE-MAX cells allot
create PA-TIK-LAT PA-SAMPLE-MAX cells allot
create PA-TRK-LAT PA-SAMPLE-MAX cells allot
create PA-SCHED   PA-SAMPLE-MAX cells allot
create PA-TENSOR  PA-SAMPLE-MAX cells allot
create PA-DRUN    PA-SAMPLE-MAX cells allot
create PA-DCYCLE  PA-SAMPLE-MAX cells allot

\ rate-summary arrays
create PA-DRATES PA-CAM-MAX cells allot
create PA-IRATES PA-CAM-MAX cells allot
create PA-TRATES PA-CAM-MAX cells allot
create PA-CAM-DONE PA-CAM-MAX cells allot

\ distinct target ids and tracker-update indices
create PA-TGT-A PA-KEY-MAX cells allot
create PA-TGT-N PA-KEY-MAX cells allot
create PA-UPD-KEY PA-KEY-MAX cells allot

variable PA-CAM#
variable PA-TGT#
variable PA-UPD-KEY#
variable PA-UPD-TS#
variable PA-UPD-FIRST
variable PA-UPD-LAST
variable PA-TT#
variable PA-TT-FIRST
variable PA-TT-LAST

variable PA-DET-LAT#
variable PA-TIK-LAT#
variable PA-TRK-LAT#
variable PA-SCHED#
variable PA-TENSOR#
variable PA-DRUN#
variable PA-DCYCLE#
variable PA-DRATE#
variable PA-IRATE#
variable PA-TRATE#

variable PA-DQ-N
variable PA-DQ-SUM
variable PA-DQ-MAX
variable PA-TQ-N
variable PA-TQ-SUM
variable PA-TQ-MAX

variable PA-I
variable PA-FIND
variable PA-BEST
variable PA-CA
variable PA-CB
variable PA-CMP

create PA-DET-REC ODREC:DETECTION-REC allot
create PA-PT-REC  ODREC:PERCEPTION-TICK-REC allot
create PA-TT-REC  ODREC:TRACKER-TICK-REC allot

: PA-MIN ( n n -- n ) {: a:n b:n :} a b < if a else b then ;
: PA-MAX ( n n -- n ) {: a:n b:n :} a b > if a else b then ;
: PA-SLOT ( n ptr a -- ptr a ) {: ix:n base:ptr :} base ix cells + ;
: PA@ ( n ptr a -- n ) PA-SLOT @ ;
: PA! ( n n ptr a -- ) {: v:n ix:n base:ptr :} v ix base PA-SLOT ! ;
: PA-PTR-SLOT ( n ptr a -- ptr ptr u8 ) PA-SLOT 0 ptr-field ;
: PA-PTR@ ( n ptr a -- ptr u8 ) PA-PTR-SLOT @ ;
: PA-PTR! ( ptr u8 n ptr a -- ) {: a:ptr ix:n base:ptr :} a ix base PA-PTR-SLOT ! ;

: PA-TEXT-SLOT ( n ptr u8 -- ptr u8 ) {: ix:n base:ptr :}
   base ix PA-TEXT-CAP * + ;

: PA-TEXT-COPY! ( ptr u8 n n ptr u8 ptr a -- ) {: a:ptr u:n ix:n base:ptr lens:ptr :}
   u PA-TEXT-CAP > if E-PA-FULL throw then
   a ix base PA-TEXT-SLOT u BYTE-COPY
   u ix lens PA! ;

: PA-STR< ( ptr u8 n ptr u8 n -- bool ) {: a:ptr u:n b:ptr v:n :}
   0 PA-I !
   0 PA-CMP !
   begin PA-I @ u v PA-MIN < PA-CMP @ 0= and while
      a PA-I @ + c@ PA-CA !
      b PA-I @ + c@ PA-CB !
      PA-CA @ PA-CB @ < if -1 PA-CMP !
      else PA-CA @ PA-CB @ > if 1 PA-CMP ! then then
      PA-I @ 1+ PA-I !
   repeat
   PA-CMP @ 0 <> if PA-CMP @ 0 < else u v < then ;

: PA-FLOAT+ ( r ptr a ptr a -- ) {: x:r base:ptr np:ptr :}
   np @ PA-SAMPLE-MAX >= if E-PA-FULL throw then
   x base np @ cells + F!
   np @ 1+ np ! ;

: PA-Q-OBSERVE ( n ptr a ptr a ptr a -- ) {: depth:n np:ptr sump:ptr maxp:ptr :}
   np @ 1+ np !
   sump @ depth + sump !
   np @ 1 = if depth maxp !
   else depth maxp @ > if depth maxp ! then then ;

: PA-COUNT-RANGE+ ( n n ptr a ptr a ptr a -- ) {: ts:n ix:n counts:ptr firsts:ptr lasts:ptr :}
   ix counts PA@ 0= if
      ts ix firsts PA!  ts ix lasts PA!
   else
      ts ix firsts PA@ PA-MIN ix firsts PA!
      ts ix lasts  PA@ PA-MAX ix lasts  PA!
   then
   ix counts PA@ 1+ ix counts PA! ;

: PA-GLOBAL-RANGE+ ( n ptr a ptr a ptr a -- ) {: ts:n countp:ptr firstp:ptr lastp:ptr :}
   countp @ 0= if
      ts firstp !  ts lastp !
   else
      ts firstp @ PA-MIN firstp !
      ts lastp  @ PA-MAX lastp  !
   then
   countp @ 1+ countp ! ;

: PA-UNIQ-SLOT ( n n ptr a -- ptr a ) {: ix:n pos:n base:ptr :}
   base ix PA-SAMPLE-MAX * pos + cells + ;

: PA-UNIQ-HAS? ( n n ptr a n -- bool ) {: ts:n ix:n base:ptr count:n :}
   0 PA-I !
   begin PA-I @ count < while
      ix PA-I @ base PA-UNIQ-SLOT @ ts = if 0 0= exit then
      PA-I @ 1+ PA-I !
   repeat
   0 0= 0= ;

: PA-UNIQ+ ( n n ptr a ptr a -- ) {: ts:n ix:n base:ptr counts:ptr :}
   ix counts PA@ {: count:n :}
   ts ix base count PA-UNIQ-HAS? if exit then
   count PA-SAMPLE-MAX >= if E-PA-FULL throw then
   ts ix count base PA-UNIQ-SLOT !
   count 1+ ix counts PA! ;

: PA-CAM$ ( n -- ptr u8 n ) {: ix:n :}
   ix PA-CAM-SER-A PA-PTR@  ix PA-CAM-SER-N PA@ ;

: PA-LNA$ ( n -- ptr u8 n ) {: ix:n :}
   ix PA-CAM-LNA-A PA-PTR@  ix PA-CAM-LNA-N PA@ ;

: PA-CAM-FIND ( ptr u8 n -- n ) {: a:ptr u:n :}
   -1 PA-FIND !
   0 PA-I !
   begin PA-I @ PA-CAM# @ < PA-FIND @ 0 < and while
      a u PA-I @ PA-CAM$ STR= if PA-I @ PA-FIND ! then
      PA-I @ 1+ PA-I !
   repeat
   PA-FIND @ ;

: PA-CAM-CLEAR ( n -- ) {: ix:n :}
   0 ix PA-CDET PA!       0 ix PA-CDET-UNIQ PA!
   0 ix PA-CDET-FIRST PA! 0 ix PA-CDET-LAST PA! 0 ix PA-CDET-RATEP PA!
   0 ix PA-CINF PA!       0 ix PA-CINF-UNIQ PA!
   0 ix PA-CINF-FIRST PA! 0 ix PA-CINF-LAST PA! 0 ix PA-CINF-RATEP PA!
   0 ix PA-CTRK PA!       0 ix PA-CTRK-UNIQ PA!
   0 ix PA-CTRK-FIRST PA! 0 ix PA-CTRK-LAST PA! 0 ix PA-CTRK-RATEP PA! ;

: PA-CAM-ADD ( ptr u8 n ptr u8 n -- n ) {: ser:ptr seru:n lna:ptr lnau:n :}
   PA-CAM# @ PA-CAM-MAX >= if E-PA-FULL throw then
   PA-CAM# @ {: ix:n :}
   ix PA-CAM-SER-BUF PA-TEXT-SLOT ix PA-CAM-SER-A PA-PTR!
   ser seru ix PA-CAM-SER-BUF PA-CAM-SER-N PA-TEXT-COPY!
   ix PA-CAM-LNA-BUF PA-TEXT-SLOT ix PA-CAM-LNA-A PA-PTR!
   lna lnau ix PA-CAM-LNA-BUF PA-CAM-LNA-N PA-TEXT-COPY!
   ix PA-CAM-CLEAR
   ix 1+ PA-CAM# !
   ix ;

: PA-CAM-ENSURE ( ptr u8 n ptr u8 n -- n ) {: ser:ptr seru:n lna:ptr lnau:n :}
   ser seru PA-CAM-FIND dup 0 >= if exit then
   drop
   ser seru lna lnau PA-CAM-ADD ;

: PA-TGT$ ( n -- ptr u8 n ) {: ix:n :}
   ix PA-TGT-A PA-PTR@  ix PA-TGT-N PA@ ;

: PA-TGT-FIND ( ptr u8 n -- n ) {: a:ptr u:n :}
   -1 PA-FIND !
   0 PA-I !
   begin PA-I @ PA-TGT# @ < PA-FIND @ 0 < and while
      a u PA-I @ PA-TGT$ STR= if PA-I @ PA-FIND ! then
      PA-I @ 1+ PA-I !
   repeat
   PA-FIND @ ;

: PA-TGT+ ( ptr u8 n -- ) {: a:ptr u:n :}
   a u PA-TGT-FIND 0 >= if exit then
   PA-TGT# @ PA-KEY-MAX >= if E-PA-FULL throw then
   PA-TGT# @ {: ix:n :}
   ix PA-TGT-BUF PA-TEXT-SLOT ix PA-TGT-A PA-PTR!
   a u ix PA-TGT-BUF PA-TGT-N PA-TEXT-COPY!
   ix 1+ PA-TGT# ! ;

: PA-UPD-HAS? ( n -- bool ) {: key:n :}
   0 PA-I !
   begin PA-I @ PA-UPD-KEY# @ < while
      PA-I @ PA-UPD-KEY PA@ key = if 0 0= exit then
      PA-I @ 1+ PA-I !
   repeat
   0 0= 0= ;

: PA-UPD-KEY+ ( n -- ) {: key:n :}
   key PA-UPD-HAS? if exit then
   PA-UPD-KEY# @ PA-KEY-MAX >= if E-PA-FULL throw then
   key PA-UPD-KEY# @ PA-UPD-KEY PA!
   PA-UPD-KEY# @ 1+ PA-UPD-KEY# ! ;

: PA-UPD-OBS ( n n n n -- ) {: ip:n key:n tp:n ts:n :}
   tp 0= if exit then
   ts PA-UPD-TS# PA-UPD-FIRST PA-UPD-LAST PA-GLOBAL-RANGE+
   ip 0 <> if key PA-UPD-KEY+
   else PR-TRKUPD @ 1+ PR-TRKUPD ! then ;

: PA-RESET-TIMING ( -- )
   0 0 TG-SAMP PA! 0 1 TG-SAMP PA! 0 2 TG-SAMP PA! 0 3 TG-SAMP PA! ;

: PA-RESET-REPORT ( -- )
   PR-RESET
   0 PR-RECORDS ! 0 PR-DETREC ! 0 PR-INFTICK ! 0 PR-TRKTICK !
   0 PR-CAMERAS ! 0 PR-TARGETS ! 0 PR-LATSAMP !
   0 PR-QSAMP ! 0 PR-TRKUPD ! 0 PR-TLSAMP !
   0.0 PR-LP50 F! 0.0 PR-LP95 F! 0.0 PR-LP99 F! 0.0 PR-LMAX F!
   PA-RESET-TIMING ;

: PA-RESET ( -- )
   PA-RESET-REPORT
   0 PA-CAM# ! 0 PA-TGT# ! 0 PA-UPD-KEY# ! 0 PA-UPD-TS# !
   0 PA-TT# !
   0 PA-DET-LAT# ! 0 PA-TIK-LAT# ! 0 PA-TRK-LAT# !
   0 PA-SCHED# ! 0 PA-TENSOR# ! 0 PA-DRUN# ! 0 PA-DCYCLE# !
   0 PA-DRATE# ! 0 PA-IRATE# ! 0 PA-TRATE# !
   0 PA-DQ-N ! 0 PA-DQ-SUM ! 0 PA-DQ-MAX !
   0 PA-TQ-N ! 0 PA-TQ-SUM ! 0 PA-TQ-MAX ! ;

: PA-OBS-DETECTION ( i64 -- ) {: root:i64 :}
   root PA-DET-REC ODREC:DETECTION-LOAD
   PR-DETREC @ 1+ PR-DETREC !
   PA-DET-REC ODREC:DET.CAMERA-A @ PA-DET-REC ODREC:DET.CAMERA-N @
   PA-DET-REC ODREC:DET.LOGICAL-A @ PA-DET-REC ODREC:DET.LOGICAL-N @ PA-CAM-ENSURE {: ix:n :}
   PA-DET-REC ODREC:DET.SDK-TS @ ix PA-CDET PA-CDET-FIRST PA-CDET-LAST PA-COUNT-RANGE+
   PA-DET-REC ODREC:DET.SDK-TS @ ix PA-DET-TS PA-CDET-UNIQ PA-UNIQ+
   PA-DET-REC ODREC:DET.TARGET-A @ PA-DET-REC ODREC:DET.TARGET-N @ PA-TGT+
   PA-DET-REC ODREC:DET.LATENCY F@ PA-DET-LAT PA-DET-LAT# PA-FLOAT+
   PA-DET-REC ODREC:DET.QUEUE-PRESENT @ 0 <> if
      PA-DET-REC ODREC:DET.QUEUE-DEPTH @ PA-DQ-N PA-DQ-SUM PA-DQ-MAX PA-Q-OBSERVE
   then
   PA-DET-REC ODREC:DET.TRACKER-INDEX-PRESENT @ PA-DET-REC ODREC:DET.TRACKER-INDEX @
   PA-DET-REC ODREC:DET.TRACKER-TS-PRESENT @ PA-DET-REC ODREC:DET.TRACKER-TS @ PA-UPD-OBS ;

: PA-OBS-TICK ( i64 -- ) {: root:i64 :}
   root PA-PT-REC ODREC:PERCEPTION-TICK-LOAD
   PR-INFTICK @ 1+ PR-INFTICK !
   PA-PT-REC ODREC:PT.CAMERA-A @ PA-PT-REC ODREC:PT.CAMERA-N @
   PA-PT-REC ODREC:PT.LOGICAL-A @ PA-PT-REC ODREC:PT.LOGICAL-N @ PA-CAM-ENSURE {: ix:n :}
   PA-PT-REC ODREC:PT.SDK-TS @ ix PA-CINF PA-CINF-FIRST PA-CINF-LAST PA-COUNT-RANGE+
   PA-PT-REC ODREC:PT.SDK-TS @ ix PA-INF-TS PA-CINF-UNIQ PA-UNIQ+
   PA-PT-REC ODREC:PT.LATENCY F@ PA-TIK-LAT PA-TIK-LAT# PA-FLOAT+
   PA-PT-REC ODREC:PT.SCHEDULE-LAG-PRESENT @ 0 <> if
      PA-PT-REC ODREC:PT.SCHEDULE-LAG F@ PA-SCHED PA-SCHED# PA-FLOAT+
   then
   PA-PT-REC ODREC:PT.TENSOR-RETRIEVE-PRESENT @ 0 <> if
      PA-PT-REC ODREC:PT.TENSOR-RETRIEVE F@ PA-TENSOR PA-TENSOR# PA-FLOAT+
   then
   PA-PT-REC ODREC:PT.DETECTOR-RUN-PRESENT @ 0 <> if
      PA-PT-REC ODREC:PT.DETECTOR-RUN F@ PA-DRUN PA-DRUN# PA-FLOAT+
   then
   PA-PT-REC ODREC:PT.DETECTOR-CYCLE-PRESENT @ 0 <> if
      PA-PT-REC ODREC:PT.DETECTOR-CYCLE F@ PA-DCYCLE PA-DCYCLE# PA-FLOAT+
   then
   PA-PT-REC ODREC:PT.QUEUE-PRESENT @ 0 <> if
      PA-PT-REC ODREC:PT.QUEUE-DEPTH @ PA-TQ-N PA-TQ-SUM PA-TQ-MAX PA-Q-OBSERVE
   then
   PA-PT-REC ODREC:PT.TRACKER-INDEX-PRESENT @ PA-PT-REC ODREC:PT.TRACKER-INDEX @
   PA-PT-REC ODREC:PT.TRACKER-TS-PRESENT @ PA-PT-REC ODREC:PT.TRACKER-TS @ PA-UPD-OBS ;

: PA-OBS-TRACKER ( i64 -- ) {: root:i64 :}
   root PA-TT-REC ODREC:TRACKER-TICK-LOAD
   PR-TRKTICK @ 1+ PR-TRKTICK !
   PA-TT-REC ODREC:TT.CAMERA-A @ PA-TT-REC ODREC:TT.CAMERA-N @
   PA-TT-REC ODREC:TT.LOGICAL-A @ PA-TT-REC ODREC:TT.LOGICAL-N @ PA-CAM-ENSURE {: ix:n :}
   PA-TT-REC ODREC:TT.TRACKER-TS @ ix PA-CTRK PA-CTRK-FIRST PA-CTRK-LAST PA-COUNT-RANGE+
   PA-TT-REC ODREC:TT.SDK-TS @ ix PA-TRK-TS PA-CTRK-UNIQ PA-UNIQ+
   PA-TT-REC ODREC:TT.TRACKER-TS @ PA-TT# PA-TT-FIRST PA-TT-LAST PA-GLOBAL-RANGE+
   PA-TT-REC ODREC:TT.LATENCY F@ PA-TRK-LAT PA-TRK-LAT# PA-FLOAT+ ;

: PA-DISPATCH ( i64 i64 -- ) {: root:i64 rt:i64 :}
   PR-RECORDS @ 1+ PR-RECORDS !
   rt SCHEMA:DETECTION = if root PA-OBS-DETECTION exit then
   rt SCHEMA:PERCEPTION-TICK = if root PA-OBS-TICK exit then
   rt SCHEMA:TRACKER-TICK = if root PA-OBS-TRACKER exit then
   E-PA-SCHEMA throw ;

: PA-LINE ( ptr u8 n -- ) JSON-TRIM {: a:ptr u:n :}
   u 0= if exit then
   a u SCHEMA:VALIDATE-LINE {: rt:i64 vst:i64 :}
   vst SCHEMA:V-OK <> if E-PA-SCHEMA throw then
   a u JSON-PARSE rt PA-DISPATCH ;

: PA-FINISH-LATENCY ( ptr a n -- ) {: base:ptr count:n :}
   count 0= if E-PA-MISSING-LATENCY throw then
   base count TSUM
   TS-SAMPLES PR-LATSAMP !
   TS-P50@ PR-LP50 F!
   TS-P95@ PR-LP95 F!
   TS-P99@ PR-LP99 F!
   TS-MAX@ PR-LMAX F! ;

: PA-FINISH-Q ( ptr a ptr a ptr a -- ) {: np:ptr sump:ptr maxp:ptr :}
   np @ PR-QSAMP !
   np @ 0 > if
      maxp @ PR-QMAX ! -1 PR-QMAXP !
      sump @ s>f np @ s>f f/ PR-QMEAN F! -1 PR-QMEANP !
   then ;

: PA-FINISH-TIMING ( ptr a ptr a n -- ) {: base:ptr np:ptr ix:n :}
   base np @ TSUM
   TS-SAMPLES ix TG-SAMP PA!
   TS-SAMPLES 0 > if
      TS-P50@ TG-P50 ix cells + F!
      TS-P95@ TG-P95 ix cells + F!
      TS-P99@ TG-P99 ix cells + F!
      TS-MAX@ TG-MX ix cells + F!
   then ;

: PA-SET-OPTF ( r bool ptr b ptr b -- ) {: x:r ok:bool valp:ptr pp:ptr :}
   ok if x valp F! -1 pp ! else 0 pp ! then ;

: PA-CAM-RATE-FINISH ( n n n n ptr a ptr a ptr a ptr a -- )
   {: count:n first:n last:n ix:n vals:ptr flags:ptr sums:ptr sump:ptr :}
   count first last RATE-WINDOW {: rate:r ok:bool :}
   ok if
      rate vals ix cells + F!
      -1 ix flags PA!
      rate sums sump PA-FLOAT+
   else
      0 ix flags PA!
   then ;

: PA-FINISH-CAMERA-RATES ( n -- ) {: ix:n :}
   ix PA-CDET-UNIQ PA@ ix PA-CDET-FIRST PA@ ix PA-CDET-LAST PA@
      ix PA-CDET-RATE PA-CDET-RATEP PA-DRATES PA-DRATE# PA-CAM-RATE-FINISH
   ix PA-CINF-UNIQ PA@ ix PA-CINF-FIRST PA@ ix PA-CINF-LAST PA@
      ix PA-CINF-RATE PA-CINF-RATEP PA-IRATES PA-IRATE# PA-CAM-RATE-FINISH
   ix PA-CTRK PA@ ix PA-CTRK-FIRST PA@ ix PA-CTRK-LAST PA@
      ix PA-CTRK-RATE PA-CTRK-RATEP PA-TRATES PA-TRATE# PA-CAM-RATE-FINISH ;

: PA-FINISH-ONE-RSUM ( ptr a ptr a ptr a ptr a ptr a ptr a ptr a ptr a -- )
   {: vals:ptr np:ptr minv:ptr minp:ptr meanv:ptr meanp:ptr maxv:ptr maxp:ptr :}
   vals np @ RSUM
   RS-COUNT 0 > if
      RS-MIN@ minv F! -1 minp !
      RS-MEAN drop meanv F! -1 meanp !
      RS-MAX@ maxv F! -1 maxp !
   then ;

: PA-FINISH-RATES ( -- )
   0 PA-I !
   begin PA-I @ PA-CAM# @ < while
      PA-I @ PA-FINISH-CAMERA-RATES
      PA-I @ 1+ PA-I !
   repeat
   PA-DRATES PA-DRATE# PR-DRMIN PR-DRMINP PR-DRMEAN PR-DRMEANP PR-DRMAX PR-DRMAXP PA-FINISH-ONE-RSUM
   PA-IRATES PA-IRATE# PR-IRMIN PR-IRMINP PR-IRMEAN PR-IRMEANP PR-IRMAX PR-IRMAXP PA-FINISH-ONE-RSUM
   PA-TRATES PA-TRATE# @ RSUM
   RS-COUNT 0 > if
      RS-SUM@ PR-TRHZ F! -1 PR-TRHZP !
      RS-MIN@ PR-TRMIN F! -1 PR-TRMINP !
      RS-MEAN drop PR-TRMEAN F! -1 PR-TRMEANP !
      RS-MAX@ PR-TRMAX F! -1 PR-TRMAXP !
   then ;

: PA-FINISH-TRACKER ( -- )
   PR-TRKTICK @ 0 > if
      PR-TRKTICK @ PR-TRKUPD !
      PR-TRKTICK @ PA-TT-FIRST @ PA-TT-LAST @ RATE-WINDOW PR-TRHZ PR-TRHZP PA-SET-OPTF
      PA-TRK-LAT# @ PR-TLSAMP !
      PA-TRK-LAT# @ 0 > if
         PA-TRK-LAT PA-TRK-LAT# @ TSUM
         TS-P50@ PR-TLP50 F! -1 PR-TLP50P !
         TS-P95@ PR-TLP95 F! -1 PR-TLP95P !
         TS-P99@ PR-TLP99 F! -1 PR-TLP99P !
         TS-MAX@ PR-TLMAX F! -1 PR-TLMAXP !
      then
   else
      PA-UPD-KEY# @ 0 > if PA-UPD-KEY# @ PR-TRKUPD ! then
      PR-TRKUPD @ PA-UPD-FIRST @ PA-UPD-LAST @ RATE-WINDOW PR-TRHZ PR-TRHZP PA-SET-OPTF
   then ;

: PA-FINISH ( -- )
   PA-CAM# @ PR-CAMERAS !
   PA-TGT# @ PR-TARGETS !
   PA-TIK-LAT# @ 0 > if PA-TIK-LAT PA-TIK-LAT# @ else PA-DET-LAT PA-DET-LAT# @ then PA-FINISH-LATENCY
   PA-TQ-N @ 0 > if PA-TQ-N PA-TQ-SUM PA-TQ-MAX else PA-DQ-N PA-DQ-SUM PA-DQ-MAX then PA-FINISH-Q
   PA-FINISH-TRACKER
   PA-SCHED  PA-SCHED#  0 PA-FINISH-TIMING
   PA-TENSOR PA-TENSOR# 1 PA-FINISH-TIMING
   PA-DRUN   PA-DRUN#   2 PA-FINISH-TIMING
   PA-DCYCLE PA-DCYCLE# 3 PA-FINISH-TIMING
   PA-FINISH-RATES ;

public

: PA-ANALYZE-FILE ( ptr u8 n -- )
   PA-RESET
   JSONLF-OPEN
   begin JSONLF-NEXT-LINE while
      PA-LINE
   repeat
   2drop
   PA-FINISH ;

: PA-SET-READINESS ( r r n r -- ) {: p95:r p99:r q:n minrate:r :}
   p95 OPT-MLP95 F! -1 OPT-MLP95P !
   p99 OPT-MLP99 F! -1 OPT-MLP99P !
   q OPT-MQ ! -1 OPT-MQP !
   minrate OPT-MDR F! -1 OPT-MDRP ! ;

private

: PA-CAM-USED? ( n ptr a -- bool ) PA-SLOT @ 0 <> ;
: PA-CAM-MARK ( n ptr a -- ) {: ix:n base:ptr :} -1 ix base PA! ;

: PA-BEST-CANDIDATE ( n -- ) {: ix:n :}
   ix PA-CAM-DONE PA-CAM-USED? if exit then
   PA-BEST @ 0 < if ix PA-BEST ! exit then
   ix PA-LNA$ PA-BEST @ PA-LNA$ PA-STR< if ix PA-BEST ! then ;

: PA-NEXT-CAMERA ( -- n )
   -1 PA-BEST !
   0 PA-I !
   begin PA-I @ PA-CAM# @ < while
      PA-I @ PA-BEST-CANDIDATE
      PA-I @ 1+ PA-I !
   repeat
   PA-BEST @ dup 0 >= if dup PA-CAM-DONE PA-CAM-MARK then ;

: PA-OPT-F ( n ptr a -- ) {: present:n cell:ptr :}
   present 0 <> if cell F@ RB-FFIX3 then ;

: PA-CAM-RATE ( n ptr a ptr a -- ) {: ix:n vals:ptr flags:ptr :}
   ix flags PA@ vals ix cells + PA-OPT-F ;

: PA-CAM-UOPT ( n ptr a ptr a -- ) {: ix:n counts:ptr vals:ptr :}
   ix counts PA@ 0 <> if ix vals PA@ RB# then ;

public

: PA-CAMERA-METRICS-CSV ( -- ptr u8 n )
   RB-RESET
   s" camera_serial,logical_name,detections,unique_sdk_frames,first_sdk_image_timestamp_ns,last_sdk_image_timestamp_ns,detector_output_rate_hz,inference_ticks,unique_inference_sdk_frames,first_inference_sdk_image_timestamp_ns,last_inference_sdk_image_timestamp_ns,inference_rate_hz,tracker_ticks,unique_tracker_sdk_frames,first_tracker_timestamp_ns,last_tracker_timestamp_ns,tracker_rate_hz" RB+ RB-NL
   0 PA-I ! begin PA-I @ PA-CAM# @ < while 0 PA-I @ PA-CAM-DONE PA! PA-I @ 1+ PA-I ! repeat
   begin PA-NEXT-CAMERA dup 0 >= while
      {: ix:n :}
      ix PA-CAM$ RB+ CM ix PA-LNA$ RB+ CM
      ix PA-CDET PA@ RB# CM ix PA-CDET-UNIQ PA@ RB# CM
      ix PA-CDET PA-CDET-FIRST PA-CAM-UOPT CM ix PA-CDET PA-CDET-LAST PA-CAM-UOPT CM
      ix PA-CDET-RATE PA-CDET-RATEP PA-CAM-RATE CM
      ix PA-CINF PA@ RB# CM ix PA-CINF-UNIQ PA@ RB# CM
      ix PA-CINF PA-CINF-FIRST PA-CAM-UOPT CM ix PA-CINF PA-CINF-LAST PA-CAM-UOPT CM
      ix PA-CINF-RATE PA-CINF-RATEP PA-CAM-RATE CM
      ix PA-CTRK PA@ RB# CM ix PA-CTRK-UNIQ PA@ RB# CM
      ix PA-CTRK PA-CTRK-FIRST PA-CAM-UOPT CM ix PA-CTRK PA-CTRK-LAST PA-CAM-UOPT CM
      ix PA-CTRK-RATE PA-CTRK-RATEP PA-CAM-RATE RB-NL
   repeat
   drop
   RB$ ;

end-package
