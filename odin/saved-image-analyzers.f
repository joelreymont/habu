\ saved-image-analyzers.f - Habu saved-image analyzer entrypoints.
\
\ Reads odin.capture.v1 NDJSON, validates rows through the shared schema
\ validator, decodes saved NetPBM frames, and writes exposure, low-light,
\ motion-blur, and timestamp-sync artifacts without invoking the Zig CLI.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/render.f
require lib/report.f
require lib/float.f
require lib/fmt.f
require lib/hashmap.f
require lib/sort.f
require tools/json.f
require tools/json-file.f
require odin/capture-schema.f
require odin/capture-schema-json.f
require odin/netpbm.f
require odin/timestamp-metrics.f
require odin/timestamp-phase.f
require odin/timestamp-render.f

package SIA
private

4 constant SA-MAX-CAM
256 constant SA-TEXT-CAP
$300000 constant SA-IMG-CAP
$10000 constant SA-PREV-CAP
56 constant SA-CELLS
256 constant SA-HIST-BINS
1000000 constant SA-NS-PER-MS

0 constant C-SER-U
1 constant C-LNA-U
2 constant C-FRAMES
3 constant C-DROP-FLAGS
4 constant C-FRAMES-DROPPED
5 constant C-REGRESSIONS
6 constant C-EXP-BASE
15 constant C-GAIN-BASE
24 constant C-AUTO-TRUE
25 constant C-AUTO-FALSE
26 constant C-AUTO-UNKNOWN
27 constant C-IMAGE-PATHS
28 constant C-IMAGE-MISSING
29 constant C-IMAGE-DECODE-FAIL
30 constant C-LUMA-FRAMES
31 constant C-PIXELS
32 constant C-LUMA-SUM
33 constant C-SAT-DARK
34 constant C-SAT-BRIGHT
35 constant C-LAST-TS-HAVE
36 constant C-LAST-TS
37 constant C-PER-SAMP
38 constant C-PER-MIN-US
39 constant C-PER-MAX-US
40 constant C-PER-SUM-US
41 constant C-LAP-SAMP
42 constant C-LAP-MIN
43 constant C-LAP-MAX
44 constant C-LAP-SUM
45 constant C-EDGE-SAMP
46 constant C-EDGE-SUM-MILLI
47 constant C-DELTA-SAMP
48 constant C-DELTA-MAX-MILLI
49 constant C-DELTA-SUM-MILLI
50 constant C-PREV-LEN
51 constant C-PREV-W
52 constant C-PREV-H
53 constant C-NOISE-SAMP
54 constant C-NOISE-SUM-MILLI
55 constant C-RESERVED

-8300 constant E-SIA-SCHEMA
-8301 constant E-SIA-CAMERA
-8302 constant E-SIA-NOFRAMES
-8303 constant E-SIA-PATH

create SA-SERIAL SA-MAX-CAM SA-TEXT-CAP * allot
create SA-LOGICAL SA-MAX-CAM SA-TEXT-CAP * allot
create SA-STATE SA-MAX-CAM SA-CELLS * cells allot
create SA-HIST SA-MAX-CAM SA-HIST-BINS * cells allot
create SA-IMG-BUF SA-IMG-CAP allot
create SA-PREV-BUF SA-MAX-CAM SA-PREV-CAP * allot
create SA-OUT-PATH FS-PATH-CAP allot
create SA-IMG-PATH FS-PATH-CAP allot

variable SA-CAM-N
variable SA-FRAME-N
variable SA-DO-IMAGES
variable SA-ROOT-A
variable SA-ROOT-U
variable SA-I
variable SA-J
variable SA-SUM
variable SA-SUMSQ
variable SA-EDGE
variable SA-SAMPLES
variable SA-LAP-SUM
variable SA-LAP-SUMSQ
variable SA-MIN
variable SA-MAX
variable SA-TMP
variable SA-SQ-X
variable SA-SQ-R
variable SA-SQ-N

: SA-TRUE ( -- bool ) 0 0= ;
: SA-FALSE ( -- bool ) SA-TRUE 0= ;
: ABS-I ( n -- n ) dup 0 < if 0 swap - then ;
: MIN2 ( n n -- n ) {: a:n b:n :} a b < if a else b then ;
: MAX2 ( n n -- n ) {: a:n b:n :} a b > if a else b then ;

: ZERO-CELLS ( ptr a n -- )
   {: p:ptr n:n :}
   0 begin dup n < while
      0 p over cells + !
      1+
   repeat drop ;

: SA-RESET-ARRAYS ( -- )
   SA-STATE SA-MAX-CAM SA-CELLS * ZERO-CELLS
   SA-HIST SA-MAX-CAM SA-HIST-BINS * ZERO-CELLS
   0 SA-CAM-N !
   0 SA-FRAME-N ! ;

: SLOT ( n ptr u8 -- ptr u8 ) swap SA-TEXT-CAP * + ;
: SER-SLOT ( n -- ptr u8 ) SA-SERIAL SLOT ;
: LNA-SLOT ( n -- ptr u8 ) SA-LOGICAL SLOT ;
: CAM-P ( n n -- ptr a ) {: cam:n off:n :} cam SA-CELLS * off + cells SA-STATE + ;
: CAM@ ( n n -- n ) CAM-P @ ;
: CAM! ( n n n -- ) {: v:n cam:n off:n :} v cam off CAM-P ! ;
: CAM+! ( n n n -- ) {: v:n cam:n off:n :} cam off CAM-P dup @ v + swap ! ;
: SER$ ( n -- ptr u8 n ) dup SER-SLOT swap C-SER-U CAM@ ;
: LNA$ ( n -- ptr u8 n ) dup LNA-SLOT swap C-LNA-U CAM@ ;
: HIST-P ( n n -- ptr a ) {: cam:n bin:n :} cam SA-HIST-BINS * bin + cells SA-HIST + ;
: PREV-SLOT ( n -- ptr u8 ) SA-PREV-CAP * SA-PREV-BUF + ;

: COPY-TEXT ( ptr u8 n ptr u8 -- n )
   {: a:ptr u:n dst:ptr :}
   u SA-TEXT-CAP >= if E-SIA-CAMERA throw then
   a dst u BYTE-COPY
   u ;

: CAM-FIND ( ptr u8 n -- n )
   {: a:ptr u:n :}
   0 SA-I !
   begin SA-I @ SA-CAM-N @ < while
      a u SA-I @ SER$ STR= if SA-I @ exit then
      SA-I @ 1+ SA-I !
   repeat
   -1 ;

: CAM-ADD ( ptr u8 n ptr u8 n -- n )
   {: sa:ptr sn:n la:ptr ln:n :}
   SA-CAM-N @ SA-MAX-CAM >= if E-SIA-CAMERA throw then
   SA-CAM-N @ {: ix:n :}
   sa sn ix SER-SLOT COPY-TEXT ix C-SER-U CAM!
   la ln ix LNA-SLOT COPY-TEXT ix C-LNA-U CAM!
   ix 1+ SA-CAM-N !
   ix ;

: CAM-ENSURE ( ptr u8 n ptr u8 n -- n )
   {: sa:ptr sn:n la:ptr ln:n :}
   sa sn CAM-FIND dup 0 >= if exit then drop
   sa sn la ln CAM-ADD ;

: STAT-P ( n n n -- ptr a ) {: cam:n base:n off:n :} cam base off + CAM-P ;
: STAT@ ( n n n -- n ) STAT-P @ ;
: STAT! ( n n n n -- ) {: v:n cam:n base:n off:n :} v cam base off STAT-P ! ;
: STAT+! ( n n n n -- ) {: v:n cam:n base:n off:n :} cam base off STAT-P dup @ v + swap ! ;

: STAT-ADD ( n n n n -- )
   {: cam:n val:n frame:n base:n :}
   cam base 0 STAT@ 0= if
      val cam base 1 STAT!
      val cam base 2 STAT!
      val cam base 4 STAT!
      val cam base 5 STAT!
      0 cam base 6 STAT!
      0 cam base 7 STAT!
      1 cam base 8 STAT!
   else
      val cam base 1 STAT@ < if val cam base 1 STAT! then
      val cam base 2 STAT@ > if val cam base 2 STAT! then
      val cam base 5 STAT@ <> if
         1 cam base 6 STAT+!
         frame cam base 7 STAT!
      then
      val cam base 5 STAT!
   then
   val cam base 3 STAT+!
   1 cam base 0 STAT+! ;

: STAT-MEAN ( n n -- n ) {: cam:n base:n :}
   cam base 0 STAT@ 0= if 0 else cam base 3 STAT@ cam base 0 STAT@ / then ;

: JNODE ( n ptr u8 n -- n ) JSON-GET ;
: JSTR$ ( n ptr u8 n -- ptr u8 n ) JNODE JSON-STRING$ ;
: JINT ( n ptr u8 n -- n ) JNODE JSON-NUMBER$ STR>NUMBER? drop ;
: JBOOL ( n ptr u8 n -- n ) JNODE JSON-BOOL@ if 1 else 0 then ;

: JOPT-STR ( n ptr u8 n -- ptr u8 n bool )
   JNODE {: v:n :}
   v -1 = if s" " SA-FALSE exit then
   v JSON-KIND J-NULL = if s" " SA-FALSE exit then
   v JSON-STRING$ SA-TRUE ;

: JOPT-INT ( n ptr u8 n -- n bool )
   JNODE {: v:n :}
   v -1 = if 0 SA-FALSE exit then
   v JSON-KIND J-NULL = if 0 SA-FALSE exit then
   v JSON-NUMBER$ STR>NUMBER? ;

: JOPT-BOOL ( n ptr u8 n -- n bool )
   JNODE {: v:n :}
   v -1 = if 0 SA-FALSE exit then
   v JSON-KIND J-NULL = if 0 SA-FALSE exit then
   v JSON-BOOL@ if 1 else 0 then SA-TRUE ;

: ABS-PATH? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0 > if a c@ 47 = else SA-FALSE then ;

: IMAGE-PATH$ ( ptr u8 n -- ptr u8 n )
   {: a:ptr u:n :}
   a u ABS-PATH? if
      u FS-PATH-CAP > if E-SIA-PATH throw then
      a SA-IMG-PATH u BYTE-COPY
      SA-IMG-PATH u exit
   then
   SA-ROOT-A @ SA-ROOT-U @ a u SA-IMG-PATH JOIN-PATH
   SA-IMG-PATH swap ;

: OUT-FILE$ ( ptr u8 n ptr u8 n -- ptr u8 n )
   SA-OUT-PATH JOIN-PATH
   SA-OUT-PATH swap ;

: CSV-OPT-N ( n n -- ) {: present:n val:n :}
   present 0 > if val RB# then ;
: CSV-OPT-N3 ( n n -- ) {: present:n val:n :}
   present 0 > if val 1 RB-FIXED3 then ;
: CSV-COMMA ( -- ) CM ;

: PCT3 ( n n -- ) {: num:n den:n :}
   den 0= if s" 0.000" RB+ exit then
   num 100000 * den / RB-MILLI3 ;

: HIST-PCT ( n n -- n )
   {: cam:n pct:n :}
   cam C-PIXELS CAM@ {: total:n :}
   total 0= if
      0
   else
      total pct * 99 + 100 / 1 MAX2 1- SA-TMP !
      0 SA-SUM !
      0 SA-J !
      begin SA-J @ SA-HIST-BINS < while
         SA-SUM @ cam SA-J @ HIST-P @ + SA-SUM !
         SA-SUM @ SA-TMP @ > if SA-J @ exit then
         SA-J @ 1+ SA-J !
      repeat
      255
   then ;

: MEAN-LUMA ( n -- n )
   {: cam:n :}
   cam C-PIXELS CAM@ 0= if 0 else cam C-LUMA-SUM CAM@ cam C-PIXELS CAM@ / then ;

: ISQRT ( n -- n )
   dup 1 <= if exit then
   SA-SQ-X !
   SA-SQ-X @ SA-SQ-R !
   begin
      SA-SQ-X @ SA-SQ-R @ / SA-SQ-R @ + 2 / SA-SQ-N !
      SA-SQ-N @ SA-SQ-R @ <
   while
      SA-SQ-N @ SA-SQ-R !
   repeat
   SA-SQ-R @ ;

: PREV-COPY ( n ptr u8 n n n -- )
   {: cam:n pix:ptr u:n w:n h:n :}
   u SA-PREV-CAP > if
      0 cam C-PREV-LEN CAM!
      exit
   then
   pix cam PREV-SLOT u BYTE-COPY
   u cam C-PREV-LEN CAM!
   w cam C-PREV-W CAM!
   h cam C-PREV-H CAM! ;

: ADD-EDGE ( n ptr u8 n n -- )
   {: cam:n pix:ptr w:n h:n :}
   w 2 >= h 2 >= and if
      0 SA-EDGE !
      0 SA-SAMPLES !
      0 SA-I !
      begin SA-I @ h 1- < while
         0 SA-J !
         begin SA-J @ w 1- < while
            SA-I @ w * SA-J @ + {: off:n :}
            pix off + c@ {: c:n :}
            pix off 1+ + c@ c - ABS-I
            pix off w + + c@ c - ABS-I + 32 >= if 1 SA-EDGE +! then
            1 SA-SAMPLES +!
            SA-J @ 1+ SA-J !
         repeat
         SA-I @ 1+ SA-I !
      repeat
      SA-SAMPLES @ 0 > if
         SA-EDGE @ 100000 * SA-SAMPLES @ / cam C-EDGE-SUM-MILLI CAM+!
         1 cam C-EDGE-SAMP CAM+!
      then
   then ;

: ADD-LAPLACIAN ( n ptr u8 n n -- )
   {: cam:n pix:ptr w:n h:n :}
   w 3 >= h 3 >= and if
      0 SA-LAP-SUM !
      0 SA-LAP-SUMSQ !
      0 SA-SAMPLES !
      1 SA-I !
      begin SA-I @ h 1- < while
         1 SA-J !
         begin SA-J @ w 1- < while
            SA-I @ w * SA-J @ + {: off:n :}
            pix off + c@ 4 *
            pix off 1- + c@ -
            pix off 1+ + c@ -
            pix off w - + c@ -
            pix off w + + c@ - {: lap:n :}
            SA-LAP-SUM @ lap + SA-LAP-SUM !
            SA-LAP-SUMSQ @ lap lap * + SA-LAP-SUMSQ !
            1 SA-SAMPLES +!
            SA-J @ 1+ SA-J !
         repeat
         SA-I @ 1+ SA-I !
      repeat
      SA-SAMPLES @ 0 > if
         SA-LAP-SUM @ SA-SAMPLES @ / {: mean:n :}
         SA-LAP-SUMSQ @ SA-SAMPLES @ / mean mean * - {: var:n :}
         var 0 < if 0 else var then {: clean:n :}
         cam C-LAP-SAMP CAM@ 0= if
            clean cam C-LAP-MIN CAM!
            clean cam C-LAP-MAX CAM!
         else
            clean cam C-LAP-MIN CAM@ < if clean cam C-LAP-MIN CAM! then
            clean cam C-LAP-MAX CAM@ > if clean cam C-LAP-MAX CAM! then
         then
         clean cam C-LAP-SUM CAM+!
         1 cam C-LAP-SAMP CAM+!
      then
   then ;

: ADD-DELTA ( n ptr u8 n n n -- )
   {: cam:n pix:ptr u:n w:n h:n :}
   cam C-PREV-LEN CAM@ u =
   cam C-PREV-W CAM@ w = and
   cam C-PREV-H CAM@ h = and if
      0 SA-SUM !
      0 begin dup u < while
         cam PREV-SLOT over + c@ pix over + c@ - ABS-I SA-SUM @ + SA-SUM !
         1+
      repeat drop
      SA-SUM @ 1000 * u / SA-TMP !
      SA-TMP @ cam C-DELTA-SUM-MILLI CAM+!
      SA-TMP @ cam C-DELTA-MAX-MILLI CAM@ > if SA-TMP @ cam C-DELTA-MAX-MILLI CAM! then
      1 cam C-DELTA-SAMP CAM+!
   then ;

: ADD-LUMA ( n ptr u8 n n n -- )
   {: cam:n pix:ptr u:n w:n h:n :}
   0 SA-SUM !
   0 SA-SUMSQ !
   0 begin dup u < while
      pix over + c@ {: lum:n :}
      lum cam HIST-P @ 1+ cam lum HIST-P !
      lum SA-SUM @ + SA-SUM !
      SA-SUMSQ @ lum lum * + SA-SUMSQ !
      lum 5 <= if 1 cam C-SAT-DARK CAM+! then
      lum 250 >= if 1 cam C-SAT-BRIGHT CAM+! then
      1+
   repeat drop
   u cam C-PIXELS CAM+!
   SA-SUM @ cam C-LUMA-SUM CAM+!
   1 cam C-LUMA-FRAMES CAM+!
   u 0 > if
      SA-SUM @ u / {: mean:n :}
      SA-SUMSQ @ u / mean mean * - {: var:n :}
      var 0 < if 0 else var then ISQRT 1000 * cam C-NOISE-SUM-MILLI CAM+!
      1 cam C-NOISE-SAMP CAM+!
   then
   cam pix w h ADD-EDGE
   cam pix w h ADD-LAPLACIAN
   cam pix u w h ADD-DELTA
   cam pix u w h PREV-COPY ;

: PROCESS-IMAGE ( n ptr u8 n -- )
   {: cam:n a:ptr u:n :}
   1 cam C-IMAGE-PATHS CAM+!
   a u IMAGE-PATH$ 2dup FILE? if
      SA-IMG-BUF SA-IMG-CAP READ-ALL {: nread:n :}
      SA-IMG-BUF nread NETPBM:DECODE {: luma:ptr lu:n ok:bool :}
      ok if
         cam luma lu NETPBM:WIDTH@ NETPBM:HEIGHT@ ADD-LUMA
      else
         1 cam C-IMAGE-DECODE-FAIL CAM+!
      then
   else
      2drop
      1 cam C-IMAGE-MISSING CAM+!
   then
   ;

: PROCESS-FRAME ( n -- )
   {: root:n :}
   root s" serial" JSTR$ {: sa:ptr sn:n :}
   root s" logical_name" JSTR$ {: la:ptr ln:n :}
   sa sn la ln CAM-ENSURE {: cam:n :}
   root s" fps_target" JINT {: fps:n :}
   root s" frame_index" JINT {: fidx:n :}
   root s" sdk_image_timestamp_ns" JINT {: sdk:n :}
   root s" host_monotonic_ns" JINT {: host:n :}
   root s" dropped" JBOOL {: dropped:n :}
   root s" duplicate" JBOOL {: dupf:n :}
   root s" timestamp_regressed" JBOOL {: regr:n :}
   1 SA-FRAME-N +!
   1 cam C-FRAMES CAM+!
   dropped 0 <> if 1 cam C-DROP-FLAGS CAM+! then
   regr 0 <> if 1 cam C-REGRESSIONS CAM+! then
   cam C-LAST-TS-HAVE CAM@ 0 <> if
      sdk cam C-LAST-TS CAM@ >= if
         sdk cam C-LAST-TS CAM@ - 1000 / {: us:n :}
         cam C-PER-SAMP CAM@ 0= if
            us cam C-PER-MIN-US CAM!
            us cam C-PER-MAX-US CAM!
         else
            us cam C-PER-MIN-US CAM@ < if us cam C-PER-MIN-US CAM! then
            us cam C-PER-MAX-US CAM@ > if us cam C-PER-MAX-US CAM! then
         then
         us cam C-PER-SUM-US CAM+!
         1 cam C-PER-SAMP CAM+!
      then
   then
   1 cam C-LAST-TS-HAVE CAM!
   sdk cam C-LAST-TS CAM!
   root s" exposure_us" JOPT-INT if
      dup 0 >= if cam swap fidx C-EXP-BASE STAT-ADD else drop then
   else drop then
   root s" gain" JOPT-INT if
      dup 0 >= if cam swap fidx C-GAIN-BASE STAT-ADD else drop then
   else drop then
   root s" auto_exposure" JOPT-BOOL if
      0 <> if 1 cam C-AUTO-TRUE CAM+! else 1 cam C-AUTO-FALSE CAM+! then
   else drop 1 cam C-AUTO-UNKNOWN CAM+! then
   cam SER$ cam LNA$ fps fidx sdk host dropped dupf regr CAMSYNC:TM-ADD
   fidx sdk host CAMSYNC:TG-ADD
   cam fidx sdk CAMSYNC:FS-ADD
   SA-DO-IMAGES @ 0 <> if
      root s" image_path" JOPT-STR if
         {: ipath:ptr ipathu:n :}
         cam ipath ipathu PROCESS-IMAGE
      else
         2drop
      then
   then ;

: PROCESS-SCHEMA ( n -- )
   {: root:n :}
   root s" command" JSTR$ CAMSYNC:TX-SCHEMA ;

: PROCESS-SUMMARY ( n -- )
   {: root:n :}
   root s" serial" JOPT-STR if
      2dup CAM-FIND {: cam:n :}
      root s" frames_dropped" JINT {: fd:n :}
      cam 0 >= if fd cam C-FRAMES-DROPPED CAM! then
      fd CAMSYNC:TM-SUMMARY
   else
      2drop
   then ;

: PROCESS-LINE ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0 <> if
      a u SCHEMA:VALIDATE-LINE {: rt:n st:n :}
      st SCHEMA:V-OK <> if E-SIA-SCHEMA throw then
      a u JSON-PARSE {: root:n :}
      rt SCHEMA:SCHEMA = if root PROCESS-SCHEMA else
         rt SCHEMA:FRAME = if root PROCESS-FRAME else
            rt SCHEMA:SUMMARY = if root PROCESS-SUMMARY then
         then
      then
   then ;

: FINISH-SYNC ( -- )
   CAMSYNC:TM-FINISH
   CAMSYNC:TM-COUNT CAMSYNC:TX-BUILD
   CAMSYNC:PHO-BUILD
   CAMSYNC:TP-BUILD
   CAMSYNC:FP-BUILD
   CAMSYNC:SR-BUILD
   CAMSYNC:TPR-BUILD ;

: COLLATE ( ptr u8 n ptr u8 n n -- )
   {: in:ptr inu:n root:ptr rootu:n images:n :}
   SA-RESET-ARRAYS
   images SA-DO-IMAGES !
   root SA-ROOT-A !
   rootu SA-ROOT-U !
   CAMSYNC:TM-RESET
   CAMSYNC:TX-RESET
   CAMSYNC:FS-RESET
   in inu JSONLF-OPEN
   begin JSONLF-NEXT-LINE while PROCESS-LINE repeat 2drop
   SA-FRAME-N @ 0= if E-SIA-NOFRAMES throw then
   FINISH-SYNC ;

: WRITE-FILE ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: dir:ptr diru:n name:ptr nameu:n data:ptr datau:n :}
   dir diru name nameu OUT-FILE$ data datau WRITE-ALL ;

: EXPOSURE-CSV ( -- ptr u8 n )
   RB-RESET
   s" serial,logical_name,frames,dropped_event_flags,frames_dropped,timestamp_regressions,exposure_samples,exposure_min_us,exposure_max_us,exposure_mean_us,exposure_first_us,exposure_last_us,exposure_changes,exposure_settled_after_frame,gain_samples,gain_min,gain_max,gain_mean,gain_first,gain_last,gain_changes,gain_settled_after_frame,auto_exposure_true,auto_exposure_false,auto_exposure_unknown,image_path_records,image_missing,image_decode_failures,luminance_frames,pixels,mean_luminance,median_luminance,p05_luminance,p95_luminance,contrast_p95_p05,saturated_dark_pct,saturated_bright_pct" RB+ RB-NL
   0 SA-I !
   begin SA-I @ SA-CAM-N @ < while
      SA-I @ SER$ RB+ CSV-COMMA
      SA-I @ LNA$ RB+ CSV-COMMA
      SA-I @ C-FRAMES CAM@ RB# CSV-COMMA
      SA-I @ C-DROP-FLAGS CAM@ RB# CSV-COMMA
      SA-I @ C-FRAMES-DROPPED CAM@ RB# CSV-COMMA
      SA-I @ C-REGRESSIONS CAM@ RB# CSV-COMMA
      SA-I @ C-EXP-BASE 0 STAT@ RB# CSV-COMMA
      SA-I @ C-EXP-BASE 0 STAT@ SA-I @ C-EXP-BASE 1 STAT@ CSV-OPT-N CSV-COMMA
      SA-I @ C-EXP-BASE 0 STAT@ SA-I @ C-EXP-BASE 2 STAT@ CSV-OPT-N CSV-COMMA
      SA-I @ C-EXP-BASE 0 STAT@ SA-I @ C-EXP-BASE STAT-MEAN CSV-OPT-N CSV-COMMA
      SA-I @ C-EXP-BASE 0 STAT@ SA-I @ C-EXP-BASE 4 STAT@ CSV-OPT-N CSV-COMMA
      SA-I @ C-EXP-BASE 0 STAT@ SA-I @ C-EXP-BASE 5 STAT@ CSV-OPT-N CSV-COMMA
      SA-I @ C-EXP-BASE 6 STAT@ RB# CSV-COMMA
      SA-I @ C-EXP-BASE 6 STAT@ SA-I @ C-EXP-BASE 7 STAT@ CSV-OPT-N CSV-COMMA
      SA-I @ C-GAIN-BASE 0 STAT@ RB# CSV-COMMA
      SA-I @ C-GAIN-BASE 0 STAT@ SA-I @ C-GAIN-BASE 1 STAT@ CSV-OPT-N3 CSV-COMMA
      SA-I @ C-GAIN-BASE 0 STAT@ SA-I @ C-GAIN-BASE 2 STAT@ CSV-OPT-N3 CSV-COMMA
      SA-I @ C-GAIN-BASE 0 STAT@ SA-I @ C-GAIN-BASE STAT-MEAN CSV-OPT-N3 CSV-COMMA
      SA-I @ C-GAIN-BASE 0 STAT@ SA-I @ C-GAIN-BASE 4 STAT@ CSV-OPT-N3 CSV-COMMA
      SA-I @ C-GAIN-BASE 0 STAT@ SA-I @ C-GAIN-BASE 5 STAT@ CSV-OPT-N3 CSV-COMMA
      SA-I @ C-GAIN-BASE 6 STAT@ RB# CSV-COMMA
      SA-I @ C-GAIN-BASE 6 STAT@ SA-I @ C-GAIN-BASE 7 STAT@ CSV-OPT-N CSV-COMMA
      SA-I @ C-AUTO-TRUE CAM@ RB# CSV-COMMA
      SA-I @ C-AUTO-FALSE CAM@ RB# CSV-COMMA
      SA-I @ C-AUTO-UNKNOWN CAM@ RB# CSV-COMMA
      SA-I @ C-IMAGE-PATHS CAM@ RB# CSV-COMMA
      SA-I @ C-IMAGE-MISSING CAM@ RB# CSV-COMMA
      SA-I @ C-IMAGE-DECODE-FAIL CAM@ RB# CSV-COMMA
      SA-I @ C-LUMA-FRAMES CAM@ RB# CSV-COMMA
      SA-I @ C-PIXELS CAM@ RB# CSV-COMMA
      SA-I @ MEAN-LUMA RB# CSV-COMMA
      SA-I @ C-PIXELS CAM@ SA-I @ 50 HIST-PCT CSV-OPT-N CSV-COMMA
      SA-I @ C-PIXELS CAM@ SA-I @ 5 HIST-PCT CSV-OPT-N CSV-COMMA
      SA-I @ C-PIXELS CAM@ SA-I @ 95 HIST-PCT CSV-OPT-N CSV-COMMA
      SA-I @ C-PIXELS CAM@ 0 > if SA-I @ 95 HIST-PCT SA-I @ 5 HIST-PCT - RB# then CSV-COMMA
      SA-I @ C-SAT-DARK CAM@ SA-I @ C-PIXELS CAM@ PCT3 CSV-COMMA
      SA-I @ C-SAT-BRIGHT CAM@ SA-I @ C-PIXELS CAM@ PCT3 RB-NL
      SA-I @ 1+ SA-I !
   repeat
   RB$ ;

: EXPOSURE-MD ( -- ptr u8 n )
   RB-RESET
   s" # Exposure Adaptation Metrics" RB+ RB-NL RB-NL
   s" schema" s" odin.capture.v1" MD-S
   s" frame records" SA-FRAME-N @ MD-N RB-NL
   s" | camera | frames | drop events | dropped frames | exposure samples | exposure min/max/mean us | gain samples | gain min/max/mean | auto true/false/unknown | image frames | mean luminance | contrast p95-p05 | bright sat | dark sat |" RB+ RB-NL
   s" | --- | ---: | ---: | ---: | ---: | --- | ---: | --- | --- | ---: | ---: | ---: | ---: | ---: |" RB+ RB-NL
   0 SA-I !
   begin SA-I @ SA-CAM-N @ < while
      LBAR SA-I @ LNA$ RB+ BAR SA-I @ C-FRAMES CAM@ RB# BAR SA-I @ C-DROP-FLAGS CAM@ RB# BAR SA-I @ C-FRAMES-DROPPED CAM@ RB# BAR
      SA-I @ C-EXP-BASE 0 STAT@ RB# BAR
      SA-I @ C-EXP-BASE 0 STAT@ 0= if s" n/a" RB+ else SA-I @ C-EXP-BASE 1 STAT@ RB# 47 RB-C SA-I @ C-EXP-BASE 2 STAT@ RB# 47 RB-C SA-I @ C-EXP-BASE STAT-MEAN RB# then BAR
      SA-I @ C-GAIN-BASE 0 STAT@ RB# BAR
      SA-I @ C-GAIN-BASE 0 STAT@ 0= if s" n/a" RB+ else SA-I @ C-GAIN-BASE 1 STAT@ 1 RB-FIXED3 47 RB-C SA-I @ C-GAIN-BASE 2 STAT@ 1 RB-FIXED3 47 RB-C SA-I @ C-GAIN-BASE STAT-MEAN 1 RB-FIXED3 then BAR
      SA-I @ C-AUTO-TRUE CAM@ RB# 47 RB-C SA-I @ C-AUTO-FALSE CAM@ RB# 47 RB-C SA-I @ C-AUTO-UNKNOWN CAM@ RB# BAR
      SA-I @ C-LUMA-FRAMES CAM@ RB# BAR SA-I @ MEAN-LUMA RB# BAR
      SA-I @ C-PIXELS CAM@ 0= if 0 else SA-I @ 95 HIST-PCT SA-I @ 5 HIST-PCT - then RB# BAR
      SA-I @ C-SAT-BRIGHT CAM@ SA-I @ C-PIXELS CAM@ PCT3 BAR
      SA-I @ C-SAT-DARK CAM@ SA-I @ C-PIXELS CAM@ PCT3 RBAR RB-NL
      SA-I @ 1+ SA-I !
   repeat
   RB$ ;

: LOWLIGHT-CSV ( -- ptr u8 n )
   RB-RESET
   s" serial,logical_name,frames,dropped_event_flags,timestamp_regressions,exposure_samples,exposure_min_us,exposure_max_us,exposure_mean_us,image_path_records,image_missing,image_decode_failures,image_frames,mean_luminance,median_luminance,p05_luminance,p95_luminance,noise_stddev_mean,snr_proxy,edge_density_pct_mean" RB+ RB-NL
   0 SA-I !
   begin SA-I @ SA-CAM-N @ < while
      SA-I @ SER$ RB+ CSV-COMMA SA-I @ LNA$ RB+ CSV-COMMA
      SA-I @ C-FRAMES CAM@ RB# CSV-COMMA SA-I @ C-DROP-FLAGS CAM@ RB# CSV-COMMA SA-I @ C-REGRESSIONS CAM@ RB# CSV-COMMA
      SA-I @ C-EXP-BASE 0 STAT@ RB# CSV-COMMA
      SA-I @ C-EXP-BASE 0 STAT@ SA-I @ C-EXP-BASE 1 STAT@ CSV-OPT-N CSV-COMMA
      SA-I @ C-EXP-BASE 0 STAT@ SA-I @ C-EXP-BASE 2 STAT@ CSV-OPT-N CSV-COMMA
      SA-I @ C-EXP-BASE 0 STAT@ SA-I @ C-EXP-BASE STAT-MEAN CSV-OPT-N CSV-COMMA
      SA-I @ C-IMAGE-PATHS CAM@ RB# CSV-COMMA SA-I @ C-IMAGE-MISSING CAM@ RB# CSV-COMMA SA-I @ C-IMAGE-DECODE-FAIL CAM@ RB# CSV-COMMA
      SA-I @ C-LUMA-FRAMES CAM@ RB# CSV-COMMA SA-I @ MEAN-LUMA RB# CSV-COMMA
      SA-I @ C-PIXELS CAM@ SA-I @ 50 HIST-PCT CSV-OPT-N CSV-COMMA
      SA-I @ C-PIXELS CAM@ SA-I @ 5 HIST-PCT CSV-OPT-N CSV-COMMA
      SA-I @ C-PIXELS CAM@ SA-I @ 95 HIST-PCT CSV-OPT-N CSV-COMMA
      SA-I @ C-NOISE-SAMP CAM@ 0 > if SA-I @ C-NOISE-SUM-MILLI CAM@ SA-I @ C-NOISE-SAMP CAM@ / RB-MILLI3 then CSV-COMMA
      SA-I @ C-NOISE-SAMP CAM@ 0 > SA-I @ C-NOISE-SUM-MILLI CAM@ 0 > and if
         SA-I @ MEAN-LUMA 1000 * SA-I @ C-NOISE-SUM-MILLI CAM@ SA-I @ C-NOISE-SAMP CAM@ / / RB-MILLI3
      then CSV-COMMA
      SA-I @ C-EDGE-SAMP CAM@ 0 > if SA-I @ C-EDGE-SUM-MILLI CAM@ SA-I @ C-EDGE-SAMP CAM@ / RB-MILLI3 then RB-NL
      SA-I @ 1+ SA-I !
   repeat
   RB$ ;

: LOWLIGHT-MD ( -- ptr u8 n )
   RB-RESET
   s" # Low-Light Metrics" RB+ RB-NL RB-NL
   s" schema" s" odin.capture.v1" MD-S
   s" frame records" SA-FRAME-N @ MD-N RB-NL
   s" | camera | frames | drops | exposure samples | images | mean luminance | p05/p50/p95 | noise stddev mean | SNR proxy | edge density mean % |" RB+ RB-NL
   s" | --- | ---: | ---: | ---: | ---: | ---: | --- | ---: | ---: | ---: |" RB+ RB-NL
   0 SA-I !
   begin SA-I @ SA-CAM-N @ < while
      LBAR SA-I @ LNA$ RB+ BAR SA-I @ C-FRAMES CAM@ RB# BAR SA-I @ C-DROP-FLAGS CAM@ RB# BAR SA-I @ C-EXP-BASE 0 STAT@ RB# BAR
      SA-I @ C-LUMA-FRAMES CAM@ RB# BAR SA-I @ MEAN-LUMA RB# BAR
      SA-I @ 5 HIST-PCT RB# 47 RB-C SA-I @ 50 HIST-PCT RB# 47 RB-C SA-I @ 95 HIST-PCT RB# BAR
      SA-I @ C-NOISE-SAMP CAM@ 0 > if SA-I @ C-NOISE-SUM-MILLI CAM@ SA-I @ C-NOISE-SAMP CAM@ / RB-MILLI3 else s" n/a" RB+ then BAR
      SA-I @ C-NOISE-SAMP CAM@ 0 > SA-I @ C-NOISE-SUM-MILLI CAM@ 0 > and if
         SA-I @ MEAN-LUMA 1000 * SA-I @ C-NOISE-SUM-MILLI CAM@ SA-I @ C-NOISE-SAMP CAM@ / / RB-MILLI3
      else s" n/a" RB+ then BAR
      SA-I @ C-EDGE-SAMP CAM@ 0 > if SA-I @ C-EDGE-SUM-MILLI CAM@ SA-I @ C-EDGE-SAMP CAM@ / RB-MILLI3 else s" n/a" RB+ then RBAR RB-NL
      SA-I @ 1+ SA-I !
   repeat
   RB$ ;

: MOTION-CSV ( -- ptr u8 n )
   RB-RESET
   s" serial,logical_name,frames,dropped_event_flags,timestamp_regressions,sdk_period_samples,sdk_period_min_ms,sdk_period_max_ms,sdk_period_mean_ms,image_path_records,image_missing,image_decode_failures,image_frames,laplacian_variance_min,laplacian_variance_max,laplacian_variance_mean,edge_density_pct_mean,frame_delta_mean,frame_delta_max" RB+ RB-NL
   0 SA-I !
   begin SA-I @ SA-CAM-N @ < while
      SA-I @ SER$ RB+ CSV-COMMA SA-I @ LNA$ RB+ CSV-COMMA
      SA-I @ C-FRAMES CAM@ RB# CSV-COMMA SA-I @ C-DROP-FLAGS CAM@ RB# CSV-COMMA SA-I @ C-REGRESSIONS CAM@ RB# CSV-COMMA
      SA-I @ C-PER-SAMP CAM@ RB# CSV-COMMA
      SA-I @ C-PER-SAMP CAM@ 0 > if SA-I @ C-PER-MIN-US CAM@ RB-MILLI3 then CSV-COMMA
      SA-I @ C-PER-SAMP CAM@ 0 > if SA-I @ C-PER-MAX-US CAM@ RB-MILLI3 then CSV-COMMA
      SA-I @ C-PER-SAMP CAM@ 0 > if SA-I @ C-PER-SUM-US CAM@ SA-I @ C-PER-SAMP CAM@ / RB-MILLI3 then CSV-COMMA
      SA-I @ C-IMAGE-PATHS CAM@ RB# CSV-COMMA SA-I @ C-IMAGE-MISSING CAM@ RB# CSV-COMMA SA-I @ C-IMAGE-DECODE-FAIL CAM@ RB# CSV-COMMA SA-I @ C-LUMA-FRAMES CAM@ RB# CSV-COMMA
      SA-I @ C-LAP-SAMP CAM@ 0 > if SA-I @ C-LAP-MIN CAM@ 1 RB-FIXED3 then CSV-COMMA
      SA-I @ C-LAP-SAMP CAM@ 0 > if SA-I @ C-LAP-MAX CAM@ 1 RB-FIXED3 then CSV-COMMA
      SA-I @ C-LAP-SAMP CAM@ 0 > if SA-I @ C-LAP-SUM CAM@ SA-I @ C-LAP-SAMP CAM@ / 1 RB-FIXED3 then CSV-COMMA
      SA-I @ C-EDGE-SAMP CAM@ 0 > if SA-I @ C-EDGE-SUM-MILLI CAM@ SA-I @ C-EDGE-SAMP CAM@ / RB-MILLI3 then CSV-COMMA
      SA-I @ C-DELTA-SAMP CAM@ 0 > if SA-I @ C-DELTA-SUM-MILLI CAM@ SA-I @ C-DELTA-SAMP CAM@ / RB-MILLI3 then CSV-COMMA
      SA-I @ C-DELTA-SAMP CAM@ 0 > if SA-I @ C-DELTA-MAX-MILLI CAM@ RB-MILLI3 then RB-NL
      SA-I @ 1+ SA-I !
   repeat
   RB$ ;

: MOTION-MD ( -- ptr u8 n )
   RB-RESET
   s" # Motion Blur Metrics" RB+ RB-NL RB-NL
   s" schema" s" odin.capture.v1" MD-S
   s" frame records" SA-FRAME-N @ MD-N RB-NL
   s" | camera | frames | drop events | regressions | SDK period mean ms | images | Laplacian variance mean | edge density mean % | frame delta mean/max |" RB+ RB-NL
   s" | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | --- |" RB+ RB-NL
   0 SA-I !
   begin SA-I @ SA-CAM-N @ < while
      LBAR SA-I @ LNA$ RB+ BAR SA-I @ C-FRAMES CAM@ RB# BAR SA-I @ C-DROP-FLAGS CAM@ RB# BAR SA-I @ C-REGRESSIONS CAM@ RB# BAR
      SA-I @ C-PER-SAMP CAM@ 0 > if SA-I @ C-PER-SUM-US CAM@ SA-I @ C-PER-SAMP CAM@ / RB-MILLI3 else s" n/a" RB+ then BAR
      SA-I @ C-LUMA-FRAMES CAM@ RB# BAR
      SA-I @ C-LAP-SAMP CAM@ 0 > if SA-I @ C-LAP-SUM CAM@ SA-I @ C-LAP-SAMP CAM@ / 1 RB-FIXED3 else s" n/a" RB+ then BAR
      SA-I @ C-EDGE-SAMP CAM@ 0 > if SA-I @ C-EDGE-SUM-MILLI CAM@ SA-I @ C-EDGE-SAMP CAM@ / RB-MILLI3 else s" n/a" RB+ then BAR
      SA-I @ C-DELTA-SAMP CAM@ 0 > if SA-I @ C-DELTA-SUM-MILLI CAM@ SA-I @ C-DELTA-SAMP CAM@ / RB-MILLI3 47 RB-C SA-I @ C-DELTA-MAX-MILLI CAM@ RB-MILLI3 else s" n/a" RB+ then RBAR RB-NL
      SA-I @ 1+ SA-I !
   repeat
   RB$ ;

: SYNC-READINESS-CSV ( -- ptr u8 n )
   RB-RESET
   s" check,result,camera_count,frame_index_sets,complete_frame_index_sets,sdk_skew_p99_ns" RB+ RB-NL
   s" sync," RB+ CAMSYNC:SR-RESULT@ if s" pass" else s" fail" then RB+ CSV-COMMA CAMSYNC:TM-COUNT RB# CSV-COMMA CAMSYNC:TX-FSETS@ RB# CSV-COMMA CAMSYNC:TX-COMPLETE@ RB# CSV-COMMA CAMSYNC:TX-SP99@ RB# RB-NL
   RB$ ;

: SYNC-READINESS-MD ( -- ptr u8 n )
   RB-RESET
   s" # Sync Readiness" RB+ RB-NL RB-NL
   s" result" CAMSYNC:SR-RESULT@ if s" pass" else s" fail" then MD-S
   s" camera count" CAMSYNC:TM-COUNT MD-N
   s" frame index sets" CAMSYNC:TX-FSETS@ MD-N
   s" complete frame index sets" CAMSYNC:TX-COMPLETE@ MD-N
   s" sdk skew p99 ns" CAMSYNC:TX-SP99@ MD-N
   RB$ ;

: PAIRING-READINESS-CSV ( -- ptr u8 n )
   RB-RESET
   s" check,result,camera_count,sdk_skew_p99_ns" RB+ RB-NL
   s" timestamp_pairing," RB+ CAMSYNC:TPR-RESULT@ if s" pass" else s" fail" then RB+ CSV-COMMA CAMSYNC:TM-COUNT RB# CSV-COMMA CAMSYNC:TX-SP99@ RB# RB-NL
   RB$ ;

: PAIRING-READINESS-MD ( -- ptr u8 n )
   RB-RESET
   s" # Timestamp Pairing Readiness" RB+ RB-NL RB-NL
   s" result" CAMSYNC:TPR-RESULT@ if s" pass" else s" fail" then MD-S
   s" camera count" CAMSYNC:TM-COUNT MD-N
   s" sdk skew p99 ns" CAMSYNC:TX-SP99@ MD-N
   RB$ ;

public

: ANALYZE-EXPOSURE ( ptr u8 n ptr u8 n ptr u8 n -- n )
   {: input:ptr inputu:n imroot:ptr imrootu:n out:ptr outu:n :}
   out outu MAKE-DIRS
   input inputu imroot imrootu 1 COLLATE
   out outu s" metrics.csv" EXPOSURE-CSV WRITE-FILE
   out outu s" summary.md" EXPOSURE-MD WRITE-FILE
   0 ;

: ANALYZE-LOW-LIGHT ( ptr u8 n ptr u8 n ptr u8 n -- n )
   {: input:ptr inputu:n imroot:ptr imrootu:n out:ptr outu:n :}
   out outu MAKE-DIRS
   input inputu imroot imrootu 1 COLLATE
   out outu s" metrics.csv" LOWLIGHT-CSV WRITE-FILE
   out outu s" summary.md" LOWLIGHT-MD WRITE-FILE
   0 ;

: ANALYZE-MOTION-BLUR ( ptr u8 n ptr u8 n ptr u8 n -- n )
   {: input:ptr inputu:n imroot:ptr imrootu:n out:ptr outu:n :}
   out outu MAKE-DIRS
   input inputu imroot imrootu 1 COLLATE
   out outu s" metrics.csv" MOTION-CSV WRITE-FILE
   out outu s" summary.md" MOTION-MD WRITE-FILE
   0 ;

: ANALYZE-SYNC ( ptr u8 n ptr u8 n n n -- n )
   {: input:ptr inputu:n out:ptr outu:n require-sync:n require-pairing:n :}
   out outu MAKE-DIRS
   input inputu s" ." 0 COLLATE
   out outu s" frame_sync.csv" CAMSYNC:TS-CSV WRITE-FILE
   out outu s" frame_sync.json" CAMSYNC:TS-JSON WRITE-FILE
   out outu s" cross_camera_sync.csv" CAMSYNC:TX-CSV WRITE-FILE
   out outu s" cross_camera_sync.json" CAMSYNC:TX-JSON WRITE-FILE
   out outu s" sync_readiness.csv" SYNC-READINESS-CSV WRITE-FILE
   out outu s" sync_readiness.md" SYNC-READINESS-MD WRITE-FILE
   out outu s" timestamp_pairing_readiness.csv" PAIRING-READINESS-CSV WRITE-FILE
   out outu s" timestamp_pairing_readiness.md" PAIRING-READINESS-MD WRITE-FILE
   require-sync 0 <> CAMSYNC:SR-RESULT@ 0= and if 1 exit then
   require-pairing 0 <> CAMSYNC:TPR-RESULT@ 0= and if 1 exit then
   0 ;

end-package
