\ cameraone-latency.f - Habu CameraOne image-time IMU latency event pipeline.
\
\ Converts odin.capture.v1 IMAGE sensor rows to odin.external_imu.v1, extracts
\ camera luminance-rise events from saved P5/P6 frames, extracts IMU norm peaks,
\ and renders latency-calibration artifacts without invoking Odin/Zig commands.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/render.f
require lib/report.f
require lib/float.f
require lib/fmt.f
require lib/sort.f
require tools/json.f
require tools/json-file.f
require odin/capture-schema.f
require odin/capture-schema-json.f
require odin/netpbm.f
require odin/latency-calibration.f

package COLAT
private

8192 constant CL-MAX-SAMPLES
512 constant CL-MAX-EVENTS
256 constant CL-TEXT-CAP
$200000 constant CL-TEXT-BUF-CAP
$300000 constant CL-IMG-CAP
1024 constant CL-PATH-CAP
1000000 constant CL-NS-PER-MS

-8400 constant E-CL-SCHEMA
-8401 constant E-CL-CAPACITY
-8402 constant E-CL-EMPTY
-8403 constant E-CL-PATH
-8404 constant E-CL-JSON

create CL-TEXT-BUF CL-TEXT-BUF-CAP allot
create CL-IMG-BUF CL-IMG-CAP allot
create CL-IMG-PATH CL-PATH-CAP allot
create CL-OUT-PATH CL-PATH-CAP allot
create CL-P0 CL-PATH-CAP allot
create CL-P1 CL-PATH-CAP allot
create CL-P2 CL-PATH-CAP allot
create CL-P3 CL-PATH-CAP allot
create CL-LOGICAL CL-TEXT-CAP allot
create CL-SERIAL CL-TEXT-CAP allot
create CL-SOURCE CL-TEXT-CAP allot

create S-TS CL-MAX-SAMPLES cells allot
create S-FRAME CL-MAX-SAMPLES cells allot
create S-AX CL-MAX-SAMPLES cells allot
create S-AY CL-MAX-SAMPLES cells allot
create S-AZ CL-MAX-SAMPLES cells allot
create S-GX CL-MAX-SAMPLES cells allot
create S-GY CL-MAX-SAMPLES cells allot
create S-GZ CL-MAX-SAMPLES cells allot

create CE-TS CL-MAX-EVENTS cells allot
create CE-FRAME CL-MAX-EVENTS cells allot
create CE-DELTA CL-MAX-EVENTS cells allot
create IE-TS CL-MAX-EVENTS cells allot
create IE-SAMPLE CL-MAX-EVENTS cells allot
create IE-VALUE CL-MAX-EVENTS cells allot
create LAT CL-MAX-EVENTS cells allot
create RESID CL-MAX-EVENTS cells allot

variable CL-SAMPLE-N
variable CL-SENSOR-SEEN
variable CL-SENSOR-SKIPPED
variable CL-SENSOR-MISSING
variable CL-SENSOR-DUP-REC
variable CL-SENSOR-STALE
variable CL-SENSOR-DROPPED
variable CL-SENSOR-DUP-TS
variable CL-SENSOR-REG
variable CL-LAST-SENSOR-TS
variable CL-HAVE-LAST-SENSOR
variable CL-LOGICAL-U
variable CL-SERIAL-U
variable CL-SOURCE-U

variable CL-FRAME-N
variable CL-IMAGE-N
variable CL-IMAGE-MISSING
variable CL-IMAGE-DECODE-FAIL
variable CL-CAM-EVENT-N
variable CL-IMU-EVENT-N
variable CL-HAVE-PREV-LUMA
variable CL-PREV-LUMA
variable CL-PREV-TS
variable CL-PREV-FRAME
variable CL-LAST-CAM-EVENT-TS
variable CL-HAVE-LAST-CAM-EVENT
variable CL-LAST-IMU-EVENT-TS
variable CL-HAVE-LAST-IMU-EVENT

variable CL-THRESHOLD-DELTA
variable CL-CAMERA-MIN-SPACING-NS
variable CL-IMU-MIN-SPACING-NS
variable CL-IMU-THRESHOLD-MILLI
variable CL-MAX-JITTER-NS
variable CL-MATCH-BY-ORDER
variable CL-REQUIRE-READY
variable CL-OFFSET-NS
variable CL-OFFSET-PROVIDED

variable CL-I
variable CL-J
variable CL-SUM
variable CL-MATCH-N
variable CL-MISSING-CAM
variable CL-MISSING-IMU
variable CL-SQ-X
variable CL-SQ-R
variable CL-SQ-N

: CL-TRUE ( -- bool ) 0 0= ;
: CL-FALSE ( -- bool ) CL-TRUE 0= ;
: ABS-I ( n -- n ) dup 0 < if negate then ;
: MIN2 ( n n -- n ) {: a:n b:n :} a b < if a else b then ;

: ZERO-CELLS ( ptr a n -- )
   {: p:ptr n:n :}
   0 begin dup n < while
      0 p over cells + !
      1+
   repeat drop ;

: CL-COPY ( ptr u8 n ptr u8 n ptr a -- )
   {: a:ptr u:n dst:ptr cap:n lenp:ptr :}
   u cap >= if E-CL-CAPACITY throw then
   a dst u BYTE-COPY
   u lenp ! ;

: LOGICAL$ ( -- ptr u8 n ) CL-LOGICAL CL-LOGICAL-U @ ;
: SERIAL$ ( -- ptr u8 n ) CL-SERIAL CL-SERIAL-U @ ;
: SOURCE$ ( -- ptr u8 n ) CL-SOURCE CL-SOURCE-U @ ;
: SET-LOGICAL ( ptr u8 n -- ) CL-LOGICAL CL-TEXT-CAP CL-LOGICAL-U CL-COPY ;
: SET-SERIAL ( ptr u8 n -- ) CL-SERIAL CL-TEXT-CAP CL-SERIAL-U CL-COPY ;
: SET-SOURCE ( ptr u8 n -- ) CL-SOURCE CL-TEXT-CAP CL-SOURCE-U CL-COPY ;

: S>MSNS ( ptr u8 n -- n )
   STR>FLOAT 0= if drop E-CL-JSON throw then
   CL-NS-PER-MS s>f f* f>s ;

: DEFAULTS ( -- )
   50 CL-THRESHOLD-DELTA !
   0 CL-CAMERA-MIN-SPACING-NS !
   25 CL-NS-PER-MS * CL-IMU-MIN-SPACING-NS !
   10500 CL-IMU-THRESHOLD-MILLI !
   500000 CL-MAX-JITTER-NS !
   1 CL-MATCH-BY-ORDER !
   1 CL-REQUIRE-READY !
   0 CL-OFFSET-NS !
   0 CL-OFFSET-PROVIDED !
   0 CL-SOURCE-U !
   s" unknown" SET-SOURCE ;

: RESET-DATA ( -- )
   0 CL-SAMPLE-N !
   0 CL-SENSOR-SEEN !
   0 CL-SENSOR-SKIPPED !
   0 CL-SENSOR-MISSING !
   0 CL-SENSOR-DUP-REC !
   0 CL-SENSOR-STALE !
   0 CL-SENSOR-DROPPED !
   0 CL-SENSOR-DUP-TS !
   0 CL-SENSOR-REG !
   0 CL-LAST-SENSOR-TS !
   0 CL-HAVE-LAST-SENSOR !
   0 CL-FRAME-N !
   0 CL-IMAGE-N !
   0 CL-IMAGE-MISSING !
   0 CL-IMAGE-DECODE-FAIL !
   0 CL-CAM-EVENT-N !
   0 CL-IMU-EVENT-N !
   0 CL-HAVE-PREV-LUMA !
   0 CL-PREV-LUMA !
   0 CL-PREV-TS !
   0 CL-PREV-FRAME !
   0 CL-HAVE-LAST-CAM-EVENT !
   0 CL-HAVE-LAST-IMU-EVENT !
   S-TS CL-MAX-SAMPLES ZERO-CELLS
   S-FRAME CL-MAX-SAMPLES ZERO-CELLS
   CE-TS CL-MAX-EVENTS ZERO-CELLS
   IE-TS CL-MAX-EVENTS ZERO-CELLS
   LAT CL-MAX-EVENTS ZERO-CELLS
   RESID CL-MAX-EVENTS ZERO-CELLS ;

: JNODE ( n ptr u8 n -- n ) JSON-GET ;
: JSTR$ ( n ptr u8 n -- ptr u8 n ) JNODE JSON-STRING$ ;
: JINT ( n ptr u8 n -- n ) JNODE JSON-NUMBER$ STR>NUMBER? drop ;
: JBOOL ( n ptr u8 n -- n ) JNODE JSON-BOOL@ if 1 else 0 then ;
: JOPT-STR ( n ptr u8 n -- ptr u8 n bool )
   JNODE {: v:n :}
   v -1 = if s" " CL-FALSE exit then
   v JSON-KIND J-NULL = if s" " CL-FALSE exit then
   v JSON-STRING$ CL-TRUE ;

: JOPT-INT ( n ptr u8 n -- n bool )
   JNODE {: v:n :}
   v -1 = if 0 CL-FALSE exit then
   v JSON-KIND J-NULL = if 0 CL-FALSE exit then
   v JSON-NUMBER$ STR>NUMBER? ;

: JARR-F ( n n -- r )
   {: arr:n idx:n :}
   arr idx JSON-ARR@ JSON-NUMBER$ STR>FLOAT drop ;

: F>MICRO ( r -- n ) 1000000.0 f* f>s ;
: JARR-MICRO ( n n -- n ) JARR-F F>MICRO ;

: SELECTOR-MATCH? ( ptr u8 n ptr u8 n -- bool )
   {: ser:ptr seru:n log:ptr logu:n :}
   CL-SERIAL-U @ 0 > if ser seru SERIAL$ STR= 0= if CL-FALSE exit then then
   CL-LOGICAL-U @ 0 > if log logu LOGICAL$ STR= 0= if CL-FALSE exit then then
   CL-TRUE ;

: ABS-PATH? ( ptr u8 n -- bool ) {: a:ptr u:n :} u 0 > if a c@ 47 = else CL-FALSE then ;
: JOIN-IMAGE$ ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: root:ptr rootu:n a:ptr u:n :}
   a u ABS-PATH? if
      u CL-PATH-CAP >= if E-CL-PATH throw then
      a CL-IMG-PATH u BYTE-COPY
      CL-IMG-PATH u exit
   then
   root rootu a u CL-IMG-PATH JOIN-PATH
   CL-IMG-PATH swap ;

: OUT-FILE$ ( ptr u8 n ptr u8 n -- ptr u8 n )
   CL-OUT-PATH JOIN-PATH
   CL-OUT-PATH swap ;

: P0-PATH$ ( ptr u8 n ptr u8 n -- ptr u8 n ) CL-P0 JOIN-PATH CL-P0 swap ;
: P1-PATH$ ( ptr u8 n ptr u8 n -- ptr u8 n ) CL-P1 JOIN-PATH CL-P1 swap ;
: CAMERAONE-DIR$ ( ptr u8 n -- ptr u8 n ) s" cameraone_imu" P0-PATH$ ;
: CAMERA-EVENTS-DIR$ ( ptr u8 n -- ptr u8 n ) s" latency/camera_events" P0-PATH$ ;
: CAMERAONE-IMU-NDJSON$ ( ptr u8 n -- ptr u8 n ) s" cameraone_imu/external_imu.ndjson" P0-PATH$ ;
: IMU-EVENTS-DIR$ ( ptr u8 n -- ptr u8 n ) s" latency/imu_events" P1-PATH$ ;
: LATENCY-DIR$ ( ptr u8 n -- ptr u8 n ) s" latency/latency_calibration" P0-PATH$ ;

: ADD-SAMPLE ( n n n n n n n n -- )
   {: ts:n frame:n ax:n ay:n az:n gx:n gy:n gz:n :}
   CL-SAMPLE-N @ CL-MAX-SAMPLES >= if E-CL-CAPACITY throw then
   CL-SAMPLE-N @ {: ix:n :}
   ts S-TS ix cells + !
   frame S-FRAME ix cells + !
   ax S-AX ix cells + !
   ay S-AY ix cells + !
   az S-AZ ix cells + !
   gx S-GX ix cells + !
   gy S-GY ix cells + !
   gz S-GZ ix cells + !
   ix 1+ CL-SAMPLE-N ! ;

: HANDLE-SENSOR ( n -- )
   {: root:n :}
   root s" serial" JSTR$ root s" logical_name" JSTR$ SELECTOR-MATCH? 0= if
      1 CL-SENSOR-SKIPPED +!
      exit
   then
   1 CL-SENSOR-SEEN +!
   CL-SERIAL-U @ 0= if root s" serial" JSTR$ SET-SERIAL then
   CL-LOGICAL-U @ 0= if root s" logical_name" JSTR$ SET-LOGICAL then
   root s" missing" JBOOL 0 <> if
      1 CL-SENSOR-MISSING +!
      1 CL-SENSOR-DROPPED +!
      exit
   then
   root s" sensor_timestamp_ns" JOPT-INT 0= if drop 1 CL-SENSOR-MISSING +! 1 CL-SENSOR-DROPPED +! exit then
   {: ts:n :}
   root s" values" JNODE {: arr:n :}
   arr JSON-KIND J-ARR <> if E-CL-JSON throw then
   arr JSON-COUNT 6 < if E-CL-JSON throw then
   root s" duplicate" JBOOL 0 <> {: duprec:bool :}
   root s" stale" JBOOL 0 <> {: stale:bool :}
   CL-HAVE-LAST-SENSOR @ 0 <> if
      ts CL-LAST-SENSOR-TS @ = if 1 CL-SENSOR-DUP-TS +! 1 CL-SENSOR-DROPPED +! exit then
      ts CL-LAST-SENSOR-TS @ < if 1 CL-SENSOR-REG +! 1 CL-SENSOR-DROPPED +! exit then
   then
   duprec if 1 CL-SENSOR-DUP-REC +! 1 CL-SENSOR-DROPPED +! exit then
   stale if 1 CL-SENSOR-STALE +! 1 CL-SENSOR-DROPPED +! exit then
   ts CL-LAST-SENSOR-TS !
   1 CL-HAVE-LAST-SENSOR !
   ts root s" image_frame_index" JINT
   arr 0 JARR-MICRO arr 1 JARR-MICRO arr 2 JARR-MICRO
   arr 3 JARR-MICRO arr 4 JARR-MICRO arr 5 JARR-MICRO
   ADD-SAMPLE ;

: MEAN-LUMA ( ptr u8 n -- n )
   {: pix:ptr u:n :}
   u 0= if 0 exit then
   0 CL-SUM !
   0 CL-I !
   begin CL-I @ u < while
      CL-SUM @ pix CL-I @ + c@ + CL-SUM !
      CL-I @ 1+ CL-I !
   repeat
   CL-SUM @ u / ;

: ADD-CAMERA-EVENT ( n n n -- )
   {: ts:n frame:n delta:n :}
   CL-CAM-EVENT-N @ CL-MAX-EVENTS >= if E-CL-CAPACITY throw then
   CL-CAM-EVENT-N @ {: ix:n :}
   ts CE-TS ix cells + !
   frame CE-FRAME ix cells + !
   delta CE-DELTA ix cells + !
   ix 1+ CL-CAM-EVENT-N ! ;

: HANDLE-FRAME ( n ptr u8 n -- )
   {: root:n imgroot:ptr imgrootu:n :}
   root s" serial" JSTR$ root s" logical_name" JSTR$ SELECTOR-MATCH? 0= if exit then
   CL-SERIAL-U @ 0= if root s" serial" JSTR$ SET-SERIAL then
   CL-LOGICAL-U @ 0= if root s" logical_name" JSTR$ SET-LOGICAL then
   1 CL-FRAME-N +!
   root s" image_path" JOPT-STR 0= if 2drop 1 CL-IMAGE-MISSING +! exit then
   imgroot imgrootu 2swap JOIN-IMAGE$ {: path:ptr pathu:n :}
   path pathu CL-IMG-BUF CL-IMG-CAP READ-ALL {: nread:n :}
   CL-IMG-BUF nread NETPBM:DECODE {: luma:ptr lu:n ok:bool :}
   ok 0= if 1 CL-IMAGE-DECODE-FAIL +! exit then
   1 CL-IMAGE-N +!
   luma lu MEAN-LUMA {: mean:n :}
   root s" sdk_image_timestamp_ns" JINT {: ts:n :}
   root s" frame_index" JINT {: frame:n :}
   CL-HAVE-PREV-LUMA @ 0= if
      mean CL-PREV-LUMA !
      ts CL-PREV-TS !
      frame CL-PREV-FRAME !
      1 CL-HAVE-PREV-LUMA !
      exit
   then
   mean CL-PREV-LUMA @ - {: delta:n :}
   delta CL-THRESHOLD-DELTA @ >= if
      CL-HAVE-LAST-CAM-EVENT @ 0= if
         ts frame delta ADD-CAMERA-EVENT
         ts CL-LAST-CAM-EVENT-TS !
         1 CL-HAVE-LAST-CAM-EVENT !
      else
         ts CL-LAST-CAM-EVENT-TS @ - CL-CAMERA-MIN-SPACING-NS @ >= if
            ts frame delta ADD-CAMERA-EVENT
            ts CL-LAST-CAM-EVENT-TS !
         then
      then
   then
   mean CL-PREV-LUMA !
   ts CL-PREV-TS !
   frame CL-PREV-FRAME ! ;

: LOAD-CAPTURE ( ptr u8 n ptr u8 n -- )
   {: input:ptr inputu:n imgroot:ptr imgrootu:n :}
   RESET-DATA
   input inputu JSONLF-OPEN
   begin JSONLF-NEXT-LINE while
      2dup SCHEMA:VALIDATE-LINE {: rt:n v:n :}
      v SCHEMA:V-OK <> if E-CL-SCHEMA throw then
      JSON-PARSE {: root:n :}
      root s" type" JSTR$ s" sensor" STR= if root HANDLE-SENSOR else
         root s" type" JSTR$ s" frame" STR= if root imgroot imgrootu HANDLE-FRAME then
      then
   repeat 2drop
   CL-SAMPLE-N @ 0= if E-CL-EMPTY throw then ;

: RB-Q ( ptr u8 n -- ) 34 RB-C RB+ 34 RB-C ;
: RB-K ( ptr u8 n -- ) RB-Q 58 RB-C ;
: RB-COMMA ( -- ) 44 RB-C ;
: RB-BOOL ( bool -- ) if s" true" else s" false" then RB+ ;
: RB-MICRO6 ( n -- )
   dup 0 < if 45 RB-C negate then
   dup 1000000 / RB#
   46 RB-C
   1000000 mod {: frac:n :}
   frac 100000 / 48 + RB-C
   frac 100000 mod 10000 / 48 + RB-C
   frac 10000 mod 1000 / 48 + RB-C
   frac 1000 mod 100 / 48 + RB-C
   frac 100 mod 10 / 48 + RB-C
   frac 10 mod 48 + RB-C ;
: CSV-F6 ( ptr a n -- ) cells + @ RB-MICRO6 ;

\ Tiny render DSL for Odin event JSON: callers name fields instead of spelling
\ punctuation and zero-padding by hand.
: JOBJ{ ( -- ) 123 RB-C ;
: JOBJ} ( -- ) 125 RB-C RB-NL ;
: JK, ( ptr u8 n -- ) RB-K ;
: JV-S, ( ptr u8 n -- ) RB-Q RB-COMMA ;
: JV-S ( ptr u8 n -- ) RB-Q ;
: JV-N, ( n -- ) RB# RB-COMMA ;
: JV-N ( n -- ) RB# ;
: JV-R1 ( -- ) s" 1.0" RB+ ;
: RB-U6 ( n -- )
   {: x:n :}
   x 999999 > if x RB# exit then
   x 100000 / 48 + RB-C
   x 100000 mod 10000 / 48 + RB-C
   x 10000 mod 1000 / 48 + RB-C
   x 1000 mod 100 / 48 + RB-C
   x 100 mod 10 / 48 + RB-C
   x 10 mod 48 + RB-C ;
: EVENT-ID ( ptr u8 n n -- )
   {: pre:ptr preu:n idx:n :}
   34 RB-C pre preu RB+ 45 RB-C idx RB-U6 34 RB-C ;
: JV-ID, ( ptr u8 n n -- ) EVENT-ID RB-COMMA ;

: WRITE-CAMERAONE-IMU ( ptr u8 n -- )
   {: out:ptr outu:n :}
   out outu MAKE-DIRS
   RB-RESET
   JOBJ{
   s" type" JK, s" schema" JV-S,
   s" schema_version" JK, s" odin.external_imu.v1" JV-S,
   s" source" JK, s" CameraOne TIME_REFERENCE::IMAGE sensor records from odin.capture.v1" JV-S,
   s" time_domain" JK, s" sdk_image_timestamp_ns" JV-S,
   s" serial" JK, SERIAL$ JV-S,
   s" logical_name" JK, LOGICAL$ JV-S
   JOBJ}
   0 CL-I !
   begin CL-I @ CL-SAMPLE-N @ < while
      JOBJ{
      s" type" JK, s" imu_sample" JV-S,
      s" schema_version" JK, s" odin.external_imu.v1" JV-S,
      s" sample_index" JK, CL-I @ JV-N,
      s" imu_timestamp_ns" JK, S-TS CL-I @ cells + @ JV-N,
      s" time_domain" JK, s" sdk_image_timestamp_ns" JV-S,
      s" frame" JK, s" sensor" JV-S,
      s" accel_m_s2" JK, 91 RB-C
      S-AX CL-I @ CSV-F6 RB-COMMA S-AY CL-I @ CSV-F6 RB-COMMA S-AZ CL-I @ CSV-F6
      93 RB-C RB-COMMA
      s" gyro_rad_s" JK, 91 RB-C
      S-GX CL-I @ CSV-F6 RB-COMMA S-GY CL-I @ CSV-F6 RB-COMMA S-GZ CL-I @ CSV-F6
      93 RB-C RB-COMMA
      s" image_frame_index" JK, S-FRAME CL-I @ cells + @ JV-N
      JOBJ}
      CL-I @ 1+ CL-I !
   repeat
   out outu s" external_imu.ndjson" OUT-FILE$ RB$ WRITE-ALL
   RB-RESET
   s" sample_index,image_frame_index,imu_timestamp_ns,accel_x_m_s2,accel_y_m_s2,accel_z_m_s2,gyro_x_rad_s,gyro_y_rad_s,gyro_z_rad_s" RB+ RB-NL
   0 CL-I !
   begin CL-I @ CL-SAMPLE-N @ < while
      CL-I @ RB# CM S-FRAME CL-I @ cells + @ RB# CM S-TS CL-I @ cells + @ RB# CM
      S-AX CL-I @ CSV-F6 CM S-AY CL-I @ CSV-F6 CM S-AZ CL-I @ CSV-F6 CM
      S-GX CL-I @ CSV-F6 CM S-GY CL-I @ CSV-F6 CM S-GZ CL-I @ CSV-F6 RB-NL
      CL-I @ 1+ CL-I !
   repeat
   out outu s" samples.csv" OUT-FILE$ RB$ WRITE-ALL
   RB-RESET
   s" # CameraOne Image-Time IMU" RB+ RB-NL RB-NL
   s" schema" s" odin.external_imu.v1" MD-S
   s" source schema" s" odin.capture.v1" MD-S
   s" serial" SERIAL$ MD-S
   s" logical name" LOGICAL$ MD-S
   s" time domain" s" sdk_image_timestamp_ns" MD-S
   s" frame" s" sensor" MD-S
   s" samples" CL-SAMPLE-N @ MD-N
   s" sensor records seen" CL-SENSOR-SEEN @ MD-N
   s" missing sensor records" CL-SENSOR-MISSING @ MD-N
   s" skipped sensor records" CL-SENSOR-SKIPPED @ MD-N
   s" stale sensor records" CL-SENSOR-STALE @ MD-N
   s" duplicate sensor records" CL-SENSOR-DUP-REC @ MD-N
   s" dropped sensor records" CL-SENSOR-DROPPED @ MD-N
   s" duplicate timestamps" CL-SENSOR-DUP-TS @ MD-N
   s" timestamp regressions" CL-SENSOR-REG @ MD-N
   out outu s" summary.md" OUT-FILE$ RB$ WRITE-ALL ;

: WRITE-CAMERA-EVENTS ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: input:ptr inputu:n imgroot:ptr imgrootu:n out:ptr outu:n :}
   out outu MAKE-DIRS
   RB-RESET
   JOBJ{ s" type" JK, s" schema" JV-S, s" schema_version" JK, s" odin.latency_calibration.v1" JV-S, s" time_domain" JK, s" sdk_image_timestamp_ns" JV-S JOBJ}
   0 CL-I !
   begin CL-I @ CL-CAM-EVENT-N @ < while
      JOBJ{
      s" type" JK, s" camera_event" JV-S,
      s" schema_version" JK, s" odin.latency_calibration.v1" JV-S,
      s" event_id" JK, s" camera" CL-I @ 1+ JV-ID,
      s" logical_name" JK, LOGICAL$ JV-S,
      s" serial" JK, SERIAL$ JV-S,
      s" timestamp_ns" JK, CE-TS CL-I @ cells + @ JV-N,
      s" time_domain" JK, s" sdk_image_timestamp_ns" JV-S,
      s" frame_index" JK, CE-FRAME CL-I @ cells + @ JV-N,
      s" luminance_delta" JK, CE-DELTA CL-I @ cells + @ JV-N,
      s" signal" JK, s" luminance_rise" JV-S,
      s" confidence" JK, JV-R1
      JOBJ}
      CL-I @ 1+ CL-I !
   repeat
   out outu s" events.ndjson" OUT-FILE$ RB$ WRITE-ALL
   RB-RESET
   s" input_path,image_root,time_domain,signal,threshold_delta,min_spacing_ns,min_timestamp_ns,max_timestamp_ns,frame_records,cameras,image_frames,image_missing,image_decode_failures,events,result" RB+ RB-NL
   input inputu RB+ CM imgroot imgrootu RB+ CM s" sdk_image_timestamp_ns,luminance-rise," RB+ CL-THRESHOLD-DELTA @ RB# CM CL-CAMERA-MIN-SPACING-NS @ RB# s" ,,," RB+
   CL-FRAME-N @ RB# CM 1 RB# CM CL-IMAGE-N @ RB# CM CL-IMAGE-MISSING @ RB# CM CL-IMAGE-DECODE-FAIL @ RB# CM CL-CAM-EVENT-N @ RB# CM
   CL-CAM-EVENT-N @ 0 > CL-IMAGE-MISSING @ 0= and CL-IMAGE-DECODE-FAIL @ 0= and if s" pass" else s" fail" then RB+ RB-NL
   out outu s" metrics.csv" OUT-FILE$ RB$ WRITE-ALL
   RB-RESET
   s" # Camera Latency Events" RB+ RB-NL RB-NL
   s" input" input inputu MD-S
   s" image root" imgroot imgrootu MD-S
   s" time domain" s" sdk_image_timestamp_ns" MD-S
   s" signal" s" luminance-rise" MD-S
   s" threshold delta" CL-THRESHOLD-DELTA @ MD-N
   s" min spacing ns" CL-CAMERA-MIN-SPACING-NS @ MD-N
   s" frame records" CL-FRAME-N @ MD-N
   s" cameras" 1 MD-N
   s" image frames" CL-IMAGE-N @ MD-N
   s" image missing" CL-IMAGE-MISSING @ MD-N
   s" image decode failures" CL-IMAGE-DECODE-FAIL @ MD-N
   s" events" CL-CAM-EVENT-N @ MD-N
   s" result" CL-CAM-EVENT-N @ 0 > if s" pass" else s" fail" then MD-S
   out outu s" summary.md" OUT-FILE$ RB$ WRITE-ALL ;

: ISQRT ( n -- n )
   dup 1 <= if exit then
   CL-SQ-X !
   CL-SQ-X @ CL-SQ-R !
   begin
      CL-SQ-X @ CL-SQ-R @ / CL-SQ-R @ + 2 / CL-SQ-N !
      CL-SQ-N @ CL-SQ-R @ <
   while
      CL-SQ-N @ CL-SQ-R !
   repeat
   CL-SQ-R @ ;

: IMU-NORM-MILLI ( n -- n )
   {: ix:n :}
   S-AX ix cells + @ dup * 
   S-AY ix cells + @ dup * +
   S-AZ ix cells + @ dup * +
   ISQRT 1000 / ;

: ADD-IMU-EVENT ( n n n -- )
   {: ts:n sample:n val:n :}
   CL-IMU-EVENT-N @ CL-MAX-EVENTS >= if E-CL-CAPACITY throw then
   CL-IMU-EVENT-N @ {: ix:n :}
   ts IE-TS ix cells + !
   sample IE-SAMPLE ix cells + !
   val IE-VALUE ix cells + !
   ix 1+ CL-IMU-EVENT-N ! ;

: DETECT-IMU-EVENTS ( -- )
   0 CL-I !
   begin CL-I @ CL-SAMPLE-N @ < while
      CL-I @ IMU-NORM-MILLI {: val:n :}
      val CL-IMU-THRESHOLD-MILLI @ >= if
         CL-I @ 0= if 0 else CL-I @ 1- IMU-NORM-MILLI then {: prev:n :}
         CL-I @ 1+ CL-SAMPLE-N @ >= if 0 else CL-I @ 1+ IMU-NORM-MILLI then {: next:n :}
         val prev >= val next > and if
            S-TS CL-I @ cells + @ {: ts:n :}
            CL-HAVE-LAST-IMU-EVENT @ 0= ts CL-LAST-IMU-EVENT-TS @ - CL-IMU-MIN-SPACING-NS @ >= or if
               ts CL-I @ val ADD-IMU-EVENT
               ts CL-LAST-IMU-EVENT-TS !
               1 CL-HAVE-LAST-IMU-EVENT !
            then
         then
      then
      CL-I @ 1+ CL-I !
   repeat ;

: WRITE-IMU-EVENTS ( ptr u8 n ptr u8 n -- )
   {: input:ptr inputu:n out:ptr outu:n :}
   DETECT-IMU-EVENTS
   out outu MAKE-DIRS
   RB-RESET
   JOBJ{ s" type" JK, s" schema" JV-S, s" schema_version" JK, s" odin.latency_calibration.v1" JV-S, s" time_domain" JK, s" sdk_image_timestamp_ns" JV-S JOBJ}
   0 CL-I !
   begin CL-I @ CL-IMU-EVENT-N @ < while
      JOBJ{
      s" type" JK, s" imu_event" JV-S,
      s" schema_version" JK, s" odin.latency_calibration.v1" JV-S,
      s" event_id" JK, s" imu" CL-I @ 1+ JV-ID,
      s" device" JK, SERIAL$ JV-S,
      s" logical_name" JK, LOGICAL$ JV-S,
      s" timestamp_ns" JK, IE-TS CL-I @ cells + @ JV-N,
      s" time_domain" JK, s" sdk_image_timestamp_ns" JV-S,
      s" sample_index" JK, IE-SAMPLE CL-I @ cells + @ JV-N,
      s" signal" JK, s" accel_norm_peak" JV-S,
      s" confidence" JK, JV-R1
      JOBJ}
      CL-I @ 1+ CL-I !
   repeat
   out outu s" events.ndjson" OUT-FILE$ RB$ WRITE-ALL
   RB-RESET
   s" input_path,source,device,logical_name,time_domain,frame,signal,threshold,min_spacing_ns,min_timestamp_ns,max_timestamp_ns,samples,window_samples,events,duplicate_timestamps,timestamp_regressions,result" RB+ RB-NL
   input inputu RB+ CM SOURCE$ RB+ CM SERIAL$ RB+ CM LOGICAL$ RB+ s" ,sdk_image_timestamp_ns,sensor,accel_norm," RB+
   CL-IMU-THRESHOLD-MILLI @ 1 RB-FIXED3 CM CL-IMU-MIN-SPACING-NS @ RB# s" ,,," RB+
   CL-SAMPLE-N @ RB# CM CL-SAMPLE-N @ RB# CM CL-IMU-EVENT-N @ RB# CM CL-SENSOR-DUP-TS @ RB# CM CL-SENSOR-REG @ RB# CM
   CL-IMU-EVENT-N @ 0 > CL-SENSOR-DUP-TS @ 0= and CL-SENSOR-REG @ 0= and if s" pass" else s" fail" then RB+ RB-NL
   out outu s" metrics.csv" OUT-FILE$ RB$ WRITE-ALL
   RB-RESET
   s" # IMU Latency Events" RB+ RB-NL RB-NL
   s" input" input inputu MD-S
   s" source" SOURCE$ MD-S
   s" device" SERIAL$ MD-S
   s" logical name" LOGICAL$ MD-S
   s" time domain" s" sdk_image_timestamp_ns" MD-S
   s" frame" s" sensor" MD-S
   s" signal" s" accel_norm" MD-S
   s" threshold milli" CL-IMU-THRESHOLD-MILLI @ MD-N
   s" min spacing ns" CL-IMU-MIN-SPACING-NS @ MD-N
   s" samples" CL-SAMPLE-N @ MD-N
   s" window samples" CL-SAMPLE-N @ MD-N
   s" events" CL-IMU-EVENT-N @ MD-N
   s" duplicate timestamps" CL-SENSOR-DUP-TS @ MD-N
   s" timestamp regressions" CL-SENSOR-REG @ MD-N
   s" result" CL-IMU-EVENT-N @ 0 > if s" pass" else s" fail" then MD-S
   out outu s" summary.md" OUT-FILE$ RB$ WRITE-ALL ;

: MATCH-N ( -- n ) CL-CAM-EVENT-N @ CL-IMU-EVENT-N @ MIN2 ;
: COMPUTE-LATENCY ( -- )
   MATCH-N CL-MATCH-N !
   CL-CAM-EVENT-N @ CL-MATCH-N @ - CL-MISSING-IMU !
   CL-IMU-EVENT-N @ CL-MATCH-N @ - CL-MISSING-CAM !
   0 CL-I !
   begin CL-I @ CL-MATCH-N @ < while
      CE-TS CL-I @ cells + @
      IE-TS CL-I @ cells + @ CL-OFFSET-NS @ + -
      LAT CL-I @ cells + !
      CL-I @ 1+ CL-I !
   repeat
   CL-MATCH-N @ 0 > if LAT CL-MATCH-N @ CL-MAX-JITTER-NS @ LATCAL:LATSTATS then ;

: LATENCY-READY? ( -- bool )
   CL-MATCH-N @ 0 > CL-MISSING-IMU @ 0= and CL-MISSING-CAM @ 0= and
   CL-MATCH-N @ 0 > if LATCAL:LC-OUT@ 0= else CL-FALSE then
   and ;

: LATENCY-RC ( -- n )
   CL-REQUIRE-READY @ 0= if 0 exit then
   LATENCY-READY? if 0 else 1 then ;

: WRITE-LATENCY ( ptr u8 n -- n )
   {: out:ptr outu:n :}
   COMPUTE-LATENCY
   out outu MAKE-DIRS
   RB-RESET
   s" camera_events,imu_events,matched_events,match_mode,camera_duplicate_ids,imu_duplicate_ids,camera_missing_imu,imu_missing_camera,offset_ns,offset_provided,camera_time_domain,imu_time_domain,latency_mean_ns,latency_median_ns,latency_min_ns,latency_max_ns,residual_p95_ns,residual_max_ns,max_jitter_ns,events_outside_jitter,result" RB+ RB-NL
   CL-CAM-EVENT-N @ RB# CM CL-IMU-EVENT-N @ RB# CM CL-MATCH-N @ RB# CM s" order,0,0," RB+
   CL-MISSING-IMU @ RB# CM CL-MISSING-CAM @ RB# CM CL-OFFSET-NS @ RB# CM CL-OFFSET-PROVIDED @ if s" yes" else s" no" then RB+ CM
   s" sdk_image_timestamp_ns,sdk_image_timestamp_ns," RB+
   CL-MATCH-N @ 0 > if LATCAL:MEAN@ else 0 then RB# CM
   CL-MATCH-N @ 0 > if LATCAL:LC-MED@ else 0 then RB# CM
   CL-MATCH-N @ 0 > if LATCAL:MIN@ else 0 then RB# CM
   CL-MATCH-N @ 0 > if LATCAL:LC-MAX@ else 0 then RB# CM
   CL-MATCH-N @ 0 > if LATCAL:RP95@ else 0 then RB# CM
   CL-MATCH-N @ 0 > if LATCAL:LC-RMAX@ else 0 then RB# CM
   CL-MAX-JITTER-NS @ RB# CM
   CL-MATCH-N @ 0 > if LATCAL:LC-OUT@ else 0 then RB# CM
   LATENCY-READY? if s" pass" else s" fail" then RB+ RB-NL
   out outu s" metrics.csv" OUT-FILE$ RB$ WRITE-ALL
   RB-RESET
   s" event_id,camera_source,imu_source,camera_timestamp_ns,imu_timestamp_ns,offset_ns,aligned_imu_timestamp_ns,latency_ns,residual_from_median_ns,within_jitter" RB+ RB-NL
   0 CL-I !
   begin CL-I @ CL-MATCH-N @ < while
      LAT CL-I @ cells + @ LATCAL:LC-MED@ - ABS-I {: resid:n :}
      s" order-" RB+ CL-I @ 1+ RB-U6 CM LOGICAL$ RB+ CM SERIAL$ RB+ CM
      CE-TS CL-I @ cells + @ RB# CM IE-TS CL-I @ cells + @ RB# CM CL-OFFSET-NS @ RB# CM
      IE-TS CL-I @ cells + @ CL-OFFSET-NS @ + RB# CM LAT CL-I @ cells + @ RB# CM resid RB# CM
      resid CL-MAX-JITTER-NS @ <= if s" yes" else s" no" then RB+ RB-NL
      CL-I @ 1+ CL-I !
   repeat
   out outu s" events.csv" OUT-FILE$ RB$ WRITE-ALL
   RB-RESET
   s" # Hardware Latency Calibration" RB+ RB-NL RB-NL
   s" schema" s" odin.latency_calibration.v1" MD-S
   s" camera events" CL-CAM-EVENT-N @ MD-N
   s" IMU events" CL-IMU-EVENT-N @ MD-N
   s" matched events" CL-MATCH-N @ MD-N
   s" match mode" s" order" MD-S
   s" camera time domain" s" sdk_image_timestamp_ns" MD-S
   s" IMU time domain" s" sdk_image_timestamp_ns" MD-S
   s" offset applied to IMU timestamps ns" CL-OFFSET-NS @ MD-N
   s" max allowed event jitter ns" CL-MAX-JITTER-NS @ MD-N
   s" result" LATENCY-READY? if s" pass" else s" fail" then MD-S
   out outu s" summary.md" OUT-FILE$ RB$ WRITE-ALL
   LATENCY-RC ;

public

: RESET ( -- ) DEFAULTS RESET-DATA ;
: LOGICAL! ( ptr u8 n -- ) SET-LOGICAL ;
: SERIAL! ( ptr u8 n -- ) SET-SERIAL ;
: THRESHOLD-DELTA! ( n -- ) CL-THRESHOLD-DELTA ! ;
: IMU-THRESHOLD-MILLI! ( n -- ) CL-IMU-THRESHOLD-MILLI ! ;
: CAMERA-MIN-SPACING-NS! ( n -- ) CL-CAMERA-MIN-SPACING-NS ! ;
: IMU-MIN-SPACING-NS! ( n -- ) CL-IMU-MIN-SPACING-NS ! ;
: MAX-JITTER-NS! ( n -- ) CL-MAX-JITTER-NS ! ;
: OFFSET-NS! ( n -- ) CL-OFFSET-NS ! 1 CL-OFFSET-PROVIDED ! ;
: ALLOW-CHARACTERIZATION! ( -- ) 0 CL-REQUIRE-READY ! ;

: ANALYZE ( ptr u8 n ptr u8 n ptr u8 n -- n )
   {: input:ptr inputu:n imgroot:ptr imgrootu:n runroot:ptr runrootu:n :}
   input inputu CL-SOURCE CL-TEXT-CAP CL-SOURCE-U CL-COPY
   input inputu imgroot imgrootu LOAD-CAPTURE
   runroot runrootu CAMERAONE-DIR$ WRITE-CAMERAONE-IMU
   input inputu imgroot imgrootu runroot runrootu CAMERA-EVENTS-DIR$ WRITE-CAMERA-EVENTS
   runroot runrootu CAMERAONE-IMU-NDJSON$ runroot runrootu IMU-EVENTS-DIR$ WRITE-IMU-EVENTS
   runroot runrootu LATENCY-DIR$ WRITE-LATENCY ;

end-package
