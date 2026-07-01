\ spsc-motion.f - Habu SPSC BMI088 motion analyzer and scenario summaries.
\
\ Reads odin.external_imu.v1 NDJSON, compares motion against optional baseline
\ IMU logs, and writes deterministic CSV/Markdown artifacts without invoking
\ any non-Habu Odin command.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/render.f
require lib/float.f
require lib/prelude.f
require tools/json.f
require tools/json-file.f
require odin/float-cell.f

package SPMOT
private

8192 constant SM-MAX-SAMPLES
512 constant SM-MAX-WINDOWS
8 constant SM-MAX-CAMERAS
256 constant SM-TEXT-CAP
1024 constant SM-PATH-CAP
$100000 constant SM-READ-CAP
1000000 constant SM-NS-PER-MS

0 constant SM-CHK-NOT
1 constant SM-CHK-PASS
2 constant SM-CHK-FAIL

-8500 constant E-SM-SCHEMA
-8501 constant E-SM-CAPACITY
-8502 constant E-SM-JSON
-8503 constant E-SM-OPTION
-8504 constant E-SM-CSV

create SM-INPUT-BUF SM-PATH-CAP allot
create SM-BASELINE-BUF SM-PATH-CAP allot
create SM-OUT-BUF SM-PATH-CAP allot
create SM-P0 SM-PATH-CAP allot
create SM-P1 SM-PATH-CAP allot
create SM-TEXT-BUF SM-READ-CAP allot
create SM-SOURCE SM-TEXT-CAP allot
create SM-DEVICE SM-TEXT-CAP allot
create SM-LOGICAL SM-TEXT-CAP allot
create SM-TIME-DOMAIN SM-TEXT-CAP allot
create SM-FRAME SM-TEXT-CAP allot

create S-TS SM-MAX-SAMPLES cells allot
create S-AX SM-MAX-SAMPLES cells allot
create S-AY SM-MAX-SAMPLES cells allot
create S-AZ SM-MAX-SAMPLES cells allot
create S-GX SM-MAX-SAMPLES cells allot
create S-GY SM-MAX-SAMPLES cells allot
create S-GZ SM-MAX-SAMPLES cells allot

create W-IX SM-MAX-WINDOWS cells allot
create W-START SM-MAX-WINDOWS cells allot
create W-END SM-MAX-WINDOWS cells allot
create W-SAMPLES SM-MAX-WINDOWS cells allot
create W-DUR SM-MAX-WINDOWS cells allot
create W-RATE SM-MAX-WINDOWS cells allot
create W-ACC-DYN-RMS SM-MAX-WINDOWS cells allot
create W-ACC-DYN-MAX SM-MAX-WINDOWS cells allot
create W-ACC-NORM-MEAN SM-MAX-WINDOWS cells allot
create W-ACC-NORM-STD SM-MAX-WINDOWS cells allot
create W-GYR-DYN-RMS SM-MAX-WINDOWS cells allot
create W-GYR-DYN-MAX SM-MAX-WINDOWS cells allot
create W-GYR-NORM-MEAN SM-MAX-WINDOWS cells allot
create W-GYR-NORM-STD SM-MAX-WINDOWS cells allot
create W-ACC-RATIO SM-MAX-WINDOWS cells allot
create W-GYR-RATIO SM-MAX-WINDOWS cells allot
create W-HAVE-ACC-RATIO SM-MAX-WINDOWS cells allot
create W-HAVE-GYR-RATIO SM-MAX-WINDOWS cells allot

create SM-WINDOW-MS 1 cells allot
create SM-TH-ACC-RMS 1 cells allot
create SM-TH-GYR-RMS 1 cells allot
create SM-TH-ACC-RATIO 1 cells allot
create SM-TH-GYR-RATIO 1 cells allot
create SM-BASE-ACC-DYN 1 cells allot
create SM-BASE-GYR-DYN 1 cells allot
create SM-R-ACC-RATIO 1 cells allot
create SM-R-GYR-RATIO 1 cells allot

create R-DUR 1 cells allot
create R-RATE 1 cells allot
create R-ACC-MX 1 cells allot
create R-ACC-MY 1 cells allot
create R-ACC-MZ 1 cells allot
create R-ACC-DYN-RMS 1 cells allot
create R-ACC-DYN-MAX 1 cells allot
create R-ACC-NORM-MEAN 1 cells allot
create R-ACC-NORM-STD 1 cells allot
create R-GYR-MX 1 cells allot
create R-GYR-MY 1 cells allot
create R-GYR-MZ 1 cells allot
create R-GYR-DYN-RMS 1 cells allot
create R-GYR-DYN-MAX 1 cells allot
create R-GYR-NORM-MEAN 1 cells allot
create R-GYR-NORM-STD 1 cells allot

create SUM-AX 1 cells allot
create SUM-AY 1 cells allot
create SUM-AZ 1 cells allot
create SUM-GX 1 cells allot
create SUM-GY 1 cells allot
create SUM-GZ 1 cells allot
create SUM-AN 1 cells allot
create SUM-GN 1 cells allot
create SUM-ANSQ 1 cells allot
create SUM-GNSQ 1 cells allot
create SUM-AMIN 1 cells allot
create SUM-AMAX 1 cells allot
create SUM-GMIN 1 cells allot
create SUM-GMAX 1 cells allot
create SUM-AVAR 1 cells allot
create SUM-GVAR 1 cells allot
create SUM-ADYNSQ 1 cells allot
create SUM-GDYNSQ 1 cells allot
create SUM-ADYNMAX 1 cells allot
create SUM-GDYNMAX 1 cells allot
create SM-MAX-R 1 cells allot

variable SM-INPUT-U
variable SM-BASELINE-U
variable SM-OUT-U
variable SM-SOURCE-U
variable SM-DEVICE-U
variable SM-LOGICAL-U
variable SM-TIME-DOMAIN-U
variable SM-FRAME-U
variable SM-N
variable SM-DUP-TS
variable SM-REG-TS
variable SM-HAVE-LAST
variable SM-LAST-TS
variable SM-FIRST-TS
variable SM-LAST-SAMPLE-TS
variable SM-PERIOD-N
variable SM-PERIOD-MIN
variable SM-PERIOD-MAX
variable SM-PERIOD-MEAN
variable SM-WINDOW-N
variable SM-I
variable SM-J
variable SM-TMP
variable SM-LOOP-LAST
variable SM-WSTART
variable SM-WEND
variable SM-WIN-IX
variable SM-WIN-WIX
variable SM-BEST
variable SM-FIELD
variable SM-START
variable SM-MAX-N
variable SM-READ-U
variable SM-HAVE-BASELINE
variable SM-BASE-SAMPLES
variable SM-HAVE-ACC-RATIO
variable SM-HAVE-GYR-RATIO
variable SM-HAVE-TH-ACC-RMS
variable SM-HAVE-TH-GYR-RMS
variable SM-HAVE-TH-ACC-RATIO
variable SM-HAVE-TH-GYR-RATIO
variable SM-REQUIRE-READY
variable R-SAMPLES
variable R-FIRST
variable R-LAST

: SM-TRUE ( -- bool ) true ;
: SM-FALSE ( -- bool ) false ;

: SM-COPY! ( ptr u8 n ptr u8 n ptr a -- )
   {: a:ptr u:n dst:ptr cap:n lenp:ptr :}
   u 0 < if E-STR-CAPACITY throw then
   u cap >= if E-STR-CAPACITY throw then
   a dst u BYTE-COPY
   u lenp ! ;

: INPUT$ ( -- ptr u8 n ) SM-INPUT-BUF SM-INPUT-U @ ;
: BASELINE$ ( -- ptr u8 n ) SM-BASELINE-BUF SM-BASELINE-U @ ;
: OUT$ ( -- ptr u8 n ) SM-OUT-BUF SM-OUT-U @ ;
: SOURCE$ ( -- ptr u8 n ) SM-SOURCE SM-SOURCE-U @ ;
: DEVICE$ ( -- ptr u8 n ) SM-DEVICE SM-DEVICE-U @ ;
: LOGICAL$ ( -- ptr u8 n ) SM-LOGICAL SM-LOGICAL-U @ ;
: TIME-DOMAIN$ ( -- ptr u8 n ) SM-TIME-DOMAIN SM-TIME-DOMAIN-U @ ;
: FRAME$ ( -- ptr u8 n ) SM-FRAME SM-FRAME-U @ ;

: SET-INPUT ( ptr u8 n -- ) SM-INPUT-BUF SM-PATH-CAP SM-INPUT-U SM-COPY! ;
: SET-BASELINE ( ptr u8 n -- ) SM-BASELINE-BUF SM-PATH-CAP SM-BASELINE-U SM-COPY! ;
: SET-OUT ( ptr u8 n -- ) SM-OUT-BUF SM-PATH-CAP SM-OUT-U SM-COPY! ;
: SET-SOURCE ( ptr u8 n -- ) SM-SOURCE SM-TEXT-CAP SM-SOURCE-U SM-COPY! ;
: SET-DEVICE ( ptr u8 n -- ) SM-DEVICE SM-TEXT-CAP SM-DEVICE-U SM-COPY! ;
: SET-LOGICAL ( ptr u8 n -- ) SM-LOGICAL SM-TEXT-CAP SM-LOGICAL-U SM-COPY! ;
: SET-TIME-DOMAIN ( ptr u8 n -- ) SM-TIME-DOMAIN SM-TEXT-CAP SM-TIME-DOMAIN-U SM-COPY! ;
: SET-FRAME ( ptr u8 n -- ) SM-FRAME SM-TEXT-CAP SM-FRAME-U SM-COPY! ;

: A@ ( ptr a n -- n ) {: base:ptr ix:n :} base ix cells + @ ;
: A! ( n ptr a n -- ) {: v:n base:ptr ix:n :} v base ix cells + ! ;
: FA@ ( ptr a n -- r ) {: base:ptr ix:n :} base ix cells + F@ ;
: FA! ( r ptr a n -- ) {: v:r base:ptr ix:n :} v base ix cells + F! ;
: F0! ( ptr a -- ) 0.0 swap F! ;

: SM-ABS ( n -- n ) dup 0 < if negate then ;
: FMIN2 ( r r -- r ) {: a:r b:r :} a b f< if a else b then ;
: FMAX2 ( r r -- r ) {: a:r b:r :} a b f> if a else b then ;
: VEC-NORM ( r r r -- r ) {: x:r y:r z:r :} x x f* y y f* f+ z z f* f+ fsqrt ;
: VEC-DELTA ( r r r r r r -- r )
   {: x:r y:r z:r mx:r my:r mz:r :}
   x mx f- y my f- z mz f- VEC-NORM ;

: RB-6 ( n -- )
   {: x:n :}
   x 100000 / 48 + RB-C
   x 100000 mod 10000 / 48 + RB-C
   x 10000 mod 1000 / 48 + RB-C
   x 1000 mod 100 / 48 + RB-C
   x 100 mod 10 / 48 + RB-C
   x 10 mod 48 + RB-C ;

: RB-FFIX6 ( r -- )
   fdup 0.0 f< if 45 RB-C fnegate then
   1000000.0 f* 0.5 f+ f>s {: scaled:n :}
   scaled 1000000 / RB# 46 RB-C scaled 1000000 mod RB-6 ;

: RB-FFIX3S ( r -- )
   fdup 0.0 f< if 45 RB-C fnegate then
   1000.0 f* 0.5 f+ f>s {: scaled:n :}
   scaled 1000 / RB# 46 RB-C scaled 1000 mod RB-3 ;

: JSON-FLOAT ( n -- r )
   JSON-NUMBER$ STR>FLOAT 0= if drop E-SM-JSON throw then ;

: JNODE ( n ptr u8 n -- n ) JSON-GET ;
: JSTR$ ( n ptr u8 n -- ptr u8 n ) JNODE JSON-STRING$ ;
: JINT ( n ptr u8 n -- n ) JNODE JSON-NUMBER$ STR>NUMBER? drop ;
: JFLOAT ( n ptr u8 n -- r ) JNODE JSON-FLOAT ;
: JARR-F ( n n -- r ) {: arr:n ix:n :} arr ix JSON-ARR@ JSON-FLOAT ;

: JOPT-STR ( n ptr u8 n -- ptr u8 n bool )
   JNODE {: v:n :}
   v -1 = if s" " SM-FALSE exit then
   v JSON-KIND J-NULL = if s" " SM-FALSE exit then
   v JSON-STRING$ SM-TRUE ;

: TIME-DOMAIN? ( ptr u8 n -- bool )
   2dup s" host_monotonic_ns" STR= if 2drop SM-TRUE exit then
   2dup s" external_timecode_ns" STR= if 2drop SM-TRUE exit then
   2dup s" gps_time_ns" STR= if 2drop SM-TRUE exit then
   s" unix_epoch_ns" STR= ;

: FRAME? ( ptr u8 n -- bool )
   2dup s" sensor" STR= if 2drop SM-TRUE exit then
   2dup s" rig" STR= if 2drop SM-TRUE exit then
   s" truck" STR= ;

: CHECK-TIME-DOMAIN ( ptr u8 n -- )
   2dup TIME-DOMAIN? 0= if 2drop E-SM-SCHEMA throw then 2drop ;

: CHECK-FRAME ( ptr u8 n -- )
   2dup FRAME? 0= if 2drop E-SM-SCHEMA throw then 2drop ;

: META-DEFAULTS ( -- )
   s" unknown" SET-SOURCE
   s" unknown" SET-DEVICE
   s" unknown" SET-LOGICAL
   s" unknown" SET-TIME-DOMAIN
   s" unknown" SET-FRAME ;

: RESET-SAMPLES ( -- )
   0 SM-N !
   0 SM-DUP-TS !
   0 SM-REG-TS !
   0 SM-HAVE-LAST !
   0 SM-LAST-TS !
   0 SM-FIRST-TS !
   0 SM-LAST-SAMPLE-TS !
   0 SM-PERIOD-N !
   0 SM-PERIOD-MIN !
   0 SM-PERIOD-MAX !
   0 SM-PERIOD-MEAN !
   0 SM-WINDOW-N ! ;

: RESET-RESULT ( -- )
   0 R-SAMPLES !
   0 R-FIRST !
   0 R-LAST !
   R-DUR F0!
   R-RATE F0!
   R-ACC-MX F0!
   R-ACC-MY F0!
   R-ACC-MZ F0!
   R-ACC-DYN-RMS F0!
   R-ACC-DYN-MAX F0!
   R-ACC-NORM-MEAN F0!
   R-ACC-NORM-STD F0!
   R-GYR-MX F0!
   R-GYR-MY F0!
   R-GYR-MZ F0!
   R-GYR-DYN-RMS F0!
   R-GYR-DYN-MAX F0!
   R-GYR-NORM-MEAN F0!
   R-GYR-NORM-STD F0! ;

: RESET-OPTIONS ( -- )
   s" results/imu/spsc_bmi/imu.ndjson" SET-INPUT
   s" " SET-BASELINE
   s" results/imu/spsc_motion" SET-OUT
   250.0 SM-WINDOW-MS F!
   0 SM-HAVE-TH-ACC-RMS !
   0 SM-HAVE-TH-GYR-RMS !
   0 SM-HAVE-TH-ACC-RATIO !
   0 SM-HAVE-TH-GYR-RATIO !
   0 SM-REQUIRE-READY !
   0 SM-HAVE-BASELINE !
   0 SM-BASE-SAMPLES !
   0 SM-HAVE-ACC-RATIO !
   0 SM-HAVE-GYR-RATIO ! ;

: RESET-ALL ( -- )
   RESET-OPTIONS
   META-DEFAULTS
   RESET-SAMPLES
   RESET-RESULT ;

: ADD-SAMPLE ( n r r r r r r -- )
   {: ts:n ax:r ay:r az:r gx:r gy:r gz:r :}
   SM-N @ SM-MAX-SAMPLES >= if E-SM-CAPACITY throw then
   SM-HAVE-LAST @ 0 <> if
      ts SM-LAST-TS @ = if SM-DUP-TS @ 1+ SM-DUP-TS ! then
      ts SM-LAST-TS @ < if SM-REG-TS @ 1+ SM-REG-TS ! then
   then
   ts SM-LAST-TS !
   1 SM-HAVE-LAST !
   ts S-TS SM-N @ A!
   ax S-AX SM-N @ FA!
   ay S-AY SM-N @ FA!
   az S-AZ SM-N @ FA!
   gx S-GX SM-N @ FA!
   gy S-GY SM-N @ FA!
   gz S-GZ SM-N @ FA!
   SM-N @ 1+ SM-N ! ;

: HANDLE-SCHEMA ( n -- )
   {: root:n :}
   root s" schema_version" JSTR$ s" odin.external_imu.v1" STR= 0= if E-SM-SCHEMA throw then
   root s" source" JOPT-STR if SET-SOURCE else 2drop then
   root s" device" JOPT-STR if SET-DEVICE else 2drop then
   root s" logical_name" JOPT-STR if SET-LOGICAL else 2drop then
   root s" time_domain" JOPT-STR if 2dup CHECK-TIME-DOMAIN SET-TIME-DOMAIN else 2drop then
   root s" frame" JOPT-STR if 2dup CHECK-FRAME SET-FRAME else 2drop then ;

: HANDLE-SAMPLE ( n -- )
   {: root:n :}
   root s" schema_version" JSTR$ s" odin.external_imu.v1" STR= 0= if E-SM-SCHEMA throw then
   root s" imu_timestamp_ns" JINT {: ts:n :}
   ts 0 < if E-SM-SCHEMA throw then
   root s" time_domain" JSTR$ 2dup CHECK-TIME-DOMAIN 2dup TIME-DOMAIN$ STR= 0= if 2drop E-SM-SCHEMA throw then 2drop
   root s" frame" JSTR$ 2dup CHECK-FRAME 2dup FRAME$ STR= 0= if 2drop E-SM-SCHEMA throw then 2drop
   root s" accel_m_s2" JNODE {: aa:n :}
   root s" gyro_rad_s" JNODE {: ga:n :}
   ts
   aa 0 JARR-F aa 1 JARR-F aa 2 JARR-F
   ga 0 JARR-F ga 1 JARR-F ga 2 JARR-F
   ADD-SAMPLE ;

: PROCESS-LINE ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0= if exit then
   a u JSON-PARSE {: root:n :}
   root s" type" JSTR$ s" schema" STR= if root HANDLE-SCHEMA exit then
   root s" type" JSTR$ s" imu_sample" STR= if root HANDLE-SAMPLE exit then
   E-SM-SCHEMA throw ;

: LOAD-LOG ( ptr u8 n -- )
   META-DEFAULTS
   RESET-SAMPLES
   JSONLF-OPEN
   begin JSONLF-NEXT-LINE while PROCESS-LINE repeat 2drop ;

: READ-FILE$ ( ptr u8 n -- ptr u8 n )
   SM-TEXT-BUF SM-READ-CAP READ-ALL {: u:n :}
   SM-TEXT-BUF u ;

: LOAD-LOG-TEXT ( ptr u8 n -- )
   {: a:ptr u:n :}
   META-DEFAULTS
   RESET-SAMPLES
   a u JSONL-START-STRICT
   begin JSONL-NEXT-ROW while
      drop drop drop
      JSONL-LINE$ PROCESS-LINE
   repeat drop drop drop ;

: BUILD-TIMESTAMP-STATS ( -- )
   SM-N @ 0= if exit then
   S-TS 0 A@ SM-FIRST-TS !
   S-TS SM-N @ 1- A@ SM-LAST-SAMPLE-TS !
   0 SM-PERIOD-N !
   0 SM-PERIOD-MIN !
   0 SM-PERIOD-MAX !
   0 SM-TMP !
   S-TS 0 A@ SM-LOOP-LAST !
   1 SM-I !
   begin SM-I @ SM-N @ < while
      S-TS SM-I @ A@ {: ts:n :}
      ts SM-LOOP-LAST @ >= if
         ts SM-LOOP-LAST @ - {: per:n :}
         SM-PERIOD-N @ 0= if
            per SM-PERIOD-MIN !
            per SM-PERIOD-MAX !
         else
            per SM-PERIOD-MIN @ < if per SM-PERIOD-MIN ! then
            per SM-PERIOD-MAX @ > if per SM-PERIOD-MAX ! then
         then
         SM-TMP @ per + SM-TMP !
         SM-PERIOD-N @ 1+ SM-PERIOD-N !
      then
      ts SM-LOOP-LAST !
      SM-I @ 1+ SM-I !
   repeat
   SM-PERIOD-N @ 0 > if SM-TMP @ SM-PERIOD-N @ / SM-PERIOD-MEAN ! then ;

: SAMPLE-RATE-F ( -- r )
   SM-PERIOD-MEAN @ 0 > if 1000000000.0 SM-PERIOD-MEAN @ s>f f/ else 0.0 then ;

: SPAN-DURATION-NS ( n n -- n )
   {: start:n cnt:n :}
   cnt 0 <= if 0 exit then
   S-TS start cnt + 1- A@ S-TS start A@ - dup 0 < if drop 0 then ;

: SUMMARIZE-SPAN ( n n -- )
   {: start:n cnt:n :}
   RESET-RESULT
   cnt 0 <= if exit then
   cnt R-SAMPLES !
   S-TS start A@ R-FIRST !
   S-TS start cnt + 1- A@ R-LAST !
   SUM-AX F0!
   SUM-AY F0!
   SUM-AZ F0!
   SUM-GX F0!
   SUM-GY F0!
   SUM-GZ F0!
   SUM-AN F0!
   SUM-GN F0!
   SUM-ANSQ F0!
   SUM-GNSQ F0!
   SUM-AMIN F0!
   SUM-AMAX F0!
   SUM-GMIN F0!
   SUM-GMAX F0!
   0 SM-I !
   begin SM-I @ cnt < while
      start SM-I @ + {: ix:n :}
      S-AX ix FA@ {: ax:r :} S-AY ix FA@ {: ay:r :} S-AZ ix FA@ {: az:r :}
      S-GX ix FA@ {: gx:r :} S-GY ix FA@ {: gy:r :} S-GZ ix FA@ {: gz:r :}
      ax ay az VEC-NORM {: an:r :}
      gx gy gz VEC-NORM {: gn:r :}
      SUM-AX F@ ax f+ SUM-AX F!
      SUM-AY F@ ay f+ SUM-AY F!
      SUM-AZ F@ az f+ SUM-AZ F!
      SUM-GX F@ gx f+ SUM-GX F!
      SUM-GY F@ gy f+ SUM-GY F!
      SUM-GZ F@ gz f+ SUM-GZ F!
      SUM-AN F@ an f+ SUM-AN F!
      SUM-GN F@ gn f+ SUM-GN F!
      SUM-ANSQ F@ an an f* f+ SUM-ANSQ F!
      SUM-GNSQ F@ gn gn f* f+ SUM-GNSQ F!
      SM-I @ 0= if
         an SUM-AMIN F!
         an SUM-AMAX F!
         gn SUM-GMIN F!
         gn SUM-GMAX F!
      else
         SUM-AMIN F@ an FMIN2 SUM-AMIN F!
         SUM-AMAX F@ an FMAX2 SUM-AMAX F!
         SUM-GMIN F@ gn FMIN2 SUM-GMIN F!
         SUM-GMAX F@ gn FMAX2 SUM-GMAX F!
      then
      SM-I @ 1+ SM-I !
   repeat
   cnt s>f {: nf:r :}
   SUM-AX F@ nf f/ R-ACC-MX F!
   SUM-AY F@ nf f/ R-ACC-MY F!
   SUM-AZ F@ nf f/ R-ACC-MZ F!
   SUM-GX F@ nf f/ R-GYR-MX F!
   SUM-GY F@ nf f/ R-GYR-MY F!
   SUM-GZ F@ nf f/ R-GYR-MZ F!
   SUM-AN F@ nf f/ R-ACC-NORM-MEAN F!
   SUM-GN F@ nf f/ R-GYR-NORM-MEAN F!
   SUM-AVAR F0!
   SUM-GVAR F0!
   SUM-ADYNSQ F0!
   SUM-GDYNSQ F0!
   SUM-ADYNMAX F0!
   SUM-GDYNMAX F0!
   0 SM-I !
   begin SM-I @ cnt < while
      start SM-I @ + {: ix:n :}
      S-AX ix FA@ {: ax:r :} S-AY ix FA@ {: ay:r :} S-AZ ix FA@ {: az:r :}
      S-GX ix FA@ {: gx:r :} S-GY ix FA@ {: gy:r :} S-GZ ix FA@ {: gz:r :}
      ax ay az VEC-NORM R-ACC-NORM-MEAN F@ f- {: ad:r :}
      gx gy gz VEC-NORM R-GYR-NORM-MEAN F@ f- {: gd:r :}
      SUM-AVAR F@ ad ad f* f+ SUM-AVAR F!
      SUM-GVAR F@ gd gd f* f+ SUM-GVAR F!
      ax ay az R-ACC-MX F@ R-ACC-MY F@ R-ACC-MZ F@ VEC-DELTA {: adv:r :}
      gx gy gz R-GYR-MX F@ R-GYR-MY F@ R-GYR-MZ F@ VEC-DELTA {: gdv:r :}
      SUM-ADYNSQ F@ adv adv f* f+ SUM-ADYNSQ F!
      SUM-GDYNSQ F@ gdv gdv f* f+ SUM-GDYNSQ F!
      SUM-ADYNMAX F@ adv FMAX2 SUM-ADYNMAX F!
      SUM-GDYNMAX F@ gdv FMAX2 SUM-GDYNMAX F!
      SM-I @ 1+ SM-I !
   repeat
   SUM-ADYNSQ F@ nf f/ fsqrt R-ACC-DYN-RMS F!
   SUM-GDYNSQ F@ nf f/ fsqrt R-GYR-DYN-RMS F!
   SUM-ADYNMAX F@ R-ACC-DYN-MAX F!
   SUM-GDYNMAX F@ R-GYR-DYN-MAX F!
   SUM-AVAR F@ nf f/ fsqrt R-ACC-NORM-STD F!
   SUM-GVAR F@ nf f/ fsqrt R-GYR-NORM-STD F!
   start cnt SPAN-DURATION-NS {: dur:n :}
   dur s>f 1000000.0 f/ R-DUR F!
   cnt 1 > dur 0 > and if cnt 1- s>f 1000000000.0 f* dur s>f f/ else 0.0 then R-RATE F! ;

: BASELINE-RATIO ( r ptr a ptr a -- n )
   {: value:r basep:ptr outp:ptr :}
   basep F@ 0.0 f> if value basep F@ f/ outp F! 1 else 0 then ;

: COMPUTE-OVERALL ( -- )
   BUILD-TIMESTAMP-STATS
   0 SM-N @ SUMMARIZE-SPAN
   SM-HAVE-BASELINE @ 0 <> if
      R-ACC-DYN-RMS F@ SM-BASE-ACC-DYN SM-R-ACC-RATIO BASELINE-RATIO SM-HAVE-ACC-RATIO !
      R-GYR-DYN-RMS F@ SM-BASE-GYR-DYN SM-R-GYR-RATIO BASELINE-RATIO SM-HAVE-GYR-RATIO !
   else
      0 SM-HAVE-ACC-RATIO !
      0 SM-HAVE-GYR-RATIO !
   then ;

: COPY-R-TO-WINDOW ( n n n n -- )
   {: slot:n wix:n start:n end:n :}
   wix W-IX slot A!
   start W-START slot A!
   end W-END slot A!
   R-SAMPLES @ W-SAMPLES slot A!
   R-DUR F@ W-DUR slot FA!
   R-RATE F@ W-RATE slot FA!
   R-ACC-DYN-RMS F@ W-ACC-DYN-RMS slot FA!
   R-ACC-DYN-MAX F@ W-ACC-DYN-MAX slot FA!
   R-ACC-NORM-MEAN F@ W-ACC-NORM-MEAN slot FA!
   R-ACC-NORM-STD F@ W-ACC-NORM-STD slot FA!
   R-GYR-DYN-RMS F@ W-GYR-DYN-RMS slot FA!
   R-GYR-DYN-MAX F@ W-GYR-DYN-MAX slot FA!
   R-GYR-NORM-MEAN F@ W-GYR-NORM-MEAN slot FA!
   R-GYR-NORM-STD F@ W-GYR-NORM-STD slot FA!
   SM-HAVE-BASELINE @ 0 <> if
      R-ACC-DYN-RMS F@ SM-BASE-ACC-DYN W-ACC-RATIO slot cells + BASELINE-RATIO W-HAVE-ACC-RATIO slot A!
      R-GYR-DYN-RMS F@ SM-BASE-GYR-DYN W-GYR-RATIO slot cells + BASELINE-RATIO W-HAVE-GYR-RATIO slot A!
   else
      0 W-HAVE-ACC-RATIO slot A!
      0 W-HAVE-GYR-RATIO slot A!
   then ;

: WINDOW-NS ( -- n )
   SM-WINDOW-MS F@ 1000000.0 f* f>s dup 1 < if drop 1 then ;

: COMPUTE-WINDOWS ( -- )
   0 SM-WINDOW-N !
   SM-N @ 0= if exit then
   WINDOW-NS {: bucket:n :}
   S-TS 0 A@ SM-WSTART !
   SM-WSTART @ bucket + SM-WEND !
   0 SM-WIN-IX !
   0 SM-WIN-WIX !
   begin SM-WIN-IX @ SM-N @ < while
      begin SM-WIN-IX @ SM-N @ < S-TS SM-WIN-IX @ A@ SM-WEND @ >= and while
         SM-WEND @ SM-WSTART !
         SM-WSTART @ bucket + SM-WEND !
         SM-WIN-WIX @ 1+ SM-WIN-WIX !
      repeat
      SM-WIN-IX @ {: startix:n :}
      begin SM-WIN-IX @ SM-N @ < S-TS SM-WIN-IX @ A@ SM-WEND @ < and while
         SM-WIN-IX @ 1+ SM-WIN-IX !
      repeat
      SM-WINDOW-N @ SM-MAX-WINDOWS >= if E-SM-CAPACITY throw then
      startix SM-WIN-IX @ startix - SUMMARIZE-SPAN
      SM-WINDOW-N @ SM-WIN-WIX @ SM-WSTART @ SM-WEND @ COPY-R-TO-WINDOW
      SM-WINDOW-N @ 1+ SM-WINDOW-N !
      SM-WEND @ SM-WSTART !
      SM-WSTART @ bucket + SM-WEND !
      SM-WIN-WIX @ 1+ SM-WIN-WIX !
   repeat ;

: COMPUTE-BASELINE ( ptr u8 n -- )
   dup 0= if 2drop 0 SM-HAVE-BASELINE ! exit then
   LOAD-LOG
   0 SM-N @ SUMMARIZE-SPAN
   R-SAMPLES @ SM-BASE-SAMPLES !
   R-ACC-DYN-RMS F@ SM-BASE-ACC-DYN F!
   R-GYR-DYN-RMS F@ SM-BASE-GYR-DYN F!
   1 SM-HAVE-BASELINE ! ;

: REPORT-RESULT$ ( -- ptr u8 n )
   SM-N @ 0= if s" fail" exit then
   SM-DUP-TS @ 0 <> SM-REG-TS @ 0 <> or if s" fail" exit then
   SM-HAVE-BASELINE @ 0 <> if s" comparison" else s" characterization" then ;

: THRESHOLD-COUNT ( -- n )
   0
   SM-HAVE-TH-ACC-RMS @ 0 <> if 1+ then
   SM-HAVE-TH-GYR-RMS @ 0 <> if 1+ then
   SM-HAVE-TH-ACC-RATIO @ 0 <> if 1+ then
   SM-HAVE-TH-GYR-RATIO @ 0 <> if 1+ then ;

: CHECK-MAX ( r ptr a n -- n )
   {: value:r thp:ptr have:n :}
   have 0= if SM-CHK-NOT exit then
   value thp F@ f> if SM-CHK-FAIL else SM-CHK-PASS then ;

: CHECK-OPT-MAX ( r n ptr a n -- n )
   {: value:r present:n thp:ptr have:n :}
   have 0= if SM-CHK-NOT exit then
   present 0= if SM-CHK-FAIL exit then
   value thp F@ f> if SM-CHK-FAIL else SM-CHK-PASS then ;

: SAMPLE-CHECK ( -- n ) SM-N @ 0 > if SM-CHK-PASS else SM-CHK-FAIL then ;
: TIMESTAMP-CHECK ( -- n ) SM-DUP-TS @ SM-REG-TS @ + 0= if SM-CHK-PASS else SM-CHK-FAIL then ;
: ACC-RMS-CHECK ( -- n ) R-ACC-DYN-RMS F@ SM-TH-ACC-RMS SM-HAVE-TH-ACC-RMS @ CHECK-MAX ;
: GYR-RMS-CHECK ( -- n ) R-GYR-DYN-RMS F@ SM-TH-GYR-RMS SM-HAVE-TH-GYR-RMS @ CHECK-MAX ;
: ACC-RATIO-CHECK ( -- n ) SM-R-ACC-RATIO F@ SM-HAVE-ACC-RATIO @ SM-TH-ACC-RATIO SM-HAVE-TH-ACC-RATIO @ CHECK-OPT-MAX ;
: GYR-RATIO-CHECK ( -- n ) SM-R-GYR-RATIO F@ SM-HAVE-GYR-RATIO @ SM-TH-GYR-RATIO SM-HAVE-TH-GYR-RATIO @ CHECK-OPT-MAX ;

: READINESS-RESULT$ ( -- ptr u8 n )
   SAMPLE-CHECK SM-CHK-FAIL = if s" fail" exit then
   TIMESTAMP-CHECK SM-CHK-FAIL = if s" fail" exit then
   THRESHOLD-COUNT 0= if s" not_run" exit then
   ACC-RMS-CHECK SM-CHK-FAIL = if s" fail" exit then
   GYR-RMS-CHECK SM-CHK-FAIL = if s" fail" exit then
   ACC-RATIO-CHECK SM-CHK-FAIL = if s" fail" exit then
   GYR-RATIO-CHECK SM-CHK-FAIL = if s" fail" exit then
   s" pass" ;

: CHECK$ ( n -- ptr u8 n )
   dup SM-CHK-NOT = if drop s" not_checked" exit then
   dup SM-CHK-PASS = if drop s" yes" exit then
   drop s" no" ;

: RB-OPT6 ( ptr a n -- )
   0 <> if F@ RB-FFIX6 else drop then ;

: CSV-HEADER ( -- )
   s" kind,window_index,start_timestamp_ns,end_timestamp_ns,samples,duration_ms,sample_rate_hz_mean,accel_dynamic_rms_m_s2,accel_dynamic_max_m_s2,accel_norm_mean_m_s2,accel_norm_stddev_m_s2,gyro_dynamic_rms_rad_s,gyro_dynamic_max_rad_s,gyro_norm_mean_rad_s,gyro_norm_stddev_rad_s,accel_dynamic_ratio_to_baseline,gyro_dynamic_ratio_to_baseline" RB+ RB-NL ;

: CSV-ROW-OVERALL ( -- )
   s" overall,-1," RB+ R-FIRST @ RB# CM R-LAST @ RB# CM R-SAMPLES @ RB# CM
   R-DUR F@ RB-FFIX3S CM R-RATE F@ RB-FFIX3S CM
   R-ACC-DYN-RMS F@ RB-FFIX6 CM R-ACC-DYN-MAX F@ RB-FFIX6 CM R-ACC-NORM-MEAN F@ RB-FFIX6 CM R-ACC-NORM-STD F@ RB-FFIX6 CM
   R-GYR-DYN-RMS F@ RB-FFIX6 CM R-GYR-DYN-MAX F@ RB-FFIX6 CM R-GYR-NORM-MEAN F@ RB-FFIX6 CM R-GYR-NORM-STD F@ RB-FFIX6 CM
   SM-R-ACC-RATIO SM-HAVE-ACC-RATIO @ RB-OPT6 CM
   SM-R-GYR-RATIO SM-HAVE-GYR-RATIO @ RB-OPT6 RB-NL ;

: CSV-ROW-WINDOW ( n -- )
   {: ix:n :}
   s" window," RB+ W-IX ix A@ RB# CM W-START ix A@ RB# CM W-END ix A@ RB# CM W-SAMPLES ix A@ RB# CM
   W-DUR ix FA@ RB-FFIX3S CM W-RATE ix FA@ RB-FFIX3S CM
   W-ACC-DYN-RMS ix FA@ RB-FFIX6 CM W-ACC-DYN-MAX ix FA@ RB-FFIX6 CM W-ACC-NORM-MEAN ix FA@ RB-FFIX6 CM W-ACC-NORM-STD ix FA@ RB-FFIX6 CM
   W-GYR-DYN-RMS ix FA@ RB-FFIX6 CM W-GYR-DYN-MAX ix FA@ RB-FFIX6 CM W-GYR-NORM-MEAN ix FA@ RB-FFIX6 CM W-GYR-NORM-STD ix FA@ RB-FFIX6 CM
   W-ACC-RATIO ix cells + W-HAVE-ACC-RATIO ix A@ RB-OPT6 CM
   W-GYR-RATIO ix cells + W-HAVE-GYR-RATIO ix A@ RB-OPT6 RB-NL ;

: METRICS-CSV$ ( -- ptr u8 n )
   COMPUTE-OVERALL
   RB-RESET
   CSV-HEADER
   CSV-ROW-OVERALL
   COMPUTE-WINDOWS
   0 SM-I !
   begin SM-I @ SM-WINDOW-N @ < while
      SM-I @ CSV-ROW-WINDOW
      SM-I @ 1+ SM-I !
   repeat
   RB$ ;

: MD-STAT-R ( ptr u8 n ptr a -- )
   {: label:ptr labelu:n p:ptr :}
   s" | " RB+ label labelu RB+ s"  | " RB+ p F@ RB-FFIX6 s"  |" RB+ RB-NL ;

: MD-STAT-N ( ptr u8 n n -- )
   {: label:ptr labelu:n v:n :}
   s" | " RB+ label labelu RB+ s"  | " RB+ v RB# s"  |" RB+ RB-NL ;

: MD-STAT-S ( ptr u8 n ptr u8 n -- )
   {: label:ptr labelu:n a:ptr u:n :}
   s" | " RB+ label labelu RB+ s"  | " RB+ a u RB+ s"  |" RB+ RB-NL ;

: MD-F3 ( ptr u8 n r -- )
   {: label:ptr labelu:n value:r :}
   s" - " RB+ label labelu RB+ s" : " RB+ value RB-FFIX3S RB-NL ;

: MD-F6-U ( ptr u8 n r ptr u8 n -- )
   {: label:ptr labelu:n value:r suffix:ptr suffixu:n :}
   s" - " RB+ label labelu RB+ s" : " RB+ value RB-FFIX6
   suffix suffixu RB+ RB-NL ;

: MD-STAT-F3 ( ptr u8 n ptr a -- )
   {: label:ptr labelu:n p:ptr :}
   s" | " RB+ label labelu RB+ s"  | " RB+ p F@ RB-FFIX3S s"  |" RB+ RB-NL ;

: MD-STAT-VEC ( ptr u8 n ptr a ptr a ptr a -- )
   {: label:ptr labelu:n x:ptr y:ptr z:ptr :}
   s" | " RB+ label labelu RB+ s"  | " RB+
   x F@ RB-FFIX6 s"  / " RB+ y F@ RB-FFIX6 s"  / " RB+ z F@ RB-FFIX6
   s"  |" RB+ RB-NL ;

: BASELINE-MD ( -- )
   SM-HAVE-BASELINE @ 0= if exit then
   s" baseline samples" SM-BASE-SAMPLES @ MD-STAT-N
   s" baseline accel dynamic RMS m/s^2" SM-BASE-ACC-DYN MD-STAT-R
   s" baseline gyro dynamic RMS rad/s" SM-BASE-GYR-DYN MD-STAT-R
   s" accel dynamic RMS ratio to baseline" SM-HAVE-ACC-RATIO @ if SM-R-ACC-RATIO else R-DUR then MD-STAT-R
   s" gyro dynamic RMS ratio to baseline" SM-HAVE-GYR-RATIO @ if SM-R-GYR-RATIO else R-DUR then MD-STAT-R ;

: PEAK-ACC-WINDOW ( -- n )
   -1 SM-BEST !
   0 SM-I !
   begin SM-I @ SM-WINDOW-N @ < while
      SM-BEST @ 0 < W-ACC-DYN-RMS SM-I @ FA@ W-ACC-DYN-RMS SM-BEST @ FA@ f> or if SM-I @ SM-BEST ! then
      SM-I @ 1+ SM-I !
   repeat
   SM-BEST @ ;

: PEAK-GYR-WINDOW ( -- n )
   -1 SM-BEST !
   0 SM-I !
   begin SM-I @ SM-WINDOW-N @ < while
      SM-BEST @ 0 < W-GYR-DYN-RMS SM-I @ FA@ W-GYR-DYN-RMS SM-BEST @ FA@ f> or if SM-I @ SM-BEST ! then
      SM-I @ 1+ SM-I !
   repeat
   SM-BEST @ ;

: SUMMARY-MD$ ( -- ptr u8 n )
   COMPUTE-OVERALL
   RB-RESET
   s" # SPSC Motion Analysis" RB+ RB-NL RB-NL
   s" input" INPUT$ MD-S
   s" baseline" SM-BASELINE-U @ 0= if s" none" else BASELINE$ then MD-S
   s" source" SOURCE$ MD-S
   s" device" DEVICE$ MD-S
   s" logical name" LOGICAL$ MD-S
   s" time domain" TIME-DOMAIN$ MD-S
   s" frame" FRAME$ MD-S
   s" window ms" SM-WINDOW-MS F@ MD-F3
   s" result" REPORT-RESULT$ MD-S
   RB-NL
   s" | metric | value |" RB+ RB-NL
   s" | --- | ---: |" RB+ RB-NL
   s" samples" SM-N @ MD-STAT-N
   s" first timestamp ns" SM-FIRST-TS @ MD-STAT-N
   s" last timestamp ns" SM-LAST-SAMPLE-TS @ MD-STAT-N
   s" duplicate timestamps" SM-DUP-TS @ MD-STAT-N
   s" timestamp regressions" SM-REG-TS @ MD-STAT-N
   s" period samples" SM-PERIOD-N @ MD-STAT-N
   s" period min ns" SM-PERIOD-MIN @ MD-STAT-N
   s" period max ns" SM-PERIOD-MAX @ MD-STAT-N
   s" period mean ns" SM-PERIOD-MEAN @ MD-STAT-N
   s" sample rate mean Hz" R-RATE MD-STAT-F3
   s" duration ms" R-DUR MD-STAT-F3
   s" accel mean x/y/z m/s^2" R-ACC-MX R-ACC-MY R-ACC-MZ MD-STAT-VEC
   s" accel dynamic RMS m/s^2" R-ACC-DYN-RMS MD-STAT-R
   s" accel dynamic max m/s^2" R-ACC-DYN-MAX MD-STAT-R
   s" accel norm mean m/s^2" R-ACC-NORM-MEAN MD-STAT-R
   s" accel norm stddev m/s^2" R-ACC-NORM-STD MD-STAT-R
   s" gyro mean x/y/z rad/s" R-GYR-MX R-GYR-MY R-GYR-MZ MD-STAT-VEC
   s" gyro dynamic RMS rad/s" R-GYR-DYN-RMS MD-STAT-R
   s" gyro dynamic max rad/s" R-GYR-DYN-MAX MD-STAT-R
   s" gyro norm mean rad/s" R-GYR-NORM-MEAN MD-STAT-R
   s" gyro norm stddev rad/s" R-GYR-NORM-STD MD-STAT-R
   BASELINE-MD
   COMPUTE-WINDOWS
   RB-NL s" ## Peak Windows" RB+ RB-NL RB-NL
   s" | metric | window | value |" RB+ RB-NL
   s" | --- | ---: | ---: |" RB+ RB-NL
   PEAK-ACC-WINDOW dup 0 >= if s" | accel dynamic RMS m/s^2 | " RB+ dup W-IX swap A@ RB# s"  | " RB+ W-ACC-DYN-RMS swap FA@ RB-FFIX6 s"  |" RB+ RB-NL else drop then
   PEAK-GYR-WINDOW dup 0 >= if s" | gyro dynamic RMS rad/s | " RB+ dup W-IX swap A@ RB# s"  | " RB+ W-GYR-DYN-RMS swap FA@ RB-FFIX6 s"  |" RB+ RB-NL else drop then
   RB-NL s" ## Windows" RB+ RB-NL RB-NL
   s" | window | samples | duration ms | accel dynamic RMS m/s^2 | gyro dynamic RMS rad/s | accel baseline ratio | gyro baseline ratio |" RB+ RB-NL
   s" | ---: | ---: | ---: | ---: | ---: | ---: | ---: |" RB+ RB-NL
   0 SM-I !
   begin SM-I @ SM-WINDOW-N @ < while
      s" | " RB+ W-IX SM-I @ A@ RB# s"  | " RB+ W-SAMPLES SM-I @ A@ RB# s"  | " RB+
      W-DUR SM-I @ FA@ RB-FFIX3S s"  | " RB+ W-ACC-DYN-RMS SM-I @ FA@ RB-FFIX6 s"  | " RB+ W-GYR-DYN-RMS SM-I @ FA@ RB-FFIX6 s"  | " RB+
      W-ACC-RATIO SM-I @ cells + W-HAVE-ACC-RATIO SM-I @ A@ RB-OPT6 s"  | " RB+
      W-GYR-RATIO SM-I @ cells + W-HAVE-GYR-RATIO SM-I @ A@ RB-OPT6 s"  |" RB+ RB-NL
      SM-I @ 1+ SM-I !
   repeat
   RB$ ;

: READINESS-CSV$ ( -- ptr u8 n )
   COMPUTE-OVERALL
   RB-RESET
   s" metric,value,threshold,pass" RB+ RB-NL
   s" samples," RB+ SM-N @ RB# s" ,>0," RB+ SAMPLE-CHECK CHECK$ RB+ RB-NL
   s" timestamp_errors," RB+ SM-DUP-TS @ SM-REG-TS @ + RB# s" ,0," RB+ TIMESTAMP-CHECK CHECK$ RB+ RB-NL
   s" accel_dynamic_rms_m_s2," RB+ R-ACC-DYN-RMS F@ RB-FFIX6 CM SM-HAVE-TH-ACC-RMS @ if SM-TH-ACC-RMS F@ RB-FFIX6 then CM ACC-RMS-CHECK CHECK$ RB+ RB-NL
   s" gyro_dynamic_rms_rad_s," RB+ R-GYR-DYN-RMS F@ RB-FFIX6 CM SM-HAVE-TH-GYR-RMS @ if SM-TH-GYR-RMS F@ RB-FFIX6 then CM GYR-RMS-CHECK CHECK$ RB+ RB-NL
   s" accel_dynamic_ratio_to_baseline," RB+ SM-R-ACC-RATIO SM-HAVE-ACC-RATIO @ RB-OPT6 CM SM-HAVE-TH-ACC-RATIO @ if SM-TH-ACC-RATIO F@ RB-FFIX6 then CM ACC-RATIO-CHECK CHECK$ RB+ RB-NL
   s" gyro_dynamic_ratio_to_baseline," RB+ SM-R-GYR-RATIO SM-HAVE-GYR-RATIO @ RB-OPT6 CM SM-HAVE-TH-GYR-RATIO @ if SM-TH-GYR-RATIO F@ RB-FFIX6 then CM GYR-RATIO-CHECK CHECK$ RB+ RB-NL
   s" result,,," RB+ READINESS-RESULT$ RB+ RB-NL
   RB$ ;

: READINESS-MD$ ( -- ptr u8 n )
   COMPUTE-OVERALL
   RB-RESET
   s" # SPSC Motion Readiness" RB+ RB-NL RB-NL
   s" result" READINESS-RESULT$ MD-S
   s" require ready" SM-REQUIRE-READY @ if s" yes" else s" no" then MD-S
   s" motion thresholds configured" THRESHOLD-COUNT MD-N
   RB-NL
   s" | metric | value | threshold | pass |" RB+ RB-NL
   s" | --- | ---: | ---: | --- |" RB+ RB-NL
   s" | samples | " RB+ SM-N @ RB# s"  | >0 | " RB+ SAMPLE-CHECK CHECK$ RB+ s"  |" RB+ RB-NL
   s" | timestamp errors | " RB+ SM-DUP-TS @ SM-REG-TS @ + RB# s"  | 0 | " RB+ TIMESTAMP-CHECK CHECK$ RB+ s"  |" RB+ RB-NL
   s" | accel dynamic RMS | " RB+ R-ACC-DYN-RMS F@ RB-FFIX6 s"  m/s^2 | " RB+ SM-HAVE-TH-ACC-RMS @ if SM-TH-ACC-RMS F@ RB-FFIX6 s"  m/s^2" RB+ then s"  | " RB+ ACC-RMS-CHECK CHECK$ RB+ s"  |" RB+ RB-NL
   s" | gyro dynamic RMS | " RB+ R-GYR-DYN-RMS F@ RB-FFIX6 s"  rad/s | " RB+ SM-HAVE-TH-GYR-RMS @ if SM-TH-GYR-RMS F@ RB-FFIX6 s"  rad/s" RB+ then s"  | " RB+ GYR-RMS-CHECK CHECK$ RB+ s"  |" RB+ RB-NL
   s" | accel dynamic ratio to baseline | " RB+ SM-R-ACC-RATIO SM-HAVE-ACC-RATIO @ RB-OPT6 s"  | " RB+ SM-HAVE-TH-ACC-RATIO @ if SM-TH-ACC-RATIO F@ RB-FFIX6 then s"  | " RB+ ACC-RATIO-CHECK CHECK$ RB+ s"  |" RB+ RB-NL
   s" | gyro dynamic ratio to baseline | " RB+ SM-R-GYR-RATIO SM-HAVE-GYR-RATIO @ RB-OPT6 s"  | " RB+ SM-HAVE-TH-GYR-RATIO @ if SM-TH-GYR-RATIO F@ RB-FFIX6 then s"  | " RB+ GYR-RATIO-CHECK CHECK$ RB+ s"  |" RB+ RB-NL
   RB-NL
   s" Readiness passes only when timestamp health is clean and every configured motion threshold passes. Ratio thresholds require a baseline log." RB+ RB-NL
   RB$ ;

: OUT-FILE$ ( ptr u8 n -- ptr u8 n )
   {: name:ptr nameu:n :}
   OUT$ name nameu SM-P0 JOIN-PATH SM-P0 swap ;

: WRITE-ARTIFACT ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n data:ptr datau:n :}
   name nameu OUT-FILE$ data datau WRITE-ALL ;

: ANALYZE-RUN ( -- n )
   OUT$ MAKE-DIRS
   BASELINE$ COMPUTE-BASELINE
   INPUT$ LOAD-LOG
   s" metrics.csv" METRICS-CSV$ WRITE-ARTIFACT
   s" summary.md" SUMMARY-MD$ WRITE-ARTIFACT
   s" readiness.csv" READINESS-CSV$ WRITE-ARTIFACT
   s" readiness.md" READINESS-MD$ WRITE-ARTIFACT
   SM-REQUIRE-READY @ 0= if 0 exit then
   READINESS-RESULT$ s" pass" STR= if 0 else 1 then ;

: FIELD$ ( ptr u8 n n -- ptr u8 n )
   {: a:ptr u:n want:n :}
   0 SM-START !
   0 SM-FIELD !
   0 SM-I !
   begin SM-I @ u <= while
      SM-I @ u = if
         SM-FIELD @ want = if a SM-START @ + SM-I @ SM-START @ - exit then
      else
         a SM-I @ + c@ 44 = if
            SM-FIELD @ want = if a SM-START @ + SM-I @ SM-START @ - exit then
            SM-FIELD @ 1+ SM-FIELD !
            SM-I @ 1+ SM-START !
         then
      then
      SM-I @ 1+ SM-I !
   repeat
   s" " ;

: CSV-FLOAT ( ptr u8 n -- r bool )
   dup 0= if 2drop 0.0 SM-FALSE exit then
   STR>FLOAT ;

create SC-LOGICAL SM-MAX-CAMERAS SM-TEXT-CAP * allot
create SC-CSVPATH SM-MAX-CAMERAS SM-PATH-CAP * allot
create SC-LOGICAL-U SM-MAX-CAMERAS cells allot
create SC-CSVPATH-U SM-MAX-CAMERAS cells allot
create SC-RESULT SM-MAX-CAMERAS cells allot
create SC-TSERR SM-MAX-CAMERAS cells allot
create SC-ACC SM-MAX-CAMERAS cells allot
create SC-GYR SM-MAX-CAMERAS cells allot
create SC-ACCR SM-MAX-CAMERAS cells allot
create SC-GYRR SM-MAX-CAMERAS cells allot
create SC-HAVE-ACCR SM-MAX-CAMERAS cells allot
create SC-HAVE-GYRR SM-MAX-CAMERAS cells allot

create SC-SCENARIO SM-TEXT-CAP allot
create SC-INPUT-ROOT SM-PATH-CAP allot

variable SC-N
variable SC-SCENARIO-U
variable SC-INPUT-ROOT-U
variable SC-FIRST-FAIL

: SC-SLOT ( n ptr u8 n -- ptr u8 ) {: ix:n base:ptr cap:n :} base ix cap * + ;
: SC-LOGICAL$ ( n -- ptr u8 n ) {: ix:n :} ix SC-LOGICAL SM-TEXT-CAP SC-SLOT SC-LOGICAL-U ix A@ ;
: SC-CSVPATH$ ( n -- ptr u8 n ) {: ix:n :} ix SC-CSVPATH SM-PATH-CAP SC-SLOT SC-CSVPATH-U ix A@ ;
: SC-COPY-FIXED ( ptr u8 n ptr u8 n -- )
   {: a:ptr u:n dst:ptr cap:n :}
   u cap >= if E-SM-CAPACITY throw then
   a dst u BYTE-COPY
   0 dst u + c! ;

: SC-RESULT-ID ( ptr u8 n -- n )
   2dup s" pass" STR= if 2drop 1 exit then
   2dup s" fail" STR= if 2drop 2 exit then
   2drop 0 ;

: SC-RESULT$ ( n -- ptr u8 n )
   dup 1 = if drop s" pass" exit then
   dup 2 = if drop s" fail" exit then
   drop s" not_run" ;

: SC-ADD-READINESS ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: logical:ptr logicalu:n path:ptr pathu:n csv:ptr csvu:n :}
   SC-N @ SM-MAX-CAMERAS >= if E-SM-CAPACITY throw then
   logical logicalu SC-N @ SC-LOGICAL SM-TEXT-CAP SC-SLOT SM-TEXT-CAP SC-COPY-FIXED
   path pathu SC-N @ SC-CSVPATH SM-PATH-CAP SC-SLOT SM-PATH-CAP SC-COPY-FIXED
   logicalu SC-LOGICAL-U SC-N @ A!
   pathu SC-CSVPATH-U SC-N @ A!
   0 SC-TSERR SC-N @ A!
   0.0 SC-ACC SC-N @ FA!
   0.0 SC-GYR SC-N @ FA!
   0 SC-HAVE-ACCR SC-N @ A!
   0 SC-HAVE-GYRR SC-N @ A!
   0 SC-RESULT SC-N @ A!
   csv csvu JSONL-START-STRICT
   begin JSONL-NEXT-ROW while
      drop drop drop
      JSONL-LINE$ {: line:ptr lineu:n :}
      lineu 0 > if
         line lineu 0 FIELD$ {: metric:ptr metricu:n :}
         line lineu 1 FIELD$ {: value:ptr valueu:n :}
         line lineu 3 FIELD$ {: pass:ptr passu:n :}
         metric metricu s" timestamp_errors" STR= if value valueu STR>NUMBER? drop SC-TSERR SC-N @ A! then
         metric metricu s" accel_dynamic_rms_m_s2" STR= if value valueu STR>FLOAT drop SC-ACC SC-N @ FA! then
         metric metricu s" gyro_dynamic_rms_rad_s" STR= if value valueu STR>FLOAT drop SC-GYR SC-N @ FA! then
         metric metricu s" accel_dynamic_ratio_to_baseline" STR= if value valueu CSV-FLOAT if SC-ACCR SC-N @ FA! 1 SC-HAVE-ACCR SC-N @ A! else fdrop then then
         metric metricu s" gyro_dynamic_ratio_to_baseline" STR= if value valueu CSV-FLOAT if SC-GYRR SC-N @ FA! 1 SC-HAVE-GYRR SC-N @ A! else fdrop then then
         metric metricu s" result" STR= if pass passu SC-RESULT-ID SC-RESULT SC-N @ A! then
      then
   repeat drop drop drop
   SC-N @ 1+ SC-N ! ;

: SC-RESULT-ALL$ ( -- ptr u8 n )
   SC-N @ 0= if s" fail" exit then
   0 SM-I !
   begin SM-I @ SC-N @ < while
      SC-RESULT SM-I @ A@ 1 <> if s" fail" exit then
      SM-I @ 1+ SM-I !
   repeat
   s" pass" ;

: SC-MAX-TS ( -- n )
   0 SM-MAX-N !
   0 SM-I !
   begin SM-I @ SC-N @ < while
      SC-TSERR SM-I @ A@ SM-MAX-N @ > if SC-TSERR SM-I @ A@ SM-MAX-N ! then
      SM-I @ 1+ SM-I !
   repeat
   SM-MAX-N @ ;

: SC-MAX-F ( ptr a -- r )
   {: base:ptr :}
   SM-MAX-R F0!
   0 SM-I !
   begin SM-I @ SC-N @ < while
      base SM-I @ FA@ SM-MAX-R F@ FMAX2 SM-MAX-R F!
      SM-I @ 1+ SM-I !
   repeat
   SM-MAX-R F@ ;

: SC-SUMMARY-CSV$ ( -- ptr u8 n )
   RB-RESET
   s" logical_name,result,timestamp_errors,accel_dynamic_rms_m_s2,gyro_dynamic_rms_rad_s,accel_dynamic_ratio_to_baseline,gyro_dynamic_ratio_to_baseline,readiness_csv" RB+ RB-NL
   0 SM-I !
   begin SM-I @ SC-N @ < while
      SM-I @ SC-LOGICAL$ RB+ CM SC-RESULT SM-I @ A@ SC-RESULT$ RB+ CM SC-TSERR SM-I @ A@ RB# CM
      SC-ACC SM-I @ FA@ RB-FFIX6 CM SC-GYR SM-I @ FA@ RB-FFIX6 CM
      SC-ACCR SM-I @ cells + SC-HAVE-ACCR SM-I @ A@ RB-OPT6 CM
      SC-GYRR SM-I @ cells + SC-HAVE-GYRR SM-I @ A@ RB-OPT6 CM
      SM-I @ SC-CSVPATH$ RB+ RB-NL
      SM-I @ 1+ SM-I !
   repeat
   RB$ ;

: SC-SUMMARY-MD$ ( -- ptr u8 n )
   RB-RESET
   s" # SPSC Motion Scenario Summary" RB+ RB-NL RB-NL
   s" scenario" SC-SCENARIO SC-SCENARIO-U @ MD-S
   s" input root" SC-INPUT-ROOT SC-INPUT-ROOT-U @ MD-S
   s" cameras" SC-N @ MD-N
   s" result" SC-RESULT-ALL$ MD-S
   s" max timestamp errors" SC-MAX-TS MD-N
   s" max accel dynamic RMS" SC-ACC SC-MAX-F s"  m/s^2" MD-F6-U
   s" max gyro dynamic RMS" SC-GYR SC-MAX-F s"  rad/s" MD-F6-U
   RB-NL
   s" | logical name | result | timestamp errors | accel dynamic RMS | gyro dynamic RMS | accel ratio | gyro ratio |" RB+ RB-NL
   s" | --- | --- | ---: | ---: | ---: | ---: | ---: |" RB+ RB-NL
   0 SM-I !
   begin SM-I @ SC-N @ < while
      s" | " RB+ SM-I @ SC-LOGICAL$ RB+ s"  | " RB+ SC-RESULT SM-I @ A@ SC-RESULT$ RB+ s"  | " RB+
      SC-TSERR SM-I @ A@ RB# s"  | " RB+ SC-ACC SM-I @ FA@ RB-FFIX6 s"  m/s^2 | " RB+
      SC-GYR SM-I @ FA@ RB-FFIX6 s"  rad/s | " RB+
      SC-ACCR SM-I @ cells + SC-HAVE-ACCR SM-I @ A@ RB-OPT6 s"  | " RB+
      SC-GYRR SM-I @ cells + SC-HAVE-GYRR SM-I @ A@ RB-OPT6 s"  |" RB+ RB-NL
      SM-I @ 1+ SM-I !
   repeat
   RB$ ;

public

: RESET ( -- ) RESET-ALL ;
: INPUT! ( ptr u8 n -- ) SET-INPUT ;
: BASELINE! ( ptr u8 n -- ) SET-BASELINE ;
: OUTPUT! ( ptr u8 n -- ) SET-OUT ;
: WINDOW-MS! ( r -- ) SM-WINDOW-MS F! ;
: MAX-ACCEL-DYNAMIC-RMS! ( r -- ) SM-TH-ACC-RMS F! 1 SM-HAVE-TH-ACC-RMS ! ;
: MAX-GYRO-DYNAMIC-RMS! ( r -- ) SM-TH-GYR-RMS F! 1 SM-HAVE-TH-GYR-RMS ! ;
: MAX-ACCEL-DYNAMIC-RATIO! ( r -- ) SM-TH-ACC-RATIO F! 1 SM-HAVE-TH-ACC-RATIO ! ;
: MAX-GYRO-DYNAMIC-RATIO! ( r -- ) SM-TH-GYR-RATIO F! 1 SM-HAVE-TH-GYR-RATIO ! ;
: REQUIRE-READY! ( -- ) 1 SM-REQUIRE-READY ! ;
: RUN ( -- n ) ANALYZE-RUN ;

: ANALYZE ( ptr u8 n ptr u8 n ptr u8 n -- n )
   {: input:ptr inputu:n base:ptr baseu:n out:ptr outu:n :}
   RESET-ALL
   input inputu SET-INPUT
   base baseu SET-BASELINE
   out outu SET-OUT
   ANALYZE-RUN ;

: SCENARIO-RESET ( ptr u8 n ptr u8 n -- )
   {: scenario:ptr scenariou:n root:ptr rootu:n :}
   0 SC-N !
   scenario scenariou SC-SCENARIO SM-TEXT-CAP SC-SCENARIO-U SM-COPY!
   root rootu SC-INPUT-ROOT SM-PATH-CAP SC-INPUT-ROOT-U SM-COPY! ;

: SCENARIO-READINESS+ ( ptr u8 n ptr u8 n ptr u8 n -- ) SC-ADD-READINESS ;
: SCENARIO-CSV$ ( -- ptr u8 n ) SC-SUMMARY-CSV$ ;
: SCENARIO-MD$ ( -- ptr u8 n ) SC-SUMMARY-MD$ ;
: SCENARIO-RESULT$ ( -- ptr u8 n ) SC-RESULT-ALL$ ;

end-package
