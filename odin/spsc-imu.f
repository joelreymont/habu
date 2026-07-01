\ spsc-imu.f - Habu SPSC BMI088 capture backend.
\
\ Opens a Stereolabs BMI088 SPSC device, maps its shared ring buffer, drains raw
\ samples, scales them to SI units, and writes odin.external_imu.v1 artifacts.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/time.f
require lib/render.f
require lib/float.f
require lib/prelude.f
require odin/float-cell.f

package SPSCIMU
private

32768 constant SI-MAP-LEN
8 constant SI-RING-STATE-LEN
20 constant SI-SAMPLE-SIZE
1024 constant SI-SAMPLE-CAP
8192 constant SI-MAX-SAMPLES
1024 constant SI-PATH-CAP
256 constant SI-TEXT-CAP
$180000 constant SI-OUT-CAP
1000000 constant SI-NS-PER-MS
1000000000 constant SI-NS-PER-S

-8600 constant E-SI-CAPACITY
-8601 constant E-SI-DEVICE
-8602 constant E-SI-MMAP
-8603 constant E-SI-RANGE

create SI-DEVICE-BUF SI-PATH-CAP allot
create SI-OUTPUT-BUF SI-PATH-CAP allot
create SI-LOGICAL-BUF SI-TEXT-CAP allot
create SI-TIME-BUF SI-TEXT-CAP allot
create SI-FRAME-BUF SI-TEXT-CAP allot
create SI-P0 SI-PATH-CAP allot
create SI-P1 SI-PATH-CAP allot
create SI-SYSFS-BUF 64 allot
create SI-OUT-BUF SI-OUT-CAP allot
create SI-SYSFS-READ 64 allot

create SI-TS SI-MAX-SAMPLES cells allot
create SI-ACX SI-MAX-SAMPLES cells allot
create SI-ACY SI-MAX-SAMPLES cells allot
create SI-ACZ SI-MAX-SAMPLES cells allot
create SI-GCX SI-MAX-SAMPLES cells allot
create SI-GCY SI-MAX-SAMPLES cells allot
create SI-GCZ SI-MAX-SAMPLES cells allot

create SI-ACC-SCALE 1 cells allot
create SI-GYR-SCALE 1 cells allot
create SI-ACC-MX 1 cells allot
create SI-ACC-MY 1 cells allot
create SI-ACC-MZ 1 cells allot
create SI-GYR-MX 1 cells allot
create SI-GYR-MY 1 cells allot
create SI-GYR-MZ 1 cells allot
create SI-ACC-NORM-MEAN 1 cells allot
create SI-ACC-NORM-RMS 1 cells allot
create SI-ACC-NORM-STD 1 cells allot
create SI-GYR-NORM-MEAN 1 cells allot
create SI-GYR-NORM-RMS 1 cells allot
create SI-GYR-NORM-STD 1 cells allot

variable SI-DEVICE-U
variable SI-OUTPUT-U
variable SI-LOGICAL-U
variable SI-TIME-U
variable SI-FRAME-U
variable SI-SAMPLES-REQ
variable SI-TIMEOUT-MS
variable SI-POLL-MS
variable SI-START-TIMER
variable SI-ACC-RANGE
variable SI-GYR-RANGE
variable SI-N
variable SI-TIMED-OUT
variable SI-DUP-TS
variable SI-REG-TS
variable SI-PERIOD-N
variable SI-PERIOD-MIN
variable SI-PERIOD-MAX
variable SI-PERIOD-MEAN
variable SI-HAVE-LAST
variable SI-LAST-TS
variable SI-I
variable SI-J
variable SI-OUT-U
variable SI-FD
variable SI-DEADLINE
variable SI-LAST-SLASH
variable SI-LOOP-LAST

TRUSTED: SI-MAP-PTR ( n -- ptr u8 ) ;

: SI-COPY! ( ptr u8 n ptr u8 n ptr a -- )
   {: a:ptr u:n dst:ptr cap:n lenp:ptr :}
   u 0 < if E-STR-CAPACITY throw then
   u cap >= if E-STR-CAPACITY throw then
   a dst u BYTE-COPY
   u lenp ! ;

: DEVICE$ ( -- ptr u8 n ) SI-DEVICE-BUF SI-DEVICE-U @ ;
: OUTPUT$ ( -- ptr u8 n ) SI-OUTPUT-BUF SI-OUTPUT-U @ ;
: LOGICAL$ ( -- ptr u8 n ) SI-LOGICAL-BUF SI-LOGICAL-U @ ;
: TIME-DOMAIN$ ( -- ptr u8 n ) SI-TIME-BUF SI-TIME-U @ ;
: FRAME$ ( -- ptr u8 n ) SI-FRAME-BUF SI-FRAME-U @ ;

: SET-DEVICE ( ptr u8 n -- ) SI-DEVICE-BUF SI-PATH-CAP SI-DEVICE-U SI-COPY! ;
: SET-OUTPUT ( ptr u8 n -- ) SI-OUTPUT-BUF SI-PATH-CAP SI-OUTPUT-U SI-COPY! ;
: SET-LOGICAL ( ptr u8 n -- ) SI-LOGICAL-BUF SI-TEXT-CAP SI-LOGICAL-U SI-COPY! ;
: SET-TIME ( ptr u8 n -- ) SI-TIME-BUF SI-TEXT-CAP SI-TIME-U SI-COPY! ;
: SET-FRAME ( ptr u8 n -- ) SI-FRAME-BUF SI-TEXT-CAP SI-FRAME-U SI-COPY! ;

: A@ ( ptr a n -- n ) {: base:ptr ix:n :} base ix cells + @ ;
: A! ( n ptr a n -- ) {: v:n base:ptr ix:n :} v base ix cells + ! ;
: F0! ( ptr a -- ) 0.0 swap F! ;

: SI-RESET-STATS ( -- )
   0 SI-N !
   0 SI-TIMED-OUT !
   0 SI-DUP-TS !
   0 SI-REG-TS !
   0 SI-PERIOD-N !
   0 SI-PERIOD-MIN !
   0 SI-PERIOD-MAX !
   0 SI-PERIOD-MEAN !
   0 SI-HAVE-LAST !
   0 SI-LAST-TS !
   SI-ACC-MX F0!
   SI-ACC-MY F0!
   SI-ACC-MZ F0!
   SI-GYR-MX F0!
   SI-GYR-MY F0!
   SI-GYR-MZ F0!
   SI-ACC-NORM-MEAN F0!
   SI-ACC-NORM-RMS F0!
   SI-ACC-NORM-STD F0!
   SI-GYR-NORM-MEAN F0!
   SI-GYR-NORM-RMS F0!
   SI-GYR-NORM-STD F0! ;

: SI-RESET ( -- )
   s" /dev/spsc_bmi0" SET-DEVICE
   s" results/imu/spsc_bmi" SET-OUTPUT
   s" cam_a0" SET-LOGICAL
   s" host_monotonic_ns" SET-TIME
   s" sensor" SET-FRAME
   200 SI-SAMPLES-REQ !
   5000 SI-TIMEOUT-MS !
   50 SI-POLL-MS !
   0 SI-START-TIMER !
   2 SI-ACC-RANGE !
   1 SI-GYR-RANGE !
   SI-RESET-STATS ;

: LE16@ ( ptr u8 -- n ) {: a:ptr :}
   a c@ a 1 + c@ 8 lshift or ;

: S16@ ( ptr u8 -- n )
   LE16@ dup 32767 > if 65536 - then ;

: LE32@ ( ptr u8 -- n ) {: a:ptr :}
   a c@ a 1 + c@ 8 lshift or a 2 + c@ 16 lshift or a 3 + c@ 24 lshift or ;

: LE64@ ( ptr u8 -- n ) {: a:ptr :}
   a c@ a 1 + c@ 8 lshift or a 2 + c@ 16 lshift or a 3 + c@ 24 lshift or
   a 4 + c@ 32 lshift or a 5 + c@ 40 lshift or a 6 + c@ 48 lshift or a 7 + c@ 56 lshift or ;

: LE32! ( n ptr u8 -- )
   {: v:n a:ptr :}
   v $FF and a c!
   v 8 rshift $FF and a 1 + c!
   v 16 rshift $FF and a 2 + c!
   v 24 rshift $FF and a 3 + c! ;

: RING-HEAD ( ptr u8 -- n ) LE32@ SI-SAMPLE-CAP 1- and ;
: RING-TAIL ( ptr u8 -- n ) 4 + LE32@ SI-SAMPLE-CAP 1- and ;
: RING-STORE-TAIL ( n ptr u8 -- ) 4 + LE32! ;
: SAMPLE-OFFSET ( n -- n ) SI-SAMPLE-CAP 1- and SI-SAMPLE-SIZE * SI-RING-STATE-LEN + ;

: ADD-RAW ( n n n n n n n -- )
   {: ts:n acx:n acy:n acz:n gcx:n gcy:n gcz:n :}
   SI-N @ SI-MAX-SAMPLES >= if E-SI-CAPACITY throw then
   SI-HAVE-LAST @ 0 <> if
      ts SI-LAST-TS @ = if SI-DUP-TS @ 1+ SI-DUP-TS ! then
      ts SI-LAST-TS @ < if SI-REG-TS @ 1+ SI-REG-TS ! then
   then
   ts SI-LAST-TS !
   1 SI-HAVE-LAST !
   ts SI-TS SI-N @ A!
   acx SI-ACX SI-N @ A!
   acy SI-ACY SI-N @ A!
   acz SI-ACZ SI-N @ A!
   gcx SI-GCX SI-N @ A!
   gcy SI-GCY SI-N @ A!
   gcz SI-GCZ SI-N @ A!
   SI-N @ 1+ SI-N ! ;

: DECODE-SAMPLE ( ptr u8 n -- )
   {: ring:ptr ix:n :}
   ring ix SAMPLE-OFFSET + {: base:ptr :}
   base LE64@
   base 14 + S16@ base 16 + S16@ base 18 + S16@
   base 8 + S16@ base 10 + S16@ base 12 + S16@
   ADD-RAW ;

: DRAIN-RING ( ptr u8 -- )
   {: ring:ptr :}
   begin SI-N @ SI-SAMPLES-REQ @ < while
      ring RING-TAIL {: tail:n :}
      tail ring RING-HEAD = if exit then
      ring tail DECODE-SAMPLE
      tail 1+ SI-SAMPLE-CAP 1- and ring RING-STORE-TAIL
   repeat ;

: FLUSH-RING ( ptr u8 -- )
   dup RING-HEAD swap RING-STORE-TAIL ;

: BASENAME$ ( ptr u8 n -- ptr u8 n )
   {: a:ptr u:n :}
   -1 SI-LAST-SLASH !
   0 SI-I !
   begin SI-I @ u < while
      a SI-I @ + c@ 47 = if SI-I @ SI-LAST-SLASH ! then
      SI-I @ 1+ SI-I !
   repeat
   SI-LAST-SLASH @ 0 < if a u exit then
   a SI-LAST-SLASH @ 1+ + u SI-LAST-SLASH @ 1+ - ;

: SYSFS$ ( ptr u8 n -- ptr u8 n )
   {: field:ptr fieldu:n :}
   SB-RESET
   s" /sys/class/bmi_spsc/" SB-APPEND
   DEVICE$ BASENAME$ SB-APPEND
   47 SB-APPEND-C
   field fieldu SB-APPEND
   SB$ ;

: PARSE-SYSFS-N ( ptr u8 n -- n )
   SI-SYSFS-READ 64 READ-ALL {: u:n :}
   SI-SYSFS-READ u TRIM STR>NUMBER? 0= if drop E-SI-RANGE throw then ;

: TRY-ACCEL-RANGE ( -- )
   s" accel_range" SYSFS$ PARSE-SYSFS-N SI-ACC-RANGE ! ;

: TRY-GYRO-RANGE ( -- )
   s" gyro_range" SYSFS$ PARSE-SYSFS-N SI-GYR-RANGE ! ;

: READ-SCALE-CONFIG ( -- )
   [: TRY-ACCEL-RANGE ;] catch 0 <> if 2 SI-ACC-RANGE ! then
   [: TRY-GYRO-RANGE ;] catch 0 <> if 1 SI-GYR-RANGE ! then ;

: WRITE-TIMER ( ptr u8 n -- )
   {: a:ptr u:n :}
   s" timer_control" SYSFS$ a u WRITE-ALL ;

: TIMER-START ( -- )
   s" 1" WRITE-TIMER ;

: TIMER-STOP ( -- )
   s" 0" WRITE-TIMER ;

: ACCEL-SCALE ( n -- r )
   dup 0 = if drop 3.0 else
   dup 1 = if drop 6.0 else
   dup 2 = if drop 12.0 else
   dup 3 = if drop 24.0 else
      drop E-SI-RANGE throw
   then then then then
   9.80665 f* 32768.0 f/ ;

: GYRO-SCALE ( n -- r )
   dup 0 = if drop 2000.0 else
   dup 1 = if drop 1000.0 else
   dup 2 = if drop 500.0 else
   dup 3 = if drop 250.0 else
   dup 4 = if drop 125.0 else
      drop E-SI-RANGE throw
   then then then then then
   3.141592653589793 f* 180.0 f/ 32768.0 f/ ;

: UPDATE-SCALES ( -- )
   SI-ACC-RANGE @ ACCEL-SCALE SI-ACC-SCALE F!
   SI-GYR-RANGE @ GYRO-SCALE SI-GYR-SCALE F! ;

: RAW-ACC ( ptr a n -- r ) A@ s>f SI-ACC-SCALE F@ f* ;
: RAW-GYR ( ptr a n -- r ) A@ s>f SI-GYR-SCALE F@ f* ;

: VEC-NORM ( r r r -- r ) {: x:r y:r z:r :} x x f* y y f* f+ z z f* f+ fsqrt ;
: FMAX2 ( r r -- r ) {: a:r b:r :} a b f> if a else b then ;
: FMIN2 ( r r -- r ) {: a:r b:r :} a b f< if a else b then ;

: BUILD-STATS ( -- )
   SI-N @ 0= if exit then
   0 SI-PERIOD-N !
   0 SI-PERIOD-MIN !
   0 SI-PERIOD-MAX !
   0 SI-J !
   SI-TS 0 A@ SI-LOOP-LAST !
   1 SI-I !
   begin SI-I @ SI-N @ < while
      SI-TS SI-I @ A@ {: ts:n :}
      ts SI-LOOP-LAST @ >= if
         ts SI-LOOP-LAST @ - {: per:n :}
         SI-PERIOD-N @ 0= if per SI-PERIOD-MIN ! per SI-PERIOD-MAX ! else
            per SI-PERIOD-MIN @ < if per SI-PERIOD-MIN ! then
            per SI-PERIOD-MAX @ > if per SI-PERIOD-MAX ! then
         then
         SI-J @ per + SI-J !
         SI-PERIOD-N @ 1+ SI-PERIOD-N !
      then
      ts SI-LOOP-LAST !
      SI-I @ 1+ SI-I !
   repeat
   SI-PERIOD-N @ 0 > if SI-J @ SI-PERIOD-N @ / SI-PERIOD-MEAN ! then ;

: SAMPLE-RATE ( -- r )
   SI-PERIOD-MEAN @ 0 > if 1000000000.0 SI-PERIOD-MEAN @ s>f f/ else 0.0 then ;

: OUT-C ( n -- )
   SI-OUT-U @ SI-OUT-CAP >= if E-SI-CAPACITY throw then
   SI-OUT-BUF SI-OUT-U @ + c!
   SI-OUT-U @ 1+ SI-OUT-U ! ;

: OUT+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   SI-OUT-U @ u + SI-OUT-CAP >= if E-SI-CAPACITY throw then
   a SI-OUT-BUF SI-OUT-U @ + u BYTE-COPY
   SI-OUT-U @ u + SI-OUT-U ! ;

: OUT# ( n -- )
   dup 0 < if 45 OUT-C negate then
   dup 10 < if 48 + OUT-C exit then
   dup 10 / RECURSE 10 mod 48 + OUT-C ;

: OUT-6 ( n -- )
   {: x:n :}
   x 100000 / 48 + OUT-C x 100000 mod 10000 / 48 + OUT-C x 10000 mod 1000 / 48 + OUT-C
   x 1000 mod 100 / 48 + OUT-C x 100 mod 10 / 48 + OUT-C x 10 mod 48 + OUT-C ;

: OUT-F6 ( r -- )
   fdup 0.0 f< if 45 OUT-C fnegate then
   1000000.0 f* 0.5 f+ f>s {: scaled:n :}
   scaled 1000000 / OUT# 46 OUT-C scaled 1000000 mod OUT-6 ;

: OUT-Q ( ptr u8 n -- ) 34 OUT-C OUT+ 34 OUT-C ;
: OUT-K ( ptr u8 n -- ) OUT-Q 58 OUT-C ;
: OUT-COMMA ( -- ) 44 OUT-C ;
: OUT-NL ( -- ) 10 OUT-C ;

: RENDER-SCHEMA ( -- )
   123 OUT-C
   s" type" OUT-K s" schema" OUT-Q OUT-COMMA
   s" schema_version" OUT-K s" odin.external_imu.v1" OUT-Q OUT-COMMA
   s" source" OUT-K s" stereolabs_bmi088_spsc" OUT-Q OUT-COMMA
   s" device" OUT-K DEVICE$ OUT-Q OUT-COMMA
   s" logical_name" OUT-K LOGICAL$ OUT-Q OUT-COMMA
   s" time_domain" OUT-K TIME-DOMAIN$ OUT-Q OUT-COMMA
   s" frame" OUT-K FRAME$ OUT-Q OUT-COMMA
   s" raw_units" OUT-K s" bmi088_counts" OUT-Q OUT-COMMA
   s" accel_range" OUT-K SI-ACC-RANGE @ OUT# OUT-COMMA
   s" gyro_range" OUT-K SI-GYR-RANGE @ OUT#
   125 OUT-C OUT-NL ;

: RENDER-SAMPLE ( n -- )
   {: ix:n :}
   123 OUT-C
   s" type" OUT-K s" imu_sample" OUT-Q OUT-COMMA
   s" schema_version" OUT-K s" odin.external_imu.v1" OUT-Q OUT-COMMA
   s" sample_index" OUT-K ix OUT# OUT-COMMA
   s" imu_timestamp_ns" OUT-K SI-TS ix A@ OUT# OUT-COMMA
   s" time_domain" OUT-K TIME-DOMAIN$ OUT-Q OUT-COMMA
   s" frame" OUT-K FRAME$ OUT-Q OUT-COMMA
   s" accel_m_s2" OUT-K 91 OUT-C
   SI-ACX ix RAW-ACC OUT-F6 OUT-COMMA SI-ACY ix RAW-ACC OUT-F6 OUT-COMMA SI-ACZ ix RAW-ACC OUT-F6
   93 OUT-C OUT-COMMA
   s" gyro_rad_s" OUT-K 91 OUT-C
   SI-GCX ix RAW-GYR OUT-F6 OUT-COMMA SI-GCY ix RAW-GYR OUT-F6 OUT-COMMA SI-GCZ ix RAW-GYR OUT-F6
   93 OUT-C OUT-COMMA
   s" raw_accel_counts" OUT-K 91 OUT-C
   SI-ACX ix A@ OUT# OUT-COMMA SI-ACY ix A@ OUT# OUT-COMMA SI-ACZ ix A@ OUT#
   93 OUT-C OUT-COMMA
   s" raw_gyro_counts" OUT-K 91 OUT-C
   SI-GCX ix A@ OUT# OUT-COMMA SI-GCY ix A@ OUT# OUT-COMMA SI-GCZ ix A@ OUT#
   93 OUT-C 125 OUT-C OUT-NL ;

: NDJSON$ ( -- ptr u8 n )
   0 SI-OUT-U !
   RENDER-SCHEMA
   0 SI-I !
   begin SI-I @ SI-N @ < while
      SI-I @ RENDER-SAMPLE
      SI-I @ 1+ SI-I !
   repeat
   SI-OUT-BUF SI-OUT-U @ ;

: RESULT$ ( -- ptr u8 n )
   SI-TIMED-OUT @ 0= SI-N @ SI-SAMPLES-REQ @ = and SI-DUP-TS @ 0= and SI-REG-TS @ 0= and if
      s" pass"
   else
      s" fail"
   then ;

: SUMMARY$ ( -- ptr u8 n )
   BUILD-STATS
   RB-RESET
   s" # SPSC BMI088 IMU Capture" RB+ RB-NL RB-NL
   s" device" DEVICE$ MD-S
   s" logical name" LOGICAL$ MD-S
   s" samples requested" SI-SAMPLES-REQ @ MD-N
   s" samples read" SI-N @ MD-N
   s" timed out" SI-TIMED-OUT @ if s" yes" else s" no" then MD-S
   s" start timer" SI-START-TIMER @ if s" yes" else s" no" then MD-S
   s" time domain" TIME-DOMAIN$ MD-S
   s" frame" FRAME$ MD-S
   s" accel range code" SI-ACC-RANGE @ MD-N
   s" gyro range code" SI-GYR-RANGE @ MD-N
   s" result" RESULT$ MD-S
   RB-NL
   s" | metric | value |" RB+ RB-NL
   s" | --- | ---: |" RB+ RB-NL
   s" | first timestamp ns | " RB+ SI-N @ 0 > if SI-TS 0 A@ else 0 then RB# s"  |" RB+ RB-NL
   s" | last timestamp ns | " RB+ SI-N @ 0 > if SI-TS SI-N @ 1- A@ else 0 then RB# s"  |" RB+ RB-NL
   s" | duplicate timestamps | " RB+ SI-DUP-TS @ RB# s"  |" RB+ RB-NL
   s" | timestamp regressions | " RB+ SI-REG-TS @ RB# s"  |" RB+ RB-NL
   s" | period samples | " RB+ SI-PERIOD-N @ RB# s"  |" RB+ RB-NL
   s" | period min ns | " RB+ SI-PERIOD-MIN @ RB# s"  |" RB+ RB-NL
   s" | period max ns | " RB+ SI-PERIOD-MAX @ RB# s"  |" RB+ RB-NL
   s" | period mean ns | " RB+ SI-PERIOD-MEAN @ RB# s"  |" RB+ RB-NL
   s" | sample rate mean Hz | " RB+ SAMPLE-RATE RB-FFIX3 s"  |" RB+ RB-NL
   RB$ ;

: PATH-IN-OUT$ ( ptr u8 n -- ptr u8 n )
   {: name:ptr nameu:n :}
   OUTPUT$ name nameu SI-P0 JOIN-PATH SI-P0 swap ;

: WRITE-OUTPUTS ( -- )
   OUTPUT$ MAKE-DIRS
   s" imu.ndjson" PATH-IN-OUT$ NDJSON$ WRITE-ALL
   s" summary.md" PATH-IN-OUT$ SUMMARY$ WRITE-ALL ;

: CAPTURE-LOOP ( ptr u8 -- )
   {: ring:ptr :}
   TIME-MONO-NS SI-TIMEOUT-MS @ SI-NS-PER-MS * + SI-DEADLINE !
   begin SI-N @ SI-SAMPLES-REQ @ < while
      ring DRAIN-RING
      SI-N @ SI-SAMPLES-REQ @ >= if exit then
      TIME-MONO-NS SI-DEADLINE @ >= if 1 SI-TIMED-OUT ! exit then
      SI-FD @ SI-POLL-MS @ POLL-IN drop
   repeat ;

: OPEN-DEVICE ( -- )
   DEVICE$ FS-PATHZ FS-O-RDWR 0 open SI-FD !
   SI-FD @ 0 < if E-FS-OPEN throw then ;

: MAP-DEVICE ( -- ptr u8 )
   0 SI-MAP-LEN MEM-PROT-RW MEM-MAP-SHARED SI-FD @ 0 mmap
   dup 0 < if drop E-SI-MMAP throw then
   SI-MAP-PTR ;

: RUN-CAPTURE ( -- n )
   SI-RESET-STATS
   READ-SCALE-CONFIG
   UPDATE-SCALES
   OPEN-DEVICE
   SI-START-TIMER @ if [: TIMER-START ;] catch drop then
   MAP-DEVICE {: ring:ptr :}
   SI-START-TIMER @ if ring FLUSH-RING then
   ring CAPTURE-LOOP
   SI-START-TIMER @ if [: TIMER-STOP ;] catch drop then
   SI-FD @ close
   WRITE-OUTPUTS
   RESULT$ s" pass" STR= if 0 else 1 then ;

public

: RESET ( -- ) SI-RESET ;
: DEVICE! ( ptr u8 n -- ) SET-DEVICE ;
: OUTPUT! ( ptr u8 n -- ) SET-OUTPUT ;
: LOGICAL! ( ptr u8 n -- ) SET-LOGICAL ;
: TIME-DOMAIN! ( ptr u8 n -- ) SET-TIME ;
: FRAME! ( ptr u8 n -- ) SET-FRAME ;
: SAMPLES! ( n -- ) SI-SAMPLES-REQ ! ;
: TIMEOUT-MS! ( n -- ) SI-TIMEOUT-MS ! ;
: POLL-MS! ( n -- ) SI-POLL-MS ! ;
: START! ( -- ) 1 SI-START-TIMER ! ;
: NO-START! ( -- ) 0 SI-START-TIMER ! ;
: RUN ( -- n ) RUN-CAPTURE ;
: SUMMARY ( -- ptr u8 n ) SUMMARY$ ;

: TEST-RESET-STATS ( -- ) SI-RESET-STATS ;
: TEST-ADD-RAW ( n n n n n n n -- ) ADD-RAW ;
: TEST-NDJSON$ ( -- ptr u8 n ) UPDATE-SCALES NDJSON$ ;
: TEST-SUMMARY$ ( -- ptr u8 n ) UPDATE-SCALES SUMMARY$ ;
: TEST-ACCEL-RANGE! ( n -- ) SI-ACC-RANGE ! ;
: TEST-GYRO-RANGE! ( n -- ) SI-GYR-RANGE ! ;
: TEST-DECODE-SAMPLE ( ptr u8 n -- ) DECODE-SAMPLE ;

end-package
