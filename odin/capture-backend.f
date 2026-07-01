\ capture-backend.f - Habu-owned CameraOne saved-image capture backend.
\
\ This drives the thin libodin_zed_capture.so C ABI directly from Habu:
\ camera config loading, concurrent grab loops, saved grayscale P5 frames, and
\ odin.capture.v1 NDJSON schema/frame/summary rows.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/ffi.f
require lib/time.f
require lib/fs.f
require lib/fs-mutate.f
require lib/task.f
require lib/render.f
require lib/float.f
require lib/fmt.f
require tools/json.f
require odin/float-cell.f
require odin/netpbm.f

package HCAP
private

4 constant MAX-CAMERAS
6 constant OUT-SLOTS
64 constant CSTR-CAP
1024 constant PATH-CAP
512 constant ERR-CAP
$200000 constant OUT-CAP
$10000 constant CONFIG-CAP
1000000 constant NS-PER-MS

0 constant FRAME-IMAGE-TS
8 constant FRAME-DROPPED
16 constant FRAME-STATUS
24 constant FRAME-BYTES

26 constant CAM-CELLS
0 constant CAM-ABI-INDEX
1 constant CAM-WIDTH
2 constant CAM-HEIGHT
3 constant CAM-FRAMES
4 constant CAM-DROPS
5 constant CAM-DUPLICATES
6 constant CAM-REGRESSIONS
7 constant CAM-WRITER-STALLS
8 constant CAM-LAST-TS
9 constant CAM-BASE-DROPS
10 constant CAM-LAST-DROPS
11 constant CAM-SAVED
12 constant CAM-OPENED
13 constant CAM-HAS-EXP
14 constant CAM-EXP
15 constant CAM-HAS-GAIN
16 constant CAM-GAIN
17 constant CAM-HAS-AEC
18 constant CAM-AEC
19 constant CAM-IMG-PTR
20 constant CAM-IMG-W
21 constant CAM-IMG-H
22 constant CAM-IMG-STEP
23 constant CAM-SENSOR-LAST-TS
24 constant CAM-SENSOR-DUPS
25 constant CAM-SENSOR-MISSING

5 constant CLK-CELLS
0 constant CLK-START
1 constant CLK-DEADLINE
2 constant CLK-ABORT
3 constant CLK-ARRIVED
4 constant CLK-RELEASED

-8200 constant E-HCAP-USAGE
-8201 constant E-HCAP-CAPACITY
-8202 constant E-HCAP-ABI
-8203 constant E-HCAP-CAMERA
-8204 constant E-HCAP-CONFIG

: TRUE ( -- bool ) 0 0= ;
: FALSE ( -- bool ) TRUE 0= ;
: ASSERT ( bool -- ) 0= if 77 throw then ;

create ABI-PATH PATH-CAP allot
create CONFIG-PATH PATH-CAP allot
create OUTPUT-DIR PATH-CAP allot
create RAW-DIR PATH-CAP allot
create RAW-PATH PATH-CAP allot
create COMBINED-PATH PATH-CAP allot
create SUMMARY-PATH PATH-CAP allot
create SUMMARY-MD-PATH PATH-CAP allot
create IMAGE-ROOT PATH-CAP allot
create SAVE-PATH PATH-CAP allot
create REL-PATH PATH-CAP allot
create RESOLUTION CSTR-CAP allot
create SDK-VERSION CSTR-CAP allot
create SYM-BUF CSTR-CAP allot
create CONFIG-BUF CONFIG-CAP allot
create SENSOR-AVAIL MAX-CAMERAS cells allot
create SENSOR-TS MAX-CAMERAS cells allot
create SENSOR-RATE MAX-CAMERAS 4 * allot
create SENSOR-ACCEL MAX-CAMERAS 3 * 4 * allot
create SENSOR-GYRO MAX-CAMERAS 3 * 4 * allot

variable ABI-U
variable CONFIG-U
variable OUTPUT-U
variable RAW-U
variable RAW-PATH-U
variable COMBINED-U
variable SUMMARY-U
variable SUMMARY-MD-U
variable IMAGE-ROOT-U
variable SAVE-PATH-U
variable REL-PATH-U
variable RESOLUTION-U
variable SDK-VERSION-U
variable CONFIG-BUF-U

create CAM-SERIAL MAX-CAMERAS CSTR-CAP * allot
create CAM-LOGICAL MAX-CAMERAS CSTR-CAP * allot
create CAM-SERIAL-U MAX-CAMERAS cells allot
create CAM-LOGICAL-U MAX-CAMERAS cells allot
variable CAMERA-N

create ERR-BUFS OUT-SLOTS ERR-CAP * allot
create FRAMES OUT-SLOTS FRAME-BYTES * allot
create OUT-BUFS OUT-SLOTS OUT-CAP * allot
create OUT-LENS OUT-SLOTS cells allot

here 7 and 8 swap - 7 and allot
create CAM-STATE MAX-CAMERAS CAM-CELLS * cells allot
here 7 and 8 swap - 7 and allot
create CLOCK CLK-CELLS cells allot
here 7 and 8 swap - 7 and allot
create SET-EXP 1 cells allot
create SET-HAS-EXP 1 cells allot
create SET-GAIN 1 cells allot
create SET-HAS-GAIN 1 cells allot
create SET-AEC 1 cells allot
create SET-HAS-AEC 1 cells allot

TASK:MIN-STACK TASK:TASK ACQ-TASK0
TASK:MIN-STACK TASK:TASK ACQ-TASK1
TASK:MIN-STACK TASK:TASK ACQ-TASK2
TASK:MIN-STACK TASK:TASK ACQ-TASK3

variable LIB
variable HANDLE
variable FN-CREATE
variable FN-DESTROY
variable FN-OPEN
variable FN-INFO
variable FN-GRAB
variable FN-SETTINGS
variable FN-SAVE-COLOR
variable FN-RETRIEVE-IMAGE
variable FN-SENSORS
variable FN-SDK-VERSION
variable FN-CLOSE

variable FPS
variable DURATION-MS
variable WARMUP-MS
variable METADATA-EVERY
variable SAVE-EVERY
variable MAX-SAVED-FRAMES
variable MANUAL-EXPOSURE
variable MANUAL-GAIN
variable INCLUDE-SENSOR
variable HCAP-I
variable SUM
variable TMP-DUP
variable TMP-REG
variable TMP-SAVED
variable TMP-SENSOR-DUP

: COPY-Z ( ptr u8 n ptr u8 n ptr a -- )
   {: src:ptr u:n dst:ptr cap:n lenp:ptr :}
   u 0 < if E-HCAP-CAPACITY throw then
   u cap >= if E-HCAP-CAPACITY throw then
   src dst u BYTE-COPY
   0 dst u + c!
   u lenp ! ;

: SLOT-CSTR ( n ptr u8 -- ptr u8 )
   swap CSTR-CAP * + ;
: CAM-SERIAL-SLOT ( n -- ptr u8 ) CAM-SERIAL SLOT-CSTR ;
: CAM-LOGICAL-SLOT ( n -- ptr u8 ) CAM-LOGICAL SLOT-CSTR ;
: CAM-SERIAL-U-P ( n -- ptr a ) cells CAM-SERIAL-U + ;
: CAM-LOGICAL-U-P ( n -- ptr a ) cells CAM-LOGICAL-U + ;
: CAM-SERIAL$ ( n -- ptr u8 n ) dup CAM-SERIAL-SLOT swap CAM-SERIAL-U-P @ ;
: CAM-LOGICAL$ ( n -- ptr u8 n ) dup CAM-LOGICAL-SLOT swap CAM-LOGICAL-U-P @ ;

: ERR-SLOT ( n -- ptr u8 ) ERR-CAP * ERR-BUFS + ;
: FRAME-SLOT ( n -- ptr u8 ) FRAME-BYTES * FRAMES + ;
: OUT-SLOT ( n -- ptr u8 ) OUT-CAP * OUT-BUFS + ;
: OUT-LENP ( n -- ptr a ) cells OUT-LENS + ;
: CAM-P ( n n -- ptr a ) {: cam:n off:n :} cam CAM-CELLS * off + cells CAM-STATE + ;
: CLK-P ( n -- ptr a ) cells CLOCK + ;
: SENSOR-AVAIL-P ( n -- ptr a ) cells SENSOR-AVAIL + ;
: SENSOR-TS-P ( n -- ptr a ) cells SENSOR-TS + ;
: SENSOR-RATE-P ( n -- ptr u8 ) 4 * SENSOR-RATE + ;
: SENSOR-ACCEL-P ( n -- ptr u8 ) 12 * SENSOR-ACCEL + ;
: SENSOR-GYRO-P ( n -- ptr u8 ) 12 * SENSOR-GYRO + ;

: ZERO-CELLS ( ptr a n -- )
   {: p:ptr count:n :}
   0 begin dup count < while
      0 p over cells + !
      1+
   repeat drop ;

: ZERO-STATE ( -- )
   CAM-STATE MAX-CAMERAS CAM-CELLS * ZERO-CELLS
   CLOCK CLK-CELLS ZERO-CELLS
   OUT-LENS OUT-SLOTS ZERO-CELLS ;

: PATH-END-SLASH? ( ptr u8 n -- bool )
   dup 0 > if 1- + c@ 47 = else 2drop FALSE then ;

: DEFAULTS ( -- )
   s" libodin_zed_capture.so" ABI-PATH PATH-CAP ABI-U COPY-Z
   s" configs/cameras.json" CONFIG-PATH PATH-CAP CONFIG-U COPY-Z
   s" results/capture/latest" OUTPUT-DIR PATH-CAP OUTPUT-U COPY-Z
   s" HD1200" RESOLUTION CSTR-CAP RESOLUTION-U COPY-Z
   60 FPS !
   10000 DURATION-MS !
   5000 WARMUP-MS !
   120 METADATA-EVERY !
   60 SAVE-EVERY !
   20 MAX-SAVED-FRAMES !
   -1 MANUAL-EXPOSURE !
   -1 MANUAL-GAIN !
   0 INCLUDE-SENSOR !
   0 CAMERA-N !
   0 LIB !
   0 HANDLE !
   0 SDK-VERSION-U ! ;

: ADD-CAMERA ( ptr u8 n ptr u8 n -- )
   {: serial:ptr serialu:n logical:ptr logicalu:n :}
   CAMERA-N @ MAX-CAMERAS >= if E-HCAP-CAMERA throw then
   CAMERA-N @ {: idx:n :}
   serial serialu idx CAM-SERIAL-SLOT CSTR-CAP idx CAM-SERIAL-U-P COPY-Z
   logical logicalu idx CAM-LOGICAL-SLOT CSTR-CAP idx CAM-LOGICAL-U-P COPY-Z
   idx 1+ CAMERA-N ! ;

: ADD-CAMERA-SPEC ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u 58 INDEX-OF {: sep:n :}
   sep 0 <= if E-HCAP-USAGE throw then
   sep u 1- >= if E-HCAP-USAGE throw then
   a sep a sep 1+ + u sep 1+ - ADD-CAMERA ;

: NUM ( ptr u8 n -- n )
   STR>NUMBER? 0= if drop E-HCAP-USAGE throw then ;

: SET-ABI ( ptr u8 n -- ) ABI-PATH PATH-CAP ABI-U COPY-Z ;
: SET-CONFIG ( ptr u8 n -- ) CONFIG-PATH PATH-CAP CONFIG-U COPY-Z ;
: SET-OUTPUT ( ptr u8 n -- ) OUTPUT-DIR PATH-CAP OUTPUT-U COPY-Z ;
: SET-RESOLUTION ( ptr u8 n -- ) RESOLUTION CSTR-CAP RESOLUTION-U COPY-Z ;
: SET-FPS ( ptr u8 n -- ) NUM FPS ! ;
: SET-DURATION-MS ( ptr u8 n -- ) NUM DURATION-MS ! ;
: SET-WARMUP-MS ( ptr u8 n -- ) NUM WARMUP-MS ! ;
: SET-METADATA-EVERY ( ptr u8 n -- ) NUM METADATA-EVERY ! ;
: SET-SAVE-EVERY ( ptr u8 n -- ) NUM SAVE-EVERY ! ;
: SET-MAX-SAVED-FRAMES ( ptr u8 n -- ) NUM MAX-SAVED-FRAMES ! ;
: SET-MANUAL-EXPOSURE ( ptr u8 n -- ) dup 0= if 2drop -1 MANUAL-EXPOSURE ! else NUM MANUAL-EXPOSURE ! then ;
: SET-MANUAL-GAIN ( ptr u8 n -- ) dup 0= if 2drop -1 MANUAL-GAIN ! else NUM MANUAL-GAIN ! then ;

: RESOLVE-IN ( n ptr u8 n -- n )
   {: handle:n a:ptr u:n :}
   a u SYM-BUF >CSTR
   handle SYM-BUF DLSYM dup 0= if E-HCAP-ABI throw then ;

: RESOLVE ( ptr u8 n -- n )
   {: a:ptr u:n :}
   LIB @ a u RESOLVE-IN ;

: OPEN-ABI ( -- )
   ABI-PATH RTLD-NOW DLOPEN dup 0= if E-HCAP-ABI throw then LIB !
   s" odin_zed_capture_create" RESOLVE FN-CREATE !
   s" odin_zed_capture_destroy" RESOLVE FN-DESTROY !
   s" odin_zed_capture_open" RESOLVE FN-OPEN !
   s" odin_zed_capture_info" RESOLVE FN-INFO !
   s" odin_zed_capture_grab" RESOLVE FN-GRAB !
   s" odin_zed_capture_settings" RESOLVE FN-SETTINGS !
   s" odin_zed_capture_save_color_frame" RESOLVE FN-SAVE-COLOR !
   s" odin_zed_capture_retrieve_image" RESOLVE FN-RETRIEVE-IMAGE !
   s" odin_zed_capture_sensors" RESOLVE FN-SENSORS !
   s" odin_zed_capture_sdk_version" RESOLVE FN-SDK-VERSION !
   s" odin_zed_capture_close" RESOLVE FN-CLOSE ! ;

: CREATE-CONTEXT ( -- )
   0 ERR-SLOT P>N 0 FFI-ARG!
   ERR-CAP 1 FFI-ARG!
   2 FN-CREATE @ FFI-CALLN dup 0= if E-HCAP-ABI throw then HANDLE ! ;

: DESTROY-CONTEXT ( -- )
   HANDLE @ 0 <> if
      HANDLE @ FN-DESTROY @ CALL1 drop
      0 HANDLE !
   then ;

: SDK-VERSION! ( -- )
   0 SDK-VERSION c!
   SDK-VERSION P>N 0 FFI-ARG!
   CSTR-CAP 1 FFI-ARG!
   2 FN-SDK-VERSION @ FFI-CALLN drop
   SDK-VERSION ZLEN SDK-VERSION-U ! ;

: CAM-ABI@ ( n -- n ) CAM-ABI-INDEX CAM-P @ ;
: FRAME-U64@ ( ptr u8 n -- n ) + FS-U64@ ;
: FRAME-IMAGE@ ( ptr u8 -- n ) FRAME-IMAGE-TS FRAME-U64@ ;
: FRAME-DROPS@ ( ptr u8 -- n ) FRAME-DROPPED FRAME-U64@ ;
: FRAME-STATUS@ ( ptr u8 -- n ) FRAME-STATUS + c@ ;

: OPEN-CAMERA ( n -- )
   {: idx:n :}
   HANDLE @ 0 FFI-ARG!
   idx CAM-SERIAL-SLOT P>N 1 FFI-ARG!
   RESOLUTION P>N 2 FFI-ARG!
   FPS @ 3 FFI-ARG!
   MANUAL-EXPOSURE @ 4 FFI-ARG!
   MANUAL-GAIN @ 5 FFI-ARG!
   idx ERR-SLOT P>N 6 FFI-ARG!
   ERR-CAP 7 FFI-ARG!
   8 FN-OPEN @ FFI-CALLN {: camidx:n :}
   camidx 0 < if E-HCAP-CAMERA throw then
   camidx idx CAM-ABI-INDEX CAM-P !
   1 idx CAM-OPENED CAM-P !
   HANDLE @ 0 FFI-ARG!
   camidx 1 FFI-ARG!
   idx CAM-WIDTH CAM-P P>N 2 FFI-ARG!
   idx CAM-HEIGHT CAM-P P>N 3 FFI-ARG!
   idx CAM-SERIAL-SLOT P>N 4 FFI-ARG!
   CSTR-CAP 5 FFI-ARG!
   6 FN-INFO @ FFI-CALLN drop
   idx CAM-SERIAL-SLOT ZLEN idx CAM-SERIAL-U-P ! ;

: OPEN-CAMERAS ( -- )
   CAMERA-N @ 0= if E-HCAP-CAMERA throw then
   0 begin dup CAMERA-N @ < while
      dup OPEN-CAMERA
      1+
   repeat drop ;

: CLOSE-CAMERAS ( -- )
   0 begin dup CAMERA-N @ < while
      dup CAM-OPENED CAM-P @ 0 <> if
         HANDLE @ 0 FFI-ARG!
         dup CAM-ABI@ 1 FFI-ARG!
         2 FN-CLOSE @ FFI-CALLN drop
      then
      1+
   repeat drop ;

: GRAB ( n -- n )
   {: idx:n :}
   HANDLE @ 0 FFI-ARG!
   idx CAM-ABI@ 1 FFI-ARG!
   idx FRAME-SLOT P>N 2 FFI-ARG!
   idx ERR-SLOT P>N 3 FFI-ARG!
   ERR-CAP 4 FFI-ARG!
   5 FN-GRAB @ FFI-CALLN ;

: SETTINGS ( n -- )
   {: idx:n :}
   0 SET-EXP ! 0 SET-HAS-EXP ! 0 SET-GAIN ! 0 SET-HAS-GAIN ! 0 SET-AEC ! 0 SET-HAS-AEC !
   HANDLE @ 0 FFI-ARG!
   idx CAM-ABI@ 1 FFI-ARG!
   SET-EXP P>N 2 FFI-ARG!
   SET-HAS-EXP P>N 3 FFI-ARG!
   SET-GAIN P>N 4 FFI-ARG!
   SET-HAS-GAIN P>N 5 FFI-ARG!
   SET-AEC P>N 6 FFI-ARG!
   SET-HAS-AEC P>N 7 FFI-ARG!
   8 FN-SETTINGS @ FFI-CALLN 0= if
      SET-HAS-EXP @ idx CAM-HAS-EXP CAM-P !
      SET-EXP @ idx CAM-EXP CAM-P !
      SET-HAS-GAIN @ idx CAM-HAS-GAIN CAM-P !
      SET-GAIN @ idx CAM-GAIN CAM-P !
      SET-HAS-AEC @ idx CAM-HAS-AEC CAM-P !
      SET-AEC @ idx CAM-AEC CAM-P !
   then ;

: SAVE-COLOR ( n -- n )
   {: idx:n :}
   HANDLE @ 0 FFI-ARG!
   idx CAM-ABI@ 1 FFI-ARG!
   SAVE-PATH P>N 2 FFI-ARG!
   idx ERR-SLOT P>N 3 FFI-ARG!
   ERR-CAP 4 FFI-ARG!
   5 FN-SAVE-COLOR @ FFI-CALLN ;

: RETRIEVE-IMAGE ( n -- n )
   {: idx:n :}
   0 idx CAM-IMG-PTR CAM-P !
   0 idx CAM-IMG-W CAM-P !
   0 idx CAM-IMG-H CAM-P !
   0 idx CAM-IMG-STEP CAM-P !
   HANDLE @ 0 FFI-ARG!
   idx CAM-ABI@ 1 FFI-ARG!
   idx CAM-IMG-PTR CAM-P P>N 2 FFI-ARG!
   idx CAM-IMG-W CAM-P P>N 3 FFI-ARG!
   idx CAM-IMG-H CAM-P P>N 4 FFI-ARG!
   idx CAM-IMG-STEP CAM-P P>N 5 FFI-ARG!
   idx ERR-SLOT P>N 6 FFI-ARG!
   ERR-CAP 7 FFI-ARG!
   8 FN-RETRIEVE-IMAGE @ FFI-CALLN ;

: SAVE-P5 ( n -- n )
   {: idx:n :}
   idx RETRIEVE-IMAGE dup 0 <> if exit then drop
   idx CAM-IMG-PTR CAM-P @ N>P
   idx CAM-IMG-W CAM-P @
   idx CAM-IMG-H CAM-P @
   idx CAM-IMG-STEP CAM-P @
   NETPBM:WRITE-P5 {: data:ptr datau:n :}
   SAVE-PATH SAVE-PATH-U @ data datau WRITE-ALL
   0 ;

: READ-SENSORS ( n -- n )
   {: idx:n :}
   0 idx SENSOR-AVAIL-P !
   0 idx SENSOR-TS-P !
   HANDLE @ 0 FFI-ARG!
   idx CAM-ABI@ 1 FFI-ARG!
   idx SENSOR-AVAIL-P P>N 2 FFI-ARG!
   idx SENSOR-TS-P P>N 3 FFI-ARG!
   idx SENSOR-RATE-P P>N 4 FFI-ARG!
   idx SENSOR-ACCEL-P P>N 5 FFI-ARG!
   idx SENSOR-GYRO-P P>N 6 FFI-ARG!
   idx ERR-SLOT P>N 7 FFI-ARG!
   ERR-CAP 8 FFI-ARG!
   9 FN-SENSORS @ FFI-CALLN ;

: BUF-C ( n ptr u8 ptr a -- )
   {: c:n buf:ptr lenp:ptr :}
   lenp @ OUT-CAP >= if E-HCAP-CAPACITY throw then
   c buf lenp @ + c!
   lenp @ 1+ lenp ! ;

: BUF+ ( ptr u8 n ptr u8 ptr a -- )
   {: a:ptr u:n buf:ptr lenp:ptr :}
   0 begin dup u < while
      a over + c@ buf lenp BUF-C
      1+
   repeat drop ;

: BUF-U ( n ptr u8 ptr a -- )
   {: n:n buf:ptr lenp:ptr :}
   n 10 < if n 48 + buf lenp BUF-C exit then
   n 10 / buf lenp RECURSE
   n 10 mod 48 + buf lenp BUF-C ;

: BUF-N ( n ptr u8 ptr a -- )
   {: n:n buf:ptr lenp:ptr :}
   n 0 < if 45 buf lenp BUF-C n negate buf lenp BUF-U exit then
   n buf lenp BUF-U ;

: BUF-POW10 ( n -- n ) {: k:n :}
   1 0 begin dup k < while
      swap 10 * swap 1+
   repeat drop ;

: BUF-FRAC ( n n ptr u8 ptr a -- )
   {: frac:n k:n buf:ptr lenp:ptr :}
   k 0= if exit then
   k 1- BUF-POW10 {: div:n :}
   frac div / 48 + buf lenp BUF-C
   frac div mod k 1- buf lenp RECURSE ;

: BUF-FIX ( r n ptr u8 ptr a -- )
   {: k:n buf:ptr lenp:ptr :}
   dup f0< if 45 buf lenp BUF-C fnegate then
   k POW10 f* 0.5 f+ f>s {: scaled:n :}
   k BUF-POW10 {: base:n :}
   scaled base / buf lenp BUF-U
   k 0 > if
      46 buf lenp BUF-C
      scaled base mod k buf lenp BUF-FRAC
   then ;

: QSTR ( ptr u8 n ptr u8 ptr a -- )
   {: a:ptr u:n buf:ptr lenp:ptr :}
   34 buf lenp BUF-C
   a u buf lenp BUF+
   34 buf lenp BUF-C ;

: KEY ( ptr u8 n ptr u8 ptr a -- )
   {: a:ptr u:n buf:ptr lenp:ptr :}
   a u buf lenp QSTR
   58 buf lenp BUF-C ;

: COMMA ( ptr u8 ptr a -- ) {: buf:ptr lenp:ptr :} 44 buf lenp BUF-C ;
: NL ( ptr u8 ptr a -- ) {: buf:ptr lenp:ptr :} 10 buf lenp BUF-C ;
: NULLV ( ptr u8 ptr a -- ) s" null" 2swap BUF+ ;
: BOOLV ( bool ptr u8 ptr a -- ) {: b:bool buf:ptr lenp:ptr :} b if s" true" else s" false" then buf lenp BUF+ ;

: PATH-C ( n ptr u8 ptr a -- )
   {: c:n buf:ptr lenp:ptr :}
   lenp @ PATH-CAP 1- >= if E-HCAP-CAPACITY throw then
   c buf lenp @ + c!
   lenp @ 1+ lenp ! ;

: PATH+ ( ptr u8 n ptr u8 ptr a -- )
   {: a:ptr u:n buf:ptr lenp:ptr :}
   0 begin dup u < while
      a over + c@ buf lenp PATH-C
      1+
   repeat drop ;

: PATH-U6 ( n ptr u8 ptr a -- )
   {: x:n buf:ptr lenp:ptr :}
   x 999999 > if
      x 10 >= if x 10 / buf lenp RECURSE then
      x 10 mod 48 + buf lenp PATH-C
      exit
   then
   x 100000 / 48 + buf lenp PATH-C
   x 100000 mod 10000 / 48 + buf lenp PATH-C
   x 10000 mod 1000 / 48 + buf lenp PATH-C
   x 1000 mod 100 / 48 + buf lenp PATH-C
   x 100 mod 10 / 48 + buf lenp PATH-C
   x 10 mod 48 + buf lenp PATH-C ;

: PATH-Z ( ptr u8 ptr a -- )
   {: buf:ptr lenp:ptr :}
   0 buf lenp @ + c! ;

: BUILD-JOIN ( ptr u8 n ptr u8 n ptr u8 ptr a -- )
   {: a:ptr u:n b:ptr bu:n dst:ptr lenp:ptr :}
   0 lenp !
   a u dst lenp PATH+
   a u PATH-END-SLASH? 0= if 47 dst lenp PATH-C then
   b bu dst lenp PATH+
   dst lenp PATH-Z ;

: BUILD-PATHS ( -- )
   OUTPUT-DIR OUTPUT-U @ s" raw" RAW-DIR RAW-U BUILD-JOIN
   RAW-DIR RAW-U @ s" multi.ndjson" RAW-PATH RAW-PATH-U BUILD-JOIN
   OUTPUT-DIR OUTPUT-U @ s" combined.ndjson" COMBINED-PATH COMBINED-U BUILD-JOIN
   OUTPUT-DIR OUTPUT-U @ s" summary.json" SUMMARY-PATH SUMMARY-U BUILD-JOIN
   OUTPUT-DIR OUTPUT-U @ s" summary.md" SUMMARY-MD-PATH SUMMARY-MD-U BUILD-JOIN
   OUTPUT-DIR OUTPUT-U @ s" images" IMAGE-ROOT IMAGE-ROOT-U BUILD-JOIN ;

: ENSURE-DIRS ( -- )
   OUTPUT-DIR OUTPUT-U @ MAKE-DIRS
   RAW-DIR RAW-U @ MAKE-DIRS
   IMAGE-ROOT IMAGE-ROOT-U @ MAKE-DIRS
   0 HCAP-I !
   begin HCAP-I @ CAMERA-N @ < while
      IMAGE-ROOT IMAGE-ROOT-U @ HCAP-I @ CAM-LOGICAL$ SAVE-PATH SAVE-PATH-U BUILD-JOIN
      SAVE-PATH SAVE-PATH-U @ MAKE-DIRS
      HCAP-I @ 1+ HCAP-I !
   repeat ;

: BUILD-SAVE-PATHS ( n n -- )
   {: idx:n frame:n :}
   IMAGE-ROOT IMAGE-ROOT-U @ idx CAM-LOGICAL$ SAVE-PATH SAVE-PATH-U BUILD-JOIN
   SAVE-PATH SAVE-PATH-U @ PATH-END-SLASH? 0= if 47 SAVE-PATH SAVE-PATH-U PATH-C then
   frame SAVE-PATH SAVE-PATH-U PATH-U6
   s" .pgm" SAVE-PATH SAVE-PATH-U PATH+
   SAVE-PATH SAVE-PATH-U PATH-Z
   0 REL-PATH-U !
   s" images/" REL-PATH REL-PATH-U PATH+
   idx CAM-LOGICAL$ REL-PATH REL-PATH-U PATH+
   47 REL-PATH REL-PATH-U PATH-C
   frame REL-PATH REL-PATH-U PATH-U6
   s" .pgm" REL-PATH REL-PATH-U PATH+
   REL-PATH REL-PATH-U PATH-Z ;

: SAVE-DUE? ( n n -- bool )
   {: idx:n frame:n :}
   idx CAM-SAVED CAM-P @ MAX-SAVED-FRAMES @ >= if FALSE exit then
   SAVE-EVERY @ 0 <= if TRUE exit then
   CAMERA-N @ 1 <= if frame SAVE-EVERY @ mod 0= exit then
   SAVE-EVERY @ idx * CAMERA-N @ / {: phase:n :}
   frame SAVE-EVERY @ + phase - SAVE-EVERY @ mod 0= ;

: EMIT-SCHEMA ( -- )
   0 OUT-SLOT 0 OUT-LENP {: buf:ptr lenp:ptr :}
   123 buf lenp BUF-C
   s" type" buf lenp KEY s" schema" buf lenp QSTR buf lenp COMMA
   s" schema_version" buf lenp KEY s" odin.capture.v1" buf lenp QSTR buf lenp COMMA
   s" helper_version" buf lenp KEY s" habu-capture-0.1.0" buf lenp QSTR buf lenp COMMA
   s" sdk_version" buf lenp KEY SDK-VERSION SDK-VERSION-U @ buf lenp QSTR buf lenp COMMA
   s" host" buf lenp KEY s" zed-box" buf lenp QSTR buf lenp COMMA
   s" command" buf lenp KEY s" capture-save-multi" buf lenp QSTR buf lenp COMMA
   s" config_path" buf lenp KEY CONFIG-PATH CONFIG-U @ buf lenp QSTR
   125 buf lenp BUF-C buf lenp NL ;

: MAYBE-N ( n bool ptr u8 ptr a -- )
   {: val:n present:bool buf:ptr lenp:ptr :}
   present if val buf lenp BUF-N else buf lenp NULLV then ;

: EMIT-FRAME ( n n n n n bool bool bool ptr u8 ptr a -- )
   {: idx:n frame:n img:n host:n dropflag:n duplicate:bool regressed:bool saved:bool buf:ptr lenp:ptr :}
   123 buf lenp BUF-C
   s" type" buf lenp KEY s" frame" buf lenp QSTR buf lenp COMMA
   s" schema_version" buf lenp KEY s" odin.capture.v1" buf lenp QSTR buf lenp COMMA
   s" serial" buf lenp KEY idx CAM-SERIAL$ buf lenp QSTR buf lenp COMMA
   s" logical_name" buf lenp KEY idx CAM-LOGICAL$ buf lenp QSTR buf lenp COMMA
   s" frame_index" buf lenp KEY frame buf lenp BUF-U buf lenp COMMA
   s" sdk_image_timestamp_ns" buf lenp KEY img buf lenp BUF-U buf lenp COMMA
   s" host_monotonic_ns" buf lenp KEY host buf lenp BUF-U buf lenp COMMA
   s" width" buf lenp KEY idx CAM-WIDTH CAM-P @ buf lenp BUF-U buf lenp COMMA
   s" height" buf lenp KEY idx CAM-HEIGHT CAM-P @ buf lenp BUF-U buf lenp COMMA
   s" fps_target" buf lenp KEY FPS @ buf lenp BUF-U buf lenp COMMA
   s" pixel_format" buf lenp KEY saved if s" p5" else s" null" then buf lenp QSTR buf lenp COMMA
   s" exposure_us" buf lenp KEY idx CAM-EXP CAM-P @ idx CAM-HAS-EXP CAM-P @ 0 <> buf lenp MAYBE-N buf lenp COMMA
   s" gain" buf lenp KEY idx CAM-GAIN CAM-P @ idx CAM-HAS-GAIN CAM-P @ 0 <> buf lenp MAYBE-N buf lenp COMMA
   s" auto_exposure" buf lenp KEY idx CAM-HAS-AEC CAM-P @ 0 <> if idx CAM-AEC CAM-P @ 0 <> buf lenp BOOLV else buf lenp NULLV then buf lenp COMMA
   s" image_path" buf lenp KEY saved if REL-PATH REL-PATH-U @ buf lenp QSTR else buf lenp NULLV then buf lenp COMMA
   s" dropped" buf lenp KEY dropflag 0 <> buf lenp BOOLV buf lenp COMMA
   s" duplicate" buf lenp KEY duplicate buf lenp BOOLV buf lenp COMMA
   s" timestamp_regressed" buf lenp KEY regressed buf lenp BOOLV
   125 buf lenp BUF-C buf lenp NL ;

: EMIT-F32 ( ptr u8 ptr a ptr u8 -- )
   {: buf:ptr lenp:ptr p:ptr :}
   p FC-F32@ 6 buf lenp BUF-FIX ;

: EMIT-GYRO-F32 ( ptr u8 ptr a ptr u8 -- )
   {: buf:ptr lenp:ptr p:ptr :}
   p FC-F32@ 0.017453292519943295 f* 6 buf lenp BUF-FIX ;

: EMIT-SENSOR-VALUES ( n ptr u8 ptr a -- )
   {: idx:n buf:ptr lenp:ptr :}
   91 buf lenp BUF-C
   buf lenp idx SENSOR-ACCEL-P EMIT-F32 44 buf lenp BUF-C
   buf lenp idx SENSOR-ACCEL-P 4 + EMIT-F32 44 buf lenp BUF-C
   buf lenp idx SENSOR-ACCEL-P 8 + EMIT-F32 44 buf lenp BUF-C
   buf lenp idx SENSOR-GYRO-P EMIT-GYRO-F32 44 buf lenp BUF-C
   buf lenp idx SENSOR-GYRO-P 4 + EMIT-GYRO-F32 44 buf lenp BUF-C
   buf lenp idx SENSOR-GYRO-P 8 + EMIT-GYRO-F32
   93 buf lenp BUF-C ;

: EMIT-SENSOR ( n n bool ptr u8 ptr a -- )
   {: idx:n frame:n present:bool buf:ptr lenp:ptr :}
   0 TMP-SENSOR-DUP !
   present if
      idx SENSOR-TS-P @ idx CAM-SENSOR-LAST-TS CAM-P @ = idx SENSOR-TS-P @ 0 <> and if
         1 TMP-SENSOR-DUP !
         idx CAM-SENSOR-DUPS CAM-P @ 1+ idx CAM-SENSOR-DUPS CAM-P !
      then
      idx SENSOR-TS-P @ 0 <> if idx SENSOR-TS-P @ idx CAM-SENSOR-LAST-TS CAM-P ! then
   else
      idx CAM-SENSOR-MISSING CAM-P @ 1+ idx CAM-SENSOR-MISSING CAM-P !
   then
   123 buf lenp BUF-C
   s" type" buf lenp KEY s" sensor" buf lenp QSTR buf lenp COMMA
   s" schema_version" buf lenp KEY s" odin.capture.v1" buf lenp QSTR buf lenp COMMA
   s" serial" buf lenp KEY idx CAM-SERIAL$ buf lenp QSTR buf lenp COMMA
   s" logical_name" buf lenp KEY idx CAM-LOGICAL$ buf lenp QSTR buf lenp COMMA
   s" sensor_kind" buf lenp KEY s" imu" buf lenp QSTR buf lenp COMMA
   s" time_reference" buf lenp KEY s" IMAGE" buf lenp QSTR buf lenp COMMA
   s" sensor_timestamp_ns" buf lenp KEY present if idx SENSOR-TS-P @ buf lenp BUF-U else buf lenp NULLV then buf lenp COMMA
   s" image_frame_index" buf lenp KEY frame buf lenp BUF-U buf lenp COMMA
   s" sample_rate_hz" buf lenp KEY present if buf lenp idx SENSOR-RATE-P EMIT-F32 else buf lenp NULLV then buf lenp COMMA
   s" values" buf lenp KEY present if idx buf lenp EMIT-SENSOR-VALUES else 91 buf lenp BUF-C 93 buf lenp BUF-C then buf lenp COMMA
   s" units" buf lenp KEY present if s" m_s2_rad_s" else s" unavailable" then buf lenp QSTR buf lenp COMMA
   s" duplicate" buf lenp KEY TMP-SENSOR-DUP @ 0 <> buf lenp BOOLV buf lenp COMMA
   s" stale" buf lenp KEY FALSE buf lenp BOOLV buf lenp COMMA
   s" missing" buf lenp KEY present 0= buf lenp BOOLV
   125 buf lenp BUF-C buf lenp NL ;

: EMIT-ERROR ( n ptr u8 n ptr u8 ptr a -- )
   {: idx:n code:ptr codeu:n buf:ptr lenp:ptr :}
   123 buf lenp BUF-C
   s" type" buf lenp KEY s" error" buf lenp QSTR buf lenp COMMA
   s" schema_version" buf lenp KEY s" odin.capture.v1" buf lenp QSTR buf lenp COMMA
   s" serial" buf lenp KEY idx CAM-SERIAL$ buf lenp QSTR buf lenp COMMA
   s" code" buf lenp KEY code codeu buf lenp QSTR buf lenp COMMA
   s" message" buf lenp KEY idx ERR-SLOT dup ZLEN buf lenp QSTR buf lenp COMMA
   s" fatal" buf lenp KEY s" false" buf lenp BUF+
   125 buf lenp BUF-C buf lenp NL ;

: EMIT-SUMMARY ( n ptr u8 ptr a -- )
   {: idx:n buf:ptr lenp:ptr :}
   123 buf lenp BUF-C
   s" type" buf lenp KEY s" summary" buf lenp QSTR buf lenp COMMA
   s" schema_version" buf lenp KEY s" odin.capture.v1" buf lenp QSTR buf lenp COMMA
   s" serial" buf lenp KEY idx CAM-SERIAL$ buf lenp QSTR buf lenp COMMA
   s" frames_seen" buf lenp KEY idx CAM-FRAMES CAM-P @ buf lenp BUF-U buf lenp COMMA
   s" frames_dropped" buf lenp KEY idx CAM-DROPS CAM-P @ buf lenp BUF-U buf lenp COMMA
   s" duplicates" buf lenp KEY idx CAM-DUPLICATES CAM-P @ buf lenp BUF-U buf lenp COMMA
   s" timestamp_regressions" buf lenp KEY idx CAM-REGRESSIONS CAM-P @ buf lenp BUF-U buf lenp COMMA
   s" writer_stalls" buf lenp KEY idx CAM-WRITER-STALLS CAM-P @ buf lenp BUF-U buf lenp COMMA
   s" result" buf lenp KEY CLK-ABORT CLK-P atomic@ 0= if s" pass" else s" fail" then buf lenp QSTR
   125 buf lenp BUF-C buf lenp NL ;

: RUNNING? ( -- bool )
   CLK-ABORT CLK-P atomic@ 0=
   TIME-MONO-NS CLK-DEADLINE CLK-P atomic@ <
   and ;

: BARRIER-WAIT ( -- )
   1 CLK-ARRIVED CLK-P atomic-add 1+ CAMERA-N @ = if
      TIME-MONO-NS {: start:n :}
      start CLK-START CLK-P atomic!
      start DURATION-MS @ NS-PER-MS * + CLK-DEADLINE CLK-P atomic!
      1 CLK-RELEASED CLK-P atomic!
   else
      begin CLK-RELEASED CLK-P atomic@ 0= while TASK:PAUSE repeat
   then ;

: WARMUP ( n -- bool )
   {: idx:n :}
   TIME-MONO-NS WARMUP-MS @ NS-PER-MS * + {: deadline:n :}
   begin TIME-MONO-NS deadline < while
      idx GRAB 0= 0= if FALSE exit then
   repeat
   TRUE ;

: NOTE-DROPS ( n n -- n )
   {: idx:n count:n :}
   idx CAM-FRAMES CAM-P @ 0= if
      count idx CAM-BASE-DROPS CAM-P !
      count idx CAM-LAST-DROPS CAM-P !
      0 exit
   then
   count idx CAM-LAST-DROPS CAM-P @ > if
      count idx CAM-LAST-DROPS CAM-P @ - {: delta:n :}
      idx CAM-DROPS CAM-P @ delta + idx CAM-DROPS CAM-P !
      count idx CAM-LAST-DROPS CAM-P !
      delta exit
   then
   0 ;

: CAPTURE-ONE ( n -- )
   {: idx:n :}
   idx OUT-SLOT idx OUT-LENP {: buf:ptr lenp:ptr :}
   idx GRAB 0= 0= if
      1 CLK-ABORT CLK-P atomic!
      idx s" grab_failed" buf lenp EMIT-ERROR
      exit
   then
   idx FRAME-SLOT {: fr:ptr :}
   fr FRAME-STATUS@ 0 <> if exit then
   idx SETTINGS
   fr FRAME-IMAGE@ {: img:n :}
   TIME-MONO-NS {: host:n :}
   fr FRAME-DROPS@ idx NOTE-DROPS {: dropdelta:n :}
   0 TMP-DUP !
   0 TMP-REG !
   idx CAM-LAST-TS CAM-P @ 0 <> if
      img idx CAM-LAST-TS CAM-P @ = if 1 TMP-DUP ! then
      img idx CAM-LAST-TS CAM-P @ < if 1 TMP-REG ! then
   then
   TMP-DUP @ 0 <> if idx CAM-DUPLICATES CAM-P @ 1+ idx CAM-DUPLICATES CAM-P ! then
   TMP-REG @ 0 <> if idx CAM-REGRESSIONS CAM-P @ 1+ idx CAM-REGRESSIONS CAM-P ! then
   img idx CAM-LAST-TS CAM-P !
   idx CAM-FRAMES CAM-P @ {: frameidx:n :}
   0 TMP-SAVED !
   idx frameidx SAVE-DUE? if
      idx frameidx BUILD-SAVE-PATHS
      idx SAVE-P5 0= if
         1 TMP-SAVED !
         idx CAM-SAVED CAM-P @ 1+ idx CAM-SAVED CAM-P !
      else
         idx CAM-WRITER-STALLS CAM-P @ 1+ idx CAM-WRITER-STALLS CAM-P !
         idx s" image_save_failed" buf lenp EMIT-ERROR
      then
   then
   idx frameidx img host dropdelta TMP-DUP @ 0 <> TMP-REG @ 0 <> TMP-SAVED @ 0 <> buf lenp EMIT-FRAME
   INCLUDE-SENSOR @ 0 <> if
      idx READ-SENSORS 0= idx SENSOR-AVAIL-P @ 0 <> and
      idx frameidx rot buf lenp EMIT-SENSOR
   then
   frameidx 1+ idx CAM-FRAMES CAM-P ! ;

: ACQ-RUN ( n -- )
   {: idx:n :}
   idx WARMUP 0= if 1 CLK-ABORT CLK-P atomic! then
   BARRIER-WAIT
   begin RUNNING? while
      idx CAPTURE-ONE
      TASK:PAUSE
   repeat
   idx idx OUT-SLOT idx OUT-LENP EMIT-SUMMARY ;

: ACQ0 ( -- ) 0 ACQ-RUN ;
: ACQ1 ( -- ) 1 ACQ-RUN ;
: ACQ2 ( -- ) 2 ACQ-RUN ;
: ACQ3 ( -- ) 3 ACQ-RUN ;

: TASK-P ( n -- ptr a )
   dup 0 = if drop ACQ-TASK0 exit then
   dup 1 = if drop ACQ-TASK1 exit then
   dup 2 = if drop ACQ-TASK2 exit then
   dup 3 = if drop ACQ-TASK3 exit then
   drop E-HCAP-CAMERA throw ;

: START-ACQ ( n -- )
   dup 0 = if drop ['] ACQ0 0 TASK-P TASK:ACTIVATE exit then
   dup 1 = if drop ['] ACQ1 1 TASK-P TASK:ACTIVATE exit then
   dup 2 = if drop ['] ACQ2 2 TASK-P TASK:ACTIVATE exit then
   dup 3 = if drop ['] ACQ3 3 TASK-P TASK:ACTIVATE exit then
   drop E-HCAP-CAMERA throw ;

: START-TASKS ( -- )
   0 begin dup CAMERA-N @ < while
      dup START-ACQ
      1+
   repeat drop ;

: JOIN-TASKS ( -- )
   0 begin dup CAMERA-N @ < while
      dup TASK-P
      begin dup TASK:DONE? 0= while TASK:PAUSE repeat
      TASK:KILL
      1+
   repeat drop ;

: APPEND-SLOT ( n -- )
   {: idx:n :}
   idx OUT-LENP @ 0 > if
      COMBINED-PATH COMBINED-U @ idx OUT-SLOT idx OUT-LENP @ APPEND-FILE
      RAW-PATH RAW-PATH-U @ idx OUT-SLOT idx OUT-LENP @ APPEND-FILE
   then ;

: WRITE-OUTPUTS ( -- )
   COMBINED-PATH COMBINED-U @ 0 OUT-SLOT 0 OUT-LENP @ WRITE-ALL
   RAW-PATH RAW-PATH-U @ 0 OUT-SLOT 0 OUT-LENP @ WRITE-ALL
   1 begin dup CAMERA-N @ 1+ < while
      dup APPEND-SLOT
      1+
   repeat drop ;

: WRITE-SUMMARY-MD ( -- )
   RB-RESET
   s" # Habu Saved-Image Capture" RB+ RB-NL RB-NL
   s" output" OUTPUT-DIR OUTPUT-U @ MD-S
   s" combined" COMBINED-PATH COMBINED-U @ MD-S
   s" cameras" CAMERA-N @ MD-N
   0 SUM !
   0 HCAP-I ! begin HCAP-I @ CAMERA-N @ < while
      SUM @ HCAP-I @ CAM-FRAMES CAM-P @ + SUM !
      HCAP-I @ 1+ HCAP-I !
   repeat
   s" frames" SUM @ MD-N
   s" result" CLK-ABORT CLK-P atomic@ 0= if s" pass" else s" fail" then MD-S
   SUMMARY-MD-PATH SUMMARY-MD-U @ RB$ WRITE-ALL ;

: WRITE-SUMMARY-JSON ( -- )
   RB-RESET
   123 RB-C 125 RB-C RB-NL
   SUMMARY-PATH SUMMARY-U @ RB$ WRITE-ALL ;

: RUN-CAPTURE ( -- )
   ZERO-STATE
   BUILD-PATHS
   OPEN-ABI
   CREATE-CONTEXT
   SDK-VERSION!
   OPEN-CAMERAS
   ENSURE-DIRS
   EMIT-SCHEMA
   START-TASKS
   JOIN-TASKS
   CLOSE-CAMERAS
   WRITE-OUTPUTS
   WRITE-SUMMARY-MD
   WRITE-SUMMARY-JSON
   DESTROY-CONTEXT ;

: CAM-JSON-STR$ ( n ptr u8 n -- ptr u8 n )
   JSON-GET dup -1 = if E-HCAP-CONFIG throw then
   dup JSON-KIND J-STR <> if E-HCAP-CONFIG throw then
   JSON-STRING$ ;

: LOAD-CONFIG-CAMERA ( n -- )
   {: node:n :}
   node s" serial" CAM-JSON-STR$
   node s" logical_name" CAM-JSON-STR$
   ADD-CAMERA ;

: LOAD-CONFIG-CAMERAS ( -- )
   CAMERA-N @ 0 <> if exit then
   CONFIG-PATH CONFIG-U @ CONFIG-BUF CONFIG-CAP READ-ALL CONFIG-BUF-U !
   CONFIG-BUF CONFIG-BUF-U @ JSON-PARSE {: root:n :}
   root s" cameras" JSON-GET {: arr:n :}
   arr -1 = if E-HCAP-CONFIG throw then
   arr JSON-KIND J-ARR <> if E-HCAP-CONFIG throw then
   0 begin dup arr JSON-COUNT < while
      arr over JSON-ARR@ LOAD-CONFIG-CAMERA
      1+
   repeat drop
   CAMERA-N @ 0= if E-HCAP-CAMERA throw then ;

: SELF-TEST-RESET ( -- )
   DEFAULTS
   ZERO-STATE
   s" 123:cam_a0" ADD-CAMERA-SPEC
   s" /tmp/habu-capture-test" SET-OUTPUT
   BUILD-PATHS ;

public

: RESET ( -- ) DEFAULTS ;
: ABI! ( ptr u8 n -- ) SET-ABI ;
: CONFIG! ( ptr u8 n -- ) SET-CONFIG ;
: OUTPUT! ( ptr u8 n -- ) SET-OUTPUT ;
: RESOLUTION! ( ptr u8 n -- ) SET-RESOLUTION ;
: FPS! ( ptr u8 n -- ) SET-FPS ;
: DURATION-MS! ( ptr u8 n -- ) SET-DURATION-MS ;
: WARMUP-MS! ( ptr u8 n -- ) SET-WARMUP-MS ;
: METADATA-EVERY! ( ptr u8 n -- ) SET-METADATA-EVERY ;
: SAVE-EVERY! ( ptr u8 n -- ) SET-SAVE-EVERY ;
: MAX-SAVED-FRAMES! ( ptr u8 n -- ) SET-MAX-SAVED-FRAMES ;
: MANUAL-EXPOSURE! ( ptr u8 n -- ) SET-MANUAL-EXPOSURE ;
: MANUAL-GAIN! ( ptr u8 n -- ) SET-MANUAL-GAIN ;
: INCLUDE-IMAGE-SENSOR! ( -- ) 1 INCLUDE-SENSOR ! ;
: CAMERA+ ( ptr u8 n -- ) ADD-CAMERA-SPEC ;
: CAMERA-N@ ( -- n ) CAMERA-N @ ;
: OUTPUT$ ( -- ptr u8 n ) OUTPUT-DIR OUTPUT-U @ ;
: COMBINED$ ( -- ptr u8 n ) COMBINED-PATH COMBINED-U @ ;
: LOAD-CONFIG ( -- ) LOAD-CONFIG-CAMERAS ;
: RUN ( -- ) LOAD-CONFIG-CAMERAS RUN-CAPTURE ;

: SELF-TEST ( -- )
   SELF-TEST-RESET
   CAMERA-N @ 1 = ASSERT
   0 CAM-SERIAL$ s" 123" STR= ASSERT
   0 CAM-LOGICAL$ s" cam_a0" STR= ASSERT
   COMBINED-PATH COMBINED-U @ s" /tmp/habu-capture-test/combined.ndjson" STR= ASSERT
   EMIT-SCHEMA
   0 OUT-SLOT 0 OUT-LENP @ s" odin.capture.v1" CONTAINS? ASSERT
   s" capture-backend self-test ok" type cr ;

end-package
