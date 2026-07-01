\ live-detector.f - Habu orchestration over the thin Odin ZED YOLO C ABI.
\
\ Opens one libodin_zed_yolo.so context, starts one acquisition task per camera
\ plus one detector task, and writes tracker/perception/detection NDJSON to
\ --output. The SDK/TensorRT calls stay in the C++ shim; scheduling, decode/NMS,
\ save policy, and NDJSON emission live here.
\
\ Load:
\   ../habu/bin/hb --load odin/live-detector.f -- \
\     --engine /home/user/models/drone/fp16_rect.engine \
\     --camera <serial>:cam_a0 --mode full --output /tmp/habu-ticks.ndjson

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/ffi.f
require lib/time.f
require lib/fs.f
require lib/fs-mutate.f
require lib/task.f
require lib/float.f
require odin/float-cell.f
require odin/yolo-decode.f

4 constant HLD-MAX-CAMERAS
5 constant HLD-OUT-SLOTS
64 constant HLD-CSTR-CAP
1024 constant HLD-PATH-CAP
512 constant HLD-ERR-CAP
$100000 constant HLD-OUT-CAP
1000000 constant HLD-NS-PER-MS
1000 constant HLD-NS-PER-MS3

17 constant HLD-CAM-CELLS
0 constant HLD-CAM-LATEST-TS
1 constant HLD-CAM-LATEST-IDX
2 constant HLD-CAM-PREV-TS
3 constant HLD-CAM-PREV-IDX
4 constant HLD-CAM-FRAMES
5 constant HLD-CAM-DROPS
6 constant HLD-CAM-REGRESSIONS
7 constant HLD-CAM-GRAB-ERRORS
8 constant HLD-CAM-TRACKER-TICKS
9 constant HLD-CAM-STARTUP-STALENESS
10 constant HLD-CAM-RESULT
11 constant HLD-CAM-BASELINE-DROPS
12 constant HLD-CAM-WIDTH
13 constant HLD-CAM-HEIGHT
14 constant HLD-CAM-HANDLE
15 constant HLD-CAM-ABI-INDEX
16 constant HLD-CAM-PRIMED-TS

12 constant HLD-CLOCK-CELLS
0 constant HLD-CLK-START
1 constant HLD-CLK-DEADLINE
2 constant HLD-CLK-STOP
3 constant HLD-CLK-ABORT
4 constant HLD-CLK-ARRIVED
5 constant HLD-CLK-RELEASED
6 constant HLD-CLK-INFERENCES
7 constant HLD-CLK-TENSOR-ERRORS
8 constant HLD-CLK-RUN-ERRORS
9 constant HLD-CLK-DETECTIONS
10 constant HLD-CLK-SAVED-FRAMES
11 constant HLD-CLK-SAVE-ERRORS

0 constant HLD-MODE-RETRIEVE
1 constant HLD-MODE-FULL
2 constant HLD-MODE-RUN

0 constant HLD-FRAME-IMAGE
8 constant HLD-FRAME-CURRENT
16 constant HLD-FRAME-DROPPED
24 constant HLD-FRAME-STATUS
32 constant HLD-FRAME-BYTES
4 constant HLD-FRAME-CELLS

-8100 constant E-HLD-USAGE
-8101 constant E-HLD-CAPACITY
-8102 constant E-HLD-ABI
-8103 constant E-HLD-CAMERA
-8104 constant E-HLD-RUNTIME

: HLD-TRUE ( -- bool ) 0 0= ;
: HLD-FALSE ( -- bool ) HLD-TRUE 0= ;
: HLD-ASSERT ( bool -- ) 0= if 77 throw then ;

create HLD-ABI-PATH HLD-PATH-CAP allot
create HLD-ENGINE-PATH HLD-PATH-CAP allot
create HLD-OUTPUT-PATH HLD-PATH-CAP allot
create HLD-SAVE-DIR HLD-PATH-CAP allot
create HLD-SAVE-PATH HLD-PATH-CAP allot
create HLD-RESOLUTION HLD-CSTR-CAP allot
create HLD-SYM-BUF HLD-CSTR-CAP allot
create HLD-LIBC-PATH
   108 c, 105 c, 98 c, 99 c, 46 c, 115 c, 111 c, 46 c, 54 c, 0 c,
variable HLD-ABI-U
variable HLD-ENGINE-U
variable HLD-OUTPUT-U
variable HLD-SAVE-DIR-U
variable HLD-SAVE-PATH-U
variable HLD-RESOLUTION-U

create HLD-CAM-SERIAL HLD-MAX-CAMERAS HLD-CSTR-CAP * allot
create HLD-CAM-LOGICAL HLD-MAX-CAMERAS HLD-CSTR-CAP * allot
create HLD-CAM-SERIAL-U HLD-MAX-CAMERAS cells allot
create HLD-CAM-LOGICAL-U HLD-MAX-CAMERAS cells allot
variable HLD-CAMERA-N

create HLD-ERR-BUFS HLD-OUT-SLOTS HLD-ERR-CAP * allot
create HLD-FRAMES HLD-OUT-SLOTS HLD-FRAME-BYTES * allot
create HLD-OUT-BUFS HLD-OUT-SLOTS HLD-OUT-CAP * allot
create HLD-OUT-LENS HLD-OUT-SLOTS cells allot

here 7 and 8 swap - 7 and allot
create HLD-CAM-STATE HLD-MAX-CAMERAS HLD-CAM-CELLS * cells allot
here 7 and 8 swap - 7 and allot
create HLD-CLOCK HLD-CLOCK-CELLS cells allot
here 7 and 8 swap - 7 and allot
create HLD-DET-NOW 1 cells allot
create HLD-REC-IDX 1 cells allot
create HLD-REC-FRAME 1 cells allot
create HLD-REC-IMG 1 cells allot
create HLD-REC-DECISION 1 cells allot
create HLD-REC-DETS 1 cells allot
create HLD-REC-TSTART 1 cells allot
create HLD-REC-TEND 1 cells allot
create HLD-REC-RSTART 1 cells allot
create HLD-REC-REND 1 cells allot
create HLD-REC-INF 1 cells allot

: HLD-REC! ( n n n n n n n n n n -- )
   HLD-REC-INF !
   HLD-REC-REND !
   HLD-REC-RSTART !
   HLD-REC-TEND !
   HLD-REC-TSTART !
   HLD-REC-DETS !
   HLD-REC-DECISION !
   HLD-REC-IMG !
   HLD-REC-FRAME !
   HLD-REC-IDX ! ;
here 7 and 8 swap - 7 and allot
create HLD-INFER-RAW 1 cells allot
create HLD-INFER-ANCHORS 1 cells allot
create HLD-INFER-NC 1 cells allot
create HLD-INFER-BOX 1 cells allot
create HLD-INFER-IW 1 cells allot
create HLD-INFER-IH 1 cells allot
create HLD-CONFIDENCE 1 cells allot

create HLD-DET-IDX 1 cells allot
create HLD-DET-FRAMEIDX 1 cells allot
create HLD-DET-IMG 1 cells allot
create HLD-DET-TSTART 1 cells allot
create HLD-DET-TEND 1 cells allot
create HLD-DET-RSTART 1 cells allot
create HLD-DET-REND 1 cells allot
create HLD-DET-DETS 1 cells allot
create HLD-DET-INF 1 cells allot

TASK:MIN-STACK TASK:TASK HLD-ACQ-TASK0
TASK:MIN-STACK TASK:TASK HLD-ACQ-TASK1
TASK:MIN-STACK TASK:TASK HLD-ACQ-TASK2
TASK:MIN-STACK TASK:TASK HLD-ACQ-TASK3
TASK:MIN-STACK TASK:TASK HLD-DET-TASK
TASK:MIN-STACK TASK:TASK HLD-SELF-TASK
variable HLD-LIB
variable HLD-HANDLE
variable HLD-FN-CREATE
variable HLD-FN-DESTROY
variable HLD-FN-OPEN-CAMERA
variable HLD-FN-CAMERA-INFO
variable HLD-FN-GRAB
variable HLD-FN-RETRIEVE-TENSOR
variable HLD-FN-RETRIEVE-TENSOR-FRAME
variable HLD-FN-RETRIEVE-TENSOR-SAVE-COLOR
variable HLD-FN-INFER
variable HLD-FN-NOW
variable HLD-FN-CLOSE-CAMERA
variable HLD-LIBC
variable HLD-FN-SCHED-YIELD

variable HLD-FPS
variable HLD-DURATION-MS
variable HLD-WARMUP-MS
variable HLD-STARTUP-DRAIN-MS
variable HLD-MAX-START-STALENESS-MS
variable HLD-MAX-INFERENCES
variable HLD-DETECTOR-WARMUP-INFERENCES
variable HLD-DLA-CORE
variable HLD-ARG-I
variable HLD-MODE
variable HLD-MAX-DETECTIONS-PER-FRAME
variable HLD-EMIT-DETECTIONS
variable HLD-SAVE-COLOR
variable HLD-SAVE-EVERY
variable HLD-MAX-SAVED-FRAMES
variable HLD-SAVE-GATE
variable HLD-DET-SELECT
variable HLD-WARM-COMPLETED
variable HLD-WARM-CURSOR

: HLD-COPY-Z ( ptr u8 n ptr u8 n ptr a -- )
   {: src:ptr u:n dst:ptr cap:n lenp:ptr :}
   u 0 < if E-HLD-CAPACITY throw then
   u cap >= if E-HLD-CAPACITY throw then
   src dst u BYTE-COPY
   0 dst u + c!
   u lenp ! ;

: HLD-SLOT-CSTR ( n ptr u8 -- ptr u8 )
   swap HLD-CSTR-CAP * + ;
: HLD-CAM-SERIAL-SLOT ( n -- ptr u8 ) HLD-CAM-SERIAL HLD-SLOT-CSTR ;
: HLD-CAM-LOGICAL-SLOT ( n -- ptr u8 ) HLD-CAM-LOGICAL HLD-SLOT-CSTR ;
: HLD-CAM-SERIAL-U-P ( n -- ptr a ) cells HLD-CAM-SERIAL-U + ;
: HLD-CAM-LOGICAL-U-P ( n -- ptr a ) cells HLD-CAM-LOGICAL-U + ;
: HLD-CAM-SERIAL$ ( n -- ptr u8 n ) dup HLD-CAM-SERIAL-SLOT swap HLD-CAM-SERIAL-U-P @ ;
: HLD-CAM-LOGICAL$ ( n -- ptr u8 n ) dup HLD-CAM-LOGICAL-SLOT swap HLD-CAM-LOGICAL-U-P @ ;

: HLD-ERR-SLOT ( n -- ptr u8 ) HLD-ERR-CAP * HLD-ERR-BUFS + ;
: HLD-FRAME-SLOT ( n -- ptr u8 ) HLD-FRAME-BYTES * HLD-FRAMES + ;
: HLD-DET-FRAME ( -- ptr u8 ) HLD-MAX-CAMERAS HLD-FRAME-SLOT ;
: HLD-OUT-SLOT ( n -- ptr u8 ) HLD-OUT-CAP * HLD-OUT-BUFS + ;
: HLD-OUT-LENP ( n -- ptr a ) cells HLD-OUT-LENS + ;
: HLD-TASK-P ( n -- ptr a )
   dup 0 = if drop HLD-ACQ-TASK0 exit then
   dup 1 = if drop HLD-ACQ-TASK1 exit then
   dup 2 = if drop HLD-ACQ-TASK2 exit then
   dup 3 = if drop HLD-ACQ-TASK3 exit then
   dup 4 = if drop HLD-DET-TASK exit then
   drop E-HLD-CAMERA throw ;

: HLD-CAM-P ( n n -- ptr a )
   {: cam:n off:n :}
   cam HLD-CAM-CELLS * off + cells HLD-CAM-STATE + ;
: HLD-CLK-P ( n -- ptr a ) cells HLD-CLOCK + ;

: HLD-ZERO-CELLS ( ptr a n -- )
   {: p:ptr count:n :}
   0 begin dup count < while
      0 p over cells + !
      1+
   repeat drop ;

: HLD-ZERO-STATE ( -- )
   HLD-CAM-STATE HLD-MAX-CAMERAS HLD-CAM-CELLS * HLD-ZERO-CELLS
   HLD-CLOCK HLD-CLOCK-CELLS HLD-ZERO-CELLS
   HLD-OUT-LENS HLD-OUT-SLOTS HLD-ZERO-CELLS
   0 HLD-DET-SELECT !
   0 HLD-SAVE-GATE ! ;

: HLD-USAGE ( -- )
   s" usage: odin/live-detector.f -- --engine <engine> --camera <serial>:<name> [--camera ...] [--abi libodin_zed_yolo.so] [--output /tmp/habu-ticks.ndjson] [--mode full|retrieve-only|run-only]" type cr
   E-HLD-USAGE throw ;

: HLD-DEFAULTS ( -- )
   s" libodin_zed_yolo.so" HLD-ABI-PATH HLD-PATH-CAP HLD-ABI-U HLD-COPY-Z
   s" /tmp/odin-habu-live-detector.ndjson" HLD-OUTPUT-PATH HLD-PATH-CAP HLD-OUTPUT-U HLD-COPY-Z
   s" /tmp/odin-habu-live-images" HLD-SAVE-DIR HLD-PATH-CAP HLD-SAVE-DIR-U HLD-COPY-Z
   s" HD1200" HLD-RESOLUTION HLD-CSTR-CAP HLD-RESOLUTION-U HLD-COPY-Z
   0 HLD-ENGINE-U !
   0 HLD-CAMERA-N !
   60 HLD-FPS !
   1000 HLD-DURATION-MS !
   500 HLD-WARMUP-MS !
   1500 HLD-STARTUP-DRAIN-MS !
   200 HLD-MAX-START-STALENESS-MS !
   0 HLD-MAX-INFERENCES !
   4 HLD-DETECTOR-WARMUP-INFERENCES !
   -1 HLD-DLA-CORE !
   HLD-MODE-FULL HLD-MODE !
   0.25 HLD-CONFIDENCE F!
   32 HLD-MAX-DETECTIONS-PER-FRAME !
   0 HLD-EMIT-DETECTIONS !
   0 HLD-SAVE-COLOR !
   12 HLD-SAVE-EVERY !
   900 HLD-MAX-SAVED-FRAMES ! ;

: HLD-PARSE-NUM ( ptr u8 n -- n )
   STR>NUMBER? 0= if drop HLD-USAGE then ;

: HLD-PARSE-FLOAT ( ptr u8 n -- r )
   STR>FLOAT 0= if drop HLD-USAGE then ;

: HLD-ARG$ ( -- ptr u8 n )
   HLD-ARG-I @ SCRIPT-ARGV$ ;
: HLD-ARG-VALUE$ ( -- ptr u8 n )
   HLD-ARG-I @ 1+ SCRIPT-ARGC >= if HLD-USAGE then
   HLD-ARG-I @ 1+ SCRIPT-ARGV$ ;
: HLD-CONSUME1 ( -- ) HLD-ARG-I @ 1+ HLD-ARG-I ! ;
: HLD-CONSUME2 ( -- ) HLD-ARG-I @ 2 + HLD-ARG-I ! ;

: HLD-ADD-CAMERA ( ptr u8 n -- )
   {: a:ptr u:n :}
   HLD-CAMERA-N @ HLD-MAX-CAMERAS >= if E-HLD-CAMERA throw then
   a u 58 INDEX-OF {: sep:n :}
   sep 0 <= if HLD-USAGE then
   sep u 1- >= if HLD-USAGE then
   HLD-CAMERA-N @ {: idx:n :}
   a sep idx HLD-CAM-SERIAL-SLOT HLD-CSTR-CAP idx HLD-CAM-SERIAL-U-P HLD-COPY-Z
   a sep 1+ + u sep 1+ - idx HLD-CAM-LOGICAL-SLOT HLD-CSTR-CAP idx HLD-CAM-LOGICAL-U-P HLD-COPY-Z
   idx 1+ HLD-CAMERA-N ! ;

: HLD-SET-MODE ( ptr u8 n -- )
   2dup s" full" STR= if 2drop HLD-MODE-FULL HLD-MODE ! exit then
   2dup s" retrieve-only" STR= if 2drop HLD-MODE-RETRIEVE HLD-MODE ! exit then
   2dup s" run-only" STR= if 2drop HLD-MODE-RUN HLD-MODE ! exit then
   2drop HLD-USAGE ;

: HLD-MODE-FULL? ( -- bool ) HLD-MODE @ HLD-MODE-FULL = ;
: HLD-MODE-RETRIEVE? ( -- bool ) HLD-MODE @ HLD-MODE-RETRIEVE = ;
: HLD-MODE-RUN? ( -- bool ) HLD-MODE @ HLD-MODE-RUN = ;

: HLD-PARSE-ONE ( -- )
   HLD-ARG$ s" --engine" STR= if
      HLD-ARG-VALUE$ HLD-ENGINE-PATH HLD-PATH-CAP HLD-ENGINE-U HLD-COPY-Z HLD-CONSUME2 exit
   then
   HLD-ARG$ s" --abi" STR= if
      HLD-ARG-VALUE$ HLD-ABI-PATH HLD-PATH-CAP HLD-ABI-U HLD-COPY-Z HLD-CONSUME2 exit
   then
   HLD-ARG$ s" --output" STR= if
      HLD-ARG-VALUE$ HLD-OUTPUT-PATH HLD-PATH-CAP HLD-OUTPUT-U HLD-COPY-Z HLD-CONSUME2 exit
   then
   HLD-ARG$ s" --camera" STR= if
      HLD-ARG-VALUE$ HLD-ADD-CAMERA HLD-CONSUME2 exit
   then
   HLD-ARG$ s" --resolution" STR= if
      HLD-ARG-VALUE$ HLD-RESOLUTION HLD-CSTR-CAP HLD-RESOLUTION-U HLD-COPY-Z HLD-CONSUME2 exit
   then
   HLD-ARG$ s" --fps" STR= if
      HLD-ARG-VALUE$ HLD-PARSE-NUM HLD-FPS ! HLD-CONSUME2 exit
   then
   HLD-ARG$ s" --duration-ms" STR= if
      HLD-ARG-VALUE$ HLD-PARSE-NUM HLD-DURATION-MS ! HLD-CONSUME2 exit
   then
   HLD-ARG$ s" --warmup-ms" STR= if
      HLD-ARG-VALUE$ HLD-PARSE-NUM HLD-WARMUP-MS ! HLD-CONSUME2 exit
   then
   HLD-ARG$ s" --startup-drain-ms" STR= if
      HLD-ARG-VALUE$ HLD-PARSE-NUM HLD-STARTUP-DRAIN-MS ! HLD-CONSUME2 exit
   then
   HLD-ARG$ s" --max-start-staleness-ms" STR= if
      HLD-ARG-VALUE$ HLD-PARSE-NUM HLD-MAX-START-STALENESS-MS ! HLD-CONSUME2 exit
   then
   HLD-ARG$ s" --max-inferences" STR= if
      HLD-ARG-VALUE$ HLD-PARSE-NUM HLD-MAX-INFERENCES ! HLD-CONSUME2 exit
   then
   HLD-ARG$ s" --detector-warmup-inferences" STR= if
      HLD-ARG-VALUE$ HLD-PARSE-NUM HLD-DETECTOR-WARMUP-INFERENCES ! HLD-CONSUME2 exit
   then
   HLD-ARG$ s" --confidence" STR= if
      HLD-ARG-VALUE$ HLD-PARSE-FLOAT HLD-CONFIDENCE F! HLD-CONSUME2 exit
   then
   HLD-ARG$ s" --max-detections-per-frame" STR= if
      HLD-ARG-VALUE$ HLD-PARSE-NUM HLD-MAX-DETECTIONS-PER-FRAME ! HLD-CONSUME2 exit
   then
   HLD-ARG$ s" --emit-detections" STR= if
      1 HLD-EMIT-DETECTIONS ! HLD-CONSUME1 exit
   then
   HLD-ARG$ s" --dla-core" STR= if
      HLD-ARG-VALUE$ HLD-PARSE-NUM HLD-DLA-CORE ! HLD-CONSUME2 exit
   then
   HLD-ARG$ s" --save-color" STR= if
      1 HLD-SAVE-COLOR ! HLD-CONSUME1 exit
   then
   HLD-ARG$ s" --save-every" STR= if
      HLD-ARG-VALUE$ HLD-PARSE-NUM HLD-SAVE-EVERY ! HLD-CONSUME2 exit
   then
   HLD-ARG$ s" --max-saved-frames" STR= if
      HLD-ARG-VALUE$ HLD-PARSE-NUM HLD-MAX-SAVED-FRAMES ! HLD-CONSUME2 exit
   then
   HLD-ARG$ s" --save-dir" STR= if
      HLD-ARG-VALUE$ HLD-SAVE-DIR HLD-PATH-CAP HLD-SAVE-DIR-U HLD-COPY-Z HLD-CONSUME2 exit
   then
   HLD-ARG$ s" --mode" STR= if
      HLD-ARG-VALUE$ HLD-SET-MODE HLD-CONSUME2 exit
   then
   HLD-ARG$ s" --help" STR= if HLD-USAGE then
   HLD-USAGE ;

: HLD-PARSE-ARGS ( -- )
   0 HLD-ARG-I !
   begin HLD-ARG-I @ SCRIPT-ARGC < while
      HLD-PARSE-ONE
   repeat
   HLD-ENGINE-U @ 0= if HLD-USAGE then
   HLD-CAMERA-N @ 0= if HLD-USAGE then ;

: HLD-RESOLVE-IN ( n ptr u8 n -- n )
   {: handle:n a:ptr u:n :}
   a u HLD-SYM-BUF >CSTR
   handle HLD-SYM-BUF DLSYM dup 0= if E-HLD-ABI throw then ;

: HLD-RESOLVE ( ptr u8 n -- n )
   {: a:ptr u:n :}
   HLD-LIB @ a u HLD-RESOLVE-IN ;

: HLD-OPEN-ABI ( -- )
   HLD-ABI-PATH RTLD-NOW DLOPEN dup 0= if E-HLD-ABI throw then HLD-LIB !
   s" odin_zed_yolo_create" HLD-RESOLVE HLD-FN-CREATE !
   s" odin_zed_yolo_destroy" HLD-RESOLVE HLD-FN-DESTROY !
   s" odin_zed_yolo_open_camera" HLD-RESOLVE HLD-FN-OPEN-CAMERA !
   s" odin_zed_yolo_camera_info" HLD-RESOLVE HLD-FN-CAMERA-INFO !
   s" odin_zed_yolo_grab" HLD-RESOLVE HLD-FN-GRAB !
   s" odin_zed_yolo_retrieve_tensor" HLD-RESOLVE HLD-FN-RETRIEVE-TENSOR !
   s" odin_zed_yolo_retrieve_tensor_frame" HLD-RESOLVE HLD-FN-RETRIEVE-TENSOR-FRAME !
   s" odin_zed_yolo_retrieve_tensor_save_color" HLD-RESOLVE HLD-FN-RETRIEVE-TENSOR-SAVE-COLOR !
   s" odin_zed_yolo_infer" HLD-RESOLVE HLD-FN-INFER !
   s" odin_zed_yolo_now" HLD-RESOLVE HLD-FN-NOW !
   s" odin_zed_yolo_close_camera" HLD-RESOLVE HLD-FN-CLOSE-CAMERA ! ;

: HLD-OPEN-YIELD ( -- )
   HLD-LIBC @ 0= if
      HLD-LIBC-PATH RTLD-NOW DLOPEN dup 0= if E-HLD-ABI throw then HLD-LIBC !
   then
   HLD-FN-SCHED-YIELD @ 0= if
      HLD-LIBC @ s" sched_yield" HLD-RESOLVE-IN HLD-FN-SCHED-YIELD !
   then ;

: HLD-YIELD ( -- )
   TASK:PAUSE ;

: HLD-CREATE-CONTEXT ( -- )
   HLD-ENGINE-PATH P>N 0 FFI-ARG!
   HLD-DLA-CORE @ 1 FFI-ARG!
   0 HLD-ERR-SLOT P>N 2 FFI-ARG!
   HLD-ERR-CAP 3 FFI-ARG!
   4 HLD-FN-CREATE @ FFI-CALLN dup 0= if E-HLD-ABI throw then HLD-HANDLE ! ;

: HLD-CALL-DESTROY ( -- )
   HLD-HANDLE @ 0 <> if
      HLD-HANDLE @ HLD-FN-DESTROY @ CALL1 drop
      0 HLD-HANDLE !
   then ;

: HLD-CAM-ABI-INDEX@ ( n -- n ) HLD-CAM-ABI-INDEX HLD-CAM-P @ ;
: HLD-FRAME-U64@ ( ptr u8 n -- n ) + FS-U64@ ;
: HLD-FRAME-IMAGE@ ( ptr u8 -- n ) HLD-FRAME-IMAGE HLD-FRAME-U64@ ;
: HLD-FRAME-CURRENT@ ( ptr u8 -- n ) HLD-FRAME-CURRENT HLD-FRAME-U64@ ;
: HLD-FRAME-DROPPED@ ( ptr u8 -- n ) HLD-FRAME-DROPPED HLD-FRAME-U64@ ;
: HLD-FRAME-STATUS@ ( ptr u8 -- n ) HLD-FRAME-STATUS + c@ ;

: HLD-GRAB ( n n -- n )
   {: idx:n errslot:n :}
   HLD-HANDLE @ 0 FFI-ARG!
   idx HLD-CAM-ABI-INDEX@ 1 FFI-ARG!
   idx HLD-FRAME-SLOT P>N 2 FFI-ARG!
   errslot HLD-ERR-SLOT P>N 3 FFI-ARG!
   HLD-ERR-CAP 4 FFI-ARG!
   5 HLD-FN-GRAB @ FFI-CALLN ;

: HLD-RETRIEVE-FRAME ( n -- n )
   {: idx:n :}
   HLD-HANDLE @ 0 FFI-ARG!
   idx HLD-CAM-ABI-INDEX@ 1 FFI-ARG!
   HLD-DET-FRAME P>N 2 FFI-ARG!
   HLD-MAX-CAMERAS HLD-ERR-SLOT P>N 3 FFI-ARG!
   HLD-ERR-CAP 4 FFI-ARG!
   5 HLD-FN-RETRIEVE-TENSOR-FRAME @ FFI-CALLN ;

: HLD-RETRIEVE-SAVE-COLOR ( n -- n )
   {: idx:n :}
   HLD-HANDLE @ 0 FFI-ARG!
   idx HLD-CAM-ABI-INDEX@ 1 FFI-ARG!
   HLD-SAVE-PATH P>N 2 FFI-ARG!
   HLD-DET-FRAME P>N 3 FFI-ARG!
   HLD-MAX-CAMERAS HLD-ERR-SLOT P>N 4 FFI-ARG!
   HLD-ERR-CAP 5 FFI-ARG!
   6 HLD-FN-RETRIEVE-TENSOR-SAVE-COLOR @ FFI-CALLN ;

: HLD-RETRIEVE-TENSOR ( n -- n )
   {: idx:n :}
   HLD-HANDLE @ 0 FFI-ARG!
   idx HLD-CAM-ABI-INDEX@ 1 FFI-ARG!
   0 HLD-ERR-SLOT P>N 2 FFI-ARG!
   HLD-ERR-CAP 3 FFI-ARG!
   4 HLD-FN-RETRIEVE-TENSOR @ FFI-CALLN ;

: HLD-INFER-WARM ( n -- n )
   {: idx:n :}
   HLD-HANDLE @ 0 FFI-ARG!
   idx HLD-CAM-ABI-INDEX@ 1 FFI-ARG!
   HLD-INFER-RAW P>N 2 FFI-ARG!
   HLD-INFER-ANCHORS P>N 3 FFI-ARG!
   HLD-INFER-NC P>N 4 FFI-ARG!
   HLD-INFER-BOX P>N 5 FFI-ARG!
   HLD-INFER-IW P>N 6 FFI-ARG!
   HLD-INFER-IH P>N 7 FFI-ARG!
   0 HLD-ERR-SLOT P>N 8 FFI-ARG!
   HLD-ERR-CAP 9 FFI-ARG!
   10 HLD-FN-INFER @ FFI-CALLN ;

: HLD-NOW ( n ptr a -- n )
   {: idx:n outp:ptr :}
   HLD-HANDLE @ 0 FFI-ARG!
   idx HLD-CAM-ABI-INDEX@ 1 FFI-ARG!
   outp P>N 2 FFI-ARG!
   3 HLD-FN-NOW @ FFI-CALLN ;

: HLD-OPEN-CAMERA ( n -- )
   {: idx:n :}
   HLD-HANDLE @ 0 FFI-ARG!
   idx HLD-CAM-SERIAL-SLOT P>N 1 FFI-ARG!
   idx HLD-CAM-LOGICAL-SLOT P>N 2 FFI-ARG!
   HLD-RESOLUTION P>N 3 FFI-ARG!
   HLD-FPS @ 4 FFI-ARG!
   0 HLD-ERR-SLOT P>N 5 FFI-ARG!
   HLD-ERR-CAP 6 FFI-ARG!
   7 HLD-FN-OPEN-CAMERA @ FFI-CALLN {: camidx:n :}
   camidx 0 < if E-HLD-CAMERA throw then
   camidx idx HLD-CAM-ABI-INDEX HLD-CAM-P !
   HLD-HANDLE @ idx HLD-CAM-HANDLE HLD-CAM-P !
   0 idx HLD-CAM-WIDTH HLD-CAM-P !
   0 idx HLD-CAM-HEIGHT HLD-CAM-P !
   HLD-HANDLE @ 0 FFI-ARG!
   camidx 1 FFI-ARG!
   idx HLD-CAM-WIDTH HLD-CAM-P P>N 2 FFI-ARG!
   idx HLD-CAM-HEIGHT HLD-CAM-P P>N 3 FFI-ARG!
   idx HLD-CAM-SERIAL-SLOT P>N 4 FFI-ARG!
   HLD-CSTR-CAP 5 FFI-ARG!
   6 HLD-FN-CAMERA-INFO @ FFI-CALLN drop
   idx HLD-CAM-SERIAL-SLOT ZLEN idx HLD-CAM-SERIAL-U-P ! ;

: HLD-OPEN-CAMERAS ( -- )
   0 begin dup HLD-CAMERA-N @ < while
      dup HLD-OPEN-CAMERA
      1+
   repeat drop ;

: HLD-BUF-C ( n ptr u8 ptr a -- )
   {: c:n buf:ptr lenp:ptr :}
   lenp @ HLD-OUT-CAP >= if E-HLD-CAPACITY throw then
   c buf lenp @ + c!
   lenp @ 1+ lenp ! ;

: HLD-BUF+ ( ptr u8 n ptr u8 ptr a -- )
   {: a:ptr u:n buf:ptr lenp:ptr :}
   0 begin dup u < while
      a over + c@ buf lenp HLD-BUF-C
      1+
   repeat drop ;

: HLD-BUF-U ( n ptr u8 ptr a -- )
   {: n:n buf:ptr lenp:ptr :}
   n 10 < if n 48 + buf lenp HLD-BUF-C exit then
   n 10 / buf lenp RECURSE
   n 10 mod 48 + buf lenp HLD-BUF-C ;

: HLD-BUF-N ( n ptr u8 ptr a -- )
   {: n:n buf:ptr lenp:ptr :}
   n 0 < if 45 buf lenp HLD-BUF-C n negate buf lenp HLD-BUF-U exit then
   n buf lenp HLD-BUF-U ;

: HLD-BUF-3 ( n ptr u8 ptr a -- )
   {: x:n buf:ptr lenp:ptr :}
   x 100 / 48 + buf lenp HLD-BUF-C
   x 100 mod 10 / 48 + buf lenp HLD-BUF-C
   x 10 mod 48 + buf lenp HLD-BUF-C ;

: HLD-BUF-4 ( n ptr u8 ptr a -- )
   {: x:n buf:ptr lenp:ptr :}
   x 1000 / 48 + buf lenp HLD-BUF-C
   x 1000 mod 100 / 48 + buf lenp HLD-BUF-C
   x 100 mod 10 / 48 + buf lenp HLD-BUF-C
   x 10 mod 48 + buf lenp HLD-BUF-C ;

: HLD-BUF-6 ( n ptr u8 ptr a -- )
   {: x:n buf:ptr lenp:ptr :}
   x 100000 / 48 + buf lenp HLD-BUF-C
   x 100000 mod 10000 / 48 + buf lenp HLD-BUF-C
   x 10000 mod 1000 / 48 + buf lenp HLD-BUF-C
   x 1000 mod 100 / 48 + buf lenp HLD-BUF-C
   x 100 mod 10 / 48 + buf lenp HLD-BUF-C
   x 10 mod 48 + buf lenp HLD-BUF-C ;

: HLD-POW10I ( n -- n ) {: k:n :}
   1 0 begin dup k < while
      swap 10 * swap 1+
   repeat drop ;

: HLD-BUF-FRAC ( n n ptr u8 ptr a -- )
   {: frac:n k:n buf:ptr lenp:ptr :}
   k 0= if exit then
   k 1- HLD-POW10I {: div:n :}
   frac div / 48 + buf lenp HLD-BUF-C
   frac div mod k 1- buf lenp RECURSE ;

: HLD-BUF-FIX ( r n ptr u8 ptr a -- )
   {: k:n buf:ptr lenp:ptr :}
   dup f0< if 45 buf lenp HLD-BUF-C fnegate then
   k POW10 f* 0.5 f+ f>s {: scaled:n :}
   k HLD-POW10I {: base:n :}
   scaled base / buf lenp HLD-BUF-U
   k 0 > if
      46 buf lenp HLD-BUF-C
      scaled base mod k buf lenp HLD-BUF-FRAC
   then ;

: HLD-BUF-MS ( n n ptr u8 ptr a -- )
   {: start:n end:n buf:ptr lenp:ptr :}
   end start <= if
      0 buf lenp HLD-BUF-U 46 buf lenp HLD-BUF-C 0 buf lenp HLD-BUF-3 exit
   then
   end start - {: ns:n :}
   ns HLD-NS-PER-MS / buf lenp HLD-BUF-U
   46 buf lenp HLD-BUF-C
   ns HLD-NS-PER-MS mod HLD-NS-PER-MS3 / buf lenp HLD-BUF-3 ;

: HLD-Q ( ptr u8 ptr a -- ) {: buf:ptr lenp:ptr :} 34 buf lenp HLD-BUF-C ;
: HLD-COMMA ( ptr u8 ptr a -- ) {: buf:ptr lenp:ptr :} 44 buf lenp HLD-BUF-C ;
: HLD-NL ( ptr u8 ptr a -- ) {: buf:ptr lenp:ptr :} 10 buf lenp HLD-BUF-C ;

: HLD-QSTR ( ptr u8 n ptr u8 ptr a -- )
   {: a:ptr u:n buf:ptr lenp:ptr :}
   34 buf lenp HLD-BUF-C
   a u buf lenp HLD-BUF+
   34 buf lenp HLD-BUF-C ;

: HLD-KEY ( ptr u8 n ptr u8 ptr a -- )
   {: a:ptr u:n buf:ptr lenp:ptr :}
   a u buf lenp HLD-QSTR
   58 buf lenp HLD-BUF-C ;

: HLD-PATH-C ( n -- )
   {: c:n :}
   HLD-SAVE-PATH-U @ HLD-PATH-CAP 1- >= if E-HLD-CAPACITY throw then
   c HLD-SAVE-PATH HLD-SAVE-PATH-U @ + c!
   HLD-SAVE-PATH-U @ 1+ HLD-SAVE-PATH-U ! ;

: HLD-PATH+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   0 begin dup u < while
      a over + c@ HLD-PATH-C
      1+
   repeat drop ;

: HLD-PATH-U6 ( n -- )
   {: x:n :}
   x 999999 > if
      x 10 >= if x 10 / RECURSE then
      x 10 mod 48 + HLD-PATH-C
      exit
   then
   x 100000 / 48 + HLD-PATH-C
   x 100000 mod 10000 / 48 + HLD-PATH-C
   x 10000 mod 1000 / 48 + HLD-PATH-C
   x 1000 mod 100 / 48 + HLD-PATH-C
   x 100 mod 10 / 48 + HLD-PATH-C
   x 10 mod 48 + HLD-PATH-C ;

: HLD-PATH-Z ( -- )
   0 HLD-SAVE-PATH HLD-SAVE-PATH-U @ + c! ;

: HLD-BUILD-SAVE-PATH ( n n -- )
   {: idx:n gate:n :}
   0 HLD-SAVE-PATH-U !
   HLD-SAVE-DIR HLD-SAVE-DIR-U @ HLD-PATH+
   HLD-SAVE-DIR-U @ 0 > if
      HLD-SAVE-DIR HLD-SAVE-DIR-U @ 1- + c@ 47 <> if 47 HLD-PATH-C then
   then
   idx HLD-CAM-LOGICAL$ HLD-PATH+
   s" _inf" HLD-PATH+
   gate HLD-PATH-U6
   s" .png" HLD-PATH+
   HLD-PATH-Z ;

: HLD-SAVE-DUE? ( n -- bool )
   {: gate:n :}
   HLD-SAVE-COLOR @ 0= if HLD-FALSE exit then
   HLD-SAVE-DIR-U @ 0= if HLD-FALSE exit then
   HLD-CLK-SAVED-FRAMES HLD-CLK-P atomic@ HLD-MAX-SAVED-FRAMES @ >= if HLD-FALSE exit then
   HLD-SAVE-EVERY @ 0= if HLD-TRUE exit then
   gate HLD-SAVE-EVERY @ mod 0= ;

: HLD-ENSURE-SAVE-DIR ( -- )
   HLD-SAVE-COLOR @ 0= if exit then
   HLD-SAVE-DIR HLD-SAVE-DIR-U @ MAKE-DIRS ;

: HLD-TRACKER-REC ( n n n n ptr u8 ptr a -- )
   {: idx:n frame:n img:n cur:n buf:ptr lenp:ptr :}
   123 buf lenp HLD-BUF-C
   s" schema_version" buf lenp HLD-KEY s" odin.tracker_tick.v1" buf lenp HLD-QSTR buf lenp HLD-COMMA
   s" camera_serial" buf lenp HLD-KEY idx HLD-CAM-SERIAL$ buf lenp HLD-QSTR buf lenp HLD-COMMA
   s" logical_name" buf lenp HLD-KEY idx HLD-CAM-LOGICAL$ buf lenp HLD-QSTR buf lenp HLD-COMMA
   s" frame_index" buf lenp HLD-KEY frame buf lenp HLD-BUF-U buf lenp HLD-COMMA
   s" sdk_image_timestamp_ns" buf lenp HLD-KEY img buf lenp HLD-BUF-U buf lenp HLD-COMMA
   s" tracker_source" buf lenp HLD-KEY s" camera_frame_heartbeat" buf lenp HLD-QSTR buf lenp HLD-COMMA
   s" tracker_update_index" buf lenp HLD-KEY frame 1+ buf lenp HLD-BUF-U buf lenp HLD-COMMA
   s" tracker_timestamp_ns" buf lenp HLD-KEY cur buf lenp HLD-BUF-U buf lenp HLD-COMMA
   s" latency_ms" buf lenp HLD-KEY img cur buf lenp HLD-BUF-MS buf lenp HLD-COMMA
   s" queue_depth" buf lenp HLD-KEY 0 buf lenp HLD-BUF-U buf lenp HLD-COMMA
   s" tracks_active" buf lenp HLD-KEY 0 buf lenp HLD-BUF-U
   125 buf lenp HLD-BUF-C buf lenp HLD-NL ;

: HLD-MODE-LABEL ( -- ptr u8 n )
   HLD-MODE-FULL? if s" full" exit then
   HLD-MODE-RUN? if s" run-only" exit then
   s" retrieve-only" ;

: HLD-TICK-REC ( ptr u8 ptr a -- )
   {: buf:ptr lenp:ptr :}
   123 buf lenp HLD-BUF-C
   s" schema_version" buf lenp HLD-KEY s" odin.perception_tick.v1" buf lenp HLD-QSTR buf lenp HLD-COMMA
   s" camera_serial" buf lenp HLD-KEY HLD-REC-IDX @ HLD-CAM-SERIAL$ buf lenp HLD-QSTR buf lenp HLD-COMMA
   s" logical_name" buf lenp HLD-KEY HLD-REC-IDX @ HLD-CAM-LOGICAL$ buf lenp HLD-QSTR buf lenp HLD-COMMA
   s" frame_index" buf lenp HLD-KEY HLD-REC-FRAME @ buf lenp HLD-BUF-U buf lenp HLD-COMMA
   s" sdk_image_timestamp_ns" buf lenp HLD-KEY HLD-REC-IMG @ buf lenp HLD-BUF-U buf lenp HLD-COMMA
   s" tick_source" buf lenp HLD-KEY s" zed_yolo_live_detector" buf lenp HLD-QSTR buf lenp HLD-COMMA
   s" inference_index" buf lenp HLD-KEY HLD-REC-INF @ buf lenp HLD-BUF-U buf lenp HLD-COMMA
   s" detections_count" buf lenp HLD-KEY HLD-REC-DETS @ buf lenp HLD-BUF-U buf lenp HLD-COMMA
   s" latency_ms" buf lenp HLD-KEY
   HLD-MODE-RUN? if 0 buf lenp HLD-BUF-U 46 buf lenp HLD-BUF-C 0 buf lenp HLD-BUF-3 else HLD-REC-IMG @ HLD-REC-DECISION @ buf lenp HLD-BUF-MS then
   buf lenp HLD-COMMA
   s" queue_depth" buf lenp HLD-KEY 0 buf lenp HLD-BUF-U buf lenp HLD-COMMA
   s" decision_timestamp_ns" buf lenp HLD-KEY HLD-REC-DECISION @ buf lenp HLD-BUF-U buf lenp HLD-COMMA
   s" schedule_lag_ms" buf lenp HLD-KEY 0 buf lenp HLD-BUF-U 46 buf lenp HLD-BUF-C 0 buf lenp HLD-BUF-3 buf lenp HLD-COMMA
   s" tensor_retrieve_ms" buf lenp HLD-KEY HLD-REC-TSTART @ HLD-REC-TEND @ buf lenp HLD-BUF-MS buf lenp HLD-COMMA
   s" detector_run_ms" buf lenp HLD-KEY HLD-REC-RSTART @ HLD-REC-REND @ buf lenp HLD-BUF-MS buf lenp HLD-COMMA
   s" detector_cycle_ms" buf lenp HLD-KEY
   HLD-MODE-RUN? if HLD-REC-RSTART @ HLD-REC-REND @ buf lenp HLD-BUF-MS else HLD-REC-TSTART @ HLD-REC-REND @ buf lenp HLD-BUF-MS then
   buf lenp HLD-COMMA
   s" mode" buf lenp HLD-KEY HLD-MODE-LABEL buf lenp HLD-QSTR
   125 buf lenp HLD-BUF-C buf lenp HLD-NL ;

: HLD-TARGET-ID ( n ptr u8 ptr a -- )
   {: di:n buf:ptr lenp:ptr :}
   34 buf lenp HLD-BUF-C
   s" yolo-" buf lenp HLD-BUF+
   di YOLO:K-LBL@ buf lenp HLD-BUF-U
   45 buf lenp HLD-BUF-C
   HLD-REC-IDX @ HLD-CAM-LOGICAL$ buf lenp HLD-BUF+
   45 buf lenp HLD-BUF-C
   HLD-REC-FRAME @ buf lenp HLD-BUF-U
   45 buf lenp HLD-BUF-C
   di buf lenp HLD-BUF-U
   34 buf lenp HLD-BUF-C ;

: HLD-DETECTION-REC ( n ptr u8 ptr a -- )
   {: di:n buf:ptr lenp:ptr :}
   123 buf lenp HLD-BUF-C
   s" schema_version" buf lenp HLD-KEY s" odin.localization_detections.v1" buf lenp HLD-QSTR buf lenp HLD-COMMA
   s" camera_serial" buf lenp HLD-KEY HLD-REC-IDX @ HLD-CAM-SERIAL$ buf lenp HLD-QSTR buf lenp HLD-COMMA
   s" logical_name" buf lenp HLD-KEY HLD-REC-IDX @ HLD-CAM-LOGICAL$ buf lenp HLD-QSTR buf lenp HLD-COMMA
   s" frame_index" buf lenp HLD-KEY HLD-REC-FRAME @ buf lenp HLD-BUF-U buf lenp HLD-COMMA
   s" sdk_image_timestamp_ns" buf lenp HLD-KEY HLD-REC-IMG @ buf lenp HLD-BUF-U buf lenp HLD-COMMA
   s" target_id" buf lenp HLD-KEY di buf lenp HLD-TARGET-ID buf lenp HLD-COMMA
   s" pixel_center_x" buf lenp HLD-KEY di YOLO:K-X0@ di YOLO:K-X1@ f+ 0.5 f* 3 buf lenp HLD-BUF-FIX buf lenp HLD-COMMA
   s" pixel_center_y" buf lenp HLD-KEY di YOLO:K-Y0@ di YOLO:K-Y1@ f+ 0.5 f* 3 buf lenp HLD-BUF-FIX buf lenp HLD-COMMA
   s" bbox" buf lenp HLD-KEY 123 buf lenp HLD-BUF-C
   s" x" buf lenp HLD-KEY di YOLO:K-X0@ 3 buf lenp HLD-BUF-FIX buf lenp HLD-COMMA
   s" y" buf lenp HLD-KEY di YOLO:K-Y0@ 3 buf lenp HLD-BUF-FIX buf lenp HLD-COMMA
   s" width" buf lenp HLD-KEY di YOLO:K-X1@ di YOLO:K-X0@ f- 3 buf lenp HLD-BUF-FIX buf lenp HLD-COMMA
   s" height" buf lenp HLD-KEY di YOLO:K-Y1@ di YOLO:K-Y0@ f- 3 buf lenp HLD-BUF-FIX
   125 buf lenp HLD-BUF-C buf lenp HLD-COMMA
   s" detection_source" buf lenp HLD-KEY s" zed_yolo_live_detector" buf lenp HLD-QSTR buf lenp HLD-COMMA
   s" confidence" buf lenp HLD-KEY di YOLO:K-CONF@ 4 buf lenp HLD-BUF-FIX buf lenp HLD-COMMA
   s" latency_ms" buf lenp HLD-KEY
   HLD-MODE-RUN? if 0 buf lenp HLD-BUF-U 46 buf lenp HLD-BUF-C 0 buf lenp HLD-BUF-3 else HLD-REC-IMG @ HLD-REC-DECISION @ buf lenp HLD-BUF-MS then
   buf lenp HLD-COMMA
   s" queue_depth" buf lenp HLD-KEY 0 buf lenp HLD-BUF-U buf lenp HLD-COMMA
   s" decision_timestamp_ns" buf lenp HLD-KEY HLD-REC-DECISION @ buf lenp HLD-BUF-U
   125 buf lenp HLD-BUF-C buf lenp HLD-NL ;

: HLD-DETECTION-RECS ( ptr u8 ptr a -- )
   {: buf:ptr lenp:ptr :}
   0 begin dup HLD-REC-DETS @ < while
      dup buf lenp HLD-DETECTION-REC
      1+
   repeat drop ;

: HLD-RUNNING? ( -- bool )
   HLD-CLK-STOP HLD-CLK-P atomic@ 0=
   TIME-MONO-NS HLD-CLK-DEADLINE HLD-CLK-P atomic@ <
   and ;

: HLD-ABORT? ( -- bool )
   HLD-CLK-ABORT HLD-CLK-P atomic@ 0= 0= ;

: HLD-PUBLISHED-IDX ( n n -- n )
   {: idx:n ts:n :}
   0 begin dup 256 < while
      idx HLD-CAM-LATEST-TS HLD-CAM-P atomic@ ts = if
         drop idx HLD-CAM-LATEST-IDX HLD-CAM-P atomic@ exit
      then
      idx HLD-CAM-PREV-TS HLD-CAM-P atomic@ ts = if
         drop idx HLD-CAM-PREV-IDX HLD-CAM-P atomic@ exit
      then
      1+
   repeat drop
   idx HLD-CAM-LATEST-IDX HLD-CAM-P atomic@ ;

: HLD-WARMUP ( n -- bool )
   {: idx:n :}
   TIME-MONO-NS HLD-WARMUP-MS @ HLD-NS-PER-MS * + {: deadline:n :}
   begin TIME-MONO-NS deadline < while
      idx idx HLD-GRAB 0= 0= if
         1 idx HLD-CAM-RESULT HLD-CAM-P atomic!
         HLD-FALSE exit
      then
   repeat
   HLD-TRUE ;

: HLD-DRAIN-FRESH? ( n -- bool )
   {: idx:n :}
   TIME-MONO-NS HLD-STARTUP-DRAIN-MS @ HLD-NS-PER-MS * + {: deadline:n :}
   begin TIME-MONO-NS deadline < while
      idx idx HLD-GRAB 0= 0= if
         1 idx HLD-CAM-RESULT HLD-CAM-P atomic!
         HLD-FALSE exit
      then
      idx HLD-FRAME-SLOT {: fr:ptr :}
      fr HLD-FRAME-STATUS@ 0= if
         fr HLD-FRAME-CURRENT@ fr HLD-FRAME-IMAGE@ >= if
            fr HLD-FRAME-CURRENT@ fr HLD-FRAME-IMAGE@ - HLD-NS-PER-MS / {: stale:n :}
            stale idx HLD-CAM-STARTUP-STALENESS HLD-CAM-P atomic!
            stale HLD-MAX-START-STALENESS-MS @ <= if HLD-TRUE exit then
         then
      then
   repeat
   1 idx HLD-CAM-RESULT HLD-CAM-P atomic!
   HLD-FALSE ;

: HLD-BARRIER-WAIT ( -- )
   1 HLD-CLK-ARRIVED HLD-CLK-P atomic-add 1+ HLD-CAMERA-N @ = if
      TIME-MONO-NS {: start:n :}
      start HLD-CLK-START HLD-CLK-P atomic!
      start HLD-DURATION-MS @ HLD-NS-PER-MS * + HLD-CLK-DEADLINE HLD-CLK-P atomic!
      1 HLD-CLK-RELEASED HLD-CLK-P atomic!
   else
      begin HLD-CLK-RELEASED HLD-CLK-P atomic@ 0= while HLD-YIELD repeat
   then ;

: HLD-EMIT-TRACKER ( n ptr u8 n -- )
   {: idx:n fr:ptr frameidx:n :}
   fr HLD-FRAME-CURRENT@ fr HLD-FRAME-IMAGE@ >= if
      idx frameidx fr HLD-FRAME-IMAGE@ fr HLD-FRAME-CURRENT@
      idx HLD-OUT-SLOT idx HLD-OUT-LENP HLD-TRACKER-REC
      1 idx HLD-CAM-TRACKER-TICKS HLD-CAM-P atomic-add drop
   then ;

: HLD-PUBLISH-FRAME ( n ptr u8 -- )
   {: idx:n fr:ptr :}
   idx HLD-CAM-FRAMES HLD-CAM-P atomic@ {: frameidx:n :}
   idx HLD-CAM-LATEST-TS HLD-CAM-P atomic@ {: oldts:n :}
   oldts 0= 0= if
      oldts idx HLD-CAM-PREV-TS HLD-CAM-P atomic!
      idx HLD-CAM-LATEST-IDX HLD-CAM-P atomic@ idx HLD-CAM-PREV-IDX HLD-CAM-P atomic!
      fr HLD-FRAME-IMAGE@ oldts < if 1 idx HLD-CAM-REGRESSIONS HLD-CAM-P atomic-add drop then
   then
   fr HLD-FRAME-IMAGE@ idx HLD-CAM-LATEST-TS HLD-CAM-P atomic!
   frameidx idx HLD-CAM-LATEST-IDX HLD-CAM-P atomic!
   fr HLD-FRAME-DROPPED@ idx HLD-CAM-BASELINE-DROPS HLD-CAM-P @ > if
      fr HLD-FRAME-DROPPED@ idx HLD-CAM-BASELINE-DROPS HLD-CAM-P @ -
      idx HLD-CAM-DROPS HLD-CAM-P atomic!
   then
   idx fr frameidx HLD-EMIT-TRACKER
   1 idx HLD-CAM-FRAMES HLD-CAM-P atomic-add drop ;

: HLD-ACQ-STEADY ( n -- )
   {: idx:n :}
   begin HLD-RUNNING? while
      idx idx HLD-GRAB 0= 0= if
         1 idx HLD-CAM-GRAB-ERRORS HLD-CAM-P atomic-add drop
      else
         idx HLD-FRAME-SLOT {: fr:ptr :}
         fr HLD-FRAME-STATUS@ 0= if
            fr HLD-FRAME-IMAGE@ 0= 0= if
               idx fr HLD-PUBLISH-FRAME
            then
         then
      then
   repeat ;

: HLD-FINALIZE-DROPS ( n -- )
   {: idx:n :}
   idx idx HLD-GRAB 0= if
      idx HLD-FRAME-SLOT HLD-FRAME-DROPPED@ idx HLD-CAM-BASELINE-DROPS HLD-CAM-P @ >= if
         idx HLD-FRAME-SLOT HLD-FRAME-DROPPED@ idx HLD-CAM-BASELINE-DROPS HLD-CAM-P @ -
         idx HLD-CAM-DROPS HLD-CAM-P atomic!
      then
   then ;

: HLD-ACQ-RUN ( n -- )
   {: idx:n :}
   idx HLD-WARMUP idx HLD-DRAIN-FRESH? and 0= if
      1 HLD-CLK-ABORT HLD-CLK-P atomic!
   then
   HLD-BARRIER-WAIT
   HLD-ABORT? 0= if idx HLD-ACQ-STEADY then
   idx HLD-FINALIZE-DROPS ;

: HLD-ACQ0 ( -- ) 0 HLD-ACQ-RUN ;
: HLD-ACQ1 ( -- ) 1 HLD-ACQ-RUN ;
: HLD-ACQ2 ( -- ) 2 HLD-ACQ-RUN ;
: HLD-ACQ3 ( -- ) 3 HLD-ACQ-RUN ;

: HLD-DET-CAP? ( -- bool )
   HLD-MAX-INFERENCES @ 0 > if
      HLD-CLK-INFERENCES HLD-CLK-P atomic@ HLD-MAX-INFERENCES @ >= exit
   then
   HLD-FALSE ;

: HLD-TENSOR-ERR ( -- )
   1 HLD-CLK-TENSOR-ERRORS HLD-CLK-P atomic-add drop ;

: HLD-RUN-ERR ( -- )
   1 HLD-CLK-RUN-ERRORS HLD-CLK-P atomic-add drop ;

: HLD-INFER-DECODE ( n -- n bool )
   {: idx:n :}
   idx HLD-INFER-WARM 0= if
      HLD-INFER-RAW @ N>P
      HLD-INFER-ANCHORS @ HLD-INFER-NC @ HLD-INFER-BOX @
      HLD-INFER-IW @ HLD-INFER-IH @
      idx HLD-CAM-WIDTH HLD-CAM-P @ idx HLD-CAM-HEIGHT HLD-CAM-P @
      HLD-CONFIDENCE F@ HLD-MAX-DETECTIONS-PER-FRAME @
      YOLO:DETECT-F32
      YOLO:K-COUNT HLD-TRUE
      exit
   then
   0 HLD-FALSE ;

: HLD-RETRIEVE-PLAIN ( n -- bool )
   HLD-RETRIEVE-FRAME 0= if HLD-TRUE exit then
   HLD-TENSOR-ERR HLD-FALSE ;

: HLD-RETRIEVE-FULL ( n -- bool )
   {: idx:n :}
   HLD-SAVE-GATE @ 1+ HLD-SAVE-GATE !
   HLD-SAVE-GATE @ HLD-SAVE-DUE? if
      idx HLD-SAVE-GATE @ HLD-BUILD-SAVE-PATH
      idx HLD-RETRIEVE-SAVE-COLOR {: rc:n :}
      rc 0 < if HLD-TENSOR-ERR HLD-FALSE exit then
      rc 0= if
         1 HLD-CLK-SAVED-FRAMES HLD-CLK-P atomic-add drop
      else
         1 HLD-CLK-SAVE-ERRORS HLD-CLK-P atomic-add drop
      then
      HLD-TRUE exit
   then
   idx HLD-RETRIEVE-PLAIN ;

: HLD-RETRIEVE-LIVE ( n -- bool )
   HLD-MODE-FULL? if HLD-RETRIEVE-FULL exit then
   HLD-RETRIEVE-PLAIN ;

: HLD-EMIT-DET-RESULT ( n n n n n n n n n n -- )
   HLD-REC!
   HLD-CAMERA-N @ HLD-OUT-SLOT HLD-CAMERA-N @ HLD-OUT-LENP HLD-TICK-REC
   HLD-REC-DETS @ 0 > if
      HLD-REC-DETS @ HLD-CLK-DETECTIONS HLD-CLK-P atomic-add drop
      HLD-EMIT-DETECTIONS @ 0= 0= if
         HLD-CAMERA-N @ HLD-OUT-SLOT HLD-CAMERA-N @ HLD-OUT-LENP HLD-DETECTION-RECS
      then
   then ;

: HLD-DET-ONE-LIVE ( n -- )
   HLD-DET-IDX !
   HLD-DET-IDX @ HLD-CAM-LATEST-TS HLD-CAM-P atomic@ 0= if exit then
   TIME-MONO-NS HLD-DET-TSTART !
   HLD-DET-IDX @ HLD-RETRIEVE-LIVE 0= if exit then
   TIME-MONO-NS HLD-DET-TEND !
   HLD-DET-FRAME HLD-FRAME-IMAGE@ dup 0= if drop exit then
   HLD-DET-IMG !
   HLD-DET-IDX @ HLD-DET-IMG @ HLD-PUBLISHED-IDX HLD-DET-FRAMEIDX !
   HLD-DET-TEND @ HLD-DET-RSTART !
   HLD-DET-TEND @ HLD-DET-REND !
   0 HLD-DET-DETS !
   HLD-MODE-FULL? if
      TIME-MONO-NS HLD-DET-RSTART !
      HLD-DET-IDX @ HLD-INFER-DECODE 0= if drop HLD-RUN-ERR exit then
      HLD-DET-DETS !
      TIME-MONO-NS HLD-DET-REND !
      0 HLD-DET-NOW !
      HLD-DET-IDX @ HLD-DET-NOW HLD-NOW drop
      1 HLD-CLK-INFERENCES HLD-CLK-P atomic-add 1+ HLD-DET-INF !
      HLD-DET-IDX @ HLD-DET-FRAMEIDX @ HLD-DET-IMG @ HLD-DET-NOW @ HLD-DET-DETS @
      HLD-DET-TSTART @ HLD-DET-TEND @ HLD-DET-RSTART @ HLD-DET-REND @ HLD-DET-INF @
      HLD-EMIT-DET-RESULT
      exit
   then
   0 HLD-DET-NOW !
   HLD-DET-IDX @ HLD-DET-NOW HLD-NOW drop
   1 HLD-CLK-INFERENCES HLD-CLK-P atomic-add 1+ HLD-DET-INF !
   HLD-DET-IDX @ HLD-DET-FRAMEIDX @ HLD-DET-IMG @ HLD-DET-NOW @ HLD-DET-DETS @
   HLD-DET-TSTART @ HLD-DET-TEND @ HLD-DET-RSTART @ HLD-DET-REND @ HLD-DET-INF @
   HLD-EMIT-DET-RESULT ;

: HLD-WAIT-LATEST ( n -- bool )
   {: idx:n :}
   begin idx HLD-CAM-LATEST-TS HLD-CAM-P atomic@ 0= while
      HLD-ABORT? if HLD-FALSE exit then
      HLD-CLK-STOP HLD-CLK-P atomic@ 0= 0= if HLD-FALSE exit then
      HLD-YIELD
   repeat
   HLD-TRUE ;

: HLD-PRIME-ONE ( n -- bool )
   {: idx:n :}
   idx HLD-WAIT-LATEST 0= if HLD-FALSE exit then
   0 begin dup 8 < while
      idx HLD-RETRIEVE-TENSOR 0= if
         drop idx HLD-CAM-LATEST-TS HLD-CAM-P atomic@ idx HLD-CAM-PRIMED-TS HLD-CAM-P atomic!
         HLD-TRUE exit
      then
      HLD-TENSOR-ERR
      HLD-YIELD
      1+
   repeat drop
   HLD-FALSE ;

: HLD-PRIME-TENSORS ( -- bool )
   0 begin dup HLD-CAMERA-N @ < while
      dup HLD-PRIME-ONE 0= if drop HLD-FALSE exit then
      1+
   repeat drop
   HLD-TRUE ;

: HLD-DET-ONE-RUN ( n -- )
   HLD-DET-IDX !
   HLD-DET-IDX @ HLD-CAM-PRIMED-TS HLD-CAM-P atomic@ dup 0= if drop exit then
   HLD-DET-IMG !
   HLD-DET-IDX @ HLD-CAM-LATEST-IDX HLD-CAM-P atomic@ HLD-DET-FRAMEIDX !
   TIME-MONO-NS HLD-DET-RSTART !
   HLD-DET-IDX @ HLD-INFER-DECODE 0= if drop HLD-RUN-ERR exit then
   HLD-DET-DETS !
   TIME-MONO-NS HLD-DET-REND !
   0 HLD-DET-NOW !
   HLD-DET-IDX @ HLD-DET-NOW HLD-NOW drop
   1 HLD-CLK-INFERENCES HLD-CLK-P atomic-add 1+ HLD-DET-INF !
   HLD-DET-IDX @ HLD-DET-FRAMEIDX @ HLD-DET-IMG @ HLD-DET-NOW @ HLD-DET-DETS @
   0 0 HLD-DET-RSTART @ HLD-DET-REND @ HLD-DET-INF @ HLD-EMIT-DET-RESULT ;

: HLD-DET-ONE ( n -- )
   HLD-MODE-RUN? if HLD-DET-ONE-RUN exit then
   HLD-DET-ONE-LIVE ;

: HLD-SELF-EMIT-TASK ( -- )
   0 9 1000000000 1001234567 0 1000000001 1002000001 1002000001 1002000001 3 HLD-EMIT-DET-RESULT ;

: HLD-DET-RUN ( -- )
   begin HLD-CLK-START HLD-CLK-P atomic@ 0= while
      HLD-ABORT? if exit then
      HLD-YIELD
   repeat
   HLD-ABORT? if exit then
   HLD-MODE-RUN? if
      HLD-PRIME-TENSORS 0= if 1 HLD-CLK-ABORT HLD-CLK-P atomic! exit then
   then
   begin HLD-RUNNING? HLD-DET-CAP? 0= and while
      HLD-DET-SELECT @ HLD-CAMERA-N @ mod HLD-DET-IDX !
      HLD-DET-SELECT @ 1+ HLD-DET-SELECT !
      HLD-DET-IDX @ HLD-DET-ONE
      HLD-YIELD
   repeat ;

: HLD-START-XT ( n n -- )
   {: xt:n slot:n :}
   xt slot HLD-TASK-P TASK:ACTIVATE ;

: HLD-START-ACQ ( n -- )
   dup 0 = if drop ['] HLD-ACQ0 0 HLD-START-XT exit then
   dup 1 = if drop ['] HLD-ACQ1 1 HLD-START-XT exit then
   dup 2 = if drop ['] HLD-ACQ2 2 HLD-START-XT exit then
   dup 3 = if drop ['] HLD-ACQ3 3 HLD-START-XT exit then
   drop E-HLD-CAMERA throw ;

: HLD-START-TASKS ( -- )
   0 begin dup HLD-CAMERA-N @ < while
      dup HLD-START-ACQ
      1+
   repeat drop
   ['] HLD-DET-RUN HLD-CAMERA-N @ HLD-START-XT ;

: HLD-JOIN-TASKS ( -- )
   0 begin dup HLD-CAMERA-N @ 1+ < while
      dup HLD-TASK-P
      begin dup TASK:DONE? 0= while TASK:PAUSE repeat
      TASK:KILL
      1+
   repeat drop
   1 HLD-CLK-STOP HLD-CLK-P atomic! ;

: HLD-SAMPLE-BASELINE ( -- )
   0 begin dup HLD-CAMERA-N @ < while
      dup 0 HLD-GRAB 0= if
         dup HLD-FRAME-SLOT HLD-FRAME-DROPPED@ over HLD-CAM-BASELINE-DROPS HLD-CAM-P !
      then
      1+
   repeat drop ;

: HLD-WARM-DETECTOR ( -- )
   HLD-DETECTOR-WARMUP-INFERENCES @ 0 <= if exit then
   HLD-CAMERA-N @ 0 <= if exit then
   0 HLD-WARM-COMPLETED !
   0 HLD-WARM-CURSOR !
   begin HLD-WARM-COMPLETED @ HLD-DETECTOR-WARMUP-INFERENCES @ < while
      HLD-WARM-CURSOR @ HLD-CAMERA-N @ mod {: idx:n :}
      HLD-WARM-CURSOR @ 1+ HLD-WARM-CURSOR !
      idx 0 HLD-GRAB 0= if
         idx HLD-FRAME-SLOT HLD-FRAME-STATUS@ 0= if
            idx HLD-RETRIEVE-TENSOR 0= if
               idx HLD-INFER-WARM 0= if
                  HLD-WARM-COMPLETED @ 1+ HLD-WARM-COMPLETED !
               then
            then
         then
      then
   repeat ;

: HLD-APPEND-SLOT ( n -- )
   {: idx:n :}
   idx HLD-OUT-LENP @ 0 > if
      HLD-OUTPUT-PATH HLD-OUTPUT-U @ idx HLD-OUT-SLOT idx HLD-OUT-LENP @ APPEND-FILE
   then ;

: HLD-WRITE-OUTPUT ( -- )
   HLD-OUTPUT-PATH HLD-OUTPUT-U @ HLD-OUT-BUFS 0 WRITE-ALL
   0 begin dup HLD-CAMERA-N @ 1+ < while
      dup HLD-APPEND-SLOT
      1+
   repeat drop ;

: HLD-SUMMARY ( -- )
   0 begin dup HLD-CAMERA-N @ < while
      s" camera_summary logical_name=" type dup HLD-CAM-LOGICAL$ type
      s"  serial=" type dup HLD-CAM-SERIAL$ type
      s"  staleness_ms=" type dup HLD-CAM-STARTUP-STALENESS HLD-CAM-P atomic@ .
      s" frames=" type dup HLD-CAM-FRAMES HLD-CAM-P atomic@ .
      s" drops=" type dup HLD-CAM-DROPS HLD-CAM-P atomic@ .
      s" regressions=" type dup HLD-CAM-REGRESSIONS HLD-CAM-P atomic@ .
      s" grab_errors=" type dup HLD-CAM-GRAB-ERRORS HLD-CAM-P atomic@ .
      s" tracker_ticks=" type dup HLD-CAM-TRACKER-TICKS HLD-CAM-P atomic@ . cr
      1+
   repeat drop
   s" detector_summary mode=" type HLD-MODE-LABEL type
   s"  cameras=" type HLD-CAMERA-N @ .
   s" inferences=" type HLD-CLK-INFERENCES HLD-CLK-P atomic@ .
   s" tensor_errors=" type HLD-CLK-TENSOR-ERRORS HLD-CLK-P atomic@ .
   s" run_errors=" type HLD-CLK-RUN-ERRORS HLD-CLK-P atomic@ .
   s" detections=" type HLD-CLK-DETECTIONS HLD-CLK-P atomic@ .
   s" saved_frames=" type HLD-CLK-SAVED-FRAMES HLD-CLK-P atomic@ .
   s" save_errors=" type HLD-CLK-SAVE-ERRORS HLD-CLK-P atomic@ .
   s" measured_ms=" type HLD-DURATION-MS @ . cr
   s" output=" type HLD-OUTPUT-PATH HLD-OUTPUT-U @ type cr ;

: HLD-RUN ( -- )
   HLD-DEFAULTS
   HLD-PARSE-ARGS
   HLD-ZERO-STATE
   HLD-OPEN-ABI
   HLD-OPEN-YIELD
   HLD-CREATE-CONTEXT
   HLD-OPEN-CAMERAS
   HLD-ENSURE-SAVE-DIR
   HLD-WARM-DETECTOR
   HLD-SAMPLE-BASELINE
   HLD-START-TASKS
   HLD-JOIN-TASKS
   HLD-WRITE-OUTPUT
   HLD-SUMMARY
   HLD-CALL-DESTROY ;

: HLD-SELF-TEST ( -- )
   HLD-DEFAULTS
   HLD-ZERO-STATE
   s" 123:cam_a0" HLD-ADD-CAMERA
   HLD-CAMERA-N @ 1 = HLD-ASSERT
   0 HLD-CAM-SERIAL$ s" 123" STR= HLD-ASSERT
   0 HLD-CAM-LOGICAL$ s" cam_a0" STR= HLD-ASSERT
   0 7 1000000000 1001234567 0 HLD-OUT-SLOT 0 HLD-OUT-LENP HLD-TRACKER-REC
   0 HLD-OUT-SLOT 0 HLD-OUT-LENP @ s" odin.tracker_tick.v1" CONTAINS? HLD-ASSERT
   0 7 1000000000 1001234567 0 1000000001 1002000001 1002000001 1002000001 1 HLD-REC!
   0 HLD-OUT-SLOT 0 HLD-OUT-LENP HLD-TICK-REC
   0 HLD-OUT-SLOT 0 HLD-OUT-LENP @ s" odin.perception_tick.v1" CONTAINS? HLD-ASSERT
   0 HLD-OUT-SLOT 0 HLD-OUT-LENP @ s" mode" CONTAINS? HLD-ASSERT
   0 8 1000000000 1001234567 0 1000000001 1002000001 1002000001 1002000001 2 HLD-EMIT-DET-RESULT
   HLD-CAMERA-N @ HLD-OUT-SLOT HLD-CAMERA-N @ HLD-OUT-LENP @ s" odin.perception_tick.v1" CONTAINS? HLD-ASSERT
   ['] HLD-SELF-EMIT-TASK HLD-SELF-TASK TASK:ACTIVATE
   begin HLD-SELF-TASK TASK:DONE? 0= while TASK:PAUSE repeat
   HLD-SELF-TASK TASK:KILL
   s" live-detector self-test ok" type cr ;

: HLD-MAIN ( -- )
   SCRIPT-ARGC 1 = if
      0 SCRIPT-ARGV$ s" --self-test" STR= if HLD-SELF-TEST exit then
   then
   HLD-RUN ;

HLD-MAIN
