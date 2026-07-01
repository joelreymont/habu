\ yolo-decode-test.f - oracle tests against src/yolo_decode.zig "iou and overlap".
\ Run: ../habu/bin/hb --load odin/yolo-decode-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/float.f
require odin/float-cell.f
require odin/yolo-decode.f

package YOLO
private
: FL-NEAR ( r r -- bool ) f- fabs 0.000001 f< ;
: T-NEAR ( r r -- ) FL-NEAR T-ASSERT ;

: RUN ( -- )
   T-RESET
   \ a=[0,0,10,10] b=[5,0,15,10] : inter 5x10=50, union 150 -> 1/3
   0.0 0.0 10.0 10.0  5.0 0.0 15.0 10.0 IOU   1.0 3.0 f/ T-NEAR
   \ a vs far box -> 0
   0.0 0.0 10.0 10.0  100.0 100.0 110.0 110.0 IOU  0.0 T-NEAR
   \ overlap1D [0,10] vs [5,15] -> 5 ; vs [100,110] -> 0 ; identical -> 10
   0.0 10.0 5.0 15.0 OVERLAP1D    5.0  T-NEAR
   0.0 10.0 100.0 110.0 OVERLAP1D 0.0  T-NEAR
   0.0 10.0 0.0 10.0 OVERLAP1D    10.0 T-NEAR
   \ FCLAMP
   5.0 0.0 10.0 FCLAMP   5.0  T-NEAR
   -3.0 0.0 10.0 FCLAMP  0.0  T-NEAR
   15.0 0.0 10.0 FCLAMP  10.0 T-NEAR
   \ letterbox 640 over 1280 -> unmap sf 2, no padding (matches the decode oracle)
   640 640 1280 1280 LETTERBOX  {: sf:r xoff:r yoff:r :}
   sf 2.0 T-NEAR  xoff 0.0 T-NEAR  yoff 0.0 T-NEAR
   \ decode anchor0 (cx,cy=320, w,h=64) -> [576,576,704,704]
   320.0 320.0 64.0 64.0  0.0 0.0 2.0  1280.0 1280.0 DECODE-BOX
   {: x0:r y0:r x1:r y1:r :}
   x0 576.0 T-NEAR  y0 576.0 T-NEAR  x1 704.0 T-NEAR  y1 704.0 T-NEAR
   \ decode anchor2 (cx,cy=100, w,h=40) -> [160,160,240,240]
   100.0 100.0 40.0 40.0  0.0 0.0 2.0  1280.0 1280.0 DECODE-BOX
   {: bx0:r by0:r bx1:r by1:r :}
   bx0 160.0 T-NEAR  by0 160.0 T-NEAR  bx1 240.0 T-NEAR  by1 240.0 T-NEAR ;

\ decode + NMS oracle, from "snap: decode + NMS of a synthetic raw buffer".
\ 3 anchors, 1 class, box=4, in 640, source 1280 (sf 2.0, no padding), conf 0.5.
\ channel-major [5][3]: anchor0 box A conf 0.9, anchor1 box A conf 0.7 (NMS-dropped),
\ anchor2 box B conf 0.8.
create RAW 15 cells allot
create RAW32 60 allot
: SETRAW ( -- )
   320.0 RAW  0 cells + F!  320.0 RAW  1 cells + F!  100.0 RAW  2 cells + F!   \ cx
   320.0 RAW  3 cells + F!  320.0 RAW  4 cells + F!  100.0 RAW  5 cells + F!   \ cy
   64.0  RAW  6 cells + F!  64.0  RAW  7 cells + F!  40.0  RAW  8 cells + F!   \ w
   64.0  RAW  9 cells + F!  64.0  RAW 10 cells + F!  40.0  RAW 11 cells + F!   \ h
   0.9   RAW 12 cells + F!  0.7   RAW 13 cells + F!  0.8   RAW 14 cells + F! ; \ class 0

: NMS-RUN ( -- )
   SETRAW
   0.4 THRES F!  32 MAXDET !
   RAW 3 1 4 640 640 1280 1280 0.5 DECODE
   NMS
   2 K-COUNT T=
   \ kept[0]: box A (highest conf 0.9)
   576.0 0 K-X0@ T-NEAR  576.0 0 K-Y0@ T-NEAR  704.0 0 K-X1@ T-NEAR  704.0 0 K-Y1@ T-NEAR
   0 0 K-LBL@ T=  0.9 0 K-CONF@ T-NEAR
   \ kept[1]: box B (conf 0.8); the 0.7 duplicate of box A is suppressed
   160.0 1 K-X0@ T-NEAR  160.0 1 K-Y0@ T-NEAR  240.0 1 K-X1@ T-NEAR  240.0 1 K-Y1@ T-NEAR
   0 1 K-LBL@ T=  0.8 1 K-CONF@ T-NEAR ;

: W32! ( n ptr u8 -- ) {: w:n p:ptr :}
   w $FF and p c!
   w 8 rshift $FF and p 1 + c!
   w 16 rshift $FF and p 2 + c!
   w 24 rshift $FF and p 3 + c! ;

: RAW32! ( n n -- ) {: bits:n idx:n :}
   bits RAW32 idx 4 * + W32! ;

: SETRAW32 ( -- )
   $43A00000 0 RAW32!  $43A00000 1 RAW32!  $42C80000 2 RAW32!   \ cx
   $43A00000 3 RAW32!  $43A00000 4 RAW32!  $42C80000 5 RAW32!   \ cy
   $42800000 6 RAW32!  $42800000 7 RAW32!  $42200000 8 RAW32!   \ w
   $42800000 9 RAW32!  $42800000 10 RAW32! $42200000 11 RAW32!  \ h
   $3F666666 12 RAW32! $3F333333 13 RAW32! $3F4CCCCD 14 RAW32! ; \ class 0

: NMS-F32-RUN ( -- )
   SETRAW32
   RAW32 3 1 4 640 640 1280 1280 0.5 32 DETECT-F32
   2 K-COUNT T=
   576.0 0 K-X0@ T-NEAR  576.0 0 K-Y0@ T-NEAR  704.0 0 K-X1@ T-NEAR  704.0 0 K-Y1@ T-NEAR
   0 0 K-LBL@ T=  0.9 0 K-CONF@ T-NEAR
   160.0 1 K-X0@ T-NEAR  160.0 1 K-Y0@ T-NEAR  240.0 1 K-X1@ T-NEAR  240.0 1 K-Y1@ T-NEAR
   0 1 K-LBL@ T=  0.8 1 K-CONF@ T-NEAR ;

RUN
NMS-RUN
NMS-F32-RUN
T-REPORT
end-package
