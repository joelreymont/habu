\ yolo-decode.f - YOLO detection geometry, ported from Odin's src/yolo_decode.zig.
\
\ The detection-geometry core: 1-D interval overlap and box IoU, plus the full
\ decode + greedy-NMS orchestration (decode() in the Zig source). Detections are
\ held in parallel float-cell arrays; NMS is done by selection (repeatedly take
\ the highest-confidence detection still unprocessed), which yields exactly the
\ sort-then-greedy result without sorting floats. Habu floats are f64 where the
\ Zig uses f32, but the oracle values are exact in both.
\ Depends on lib/errors.f lib/string.f lib/float.f.

package YOLO
private
: FMIN ( r r -- r ) {: a:r b:r :} a b f< if a else b then ;
: FMAX ( r r -- r ) {: a:r b:r :} a b f> if a else b then ;

\ 1-D overlap of [amin,amax] and [bmin,bmax] = max(0, min(maxes) - max(mins)).
public
: OVERLAP1D ( r r r r -- r ) {: amin:r amax:r bmin:r bmax:r :}
   amax bmax FMIN  amin bmin FMAX  f-
   0.0 FMAX ;

\ IoU of boxes a=(ax0,ay0,ax1,ay1) and b=(bx0,by0,bx1,by1).
: IOU ( r r r r r r r r -- r ) {: ax0:r ay0:r ax1:r ay1:r bx0:r by0:r bx1:r by1:r :}
   ax0 ax1 bx0 bx1 OVERLAP1D
   ay0 ay1 by0 by1 OVERLAP1D
   f* {: inter:r :}
   ax1 ax0 f- ay1 ay0 f- f*
   bx1 bx0 f- by1 by0 f- f*
   f+ inter f- {: uni:r :}
   uni 0.0 f= if 0.0 else inter uni f/ then ;

\ Clamp v to [lo, hi].
: FCLAMP ( r r r -- r ) {: v:r lo:r hi:r :}
   v lo FMAX hi FMIN ;

\ Letterbox unmap factors for an inference (iw,ih) over a source (ow,oh).
\ Returns the unmap scale (1/fit-scale) and the centering offsets, matching the
\ Zig: sf=min(iw/ow,ih/oh); xoff=(iw-sf*ow)/2; yoff=(ih-sf*oh)/2; sf:=1/sf.
: LETTERBOX ( n n n n -- r r r ) {: iw:n ih:n ow:n oh:n :}
   iw s>f ow s>f f/   ih s>f oh s>f f/  FMIN {: scale:r :}
   iw s>f  scale ow s>f f*  f-  0.5 f* {: xoff:r :}
   ih s>f  scale oh s>f f*  f-  0.5 f* {: yoff:r :}
   1.0 scale f/  xoff  yoff ;

\ Decode one anchor's (cx,cy,w,h) into a clamped [x0,y0,x1,y1] source-space box,
\ given the letterbox offsets/scale and source extent (ow,oh).
: DECODE-BOX ( r r r r r r r r r -- r r r r ) {: cx:r cy:r w:r h:r xoff:r yoff:r sf:r ow:r oh:r :}
   cx xoff f- {: x:r :}
   cy yoff f- {: y:r :}
   x  0.5 w f* f-  sf f*  0.0 ow FCLAMP
   y  0.5 h f* f-  sf f*  0.0 oh FCLAMP
   x  0.5 w f* f+  sf f*  0.0 ow FCLAMP
   y  0.5 h f* f+  sf f*  0.0 oh FCLAMP ;

\ ---------------------------------------------------------------------------
\ decode() + greedy NMS. raw is a channel-major f32-style buffer indexed
\ raw[channel*num_anchors + anchor]; channels 0..3 are cx,cy,w,h and box+j is
\ class j's score. Decoded detections accumulate in the D* arrays; NMS selects
\ them highest-confidence-first into the K* arrays (== sort-then-greedy).
\ ---------------------------------------------------------------------------

1024 constant CAP
-6208 constant E-FULL
create DX0 CAP cells allot   create DY0 CAP cells allot
create DX1 CAP cells allot   create DY1 CAP cells allot
create DLBL CAP cells allot  create DCONF CAP cells allot
create DDONE CAP cells allot
create KX0 CAP cells allot   create KY0 CAP cells allot
create KX1 CAP cells allot   create KY1 CAP cells allot
create KLBL CAP cells allot  create KCONF CAP cells allot
variable DN    variable KN
variable JI variable SI  variable KI  variable MI  variable BJ
variable S  variable BS  variable BOX-X0  variable BOX-Y0  variable BOX-X1  variable BOX-Y1
variable BEST variable SUP variable THRES variable MAXDET

\ decode one anchor: argmax over classes, threshold, box-decode, validity, append
private
: ANCHOR ( ptr a n n n n r r r r r r -- ) {: raw:ptr na:n nc:n box:n ai:n ow:r oh:r sf:r xoff:r yoff:r conf:r :}
   raw  box na * ai +  cells + F@  BS F!   0 BJ !
   1 JI !
   begin JI @ nc < while
      raw  box JI @ + na * ai +  cells + F@  S F!
      S F@ BS F@ f> if  S F@ BS F!  JI @ BJ !  then
      JI @ 1+ JI !
   repeat
   BS F@ conf f> if
      raw 0 na * ai + cells + F@   raw 1 na * ai + cells + F@
      raw 2 na * ai + cells + F@   raw 3 na * ai + cells + F@
      xoff yoff sf ow oh DECODE-BOX
      BOX-Y1 F!  BOX-X1 F!  BOX-Y0 F!  BOX-X0 F!
      BOX-X0 F@ BOX-X1 F@ f>  BOX-Y0 F@ BOX-Y1 F@ f>  or 0= if
         DN @ CAP >= if E-FULL throw then       \ guard: never silently drop a detection
         BOX-X0 F@ DX0 DN @ cells + F!   BOX-Y0 F@ DY0 DN @ cells + F!
         BOX-X1 F@ DX1 DN @ cells + F!   BOX-Y1 F@ DY1 DN @ cells + F!
         BJ @ DLBL DN @ cells + !  BS F@ DCONF DN @ cells + F!
         DN @ 1+ DN !
      then
   then ;

: ANCHOR-F32 ( ptr u8 n n n n r r r r r r -- ) {: raw:ptr na:n nc:n box:n ai:n ow:r oh:r sf:r xoff:r yoff:r conf:r :}
   raw  box na * ai +  FC-F32-I@  BS F!   0 BJ !
   1 JI !
   begin JI @ nc < while
      raw  box JI @ + na * ai +  FC-F32-I@  S F!
      S F@ BS F@ f> if  S F@ BS F!  JI @ BJ !  then
      JI @ 1+ JI !
   repeat
   BS F@ conf f> if
      raw 0 na * ai + FC-F32-I@   raw 1 na * ai + FC-F32-I@
      raw 2 na * ai + FC-F32-I@   raw 3 na * ai + FC-F32-I@
      xoff yoff sf ow oh DECODE-BOX
      BOX-Y1 F!  BOX-X1 F!  BOX-Y0 F!  BOX-X0 F!
      BOX-X0 F@ BOX-X1 F@ f>  BOX-Y0 F@ BOX-Y1 F@ f>  or 0= if
         DN @ CAP >= if E-FULL throw then
         BOX-X0 F@ DX0 DN @ cells + F!   BOX-Y0 F@ DY0 DN @ cells + F!
         BOX-X1 F@ DX1 DN @ cells + F!   BOX-Y1 F@ DY1 DN @ cells + F!
         BJ @ DLBL DN @ cells + !  BS F@ DCONF DN @ cells + F!
         DN @ 1+ DN !
      then
   then ;

public
: DECODE ( ptr a n n n n n n n r -- ) {: raw:ptr na:n nc:n box:n iw:n ih:n ow:n oh:n conf:r :}
   0 DN !
   iw ih ow oh LETTERBOX {: sf:r xoff:r yoff:r :}
   0 SI !
   begin SI @ na < while
      raw na nc box SI @  ow s>f oh s>f  sf xoff yoff conf  ANCHOR
      SI @ 1+ SI !
   repeat ;

: DECODE-F32 ( ptr u8 n n n n n n n r -- ) {: raw:ptr na:n nc:n box:n iw:n ih:n ow:n oh:n conf:r :}
   0 DN !
   iw ih ow oh LETTERBOX {: sf:r xoff:r yoff:r :}
   0 SI !
   begin SI @ na < while
      raw na nc box SI @  ow s>f oh s>f  sf xoff yoff conf  ANCHOR-F32
      SI @ 1+ SI !
   repeat ;

\ set MI = index of the highest-confidence not-yet-processed detection, or -1
private
: NMS-FINDMAX ( -- )
   -1 MI !   0 SI !
   begin SI @ DN @ < while
      DDONE SI @ cells + @ 0= if
         MI @ 0 < if
            SI @ MI !   DCONF SI @ cells + F@ BEST F!
         else
            DCONF SI @ cells + F@ BEST F@ f> if
               SI @ MI !   DCONF SI @ cells + F@ BEST F!
            then
         then
      then
      SI @ 1+ SI !
   repeat ;

\ does detection di overlap any already-kept box by more than the NMS threshold?
: NMS-SUPPRESSED? ( n -- bool ) {: di:n :}
   0 SUP !   0 KI !
   begin KI @ KN @ < SUP @ 0= and while
      DX0 di cells + F@  DY0 di cells + F@  DX1 di cells + F@  DY1 di cells + F@
      KX0 KI @ cells + F@  KY0 KI @ cells + F@  KX1 KI @ cells + F@  KY1 KI @ cells + F@
      IOU  THRES F@ f> if -1 SUP ! then
      KI @ 1+ KI !
   repeat
   SUP @ 0 <> ;

\ greedy NMS over the decoded detections into the kept arrays
public
: NMS ( -- )
   0 KN !
   0 SI ! begin SI @ DN @ < while  0 DDONE SI @ cells + !  SI @ 1+ SI !  repeat
   begin
      NMS-FINDMAX  MI @ 0 >=  KN @ MAXDET @ <  and
   while
      1 DDONE MI @ cells + !
      MI @ NMS-SUPPRESSED? 0= if
         DX0 MI @ cells + F@ KX0 KN @ cells + F!   DY0 MI @ cells + F@ KY0 KN @ cells + F!
         DX1 MI @ cells + F@ KX1 KN @ cells + F!   DY1 MI @ cells + F@ KY1 KN @ cells + F!
         DLBL MI @ cells + @ KLBL KN @ cells + ! DCONF MI @ cells + F@ KCONF KN @ cells + F!
         KN @ 1+ KN !
      then
   repeat ;

: K-COUNT ( -- n ) KN @ ;
: K-X0@ ( n -- r ) cells KX0 + F@ ;
: K-Y0@ ( n -- r ) cells KY0 + F@ ;
: K-X1@ ( n -- r ) cells KX1 + F@ ;
: K-Y1@ ( n -- r ) cells KY1 + F@ ;
: K-LBL@ ( n -- n ) cells KLBL + @ ;
: K-CONF@ ( n -- r ) cells KCONF + F@ ;
: DETECT ( ptr a n n n n n n n r n -- ) {: raw:ptr na:n nc:n box:n iw:n ih:n ow:n oh:n conf:r keep:n :}
   0.4 THRES F!  keep MAXDET !
   raw na nc box iw ih ow oh conf DECODE
   NMS ;
: DETECT-F32 ( ptr u8 n n n n n n n r n -- ) {: raw:ptr na:n nc:n box:n iw:n ih:n ow:n oh:n conf:r keep:n :}
   0.4 THRES F!  keep MAXDET !
   raw na nc box iw ih ow oh conf DECODE-F32
   NMS ;
end-package
