\ tracker.f - tracking geometry, ported from src/tracker.zig (iou + distance).
\ BBox IoU over (x,y,width,height) rectangles and Euclidean pixel distance, the
\ gating primitives the per-frame detection-to-track association is built on.
\ Floats are kept in scratch vars across the early-out so no local is bound after
\ an exit. Depends on lib/errors.f lib/string.f lib/float.f. The full association
\ algorithm (gated candidates -> greedy nearest assignment -> track lifecycle /
\ confirm_hits / max_age) is the next step on this analyzer.

package TRACKER
private
: FMINR ( r r -- r ) {: a:r b:r :} a b f< if a else b then ;
: FMAXR ( r r -- r ) {: a:r b:r :} a b f> if a else b then ;

variable TRK-IW  variable TRK-IH  variable TRK-INT  variable TRK-UNI

\ IoU of two boxes given as (x,y,width,height)
public
: TRK-IOU ( r r r r r r r r -- r ) {: ax:r ay:r aw:r ah:r bx:r by:r bw:r bh:r :}
   ax aw f+  bx bw f+  FMINR   ax bx FMAXR   f-  TRK-IW !    \ ix2 - ix1
   ay ah f+  by bh f+  FMINR   ay by FMAXR   f-  TRK-IH !    \ iy2 - iy1
   TRK-IW @ 0.0 f>  TRK-IH @ 0.0 f>  and  0= if 0.0 exit then
   TRK-IW @ TRK-IH @ f* TRK-INT !
   aw ah f*  bw bh f* f+  TRK-INT @ f-  TRK-UNI !
   TRK-UNI @ 0.0 f>  0= if 0.0 exit then
   TRK-INT @ TRK-UNI @ f/ ;

\ Euclidean distance between two pixel centers
: TRK-DIST ( r r r r -- r ) {: ax:r ay:r bx:r by:r :}
   ax bx f- {: dx:r :}   ay by f- {: dy:r :}
   dx dx f*  dy dy f*  f+  fsqrt ;

\ squared distance — monotonic with TRK-DIST, used for gating/selection (no sqrt)
private
: TRK-DIST2 ( r r r r -- r ) {: ax:r ay:r bx:r by:r :}
   ax bx f- {: dx:r :}   ay by f- {: dy:r :}   dx dx f*  dy dy f*  f+ ;

\ ---------------------------------------------------------------------------
\ Per-frame detection-to-track association (associateFrame + trackCamera, one
\ camera). Detections for a frame are buffered with FD-ADD, then TK-FRAME runs:
\ build gated candidate (track,det) pairs, assign greedily by ascending distance
\ (selection, == sort-then-greedy), update matched tracks (confirm at confirm_hits),
\ spawn tracks for unmatched detections, age out tracks past max_age. The reported
\ counters mirror the .zig Report: records, tracks_created/confirmed,
\ detections_matched/new. Distances are squared throughout; track length stats and
\ the JSON track-update emission are the next steps.
\ ---------------------------------------------------------------------------

256 constant TK-MAX
64 constant FD-MAX
1024 constant CA-MAX

create TK-CX TK-MAX cells allot   create TK-CY TK-MAX cells allot
create TK-BX TK-MAX cells allot   create TK-BY TK-MAX cells allot
create TK-BW TK-MAX cells allot   create TK-BH TK-MAX cells allot
create TK-HASBOX TK-MAX cells allot
create TK-HITS TK-MAX cells allot create TK-MISSES TK-MAX cells allot
create TK-CONFIRMED TK-MAX cells allot  create TK-ALIVE TK-MAX cells allot
create TK-USED TK-MAX cells allot
create TK-FIRST TK-MAX cells allot  create TK-LAST TK-MAX cells allot  \ first/last frame index
create TK-LEN TK-MAX cells allot                                       \ scratch: track lengths
variable TK-N  variable TK-CURFIDX
variable TK-LSUM variable TK-LCNT variable TK-LMED variable TK-LMAXV variable TK-THITS variable TK-LI

create FD-CX FD-MAX cells allot   create FD-CY FD-MAX cells allot
create FD-BX FD-MAX cells allot   create FD-BY FD-MAX cells allot
create FD-BW FD-MAX cells allot   create FD-BH FD-MAX cells allot
create FD-HASBOX FD-MAX cells allot  create FD-TRK FD-MAX cells allot
variable FD-N

create CA-T CA-MAX cells allot   create CA-D CA-MAX cells allot
create CA-COST CA-MAX cells allot create CA-DONE CA-MAX cells allot
variable CA-N  variable CA-MIN  variable CA-BEST

variable TK-MAXD2 variable TK-MINIOU variable TK-CONFIRM variable TK-MAXAGE
variable TK-TI variable TK-DI variable TK-SI variable TK-TIV variable TK-COST variable TK-PASS
variable TK-CREATED variable TK-CONFCNT variable TK-MATCHED variable TK-NEW variable TK-RECORDS
variable TK-FCNT    \ distinct frames processed (one per TK-FRAME call)

-6206 constant E-TK-FULL  -6207 constant E-CA-FULL  -6209 constant E-FD-FULL
public
: TK-RESET ( -- )
   0 TK-N !  0 TK-CREATED !  0 TK-CONFCNT !  0 TK-MATCHED !  0 TK-NEW !  0 TK-RECORDS !  0 TK-FCNT !  0 FD-N ! ;
: TK-SETUP ( r r n n -- ) {: maxd:r miniou:r confirm:n maxage:n :}      \ max dist, min iou, confirm, age
   maxd maxd f* TK-MAXD2 F!  miniou TK-MINIOU F!  confirm TK-CONFIRM !  maxage TK-MAXAGE ! ;
private
: FD-RESET ( -- ) 0 FD-N ! ;
public
: FD-ADD ( r r r r r r n -- ) {: cx:r cy:r bx:r by:r bw:r bh:r hasbox:n :}      \ buffer one frame detection
   FD-N @ FD-MAX >= if E-FD-FULL throw then         \ guard: never silently drop a detection
   cx FD-CX FD-N @ cells + F!  cy FD-CY FD-N @ cells + F!
   bx FD-BX FD-N @ cells + F!  by FD-BY FD-N @ cells + F!
   bw FD-BW FD-N @ cells + F!  bh FD-BH FD-N @ cells + F!
   hasbox FD-HASBOX FD-N @ cells + !
   FD-N @ 1+ FD-N !  TK-RECORDS @ 1+ TK-RECORDS ! ;

\ gate a (track ti, det di) pair and append a candidate if it passes
private
: CAND-TRY ( n n -- ) {: ti:n di:n :}
   TK-CX ti cells + F@  TK-CY ti cells + F@  FD-CX di cells + F@  FD-CY di cells + F@  TRK-DIST2  TK-COST F!
   TK-COST F@ TK-MAXD2 F@ f> if exit then
   -1 TK-PASS !
   TK-MINIOU F@ 0.0 f> if
      TK-HASBOX ti cells + @ 0 <>  FD-HASBOX di cells + @ 0 <>  and if
         TK-BX ti cells + F@ TK-BY ti cells + F@ TK-BW ti cells + F@ TK-BH ti cells + F@
         FD-BX di cells + F@ FD-BY di cells + F@ FD-BW di cells + F@ FD-BH di cells + F@
         TRK-IOU  TK-MINIOU F@ f< if 0 TK-PASS ! then
      else
         0 TK-PASS !
      then
   then
   TK-PASS @ 0 <> if
      CA-N @ CA-MAX >= if E-CA-FULL throw then       \ guard: never silently drop a candidate
      ti CA-T CA-N @ cells + !  di CA-D CA-N @ cells + !  TK-COST F@ CA-COST CA-N @ cells + F!
      0 CA-DONE CA-N @ cells + !  CA-N @ 1+ CA-N !
   then ;

\ set CA-MIN = index of the lowest-cost unprocessed candidate, or -1
: FINDMINCAND ( -- )
   -1 CA-MIN !  0 TK-SI !
   begin TK-SI @ CA-N @ < while
      CA-DONE TK-SI @ cells + @ 0= if
         CA-MIN @ 0 < if
            TK-SI @ CA-MIN !  CA-COST TK-SI @ cells + F@ CA-BEST F!
         else
            CA-COST TK-SI @ cells + F@ CA-BEST F@ f< if
               TK-SI @ CA-MIN !  CA-COST TK-SI @ cells + F@ CA-BEST F!
            then
         then
      then
      TK-SI @ 1+ TK-SI !
   repeat ;

: TK-SPAWN ( -- )    \ create a track from the current frame det (TK-DI)
   TK-N @ TK-MAX >= if E-TK-FULL throw then         \ guard: never silently drop a track
   FD-CX TK-DI @ cells + F@  TK-CX TK-N @ cells + F!   FD-CY TK-DI @ cells + F@  TK-CY TK-N @ cells + F!
   FD-BX TK-DI @ cells + F@  TK-BX TK-N @ cells + F!   FD-BY TK-DI @ cells + F@  TK-BY TK-N @ cells + F!
   FD-BW TK-DI @ cells + F@  TK-BW TK-N @ cells + F!   FD-BH TK-DI @ cells + F@  TK-BH TK-N @ cells + F!
   FD-HASBOX TK-DI @ cells + @  TK-HASBOX TK-N @ cells + !
   1 TK-HITS TK-N @ cells + !  0 TK-MISSES TK-N @ cells + !  1 TK-ALIVE TK-N @ cells + !
   TK-CURFIDX @ TK-FIRST TK-N @ cells + !  TK-CURFIDX @ TK-LAST TK-N @ cells + !
   1 TK-CONFIRM @ >= if 1 else 0 then  TK-CONFIRMED TK-N @ cells + !
   TK-CREATED @ 1+ TK-CREATED !   TK-NEW @ 1+ TK-NEW !
   1 TK-CONFIRM @ >= if TK-CONFCNT @ 1+ TK-CONFCNT ! then
   TK-N @ 1+ TK-N ! ;

: TK-UPDATE ( -- )   \ update the matched track for the current frame det (TK-DI)
   FD-TRK TK-DI @ cells + @ TK-TIV !
   FD-CX TK-DI @ cells + F@  TK-CX TK-TIV @ cells + F!   FD-CY TK-DI @ cells + F@  TK-CY TK-TIV @ cells + F!
   FD-BX TK-DI @ cells + F@  TK-BX TK-TIV @ cells + F!   FD-BY TK-DI @ cells + F@  TK-BY TK-TIV @ cells + F!
   FD-BW TK-DI @ cells + F@  TK-BW TK-TIV @ cells + F!   FD-BH TK-DI @ cells + F@  TK-BH TK-TIV @ cells + F!
   FD-HASBOX TK-DI @ cells + @  TK-HASBOX TK-TIV @ cells + !
   TK-HITS TK-TIV @ cells + dup @ 1+ swap !   0 TK-MISSES TK-TIV @ cells + !
   TK-CURFIDX @ TK-LAST TK-TIV @ cells + !
   TK-CONFIRMED TK-TIV @ cells + @ 0=  TK-HITS TK-TIV @ cells + @ TK-CONFIRM @ >=  and if
      1 TK-CONFIRMED TK-TIV @ cells + !  TK-CONFCNT @ 1+ TK-CONFCNT !
   then
   TK-MATCHED @ 1+ TK-MATCHED ! ;

public
: TK-FRAME ( n -- )    \ associate the buffered frame's detections (frame index), then clear it
   {: fidx:n :} fidx TK-CURFIDX !  TK-FCNT @ 1+ TK-FCNT !
   0 CA-N !
   0 TK-SI ! begin TK-SI @ TK-N @ < while  0 TK-USED TK-SI @ cells + !  TK-SI @ 1+ TK-SI !  repeat
   0 TK-SI ! begin TK-SI @ FD-N @ < while  -1 FD-TRK TK-SI @ cells + !  TK-SI @ 1+ TK-SI !  repeat
   0 TK-TI !
   begin TK-TI @ TK-N @ < while
      TK-ALIVE TK-TI @ cells + @ 0 <> if
         0 TK-DI !
         begin TK-DI @ FD-N @ < while  TK-TI @ TK-DI @ CAND-TRY  TK-DI @ 1+ TK-DI !  repeat
      then
      TK-TI @ 1+ TK-TI !
   repeat
   begin FINDMINCAND CA-MIN @ 0 >= while
      1 CA-DONE CA-MIN @ cells + !
      TK-USED  CA-T CA-MIN @ cells + @  cells + @ 0=
      FD-TRK   CA-D CA-MIN @ cells + @  cells + @ -1 =  and if
         1  TK-USED  CA-T CA-MIN @ cells + @  cells + !
         CA-T CA-MIN @ cells + @  FD-TRK  CA-D CA-MIN @ cells + @  cells + !
      then
   repeat
   0 TK-TI !
   begin TK-TI @ TK-N @ < while
      TK-ALIVE TK-TI @ cells + @ 0 <>  TK-USED TK-TI @ cells + @ 0=  and if
         TK-MISSES TK-TI @ cells + dup @ 1+ swap !
      then
      TK-TI @ 1+ TK-TI !
   repeat
   0 TK-DI !
   begin TK-DI @ FD-N @ < while
      FD-TRK TK-DI @ cells + @ -1 = if TK-SPAWN else TK-UPDATE then
      TK-DI @ 1+ TK-DI !
   repeat
   0 TK-TI !
   begin TK-TI @ TK-N @ < while
      TK-ALIVE TK-TI @ cells + @ 0 <>  TK-MISSES TK-TI @ cells + @ TK-MAXAGE @ >  and if
         0 TK-ALIVE TK-TI @ cells + !
      then
      TK-TI @ 1+ TK-TI !
   repeat
   FD-RESET ;

: TK-RECORDS@ ( -- n ) TK-RECORDS @ ;
private
: TK-FRAMES@  ( -- n ) TK-FCNT @ ;
public
: TK-CREATED@ ( -- n ) TK-CREATED @ ;
: TK-CONFCNT@ ( -- n ) TK-CONFCNT @ ;
: TK-MATCHED@ ( -- n ) TK-MATCHED @ ;
: TK-NEW@     ( -- n ) TK-NEW @ ;

\ --- track length statistics (retireTrack / trackCamera summary) -------------
\ Each track's length is last_frame - first_frame + 1; reports sum/count (=> mean),
\ median (sorted[count/2], matching the .zig), max, and total hits (=> mean hits).
\ Needs lib/sort.f for the median sort.
private
: MAXI ( n n -- n ) {: a:n b:n :} a b > if a else b then ;
public
: TK-LEN-STATS ( -- )
   0 TK-LSUM !  0 TK-LMAXV !  0 TK-THITS !  TK-N @ TK-LCNT !
   0 TK-LI !
   begin TK-LI @ TK-N @ < while
      TK-LAST TK-LI @ cells + @  TK-FIRST TK-LI @ cells + @  -  1+  TK-LEN TK-LI @ cells + !
      TK-LSUM @ TK-LEN TK-LI @ cells + @ + TK-LSUM !
      TK-LMAXV @ TK-LEN TK-LI @ cells + @ MAXI TK-LMAXV !
      TK-THITS @ TK-HITS TK-LI @ cells + @ + TK-THITS !
      TK-LI @ 1+ TK-LI !
   repeat
   TK-N @ 0 > if
      TK-LEN TK-N @ [: < ;] SORT!
      TK-N @ 2 / cells TK-LEN + @ TK-LMED !
   else 0 TK-LMED ! then ;
: TK-LSUM@  ( -- n ) TK-LSUM @ ;
: TK-LCNT@  ( -- n ) TK-LCNT @ ;
: TK-LMAX@  ( -- n ) TK-LMAXV @ ;
: TK-LMED@  ( -- n ) TK-LMED @ ;
: TK-THITS@ ( -- n ) TK-THITS @ ;
end-package
