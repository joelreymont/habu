\ timestamp-metrics.f - per-camera frame-timing kernel, ported from
\ src/timestamp_metrics.zig (CameraTiming + updateCameraTiming + addPeriodSample
\ + finish). Cameras are held in parallel arrays keyed by serial; TM-ADD folds one
\ frame in (init on first sight of a serial, else update), TM-SUMMARY records the
\ summary frames_dropped, TM-FINISH computes the period means.
\
\ Per camera it tracks: frame count, period sample count, sdk/host period
\ mean/min/max and max |period - target| jitter, dropped/duplicate/regression
\ flag counts, frame-index gap drops, and first/last index+timestamps. Periods are
\ only sampled across monotonic frames (sdk and host both non-decreasing), matching
\ updateCameraTiming. Integer ns throughout (sums fit i64 for realistic runs).
\
\ Signatures use type keywords only; a serial is `ptr u8 n`. Depends on
\ lib/errors.f lib/string.f lib/hashmap.f lib/sort.f. The byte-exact CSV/JSON
\ renderers live in odin/timestamp-render.f.

package CAMSYNC
private
8 constant TM-MAX
1000000000 constant NS-PER-S

create TM-SER-A   TM-MAX cells allot
create TM-SER-N   TM-MAX cells allot
create TM-LNA-A   TM-MAX cells allot      \ logical_name ptr
create TM-LNA-N   TM-MAX cells allot      \ logical_name len
create TM-FPS     TM-MAX cells allot
create TM-TGTP    TM-MAX cells allot     \ target_period_ns
create TM-FRAMES  TM-MAX cells allot
create TM-PSAMP   TM-MAX cells allot     \ period_samples
create TM-SDK-SUM TM-MAX cells allot
create TM-SDK-MIN TM-MAX cells allot
create TM-SDK-MAX TM-MAX cells allot
create TM-SDK-JIT TM-MAX cells allot
create TM-SDK-MEAN TM-MAX cells allot
create TM-HST-SUM TM-MAX cells allot
create TM-HST-MIN TM-MAX cells allot
create TM-HST-MAX TM-MAX cells allot
create TM-HST-JIT TM-MAX cells allot
create TM-HST-MEAN TM-MAX cells allot
create TM-DROP    TM-MAX cells allot      \ dropped_event_flags
create TM-DUP     TM-MAX cells allot      \ duplicate_flags
create TM-REGR    TM-MAX cells allot      \ timestamp_regression_flags
create TM-GAP     TM-MAX cells allot      \ index_gap_drops
create TM-MAXGAP  TM-MAX cells allot      \ max_index_gap_drop
create TM-FIDX0   TM-MAX cells allot      \ first_frame_index
create TM-LIDX    TM-MAX cells allot      \ last_frame_index
create TM-FSDK    TM-MAX cells allot      \ first_sdk_timestamp_ns
create TM-LSDK    TM-MAX cells allot      \ last_sdk_timestamp_ns
create TM-FHST    TM-MAX cells allot      \ first_host_monotonic_ns
create TM-LHST    TM-MAX cells allot      \ last_host_monotonic_ns
create TM-FDROP   TM-MAX cells allot      \ frames_dropped (from summary)
variable TM-N   variable TM-SI   variable TM-FI

: MAX2 ( n n -- n ) {: a:n b:n :} a b > if a else b then ;
: ABS-DIFF ( n n -- n ) {: a:n b:n :} a b >= if a b - else b a - then ;
: TGT-PERIOD ( n -- n ) {: fps:n :} fps 0= if 0 else NS-PER-S fps / then ;

public
: TM-RESET ( -- ) 0 TM-N ! ;

\ index of camera by serial, or -1
private
: TM-FIND ( ptr u8 n -- n ) {: sa:ptr sn:n :}
   -1 TM-FI !
   0 TM-SI !
   begin TM-SI @ TM-N @ < TM-FI @ 0 < and while
      sa sn  TM-SER-A TM-SI @ cells + @  TM-SER-N TM-SI @ cells + @  STR= if
         TM-SI @ TM-FI !
      then
      TM-SI @ 1+ TM-SI !
   repeat
   TM-FI @ ;

\ append a new camera row (first frame for this serial)
-6203 constant E-TM-FULL
: TM-INIT ( ptr u8 n ptr u8 n n n n n n n n -- ) {: sa:ptr sn:n lna:ptr lnu:n fps:n fidx:n sdk:n host:n drp:n dupf:n regr:n :}
   TM-N @ TM-MAX >= if E-TM-FULL throw then         \ guard: never silently drop a camera
   TM-N @ {: ix:n :}
   sa   TM-SER-A ix cells + !    sn TM-SER-N ix cells + !
   lna  TM-LNA-A ix cells + !    lnu TM-LNA-N ix cells + !
   fps  TM-FPS   ix cells + !    fps TGT-PERIOD TM-TGTP ix cells + !
   1    TM-FRAMES ix cells + !   0 TM-PSAMP ix cells + !
   0 TM-SDK-SUM ix cells + !     0 TM-SDK-MIN ix cells + !
   0 TM-SDK-MAX ix cells + !     0 TM-SDK-JIT ix cells + !   0 TM-SDK-MEAN ix cells + !
   0 TM-HST-SUM ix cells + !     0 TM-HST-MIN ix cells + !
   0 TM-HST-MAX ix cells + !     0 TM-HST-JIT ix cells + !   0 TM-HST-MEAN ix cells + !
   drp  TM-DROP ix cells + !     dupf TM-DUP ix cells + !    regr TM-REGR ix cells + !
   0 TM-GAP ix cells + !         0 TM-MAXGAP ix cells + !
   fidx TM-FIDX0 ix cells + !    fidx TM-LIDX ix cells + !
   sdk  TM-FSDK ix cells + !     sdk TM-LSDK ix cells + !
   host TM-FHST ix cells + !     host TM-LHST ix cells + !
   0 TM-FDROP ix cells + !
   ix 1+ TM-N ! ;

\ fold one period sample (sdk/host deltas) into camera ix (addPeriodSample)
: TM-PERIOD ( n n n -- ) {: ix:n sd:n hd:n :}
   TM-PSAMP ix cells + dup @ 1+ swap !
   TM-SDK-SUM ix cells + dup @ sd + swap !
   TM-HST-SUM ix cells + dup @ hd + swap !
   TM-PSAMP ix cells + @ 1 = if
      sd TM-SDK-MIN ix cells + !   hd TM-HST-MIN ix cells + !
   else
      sd TM-SDK-MIN ix cells + @ < if sd TM-SDK-MIN ix cells + ! then
      hd TM-HST-MIN ix cells + @ < if hd TM-HST-MIN ix cells + ! then
   then
   TM-SDK-MAX ix cells + dup @ sd MAX2 swap !
   TM-HST-MAX ix cells + dup @ hd MAX2 swap !
   TM-SDK-JIT ix cells + dup @  sd TM-TGTP ix cells + @ ABS-DIFF  MAX2 swap !
   TM-HST-JIT ix cells + dup @  hd TM-TGTP ix cells + @ ABS-DIFF  MAX2 swap ! ;

\ update an existing camera with a later frame (updateCameraTiming)
: TM-UPD ( n n n n n n n -- ) {: ix:n fidx:n sdk:n host:n drp:n dupf:n regr:n :}
   TM-FRAMES ix cells + dup @ 1+ swap !
   drp  0 <> if TM-DROP ix cells + dup @ 1+ swap ! then
   dupf 0 <> if TM-DUP  ix cells + dup @ 1+ swap ! then
   regr 0 <> if TM-REGR ix cells + dup @ 1+ swap ! then
   fidx  TM-LIDX ix cells + @ 1+  > if
      TM-GAP    ix cells + dup @  fidx TM-LIDX ix cells + @ - 1-  +     swap !
      TM-MAXGAP ix cells + dup @  fidx TM-LIDX ix cells + @ - 1-  MAX2  swap !
   then
   sdk TM-LSDK ix cells + @ >=  host TM-LHST ix cells + @ >=  and if
      ix  sdk TM-LSDK ix cells + @ -   host TM-LHST ix cells + @ -   TM-PERIOD
   then
   fidx TM-LIDX ix cells + !   sdk TM-LSDK ix cells + !   host TM-LHST ix cells + ! ;

\ fold one frame in: ( serial logical_name fps frame_index sdk_ts host_ts dropped dup regressed )
public
: TM-ADD ( ptr u8 n ptr u8 n n n n n n n n -- ) {: sa:ptr sn:n lna:ptr lnu:n fps:n fidx:n sdk:n host:n drp:n dupf:n regr:n :}
   sa sn TM-FIND {: ix:n :}
   ix 0 < if
      sa sn lna lnu fps fidx sdk host drp dupf regr TM-INIT
   else
      ix fidx sdk host drp dupf regr TM-UPD
   then ;

\ record a summary record's frames_dropped for a serial (addSummaryRecord)
: TM-SUMMARY ( ptr u8 n n -- ) {: sa:ptr sn:n fd:n :}
   sa sn TM-FIND {: ix:n :}
   ix 0 >= if fd TM-FDROP ix cells + ! then ;

\ compute period means (CameraTiming.finish over all cameras)
: TM-FINISH ( -- )
   0 TM-SI !
   begin TM-SI @ TM-N @ < while
      TM-PSAMP TM-SI @ cells + @ 0 > if
         TM-SDK-SUM TM-SI @ cells + @  TM-PSAMP TM-SI @ cells + @  /  TM-SDK-MEAN TM-SI @ cells + !
         TM-HST-SUM TM-SI @ cells + @  TM-PSAMP TM-SI @ cells + @  /  TM-HST-MEAN TM-SI @ cells + !
      then
      TM-SI @ 1+ TM-SI !
   repeat ;

\ accessors ( camera-index -- value )
private
: TM-SER@      ( n -- ptr u8 n ) {: ix:n :} TM-SER-A ix cells + @  TM-SER-N ix cells + @ ;
: TM-LNA@      ( n -- ptr u8 n ) {: ix:n :} TM-LNA-A ix cells + @  TM-LNA-N ix cells + @ ;
: TM-FPS@      ( n -- n ) cells TM-FPS + @ ;
: TM-TGTP@     ( n -- n ) cells TM-TGTP + @ ;
public
: TM-FRAMES@   ( n -- n ) cells TM-FRAMES + @ ;
: TM-PSAMP@    ( n -- n ) cells TM-PSAMP + @ ;
: TM-SDK-MEAN@ ( n -- n ) cells TM-SDK-MEAN + @ ;
: TM-HST-MEAN@ ( n -- n ) cells TM-HST-MEAN + @ ;
: TM-SDK-MIN@  ( n -- n ) cells TM-SDK-MIN + @ ;
: TM-SDK-MAX@  ( n -- n ) cells TM-SDK-MAX + @ ;
: TM-SDK-JIT@  ( n -- n ) cells TM-SDK-JIT + @ ;
private
: TM-HST-MIN@  ( n -- n ) cells TM-HST-MIN + @ ;
: TM-HST-MAX@  ( n -- n ) cells TM-HST-MAX + @ ;
: TM-HST-JIT@  ( n -- n ) cells TM-HST-JIT + @ ;
public
: TM-GAP@      ( n -- n ) cells TM-GAP + @ ;
: TM-MAXGAP@   ( n -- n ) cells TM-MAXGAP + @ ;
: TM-DROP@     ( n -- n ) cells TM-DROP + @ ;
private
: TM-DUP@      ( n -- n ) cells TM-DUP + @ ;
: TM-REGR@     ( n -- n ) cells TM-REGR + @ ;
public
: TM-FDROP@    ( n -- n ) cells TM-FDROP + @ ;
private
: TM-FIDX0@    ( n -- n ) cells TM-FIDX0 + @ ;
: TM-LIDX@     ( n -- n ) cells TM-LIDX + @ ;
: TM-FSDK@     ( n -- n ) cells TM-FSDK + @ ;
: TM-LSDK@     ( n -- n ) cells TM-LSDK + @ ;
: TM-FHST@     ( n -- n ) cells TM-FHST + @ ;
: TM-LHST@     ( n -- n ) cells TM-LHST + @ ;
public
: TM-COUNT     ( -- n ) TM-N @ ;
\ derived host_minus_sdk first/last (i128 in the Zig; i64 suffices for real data)
private
: TM-HMSDK-F@  ( n -- n ) {: ix:n :} ix TM-FHST@ ix TM-FSDK@ - ;
: TM-HMSDK-L@  ( n -- n ) {: ix:n :} ix TM-LHST@ ix TM-LSDK@ - ;
\ total frame records = sum of per-camera frames (JSON frame_records)
: TM-FRECS@    ( -- n ) 0  0 TM-SI ! begin TM-SI @ TM-N @ < while  TM-SI @ TM-FRAMES@ +  TM-SI @ 1+ TM-SI ! repeat ;

\ ---------------------------------------------------------------------------
\ Cross-camera skew (buildCrossCameraSkew + FrameIndexGroup + isMultiHelperCommand).
\ Frames are grouped by frame_index in parallel arrays; the frame_index -> group
\ lookup is O(1) via lib/hashmap.f (HM-PROBE over TG-HK/TG-HU/TG-HV), so collation
\ is O(frames), not the former O(frames * groups) linear scan. Capacity is guarded
\ (E-TG-FULL throw), never a silent cap.
\ For each complete-enough group (>=2 cameras, common lifecycle) the SDK/host skew
\ is max_ts - min_ts; mean/min/max are reduced directly here. p95/p99 nearest-rank
\ (which needs a sorted skew array) is the next step.
\ ---------------------------------------------------------------------------

2048 constant TG-MAX                    \ max distinct frame-index groups (guarded; bounded by data space)
4096 constant TG-CAP                    \ hash capacity (pow2 > TG-MAX, load < 0.5)
-6201 constant E-TG-FULL
create TG-IDX  TG-MAX cells allot      \ frame_index key (group-indexed)
create TG-SEEN TG-MAX cells allot      \ cameras_seen
create TG-SMIN TG-MAX cells allot      \ sdk min/max in this index set
create TG-SMAX TG-MAX cells allot
create TG-HMIN TG-MAX cells allot
create TG-HMAX TG-MAX cells allot
create TG-HK   TG-CAP cells allot      \ hash table: frame_index key
create TG-HU   TG-CAP cells allot      \ hash table: slot-used flag
create TG-HV   TG-CAP cells allot      \ hash table: group-index value
variable TG-N   variable TG-SLOT  variable TG-GI

variable TX-SCHEMA-N  variable TX-MULTI-N  variable TX-NONMULTI
variable TX-FSETS  variable TX-COMPLETE  variable TX-INCOMPLETE
variable TX-MINCAM variable TX-MAXCAM  variable TX-COMMON
variable TX-SN     variable TX-CURS  variable TX-CURH
variable TX-SSUM   variable TX-SMINV variable TX-SMAXV variable TX-SMEAN
variable TX-HSUM   variable TX-HMINV variable TX-HMAXV variable TX-HMEAN
variable TX-SP95   variable TX-SP99  variable TX-HP95  variable TX-HP99
variable TX-CAMCNT
create TX-SSKEW TG-MAX cells allot      \ collected sdk skews (sorted in place)
create TX-HSKEW TG-MAX cells allot      \ collected host skews

: MIN2 ( n n -- n ) {: a:n b:n :} a b < if a else b then ;

\ nearest-rank percentile over an ascending-sorted cell array (percentileNearestRank)
: PCTL ( ptr a n n -- n ) {: base:ptr len:n pct:n :}
   len 0= if 0 else
      base  len pct * 99 + 100 /  1 MAX2  1-  len 1- MIN2  cells +  @
   then ;

public
: TG-RESET ( -- ) 0 TG-N !  TG-HU TG-CAP HM-CLEAR ;
\ fold a frame into its index group, O(1) via the lib/hashmap.f probe
: TG-ADD ( n n n -- ) {: fidx:n sdk:n host:n :}
   TG-HK TG-HU TG-CAP fidx HM-PROBE TG-SLOT !
   TG-HU TG-SLOT @ cells + @ 0= if              \ empty slot -> new group
      TG-N @ TG-MAX >= if E-TG-FULL throw then  \ guard: never silently drop a group
      fidx TG-HK TG-SLOT @ cells + !  -1 TG-HU TG-SLOT @ cells + !  TG-N @ TG-HV TG-SLOT @ cells + !
      fidx TG-IDX TG-N @ cells + !   1 TG-SEEN TG-N @ cells + !
      sdk  TG-SMIN TG-N @ cells + !  sdk TG-SMAX TG-N @ cells + !
      host TG-HMIN TG-N @ cells + !  host TG-HMAX TG-N @ cells + !
      TG-N @ 1+ TG-N !
   else                                         \ existing group = TG-HV[slot]
      TG-HV TG-SLOT @ cells + @ TG-GI !
      TG-SEEN TG-GI @ cells + dup @ 1+ swap !
      TG-SMIN TG-GI @ cells + dup @ sdk  MIN2 swap !
      TG-SMAX TG-GI @ cells + dup @ sdk  MAX2 swap !
      TG-HMIN TG-GI @ cells + dup @ host MIN2 swap !
      TG-HMAX TG-GI @ cells + dup @ host MAX2 swap !
   then ;

\ classify schema command -> multi-helper?
private
: MULTI-CMD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" capture-null-multi" STR= if 0 0= exit then
   a u s" capture-save-multi" STR= if 0 0= exit then
   0 0= 0= ;
public
: TX-RESET ( -- )
   0 TX-SCHEMA-N !  0 TX-MULTI-N !  TG-RESET ;
: TX-SCHEMA ( ptr u8 n -- ) {: a:ptr u:n :}    \ count a schema record by its command
   TX-SCHEMA-N @ 1+ TX-SCHEMA-N !
   a u MULTI-CMD? if TX-MULTI-N @ 1+ TX-MULTI-N ! then ;

: TX-BUILD ( n -- ) {: camcount:n :}
   camcount TX-CAMCNT !
   TX-SCHEMA-N @ TX-MULTI-N @ - TX-NONMULTI !
   TX-SCHEMA-N @ 0 >  TX-NONMULTI @ 0=  and  TX-COMMON !
   TG-N @ TX-FSETS !
   0 TX-COMPLETE !  0 TX-INCOMPLETE !  0 TX-MINCAM !  0 TX-MAXCAM !
   0 TX-SN !  0 TX-SSUM !  0 TX-HSUM !  0 TX-SMAXV !  0 TX-HMAXV !
   0 TM-SI !
   begin TM-SI @ TG-N @ < while
      \ min/max cameras per set
      TX-MINCAM @ 0=  TG-SEEN TM-SI @ cells + @ TX-MINCAM @ <  or if
         TG-SEEN TM-SI @ cells + @ TX-MINCAM ! then
      TX-MAXCAM @ TG-SEEN TM-SI @ cells + @ MAX2 TX-MAXCAM !
      \ complete vs incomplete
      TG-SEEN TM-SI @ cells + @ camcount = if
         TX-COMPLETE @ 1+ TX-COMPLETE !
      else
         TX-INCOMPLETE @ 1+ TX-INCOMPLETE !
      then
      \ skew, only for >=2-camera sets under a common lifecycle
      TG-SEEN TM-SI @ cells + @ 2 >= if
         TX-COMMON @ 0 <> if
            TG-SMAX TM-SI @ cells + @  TG-SMIN TM-SI @ cells + @  -  TX-CURS !
            TG-HMAX TM-SI @ cells + @  TG-HMIN TM-SI @ cells + @  -  TX-CURH !
            TX-SN @ 0= if  TX-CURS @ TX-SMINV !  TX-CURH @ TX-HMINV !  then
            TX-CURS @ TX-SMINV @ MIN2 TX-SMINV !   TX-CURH @ TX-HMINV @ MIN2 TX-HMINV !
            TX-CURS @ TX-SMAXV @ MAX2 TX-SMAXV !   TX-CURH @ TX-HMAXV @ MAX2 TX-HMAXV !
            TX-SSUM @ TX-CURS @ + TX-SSUM !        TX-HSUM @ TX-CURH @ + TX-HSUM !
            TX-CURS @ TX-SSKEW TX-SN @ cells + !   TX-CURH @ TX-HSKEW TX-SN @ cells + !
            TX-SN @ 1+ TX-SN !
         then
      then
      TM-SI @ 1+ TM-SI !
   repeat
   TX-SN @ 0 > if
      TX-SSUM @ TX-SN @ / TX-SMEAN !   TX-HSUM @ TX-SN @ / TX-HMEAN !
      TX-SSKEW TX-SN @ [: < ;] SORT!   TX-HSKEW TX-SN @ [: < ;] SORT!
      TX-SSKEW TX-SN @ 95 PCTL TX-SP95 !   TX-SSKEW TX-SN @ 99 PCTL TX-SP99 !
      TX-HSKEW TX-SN @ 95 PCTL TX-HP95 !   TX-HSKEW TX-SN @ 99 PCTL TX-HP99 !
   else
      0 TX-SMEAN !  0 TX-HMEAN !  0 TX-SMINV !  0 TX-HMINV !
      0 TX-SP95 !  0 TX-SP99 !  0 TX-HP95 !  0 TX-HP99 !
   then ;

: TX-FSETS@      ( -- n ) TX-FSETS @ ;
: TX-COMPLETE@   ( -- n ) TX-COMPLETE @ ;
: TX-INCOMPLETE@ ( -- n ) TX-INCOMPLETE @ ;
: TX-COMMON@     ( -- bool ) TX-COMMON @ 0 <> ;
: TX-SMAX@       ( -- n ) TX-SMAXV @ ;
: TX-SMIN@       ( -- n ) TX-SMINV @ ;
: TX-SMEAN@      ( -- n ) TX-SMEAN @ ;
: TX-HMAX@       ( -- n ) TX-HMAXV @ ;
: TX-MULTI@      ( -- n ) TX-MULTI-N @ ;
: TX-NONMULTI@   ( -- n ) TX-NONMULTI @ ;
: TX-SP95@       ( -- n ) TX-SP95 @ ;
: TX-SP99@       ( -- n ) TX-SP99 @ ;
private
: TX-HMIN@       ( -- n ) TX-HMINV @ ;
: TX-HMEAN@      ( -- n ) TX-HMEAN @ ;
: TX-HP95@       ( -- n ) TX-HP95 @ ;
: TX-HP99@       ( -- n ) TX-HP99 @ ;
: TX-CAMCOUNT@   ( -- n ) TX-CAMCNT @ ;
: TX-MINCAM@     ( -- n ) TX-MINCAM @ ;
: TX-MAXCAM@     ( -- n ) TX-MAXCAM @ ;
end-package
