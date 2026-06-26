\ config.f - camera-rig config validation, ported from Odin's src/config.zig.
\
\ Validates a set of CameraConfig records (identity, unique serials, full 4-camera
\ rig, localization readiness). Cameras are held in parallel arrays; validators
\ return an error code (CC-OK or a CC-* code) mirroring config.zig's ValidationError.
\ Only the fields the validators inspect are modeled: serial (ptr/len), the three
\ other identity string lengths, and a per-camera "geometry complete" flag (the
\ AND of fov/fov-source/orientation/mount/intrinsics/extrinsics/residual).
\ Depends on lib/errors.f lib/string.f.

package CONFIG
private
8 constant MAX
0 constant OK
1 constant MISSING-CAMERA
2 constant NOT-FULL-RIG
3 constant MISSING-IDENTITY
4 constant DUPLICATE-SERIAL
5 constant UNKNOWN-GEOMETRY

create SER-A   MAX cells allot      \ serial pointer
create SER-N   MAX cells allot      \ serial length
create NAME-N  MAX cells allot      \ logical_name length
create SDKID-N MAX cells allot      \ sdk_id length
create MODEL-N MAX cells allot      \ model length
create GEOMS   MAX cells allot      \ geometry-complete flag
variable N
variable IX  variable JX  variable ERR

public
: RESET ( -- ) 0 N ! ;
: ADD ( ptr u8 n n n n n -- ) {: sa:ptr sn:n namen:n sdkn:n modeln:n geom:n :}
   N @ {: k:n :}
   sa     SER-A   k cells + !   sn     SER-N   k cells + !
   namen  NAME-N  k cells + !   sdkn   SDKID-N k cells + !
   modeln MODEL-N k cells + !   geom   GEOMS   k cells + !
   k 1+ N ! ;

\ all four identity strings non-empty
private
: IDENT? ( n -- bool ) {: ix:n :}
   SER-N   ix cells + @ 0 >
   NAME-N  ix cells + @ 0 > and
   SDKID-N ix cells + @ 0 > and
   MODEL-N ix cells + @ 0 > and ;

: SER= ( n n -- bool ) {: ix:n jx:n :}    \ serial[ix] == serial[jx]
   SER-A ix cells + @  SER-N ix cells + @
   SER-A jx cells + @  SER-N jx cells + @  STR= ;

: SET-ERR ( n -- ) {: c:n :}            \ record the first error only
   ERR @ OK = if c ERR ! then ;

public
: UNIQUE ( -- n )                     \ identity + duplicate-serial scan
   N @ 0= if MISSING-CAMERA exit then
   OK ERR !
   0 IX !
   begin IX @ N @ < while
      IX @ IDENT? 0= if MISSING-IDENTITY SET-ERR then
      IX @ 1+ JX !
      begin JX @ N @ < while
         IX @ JX @ SER= if DUPLICATE-SERIAL SET-ERR then
         JX @ 1+ JX !
      repeat
      IX @ 1+ IX !
   repeat
   ERR @ ;

: FULL-RIG ( -- n )
   N @ 4 <> if NOT-FULL-RIG exit then
   UNIQUE ;

: LOC-READY ( -- n )
   FULL-RIG {: e:n :}
   e OK <> if e exit then
   OK ERR !
   0 IX !
   begin IX @ N @ < while
      GEOMS IX @ cells + @ 0= if UNKNOWN-GEOMETRY SET-ERR then
      IX @ 1+ IX !
   repeat
   ERR @ ;
end-package
