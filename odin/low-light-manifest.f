\ low-light-manifest.f - low-light scenario manifest validation, ported from
\ src/low_light.zig parseLowLightManifest + validateLowLightManifest. Validates one
\ manifest JSON object over habu tools/json.f: exact schema_version, eight required
\ non-empty (trimmed) strings, a target_proxy sub-object (description +
\ contrast_reference non-empty; optional physical_width_m/height_m/range_m/
\ angular_width_mrad must be positive), and repeats != 0. Returns the .zig Error set
\ as LL-* status codes (first error wins, in the .zig check order).
\
\ Numbers are json.f text spans parsed with lib/float.f STR>FLOAT for the positive /
\ non-zero checks. Signatures use type keywords only; a JSON node is i64.
\ Load: lib/errors.f lib/string.f lib/memory.f lib/float.f tools/json.f

package LOWLIGHT
private
0 constant LL-OK
1 constant LL-INVALID-JSON
2 constant LL-UNSUPPORTED-SCHEMA
3 constant LL-INVALID-MANIFEST

: true  ( -- bool ) 0 0= ;
: false ( -- bool ) 0 0= 0= ;

variable LL-ERR  variable LL-SI  variable LL-FOUND
: LL-OK?  ( -- bool ) LL-ERR @ LL-OK = ;
: LL-FAIL ( i64 -- ) {: c:i64 :} LL-ERR @ LL-OK = if c LL-ERR ! then ;

\ trimmed-nonempty: any byte that is not space/tab/cr/lf
: LL-WS? ( n -- bool ) {: c:n :} c $20 = c $9 = or c $D = or c $A = or ;
: LL-NONEMPTY? ( ptr u8 i64 -- bool ) {: a:ptr u:i64 :}
   0 LL-SI !  0 LL-FOUND !
   begin LL-SI @ u < LL-FOUND @ 0= and while
      a LL-SI @ + c@ LL-WS? 0= if -1 LL-FOUND ! then
      LL-SI @ 1+ LL-SI !
   repeat
   LL-FOUND @ 0 <> ;

\ number-text predicates via STR>FLOAT
: LL-POS? ( ptr u8 i64 -- bool ) {: a:ptr u:i64 :}
   a u STR>FLOAT {: r:r ok:bool :}
   ok 0= if false exit then
   r 0.0 f> ;
: LL-ZERO? ( ptr u8 i64 -- bool ) {: a:ptr u:i64 :}
   a u STR>FLOAT {: r:r ok:bool :}
   ok 0= if false exit then
   r 0.0 f<  r 0.0 f>  or  0= ;

\ require: present, string, trimmed-nonempty
: LL-REQ-STR ( i64 ptr u8 i64 -- ) {: root:i64 key:ptr ku:i64 :}
   root key ku JSON-GET {: v:i64 :}
   v -1 = if LL-INVALID-JSON LL-FAIL else
      v JSON-KIND J-STR <> if LL-INVALID-JSON LL-FAIL else
         v JSON-STRING$ LL-NONEMPTY? 0= if LL-INVALID-MANIFEST LL-FAIL then
      then
   then ;

\ optional number: if present it must be a positive number
: LL-OPT-POS ( i64 ptr u8 i64 -- ) {: root:i64 key:ptr ku:i64 :}
   root key ku JSON-GET {: v:i64 :}
   v -1 <> if
      v JSON-KIND J-NUM <> if LL-INVALID-JSON LL-FAIL else
         v JSON-NUMBER$ LL-POS? 0= if LL-INVALID-MANIFEST LL-FAIL then
      then
   then ;

: LL-REQ-SCHEMA ( i64 -- ) {: root:i64 :}
   root s" schema_version" JSON-GET {: v:i64 :}
   v -1 = if LL-INVALID-JSON LL-FAIL else
      v JSON-KIND J-STR <> if LL-INVALID-JSON LL-FAIL else
         v JSON-STRING$ s" odin.low_light_manifest.v1" STR= 0= if LL-UNSUPPORTED-SCHEMA LL-FAIL then
      then
   then ;

: LL-REQ-TARGET ( i64 -- ) {: root:i64 :}
   root s" target_proxy" JSON-GET {: tp:i64 :}
   tp -1 = if LL-INVALID-JSON LL-FAIL else
      tp JSON-KIND J-OBJ <> if LL-INVALID-JSON LL-FAIL else
         tp s" description" LL-REQ-STR        tp s" contrast_reference" LL-REQ-STR
         tp s" physical_width_m" LL-OPT-POS   tp s" physical_height_m" LL-OPT-POS
         tp s" range_m" LL-OPT-POS            tp s" angular_width_mrad" LL-OPT-POS
      then
   then ;

: LL-REQ-REPEATS ( i64 -- ) {: root:i64 :}
   root s" repeats" JSON-GET {: v:i64 :}
   v -1 = if LL-INVALID-JSON LL-FAIL else
      v JSON-KIND J-NUM <> if LL-INVALID-JSON LL-FAIL else
         v JSON-NUMBER$ LL-ZERO? if LL-INVALID-MANIFEST LL-FAIL then
      then
   then ;

\ validate a parsed manifest object node -> status
: LL-VALIDATE-OBJ ( i64 -- n ) {: root:i64 :}
   LL-OK LL-ERR !
   root LL-REQ-SCHEMA
   root s" scenario"           LL-REQ-STR
   root s" captured_at_utc"    LL-REQ-STR
   root s" light_reference"    LL-REQ-STR
   root s" lighting_condition" LL-REQ-STR
   root s" time_of_day"        LL-REQ-STR
   root s" exposure_mode"      LL-REQ-STR
   root s" gain_mode"          LL-REQ-STR
   root s" exposure_plan"      LL-REQ-STR
   root LL-REQ-TARGET
   root LL-REQ-REPEATS
   LL-ERR @ ;

\ validate manifest JSON text -> status (mirror parseLowLightManifest)
public
: LL-VALIDATE ( ptr u8 i64 -- n ) {: a:ptr u:i64 :}
   a u JSON-PARSE-TRY drop {: root:i64 st:i64 :}
   st JSON-PARSE-OK = if
      root JSON-KIND J-OBJ = if root LL-VALIDATE-OBJ else LL-INVALID-JSON then
   else LL-INVALID-JSON then ;
end-package
