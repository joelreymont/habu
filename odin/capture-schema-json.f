\ capture-schema-json.f - full NDJSON record validation, ported from
\ src/capture_schema.zig validateObject/validateLine. Layered on capture-schema.f
\ (record-type + version) and habu tools/json.f (JSON-PARSE / JSON-GET / JSON-KIND).
\
\ validateObject's contract (mirrored exactly, in this order):
\   1. reject any key ending in a wrong timestamp unit (_timestamp_ms/_us, _monotonic_ms/_us)
\   2. required string "type" -> parseRecordType (CS-RTYPE); unknown -> UNKNOWN-RTYPE
\   3. required string "schema_version" -> exact match; else UNSUPPORTED-SCHEMA
\   4. per record type: every required field present with the right JSON kind, plus
\      the time_reference / result enum checks.
\ First error wins (CS-FAIL records only the first), matching the .zig early returns.
\
\ JSON numbers are stored by json.f as text spans, so the .integer-vs-number
\ distinction is a text-shape test: an integer has no '.' / 'e' / 'E'.
\
\ Signatures use type keywords only (i64/ptr u8/bool); descriptive names live in
\ the {: :} locals. A JSON node is an i64 index; a JSON string is `ptr u8 i64`.
\
\ Load order: lib/errors.f lib/string.f lib/memory.f tools/json.f
\             odin/capture-schema.f odin/capture-schema-json.f

\ validation status codes (mirror ValidationError)
package SCHEMA
public
0 constant V-OK
private
1 constant EMPTY-STREAM
2 constant EXPECTED-OBJECT
3 constant INVALID-JSON
4 constant INVALID-FIELD-TYPE
5 constant INVALID-TS-UNITS
6 constant MISSING-FIELD
7 constant MIXED-VERSION
8 constant UNKNOWN-ENUM
9 constant UNKNOWN-RTYPE
10 constant UNSUPPORTED-SCHEMA

\ JSON kind codes for required fields (mirror JsonKind)
0 constant K-STR
1 constant K-INT
2 constant K-NUM
3 constant K-BOOL
4 constant K-ARR
5 constant K-NSTR
6 constant K-NINT
7 constant K-NNUM
8 constant K-NBOOL

\ checked boolean literals (Habu has no true/false words; flags come from 0=)
: true  ( -- bool ) 0 0= ;
: false ( -- bool ) 0 0= 0= ;

variable ERR        \ first error seen (CS-V-OK = none yet)
variable RT         \ parsed record type (CS-* from capture-schema.f), -1 unknown
variable SI         \ object-pair scan index (timestamp-unit scan)
variable BI         \ byte scan index (number-shape scan)
variable FOUND      \ found-flag for the byte scan

: OK? ( -- bool ) ERR @ V-OK = ;
: FAIL ( i64 -- ) {: c:i64 :} ERR @ V-OK = if c ERR ! then ;

\ --- number shape: integer has no '.' 'e' 'E' ($2E $65 $45) ---
: FLOATCH? ( n -- bool ) {: c:n :} c $2E = c $65 = or c $45 = or ;
: FLOATY? ( ptr u8 i64 -- bool ) {: a:ptr u:i64 :}
   0 BI !  0 FOUND !
   begin BI @ u < FOUND @ 0= and while
      a BI @ + c@ FLOATCH? if -1 FOUND ! then
      BI @ 1+ BI !
   repeat
   FOUND @ 0 <> ;
: INT? ( i64 -- bool ) {: v:i64 :}
   v JSON-KIND J-NUM = if v JSON-NUMBER$ FLOATY? 0= else false then ;

\ --- does a value node match a required JSON kind? (mirror matchesKind) ---
: MATCH? ( i64 i64 -- bool ) {: v:i64 kind:i64 :}
   v JSON-KIND {: k:i64 :}
   kind K-STR   = if k J-STR  = exit then
   kind K-INT   = if v INT? exit then
   kind K-NUM   = if k J-NUM  = exit then
   kind K-BOOL  = if k J-BOOL = exit then
   kind K-ARR   = if k J-ARR  = exit then
   kind K-NSTR  = if k J-NULL = k J-STR  = or exit then
   kind K-NINT  = if k J-NULL = if true exit then v INT? exit then
   kind K-NNUM  = if k J-NULL = k J-NUM  = or exit then
   kind K-NBOOL = if k J-NULL = k J-BOOL = or exit then
   false ;

\ --- one required field: present and right kind (mirror requireFields step) ---
: REQ ( i64 ptr u8 i64 i64 -- ) {: root:i64 key:ptr ku:i64 kind:i64 :}
   root key ku JSON-GET {: v:i64 :}
   v -1 = if MISSING-FIELD FAIL else
      v kind MATCH? 0= if INVALID-FIELD-TYPE FAIL then
   then ;

\ required string field -> node, or -1 (and records the error). mirror requiredString
: REQ-STR ( i64 ptr u8 i64 -- i64 ) {: root:i64 key:ptr ku:i64 :}
   root key ku JSON-GET {: v:i64 :}
   v -1 = if MISSING-FIELD FAIL -1 else
      v JSON-KIND J-STR <> if INVALID-FIELD-TYPE FAIL -1 else v then
   then ;

\ --- reject wrong timestamp units on ANY key (runs first) ---
: ENDS? ( ptr u8 i64 ptr u8 i64 -- bool ) {: a:ptr au:i64 b:ptr bu:i64 :}   \ a ends with b
   bu au > if false exit then
   a au bu - +  bu  b bu  STR= ;
: BAD-KEY? ( ptr u8 i64 -- bool ) {: a:ptr u:i64 :}
   a u s" _timestamp_ms" ENDS? if true exit then
   a u s" _timestamp_us" ENDS? if true exit then
   a u s" _monotonic_ms" ENDS? if true exit then
   a u s" _monotonic_us" ENDS? if true exit then
   false ;
: CHECK-TS ( i64 -- ) {: root:i64 :}
   0 SI !
   begin SI @ root JSON-COUNT < OK? and while
      root SI @ JSON-OBJ@ drop BAD-KEY? if INVALID-TS-UNITS FAIL then
      SI @ 1+ SI !
   repeat ;

\ --- enum value sets ---
: TR-OK? ( ptr u8 i64 -- bool ) {: a:ptr u:i64 :}     \ time_reference
   a u s" IMAGE"   STR= if true exit then
   a u s" CURRENT" STR= if true exit then
   false ;
: RESULT-OK? ( ptr u8 i64 -- bool ) {: a:ptr u:i64 :} \ summary result
   a u s" pass"             STR= if true exit then
   a u s" fail"             STR= if true exit then
   a u s" characterization" STR= if true exit then
   false ;

\ --- per record-type required fields ---
: FIELDS-SCHEMA ( i64 -- ) {: root:i64 :}
   root s" helper_version" K-STR  REQ
   root s" sdk_version"    K-STR  REQ
   root s" host"           K-STR  REQ
   root s" command"        K-STR  REQ
   root s" config_path"    K-NSTR REQ ;
: FIELDS-FRAME ( i64 -- ) {: root:i64 :}
   root s" serial"                 K-STR   REQ
   root s" logical_name"           K-STR   REQ
   root s" frame_index"            K-INT   REQ
   root s" sdk_image_timestamp_ns" K-INT   REQ
   root s" host_monotonic_ns"      K-INT   REQ
   root s" width"                  K-INT   REQ
   root s" height"                 K-INT   REQ
   root s" fps_target"             K-INT   REQ
   root s" pixel_format"           K-STR   REQ
   root s" exposure_us"            K-NINT  REQ
   root s" gain"                   K-NNUM  REQ
   root s" auto_exposure"          K-NBOOL REQ
   root s" image_path"             K-NSTR  REQ
   root s" dropped"                K-BOOL  REQ
   root s" duplicate"              K-BOOL  REQ
   root s" timestamp_regressed"    K-BOOL  REQ ;
: FIELDS-SENSOR ( i64 -- ) {: root:i64 :}
   root s" serial"             K-STR   REQ
   root s" logical_name"       K-STR   REQ
   root s" sensor_kind"        K-STR   REQ
   root s" time_reference"     K-STR   REQ
   root s" sensor_timestamp_ns" K-NINT REQ
   root s" image_frame_index"  K-NINT  REQ
   root s" sample_rate_hz"     K-NNUM  REQ
   root s" values"             K-ARR   REQ
   root s" units"              K-STR   REQ
   root s" duplicate"          K-BOOL  REQ
   root s" stale"              K-BOOL  REQ
   root s" missing"            K-BOOL  REQ ;
: FIELDS-RESOURCE ( i64 -- ) {: root:i64 :}
   root s" host_monotonic_ns"        K-INT   REQ
   root s" source"                   K-STR   REQ
   root s" cpu_percent"              K-NNUM  REQ
   root s" ram_available_bytes"      K-NINT  REQ
   root s" ram_total_bytes"          K-NINT  REQ
   root s" swap_used_bytes"          K-NINT  REQ
   root s" swap_total_bytes"         K-NINT  REQ
   root s" gpu_memory_used_bytes"    K-NINT  REQ
   root s" thermal_throttled"        K-NBOOL REQ
   root s" power_mode"               K-NSTR  REQ
   root s" disk_free_bytes"          K-NINT  REQ
   root s" disk_write_bytes_per_sec" K-NINT  REQ ;
: FIELDS-ERROR ( i64 -- ) {: root:i64 :}
   root s" serial"  K-NSTR REQ
   root s" code"    K-STR  REQ
   root s" message" K-STR  REQ
   root s" fatal"   K-BOOL REQ ;
: FIELDS-SUMMARY ( i64 -- ) {: root:i64 :}
   root s" serial"                K-NSTR REQ
   root s" frames_seen"           K-INT  REQ
   root s" frames_dropped"        K-INT  REQ
   root s" duplicates"            K-INT  REQ
   root s" timestamp_regressions" K-INT  REQ
   root s" writer_stalls"         K-INT  REQ
   root s" result"                K-STR  REQ ;

: REQ-ENUM-TR ( i64 -- ) {: root:i64 :}
   root s" time_reference" REQ-STR {: v:i64 :}
   v -1 <> if
      v JSON-STRING$ TR-OK? 0= if UNKNOWN-ENUM FAIL then
   then ;
: REQ-ENUM-RESULT ( i64 -- ) {: root:i64 :}
   root s" result" REQ-STR {: v:i64 :}
   v -1 <> if
      v JSON-STRING$ RESULT-OK? 0= if UNKNOWN-ENUM FAIL then
   then ;

: DISPATCH ( i64 i64 -- ) {: root:i64 rt:i64 :}
   rt SCHEMA   = if root FIELDS-SCHEMA   exit then
   rt FRAME    = if root FIELDS-FRAME    exit then
   rt SENSOR   = if root FIELDS-SENSOR   root REQ-ENUM-TR exit then
   rt RESOURCE = if root FIELDS-RESOURCE exit then
   rt ERROR    = if root FIELDS-ERROR    exit then
   rt SUMMARY  = if root FIELDS-SUMMARY  root REQ-ENUM-RESULT exit then ;

: PARSE-TYPE ( i64 -- ) {: root:i64 :}
   root s" type" REQ-STR {: v:i64 :}
   v -1 <> if
      v JSON-STRING$ RTYPE RT !
      RT @ UNKNOWN = if UNKNOWN-RTYPE FAIL then
   then ;
: CHECK-VERSION ( i64 -- ) {: root:i64 :}
   root s" schema_version" REQ-STR {: v:i64 :}
   v -1 <> if
      v JSON-STRING$ VERSION-OK? 0= if UNSUPPORTED-SCHEMA FAIL then
   then ;

\ validate a parsed object node -> ( rtype status ); rtype valid only when status=OK
: VALIDATE ( i64 -- i64 i64 ) {: root:i64 :}
   root JSON-KIND J-OBJ <> if -1 EXPECTED-OBJECT exit then
   V-OK ERR !  -1 RT !
   root CHECK-TS
   root PARSE-TYPE
   root CHECK-VERSION
   OK? if root RT @ DISPATCH then
   RT @ ERR @ ;

\ validate one NDJSON line -> ( rtype status ); mirror validateLine
public
: VALIDATE-LINE ( ptr u8 i64 -- i64 i64 ) {: a:ptr u:i64 :}
   a u JSON-PARSE-TRY drop {: root:i64 st:i64 :}
   st JSON-PARSE-OK = if root VALIDATE else -1 INVALID-JSON then ;
end-package
