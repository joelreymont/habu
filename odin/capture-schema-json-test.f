\ capture-schema-json-test.f - validateObject/validateLine oracle.
\ Builds NDJSON lines programmatically (s" can't hold a literal quote), mirroring
\ habu tools/json-test.f's builder idiom. Signatures use type keywords only.
\ Run: cat lib/errors.f lib/string.f lib/memory.f lib/test.f tools/json.f \
\        odin/capture-schema.f odin/capture-schema-json.f odin/capture-schema-json-test.f | bin/hb

\ --- tiny JSON line builder ---
package SCHEMA
private
$800 constant TB-CAP
create TB TB-CAP allot
variable TB-N  variable TB-CP
: TB+C ( n -- ) TB TB-N @ + c!  TB-N @ 1+ TB-N ! ;
: TB+ ( ptr u8 i64 -- ) {: a:ptr u:i64 :}
   0 TB-CP !
   begin TB-CP @ u < while  a TB-CP @ + c@ TB+C  TB-CP @ 1+ TB-CP !  repeat ;
: Q$ ( ptr u8 i64 -- ) {: a:ptr u:i64 :} J-DQ TB+C a u TB+ J-DQ TB+C ;   \ "string"
: J{ ( -- ) 0 TB-N ! J-LBRACE TB+C ;
: J} ( -- ) J-RBRACE  TB TB-N @ 1- +  c! ;            \ overwrite trailing comma with }
: J$ ( -- ptr u8 i64 ) TB TB-N @ ;
: SVAL ( ptr u8 i64 ptr u8 i64 -- ) {: k:ptr ku:i64 v:ptr vu:i64 :}   \ "key":"val",
   k ku Q$ J-COLON TB+C  v vu Q$ J-COMMA TB+C ;
: RVAL ( ptr u8 i64 ptr u8 i64 -- ) {: k:ptr ku:i64 r:ptr ru:i64 :}   \ "key":raw,  (num/bool/null)
   k ku Q$ J-COLON TB+C  r ru TB+ J-COMMA TB+C ;

\ --- record fixtures (return ptr u8 i64) ---
: F-COMMON ( -- ) s" schema_version" s" odin.capture.v1" SVAL ;
: SCHEMA-BODY ( -- )
   s" helper_version" s" 0.0.0" SVAL  s" sdk_version" s" unknown" SVAL
   s" host" s" zed-box" SVAL  s" command" s" capture" SVAL
   s" config_path" s" null" RVAL ;
: L-SCHEMA ( -- ptr u8 i64 )    J{ s" type" s" schema" SVAL F-COMMON SCHEMA-BODY J} J$ ;
: L-SCHEMA-V2 ( -- ptr u8 i64 ) J{ s" type" s" schema" SVAL
   s" schema_version" s" odin.capture.v2" SVAL SCHEMA-BODY J} J$ ;
: L-SCHEMA-MISS ( -- ptr u8 i64 )   \ host omitted
   J{ s" type" s" schema" SVAL F-COMMON
   s" helper_version" s" 0.0.0" SVAL  s" sdk_version" s" unknown" SVAL
   s" command" s" capture" SVAL  s" config_path" s" null" RVAL J} J$ ;
: L-SCHEMA-BADTYPE ( -- ptr u8 i64 )  \ host is a number, not a string
   J{ s" type" s" schema" SVAL F-COMMON
   s" helper_version" s" 0.0.0" SVAL  s" sdk_version" s" unknown" SVAL
   s" host" s" 123" RVAL  s" command" s" capture" SVAL
   s" config_path" s" null" RVAL J} J$ ;
: L-SCHEMA-TSUNIT ( -- ptr u8 i64 )   \ a wrong-unit key present
   J{ s" foo_timestamp_ms" s" 1" RVAL s" type" s" schema" SVAL F-COMMON SCHEMA-BODY J} J$ ;
: L-BADRTYPE ( -- ptr u8 i64 )  J{ s" type" s" bogus" SVAL J} J$ ;

: ERROR-BODY ( -- )
   s" serial" s" null" RVAL  s" code" s" E1" SVAL  s" message" s" boom" SVAL ;
: L-ERROR ( -- ptr u8 i64 )     J{ s" type" s" error" SVAL F-COMMON ERROR-BODY
   s" fatal" s" true" RVAL J} J$ ;
: L-ERROR-BADBOOL ( -- ptr u8 i64 ) J{ s" type" s" error" SVAL F-COMMON ERROR-BODY
   s" fatal" s" yes" SVAL J} J$ ;     \ fatal is a string, not a bool

: SUMMARY-COUNTS ( -- )
   s" serial" s" null" RVAL  s" frames_dropped" s" 0" RVAL  s" duplicates" s" 0" RVAL
   s" timestamp_regressions" s" 0" RVAL  s" writer_stalls" s" 0" RVAL ;
: L-SUMMARY ( -- ptr u8 i64 )   J{ s" type" s" summary" SVAL F-COMMON
   s" frames_seen" s" 1" RVAL SUMMARY-COUNTS  s" result" s" pass" SVAL J} J$ ;
: L-SUMMARY-BADENUM ( -- ptr u8 i64 ) J{ s" type" s" summary" SVAL F-COMMON
   s" frames_seen" s" 1" RVAL SUMMARY-COUNTS  s" result" s" bogus" SVAL J} J$ ;
: L-SUMMARY-FLOATINT ( -- ptr u8 i64 ) J{ s" type" s" summary" SVAL F-COMMON
   s" frames_seen" s" 1.5" RVAL SUMMARY-COUNTS  s" result" s" pass" SVAL J} J$ ;

: L-FRAME ( -- ptr u8 i64 )     J{ s" type" s" frame" SVAL F-COMMON
   s" serial" s" SN1" SVAL  s" logical_name" s" front" SVAL
   s" frame_index" s" 0" RVAL  s" sdk_image_timestamp_ns" s" 1" RVAL
   s" host_monotonic_ns" s" 2" RVAL  s" width" s" 900" RVAL  s" height" s" 600" RVAL
   s" fps_target" s" 60" RVAL  s" pixel_format" s" NV12" SVAL
   s" exposure_us" s" null" RVAL  s" gain" s" 1.5" RVAL  s" auto_exposure" s" true" RVAL
   s" image_path" s" null" RVAL  s" dropped" s" false" RVAL  s" duplicate" s" false" RVAL
   s" timestamp_regressed" s" false" RVAL J} J$ ;
: L-SENSOR ( -- ptr u8 i64 )    J{ s" type" s" sensor" SVAL F-COMMON
   s" serial" s" SN1" SVAL  s" logical_name" s" front" SVAL  s" sensor_kind" s" imu" SVAL
   s" time_reference" s" IMAGE" SVAL  s" sensor_timestamp_ns" s" 1" RVAL
   s" image_frame_index" s" null" RVAL  s" sample_rate_hz" s" 100.0" RVAL
   s" values" s" [1,2,3]" RVAL  s" units" s" mps2" SVAL  s" duplicate" s" false" RVAL
   s" stale" s" false" RVAL  s" missing" s" false" RVAL J} J$ ;
: L-SENSOR-BADENUM ( -- ptr u8 i64 ) J{ s" type" s" sensor" SVAL F-COMMON
   s" serial" s" SN1" SVAL  s" logical_name" s" front" SVAL  s" sensor_kind" s" imu" SVAL
   s" time_reference" s" NOPE" SVAL  s" sensor_timestamp_ns" s" 1" RVAL
   s" image_frame_index" s" null" RVAL  s" sample_rate_hz" s" 100.0" RVAL
   s" values" s" [1,2,3]" RVAL  s" units" s" mps2" SVAL  s" duplicate" s" false" RVAL
   s" stale" s" false" RVAL  s" missing" s" false" RVAL J} J$ ;

\ assert a line validates to ( rtype status )
: V= ( ptr u8 i64 i64 i64 -- ) {: rt:i64 st:i64 :} VALIDATE-LINE {: art:i64 ast:i64 :}
   ast st T=  art rt T= ;
: VST= ( ptr u8 i64 i64 -- ) {: st:i64 :} VALIDATE-LINE nip st T= ;  \ status only

: CSJ-RUN ( -- )
   T-RESET
   L-SCHEMA           SCHEMA V-OK V=
   L-FRAME            FRAME  V-OK V=
   L-SENSOR           SENSOR V-OK V=
   L-ERROR            ERROR  V-OK V=
   L-SUMMARY          SUMMARY V-OK V=
   L-SCHEMA-V2        UNSUPPORTED-SCHEMA VST=
   L-SCHEMA-MISS      MISSING-FIELD VST=
   L-SCHEMA-BADTYPE   INVALID-FIELD-TYPE VST=
   L-SCHEMA-TSUNIT    INVALID-TS-UNITS VST=
   L-BADRTYPE         UNKNOWN-RTYPE VST=
   L-ERROR-BADBOOL    INVALID-FIELD-TYPE VST=
   L-SUMMARY-BADENUM  UNKNOWN-ENUM VST=
   L-SUMMARY-FLOATINT INVALID-FIELD-TYPE VST=
   L-SENSOR-BADENUM   UNKNOWN-ENUM VST=
   s" [1,2,3]"        EXPECTED-OBJECT VST=
   s" {oops"          INVALID-JSON VST= ;

CSJ-RUN
T-REPORT
end-package
