\ capture-schema.f - NDJSON capture record schema, ported from src/capture_schema.zig.
\
\ Record-type classification and exact schema-version check (the pure validators).
\ The full per-record field validation builds on habu tools/json.f (JSON-PARSE →
\ JSON-GET by key → JSON-KIND), added next. The first six record-type codes match
\ the capture_schema.zig RecordType enum order; live detector schemas use stable
\ extension codes because they dispatch by schema_version, not by a "type" field.
\ Depends on lib/errors.f lib/string.f.

package SCHEMA
public
0 constant SCHEMA
1 constant FRAME
2 constant SENSOR
3 constant RESOURCE
4 constant ERROR
5 constant SUMMARY
6 constant DETECTION
7 constant PERCEPTION-TICK
8 constant TRACKER-TICK
-1 constant UNKNOWN

\ record type name -> code, or UNKNOWN
public
: RTYPE ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u s" schema"   STR= if SCHEMA   exit then
   a u s" frame"    STR= if FRAME    exit then
   a u s" sensor"   STR= if SENSOR   exit then
   a u s" resource" STR= if RESOURCE exit then
   a u s" error"    STR= if ERROR    exit then
   a u s" summary"  STR= if SUMMARY  exit then
   UNKNOWN ;

\ the one supported schema version, exact match
: CAPTURE-VERSION? ( ptr u8 n -- bool ) s" odin.capture.v1" STR= ;
: DETECTION-VERSION? ( ptr u8 n -- bool ) s" odin.localization_detections.v1" STR= ;
: PERCEPTION-TICK-VERSION? ( ptr u8 n -- bool ) s" odin.perception_tick.v1" STR= ;
: TRACKER-TICK-VERSION? ( ptr u8 n -- bool ) s" odin.tracker_tick.v1" STR= ;

: VERSION-OK? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u CAPTURE-VERSION? if 0 0= exit then
   a u DETECTION-VERSION? if 0 0= exit then
   a u PERCEPTION-TICK-VERSION? if 0 0= exit then
   a u TRACKER-TICK-VERSION? if 0 0= exit then
   0 0= 0= ;

: VERSION-RTYPE ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u DETECTION-VERSION? if DETECTION exit then
   a u PERCEPTION-TICK-VERSION? if PERCEPTION-TICK exit then
   a u TRACKER-TICK-VERSION? if TRACKER-TICK exit then
   UNKNOWN ;
end-package
