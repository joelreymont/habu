\ capture-schema.f - NDJSON capture record schema, ported from src/capture_schema.zig.
\
\ Record-type classification and exact schema-version check (the pure validators).
\ The full per-record field validation builds on habu tools/json.f (JSON-PARSE →
\ JSON-GET by key → JSON-KIND), added next. Record-type codes match the .zig
\ RecordType enum order. Depends on lib/errors.f lib/string.f.

package SCHEMA
public
0 constant SCHEMA
1 constant FRAME
2 constant SENSOR
3 constant RESOURCE
4 constant ERROR
5 constant SUMMARY
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
: VERSION-OK? ( ptr u8 n -- bool )  s" odin.capture.v1" STR= ;
end-package
