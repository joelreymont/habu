\ low-light-manifest-test.f - oracle from src/low_light.zig "low-light parses
\ scenario manifest" + "low-light rejects invalid manifests". Manifests are built
\ programmatically (nested target_proxy object).
\ Run: cat lib/errors.f lib/string.f lib/memory.f lib/float.f lib/test.f tools/json.f \
\        odin/low-light-manifest.f odin/low-light-manifest-test.f | bin/hb

package LOWLIGHT
private
$1000 constant TB-CAP
create TB TB-CAP allot
variable TB-N  variable TB-CP
: TB+C ( n -- ) TB TB-N @ + c!  TB-N @ 1+ TB-N ! ;
: TB+ ( ptr u8 i64 -- ) {: a:ptr u:i64 :}
   0 TB-CP ! begin TB-CP @ u < while a TB-CP @ + c@ TB+C TB-CP @ 1+ TB-CP ! repeat ;
: Q$ ( ptr u8 i64 -- ) {: a:ptr u:i64 :} J-DQ TB+C a u TB+ J-DQ TB+C ;
: KEY: ( ptr u8 i64 -- ) Q$ J-COLON TB+C ;
: SVAL ( ptr u8 i64 ptr u8 i64 -- ) {: k:ptr ku:i64 v:ptr vu:i64 :} k ku KEY: v vu Q$ J-COMMA TB+C ;
: RVAL ( ptr u8 i64 ptr u8 i64 -- ) {: k:ptr ku:i64 r:ptr ru:i64 :} k ku KEY: r ru TB+ J-COMMA TB+C ;
: OBJ{ ( ptr u8 i64 -- ) KEY: J-LBRACE TB+C ;
: OBJ} ( -- ) J-RBRACE TB TB-N @ 1- + c!  J-COMMA TB+C ;   \ close inner obj, keep outer comma
: J{ ( -- ) 0 TB-N ! J-LBRACE TB+C ;
: J} ( -- ) J-RBRACE TB TB-N @ 1- + c! ;
: J$ ( -- ptr u8 i64 ) TB TB-N @ ;

: M-TARGET ( ptr u8 i64 -- ) {: rv:ptr rvu:i64 :}     \ range_m raw text
   s" target_proxy" OBJ{
      s" description" s" dark proxy" SVAL
      s" range_m" rv rvu RVAL
      s" contrast_reference" s" samples" SVAL
   OBJ} ;
: M-BUILD ( ptr u8 i64 ptr u8 i64 -- ptr u8 i64 ) {: sv:ptr svu:i64 rv:ptr rvu:i64 :}  \ schema, range
   J{
   s" schema_version" sv svu SVAL
   s" scenario" s" dusk-auto" SVAL
   s" captured_at_utc" s" 2026-06-24T18:00:00Z" SVAL
   s" light_reference" s" incident lux" SVAL
   s" lighting_condition" s" dusk sky" SVAL
   s" time_of_day" s" dusk" SVAL
   rv rvu M-TARGET
   s" exposure_mode" s" auto exposure" SVAL
   s" gain_mode" s" auto gain" SVAL
   s" exposure_plan" s" record" SVAL
   s" warmup_ms" s" 5000" RVAL
   s" settling_ms" s" 2000" RVAL
   s" repeats" s" 3" RVAL
   J} J$ ;

: LLM-RUN ( -- )
   T-RESET
   s" odin.low_light_manifest.v1" s" 100.0" M-BUILD  LL-VALIDATE  LL-OK T=
   s" odin.low_light_manifest.v2" s" 100.0" M-BUILD  LL-VALIDATE  LL-UNSUPPORTED-SCHEMA T=
   s" odin.low_light_manifest.v1" s" -1.0"  M-BUILD  LL-VALIDATE  LL-INVALID-MANIFEST T= ;

LLM-RUN
T-REPORT
end-package
