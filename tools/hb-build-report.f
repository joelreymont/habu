\ hb-build-report.f - structured machine-readable hb-build reports.
\ Load after lib/json-write.f and lib/build-cache.f.

require lib/build-cache.f
require lib/memory.f
require lib/json-write.f

package HB-BUILD

: FIELD-COMMA ( -- )
   JW-COMMA ;

public

: RENDER ( ptr u8 n BUILD-CACHE:source bool bool bool n -- ptr u8 n )
   {: root:ptr rootu:n source:BUILD-CACHE:source artifact:bool object:bool maker:bool elapsed:n :}
   JW-RESET
   JW-OBJECT-START
   s" schema" s" hb-build-report" JW-FIELD-S FIELD-COMMA
   s" version" 1 JW-FIELD-U FIELD-COMMA
   s" cache_root" root rootu JW-FIELD-S FIELD-COMMA
   s" cache_source" source BUILD-CACHE:SOURCE$ JW-FIELD-S FIELD-COMMA
   s" artifact_hit" artifact JW-FIELD-BOOL FIELD-COMMA
   s" object_hit" object JW-FIELD-BOOL FIELD-COMMA
   s" maker_hit" maker JW-FIELD-BOOL FIELD-COMMA
   s" elapsed_ns" elapsed JW-FIELD-U
   JW-OBJECT-END
   JW$ ;

;package
