\ An image build refuses DATA holding an undeclared code pointer or a mapped
\ address, and admits a mapped pointer lifecycle cleanup clears, cold and warm.
\ bin/hb --load test/native-file-admission-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-env.f
require test/aot-builder-engine.f
require test/native-file-e2e.f

using NATIVE-FILE-E2E
package NATIVE-FILE-ADMISSION-TEST
: BUILD ( -- n )
   AOT-BUILDER-ENGINE:PATH$ {: maker:ptr makeru:n :}
   PROC-ARGV-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN s" cache" AT >LEN PROC-ENV+
   s" HB_TMP" >LEN ROOT$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --" ARG s" app.f" AT ARG s" 0" ARG s" 1" ARG
   maker makeru AOT-BUILDER-ENGINE:STDIN$ CAPTURE ;
\ These probes deliberately store code pointers without declaring relocation
\ cells. Trusted fixture words let image admission inspect them after loading.
: COLD-UNSAFE ( -- )
   s" app.f" S\" require unsafe.f\n: MAIN ( -- ) ADMIT-BAD @ . ;\n" PUT
   s" unsafe.f" S\" : ADMIT-BUMP ( n -- n ) 1+ ;\nTRUSTED: ADMIT-RAW, ( -- ) ['] ADMIT-BUMP , ;\ncreate ADMIT-BAD ADMIT-RAW,\ns\" unsafe-source-ran\" type cr\n" PUT ;
: COLD-UNALIGNED ( -- )
   s" app.f" S\" require unaligned.f\n: MAIN ( -- ) ;\n" PUT
   s" unaligned.f" S\" : ADMIT-BUMP ( n -- n ) 1+ ;\nTRUSTED: ADMIT-RAW, ( -- ) ['] ADMIT-BUMP , ;\ncreate ADMIT-RAW 1 c, ADMIT-RAW,\ns\" unaligned-source-ran\" type cr\n" PUT ;
: COLD-DOES-UNALIGNED ( -- )
   s" app.f" S\" require does-unaligned.f\n: MAIN ( -- ) ;\n" PUT
   s" does-unaligned.f" S\" : ADMIT-BUMP ( n -- n ) 1+ ;\n: ADMIT-MAKE ( n -- ) create 1 c, , does> ( -- n ) drop 0 ;\nTRUSTED: ADMIT-RAW-MAKE ( -- ) ['] ADMIT-BUMP ADMIT-MAKE ;\nADMIT-RAW-MAKE ADMIT-RAW\ns\" does-unaligned-source-ran\" type cr\n" PUT ;
: COLD-TYPED-MAPPED ( -- )
   s" app.f" S\" require typed-mapped.f\n: MAIN ( -- ) ;\n" PUT
   s" typed-mapped.f"
      S\" require src/lib/memory.f\npackage ADMIT-TYPED\npublic\nTYPED-VARIABLE BAD n\n: BAD! ( -- ) MEM-ALLOC-64K {: p:ptr bytes:n :} p BYTE-VIEW NULL-PTR BYTE-VIEW - BAD ! bytes drop ;\nBAD!\n;package\ns\" typed-mapped-source-ran\" type cr\n" PUT ;
: LIFECYCLE ( -- )
   s" app.f" S\" require lifecycle.f\n: MAIN ( -- ) ADMIT-LIFE:SAFE? if s\" life-ok\" type cr then ;\n" PUT
   s" lifecycle.f"
      S\" require src/lib/memory.f\nrequire src/lib/image-lifecycle.f\npackage ADMIT-LIFE\nPERSISTED-PTR-VARIABLE PBUF\n: CLEAN ( -- ) PBUF @ BYTE-VIEW 65536 munmap 0<> if E-MEM-UNMAP throw then NULL-PTR PBUF ! ;\n: ARM ( -- ) MEM-ALLOC-64K {: p:ptr bytes:n :} p PBUF ! bytes drop ['] CLEAN IMAGE-LIFECYCLE:REGISTER ;\nARM\npublic\n: SAFE? ( -- bool ) PBUF @ NULL-PTR = ;\n;package\ns\" life-source-ran\" type cr\n" PUT ;
: STRICT-STATE ( -- )
   s" app.f" S\" require scalars.f\nrequire defer.f\n: MAIN ( -- ) SHORT-ID @ LONG-ID @ + . ADMIT-DEFER:VALUE@ . ;\n" PUT
   s" scalars.f" S\" variable SHORT-ID variable LONG-ID\n2 SHORT-ID ! 3 LONG-ID !\n" PUT
   s" defer.f"
      S\" package ADMIT-DEFER\nSTRUCTURE item DERIVE addr FIELD value n ;STRUCTURE\nDEFER-LAYOUT-BUFFER ROWS item\n2 ROWS-BIND\n456 ITEM-MAKE 0 ROWS !\n4 ROWS-GROW\npublic\n: VALUE@ ( -- n ) 0 ROWS ITEM-VALUE @ ;\n;package\ns\" strict-source-ran\" type cr\n" PUT ;
: RUN-IMAGE ( -- n )
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING s" hb-aot-got" AT s" " CAPTURE ;
: SAID? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   OUT$ a u CONTAINS? ERR$ a u CONTAINS? or ;
public
: RUN ( -- )
   T-RESET
   s" native-file-admission" TREE
   s" cache" AT MAKE-DIR
   s" undeclared code pointer declines" T-LABEL
   COLD-UNSAFE
   BUILD 70 T=
   OUT$ s" unsafe-source-ran" CONTAINS? TTRUE
   s" undeclared code/dict pointer" SAID? TTRUE
   \ The failed cold build still cached definitions, so the warm build refuses
   \ the raw pointer against cached code.
   BUILD 70 T=
   OUT$ s" unsafe-source-ran" CONTAINS? TTRUE
   OUT$ s" definition hits " CONTAINS? TTRUE
   OUT$ S\" definition hits 0\n" CONTAINS? TFALSE
   s" undeclared code/dict pointer" SAID? TTRUE
   s" unaligned pointer declines" T-LABEL
   COLD-UNALIGNED
   BUILD 70 T=
   OUT$ s" unaligned-source-ran" CONTAINS? TTRUE
   s" undeclared code/dict pointer" SAID? TTRUE
   s" nonempty does> keeps its raw interior byte scan" T-LABEL
   COLD-DOES-UNALIGNED
   BUILD 70 T=
   OUT$ s" does-unaligned-source-ran" CONTAINS? TTRUE
   s" undeclared code/dict pointer" SAID? TTRUE
   s" aligned typed scalar still refuses a mapped address" T-LABEL
   COLD-TYPED-MAPPED
   BUILD 70 T=
   OUT$ s" typed-mapped-source-ran" CONTAINS? TTRUE
   s" mapped" SAID? TTRUE
   s" mapped pointer with lifecycle cleanup builds from source twice" T-LABEL
   LIFECYCLE
   BUILD 0 T=
   OUT$ s" life-source-ran" CONTAINS? TTRUE
   RUN-IMAGE 0 T=
   OUT$ s" life-ok" CONTAINS? TTRUE
   BUILD 0 T=
   OUT$ s" life-source-ran" CONTAINS? TTRUE
   RUN-IMAGE 0 T=
   OUT$ s" life-ok" CONTAINS? TTRUE
   s" adjacent scalars and deferred layout growth survive warm reuse" T-LABEL
   STRICT-STATE
   BUILD 0 T=
   OUT$ s" strict-source-ran" CONTAINS? TTRUE
   RUN-IMAGE 0 T=
   OUT$ S\" 5\n456\n" CONTAINS? TTRUE
   BUILD 0 T=
   OUT$ s" strict-source-ran" CONTAINS? TTRUE
   OUT$ s" definition hits " CONTAINS? TTRUE
   OUT$ S\" definition hits 0\n" CONTAINS? TFALSE
   RUN-IMAGE 0 T=
   OUT$ S\" 5\n456\n" CONTAINS? TTRUE
   T-REPORT
   s" native file admission artifacts: " type ROOT$ type cr ;
;package
;using

NATIVE-FILE-ADMISSION-TEST:RUN
