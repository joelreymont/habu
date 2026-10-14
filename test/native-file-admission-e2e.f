\ An unsafe FILE source must run again on the next build. The final image
\ still makes the ordinary AOT decision after source and lifecycle cleanup.
\ bin/hb --load test/native-file-admission-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require test/aot-maker-engine.f

package NATIVE-FILE-ADMISSION-TEST

create ROOT FS-PATH-CAP allot variable ROOT-U
create PATH FS-PATH-CAP allot
$8000 constant CAP
create OUT CAP allot variable OUT-U
create ERR CAP allot variable ERR-U

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: AT ( ptr u8 n -- ptr u8 n )
   {: name:ptr u:n :}
   ROOT$ name u PATH JOIN-PATH PATH swap ;
: PUT ( ptr u8 n ptr u8 n -- )
   {: name:ptr nameu:n body:ptr bodyu:n :}
   name nameu AT body bodyu WRITE-ALL ;
: ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;
: BUILD ( -- n )
   AOT-MAKER-ENGINE:PATH$ {: maker:ptr makeru:n :}
   PROC-ARGV-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN s" cache" AT >LEN PROC-ENV+
   s" HB_TMP" >LEN ROOT$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --" ARG s" app.f" AT ARG s" 0" ARG s" 1" ARG
   maker makeru >LEN
   AOT-MAKER-ENGINE:STDIN$ >LEN
   OUT CAP >LEN ERR CAP >LEN 600000 >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U ! rc ;
: COLD-UNSAFE ( -- )
   s" app.f" S\" require unsafe.f\n: MAIN ( -- ) ADMIT-BAD @ . ;\n" PUT
   s" unsafe.f" S\" : ADMIT-BUMP ( n -- n ) 1+ ;\ncreate ADMIT-BAD ' ADMIT-BUMP ,\ns\" unsafe-source-ran\" type cr\n" PUT ;
: COLD-UNALIGNED ( -- )
   s" app.f" S\" require unaligned.f\n: MAIN ( -- ) ;\n" PUT
   s" unaligned.f" S\" : ADMIT-BUMP ( n -- n ) 1+ ;\ncreate ADMIT-RAW 1 c, ' ADMIT-BUMP ,\ns\" unaligned-source-ran\" type cr\n" PUT ;
: COLD-DOES-UNALIGNED ( -- )
   s" app.f" S\" require does-unaligned.f\n: MAIN ( -- ) ;\n" PUT
   s" does-unaligned.f" S\" : ADMIT-BUMP ( n -- n ) 1+ ;\n: ADMIT-MAKE ( n -- ) create 1 c, , does> ( -- n ) drop 0 ;\n' ADMIT-BUMP ADMIT-MAKE ADMIT-RAW\ns\" does-unaligned-source-ran\" type cr\n" PUT ;
: COLD-TYPED-MAPPED ( -- )
   s" app.f" S\" require typed-mapped.f\n: MAIN ( -- ) ;\n" PUT
   s" typed-mapped.f"
      S\" require src/lib/memory.f\npackage ADMIT-TYPED\npublic\nTYPED-VARIABLE BAD n\n: BAD! ( -- ) MEM-ALLOC-64K {: p:ptr bytes:n :} p BYTE-VIEW NULL-PTR BYTE-VIEW - BAD ! bytes drop ;\nBAD!\n;package\ns\" typed-mapped-source-ran\" type cr\n" PUT ;
: LIFECYCLE ( -- )
   s" app.f" S\" require lifecycle.f\n: MAIN ( -- ) ADMIT-LIFE:SAFE? if s\" life-ok\" type cr then ;\n" PUT
   s" lifecycle.f"
      S\" require src/lib/memory.f\nrequire src/lib/image-lifecycle.f\npackage ADMIT-LIFE\nPERSISTED-PTR-VARIABLE PBUF\n: CLEAN ( -- ) PBUF @ BYTE-VIEW 65536 munmap 0<> if E-MEM-UNMAP throw then NULL-PTR PBUF ! ;\n: ARM ( -- ) MEM-ALLOC-64K {: p:ptr bytes:n :} p PBUF ! bytes drop ['] CLEAN IMAGE-LIFECYCLE:REGISTER ;\nARM\npublic\n: SAFE? ( -- bool ) PBUF @ NULL-PTR = ;\n;package\ns\" life-source-ran\" type cr\n" PUT ;
: RUN-IMAGE ( -- n )
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   s" hb-aot-got" AT >LEN s" " >LEN
   OUT CAP >LEN ERR CAP >LEN 600000 >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U ! rc ;
: SAID? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   OUT OUT-U @ a u CONTAINS? ERR ERR-U @ a u CONTAINS? or ;
: STRICT-SOURCE ( bool -- ) {: json?:bool :}
   s" prefix.f" S\" : ADMIT-PREFIX ( -- ) ;\n" PUT
   s" downstream.f" S\" : MAIN ( -- ) ;\n" PUT
   s" selected.f" S\" require src/lib/memory.f\npackage ADMIT-LIVE\npublic\nPERSISTED-PTR-VARIABLE PBUF\n: ARM ( -- ) MEM-ALLOC-64K {: p:ptr bytes:n :} p PBUF ! bytes drop ;\nARM\n;package\n" PUT
   json? if s" selected.f" AT S\" -1 JSON-DIAGS !\n" APPEND-FILE then ;
: STRICT-UNALIGNED ( -- )
   s" prefix.f" S\" : ADMIT-PREFIX ( -- ) ;\n" PUT
   s" downstream.f" S\" : MAIN ( -- ) ;\n" PUT
   s" selected.f" S\" : ADMIT-BUMP ( n -- n ) 1+ ;\ncreate ADMIT-RAW 1 c, ' ADMIT-BUMP ,\n" PUT ;
: STRICT-PREFIX-UPDATE ( -- )
   s" prefix.f" S\" create ADMIT-RAW 16 allot\n" PUT
   s" downstream.f" S\" : MAIN ( -- ) ;\n" PUT
   s" selected.f" S\" : ADMIT-BUMP ( n -- n ) 1+ ;\n' ADMIT-BUMP ADMIT-RAW 1+ !\n" PUT ;
: STRICT-ACCEPTED ( -- )
   s" prefix.f" S\" : ADMIT-PREFIX ( -- ) ;\n" PUT
   s" selected.f"
      S\" defer ADMIT-ACTION ( n -- n )\n: ADMIT-BUMP ( n -- n ) 1+ ;\n: ADMIT-INSTALL ( -- ) ['] ADMIT-BUMP is ADMIT-ACTION ;\nADMIT-INSTALL\n: ADMIT-TEXT ( -- ptr u8 n ) s\" literal-owned-by-file\" ;\n" PUT
   s" downstream.f"
      S\" : MAIN ( -- ) 41 ADMIT-ACTION . ADMIT-TEXT type cr ;\n" PUT ;
: STRICT-SCALARS ( -- )
   s" prefix.f" S\" : ADMIT-PREFIX ( -- ) ;\n" PUT
   s" selected.f"
      S\" variable SHORT-ID variable LONG-ID\n2 SHORT-ID ! 3 LONG-ID !\n" PUT
   s" downstream.f"
      S\" : MAIN ( -- ) SHORT-ID @ LONG-ID @ + . ;\n" PUT ;
: STRICT-DEFER ( -- )
   s" prefix.f" S\" : ADMIT-PREFIX ( -- ) ;\n" PUT
   s" selected.f"
      S\" package ADMIT-DEFER\nSTRUCTURE item DERIVE addr FIELD value n ;STRUCTURE\nDEFER-LAYOUT-BUFFER ROWS item\n2 ROWS-BIND\n456 ITEM-MAKE 0 ROWS !\n4 ROWS-GROW\npublic\n: VALUE@ ( -- n ) 0 ROWS ITEM-VALUE @ ;\n;package\n" PUT
   s" downstream.f"
      S\" : MAIN ( -- ) ADMIT-DEFER:VALUE@ . ;\n" PUT ;
: STRICT-TYPED-MAPPED ( -- )
   s" prefix.f" S\" : ADMIT-PREFIX ( -- ) ;\n" PUT
   s" downstream.f" S\" : MAIN ( -- ) ;\n" PUT
   s" selected.f"
      S\" require src/lib/memory.f\npackage ADMIT-TYPED\npublic\nTYPED-VARIABLE BAD n\n: BAD! ( -- ) MEM-ALLOC-64K {: p:ptr bytes:n :} p BYTE-VIEW NULL-PTR BYTE-VIEW - BAD ! bytes drop ;\nBAD!\n;package\n" PUT ;
: RUN-STRICT ( -- n )
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   s" strict-hb" AT >LEN s" MAIN" >LEN
   OUT CAP >LEN ERR CAP >LEN 600000 >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U ! rc ;
: FILE-DRIVER ( ptr u8 n -- n ) {: mode:ptr modeu:n :}
   PROC-ARGV-ENV-RESET
   s" HB_TMP" >LEN ROOT$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --load" ARG s" tools/native-unit-build.f" ARG s" --" ARG
   mode modeu ARG
   s" prefix.f" AT ARG s" selected.f" AT ARG s" downstream.f" AT ARG
   s" strict.unit" AT ARG s" strict-hb" AT ARG
   ENGINE-CANDIDATE:PATH$ >LEN s" " >LEN
   OUT CAP >LEN ERR CAP >LEN 600000 >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N OUT-U ! erru LEN>N ERR-U ! rc ;
: EXPORT ( -- n ) s" --export-file" FILE-DRIVER ;
: IMPORT ( -- n ) s" --import-file" FILE-DRIVER ;

public
: RUN ( -- )
   T-RESET
   s" native-file-admission" HB-TMP-MKDIR {: root:ptr size:n :}
   root ROOT size BYTE-COPY size ROOT-U !
   s" cache" AT MAKE-DIR
   COLD-UNSAFE
   BUILD 70 T=
   OUT OUT-U @ s" unsafe-source-ran" CONTAINS? TTRUE
   s" undeclared code/dict pointer" SAID? TTRUE
   BUILD 70 T=
   OUT OUT-U @ s" unsafe-source-ran" CONTAINS? TTRUE
   s" undeclared code/dict pointer" SAID? TTRUE
   s" unaligned pointer declines on cold and warm builds" T-LABEL
   COLD-UNALIGNED
   BUILD 70 T=
   OUT OUT-U @ s" unaligned-source-ran" CONTAINS? TTRUE
   s" undeclared code/dict pointer" SAID? TTRUE
   BUILD 70 T=
   OUT OUT-U @ s" unaligned-source-ran" CONTAINS? TTRUE
   s" undeclared code/dict pointer" SAID? TTRUE
   s" nonempty does> keeps its raw interior byte scan" T-LABEL
   COLD-DOES-UNALIGNED
   BUILD 70 T=
   OUT OUT-U @ s" does-unaligned-source-ran" CONTAINS? TTRUE
   s" undeclared code/dict pointer" SAID? TTRUE
   s" aligned typed scalar still refuses a mapped address" T-LABEL
   COLD-TYPED-MAPPED
   BUILD 70 T=
   OUT OUT-U @ s" typed-mapped-source-ran" CONTAINS? TTRUE
   s" mapped" SAID? TTRUE
   BUILD 70 T=
   OUT OUT-U @ s" typed-mapped-source-ran" CONTAINS? TTRUE
   s" mapped" SAID? TTRUE
   s" mapped pointer with lifecycle cleanup builds from source twice" T-LABEL
   LIFECYCLE
   BUILD 0 T=
   OUT OUT-U @ s" life-source-ran" CONTAINS? TTRUE
   RUN-IMAGE 0 T=
   OUT OUT-U @ s" life-ok" CONTAINS? TTRUE
   BUILD 0 T=
   OUT OUT-U @ s" life-source-ran" CONTAINS? TTRUE
   RUN-IMAGE 0 T=
   OUT OUT-U @ s" life-ok" CONTAINS? TTRUE
   s" explicit FILE export names an unsafe declared cell" T-LABEL
   false STRICT-SOURCE
   EXPORT 0<> TTRUE
   s" word=PBUF" SAID? TTRUE
   s" mapped" SAID? TTRUE
   true STRICT-SOURCE
   EXPORT 0<> TTRUE
   s" schema_version" SAID? TTRUE
   s" E-AOT-UNSUPPORTED" SAID? TTRUE
   s" PBUF" SAID? TTRUE
   s" explicit FILE export refuses unaligned new DATA" T-LABEL
   STRICT-UNALIGNED
   EXPORT 70 T=
   s" undeclared code/dict pointer" SAID? TTRUE
   s" explicit FILE export refuses unaligned prefix update" T-LABEL
   STRICT-PREFIX-UPDATE
   EXPORT 70 T=
   s" undeclared code/dict pointer" SAID? TTRUE
   s" explicit FILE export refuses aligned typed mapped data" T-LABEL
   STRICT-TYPED-MAPPED
   EXPORT 70 T=
   s" mapped" SAID? TTRUE
   s" declared slot and literal text survive FILE replay" T-LABEL
   STRICT-ACCEPTED
   EXPORT 0 T=
   RUN-STRICT 0 T=
   OUT OUT-U @ S\" 42\nliteral-owned-by-file" CONTAINS? TTRUE
   IMPORT 0 T=
   OUT OUT-U @ s" native-build: file unit hit" CONTAINS? TTRUE
   RUN-STRICT 0 T=
   OUT OUT-U @ S\" 42\nliteral-owned-by-file" CONTAINS? TTRUE
   s" adjacent scalar variables survive FILE replay" T-LABEL
   STRICT-SCALARS
   EXPORT 0 T=
   RUN-STRICT 0 T=
   OUT OUT-U @ S\" 5\n" CONTAINS? TTRUE
   IMPORT 0 T=
   OUT OUT-U @ s" native-build: file unit hit" CONTAINS? TTRUE
   RUN-STRICT 0 T=
   OUT OUT-U @ S\" 5\n" CONTAINS? TTRUE
   s" deferred layout growth survives FILE replay" T-LABEL
   STRICT-DEFER
   EXPORT 0 T=
   RUN-STRICT 0 T=
   OUT OUT-U @ S\" 456\n" CONTAINS? TTRUE
   IMPORT 0 T=
   OUT OUT-U @ s" native-build: file unit hit" CONTAINS? TTRUE
   RUN-STRICT 0 T=
   OUT OUT-U @ S\" 456\n" CONTAINS? TTRUE
   T-REPORT
   s" native file admission artifacts: " type ROOT$ type cr ;
;package

NATIVE-FILE-ADMISSION-TEST:RUN
