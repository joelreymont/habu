\ A stripped hb-build uses the FILE loader, then links after the loader closes.
\ Direct maker counts prove cold compilation and warm reuse through that
\ same opener; hb-build then proves the output is a runnable stripped image.
\ Run: bin/hb --load test/native-file-stripped-path-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs-mutate.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require test/aot-builder-engine.f
require test/native-file-e2e.f

using NATIVE-FILE-E2E
package NATIVE-FILE-STRIPPED-PATH-TEST

: MAKER ( ptr u8 n -- n ) {: cache:ptr cacheu:n :}
   AOT-BUILDER-ENGINE:PATH$ {: maker:ptr makeru:n :}
   PROC-ARGV-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN cache cacheu AT >LEN PROC-ENV+
   s" HB_TMP" >LEN ROOT$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --" ARG s" app.f" AT ARG s" 0" ARG s" 1" ARG
   maker makeru AOT-BUILDER-ENGINE:STDIN$ CAPTURE ;

: BUILD ( ptr u8 n -- n ) {: cache:ptr cacheu:n :}
   PROC-ARGV-ENV-RESET
   s" HABU_BUILD_CACHE" >LEN cache cacheu AT >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE-CANDIDATE:PATH$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --load" ARG s" tools/hb-build.f" ARG s" --" ARG
   s" app.f" AT ARG s" -o" ARG s" app-hb" AT ARG
   ENGINE-CANDIDATE:PATH$ s" " CAPTURE ;

: CHECK-RUN ( -- )
   RUN-APP SUCCESS
   OUT$ S\" 42\n" CONTAINS? TTRUE ;

: REPORT ( -- )
   OUT$ s" definition hits " CONTAINS? TTRUE ;
: HITS ( -- )
   REPORT
   OUT$ S\" definition hits 0\n" CONTAINS? TFALSE ;

: USER-FILE ( ptr u8 n ptr u8 n -- )
   {: mode:ptr modeu:n product:ptr productu:n :}
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   s" --load" ARG s" tools/native-unit-build.f" ARG s" --" ARG
   mode modeu ARG
   s" prefix.f" AT ARG s" a.f" AT ARG s" app.f" AT ARG
   s" user.unit" AT ARG product productu AT ARG
   ENGINE-CANDIDATE:PATH$ s" " CAPTURE SUCCESS ;

: RUN-USER-IMAGE ( ptr u8 n -- )
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   AT s" MAIN" CAPTURE SUCCESS
   OUT$ S\" 17\n29\n1\n1\n" T$= ;

\ A cached accessor already contains its offset. Its declaration must also
\ advance #USER before a source-loaded sibling allocates the next slot.
: USER-SLOTS ( -- )
   s" imported FILE user slots precede source-loaded slots" T-LABEL
   s" prefix.f" S\" require src/lib/task.f\n" PUT
   s" a.f" S\" require src/lib/task.f\npackage USER-A\npublic\nTASK:#USER CELL TASK:+USER SLOT drop\n;package\ns\q user A source ran\q type cr\n" PUT
   s" app.f" S\" require a.f\n: MAIN ( -- ) 17 USER-A:SLOT ! ;\n" PUT
   s" --export-file" s" user-initial-hb" USER-FILE
   OUT$ s" user A source ran" CONTAINS? TTRUE
   s" b.f" S\" require src/lib/task.f\npackage USER-B\npublic\nTASK:#USER CELL TASK:+USER SLOT drop\n;package\ns\q user B source ran\q type cr\n" PUT
   s" app.f" S\" require a.f\nrequire b.f\n: MAIN ( -- )\n   17 USER-A:SLOT ! 29 USER-B:SLOT !\n   USER-A:SLOT @ . USER-B:SLOT @ .\n   USER-B:SLOT BYTE-VIEW USER-A:SLOT BYTE-VIEW - CELL = if 1 else 0 then .\n   TASK:#USER USER-A:SLOT BYTE-VIEW data-base BYTE-VIEW - - 2 cells = if 1 else 0 then . ;\n" PUT
   s" --import-file" s" user-import-hb" USER-FILE
   OUT$ s" native-build: file unit hit" CONTAINS? TTRUE
   OUT$ s" user A source ran" CONTAINS? TFALSE
   OUT$ s" user B source ran" CONTAINS? TTRUE
   s" user-import-hb" RUN-USER-IMAGE ;

public
: MAIN ( -- )
   T-RESET
   s" stripped build clears transient application path" T-LABEL
   s" native-file-stripped-path" TREE
   s" cache" AT MAKE-DIR
   s" leaf.f" S\" package FILE-PATH-LEAF\npublic\n: VALUE ( -- n ) 42 ;\n;package\n" PUT
   s" app.f" S\" require leaf.f\n: MAIN ( -- ) FILE-PATH-LEAF:VALUE . ;\n" PUT
   s" cache" MAKER SUCCESS
   OUT$ S\" definition hits 0\n" CONTAINS? TTRUE
   s" cache" MAKER SUCCESS
   HITS
   s" cache" BUILD SUCCESS
   CHECK-RUN
   s" app.f" S\" require leaf.f\n\n: MAIN ( -- ) FILE-PATH-LEAF:VALUE . ;\n" PUT
   s" cache" MAKER SUCCESS
   HITS
   s" cache" BUILD SUCCESS
   CHECK-RUN
   s" env-cache" AT MAKE-DIR
   s" app.f" S\" require src/lib/fs.f\nrequire src/lib/engine-id.f\nrequire src/lib/process-env.f\n: MAIN ( -- ) PROC-ENV-RESET 42 . ;\n" PUT
   s" env-cache" MAKER SUCCESS
   OUT$ S\" definition hits 0\n" CONTAINS? TTRUE
   s" env-cache" MAKER SUCCESS
   REPORT
   s" env-cache" BUILD SUCCESS
   CHECK-RUN
   USER-SLOTS
   T-REPORT
   s" native file stripped artifacts: " type ROOT$ type cr ;
;package
;using

NATIVE-FILE-STRIPPED-PATH-TEST:MAIN
