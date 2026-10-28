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
   s" HABU_OUT" >LEN cache cacheu AT >LEN PROC-ENV+
   s" HB_TMP" >LEN ROOT$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --" ARG s" app.f" AT ARG s" 0" ARG s" 1" ARG
   maker makeru AOT-BUILDER-ENGINE:STDIN$ CAPTURE ;

: BUILD ( ptr u8 n -- n ) {: cache:ptr cacheu:n :}
   PROC-ARGV-ENV-RESET
   s" HABU_OUT" >LEN cache cacheu AT >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE-CANDIDATE:PATH$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --load" ARG s" tools/hb-build.f" ARG s" --" ARG
   s" app.f" AT ARG s" -o" ARG s" app-hb" AT ARG
   ENGINE-CANDIDATE:PATH$ s" " CAPTURE ;

: CHECK-RUN ( ptr u8 n -- )
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   AT s" " CAPTURE SUCCESS
   OUT$ S\" 42\n" T$= ;

: REUSED ( -- )
   OUT$ s" files reused " CONTAINS? TTRUE
   OUT$ s" files reused 0" CONTAINS? TFALSE ;

public
: MAIN ( -- )
   T-RESET
   s" stripped build clears transient application path" T-LABEL
   s" native-file-stripped-path" TREE
   s" cache" AT MAKE-DIR
   s" leaf.f" S\" package FILE-PATH-LEAF\npublic\n: VALUE ( -- n ) 42 ;\n;package\n" PUT
   s" app.f" S\" require leaf.f\n: MAIN ( -- ) FILE-PATH-LEAF:VALUE . ;\n" PUT
   s" cache" MAKER SUCCESS
   OUT$ s" files reused 0" CONTAINS? TTRUE
   s" app.f" S\" require leaf.f\n\n: MAIN ( -- ) FILE-PATH-LEAF:VALUE . ;\n" PUT
   s" cache" MAKER SUCCESS
   REUSED
   s" hb-aot-got" CHECK-RUN
   \ Keep the simple leaf above free of library requirements: its linker loads
   \ them after the capture window. This app loads them inside the window.
   s" env-cache" AT MAKE-DIR
   s" app.f" S\" require src/lib/fs.f\nrequire src/lib/engine-id.f\nrequire src/lib/process-env.f\n: MAIN ( -- ) PROC-ENV-RESET 42 . ;\n" PUT
   s" env-cache" BUILD SUCCESS
   s" app-hb" CHECK-RUN
   T-REPORT
   s" native file stripped artifacts: " type ROOT$ type cr ;
;package
;using

NATIVE-FILE-STRIPPED-PATH-TEST:MAIN
