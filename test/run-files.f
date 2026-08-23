\ run-files.f - AOT runner support file registry.

require lib/errors.f
require lib/string.f

: TR-FILES-END? ( ptr u8 n -- bool )
   s" ;TR-FILES" STR= ;

: TR-FILES-ITEM, ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 < if E-STR-BOUNDS throw then
   u STR-BYTE-MAX > if E-STR-BOUNDS throw then
   u c,
   0 begin dup u < while
      dup a + c@ c,
      1+
   repeat drop ;

: TR-FILES-PARSE ( -- )
   begin
      parse-name dup 0= if 2drop E-STR-BOUNDS throw then
      2dup TR-FILES-END? if 2drop 0 c, exit then
      TR-FILES-ITEM,
   again ;

: TR-FILES-WALK ( ptr a [ ptr u8 n -- ] -- ) {: p:ptr q :}
   p begin dup c@ 0= 0= while
      dup 1+ over c@ q execute
      dup c@ 1 + +
   repeat drop ;

: TR-FILES-RUN ( [ ptr u8 n -- ] ptr a -- )
   swap TR-FILES-WALK ;

: TR-FILES: ( -- )
   create TR-FILES-PARSE
   does> ( [ ptr u8 n -- ] -- )
      TR-FILES-RUN ;

TR-FILES: TR-AOT-RUNNER-SUPPORT-FILES
   lib/errors.f lib/string.f lib/memory.f lib/cad-num-arithmetic.f lib/cad-num-types.f lib/vector.f lib/fs.f lib/fs-mutate.f
   lib/process.f lib/process-argv.f lib/process-env.f lib/process-fork.f lib/test/record.f lib/test/runner.f
   lib/source.f lib/build.f lib/codesign.f lib/sort.f lib/codegen.f lib/type/deftype.f lib/content-key.f tools/build-fixpoint.f
   lib/object.f lib/object-cache.f lib/object-index.f lib/object-resolve.f
   lib/object-link.f tools/cli-run.f tools/object-image.f tools/hb-build-lib.f
   tools/lint/text.f tools/lint/token.f
   tools/lint/lib.f tools/lint/json-writer.f tools/lint/source-lex.f
   tools/aot-lint-core.f tools/signature-lint-core.f tools/hb-build-direct-lints.f
   tools/json.f tools/gate-json-assert-core.f tools/aot-call-report-lib.f
   test/gate-stats.f test/gate-common-lib.f test/gate-build-common.f
   test/gate-build-hbb.f src/habu/aot-closure.f
   test/gate-aot-positive-lib.f test/gate-aot-negative-lib.f
;TR-FILES
