\ hb-build resolves one build cache root and hands it to its children. With
\ HABU_BUILD_CACHE empty, a REPL build whose root comes from XDG_CACHE_HOME
\ leaves the maker's definition cache under that root.
\ Run: bin/hb --load test/native-file-root-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-list.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require test/native-file-e2e.f

using NATIVE-FILE-E2E
package NATIVE-FILE-ROOT-TEST

$10000 constant NAMES-CAP
create NAMES NAMES-CAP allot

: ENV-EMPTY ( ptr u8 n -- )
   >LEN s" " >LEN PROC-ENV+ ;

\ BUILD-APP's REPL build, its cache root selected by XDG_CACHE_HOME alone. It
\ runs at tier 1 (test/compiler/aot-mode.f): hb-build builds at its caller's
\ tier, and only a tier 1 product compiles through file objects.
\ The tool library loads first, at the default tier, so only the product
\ compiles at tier 1.
: BUILD ( -- n )
   PROC-ARGV-ENV-RESET
   s" HABU_BUILD_CACHE" ENV-EMPTY
   s" XDG_CACHE_HOME" >LEN s" xdg" AT >LEN PROC-ENV+
   s" HOME" ENV-EMPTY
   s" TMPDIR" ENV-EMPTY
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE-CANDIDATE:PATH$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --load" ARG s" tools/hb-build-lib.f" ARG s" test/compiler/aot-mode.f" ARG
   s" tools/hb-build.f" ARG s" --" ARG
   s" --repl" ARG s" app.f" AT ARG s" -o" ARG s" app-hb" AT ARG
   ENGINE-CANDIDATE:PATH$ s" " CAPTURE ;

\ The XDG root holds at least one file object.
: FILE-CACHED? ( -- bool )
   s" xdg/habu-build" AT NAMES NAMES-CAP FS-LIST:NAMES {: u:n :}
   NAMES u s" hb-file-" CONTAINS? ;

public
: MAIN ( -- )
   T-RESET
   s" build children share the parent's cache root" T-LABEL
   s" native-file-root" TREE
   s" leaf.f" S\" package FILE-ROOT-LEAF\npublic\n: VALUE ( -- n ) 42 ;\n;package\n" PUT
   s" app.f" S\" require leaf.f\n: MAIN ( -- ) FILE-ROOT-LEAF:VALUE . ;\n" PUT
   BUILD SUCCESS
   RUN-APP SUCCESS
   OUT$ S\" 42\n" T$=
   FILE-CACHED? TTRUE
   T-REPORT
   s" native file root artifacts: " type ROOT$ type cr ;
;package
;using

NATIVE-FILE-ROOT-TEST:MAIN
