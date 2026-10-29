\ Compiled words can load multiple source files during an ordinary APP-BUILD.
\ bin/hb --load test/native-file-dynamic-load-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs-mutate.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require test/native-file-e2e.f
require test/native-file-app-lib.f

using NATIVE-FILE-E2E
package NATIVE-FILE-DYNAMIC-LOAD-TEST

: RUN-BUILD ( ptr u8 n -- n )
   {: builder:ptr builderu:n :}
   s" HABU_OUT" >LEN s" cache" AT >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN ENGINE-CANDIDATE:PATH$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --" ARG s" app.f" AT ARG s" app-hb" AT ARG
   builder builderu S\" 1 set-tier\nAPP-BUILD:RUN-SAVED-REPORT\n" CAPTURE ;

: BUILD ( -- n )
   NATIVE-FILE-APP:BUILDER$ PROC-ARGV-ENV-RESET RUN-BUILD ;

: ENV-BUILD ( ptr u8 n -- n )
   {: root:ptr rootu:n :}
   NATIVE-FILE-APP:BUILDER$
   PROC-ARGV-ENV-RESET
   s" SHARED_ROOT" >LEN root rootu >LEN PROC-ENV+
   RUN-BUILD ;

: EMPTY-BUILD ( bool -- n )
   {: present:bool :}
   NATIVE-FILE-APP:BUILDER$
   PROC-ARGV-ENV-RESET
   present if
      s" HABU_FILE_EMPTY_INPUT" >LEN s" " >LEN PROC-ENV+
   then
   RUN-BUILD ;

: REUSED ( -- )
   OUT$ s" files reused " CONTAINS? TTRUE
   OUT$ S\" files reused 0\n" CONTAINS? TFALSE ;

: ENVIRONMENT ( -- )
   s" cached definitions preserve environment-selected loads" T-LABEL
   s" native-file-env-load" TREE
   s" cache" AT FS:MAKE-DIR s" a" AT FS:MAKE-DIR s" b" AT FS:MAKE-DIR
   s" a/type.f" S\" package SHARED public NEWTYPE rownum\n: ANSWER ( -- n ) 42 ; ;package\n" PUT
   s" b/type.f" S\" package SHARED public NEWTYPE rownum\n: ANSWER ( -- n ) 99 ; ;package\n" PUT
   s" loader.f" S\" package LOADER\n: LOAD ( -- ) s\" type.f\" required ;\n: RUN ( -- ) s\" SHARED_ROOT\" GETENV [: LOAD ;] SOURCE-ROOT:WITH ;\n' RUN ;package execute\n" PUT
   s" app.f" S\" require loader.f\n: MAIN ( -- ) SHARED:ANSWER . ;\n" PUT
   s" a" AT ENV-BUILD SUCCESS
   RUN-APP SUCCESS OUT$ S\" 42\n" T$=
   \ a/type.f and b/type.f are one instance, type.f under the root loader.f
   \ loads it from, so they share one object key. Under b, that object's input
   \ rows, read against root b, find other bytes: b/type.f compiles from
   \ source and its object replaces a's. app.f's object holds, since MAIN
   \ depends on the declaration of SHARED:ANSWER, which b's ANSWER shares, and
   \ calls the live word. loader.f's load runs from a compiled word, so it
   \ compiles on every build: this build reuses app.f alone, and the next
   \ reuses b/type.f too.
   s" b" AT ENV-BUILD SUCCESS
   OUT$ S\" files reused 1\n" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ S\" 99\n" T$=
   s" b" AT ENV-BUILD SUCCESS
   OUT$ S\" files reused 2\n" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ S\" 99\n" T$=
   s" cached source distinguishes unset and empty environment values" T-LABEL
   s" app.f" S\" : EMPTY-QUERY ( -- ) s\" \" GETENV 2drop ;\nEMPTY-QUERY\n: PRESENT? ( -- bool ) s\" HABU_FILE_EMPTY_INPUT\" GETENV drop NULL-PTR <> ;\nPRESENT? constant PRESENT\n: MAIN ( -- ) PRESENT . ;\n" PUT
   false EMPTY-BUILD SUCCESS
   RUN-APP SUCCESS OUT$ S\" 0\n" T$=
   true EMPTY-BUILD SUCCESS
   RUN-APP SUCCESS OUT$ S\" -1\n" T$= ;

public
: MAIN ( -- )
   T-RESET
   s" compiled required actions run with cached definitions" T-LABEL
   s" native-file-dynamic-load" TREE
   s" cache" AT FS:MAKE-DIR
   s" a.f" S\" package A public : VALUE ( -- n ) 41 ; ;package\n" PUT
   s" b.f" S\" package B public : VALUE ( -- n ) 1 ; ;package\n" PUT
   s" app.f" S\" : LOAD-BOTH ( -- ) s\" a.f\" required s\" b.f\" required ;\nLOAD-BOTH\n: MAIN ( -- ) A:VALUE B:VALUE + . ;\n" PUT
   BUILD SUCCESS
   OUT$ S\" files reused 0\n" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ s" 42" CONTAINS? TTRUE
   BUILD SUCCESS
   REUSED
   RUN-APP SUCCESS OUT$ s" 42" CONTAINS? TTRUE
   s" b.f" S\" package B public : VALUE ( -- n ) 3 ; ;package\n" PUT
   BUILD SUCCESS
   REUSED
   RUN-APP SUCCESS OUT$ s" 44" CONTAINS? TTRUE
   s" bump.f" S\" 1 N +!\n" PUT
   s" app.f" S\" variable N\ninclude bump.f\ninclude bump.f\n: MAIN ( -- ) N @ . ;\n" PUT
   BUILD SUCCESS
   OUT$ S\" files reused 0\n" CONTAINS? TTRUE
   RUN-APP SUCCESS OUT$ s" 2" CONTAINS? TTRUE
   BUILD SUCCESS
   REUSED
   RUN-APP SUCCESS OUT$ s" 2" CONTAINS? TTRUE
   ENVIRONMENT
   T-REPORT ;
;package
;using

NATIVE-FILE-DYNAMIC-LOAD-TEST:MAIN
