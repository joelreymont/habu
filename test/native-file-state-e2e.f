\ DATA, prior writes, typed cells, literals, reopened packages, created words,
\ TASK slots, task chains and exit hooks, and foreign functions survive cold
\ compilation and warm definition reuse in small applications.
\ bin/hb --load test/native-file-state-e2e.f

require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-env.f
require tools/image-size-lib.f
require test/native-file-app-lib.f
require test/native-file-e2e.f

using NATIVE-FILE-E2E
package NATIVE-FILE-STATE-TEST

$8000 constant CAP
create EXPORT-PATH FS-PATH-CAP allot
create IMPORT-PATH FS-PATH-CAP allot
DYNAMIC-BUFFER CUT u8  variable CUT-U
DYNAMIC-BUFFER IMAGE-A u8
DYNAMIC-BUFFER IMAGE-B u8
variable EXPORT-U
variable IMPORT-U
variable CODE-A-O  variable CODE-A-U
variable CODE-B-O  variable CODE-B-U

: BUILD ( ptr u8 n -- n )
   {: product:ptr productu:n :}
   ROOT$ s" app.f" product productu NATIVE-FILE-APP:BUILD-AT
   NATIVE-FILE-APP:OUT$ NATIVE-FILE-APP:ERR$ STREAMS! ;

: APP-SOURCE ( -- )
   s" app.f"
   S\" require prefix.f\nrequire unit.f\nrequire downstream.f\n: MAIN ( -- ) FILE-STATE-CLIENT:VALUE . FILE-STATE-CLIENT:LONG . FILE-STATE-CLIENT:DEPENDENCY . FILE-STATE-A:LINK@ c@ . FILE-STATE-A:LETTERS$ drop c@ . FILE-STATE-A:EMPTY$ nip . FILE-STATE-A:WORDS$ dup . drop c@ . FILE-STATE-A:BEFORE-SPILL$ dup . drop c@ . FILE-STATE-A:AFTER-SPILL$ dup . drop c@ . 35 FILE-STATE-A:QUOTE@ execute . ;\n" PUT ;

: PREPARE ( -- )
   s" cache" AT MAKE-DIR
   s" test/fixtures/native-file-state-prefix.f" s" prefix.f" AT 1
      NATIVE-FILE-APP:COPY-SKIP-LINES
   s" test/fixtures/native-file-state-unit.f" s" unit.f" AT 1
      NATIVE-FILE-APP:COPY-SKIP-LINES
   s" unit.f" AT S\" s\" state-unit-source-ran\" type cr\n" APPEND-FILE
   s" test/fixtures/native-file-state-downstream.f" s" downstream.f" AT 1
      NATIVE-FILE-APP:COPY-SKIP-LINES
   s" test/fixtures/native-file-state-rejected.f" s" bad.f" AT 0
      NATIVE-FILE-APP:COPY-SKIP-LINES
   APP-SOURCE ;

: FRESH-DOWNSTREAM ( -- )
   s" test/fixtures/native-file-state-downstream.f" s" fresh.f" AT 1
      NATIVE-FILE-APP:COPY-SKIP-LINES
   s" app.f" S\" require prefix.f\nrequire unit.f\nrequire fresh.f\n: MAIN ( -- ) FILE-STATE-CLIENT:VALUE . FILE-STATE-CLIENT:LONG . FILE-STATE-CLIENT:DEPENDENCY . FILE-STATE-A:LINK@ c@ . FILE-STATE-A:LETTERS$ drop c@ . FILE-STATE-A:EMPTY$ nip . FILE-STATE-A:WORDS$ dup . drop c@ . FILE-STATE-A:BEFORE-SPILL$ dup . drop c@ . FILE-STATE-A:AFTER-SPILL$ dup . drop c@ . 35 FILE-STATE-A:QUOTE@ execute . ;\n" PUT ;

: RUN-STATE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   path pathu s" " CAPTURE SUCCESS
   OUT$ S\" 101\n35\n14\n66\n65\n0\n11\n102\n12\n98\n11\n97\n42\n" CONTAINS? TTRUE ;

: REJECT-TYPED-CLIENT ( -- )
   s" app.f" S\" require prefix.f\nrequire unit.f\nrequire bad.f\n: MAIN ( -- ) ;\n" PUT
   s" bad-hb" BUILD 70 T=
   \ The ordinary loader runs the earlier unit action before rejecting bad.f.
   OUT$ s" state-unit-source-ran" CONTAINS? TTRUE
   ERR$ s" expected: ptr a actual: ptr u8" CONTAINS? TTRUE ;

: TASK-FILES ( -- )
   s" cache" AT MAKE-DIR
   s" prefix.f"
   S\" require src/lib/task.f\npackage FILE-TASK-STATE-PREFIX\npublic\nTASK:#USER CELL TASK:+USER PREFIX-SLOT drop\n;package\n" PUT
   s" unit.f"
   S\" require prefix.f\npackage FILE-TASK-STATE-UNIT\npublic\nTASK:#USER CELL TASK:+USER LEAF drop\n: LEAF@ ( -- n ) LEAF @ ;\n111 LEAF !\n;package\ns\" task-state-unit-ran\" type cr\n" PUT
   s" downstream.f"
   S\" require unit.f\npackage FILE-TASK-STATE-CLIENT\npublic\nTASK:#USER CELL TASK:+USER OWN drop\n: LEAF@ ( -- n ) FILE-TASK-STATE-UNIT:LEAF@ ;\n: OWN@ ( -- n ) OWN @ ;\n222 OWN !\n;package\n" PUT
   s" app.f"
   S\" require prefix.f\nrequire unit.f\nrequire downstream.f\n: MAIN ( -- ) FILE-TASK-STATE-CLIENT:LEAF@ . FILE-TASK-STATE-CLIENT:OWN@ . ;\n" PUT ;

: RUN-TASK-STATE ( ptr u8 n -- )
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   AT s" " CAPTURE SUCCESS
   OUT$ S\" 111\n222\n" CONTAINS? TTRUE ;

: GROWN-PREFIX ( -- )
   s" prefix.f"
   S\" require src/lib/task.f\npackage FILE-TASK-STATE-PREFIX\npublic\nTASK:#USER CELL TASK:+USER PREFIX-SLOT drop\nTASK:#USER CELL TASK:+USER PREFIX-SLOT-2 drop\n;package\n" PUT
   s" app.f"
   S\" require prefix.f\nrequire unit.f\nrequire downstream.f\n: MAIN ( -- ) -1 FILE-TASK-STATE-PREFIX:PREFIX-SLOT-2 ! FILE-TASK-STATE-CLIENT:LEAF@ . FILE-TASK-STATE-CLIENT:OWN@ . ;\n" PUT ;

: TASK-STATE-CASE ( -- )
   s" a consumer-only edit reuses the leaf TASK reservation" T-LABEL
   s" native-file-task-state" TREE
   TASK-FILES
   s" task-cold-hb" BUILD SUCCESS
   OUT$ S\" definition hits 0\n" CONTAINS? TTRUE
   OUT$ s" task-state-unit-ran" CONTAINS? TTRUE
   s" task-cold-hb" RUN-TASK-STATE
   s" downstream.f" AT S\" \\ task consumer comment\n" APPEND-FILE
   s" task-warm-hb" BUILD SUCCESS
   OUT$ s" definition hits " CONTAINS? TTRUE
   OUT$ S\" definition hits 0\n" CONTAINS? TFALSE
   s" task-warm-hb" RUN-TASK-STATE
   s" a grown prefix reservation moves the cached leaf slot" T-LABEL
   GROWN-PREFIX
   s" task-grown-hb" BUILD SUCCESS
   OUT$ s" definition hits " CONTAINS? TTRUE
   OUT$ S\" definition hits 0\n" CONTAINS? TFALSE
   OUT$ s" task-state-unit-ran" CONTAINS? TTRUE
   s" task-grown-hb" RUN-TASK-STATE ;

: FFI-FILES ( -- )
   s" cache" AT MAKE-DIR
   s" child.f"
   S\" require src/lib/ffi-abi.f\nPROCESS-SYMBOLS\nFUNCTION: BEFORE getpid ( -- i32 ) ;FUNCTION\n" PUT
   s" unit.f"
   S\" require child.f\npackage FILE-FFI\npublic\n: PID-OK ( -- n ) BEFORE 0 > if 1 else 0 then ;\n;package\ns\" ffi-unit-source-ran\" type cr\n" PUT
   s" downstream.f"
   S\" require unit.f\npackage FILE-FFI-CLIENT\npublic\n: RESULT ( -- n ) FILE-FFI:PID-OK BEFORE 0 > if 1 else 0 then + ;\n;package\n" PUT
   s" app.f" S\" require downstream.f\n: MAIN ( -- ) FILE-FFI-CLIENT:RESULT . ;\n" PUT ;

: RUN-FFI ( ptr u8 n -- )
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   AT s" " CAPTURE SUCCESS
   OUT$ S\" 2\n" CONTAINS? TTRUE ;

: FFI-CASE ( -- )
   s" cached definitions keep calling foreign functions from required files" T-LABEL
   s" native-file-ffi" TREE
   FFI-FILES
   s" ffi-cold-hb" BUILD SUCCESS
   OUT$ S\" definition hits 0\n" CONTAINS? TTRUE
   OUT$ s" ffi-unit-source-ran" CONTAINS? TTRUE
   s" ffi-cold-hb" RUN-FFI
   s" ffi-warm-hb" BUILD SUCCESS
   OUT$ s" definition hits " CONTAINS? TTRUE
   OUT$ S\" definition hits 0\n" CONTAINS? TFALSE
   OUT$ s" ffi-unit-source-ran" CONTAINS? TTRUE
   s" ffi-warm-hb" RUN-FFI ;

\ A cached task keeps its callback context chain membership and its exit hook.
: HOOKS-FILES ( -- )
   s" cache" AT MAKE-DIR
   s" task.f"
   S\" require src/lib/ffi-callback.f\nrequire src/lib/task.f\npackage FILE-TASK-HOOKS\npublic\n$10000 TASK:TASK CHAINED\nCALLBACK: ENTRY ( -- ) ;CALLBACK\n: CHAIN-RUN ( -- )\n   CHAINED TASK:EXPOSE\n   ENTRY CHAINED TASK:CONTEXT FFI-CB:ENTRY drop\n   CHAINED TASK:UNEXPOSE CHAINED TASK:KILL\n   s\q task chain ok\q type cr ;\n$10000 TASK:TASK HOOKED\n: EXIT-HOOK ( -- ) E-TASK-STATE throw ;\n' EXIT-HOOK HOOKED TASK:AT-EXIT\n: BODY ( -- ) 7 TASK:RETURN ;\n: EXIT-RUN ( -- )\n   ['] BODY HOOKED TASK:ACTIVATE\n   HOOKED TASK:JOIN MATCH result\n      ok OF drop s\q missing task exit hook\q 74 die ENDOF\n      err OF E-TASK-STATE <> if 74 throw then ENDOF\n   ;MATCH\n   s\q task exit ok\q type cr ;\n;package\ns\q task-hooks-unit-ran\q type cr\n" PUT
   s" app.f" S\" require task.f\n: MAIN ( -- ) FILE-TASK-HOOKS:CHAIN-RUN FILE-TASK-HOOKS:EXIT-RUN ;\n" PUT ;

: RUN-HOOKS ( ptr u8 n -- )
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   AT s" " CAPTURE SUCCESS
   OUT$ S\" task chain ok\ntask exit ok\n" CONTAINS? TTRUE ;

: HOOKS-CASE ( -- )
   s" cached task declarations keep their callback chain and exit hooks" T-LABEL
   s" native-file-task-hooks" TREE
   HOOKS-FILES
   s" hooks-cold-hb" BUILD SUCCESS
   OUT$ S\" definition hits 0\n" CONTAINS? TTRUE
   OUT$ s" task-hooks-unit-ran" CONTAINS? TTRUE
   s" hooks-cold-hb" RUN-HOOKS
   s" hooks-warm-hb" BUILD SUCCESS
   OUT$ s" definition hits " CONTAINS? TTRUE
   OUT$ S\" definition hits 0\n" CONTAINS? TFALSE
   OUT$ s" task-hooks-unit-ran" CONTAINS? TTRUE
   s" hooks-warm-hb" RUN-HOOKS ;

: PAIR-PATHS ( -- )
   s" cold-hb" AT {: a:ptr u:n :}
   a EXPORT-PATH u BYTE-COPY
   u EXPORT-U !
   s" warm-hb" AT {: b:ptr v:n :}
   b IMPORT-PATH v BYTE-COPY
   v IMPORT-U ! ;

: SAME-CODE? ( -- bool )
   EXPORT-PATH EXPORT-U @ IMAGE-SIZE:MEASURE
   IMAGE-SIZE:CODE-BLOB-RANGE CODE-A-U ! CODE-A-O !
   IMPORT-PATH IMPORT-U @ IMAGE-SIZE:MEASURE
   IMAGE-SIZE:CODE-BLOB-RANGE CODE-B-U ! CODE-B-O !
   CODE-A-U @ CODE-B-U @ <> if false exit then
   EXPORT-PATH EXPORT-U @ FILE-SIZE {: asize:n :}
   IMPORT-PATH IMPORT-U @ FILE-SIZE {: bsize:n :}
   asize IMAGE-A-RESERVE
   bsize IMAGE-B-RESERVE
   EXPORT-PATH EXPORT-U @ 0 IMAGE-A BYTE-VIEW asize READ-ALL asize <> if E-FS-IO throw then
   IMPORT-PATH IMPORT-U @ 0 IMAGE-B BYTE-VIEW bsize READ-ALL bsize <> if E-FS-IO throw then
   0 IMAGE-A BYTE-VIEW CODE-A-O @ BYTE+ CODE-A-U @
   0 IMAGE-B BYTE-VIEW CODE-B-O @ BYTE+ CODE-B-U @ STR= ;

\ The first child's output, grown to its length; an empty one has no byte.
: CUT! ( ptr u8 n -- )
   {: a:ptr u:n :}
   u CUT-U ! u 0= if exit then
   u CUT-RESERVE a 0 CUT u BYTE-COPY ;
: CUT$ ( -- ptr u8 n ) CUT-U @ 0= if s" " exit then 0 CUT CUT-U @ ;
: SAME-CUT? ( -- bool )
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   EXPORT-PATH EXPORT-U @ s" cp@ dbase@ - . ndict@ . wordlist . here data-base - ."
      CAPTURE SUCCESS
   OUT$ CUT!
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   IMPORT-PATH IMPORT-U @ s" cp@ dbase@ - . ndict@ . wordlist . here data-base - ."
      CAPTURE SUCCESS
   CUT$ OUT$ STR= ;

public
: MAIN ( -- )
   T-RESET
   s" cold and warm definition reuse preserves state" T-LABEL
   s" native-file-state" TREE
   PREPARE
   s" cold-hb" BUILD SUCCESS
   OUT$ S\" definition hits 0\n" CONTAINS? TTRUE
   OUT$ s" state-unit-source-ran" CONTAINS? TTRUE
   s" warm-hb" BUILD SUCCESS
   OUT$ s" definition hits " CONTAINS? TTRUE
   OUT$ S\" definition hits 0\n" CONTAINS? TFALSE
   OUT$ s" state-unit-source-ran" CONTAINS? TTRUE
   PAIR-PATHS
   SAME-CODE? TTRUE
   SAME-CUT? TTRUE
   EXPORT-PATH EXPORT-U @ RUN-STATE
   IMPORT-PATH IMPORT-U @ RUN-STATE
   FRESH-DOWNSTREAM
   s" fresh-hb" BUILD SUCCESS
   OUT$ s" state-unit-source-ran" CONTAINS? TTRUE
   s" fresh-hb" AT RUN-STATE
   REJECT-TYPED-CLIENT
   TASK-STATE-CASE
   FFI-CASE
   HOOKS-CASE
   T-REPORT
   s" native file unit artifacts: " type ROOT$ type cr ;
;package
;using

NATIVE-FILE-STATE-TEST:MAIN
