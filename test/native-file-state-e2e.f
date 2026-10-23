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
create FRESH-CACHE FS-PATH-CAP allot
create FRESH-SOURCE FS-PATH-CAP allot
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
   OUT$ S\" files reused 0\n" CONTAINS? TTRUE
   OUT$ s" task-state-unit-ran" CONTAINS? TTRUE
   s" task-cold-hb" RUN-TASK-STATE
   s" downstream.f" AT S\" \\ task consumer comment\n" APPEND-FILE
   s" task-warm-hb" BUILD SUCCESS
   OUT$ s" files reused " CONTAINS? TTRUE
   OUT$ S\" files reused 0\n" CONTAINS? TFALSE
   s" task-warm-hb" RUN-TASK-STATE
   s" a grown prefix reservation moves the cached leaf slot" T-LABEL
   GROWN-PREFIX
   s" task-grown-hb" BUILD SUCCESS
   OUT$ s" files reused " CONTAINS? TTRUE
   OUT$ S\" files reused 0\n" CONTAINS? TFALSE
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
   OUT$ S\" files reused 0\n" CONTAINS? TTRUE
   OUT$ s" ffi-unit-source-ran" CONTAINS? TTRUE
   s" ffi-cold-hb" RUN-FFI
   s" ffi-warm-hb" BUILD SUCCESS
   OUT$ s" files reused " CONTAINS? TTRUE
   OUT$ S\" files reused 0\n" CONTAINS? TFALSE
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
   OUT$ S\" files reused 0\n" CONTAINS? TTRUE
   OUT$ s" task-hooks-unit-ran" CONTAINS? TTRUE
   s" hooks-cold-hb" RUN-HOOKS
   s" hooks-warm-hb" BUILD SUCCESS
   OUT$ s" files reused " CONTAINS? TTRUE
   OUT$ S\" files reused 0\n" CONTAINS? TFALSE
   OUT$ s" task-hooks-unit-ran" CONTAINS? TTRUE
   s" hooks-warm-hb" RUN-HOOKS ;

\ A warm hit on a file holding a `does>` definer installs the definer and its
\ clause record as the cold compile published them. use.f calls the trusted
\ clause by name and app.f makes a word with the definer.
: DOES-FILES ( -- )
   s" cache" AT MAKE-DIR
   s" does.f"
   S\" package FILE-DOES-STATE\npublic\n: MAKE ( n -- ) create , does> ( -- n ) @ ;\ns\" MAKE;does\" s\" ptr n -- n\" trust\n;package\n" PUT
   s" use.f"
   S\" require does.f\npackage FILE-DOES-USE\npublic\ncreate SEVEN 7 ,\n: SEVEN@ ( -- n ) SEVEN FILE-DOES-STATE:MAKE;does ;\n;package\n" PUT
   s" app.f" S\" require does.f\nrequire use.f\n5 FILE-DOES-STATE:MAKE FIVE\n: MAIN ( -- ) FIVE . FILE-DOES-USE:SEVEN@ . ;\n" PUT ;

\ The definer's prelude grows, so its clause starts later in its body.
: DOES-MOVED ( -- )
   s" does.f"
   S\" package FILE-DOES-STATE\npublic\n: MAKE ( n -- ) dup 1 + create , drop does> ( -- n ) @ ;\ns\" MAKE;does\" s\" ptr n -- n\" trust\n;package\n" PUT ;

\ The product built with code prints want; a failed build left no product to run.
: RUN-DOES ( n ptr u8 n ptr u8 n -- )
   {: code:n name:ptr nameu:n want:ptr wantu:n :}
   code 0<> if exit then
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   name nameu AT s" " CAPTURE SUCCESS
   OUT$ want wantu CONTAINS? TTRUE ;

\ The comparisons read the product at EXPORT-PATH against the one at IMPORT-PATH.
: PATH-A! ( ptr u8 n -- )
   {: a:ptr u:n :}
   a EXPORT-PATH u BYTE-COPY u EXPORT-U ! ;
: PATH-B! ( ptr u8 n -- )
   {: b:ptr v:n :}
   b IMPORT-PATH v BYTE-COPY v IMPORT-U ! ;
: PAIR-PATHS ( -- ) s" cold-hb" AT PATH-A! s" warm-hb" AT PATH-B! ;

\ Both products' whole files, read into IMAGE-A and IMAGE-B; leaves their sizes.
: READ-PAIR ( -- n n )
   EXPORT-PATH EXPORT-U @ FILE-SIZE {: asize:n :}
   IMPORT-PATH IMPORT-U @ FILE-SIZE {: bsize:n :}
   asize IMAGE-A-RESERVE
   bsize IMAGE-B-RESERVE
   EXPORT-PATH EXPORT-U @ 0 IMAGE-A BYTE-VIEW asize READ-ALL asize <> if E-FS-IO throw then
   IMPORT-PATH IMPORT-U @ 0 IMAGE-B BYTE-VIEW bsize READ-ALL bsize <> if E-FS-IO throw then
   asize bsize ;

: SAME-CODE? ( -- bool )
   EXPORT-PATH EXPORT-U @ IMAGE-SIZE:MEASURE
   IMAGE-SIZE:CODE-BLOB-RANGE CODE-A-U ! CODE-A-O !
   IMPORT-PATH IMPORT-U @ IMAGE-SIZE:MEASURE
   IMAGE-SIZE:CODE-BLOB-RANGE CODE-B-U ! CODE-B-O !
   CODE-A-U @ CODE-B-U @ <> if false exit then
   READ-PAIR 2drop
   0 IMAGE-A BYTE-VIEW CODE-A-O @ BYTE+ CODE-A-U @
   0 IMAGE-B BYTE-VIEW CODE-B-O @ BYTE+ CODE-B-U @ STR= ;

\ The two products' whole files are equal.
: SAME-IMAGE? ( -- bool )
   READ-PAIR {: asize:n bsize:n :}
   0 IMAGE-A BYTE-VIEW asize 0 IMAGE-B BYTE-VIEW bsize STR= ;

\ The first child's output, grown to its length; an empty one has no byte.
: CUT! ( ptr u8 n -- )
   {: a:ptr u:n :}
   u CUT-U ! u 0= if exit then
   u CUT-RESERVE a 0 CUT u BYTE-COPY ;
: CUT$ ( -- ptr u8 n ) CUT-U @ 0= if s" " exit then 0 CUT CUT-U @ ;

\ root/product built from app.f with the empty cache root/cache of its own: it
\ is the product a warm build of the same sources must equal.
create COLD-CACHE FS-PATH-CAP allot  variable COLD-CACHE-U
create COLD-SRC FS-PATH-CAP allot  variable COLD-SRC-U
: COLD-BUILD ( ptr u8 n ptr u8 n -- n )
   {: product:ptr productu:n cache:ptr cacheu:n :}
   cache cacheu AT {: c:ptr cu:n :}
   c cu MAKE-DIR
   c COLD-CACHE cu BYTE-COPY cu COLD-CACHE-U !
   s" app.f" AT {: a:ptr au:n :}
   a COLD-SRC au BYTE-COPY au COLD-SRC-U !
   COLD-SRC COLD-SRC-U @ product productu AT COLD-CACHE COLD-CACHE-U @
      NATIVE-FILE-APP:BUILD
   NATIVE-FILE-APP:OUT$ NATIVE-FILE-APP:ERR$ STREAMS! ;

: DOES-CASE ( -- )
   s" a warm hit installs a does> definer byte-equal to cold" T-LABEL
   s" native-file-does" TREE
   DOES-FILES
   s" cold-hb" BUILD dup SUCCESS {: cold:n :}
   OUT$ S\" files reused 0\n" CONTAINS? TTRUE
   cold s" cold-hb" S\" 5\n7\n" RUN-DOES
   true NATIVE-FILE-APP:DIAG? !
   s" warm-hb" BUILD dup SUCCESS {: warm:n :}
   false NATIVE-FILE-APP:DIAG? !
   ERR$ s" /does.f records=" CONTAINS? TTRUE
   warm s" warm-hb" S\" 5\n7\n" RUN-DOES
   cold warm or 0= if PAIR-PATHS SAME-IMAGE? TTRUE then
   \ The definer's clause starts later. The cold reference is built in the
   \ same tree, with a fresh cache, because a product records its source paths.
   s" an edited does> definer misses, and so does the file that executed it, giving cold's product and output" T-LABEL
   DOES-MOVED
   s" moved-hb" BUILD dup SUCCESS {: moved:n :}
   moved s" moved-hb" S\" 6\n7\n" RUN-DOES
   s" moved-cold-hb" s" moved-cache" COLD-BUILD dup SUCCESS {: ref:n :}
   OUT$ S\" files reused 0\n" CONTAINS? TTRUE
   ref s" moved-cold-hb" S\" 6\n7\n" RUN-DOES
   ref moved or 0<> if exit then
   s" moved-cold-hb" AT PATH-A!
   s" moved-hb" AT PATH-B!
   SAME-IMAGE? TTRUE ;

: SAME-CUT? ( -- bool )
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   EXPORT-PATH EXPORT-U @ s" cp@ dbase@ - . ndict@ . wordlist . here data-base - ."
      CAPTURE SUCCESS
   OUT$ CUT!
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   IMPORT-PATH IMPORT-U @ s" cp@ dbase@ - . ndict@ . wordlist . here data-base - ."
      CAPTURE SUCCESS
   CUT$ OUT$ STR= ;

\ Records with no compiled body of their own: lib.f declares casts, redef.f
\ retires a word before its redefinition. app.f calls the public cast and the
\ live word. The private wide cast, between two words that take nothing, is
\ marked DNAME-WIDE; it and the public cast carry DNAME-MIN-IN 1.
: BARE-FILES ( -- )
   s" cache" AT MAKE-DIR
   s" lib.f"
   S\" package FILE-BARE\n: BEFORE ( -- ) ;\nCAST: WIDE ( [ SPAN:span<u8> -- ] -- [ SPAN:span<u8> -- ] )\n: AFTER ( -- ) ;\npublic\nCAST: BARE>IDX ( n -- idx )\n;package\n" PUT
   s" redef.f"
   S\" package FILE-REDEF\npublic\n: TWICE ( -- n ) 1 ;\nundefine TWICE\n: TWICE ( -- n ) 2 ;\n;package\n" PUT
   s" app.f"
   S\" require lib.f\nrequire redef.f\n: MAIN ( -- ) 40 FILE-BARE:BARE>IDX IDX>N FILE-REDEF:TWICE + . ;\n" PUT ;

: RUN-BARE ( ptr u8 n -- )
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   AT s" " CAPTURE SUCCESS
   OUT$ S\" 42\n" CONTAINS? TTRUE ;

\ BUILD with the empty cache directory cache of its own, the cold build to
\ compare against.
: FRESH-BUILD ( ptr u8 n -- n )
   AT {: c:ptr cu:n :}
   c FRESH-CACHE cu BYTE-COPY
   FRESH-CACHE cu MAKE-DIR
   s" app.f" AT {: a:ptr au:n :}
   a FRESH-SOURCE au BYTE-COPY
   FRESH-SOURCE au s" fresh-bare-hb" AT FRESH-CACHE cu NATIVE-FILE-APP:BUILD
   NATIVE-FILE-APP:OUT$ NATIVE-FILE-APP:ERR$ STREAMS! ;

\ A consumer passing a byte pointer where the cast takes n.
: BARE-MISUSE ( -- )
   s" app.f"
   S\" require lib.f\nrequire redef.f\n: MAIN ( -- ) s\" x\" drop FILE-BARE:BARE>IDX IDX>N . ;\n" PUT ;

\ Top-level uses of lib.f's words on an empty interpret stack: the neighbours
\ run, so a flag restored onto either refuses it, and the last word refuses
\ through the gate its flag drives. The wide use first casts one cell, so a
\ minimum input restored too large refuses it at BARE>IDX.
: WIDE-USE ( -- )
   s" app.f" S\" require lib.f\npackage FILE-BARE\nBEFORE AFTER 1 BARE>IDX drop WIDE\n;package\n" PUT ;
: MIN-IN-USE ( -- )
   s" app.f" S\" require lib.f\npackage FILE-BARE\nBEFORE AFTER BARE>IDX\n;package\n" PUT ;

\ A line a warm build's cache diagnostics print.
: DIAG-LINE? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   a u s" native-file: " STARTS-WITH?
   a u s" native-cache: " STARTS-WITH? or ;

\ From at, the next line of ERR$ that is no cache diagnostic, the offset
\ after it, and false past the end.
: KEPT-LINE ( n -- ptr u8 n n bool )
   {: at:n :}
   ERR$ 10 at SPLIT-NEXT {: p:ptr pu:n next:n ok:bool :}
   ok if p pu DIAG-LINE? if next RECURSE exit then then
   p pu next ok ;

\ From offsets in ERR$ and CUT$, the kept lines of ERR$ are the lines of CUT$,
\ in order, byte for byte, to both ends.
: SAME-ERR-AT? ( n n -- bool )
   {: wat:n cat:n :}
   wat KEPT-LINE {: w:ptr wu:n wnext:n wok:bool :}
   CUT$ 10 cat SPLIT-NEXT {: c:ptr cu:n cnext:n cok:bool :}
   wok cok and 0= if wok cok or 0= exit then
   w wu c cu STR= 0= if false exit then
   wnext cnext RECURSE ;

\ The warm stderr, its cache diagnostic lines removed, is the cold one in CUT.
: SAME-ERR? ( -- bool ) 0 0 SAME-ERR-AT? ;

\ app.f built cold into cache refuses with line; built warm it reuses lib.f
\ and refuses exactly as the cold build did, beside its cache diagnostics.
: SAME-REFUSAL ( ptr u8 n ptr u8 n -- )
   {: cache:ptr cacheu:n line:ptr lineu:n :}
   cache cacheu FRESH-BUILD 70 T=
   ERR$ line lineu CONTAINS? TTRUE
   ERR$ CUT!
   1 NATIVE-FILE-APP:DIAG? !
   s" bad-warm-hb" BUILD
   0 NATIVE-FILE-APP:DIAG? !
   70 T=
   ERR$ s" lib.f records=" CONTAINS? TTRUE
   SAME-ERR? TTRUE ;

: BARE-CASE ( -- )
   s" a file declaring a cast reuses into the cold state" T-LABEL
   s" native-file-bare" TREE
   BARE-FILES
   s" cold-hb" BUILD SUCCESS
   OUT$ S\" files reused 0\n" CONTAINS? TTRUE
   s" cold-hb" RUN-BARE
   s" app.f" AT S\" \\ unrelated app edit\n" APPEND-FILE
   1 NATIVE-FILE-APP:DIAG? !
   s" warm-hb" BUILD
   0 NATIVE-FILE-APP:DIAG? !
   SUCCESS
   OUT$ S\" files reused 1\n" CONTAINS? TTRUE
   ERR$ s" native-file: reused path=" CONTAINS? TTRUE
   ERR$ s" lib.f records=" CONTAINS? TTRUE
   ERR$ s" redef.f reason=unsupported-retired" CONTAINS? TTRUE
   PAIR-PATHS
   SAME-CODE? TTRUE
   SAME-CUT? TTRUE
   s" warm-hb" RUN-BARE
   s" the reused cast refuses a misuse as the cold one does" T-LABEL
   BARE-MISUSE
   s" misuse-cache" s" at 'FILE-BARE:BARE>IDX' expected: n actual: ptr u8" SAME-REFUSAL
   s" the reused wide cast refuses at the prompt as the cold one does" T-LABEL
   WIDE-USE
   s" wide-cache" S\" hb: interpret-mode layout value: WIDE\n" SAME-REFUSAL
   s" the reused cast refuses underdepth at the prompt as the cold one does" T-LABEL
   MIN-IN-USE
   s" min-in-cache" S\" hb: interpret stack underdepth: BARE>IDX\n" SAME-REFUSAL ;

public
: MAIN ( -- )
   T-RESET
   s" cold and warm definition reuse preserves state" T-LABEL
   s" native-file-state" TREE
   PREPARE
   s" cold-hb" BUILD SUCCESS
   OUT$ S\" files reused 0\n" CONTAINS? TTRUE
   OUT$ s" state-unit-source-ran" CONTAINS? TTRUE
   s" warm-hb" BUILD SUCCESS
   OUT$ s" files reused " CONTAINS? TTRUE
   OUT$ S\" files reused 0\n" CONTAINS? TFALSE
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
   BARE-CASE
   DOES-CASE
   T-REPORT
   s" native file unit artifacts: " type ROOT$ type cr ;
;package
;using

NATIVE-FILE-STATE-TEST:MAIN
