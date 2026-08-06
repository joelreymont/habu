\ gate-runner-lib.f - reusable dispatch for native test runner phases.
\
\ Loaded after gate support libraries. This file defines phase dispatch only;
\ entry files decide when to run it.

require lib/adt/option.f                 \ option<n> STR>NUMBER? consumer (switchover wave A)

package GATE-RUNNER

using GATE

64 constant USAGE-RC
-1 constant ID-UNKNOWN
0 constant ID-TOOL
1 constant ID-TOOL-REPAIR
2 constant ID-TOOL-REPAIR-CHECK
3 constant ID-TOOL-REPAIR-PACKET
4 constant ID-TOOL-DOC
5 constant ID-TOOL-DOC-PUBLIC
7 constant ID-TOOL-DOC-SCHEMA
8 constant ID-TOOL-LINTS
9 constant ID-TOOL-LINT-REPL
10 constant ID-TOOL-LINT-AOT
11 constant ID-TOOL-LINT-NAMES
12 constant ID-TOOL-LINT-BUNDLE
13 constant ID-TOOL-TYPED
14 constant ID-TOOL-SEMANTICS
15 constant ID-CHECK-CLI
16 constant ID-TAIL
17 constant ID-LINT-TOOLS
18 constant ID-LINT-ARTIFACTS
19 constant ID-LINT-LIBS
20 constant ID-REPAIR
21 constant ID-FIXTURES
22 constant ID-RUNTIME
23 constant ID-VALIDATE
24 constant ID-DIAG-REPAIR
25 constant ID-DIAG-UNDEF-PRIMARY
26 constant ID-DIAG-ALL-STRICT
27 constant ID-DIAG-FILE-UNSAFE
28 constant ID-DICTIONARY
29 constant ID-DEBUG
30 constant ID-TAIL-FAST
31 constant ID-TAIL-PURE
32 constant ID-TAIL-RUNNER
33 constant ID-TAIL-BUILD
35 constant ID-LINT-LIBS-CORE
36 constant ID-LINT-LIBS-PTX
37 constant ID-LINT-LIBS-PTX-NEG
38 constant ID-LINT-LIBS-PTX-TOOL
39 constant ID-LINT-ARTIFACTS-FAST
40 constant ID-TAIL-PROCESS
41 constant ID-DIAG-LABEL-COPY
variable START-NS
variable ARG-I

: USAGE ( -- )
   s" usage: bin/hb --load test/gate-runner-support.f test/gate-runner-entry.f -- PHASE [--pool-slots N] [--timings]" USAGE-RC die ;

: ARG0= ( ptr u8 n -- bool )
   0 SCRIPT-ARGV$ STR= ;

: TOKEN$ ( -- ptr u8 n )
   0 SCRIPT-ARGV$ ;

: TAB. ( -- )
   STR-TAB emit ;

: ARG$ ( -- ptr u8 n )
   ARG-I @ SCRIPT-ARGV$ ;

: ARG-VALUE$ ( -- ptr u8 n )
   ARG-I @ 1+ SCRIPT-ARGC >= if USAGE then
   ARG-I @ 1+ SCRIPT-ARGV$ ;

: POS-NUM ( ptr u8 n -- n )
   STR>NUMBER? MATCH option
     none OF USAGE ENDOF
     some OF ENDOF
   ;MATCH
   dup 1 < if drop USAGE then ;

: ADVANCE ( n -- )
   ARG-I @ + ARG-I ! ;

: POOL-OPT ( -- )
   ARG-VALUE$ POS-NUM GT-POOL-SLOTS!
   2 ADVANCE ;

: TIMINGS-OPT ( -- )
   GSI-TIMINGS!
   1 ADVANCE ;

: PARSE-ARG ( -- )
   ARG$ s" --pool-slots" STR= if POOL-OPT exit then
   ARG$ s" --timings" STR= if TIMINGS-OPT exit then
   USAGE ;

: CHECK-ARGS ( -- )
   SCRIPT-ARGC 1 < if USAGE then
   1 ARG-I !
   begin ARG-I @ SCRIPT-ARGC < while
      PARSE-ARG
   repeat ;

: TOKEN-ID ( -- n )
   s" tool" ARG0= if ID-TOOL exit then
   s" tool-repair" ARG0= if ID-TOOL-REPAIR exit then
   s" tool-repair-check" ARG0= if ID-TOOL-REPAIR-CHECK exit then
   s" tool-repair-packet" ARG0= if ID-TOOL-REPAIR-PACKET exit then
   s" tool-doc" ARG0= if ID-TOOL-DOC exit then
   s" tool-doc-public" ARG0= if ID-TOOL-DOC-PUBLIC exit then
   s" tool-doc-schema" ARG0= if ID-TOOL-DOC-SCHEMA exit then
   s" tool-lints" ARG0= if ID-TOOL-LINTS exit then
   s" tool-lint-repl" ARG0= if ID-TOOL-LINT-REPL exit then
   s" tool-lint-aot" ARG0= if ID-TOOL-LINT-AOT exit then
   s" tool-lint-names" ARG0= if ID-TOOL-LINT-NAMES exit then
   s" tool-lint-bundle" ARG0= if ID-TOOL-LINT-BUNDLE exit then
   s" tool-typed" ARG0= if ID-TOOL-TYPED exit then
   s" tool-semantics" ARG0= if ID-TOOL-SEMANTICS exit then
   s" check-cli" ARG0= if ID-CHECK-CLI exit then
   s" tail" ARG0= if ID-TAIL exit then
   s" lint-tools" ARG0= if ID-LINT-TOOLS exit then
   s" lint-artifacts" ARG0= if ID-LINT-ARTIFACTS exit then
   s" lint-libs" ARG0= if ID-LINT-LIBS exit then
   s" repair" ARG0= if ID-REPAIR exit then
   s" fixtures" ARG0= if ID-FIXTURES exit then
   s" runtime" ARG0= if ID-RUNTIME exit then
   s" validate" ARG0= if ID-VALIDATE exit then
   s" diag-repair" ARG0= if ID-DIAG-REPAIR exit then
   s" diag-undef-primary" ARG0= if ID-DIAG-UNDEF-PRIMARY exit then
   s" diag-all-strict" ARG0= if ID-DIAG-ALL-STRICT exit then
   s" diag-file-unsafe" ARG0= if ID-DIAG-FILE-UNSAFE exit then
   s" diag-label-copy" ARG0= if ID-DIAG-LABEL-COPY exit then
   s" dictionary" ARG0= if ID-DICTIONARY exit then
   s" debug" ARG0= if ID-DEBUG exit then
   s" tail-fast" ARG0= if ID-TAIL-FAST exit then
   s" tail-pure" ARG0= if ID-TAIL-PURE exit then
   s" tail-runner" ARG0= if ID-TAIL-RUNNER exit then
   s" tail-build" ARG0= if ID-TAIL-BUILD exit then
   s" lint-libs-core" ARG0= if ID-LINT-LIBS-CORE exit then
   s" lint-libs-ptx" ARG0= if ID-LINT-LIBS-PTX exit then
   s" lint-libs-ptx-neg" ARG0= if ID-LINT-LIBS-PTX-NEG exit then
   s" lint-libs-ptx-tool" ARG0= if ID-LINT-LIBS-PTX-TOOL exit then
   s" lint-artifacts-fast" ARG0= if ID-LINT-ARTIFACTS-FAST exit then
   s" tail-process" ARG0= if ID-TAIL-PROCESS exit then
   ID-UNKNOWN ;

: STDLIB ( -- )
   s" test/gate-stdlib-cases.f" included ;

: TOOL ( -- )
   STDLIB-GATE:SKIP-SEMANTIC!
   STDLIB ;

: DISPATCH-ID ( n -- )
   case
      ID-TOOL of AOT-CALL-GATE:RUN endof
      ID-TOOL-REPAIR of GSI-TOOL-REPAIR endof
      ID-TOOL-REPAIR-CHECK of GSI-TOOL-REPAIR-CHECK endof
      ID-TOOL-REPAIR-PACKET of GSI-TOOL-REPAIR-PACKET endof
      ID-TOOL-DOC of TOOL-SEMANTICS:DOC endof
      ID-TOOL-DOC-PUBLIC of GSI-TOOL-DOC-PUBLIC endof
      ID-TOOL-DOC-SCHEMA of GSI-TOOL-DOC-SCHEMA endof
      ID-TOOL-LINTS of GSI-TOOL-LINT-PHASE endof
      ID-TOOL-LINT-REPL of GSI-TOOL-LINT-REPL-PHASE endof
      ID-TOOL-LINT-AOT of GSI-TOOL-LINT-AOT endof
      ID-TOOL-LINT-NAMES of GSI-TOOL-LINT-NAMES endof
      ID-TOOL-LINT-BUNDLE of GSI-TOOL-LINT-BUNDLE endof
      ID-TOOL-TYPED of GSI-TOOL-TYPED endof
      ID-TOOL-SEMANTICS of TOOL-SEMANTICS:RUN endof
      ID-CHECK-CLI of CHECK-CLI-GATE:RUN endof
      ID-TAIL of STDLIB endof
      ID-LINT-TOOLS of GSI-LINT-TOOLS endof
      ID-LINT-ARTIFACTS of STDLIB endof
      ID-LINT-LIBS of STDLIB endof
      ID-LINT-LIBS-CORE of STDLIB-INLINE:GSI-LINT-LIBS-CORE endof
      ID-LINT-LIBS-PTX of STDLIB-INLINE:GSI-LINT-LIBS-PTX endof
      ID-LINT-LIBS-PTX-NEG of STDLIB-INLINE:GSI-LINT-LIBS-PTX-NEG endof
      ID-LINT-LIBS-PTX-TOOL of STDLIB-INLINE:GSI-LINT-LIBS-PTX-TOOL endof
      ID-LINT-ARTIFACTS-FAST of STDLIB-INLINE:GSI-LINT-ARTIFACTS-FAST endof
      ID-REPAIR of GENG-REPAIR-SLICE endof
      ID-FIXTURES of GENG-FIXTURES-SLICE endof
      ID-RUNTIME of GENG-RUNTIME-SLICE endof
      ID-VALIDATE of ENGINE-GATE:VALIDATE-SLICE endof
      ID-DIAG-REPAIR of GATE-DIAGNOSTICS:REPAIR endof
      ID-DIAG-UNDEF-PRIMARY of GATE-DIAGNOSTICS:UNDEFINED-PRIMARY endof
      ID-DIAG-ALL-STRICT of GATE-DIAGNOSTICS:ALL-STRICT endof
      ID-DIAG-FILE-UNSAFE of GATE-DIAGNOSTICS:FILE-UNSAFE endof
      ID-DIAG-LABEL-COPY of GATE-DIAGNOSTICS:LABEL-COPY-SLICE endof
      ID-DICTIONARY of GATE-DICTIONARY:RUN endof
      ID-DEBUG of GDB-RUN endof
      ID-TAIL-FAST of GSI-TAIL-FAST endof
      ID-TAIL-PURE of TAIL-PURE:RUN endof
      ID-TAIL-RUNNER of GSI-TAIL-RUNNER endof
      ID-TAIL-BUILD of GSI-TAIL-BUILD endof
      ID-TAIL-PROCESS of TAIL-PROCESS:RUN endof
      USAGE
   endcase ;

: DISPATCH ( -- )
   TOKEN-ID DISPATCH-ID ;

: RUN-ID ( n -- )
   mono-ns START-NS !
   DISPATCH-ID ;

: ELAPSED-MS ( -- n )
   mono-ns START-NS @ - PROC-NS-PER-MS / ;

: SUBJECT-ID$ ( n -- ptr u8 n )
   case
      ID-CHECK-CLI of s" candidate-cli" endof
      ID-RUNTIME of s" candidate-cli" endof
      ID-DICTIONARY of s" candidate-source" endof
      ID-VALIDATE of s" candidate-source" endof
      s" host-source" rot
   endcase ;

: SUBJECT$ ( -- ptr u8 n )
   TOKEN-ID SUBJECT-ID$ ;

: RUNNER$ ( -- ptr u8 n )
   s" gate-runner" ;

: SEMANTIC-TOOL-ID? ( n -- bool )
   case
      ID-TOOL-REPAIR of 0 0= endof
      ID-TOOL-REPAIR-CHECK of 0 0= endof
      ID-TOOL-REPAIR-PACKET of 0 0= endof
      ID-TOOL-DOC of 0 0= endof
      ID-TOOL-DOC-PUBLIC of 0 0= endof
      ID-TOOL-DOC-SCHEMA of 0 0= endof
      ID-TOOL-LINTS of 0 0= endof
      ID-TOOL-LINT-REPL of 0 0= endof
      ID-TOOL-LINT-AOT of 0 0= endof
      ID-TOOL-LINT-NAMES of 0 0= endof
      ID-TOOL-LINT-BUNDLE of 0 0= endof
      ID-TOOL-TYPED of 0 0= endof
      ID-TOOL-SEMANTICS of 0 0= endof
      ID-CHECK-CLI of 0 0= endof
      ID-LINT-TOOLS of 0 0= endof
      ID-LINT-LIBS-CORE of 0 0= endof
      ID-LINT-LIBS-PTX of 0 0= endof
      ID-LINT-LIBS-PTX-NEG of 0 0= endof
      ID-LINT-LIBS-PTX-TOOL of 0 0= endof
      ID-LINT-ARTIFACTS-FAST of 0 0= endof
      ID-TAIL-FAST of 0 0= endof
      ID-TAIL-PURE of 0 0= endof
      ID-TAIL-RUNNER of 0 0= endof
      ID-TAIL-BUILD of 0 0= endof
      ID-TAIL-PROCESS of 0 0= endof
      0 0= 0= swap
   endcase ;

: SEMANTIC-TOOL? ( -- bool )
   TOKEN-ID SEMANTIC-TOOL-ID? ;

: BOUNDARY$ ( -- ptr u8 n )
   SEMANTIC-TOOL? if s" inprocess" exit then
   s" process" ;

: SHA$ ( -- ptr u8 n )
   s" -" ;

: STATS ( -- )
   TOKEN$ SUBJECT$ RUNNER$ BOUNDARY$ SHA$ GS-TEST
   TOKEN$ ELAPSED-MS GS-SPAN ;

: PASS ( -- )
   STATS
   s" PASS: " type
   TOKEN$ type
   s"  (" type
   ELAPSED-MS GT-U-TYPE
   s" ms)" type cr ;

public

: RUN ( -- )
   mono-ns START-NS !
   CHECK-ARGS
   DISPATCH
   PASS ;

;package
