\ gate-runner-entry.f - dispatch for the baked native gate runner.
\
\ Loaded by the warm gate image after gate support libraries have already been
\ baked. Keep this file side-effect-only: it selects one phase from SCRIPT-ARGV.

64 constant GR-USAGE-RC
-1 constant GR-ID-UNKNOWN
0 constant GR-ID-TOOL
1 constant GR-ID-TOOL-REPAIR
2 constant GR-ID-TOOL-REPAIR-CHECK
3 constant GR-ID-TOOL-REPAIR-PACKET
4 constant GR-ID-TOOL-DOC
5 constant GR-ID-TOOL-DOC-PUBLIC
6 constant GR-ID-TOOL-DOC-STATUS
7 constant GR-ID-TOOL-DOC-SCHEMA
8 constant GR-ID-TOOL-LINTS
9 constant GR-ID-TOOL-LINT-REPL
10 constant GR-ID-TOOL-LINT-AOT
11 constant GR-ID-TOOL-LINT-NAMES
12 constant GR-ID-TOOL-LINT-BUNDLE
13 constant GR-ID-TOOL-TYPED
14 constant GR-ID-TOOL-SEMANTICS
15 constant GR-ID-CHECK-CLI
16 constant GR-ID-TAIL
17 constant GR-ID-LINT-TOOLS
18 constant GR-ID-LINT-MANIFEST
19 constant GR-ID-LINT-ARTIFACTS
20 constant GR-ID-LINT-LIBS
21 constant GR-ID-REPAIR
22 constant GR-ID-FIXTURES
23 constant GR-ID-RUNTIME
24 constant GR-ID-VALIDATE
25 constant GR-ID-DIAG-REPAIR
26 constant GR-ID-DIAG-UNDEF-PRIMARY
27 constant GR-ID-DIAG-ALL-STRICT
28 constant GR-ID-DIAG-FILE-UNSAFE
29 constant GR-ID-DICTIONARY
30 constant GR-ID-DEBUG
31 constant GR-ID-TAIL-FAST
32 constant GR-ID-TAIL-PURE
33 constant GR-ID-TAIL-RUNNER
34 constant GR-ID-TAIL-BUILD
35 constant GR-ID-TAIL-WARM
36 constant GR-ID-LINT-LIBS-CORE
37 constant GR-ID-LINT-LIBS-PTX
38 constant GR-ID-LINT-LIBS-PTX-NEG
39 constant GR-ID-LINT-LIBS-PTX-TOOL
40 constant GR-ID-LINT-ARTIFACTS-FAST
variable GR-START-NS
variable GR-ARG-I

: GR-USAGE ( -- )
   s" usage: hb-gate-warm --load test/gate-runner-entry.f -- PHASE [--pool-slots N] [--timings]" GR-USAGE-RC die ;

: GR-ARG0= ( ptr u8 n -- bool )
   0 SCRIPT-ARGV$ STR= ;

: GR-TOKEN$ ( -- ptr u8 n )
   0 SCRIPT-ARGV$ ;

: GR-TAB. ( -- )
   STR-TAB emit ;

: GR-ARG$ ( -- ptr u8 n )
   GR-ARG-I @ SCRIPT-ARGV$ ;

: GR-ARG-VALUE$ ( -- ptr u8 n )
   GR-ARG-I @ 1+ SCRIPT-ARGC >= if GR-USAGE then
   GR-ARG-I @ 1+ SCRIPT-ARGV$ ;

: GR-POS-NUM ( ptr u8 n -- n )
   STR>NUMBER? 0= if drop GR-USAGE then
   dup 1 < if drop GR-USAGE then ;

: GR-ADVANCE ( n -- )
   GR-ARG-I @ + GR-ARG-I ! ;

: GR-POOL-OPT ( -- )
   GR-ARG-VALUE$ GR-POS-NUM GT-POOL-SLOTS!
   2 GR-ADVANCE ;

: GR-TIMINGS-OPT ( -- )
   GSI-TIMINGS!
   1 GR-ADVANCE ;

: GR-PARSE-ARG ( -- )
   GR-ARG$ s" --pool-slots" STR= if GR-POOL-OPT exit then
   GR-ARG$ s" --timings" STR= if GR-TIMINGS-OPT exit then
   GR-USAGE ;

: GR-CHECK-ARGS ( -- )
   SCRIPT-ARGC 1 < if GR-USAGE then
   1 GR-ARG-I !
   begin GR-ARG-I @ SCRIPT-ARGC < while
      GR-PARSE-ARG
   repeat ;

: GR-TOKEN-ID ( -- n )
   s" tool" GR-ARG0= if GR-ID-TOOL exit then
   s" tool-repair" GR-ARG0= if GR-ID-TOOL-REPAIR exit then
   s" tool-repair-check" GR-ARG0= if GR-ID-TOOL-REPAIR-CHECK exit then
   s" tool-repair-packet" GR-ARG0= if GR-ID-TOOL-REPAIR-PACKET exit then
   s" tool-doc" GR-ARG0= if GR-ID-TOOL-DOC exit then
   s" tool-doc-public" GR-ARG0= if GR-ID-TOOL-DOC-PUBLIC exit then
   s" tool-doc-status" GR-ARG0= if GR-ID-TOOL-DOC-STATUS exit then
   s" tool-doc-schema" GR-ARG0= if GR-ID-TOOL-DOC-SCHEMA exit then
   s" tool-lints" GR-ARG0= if GR-ID-TOOL-LINTS exit then
   s" tool-lint-repl" GR-ARG0= if GR-ID-TOOL-LINT-REPL exit then
   s" tool-lint-aot" GR-ARG0= if GR-ID-TOOL-LINT-AOT exit then
   s" tool-lint-names" GR-ARG0= if GR-ID-TOOL-LINT-NAMES exit then
   s" tool-lint-bundle" GR-ARG0= if GR-ID-TOOL-LINT-BUNDLE exit then
   s" tool-typed" GR-ARG0= if GR-ID-TOOL-TYPED exit then
   s" tool-semantics" GR-ARG0= if GR-ID-TOOL-SEMANTICS exit then
   s" check-cli" GR-ARG0= if GR-ID-CHECK-CLI exit then
   s" tail" GR-ARG0= if GR-ID-TAIL exit then
   s" lint-tools" GR-ARG0= if GR-ID-LINT-TOOLS exit then
   s" lint-manifest" GR-ARG0= if GR-ID-LINT-MANIFEST exit then
   s" lint-artifacts" GR-ARG0= if GR-ID-LINT-ARTIFACTS exit then
   s" lint-libs" GR-ARG0= if GR-ID-LINT-LIBS exit then
   s" repair" GR-ARG0= if GR-ID-REPAIR exit then
   s" fixtures" GR-ARG0= if GR-ID-FIXTURES exit then
   s" runtime" GR-ARG0= if GR-ID-RUNTIME exit then
   s" validate" GR-ARG0= if GR-ID-VALIDATE exit then
   s" diag-repair" GR-ARG0= if GR-ID-DIAG-REPAIR exit then
   s" diag-undef-primary" GR-ARG0= if GR-ID-DIAG-UNDEF-PRIMARY exit then
   s" diag-all-strict" GR-ARG0= if GR-ID-DIAG-ALL-STRICT exit then
   s" diag-file-unsafe" GR-ARG0= if GR-ID-DIAG-FILE-UNSAFE exit then
   s" dictionary" GR-ARG0= if GR-ID-DICTIONARY exit then
   s" debug" GR-ARG0= if GR-ID-DEBUG exit then
   s" tail-fast" GR-ARG0= if GR-ID-TAIL-FAST exit then
   s" tail-pure" GR-ARG0= if GR-ID-TAIL-PURE exit then
   s" tail-runner" GR-ARG0= if GR-ID-TAIL-RUNNER exit then
   s" tail-build" GR-ARG0= if GR-ID-TAIL-BUILD exit then
   s" tail-warm" GR-ARG0= if GR-ID-TAIL-WARM exit then
   s" lint-libs-core" GR-ARG0= if GR-ID-LINT-LIBS-CORE exit then
   s" lint-libs-ptx" GR-ARG0= if GR-ID-LINT-LIBS-PTX exit then
   s" lint-libs-ptx-neg" GR-ARG0= if GR-ID-LINT-LIBS-PTX-NEG exit then
   s" lint-libs-ptx-tool" GR-ARG0= if GR-ID-LINT-LIBS-PTX-TOOL exit then
   s" lint-artifacts-fast" GR-ARG0= if GR-ID-LINT-ARTIFACTS-FAST exit then
   GR-ID-UNKNOWN ;

: GR-STDLIB ( -- )
   s" test/gate-stdlib-cases.f" included ;

: GR-TOOL ( -- )
   SUITE-SKIP-TOOL-SEMANTIC!
   GR-STDLIB ;

: GR-DISPATCH ( -- )
   GR-TOKEN-ID case
      GR-ID-TOOL of GR-TOOL endof
      GR-ID-TOOL-REPAIR of GSI-TOOL-REPAIR endof
      GR-ID-TOOL-REPAIR-CHECK of GSI-TOOL-REPAIR-CHECK endof
      GR-ID-TOOL-REPAIR-PACKET of GSI-TOOL-REPAIR-PACKET endof
      GR-ID-TOOL-DOC of GSI-TOOL-DOC endof
      GR-ID-TOOL-DOC-PUBLIC of GSI-TOOL-DOC-PUBLIC endof
      GR-ID-TOOL-DOC-STATUS of GSI-TOOL-DOC-STATUS endof
      GR-ID-TOOL-DOC-SCHEMA of GSI-TOOL-DOC-SCHEMA endof
      GR-ID-TOOL-LINTS of GSI-TOOL-LINT-PHASE endof
      GR-ID-TOOL-LINT-REPL of GSI-TOOL-LINT-REPL-PHASE endof
      GR-ID-TOOL-LINT-AOT of GSI-TOOL-LINT-AOT endof
      GR-ID-TOOL-LINT-NAMES of GSI-TOOL-LINT-NAMES endof
      GR-ID-TOOL-LINT-BUNDLE of GSI-TOOL-LINT-BUNDLE endof
      GR-ID-TOOL-TYPED of GSI-TOOL-TYPED endof
      GR-ID-TOOL-SEMANTICS of GSI-TOOL-SEMANTICS endof
      GR-ID-CHECK-CLI of GR-STDLIB endof
      GR-ID-TAIL of GR-STDLIB endof
      GR-ID-LINT-TOOLS of GSI-LINT-TOOLS endof
      GR-ID-LINT-MANIFEST of GR-STDLIB endof
      GR-ID-LINT-ARTIFACTS of GR-STDLIB endof
      GR-ID-LINT-LIBS of GR-STDLIB endof
      GR-ID-LINT-LIBS-CORE of GSI-LINT-LIBS-CORE endof
      GR-ID-LINT-LIBS-PTX of GSI-LINT-LIBS-PTX endof
      GR-ID-LINT-LIBS-PTX-NEG of GSI-LINT-LIBS-PTX-NEG endof
      GR-ID-LINT-LIBS-PTX-TOOL of GSI-LINT-LIBS-PTX-TOOL endof
      GR-ID-LINT-ARTIFACTS-FAST of GSI-LINT-ARTIFACTS-FAST endof
      GR-ID-REPAIR of GENG-REPAIR-SLICE endof
      GR-ID-FIXTURES of GENG-FIXTURES-SLICE endof
      GR-ID-RUNTIME of GENG-RUNTIME-SLICE endof
      GR-ID-VALIDATE of GENG-VALIDATE-SLICE endof
      GR-ID-DIAG-REPAIR of GDX-REPAIR-SLICE endof
      GR-ID-DIAG-UNDEF-PRIMARY of GDX-UNDEF-PRIMARY-SLICE endof
      GR-ID-DIAG-ALL-STRICT of GDX-ALL-STRICT-SLICE endof
      GR-ID-DIAG-FILE-UNSAFE of GDX-FILE-UNSAFE-SLICE endof
      GR-ID-DICTIONARY of GD-MAIN endof
      GR-ID-DEBUG of GDB-RUN endof
      GR-ID-TAIL-FAST of GSI-TAIL-FAST endof
      GR-ID-TAIL-PURE of GSI-TAIL-PURE endof
      GR-ID-TAIL-RUNNER of GSI-TAIL-RUNNER endof
      GR-ID-TAIL-BUILD of GSI-TAIL-BUILD endof
      GR-ID-TAIL-WARM of GSI-TAIL-WARM endof
      GR-USAGE
   endcase ;

: GR-ELAPSED-MS ( -- n )
   mono-ns GR-START-NS @ - PROC-NS-PER-MS / ;

: GR-SUBJECT$ ( -- ptr u8 n )
   GR-TOKEN-ID case
      GR-ID-CHECK-CLI of s" candidate-cli" endof
      GR-ID-RUNTIME of s" candidate-cli" endof
      GR-ID-DICTIONARY of s" candidate-source" endof
      GR-ID-VALIDATE of s" candidate-source" endof
      s" host-source" rot
   endcase ;

: GR-RUNNER$ ( -- ptr u8 n )
   s" gate-runner" ;

: GR-SEMANTIC-TOOL? ( -- bool )
   GR-TOKEN-ID case
      GR-ID-TOOL-REPAIR of 0 0= endof
      GR-ID-TOOL-REPAIR-CHECK of 0 0= endof
      GR-ID-TOOL-REPAIR-PACKET of 0 0= endof
      GR-ID-TOOL-DOC of 0 0= endof
      GR-ID-TOOL-DOC-PUBLIC of 0 0= endof
      GR-ID-TOOL-DOC-STATUS of 0 0= endof
      GR-ID-TOOL-DOC-SCHEMA of 0 0= endof
      GR-ID-TOOL-LINTS of 0 0= endof
      GR-ID-TOOL-LINT-REPL of 0 0= endof
      GR-ID-TOOL-LINT-AOT of 0 0= endof
      GR-ID-TOOL-LINT-NAMES of 0 0= endof
      GR-ID-TOOL-LINT-BUNDLE of 0 0= endof
      GR-ID-TOOL-TYPED of 0 0= endof
      GR-ID-TOOL-SEMANTICS of 0 0= endof
      GR-ID-LINT-TOOLS of 0 0= endof
      GR-ID-LINT-LIBS-CORE of 0 0= endof
      GR-ID-LINT-LIBS-PTX of 0 0= endof
      GR-ID-LINT-LIBS-PTX-NEG of 0 0= endof
      GR-ID-LINT-LIBS-PTX-TOOL of 0 0= endof
      GR-ID-LINT-ARTIFACTS-FAST of 0 0= endof
      GR-ID-TAIL-FAST of 0 0= endof
      GR-ID-TAIL-PURE of 0 0= endof
      GR-ID-TAIL-RUNNER of 0 0= endof
      GR-ID-TAIL-BUILD of 0 0= endof
      GR-ID-TAIL-WARM of 0 0= endof
      0 0= 0= swap
   endcase ;

: GR-BOUNDARY$ ( -- ptr u8 n )
   GR-SEMANTIC-TOOL? if s" inprocess" exit then
   s" process" ;

: GR-SHA$ ( -- ptr u8 n )
   s" -" ;

: GR-STATS ( -- )
   GR-TOKEN$ GR-SUBJECT$ GR-RUNNER$ GR-BOUNDARY$ GR-SHA$ GS-TEST
   GR-TOKEN$ GR-ELAPSED-MS GS-SPAN ;

: GR-PASS ( -- )
   GR-STATS
   s" PASS: " type
   GR-TOKEN$ type
   s"  (" type
   GR-ELAPSED-MS GT-U-TYPE
   s" ms)" type cr ;

: GR-MAIN ( -- )
   mono-ns GR-START-NS !
   GR-CHECK-ARGS
   GR-DISPATCH
   GR-PASS ;

GR-MAIN
