\ cold-runtime-test.f - where an engine's cold runtime comes from, at the process
\ entry.
\
\ WHAT THIS PINS. One fact decides it and src/habu/habu2.f SEEDED-RUNTIME? is the
\ one place that reads it: the AOT window a build captured. A build that captured
\ one bakes the whole runtime into the image and installs it at boot; a build that
\ captured nothing bakes none, so the engine reads its runtime from the checkout's
\ own prefix source before the first token of its baked program. Only
\ src/habu/aot-capture.f fills the capture buffers and only the stdin driver
\ carries it, so every stage2 and maker engine is the second kind - the kind the
\ no-binary recovery chain (docs/bootstrap.md) and hb-build's cached maker are
\ made of, and the kind src/habu/hide.f and src/habu/prefix-rewind.f are written
\ against: they rewind a dictionary that already holds the core prefix.
\
\ RED-FIRST. While the emitter emitted the seeded arm for both kinds, EM-SEED-AOT's
\ mandatory-seed check and EM-SEAL-SEEDED-RUNTIME's friend latch went into an
\ engine that carries no seed: a stage2 engine died at its own boot with
\ `hb: AOT metadata corrupt` and exit 82 (ENGINE-ERROR:AOT-SEED) before its driver
\ ran, which is why case 2 asserts that message's ABSENCE alongside the driver's
\ own named refusal. Relaxing only the count check moved the death to the payload
\ instead (`E-UNDEFINED: USIGS`, src/habu/hide.f naming a checker word no prefix
\ had loaded) - the failure case 3 names from the other side.
\
\ THE SEEDED ARM IS SOMEBODY ELSE'S. That the installed product opens no prefix
\ source at all - the property that breaks if the two arms are confused the other
\ way - is tools/hb-open-failure-test.f (suite hb-open-failure), which boots a
\ copied bin/hb in an empty directory and requires silence and rc 0.
\
\ Run: bin/hb --load test/cold-runtime-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-cwd.f
require lib/codesign.f
require tools/build-fixpoint.f

using BUILD-FIXPOINT                     \ the real stage payload and build tmp root

package COLD-RUNTIME-TEST

$4000 constant CR-CAP
600000 constant CR-TIMEOUT-MS
74 constant CR-SOURCE-RC                 \ src/habu/stage2.f READ-SRC dies with this

create CR-ROOT FS-PATH-CAP allot   variable CR-ROOT-U
create CR-OUT CR-CAP allot
create CR-ERR CR-CAP allot

: CR-ROOT$ ( -- ptr u8 n )
   CR-ROOT CR-ROOT-U @ ;

: CR-ERR$ ( n -- ptr u8 n ) {: u:n :}
   CR-ERR u ;

: CR-SETUP ( -- )
   CLEANUP-RESET
   s" habu-cold-runtime" TMPDIR-MKDIR {: a:ptr u:n :}
   a CR-ROOT u BYTE-COPY
   u CR-ROOT-U !
   CR-ROOT$ CLEANUP-TREE+
   CR-ROOT$ BF-TMP! ;

: CR-CAPTURE>N ( result<pcap:captured,pcap:failed> -- n n n )   \ outu erru rc
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} o LEN>N e LEN>N 0 ENDOF
     err OF PCAP-FAILED:UNMAKE  {: o:len e:len c:rc :} o LEN>N e LEN>N c RC>N ENDOF
   ;MATCH ;

\ The stage engine's own entry: HB_TMP and the `-- <tmp>` row every stage build
\ hands it (tools/build-fixpoint.f BF-PREPARE-STAGE-ARGV), captured so the child's
\ diagnostic is the evidence and not an exit code on its own.
: CR-STAGE-ARGV ( -- )
   BF-PREPARE-ENV
   PROC-ARGV-RESET
   s" --" >LEN PROC-ARGV+
   CR-ROOT$ >LEN PROC-ARGV+ ;

: CR-RUN-STAGE ( -- n n n )
   CR-STAGE-ARGV
   s" stage2-got" BF-A$ >LEN CR-OUT CR-CAP >LEN CR-ERR CR-CAP >LEN CR-TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE CR-CAPTURE>N ;

\ The same engine with the temp root as cwd: nothing named src/ is reachable from
\ there, so the boot's own prefix read is what answers.
: CR-RUN-STAGE-OUTSIDE ( -- n n n )
   CR-STAGE-ARGV
   s" stage2-got" BF-A$ >LEN CR-ROOT$ >LEN CR-OUT CR-CAP >LEN CR-ERR CR-CAP >LEN
   CR-TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE CR-CAPTURE>N ;

\ ---- cases -----------------------------------------------------------------

\ The production entry, spelled as tools/hb-build-lib.f spells it for its maker:
\ the real assembled stage payload, built by the installed engine through --build.
: CR-STAGE-BUILDS ( -- )
   BF-STAGE2-SOURCE
   s" stage2-got" BF-REMOVE-TMP
   BF-ENGINE$ s" stage2-src" BF-A$ COMPILER-BUILD:RUN {: rc:n :}
   s" a stage2 payload builds an engine through --build" T-LABEL
   rc 0 T=
   s" ... and the engine is where its driver put it" T-LABEL
   s" stage2-got" BF-A$ FILE? TTRUE ;

: CR-STAGE-BOOTS ( -- )
   s" stage2-src" BF-REMOVE-TMP                  \ nothing left for the booted driver to read
   CR-RUN-STAGE {: outu:n erru:n rc:n :}
   s" the built engine boots through its prefix and reaches its own driver" T-LABEL
   rc CR-SOURCE-RC T=
   s" ... naming the source that driver could not open" T-LABEL
   erru CR-ERR$ s" stage2: cannot open source" CONTAINS? TTRUE
   s" ... and never asking for a seeded runtime it does not carry" T-LABEL
   erru CR-ERR$ s" hb: AOT metadata corrupt" CONTAINS? TFALSE ;

: CR-PREFIX-IS-SOURCE ( -- )
   CR-RUN-STAGE-OUTSIDE {: outu:n erru:n rc:n :}
   s" run where its prefix source is not, the same engine names the first file" T-LABEL
   rc CR-SOURCE-RC T=
   s" ... which is the first row of the core prefix" T-LABEL
   erru CR-ERR$ s" hb: cannot open src/core/util.f" CONTAINS? TTRUE ;

public

: COLD-RUNTIME-TEST-MAIN ( -- )
   T-RESET
   CR-SETUP
   CR-STAGE-BUILDS
   CR-STAGE-BOOTS
   CR-PREFIX-IS-SOURCE
   BF-TMP-RESET
   CLEANUP-RUN
   T-REPORT
   s" cold-runtime-test: ok" type cr ;

;package

COLD-RUNTIME-TEST:COLD-RUNTIME-TEST-MAIN
