\ hb-open-failure-test.f - the installed engine's source-open contract, checked
\ from a directory that is not a checkout.
\
\ bin/hb is the SEEDED PRODUCT (tools/build-fixpoint.f "TWO ENGINES, ONE
\ PREFIX"): its cold runtime arrives from the baked AOT artifact, so a boot opens
\ no prefix source and the engine starts anywhere. This file asserted the
\ opposite - a boot outside the repo dying 74 on src/core/util.f, which only an
\ engine whose build captured nothing does (src/habu/habu2.f EMIT-COLD-PREFIX,
\ emitted for the stage2/maker engines and never for the product; the rule is
\ SEEDED-RUNTIME? beside it, and test/cold-runtime-test.f pins that other arm) -
\ and went red on the product for that reason. Both halves of the real contract
\ are here now: the product needs no checkout, and a source file it is handed and
\ cannot open is still named on stderr with exit 74 rather than skipped.
\ The bare file argument is deliberate - that argv row reads the file through the
\ engine's own raw arm (habu2.f LSRCRD/sopenerr, "hb: cannot open <path>"), which
\ is the last read with no registry above it. The `--load` row asks the require
\ registry instead and answers "include: cannot open ...": tools/load-argv-test.f
\ owns that one.
\
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f \
\   lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f \
\   lib/process-env.f lib/process-cwd.f tools/hb-open-failure-test.f

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

package HOF

$2000 constant HOF-CAP
10000 constant HOF-TIMEOUT-MS

create HOF-ROOT FS-PATH-CAP allot
create HOF-EXE  FS-PATH-CAP allot
create HOF-IN   1 allot                       \ zero-length stdin: EOF, never a REPL
create HOF-OUT  HOF-CAP allot
create HOF-ERR  HOF-CAP allot

variable HOF-ROOT-U
variable HOF-EXE-U

: HOF-ROOT$ ( -- ptr u8 n )
   HOF-ROOT HOF-ROOT-U @ ;

: HOF-EXE$ ( -- ptr u8 n )
   HOF-EXE HOF-EXE-U @ ;

\ Copy the built engine into a fresh empty temp dir and mark it executable.
: HOF-SETUP ( -- )
   CLEANUP-RESET
   s" hb-open-failure" HB-TMP-MKDIR {: a:ptr u:n :}
   a HOF-ROOT u BYTE-COPY
   u HOF-ROOT-U !
   HOF-ROOT$ CLEANUP-TREE+
   HOF-ROOT$ s" hb" HOF-EXE JOIN-PATH HOF-EXE-U !
   s" bin/hb" HOF-EXE$ COPY-FILE-STREAM
   HOF-EXE$ CHMOD-X ;

: HOF-CAPTURE>N ( result<pcap:captured,pcap:failed> -- n n n )   \ outn errn code (0 on clean exit)
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} o LEN>N e LEN>N 0 ENDOF
     err OF PCAP-FAILED:UNMAKE  {: o:len e:len c:rc :} o LEN>N e LEN>N c RC>N ENDOF
   ;MATCH ;

\ Run the copied engine with the empty temp dir as cwd; capture stdout/stderr/rc.
: HOF-RUN ( -- n n n )
   HOF-EXE$ >LEN HOF-ROOT$ >LEN HOF-IN 0 >LEN
   HOF-OUT HOF-CAP >LEN HOF-ERR HOF-CAP >LEN HOF-TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE
   HOF-CAPTURE>N ;

: HOF-RESET ( -- )
   PROC-ARGV-RESET
   PROC-ENV-RESET ;

: HOF-BOOTS-OUTSIDE-REPO ( -- )
   HOF-RESET
   HOF-RUN {: outu:n erru:n rc:n :}
   s" engine boots outside any checkout" T-LABEL
   rc 0 T=
   s" boot writes nothing to stdout" T-LABEL
   outu 0 T=
   s" boot writes nothing to stderr" T-LABEL
   erru 0 T= ;

: HOF-MISSING-SOURCE-NAMED ( -- )
   HOF-RESET
   s" missing.f" >LEN PROC-ARGV+
   HOF-RUN {: outu:n erru:n rc:n :}
   s" an unopenable source file exits 74" T-LABEL
   rc 74 T=
   s" the refusal writes nothing to stdout" T-LABEL
   outu 0 T=
   s" stderr names the file the engine could not open" T-LABEL
   HOF-ERR erru s\" hb: cannot open missing.f\n" T$= ;

: HB-OPEN-FAILURE-TEST-MAIN ( -- )
   T-RESET
   HOF-SETUP
   HOF-BOOTS-OUTSIDE-REPO
   HOF-MISSING-SOURCE-NAMED
   CLEANUP-RUN
   T-REPORT
   s" hb-open-failure-test: ok" type cr ;

HB-OPEN-FAILURE-TEST-MAIN

;package
