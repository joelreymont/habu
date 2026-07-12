\ hb-open-failure-test.f - regression for the baked source-open diagnostic.
\ Started outside the repo, the engine must name the first unresolved baked
\ prefix source on stderr and exit 74, not fail silently.
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
   s" hb-open-failure" TMPDIR-MKDIR {: a:ptr u:n :}
   a HOF-ROOT u BYTE-COPY
   u HOF-ROOT-U !
   HOF-ROOT$ CLEANUP-TREE+
   HOF-ROOT$ s" hb" HOF-EXE JOIN-PATH HOF-EXE-U !
   s" bin/hb" HOF-EXE$ COPY-FILE-STREAM
   HOF-EXE$ CHMOD-X ;

: HOF-CAPTURE>N ( len len rc -- n n n ) {: outu:len erru:len rc:rc :}
   outu LEN>N erru LEN>N rc RC>N ;

\ Run the copied engine with the empty temp dir as cwd; capture stdout/stderr/rc.
: HOF-RUN ( -- n n n )
   HOF-EXE$ >LEN HOF-ROOT$ >LEN HOF-OUT HOF-CAP >LEN HOF-ERR HOF-CAP >LEN HOF-TIMEOUT-MS >MS
   RUN-ARGV-ENV-CWD-CAPTURE
   HOF-CAPTURE>N ;

: HOF-OUTSIDE-NAMES-MISSING ( -- )
   HOF-SETUP
   PROC-ARGV-RESET
   PROC-ENV-RESET
   HOF-RUN {: outu:n erru:n rc:n :}
   s" engine exits 74 outside repo" T-LABEL
   rc 74 T=
   s" engine writes nothing to stdout" T-LABEL
   outu 0 T=
   s" stderr names the first missing prefix source" T-LABEL
   HOF-ERR erru s" hb: cannot open src/core/util.f" CONTAINS? TTRUE
   CLEANUP-RUN ;

: HB-OPEN-FAILURE-TEST-MAIN ( -- )
   T-RESET
   HOF-OUTSIDE-NAMES-MISSING
   T-REPORT
   s" hb-open-failure-test: ok" type cr ;

HB-OPEN-FAILURE-TEST-MAIN

;package
