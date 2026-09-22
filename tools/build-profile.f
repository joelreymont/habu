\ build-profile.f - run the engine self-build under the internal profiler.
\
\ Joel, 2026-09-16: "make sure that the build profiler uses our own profiler.
\ that's the best view into our code." The self-build numbers in LESSONS.md were
\ taken with `strace -c` and `perf record`, and neither can name a word the
\ engine compiled for itself: perf sees one anonymous mapping. This runs the
\ same build under prof-on and writes what our own profiler saw.
\
\   bin/hb --load tools/build-profile.f -- <engine-out> <text-report> <json-report>
\
\ The engine lands at <engine-out> through the same driver, and the build's own
\ phase timers still go to stdout, so they sit beside the profile rather than
\ being replaced by it.
\
\ Its product is byte-identical to tools/native-build.f for the same tree and
\ host: capture stores window coordinates, and retired symbol storage is zeroed
\ when it grows. The profiler's own DATA allocations do not enter the artifact.
\
\ WHY IT DOES NOT `include tools/native-build.f`. That file ends in
\ NATIVE-BUILD:RUN, which ends in `die`, which is an unconditional exit_group:
\ nothing after the build ever runs, so the report could never be written. This
\ calls NATIVE-BUILD:RUN-PATH-RC, the same driver without the argv convention
\ and the exit, and prints the report itself.
1 set-tier
require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/executable-build.f
require tools/native-build-core.f

package BUILD-PROFILE
private

-7405 constant E-BUILD-PROFILE-IO      \ a report file this tool cannot write

20 constant REPORT-FD-MIN              \ keep the saved stdout clear of the build's own descriptors
0 constant F-DUPFD                     \ fcntl: duplicate to the lowest descriptor at or above the argument

variable SAVED-OUT
variable REPORT-FD

: ARGS-OK? ( -- bool )
   SCRIPT-ARGC 3 = ;

: USAGE ( -- )
   s" usage: bin/hb --load tools/build-profile.f -- <engine-out> <text-report> <json-report>"
   E-BUILD-PROFILE-IO die ;

: ENGINE$ ( -- ptr u8 n )   0 SCRIPT-ARGV$ ;
: TEXT$   ( -- ptr u8 n )   1 SCRIPT-ARGV$ ;
: JSON$   ( -- ptr u8 n )   2 SCRIPT-ARGV$ ;

\ The reports are written by the engine straight to fd 1, which is what makes
\ them safe to take from a signal handler. Pointing fd 1 at a file for the
\ length of one report is therefore the whole of "write it to a file".
: REDIRECT ( ptr u8 n -- ) {: pa:ptr pu:n :}
   1 F-DUPFD REPORT-FD-MIN fcntl SAVED-OUT !
   SAVED-OUT @ 0 < if s" build-profile: cannot save stdout" E-BUILD-PROFILE-IO die then
   pa pu FS-PATHZ FS-O-WRONLY FS-O-CREAT or FS-O-TRUNC or FS-MODE-0644 open REPORT-FD !
   REPORT-FD @ 0 < if s" build-profile: cannot open a report file" E-BUILD-PROFILE-IO die then
   REPORT-FD @ 1 dup2 0 < if s" build-profile: cannot redirect stdout" E-BUILD-PROFILE-IO die then ;

: RESTORE ( -- )
   SAVED-OUT @ 1 dup2 drop
   REPORT-FD @ close
   SAVED-OUT @ close ;

: TEXT-REPORT ( -- )   TEXT$ REDIRECT  prof-report  RESTORE ;
: JSON-REPORT ( -- )   JSON$ REDIRECT  prof-json    RESTORE ;

\ The typed code reference the build driver expects, exactly as
\ tools/native-build.f hands it over.
: ORIGIN ( n n -- n ) code-origin ;

: DRIVE-RC ( -- n )
   ENGINE$ ['] ORIGIN 0 0= 0= NATIVE-BUILD:RUN-PATH-RC ;

public
: MAIN ( -- )
   ARGS-OK? 0= if USAGE then
   0 prof-on
   ['] DRIVE-RC EXECUTABLE-BUILD:WITH {: rc:n :}
   prof-off
   TEXT-REPORT
   JSON-REPORT
   s" build-profile: reports in " type TEXT$ type s"  and " type JSON$ type cr
   s" " rc die ;
;package

BUILD-PROFILE:MAIN
