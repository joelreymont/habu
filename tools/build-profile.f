\ build-profile.f - build hb, or a program, under Habu's own profiler, which
\ names the words the build runs (perf sees one anonymous mapping).
\ Run: bin/hb --load tools/build-profile.f -- [--target name] (--engine | source.f) <out> <text-report> <json-report>
\ The build is tools/hb-build.f's, linked in this process: hb's own
\ (HB-BUILD-CORE:ENGINE) under --engine, else the stripped program's
\ (HB-BUILD-CORE:APP). A macOS <out> keeps the linker's ad hoc signature.
require src/lib/error.f
require src/lib/string.f
require src/lib/fs.f
require src/compiler/target/model.f
require tools/hb-build-core.f

package BUILD-PROFILE
private

-7405 constant E-BUILD-PROFILE-IO      \ a report file this tool cannot write
64 constant USAGE-RC                   \ sysexits EX_USAGE
66 constant NOINPUT-RC                 \ sysexits EX_NOINPUT

20 constant REPORT-FD-MIN              \ keep the saved stdout clear of the build's own descriptors
0 constant F-DUPFD                     \ fcntl: duplicate to the lowest descriptor at or above the argument

100 constant TICK-US                   \ 13,000 samples of a 1.3 s warm build, for a tenth more wall time

variable SAVED-OUT
variable REPORT-FD

: OPTION? ( -- bool )
   SCRIPT-ARGC 0 > if 0 SCRIPT-ARGV$ s" --target" STR= else false then ;

\ The index of `--engine` or the source: past `--target name` when it leads.
: FIRST ( -- n )
   OPTION? if 2 else 0 then ;

: OPERANDS ( -- n )   SCRIPT-ARGC FIRST - ;
: ENGINE? ( -- bool )   FIRST SCRIPT-ARGV$ s" --engine" STR= ;

: USAGE ( -- )
   s" usage: bin/hb --load tools/build-profile.f -- [--target name] (--engine | source.f) <out> <text-report> <json-report>"
   USAGE-RC die ;

: SOURCE$ ( -- ptr u8 n )   FIRST SCRIPT-ARGV$ ;
: OUT$    ( -- ptr u8 n )   SCRIPT-ARGC 3 - SCRIPT-ARGV$ ;
: TEXT$   ( -- ptr u8 n )   SCRIPT-ARGC 2 - SCRIPT-ARGV$ ;
: JSON$   ( -- ptr u8 n )   SCRIPT-ARGC 1 - SCRIPT-ARGV$ ;

: DASHED? ( ptr u8 n -- bool )   s" -" STARTS-WITH? ;

: ARGS-OK? ( -- bool )
   OPERANDS 4 <> if false exit then
   ENGINE? SOURCE$ DASHED? 0= or
   OUT$ DASHED? 0= and  TEXT$ DASHED? 0= and  JSON$ DASHED? 0= and ;

: SOURCE-OK? ( -- bool )   ENGINE? SOURCE$ FS:FILE? or ;

\ The option's name, resolved as tools/hb-build.f resolves its --target.
: TARGET ( -- RTARGET:resolved-target )
   OPTION? 0= if RTARGET:HOST-TARGET exit then
   1 SCRIPT-ARGV$ {: a:ptr u:n :}
   a u RTARGET:PLATFORM? 0= if
      2 s" build-profile: unknown target " write drop
      a u USAGE-RC die
   then
   a u RTARGET:RESOLVE ;

\ The reports are written by the engine straight to fd 1, which is what makes
\ them safe to take from a signal handler. Pointing fd 1 at a file for the
\ length of one report is therefore the whole of "write it to a file".
: REDIRECT ( ptr u8 n -- ) {: pa:ptr pu:n :}
   1 F-DUPFD REPORT-FD-MIN fcntl SAVED-OUT !
   SAVED-OUT @ 0 < if s" build-profile: cannot save stdout" E-BUILD-PROFILE-IO die then
   pa pu FS:>PATHZ FS:O-WRONLY FS:O-CREAT or FS:O-TRUNC or FS:MODE-0644 open REPORT-FD !
   REPORT-FD @ 0 < if s" build-profile: cannot open a report file" E-BUILD-PROFILE-IO die then
   REPORT-FD @ 1 dup2 0 < if s" build-profile: cannot redirect stdout" E-BUILD-PROFILE-IO die then ;

: RESTORE ( -- )
   SAVED-OUT @ 1 dup2 drop
   REPORT-FD @ close
   SAVED-OUT @ close ;

: TEXT-REPORT ( -- )   TEXT$ REDIRECT  prof-report  RESTORE ;
: JSON-REPORT ( -- )   JSON$ REDIRECT  prof-json    RESTORE ;

: BUILD ( -- )
   ENGINE? if
      [: OUT$ ;] TARGET HB-BUILD-CORE:ENGINE
   else
      SOURCE$ [: OUT$ ;] TARGET HB-BUILD-CORE:APP
   then ;

public
: MAIN ( -- )
   ARGS-OK? 0= if USAGE then
   SOURCE-OK? 0= if s" build-profile: no such source" NOINPUT-RC die then
   TICK-US prof-rate  0 prof-on
   ['] BUILD catch {: rc:n :}
   prof-off
   TEXT-REPORT
   JSON-REPORT
   s" build-profile: reports in " type TEXT$ type s"  and " type JSON$ type cr
   rc 0<> if
      s" build-profile: the build threw " type rc . s" at " type HB-BUILD-CORE:ROW$ type cr
      s" " 1 die
   then ;
;package

BUILD-PROFILE:MAIN
