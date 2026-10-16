\ keyed-image-reap-child.f - the two processes test/keyed-image-reap-test.f
\ starts for each build: the keyed-image build of a family of its own, and the
\ builder that build hands the save to. Both are entered from stdin, as a gate
\ build row enters a family module.
\
\ BUILD (argv: <family> <key hex> <mark> <release> <expiry>) settles the
\ family's image through KEYED-IMAGE:ENSURE, in the cache root HABU_BUILD_CACHE
\ names, with this engine as the builder's. The builder is this file again,
\ entered at SAVE (argv: <image path> <mark> <release> <expiry>): it writes
\ "<pid> <image path>" to the mark, which tells the test the builder runs and
\ where its work directory is, waits until the release exists, then saves a
\ stand-in image - an executable file, which is all the publish checks. A
\ builder the test did not release before the deadline, RELEASE-SECONDS, gives
\ up, so a test that stops early leaves nothing running.
\
\ A deadline is no build failure, and a throw code does not cross a process
\ boundary: SAVE exits PROC-TIMEOUT-RC when its wait expires, KEYED-IMAGE's
\ BUILD-RUN throws E-PROC-TIMEOUT on that status or on its own capture
\ deadline, and BUILD exits PROC-TIMEOUT-RC again, so the test reads 124. A
\ builder whose build the test killed has no parent to read that exit, so the
\ expiring builder first writes its expiry file, which the test reads instead.

require src/lib/errors.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/task.f
require src/lib/time.f
require src/lib/engine-candidate.f
require src/lib/test/wait.f
require test/keyed-image.f

package KEYED-IMAGE-REAP-CHILD

120 constant RELEASE-SECONDS
50 constant POLL-MS

create PATH-BUF FS-PATH-CAP allot
variable PATH-U

: PATH$ ( -- ptr u8 n )
   PATH-BUF PATH-U @ ;

: PROGRAM$ ( -- ptr u8 n )
   S\" require test/keyed-image-reap-child.f\nKEYED-IMAGE-REAP-CHILD:SAVE\n" ;

\ The builder's argv after `--`: the image path, then this build's mark and
\ release.
: SAVE-ARGS ( ptr u8 n -- )
   >LEN PROC-ARGV+
   2 SCRIPT-ARGV$ >LEN PROC-ARGV+
   3 SCRIPT-ARGV$ >LEN PROC-ARGV+
   4 SCRIPT-ARGV$ >LEN PROC-ARGV+ ;

: MARK ( -- )
   SB-RESET
   getpid FMT:SB-INT
   s"  " SB-APPEND
   0 SCRIPT-ARGV$ SB-APPEND
   1 SCRIPT-ARGV$ SB$ ATOMIC-WRITE-FILE ;

: RELEASED? ( -- bool )
   2 SCRIPT-ARGV$ EXISTS? ;

\ Wait until the release exists. A builder the test did not release before the
\ deadline writes its expiry file, then names the wait and throws
\ E-PROC-TIMEOUT: the file comes first, since a builder whose build was killed
\ writes its name into a pipe nobody reads.
: AWAIT-RELEASE ( -- )
   TIME:EPOCH-SECONDS RELEASE-SECONDS + {: deadline:n :}
   begin RELEASED? 0= while
      TIME:EPOCH-SECONDS deadline > if
         3 SCRIPT-ARGV$ s" expired" WRITE-ALL
         s" keyed-image-reap-child AWAIT-RELEASE" TEST-WAIT:EXPIRED
      then
      POLL-MS >MS TASK:SLEEP
   repeat ;

: SETTLE-IMAGE ( -- )
   1 SCRIPT-ARGV$ {: key:ptr keyu:n :}
   keyu KEYED-IMAGE:KEY-HEX-LEN <> if E-STR-BOUNDS throw then
   key 0 SCRIPT-ARGV$ PATH-BUF PATH-U KEYED-IMAGE:PATH!
   0 SCRIPT-ARGV$ PATH$ ENGINE-CANDIDATE:PATH$ PROGRAM$ ['] SAVE-ARGS
   KEYED-IMAGE:ENSURE ;

public

\ A builder that met a deadline leaves its capture unprinted
\ (tools/keyed-image.f BUILD-RUN): BUILD prints it into the log the test reads,
\ then exits PROC-TIMEOUT-RC. Every other code is rethrown.
: BUILD ( -- )
   ['] SETTLE-IMAGE catch {: code:n :}
   code E-PROC-TIMEOUT = if
      KEYED-IMAGE:CAPTURE-OUT$ type
      2 KEYED-IMAGE:CAPTURE-ERR$ write drop
      s" keyed-image-reap-child BUILD: the builder met a deadline"
      PROC-TIMEOUT-RC die
   then
   code 0<> if code throw then ;

\ AWAIT-RELEASE has printed the wait's name, so the exit says nothing more.
: SAVE ( -- )
   MARK
   ['] AWAIT-RELEASE catch {: code:n :}
   code E-PROC-TIMEOUT = if s" " PROC-TIMEOUT-RC die then
   code 0<> if code throw then
   0 SCRIPT-ARGV$ s" stand-in image" WRITE-ALL
   0 SCRIPT-ARGV$ CHMOD-X ;

;package
