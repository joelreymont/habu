\ keyed-image-reap-child.f - the two processes test/keyed-image-reap-test.f
\ starts for each build: the keyed-image build of a family of its own, and the
\ builder that build hands the save to. Both are entered from stdin, as a gate
\ build row enters a family module.
\
\ BUILD (argv: <family> <key hex> <mark> <release>) settles the family's image
\ through KEYED-IMAGE:ENSURE, in the cache root HABU_BUILD_CACHE names, with
\ this engine as the builder's. The builder is this file again, entered at SAVE
\ (argv: <image path> <mark> <release>): it writes "<pid> <image path>" to the
\ mark, which tells the test the builder runs and where its work directory is,
\ waits until the release exists, then saves a stand-in image - an executable
\ file, which is all the publish checks. A builder never released gives up after
\ RELEASE-SECONDS, so a test that stops early leaves nothing running.

require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process-argv.f
require lib/task.f
require lib/time.f
require lib/engine-candidate.f
require test/keyed-image.f

package KEYED-IMAGE-REAP-CHILD

120 constant RELEASE-SECONDS
50 constant POLL-MS
75 constant UNRELEASED-RC

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
   3 SCRIPT-ARGV$ >LEN PROC-ARGV+ ;

: MARK ( -- )
   SB-RESET
   getpid FMT:SB-INT
   s"  " SB-APPEND
   0 SCRIPT-ARGV$ SB-APPEND
   1 SCRIPT-ARGV$ SB$ ATOMIC-WRITE-FILE ;

: RELEASED? ( -- bool )
   2 SCRIPT-ARGV$ EXISTS? ;

public

: BUILD ( -- )
   1 SCRIPT-ARGV$ {: key:ptr keyu:n :}
   keyu KEYED-IMAGE:KEY-HEX-LEN <> if E-STR-BOUNDS throw then
   key 0 SCRIPT-ARGV$ PATH-BUF PATH-U KEYED-IMAGE:PATH!
   0 SCRIPT-ARGV$ PATH$ ENGINE-CANDIDATE:PATH$ PROGRAM$ ['] SAVE-ARGS
   KEYED-IMAGE:ENSURE ;

: SAVE ( -- )
   MARK
   TIME:EPOCH-SECONDS RELEASE-SECONDS + {: deadline:n :}
   begin RELEASED? 0= while
      TIME:EPOCH-SECONDS deadline > if
         s" keyed-image-reap-child: never released" UNRELEASED-RC die
      then
      POLL-MS >MS TASK:SLEEP
   repeat
   0 SCRIPT-ARGV$ s" stand-in image" WRITE-ALL
   0 SCRIPT-ARGV$ CHMOD-X ;

;package
