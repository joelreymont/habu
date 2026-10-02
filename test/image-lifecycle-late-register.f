\ A persistent lifecycle hook that registers a one-shot hook is refused by name
\ at the capture, and no image is written.
\ IMAGE-LIFECYCLE:PREPARE runs the one-shot hooks, releases their table, then
\ runs the persistent ones; the capture that follows releases every
\ DYNAMIC-BUFFER and copies DATA. Admitted, a one-shot hook registered by a
\ persistent one left its count in the image over no table, and that image's
\ own PREPARE threw E-BOUNDS (7122) at its first capture.
\ The program is saved from the keyed linker image (test/preloaded-engine.f),
\ which carries the linker's persistent PROC-MAPS:RELOAD hook
\ (tools/aot-build-core.f): the same PREPARE runs it after the program's own.
require lib/test.f
require lib/test/outcome.f
require lib/string.f
require lib/fmt.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require test/preloaded-engine.f

package IMAGE-LIFECYCLE-LATE-TEST
private

$10000 constant CAP
180000 constant TIMEOUT-MS
67 constant UNCAUGHT-RC                  \ hb's exit status for an uncaught throw
create OUT CAP allot
create ERR CAP allot
create ROOT-BUF FS-PATH-CAP allot
create IMAGE-BUF FS-PATH-CAP allot
variable ROOT-U
variable IMAGE-U

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE-BUF IMAGE-U @ ;

: SETUP ( -- )
   CLEANUP-RESET
   s" image-lifecycle-late" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY u ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT$ s" late-hook" IMAGE-BUF JOIN-PATH IMAGE-U ! ;

\ The persistent hook registers a one-shot hook each time it runs, and the
\ program saves itself to the path its argv names.
: PROGRAM$ ( -- ptr u8 n )
   SB-RESET
   S\" 1 set-tier\nrequire tools/aot-build.f\n" SB-APPEND
   S\" : LATE-HOOK ( -- ) [: ;] IMAGE-LIFECYCLE:REGISTER ;\n" SB-APPEND
   S\" ' LATE-HOOK IMAGE-LIFECYCLE:REGISTER-PERSISTENT\n" SB-APPEND
   S\" 0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" SB-APPEND
   SB$ ;

\ The line the child prints for an uncaught code, rendered from the name so the
\ needle follows lib/errors.f instead of repeating its number.
: NEEDLE$ ( -- ptr u8 n )
   SB-RESET
   s" uncaught throw code " SB-APPEND
   E-LIFECYCLE-LATE FMT:SB-INT
   SB$ ;

: SAVE ( -- len len outcome )
   PRELOADED-ENGINE:LINKER$ {: host:ptr hostu:n :}
   PROC-ARGV-ENV-RESET
   s" --" >LEN PROC-ARGV+
   IMAGE$ >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   host hostu >LEN PROGRAM$ >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME ;

: CHECK ( -- )
   s" a one-shot hook a persistent hook registers stops the save" T-LABEL
   SAVE {: outu:len erru:len oc :}
   PROGRAM$ OUT outu LEN>N ERR erru LEN>N oc UNCAUGHT-RC T-OUTCOME-EXITED=
   s" the refusal names the late registration" T-LABEL
   ERR erru LEN>N NEEDLE$ CONTAINS?
   dup 0= if OUT outu LEN>N type ERR erru LEN>N type then
   TTRUE
   s" and leaves no image behind" T-LABEL
   IMAGE$ EXISTS? TFALSE ;

: RUN ( -- )
   T-RESET SETUP
   [: CHECK ;] [: CLEANUP-RUN ;] finally
   T-REPORT ;

RUN
;package
