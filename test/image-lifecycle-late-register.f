\ A persistent lifecycle hook that registers a one-shot hook is refused by name
\ at the capture, and no image is written. IMAGE-LIFECYCLE:PREPARE runs the
\ one-shot hooks, releases their table, then the persistent ones: admitted, the
\ late hook's count reaches the image over no table and that image's PREPARE
\ throws E-BOUNDS.
require src/lib/test.f
require src/lib/test/outcome.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require test/preloaded-engine.f

package IMAGE-LIFECYCLE-LATE-TEST
private

$10000 constant CAP
180000 constant TIMEOUT-MS
67 constant UNCAUGHT-RC                  \ hb's exit status for an uncaught throw
create OUT CAP allot
create ERR CAP allot
create ROOT-BUF PATH-CAP allot
create IMAGE-BUF PATH-CAP allot
variable ROOT-U
variable IMAGE-U

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE-BUF IMAGE-U @ ;

: SETUP ( -- )
   FS:CLEANUP-RESET
   s" image-lifecycle-late" FS:MKTEMP-DIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY u ROOT-U !
   ROOT$ FS:CLEANUP-TREE+
   ROOT$ s" late-hook" IMAGE-BUF FS:JOIN-PATH IMAGE-U ! ;

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
\ needle follows src/lib/errors.f instead of repeating its number.
: NEEDLE$ ( -- ptr u8 n )
   SB-RESET
   s" uncaught throw code " SB-APPEND
   E-LIFECYCLE-LATE FMT:SB-INT
   SB$ ;

\ The keyed linker image (test/preloaded-engine.f) saves the program; its
\ PROC-MAPS:RELOAD hook (tools/aot-build-core.f) runs after the program's own.
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
   IMAGE$ FS:EXISTS? TFALSE ;

: RUN ( -- )
   T-RESET SETUP
   [: CHECK ;] [: FS:CLEANUP-RUN ;] finally
   T-REPORT ;

RUN
;package
