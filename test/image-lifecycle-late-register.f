\ A persistent lifecycle hook that registers a one-shot hook is refused by name
\ at the stripped link, and no image is written. IMAGE-LIFECYCLE:PREPARE runs
\ the one-shot hooks and releases their table before it runs the persistent ones.
require src/lib/test.f
require src/lib/test/outcome.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require test/aot-builder-engine.f

package IMAGE-LIFECYCLE-LATE-TEST
private

$10000 constant CAP
180000 constant TIMEOUT-MS
67 constant UNCAUGHT-RC                  \ hb's exit status for an uncaught throw
create OUT CAP allot
create ERR CAP allot
create ROOT-BUF PATH-CAP allot
create IMAGE-BUF PATH-CAP allot
create SUBJECT-BUF PATH-CAP allot
variable ROOT-U
variable IMAGE-U
variable SUBJECT-U

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE-BUF IMAGE-U @ ;
: SUBJECT$ ( -- ptr u8 n ) SUBJECT-BUF SUBJECT-U @ ;

: SETUP ( -- )
   FS:CLEANUP-RESET
   s" image-lifecycle-late" FS:MKTEMP-DIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY u ROOT-U !
   ROOT$ FS:CLEANUP-TREE+
   ROOT$ s" hb-aot-got" IMAGE-BUF FS:JOIN-PATH IMAGE-U !
   ROOT$ s" late-hook.f" SUBJECT-BUF FS:JOIN-PATH SUBJECT-U ! ;

\ The persistent hook registers a one-shot hook each time it runs.
: PROGRAM$ ( -- ptr u8 n )
   SB-RESET
   S\" : LATE-HOOK ( -- ) [: ;] IMAGE-LIFECYCLE:REGISTER ;\n" SB-APPEND
   S\" ' LATE-HOOK IMAGE-LIFECYCLE:REGISTER-PERSISTENT\n" SB-APPEND
   S\" : MAIN ( -- ) ;\n" SB-APPEND
   SB$ ;

\ The line the child prints for an uncaught code, rendered from the name so the
\ needle follows src/lib/error.f instead of repeating its number.
: NEEDLE$ ( -- ptr u8 n )
   SB-RESET
   s" uncaught throw code " SB-APPEND
   E-LIFECYCLE-LATE FMT:SB-INT
   SB$ ;

\ The saved AOT builder links the program into $TMPDIR/hb-aot-got.
: LINK ( -- len len outcome )
   AOT-BUILDER-ENGINE:PATH$ {: maker:ptr makeru:n :}
   SUBJECT$ PROGRAM$ FS:WRITE-ALL
   PROC-ARGV-ENV-RESET
   s" --" >LEN PROC-ARGV+
   SUBJECT$ >LEN PROC-ARGV+
   s" 0" >LEN PROC-ARGV+
   s" TMPDIR" >LEN ROOT$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   maker makeru >LEN AOT-BUILDER-ENGINE:STDIN$ >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME ;

: CHECK ( -- )
   s" a one-shot hook a persistent hook registers stops the link" T-LABEL
   LINK {: outu:len erru:len oc :}
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
