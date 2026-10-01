\ preloaded-engine.f - the engine with the AOT linker loaded, built once per tree
\ and run in place.
\
\ A row that links a stripped image used to compile the linker -
\ tools/aot-build.f and every src/habu/aot-*.f it loads - in each child: about
\ 7.6 s of every such child, measured on one host. So it is compiled once, into
\ a keyed image (test/keyed-image.f) the rows run instead of the engine:
\ LINKER$, hb-linker-<key>, the saver image test/app-image-engine.f builds with
\ tools/aot-build.f loaded on top and saved. Its key folds the saver image's
\ key - so it moves whenever that one does, and the host is never hashed - the
\ program, and the ordered closure of tools/aot-build.f.
\
\ Under a gate the gate's app-image and linker build rows settle both before a
\ row that loads this module starts (test/gate-images.f), so the row only finds
\ them. Production hb-build keeps compiling the linker per build
\ (tools/hb-build-lib.f): a prebuilt linker links only the subjects rule 3
\ admits, and its maker refuses the rest by name.
\
\ RULES FOR A ROW THAT RUNS IT, beside test/app-image-engine.f's.
\ 1. Take the path before staging the child's argv or env: a build stages its
\    own in the same process-wide tables.
\ 2. A link on LINKER$ keeps the production maker script verbatim; its require
\    of tools/aot-build.f is then a no-op.
\ 3. A subject linked on LINKER$ reaches nothing the linker's load left above
\    the engine. That load ran before the capture window opens
\    (tools/aot-build-open.f), and it holds the linker's lib closure in the
\    require registry, its packages and words in the dictionary and its cells
\    below the window. A subject's require of one of those modules resolves to
\    that copy, a name of one it never required resolves too, and a `package`
\    line naming one of its packages reopens it; the engine's maker compiles
\    the module inside the window (lib/executable-build.f excepted: it carries
\    the copy its opener loaded, and that file's names even when the subject
\    never required it), refuses the name and creates the package.
\    So the maker refuses a closure that reaches such a word (`aot: closure
\    reaches a word defined before the capture window opened word=NAME`,
\    `E-AOT-PRE-WINDOW` under `--json-errors`, src/habu/aot-closure.f
\    ADD-CLO) or such a cell (`aot: address refers to data outside the
\    restored span`). A subject that defines a word the image holds, globally
\    or in a package it reopens, dies at that line (`duplicate definition`,
\    rc 78), where the engine's maker dies at the library's line once the
\    linker loads. A subject that requires such a module, or adds new words to
\    such a package, and reaches none of the image's words links as on the
\    engine. The image-lifecycle registry holds the linker's persistent hooks
\    ahead of the subject's, so PREPARE runs them after the subject's, where
\    the engine's maker runs them first.
\ 4. A row whose children run ENGINE-CANDIDATE:PATH$ does not itself run on it:
\    inside an image ENGINE-ID:PATH$ names the image, so outside a gate its
\    children would run the image too.

require lib/errors.f
require lib/fs.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/content-key.f
require test/keyed-image.f
require test/app-image-engine.f

package PRELOADED-ENGINE

create LINKER-KEY KEYED-IMAGE:KEY-HEX-LEN allot
create LINKER-BUF FS-PATH-CAP allot

variable LINKER-U
variable LINKER-RESOLVED?

: LINKER-PATH$ ( -- ptr u8 n )
   LINKER-BUF LINKER-U @ ;

\ The builder is handed its program on stdin: APP-IMAGE:SAVE must run from the
\ outer stdin stream after every required file has returned.
: LINKER-PROGRAM$ ( -- ptr u8 n )
   S\" require tools/aot-build.f\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" ;

\ The saver image's key is taken before this key opens: both fold through the
\ one content-key context.
: LINKER-RESOLVE ( -- )
   LINKER-RESOLVED? @ 0 <> if exit then
   APP-IMAGE-ENGINE:KEY$ {: app:ptr appu:n :}
   CONTENT-KEY:OPEN
   s" preloaded-engine-v1" CONTENT-KEY:TEXT+
   app appu CONTENT-KEY:TEXT+
   LINKER-PROGRAM$ CONTENT-KEY:TEXT+
   s" tools/aot-build.f" KEYED-IMAGE:CLOSURE+
   LINKER-KEY CONTENT-KEY:FINAL-HEX
   LINKER-KEY s" linker" LINKER-BUF LINKER-U KEYED-IMAGE:PATH!
   0 0= LINKER-RESOLVED? ! ;

\ The builder takes the output image as its only argument.
: OUT-ARG ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

\ The saver image is settled first, whether the linker is on disk or not: it is
\ the linker's builder, and settling it dates it as used beside the linker.
: LINKER-ENSURE ( -- )
   APP-IMAGE-ENGINE:PATH$ {: host:ptr hostu:n :}
   LINKER-RESOLVE
   s" linker" LINKER-PATH$ host hostu LINKER-PROGRAM$ ['] OUT-ARG
   KEYED-IMAGE:ENSURE ;

: NULL-IN ( -- fd )
   s" /dev/null" FS-PATHZ open-rd dup 0 < if drop E-FS-OPEN throw then >FD ;

public

\ Settle the saver image and the linker.
: ENSURE ( -- )
   LINKER-ENSURE ;

: LINKER$ ( -- ptr u8 n )
   LINKER-ENSURE
   LINKER-PATH$ ;

\ Run a file on LINKER$ as `--load <file>`, with this process's stdout and
\ stderr and an empty stdin, and die with the child's status when it fails. A
\ child whose deadline expired exits PROC-TIMEOUT-RC (test/gate-common-lib.f
\ GE-CHILD-RUN), which is named and thrown again, so the gate pool labels the
\ row TIMEOUT-UNDER-LOAD.
: LINKER-LOAD ( ptr u8 n -- ) {: a:ptr u:n :}
   LINKER$ {: eng:ptr engu:n :}
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" --load" >LEN PROC-ARGV+
   a u >LEN PROC-ARGV+
   NULL-IN {: in:fd :}
   eng engu >LEN in -1 >FD -1 >FD PROC-RUN-ARGV-ENV-IO-RC
   in FD>N close
   MATCH result
      ok OF ENDOF
      err OF ENDOF
   ;MATCH {: rc:n :}
   rc PROC-TIMEOUT-RC = if
      s" preloaded-engine: linked row ran out of time" type cr
      E-PROC-TIMEOUT throw
   then
   rc 0 <> if s" preloaded-engine: linked row failed" rc die then ;

;package
