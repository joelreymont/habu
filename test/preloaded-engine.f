\ preloaded-engine.f - the engine with the AOT linker (tools/aot-build.f)
\ loaded, a keyed image (test/keyed-image.f) built on a key miss and run in
\ place: compiling the linker cost about 7.6 s per linking child. Production
\ hb-build compiles the linker per build; raw maker children use
\ test/aot-builder-engine.f.

require src/lib/errors.f
require src/lib/fs.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/content-key.f
require tools/build-out.f
require test/keyed-image.f
require test/app-image-engine.f

package PRELOADED-ENGINE

create LINKER-KEY KEYED-IMAGE:KEY-HEX-LEN allot
create HOST-KEY KEYED-IMAGE:KEY-HEX-LEN allot   \ the saver image's, naming its img/
create LINKER-BUF PATH-CAP allot

variable LINKER-U
variable LINKER-RESOLVED?

: LINKER-PATH$ ( -- ptr u8 n )
   LINKER-BUF LINKER-U @ ;

\ The builder is handed its program on stdin: APP-IMAGE:SAVE must run from the
\ outer stdin stream after every required file has returned.
: LINKER-PROGRAM$ ( -- ptr u8 n )
   S\" require tools/aot-build.f\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" ;

\ The key folds the saver image's (test/app-image-engine.f), the program and
\ tools/aot-build.f's ordered closure; test/gate-images.f settles it under a
\ gate.
: LINKER-KEY-BODY ( ptr u8 n CONTENT-KEY:fold -- )
   s" preloaded-engine-v1" CONTENT-KEY:TEXT+
   -rot CONTENT-KEY:TEXT+
   LINKER-PROGRAM$ CONTENT-KEY:TEXT+
   s" tools/aot-build.f" KEYED-IMAGE:CLOSURE+
   LINKER-KEY CONTENT-KEY:FINAL-HEX ;

\ The linker's slot is under the saver image at host, which builds it.
: LINKER-RESOLVE ( ptr u8 n -- )
   {: host:ptr hostu:n :}
   LINKER-RESOLVED? @ 0 <> if exit then
   APP-IMAGE-ENGINE:KEY$ [: LINKER-KEY-BODY ;] CONTENT-KEY:WITH-FOLD
   host hostu HOST-KEY BUILD-OUT:KEY-OF!
   s" linker" HOST-KEY LINKER-BUF LINKER-U KEYED-IMAGE:PATH!
   0 0= LINKER-RESOLVED? ! ;

\ The builder takes the output image as its only argument.
: OUT-ARG ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

\ The saver image is settled first, whether the linker is fresh or not: it is
\ the linker's builder, and its key is folded into the linker's.
: LINKER-ENSURE ( -- )
   APP-IMAGE-ENGINE:PATH$ {: host:ptr hostu:n :}
   host hostu LINKER-RESOLVE
   LINKER-KEY s" linker" HOST-KEY host hostu LINKER-PROGRAM$ ['] OUT-ARG
   KEYED-IMAGE:ENSURE ;

: NULL-IN ( -- fd )
   s" /dev/null" FS:>PATHZ open-rd dup 0 < if drop E-FS-OPEN throw then >FD ;

public

: ENSURE ( -- )
   LINKER-ENSURE ;

\ Rules for a row that runs LINKER$, beside test/app-image-engine.f's:
\ 1. Take the path before staging the child's argv or env: a build stages its
\    own in the same process-wide tables.
\ 2. A link keeps the production maker script verbatim; its require of
\    tools/aot-build.f is then a no-op.
\ 3. A subject reaches no word the linker's load left above the engine and no
\    cell of it but those an AOT ownership claim names: that load ran before the
\    capture window opened (tools/aot-build-open.f). A require of one of its
\    modules, a name it never required and a `package` line naming one of its
\    packages all resolve to that copy, where the engine's maker compiles inside
\    the window. The maker refuses a closure reaching such a word
\    (E-AOT-PRE-WINDOW, src/habu/aot-closure.f ADD-CLO) or cell (`aot: address
\    refers to data outside the restored span`). src/habu/aot-owned-cells.f
\    COLLECT claims the FFI and TASK state baked below the window
\    (test/stripped-preloaded-runtime.f). A subject defining a word the image
\    holds dies at that line (`duplicate definition`, rc 78). A subject that
\    requires such a module or adds words to such a package, and reaches none of
\    the image's words, links as on the engine. The linker's persistent hooks
\    register ahead of the subject's, so PREPARE runs them after the subject's,
\    where the engine's maker runs them first.
\ 4. A row whose children run ENGINE-CANDIDATE:PATH$ does not itself run on it:
\    inside an image ENGINE-ID:PATH$ names the image.
: LINKER$ ( -- ptr u8 n )
   LINKER-ENSURE
   LINKER-PATH$ ;

\ A child whose deadline expired exits PROC-TIMEOUT-RC (test/gate-common-lib.f
\ GE-CHILD-RUN) and is thrown again.
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
