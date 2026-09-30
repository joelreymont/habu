\ preloaded-engine.f - the engine with the image saver loaded, and with the AOT
\ linker loaded on top, each built once per tree and run in place.
\
\ A row that saves an application image used to compile src/habu/app-image.f in
\ its child before the subject, and a row that links a stripped image compiled
\ the linker - tools/aot-build.f and every src/habu/aot-*.f it loads - in each
\ child: about 4.8 s and 7.6 s of every such child, measured on one host. So
\ both are compiled once, into two keyed images (test/keyed-image.f) the rows
\ run instead of the engine:
\
\ - APP-IMAGE$, hb-app-image-<key>: the engine with src/habu/app-image.f loaded
\   and saved. Its key folds that engine's bytes, the program the builder is
\   handed and the ordered closure of src/habu/app-image.f.
\ - LINKER$, hb-linker-<key>: APP-IMAGE$ with tools/aot-build.f loaded and
\   saved. Its key folds APP-IMAGE$'s key - so it moves whenever that one does,
\   and the host is never hashed - the program, and the ordered closure of
\   tools/aot-build.f.
\
\ The gate settles both before its first fork (test/gate-stdlib-lib.f
\ SUITE-SETUP), so its rows only find them. Production hb-build keeps compiling
\ the linker per build (tools/hb-build-lib.f): a prebuilt linker is only valid
\ for the subjects rule 4 admits.
\
\ RULES FOR A ROW THAT RUNS ONE.
\ 1. Take the path before staging the child's argv or env: a build stages its
\    own in the same process-wide tables.
\ 2. A restored image starts at tier 0, and APP-IMAGE:SAVE refuses code
\    compiled there (`snap: retained code lacks native provenance`, exit 100).
\    src/habu/app-image.f sets tier 1 itself, so a program run on APP-IMAGE$
\    starts with `1 set-tier` where it required that file.
\ 3. A link on LINKER$ keeps the production maker script verbatim; its require
\    of tools/aot-build.f is then a no-op.
\ 4. A subject linked on LINKER$ requires nothing in the linker's own lib
\    closure. A library already loaded is the copy the subject's require
\    resolves to, and its cells sit below the capture window
\    (tools/aot-build-open.f).
\ 5. A row whose children run ENGINE-CANDIDATE:PATH$ does not itself run on one
\    of these images: inside an image ENGINE-ID:PATH$ names the image, so
\    outside a gate its children would run the image too.

require lib/errors.f
require lib/fs.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/content-key.f
require lib/engine-candidate.f
require test/keyed-image.f

package PRELOADED-ENGINE

create APP-KEY KEYED-IMAGE:KEY-HEX-LEN allot
create APP-BUF FS-PATH-CAP allot
create LINKER-KEY KEYED-IMAGE:KEY-HEX-LEN allot
create LINKER-BUF FS-PATH-CAP allot

variable APP-U
variable APP-RESOLVED?
variable LINKER-U
variable LINKER-RESOLVED?

: APP-PATH$ ( -- ptr u8 n )
   APP-BUF APP-U @ ;

: LINKER-PATH$ ( -- ptr u8 n )
   LINKER-BUF LINKER-U @ ;

\ Each builder is handed its program on stdin: APP-IMAGE:SAVE must run from the
\ outer stdin stream after every required file has returned.
: APP-PROGRAM$ ( -- ptr u8 n )
   S\" require src/habu/app-image.f\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" ;

: LINKER-PROGRAM$ ( -- ptr u8 n )
   S\" require tools/aot-build.f\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" ;

: APP-RESOLVE ( -- )
   APP-RESOLVED? @ 0 <> if exit then
   CONTENT-KEY:OPEN
   s" preloaded-engine-v1" CONTENT-KEY:TEXT+
   ENGINE-CANDIDATE:PATH$ s" host-engine" CONTENT-KEY:FILE-NAMED+
   APP-PROGRAM$ CONTENT-KEY:TEXT+
   s" src/habu/app-image.f" KEYED-IMAGE:CLOSURE+
   APP-KEY CONTENT-KEY:FINAL-HEX
   APP-KEY s" app-image" APP-BUF APP-U KEYED-IMAGE:PATH!
   0 0= APP-RESOLVED? ! ;

: LINKER-RESOLVE ( -- )
   LINKER-RESOLVED? @ 0 <> if exit then
   APP-RESOLVE
   CONTENT-KEY:OPEN
   s" preloaded-engine-v1" CONTENT-KEY:TEXT+
   APP-KEY KEYED-IMAGE:KEY-HEX-LEN CONTENT-KEY:TEXT+
   LINKER-PROGRAM$ CONTENT-KEY:TEXT+
   s" tools/aot-build.f" KEYED-IMAGE:CLOSURE+
   LINKER-KEY CONTENT-KEY:FINAL-HEX
   LINKER-KEY s" linker" LINKER-BUF LINKER-U KEYED-IMAGE:PATH!
   0 0= LINKER-RESOLVED? ! ;

\ Both builders take the output image as their only argument.
: OUT-ARG ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: APP-ENSURE ( -- )
   APP-RESOLVE
   s" app-image" APP-PATH$ ENGINE-CANDIDATE:PATH$ APP-PROGRAM$ ['] OUT-ARG
   KEYED-IMAGE:ENSURE ;

\ The host is settled first, whether the linker is on disk or not: it is the
\ linker's builder, and settling it dates it as used beside the linker.
: LINKER-ENSURE ( -- )
   APP-ENSURE
   LINKER-RESOLVE
   s" linker" LINKER-PATH$ APP-PATH$ LINKER-PROGRAM$ ['] OUT-ARG
   KEYED-IMAGE:ENSURE ;

: NULL-IN ( -- fd )
   s" /dev/null" FS-PATHZ open-rd dup 0 < if drop E-FS-OPEN throw then >FD ;

public

\ Settle both images; the gate calls this once before its first fork.
: ENSURE ( -- )
   LINKER-ENSURE ;

: APP-IMAGE$ ( -- ptr u8 n )
   APP-ENSURE
   APP-PATH$ ;

: LINKER$ ( -- ptr u8 n )
   LINKER-ENSURE
   LINKER-PATH$ ;

\ Run a file on LINKER$ as `--load <file>`, with this process's stdout and
\ stderr and an empty stdin, and die with the child's status when it fails.
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
   rc 0 <> if s" preloaded-engine: linked row failed" rc die then ;

;package
