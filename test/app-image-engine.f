\ app-image-engine.f - the engine with the image saver loaded, built once per
\ tree and run in place.
\
\ A row that saves an application image used to compile src/habu/app-image.f in
\ its child before the subject: about 4.8 s of every such child, measured on one
\ host. So the saver is compiled once, into a keyed image (test/keyed-image.f)
\ the rows run instead of the engine: hb-app-image-<key>, the engine with
\ src/habu/app-image.f loaded and saved. Its key folds that engine's bytes, the
\ program the builder is handed and the ordered closure of src/habu/app-image.f.
\ test/preloaded-engine.f builds the linker image on top of this one.
\
\ Under a gate the gate's app-image build row settles it before a row that
\ loads this module starts (test/gate-images.f), so the row only finds it.
\
\ RULES FOR A ROW THAT RUNS IT.
\ 1. Take the path before staging the child's argv or env: a build stages its
\    own in the same process-wide tables.
\ 2. A restored image starts at tier 0, and APP-IMAGE:SAVE refuses code
\    compiled there (`snap: retained code lacks native provenance`, exit 100).
\    src/habu/app-image.f sets tier 1 itself, so a program run on PATH$ starts
\    with `1 set-tier` where it required that file.
\ 3. A row whose children run ENGINE-CANDIDATE:PATH$ does not itself run on it:
\    inside an image ENGINE-ID:PATH$ names the image, so outside a gate its
\    children would run the image too.

require lib/errors.f
require lib/fs.f
require lib/process-argv.f
require lib/content-key.f
require lib/engine-candidate.f
require test/keyed-image.f

package APP-IMAGE-ENGINE

create KEY-HEX KEYED-IMAGE:KEY-HEX-LEN allot
create PATH-BUF FS-PATH-CAP allot

variable PATH-U
variable RESOLVED?

: PATH-BYTES ( -- ptr u8 n )
   PATH-BUF PATH-U @ ;

\ The builder is handed its program on stdin: APP-IMAGE:SAVE must run from the
\ outer stdin stream after every required file has returned.
: PROGRAM$ ( -- ptr u8 n )
   S\" require src/habu/app-image.f\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" ;

: FAMILY$ ( -- ptr u8 n )
   s" app-image" ;

: RESOLVE ( -- )
   RESOLVED? @ 0 <> if exit then
   CONTENT-KEY:OPEN
   s" app-image-engine-v1" CONTENT-KEY:TEXT+
   ENGINE-CANDIDATE:PATH$ s" host-engine" CONTENT-KEY:FILE-NAMED+
   PROGRAM$ CONTENT-KEY:TEXT+
   s" src/habu/app-image.f" KEYED-IMAGE:CLOSURE+
   KEY-HEX CONTENT-KEY:FINAL-HEX
   KEY-HEX FAMILY$ PATH-BUF PATH-U KEYED-IMAGE:PATH!
   0 0= RESOLVED? ! ;

\ The builder takes the output image as its only argument.
: OUT-ARG ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

public

\ Build the image unless the keyed artifact is already on disk.
: ENSURE ( -- )
   RESOLVE
   FAMILY$ PATH-BYTES ENGINE-CANDIDATE:PATH$ PROGRAM$ ['] OUT-ARG
   KEYED-IMAGE:ENSURE ;

\ The keyed image, to run as an engine.
: PATH$ ( -- ptr u8 n )
   ENSURE
   PATH-BYTES ;

\ The key as hex, for test/preloaded-engine.f: the linker is built on this
\ image, so its key folds this one and the host is never hashed.
: KEY$ ( -- ptr u8 n )
   RESOLVE
   KEY-HEX KEYED-IMAGE:KEY-HEX-LEN ;

;package
