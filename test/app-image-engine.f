\ The engine with the image saver loaded, built once per tree and run in place:
\ a keyed image (test/keyed-image.f), img/app-image, the engine with
\ src/habu/app-image.f loaded and saved, which saves each saving row's child
\ about 4.8 s. Its key folds that engine's bytes, the builder's program and the
\ ordered closure of src/habu/app-image.f. test/preloaded-engine.f builds the
\ linker image on it; under a gate, test/gate-images.f's row builds it first.

require src/lib/errors.f
require src/lib/fs.f
require src/lib/process-argv.f
require src/lib/content-key.f
require src/lib/engine-candidate.f
require tools/build-out.f
require test/keyed-image.f

package APP-IMAGE-ENGINE

create KEY-HEX KEYED-IMAGE:KEY-HEX-LEN allot
create ENGINE-KEY KEYED-IMAGE:KEY-HEX-LEN allot   \ the candidate's, naming its img/
create PATH-BUF PATH-CAP allot

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

: KEY-BODY ( CONTENT-KEY:fold -- )
   s" app-image-engine-v1" CONTENT-KEY:TEXT+
   ENGINE-CANDIDATE:PATH$ s" host-engine" CONTENT-KEY:FILE-NAMED+
   PROGRAM$ CONTENT-KEY:TEXT+
   s" src/habu/app-image.f" KEYED-IMAGE:CLOSURE+
   KEY-HEX CONTENT-KEY:FINAL-HEX ;

: RESOLVE ( -- )
   RESOLVED? @ 0 <> if exit then
   [: KEY-BODY ;] CONTENT-KEY:WITH-FOLD
   ENGINE-CANDIDATE:PATH$ ENGINE-KEY BUILD-OUT:KEY-OF!
   FAMILY$ ENGINE-KEY PATH-BUF PATH-U KEYED-IMAGE:PATH!
   0 0= RESOLVED? ! ;

: OUT-ARG ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

public

\ Build the image unless the one on disk was built under its key.
: ENSURE ( -- )
   RESOLVE
   KEY-HEX FAMILY$ ENGINE-KEY ENGINE-CANDIDATE:PATH$ PROGRAM$ ['] OUT-ARG
   KEYED-IMAGE:ENSURE ;

\ The keyed image, to run as an engine. Take the path before staging the child's
\ argv or env: a build stages its own in the same process-wide tables. A row
\ whose children run ENGINE-CANDIDATE:PATH$ does not itself run on it: inside an
\ image ENGINE-ID:PATH$ names the image, so outside a gate its children would
\ run the image too.
: PATH$ ( -- ptr u8 n )
   ENSURE
   PATH-BYTES ;

\ The key as hex, for test/preloaded-engine.f: the linker is built on this
\ image, so its key folds this one.
: KEY$ ( -- ptr u8 n )
   RESOLVE
   KEY-HEX KEYED-IMAGE:KEY-HEX-LEN ;

;package
