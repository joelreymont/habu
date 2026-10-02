\ saved-builder.f - the saved native builder, built once per tree and run in
\ place.
\
\ tools/native-builder-image.f saved as an application image is a native builder
\ with the build closure already compiled. Each saved native builder row
\ (test/native-builder-image-lib.f) used to save its own first: about 20 s CPU
\ of every such row, measured on one host. So it is saved once, into a keyed
\ image (test/keyed-image.f) the rows run: hb-saved-builder-<key>, the engine
\ with tools/native-builder-image.f loaded and saved. Its key folds that
\ engine's bytes, the program the builder is handed and the ordered closure of
\ tools/native-builder-image.f.
\
\ The builder compiles the tree of the directory it runs in, not the one it was
\ saved from (src/core/include.f SOURCE-ROOT:WITH-CWD), so a row runs it in a
\ private copy of the tree to build that copy's edits.
\
\ Under a gate the gate's saved-builder build row settles it before a row that
\ loads this module starts (test/gate-images.f), so the row only finds it. A
\ row takes the path before staging a child's argv or env: a build stages its
\ own in the same process-wide tables.

require lib/errors.f
require lib/fs.f
require lib/process-argv.f
require lib/content-key.f
require lib/engine-candidate.f
require test/keyed-image.f

package SAVED-BUILDER

create KEY-HEX KEYED-IMAGE:KEY-HEX-LEN allot
create PATH-BUF FS-PATH-CAP allot

variable PATH-U
variable RESOLVED?

: PATH-BYTES ( -- ptr u8 n )
   PATH-BUF PATH-U @ ;

\ The builder is handed its program on stdin: APP-IMAGE:SAVE must run from the
\ outer stdin stream after every required file has returned.
: PROGRAM$ ( -- ptr u8 n )
   S\" require tools/native-builder-image.f\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" ;

: FAMILY$ ( -- ptr u8 n )
   s" saved-builder" ;

: RESOLVE ( -- )
   RESOLVED? @ 0 <> if exit then
   CONTENT-KEY:OPEN
   s" saved-builder-v1" CONTENT-KEY:TEXT+
   ENGINE-CANDIDATE:PATH$ s" host-engine" CONTENT-KEY:FILE-NAMED+
   PROGRAM$ CONTENT-KEY:TEXT+
   s" tools/native-builder-image.f" KEYED-IMAGE:CLOSURE+
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

\ The keyed builder, to run in a tree as `<builder> [--] <out> [whitebox]`.
: PATH$ ( -- ptr u8 n )
   ENSURE
   PATH-BYTES ;

;package
