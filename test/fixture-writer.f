\ fixture-writer.f - the native fixture writer as a keyed application image,
\ built once per tree and run in place.
\
\ Every partial-capture fixture runs test/native-fixture-write.f: once to emit
\ the empty cold host test/cold-engine.f shares, and once for each partial image
\ it writes. Loaded as a script, the writer compiled tools/native-emit.f at tier 1
\ in every process: 22.8 s a write, against 0.4 s for the emission itself,
\ measured on one host. So tools/app-build.f - the builder `hb-build --repl`
\ runs - compiles it once into an application image that starts at the writer,
\ and every write runs that image with the writer's own arguments.
\
\ THE KEY IS WHAT THE IMAGE IS BUILT FROM: the engine binary that runs the
\ builder, the program the builder is handed, and the ordered require/include
\ closures of the builder and of the writer. All of them go into the content key
\ below, so another engine or an edit anywhere in either closure changes the key
\ and the image is rebuilt; no stale writer runs. test/keyed-image.f names,
\ builds, publishes and retains the image under that key.
\
\ PATH$ hands out the keyed image itself, where test/cold-engine.f hands out
\ copies of its host: the writer is only ever executed, and it writes nothing
\ but the paths its caller names, so no caller can clobber the shared bytes.

require lib/errors.f
require lib/fs.f
require lib/process-argv.f
require lib/content-key.f
require lib/engine-candidate.f
require test/keyed-image.f

package FIXTURE-WRITER

create KEY-HEX KEYED-IMAGE:KEY-HEX-LEN allot
create PATH-BUF FS-PATH-CAP allot

variable PATH-U
variable RESOLVED?

: WRITER$ ( -- ptr u8 n )
   s" test/native-fixture-write.f" ;

: BUILDER$ ( -- ptr u8 n )
   s" tools/app-build.f" ;

\ The builder's stdin, which names BUILDER$ again: APP-IMAGE:SAVE must run from
\ the outer stdin stream after every required file has returned, so the builder
\ is required there instead of named with --load.
: PROGRAM$ ( -- ptr u8 n )
   S\" require tools/app-build.f\nAPP-BUILD:RUN\n" ;

: FAMILY$ ( -- ptr u8 n )
   s" fixture-writer" ;

: PATH-BYTES ( -- ptr u8 n )
   PATH-BUF PATH-U @ ;

: KEY! ( -- )
   CONTENT-KEY:OPEN
   s" fixture-writer-v1" CONTENT-KEY:TEXT+
   ENGINE-CANDIDATE:PATH$ s" host-engine" CONTENT-KEY:FILE-NAMED+
   HB-TARGET-LINUX-X86-64? if
      ENGINE-CANDIDATE:PATH$ CONTENT-KEY:TEXT+
      s" test/native-fixture-native.f" KEYED-IMAGE:CLOSURE+
   then
   PROGRAM$ CONTENT-KEY:TEXT+
   BUILDER$ KEYED-IMAGE:CLOSURE+
   WRITER$ KEYED-IMAGE:CLOSURE+
   KEY-HEX CONTENT-KEY:FINAL-HEX ;

: RESOLVE ( -- )
   RESOLVED? @ 0 <> if exit then
   KEY!
   KEY-HEX FAMILY$ PATH-BUF PATH-U KEYED-IMAGE:PATH!
   0 0= RESOLVED? ! ;

\ tools/app-build.f takes the application source and the output image.
: BUILD-ARGS ( ptr u8 n -- ) {: out:ptr outu:n :}
   WRITER$ >LEN PROC-ARGV+
   out outu >LEN PROC-ARGV+ ;

public

\ Build the shared writer image unless the keyed artifact is already on disk.
: ENSURE ( -- )
   RESOLVE
   FAMILY$ PATH-BYTES ENGINE-CANDIDATE:PATH$ PROGRAM$ ['] BUILD-ARGS
   KEYED-IMAGE:ENSURE ;

\ The keyed image, for a caller to run as `<image> -- output [artifact producer]`
\ with the stdin program it wants run after the write, usually an empty one.
\ Take it before staging the child's arguments: when it has to build the image,
\ the build stages its own in the same process-wide argv.
: PATH$ ( -- ptr u8 n )
   ENSURE
   PATH-BYTES ;

\ The key as hex, for test/cold-engine.f: the host it caches is this image's
\ output, so its own key is derived from this one.
: KEY$ ( -- ptr u8 n )
   RESOLVE
   KEY-HEX KEYED-IMAGE:KEY-HEX-LEN ;

;package
