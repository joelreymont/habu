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
\ and the image is rebuilt; no stale writer runs.
\
\ PATH$ hands out the keyed image itself, where test/cold-engine.f hands out
\ copies of its host: the writer is only ever executed, and it writes nothing
\ but the paths its caller names, so no caller can clobber the shared bytes.

require lib/errors.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/build-cache.f
require lib/content-key.f
require lib/engine-candidate.f
require tools/event-closure-lib.f

package FIXTURE-WRITER

$10000 constant IO-CAP
240000 constant BUILD-TIMEOUT-MS
75 constant WRITER-RC
64 constant KEY-HEX-LEN
128 constant NAME-CAP

create KEY-HEX KEY-HEX-LEN allot
create NAME-BUF NAME-CAP allot
create PATH-BUF FS-PATH-CAP allot
create WORK-BUF FS-PATH-CAP allot
create TMP-BUF FS-PATH-CAP allot
create OUT IO-CAP allot
create ERR IO-CAP allot

variable NAME-U
variable PATH-U
variable WORK-U
variable TMP-U
variable EMIT-RC
variable RESOLVED?
variable CLOSURE-IDX

: WRITER$ ( -- ptr u8 n )
   s" test/native-fixture-write.f" ;

: BUILDER$ ( -- ptr u8 n )
   s" tools/app-build.f" ;

\ The builder's stdin, which names BUILDER$ again: APP-IMAGE:SAVE must run from
\ the outer stdin stream after every required file has returned, so the builder
\ is required there instead of named with --load.
: PROGRAM$ ( -- ptr u8 n )
   S\" require tools/app-build.f\nAPP-BUILD:RUN\n" ;

: PATH-BYTES ( -- ptr u8 n )
   PATH-BUF PATH-U @ ;

: WORK-BYTES ( -- ptr u8 n )
   WORK-BUF WORK-U @ ;

: TMP-BYTES ( -- ptr u8 n )
   TMP-BUF TMP-U @ ;

\ Discovery rejects fail-closed, so a closure that cannot be reproduced cannot
\ be keyed - the key never silently covers fewer files than the build reads.
: CLOSURE+ ( CONTENT-KEY:fold ptr u8 n -- CONTENT-KEY:fold )
   EC:BUILD
   0 CLOSURE-IDX !
   begin CLOSURE-IDX @ EC:COUNT < while
      CLOSURE-IDX @ EC:PATH$ CLOSURE-IDX @ EC:NAME$ CONTENT-KEY:FILE-NAMED+
      CLOSURE-IDX @ 1+ CLOSURE-IDX !
   repeat ;

: KEY! ( -- )
   CONTENT-KEY:OPEN
   s" fixture-writer-v1" CONTENT-KEY:TEXT+
   ENGINE-CANDIDATE:PATH$ s" host-engine" CONTENT-KEY:FILE-NAMED+
   PROGRAM$ CONTENT-KEY:TEXT+
   BUILDER$ CLOSURE+
   WRITER$ CLOSURE+
   KEY-HEX CONTENT-KEY:FINAL-HEX ;

: NAME! ( -- )
   s" hb-fixture-writer-" {: a:ptr u:n :}
   u KEY-HEX-LEN + NAME-CAP > if E-FS-CAPACITY throw then
   a NAME-BUF u BYTE-COPY
   KEY-HEX NAME-BUF u + KEY-HEX-LEN BYTE-COPY
   u KEY-HEX-LEN + NAME-U ! ;

: RESOLVE ( -- )
   RESOLVED? @ 0 <> if exit then
   KEY!
   NAME!
   BUILD-CACHE:ROOT$ NAME-BUF NAME-U @ PATH-BUF JOIN-PATH PATH-U !
   0 0= RESOLVED? ! ;

: COPY-OUT! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr up:ptr :}
   u 0 <= if E-FS-PATH throw then
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   a dst u BYTE-COPY
   u up ! ;

\ The builder saves into a private directory of its own, so a half-written image
\ is never visible at the keyed path: only the closing rename publishes it.
: WORK-OPEN ( -- )
   BUILD-CACHE:ROOT$ s" fixture-writer" MAKE-TEMP-DIR WORK-BUF WORK-U COPY-OUT!
   WORK-BYTES s" hb-fixture-writer" TMP-BUF JOIN-PATH TMP-U ! ;

: WORK-CLOSE ( -- )
   WORK-BYTES REMOVE-TREE ;

: ARG ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

\ tools/app-build.f takes the application source and the output image.
: BUILD-ARGS ( -- )
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" --" ARG
   WRITER$ ARG
   TMP-BYTES ARG ;

: BUILD-RUN ( -- )
   BUILD-ARGS
   ENGINE-CANDIDATE:PATH$ >LEN PROGRAM$ >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN BUILD-TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   OUT outu LEN>N type
   2 ERR erru LEN>N write drop
   rc 0 <> if
      WORK-CLOSE
      s" fixture-writer: writer image build failed" rc die
   then ;

: PUBLISH ( -- )
   TMP-BYTES EXECUTABLE? 0= if
      WORK-CLOSE
      s" fixture-writer: builder produced no executable image" WRITER-RC die
   then
   TMP-BYTES PATH-BYTES RENAME-FILE ;

\ A second builder racing this one saves an image from the same keyed sources
\ and the rename above is atomic, so losing the race costs one discarded build and
\ nothing else. The work directory goes whatever the build did, and a failure
\ keeps its own code: a throw is caught here, and BUILD-RUN and PUBLISH remove
\ the directory themselves before they die, because die ends the process.
: EMIT ( -- )
   WORK-OPEN
   ['] BUILD-RUN catch EMIT-RC !
   EMIT-RC @ 0 = if ['] PUBLISH catch EMIT-RC ! then
   WORK-CLOSE
   EMIT-RC @ 0 <> if EMIT-RC @ throw then ;

public

\ Build the shared writer image unless the keyed artifact is already on disk.
: ENSURE ( -- )
   RESOLVE
   PATH-BYTES EXECUTABLE? if exit then
   EMIT ;

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
   KEY-HEX KEY-HEX-LEN ;

;package
