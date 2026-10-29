\ fixture-writer.f - the native fixture writer (test/native-fixture-write.f) as
\ an application image tools/app-build.f builds on a key miss, run in place: as
\ a script it compiled tools/native-emit.f at tier 1 per write, 22.8 s against
\ 0.4 s for the emission. The key covers the engine binary, the builder's
\ program and both ordered require/include closures (test/keyed-image.f). PATH$
\ hands out the image itself: it writes only the paths its caller names.

require src/lib/errors.f
require src/lib/fs.f
require src/lib/process-argv.f
require src/lib/content-key.f
require src/lib/engine-candidate.f
require tools/build-out.f
require test/keyed-image.f

package FIXTURE-WRITER

create KEY-HEX KEYED-IMAGE:KEY-HEX-LEN allot
create ENGINE-KEY KEYED-IMAGE:KEY-HEX-LEN allot   \ the candidate's, naming its img/
create PATH-BUF PATH-CAP allot

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

: KEY-BODY ( CONTENT-KEY:fold -- )
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

: KEY! ( -- )
   [: KEY-BODY ;] CONTENT-KEY:WITH-FOLD ;

: RESOLVE ( -- )
   RESOLVED? @ 0 <> if exit then
   KEY!
   ENGINE-CANDIDATE:PATH$ ENGINE-KEY BUILD-OUT:KEY-OF!
   FAMILY$ ENGINE-KEY PATH-BUF PATH-U KEYED-IMAGE:PATH!
   0 0= RESOLVED? ! ;

\ tools/app-build.f takes the application source and the output image.
: BUILD-ARGS ( ptr u8 n -- ) {: out:ptr outu:n :}
   WRITER$ >LEN PROC-ARGV+
   out outu >LEN PROC-ARGV+ ;

public

: ENSURE ( -- )
   RESOLVE
   KEY-HEX FAMILY$ ENGINE-KEY ENGINE-CANDIDATE:PATH$ PROGRAM$ ['] BUILD-ARGS
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
