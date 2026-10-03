\ whitebox-engine.f - the unsealed engine whitebox suites run on, built once
\ per gate and shared.
\
\ A whitebox suite reaches inside the engine it tests: it reopens an engine
\ package, names a pre-hook global, ticks an internal word. The shipped image
\ closes every one of those doors - src/core/internal-mark.f marks each record
\ with no checker-known effect DNAME-INT, and interpret and tick then fail
\ closed on the bare token. A TRUSTED: wrapper does not reopen them, because the
\ refusal is on the token and not on the body around it.
\
\ So such a suite runs on an engine built WITHOUT that pass instead: the same
\ tools/native-build.f image the product is, from the same tree, with
\ HABU_WHITEBOX_IMAGE=1 telling the seal pass to stand down. It is built once,
\ into the build cache under its content key, and every caller copies it;
\ it is never installed and never becomes bin/hb, so the product engine the rest
\ of the gate runs on keeps its seal and test/internal-word-gate.f keeps pinning
\ that.
\
\ The key and the name are test/whitebox-key.f's: an edit anywhere in the
\ builder's closure, or another engine, names another file, and no stale host
\ is reused.
\
\ PROVIDE names the private path a caller wants its own copy at, so no suite
\ runs from - or clobbers - the shared bytes; PATH$ names the keyed artifact
\ itself for a caller that only reads it. Under a gate the engine must be
\ granted (test/image-grant.f): the gate's whitebox-engine build row settles it
\ and copies it for the WHITEBOX-SUITE rows (test/gate-images.f), and a row
\ that fetches its own copy starts only after that row passed.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-root.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/build-cache.f
require lib/engine-candidate.f
require test/image-grant.f
require test/keyed-image.f               \ the build's CPU budget and hang guard
require test/whitebox-key.f

package WHITEBOX-ENGINE

$10000 constant IO-CAP

create PATH-BUF FS-PATH-CAP allot
create WORK-BUF FS-PATH-CAP allot
create TMP-BUF FS-PATH-CAP allot
create OUT IO-CAP allot
create ERR IO-CAP allot

variable PATH-U
variable WORK-U
variable WORK-FD
variable TMP-U
variable EMIT-RC
variable RESOLVED?

\ The exit status of a build that produced no executable image.
75 constant WB-RC

\ The variable the build-time passes that shape the shipped image read to learn
\ that this one is for whitebox suites: src/core/internal-mark.f's seal, and any
\ later pass with the same question. It is set for this build and nothing else,
\ so it is spelled in those files and in this one, and in no shipped engine's
\ boot path at all.
: SWITCH$ ( -- ptr u8 n )
   s" HABU_WHITEBOX_IMAGE" ;

: PATH-BYTES ( -- ptr u8 n )
   PATH-BUF PATH-U @ ;

: WORK-BYTES ( -- ptr u8 n )
   WORK-BUF WORK-U @ ;

: TMP-BYTES ( -- ptr u8 n )
   TMP-BUF TMP-U @ ;

\ The family name: the prefix of the build's work directory, and the name a gate
\ grants the engine under (test/image-grant.f).
: WORK-PREFIX$ ( -- ptr u8 n )
   s" whitebox-engine" ;

: RESOLVE ( -- )
   RESOLVED? @ 0 <> if exit then
   WHITEBOX-KEY:BUILDER$ PATH-BUF PATH-U WHITEBOX-KEY:ENTRY-PATH!
   0 0= RESOLVED? ! ;

: COPY-OUT! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr up:ptr :}
   u 0 <= if E-FS-PATH throw then
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   a dst u BYTE-COPY
   u up ! ;

\ The builder writes into a private directory of its own, so a half-written
\ image is never visible at the keyed path: only the closing rename publishes it.
\ It is held until WORK-CLOSE and registered for removal at exit, so a die
\ before WORK-CLOSE still removes it, and the next build removes it after a
\ kill (BUILD-CACHE:WORK-OPEN).
: WORK-OPEN ( -- )
   WORK-PREFIX$ BUILD-CACHE:WORK-OPEN FD>N WORK-FD !
   WORK-BUF WORK-U COPY-OUT!
   WORK-BYTES s" hb-whitebox" TMP-BUF JOIN-PATH TMP-U ! ;

: WORK-CLOSE ( -- )
   WORK-BYTES WORK-FD @ >FD BUILD-CACHE:WORK-CLOSE ;

: ARG ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

\ TWO HALVES, AND NEITHER ALONE BUILDS THIS. The variable reaches the seal pass,
\ which runs deep inside the target load where no argument can be handed to it;
\ the `whitebox` argument is what the builder checks the finished image against
\ before it promotes anything, so an ambient variable cannot turn some other
\ build's product into this. Both are set here, deliberately, for this one run.
\
\ The variable goes in BEFORE the inherit, so the one row the child reads is
\ this one whatever the parent's own environment says.
: BUILD-ARGS ( -- )
   PROC-ARGV-ENV-RESET
   SWITCH$ >LEN s" 1" >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --load" ARG
   WHITEBOX-KEY:BUILDER$ ARG
   s" --" ARG
   TMP-BYTES ARG
   s" whitebox" ARG ;

: BUILD-RUN ( -- )
   BUILD-ARGS
   ENGINE-CANDIDATE:PATH$ >LEN s" " >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN KEYED-IMAGE:BUILD-TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   OUT outu LEN>N type
   2 ERR erru LEN>N write drop
   \ A deadline that expired, this capture's or one in the build
   \ (tools/native-build-args.f), is named and thrown again, so the gate pool
   \ labels this build row TIMEOUT-UNDER-LOAD.
   rc PROC-TIMEOUT-RC = if
      s" whitebox-engine: unsealed engine build ran out of time" type cr
      E-PROC-TIMEOUT throw
   then
   rc 0 <> if s" whitebox-engine: unsealed engine build failed" rc die then ;

: PUBLISH ( -- )
   TMP-BYTES EXECUTABLE? 0= if
      s" whitebox-engine: builder produced no executable image" WB-RC die
   then
   TMP-BYTES PATH-BYTES RENAME-FILE ;

\ A second builder racing this one writes the same keyed bytes and the rename
\ above is atomic, so losing the race costs one discarded build and nothing
\ else. The work directory goes whatever the build did, and a failure keeps its
\ own code: a throw is caught here, and a die in BUILD-RUN or PUBLISH ends the
\ process, whose exit registry removes what WORK-OPEN registered. A published
\ engine then prunes its family (BUILD-CACHE:PRUNE), which reports its own
\ failures and never fails the build. WORK-PREFIX$ is also the stem of the
\ whitebox-engine-<seed>-<attempt> work directories a builder that holds none
\ made, which pruning takes once they are a day old.
: EMIT ( -- )
   WORK-OPEN
   ['] BUILD-RUN catch EMIT-RC !
   EMIT-RC @ 0 = if ['] PUBLISH catch EMIT-RC ! then
   WORK-CLOSE
   EMIT-RC @ 0 <> if EMIT-RC @ throw then
   WHITEBOX-KEY:IMAGE-PREFIX$ s" " WORK-PREFIX$ PATH-BYTES BUILD-CACHE:PRUNE ;

public

\ Build the shared unsealed engine unless the keyed artifact is already on disk,
\ once the engine is granted (IMAGE-GRANT:CHECK; the family name is the work
\ prefix). An engine found there is marked in use (BUILD-CACHE:USED), and one
\ a pruner took meanwhile is built again.
: ENSURE ( -- )
   WORK-PREFIX$ IMAGE-GRANT:CHECK
   RESOLVE
   PATH-BYTES EXECUTABLE? if PATH-BYTES BUILD-CACHE:USED if exit then then
   EMIT ;

\ The keyed artifact, for a caller that only wants to name it.
: PATH$ ( -- ptr u8 n )
   ENSURE
   PATH-BYTES ;

\ Put a private copy of the shared unsealed engine at the caller's own path.
: PROVIDE ( ptr u8 n -- ) {: dst:ptr dstu:n :}
   ENSURE
   dst dstu EXISTS? if dst dstu REMOVE-FILE then
   PATH-BYTES dst dstu COPY-FILE-STREAM
   dst dstu CHMOD-X ;

;package
