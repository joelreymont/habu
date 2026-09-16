\ cold-engine.f - the cold fixture engine, emitted once per tree and shared.
\
\ Every partial-capture fixture needs the empty cold host that
\ test/native-fixture-write.f emits from one output argument. That emission
\ recompiles the optimizing native writer from source - 20 s measured, paid
\ thirteen times over a gate run - and its product depends on exactly two
\ things: the engine binary that runs the writer, and the writer's own ordered
\ require/include closure. Both go into the content key below, so the artifact
\ is emitted once into the build cache and every later caller copies it; a tree
\ edit anywhere in that closure changes the key and no stale host is reused.
\
\ PROVIDE is the whole interface: it names the private path a fixture wants its
\ own copy at. The keyed artifact itself is never handed out, so no fixture can
\ run from - or clobber - the shared bytes.

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
require lib/content-key.f
require lib/engine-candidate.f
require tools/event-closure-lib.f

package COLD-ENGINE

$10000 constant IO-CAP
240000 constant WRITER-TIMEOUT-MS
75 constant COLD-RC
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

: PATH-BYTES ( -- ptr u8 n )
   PATH-BUF PATH-U @ ;

: WORK-BYTES ( -- ptr u8 n )
   WORK-BUF WORK-U @ ;

: TMP-BYTES ( -- ptr u8 n )
   TMP-BUF TMP-U @ ;

\ Discovery rejects fail-closed, so a closure that cannot be reproduced cannot
\ be keyed - the key never silently covers fewer files than the build reads.
: CLOSURE-CK+ ( CONTENT-KEY:fold -- CONTENT-KEY:fold )
   WRITER$ EC:BUILD
   0 CLOSURE-IDX !
   begin CLOSURE-IDX @ EC:COUNT < while
      CLOSURE-IDX @ EC:PATH$ CONTENT-KEY:FILE+
      CLOSURE-IDX @ 1+ CLOSURE-IDX !
   repeat ;

: KEY! ( -- )
   CONTENT-KEY:OPEN
   s" cold-fixture-engine-v1" CONTENT-KEY:TEXT+
   ENGINE-CANDIDATE:PATH$ CONTENT-KEY:FILE+
   CLOSURE-CK+
   KEY-HEX CONTENT-KEY:FINAL-HEX ;

: NAME! ( -- )
   s" hb-cold-" {: a:ptr u:n :}
   u KEY-HEX-LEN + NAME-CAP > if E-FS-CAPACITY throw then
   a NAME-BUF u BYTE-COPY
   KEY-HEX NAME-BUF u + KEY-HEX-LEN BYTE-COPY
   u KEY-HEX-LEN + NAME-U ! ;

: PATH! ( -- )
   KEY!
   NAME!
   BUILD-CACHE:ROOT$ NAME-BUF NAME-U @ PATH-BUF JOIN-PATH PATH-U ! ;

: RESOLVE ( -- )
   RESOLVED? @ 0 <> if exit then
   PATH!
   0 0= RESOLVED? ! ;

: COPY-OUT! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr up:ptr :}
   u 0 <= if E-FS-PATH throw then
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   a dst u BYTE-COPY
   u up ! ;

\ The writer emits into a private directory of its own, so a half-written host
\ is never visible at the keyed path: only the closing rename publishes it.
: WORK-OPEN ( -- )
   BUILD-CACHE:ROOT$ s" cold-engine" MAKE-TEMP-DIR WORK-BUF WORK-U COPY-OUT!
   WORK-BYTES s" hb-cold" TMP-BUF JOIN-PATH TMP-U ! ;

: WORK-CLOSE ( -- )
   WORK-BYTES REMOVE-TREE ;

: ARG ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: WRITER-ARGS ( -- )
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" --load" ARG
   WRITER$ ARG
   s" --" ARG
   TMP-BYTES ARG ;

: WRITER-RUN ( -- )
   WRITER-ARGS
   ENGINE-CANDIDATE:PATH$ >LEN s" " >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN WRITER-TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   OUT outu LEN>N type
   2 ERR erru LEN>N write drop
   rc 0 <> if s" cold-engine: native fixture writer failed" rc die then ;

: PUBLISH ( -- )
   TMP-BYTES EXECUTABLE? 0= if
      s" cold-engine: writer produced no executable host" COLD-RC die then
   TMP-BYTES PATH-BYTES RENAME-FILE ;

\ A second builder racing this one writes the same keyed bytes and the rename
\ above is atomic, so losing the race costs one discarded emission and nothing
\ else. The work directory goes whatever the emission did, and a failure keeps
\ its own code.
: EMIT ( -- )
   WORK-OPEN
   ['] WRITER-RUN catch EMIT-RC !
   EMIT-RC @ 0 = if ['] PUBLISH catch EMIT-RC ! then
   WORK-CLOSE
   EMIT-RC @ 0 <> if EMIT-RC @ throw then ;

public

\ Emit the shared cold host unless the keyed artifact is already on disk.
: ENSURE ( -- )
   RESOLVE
   PATH-BYTES EXECUTABLE? if exit then
   EMIT ;

\ The keyed artifact, for a caller that only wants to name it.
: PATH$ ( -- ptr u8 n )
   ENSURE
   PATH-BYTES ;

\ Put a private copy of the shared cold host at the caller's own path.
: PROVIDE ( ptr u8 n -- ) {: dst:ptr dstu:n :}
   ENSURE
   dst dstu EXISTS? if dst dstu REMOVE-FILE then
   PATH-BYTES dst dstu COPY-FILE-STREAM
   dst dstu CHMOD-X ;

;package
