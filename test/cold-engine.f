\ cold-engine.f - the cold fixture engine, emitted once per tree and shared.
\
\ Every partial-capture fixture needs the empty cold host that
\ test/native-fixture-write.f emits from one output argument. That emission is a
\ run of the writer image test/fixture-writer.f builds, so the host depends on
\ exactly one thing: that image, whose key covers the engine that builds it and
\ the ordered require/include closures of its builder and of the writer. The
\ host's own key is derived from that key, so the artifact is emitted once into
\ the build cache and every later caller copies it; another engine or a tree
\ edit anywhere in those closures changes the key and no stale host is reused.
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
require test/fixture-writer.f
require test/image-grant.f

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
variable WORK-FD
variable TMP-U
variable EMIT-RC
variable RESOLVED?

: PATH-BYTES ( -- ptr u8 n )
   PATH-BUF PATH-U @ ;

: WORK-BYTES ( -- ptr u8 n )
   WORK-BUF WORK-U @ ;

: TMP-BYTES ( -- ptr u8 n )
   TMP-BUF TMP-U @ ;

: KEY! ( -- )
   FIXTURE-WRITER:KEY$ {: writer:ptr writeru:n :}
   CONTENT-KEY:OPEN
   s" cold-fixture-engine-v3" CONTENT-KEY:TEXT+
   writer writeru CONTENT-KEY:TEXT+
   KEY-HEX CONTENT-KEY:FINAL-HEX ;

: IMAGE-PREFIX$ ( -- ptr u8 n )
   s" hb-cold-" ;

: WORK-PREFIX$ ( -- ptr u8 n )
   s" cold-engine" ;

: NAME! ( -- )
   IMAGE-PREFIX$ {: a:ptr u:n :}
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
\ It is held until WORK-CLOSE and registered for removal at exit, so a die
\ before WORK-CLOSE still removes it, and the next build removes it after a
\ kill (BUILD-CACHE:WORK-OPEN).
: WORK-OPEN ( -- )
   WORK-PREFIX$ BUILD-CACHE:WORK-OPEN FD>N WORK-FD !
   WORK-BUF WORK-U COPY-OUT!
   WORK-BYTES s" hb-cold" TMP-BUF JOIN-PATH TMP-U ! ;

: WORK-CLOSE ( -- )
   WORK-BYTES WORK-FD @ >FD BUILD-CACHE:WORK-CLOSE ;

: ARG ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: WRITER-ARGS ( -- )
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" --" ARG
   TMP-BYTES ARG ;

\ The writer path comes first: see FIXTURE-WRITER:PATH$.
: WRITER-RUN ( -- )
   FIXTURE-WRITER:PATH$ {: writer:ptr writeru:n :}
   WRITER-ARGS
   writer writeru >LEN s" " >LEN
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
\ its own code: a throw is caught here, and a die in WRITER-RUN or PUBLISH ends
\ the process, whose exit registry removes what WORK-OPEN registered. A
\ published host then prunes its family (BUILD-CACHE:PRUNE), which reports
\ its own failures and never fails the emission. WORK-PREFIX$ is also the stem
\ of the cold-engine-<seed>-<attempt> work directories a builder that holds
\ none made, which pruning takes once they are a day old.
: EMIT ( -- )
   WORK-OPEN
   ['] WRITER-RUN catch EMIT-RC !
   EMIT-RC @ 0 = if ['] PUBLISH catch EMIT-RC ! then
   WORK-CLOSE
   EMIT-RC @ 0 <> if EMIT-RC @ throw then
   IMAGE-PREFIX$ s" " WORK-PREFIX$ PATH-BYTES BUILD-CACHE:PRUNE ;

public

\ Settle the shared writer image and the cold host it emits, each built only
\ when its keyed artifact is not already on disk. The writer is settled even when
\ the host is present: every fixture write runs it. Under a gate the host must
\ be granted (IMAGE-GRANT:CHECK; the family name is the work prefix), and the
\ gate's cold-engine build row settles both before a row that needs them starts
\ (test/gate-images.f). A host found on disk is marked in use
\ (BUILD-CACHE:USED), and one a pruner took meanwhile is emitted again.
: ENSURE ( -- )
   WORK-PREFIX$ IMAGE-GRANT:CHECK
   FIXTURE-WRITER:ENSURE
   RESOLVE
   PATH-BYTES EXECUTABLE? if PATH-BYTES BUILD-CACHE:USED if exit then then
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
