\ cold-engine.f - the cold fixture engine, settled once per gate run,
\ emitted on a key miss and shared across runs.
\
\ Every partial-capture fixture needs the empty cold host that
\ test/native-fixture-write.f emits from one output argument. That emission is a
\ run of the writer image test/fixture-writer.f builds, so the host depends on
\ exactly one thing: that image, whose key covers the engine that builds it and
\ the ordered require/include closures of its builder and of the writer. The
\ host's own key is derived from that key, so the artifact is emitted once into
\ img/cold-engine and every later caller copies it; another engine or a tree
\ edit anywhere in those closures changes the key and no stale host is reused.
\
\ PROVIDE is the whole interface: it names the private path a fixture wants its
\ own copy at. The keyed artifact itself is never handed out, so no fixture can
\ run from - or clobber - the shared bytes.

require src/lib/errors.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/content-key.f
require tools/build-out.f
require tools/keyed-image.f
require test/fixture-writer.f
require test/image-grant.f
require test/suite-budget.f              \ CHILD-MS, the emission's hang guard

package COLD-ENGINE

$10000 constant IO-CAP
75 constant COLD-RC

create KEY-HEX KEYED-IMAGE:KEY-HEX-LEN allot
create ENGINE-KEY KEYED-IMAGE:KEY-HEX-LEN allot   \ the writer image's, naming its img/
create PATH-BUF PATH-CAP allot
create WORK-BUF PATH-CAP allot
create TMP-BUF PATH-CAP allot
create OUT IO-CAP allot
create ERR IO-CAP allot

variable PATH-U
variable WORK-U
variable TMP-U
variable EMIT-RC
variable RESOLVED?

: PATH-BYTES ( -- ptr u8 n )
   PATH-BUF PATH-U @ ;

: WORK-BYTES ( -- ptr u8 n )
   WORK-BUF WORK-U @ ;

: TMP-BYTES ( -- ptr u8 n )
   TMP-BUF TMP-U @ ;

: KEY-BODY ( ptr u8 n CONTENT-KEY:fold -- )
   s" cold-fixture-engine-v3" CONTENT-KEY:TEXT+
   -rot CONTENT-KEY:TEXT+
   KEY-HEX CONTENT-KEY:FINAL-HEX ;

: KEY! ( -- )
   FIXTURE-WRITER:KEY$ [: KEY-BODY ;] CONTENT-KEY:WITH-FOLD ;

\ The family name: the image's name under img/, the stem of its work
\ directories, and the name a gate grants the host under (test/image-grant.f).
: FAMILY$ ( -- ptr u8 n )
   s" cold-engine" ;

: RESOLVE ( -- )
   RESOLVED? @ 0 <> if exit then
   KEY!
   FIXTURE-WRITER:PATH$ ENGINE-KEY BUILD-OUT:KEY-OF!
   FAMILY$ ENGINE-KEY PATH-BUF PATH-U KEYED-IMAGE:PATH!
   0 0= RESOLVED? ! ;

: COPY-OUT! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr up:ptr :}
   u 0 <= if E-FS-PATH throw then
   u PATH-CAP > if E-FS-CAPACITY throw then
   a dst u BYTE-COPY
   u up ! ;

\ The writer emits into a work directory of its own (KEYED-IMAGE:WORK-OPEN),
\ so a half-written host is never visible at the family's path: only
\ KEYED-IMAGE:PUBLISH puts it there.
: WORK-OPEN ( -- )
   FAMILY$ ENGINE-KEY KEYED-IMAGE:WORK-OPEN WORK-BUF WORK-U COPY-OUT!
   WORK-BYTES FAMILY$ TMP-BUF FS:JOIN-PATH TMP-U ! ;

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
   OUT IO-CAP >LEN ERR IO-CAP >LEN SUITE-BUDGET:CHILD-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
   {: outu:len erru:len rc:n :}
   OUT outu LEN>N type
   2 ERR erru LEN>N write drop
   \ An expired deadline is named and thrown again, so the gate pool labels the
   \ cold-engine build row TIMEOUT-UNDER-LOAD.
   rc PROC-TIMEOUT-RC = if
      s" cold-engine: native fixture writer ran out of time" type cr
      E-PROC-TIMEOUT throw
   then
   rc 0 <> if s" cold-engine: native fixture writer failed" rc die then ;

: PUBLISH ( -- )
   TMP-BYTES FS:EXECUTABLE? 0= if
      s" cold-engine: writer produced no executable host" COLD-RC die then
   KEY-HEX FAMILY$ ENGINE-KEY TMP-BYTES KEYED-IMAGE:PUBLISH ;

\ A second builder racing this one writes the same keyed bytes through
\ KEYED-IMAGE:PUBLISH's atomic replaces, so the race costs one redundant
\ emission and fails neither. The work directory goes whatever the emission
\ did, and a failure keeps its own code: a throw is caught here, and a die in
\ WRITER-RUN or PUBLISH ends the process, whose exit registry removes what
\ KEYED-IMAGE:WORK-OPEN registered.
: EMIT ( -- )
   WORK-OPEN
   ['] WRITER-RUN catch EMIT-RC !
   EMIT-RC @ 0 = if ['] PUBLISH catch EMIT-RC ! then
   WORK-BYTES KEYED-IMAGE:WORK-CLOSE
   EMIT-RC @ 0 <> if EMIT-RC @ throw then ;

public

\ Settle the shared writer image and the cold host it emits, each built only
\ when the one on disk was not built under its key. The writer is settled even
\ when the host is fresh: every fixture write runs it. Under a gate the host
\ must be granted (IMAGE-GRANT:CHECK under FAMILY$), and the gate's
\ cold-engine build row settles both before a row that needs them starts
\ (test/gate-images.f).
: ENSURE ( -- )
   FAMILY$ IMAGE-GRANT:CHECK
   FIXTURE-WRITER:ENSURE
   RESOLVE
   KEY-HEX FAMILY$ ENGINE-KEY KEYED-IMAGE:FRESH? if exit then
   EMIT ;

\ The family's host, for a caller that only wants to name it.
: PATH$ ( -- ptr u8 n )
   ENSURE
   PATH-BYTES ;

\ Put a private copy of the shared cold host at the caller's own path.
: PROVIDE ( ptr u8 n -- ) {: dst:ptr dstu:n :}
   ENSURE
   dst dstu FS:EXISTS? if dst dstu FS:REMOVE-FILE then
   PATH-BYTES dst dstu FS:COPY-FILE-STREAM
   dst dstu FS:CHMOD-X ;

;package
