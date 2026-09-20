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
\ into the build cache under the content key below, and every caller copies it;
\ it is never installed and never becomes bin/hb, so the product engine the rest
\ of the gate runs on keeps its seal and test/internal-word-gate.f keeps pinning
\ that.
\
\ THE KEY IS WHAT THE IMAGE IS BUILT FROM: the engine binary that runs the
\ builder, and the builder's own ordered require/include closure - which is the
\ engine's whole boot prefix, because tools/native-build.f names every prefix
\ source it compiles. Both go into the content key below, exactly as
\ test/cold-engine.f keys the cold host, so an edit anywhere in that closure
\ changes the key and no stale host is reused. Keying on the installed binary
\ alone made the artifact track bin/hb instead of the checkout: a tree whose
\ engine sources changed without a reinstall got a whitebox host built from the
\ older sources.
\
\ PROVIDE is the whole interface: it names the private path a caller wants its
\ own copy at. The keyed artifact itself is never handed out, so no suite can
\ run from - or clobber - the shared bytes. ENTRY-PATH! is the same derivation
\ for a caller that names its own entry and buffers;
\ test/whitebox-engine-key-test.f keys a copied tree with it.

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

package WHITEBOX-ENGINE

$10000 constant IO-CAP
360000 constant BUILD-TIMEOUT-MS
75 constant WB-RC
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

public

\ The builder this host is built from, and whose closure the key folds. Public
\ because test/whitebox-engine-key-test.f copies exactly this entry's closure: a
\ second spelling of the path there would go on keying the old entry after a
\ rename.
: BUILDER$ ( -- ptr u8 n )
   s" tools/native-build.f" ;

private

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

\ Discovery rejects fail-closed, so a closure that cannot be reproduced cannot be
\ keyed - the key never silently covers fewer files than the build reads.
: CLOSURE-CK+ ( CONTENT-KEY:fold ptr u8 n -- CONTENT-KEY:fold ) {: a:ptr u:n :}
   a u EC:BUILD
   0 CLOSURE-IDX !
   begin CLOSURE-IDX @ EC:COUNT < while
      CLOSURE-IDX @ EC:PATH$ CLOSURE-IDX @ EC:NAME$ CONTENT-KEY:FILE-NAMED+
      CLOSURE-IDX @ 1+ CLOSURE-IDX !
   repeat ;

: KEY! ( ptr u8 n -- ) {: a:ptr u:n :}
   CONTENT-KEY:OPEN
   s" whitebox-engine-v3" CONTENT-KEY:TEXT+
   ENGINE-CANDIDATE:PATH$ s" host-engine" CONTENT-KEY:FILE-NAMED+
   a u CLOSURE-CK+
   KEY-HEX CONTENT-KEY:FINAL-HEX ;

: NAME! ( -- )
   s" hb-whitebox-" {: a:ptr u:n :}
   u KEY-HEX-LEN + NAME-CAP > if E-FS-CAPACITY throw then
   a NAME-BUF u BYTE-COPY
   KEY-HEX NAME-BUF u + KEY-HEX-LEN BYTE-COPY
   u KEY-HEX-LEN + NAME-U ! ;

public

\ The keyed artifact path a builder entry resolves to, written into the caller's
\ own buffer (FS-PATH-CAP bytes) and length cell. RESOLVE names the tree's
\ builder; test/whitebox-engine-key-test.f names a copied tree's, which is why
\ the derivation takes both from the caller instead of reading this module's.
: ENTRY-PATH! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr up:ptr :}
   a u KEY!
   NAME!
   BUILD-CACHE:ROOT$ NAME-BUF NAME-U @ dst JOIN-PATH up ! ;

private

: RESOLVE ( -- )
   RESOLVED? @ 0 <> if exit then
   BUILDER$ PATH-BUF PATH-U ENTRY-PATH!
   0 0= RESOLVED? ! ;

: COPY-OUT! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr up:ptr :}
   u 0 <= if E-FS-PATH throw then
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   a dst u BYTE-COPY
   u up ! ;

\ The builder writes into a private directory of its own, so a half-written
\ image is never visible at the keyed path: only the closing rename publishes it.
: WORK-OPEN ( -- )
   BUILD-CACHE:ROOT$ s" whitebox-engine" MAKE-TEMP-DIR WORK-BUF WORK-U COPY-OUT!
   WORK-BYTES s" hb-whitebox" TMP-BUF JOIN-PATH TMP-U ! ;

: WORK-CLOSE ( -- )
   WORK-BYTES REMOVE-TREE ;

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
   BUILDER$ ARG
   s" --" ARG
   TMP-BYTES ARG
   s" whitebox" ARG ;

: BUILD-RUN ( -- )
   BUILD-ARGS
   ENGINE-CANDIDATE:PATH$ >LEN s" " >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN BUILD-TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   OUT outu LEN>N type
   2 ERR erru LEN>N write drop
   rc 0 <> if s" whitebox-engine: unsealed engine build failed" rc die then ;

: PUBLISH ( -- )
   TMP-BYTES EXECUTABLE? 0= if
      s" whitebox-engine: builder produced no executable image" WB-RC die then
   TMP-BYTES PATH-BYTES RENAME-FILE ;

\ A second builder racing this one writes the same keyed bytes and the rename
\ above is atomic, so losing the race costs one discarded build and nothing
\ else. The work directory goes whatever the build did, and a failure keeps its
\ own code.
: EMIT ( -- )
   WORK-OPEN
   ['] BUILD-RUN catch EMIT-RC !
   EMIT-RC @ 0 = if ['] PUBLISH catch EMIT-RC ! then
   WORK-CLOSE
   EMIT-RC @ 0 <> if EMIT-RC @ throw then ;

public

\ Build the shared unsealed engine unless the keyed artifact is already on disk.
: ENSURE ( -- )
   RESOLVE
   PATH-BYTES EXECUTABLE? if exit then
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
