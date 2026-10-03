\ build-fixpoint-test-lib.f - the fixture the build-fixpoint gate rows share.
\ Loaded by tools/build-fixpoint-test.f, tools/build-fixpoint-sandbox-test.f,
\ tools/build-fixpoint-source-test.f and tools/build-fixpoint-snapshot-test.f:
\ the scratch tree, BFT-PREPARE, the capture buffers, the stale-seed sandbox and
\ the step and finish words every driver runs. It defines no MAIN; each row file
\ runs the cases it owns.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-cwd.f
require lib/build.f
require lib/codesign.f
require tools/build-fixpoint.f
require tools/event-closure-lib.f      \ EC:BUILD, used by the sandbox and the chain-key fixtures
require test/suite-budget.f            \ CHILD-MS, every step's hang guard

\ This fixture drives the tool's internals - the emitted stage sources, the
\ stamp preimage, the chain fold - so it REOPENS package BUILD-FIXPOINT rather
\ than importing a public surface. Exporting those internals would widen the
\ tool's own interface for the benefit of its own test. The local fixture
\ scopes this file used to carry (BFT-CAP, BFT-CHAIN, STALE-SEED) were there
\ only because the file had no package of its own; they are ordinary private
\ words of the tool's package now.
package BUILD-FIXPOINT

8192 constant BFT-CAPTURE-CAP
$40000 constant BFT-BIG-CAP
SUITE-BUDGET:CHILD-MS constant BFT-TIMEOUT-MS

variable BFT-ROOT-U
variable BFT-HB-NEW-U
variable BFT-HB-U
variable BFT-PREFIX-U
variable BFT-STAGE2-U
variable BFT-STAMP-U
variable BFT-STAMP2-U
variable BFT-NEST-U
variable BFT-NOTDIR-U
variable BFT-ENG-A-U
variable BFT-ENG-B-U
variable BFT-CERT-U
variable BFT-STALE-U
variable BFT-STALE-HB-U
variable BFT-STALE-TMP-U
variable BFT-STALE-STAMP-U
variable BFT-STALE-PAYLOAD-U
variable BFT-STALE-MARK-U
variable BFT-CP-U
TYPED-VARIABLE BFT-BIG-OUT-A ptr u8
TYPED-VARIABLE BFT-BIG-ERR-A ptr u8
TYPED-VARIABLE BFT-READ-A ptr u8
variable BFT-READ-CAP

create BFT-ROOT-BUF FS-PATH-CAP allot
create BFT-HB-NEW-BUF FS-PATH-CAP allot
create BFT-HB-BUF FS-PATH-CAP allot
create BFT-PREFIX-BUF FS-PATH-CAP allot
create BFT-STAGE2-BUF FS-PATH-CAP allot
create BFT-STAMP-BUF FS-PATH-CAP allot
create BFT-STAMP2-BUF FS-PATH-CAP allot
create BFT-NEST-BUF FS-PATH-CAP allot
create BFT-NOTDIR-BUF FS-PATH-CAP allot
create BFT-ENG-A-BUF FS-PATH-CAP allot
create BFT-ENG-B-BUF FS-PATH-CAP allot
create BFT-CERT-BUF FS-PATH-CAP allot
create BFT-STALE-BUF FS-PATH-CAP allot
create BFT-STALE-HB-BUF FS-PATH-CAP allot
create BFT-STALE-TMP-BUF FS-PATH-CAP allot
create BFT-STALE-STAMP-BUF FS-PATH-CAP allot
create BFT-STALE-PAYLOAD-BUF FS-PATH-CAP allot
create BFT-STALE-MARK-BUF FS-PATH-CAP allot
create BFT-CP-BUF FS-PATH-CAP allot
create BFT-NL 10 c,
create BFT-OUT BFT-CAPTURE-CAP allot
create BFT-ERR BFT-CAPTURE-CAP allot

: BFT-READ-BUF! ( ptr u8 -- )
   BFT-READ-A ! ;

: BFT-READ-BUF ( -- ptr u8 )
   BFT-READ-A @ ;

: BFT-ALLOC-READ ( n -- )
   dup BFT-READ-CAP @ <= if drop exit then
   dup MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop BFT-READ-BUF!
   BFT-READ-CAP ! ;

: BFT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   u lenp ! ;

: BFT-PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: pa:ptr pu na:ptr nu dst:ptr lenp:ptr :}
   pa pu na nu dst JOIN-PATH lenp ! ;

: BFT-ROOT ( -- ptr u8 n )
   BFT-ROOT-BUF BFT-ROOT-U @ ;

: BFT-HB-NEW ( -- ptr u8 n )
   BFT-HB-NEW-BUF BFT-HB-NEW-U @ ;

: BFT-HB ( -- ptr u8 n )
   BFT-HB-BUF BFT-HB-U @ ;

: BFT-PREFIX ( -- ptr u8 n )
   BFT-PREFIX-BUF BFT-PREFIX-U @ ;

: BFT-STAGE2 ( -- ptr u8 n )
   BFT-STAGE2-BUF BFT-STAGE2-U @ ;

: BFT-STAMP ( -- ptr u8 n )
   BFT-STAMP-BUF BFT-STAMP-U @ ;

: BFT-STAMP2 ( -- ptr u8 n )
   BFT-STAMP2-BUF BFT-STAMP2-U @ ;

: BFT-NEST ( -- ptr u8 n )
   BFT-NEST-BUF BFT-NEST-U @ ;

: BFT-NOTDIR ( -- ptr u8 n )
   BFT-NOTDIR-BUF BFT-NOTDIR-U @ ;

: BFT-ENG-A ( -- ptr u8 n )
   BFT-ENG-A-BUF BFT-ENG-A-U @ ;

: BFT-ENG-B ( -- ptr u8 n )
   BFT-ENG-B-BUF BFT-ENG-B-U @ ;

: BFT-CERT ( -- ptr u8 n )
   BFT-CERT-BUF BFT-CERT-U @ ;

: BFT-STALE ( -- ptr u8 n )
   BFT-STALE-BUF BFT-STALE-U @ ;

: BFT-STALE-HB ( -- ptr u8 n )
   BFT-STALE-HB-BUF BFT-STALE-HB-U @ ;

: BFT-STALE-TMP ( -- ptr u8 n )
   BFT-STALE-TMP-BUF BFT-STALE-TMP-U @ ;

: BFT-STALE-STAMP ( -- ptr u8 n )
   BFT-STALE-STAMP-BUF BFT-STALE-STAMP-U @ ;

: BFT-STALE-PAYLOAD ( -- ptr u8 n )
   BFT-STALE-PAYLOAD-BUF BFT-STALE-PAYLOAD-U @ ;

: BFT-STALE-MARK ( -- ptr u8 n )
   BFT-STALE-MARK-BUF BFT-STALE-MARK-U @ ;

: BFT-BIG-OUT ( -- ptr u8 )
   BFT-BIG-OUT-A @ ;

: BFT-BIG-ERR ( -- ptr u8 )
   BFT-BIG-ERR-A @ ;

: BFT-ALLOC-BIG ( -- )
   BFT-BIG-CAP MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop BFT-BIG-OUT-A !
   BFT-BIG-CAP MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop BFT-BIG-ERR-A ! ;

: BFT-EMPTY$ ( -- ptr u8 n )
   SB-RESET
   SB$ ;

: BFT-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: BFT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-build-fixpoint" HB-TMP-MKDIR {: a:ptr u :}
   a u BFT-ROOT-BUF BFT-ROOT-U BFT-COPY!
   BFT-ROOT CLEANUP-TREE+
   BFT-ROOT s" hb-new" BFT-HB-NEW-BUF BFT-HB-NEW-U BFT-PATH!
   BFT-ROOT s" hb-stdin" BFT-HB-BUF BFT-HB-U BFT-PATH!
   s" bin/hb" BFT-HB COPY-FILE-STREAM
   BFT-HB CHMOD-X
   BFT-ROOT s" prefix-src" BFT-PREFIX-BUF BFT-PREFIX-U BFT-PATH!
   BFT-ROOT s" stage2-src" BFT-STAGE2-BUF BFT-STAGE2-U BFT-PATH!
   BFT-ROOT s" fixpoint-stamp" BFT-STAMP-BUF BFT-STAMP-U BFT-PATH!
   BFT-ROOT s" fixpoint-stamp2" BFT-STAMP2-BUF BFT-STAMP2-U BFT-PATH!
   BFT-ROOT s" nested/stamps/stamp" BFT-NEST-BUF BFT-NEST-U BFT-PATH!
   BFT-ROOT s" not-a-dir" BFT-NOTDIR-BUF BFT-NOTDIR-U BFT-PATH!
   BFT-ROOT s" engine-a" BFT-ENG-A-BUF BFT-ENG-A-U BFT-PATH!
   BFT-ROOT s" engine-b" BFT-ENG-B-BUF BFT-ENG-B-U BFT-PATH!
   BFT-ROOT s" cert-source.f" BFT-CERT-BUF BFT-CERT-U BFT-PATH!
   BFT-NOTDIR s" plain file, not a directory" WRITE-ALL ;

: BFT-ARGV-LOAD-LIBS ( -- )   \ the --load prefix through build-fixpoint.f, WITHOUT the CLI entry companion
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f" BFT-ARG+
   s" lib/string.f" BFT-ARG+
   s" lib/memory.f" BFT-ARG+
   s" lib/fs.f" BFT-ARG+
   s" lib/fs-mutate.f" BFT-ARG+
   s" lib/process.f" BFT-ARG+
   s" lib/process-argv.f" BFT-ARG+
   s" lib/process-env.f" BFT-ARG+
   s" lib/build.f" BFT-ARG+
   s" lib/codesign.f" BFT-ARG+
   s" tools/build-fixpoint.f" BFT-ARG+ ;

: BFT-ARGV-LOAD-FILES ( -- )
   BFT-ARGV-LOAD-LIBS
   s" tools/build-fixpoint-main.f" BFT-ARG+ ;

: BFT-CAPTURE>N ( result<pcap:captured,pcap:failed> -- n n n )   \ outn errn code (0 on clean exit)
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} o LEN>N e LEN>N 0 ENDOF
     err OF PCAP-FAILED:UNMAKE  {: o:len e:len c:rc :} o LEN>N e LEN>N c RC>N ENDOF
   ;MATCH ;

\ build-fixpoint exits PROC-TIMEOUT-RC when a deadline expired under it (BF-CLI,
\ lib/process.f). Replay the child's stderr, which names the throw, and throw
\ E-PROC-TIMEOUT again, so BFT-STEP hands the row to the gate pool's timeout
\ label instead of failing it as one more unexpected exit code.
: BFT-FIXPOINT-RC ( n n n ptr u8 -- n n n ) {: outn:n errn:n code:n err:ptr :}
   code PROC-TIMEOUT-RC = if err errn type E-PROC-TIMEOUT throw then
   outn errn code ;

: BFT-READ ( ptr u8 n -- n ) {: pa:ptr pu:n :}
   pa pu FILE-SIZE BFT-ALLOC-READ
   pa pu BFT-READ-BUF BFT-READ-CAP @ READ-ALL ;

\ The stale-seed sandbox: a private copy of src/, lib/, the refresh's tools/
\ closure and bin/hb, with its own tmp and stamp, so a case can break a source
\ file and run the refresh there - the real workspace bin/hb is never touched.
\ The sandbox cases (tools/build-fixpoint-sandbox-test.f) plant a crash or a
\ type error in its copy of src/arch/arm64/mnem.f; the watermark cases
\ (tools/build-fixpoint-test.f) cut its copy of src/core/lower-cert-seal.f.
: BFT-STALE-DST ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   BFT-STALE a u SOURCE-ROOT:CWD$ SOURCE-ROOT:RELATIVE
   BFT-CP-BUF JOIN-PATH BFT-CP-U !
   BFT-CP-BUF BFT-CP-U @ ;

: BFT-STALE-COPY-FILE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u BFT-STALE-DST {: d:ptr du:n :}
   d du BF-PARENT-U {: pu:n :}
   pu 0 > if d pu MAKE-DIRS then
   a u d du COPY-FILE-STREAM ;

: BFT-STALE-COPY-ENTRY ( ptr u8 n -- ) {: a:ptr u:n :}
   a u FILE? if a u BFT-STALE-COPY-FILE then ;

: BFT-STALE-COPY-TREE ( ptr u8 n -- )
   [: BFT-STALE-COPY-ENTRY ;] WALK-FILES ;

: BFT-STALE-PATHS! ( -- )
   s" habu-bft-stale" HB-TMP-MKDIR {: a:ptr u:n :}
   a u BFT-STALE-BUF BFT-STALE-U BFT-COPY!
   BFT-STALE CLEANUP-TREE+
   BFT-STALE s" bin/hb" BFT-STALE-HB-BUF BFT-STALE-HB-U BFT-PATH!
   BFT-STALE s" tmp" BFT-STALE-TMP-BUF BFT-STALE-TMP-U BFT-PATH!
   BFT-STALE s" stamp" BFT-STALE-STAMP-BUF BFT-STALE-STAMP-U BFT-PATH!
   BFT-STALE s" src/arch/arm64/mnem.f" BFT-STALE-PAYLOAD-BUF BFT-STALE-PAYLOAD-U BFT-PATH!
   BFT-STALE s" src/core/lower-cert-seal.f" BFT-STALE-MARK-BUF BFT-STALE-MARK-U BFT-PATH! ;

\ The sandbox needs every tools/ file the refresh loads. That list used to be
\ written out by hand and went stale the moment build-fixpoint.f grew a require:
\ the sandboxed refresh then died on a missing file instead of on the fault the
\ test was about. Ask the source instead - the same ordered closure walk the
\ stamp key uses - so the sandbox tracks the tool's own requires. src/ and lib/
\ still come over whole, because the stage build reads far more of them than
\ build-fixpoint.f's own requires name.
\ TWO CLOSURES, and the second is the same lesson a second time. The stamp key
\ now opens the CAPTURE TOOL's closure as well, before it consults --force and
\ before anything is written, so a sandbox without it dies -2102 (E-FS-OPEN)
\ ahead of every fault these fixtures inject: the stale-seed crash and the
\ certify injection both came back as a bare uncaught throw. The entry is asked
\ from the tool - ENTRY$ - rather than spelled here, so whatever the key walks
\ is what the sandbox carries.

variable IX

: COPY-CLOSURE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u EC:BUILD
   0 IX !
   begin IX @ EC:COUNT < while
      IX @ EC:PATH$ BFT-STALE-COPY-ENTRY
      IX @ 1+ IX !
   repeat ;

: BFT-STALE-PREPARE ( -- )
   BFT-STALE-PATHS!
   BFT-ALLOC-BIG
   BFT-STALE-TMP MAKE-DIRS
   s" src" BFT-STALE-COPY-TREE
   s" lib" BFT-STALE-COPY-TREE
   s" tools/build-fixpoint.f" COPY-CLOSURE
   ENTRY$ COPY-CLOSURE
   s" tools/build-fixpoint-main.f" BFT-STALE-COPY-FILE
   s" bin/hb" BFT-STALE-COPY-FILE
   BFT-STALE-HB CHMOD-X ;

: BFT-STALE-ARGV ( -- )
   PROC-ARGV-RESET
   PROC-ENV-RESET
   s" HB_TMP" >LEN BFT-STALE-TMP >LEN PROC-ENV+
   s" HABU_FIXPOINT_STAMP" >LEN BFT-STALE-STAMP >LEN PROC-ENV+
   BFT-ARGV-LOAD-FILES
   s" --" BFT-ARG+
   s" install" BFT-ARG+
   s" --force" BFT-ARG+ ;

: BFT-STALE-SPAWN ( -- n n n )
   BFT-STALE-HB >LEN BFT-STALE >LEN
   BFT-BIG-OUT BFT-BIG-CAP >LEN
   BFT-BIG-ERR BFT-BIG-CAP >LEN
   BFT-TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE
   BFT-CAPTURE>N BFT-BIG-ERR BFT-FIXPOINT-RC ;

\ A child that outlives BFT-TIMEOUT-MS (E-PROC-TIMEOUT from lib/process) leaves
\ the row uncaught once its step is named: the engine's report is then the row's
\ last stderr line, which the gate pool labels TIMEOUT-UNDER-LOAD (test/gate-pool.f
\ GT-POOL-INNER-TIMEOUT?). Every other throw ends the row here as a failed step.
: BFT-STEP ( ptr u8 n [ -- ] -- ) {: a:ptr u:n q :}
   a u T-LABEL
   q catch {: rc:n :}
   rc 0= if exit then
   a u type s" : throw " type rc . cr
   rc E-PROC-TIMEOUT = if rc throw then
   s" build-fixpoint-test-lib: subtest threw" T-EX-FAIL die ;

\ The common driver tail: every row removes its scratch tree, then reports.
: BFT-FINISH ( ptr u8 n -- ) {: msg:ptr msgu:n :}
   CLEANUP-RUN
   BFT-ROOT EXISTS? TFALSE
   T-REPORT
   msg msgu type cr ;

;package
