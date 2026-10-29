\ build-fixpoint-test-lib.f - the fixture the build-fixpoint gate rows share.
\ Loaded by tools/build-fixpoint-test.f, tools/build-fixpoint-sandbox-test.f
\ and tools/build-fixpoint-source-test.f:
\ the scratch tree, BFT-PREPARE, the capture buffers, the stale-seed sandbox and
\ the step and finish words every driver runs. It defines no MAIN; each row file
\ runs the cases it owns.

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/process-cwd.f
require src/lib/build.f
require src/lib/codesign.f
require src/lib/tree-copy.f                \ the stale-seed sandbox's private tree
require tools/build-fixpoint.f
require tools/event-closure-lib.f      \ EC:BUILD, used by the sandbox and the chain-key fixtures

\ The fixture drives the tool's internals (stage sources, stamp preimage, chain
\ fold), so it reopens package BUILD-FIXPOINT instead of widening the tool's
\ interface for its own test.
package BUILD-FIXPOINT

8192 constant BFT-CAPTURE-CAP
$40000 constant BFT-BIG-CAP

variable BFT-ROOT-U
variable BFT-HB-U
variable BFT-PREFIX-U
variable BFT-STAGE2-U
variable BFT-OUTDIR-U
variable BFT-OUTDIR2-U
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
variable BFT-STALE-OUTDIR-U
variable BFT-STALE-STAMP-U
variable BFT-STALE-PAYLOAD-U
variable BFT-STALE-MARK-U
variable BFT-CP-U
TYPED-VARIABLE BFT-BIG-OUT-A ptr u8
TYPED-VARIABLE BFT-BIG-ERR-A ptr u8
TYPED-VARIABLE BFT-READ-A ptr u8
variable BFT-READ-CAP

create BFT-ROOT-BUF PATH-CAP allot
create BFT-HB-BUF PATH-CAP allot
create BFT-PREFIX-BUF PATH-CAP allot
create BFT-STAGE2-BUF PATH-CAP allot
create BFT-OUTDIR-BUF PATH-CAP allot
create BFT-OUTDIR2-BUF PATH-CAP allot
create BFT-STAMP-BUF PATH-CAP allot
create BFT-STAMP2-BUF PATH-CAP allot
create BFT-NEST-BUF PATH-CAP allot
create BFT-NOTDIR-BUF PATH-CAP allot
create BFT-ENG-A-BUF PATH-CAP allot
create BFT-ENG-B-BUF PATH-CAP allot
create BFT-CERT-BUF PATH-CAP allot
create BFT-STALE-BUF PATH-CAP allot
create BFT-STALE-HB-BUF PATH-CAP allot
create BFT-STALE-TMP-BUF PATH-CAP allot
create BFT-STALE-OUTDIR-BUF PATH-CAP allot
create BFT-STALE-STAMP-BUF PATH-CAP allot
create BFT-STALE-PAYLOAD-BUF PATH-CAP allot
create BFT-STALE-MARK-BUF PATH-CAP allot
create BFT-CP-BUF PATH-CAP allot
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
   u PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   u lenp ! ;

: BFT-PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: pa:ptr pu na:ptr nu dst:ptr lenp:ptr :}
   pa pu na nu dst FS:JOIN-PATH lenp ! ;

: BFT-ROOT ( -- ptr u8 n )
   BFT-ROOT-BUF BFT-ROOT-U @ ;

: BFT-HB ( -- ptr u8 n )
   BFT-HB-BUF BFT-HB-U @ ;

: BFT-PREFIX ( -- ptr u8 n )
   BFT-PREFIX-BUF BFT-PREFIX-U @ ;

: BFT-STAGE2 ( -- ptr u8 n )
   BFT-STAGE2-BUF BFT-STAGE2-U @ ;

\ Each fixpoint child gets its own out root (HABU_OUT), so its refresh stamp is
\ <out root>/fixpoint/stamp under this fixture's scratch.
: BFT-OUTDIR ( -- ptr u8 n )
   BFT-OUTDIR-BUF BFT-OUTDIR-U @ ;

: BFT-OUTDIR2 ( -- ptr u8 n )
   BFT-OUTDIR2-BUF BFT-OUTDIR2-U @ ;

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

: BFT-STALE-OUTDIR ( -- ptr u8 n )
   BFT-STALE-OUTDIR-BUF BFT-STALE-OUTDIR-U @ ;

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
   FS:CLEANUP-RESET
   s" habu-build-fixpoint" FS:MKTEMP-DIR {: a:ptr u :}
   a u BFT-ROOT-BUF BFT-ROOT-U BFT-COPY!
   BFT-ROOT FS:CLEANUP-TREE+
   BFT-ROOT s" hb-stdin" BFT-HB-BUF BFT-HB-U BFT-PATH!
   s" bin/hb" BFT-HB FS:COPY-FILE-STREAM
   BFT-HB FS:CHMOD-X
   BFT-ROOT s" prefix-src" BFT-PREFIX-BUF BFT-PREFIX-U BFT-PATH!
   BFT-ROOT s" stage2-src" BFT-STAGE2-BUF BFT-STAGE2-U BFT-PATH!
   BFT-ROOT s" out" BFT-OUTDIR-BUF BFT-OUTDIR-U BFT-PATH!
   BFT-ROOT s" out2" BFT-OUTDIR2-BUF BFT-OUTDIR2-U BFT-PATH!
   BFT-OUTDIR s" fixpoint/stamp" BFT-STAMP-BUF BFT-STAMP-U BFT-PATH!
   BFT-OUTDIR2 s" fixpoint/stamp" BFT-STAMP2-BUF BFT-STAMP2-U BFT-PATH!
   BFT-STAMP SOURCE-ROOT:DIRNAME FS:MAKE-DIRS
   BFT-ROOT s" nested/stamps/stamp" BFT-NEST-BUF BFT-NEST-U BFT-PATH!
   BFT-ROOT s" not-a-dir" BFT-NOTDIR-BUF BFT-NOTDIR-U BFT-PATH!
   BFT-ROOT s" engine-a" BFT-ENG-A-BUF BFT-ENG-A-U BFT-PATH!
   BFT-ROOT s" engine-b" BFT-ENG-B-BUF BFT-ENG-B-U BFT-PATH!
   BFT-ROOT s" cert-source.f" BFT-CERT-BUF BFT-CERT-U BFT-PATH!
   BFT-NOTDIR s" plain file, not a directory" FS:WRITE-ALL ;

: BFT-ARGV-LOAD-LIBS ( -- )   \ the --load prefix through build-fixpoint.f, WITHOUT the CLI entry companion
   s" --load"  >LEN PROC-ARGV+
   s" src/lib/errors.f" BFT-ARG+
   s" src/lib/string.f" BFT-ARG+
   s" src/lib/memory.f" BFT-ARG+
   s" src/lib/fs.f" BFT-ARG+
   s" src/lib/fs-mutate.f" BFT-ARG+
   s" src/lib/process.f" BFT-ARG+
   s" src/lib/process-argv.f" BFT-ARG+
   s" src/lib/process-env.f" BFT-ARG+
   s" src/lib/build.f" BFT-ARG+
   s" src/lib/codesign.f" BFT-ARG+
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
\ src/lib/process.f). Replay the child's stderr, which names the throw, and throw
\ E-PROC-TIMEOUT again, so BFT-STEP lets it leave the row uncaught instead of
\ failing it as one more unexpected exit code.
: BFT-FIXPOINT-RC ( n n n ptr u8 -- n n n ) {: outn:n errn:n code:n err:ptr :}
   code PROC-TIMEOUT-RC = if err errn type E-PROC-TIMEOUT throw then
   outn errn code ;

: BFT-READ ( ptr u8 n -- n ) {: pa:ptr pu:n :}
   pa pu FS:FILE-SIZE BFT-ALLOC-READ
   pa pu BFT-READ-BUF BFT-READ-CAP @ FS:READ-ALL ;

\ The stale-seed sandbox: a private copy of src/, src/lib/, the refresh's tools/
\ closure and bin/hb, with its own tmp and stamp, so a case can break a source
\ file and run the refresh there - the real workspace bin/hb is never touched.
\ The sandbox cases (tools/build-fixpoint-sandbox-test.f) plant a crash or a
\ type error in its copy of src/arch/arm64/mnem.f; the watermark cases
\ (tools/build-fixpoint-test.f) cut its copy of src/core/lower-cert-seal.f.
: BFT-STALE-PATHS! ( -- )
   s" habu-bft-stale" FS:MKTEMP-DIR {: a:ptr u:n :}
   a u BFT-STALE-BUF BFT-STALE-U BFT-COPY!
   BFT-STALE FS:CLEANUP-TREE+
   BFT-STALE s" bin/hb" BFT-STALE-HB-BUF BFT-STALE-HB-U BFT-PATH!
   BFT-STALE s" tmp" BFT-STALE-TMP-BUF BFT-STALE-TMP-U BFT-PATH!
   BFT-STALE s" out" BFT-STALE-OUTDIR-BUF BFT-STALE-OUTDIR-U BFT-PATH!
   BFT-STALE-OUTDIR s" fixpoint/stamp" BFT-STALE-STAMP-BUF BFT-STALE-STAMP-U BFT-PATH!
   BFT-STALE s" src/arch/arm64/mnem.f" BFT-STALE-PAYLOAD-BUF BFT-STALE-PAYLOAD-U BFT-PATH!
   BFT-STALE s" src/core/lower-cert-seal.f" BFT-STALE-MARK-BUF BFT-STALE-MARK-U BFT-PATH! ;

\ The sandbox carries every closure the refresh opens, asked from the source by the
\ stamp key's ordered walk: build-fixpoint.f's requires; the capture tool's
\ (ENTRY$), which the stamp key opens before --force and before any write, so a
\ sandbox without it dies E-FS-OPEN ahead of every injected fault; and the certify
\ driver's (BF-CERTIFY-DRIVER$, spawned as its own child). src/ and src/lib/ come
\ over whole: the stage build reads more of them than any require names.

variable IX

: COPY-CLOSURE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u EC:BUILD
   0 IX !
   begin IX @ EC:COUNT < while
      IX @ EC:PATH$ BFT-STALE TREE-COPY:FILE
      IX @ 1+ IX !
   repeat ;

: BFT-STALE-PREPARE ( -- )
   BFT-STALE-PATHS!
   BFT-ALLOC-BIG
   BFT-STALE-TMP FS:MAKE-DIRS
   s" src" BFT-STALE TREE-COPY:TREE
   s" tools/build-fixpoint.f" COPY-CLOSURE
   ENTRY$ COPY-CLOSURE
   BF-CERTIFY-DRIVER$ COPY-CLOSURE
   s" tools/build-fixpoint-main.f" BFT-STALE TREE-COPY:FILE
   s" bin/hb" BFT-STALE TREE-COPY:FILE
   BFT-STALE-HB FS:CHMOD-X ;

: BFT-STALE-ARGV ( -- )
   PROC-ARGV-RESET
   PROC-ENV-RESET
   s" TMPDIR" >LEN BFT-STALE-TMP >LEN PROC-ENV+
   s" HABU_OUT" >LEN BFT-STALE-OUTDIR >LEN PROC-ENV+
   BFT-ARGV-LOAD-FILES
   s" --" BFT-ARG+
   s" install" BFT-ARG+
   s" --force" BFT-ARG+ ;

: BFT-STALE-SPAWN ( -- n n n )
   BFT-STALE-HB >LEN BFT-STALE >LEN
   BFT-BIG-OUT BFT-BIG-CAP >LEN
   BFT-BIG-ERR BFT-BIG-CAP >LEN
   PROCESS:WAIT-FOREVER
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE
   BFT-CAPTURE>N BFT-BIG-ERR BFT-FIXPOINT-RC ;

\ E-PROC-TIMEOUT leaves the row uncaught once its step is named: the engine's
\ report is then the row's last stderr line. Every other throw ends the row here
\ as a failed step.
: BFT-STEP ( ptr u8 n [ -- ] -- ) {: a:ptr u:n q :}
   a u T-LABEL
   q catch {: rc:n :}
   rc 0= if exit then
   a u type s" : throw " type rc . cr
   rc E-PROC-TIMEOUT = if rc throw then
   s" build-fixpoint-test-lib: subtest threw" T-EX-FAIL die ;

: BFT-FINISH ( ptr u8 n -- ) {: msg:ptr msgu:n :}
   FS:CLEANUP-RUN
   BFT-ROOT FS:EXISTS? TFALSE
   T-REPORT
   msg msgu type cr ;

;package
