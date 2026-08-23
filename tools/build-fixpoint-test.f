\ build-fixpoint-test.f - checked fixture for tools/build-fixpoint.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f
\ lib/fs-mutate.f lib/process.f
\ lib/process-argv.f lib/process-env.f lib/process-cwd.f lib/build.f lib/codesign.f
\ tools/build-fixpoint.f tools/build-fixpoint-test.f

require lib/errors.f
require lib/string.f
require lib/adt/option.f                 \ option<CAD-NUM:index> STR:FIND-SUB consumer
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
require tools/source-arena-policy.f
require lib/test/vmsize.f
require tools/event-closure-lib.f      \ EC:BUILD, used by the sandbox and the chain-key fixtures

\ This fixture drives the tool's internals - the emitted stage sources, the
\ stamp preimage, the chain fold - so it REOPENS package BUILD-FIXPOINT rather
\ than importing a public surface. Exporting those internals would widen the
\ tool's own interface for the benefit of its own test. The local fixture
\ scopes this file used to carry (BFT-SNAP-HOOK, BFT-CAP, BFT-CHAIN,
\ STALE-SEED) were there only because the file had no package of its own; they
\ are ordinary private words of the tool's package now.
package BUILD-FIXPOINT

8192 constant BFT-CAPTURE-CAP
$40000 constant BFT-BIG-CAP
120000 constant BFT-TIMEOUT-MS
13 constant BFT-BUILD-ARGV#

\ The snapshot trailer's size and field offsets are owned by src/habu/layout.f
\ (SNAP-TRL-BYTES, SNAP-TRL-NDICT, SNAP-TRL-REGLEN, SNAP-TRL-DATALEN,
\ SNAP-TRL-VERSION); the writer, the loader and this fixture all read them from
\ there, so a format change cannot leave one side addressing the wrong cells.

$A5 constant FORGE
$4A constant MISSING-RC

variable BFT-ROOT-U
variable BFT-HB-NEW-U
variable BFT-HB-U
variable BFT-PREFIX-U
variable BFT-STAGE2-U
variable BFT-RUN-U
variable BFT-SNAP-U
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
variable BFT-STALE-HIDE-U
variable BFT-STALE-MARK-U
variable BFT-CP-U
variable BFT-BIG-OUT-A
variable BFT-BIG-ERR-A
variable BFT-BUILD-FILES
variable BFT-READ-A
variable BFT-READ-CAP
variable BFT-BYTES-A
variable BFT-BYTES-N
variable BFT-PROF-I
variable BFT-REG-I
variable BFT-JIT-I
variable BFT-IMG-I
variable BFT-IMG-BUILD-I
variable BFT-HABU1-I

create BFT-ROOT-BUF FS-PATH-CAP allot
create BFT-HB-NEW-BUF FS-PATH-CAP allot
create BFT-HB-BUF FS-PATH-CAP allot
create BFT-PREFIX-BUF FS-PATH-CAP allot
create BFT-STAGE2-BUF FS-PATH-CAP allot
create BFT-RUN-BUF FS-PATH-CAP allot
create BFT-SNAP-BUF FS-PATH-CAP allot
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
create BFT-STALE-HIDE-BUF FS-PATH-CAP allot
create BFT-STALE-MARK-BUF FS-PATH-CAP allot
create BFT-CP-BUF FS-PATH-CAP allot
create BFT-NL 10 c,
create BFT-KEY1 64 allot
create BFT-OUT BFT-CAPTURE-CAP allot
create BFT-ERR BFT-CAPTURE-CAP allot

: BFT-READ-FIELD ( -- ptr ptr u8 )
   BFT-READ-A 0 ptr-field ;

: BFT-READ-BUF! ( ptr u8 -- )
   BFT-READ-FIELD ! ;

: BFT-READ-BUF ( -- ptr u8 )
   BFT-READ-FIELD @ ;

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

: BFT-RUN ( -- ptr u8 n )
   BFT-RUN-BUF BFT-RUN-U @ ;

: BFT-SNAP ( -- ptr u8 n )
   BFT-SNAP-BUF BFT-SNAP-U @ ;

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

: BFT-STALE-HIDE ( -- ptr u8 n )
   BFT-STALE-HIDE-BUF BFT-STALE-HIDE-U @ ;

: BFT-STALE-MARK ( -- ptr u8 n )
   BFT-STALE-MARK-BUF BFT-STALE-MARK-U @ ;

: BFT-BIG-OUT-FIELD ( -- ptr ptr u8 )
   BFT-BIG-OUT-A 0 ptr-field ;

: BFT-BIG-ERR-FIELD ( -- ptr ptr u8 )
   BFT-BIG-ERR-A 0 ptr-field ;

: BFT-BIG-OUT ( -- ptr u8 )
   BFT-BIG-OUT-FIELD @ ;

: BFT-BIG-ERR ( -- ptr u8 )
   BFT-BIG-ERR-FIELD @ ;

: BFT-ALLOC-BIG ( -- )
   BFT-BIG-CAP MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop BFT-BIG-OUT-FIELD !
   BFT-BIG-CAP MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop BFT-BIG-ERR-FIELD ! ;

: BFT-EMPTY$ ( -- ptr u8 n )
   SB-RESET
   SB$ ;

: BFT-ENV$ ( -- ptr u8 n )
   BFT-ROOT ;

: BFT-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: BFT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-build-fixpoint" TMPDIR-MKDIR {: a:ptr u :}
   a u BFT-ROOT-BUF BFT-ROOT-U BFT-COPY!
   BFT-ROOT CLEANUP-TREE+
   BFT-ROOT s" hb-new" BFT-HB-NEW-BUF BFT-HB-NEW-U BFT-PATH!
   BFT-ROOT s" hb-stdin" BFT-HB-BUF BFT-HB-U BFT-PATH!
   BFT-ROOT s" prefix-src" BFT-PREFIX-BUF BFT-PREFIX-U BFT-PATH!
   BFT-ROOT s" stage2-src" BFT-STAGE2-BUF BFT-STAGE2-U BFT-PATH!
   BFT-ROOT s" stage2-run-src" BFT-RUN-BUF BFT-RUN-U BFT-PATH!
   BFT-ROOT s" hb-snap-src" BFT-SNAP-BUF BFT-SNAP-U BFT-PATH!
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

: BFT-ARGV-ENV+ ( ptr u8 n ptr u8 n -- ) {: tmp:ptr tmpu:n stamp:ptr stampu:n :}
   PROC-ARGV-RESET
   PROC-ENV-RESET
   s" HB_TMP" >LEN tmp tmpu >LEN PROC-ENV+
   s" HABU_FIXPOINT_STAMP" >LEN stamp stampu >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN BFT-HB >LEN PROC-ENV+ ;

: BFT-ARGV-FIXPOINT ( ptr u8 n ptr u8 n -- n )
   BFT-ARGV-ENV+
   BFT-ARGV-LOAD-FILES
   PROC-ARGV-N @ COUNT>N ;

: BFT-ARGV-FIXPOINT-NO-MAIN ( ptr u8 n ptr u8 n -- n )   \ BFT-ARGV-FIXPOINT minus the -main companion: reproduces the recovery footgun
   BFT-ARGV-ENV+
   BFT-ARGV-LOAD-LIBS
   PROC-ARGV-N @ COUNT>N ;

: BFT-ARGV-NO-PREAMBLE ( ptr u8 n ptr u8 n -- n )   \ tool source WITHOUT the lib preamble: reproduces the missing-preamble footgun (bare `E-UNDEFINED: FS-PATH-CAP` pre-fix)
   BFT-ARGV-ENV+
   s" --load" >LEN PROC-ARGV+
   s" tools/build-fixpoint.f" BFT-ARG+
   PROC-ARGV-N @ COUNT>N ;

: BFT-ARGV-BUILD ( -- n )
   BFT-ENV$ BFT-STAMP BFT-ARGV-FIXPOINT ;

: BFT-ARGV-FAIL ( -- n )
   BFT-NOTDIR BFT-STAMP2 BFT-ARGV-FIXPOINT ;

: BFT-CAPTURE>N ( result<pcap:captured,pcap:failed> -- n n n )   \ outn errn code (0 on clean exit)
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} o LEN>N e LEN>N 0 ENDOF
     err OF PCAP-FAILED:UNMAKE  {: o:len e:len c:rc :} o LEN>N e LEN>N c RC>N ENDOF
   ;MATCH ;

: BFT-ARGV-ALL-FORCE ( -- )
   s" --" BFT-ARG+
   s" all" BFT-ARG+
   s" --force" BFT-ARG+ ;

: BFT-ARGV-ALL ( -- )
   s" --" BFT-ARG+
   s" all" BFT-ARG+ ;

: BFT-ARGV-INSTALL-FORCE ( -- )
   s" --" BFT-ARG+
   s" install" BFT-ARG+
   s" --force" BFT-ARG+ ;

: BFT-SPAWN-FIXPOINT ( -- n n n )
   s" bin/hb" >LEN PROC-ARGV-CHECK-PATH
   BFT-CAPTURE-CAP >LEN BFT-CAPTURE-CAP >LEN PROC-CAPTURE-CHECK-CAPS
   s" bin/hb" >LEN PROC-ARGV-PREPARE {: pathz:ptr argv:ptr :}
   PROC-ENV-PREPARE {: envp:ptr :}
   BFT-TIMEOUT-MS >MS PROC-CAPTURE-BEGIN
   pathz argv envp PROC-SPAWN-ARGV-ENV-CAPTURE
   BFT-OUT BFT-CAPTURE-CAP >LEN BFT-ERR BFT-CAPTURE-CAP >LEN PROC-RUN-CAPTURE-LOOP
   PROC-CAPTURE-FINISH-RC
   BFT-CAPTURE>N ;

: BFT-RUN-BUILD ( -- n n n )
   BFT-ARGV-BUILD BFT-BUILD-ARGV# T=
   BFT-ARGV-ALL-FORCE
   BFT-SPAWN-FIXPOINT ;

: BFT-RUN-CACHED ( -- n n n )
   BFT-ARGV-BUILD BFT-BUILD-ARGV# T=
   BFT-ARGV-ALL
   BFT-SPAWN-FIXPOINT ;

: BFT-STAMP-SCOPE ( -- )
   BFT-ROOT BF-TMP!
   BFT-STAMP BF-STAMP-PATH! ;

: BFT-STAMP-UNSCOPE ( -- )
   BF-STAMP-PATH-RESET
   BF-ENGINE-RESET
   BF-TMP-RESET ;

: BFT-RECORD! ( -- )
   BF-PREFIX-SOURCE
   BF-RECORD-PREFIX
   BF-STAGE2-SOURCE
   BF-RECORD-STAGE
   BF-STDIN-SOURCE
   BF-RECORD-STDIN ;

: BFT-TEST-BUILD ( -- )
   BFT-RUN-BUILD 0 T=
   {: outu erru :}
   BFT-ERR erru BFT-EMPTY$ T$=
   BFT-OUT outu s" bin/hb refresh OK: compiler fixpoint" CONTAINS? TTRUE
   BFT-OUT outu s" boot prefix = " CONTAINS? TTRUE      \ the census reports both phases
   BFT-OUT outu s" assembled = " CONTAINS? TTRUE
   BFT-OUT outu s" snapshot image OK: candidate validated" CONTAINS? TFALSE
   BFT-OUT outu s" fixpoint: cached " CONTAINS? TFALSE
   BFT-HB FILE? TTRUE
   BFT-HB-NEW FILE? TFALSE
   BFT-STAMP FILE? TTRUE
   BFT-STAMP FILE-SIZE BF-STAMP-HEX-U 1 + T=
   BFT-STAMP-SCOPE
   BFT-HB BF-ENGINE!
   BF-STAMP-MATCH? TTRUE
   BFT-STAMP-UNSCOPE ;

: BFT-TEST-STAMP-SEED ( -- )
   BFT-STAMP-SCOPE
   BFT-RECORD!
   BF-STAMP-WRITE
   BFT-STAMP FILE? TTRUE
   BFT-STAMP FILE-SIZE BF-STAMP-HEX-U 1 + T=
   BF-STAMP-MATCH? TTRUE
   BFT-STAMP REMOVE-FILE
   BFT-STAMP-UNSCOPE ;

: BFT-TEST-CACHED-SKIP ( -- )
   BFT-ROOT BF-TMP!
   s" hb-stage" BF-REMOVE-TMP
   BF-TMP-RESET
   BFT-RUN-CACHED 0 T=
   {: outu:n erru:n :}
   BFT-ERR erru BFT-EMPTY$ T$=
   BFT-OUT outu s" fixpoint: cached " CONTAINS? TTRUE
   BFT-OUT outu s" bin/hb refresh OK" CONTAINS? TFALSE
   BFT-ROOT BF-TMP!
   s" hb-stage" BF-A$ FILE? TFALSE
   BF-TMP-RESET
   BFT-HB FILE? TTRUE ;

: BFT-TEST-BUILD-FAIL-NO-STAMP ( -- )
   BFT-ARGV-FAIL BFT-BUILD-ARGV# T=
   BFT-ARGV-ALL-FORCE
   BFT-SPAWN-FIXPOINT {: outu:n erru:n rcn:n :}
   rcn BF-BUILD-RC T=
   BFT-ERR erru s" build-fixpoint: failed" CONTAINS? TTRUE
   BFT-STAMP2 FILE? TFALSE ;

\ Recovery footgun (dot habu-stale-bin-hb): build-fixpoint.f loaded WITHOUT its
\ tools/build-fixpoint-main.f companion but handed an explicit `install` verb.
\ Pre-fix BF-CLI was never called, so the verb was silently dropped - the loaded
\ stdin engine read its program from the closed stdin, hit EOF, and exited 0
\ having built nothing (rc 0, empty output). The tail self-dispatch must now run
\ the verb; a non-directory HB_TMP makes the install fail fast and die loudly,
\ so a build verb can no longer exit 0 having done nothing. Base (unfixed
\ source): rc 0, empty stderr -> both direction assertions fail.
: BFT-TEST-NO-MAIN-DISPATCHES ( -- )
   BFT-NOTDIR BFT-STAMP2 BFT-ARGV-FIXPOINT-NO-MAIN BFT-BUILD-ARGV# 1 - T=
   BFT-ARGV-INSTALL-FORCE
   BFT-SPAWN-FIXPOINT {: outu:n erru:n rcn:n :}
   rcn BF-BUILD-RC T=
   BFT-ERR erru s" build-fixpoint: failed" CONTAINS? TTRUE
   BFT-STAMP2 FILE? TFALSE ;

\ Missing-preamble footgun (dot habu-make-build-fixpoint): build-fixpoint.f loaded
\ WITHOUT its lib preamble handed an explicit `install` verb. Pre-fix the first
\ FS-PATH-CAP buffer create died mid-load with a bare `E-UNDEFINED: FS-PATH-CAP`
\ (rc 70) - no hint the preamble was missing. The load-discipline guard must now
\ fail fast with BF-USAGE-RC and a named diagnostic that lists the required load.
\ Base (unfixed source): rc 70, stderr `E-UNDEFINED: FS-PATH-CAP` -> both direction
\ assertions fail.
: BFT-TEST-MISSING-PREAMBLE ( -- )
   BFT-NOTDIR BFT-STAMP2 BFT-ARGV-NO-PREAMBLE DROP
   BFT-ARGV-INSTALL-FORCE
   BFT-SPAWN-FIXPOINT {: outu:n erru:n rcn:n :}
   rcn BF-USAGE-RC T=
   BFT-ERR erru s" missing required load" CONTAINS? TTRUE ;

\ The stamp key with ONE emitted source mutated between its emission and its
\ digest row. A word per phase would be a copy of the key sequence per phase,
\ and the copy is what goes stale: a phase added to BF-STAMP-KEY! and forgotten
\ here would leave its mutation case silently testing the old key. One
\ sequence, one selector.
0 constant BFT-MUT-NONE
1 constant BFT-MUT-PREFIX
2 constant BFT-MUT-STAGE
3 constant BFT-MUT-STDIN

: BFT-KEY-MUT! ( n -- ) {: mut:n :}
   BF-STAMP-KEY-BEGIN
   BF-PREFIX-SOURCE
   mut BFT-MUT-PREFIX = if s" prefix-src" BF-A$ s" \ mutated boot prefix" APPEND-FILE then
   BF-STAMP-PREFIX-KEY+
   BF-STAGE2-SOURCE
   mut BFT-MUT-STAGE = if s" stage2-src" BF-A$ s" \ mutated stage source" APPEND-FILE then
   BF-STAMP-STAGE-KEY+
   BF-STDIN-SOURCE
   mut BFT-MUT-STDIN = if s" stage2-src" BF-A$ s" \ mutated stdin source" APPEND-FILE then
   BF-STAMP-STDIN-KEY+
   BF-STAMP-KEY-END ;

: BFT-KEY-DIFFERS ( n -- ) {: mut:n :}
   mut BFT-KEY-MUT!
   BFT-KEY1 BF-STAMP-HEX-U BF-STAMP-KEY BF-STAMP-HEX-U STR= TFALSE ;

\ Every emitted source the key covers is load-bearing, and the unmutated run is
\ the control: without it, three keys differing would also be the signature of
\ a key that simply never reproduces.
: BFT-TEST-STAMP-SOURCE-KEY ( -- )
   BFT-STAMP-SCOPE
   BF-STAMP-KEY!
   BF-STAMP-KEY BFT-KEY1 BF-STAMP-HEX-U BYTE-COPY
   BFT-MUT-PREFIX BFT-KEY-DIFFERS
   BFT-MUT-STAGE BFT-KEY-DIFFERS
   BFT-MUT-STDIN BFT-KEY-DIFFERS
   BFT-MUT-NONE BFT-KEY-MUT!
   BFT-KEY1 BF-STAMP-HEX-U BF-STAMP-KEY BF-STAMP-HEX-U STR= TTRUE
   BFT-STAMP-UNSCOPE ;

: BFT-ZERO-KEY$ ( -- ptr u8 n )
   s" 0000000000000000000000000000000000000000000000000000000000000000" ;

: BFT-TEST-STAMP-CORRUPT ( -- )
   BFT-STAMP-SCOPE
   SB-RESET
   BFT-ZERO-KEY$ SB-APPEND
   BF-LF SB-APPEND-C
   BFT-STAMP SB$ WRITE-ALL
   BF-STAMP-MATCH? TFALSE
   BFT-STAMP s" short" WRITE-ALL
   BF-STAMP-MATCH? TFALSE
   BFT-STAMP REMOVE-FILE
   BF-STAMP-MATCH? TFALSE
   BFT-STAMP-UNSCOPE ;

: BFT-WRITE-ENGINES ( -- )
   BFT-ENG-A s" engine-a-bytes" WRITE-ALL
   BFT-ENG-B s" engine-b-bytes" WRITE-ALL ;

: BFT-TEST-STAMP-ENGINE ( -- )
   BFT-STAMP-SCOPE
   BFT-RECORD!
   BFT-WRITE-ENGINES
   BFT-ENG-A BF-ENGINE!
   BF-STAMP-WRITE
   BF-STAMP-MATCH? TTRUE
   BFT-ENG-B BF-ENGINE!
   BF-STAMP-MATCH? TFALSE
   BFT-ENG-A BF-ENGINE!
   -1 BF-FORCE !
   BF-STAMP-MATCH? TFALSE
   0 BF-FORCE !
   BF-STAMP-MATCH? TTRUE
   BFT-STAMP-UNSCOPE ;

: BFT-TEST-ALL-STAMP-GUARD ( -- )
   BFT-STAMP-SCOPE
   BFT-RECORD!
   BFT-WRITE-ENGINES
   BFT-STAMP FILE? if BFT-STAMP REMOVE-FILE then
   BFT-ENG-A BF-ENGINE!
   BF-ALL-STAMP
   BFT-STAMP FILE? TFALSE
   BFT-HB BF-ENGINE!
   BF-ALL-STAMP
   BFT-STAMP FILE? TTRUE
   BFT-STAMP-UNSCOPE ;

: BFT-TEST-STAMP-NESTED ( -- )
   BFT-ROOT BF-TMP!
   BFT-NEST BF-STAMP-PATH!
   BFT-RECORD!
   BF-STAMP-WRITE
   BFT-NEST FILE? TTRUE
   BF-STAMP-MATCH? TTRUE
   BFT-STAMP-UNSCOPE ;

: BFT-READ ( ptr u8 n -- n ) {: pa:ptr pu:n :}
   pa pu FILE-SIZE BFT-ALLOC-READ
   pa pu BFT-READ-BUF BFT-READ-CAP @ READ-ALL ;

\ Stale-seed install regression: a refresh child that dies (here: a crash baked
\ into the fixture's src/habu/hide.f, the first stage2 prefix file, so the
\ bootstrap child aborts with SIGABRT rc 134 exactly like a seed that cannot
\ load the current engine prefix) must fail the install loudly: deterministic
\ BF-BUILD-RC exit, a named stderr diagnostic, the engine binary byte-unchanged,
\ and no stamp. Before the BF-CLI boundary the E-BUILD-STATUS throw escaped to
\ BTHROW's no-handler exit: silent, exit code masked to the low 8 bits.
\ The crash rides a TRUSTED: boundary so BLOCKING certification cannot see it
\ (a `0 set-check` body is still checked by VERIFY:SOURCE-BUF and would be
\ rejected statically before ever running) -- the stale-seed scenario this
\ models is a semantic runtime failure, not a type error.
: BFT-STALE-DST ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   BFT-STALE a u BFT-CP-BUF JOIN-PATH BFT-CP-U !
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

: BFT-STALE-SABOTAGE ( -- )
   s" src/habu/hide.f" BFT-READ {: u:n :}
   BFT-STALE-HIDE s" TRUSTED: BFT-STALE-CRASH ( -- ) 1 0 ! ; BFT-STALE-CRASH" WRITE-ALL
   BFT-STALE-HIDE BFT-NL 1 APPEND-FILE
   BFT-STALE-HIDE BFT-READ-BUF u APPEND-FILE ;

: BFT-STALE-PATHS! ( -- )
   s" habu-bft-stale" TMPDIR-MKDIR {: a:ptr u:n :}
   a u BFT-STALE-BUF BFT-STALE-U BFT-COPY!
   BFT-STALE CLEANUP-TREE+
   BFT-STALE s" bin/hb" BFT-STALE-HB-BUF BFT-STALE-HB-U BFT-PATH!
   BFT-STALE s" tmp" BFT-STALE-TMP-BUF BFT-STALE-TMP-U BFT-PATH!
   BFT-STALE s" stamp" BFT-STALE-STAMP-BUF BFT-STALE-STAMP-U BFT-PATH!
   BFT-STALE s" src/habu/hide.f" BFT-STALE-HIDE-BUF BFT-STALE-HIDE-U BFT-PATH!
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
   BFT-STALE-HB CHMOD-X
   BFT-STALE-SABOTAGE ;

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
   BFT-CAPTURE>N ;

: BFT-TEST-STALE-INSTALL ( -- )
   BFT-STALE-PREPARE
   BFT-STALE-ARGV
   BFT-STALE-SPAWN {: outu:n erru:n rcn:n :}
   rcn BF-BUILD-RC T=
   BFT-BIG-ERR erru s" build-fixpoint: failed" CONTAINS? TTRUE
   BFT-BIG-ERR erru s" E-BUILD-STATUS" CONTAINS? TTRUE
   BFT-BIG-ERR erru s" habu-crash regs" CONTAINS? TTRUE
   BFT-STALE-HB s" bin/hb" BF-FILE= TTRUE
   BFT-STALE-STAMP FILE? TFALSE ;

\ Staged-fixpoint refusal regression (dot habu-staged-fixpoint-src-0b5fc6e6):
\ a deliberately type-broken CHECKED definition in a source file of the
\ assembled stage list (here appended to the sandbox copy of src/habu/hide.f,
\ the first emitted file, so the certify scan reaches it early) must make the
\ refresh REFUSE at the blocking pre-pass: deterministic BF-BUILD-RC exit, the
\ certify diagnostic naming the injected word on stdout, the E-BUILD-CERTIFY
\ name on stderr, the sandbox engine byte-unchanged, and no stamp. Runs in the
\ BFT-STALE sandbox tree (private tmp + scratch install target - the real
\ workspace bin/hb is never touched) with a fresh sabotage replacing the
\ stale-seed one.
: BFT-STALE-HIDE-RESTORE ( -- )
   s" src/habu/hide.f" BFT-READ {: u:n :}
   BFT-STALE-HIDE BFT-READ-BUF u WRITE-ALL ;

: BFT-CERT-INJ-SABOTAGE ( -- )
   BFT-STALE-HIDE-RESTORE
   BFT-STALE-HIDE s" : BFT-CERT-INJ ( n -- n ) drop ;" APPEND-FILE
   BFT-STALE-HIDE BFT-NL 1 APPEND-FILE ;

: BFT-TEST-CERT-INJECT-INSTALL ( -- )
   BFT-CERT-INJ-SABOTAGE
   BFT-STALE-ARGV
   BFT-STALE-SPAWN {: outu:n erru:n rcn:n :}
   rcn BF-BUILD-RC T=
   BFT-BIG-OUT outu s" certify: stage2-src rejected" CONTAINS? TTRUE
   BFT-BIG-OUT outu s" bft-cert-inj" CONTAINS? TTRUE
   BFT-BIG-ERR erru s" build-fixpoint: failed" CONTAINS? TTRUE
   BFT-BIG-ERR erru s" E-BUILD-CERTIFY" CONTAINS? TTRUE
   BFT-STALE-HB s" bin/hb" BF-FILE= TTRUE
   BFT-STALE-STAMP FILE? TFALSE ;

\ typed STR:FIND-SUB boundary: route byte-lengths through the STR: role surface,
\ project the option<CAD-NUM:index> result back to the switchover option<idx>.
;package

package CAD-NUM
public
: BFT-IX>N ( CAD-NUM:index -- n ) INDEX>N ;
;package

package BUILD-FIXPOINT
: BFT-FIND ( ptr u8 n ptr u8 n -- option<idx> ) {: a:ptr u:n b:ptr v:n :}
   a u STR:LENGTH b v STR:LENGTH STR:FIND-SUB MATCH option
     none OF OPTION:NONE ENDOF
     some OF CAD-NUM:BFT-IX>N >IDX OPTION:SOME ENDOF
   ;MATCH ;

: BFT-FIND-AFTER ( ptr u8 n n ptr u8 n -- option<idx> ) {: a:ptr u:n start:n needle:ptr nu:n :}
   start 0 < if OPTION:NONE exit then
   start u >= if OPTION:NONE exit then
   a start BYTE+ u start - needle nu BFT-FIND MATCH option
     none OF OPTION:NONE ENDOF
     some OF IDX>N start + >IDX OPTION:SOME ENDOF
   ;MATCH ;

: BFT-FOUND ( option<idx> -- n )                     \ assert found; found index (-1 after a recorded miss)
   MATCH option
     none OF STR-FALSE TTRUE -1 ENDOF
     some OF STR-TRUE TTRUE IDX>N ENDOF
   ;MATCH ;

: BFT-NOT-FOUND ( option<idx> -- )
   MATCH option
     none OF STR-TRUE ENDOF
     some OF drop STR-FALSE ENDOF
   ;MATCH TTRUE ;

\ Does an emitted source DEFINE this name at top level? The question the
\ single-load contract needs answered about the assembled payload is "is there a
\ second copy of this prefix file in here", and a substring search cannot answer
\ it: src/habu/hide.f still ships in the payload, so the text
\ BFR-HIDE-DICT-FROM-EARLIEST is present whether or not anything calls it, and
\ every prefix name appears in the comments of the files that call it.
\
\ So this reuses the certify scanner's own tokenizer - the same NEXT-SCAN that
\ decides what VERIFY:SOURCE-BUF checked - and reads the token in the DEFINITION
\ position after a `:`. A name in a comment, in a string, or at a call site is
\ not in that position. The hostile fixtures below are what prove it.
;package

package VERIFY
variable DEF-HIT
variable DEF-A
variable DEF-U

: DEF-WANT$ ( -- ptr u8 n )
   DEF-A @ DEF-U @ ;

public

: DEFINES? ( ptr u8 n ptr u8 n -- bool ) {: src:ptr srcu:n want:ptr wantu:n :}
   want DEF-A !  wantu DEF-U !
   0 DEF-HIT !
   src srcu SOURCE! SCAN-RESET
   begin NEXT-SCAN dup 0 > while
      s" :" CORE-STR= if
         NEXT-SCAN dup 0 > if
            DEF-WANT$ CORE-STR= if -1 DEF-HIT ! then
         else 2drop then
      then
   repeat 2drop
   DEF-HIT @ 0<> ;
;package

package BUILD-FIXPOINT

\ Hostile fixtures for the definition scan above. Each puts the subject name in
\ a position that is NOT a top-level definition, and each must answer false -
\ otherwise the single-load assertions below would be satisfied by the comments
\ and call sites the payload is full of. The two positives beside them keep the
\ whole thing from passing by always answering false.
: BFT-DEF-DECOY$ ( -- ptr u8 n )
   s" BFT-DECOY" ;

: BFT-DEF? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u BFT-DEF-DECOY$ VERIFY:DEFINES? ;

: BFT-TEST-DEFINES-SCAN ( -- )
   s\" \\ : BFT-DECOY ( -- ) ;\n" BFT-DEF? TFALSE
   s\" ( : BFT-DECOY ( -- ) ;)\n" BFT-DEF? TFALSE
   s\" : OTHER ( -- ) s\\\" BFT-DECOY\\\" 2drop ;\n" BFT-DEF? TFALSE
   s\" : OTHER ( -- ) BFT-DECOY ;\n" BFT-DEF? TFALSE
   s\" : BFT-DECOYS ( -- ) ;\n" BFT-DEF? TFALSE
   s\" : BFT-DECOY ( -- ) ;\n" BFT-DEF? TTRUE
   s\" \\ BFT-DECOY in a comment first\n: BFT-DECOY ( -- ) ;\n" BFT-DEF? TTRUE ;

\ THE HOST CAPABILITY THE BUILD REQUIRES, refused at the door. The payload
\ rewinds to the mark src/core/lower-cert-seal.f records at the core prefix's
\ end and carries no copy of that prefix, so a host engine without the mark
\ cannot build this tree at all - BF-PREFLIGHT has to say so before a byte is
\ emitted, not leave an undefined core word to surface from inside a generated
\ file. The sabotage is that file with its PREFIX-MARK block cut off and nothing
\ else changed: an engine reads its boot prefix from the tree it runs in, so the
\ sandbox's copy IS the child's prefix, and the sandbox engine is byte-identical
\ to the workspace one. Runs after the certify-injection case, so it restores
\ that case's sabotaged src/habu/hide.f first.
: BFT-MARK-SABOTAGE ( -- )
   BFT-STALE-HIDE-RESTORE
   s" src/core/lower-cert-seal.f" BFT-READ {: u:n :}
   BFT-READ-BUF u s" package PREFIX-MARK" BFT-FIND BFT-FOUND {: cut:n :}
   cut 0 > TTRUE
   BFT-STALE-MARK BFT-READ-BUF cut WRITE-ALL ;

: BFT-TEST-WATERMARK-REQUIRED ( -- )
   BFT-MARK-SABOTAGE
   BFT-STALE-ARGV
   BFT-STALE-SPAWN {: outu:n erru:n rcn:n :}
   rcn BF-BUILD-RC T=
   BFT-BIG-ERR erru s" no core-prefix watermark" CONTAINS? TTRUE
   BFT-BIG-ERR erru s" src/core/lower-cert-seal.f" CONTAINS? TTRUE
   BFT-BIG-OUT outu s" self-check census" CONTAINS? TFALSE
   BFT-STALE-HB s" bin/hb" BF-FILE= TTRUE
   BFT-STALE-STAMP FILE? TFALSE ;

\ THE SAME PROBE'S VALUE LAYER, forged twice. Both fixtures leave every name in
\ place, so the resolvability clause above passes and only the value can refuse -
\ which is the whole point of reading one: a name resolves whether or not
\ anything ever wrote through it, and an engine built from a tree that carried
\ the words but never took the boundary is exactly the host that would emit a
\ rewind restoring nothing.
\
\ The fixture stops the file one line short of taking the boundary width, so
\ every PREFIX-MARK name resolves, the mark still records the dictionary end and
\ the include registry, and only CURSORS reads zero. That is the one state the
\ name clause is blind to and the value clause exists for. The rewrite starts
\ from the workspace file, so it cannot inherit an earlier case's damage.
: BFT-MARK-VALUE-FORGE ( ptr u8 n -- ) {: tail:ptr tailu:n :}
   BFT-STALE-HIDE-RESTORE
   s" src/core/lower-cert-seal.f" BFT-READ {: u:n :}
   BFT-READ-BUF u s" CHECKER-BOUND:CURSORS CU !" BFT-FIND BFT-FOUND {: cut:n :}
   cut 0 > TTRUE
   BFT-STALE-MARK BFT-READ-BUF cut WRITE-ALL
   BFT-STALE-MARK tail tailu APPEND-FILE ;

: BFT-MARK-VALUE-REFUSED ( ptr u8 n -- ) {: msg:ptr msgu:n :}
   BFT-STALE-ARGV
   BFT-STALE-SPAWN {: outu:n erru:n rcn:n :}
   rcn BF-BUILD-RC T=
   BFT-BIG-ERR erru msg msgu CONTAINS? TTRUE
   BFT-BIG-OUT outu s" self-check census" CONTAINS? TFALSE
   BFT-STALE-HB s" bin/hb" BF-FILE= TTRUE
   BFT-STALE-STAMP FILE? TFALSE ;

: BFT-TEST-WATERMARK-VALUE ( -- )
   s\" ;package\n" BFT-MARK-VALUE-FORGE
   s" carries no checker boundary" BFT-MARK-VALUE-REFUSED ;

\ THE SINGLE-LOAD CONTRACT, over the two sources a real build writes.
\
\ Each probe name is asked of BOTH assemblies, and the pair is the claim: the
\ boot prefix defines it, the payload does not. Asking only the payload would
\ also pass if the prefix had stopped being certified anywhere at all, which is
\ the failure the certify-only phase exists to prevent.
\
\ Three probes spanning the prefix's extent - util.f is its first file,
\ checker.f its largest, xref.f one of its last - because a payload that stopped
\ re-emitting only its first file would satisfy one probe.
: BFT-PREFIX-ONCE ( ptr u8 n -- ) {: name:ptr nameu:n :}
   BFT-STAGE2 BFT-READ {: u:n :}
   BFT-READ-BUF u name nameu VERIFY:DEFINES? TFALSE
   BFT-PREFIX BFT-READ {: v:n :}
   BFT-READ-BUF v name nameu VERIFY:DEFINES? TTRUE ;

\ Both assemblies are written here rather than inherited from an earlier step,
\ so the pair the contract compares is one build's. The payload is the STDIN
\ source - the one that becomes bin/hb - because that is the assembly a shipped
\ engine is compiled from; BF-STAGE2-SOURCE writes the same file with the stage
\ driver instead, and BFT-TEST-CERTIFY-PHASE-SOURCES already pins that the two
\ differ.
: BFT-TEST-STAGE2-SOURCE ( -- )
   BFT-ROOT BF-TMP!
   BF-PREFIX-SOURCE
   BF-STDIN-SOURCE
   s" CORE-STR=" BFT-PREFIX-ONCE
   s" ATOMA-FIELD" BFT-PREFIX-ONCE
   s" SEAL-DICT-GUARD" BFT-PREFIX-ONCE
   BFT-STAGE2 BFT-READ {: u :}
   BFT-READ-BUF u s" : HOOK ( ptr u8 n -- n ) CHECK! dup -1 <> if 70 throw then ; ' HOOK set-check" CONTAINS? TFALSE
   BFT-READ-BUF u s" RPD@" VERIFY:DEFINES? TTRUE
   BFT-READ-BUF u s" STDIN-OUT" VERIFY:DEFINES? TTRUE
   BFT-READ-BUF u s" BFR-CHECK-OFF" BFT-FIND BFT-FOUND {: off:n :}
   BFT-READ-BUF u off s" PREFIX-REWIND:TO-CORE" BFT-FIND-AFTER BFT-FOUND {: rewind:n :}
   BFT-READ-BUF u rewind s" LOWER-CERT-HOOK:INSTALL" BFT-FIND-AFTER BFT-FOUND {: hook:n :}
   BFT-READ-BUF u hook s" SEAL-FRIEND" BFT-FIND-AFTER BFT-FOUND {: seal:n :}
   BFT-READ-BUF u seal s" driver-io.f" BFT-FIND-AFTER BFT-FOUND {: driver:n :}
   off rewind < TTRUE
   rewind hook < TTRUE
   hook seal < TTRUE
   seal driver < TTRUE
   BFT-READ-BUF u BF-BOUNDARY-RAW-OFF$ CONTAINS? TFALSE
   BFT-READ-BUF u s\" s\" ASM-CODE\" s\" -- asm\" TRUST" CONTAINS? TFALSE
   BF-TMP-RESET ;

: BFT-TEST-NO-STAGE2-RUN-SOURCE ( -- )
   BFT-RUN FILE? TFALSE ;

: BFT-TEST-CHECKED-REGALLOC ( -- )
   BFT-STAGE2 BFT-READ {: u :}
   BFT-READ-BUF u s" : BPROF-ON" BFT-FIND BFT-FOUND BFT-PROF-I !
   BFT-READ-BUF u BFT-PROF-I @ s" : EMIT-VRINIT" BFT-FIND-AFTER BFT-FOUND BFT-REG-I !
   BFT-READ-BUF u BFT-REG-I @ s" : FOLD-ENTRY" BFT-FIND-AFTER BFT-FOUND BFT-JIT-I !
   BFT-PROF-I @ BFT-REG-I @ < TTRUE
   BFT-REG-I @ BFT-JIT-I @ < TTRUE ;

\ The target-image writers (elf/macho/sign) compile CHECKED in stage2: no
\ 0 set-check window opens at or after the image region and no synthetic
\ TRUST rows are injected, so a stack-effect regression in ASM-CODE/
\ BUILD-IMAGE/BUILD-SNAP-HDR/SET-SIGID/CODESIG2 fails the stage compile.
: BFT-TEST-CHECKED-TARGET-IMAGE ( -- )
   BFT-STAGE2 BFT-READ {: u :}
   BFT-READ-BUF u s" : ASM-CODELEN!" BFT-FIND BFT-FOUND BFT-IMG-I !
   BFT-READ-BUF u BFT-IMG-I @ s" : BUILD-IMAGE" BFT-FIND-AFTER BFT-FOUND BFT-IMG-BUILD-I !
   BFT-READ-BUF u BFT-IMG-BUILD-I @ s" : RPD@" BFT-FIND-AFTER BFT-FOUND BFT-HABU1-I !
   BFT-READ-BUF u BFT-IMG-I @ BF-BOUNDARY-RAW-OFF$ BFT-FIND-AFTER BFT-NOT-FOUND
   BFT-IMG-I @ BFT-IMG-BUILD-I @ < TTRUE
   BFT-IMG-BUILD-I @ BFT-HABU1-I @ < TTRUE ;

: BFT-TEST-STAGE2-SCRATCH ( -- )
   BFT-STAGE2 BFT-READ {: u :}
   BFT-READ-BUF u s" SOURCE-CAP allot" CONTAINS? TFALSE
   BFT-READ-BUF u s" stage2: source mmap failed" CONTAINS? TTRUE ;

\ The snap tail's RETIRE-AND-PERSIST is a named TRUSTED: boundary,
\ not a `0 set-check` window: the emitted snap source carries NO raw
\ check-off line - BFR-CHECK-OFF (src/habu/hide.f) is the one named
\ checking-disabled boundary a generated source may call.
: BFT-TEST-SNAP-SOURCE ( -- )
   BFT-ROOT BF-TMP!
   BF-SNAP-SOURCE
   BFT-SNAP BFT-READ {: u :}
   BFT-READ-BUF u s" : SNAP-TAIL-MARK" BFT-FIND BFT-FOUND {: mark:n :}
   BFT-READ-BUF u mark s" SEAL-FRIEND" BFT-FIND-AFTER BFT-FOUND {: seal:n :}
   BFT-READ-BUF u seal s" : ASM-CODELEN!" BFT-FIND-AFTER BFT-FOUND {: build:n :}
   BFT-READ-BUF u build s" TRUSTED: RETIRE-AND-PERSIST ( -- )" BFT-FIND-AFTER BFT-FOUND {: retire:n :}
   mark seal < TTRUE
   seal build < TTRUE
   build retire < TTRUE
   BFT-READ-BUF u BF-BOUNDARY-RAW-OFF$ CONTAINS? TFALSE
   BFT-READ-BUF u s" SNAP-MAGIC" CONTAINS? TTRUE
   BFT-READ-BUF u s" CHECKER-SNAPSHOT-PREPARE data-base ENGINE-SNAP-XT-CELL + !" CONTAINS? TTRUE
   \ THE KEEP SURFACE, stated as what it is. The core prefix is not re-read here
   \ - the payload rewinds to the mark at its end and the compiling host's own
   \ copy stays live - so the one file the image must bring back is the one the
   \ engine loads AFTER that mark and the rewind therefore takes away.
   BFT-READ-BUF u s" CORE-STR=" VERIFY:DEFINES? TFALSE
   BFT-READ-BUF u s" ATOMA-FIELD" VERIFY:DEFINES? TFALSE
   BFT-READ-BUF u s" SEAL-DICT-GUARD" VERIFY:DEFINES? TFALSE
   BFT-READ-BUF u s" SCRIPT-ARGV$" VERIFY:DEFINES? TTRUE
   BFT-READ-BUF u s" : SCRIPT-ARGV$" BFT-FIND BFT-FOUND mark < TTRUE
   BF-TMP-RESET ;

\ Doctored snapshot-trailer regression (TFAM 12 item 6, dot
\ habu-tfam-12-layout-057181a9): the loader EM-SNAPSHOT-RESTORE
\ (src/habu/habu2.f) validates the format-versioned trailer before restoring
\ regions - a version cell that is not SNAP-FORMAT-VERSION exits 80
\ (E-SNAP-VERSION), an oversized region-len/data-len/ndict field exits 79
\ (corrupt trailer). The snapshot binary
\ is built through the SAME gated route
\ the pipeline uses (BF-SNAP-SOURCE + BF-CERTIFY-SNAP + hb-stdin --build
\ hb-snap-src -> hb-snap0, the BF-BUILD-SNAP-FROM-STDIN mechanism): snap.f's
\ former 0 set-check window is now the TRUSTED: RETIRE-AND-PERSIST boundary, so
\ the emitted snap source certifies clean and the `-- snap` route is gated
\ end-to-end again (dot habu-tfam-12-item-346f03c2 part 1).
\ The target header's full 64-bit text-size field owns the trailer location;
\ padding and the codesign blob may follow the trailer.
\ - A patched image must be re-signed (CODESIGN:FORCE) or macOS SIGKILLs it
\   before the loader runs.
\ - EM-SNAPSHOT-RESTORE labels both fatal exits on fd 2 before NR-EXIT-GROUP:
\   rc 79 prints "hb: snapshot trailer corrupt", rc 80 prints
\   "hb: snapshot format version unsupported". BFT-DOCTORED-CAPTURE captures the
\   doctored engine's stderr, so this fixture asserts the diagnostic TEXT and not
\   just the rc.
: BFT-BYTES-FIELD ( -- ptr ptr u8 )
   BFT-BYTES-A 0 ptr-field ;

: BFT-BYTES ( -- ptr u8 )
   BFT-BYTES-FIELD @ ;

\ The image is built from the engine BF-BUILD-SNAP-FROM-STDIN builds it from -
\ the capture host - asked from the tool rather than named here. A snapshot
\ restore skips the AOT seed, so an image made by the seeded product carries
\ call sites nothing resolves and exits 82 at boot; that is the seed refusing
\ correctly, about an engine the build never asks for.
: BFT-SNAP0-BUILD ( -- )
   BF-SNAP-SOURCE
   BF-CERTIFY-SNAP
   s" hb-snap0" BF-REMOVE-TMP
   BF-SNAP-ENGINE$ s" hb-snap-src" COMPILER-BUILD:RUN-TMP BF-RC0
   s" hb-snap0" BF-EXPECT
   s" hb-snap0" BF-CODESIGN-FORCE-TMP
   s" hb-snap0" BF-CHMOD-X-TMP ;

: BFT-EMPTY-STDIN! ( -- )
   s" empty-stdin" BF-A$ BFT-EMPTY$ WRITE-ALL ;

: BFT-SNAP-RUN ( ptr u8 n -- n )
   s" empty-stdin" BF-RUN-ENV-TMP-INFILE ;

: BFT-BYTES-READ ( -- )
   s" hb-snap0" BF-A$ FILE-SIZE {: sz:n :}
   sz MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop BFT-BYTES-FIELD !
   s" hb-snap0" BF-A$ BFT-BYTES sz READ-ALL BFT-BYTES-N ! ;

: BFT-BYTE@ ( n -- n ) {: off:n :}
   BFT-BYTES off BYTE+ c@ ;

: BFT-BYTE! ( n n -- ) {: val:n off:n :}
   val BFT-BYTES off BYTE+ c! ;

: U64@ ( n -- n ) {: off:n :}
   0
   8 0 ?do
      off i + BFT-BYTE@ i 8 * lshift or
   loop ;

: TRAILER-OFF ( -- n )
   IMAGE-TEXT-SIZE-OFF U64@ IMAGE-TEXT-TRAILER-ADJ + SNAP-TRL-BYTES - ;

: DATA-OFF ( -- n )
   TRAILER-OFF dup SNAP-TRL-DATALEN + U64@ - ;

: HOOK-OFF ( -- n )
   DATA-OFF ENGINE-SNAP-XT-CELL + ;

: BFT-DOCTOR-WRITE ( -- )
   s" hb-doctored" BF-REMOVE-TMP
   s" hb-doctored" BF-A$ BFT-BYTES BFT-BYTES-N @ WRITE-ALL
   s" hb-doctored" BF-CODESIGN-FORCE-TMP
   s" hb-doctored" BF-CHMOD-X-TMP ;

variable BFT-DOC-ERR-U
variable BFT-DOC-OUT-U
variable BFT-DOC-EXITED
variable BFT-DOC-CODE

: BFT-DOC-ERR$ ( -- ptr u8 n )
   BFT-ERR BFT-DOC-ERR-U @ ;

: BFT-DOC-OUT$ ( -- ptr u8 n )
   BFT-OUT BFT-DOC-OUT-U @ ;

\ Doctor one trailer byte, run the patched snapshot engine with empty stdin and
\ its stderr CAPTURED (the labeled diagnostic goes to fd 2), record the exit
\ kind/code and stderr length, then restore the byte for the next case.
: BFT-DOCTORED-CAPTURE ( n n -- ) {: off:n val:n :}
   off BFT-BYTE@ {: orig:n :}
   val off BFT-BYTE!
   BFT-DOCTOR-WRITE
   PROC-ARGV-RESET
   s" hb-doctored" BF-A$ >LEN  BFT-EMPTY$ >LEN
   BFT-OUT BFT-CAPTURE-CAP >LEN  BFT-ERR BFT-CAPTURE-CAP >LEN  BFT-TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE-OUTCOME
   MATCH outcome
     exited OF BFT-DOC-CODE ! 0 0= BFT-DOC-EXITED ! ENDOF
     signaled OF BFT-DOC-CODE ! 0 0= 0= BFT-DOC-EXITED ! ENDOF
     timeout OF 0 BFT-DOC-CODE ! 0 0= 0= BFT-DOC-EXITED ! ENDOF
   ;MATCH {: ou:len eu:len :}
   ou LEN>N BFT-DOC-OUT-U !
   eu LEN>N BFT-DOC-ERR-U !
   orig off BFT-BYTE! ;

\ A labeled fatal exit: process EXITed with the contract code and its stderr
\ carries the named diagnostic (proves the exit is no longer a bare rc-only).
: BFT-ASSERT-SNAP-EXIT ( n ptr u8 n -- ) {: code:n msg:ptr msgu:n :}
   BFT-DOC-EXITED @ TTRUE
   BFT-DOC-CODE @ code T=
   BFT-DOC-ERR$ msg msgu CONTAINS? TTRUE ;

: PROBE! ( -- )
   s" snap-hook-probe.f" BF-A$
   s\" package SNAP-HOOK-PROBE public\n: ASSERT-HOOKS ( -- )\n   data-base ENGINE-SNAP-XT-CELL + @ 0 <> if 1 throw then\n   data-base COMPILE-PREFLIGHT-CELL + @ 0= if 2 throw then ;\n;package\nSNAP-HOOK-PROBE:ASSERT-HOOKS\n: BFT-SNAP-PI ( -- ) ; immediate\ns\" BFT-SNAP-PI\" 0 parse-imm\n: BFT-SNAP-OK ( -- n ) BFT-SNAP-PI 73 ;\n: BFT-SNAP-ASSERT ( -- ) BFT-SNAP-OK 73 <> if 3 throw then ;\nBFT-SNAP-ASSERT\n"
   WRITE-ALL ;

: PROBE-ARGV ( -- )
   PROC-ARGV-RESET
   s" --load" BFT-ARG+
   s" snap-hook-probe.f" BF-A$ BFT-ARG+
   s" --" BFT-ARG+
   BFT-ROOT BFT-ARG+ ;

: PROBE-CAPTURE ( -- )
   PROBE-ARGV
   PROC-ENV-RESET
   s" HB_TMP" >LEN BFT-ROOT >LEN PROC-ENV+
   s" hb-doctored" BF-A$ >LEN BFT-OUT BFT-CAPTURE-CAP >LEN
   BFT-ERR BFT-CAPTURE-CAP >LEN BFT-TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME
   MATCH outcome
     exited OF BFT-DOC-CODE ! 0 0= BFT-DOC-EXITED ! ENDOF
     signaled OF BFT-DOC-CODE ! 0 0= 0= BFT-DOC-EXITED ! ENDOF
     timeout OF 0 BFT-DOC-CODE ! 0 0= 0= BFT-DOC-EXITED ! ENDOF
   ;MATCH {: ou:len eu:len :}
   ou LEN>N BFT-DOC-OUT-U !
   eu LEN>N BFT-DOC-ERR-U ! ;

: RAW ( -- )
   HOOK-OFF {: off:n :}
   off 0 >= TTRUE
   off 8 + TRAILER-OFF <= TTRUE
   off U64@ 0 T= ;

: STARTUP ( -- )
   HOOK-OFF {: off:n :}
   off BFT-BYTE@ {: orig:n :}
   FORGE off BFT-BYTE!
   off U64@ 0= TFALSE
   BFT-DOCTOR-WRITE
   s" hb-doctored" BF-CODESIGN-VERIFY-TMP
   PROBE!
   PROBE-CAPTURE
   orig off BFT-BYTE!
   BFT-DOC-EXITED @ TTRUE
   BFT-DOC-ERR$ BFT-EMPTY$ T$=
   BFT-DOC-CODE @ 0 T= ;

: NOHOOK-SOURCE ( -- )
   s" hb-snap-nohook-src" BF-RESET-OUT
   s" hb-snap-nohook-src" BF-APPEND-SNAP-PRESEAL
   s" hb-snap-nohook-src" s" 0 data-base ENGINE-SNAP-XT-CELL + !" BF-APPEND-LINE
   s" hb-snap-nohook-src" s" src/habu/snap.f" BF-APPEND-SNAP-SEALED ;

: NOHOOK-ARGV ( -- )
   PROC-ARGV-RESET
   s" --build" BFT-ARG+
   s" hb-snap-nohook-src" BF-A$ BFT-ARG+
   s" --" BFT-ARG+
   BFT-ROOT BFT-ARG+ ;

: NOHOOK-CAPTURE ( -- )
   NOHOOK-ARGV
   PROC-ENV-RESET
   s" HB_TMP" >LEN BFT-ROOT >LEN PROC-ENV+
   BFT-HB >LEN BFT-OUT BFT-CAPTURE-CAP >LEN
   BFT-ERR BFT-CAPTURE-CAP >LEN BFT-TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME
   MATCH outcome
     exited OF BFT-DOC-CODE ! 0 0= BFT-DOC-EXITED ! ENDOF
     signaled OF BFT-DOC-CODE ! 0 0= 0= BFT-DOC-EXITED ! ENDOF
     timeout OF 0 BFT-DOC-CODE ! 0 0= 0= BFT-DOC-EXITED ! ENDOF
   ;MATCH {: ou:len eu:len :}
   ou LEN>N BFT-DOC-OUT-U !
   eu LEN>N BFT-DOC-ERR-U ! ;

: VERIFY-IMAGE ( -- )
   RAW
   STARTUP ;

: MISSING ( -- )
   BFT-ROOT BF-TMP!
   NOHOOK-SOURCE
   s" snap-hook-missing" s" hb-snap-nohook-src" BF-A$ BF-CERTIFY-GENERATED
   NOHOOK-CAPTURE
   BFT-DOC-EXITED @ TTRUE
   BFT-DOC-CODE @ MISSING-RC T=
   BFT-DOC-OUT$ BFT-EMPTY$ T$=
   BFT-DOC-ERR$ s" snap: engine snapshot hook missing" T$=
   BF-TMP-RESET ;

: TEST-TRAILER ( -- )
   BFT-ROOT BF-TMP!
   BFT-SNAP0-BUILD
   BFT-EMPTY-STDIN!
   s" hb-snap0" BFT-SNAP-RUN 0 T=
   s" hb-snap0" BF-A$ s" lib/prelude.f" BF-RUN-LOAD-STAGE 0 T=
   BFT-BYTES-READ
   VERIFY-IMAGE
   TRAILER-OFF {: tr:n :}
   tr SNAP-TRL-VERSION + BFT-BYTE@ SNAP-FORMAT-VERSION T=
   tr SNAP-TRL-VERSION + 2 BFT-DOCTORED-CAPTURE
   80 s" hb: snapshot format version unsupported" BFT-ASSERT-SNAP-EXIT
   tr SNAP-TRL-VERSION + $FF BFT-DOCTORED-CAPTURE
   80 s" hb: snapshot format version unsupported" BFT-ASSERT-SNAP-EXIT
   \ +4/+3: a MIDDLE byte of the 8-byte field keeps the value positive but
   \ far above REGION/DICT-CAP (top bytes could go negative or SIGSEGV).
   tr SNAP-TRL-REGLEN + 4 + $FF BFT-DOCTORED-CAPTURE
   79 s" hb: snapshot trailer corrupt" BFT-ASSERT-SNAP-EXIT
   tr SNAP-TRL-NDICT + 3 + $FF BFT-DOCTORED-CAPTURE
   79 s" hb: snapshot trailer corrupt" BFT-ASSERT-SNAP-EXIT
   BF-TMP-RESET ;

\ ---- effective source boundary --------------------------------------------
\ The cold prefix already occupies IBUFSZ and the reader performs an EOF probe,
\ so IBUFSZ+1 is not the runtime input boundary. Bracket the boundary with a
\ bounded exponential search, refine it by binary search, then rerun the adjacent
\ successful/failing sizes against the freshly built candidate's --build path.

$10000 constant PROBE-START
1 constant EOF-BYTES
SOURCE-ARENA-CAP 2 / constant PREV-CAP

variable EXITED
variable EXIT-CODE
variable ERR-U
variable SRC-A
variable OK-N
variable BAD-N

: SRC-FIELD ( -- ptr ptr u8 )
   SRC-A 0 ptr-field ;

: SRC-BUF ( -- ptr u8 )
   SRC-FIELD @ ;

: SRC-ALLOC ( -- )
   SRC-A @ 0 <> if exit then
   SOURCE-ARENA-CAP MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop SRC-FIELD !
   SOURCE-ARENA-CAP 0 ?do 32 SRC-BUF i + c! loop ;

: ERR$ ( -- ptr u8 n )
   BFT-ERR ERR-U @ ;

: WRITE-SRC ( n -- ) {: bytes:n :}
   s" bft-srcfull.f" BF-A$ SRC-BUF bytes WRITE-ALL ;

: RUN-CANDIDATE ( -- )
   PROC-ENV-RESET
   s" HB_TMP" >LEN BFT-ROOT >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   BFT-HB >LEN BFT-OUT BFT-CAPTURE-CAP >LEN
   BFT-ERR BFT-CAPTURE-CAP >LEN BFT-TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME
   MATCH outcome
     exited OF EXIT-CODE ! 0 0= EXITED ! ENDOF
     signaled OF EXIT-CODE ! 0 0= 0= EXITED ! ENDOF
     timeout OF 0 EXIT-CODE ! 0 0= 0= EXITED ! ENDOF
   ;MATCH {: ou:len eu:len :}
   eu LEN>N ERR-U ! ;

: RUN-SOURCE ( -- )
   PROC-ARGV-RESET
   s" --build" >LEN PROC-ARGV+
   s" bft-srcfull.f" BF-A$ >LEN PROC-ARGV+
   RUN-CANDIDATE ;

: PROBE ( n -- bool ) {: bytes:n :}
   bytes WRITE-SRC
   RUN-SOURCE
   EXITED @ 0= if
      EXITED @ TTRUE
      0 0= 0= exit
   then
   EXIT-CODE @ 0= if
      ERR-U @ 0 T=
      0 0= exit
   then
   EXIT-CODE @ 74 T=
   ERR$ s" hb: source prefix buffer full" CONTAINS? TTRUE
   0 0= 0= ;

: EXP-NEXT ( -- n )
   OK-N @ 2 * SOURCE-ARENA-CAP min ;

: EXP-STEP ( -- bool )
   EXP-NEXT {: bytes:n :}
   bytes PROBE if
      bytes SOURCE-ARENA-CAP < {: below:bool :}
      below TTRUE
      below if
         bytes OK-N !
         0 0= 0= exit
      then
      bytes BAD-N !
      0 0= exit
   then
   bytes BAD-N !
   0 0= ;

: EXP ( -- )
   PROBE-START PROBE TTRUE
   PROBE-START OK-N !
   begin EXP-STEP until ;

: MID ( -- n )
   OK-N @ BAD-N @ OK-N @ - 2 / + ;

: BINARY ( -- )
   begin BAD-N @ OK-N @ - 1 > while
      MID {: bytes:n :}
      bytes PROBE if
         bytes OK-N !
      else
         bytes BAD-N !
      then
   repeat ;

: LIVE-SIZE ( -- n )
   SOURCE-ARENA-CAP OK-N @ - EOF-BYTES > TTRUE
   SOURCE-ARENA-CAP OK-N @ - EOF-BYTES -
   BF-STAGE2-SOURCE
   s" stage2-src" BF-A$ FILE-SIZE + ;

: POLICY ( -- )
   LIVE-SIZE SOURCE-ARENA:NEED {: need:n :}
   SOURCE-ARENA-CAP need >= TTRUE
   need SOURCE-ARENA:NEXT-POW2 SOURCE-ARENA-CAP T= ;

: SOURCE-BOUNDARY ( -- )
   BFT-ROOT BF-TMP!
   SRC-ALLOC
   EXP
   BINARY
   BAD-N @ OK-N @ 1 + T=
   OK-N @ PREV-CAP > TTRUE
   OK-N @ PROBE TTRUE
   BAD-N @ PROBE TFALSE
   POLICY
   BF-TMP-RESET ;

: RUN-BUILD ( ptr u8 n -- )
   PROC-ARGV-RESET
   s" --build" >LEN PROC-ARGV+
   >LEN PROC-ARGV+
   RUN-CANDIDATE ;

: EXPECT-OK ( -- )
   EXITED @ TTRUE
   EXIT-CODE @ 0 T=
   ERR-U @ 0 T= ;

: EXPECT-74 ( ptr u8 n -- ) {: diag:ptr diagu:n :}
   EXITED @ TTRUE
   EXIT-CODE @ 74 T=
   ERR$ diag diagu T$= ;

: STAGE2-DRIVER$ ( -- ptr u8 n )
   s" bft-stage2-driver.f" ;

: STAGE2-DRIVER ( -- ptr u8 n )
   STAGE2-DRIVER$ BF-A$ ;

: DRIVER-BASE ( ptr u8 n -- ) {: out:ptr outu:n :}
   out outu BF-RESET-OUT
   out outu BF-APPEND-RUN-PRELUDE
   out outu BF-APPEND-COMMON
   out outu COMPILER-BUILD:SEAL
   out outu BF-APPEND-DRIVER-IO ;

: WRITE-STAGE2-DRIVER ( -- )
   STAGE2-DRIVER$ {: out:ptr outu:n :}
   out outu DRIVER-BASE
   out outu s" src/habu/stage2.f" s" : RUN ( -- )" BF-APPEND-SOURCE-BEFORE
   out outu s" : BFT-S2-READ-EXIT ( -- ) READ-SRC DRV-EXIT-OK ;" BF-APPEND-LINE
   out outu s" BFT-S2-READ-EXIT" BF-APPEND-LINE ;

: WRITE-SPACES ( ptr u8 n n -- ) {: path:ptr pathu:n u:n :}
   path pathu SRC-BUF u WRITE-ALL ;

: STAGE2 ( -- )
   BFT-ROOT BF-TMP!
   SRC-ALLOC
   WRITE-STAGE2-DRIVER
   BFT-STAGE2 SOURCE-ARENA-CAP 1 - WRITE-SPACES
   STAGE2-DRIVER RUN-BUILD
   EXPECT-OK
   BFT-STAGE2 SRC-BUF 1 APPEND-FILE
   STAGE2-DRIVER RUN-BUILD
   s" stage2: source exceeds buffer" EXPECT-74
   BF-TMP-RESET ;

: MAKER-SOURCE ( -- ptr u8 n )
   s" hb-maker-src" BF-A$ ;

: MAKER-DRIVER$ ( -- ptr u8 n )
   s" bft-maker-driver.f" ;

: MAKER-DRIVER ( -- ptr u8 n )
   MAKER-DRIVER$ BF-A$ ;

: WRITE-MAKER-DRIVER ( -- )
   MAKER-DRIVER$ {: out:ptr outu:n :}
   out outu DRIVER-BASE
   out outu s" src/habu/maker.f" s" : MK-RUN" BF-APPEND-SOURCE-BEFORE
   out outu s" LOWER-CERT-HOOK:INSTALL" BF-APPEND-LINE
   out outu S\" s\" MK-READ-SRC\" s\" --\" TRUST" BF-APPEND-LINE
   out outu s" : BFT-MK-READ-EXIT ( -- ) MK-READ-SRC DRV-EXIT-OK ;" BF-APPEND-LINE
   out outu s" BFT-MK-READ-EXIT" BF-APPEND-LINE ;

: MAKER ( -- )
   BFT-ROOT BF-TMP!
   SRC-ALLOC
   WRITE-MAKER-DRIVER
   MAKER-SOURCE SOURCE-ARENA-CAP 1 - WRITE-SPACES
   MAKER-DRIVER RUN-BUILD
   EXPECT-OK
   MAKER-SOURCE SRC-BUF 1 APPEND-FILE
   MAKER-DRIVER RUN-BUILD
   s" maker: source exceeds buffer" EXPECT-74
   BF-TMP-RESET ;

: BFT-TEST-TMP-OVERRIDE ( -- )
   BFT-ROOT BF-TMP!
   BF-TMP$ BFT-ROOT T$=
   s" stage2-src" BF-A$ BFT-STAGE2 T$=
   BF-STAGE2-SOURCE
   BFT-STAGE2 FILE? TTRUE
   BFT-TEST-STAGE2-SCRATCH
   BF-TMP-RESET ;

: BFT-TEST-STAGE-ARGV-RESET ( -- )
   BFT-ROOT BF-TMP!
   PROC-ARGV-RESET
   s" stale" >LEN PROC-ARGV+
   s" bin/hb" BF-PREPARE-STAGE-ARGV 2drop
   PROC-ARGV-N @ COUNT>N 2 T=
   PROC-ARGV-RESET
   s" stale" >LEN PROC-ARGV+
   s" bin/hb" s" stage2-src" BF-A$ BF-PREPARE-LOAD-STAGE-ARGV 2drop
   PROC-ARGV-N @ COUNT>N 4 T=
   BF-TMP-RESET ;

: BFT-BUILD-FILE? ( ptr u8 n -- bool )
   BASENAME s" build-" STARTS-WITH? ;

: BFT-CHECK-BUILD-FILE ( ptr u8 n -- ) {: a:ptr u :}
   a u FILE? if
      a u BFT-BUILD-FILE? if
         BFT-BUILD-FILES @ 1 + BFT-BUILD-FILES !
      then
   then ;

: BFT-TEST-NO-BUILD-SHIMS ( -- )
   0 BFT-BUILD-FILES !
   BFT-ROOT [: BFT-CHECK-BUILD-FILE ;] WALK-FILES
   BFT-BUILD-FILES @ 0 T= ;

: BFT-CERT-WRITE ( ptr u8 n -- )
   BFT-CERT 2swap WRITE-ALL ;

: BFT-TEST-CERTIFY-GOOD ( -- )
   s" : BFT-CERT-GOOD ( n -- n ) 1 + ;" BFT-CERT-WRITE
   s" cert-good" BFT-CERT BF-CERTIFY-RC 0 T= ;

: BFT-TEST-CERTIFY-BAD ( -- )
   s" : BFT-CERT-BAD ( n -- n ) drop ;" BFT-CERT-WRITE
   s" cert-bad" BFT-CERT BF-CERTIFY-RC 70 T=
   BF-CERT-DIAG-U @ 0 > TTRUE ;

\ Preflight-retirement regression: the retired habu1/habu2/icode typed-shape
\ asserts guarded emitter words against stack-effect regressions; their
\ replacement is the checker itself (habu1/habu2 compile checked in the stage;
\ icode is covered by the blocking static certify). Prove the real check
\ rejects the guarded corruption - an emitter body underflowing its declared
\ inputs (the spawn-descriptor-underflow class the asserts were added for).
: BFT-TEST-RETIRE-REGRESSION ( -- )
   s" : SPAWN-DUP2-ACTION ( n n -- ) drop drop drop ;" BFT-CERT-WRITE
   s" retire-spawn-underflow" BFT-CERT BF-CERTIFY-RC 70 T=
   BF-CERT-DIAG-U @ 0 > TTRUE ;

\ Minimal refresh-prelude for scratch generated-source fixtures: one
\ BFR-CHECK-OFF line followed by one LOWER-CERT-HOOK:INSTALL line, the same
\ shape the real emitted stage source opens with.
: BFT-CERT-PRELUDE+ ( -- )
   s\" \\ audit prelude\nBFR-CHECK-OFF\nLOWER-CERT-HOOK:INSTALL\n" SB-APPEND ;

: BFT-CERT-SB-WRITE ( -- )
   BFT-CERT SB$ WRITE-ALL ;

: BFT-CERT-LINE+ ( ptr u8 n -- )
   SB-APPEND
   BF-LF SB-APPEND-C ;

\ Certification is BLOCKING: a generated stage source that rejects must fail
\ the build with E-BUILD-CERTIFY, not warn and proceed (fail-open). The
\ install-path proof is BFT-TEST-CERT-INJECT-INSTALL below; this pins the
\ unit behavior on a scratch source whose only defect is the type-broken
\ definition.
: BFT-TEST-CERTIFY-BLOCKING ( -- )
   SB-RESET
   BFT-CERT-PRELUDE+
   s" : BFT-CERT-BAD2 ( n -- n ) drop ;" BFT-CERT-LINE+
   BFT-CERT-SB-WRITE
   [: s" cert-blocking" BFT-CERT BF-CERTIFY-GENERATED ;] E-BUILD-CERTIFY TTHROWSQ ;

: BFT-TEST-CERTIFY-GOOD-PASSES ( -- )
   SB-RESET
   BFT-CERT-PRELUDE+
   s" : BFT-CERT-GOOD2 ( n -- n ) 1 + ;" BFT-CERT-LINE+
   BFT-CERT-SB-WRITE
   s" cert-good2" BFT-CERT BF-CERTIFY-GENERATED ;


\ Self-certification guard: checker.f must certify as the tail of its exact
\ pre-hook prefix. Its layout assertions consume cell.f's CORE-LAYOUT-RC and
\ PTR-VARIABLE has its own pre-checker owner. The generic structure DSL is
\ deliberately post-hook and must not enter this prefix. The source verifier
\ remains independently checked through the same VERIFY:SOURCE-BUF path.
: BFT-CERT-CHECKER$ ( -- ptr u8 n )
   s" cert-checker" BF-A$ ;

: BFT-CERT-CHECKER-BASE ( ptr u8 n -- ) {: out:ptr outu:n :}
   out outu BF-RESET-OUT
   out outu s" src/core/util.f" BF-APPEND-SOURCE
   out outu s" src/core/cell.f" BF-APPEND-SOURCE
   out outu s" src/core/pointer-storage.f" BF-APPEND-SOURCE
   out outu s" src/core/engine-error.f" BF-APPEND-SOURCE
   out outu s" src/core/checker.f" BF-APPEND-SOURCE ;

: BFT-TEST-CERTIFY-CHECKER-SELF ( -- )
   BFT-ROOT BF-TMP!
   s" cert-checker" BFT-CERT-CHECKER-BASE
   s" checker-self" BFT-CERT-CHECKER$ BF-CERTIFY-RC 0 T=
   s" verify-source-self" s" src/habu/verify-source.f" BF-CERTIFY-RC 0 T=
   BF-TMP-RESET ;

\ Per-file TFAM-prefix certification: type-schema.f, type-family.f, render.f,
\ and sumtype.f certify clean via the same VERIFY:SOURCE-BUF path, each
\ verified as the tail of its exact BF-APPEND-CHECKER-BOOT prefix context
\ (util, cell, pointer storage, engine error, checker, then the earlier TFAM
\ files), so de-typing any one file fails its own assert. render.f sits between
\ type-family.f and sumtype.f in the real prefix and certifies since its cleanup
\ (habu-make-fixpoint-certify-a11dbad5).
: BFT-CERT-TFAM$ ( -- ptr u8 n )
   s" cert-tfam" BF-A$ ;

: BFT-CERT-TFAM-BASE ( -- )
   s" cert-tfam" {: out:ptr outu:n :}
   out outu BFT-CERT-CHECKER-BASE
   out outu s" src/core/engine-error-effects.f" BF-APPEND-SOURCE
   out outu s" src/core/lower-cert-base.f" BF-APPEND-SOURCE ;

: BFT-TEST-CERTIFY-TFAM-PREFIX ( -- )
   BFT-ROOT BF-TMP!
   BFT-CERT-TFAM-BASE
   s" cert-tfam" s" src/core/type-schema.f" BF-APPEND-SOURCE
   s" tfam-type-schema" BFT-CERT-TFAM$ BF-CERTIFY-RC 0 T=
   s" cert-tfam" s" src/core/type-family.f" BF-APPEND-SOURCE
   s" tfam-type-family" BFT-CERT-TFAM$ BF-CERTIFY-RC 0 T=
   s" cert-tfam" s" src/core/render.f" BF-APPEND-SOURCE
   s" tfam-render" BFT-CERT-TFAM$ BF-CERTIFY-RC 0 T=
   s" cert-tfam" s" src/core/sumtype.f" BF-APPEND-SOURCE
   s" tfam-sumtype" BFT-CERT-TFAM$ BF-CERTIFY-RC 0 T=
   BF-TMP-RESET ;

\ The stage2 and stdin build phases both emit into the one fixed `stage2-src`
\ stage-input path. The first generation uses COMPILER-BUILD's verified
\ `--build` route; later hb-stage generations read that same path. Therefore
\ BF-CERTIFY-STAGE2 and
\ BF-CERTIFY-STDIN read the same path at different times. Prove the certify path
\ exists in each phase and that the stdin phase OVERWRITES it with distinct
\ content — so BF-CERTIFY-STDIN certifies the stdin driver source, not stage2 twice.
\ The boot prefix is a certify phase of its own, and it is BLOCKING. The
\ subject is the real assembly through the real phase word, not a hand-built
\ stand-in: BF-PREFIX-SOURCE writes the bytes the build writes and
\ BF-CERTIFY-PREFIX is the word the build calls. The good case proves those
\ exact bytes certify; the bad case is the SAME bytes with one type-broken
\ definition appended, which must throw rather than warn - the fail-open
\ variant would pass the good case identically.
\ The two membership assertions are structural, not decorative: they name a
\ definition from the first checker-boot file and one from deep inside
\ checker.f, so an assembly that silently emitted nothing, or stopped after its
\ first file, still certifies clean and would pass without them.
: BFT-TEST-CERTIFY-BOOT-PREFIX ( -- )
   BFT-ROOT BF-TMP!
   BF-PREFIX-SOURCE
   BFT-PREFIX FILE? TTRUE
   BF-CERTIFY-PREFIX
   BFT-PREFIX BFT-READ {: u:n :}
   BFT-READ-BUF u s" : CORE-STR=" CONTAINS? TTRUE
   BFT-READ-BUF u s" checker-registry.f - typed checker effect store" CONTAINS? TTRUE
   s" prefix-src" BF-A$ s" : BFT-PFX-BAD ( n -- n ) drop ;" APPEND-FILE
   [: BF-CERTIFY-PREFIX ;] E-BUILD-CERTIFY TTHROWSQ
   BF-TMP-RESET ;

: BFT-TEST-CERTIFY-PHASE-SOURCES ( -- )
   BFT-ROOT BF-TMP!
   BF-STAGE2-SOURCE
   BFT-STAGE2 FILE? TTRUE
   BF-RECORD-STAGE
   BF-STDIN-SOURCE
   BFT-STAGE2 FILE? TTRUE
   BF-RECORD-STDIN
   BF-REC-STAGE-DG BF-STAMP-DG-U BF-REC-STDIN-DG BF-STAMP-DG-U STR= TFALSE
   BF-TMP-RESET ;

\ Hash-pin mismatch: pin a sandbox boot-prefix file, reload unchanged (no
\ throw), then mutate it mid-sequence - the reload must fail closed with
\ E-BUILD-BOOT-DRIFT rather than silently entering the image.
: BFT-PIN-RELOAD ( -- )
   BFT-CERT BF-PIN-FILE ;

: BFT-TEST-BOOT-PIN ( -- )
   BF-PIN-RESET
   BF-PIN-ON!
   s" \ boot prefix v1" BFT-CERT-WRITE
   BFT-CERT BF-PIN-FILE
   BFT-CERT BF-PIN-FILE
   BFT-CERT s" \ mid-build edit" APPEND-FILE
   [: BFT-PIN-RELOAD ;] E-BUILD-BOOT-DRIFT TTHROWSQ
   BF-PIN-OFF!
   BF-PIN-RESET ;

\ The native chain's contribution to the stamp key (package STAMP-KEY in
\ tools/build-fixpoint.f). Two claims, and the second is what makes the first
\ mean anything for the real refresh.
\
\ CLOSURE-KEY runs the production digest word over a fixture entry, so the
\ closure it walks is built here rather than read from the tree: a content edit
\ to a file the entry loads must change the digest, an edit to a file it does
\ not load must leave the digest alone, and restoring the first file's bytes
\ must restore the original digest exactly. The entry carries two decoys - a
\ `require` of the unrelated file inside a `\` comment, and the same text inside
\ a string literal - so a discovery pass that matched loader text instead of
\ loader FORMS would pull the unrelated file into the closure and fail both the
\ membership assertions and the untouched-digest claim.
\
\ STAMP-FOLD then pins that the refresh's own key preimage carries that digest,
\ for THIS repository's chain entry: it rebuilds the length-framed `chain-src`
\ fragment BF-STAMP-KEY-BEGIN is supposed to have appended and finds those exact
\ bytes in the preimage. The same fragment built from the fixture entry's digest
\ must be absent, so keying the wrong entry file fails here rather than passing
\ on the tag alone.

variable ENTRY-U
variable DEP-U
variable OTHER-U
create ENTRY-BUF FS-PATH-CAP allot
create DEP-BUF FS-PATH-CAP allot
create OTHER-BUF FS-PATH-CAP allot
create DG-A 40 allot
create DG-B 40 allot
create DG-C 40 allot

: FIXTURE-ENTRY$ ( -- ptr u8 n )   ENTRY-BUF ENTRY-U @ ;
: DEP$ ( -- ptr u8 n )     DEP-BUF DEP-U @ ;
: OTHER$ ( -- ptr u8 n )   OTHER-BUF OTHER-U @ ;

\ Entry source: one real `require`, plus the same loader text hidden in a
\ comment and in a string literal.
: ENTRY-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" \ require " SB-APPEND OTHER$ SB-APPEND BFT-NL 1 SB-APPEND
   s\" s\" require " SB-APPEND OTHER$ SB-APPEND 34 SB-APPEND-C BFT-NL 1 SB-APPEND
   s" require " SB-APPEND DEP$ SB-APPEND BFT-NL 1 SB-APPEND
   SB$ ;

: WRITE-FIXTURES ( -- )
   DEP$ s\" \\ dep v1\n" WRITE-ALL
   OTHER$ s\" \\ other v1\n" WRITE-ALL
   FIXTURE-ENTRY$ ENTRY-SRC$ WRITE-ALL ;

: MEMBER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0 begin dup EC:COUNT < while
      dup EC:PATH$ a u STR= if drop BF-TRUE exit then
      1+
   repeat drop BF-FALSE ;

\ One preimage field, as the key writes it: a length byte then that many bytes.
: FIELD+ ( ptr u8 n -- ) {: a:ptr u:n :}
   u SB-APPEND-C
   a u SB-APPEND ;

\ The framed capture-source fragment BF-STAMP-KEY-BEGIN appends for a given
\ digest: the tag as one field, the digest as the next. The tag comes from the
\ tool (BF-STAMP-CAPTURE-TAG$), not from a copy here - a copy is what let this
\ case go on looking for `chain-src` after the key started writing
\ `capture-src`, while the negative case beside it passed for the wrong reason.
: FRAGMENT$ ( ptr u8 -- ptr u8 n ) {: dg:ptr :}
   SB-RESET
   BF-STAMP-CAPTURE-TAG$ FIELD+
   dg BF-STAMP-DG-U FIELD+
   SB$ ;

: PREIMAGE-HAS? ( ptr u8 n -- bool )
   BF-STAMP-BUF BF-STAMP-U @ 2swap CONTAINS? ;

: PREP ( -- )
   BFT-ROOT s" chain-entry.f" ENTRY-BUF ENTRY-U BFT-PATH!
   BFT-ROOT s" chain-dep.f" DEP-BUF DEP-U BFT-PATH!
   BFT-ROOT s" chain-other.f" OTHER-BUF OTHER-U BFT-PATH! ;

: TEST-CLOSURE-KEY ( -- )
   PREP
   WRITE-FIXTURES
   FIXTURE-ENTRY$ EC:BUILD
   EC:COUNT 2 T=
   FIXTURE-ENTRY$ MEMBER? TTRUE
   DEP$ MEMBER? TTRUE
   OTHER$ MEMBER? TFALSE
   FIXTURE-ENTRY$ DG-A CHAIN-DIGEST!
   DEP$ s\" \\ dep v2\n" APPEND-FILE
   FIXTURE-ENTRY$ DG-B CHAIN-DIGEST!
   DG-A BF-STAMP-DG-U DG-B BF-STAMP-DG-U STR= TFALSE
   OTHER$ s\" \\ other v2\n" APPEND-FILE
   FIXTURE-ENTRY$ DG-C CHAIN-DIGEST!
   DG-B BF-STAMP-DG-U DG-C BF-STAMP-DG-U STR= TTRUE
   DEP$ s\" \\ dep v1\n" WRITE-ALL
   FIXTURE-ENTRY$ DG-C CHAIN-DIGEST!
   DG-A BF-STAMP-DG-U DG-C BF-STAMP-DG-U STR= TTRUE ;

: TEST-STAMP-FOLD ( -- )
   PREP
   WRITE-FIXTURES
   ENTRY$ DG-A CHAIN-DIGEST!
   FIXTURE-ENTRY$ DG-B CHAIN-DIGEST!
   DG-A BF-STAMP-DG-U DG-B BF-STAMP-DG-U STR= TFALSE
   BF-STAMP-KEY-BEGIN
   DG-A FRAGMENT$ PREIMAGE-HAS? TTRUE
   DG-B FRAGMENT$ PREIMAGE-HAS? TFALSE ;

\ typed-local-lint: allow-bare-local - q keeps the named subtest quotation effect.
: BFT-STEP ( ptr u8 n [ -- ] -- ) {: a:ptr u:n q :}
   a u T-LABEL
   q catch {: rc:n :}
   rc 0= if exit then
   a u type s" : throw " type rc . cr
   s" build-fixpoint-test: subtest threw" T-EX-FAIL die ;

\ Public so the driver below can run it with the package CLOSED: the subtests
\ certify generated engine sources in this process (VERIFY:SOURCE-BUF), and the
\ checker resolves the verified source's names in whatever package scope is open
\ when it runs.
public
\ Growing the shared source buffer must not accumulate mappings. One measured
\ growth of S pages is the unit; growing S -> 2S -> 4S -> 8S afterwards costs 8
\ units live, and 15 if every superseded span is kept. The bar sits between them,
\ and the whole assertion is a RATIO of measured pages, so it assumes no page
\ size. Drives BF-SOURCE-ENSURE directly: the leak is in the growth path, not in
\ whatever read happens to call it.
1024 1024 * constant BFT-GROW-STEP

: BFT-GROW ( n -- )
   s" (growth probe)" rot BF-SOURCE-ENSURE ;

: BFT-SOURCE-GROWTH-RELEASES ( -- )
   VMSIZE:PAGES {: m0:n :}
   BFT-GROW-STEP BFT-GROW
   VMSIZE:PAGES {: m1:n :}
   BFT-GROW-STEP 2 * BFT-GROW
   BFT-GROW-STEP 4 * BFT-GROW
   BFT-GROW-STEP 8 * BFT-GROW
   VMSIZE:PAGES {: m2:n :}
   m1 m0 - {: unit:n :}
   unit 0 > TTRUE
   m2 m0 - unit 11 * <= TTRUE ;

: BFT-RUN-ALL ( -- )
   T-RESET
   BFT-PREPARE
   s" tmp override" [: BFT-TEST-TMP-OVERRIDE ;] BFT-STEP
   s" stage argv reset" [: BFT-TEST-STAGE-ARGV-RESET ;] BFT-STEP
   s" stamp seed" [: BFT-TEST-STAMP-SEED ;] BFT-STEP
   s" build" [: BFT-TEST-BUILD ;] BFT-STEP
   s" cached skip" [: BFT-TEST-CACHED-SKIP ;] BFT-STEP
   s" build fail no stamp" [: BFT-TEST-BUILD-FAIL-NO-STAMP ;] BFT-STEP
   s" no-main self dispatch" [: BFT-TEST-NO-MAIN-DISPATCHES ;] BFT-STEP
   s" missing preamble diag" [: BFT-TEST-MISSING-PREAMBLE ;] BFT-STEP
   s" stale seed install" [: BFT-TEST-STALE-INSTALL ;] BFT-STEP
   s" cert inject install" [: BFT-TEST-CERT-INJECT-INSTALL ;] BFT-STEP
   s" watermark required" [: BFT-TEST-WATERMARK-REQUIRED ;] BFT-STEP
   s" watermark value" [: BFT-TEST-WATERMARK-VALUE ;] BFT-STEP
   s" stamp source key" [: BFT-TEST-STAMP-SOURCE-KEY ;] BFT-STEP
   s" chain closure key" [: TEST-CLOSURE-KEY ;] BFT-STEP
   s" chain stamp fold" [: TEST-STAMP-FOLD ;] BFT-STEP
   s" stamp corrupt" [: BFT-TEST-STAMP-CORRUPT ;] BFT-STEP
   s" stamp engine" [: BFT-TEST-STAMP-ENGINE ;] BFT-STEP
   s" all stamp guard" [: BFT-TEST-ALL-STAMP-GUARD ;] BFT-STEP
   s" stamp nested" [: BFT-TEST-STAMP-NESTED ;] BFT-STEP
   s" no build shims" [: BFT-TEST-NO-BUILD-SHIMS ;] BFT-STEP
   s" certify good" [: BFT-TEST-CERTIFY-GOOD ;] BFT-STEP
   s" certify bad" [: BFT-TEST-CERTIFY-BAD ;] BFT-STEP
   s" retire regression" [: BFT-TEST-RETIRE-REGRESSION ;] BFT-STEP
   s" certify blocking" [: BFT-TEST-CERTIFY-BLOCKING ;] BFT-STEP
   s" boot pin mismatch" [: BFT-TEST-BOOT-PIN ;] BFT-STEP
   s" certify good passes" [: BFT-TEST-CERTIFY-GOOD-PASSES ;] BFT-STEP
   s" certify checker self" [: BFT-TEST-CERTIFY-CHECKER-SELF ;] BFT-STEP
   s" certify tfam prefix" [: BFT-TEST-CERTIFY-TFAM-PREFIX ;] BFT-STEP
   s" certify boot prefix" [: BFT-TEST-CERTIFY-BOOT-PREFIX ;] BFT-STEP
   s" certify phase sources" [: BFT-TEST-CERTIFY-PHASE-SOURCES ;] BFT-STEP
   s" defines scan" [: BFT-TEST-DEFINES-SCAN ;] BFT-STEP
   s" stage2 source" [: BFT-TEST-STAGE2-SOURCE ;] BFT-STEP
   s" no stage2 run source" [: BFT-TEST-NO-STAGE2-RUN-SOURCE ;] BFT-STEP
   s" checked target image" [: BFT-TEST-CHECKED-TARGET-IMAGE ;] BFT-STEP
   s" checked regalloc" [: BFT-TEST-CHECKED-REGALLOC ;] BFT-STEP
   s" snap source" [: BFT-TEST-SNAP-SOURCE ;] BFT-STEP
   s" snap missing hook" [: MISSING ;] BFT-STEP
   s" snap trailer" [: TEST-TRAILER ;] BFT-STEP
   s" source boundary" [: SOURCE-BOUNDARY ;] BFT-STEP
   s" stage2 source cap" [: STAGE2 ;] BFT-STEP
   s" maker source cap" [: MAKER ;] BFT-STEP
   s" source buffer growth releases" [: BFT-SOURCE-GROWTH-RELEASES ;] BFT-STEP
   CLEANUP-RUN
   BFT-ROOT EXISTS? TFALSE
   T-REPORT
   s" build-fixpoint-test: ok" type cr ;

;package

BUILD-FIXPOINT:BFT-RUN-ALL
