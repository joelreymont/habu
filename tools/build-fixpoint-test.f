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

8192 constant BFT-CAPTURE-CAP
$40000 constant BFT-BIG-CAP
120000 constant BFT-TIMEOUT-MS
13 constant BFT-BUILD-ARGV#

\ Snapshot trailer field offsets from the magic cell (src/habu/snap-lib.f
\ SNAP-WRITE-BYTES): magic +0, text base +8, ndict +16, region len +24,
\ data len +32.
16 constant BFT-TRL-NDICT
24 constant BFT-TRL-REGLEN

package BFT-SNAP-HOOK
private
32 constant TRL-DATALEN
$A5 constant FORGE
$4A constant MISSING-RC
;package

variable BFT-ROOT-U
variable BFT-HB-NEW-U
variable BFT-HB-U
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
   BF-STAGE2-SOURCE
   BF-RECORD-STAGE
   BF-STDIN-SOURCE
   BF-RECORD-STDIN ;

: BFT-TEST-BUILD ( -- )
   BFT-RUN-BUILD 0 T=
   {: outu erru :}
   BFT-ERR erru BFT-EMPTY$ T$=
   BFT-OUT outu s" bin/hb refresh OK: compiler fixpoint" CONTAINS? TTRUE
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

: BFT-STAGE-MUTATED-KEY! ( -- )
   BF-STAMP-KEY-BEGIN
   BF-STAGE2-SOURCE
   s" stage2-src" BF-A$ s" \ mutated stage source" APPEND-FILE
   BF-STAMP-STAGE-KEY+
   BF-STDIN-SOURCE
   BF-STAMP-STDIN-KEY+
   BF-STAMP-KEY-END ;

: BFT-STDIN-MUTATED-KEY! ( -- )
   BF-STAMP-KEY-BEGIN
   BF-STAGE2-SOURCE
   BF-STAMP-STAGE-KEY+
   BF-STDIN-SOURCE
   s" stage2-src" BF-A$ s" \ mutated stdin source" APPEND-FILE
   BF-STAMP-STDIN-KEY+
   BF-STAMP-KEY-END ;

: BFT-TEST-STAMP-SOURCE-KEY ( -- )
   BFT-STAMP-SCOPE
   BF-STAMP-KEY!
   BF-STAMP-KEY BFT-KEY1 BF-STAMP-HEX-U BYTE-COPY
   BFT-STAGE-MUTATED-KEY!
   BFT-KEY1 BF-STAMP-HEX-U BF-STAMP-KEY BF-STAMP-HEX-U STR= TFALSE
   BFT-STDIN-MUTATED-KEY!
   BFT-KEY1 BF-STAMP-HEX-U BF-STAMP-KEY BF-STAMP-HEX-U STR= TFALSE
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
   BFT-STALE s" src/habu/hide.f" BFT-STALE-HIDE-BUF BFT-STALE-HIDE-U BFT-PATH! ;

: BFT-STALE-PREPARE ( -- )
   BFT-STALE-PATHS!
   BFT-ALLOC-BIG
   BFT-STALE-TMP MAKE-DIRS
   s" src" BFT-STALE-COPY-TREE
   s" lib" BFT-STALE-COPY-TREE
   s" tools/build-fixpoint.f" BFT-STALE-COPY-FILE
   s" tools/build-fixpoint-main.f" BFT-STALE-COPY-FILE
   s" tools/stdin-closure-lib.f" BFT-STALE-COPY-FILE
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
: BFT-CERT-INJ-SABOTAGE ( -- )
   s" src/habu/hide.f" BFT-READ {: u:n :}
   BFT-STALE-HIDE BFT-READ-BUF u WRITE-ALL
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
package CAD-NUM
public
: BFT-IX>N ( CAD-NUM:index -- n ) INDEX>N ;
;package
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

: BFT-TEST-STAGE2-SOURCE ( -- )
   BFT-STAGE2 BFT-READ {: u :}
   BFT-READ-BUF u s" : HOOK ( ptr u8 n -- n ) CHECK! dup -1 <> if 70 throw then ; ' HOOK set-check" CONTAINS? TFALSE
   BFT-READ-BUF u s" variable SEQ" CONTAINS? TTRUE
   BFT-READ-BUF u s" : STR=" CONTAINS? TFALSE
   BFT-READ-BUF u s" : CORE-STR=" CONTAINS? TTRUE
   BFT-READ-BUF u s" include src/core/checker-registry.f" CONTAINS? TFALSE
   BFT-READ-BUF u s" checker-registry.f - typed checker effect store" CONTAINS? TTRUE
   BFT-READ-BUF u s" src/core/include.f" CONTAINS? TTRUE
   BFT-READ-BUF u s" TRUSTED: INCLUDE-MMAP-PTR ( n -- ptr u8 )" CONTAINS? TTRUE
   BFT-READ-BUF u s" TRUSTED: INCLUDE-EVALUATE ( ptr u8 n -- )" CONTAINS? TTRUE
   BFT-READ-BUF u s" : include ( -- )" CONTAINS? TTRUE
   BFT-READ-BUF u s" BFR-USIGS-RESET" CONTAINS? TTRUE
   BFT-READ-BUF u s" BFR-CHECK-OFF" CONTAINS? TTRUE
   BFT-READ-BUF u s" BFR-HIDE-DICT-FROM-EARLIEST" CONTAINS? TTRUE
   BFT-READ-BUF u s" : ATOMA-FIELD ( n -- ptr ptr u8 )" CONTAINS? TTRUE
   BFT-READ-BUF u s" : ATOMA-FIELD" CONTAINS? TTRUE
   BFT-READ-BUF u s" 0 constant T-CON" CONTAINS? TTRUE
   BFT-READ-BUF u s" LOWER-CERT-HOOK:INSTALL" CONTAINS? TTRUE
   BFT-READ-BUF u s" undefine FULL-XT" BFT-FIND BFT-FOUND {: seal-base:n :}
   BFT-READ-BUF u seal-base s" SEAL-FRIEND" BFT-FIND-AFTER BFT-FOUND {: seal:n :}
   BFT-READ-BUF u seal s" \ driver-io.f" BFT-FIND-AFTER BFT-FOUND {: driver:n :}
   seal-base seal < TTRUE
   seal driver < TTRUE
   BFT-READ-BUF u BF-BOUNDARY-RAW-OFF$ CONTAINS? TFALSE
   BFT-READ-BUF u s\" s\" ASM-CODE\" s\" -- asm\" TRUST" CONTAINS? TFALSE
   BFT-READ-BUF u s" STDIN-OUT" CONTAINS? TTRUE ;

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
   BFT-READ-BUF u s" S2-SOURCE-CAP allot" CONTAINS? TFALSE
   BFT-READ-BUF u s" stage2: source mmap failed" CONTAINS? TTRUE ;

\ The snap tail's SNAP-RETIRE-GO is a named TRUSTED: boundary,
\ not a `0 set-check` window: the emitted snap source must carry NO raw
\ check-off line so BF-AUDIT-BOUNDARY can pin the refresh prelude's
\ BFR-CHECK-OFF as the only checking-disabled span.
: BFT-TEST-SNAP-SOURCE ( -- )
   BFT-ROOT BF-TMP!
   BF-SNAP-SOURCE
   BFT-SNAP BFT-READ {: u :}
   BFT-READ-BUF u s" : SNAP-TAIL-MARK" BFT-FIND BFT-FOUND {: mark:n :}
   BFT-READ-BUF u mark s" SEAL-FRIEND" BFT-FIND-AFTER BFT-FOUND {: seal:n :}
   BFT-READ-BUF u seal s" : ASM-CODELEN!" BFT-FIND-AFTER BFT-FOUND {: build:n :}
   BFT-READ-BUF u build s" TRUSTED: SNAP-RETIRE-GO ( -- )" BFT-FIND-AFTER BFT-FOUND {: retire:n :}
   mark seal < TTRUE
   seal build < TTRUE
   build retire < TTRUE
   BFT-READ-BUF u BF-BOUNDARY-RAW-OFF$ CONTAINS? TFALSE
   BFT-READ-BUF u s" : ATOMA-FIELD ( n -- ptr ptr u8 )" CONTAINS? TTRUE
   BFT-READ-BUF u s" TRUSTED: INCLUDE-MMAP-PTR ( n -- ptr u8 )" CONTAINS? TTRUE
   BFT-READ-BUF u s" TRUSTED: INCLUDE-EVALUATE ( ptr u8 n -- )" CONTAINS? TTRUE
   BFT-READ-BUF u s" : INCLUDE-READ-ALL ( ptr u8 n -- ptr u8 n )" CONTAINS? TTRUE
   BFT-READ-BUF u s" : included ( ptr u8 n -- )" CONTAINS? TTRUE
   BFT-READ-BUF u s" SNAP-MAGIC" CONTAINS? TTRUE
   BFT-READ-BUF u s" CHECKER-SNAPSHOT-PREPARE data-base ENGINE-SNAP-XT-CELL + !" CONTAINS? TTRUE
   BF-TMP-RESET ;

\ Doctored snapshot-trailer regression (TFAM 12 item 6, dot
\ habu-tfam-12-layout-057181a9): the loader EM-SNAPSHOT-RESTORE
\ (src/habu/habu2.f) validates the 40-byte trailer before restoring regions;
\ an oversized region-len/data-len/ndict field exits 79. The snapshot binary
\ is built through the SAME gated route
\ the pipeline uses (BF-SNAP-SOURCE + BF-CERTIFY-SNAP + hb-stdin --build
\ hb-snap-src -> hb-snap0, the BF-BUILD-SNAP-FROM-STDIN mechanism): snap.f's
\ former 0 set-check window is now the TRUSTED: SNAP-RETIRE-GO boundary, so
\ the emitted snap source certifies clean and the `-- snap` route is gated
\ end-to-end again (dot habu-tfam-12-item-346f03c2 part 1).
\ The target header's full 64-bit text-size field owns the trailer location;
\ padding and the codesign blob may follow the trailer.
\ - A patched image must be re-signed (CODESIGN:FORCE) or macOS SIGKILLs it
\   before the loader runs.
\ - EM-SNAPSHOT-RESTORE labels corruption on fd 2 before NR-EXIT-GROUP.
\   BFT-DOCTORED-CAPTURE asserts both rc 79 and the diagnostic text.
: BFT-BYTES-FIELD ( -- ptr ptr u8 )
   BFT-BYTES-A 0 ptr-field ;

: BFT-BYTES ( -- ptr u8 )
   BFT-BYTES-FIELD @ ;

: BFT-SNAP0-BUILD ( -- )
   BF-SNAP-SOURCE
   BF-CERTIFY-SNAP
   s" hb-snap0" BF-REMOVE-TMP
   s" hb-stdin" s" hb-snap-src" COMPILER-BUILD:RUN-TMP BF-RC0
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

package BFT-SNAP-HOOK
private

: U64@ ( n -- n ) {: off:n :}
   0
   8 0 ?do
      off i + BFT-BYTE@ i 8 * lshift or
   loop ;

: TRAILER-OFF ( -- n )
   IMAGE-TEXT-SIZE-OFF U64@ IMAGE-TEXT-TRAILER-ADJ + 40 - ;

: DATA-OFF ( -- n )
   TRAILER-OFF dup TRL-DATALEN + U64@ - ;

: HOOK-OFF ( -- n )
   DATA-OFF ENGINE-SNAP-XT-CELL + ;

;package

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

package BFT-SNAP-HOOK
private

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

public

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

;package

package BFT-SNAP-HOOK
private

: TEST-TRAILER ( -- )
   BFT-ROOT BF-TMP!
   BFT-SNAP0-BUILD
   BFT-EMPTY-STDIN!
   s" hb-snap0" BFT-SNAP-RUN 0 T=
   s" hb-snap0" BF-A$ s" lib/prelude.f" BF-RUN-LOAD-STAGE 0 T=
   BFT-BYTES-READ
   VERIFY-IMAGE
   TRAILER-OFF {: tr:n :}
   \ +4/+3: a MIDDLE byte of the 8-byte field keeps the value positive but
   \ far above REGION/DICT-CAP (top bytes could go negative or SIGSEGV).
   tr BFT-TRL-REGLEN + 4 + $FF BFT-DOCTORED-CAPTURE
   79 s" hb: snapshot trailer corrupt" BFT-ASSERT-SNAP-EXIT
   tr BFT-TRL-NDICT + 3 + $FF BFT-DOCTORED-CAPTURE
   79 s" hb: snapshot trailer corrupt" BFT-ASSERT-SNAP-EXIT
   BF-TMP-RESET ;

;package

\ ---- effective source boundary --------------------------------------------
\ The cold prefix already occupies IBUFSZ and the reader performs an EOF probe,
\ so IBUFSZ+1 is not the runtime input boundary. Bracket the boundary with a
\ bounded exponential search, refine it by binary search, then rerun the adjacent
\ successful/failing sizes against the freshly built candidate's --build path.
package BFT-CAP

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

public

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

private

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
   out outu s" src/habu/stage2.f" s" : S2-RUN" BF-APPEND-SOURCE-BEFORE
   out outu s" : BFT-S2-READ-EXIT ( -- ) READ-SRC DRV-EXIT-OK ;" BF-APPEND-LINE
   out outu s" BFT-S2-READ-EXIT" BF-APPEND-LINE ;

: WRITE-SPACES ( ptr u8 n n -- ) {: path:ptr pathu:n u:n :}
   path pathu SRC-BUF u WRITE-ALL ;

public

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

private

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

public

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

;package

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

\ Minimal boundary-audit-clean prelude for scratch generated-source fixtures:
\ one BFR-CHECK-OFF line followed by one LOWER-CERT-HOOK:INSTALL line, exactly
\ the documented refresh-prelude window BF-AUDIT-BOUNDARY pins.
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
\ unit behavior on an audit-clean scratch source whose only defect is the
\ type-broken definition.
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

\ Boundary audit (BF-AUDIT-BOUNDARY): the generated-source certify refuses any
\ unchecked-window drift with E-BUILD-CERTIFY - a missing or duplicated
\ BFR-CHECK-OFF line, a raw `0 set-check` line, or a hook reinstall that does
\ not follow the check-off. Each case is a scratch source differing from the
\ audit-clean prelude in exactly the violation under test.
: BFT-AUDIT-EXPECT-REJECT ( -- )
   [: s" cert-audit" BFT-CERT BF-CERTIFY-GENERATED ;] E-BUILD-CERTIFY TTHROWSQ ;

: BFT-TEST-AUDIT-MISSING-BOUNDARY ( -- )
   s" : BFT-AUD-OK1 ( n -- n ) 1 + ;" BFT-CERT-WRITE
   BFT-AUDIT-EXPECT-REJECT ;

: BFT-TEST-AUDIT-DOUBLE-BOUNDARY ( -- )
   SB-RESET
   BFT-CERT-PRELUDE+
   s" BFR-CHECK-OFF" BFT-CERT-LINE+
   BFT-CERT-SB-WRITE
   BFT-AUDIT-EXPECT-REJECT ;

: BFT-TEST-AUDIT-RAW-OFF ( -- )
   SB-RESET
   BFT-CERT-PRELUDE+
   s" 0 set-check" BFT-CERT-LINE+
   BFT-CERT-SB-WRITE
   BFT-AUDIT-EXPECT-REJECT ;

: BFT-TEST-AUDIT-ORDER ( -- )
   SB-RESET
   s\" \\ audit order\nLOWER-CERT-HOOK:INSTALL\nBFR-CHECK-OFF\n" SB-APPEND
   BFT-CERT-SB-WRITE
   BFT-AUDIT-EXPECT-REJECT ;

\ Real-list audit injection: the REAL emitted stage2 source plus one appended
\ raw `0 set-check` line must refuse at the audit, before the checker scan.
: BFT-TEST-AUDIT-REAL-INJECT ( -- )
   BFT-ROOT BF-TMP!
   BF-STAGE2-SOURCE
   s" stage2-src" BF-A$ s\" 0 set-check\n" APPEND-FILE
   [: s" stage2-src" s" stage2-src" BF-A$ BF-CERTIFY-GENERATED ;] E-BUILD-CERTIFY TTHROWSQ
   BF-TMP-RESET ;

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
\ typed-local-lint: allow-bare-local - q keeps the named subtest quotation effect.
: BFT-STEP ( ptr u8 n [ -- ] -- ) {: a:ptr u:n q :}
   a u T-LABEL
   q catch {: rc:n :}
   rc 0= if exit then
   a u type s" : throw " type rc . cr
   s" build-fixpoint-test: subtest threw" T-EX-FAIL die ;

package BFT-SNAP-HOOK
public

: RUN ( -- )
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
   s" stamp source key" [: BFT-TEST-STAMP-SOURCE-KEY ;] BFT-STEP
   s" stamp corrupt" [: BFT-TEST-STAMP-CORRUPT ;] BFT-STEP
   s" stamp engine" [: BFT-TEST-STAMP-ENGINE ;] BFT-STEP
   s" all stamp guard" [: BFT-TEST-ALL-STAMP-GUARD ;] BFT-STEP
   s" stamp nested" [: BFT-TEST-STAMP-NESTED ;] BFT-STEP
   s" no build shims" [: BFT-TEST-NO-BUILD-SHIMS ;] BFT-STEP
   s" certify good" [: BFT-TEST-CERTIFY-GOOD ;] BFT-STEP
   s" certify bad" [: BFT-TEST-CERTIFY-BAD ;] BFT-STEP
   s" retire regression" [: BFT-TEST-RETIRE-REGRESSION ;] BFT-STEP
   s" certify blocking" [: BFT-TEST-CERTIFY-BLOCKING ;] BFT-STEP
   s" audit missing boundary" [: BFT-TEST-AUDIT-MISSING-BOUNDARY ;] BFT-STEP
   s" audit double boundary" [: BFT-TEST-AUDIT-DOUBLE-BOUNDARY ;] BFT-STEP
   s" audit raw off" [: BFT-TEST-AUDIT-RAW-OFF ;] BFT-STEP
   s" audit order" [: BFT-TEST-AUDIT-ORDER ;] BFT-STEP
   s" audit real inject" [: BFT-TEST-AUDIT-REAL-INJECT ;] BFT-STEP
   s" boot pin mismatch" [: BFT-TEST-BOOT-PIN ;] BFT-STEP
   s" certify good passes" [: BFT-TEST-CERTIFY-GOOD-PASSES ;] BFT-STEP
   s" certify checker self" [: BFT-TEST-CERTIFY-CHECKER-SELF ;] BFT-STEP
   s" certify tfam prefix" [: BFT-TEST-CERTIFY-TFAM-PREFIX ;] BFT-STEP
   s" certify phase sources" [: BFT-TEST-CERTIFY-PHASE-SOURCES ;] BFT-STEP
   s" stage2 source" [: BFT-TEST-STAGE2-SOURCE ;] BFT-STEP
   s" no stage2 run source" [: BFT-TEST-NO-STAGE2-RUN-SOURCE ;] BFT-STEP
   s" checked target image" [: BFT-TEST-CHECKED-TARGET-IMAGE ;] BFT-STEP
   s" checked regalloc" [: BFT-TEST-CHECKED-REGALLOC ;] BFT-STEP
   s" snap source" [: BFT-TEST-SNAP-SOURCE ;] BFT-STEP
   s" snap missing hook" [: BFT-SNAP-HOOK:MISSING ;] BFT-STEP
   s" snap trailer" [: TEST-TRAILER ;] BFT-STEP
   s" source boundary" [: BFT-CAP:SOURCE-BOUNDARY ;] BFT-STEP
   s" stage2 source cap" [: BFT-CAP:STAGE2 ;] BFT-STEP
   s" maker source cap" [: BFT-CAP:MAKER ;] BFT-STEP
   CLEANUP-RUN
   BFT-ROOT EXISTS? TFALSE
   T-REPORT
   s" build-fixpoint-test: ok" type cr ;

;package

BFT-SNAP-HOOK:RUN
