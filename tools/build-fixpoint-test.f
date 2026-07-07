\ build-fixpoint-test.f - checked fixture for tools/build-fixpoint.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f
\ lib/fs-mutate.f lib/process.f
\ lib/process-argv.f lib/process-env.f lib/process-cwd.f lib/build.f lib/codesign.f
\ tools/build-fixpoint.f tools/build-fixpoint-test.f

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

8192 constant BFT-CAPTURE-CAP
$40000 constant BFT-BIG-CAP
$100000 constant BFT-READ-CAP
120000 constant BFT-TIMEOUT-MS
13 constant BFT-BUILD-ARGV#

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
create BFT-CHECK-OFF-LINE
10 c, 48 c, 32 c, 115 c, 101 c, 116 c, 45 c,
99 c, 104 c, 101 c, 99 c, 107 c, 10 c,

: BFT-READ-FIELD ( -- ptr ptr u8 )
   BFT-READ-A 0 ptr-field ;

: BFT-READ-BUF! ( ptr u8 -- )
   BFT-READ-FIELD ! ;

: BFT-READ-BUF ( -- ptr u8 )
   BFT-READ-FIELD @ ;

: BFT-ALLOC-READ ( -- )
   BFT-READ-CAP MEM-ALLOC-BYTES drop BFT-READ-BUF! ;

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
   BFT-BIG-CAP MEM-ALLOC-BYTES drop BFT-BIG-OUT-FIELD !
   BFT-BIG-CAP MEM-ALLOC-BYTES drop BFT-BIG-ERR-FIELD ! ;

: BFT-EMPTY$ ( -- ptr u8 n )
   SB-RESET
   SB$ ;

: BFT-CHECK-OFF-LINE$ ( -- ptr u8 n )
   BFT-CHECK-OFF-LINE 13 ;

: BFT-ENV$ ( -- ptr u8 n )
   BFT-ROOT ;

: BFT-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: BFT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-build-fixpoint" TMPDIR-MKDIR {: a:ptr u :}
   a u BFT-ROOT-BUF BFT-ROOT-U BFT-COPY!
   BFT-ALLOC-READ
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

: BFT-ARGV-LOAD-FILES ( -- )
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
   s" tools/build-fixpoint.f" BFT-ARG+
   s" tools/build-fixpoint-main.f" BFT-ARG+ ;

: BFT-ARGV-FIXPOINT ( ptr u8 n ptr u8 n -- n ) {: tmp:ptr tmpu:n stamp:ptr stampu:n :}
   PROC-ARGV-RESET
   PROC-ENV-RESET
   s" HB_TMP" >LEN tmp tmpu >LEN PROC-ENV+
   s" HABU_FIXPOINT_STAMP" >LEN stamp stampu >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN BFT-HB >LEN PROC-ENV+
   BFT-ARGV-LOAD-FILES
   PROC-ARGV-N @ COUNT>N ;

: BFT-ARGV-BUILD ( -- n )
   BFT-ENV$ BFT-STAMP BFT-ARGV-FIXPOINT ;

: BFT-ARGV-FAIL ( -- n )
   BFT-NOTDIR BFT-STAMP2 BFT-ARGV-FIXPOINT ;

: BFT-CAPTURE>N ( len len rc -- n n n ) {: outu erru rc :}
   outu LEN>N erru LEN>N rc RC>N ;

: BFT-ARGV-ALL-FORCE ( -- )
   s" --" BFT-ARG+
   s" all" BFT-ARG+
   s" --force" BFT-ARG+ ;

: BFT-ARGV-ALL ( -- )
   s" --" BFT-ARG+
   s" all" BFT-ARG+ ;

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

: BFT-READ ( ptr u8 n -- n )
   BFT-READ-BUF BFT-READ-CAP READ-ALL ;

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
   RUN-ARGV-ENV-CWD-CAPTURE
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

: BFT-FIND-AFTER ( ptr u8 n n ptr u8 n -- n ) {: a:ptr u start needle:ptr nu :}
   start 0 < if -1 exit then
   start u >= if -1 exit then
   a start BYTE+ u start - needle nu FIND-SUB
   dup 0 < if exit then
   start + ;

: BFT-FOUND ( n -- )
   0 >= TTRUE ;

: BFT-NOT-FOUND ( n -- )
   0 < TTRUE ;

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
   BFT-READ-BUF u s" ' HOOK set-check" CONTAINS? TTRUE
   BFT-READ-BUF u BFT-CHECK-OFF-LINE$ CONTAINS? TFALSE
   BFT-READ-BUF u s\" s\" ASM-CODE\" s\" -- asm\" TRUST" CONTAINS? TFALSE
   BFT-READ-BUF u s" STDIN-OUT" CONTAINS? TTRUE ;

: BFT-TEST-NO-STAGE2-RUN-SOURCE ( -- )
   BFT-RUN FILE? TFALSE ;

: BFT-TEST-CHECKED-REGALLOC ( -- )
   BFT-STAGE2 BFT-READ {: u :}
   BFT-READ-BUF u s" : BPROF-ON" FIND-SUB dup BFT-FOUND BFT-PROF-I !
   BFT-READ-BUF u BFT-PROF-I @ s" : EMIT-VRINIT" BFT-FIND-AFTER dup BFT-FOUND BFT-REG-I !
   BFT-READ-BUF u BFT-REG-I @ s" : FOLD-ENTRY" BFT-FIND-AFTER dup BFT-FOUND BFT-JIT-I !
   BFT-PROF-I @ BFT-REG-I @ < TTRUE
   BFT-REG-I @ BFT-JIT-I @ < TTRUE ;

\ The target-image writers (elf/macho/sign) compile CHECKED in stage2: no
\ 0 set-check window opens at or after the image region and no synthetic
\ TRUST rows are injected, so a stack-effect regression in ASM-CODE/
\ BUILD-IMAGE/BUILD-SNAP-HDR/SET-SIGID/CODESIG2 fails the stage compile.
: BFT-TEST-CHECKED-TARGET-IMAGE ( -- )
   BFT-STAGE2 BFT-READ {: u :}
   BFT-READ-BUF u s" : ASM-CODELEN!" FIND-SUB dup BFT-FOUND BFT-IMG-I !
   BFT-READ-BUF u BFT-IMG-I @ s" : BUILD-IMAGE" BFT-FIND-AFTER dup BFT-FOUND BFT-IMG-BUILD-I !
   BFT-READ-BUF u BFT-IMG-BUILD-I @ s" : RPD@" BFT-FIND-AFTER dup BFT-FOUND BFT-HABU1-I !
   BFT-READ-BUF u BFT-IMG-I @ BFT-CHECK-OFF-LINE$ BFT-FIND-AFTER BFT-NOT-FOUND
   BFT-IMG-I @ BFT-IMG-BUILD-I @ < TTRUE
   BFT-IMG-BUILD-I @ BFT-HABU1-I @ < TTRUE ;

: BFT-TEST-STAGE2-SCRATCH ( -- )
   BFT-STAGE2 BFT-READ {: u :}
   BFT-READ-BUF u s" S2-SOURCE-CAP allot" CONTAINS? TFALSE
   BFT-READ-BUF u s" stage2: source mmap failed" CONTAINS? TTRUE ;

: BFT-TEST-SNAP-SOURCE ( -- )
   BFT-ROOT BF-TMP!
   BF-SNAP-SOURCE
   BFT-SNAP BFT-READ {: u :}
   BFT-READ-BUF u s" : ATOMA-FIELD ( n -- ptr ptr u8 )" CONTAINS? TTRUE
   BFT-READ-BUF u s" TRUSTED: INCLUDE-MMAP-PTR ( n -- ptr u8 )" CONTAINS? TTRUE
   BFT-READ-BUF u s" TRUSTED: INCLUDE-EVALUATE ( ptr u8 n -- )" CONTAINS? TTRUE
   BFT-READ-BUF u s" : INCLUDE-READ-ALL ( ptr u8 n -- ptr u8 n )" CONTAINS? TTRUE
   BFT-READ-BUF u s" : included ( ptr u8 n -- )" CONTAINS? TTRUE
   BFT-READ-BUF u s" SNAP-MAGIC" CONTAINS? TTRUE
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

\ Certification is BLOCKING: a generated stage source that rejects must fail
\ the build with E-BUILD-STATUS, not warn and proceed (fail-open). The
\ install-path proof is the red/green install pair recorded on
\ habu-make-fixpoint-certify-a11dbad5; this pins the unit behavior.
: BFT-TEST-CERTIFY-BLOCKING ( -- )
   s" : BFT-CERT-BAD2 ( n -- n ) drop ;" BFT-CERT-WRITE
   [: s" cert-blocking" BFT-CERT BF-CERTIFY-GENERATED ;] E-BUILD-STATUS TTHROWSQ ;

: BFT-TEST-CERTIFY-GOOD-PASSES ( -- )
   s" : BFT-CERT-GOOD2 ( n -- n ) 1 + ;" BFT-CERT-WRITE
   s" cert-good2" BFT-CERT BF-CERTIFY-GENERATED ;

\ Self-certification guard: the checker's own source and the pre-compile source
\ verifier must certify clean via the same VERIFY:SOURCE-BUF path the fixpoint
\ install uses, so any future de-typing of checker.f/verify-source.f turns this
\ suite red immediately. The whole stage2/stdin sources now certify rc 0 and
\ certification is BLOCKING (BFT-TEST-CERTIFY-BLOCKING); the fixpoint install
\ is the end-to-end assertion.
: BFT-TEST-CERTIFY-CHECKER-SELF ( -- )
   s" checker-self" s" src/core/checker.f" BF-CERTIFY-RC 0 T=
   s" verify-source-self" s" src/habu/verify-source.f" BF-CERTIFY-RC 0 T= ;

\ Per-file TFAM-prefix certification: type-schema.f, type-family.f, render.f,
\ and sumtype.f certify clean via the same VERIFY:SOURCE-BUF path, each
\ verified as the tail of its exact BF-APPEND-CHECKER-BOOT prefix context
\ (util, structures, checker, then the earlier TFAM files), so de-typing any
\ one file fails its own assert. render.f sits between type-family.f and
\ sumtype.f in the real prefix and certifies since its typed cleanup
\ (habu-make-fixpoint-certify-a11dbad5).
: BFT-CERT-TFAM$ ( -- ptr u8 n )
   s" cert-tfam" BF-A$ ;

: BFT-CERT-TFAM-BASE ( -- )
   s" cert-tfam" BF-RESET-OUT
   s" cert-tfam" s" src/core/util.f" BF-APPEND-SOURCE
   s" cert-tfam" s" src/core/structures.f" BF-APPEND-SOURCE
   s" cert-tfam" s" src/core/checker.f" BF-APPEND-SOURCE ;

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
\ stage-input path (hb-stage reads that name), so BF-CERTIFY-STAGE2 and
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

: BFT-MAIN ( -- )
   T-RESET
   BFT-PREPARE
   s" tmp override" [: BFT-TEST-TMP-OVERRIDE ;] BFT-STEP
   s" stage argv reset" [: BFT-TEST-STAGE-ARGV-RESET ;] BFT-STEP
   s" stamp seed" [: BFT-TEST-STAMP-SEED ;] BFT-STEP
   s" build" [: BFT-TEST-BUILD ;] BFT-STEP
   s" cached skip" [: BFT-TEST-CACHED-SKIP ;] BFT-STEP
   s" build fail no stamp" [: BFT-TEST-BUILD-FAIL-NO-STAMP ;] BFT-STEP
   s" stale seed install" [: BFT-TEST-STALE-INSTALL ;] BFT-STEP
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
   CLEANUP-RUN
   BFT-ROOT EXISTS? TFALSE
   T-REPORT
   s" build-fixpoint-test: ok" type cr ;

BFT-MAIN
