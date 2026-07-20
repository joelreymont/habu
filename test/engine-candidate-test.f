\ engine-candidate-test.f - focused coverage for the shared validated-engine
\ resolver (lib/engine-candidate.f).
\
\ Proves:
\   - a child-env default-table override resolves to that executable (branch 1);
\   - a default override naming this engine resolves back to this engine, the
\     engine-identity composition (ENGINE-ID:PATH$);
\   - a non-executable override fails closed with E-FS-OPEN before it is handed out;
\   - with the override forced empty, a real child engine's resolver falls through
\     to its OWN identity (branch 3), asserted in-child and reported via exit code;
\   - the resolved path drives PROCESS-PTY:SPAWN, so ENGINE-CANDIDATE is the
\     engine-path provider for engine-under-test supervision: spawn the resolved
\     engine on a trivial driver, release it, watch it exit, tear down.
\
\ Run: bin/hb --load test/engine-candidate-test.f

require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/engine-id.f
require lib/engine-candidate.f
require lib/process-pty-io.f

package ENGINE-CANDIDATE-TEST

$1000 constant PATH-CAP
$4000 constant IO-CAP
$7530 constant DEADLINE-MS         \ 30s ceiling; AWAIT/capture return as soon as the child exits

create ROOT  PATH-CAP allot   variable ROOT-U
create PLAIN PATH-CAP allot   variable PLAIN-U
create DRV   PATH-CAP allot   variable DRV-U
create CHK   PATH-CAP allot   variable CHK-U
create OUT   IO-CAP  allot
create ERR   IO-CAP  allot

: NAME$ ( -- ptr u8 n )       s" HABU_UNDER_TEST" ;
: TRUE-PATH$ ( -- ptr u8 n )  s" /usr/bin/true" ;   \ a real executable on both OSes

\ The fallback driver: a child engine with HABU_UNDER_TEST="" must resolve the
\ candidate to its own identity; a mismatch throws so the child exits nonzero.
: FALLBACK-SRC$ ( -- ptr u8 n )
   s" require lib/engine-candidate.f : EC-CHK ( -- ) ENGINE-CANDIDATE:PATH$ ENGINE-ID:PATH$ STR= 0= if E-PROC-OUTPUT throw then ; EC-CHK" ;

\ ---- fixtures ---------------------------------------------------------------
: SETUP ( -- )
   CLEANUP-RESET
   s" hb-engine-candidate" TMPDIR-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY  u ROOT-U !
   ROOT ROOT-U @ CLEANUP-TREE+
   ROOT ROOT-U @ s" plain"    PLAIN JOIN-PATH PLAIN-U !
   ROOT ROOT-U @ s" driver.f" DRV   JOIN-PATH DRV-U !
   ROOT ROOT-U @ s" chk.f"    CHK   JOIN-PATH CHK-U !
   PLAIN PLAIN-U @ s" not executable" WRITE-ALL       \ a plain, non-executable file
   DRV   DRV-U @   s" 0 drop" WRITE-ALL                \ trivial: loads and exits
   CHK   CHK-U @   FALLBACK-SRC$ WRITE-ALL ;

\ ---- resolution + validation (in-process, deterministic) --------------------
\ The child-env default table outranks the live environment, so these pin each
\ branch regardless of the gate's own HABU_UNDER_TEST.
: OVERRIDE-RESOLVES ( -- )                 \ branch 1: default override -> that executable
   PROC-ENV-DEFAULT-RESET
   NAME$ >LEN TRUE-PATH$ >LEN PROC-ENV-DEFAULT+
   ENGINE-CANDIDATE:PATH$ TRUE-PATH$ T$= ;

: SELF-RESOLVES ( -- )                     \ default override at this engine -> this engine
   PROC-ENV-DEFAULT-RESET
   NAME$ >LEN ENGINE-ID:PATH$ >LEN PROC-ENV-DEFAULT+
   ENGINE-CANDIDATE:PATH$ ENGINE-ID:PATH$ T$= ;

: NONEXEC-REJECTED ( -- )                  \ a non-executable override fails closed
   PROC-ENV-DEFAULT-RESET
   NAME$ >LEN PLAIN PLAIN-U @ >LEN PROC-ENV-DEFAULT+
   [: ENGINE-CANDIDATE:PATH$ 2drop ;] E-FS-OPEN TTHROWSQ ;

\ ---- fallback to self, deterministic via a child with the override forced empty --
\ In-process the gate's live HABU_UNDER_TEST cannot be unset, so a real child
\ engine runs with HABU_UNDER_TEST="" and asserts the resolver returned its own
\ path; the child exits nonzero on mismatch.
: FALLBACK-TO-SELF ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   CHK CHK-U @ >LEN PROC-ARGV+
   PROC-ENV-RESET
   NAME$ >LEN s" " >LEN PROC-ENV+          \ empty override -> resolver must fall through to self
   PROC-ENV-INHERIT-MISSING                \ keep PATH/HB_TMP/TMPDIR so the child engine can run
   ENGINE-ID:PATH$ >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN DEADLINE-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N {: outu:len erru:len rc:n :}
   rc 0 <> if ERR erru LEN>N type cr then  \ surface child stderr on failure
   rc 0 T= ;

\ ---- composition: the resolver feeds the PTY supervisor ---------------------
\ ENGINE-CANDIDATE:PATH$ is the engine-under-test path PROCESS-PTY:SPAWN takes.
\ Pin the resolver at this engine, spawn it on the trivial driver, release it,
\ watch it exit, tear down.
: SUPERVISE-RESOLVED ( -- )
   PROC-ENV-DEFAULT-RESET
   NAME$ >LEN ENGINE-ID:PATH$ >LEN PROC-ENV-DEFAULT+
   ENGINE-CANDIDATE:PATH$ ENGINE-ID:PATH$ T$=          \ deterministic: resolves to self
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   DRV DRV-U @ >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN PROCESS-PTY:SPAWN
   PROCESS-PTY:LAUNCH
   DEADLINE-MS PROCESS-PTY:AWAIT TTRUE
   PROCESS-PTY:TEARDOWN ;

: RUN ( -- )
   T-RESET
   SETUP
   OVERRIDE-RESOLVES
   SELF-RESOLVES
   NONEXEC-REJECTED
   FALLBACK-TO-SELF
   SUPERVISE-RESOLVED
   CLEANUP-RUN
   T-REPORT
   s" engine-candidate-test: ok" type cr ;

RUN

;package
