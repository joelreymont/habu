\ maki/cad-replay-test.f - regression: the fresh-process replay child must run the
\ SAME engine as its parent, so a candidate engine whose bytes differ from bin/hb
\ still replays its own promoted selection instead of being blamed for a harness
\ artifact.
\
\ The section-7.4 cad-store key hashes the running engine binary (lib/engine-id.f).
\ Before the fix, maki/cad-test.f's fresh-process replay child launched a hardcoded
\ s" bin/hb", so when the gate ran the maki suite under a candidate engine whose
\ content differed from bin/hb the child computed a DIFFERENT store key, missed the
\ selection the parent had promoted, and the two replay assertions - "replay hit ->
\ stored selection 7" (F100) and "no using-defaults" (F101) - went red. The clean
\ master stayed green only because its candidate was byte-identical to bin/hb.
\
\ This test reproduces the cross-engine case honestly: it clones THIS running engine
\ to a private path and appends a marker byte so the clone's content hash differs (a
\ stand-in for a genuinely different candidate build), then runs the whole cad-test
\ suite under that clone with HABU_UNDER_TEST pointing at it. With the replay child
\ resolved from HABU_UNDER_TEST the child runs the clone too, its key matches the
\ parent's, the replay is a hit, and the suite exits 0. Against the unfixed
\ hardcoded-bin/hb child the same run exits non-zero on F100/F101.

require lib/test.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f              \ COPY-FILE-STREAM / APPEND-FILE / CHMOD-X / TMPDIR-MKDIR / REMOVE-TREE
require lib/process.f
require lib/process-argv.f
require lib/process-env.f            \ RUN-ARGV-ENV-CAPTURE + PROC-ENV+ + PROC-ENV-INHERIT-MISSING
require lib/engine-id.f              \ ENGINE-PATH$: the engine this test clones

package MAKI-CAD-REPLAY-TEST

120000 constant SUITE-TIMEOUT-MS     \ generous: the child runs the entire cad-test suite

create SUITE-OUT $8000 allot   create SUITE-ERR $2000 allot
create CLONE-DIR  FS-PATH-CAP allot   variable CLONE-DIR-U
create CLONE-PATH FS-PATH-CAP allot   variable CLONE-PATH-U

: CLONE-DIR$ ( -- ptr u8 n )   CLONE-DIR  CLONE-DIR-U  @ ;
: CLONE-PATH$ ( -- ptr u8 n )  CLONE-PATH CLONE-PATH-U @ ;

\ Trailing bytes past the Mach-O/code-signature blob are ignored by the loader, so
\ this marker changes only the content hash - the clone still runs as an engine.
: MARKER$ ( -- ptr u8 n )  s" HABU-CAD-REPLAY-REGRESSION-ENGINE-CLONE-MARKER" ;

: CLONE-MKDIR ( -- )                 \ fresh private temp dir -> CLONE-DIR
   s" habu-cad-replay-engine" TMPDIR-MKDIR {: a:ptr u:n :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a CLONE-DIR u BYTE-COPY  u CLONE-DIR-U ! ;

: CLONE-BUILD ( -- )                 \ clone THIS engine, perturb its hash, keep it runnable
   CLONE-MKDIR
   CLONE-DIR$ s" hb-clone" CLONE-PATH JOIN-PATH CLONE-PATH-U !
   ENGINE-PATH$ CLONE-PATH$ COPY-FILE-STREAM
   CLONE-PATH$ MARKER$      APPEND-FILE
   CLONE-PATH$ CHMOD-X ;

: CLONE-CLEAN ( -- )
   CLONE-DIR-U @ 0 > if CLONE-DIR$ REMOVE-TREE then
   0 CLONE-DIR-U ! ;

: SUITE-DIAG ( len len -- )  {: outu:len erru:len :}   \ surface the child streams on failure
   SUITE-OUT outu LEN>N type cr
   SUITE-ERR erru LEN>N type cr ;

: SUITE-RC ( -- rc )                 \ run the whole cad-test suite under the clone; child completion rc
   PROC-ARGV-ENV-RESET
   s" --load"          >LEN PROC-ARGV+
   s" maki/cad-test.f" >LEN PROC-ARGV+
   s" HABU_UNDER_TEST" >LEN CLONE-PATH$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING          \ keep PATH/HOME/HB_TMP/TMPDIR; HABU_UNDER_TEST override wins
   CLONE-PATH$ >LEN  SUITE-OUT $8000 >LEN  SUITE-ERR $2000 >LEN  SUITE-TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE 0 >RC ENDOF          \ clean suite exit -> rc 0
     err OF PCAP-FAILED:UNMAKE ENDOF                  \ non-zero suite: (out err code) on stack
   ;MATCH
   {: outu:len erru:len rc:rc :}
   rc RC>N 0 <> if outu erru SUITE-DIAG then
   rc ;

T-RESET

\ The distinct-engine clone promotes and replays through ITSELF, so the whole
\ cad-test suite - F100 "replay hit" and F101 "no using-defaults" included - is green.
CLONE-BUILD
SUITE-RC RC>N 0 T=
CLONE-CLEAN

T-REPORT

;package
