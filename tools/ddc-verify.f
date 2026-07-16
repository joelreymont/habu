\ ddc-verify.f - Diverse Double-Compiling audit for the bin/hb fixpoint.
\
\ Builds bin/hb two independent ways and requires byte-identical output:
\   NATIVE chain - the self-hosted fixpoint (bin/hb rebuilds bin/hb).
\   GFORTH chain - the independent recovery host: tools/bootstrap.sh emits a raw
\                  seed engine (hb-stdin) via Gforth, which is then taken to the
\                  SAME native fixpoint the full recovery runs.
\ A seed backdoor would have to be mirrored in BOTH Gforth and the native seed to
\ survive the sha256 compare, reducing seed trust to "no coordinated cross-host
\ backdoor". This is an EXPLICIT audit (never per-commit): it needs a working
\ Gforth recovery host and is gated on HABU_ALLOW_BOOTSTRAP=1, like the launcher
\ it drives (docs/bootstrap.md).
\
\ WHY WE COMPARE AT THE FIXPOINT, NOT THE RAW SEED: the CHECK_ONLY seed hb-stdin
\ is captured by a Gforth-lineage stage whose live REPL sits at different
\ absolute addresses than the native host. Its baked AOT-REPL blob therefore
\ carries the capture host's movz/movk address immediates. EM-SEED-AOT
\ re-relocates them at boot, so those bytes are DEAD yet host-dependent, and a
\ raw seed-vs-fixpoint compare diverges by design. The real recovery chain
\ (tools/bootstrap.sh) installs the seed and runs the native fixpoint refresh;
\ that refresh re-captures the AOT blob from the canonical small engine, erasing
\ the dead host addresses. DDC reproduces exactly that step - gforth seed ->
\ native fixpoint refresh - and requires the resulting engine to be
\ byte-identical to the pure-native fixpoint bin/hb.
\
\ The pure comparison words (DDC-SAME?/DDC-FIRST-DIFF/DDC-REPORT) plus the path
\ helpers are covered by tools/ddc-verify-test.f; the full DDC-VERIFY
\ orchestration is manual/audit because it runs the whole gforth+refresh chain.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f

79 constant DDC-E-USAGE
75 constant DDC-E-CHAIN
1 constant DDC-DIVERGENT-RC
$400000 constant DDC-BIN-CAP           \ per-artifact read buffer (bin/hb ~165 KB)
64 constant DDC-HEX-CAP                 \ SHA256>HEX writes 32 bytes x 2 = 64 chars
$8000 constant DDC-CAP-OUT
600000 constant DDC-CHAIN-TIMEOUT-MS

create DDC-A-BUF DDC-BIN-CAP allot
create DDC-B-BUF DDC-BIN-CAP allot
create DDC-A-HEX DDC-HEX-CAP allot
create DDC-B-HEX DDC-HEX-CAP allot
create DDC-DIG 32 allot
create DDC-OUT DDC-CAP-OUT allot
create DDC-ERR DDC-CAP-OUT allot
variable DDC-A-U
variable DDC-B-U

: DDC-READ ( ptr u8 n ptr u8 n -- n ) {: pa:ptr pu:n buf:ptr cap:n :}
   pa pu buf cap READ-ALL ;

\ Load both artifact files into the compare buffers; throw on read failure so a
\ missing/short chain artifact is never silently treated as a match.
: DDC-LOAD ( ptr u8 n ptr u8 n -- ) {: pa:ptr pu:n pb:ptr pb-u:n :}
   pa pu DDC-A-BUF DDC-BIN-CAP DDC-READ DDC-A-U !
   pb pb-u DDC-B-BUF DDC-BIN-CAP DDC-READ DDC-B-U ! ;

: DDC-MINLEN ( -- n ) DDC-A-U @ DDC-B-U @ MIN ;

\ First differing byte offset over the loaded buffers, or -1 when byte-identical.
\ When the common prefix matches but lengths differ, the shorter length is the
\ first mismatch offset.
: DDC-FIRST-DIFF ( -- n )
   0 begin dup DDC-MINLEN < while
      dup DDC-A-BUF + c@  over DDC-B-BUF + c@  <> if exit then
      1+
   repeat
   drop
   DDC-A-U @ DDC-B-U @ = if -1 else DDC-MINLEN then ;

: DDC-SAME? ( -- bool )
   DDC-FIRST-DIFF -1 = ;

\ SHA256 hex of a loaded buffer into a 64-char out buffer.
: DDC-BUF-HEX ( ptr u8 n ptr u8 -- ) {: a:ptr u:n out:ptr :}
   a u DDC-DIG SHA256
   DDC-DIG out SHA256>HEX ;

: DDC-U. ( n -- ) {: n:n :}
   n 0 < if E-STR-BOUNDS throw then
   n 10 >= if n 10 / RECURSE then
   n 10 mod STR-ZERO + emit ;

\ Report the verdict on stdout; return DDC-DIVERGENT-RC on mismatch, 0 on match.
: DDC-REPORT ( ptr u8 n ptr u8 n -- n ) {: la:ptr lu:n lb:ptr lbu:n :}
   DDC-A-BUF DDC-A-U @ DDC-A-HEX DDC-BUF-HEX
   DDC-B-BUF DDC-B-U @ DDC-B-HEX DDC-BUF-HEX
   DDC-SAME? if
      s" ddc: byte-identical " type DDC-A-HEX DDC-HEX-CAP type cr
      0 exit
   then
   s" ddc: DIVERGENT" type cr
   s" ddc:   " type la lu type s"  " type DDC-A-HEX DDC-HEX-CAP type s"  len " type DDC-A-U @ DDC-U. cr
   s" ddc:   " type lb lbu type s"  " type DDC-B-HEX DDC-HEX-CAP type s"  len " type DDC-B-U @ DDC-U. cr
   s" ddc:   first differing byte at offset " type DDC-FIRST-DIFF DDC-U. cr
   DDC-DIVERGENT-RC ;

\ ---- audit orchestration (manual; runs the gforth chain + native refresh) ----

create DDC-GF-ROOT FS-PATH-CAP allot         \ gforth chain HB_TMP root
variable DDC-GF-ROOT-U
create DDC-RF-ROOT FS-PATH-CAP allot          \ native-refresh HB_TMP + cache root
variable DDC-RF-ROOT-U
create DDC-STDIN FS-PATH-CAP allot            \ raw gforth seed: <gf-root>/hb-stdin
variable DDC-STDIN-U
create DDC-FXP FS-PATH-CAP allot              \ gforth-chain fixpoint: <rf-root>/hb-ddc-fixpoint
variable DDC-FXP-U

: DDC-GF-ROOT! ( ptr u8 n -- ) {: a:ptr u:n :} a DDC-GF-ROOT u BYTE-COPY  u DDC-GF-ROOT-U ! ;
: DDC-RF-ROOT! ( ptr u8 n -- ) {: a:ptr u:n :} a DDC-RF-ROOT u BYTE-COPY  u DDC-RF-ROOT-U ! ;
: DDC-GF-ROOT$ ( -- ptr u8 n ) DDC-GF-ROOT DDC-GF-ROOT-U @ ;
: DDC-RF-ROOT$ ( -- ptr u8 n ) DDC-RF-ROOT DDC-RF-ROOT-U @ ;

\ raw gforth seed path under the gforth HB_TMP root
: DDC-STDIN$ ( -- ptr u8 n )
   DDC-GF-ROOT$ s" hb-stdin" DDC-STDIN JOIN-PATH DDC-STDIN-U !
   DDC-STDIN DDC-STDIN-U @ ;

\ gforth-chain fixpoint path under the native-refresh root
: DDC-FXP$ ( -- ptr u8 n )
   DDC-RF-ROOT$ s" hb-ddc-fixpoint" DDC-FXP JOIN-PATH DDC-FXP-U !
   DDC-FXP DDC-FXP-U @ ;

: DDC-REQUIRE-BOOTSTRAP ( -- )
   s" HABU_ALLOW_BOOTSTRAP" GETENV s" 1" STR= 0= if
      s" ddc: set HABU_ALLOW_BOOTSTRAP=1 (drives the gforth recovery host)" type cr
      DDC-E-USAGE throw
   then ;

\ Report the failing chain plus its captured stderr, then throw; the failing
\ chain's diagnostics are never swallowed.
: DDC-CHAIN-FAIL ( ptr u8 n len rc -- ) {: label:ptr labelu:n errl:len rc:rc :}
   s" ddc: " type label labelu type s"  chain failed rc " type rc RC>N DDC-U. cr
   DDC-ERR errl LEN>N type cr
   DDC-E-CHAIN throw ;

\ Run tools/bootstrap.sh CHECK_ONLY into the gforth HB_TMP root; it emits hb-stdin
\ there and never touches the checkout's bin/hb, so the native reference stays intact.
: DDC-RUN-GFORTH ( -- )
   PROC-ARGV-RESET
   PROC-ENV-RESET
   s" HABU_ALLOW_BOOTSTRAP" >LEN s" 1" >LEN PROC-ENV+
   s" HABU_BOOTSTRAP_CHECK_ONLY" >LEN s" 1" >LEN PROC-ENV+
   s" HB_TMP" >LEN DDC-GF-ROOT$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" tools/bootstrap.sh" >LEN
   DDC-OUT DDC-CAP-OUT >LEN  DDC-ERR DDC-CAP-OUT >LEN
   DDC-CHAIN-TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE {: outl:len errl:len rc:rc :}
   rc RC>N 0 <> if s" gforth CHECK_ONLY" errl rc DDC-CHAIN-FAIL then ;

: DDC-ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

\ The exact fixpoint-refresh load list from tools/bootstrap.sh - the recovery
\ chain's final "mv hb-stdin bin/hb; install --force" step, re-targeted below to a
\ scratch engine path via HABU_FIXPOINT_ENGINE.
: DDC-REFRESH-ARGV+ ( -- )
   s" --load" DDC-ARG
   s" lib/errors.f" DDC-ARG        s" lib/string.f" DDC-ARG
   s" lib/memory.f" DDC-ARG        s" lib/fs.f" DDC-ARG
   s" lib/fs-mutate.f" DDC-ARG
   s" lib/process.f" DDC-ARG       s" lib/process-argv.f" DDC-ARG
   s" lib/process-env.f" DDC-ARG   s" lib/codesign.f" DDC-ARG
   s" tools/build-fixpoint.f" DDC-ARG
   s" tools/build-fixpoint-main.f" DDC-ARG
   s" --" DDC-ARG  s" install" DDC-ARG  s" --force" DDC-ARG ;

\ Refresh the raw gforth seed to the native fixpoint at <rf-root>/hb-ddc-fixpoint.
\ HABU_FIXPOINT_ENGINE re-targets the install so the checkout's bin/hb is never
\ replaced; XDG_CACHE_HOME isolates the fixpoint stamp under the scratch root.
: DDC-RUN-REFRESH ( -- )
   PROC-ARGV-RESET
   PROC-ENV-RESET
   DDC-REFRESH-ARGV+
   s" HB_TMP" >LEN DDC-RF-ROOT$ >LEN PROC-ENV+
   s" XDG_CACHE_HOME" >LEN DDC-RF-ROOT$ >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN DDC-FXP$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   DDC-STDIN$ >LEN
   DDC-OUT DDC-CAP-OUT >LEN  DDC-ERR DDC-CAP-OUT >LEN
   DDC-CHAIN-TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE {: outl:len errl:len rc:rc :}
   rc RC>N 0 <> if s" native fixpoint refresh" errl rc DDC-CHAIN-FAIL then ;

: DDC-VERIFY ( -- n )
   DDC-REQUIRE-BOOTSTRAP
   s" habu-ddc-gf" TMPDIR-MKDIR DDC-GF-ROOT!
   s" habu-ddc-rf" TMPDIR-MKDIR DDC-RF-ROOT!
   DDC-RUN-GFORTH
   DDC-RUN-REFRESH
   DDC-FXP$ s" bin/hb" DDC-LOAD
   s" hb (gforth chain: seed + native refresh)"
   s" bin/hb (native fixpoint)" DDC-REPORT ;

: DDC-MAIN ( -- )
   DDC-VERIFY {: rc:n :}
   rc 0 <> if s" " rc die then ;
