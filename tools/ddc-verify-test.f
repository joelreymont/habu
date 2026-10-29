\ ddc-verify-test.f - focused tests for the DDC comparison core.
\ Run: bin/hb --load src/lib/errors.f src/lib/string.f src/lib/test.f src/lib/memory.f src/lib/fs.f
\ src/lib/fs-mutate.f src/lib/process.f src/lib/process-argv.f src/lib/process-env.f
\ tools/ddc-verify.f tools/ddc-verify-test.f
\
\ Covers DDC-LOAD/DDC-SAME?/DDC-FIRST-DIFF/DDC-REPORT on synthetic artifact
\ files, the seed path read from the bootstrap's stdout (DDC-SEED!) and the
\ DDC-FXP$ scratch path. The full DDC-VERIFY
\ orchestration (runs the gforth+refresh chain) is an explicit manual audit, not
\ exercised here.

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require tools/ddc-verify.f

package DDCT

create ROOT-BUF PATH-CAP allot
variable ROOT-U
create A-PATH PATH-CAP allot
variable A-U
create B-PATH PATH-CAP allot
variable B-U

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: A$ ( -- ptr u8 n ) A-PATH A-U @ ;
: B$ ( -- ptr u8 n ) B-PATH B-U @ ;

: PATH! ( ptr u8 n ptr u8 ptr n -- ) {: name:ptr nameu:n dst:ptr up:ptr :}
   ROOT$ name nameu dst FS:JOIN-PATH up ! ;

: ROOT! ( ptr u8 n -- ) {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY  u ROOT-U ! ;

: PREPARE ( -- )
   s" habu-ddc-test" FS:MKTEMP-DIR ROOT!
   s" a.bin" A-PATH A-U PATH!
   s" b.bin" B-PATH B-U PATH! ;

: TEST-IDENTICAL ( -- )
   A$ s" fixpoint-artifact-bytes" FS:WRITE-ALL
   B$ s" fixpoint-artifact-bytes" FS:WRITE-ALL
   A$ B$ DDC-LOAD
   s" identical same?" T-LABEL
   DDC-SAME? TTRUE
   s" identical first-diff" T-LABEL
   DDC-FIRST-DIFF -1 T=
   s" identical report rc" T-LABEL
   s" a" s" b" DDC-REPORT 0 T= ;

: TEST-ONE-BYTE-DIFF ( -- )
   A$ s" fixpoint-artifact-bytes" FS:WRITE-ALL
   B$ s" fixpoint-artifact-Xytes" FS:WRITE-ALL   \ differs at offset 18
   A$ B$ DDC-LOAD
   s" diff same?" T-LABEL
   DDC-SAME? TFALSE
   s" diff first offset" T-LABEL
   DDC-FIRST-DIFF 18 T=
   s" diff report rc" T-LABEL
   s" a" s" b" DDC-REPORT DDC-DIVERGENT-RC T= ;

: TEST-LENGTH-DIFF ( -- )
   A$ s" short" FS:WRITE-ALL
   B$ s" short-plus-tail" FS:WRITE-ALL
   A$ B$ DDC-LOAD
   s" length same?" T-LABEL
   DDC-SAME? TFALSE
   s" length first offset" T-LABEL
   DDC-FIRST-DIFF 5 T= ;         \ shorter length is the first mismatch offset

\ The gforth seed is wherever the bootstrap's stdout line names it, among the
\ other lines it prints; the native-refresh fixpoint hangs off its scratch root
\ at a fixed leaf name. These are what the audit env/argv are built from.
: TEST-PATHS ( -- )
   S\" checking\nbootstrap check OK: /tmp/ddc-gf-root/habu-gforth.x1/hb-stdin\n"
   DDC-SEED!
   s" seed path" T-LABEL
   DDC-STDIN$ s" /tmp/ddc-gf-root/habu-gforth.x1/hb-stdin" STR= TTRUE
   s" /tmp/ddc-rf-root" DDC-RF-ROOT!
   s" fixpoint path" T-LABEL
   DDC-FXP$ s" /tmp/ddc-rf-root/hb-ddc-fixpoint" STR= TTRUE ;

\ A bootstrap stdout with no seed line names no seed.
: TEST-SEED-MISSING ( -- )
   s" seed line missing" T-LABEL
   [: S\" checking\nbootstrap check failed\n" DDC-SEED! ;] DDC-E-SEED TTHROWSQ ;

\ HABU_ALLOW_BOOTSTRAP=1 is the audit gate; without it DDC-REQUIRE-BOOTSTRAP must
\ fail closed with DDC-E-USAGE. Assert the outcome against the actual env so the
\ test is correct whether or not the audit gate is set in the caller.
: BOOTSTRAP-SET? ( -- bool ) s" HABU_ALLOW_BOOTSTRAP" GETENV s" 1" STR= ;
: TEST-REQUIRE-BOOTSTRAP ( -- )
   s" require-bootstrap matches env" T-LABEL
   [: DDC-REQUIRE-BOOTSTRAP ;] catch
   BOOTSTRAP-SET? if 0 T= else DDC-E-USAGE T= then ;

: MAIN ( -- )
   T-RESET
   PREPARE
   TEST-IDENTICAL
   TEST-ONE-BYTE-DIFF
   TEST-LENGTH-DIFF
   TEST-PATHS
   TEST-SEED-MISSING
   TEST-REQUIRE-BOOTSTRAP
   ROOT$ FS:REMOVE-TREE
   T-REPORT
   s" ddc-verify-test: ok" type cr ;

MAIN

;package
