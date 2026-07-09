\ ddc-verify-test.f - focused tests for the DDC comparison core.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f
\ lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f
\ tools/ddc-verify.f tools/ddc-verify-test.f
\
\ Covers DDC-LOAD/DDC-SAME?/DDC-FIRST-DIFF/DDC-REPORT on synthetic artifact
\ files. The full DDC-VERIFY orchestration (runs the gforth chain) is an
\ explicit manual audit, not exercised here.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require tools/ddc-verify.f

package DDCT

create ROOT-BUF FS-PATH-CAP allot
variable ROOT-U
create A-PATH FS-PATH-CAP allot
variable A-U
create B-PATH FS-PATH-CAP allot
variable B-U

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: A$ ( -- ptr u8 n ) A-PATH A-U @ ;
: B$ ( -- ptr u8 n ) B-PATH B-U @ ;

: PATH! ( ptr u8 n ptr u8 ptr a -- ) {: name:ptr nameu:n dst:ptr up:ptr :}
   ROOT$ name nameu dst JOIN-PATH up ! ;

: ROOT! ( ptr u8 n -- ) {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY  u ROOT-U ! ;

: PREPARE ( -- )
   s" habu-ddc-test" TMPDIR-MKDIR ROOT!
   s" a.bin" A-PATH A-U PATH!
   s" b.bin" B-PATH B-U PATH! ;

: TEST-IDENTICAL ( -- )
   A$ s" fixpoint-artifact-bytes" WRITE-ALL
   B$ s" fixpoint-artifact-bytes" WRITE-ALL
   A$ B$ DDC-LOAD
   s" identical same?" T-LABEL
   DDC-SAME? TTRUE
   s" identical first-diff" T-LABEL
   DDC-FIRST-DIFF -1 T=
   s" identical report rc" T-LABEL
   s" a" s" b" DDC-REPORT 0 T= ;

: TEST-ONE-BYTE-DIFF ( -- )
   A$ s" fixpoint-artifact-bytes" WRITE-ALL
   B$ s" fixpoint-artifact-Xytes" WRITE-ALL   \ differs at offset 18
   A$ B$ DDC-LOAD
   s" diff same?" T-LABEL
   DDC-SAME? TFALSE
   s" diff first offset" T-LABEL
   DDC-FIRST-DIFF 18 T=
   s" diff report rc" T-LABEL
   s" a" s" b" DDC-REPORT DDC-DIVERGENT-RC T= ;

: TEST-LENGTH-DIFF ( -- )
   A$ s" short" WRITE-ALL
   B$ s" short-plus-tail" WRITE-ALL
   A$ B$ DDC-LOAD
   s" length same?" T-LABEL
   DDC-SAME? TFALSE
   s" length first offset" T-LABEL
   DDC-FIRST-DIFF 5 T= ;         \ shorter length is the first mismatch offset

: MAIN ( -- )
   T-RESET
   PREPARE
   TEST-IDENTICAL
   TEST-ONE-BYTE-DIFF
   TEST-LENGTH-DIFF
   ROOT$ REMOVE-TREE
   T-REPORT
   s" ddc-verify-test: ok" type cr ;

MAIN

end-package
