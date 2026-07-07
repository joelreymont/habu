\ ddc-verify.f - Diverse Double-Compiling audit for the bin/hb fixpoint.
\
\ Builds bin/hb two ways and requires byte-identical output:
\   NATIVE chain  - the self-hosted fixpoint (bin/hb rebuilds bin/hb).
\   GFORTH chain  - the independent bootstrap host (tools/bootstrap.sh with
\                   HABU_BOOTSTRAP_CHECK_ONLY=1, which stops after emitting a
\                   SMALL hb-stdin engine without touching the checkout's bin/hb).
\ A seed backdoor would have to be mirrored in BOTH gforth and the native seed
\ to survive the sha256 compare, reducing seed trust to "no coordinated
\ cross-host backdoor". This is an EXPLICIT audit (never per-commit): it needs a
\ working gforth recovery host and is gated on HABU_ALLOW_BOOTSTRAP=1, like the
\ launcher it drives (docs/bootstrap.md).
\
\ The pure comparison words (DDC-SAME?/DDC-FIRST-DIFF/DDC-REPORT) are covered by
\ tools/ddc-verify-test.f; the full DDC-VERIFY orchestration is manual/audit
\ because it runs the whole gforth chain.

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
$400000 constant DDC-BIN-CAP           \ per-artifact read buffer (bin/hb ~130 KB)
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

: DDC-TRUE ( -- bool ) 0 0= ;
: DDC-FALSE ( -- bool ) 0 0= 0= ;

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

\ ---- audit orchestration (manual; runs the gforth chain) --------------------

create DDC-TMP-BUF FS-PATH-CAP allot
variable DDC-TMP-U
create DDC-GF-BUF FS-PATH-CAP allot
variable DDC-GF-U

: DDC-TMP! ( ptr u8 n -- ) {: a:ptr u:n :}
   a DDC-TMP-BUF u BYTE-COPY  u DDC-TMP-U ! ;

: DDC-REQUIRE-BOOTSTRAP ( -- )
   s" HABU_ALLOW_BOOTSTRAP" GETENV s" 1" STR= 0= if
      s" ddc: set HABU_ALLOW_BOOTSTRAP=1 (drives the gforth recovery host)" type cr
      DDC-E-USAGE throw
   then ;

: DDC-GFORTH-ARTIFACT ( -- ptr u8 n )
   DDC-TMP-BUF DDC-TMP-U @ s" hb-stdin" DDC-GF-BUF JOIN-PATH DDC-GF-U !
   DDC-GF-BUF DDC-GF-U @ ;

\ Run tools/bootstrap.sh CHECK_ONLY into a fresh HB_TMP; it emits hb-stdin there
\ and never touches the checkout's bin/hb, so the native reference stays intact.
: DDC-RUN-GFORTH ( -- )
   PROC-ARGV-RESET
   PROC-ENV-RESET
   s" HABU_ALLOW_BOOTSTRAP" >LEN s" 1" >LEN PROC-ENV+
   s" HABU_BOOTSTRAP_CHECK_ONLY" >LEN s" 1" >LEN PROC-ENV+
   s" HB_TMP" >LEN DDC-TMP-BUF DDC-TMP-U @ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" tools/bootstrap.sh" >LEN
   DDC-OUT DDC-CAP-OUT >LEN  DDC-ERR DDC-CAP-OUT >LEN
   DDC-CHAIN-TIMEOUT-MS >MS RUN-ARGV-ENV-CAPTURE {: outl:len errl:len rc:rc :}
   rc RC>N 0 <> if
      s" ddc: gforth bootstrap chain failed rc " type rc RC>N DDC-U. cr
      DDC-ERR errl LEN>N type cr
      DDC-E-CHAIN throw
   then ;

: DDC-VERIFY ( -- n )
   DDC-REQUIRE-BOOTSTRAP
   s" habu-ddc" TMPDIR-MKDIR DDC-TMP!
   DDC-RUN-GFORTH
   s" bin/hb" DDC-GFORTH-ARTIFACT DDC-LOAD
   s" bin/hb (native fixpoint)" s" hb-stdin (gforth chain)" DDC-REPORT ;

: DDC-MAIN ( -- )
   DDC-VERIFY {: rc:n :}
   rc 0 <> if s" " rc die then ;
