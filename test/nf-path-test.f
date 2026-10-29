\ nf-path-test.f - complete TMPDIR paths through the Gforth native fixture.

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/test/runner.f
require test/gate-pool.f
require test/pcap-result.f

package NF-PATH-TEST
private

97 constant LONG-ROOT-U
122 constant OVERFLOW-ROOT-U
$4000 constant CAP
120000 constant BUILD-TIMEOUT-MS
10000 constant PROBE-TIMEOUT-MS
$2F constant SLASH
$78 constant LOWER-X

create ROOT PATH-CAP allot       variable ROOT-U
create LONG-ROOT PATH-CAP allot  variable LONG-U
create QUOTED-ROOT PATH-CAP allot variable QUOTED-U
create OVERFLOW-ROOT PATH-CAP allot variable OVERFLOW-U
create FORGED-ROOT PATH-CAP allot variable FORGED-U
create BIN-SFX PATH-CAP allot     variable BIN-SFX-U
create OUT-SFX PATH-CAP allot     variable OUT-SFX-U
variable BIN#  variable OUT#  variable OUT-OK
create OUT CAP allot
create ERR CAP allot

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: LONG$ ( -- ptr u8 n ) LONG-ROOT LONG-U @ ;
: QUOTED$ ( -- ptr u8 n ) QUOTED-ROOT QUOTED-U @ ;
: OVERFLOW$ ( -- ptr u8 n ) OVERFLOW-ROOT OVERFLOW-U @ ;
: FORGED$ ( -- ptr u8 n ) FORGED-ROOT FORGED-U @ ;

: TARGET$ ( -- ptr u8 n )
   HB-TARGET-LINUX? if s" linux-aarch64" exit then
   HB-TARGET-MACOS? if s" macos-aarch64" exit then
   HB-TARGET-LINUX-X86-64? if s" linux-x86-64" exit then
   s" nf-path-test: unknown target" 76 die ;

: GFORTH$ ( -- ptr u8 n )
   s" GFORTH" GETENV dup 0= if 2drop s" gforth" then ;

: PAD-X ( ptr u8 n n -- )
   {: dst:ptr at:n want:n :}
   at begin dup want < while
      dup dst + LOWER-X swap c!
      1+
   repeat drop ;

: LONG-ROOT! ( -- )
   ROOT-U @ 1 + LONG-ROOT-U >= if E-FS-CAPACITY throw then
   ROOT LONG-ROOT ROOT-U @ BYTE-COPY
   SLASH LONG-ROOT ROOT-U @ + c!
   LONG-ROOT ROOT-U @ 1 + LONG-ROOT-U PAD-X
   LONG-ROOT-U LONG-U ! ;

: OVERFLOW-ROOT! ( -- )
   ROOT-U @ 1 + OVERFLOW-ROOT-U >= if E-FS-CAPACITY throw then
   ROOT OVERFLOW-ROOT ROOT-U @ BYTE-COPY
   SLASH OVERFLOW-ROOT ROOT-U @ + c!
   OVERFLOW-ROOT ROOT-U @ 1 + OVERFLOW-ROOT-U PAD-X
   OVERFLOW-ROOT-U OVERFLOW-U ! ;

\ LONG-ROOT! pads this root to exactly LONG-ROOT-U bytes, so a base of that
\ length or more cannot produce the path at all. Under the pool TMPDIR is the
\ slot directory - <suite root>/pool-<pid>-<seq>-tmp, 97 bytes measured - which
\ makes SETUP throw E-FS-CAPACITY before the first build, so this test runs on
\ its own (docs/gate.md), where TMPDIR, then /tmp, is the shortest base a run
\ can offer; FS:CLEANUP-TREE+ below removes the tree.
: SETUP ( -- )
   FS:CLEANUP-RESET
   s" habu-nf-path" FS:MKTEMP-DIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY  u ROOT-U !
   ROOT$ FS:CLEANUP-TREE+
   LONG-ROOT!
   OVERFLOW-ROOT!
   ROOT$ S\" lane one 'quote; dollar$ \qdouble\q [brackets]" QUOTED-ROOT FS:JOIN-PATH QUOTED-U !
   LONG$ FS:MAKE-DIRS
   QUOTED$ FS:MAKE-DIRS
   ROOT$ s" forged" FORGED-ROOT FS:JOIN-PATH FORGED-U !
   FORGED$ FS:MAKE-DIRS ;

: GF-ENV ( ptr u8 n -- ) {: root:ptr rootu:n :}
   PROC-ENV-RESET
   s" TMPDIR" >LEN root rootu >LEN PROC-ENV+
   s" HABU_TARGET" >LEN TARGET$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING ;

: GF-FILE-ARGV ( -- )
   PROC-ARGV-RESET
   GFORTH$ >LEN PROC-ARGV+
   s" test/bootstrap-wide-memory.fs" >LEN PROC-ARGV+ ;

: GF-START ( ptr u8 n ptr u8 n -- )
   {: root:ptr rootu:n label:ptr labelu:n :}
   GF-FILE-ARGV
   root rootu GF-ENV
   s" /usr/bin/env" label labelu BUILD-TIMEOUT-MS GT-POOL-START ;

: SUFFIX$ ( ptr u8 n ptr u8 n -- ptr u8 n )   \ name past prefix, else empty
   {: a:ptr u:n p:ptr pu:n :}
   a u p pu STARTS-WITH? 0= if a 0 exit then
   a pu +  u pu - ;

: SEEN-BIN ( ptr u8 n -- ) {: a:ptr u:n :}
   a BIN-SFX u BYTE-COPY  u BIN-SFX-U !  1 BIN# +! ;

: SEEN-OUT ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu:n a:ptr u:n :}
   a OUT-SFX u BYTE-COPY  u OUT-SFX-U !  1 OUT# +!
   path pathu OUT CAP FS:READ-ALL {: outu:n :}
   OUT outu S\" ok\n" STR= OUT-OK ! ;

: ARTIFACT ( ptr u8 n -- ) {: path:ptr pathu:n :}
   path pathu FS:BASENAME {: name:ptr nameu:n :}
   name nameu s" nf-bin-" SUFFIX$ {: b:ptr bu:n :}
   b bu STR-DIGITS? if b bu SEEN-BIN exit then
   name nameu s" nf-out-" SUFFIX$ {: o:ptr ou:n :}
   o ou STR-DIGITS? if path pathu o ou SEEN-OUT then ;

\ nf.fs names its artifacts <root>/nf-bin-<pid> and <root>/nf-out-<pid> after
\ the gforth child, whose pid the pool does not report. Each root is a fresh
\ directory, so one bin and one out sharing a digit suffix is that exact pair.
: ARTIFACTS-OK? ( ptr u8 n -- bool ) {: root:ptr rootu:n :}
   0 BIN# !  0 OUT# !  false OUT-OK !
   root rootu [: ARTIFACT ;] FS:WALK-FILES
   BIN# @ 1 = OUT# @ 1 = and OUT-OK @ 0<> and
   BIN-SFX BIN-SFX-U @ OUT-SFX OUT-SFX-U @ STR= and ;

: CONCURRENT-CASE ( -- )
   GT-POOL-RESET
   LONG$ s" a 97-byte fixture root builds and runs concurrently" GF-START
   QUOTED$ s" spaces, quotes, and shell metacharacters stay one complete path" GF-START
   GT-POOL-DRAIN
   s" the long-root artifact and complete output path exist" T-LABEL
   LONG$ ARTIFACTS-OK? TTRUE
   s" the quoted-root artifact and complete output path exist" T-LABEL
   QUOTED$ ARTIFACTS-OK? TTRUE ;

: FORGED-ARGV ( -- )
   PROC-ARGV-RESET
   s" sh" >LEN PROC-ARGV+
   s" -c" >LEN PROC-ARGV+
   S\" printf 'fake binary\\n' > \q$1/nf-bin-4242\q; printf 'ok\\n' > \q$1/nf-out-4242\q; printf 'ok\\n'; exit 23" >LEN PROC-ARGV+
   s" nf-path-forge" >LEN PROC-ARGV+
   FORGED$ >LEN PROC-ARGV+ ;

: FORGED-RUN ( -- n n n )
   FORGED-ARGV
   PROC-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" /usr/bin/env" >LEN OUT CAP >LEN ERR CAP >LEN PROBE-TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE PCAP-RESULT:>N ;

: NONZERO-CONTROL-CASE ( -- )
   FORGED-RUN {: outu:n erru:n rc:n :}
   s" a child that forged the expected files preserves its nonzero exit" T-LABEL
   rc 23 T=
   s" the negative control produced expected-looking output and artifacts" T-LABEL
   OUT outu S\" ok\n" T$=
   erru 0 T=
   FORGED$ ARTIFACTS-OK? TTRUE
   s" expected-looking artifacts do not turn a nonzero child green" T-LABEL
   rc 0= OUT outu S\" ok\n" STR= and
   FORGED$ ARTIFACTS-OK? and TFALSE ;

: GF-EVAL ( ptr u8 n ptr u8 n -- n n n )
   {: root:ptr rootu:n src:ptr srcu:n :}
   PROC-ARGV-RESET
   GFORTH$ >LEN PROC-ARGV+
   s" -e" >LEN PROC-ARGV+
   src srcu >LEN PROC-ARGV+
   root rootu GF-ENV
   s" /usr/bin/env" >LEN OUT CAP >LEN ERR CAP >LEN PROBE-TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE PCAP-RESULT:>N ;

: COMMAND-CAPACITY-CASE ( -- )
   LONG$ S\" require test/nf.fs NF-REPL-CMD$ nip 256 <= abort\q nf path command stayed inside the obsolete bound\q bye" GF-EVAL
   s" a long complete REPL command exceeds the obsolete 256-byte buffer" T-LABEL
   0 T= drop drop ;

: OVERFLOW-CASE ( -- )
   OVERFLOW$ s" require test/nf.fs bye" GF-EVAL
   {: outu:n erru:n rc:n :}
   s" a root beyond the fixture path contract refuses" T-LABEL
   rc 0<> TTRUE
   s" path overflow keeps its named diagnostic" T-LABEL
   ERR erru s" nf.fs: scratch path exceeds NF-PATH-CAP" CONTAINS? TTRUE ;

public

: RUN ( -- )
   T-RESET
   SETUP
   CONCURRENT-CASE
   NONZERO-CONTROL-CASE
   COMMAND-CAPACITY-CASE
   OVERFLOW-CASE
   FS:CLEANUP-RUN
   T-REPORT ;

;package

NF-PATH-TEST:RUN
