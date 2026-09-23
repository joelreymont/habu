\ nf-path-test.f - complete HB_TMP paths through the Gforth native fixture.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/test/runner.f
require test/gate-pool.f

package NF-PATH-TEST
private

97 constant LONG-ROOT-U
122 constant OVERFLOW-ROOT-U
$4000 constant CAP
120000 constant BUILD-TIMEOUT-MS
10000 constant PROBE-TIMEOUT-MS
$2F constant SLASH
$78 constant LOWER-X

create ROOT FS-PATH-CAP allot       variable ROOT-U
create LONG-ROOT FS-PATH-CAP allot  variable LONG-U
create QUOTED-ROOT FS-PATH-CAP allot variable QUOTED-U
create OVERFLOW-ROOT FS-PATH-CAP allot variable OVERFLOW-U
create FORGED-ROOT FS-PATH-CAP allot variable FORGED-U
create PATH-BUF FS-PATH-CAP allot
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

\ The one fixture root in the tree that is not HB-TMP-MKDIR's answer. LONG-ROOT!
\ pads this root to exactly LONG-ROOT-U bytes, so a base of that length or more
\ cannot produce the path at all, and under the pool HB_TMP is the slot
\ directory - <suite root>/pool-<pid>-<seq>-tmp, 97 bytes measured - which makes
\ SETUP throw E-FS-CAPACITY before the first build. TMPDIR, then /tmp, is the
\ shortest base a run can offer; CLEANUP-TREE+ below removes the tree.
: SETUP ( -- )
   CLEANUP-RESET
   s" habu-nf-path" TMPDIR-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY  u ROOT-U !
   ROOT$ CLEANUP-TREE+
   LONG-ROOT!
   OVERFLOW-ROOT!
   ROOT$ S\" lane one 'quote; dollar$ \qdouble\q [brackets]" QUOTED-ROOT JOIN-PATH QUOTED-U !
   LONG$ MAKE-DIRS
   QUOTED$ MAKE-DIRS
   ROOT$ s" forged" FORGED-ROOT JOIN-PATH FORGED-U !
   FORGED$ MAKE-DIRS ;

: GF-ENV ( ptr u8 n -- ) {: root:ptr rootu:n :}
   PROC-ENV-RESET
   s" HB_TMP" >LEN root rootu >LEN PROC-ENV+
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

: ROOT-FILE? ( ptr u8 n ptr u8 n -- bool )
   {: root:ptr rootu:n name:ptr nameu:n :}
   root rootu name nameu PATH-BUF JOIN-PATH {: pathu:n :}
   PATH-BUF pathu FILE? ;

: ROOT-OUT-OK? ( ptr u8 n -- bool )
   {: root:ptr rootu:n :}
   root rootu s" nf-out" PATH-BUF JOIN-PATH {: pathu:n :}
   PATH-BUF pathu FILE? 0= if false exit then
   PATH-BUF pathu OUT CAP READ-ALL {: outu:n :}
   OUT outu S\" ok\n" STR= ;

: CONCURRENT-CASE ( -- )
   GT-POOL-RESET
   LONG$ s" a 97-byte fixture root builds and runs concurrently" GF-START
   QUOTED$ s" spaces, quotes, and shell metacharacters stay one complete path" GF-START
   GT-POOL-DRAIN
   s" the long-root artifact and complete output path exist" T-LABEL
   LONG$ s" nf-bin" ROOT-FILE? TTRUE
   LONG$ ROOT-OUT-OK? TTRUE
   s" the quoted-root artifact and complete output path exist" T-LABEL
   QUOTED$ s" nf-bin" ROOT-FILE? TTRUE
   QUOTED$ ROOT-OUT-OK? TTRUE ;

: FORGED-ARGV ( -- )
   PROC-ARGV-RESET
   s" sh" >LEN PROC-ARGV+
   s" -c" >LEN PROC-ARGV+
   S\" printf 'fake binary\\n' > \q$1/nf-bin\q; printf 'ok\\n' > \q$1/nf-out\q; printf 'ok\\n'; exit 23" >LEN PROC-ARGV+
   s" nf-path-forge" >LEN PROC-ARGV+
   FORGED$ >LEN PROC-ARGV+ ;

: FORGED-RUN ( -- n n n )
   FORGED-ARGV
   PROC-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" /usr/bin/env" >LEN OUT CAP >LEN ERR CAP >LEN PROBE-TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE MATCH result
     ok OF
       PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
       outu LEN>N erru LEN>N 0
     ENDOF
     err OF
       PCAP-FAILED:UNMAKE {: outu:len erru:len rc:rc :}
       outu LEN>N erru LEN>N rc RC>N
     ENDOF
   ;MATCH ;

: NONZERO-CONTROL-CASE ( -- )
   FORGED-RUN {: outu:n erru:n rc:n :}
   s" a child that forged the expected files preserves its nonzero exit" T-LABEL
   rc 23 T=
   s" the negative control produced expected-looking output and artifacts" T-LABEL
   OUT outu S\" ok\n" T$=
   erru 0 T=
   FORGED$ s" nf-bin" ROOT-FILE? TTRUE
   FORGED$ ROOT-OUT-OK? TTRUE
   s" expected-looking artifacts do not turn a nonzero child green" T-LABEL
   rc 0= OUT outu S\" ok\n" STR= and
   FORGED$ s" nf-bin" ROOT-FILE? and FORGED$ ROOT-OUT-OK? and TFALSE ;

: GF-EVAL ( ptr u8 n ptr u8 n -- n n n )
   {: root:ptr rootu:n src:ptr srcu:n :}
   PROC-ARGV-RESET
   GFORTH$ >LEN PROC-ARGV+
   s" -e" >LEN PROC-ARGV+
   src srcu >LEN PROC-ARGV+
   root rootu GF-ENV
   s" /usr/bin/env" >LEN OUT CAP >LEN ERR CAP >LEN PROBE-TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE MATCH result
     ok OF
       PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
       outu LEN>N erru LEN>N 0
     ENDOF
     err OF
       PCAP-FAILED:UNMAKE {: outu:len erru:len rc:rc :}
       outu LEN>N erru LEN>N rc RC>N
     ENDOF
   ;MATCH ;

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
   CLEANUP-RUN
   T-REPORT ;

;package

NF-PATH-TEST:RUN
