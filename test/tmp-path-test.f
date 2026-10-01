\ tmp-path-test.f - TMP-PATH holds a PATH-CAP path and no length wraps past it.
\
\ TMP-PATH (src/os/env-base.f) answers HB_TMP, a slash and the caller's name in
\ one buffer of PATH-CAP bytes, and ends the process with exit 76 when they do
\ not fit. It can fail four ways: an exact fill refused, one byte too many
\ admitted, a negative length taken as room, and a length whose sum with the
\ root wraps back into range. The refusal is a die, so each case is a child
\ engine (test/tmp-path-child.f) whose HB_TMP is this test's own directory.
\
\ Run: bin/hb --load test/tmp-path-test.f

require lib/string.f
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/engine-candidate.f

package TMP-PATH-TEST
private

$1000 constant IO-CAP
120000 constant TIMEOUT-MS
76 constant REFUSE-RC
$2F constant SLASH
$0A constant LF

create ROOT FS-PATH-CAP allot   variable ROOT-U
create OUT IO-CAP allot
create ERR IO-CAP allot

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;

: SETUP ( -- )
   CLEANUP-RESET
   s" habu-tmp-path" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY  u ROOT-U !
   ROOT$ CLEANUP-TREE+ ;

: RUN ( ptr u8 n -- n n n ) {: case:ptr caseu:n :}   \ outu erru rc
   ENGINE-CANDIDATE:PATH$ {: engine:ptr engineu:n :}
   PROC-ARGV-ENV-RESET
   s" HB_TMP" >LEN ROOT$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --load" >LEN PROC-ARGV+
   s" test/tmp-path-child.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   case caseu >LEN PROC-ARGV+
   engine engineu >LEN s" " >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   outu LEN>N erru LEN>N rc ;

: EXACT-CASE ( -- )
   s" exact" RUN {: outu:n erru:n rc:n :}
   s" a name that fills PATH-CAP with the root and its slash is answered" T-LABEL
   rc 0 T=
   s" ... as a path of PATH-CAP bytes" T-LABEL
   outu PATH-CAP 1 + T=
   s" ... that starts with the root" T-LABEL
   OUT ROOT-U @ ROOT$ STR= TTRUE
   s" ... then the slash" T-LABEL
   OUT ROOT-U @ + c@ SLASH T=
   s" ... and ends with the name's last byte" T-LABEL
   OUT PATH-CAP 1 - + c@ [char] a T=          \ test/tmp-path-child.f LOWER-A
   s" ... and nothing after it but the line end" T-LABEL
   OUT PATH-CAP + c@ LF T= ;

: REFUSED ( ptr u8 n ptr u8 n -- ) {: case:ptr caseu:n label:ptr labelu:n :}
   case caseu RUN {: outu:n erru:n rc:n :}
   label labelu T-LABEL
   rc REFUSE-RC T=
   s" ... by TMP-PATH's own message" T-LABEL
   ERR erru s" env: TMP-PATH exceeds buffer" CONTAINS? TTRUE
   s" ... before anything is typed" T-LABEL
   outu 0 T= ;

public

: TMP-PATH-TEST-MAIN ( -- )
   T-RESET
   SETUP
   EXACT-CASE
   s" over" s" one byte past PATH-CAP is refused with exit 76" REFUSED
   s" negative" s" a negative length is refused, not taken as room" REFUSED
   s" huge" s" the maximum cell is refused, not wrapped into range" REFUSED
   CLEANUP-RUN
   T-REPORT
   s" tmp-path-test: ok" type cr ;

;package

TMP-PATH-TEST:TMP-PATH-TEST-MAIN
