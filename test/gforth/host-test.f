\ host-test.f - every test/gforth/cases program under the Gforth host and native.
\
\ Run from the repository root. Each case program runs twice with stdin from
\ /dev/null: as `gforth -m 1G src/host/gforth/boot.fs <case>`, the Gforth found
\ as test/nf-path-test.f finds it (GFORTH, else gforth on PATH), and as
\ `<engine> --load <case>`, the engine lib/engine-candidate.f resolves (the
\ running bin/hb unless HABU_UNDER_TEST names one). A case matches when the two
\ exit codes, stdouts and stderrs (the diagnostic JSON) are byte-identical.
\ $HB_TMP/gforth-host holds this run's <case>.{gf,hb}.{out,err,rc}, an rc file
\ the decimal exit code. One line per case; the exit code is the verdict.

require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/fs.f
require lib/fs-list.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/engine-candidate.f
require lib/test/assert.f                 \ T-EX-FAIL

package GFORTH-HOST-TEST
private

0 constant GF-SIDE
1 constant HB-SIDE
$10000 constant CAP                       \ the most one stream of one run may write
60000 constant RUN-TIMEOUT-MS             \ the longest one run of one case may take
4096 constant LISTING-CAP
10 constant LF

create ROOT-BUF FS-PATH-CAP allot  variable ROOT-U
create PROG-BUF FS-PATH-CAP allot
create ART-BUF FS-PATH-CAP allot
create LISTING LISTING-CAP allot
create OUTS CAP 2 * allot
create ERRS CAP 2 * allot
2 TYPED-BUFFER SIDE-OUT-U n
2 TYPED-BUFFER SIDE-ERR-U n
2 TYPED-BUFFER SIDE-RC n
variable CASES-RUN
variable CASES-BAD

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: CASES$ ( -- ptr u8 n ) s" test/gforth/cases" ;
: OUT ( n -- ptr u8 ) CAP * OUTS + ;
: ERR ( n -- ptr u8 ) CAP * ERRS + ;
: OUT$ ( n -- ptr u8 n ) dup OUT swap SIDE-OUT-U @ ;
: ERR$ ( n -- ptr u8 n ) dup ERR swap SIDE-ERR-U @ ;
: SIDE$ ( n -- ptr u8 n ) GF-SIDE = if s" gf" else s" hb" then ;
: SAME$ ( bool -- ptr u8 n ) if s" same" else s" differs" then ;

: FAIL ( ptr u8 n ptr u8 n -- ) {: what:ptr whatu:n name:ptr nameu:n :}
   SB-RESET
   s" gforth-host: " SB-APPEND
   what whatu SB-APPEND
   name nameu SB-APPEND
   SB$ T-EX-FAIL die ;

\ The artifact directory, emptied so it holds this run's cases only.
: ROOT! ( -- )
   s" HB_TMP" GETENV {: tmp:ptr tmpu:n :}
   tmpu 0= if s" HB_TMP must name a scratch directory" s" " FAIL then
   tmp tmpu s" gforth-host" ROOT-BUF JOIN-PATH ROOT-U !
   ROOT$ EXISTS? if ROOT$ REMOVE-TREE then
   ROOT$ MAKE-DIRS ;

: GFORTH$ ( -- ptr u8 n )
   s" GFORTH" GETENV dup 0= if 2drop s" gforth" then ;

\ Stage one side's arguments and answer the executable that runs them.
: GF-ARGV ( ptr u8 n -- ptr u8 n ) {: prog:ptr progu:n :}
   PROC-ARGV-RESET
   GFORTH$ >LEN PROC-ARGV+
   s" -m" >LEN PROC-ARGV+
   s" 1G" >LEN PROC-ARGV+
   s" src/host/gforth/boot.fs" >LEN PROC-ARGV+
   prog progu >LEN PROC-ARGV+
   s" /usr/bin/env" ;

: HB-ARGV ( ptr u8 n -- ptr u8 n ) {: prog:ptr progu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   prog progu >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ ;

: RUN-SIDE ( ptr u8 n n -- ) {: prog:ptr progu:n side:n :}
   prog progu side GF-SIDE = if GF-ARGV else HB-ARGV then {: path:ptr pathu:n :}
   PROC-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   path pathu >LEN
   side OUT CAP >LEN
   side ERR CAP >LEN
   RUN-TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE MATCH result
     ok OF
       PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
       outu LEN>N erru LEN>N 0
     ENDOF
     err OF
       PCAP-FAILED:UNMAKE {: outu:len erru:len rc:rc :}
       outu LEN>N erru LEN>N rc RC>N
     ENDOF
   ;MATCH
   side SIDE-RC !
   side SIDE-ERR-U !
   side SIDE-OUT-U ! ;

\ <root>/<case>.<side><ext>, in ART-BUF.
: ART$ ( ptr u8 n n ptr u8 n -- ptr u8 n )
   {: name:ptr nameu:n side:n ext:ptr extu:n :}
   SB-RESET
   name nameu SB-APPEND
   s" ." SB-APPEND
   side SIDE$ SB-APPEND
   ext extu SB-APPEND
   ROOT$ SB$ ART-BUF JOIN-PATH ART-BUF swap ;

: SAVE-SIDE ( ptr u8 n n -- ) {: name:ptr nameu:n side:n :}
   name nameu side s" .out" ART$ side OUT$ WRITE-ALL
   name nameu side s" .err" ART$ side ERR$ WRITE-ALL
   name nameu side s" .rc" ART$
   SB-RESET side SIDE-RC @ FMT:SB-INT LF SB-APPEND-C
   SB$ WRITE-ALL ;

: RC-SAME? ( -- bool ) GF-SIDE SIDE-RC @ HB-SIDE SIDE-RC @ = ;
: OUT-SAME? ( -- bool ) GF-SIDE OUT$ HB-SIDE OUT$ STR= ;
: ERR-SAME? ( -- bool ) GF-SIDE ERR$ HB-SIDE ERR$ STR= ;

\ What differs; the artifacts hold both sides' streams.
: TELL-MISMATCH ( -- )
   s"  mismatch: rc gf=" type GF-SIDE SIDE-RC @ FMT:.INT
   s"  hb=" type HB-SIDE SIDE-RC @ FMT:.INT
   s" , stdout " type OUT-SAME? SAME$ type
   s" , stderr " type ERR-SAME? SAME$ type ;

\ Prints the case's line and answers whether the two sides agree.
: TELL ( ptr u8 n -- bool )
   type
   RC-SAME? OUT-SAME? and ERR-SAME? and
   dup if s"  match" type else TELL-MISMATCH then cr ;

\ One directory entry, a case program <name>.f.
: PROGRAM? ( ptr u8 n -- bool ) {: entry:ptr entryu:n :}
   entryu 2 <= if false exit then
   entry entryu + 2 - 2 s" .f" STR= ;

: RUN-CASE ( ptr u8 n -- ) {: entry:ptr entryu:n :}
   entry entryu PROGRAM? 0= if s" not a .f case program: " entry entryu FAIL then
   CASES$ entry entryu PROG-BUF JOIN-PATH {: progu:n :}
   entry entryu 2 - {: name:ptr nameu:n :}
   PROG-BUF progu GF-SIDE RUN-SIDE
   name nameu GF-SIDE SAVE-SIDE
   PROG-BUF progu HB-SIDE RUN-SIDE
   name nameu HB-SIDE SAVE-SIDE
   1 CASES-RUN +!
   name nameu TELL 0= if 1 CASES-BAD +! then ;

\ The listing's newline-separated entries, in byte order.
: EACH-CASE ( ptr u8 n -- ) {: a:ptr u:n :}
   0 begin
      >r a u LF r> SPLIT-NEXT
   while
      >r dup 0 > if RUN-CASE else 2drop then r>
   repeat
   drop 2drop ;

public

: RUN ( -- )
   ROOT!
   0 CASES-RUN !
   0 CASES-BAD !
   CASES$ LISTING LISTING-CAP FS-LIST:NAMES {: u:n :}
   LISTING u EACH-CASE
   CASES-RUN @ 0= if s" no case programs in " CASES$ FAIL then
   CASES-BAD @ 0= if exit then
   SB-RESET
   s" gforth-host: " SB-APPEND
   CASES-BAD @ FMT:SB-INT
   s"  of " SB-APPEND
   CASES-RUN @ FMT:SB-INT
   s"  cases differ" SB-APPEND
   SB$ T-EX-FAIL die ;

;package

GFORTH-HOST-TEST:RUN
