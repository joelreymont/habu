\ Failure boundaries: bad or duplicate parameter names, empty or unclosed
\ lists, separators, trailing bytes, and an undeclared payload parameter.
\ Run each source through both loading and check.f's replay. Keep sources and
\ logs as a repeatable artifact; the accepted source exercises generated words.
require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/process-cwd.f
require src/lib/engine-candidate.f
require test/suite-budget.f

package DECLARATION-HEADERS-E2E
private
$8000 constant IO-CAP
create ROOT FS-PATH-CAP allot variable ROOT-U
create PATH FS-PATH-CAP allot
create TARGET FS-PATH-CAP allot variable TARGET-U
create SOURCE FS-PATH-CAP allot variable SOURCE-U
create OUT IO-CAP allot variable OUT-U
create ERR IO-CAP allot variable ERR-U
variable RC
: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: AT ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   ROOT$ a u PATH JOIN-PATH PATH swap ;
: LINK ( ptr u8 n -- ) {: a:ptr u:n :}
   SOURCE-ROOT:CWD$ a u TARGET JOIN-PATH TARGET-U !
   TARGET TARGET-U @ a u AT MAKE-SYMLINK ;
: RESULT ( result<pcap:captured,pcap:failed> -- )
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: ou:len eu:len :}
         ou LEN>N OUT-U ! eu LEN>N ERR-U ! 0 RC ! ENDOF
      err OF PCAP-FAILED:UNMAKE {: ou:len eu:len rc:rc :}
         ou LEN>N OUT-U ! eu LEN>N ERR-U ! rc RC>N RC ! ENDOF
   ;MATCH ;
: ARGS ( ptr u8 n bool -- ) {: a:ptr u:n verify:bool :}
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --load" >LEN PROC-ARGV+
   verify if
      s" tools/check.f" >LEN PROC-ARGV+
      s" --" >LEN PROC-ARGV+
      s" --verify-only" >LEN PROC-ARGV+
   then
   a u >LEN PROC-ARGV+ ;
: RUN-HERE ( -- )
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ >LEN ROOT$ >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN SUITE-BUDGET:CHILD-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE RESULT ;
: SAVE ( ptr u8 n -- ) {: stem:ptr u:n :}
   stem u AT MAKE-DIRS
   stem u s" out" TARGET JOIN-PATH TARGET swap AT OUT OUT-U @ WRITE-ALL
   stem u s" err" TARGET JOIN-PATH TARGET swap AT ERR ERR-U @ WRITE-ALL ;
: NEED-OK ( -- )
   RC @ 0<> if OUT OUT-U @ type ERR ERR-U @ type then
   RC @ 0 T= ;
: ACCEPT ( -- )
   s" test/declaration-headers-program.f" false ARGS RUN-HERE
   s" load" SAVE NEED-OK
   OUT OUT-U @ s" declaration headers: ok" CONTAINS? TTRUE
   s" test/declaration-headers-program.f" true ARGS RUN-HERE
   s" verify" SAVE NEED-OK ;
: REJECT ( ptr u8 n ptr u8 n -- )
   {: stem:ptr su:n a:ptr u:n :}
   stem su AT MAKE-DIRS
   stem su s" source.f" SOURCE JOIN-PATH SOURCE-U !
   SOURCE SOURCE-U @ AT a u WRITE-ALL
   SOURCE SOURCE-U @ false ARGS RUN-HERE
   stem su s" load" TARGET JOIN-PATH TARGET swap SAVE
   RC @ 70 T=
   SOURCE SOURCE-U @ true ARGS RUN-HERE
   stem su s" verify" TARGET JOIN-PATH TARGET swap SAVE
   RC @ 70 T= ;
: REJECTS ( -- )
   s" empty" s" NEWTYPE bad<>" REJECT
   s" duplicate" s" NEWTYPE bad<a,a>" REJECT
   s" reserved" s" NEWTYPE bad<n>" REJECT
   s" long-param" s" NEWTYPE bad<item>" REJECT
   s" unclosed" s" NEWTYPE bad<a" REJECT
   s" separator" s" NEWTYPE bad<a,>" REJECT
   s" trailing" s" NEWTYPE bad<a>b" REJECT
   s" undeclared" s" SUMTYPE bad<a> VARIANT item b ;VARIANT ;SUMTYPE" REJECT
   s" sum" s" SUMTYPE bad<a,a> VARIANT item ;VARIANT ;SUMTYPE" REJECT
   s" product" s" PRODUCT bad<a,a> FIELD value a ;PRODUCT" REJECT
   s" structure" s" STRUCTURE bad<a,a> FIELD value a ;STRUCTURE" REJECT
   s" enum" s" ENUM bad<a,a> VARIANT item ;VARIANT ;ENUM" REJECT ;
: SETUP ( -- )
   s" declaration-headers-e2e" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   s" src" LINK s" tools" LINK s" test" LINK ;
public
: RUN ( -- )
   T-RESET SETUP ACCEPT REJECTS
   s" declaration headers tree: " type ROOT$ type cr
   T-REPORT ;
;package
DECLARATION-HEADERS-E2E:RUN
