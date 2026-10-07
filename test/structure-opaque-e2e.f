\ End-to-end OPAQUE structure clause on the engine under test: a consumer loads
\ the fixture and is refused its generated pair, tools/check.f replays the
\ fixture and certifies OP's own calls, and refuses a consumer that reaches the
\ pair through OP's public wordlist. The printed tree keeps every source the
\ children ran and every child log for replay.
require lib/test.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-cwd.f
require lib/engine-candidate.f
require test/suite-budget.f              \ CHILD-MS, every child's hang guard

package STRUCTURE-OPAQUE-E2E
private
$8000 constant IO-CAP

create ROOT FS-PATH-CAP allot variable ROOT-U
create PATH FS-PATH-CAP allot
create TARGET FS-PATH-CAP allot variable TARGET-U
create OUT IO-CAP allot variable OUT-U
create ERR IO-CAP allot variable ERR-U
variable RC

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;

: AT-A ( ptr u8 n -- ptr u8 n ) {: rel:ptr relu:n :}
   ROOT$ rel relu PATH JOIN-PATH PATH swap ;

: LINK ( ptr u8 n -- ) {: rel:ptr relu:n :}
   SOURCE-ROOT:CWD$ rel relu TARGET JOIN-PATH TARGET-U !
   TARGET TARGET-U @ rel relu AT-A MAKE-SYMLINK ;

: COPY-TEST ( ptr u8 n -- ) {: rel:ptr relu:n :}
   SOURCE-ROOT:CWD$ rel relu TARGET JOIN-PATH TARGET-U !
   TARGET TARGET-U @ rel relu AT-A COPY-FILE-STREAM ;

\ OP's public wordlist would answer OP:BOX-MAKE if the replay recorded the pair
\ in the section the declaration was read in rather than in OP's private one.
: FORGE$ ( -- ptr u8 n )
   S\" require test/structure-opaque-fixture.f\npackage OPQF\npublic\n: ROUND ( n -- n ) OP:WRAP OP:PEEK ;\n: FORGE ( n -- n ) OP:BOX-MAKE OP:BOX-UNMAKE ;\n;package\n" ;

: SETUP ( -- )
   s" structure-opaque-e2e" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   s" src" LINK s" lib" LINK s" tools" LINK
   s" test" AT-A MAKE-DIRS
   s" test/structure-opaque-fixture.f" COPY-TEST
   s" test/structure-opaque-program.f" COPY-TEST
   s" test/structure-opaque-forge.f" AT-A FORGE$ WRITE-ALL ;

: CAPTURE-RESULT ( result<pcap:captured,pcap:failed> -- )
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! 0 RC ! ENDOF
      err OF PCAP-FAILED:UNMAKE {: outu:len erru:len code:rc :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! code RC>N RC ! ENDOF
   ;MATCH ;

: LOAD-ARGS ( ptr u8 n -- ) {: source:ptr size:n :}
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --load" >LEN PROC-ARGV+
   source size >LEN PROC-ARGV+ ;

: VERIFY-ARGS ( ptr u8 n -- ) {: source:ptr size:n :}
   s" tools/check.f" LOAD-ARGS
   s" --" >LEN PROC-ARGV+
   s" --verify-only" >LEN PROC-ARGV+
   source size >LEN PROC-ARGV+ ;

: RUN-HERE ( -- )
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ >LEN ROOT$ >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN SUITE-BUDGET:CHILD-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE CAPTURE-RESULT ;

: SAVE-LOG ( ptr u8 n ptr u8 n -- )
   {: outpath:ptr outu:n errpath:ptr erru:n :}
   outpath outu AT-A OUT OUT-U @ WRITE-ALL
   errpath erru AT-A ERR ERR-U @ WRITE-ALL ;

: NEED-OK ( -- )
   RC @ 0<> if OUT OUT-U @ type ERR ERR-U @ type then
   RC @ 0 T= ;

: ERR-HAS ( ptr u8 n -- ) {: a:ptr u:n :}
   ERR ERR-U @ a u CONTAINS? TTRUE ;

: PROGRAM ( -- )
   s" test/structure-opaque-program.f" LOAD-ARGS RUN-HERE
   s" program.out" s" program.err" SAVE-LOG
   s" a consumer names OP:box and is refused the generated pair" T-LABEL
   NEED-OK
   OUT OUT-U @ s" structure-opaque-program: ok" CONTAINS? TTRUE ;

: VERIFY ( -- )
   s" test/structure-opaque-fixture.f" VERIFY-ARGS RUN-HERE
   s" verify.out" s" verify.err" SAVE-LOG
   s" tools/check.f replays the declaration and certifies OP's own calls" T-LABEL
   NEED-OK ;

\ The check stops at the first refusal, so ROUND, OP's restored public section,
\ certified before FORGE was refused.
: FORGE ( -- )
   s" test/structure-opaque-forge.f" VERIFY-ARGS RUN-HERE
   s" forge.out" s" forge.err" SAVE-LOG
   s" tools/check.f refuses the replayed pair through OP's public wordlist" T-LABEL
   RC @ 70 T=
   S\" \"code\":\"E-UNDEFINED\"" ERR-HAS
   S\" \"word\":\"forge\"" ERR-HAS
   S\" \"token\":\"OP:BOX-MAKE\"" ERR-HAS ;

public
: RUN ( -- )
   T-RESET
   SETUP PROGRAM VERIFY FORGE
   s" structure-opaque-e2e tree: " type ROOT$ type cr
   T-REPORT ;
;package

STRUCTURE-OPAQUE-E2E:RUN
