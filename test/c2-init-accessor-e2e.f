\ Save generated initialized-field effects, then compile fresh consumers from
\ the image at both tiers. Logs and the image remain in the printed directory.
require lib/test.f
require lib/fs-mutate.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-cwd.f
require lib/engine-candidate.f

package C2-INIT-ACCESSOR-E2E
private

$8000 constant CAP
120000 constant TIMEOUT-MS
create ROOT FS-PATH-CAP allot  variable ROOT-U
create PATH FS-PATH-CAP allot  variable PATH-U
create IMAGE FS-PATH-CAP allot variable IMAGE-U
create OUT CAP allot variable OUT-U
create ERR CAP allot variable ERR-U
variable RC

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;

: PATH! ( ptr u8 n -- ) {: rel:ptr size:n :}
   ROOT$ rel size PATH JOIN-PATH PATH-U ! ;

: SETUP ( -- )
   s" c2-init-accessor-e2e" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   ROOT$ s" hb-saved" IMAGE JOIN-PATH IMAGE-U ! ;

: RESULT ( result<pcap:captured,pcap:failed> -- )
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! 0 RC ! ENDOF
      err OF PCAP-FAILED:UNMAKE {: outu:len erru:len code:rc :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! code RC>N RC ! ENDOF
   ;MATCH ;

: SAVE-LOG ( ptr u8 n ptr u8 n -- )
   {: outpath:ptr outu:n errpath:ptr erru:n :}
   outpath outu PATH! PATH PATH-U @ OUT OUT-U @ WRITE-ALL
   errpath erru PATH! PATH PATH-U @ ERR ERR-U @ WRITE-ALL ;

: CHECK ( ptr u8 n -- ) {: marker:ptr size:n :}
   RC @ 0<> if OUT OUT-U @ type ERR ERR-U @ type then
   RC @ 0 T=
   OUT OUT-U @ marker size CONTAINS? TTRUE ;

: BUILD ( -- )
   PROC-ARGV-ENV-RESET
   s" HABU_C2_ACCESSOR_IMAGE" >LEN IMAGE$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ >LEN
   S\" require src/habu/app-image.f\nrequire test/c2-init-accessors.f\ns\q HABU_C2_ACCESSOR_IMAGE\q GETENV APP-IMAGE:SAVE\n" >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE RESULT
   s" build.out" s" build.err" SAVE-LOG
   s" the generated declaration saves into an application image" T-LABEL
   RC @ 0 T=
   RC @ 0= if IMAGE$ EXECUTABLE? TTRUE then ;

: SOURCE ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/c2-init-accessor-saved.f" >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   IMAGE$ >LEN SOURCE-ROOT:CWD$ >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE RESULT
   s" source.out" s" source.err" SAVE-LOG
   s" the saved public effects type-check in a fresh process" T-LABEL
   s" c2-init-accessor-saved: ok" CHECK ;

: NATIVE ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   PROC-ENV-INHERIT-MISSING
   IMAGE$ >LEN SOURCE-ROOT:CWD$ >LEN
   S\" 1 set-tier\nrequire test/c2-init-accessor-saved.f\n" >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE RESULT
   s" native.out" s" native.err" SAVE-LOG
   s" the saved public effects type-check at tier one" T-LABEL
   s" c2-init-accessor-saved: ok" CHECK ;

public

: RUN ( -- )
   T-RESET
   SETUP BUILD
   RC @ 0= if SOURCE NATIVE then
   s" c2-init-accessor-e2e tree: " type ROOT$ type cr
   T-REPORT ;

;package

C2-INIT-ACCESSOR-E2E:RUN
