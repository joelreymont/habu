\ End-to-end engine image identity across executable pathname replacement.
\ The temporary tree, executable copies and child output are retained as evidence.
\ Run: bin/hb --load test/engine-id-replace-e2e.f
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process-argv.f
require lib/engine-candidate.f

package ENGINE-ID-REPLACE-E2E

create ROOT FS-PATH-CAP allot variable ROOT-U
create IMAGE FS-PATH-CAP allot variable IMAGE-U
create REPLACEMENT FS-PATH-CAP allot variable REPLACEMENT-U
create OLD FS-PATH-CAP allot variable OLD-U
create LOG FS-PATH-CAP allot
create OUT $4000 allot variable OUT-U
create ERR $4000 allot variable ERR-U
variable RC

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;
: REPLACEMENT$ ( -- ptr u8 n ) REPLACEMENT REPLACEMENT-U @ ;
: OLD$ ( -- ptr u8 n ) OLD OLD-U @ ;

: SETUP ( -- )
   s" hb-engine-id-replace" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   ROOT$ s" engine" IMAGE JOIN-PATH IMAGE-U !
   ROOT$ s" replacement" REPLACEMENT JOIN-PATH REPLACEMENT-U !
   ROOT$ s" old-engine" OLD JOIN-PATH OLD-U !
   ENGINE-CANDIDATE:PATH$ IMAGE$ COPY-FILE-STREAM
   IMAGE$ CHMOD-X
   REPLACEMENT$ s" replacement image, not the running executable" WRITE-ALL ;

: CAPTURE ( result<pcap:captured,pcap:failed> -- )
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! 0 RC ! ENDOF
      err OF PCAP-FAILED:UNMAKE {: outu:len erru:len code:rc :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! code RC>N RC ! ENDOF
   ;MATCH ;

: LOG-RESULT ( ptr u8 n ptr u8 n -- ) {: outname:ptr outu:n errname:ptr erru:n :}
   ROOT$ outname outu LOG JOIN-PATH {: size:n :}
   LOG size OUT OUT-U @ WRITE-ALL
   ROOT$ errname erru LOG JOIN-PATH {: esize:n :}
   LOG esize ERR ERR-U @ WRITE-ALL ;

: RUN-CHILD ( bool -- ) {: replace?:bool :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/engine-id-replace-child.f" >LEN PROC-ARGV+
   replace? if
      s" --" >LEN PROC-ARGV+
      REPLACEMENT$ >LEN PROC-ARGV+
      OLD$ >LEN PROC-ARGV+
   then
   IMAGE$ >LEN OUT $4000 >LEN ERR $4000 >LEN 180000 >MS
   RUN-ARGV-CAPTURE CAPTURE
   replace? if
      s" replace.out" s" replace.err" LOG-RESULT
   else
      s" unchanged.out" s" unchanged.err" LOG-RESULT
   then
   RC @ 0<> if ERR ERR-U @ type then
   RC @ 0 T= ;

public

: RUN ( -- )
   T-RESET
   SETUP
   false RUN-CHILD
   true RUN-CHILD
   OLD$ FILE? TTRUE
   IMAGE$ FILE? TTRUE
   T-REPORT
   s" engine-id replacement artifact: " type ROOT$ type cr ;

;package

ENGINE-ID-REPLACE-E2E:RUN
