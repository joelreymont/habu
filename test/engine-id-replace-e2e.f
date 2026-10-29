\ End-to-end engine image identity across executable pathname replacement.
\ The temporary tree, executable copies and child output are retained as evidence.
\ Run: bin/hb --load test/engine-id-replace-e2e.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-argv.f
require src/lib/engine-candidate.f
require test/pcap-result.f

package ENGINE-ID-REPLACE-E2E

create ROOT PATH-CAP allot variable ROOT-U
create IMAGE PATH-CAP allot variable IMAGE-U
create REPLACEMENT PATH-CAP allot variable REPLACEMENT-U
create OLD PATH-CAP allot variable OLD-U
create LOG PATH-CAP allot
create OUT $4000 allot variable OUT-U
create ERR $4000 allot variable ERR-U
variable RC

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;
: REPLACEMENT$ ( -- ptr u8 n ) REPLACEMENT REPLACEMENT-U @ ;
: OLD$ ( -- ptr u8 n ) OLD OLD-U @ ;

: SETUP ( -- )
   s" hb-engine-id-replace" FS:MKTEMP-DIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   ROOT$ s" engine" IMAGE FS:JOIN-PATH IMAGE-U !
   ROOT$ s" replacement" REPLACEMENT FS:JOIN-PATH REPLACEMENT-U !
   ROOT$ s" old-engine" OLD FS:JOIN-PATH OLD-U !
   ENGINE-CANDIDATE:PATH$ IMAGE$ FS:COPY-FILE-STREAM
   IMAGE$ FS:CHMOD-X
   REPLACEMENT$ s" replacement image, not the running executable" FS:WRITE-ALL ;

: LOG-RESULT ( ptr u8 n ptr u8 n -- ) {: outname:ptr outu:n errname:ptr erru:n :}
   ROOT$ outname outu LOG FS:JOIN-PATH {: size:n :}
   LOG size OUT OUT-U @ FS:WRITE-ALL
   ROOT$ errname erru LOG FS:JOIN-PATH {: esize:n :}
   LOG esize ERR ERR-U @ FS:WRITE-ALL ;

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
   RUN-ARGV-CAPTURE PCAP-RESULT:>N RC ! ERR-U ! OUT-U !
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
   OLD$ FS:FILE? TTRUE
   IMAGE$ FS:FILE? TTRUE
   T-REPORT
   s" engine-id replacement artifact: " type ROOT$ type cr ;

;package

ENGINE-ID-REPLACE-E2E:RUN
