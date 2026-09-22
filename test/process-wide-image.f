\ Process-wide facilities must not carry descriptors, signal dispositions, or
\ environment mappings through an application image.  Build an image after
\ both facilities have allocated their state, then run it in a fresh process
\ and prove that each facility starts from its cold state.
require lib/test.f
require lib/fs-mutate.f
require lib/process-cwd.f
require lib/engine-candidate.f
require lib/fmt.f

package PROCESS-WIDE-IMAGE-TEST

$10000 constant CAP
300000 constant TIMEOUT-MS
create OUT CAP allot
create ERR CAP allot
create ROOT-BUF FS-PATH-CAP allot
create IMAGE-BUF FS-PATH-CAP allot
variable ROOT-U
variable IMAGE-U
variable WANT-ENV-N

: ROOT$ ( -- ptr u8 n ) ROOT-BUF ROOT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE-BUF IMAGE-U @ ;

: PREPARE ( -- )
   CLEANUP-RESET
   s" process-wide-image" TMPDIR-MKDIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY u ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT$ s" restored" IMAGE-BUF JOIN-PATH IMAGE-U ! ;

: RESULT ( result<pcap:captured,pcap:failed> -- n n n )
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         outu LEN>N erru LEN>N 0 ENDOF
      err OF PCAP-FAILED:UNMAKE {: outu:len erru:len rc:rc :}
         outu LEN>N erru LEN>N rc RC>N ENDOF
   ;MATCH ;

: CLEAN ( n n n -- n ) {: outu:n erru:n rc:n :}
   rc 0<> erru 0<> or if OUT outu type ERR erru type then
   rc 0 T= erru 0 T= outu ;

: RUN-INPUT ( ptr u8 n ptr u8 n -- n n n )
   {: path:ptr pathu:n input:ptr inputu:n :}
   path pathu >LEN ROOT$ >LEN input inputu >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE RESULT ;

: BUILD ( -- )
   PROC-ARGV-ENV-RESET
   s" --" >LEN PROC-ARGV+ IMAGE$ >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ >LEN
   S" require src/habu/app-image.f
require lib/signal.f
require lib/process-env.f
SIGNAL:INIT
PROC-ENV-INHERIT-MISSING
0 SCRIPT-ARGV$ APP-IMAGE:SAVE
" >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE RESULT CLEAN drop
   IMAGE$ EXECUTABLE? TTRUE ;

: EXPECT$ ( -- ptr u8 n )
   SB-RESET
   s" 3" SB-APPEND $0A SB-APPEND-C $0A SB-APPEND-C
   WANT-ENV-N @ FMT:SB-U $0A SB-APPEND-C $0A SB-APPEND-C
   WANT-ENV-N @ FMT:SB-U $0A SB-APPEND-C $0A SB-APPEND-C
   SB$ ;

: CHECK-RESTORED ( -- )
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" HABU_IMAGE_RESET_EXTRA_A" >LEN s" alpha" >LEN PROC-ENV+
   s" HABU_IMAGE_RESET_EXTRA_B" >LEN s" beta" >LEN PROC-ENV+
   PROC-ENV-N @ WANT-ENV-N !
   IMAGE$
   S" SIGNAL:INIT
PROC-ENV-INHERIT-MISSING
SIGNAL:FD FD>N . cr
PROC-ENV-INHERITED-N @ . cr
PROC-ENVP-COUNT . cr
SIGNAL:RELEASE
" RUN-INPUT CLEAN
   OUT swap EXPECT$ T$= ;

: RUN ( -- )
   T-RESET PREPARE
   BUILD
   CHECK-RESTORED
   CLEANUP-RUN
   T-REPORT ;

RUN
;package
