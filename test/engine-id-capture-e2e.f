\ A copied engine's physical pathname must not travel into an application image.
\ The child loads app-image through the normal source loader before its first
\ explicit ENGINE-ID request. Keep the saved image and logs as evidence.
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/fmt.f
require lib/process-cwd.f
require lib/engine-candidate.f

package ENGINE-ID-CAPTURE-E2E
private

$4000 constant IO-CAP
180000 constant TIMEOUT-MS
create ROOT FS-PATH-CAP allot variable ROOT-U
create TREE FS-PATH-CAP allot variable TREE-U
create ENGINE FS-PATH-CAP allot variable ENGINE-U
create SAVED FS-PATH-CAP allot variable SAVED-U
create LOG FS-PATH-CAP allot
create OUT IO-CAP allot variable OUT-U
create ERR IO-CAP allot variable ERR-U
variable RC
variable PATH-OFF

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: TREE$ ( -- ptr u8 n ) TREE TREE-U @ ;
: ENGINE$ ( -- ptr u8 n ) ENGINE ENGINE-U @ ;
: SAVED$ ( -- ptr u8 n ) SAVED SAVED-U @ ;

: SETUP ( -- )
   s" hb-engine-id-capture" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   ROOT$ s" tree" TREE JOIN-PATH TREE-U !
   TREE$ s" bin" LOG JOIN-PATH {: binu:n :}
   LOG binu MAKE-DIRS
   TREE$ s" bin/hb" ENGINE JOIN-PATH ENGINE-U !
   ROOT$ s" saved" SAVED JOIN-PATH SAVED-U !
   ENGINE-CANDIDATE:PATH$ ENGINE$ COPY-FILE-STREAM
   ENGINE$ CHMOD-X ;

: RESULT ( result<pcap:captured,pcap:failed> -- )
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

: CHECK-RESULT ( -- )
   RC @ 0<> if OUT OUT-U @ type ERR ERR-U @ type then
   RC @ 0 T= ERR-U @ 0 T= ;

: PROBE-NUMBER ( n -- n n )
   {: start:n :}
   OUT OUT-U @ 10 start SPLIT-NEXT {: a:ptr u:n next:n found:bool :}
   found 0= if E-FS-IO throw then
   a u TRIM STR>NUMBER? MATCH option
      none OF E-FS-IO throw ENDOF
      some OF next swap ENDOF
   ;MATCH ;

: PROBE-OFFSET ( -- )
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   ENGINE$ >LEN SOURCE-ROOT:CWD$ >LEN
   \ Compare the live PATH$ length with the cell adjacent to its buffer.
   S\" ENGINE-ID:PATH$ drop data-base - .\nENGINE-ID:PATH$ nip .\nENGINE-ID:PATH$ drop PATH-CAP 1+ 1 cells 1- + 1 cells negate and + @ .\n" >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE RESULT
   s" probe.out" s" probe.err" LOG-RESULT
   CHECK-RESULT
   0 PROBE-NUMBER {: next:n off:n :}
   off PATH-OFF !
   next PROBE-NUMBER {: next2:n public-u:n :}
   next2 PROBE-NUMBER nip {: stored-u:n :}
   public-u 0 > TTRUE
   stored-u public-u T= ;

: SAVE-IMAGE ( -- )
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   s" --" >LEN PROC-ARGV+ SAVED$ >LEN PROC-ARGV+
   ENGINE$ >LEN SOURCE-ROOT:CWD$ >LEN
   S\" require src/habu/app-image.f\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE RESULT
   s" save.out" s" save.err" LOG-RESULT
   CHECK-RESULT
   SAVED$ EXECUTABLE? TTRUE ;

\ PATH-OFF comes from PATH$ on an isolated copy of the same engine. A saved
\ process reads that DATA span before requesting its own engine identity.
: CAPTURE-SOURCE$ ( -- ptr u8 n )
   SB-RESET s" data-base " SB-APPEND
   PATH-OFF @ FMT:SB-U s"  + " SB-APPEND
   S\" PATH-CAP 1+ type cr\n" SB-APPEND
   \ EID-PATH-U is the cell-aligned variable immediately after the buffer.
   S\" s\q length=\q type data-base " SB-APPEND
   PATH-OFF @ FMT:SB-U
   s"  + PATH-CAP 1+ 1 cells 1- + 1 cells negate and + @ . cr" SB-APPEND
   SB$ ;

: CAPTURE-PATH-ZERO? ( -- bool )
   true PATH-CAP 1+ 0 ?do OUT i + c@ 0= and loop ;

: CHECK-CAPTURE-DATA ( -- )
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   SAVED$ >LEN SOURCE-ROOT:CWD$ >LEN
   CAPTURE-SOURCE$ >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE RESULT
   s" capture.out" s" capture.err" LOG-RESULT
   CHECK-RESULT
   OUT-U @ PATH-CAP 1+ >= TTRUE
   CAPTURE-PATH-ZERO? TTRUE
   OUT OUT-U @ ENGINE$ CONTAINS? TFALSE
   OUT OUT-U @ s" length=0" CONTAINS? TTRUE ;

: RUN-SAVED ( -- )
   PROC-ARGV-ENV-RESET PROC-ENV-INHERIT-MISSING
   SAVED$ >LEN SOURCE-ROOT:CWD$ >LEN
   S\" ENGINE-ID:PATH$ type cr\n" >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE RESULT
   s" reload.out" s" reload.err" LOG-RESULT
   CHECK-RESULT
   OUT OUT-U @ SAVED$ CONTAINS? TTRUE ;

public

: RUN ( -- )
   T-RESET SETUP
   PROBE-OFFSET SAVE-IMAGE CHECK-CAPTURE-DATA RUN-SAVED
   s" engine-id capture tree: " type ROOT$ type cr
   T-REPORT ;

;package

ENGINE-ID-CAPTURE-E2E:RUN
