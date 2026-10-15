\ End-to-end ZIP:OPEN refusal under an unusable TMPDIR. A child engine runs
\ test/zip-temp-source.f with TMPDIR naming a regular file, so the source
\ snapshot cannot be created and OPEN throws ZIP:E-OPEN with no memory fallback.
\ The child reads its archive path from argv, so it runs spawned and stays
\ unregistered. The temporary tree is retained as evidence.
\ Run: bin/hb --load test/zip-temp-e2e.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f

package ZIP-TEMP-E2E

$4000 constant CAP
create ROOT FS-PATH-CAP allot variable ROOT-U
create BAD FS-PATH-CAP allot variable BAD-U
create ARCHIVE FS-PATH-CAP allot variable ARCHIVE-U
create OUT CAP allot variable OUT-U
create ERR CAP allot variable ERR-U
variable RC

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: BAD$ ( -- ptr u8 n ) BAD BAD-U @ ;
: ARCHIVE$ ( -- ptr u8 n ) ARCHIVE ARCHIVE-U @ ;

\ BAD is a regular file, so a TMPDIR naming it fails mkstemp with ENOTDIR.
: SETUP ( -- )
   s" zip-temp" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   ROOT$ s" not-a-dir" BAD JOIN-PATH BAD-U !
   ROOT$ s" temp.zip" ARCHIVE JOIN-PATH ARCHIVE-U !
   BAD$ s" x" WRITE-ALL ;

: CAPTURE ( result<pcap:captured,pcap:failed> -- )
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! 0 RC ! ENDOF
      err OF PCAP-FAILED:UNMAKE {: outu:len erru:len code:rc :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! code RC>N RC ! ENDOF
   ;MATCH ;

\ TMPDIR is staged before the parent's rows are inherited, and inheriting skips
\ a name already staged, so the child sees exactly one TMPDIR: BAD.
: RUN-CHILD ( -- )
   PROC-ARGV-ENV-RESET
   s" TMPDIR" >LEN BAD$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --load" >LEN PROC-ARGV+
   s" test/zip-temp-source.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   ARCHIVE$ >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN OUT CAP >LEN ERR CAP >LEN 60000 >MS
   RUN-ARGV-ENV-CAPTURE CAPTURE
   RC @ 0<> if OUT OUT-U @ type ERR ERR-U @ type then
   RC @ 0 T=
   OUT OUT-U @ s" test: ok" CONTAINS? TTRUE ;

public

: RUN ( -- )
   T-RESET
   SETUP
   RUN-CHILD
   T-REPORT
   s" zip-temp artifact: " type ROOT$ type cr ;

;package

ZIP-TEMP-E2E:RUN
