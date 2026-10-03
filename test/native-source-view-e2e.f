\ Source-owned native load acceptance. Run on a qualified engine:
\ bin/hb --load test/native-source-view-e2e.f
\ The private tree and child output remain under the printed directory.

require lib/test.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/fs-list.f
require lib/process-cwd.f
require lib/engine-candidate.f

package NATIVE-SOURCE-VIEW-TEST

$4000 constant OUT-CAP
600000 constant CHILD-TIMEOUT-MS

create ROOT FS-PATH-CAP allot  variable ROOT-U
create PATH FS-PATH-CAP allot  variable PATH-U
create TARGET FS-PATH-CAP allot  variable TARGET-U
create REL FS-PATH-CAP allot     variable REL-U
create OUT OUT-CAP allot       variable OUT-U
create ERR OUT-CAP allot       variable ERR-U
variable RC

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;

: ROOT-PATH! ( ptr u8 n -- ) {: a:ptr u:n :}
   ROOT$ a u PATH JOIN-PATH PATH-U ! ;

: PUT ( ptr u8 n ptr u8 n -- ) {: rel:ptr relu:n body:ptr bodyu:n :}
   rel relu ROOT-PATH!
   PATH PATH-U @ body bodyu WRITE-ALL ;

: LINK ( ptr u8 n -- ) {: rel:ptr relu:n :}
   SOURCE-ROOT:CWD$ rel relu TARGET JOIN-PATH TARGET-U !
   rel relu ROOT-PATH!
   TARGET TARGET-U @ PATH PATH-U @ MAKE-SYMLINK ;

: SETUP ( -- )
   s" native-source-view-e2e" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   s" nest" ROOT-PATH! PATH PATH-U @ MAKE-DIRS
   s" src" LINK s" lib" LINK s" tools" LINK s" test" LINK
   s" nest/entry.f"
      S\" s\" nested.f\" included\ns\" nested.f\" included\npackage SVTEST\npublic\n: VALUE ( -- n ) SVDEP:VALUE 1 + ;\n;package\n"
      PUT
   s" nest/nested.f" S\" s\" dep.f\" required\n" PUT
   s" dep.f" S\" package SVDEP\npublic\n: VALUE ( -- n ) 41 ;\n;package\n" PUT
   s" missing.f" S\" s\" absent-child.f\" required\n" PUT ;

: CAPTURE-RESULT ( result<pcap:captured,pcap:failed> -- )
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! 0 RC ! ENDOF
      err OF PCAP-FAILED:UNMAKE {: outu:len erru:len code:rc :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! code RC>N RC ! ENDOF
   ;MATCH ;

\ The child's MISSING case collects missing.f, whose required absent-child.f
\ does not exist. The refusal keeps the read's code, and stderr names the file,
\ which the code alone does not; nothing else reaches stderr.
: MISSING-LINE$ ( -- ptr u8 n )
   ROOT$ SOURCE-ROOT:CANON-OS drop s" absent-child.f" PATH JOIN-PATH PATH-U !
   SB-RESET
   s" cannot read " SB-APPEND
   PATH PATH-U @ SB-APPEND
   S\" \n" SB-APPEND
   SB$ ;

: RUN-CHILD ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/native-source-view-child.f" >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ >LEN ROOT$ >LEN
   OUT OUT-CAP >LEN ERR OUT-CAP >LEN CHILD-TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE CAPTURE-RESULT
   RC @ 0<> if OUT OUT-U @ type ERR ERR-U @ type then
   RC @ 0 T=
   ERR ERR-U @ MISSING-LINE$ T$=
   OUT OUT-U @ s" source-view: ok" CONTAINS? TTRUE
   s" result.txt" ROOT-PATH!
   PATH PATH-U @ OUT OUT-U @ WRITE-ALL ;

: LINK-SRC ( ptr u8 n -- ) {: name:ptr size:n :}
   name size s" core" STR= if exit then
   s" src" name size REL JOIN-PATH REL-U !
   REL REL-U @ LINK ;

: LINK-CORE ( ptr u8 n -- ) {: name:ptr size:n :}
   name size s" include.f" STR= if exit then
   s" src/core" name size REL JOIN-PATH REL-U !
   REL REL-U @ LINK ;

: SETUP-BOOTSTRAP ( -- )
   s" bootstrap" ROOT-PATH!
   PATH ROOT PATH-U @ BYTE-COPY PATH-U @ ROOT-U !
   ROOT$ MAKE-DIRS
   s" lib" LINK s" tools" LINK s" test" LINK
   s" src/core" ROOT-PATH! PATH PATH-U @ MAKE-DIRS
   s" src" [: LINK-SRC ;] FS-LIST:EACH
   s" src/core" [: LINK-CORE ;] FS-LIST:EACH
   SOURCE-ROOT:CWD$ s" src/core/include.f" TARGET JOIN-PATH TARGET-U !
   s" src/core/include.f" ROOT-PATH!
   TARGET TARGET-U @ PATH PATH-U @ COPY-FILE-STREAM
   PATH PATH-U @ S\" \n: BOOTSTRAP-PATH ( -- ptr u8 n ) s\" bootstrap-late.f\" ;\nBOOTSTRAP-PATH included\n" APPEND-FILE ;

: RUN-BOOTSTRAP ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/native-source-bootstrap-child.f" >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ >LEN ROOT$ >LEN
   OUT OUT-CAP >LEN ERR OUT-CAP >LEN CHILD-TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE CAPTURE-RESULT
   RC @ 76 T=
   ERR ERR-U @ s" defer: unset execution vector" CONTAINS? TTRUE
   OUT OUT-U @ s" bootstrap-read" CONTAINS? 0= TTRUE
   s" result.txt" ROOT-PATH!
   PATH PATH-U @ OUT OUT-U @ WRITE-ALL
   s" error.txt" ROOT-PATH!
   PATH PATH-U @ ERR ERR-U @ WRITE-ALL ;

public

: RUN ( -- )
   T-RESET
   s" one source view owns fallback resolution and bytes across both loaders" T-LABEL
   SETUP RUN-CHILD
   SETUP-BOOTSTRAP RUN-BOOTSTRAP
   T-REPORT
   s" source-view tree: " type ROOT$ type cr ;

;package

NATIVE-SOURCE-VIEW-TEST:RUN
