\ Build a C2 image, run borrowed records, capture the declarations, then
\ use their constructors from a fresh saved image in a relocated source tree.
require lib/test.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-cwd.f
require lib/engine-candidate.f

package C2-VIEW-RECORD-E2E
private

$8000 constant IO-CAP
600000 constant BUILD-TIMEOUT-MS
30000 constant CHILD-TIMEOUT-MS

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

: SETUP ( -- )
   s" c2-view-record-e2e" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   s" src" LINK s" lib" LINK s" tools" LINK
   s" test" AT-A MAKE-DIRS
   s" test/c2-view-record-defs.f" COPY-TEST
   s" test/c2-view-record-source.f" COPY-TEST
   s" test/c2-view-record-refusals.f" COPY-TEST
   s" test/c2-view-record-use-refusals.f" COPY-TEST
   s" test/c2-retained-contract-wide.f" COPY-TEST
   s" test/c2-view-record-saved.f" COPY-TEST ;

: CAPTURE-RESULT ( result<pcap:captured,pcap:failed> -- )
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! 0 RC ! ENDOF
      err OF PCAP-FAILED:UNMAKE {: outu:len erru:len code:rc :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! code RC>N RC ! ENDOF
   ;MATCH ;

: ARGS ( ptr u8 n -- ) {: source:ptr size:n :}
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --load" >LEN PROC-ARGV+
   source size >LEN PROC-ARGV+ ;

: RUN-ON ( ptr u8 n ptr u8 n n -- )
   {: engine:ptr engineu:n cwd:ptr cwdu:n timeout:n :}
   PROC-ENV-INHERIT-MISSING
   engine engineu >LEN cwd cwdu >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN timeout >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE CAPTURE-RESULT ;

: RUN-INPUT ( ptr u8 n ptr u8 n -- )
   {: engine:ptr engineu:n input:ptr inputu:n :}
   PROC-ENV-INHERIT-MISSING
   engine engineu AT-A >LEN ROOT$ >LEN input inputu >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN CHILD-TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE CAPTURE-RESULT ;

: SAVE-LOG ( ptr u8 n ptr u8 n -- )
   {: outpath:ptr outu:n errpath:ptr erru:n :}
   outpath outu AT-A OUT OUT-U @ WRITE-ALL
   errpath erru AT-A ERR ERR-U @ WRITE-ALL ;

: NEED-OK ( -- )
   RC @ 0<> IF OUT OUT-U @ type ERR ERR-U @ type THEN
   RC @ 0 T= ;

: EXPECT-OK ( ptr u8 n -- ) {: marker:ptr markeru:n :}
   NEED-OK
   OUT OUT-U @ marker markeru CONTAINS? TTRUE ;

: BUILD ( -- )
   s" test/c2-memory-image-build.f" ARGS
   s" --" >LEN PROC-ARGV+
   s" hb-root" AT-A >LEN PROC-ARGV+
   s" hb-ordinary" AT-A >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ SOURCE-ROOT:CWD$ BUILD-TIMEOUT-MS RUN-ON
   s" build.out" s" build.err" SAVE-LOG
   s" rooted image builds" T-LABEL NEED-OK
   s" hb-root" AT-A FILE? TTRUE ;

: RUN-IMAGE ( ptr u8 n ptr u8 n -- )
   {: image:ptr imageu:n source:ptr sourceu:n :}
   source sourceu ARGS
   image imageu AT-A ROOT$ CHILD-TIMEOUT-MS RUN-ON ;

: SOURCE-CASES ( -- )
   s" hb-root" s" test/c2-view-record-source.f" RUN-IMAGE
   s" source.out" s" source.err" SAVE-LOG
   s" borrowed records round trip through nested records and a sum" T-LABEL
   s" c2-view-record-source: ok" EXPECT-OK
   s" hb-root" s" test/c2-view-record-refusals.f" RUN-IMAGE
   s" refusals.out" s" refusals.err" SAVE-LOG
   s" borrowed records keep escape and pointer fences" T-LABEL
   s" c2-view-record-refusals: ok" EXPECT-OK ;

: SAVED-CASE ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --" >LEN PROC-ARGV+
   s" hb-saved" AT-A >LEN PROC-ARGV+
   s" hb-root"
   S\" 1 set-tier\nrequire test/c2-view-record-defs.f\nrequire src/habu/app-image.f\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n"
   RUN-INPUT
   s" save.out" s" save.err" SAVE-LOG
   s" borrowed field schemas survive native capture" T-LABEL NEED-OK
   s" hb-saved" AT-A EXECUTABLE? TTRUE
   s" hb-saved" s" test/c2-view-record-saved.f" RUN-IMAGE
   s" saved.out" s" saved.err" SAVE-LOG
   s" a fresh saved image constructs and consumes the records" T-LABEL
   s" c2-view-record-saved: ok" EXPECT-OK ;

public

: RUN ( -- )
   T-RESET
   SETUP BUILD SOURCE-CASES SAVED-CASE
   s" c2-view-record-e2e tree: " type ROOT$ type cr
   T-REPORT ;

;package

C2-VIEW-RECORD-E2E:RUN
