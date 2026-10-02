\ Build the rooted C2 engine, then save XML-C2 and compile a fresh consumer.
\ The printed tree keeps the native images and child logs for replay.
require lib/test.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-cwd.f
require lib/engine-candidate.f

package C2-XML-CONSUMER-E2E
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
   s" c2-xml-consumer-e2e" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   s" src" LINK s" lib" LINK s" tools" LINK
   s" test" AT-A MAKE-DIRS
   s" test/c2-xml-consumer-program.f" COPY-TEST
   s" test/c2-xml-consumer-refusals.f" COPY-TEST
   s" test/c2-xml-consumer-saved.f" COPY-TEST ;

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

: SAVE-LOG ( ptr u8 n ptr u8 n -- )
   {: outpath:ptr outu:n errpath:ptr erru:n :}
   outpath outu AT-A OUT OUT-U @ WRITE-ALL
   errpath erru AT-A ERR ERR-U @ WRITE-ALL ;

: NEED-OK ( -- )
   RC @ 0<> if OUT OUT-U @ type ERR ERR-U @ type then
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
   s" a rooted C2 image builds from the current source" T-LABEL NEED-OK
   RC @ 0<> if exit then
   s" hb-root" AT-A EXECUTABLE? TTRUE
   s" hb-root.names" AT-A {: names:ptr namesu:n :}
   names TARGET namesu BYTE-COPY namesu TARGET-U !
   TARGET TARGET-U @ s" hb-saved.names" AT-A COPY-FILE-STREAM ;

: RUN-IMAGE ( ptr u8 n ptr u8 n -- )
   {: image:ptr imageu:n source:ptr sourceu:n :}
   source sourceu ARGS
   image imageu AT-A ROOT$ CHILD-TIMEOUT-MS RUN-ON ;

: RUN-INPUT ( ptr u8 n ptr u8 n -- )
   {: image:ptr imageu:n input:ptr inputu:n :}
   image imageu AT-A >LEN ROOT$ >LEN input inputu >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN CHILD-TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE CAPTURE-RESULT ;

: SOURCE-CASES ( -- )
   s" hb-root" s" test/c2-xml-consumer-program.f" RUN-IMAGE
   s" source.out" s" source.err" SAVE-LOG
   s" source-loaded XML-C2 borrows names across cursor cleanup" T-LABEL
   s" c2-xml-consumer-program: ok" EXPECT-OK
   s" hb-root" s" test/c2-xml-consumer-refusals.f" RUN-IMAGE
   s" refusals.out" s" refusals.err" SAVE-LOG
   s" checked source rejects cursor and source authority escapes" T-LABEL
   s" c2-xml-consumer-refusals: ok" EXPECT-OK ;

: SAVE-XML ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --" >LEN PROC-ARGV+
   s" hb-saved" AT-A >LEN PROC-ARGV+
   s" hb-root"
   S\" require src/habu/app-image.f\nrequire lib/xml/c2.f\nrequire lib/c2-bytes.f\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n"
   RUN-INPUT
   s" save.out" s" save.err" SAVE-LOG
   s" the typed XML-C2 module saves into a new image" T-LABEL NEED-OK
   RC @ 0<> if exit then
   s" hb-saved" AT-A EXECUTABLE? TTRUE ;

: SAVED-CASES ( -- )
   s" hb-saved" s" test/c2-xml-consumer-saved.f" RUN-IMAGE
   s" saved.out" s" saved.err" SAVE-LOG
   s" a fresh saved-image consumer decodes into separate destinations" T-LABEL
   s" c2-xml-consumer-saved: ok" EXPECT-OK
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" hb-saved"
   S\" 1 set-tier\nrequire test/c2-xml-consumer-saved.f\n"
   RUN-INPUT
   s" saved-tier1.out" s" saved-tier1.err" SAVE-LOG
   s" a fresh saved-image consumer compiles at tier 1" T-LABEL
   s" c2-xml-consumer-saved: ok" EXPECT-OK
   s" hb-saved" s" test/c2-xml-consumer-refusals.f" RUN-IMAGE
   s" saved-refusals.out" s" saved-refusals.err" SAVE-LOG
   s" saved-image authority checks remain closed" T-LABEL
   s" c2-xml-consumer-refusals: ok" EXPECT-OK ;

public
: RUN ( -- )
   T-RESET
   SETUP BUILD
   RC @ 0= if SOURCE-CASES SAVE-XML then
   RC @ 0= if SAVED-CASES then
   s" c2-xml-consumer-e2e tree: " type ROOT$ type cr
   T-REPORT ;
;package

C2-XML-CONSUMER-E2E:RUN
