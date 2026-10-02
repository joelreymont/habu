\ Native embedded field loans, fresh source consumers and a saved wrapper at both tiers.
\ The printed directory retains the images, source copies and child logs.
require lib/test.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-cwd.f
require lib/engine-candidate.f

package C2-FIELD-LOAN-E2E
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
   s" c2-field-loan-e2e" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   s" src" LINK s" lib" LINK s" tools" LINK
   s" test" AT-A MAKE-DIRS
   s" test/c2-field-loan-program.f" COPY-TEST
   s" test/c2-field-loan-refusals.f" COPY-TEST
   s" test/c2-field-loan-lifecycle.f" COPY-TEST
   s" test/c2-field-loan-kind.f" COPY-TEST
   s" test/c2-field-loan-ordinary.f" COPY-TEST
   s" test/c2-field-loan-wrapper.f" COPY-TEST
   s" test/c2-field-loan-saved.f" COPY-TEST ;

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

: CHECK ( ptr u8 n -- ) {: marker:ptr markeru:n :}
   RC @ 0<> if OUT OUT-U @ type ERR ERR-U @ type then
   RC @ 0 T=
   OUT OUT-U @ marker markeru CONTAINS? TTRUE ;

: BUILD ( -- )
   s" test/c2-memory-image-build.f" ARGS
   s" --" >LEN PROC-ARGV+
   s" hb-root" AT-A >LEN PROC-ARGV+
   s" hb-ordinary" AT-A >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ SOURCE-ROOT:CWD$ BUILD-TIMEOUT-MS RUN-ON
   s" build.out" s" build.err" SAVE-LOG
   s" native C2 field loan image builds" T-LABEL RC @ 0 T=
   RC @ 0<> if exit then
   s" hb-root" AT-A EXECUTABLE? TTRUE
   s" hb-ordinary" AT-A CHMOD-X
   s" hb-root.names" AT-A {: names:ptr namesu:n :}
   names TARGET namesu BYTE-COPY namesu TARGET-U !
   TARGET TARGET-U @ s" hb-ordinary.names" AT-A COPY-FILE-STREAM ;

: RUN-IMAGE ( ptr u8 n ptr u8 n -- )
   {: image:ptr imageu:n source:ptr sourceu:n :}
   source sourceu ARGS
   image imageu AT-A ROOT$ CHILD-TIMEOUT-MS RUN-ON ;

: RUN-INPUT ( ptr u8 n ptr u8 n -- )
   {: image:ptr imageu:n input:ptr inputu:n :}
   PROC-ENV-INHERIT-MISSING
   image imageu AT-A >LEN ROOT$ >LEN input inputu >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN CHILD-TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE CAPTURE-RESULT ;

: SOURCE-CASES ( -- )
   s" hb-root" s" test/c2-field-loan-kind.f" RUN-IMAGE
   s" kind.out" s" kind.err" SAVE-LOG
   s" the exact field entry has scope kind eight" T-LABEL
   s" c2-field-loan-kind: ok" CHECK
   s" hb-ordinary" s" test/c2-field-loan-ordinary.f" RUN-IMAGE
   s" ordinary.out" s" ordinary.err" SAVE-LOG
   s" the ordinary image carries no field authority" T-LABEL
   s" c2-field-loan-ordinary: ok" CHECK
   s" hb-root" s" test/c2-field-loan-program.f" RUN-IMAGE
   s" source.out" s" source.err" SAVE-LOG
   s" a fresh source program edits an embedded field" T-LABEL
   s" c2-field-loan-program: ok" CHECK
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" hb-root"
   S\" 1 set-tier\nrequire test/c2-field-loan-program.f\ns\q c2-field-loan-tier1: ok\q type cr\n"
   RUN-INPUT
   s" tier1.out" s" tier1.err" SAVE-LOG
   s" fresh tier one code edits an embedded field" T-LABEL
   s" c2-field-loan-tier1: ok" CHECK
   s" hb-root" s" test/c2-field-loan-refusals.f" RUN-IMAGE
   s" refusals.out" s" refusals.err" SAVE-LOG
   s" field-loan authority refusals hold" T-LABEL
   s" c2-field-loan-refusals: ok" CHECK
   s" hb-root" s" test/c2-field-loan-lifecycle.f" RUN-IMAGE
   s" lifecycle.out" s" lifecycle.err" SAVE-LOG
   s" throw and task halt close the complete field-loan scope" T-LABEL
   s" c2-field-loan-lifecycle: ok" CHECK ;

: SAVED-CASES ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --" >LEN PROC-ARGV+
   s" hb-saved" AT-A >LEN PROC-ARGV+
   s" hb-root"
   S\" require src/habu/app-image.f\nrequire test/c2-field-loan-wrapper.f\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n"
   RUN-INPUT
   s" save.out" s" save.err" SAVE-LOG
   s" a field-loan wrapper saves with its complete scheme" T-LABEL
   RC @ 0 T=
   RC @ 0<> if exit then
   s" hb-saved" AT-A EXECUTABLE? TTRUE
   s" hb-saved" s" test/c2-field-loan-saved.f" RUN-IMAGE
   s" saved-source.out" s" saved-source.err" SAVE-LOG
   s" a fresh saved-image source consumer opens a field loan" T-LABEL
   s" c2-field-loan-saved: ok" CHECK
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" hb-saved"
   S\" 1 set-tier\nrequire test/c2-field-loan-saved.f\ns\q c2-field-loan-saved-tier1: ok\q type cr\n"
   RUN-INPUT
   s" saved-tier1.out" s" saved-tier1.err" SAVE-LOG
   s" a fresh saved-image tier one consumer opens a field loan" T-LABEL
   s" c2-field-loan-saved-tier1: ok" CHECK ;

public

: RUN ( -- )
   T-RESET
   SETUP BUILD
   RC @ 0= if SOURCE-CASES SAVED-CASES then
   s" c2-field-loan-e2e tree: " type ROOT$ type cr
   T-REPORT ;

;package

C2-FIELD-LOAN-E2E:RUN
