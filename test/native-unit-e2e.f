\ Explicit NBR package artifact acceptance. Run on a qualified native host:
\ bin/hb --load test/native-unit-e2e.f
\ The private tree, artifact, cold/imported engines and names remain printed.

require lib/test.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/fs-list.f
require lib/process-cwd.f
require lib/engine-candidate.f

package NATIVE-UNIT-TEST

$8000 constant OUT-CAP
600000 constant BUILD-TIMEOUT-MS

create ROOT FS-PATH-CAP allot   variable ROOT-U
create PATH FS-PATH-CAP allot
create TARGET FS-PATH-CAP allot variable TARGET-U
create REL FS-PATH-CAP allot    variable REL-U
create OUT OUT-CAP allot       variable OUT-U
create ERR OUT-CAP allot       variable ERR-U
variable RC

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;

: AT-A ( ptr u8 n -- ptr u8 n ) {: rel:ptr relu:n :}
   ROOT$ rel relu PATH JOIN-PATH PATH swap ;

: LINK ( ptr u8 n -- ) {: rel:ptr relu:n :}
   SOURCE-ROOT:CWD$ rel relu TARGET JOIN-PATH TARGET-U !
   TARGET TARGET-U @ rel relu AT-A MAKE-SYMLINK ;

: LINK-SRC ( ptr u8 n -- ) {: name:ptr size:n :}
   name size s" arch" STR= if exit then
   name size s" compiler" STR= if exit then
   s" src" name size REL JOIN-PATH REL-U !
   REL REL-U @ LINK ;

: LINK-COMPILER ( ptr u8 n -- ) {: name:ptr size:n :}
   name size s" native" STR= if exit then
   s" src/compiler" name size REL JOIN-PATH REL-U !
   REL REL-U @ LINK ;

: LINK-NATIVE ( ptr u8 n -- ) {: name:ptr size:n :}
   name size s" branch.f" STR= if exit then
   s" src/compiler/native" name size REL JOIN-PATH REL-U !
   REL REL-U @ LINK ;

: LINK-ARCH ( ptr u8 n -- ) {: name:ptr size:n :}
   name size s" arm64" STR= if exit then
   s" src/arch" name size REL JOIN-PATH REL-U !
   REL REL-U @ LINK ;

: LINK-A64 ( ptr u8 n -- ) {: name:ptr size:n :}
   name size s" passes.f" STR= if exit then
   s" src/arch/arm64" name size REL JOIN-PATH REL-U !
   REL REL-U @ LINK ;

: COPY-PASSES ( -- )
   SOURCE-ROOT:CWD$ s" src/arch/arm64/passes.f" TARGET JOIN-PATH TARGET-U !
   TARGET TARGET-U @ s" src/arch/arm64/passes.f" AT-A COPY-FILE-STREAM ;

: COPY-BRANCH ( -- )
   SOURCE-ROOT:CWD$ s" src/compiler/native/branch.f" TARGET JOIN-PATH TARGET-U !
   TARGET TARGET-U @ s" src/compiler/native/branch.f" AT-A COPY-FILE-STREAM ;

: SETUP ( -- )
   s" native-unit-e2e" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   s" src/arch/arm64" AT-A MAKE-DIRS
   s" src/compiler/native" AT-A MAKE-DIRS
   s" lib" LINK s" tools" LINK s" test" LINK
   s" src" [: LINK-SRC ;] FS-LIST:EACH
   s" src/compiler" [: LINK-COMPILER ;] FS-LIST:EACH
   s" src/compiler/native" [: LINK-NATIVE ;] FS-LIST:EACH
   s" src/arch" [: LINK-ARCH ;] FS-LIST:EACH
   s" src/arch/arm64" [: LINK-A64 ;] FS-LIST:EACH
   COPY-PASSES COPY-BRANCH ;

: CLIENT-OLD ( -- )
   s" src/arch/arm64/passes.f" AT-A
   S\" \npackage A64PASS\npublic\n: UNIT-CLIENT ( -- n ) 4096 4100 NBR:BL-WORD 4096 swap NBR:BL-TARGET ;\n;package\n"
   APPEND-FILE ;

: CLIENT-EDITED ( -- )
   COPY-PASSES
   s" src/arch/arm64/passes.f" AT-A
   S\" \npackage A64PASS\npublic\n: UNIT-CLIENT ( -- n ) 4096 4104 NBR:BL-WORD 4096 swap NBR:BL-TARGET ;\n;package\n"
   APPEND-FILE ;

: CLIENT-INVALID ( -- )
   COPY-PASSES
   s" src/arch/arm64/passes.f" AT-A
   S\" \npackage A64PASS\npublic\n: UNIT-CLIENT ( ptr u8 -- bool ) NBR:BL? ;\n;package\n"
   APPEND-FILE ;

: CAPTURE-RESULT ( result<pcap:captured,pcap:failed> -- )
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! 0 RC ! ENDOF
      err OF PCAP-FAILED:UNMAKE {: outu:len erru:len code:rc :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! code RC>N RC ! ENDOF
   ;MATCH ;

: RUN-CHILD ( -- )
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ >LEN ROOT$ >LEN
   OUT OUT-CAP >LEN ERR OUT-CAP >LEN BUILD-TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE CAPTURE-RESULT ;

: SAVE-LOG ( ptr u8 n ptr u8 n -- )
   {: outpath:ptr outu:n errpath:ptr erru:n :}
   outpath outu AT-A OUT OUT-U @ WRITE-ALL
   errpath erru AT-A ERR ERR-U @ WRITE-ALL ;

: BUILD-ARGS ( ptr u8 n -- ) {: tool:ptr toolu:n :}
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --load" >LEN PROC-ARGV+
   tool toolu >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" HABU_WHITEBOX_IMAGE" >LEN s" 1" >LEN PROC-ENV+ ;

: BUILD-TAIL ( ptr u8 n -- ) {: rel:ptr relu:n :}
   rel relu AT-A >LEN PROC-ARGV+
   s" whitebox" >LEN PROC-ARGV+
   RUN-CHILD ;

: CHECK-OK ( -- )
   RC @ 0<> if OUT OUT-U @ type ERR ERR-U @ type then
   RC @ 0 T= ;

: EXPORT-UNIT ( -- )
   s" tools/native-unit-build.f" BUILD-ARGS
   s" --export-unit" >LEN PROC-ARGV+
   s" NBR" >LEN PROC-ARGV+
   s" nbr.unit" AT-A >LEN PROC-ARGV+
   s" hb-export" BUILD-TAIL
   s" export.out" s" export.err" SAVE-LOG
   CHECK-OK
   s" nbr.unit" AT-A FILE? TTRUE ;

: COLD-BUILD ( -- )
   s" tools/native-build.f" BUILD-ARGS
   s" hb-cold" BUILD-TAIL
   s" cold.out" s" cold.err" SAVE-LOG
   CHECK-OK ;

: IMPORT-UNIT ( ptr u8 n -- ) {: out:ptr outu:n :}
   s" tools/native-unit-build.f" BUILD-ARGS
   s" --import-unit" >LEN PROC-ARGV+
   s" nbr.unit" AT-A >LEN PROC-ARGV+
   out outu BUILD-TAIL ;

: IMPORT-BUILD ( -- )
   s" hb-import" IMPORT-UNIT
   s" import.out" s" import.err" SAVE-LOG
   CHECK-OK
   OUT OUT-U @ s" native-build: NBR unit hit" CONTAINS? TTRUE ;

: COMPARE ( ptr u8 n ptr u8 n -- ) {: left:ptr leftu:n right:ptr rightu:n :}
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --load" >LEN PROC-ARGV+
   s" tools/two-generation-build.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" --compare" >LEN PROC-ARGV+
   left leftu AT-A >LEN PROC-ARGV+
   right rightu AT-A >LEN PROC-ARGV+
   RUN-CHILD CHECK-OK ;

: RUN-CLIENT ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/native-unit-client.f" >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   s" hb-import" AT-A >LEN ROOT$ >LEN
   OUT OUT-CAP >LEN ERR OUT-CAP >LEN BUILD-TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE CAPTURE-RESULT
   s" client.out" s" client.err" SAVE-LOG
   CHECK-OK
   OUT OUT-U @ s" unit-client: 4104" CONTAINS? TTRUE ;

: RUN-C2 ( ptr u8 n ptr u8 n -- ) {: image:ptr imageu:n source:ptr sourceu:n :}
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --load" >LEN PROC-ARGV+
   source sourceu >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   image imageu AT-A >LEN ROOT$ >LEN
   OUT OUT-CAP >LEN ERR OUT-CAP >LEN BUILD-TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE CAPTURE-RESULT
   CHECK-OK ;

: C2-PRODUCTS ( -- )
   s" native unit and cold products both load typed C2" T-LABEL
   s" hb-cold" s" test/c2-init-program.f" RUN-C2
   OUT OUT-U @ s" c2-init-program: ok" CONTAINS? TTRUE
   s" hb-import" s" test/c2-init-program.f" RUN-C2
   OUT OUT-U @ s" c2-init-program: ok" CONTAINS? TTRUE
   s" hb-cold" s" lib/xml/c2.f" RUN-C2
   s" hb-import" s" lib/xml/c2.f" RUN-C2 ;

: INVALID-CLIENT ( -- )
   CLIENT-INVALID
   s" hb-bad" IMPORT-UNIT
   s" invalid.out" s" invalid.err" SAVE-LOG
   RC @ 0<> TTRUE
   ERR ERR-U @ s" expected: n actual: ptr u8" CONTAINS? TTRUE
   s" hb-bad" AT-A EXISTS? 0= TTRUE ;

: CHANGED-UNIT ( -- )
   CLIENT-EDITED
   s" src/compiler/native/branch.f" AT-A
   S\" \npackage NBR public : UNIT-NEW ( -- n ) 9 ; ;package\n" APPEND-FILE
   s" hb-stale" IMPORT-UNIT
   s" stale.out" s" stale.err" SAVE-LOG
   RC @ 0<> TTRUE
   s" hb-stale" AT-A EXISTS? 0= TTRUE ;

: UNDECLARED-EFFECT ( -- )
   COPY-BRANCH
   s" src/compiler/native/branch.f" AT-A
   S\" \n0 set-tier\n" APPEND-FILE
   s" tools/native-unit-build.f" BUILD-ARGS
   s" --export-unit" >LEN PROC-ARGV+
   s" NBR" >LEN PROC-ARGV+
   s" unsupported.unit" AT-A >LEN PROC-ARGV+
   s" hb-unsupported" BUILD-TAIL
   s" unsupported.out" s" unsupported.err" SAVE-LOG
   RC @ 0<> TTRUE
   s" unsupported.unit" AT-A EXISTS? 0= TTRUE ;

public

: RUN ( -- )
   T-RESET
   s" NBR unit import preserves edited client, bytes and fresh checking" T-LABEL
   SETUP CLIENT-OLD EXPORT-UNIT
   CLIENT-EDITED COLD-BUILD IMPORT-BUILD
   s" hb-cold" s" hb-import" COMPARE
   s" hb-cold.names" s" hb-import.names" COMPARE
   RUN-CLIENT INVALID-CLIENT CHANGED-UNIT UNDECLARED-EFFECT
   C2-PRODUCTS
   T-REPORT
   s" native unit tree: " type ROOT$ type cr ;

;package

NATIVE-UNIT-TEST:RUN
