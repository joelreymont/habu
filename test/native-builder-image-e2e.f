\ Saved native builder acceptance through fresh processes and real source loads.
\ Run explicitly: bin/hb --load test/native-builder-image-e2e.f
\ The printed private directory retains the source tree, saved builder, four
\ engines and their name maps for byte comparison and repeatable inspection.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process-cwd.f
require lib/engine-candidate.f
require lib/time.f
require tools/chain-run.f

package NATIVE-BUILDER-IMAGE-TEST

$4000 constant CAP
1800000 constant BUILD-TIMEOUT-MS

create ROOT FS-PATH-CAP allot          variable ROOT-U
create TREE FS-PATH-CAP allot          variable TREE-U
create TMP FS-PATH-CAP allot           variable TMP-U
create IMAGE FS-PATH-CAP allot         variable IMAGE-U
create COLD FS-PATH-CAP allot          variable COLD-U
create SAVED FS-PATH-CAP allot         variable SAVED-U
create COLD-WHITE FS-PATH-CAP allot    variable COLD-WHITE-U
create SAVED-WHITE FS-PATH-CAP allot   variable SAVED-WHITE-U
create REJECTED FS-PATH-CAP allot      variable REJECTED-U
create REPL FS-PATH-CAP allot          variable REPL-U
create DEST FS-PATH-CAP allot          variable DEST-U
create NAME-A FS-PATH-CAP allot        variable NAME-A-U
create NAME-B FS-PATH-CAP allot        variable NAME-B-U
create SOURCE $10000 allot
create OUT CAP allot                   variable OUT-U
create ERR CAP allot                   variable ERR-U
variable RC

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: TREE$ ( -- ptr u8 n ) TREE TREE-U @ ;
: TMP$ ( -- ptr u8 n ) TMP TMP-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;
: COLD$ ( -- ptr u8 n ) COLD COLD-U @ ;
: SAVED$ ( -- ptr u8 n ) SAVED SAVED-U @ ;
: COLD-WHITE$ ( -- ptr u8 n ) COLD-WHITE COLD-WHITE-U @ ;
: SAVED-WHITE$ ( -- ptr u8 n ) SAVED-WHITE SAVED-WHITE-U @ ;
: REJECTED$ ( -- ptr u8 n ) REJECTED REJECTED-U @ ;
: REPL$ ( -- ptr u8 n ) REPL REPL-U @ ;
: DEST$ ( -- ptr u8 n ) DEST DEST-U @ ;

: ROOT-PATH! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr up:ptr :}
   ROOT$ a u dst JOIN-PATH up ! ;

: TREE-PATH! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr up:ptr :}
   TREE$ a u dst JOIN-PATH up ! ;

: PARENT-U ( ptr u8 n -- n ) {: a:ptr u:n :}
   u begin dup 0 > while
      1 -
      a over + c@ 47 = if exit then
   repeat ;

: SETUP ( -- )
   s" native-builder-image-e2e" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   s" tree" TREE TREE-U ROOT-PATH! TREE$ MAKE-DIRS
   s" tmp" TMP TMP-U ROOT-PATH! TMP$ MAKE-DIRS
   s" saved-builder" IMAGE IMAGE-U ROOT-PATH!
   s" cold-hb" COLD COLD-U ROOT-PATH!
   s" saved-hb" SAVED SAVED-U ROOT-PATH!
   s" cold-whitebox-hb" COLD-WHITE COLD-WHITE-U ROOT-PATH!
   s" saved-whitebox-hb" SAVED-WHITE SAVED-WHITE-U ROOT-PATH!
   s" rejected-hb" REJECTED REJECTED-U ROOT-PATH!
   s" src/habu/repl.f" REPL REPL-U TREE-PATH! ;

\ Private source inputs make the checker edit observable without touching the
\ checkout, and keep both build paths on exactly the same source bytes.
: COPY-MEMBER ( ptr u8 n -- ) {: a:ptr u:n :}
   a u FILE? 0= if exit then
   a u SOURCE-ROOT:CWD$ SOURCE-ROOT:RELATIVE {: rel:ptr relu:n :}
   rel relu DEST DEST-U TREE-PATH!
   DEST$ {: dest:ptr destu:n :}
   dest destu PARENT-U {: parentu:n :}
   dest parentu MAKE-DIRS
   a u DEST$ COPY-FILE-STREAM ;

: COPY-SOURCES ( -- )
   s" src" [: COPY-MEMBER ;] WALK-FILES
   s" lib" [: COPY-MEMBER ;] WALK-FILES
   s" tools" [: COPY-MEMBER ;] WALK-FILES
   s" test/c2-init-program.f" COPY-MEMBER
   s" test/c2-owner-producer-program.f" COPY-MEMBER
   s" test/c2-owner-producer-refusals.f" COPY-MEMBER ;

: SEED-PROOF ( -- )
   REPL$ S\" \npackage BUILDER-IMAGE-PROOF\nprivate\n: CALLEE ( n -- n ) 1 + ;\npublic\n: RUN ( -- n ) 41 CALLEE ;\n;package\n" APPEND-FILE ;

: EDIT-CALLEE ( -- )
   REPL$ SOURCE $10000 READ-ALL {: size:n :}
   SOURCE size s" : CALLEE ( n -- n ) 1 + ;" FIND-SUB MATCH option
      none OF E-BUILD-SOURCE throw ENDOF
      some OF IDX>N {: at:n :}
         50 SOURCE at + s" : CALLEE ( n -- n ) " nip + c!
      ENDOF
   ;MATCH
   REPL$ SOURCE size WRITE-ALL ;

: CAPTURE-RESULT ( result<pcap:captured,pcap:failed> -- )
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! 0 RC ! ENDOF
      err OF PCAP-FAILED:UNMAKE {: outu:len erru:len rc:rc :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! rc RC>N RC ! ENDOF
   ;MATCH ;

: SUCCESS ( -- )
   RC @ 0<> if OUT OUT-U @ type ERR ERR-U @ type then
   RC @ 0 T= ;

: ARGV+ ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

: ELAPSED ( n ptr u8 n -- ) {: start:n label:ptr size:n :}
   s" native-builder-image-e2e ms " type label size type s" : " type
   TIME:MONO-NS start - 1000000 / . ;

: ENV! ( bool -- ) {: white:bool :}
   s" HB_TMP" >LEN TMP$ >LEN PROC-ENV+
   s" HABU_WHITEBOX_IMAGE" >LEN
   white if s" 1" else NULL$ then >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING ;

: BUILD-IMAGE ( -- )
   s" a qualified donor saves a reusable native builder" T-LABEL
   TIME:MONO-NS {: start:n :}
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --" ARGV+ IMAGE$ ARGV+
   false ENV!
   ENGINE-CANDIDATE:PATH$ >LEN TREE$ >LEN
   S\" require tools/native-builder-image.f\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" >LEN
   OUT CAP >LEN ERR CAP >LEN BUILD-TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE CAPTURE-RESULT
   start s" save-builder" ELAPSED
   SUCCESS
   IMAGE$ EXECUTABLE? TTRUE ;

: SOURCE-BUILD ( ptr u8 n bool -- ) {: path:ptr pathu:n white:bool :}
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --load" ARGV+
   s" tools/native-build.f" ARGV+
   s" --" ARGV+
   path pathu ARGV+
   white if s" whitebox" ARGV+ then
   white ENV!
   ENGINE-CANDIDATE:PATH$ >LEN TREE$ >LEN
   OUT CAP >LEN ERR CAP >LEN BUILD-TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE CAPTURE-RESULT ;

: SAVED-BUILD ( ptr u8 n bool bool -- )
   {: path:ptr pathu:n white:bool separator:bool :}
   PROC-CWD:ARGV-ENV-CWD-RESET
   separator if s" --" ARGV+ then
   path pathu ARGV+
   white if s" whitebox" ARGV+ then
   white ENV!
   IMAGE$ >LEN TREE$ >LEN
   OUT CAP >LEN ERR CAP >LEN BUILD-TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE CAPTURE-RESULT ;

: NAMES! ( ptr u8 n ptr u8 ptr n -- )
   {: path:ptr pathu:n dst:ptr up:ptr :}
   pathu s" .names" nip + FS-PATH-CAP > if E-FS-PATH throw then
   path dst pathu BYTE-COPY
   s" .names" dst pathu + swap BYTE-COPY
   pathu s" .names" nip + up ! ;

: SAME-PRODUCT ( ptr u8 n ptr u8 n -- )
   {: a:ptr au:n b:ptr bu:n :}
   a au b bu CHAIN-RUN:SAME-FILES? TTRUE
   a au NAME-A NAME-A-U NAMES!
   b bu NAME-B NAME-B-U NAMES!
   NAME-A NAME-A-U @ NAME-B NAME-B-U @ CHAIN-RUN:SAME-FILES? TTRUE ;

: RUN-PROOF ( ptr u8 n -- ) {: path:ptr pathu:n :}
   PROC-CWD:ARGV-ENV-CWD-RESET
   path pathu >LEN TREE$ >LEN
   S\" BUILDER-IMAGE-PROOF:RUN . cr\n" >LEN
   OUT CAP >LEN ERR CAP >LEN 10000 >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE CAPTURE-RESULT
   SUCCESS ERR-U @ 0 T=
   OUT OUT-U @ S\" 43\n\n" T$= ;

: RUN-C2 ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu:n source:ptr sourceu:n :}
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --load" ARGV+
   source sourceu ARGV+
   path pathu >LEN TREE$ >LEN
   OUT CAP >LEN ERR CAP >LEN 10000 >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE CAPTURE-RESULT
   SUCCESS ;

: PRODUCT-C2 ( -- )
   s" the normal product loads typed C2 initialization, owners and XML" T-LABEL
   COLD$ s" test/c2-init-program.f" RUN-C2
   OUT OUT-U @ s" c2-init-program: ok" CONTAINS? TTRUE
   COLD$ s" lib/xml/c2.f" RUN-C2
   COLD$ s" test/c2-owner-producer-program.f" RUN-C2
   OUT OUT-U @ s" c2-owner-producer-program: ok" CONTAINS? TTRUE
   COLD$ s" test/c2-owner-producer-refusals.f" RUN-C2
   PROC-CWD:ARGV-ENV-CWD-RESET
   COLD$ >LEN TREE$ >LEN
   S\" 1 set-tier\nrequire test/c2-init-program.f\n" >LEN
   OUT CAP >LEN ERR CAP >LEN 10000 >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE CAPTURE-RESULT
   SUCCESS
   OUT OUT-U @ s" c2-init-program: ok" CONTAINS? TTRUE
   PROC-CWD:ARGV-ENV-CWD-RESET
   COLD$ >LEN TREE$ >LEN
   S\" 1 set-tier\nrequire lib/xml/c2.f\n" >LEN
   OUT CAP >LEN ERR CAP >LEN 10000 >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE CAPTURE-RESULT
   SUCCESS
   PROC-CWD:ARGV-ENV-CWD-RESET
   COLD$ >LEN TREE$ >LEN
   S\" 1 set-tier\nrequire test/c2-owner-producer-program.f\n" >LEN
   OUT CAP >LEN ERR CAP >LEN 10000 >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE CAPTURE-RESULT
   SUCCESS
   OUT OUT-U @ s" c2-owner-producer-program: ok" CONTAINS? TTRUE ;

: PRODUCT-PARITY ( -- )
   s" a saved builder and the source path publish identical edited products" T-LABEL
   TIME:MONO-NS {: cold:n :}
   COLD$ false SOURCE-BUILD
   cold s" cold-product" ELAPSED
   SUCCESS
   TIME:MONO-NS {: saved:n :}
   SAVED$ false false SAVED-BUILD
   saved s" saved-product" ELAPSED
   SUCCESS
   COLD$ SAVED$ SAME-PRODUCT
   SAVED$ RUN-PROOF ;

: WHITEBOX-PARITY ( -- )
   s" saved-builder -- output whitebox preserves image class and bytes" T-LABEL
   TIME:MONO-NS {: cold:n :}
   COLD-WHITE$ true SOURCE-BUILD
   cold s" cold-whitebox" ELAPSED
   SUCCESS
   TIME:MONO-NS {: saved:n :}
   SAVED-WHITE$ true true SAVED-BUILD
   saved s" saved-whitebox" ELAPSED
   SUCCESS
   COLD-WHITE$ SAVED-WHITE$ SAME-PRODUCT ;

: SAVED-ARGS ( -- )
   s" a saved builder refuses missing and invalid arguments before compilation" T-LABEL
   PROC-CWD:ARGV-ENV-CWD-RESET
   false ENV!
   IMAGE$ >LEN TREE$ >LEN
   OUT CAP >LEN ERR CAP >LEN 10000 >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE CAPTURE-RESULT
   RC @ 74 T=
   ERR ERR-U @ S\" native-build: one explicit output path is required, then an optional `whitebox`\n" T$=
   PROC-CWD:ARGV-ENV-CWD-RESET
   REJECTED$ ARGV+ s" wightbox" ARGV+
   false ENV!
   IMAGE$ >LEN TREE$ >LEN
   OUT CAP >LEN ERR CAP >LEN 10000 >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE CAPTURE-RESULT
   RC @ 74 T=
   ERR ERR-U @ S\" native-build: the only second argument is `whitebox`\n" T$=
   PROC-CWD:ARGV-ENV-CWD-RESET
   REJECTED$ ARGV+ s" whitebox" ARGV+ s" extra" ARGV+
   false ENV!
   IMAGE$ >LEN TREE$ >LEN
   OUT CAP >LEN ERR CAP >LEN 10000 >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE CAPTURE-RESULT
   RC @ 74 T=
   ERR ERR-U @ S\" native-build: one explicit output path is required, then an optional `whitebox`\n" T$=
   REJECTED$ FILE? 0= TTRUE ;

: REJECT-EDIT ( -- )
   s" a saved builder freshly rejects invalid edited source" T-LABEL
   REPL$ S\" \npackage BUILDER-IMAGE-BAD public\n: BAD ( n -- n ) s\" wrong\" ;\n;package\n" APPEND-FILE
   TIME:MONO-NS {: start:n :}
   REJECTED$ false false SAVED-BUILD
   start s" saved-bad-source" ELAPSED
   \ The checker's reject (70) is caught and named; the builder exits BUILD-RC.
   RC @ 74 T=
   OUT OUT-U @ s" native-build: uncaught throw code 70" CONTAINS? TTRUE
   ERR ERR-U @ s" habu: in bad" CONTAINS? TTRUE
   ERR ERR-U @ s" expected:" CONTAINS? TTRUE
   ERR ERR-U @ s" actual:" CONTAINS? TTRUE
   REJECTED$ FILE? 0= TTRUE ;

: RUN ( -- )
   T-RESET
   SETUP
   s" native-builder-image-e2e artifacts: " type ROOT$ type cr
   COPY-SOURCES
   SEED-PROOF
   BUILD-IMAGE
   SAVED-ARGS
   EDIT-CALLEE
   PRODUCT-PARITY
   PRODUCT-C2
   WHITEBOX-PARITY
   REJECT-EDIT
   T-REPORT ;

RUN
;package
