\ Checked source capture, file read, and fresh native boot of prefix CODE literals.
require lib/test.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/engine-id.f
require lib/engine-candidate.f

package PREFIX-LITERAL-SUITE

$4000 constant IO-CAP
create OUT IO-CAP allot
create ERR IO-CAP allot
create ART FS-PATH-CAP allot
variable ART-U
create IMAGE FS-PATH-CAP allot
variable IMAGE-U
create MODE-TEXT $100 allot
variable MODE-U
variable COMMAND-N

: ART$ ( -- ptr u8 n ) ART ART-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;
: MODE$ ( -- ptr u8 n ) MODE-TEXT MODE-U @ ;
: ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;


: CASE-SETUP ( ptr u8 n -- ) {: mode:ptr modeu:n :}
   mode modeu T-LABEL
   mode MODE-TEXT modeu BYTE-COPY modeu MODE-U !
   s" habu-prefix-literal" TMPDIR-MKDIR {: root:ptr u:n :}
   root u CLEANUP-TREE+
   root u s" capture.aot" ART JOIN-PATH ART-U !
   root u s" hb-literal" IMAGE JOIN-PATH IMAGE-U ! ;


: PRODUCER-ARGS ( -- )
   PROC-ARGV-RESET
   s" --load" ARG s" test/native-window-owner-child.f" ARG s" --" ARG
   s" test/aot-prefix-literal-producer.f" ARG
   s" src/core/declaration-transaction.f" ARG
   s" src/core/generated-declaration.f" ARG
   s" src/core/decl-event.f" ARG
   s" src/core/structure-make.f" ARG
   s" src/core/structure-decl.f" ARG
   s" src/core/enum-decl.f" ARG
   s" src/core/structures.f" ARG
   s" src/core/bytes.f" ARG
   s" src/core/dynamic-storage.f" ARG
   HB-TARGET-LINUX? if
      s" src/os/linux/target.f" ARG s" src/os/linux/layout.f" ARG
   else
      s" src/os/macos/target.f" ARG s" src/os/macos/layout.f" ARG
   then
   s" src/habu/layout.f" ARG
   s" src/os/env-base.f" ARG
   s" src/core/include.f" ARG
   s" src/core/sha256.f" ARG
   s" lib/prelude.f" ARG
   PROC-ENV-RESET
   s" HABU_LITERAL_MODE" >LEN MODE$ >LEN PROC-ENV+
   s" HABU_LITERAL_ARTIFACT" >LEN ART$ >LEN PROC-ENV+
   s" HABU_LITERAL_ENGINE" >LEN ENGINE-CANDIDATE:PATH$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING ;


: READER-ARGS ( -- )
   PROC-ARGV-RESET
   s" --load" ARG s" test/aot-prefix-literal-reader.f" ARG s" --" ARG
   ART$ ARG IMAGE$ ARG ENGINE-CANDIDATE:PATH$ ARG
   PROC-ENV-RESET PROC-ENV-INHERIT-MISSING ;


: CONSUMER-ARGS ( -- )
   PROC-ARGV-RESET
   s" --load" ARG s" test/aot-prefix-literal-consumer.f" ARG
   s" --" ARG MODE$ ARG
   PROC-ENV-RESET PROC-ENV-INHERIT-MISSING ;


: ZTYPE ( ptr u8 -- )
   begin dup c@ 0<> while dup c@ emit 1+ repeat drop ;


: COMMAND. ( -- )
   COMMAND-N @ 1+ 0 ?do
      i >IDX PROC-ARGV-SLOT @ ZTYPE space
   loop cr ;


: OUTCOME. ( -- )
   PROC-CAPTURE-OUTCOME
   MATCH outcome
      exited OF s" exited " type . ENDOF
      signaled OF s" signaled " type . ENDOF
      timeout OF s" timeout" type cr ENDOF
   ;MATCH ;


: FAILURE. ( len len -- ) {: outu:len erru:len :}
   s" prefix-literal mode=" type MODE$ type cr COMMAND. OUTCOME.
   s" stdout bytes=" type outu LEN>N .
   s" stderr bytes=" type erru LEN>N .
   s" capture capacity=" type IO-CAP .
   OUT outu LEN>N type ERR erru LEN>N type cr ;


: CHILD ( ptr u8 n n ptr u8 n -- bool )
   {: engine:ptr engineu:n expected:n message:ptr messageu:n :}
   PROC-ARGV-N @ COUNT>N COMMAND-N !
   engine engineu >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN 30000 >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   OUT outu LEN>N message messageu CONTAINS?
   ERR erru LEN>N message messageu CONTAINS? or {: said:bool :}
   rc expected <> said 0= or if outu erru FAILURE. then
   rc expected T= said TTRUE
   rc expected = said and ;


: ACCEPT ( ptr u8 n ptr u8 n -- ) {: mode:ptr modeu:n name:ptr nameu:n :}
   mode modeu CASE-SETUP PRODUCER-ARGS
   ENGINE-CANDIDATE:PATH$ 0 s" prefix-literal: captured" CHILD 0= if exit then
   OUT PROC-OUT-LEN @ LEN>N name nameu CONTAINS? TTRUE
   OUT PROC-OUT-LEN @ LEN>N S\" window: 0\n" CONTAINS? TTRUE
   ART$ EXISTS? TTRUE
   READER-ARGS
   ENGINE-CANDIDATE:PATH$ 0 s" prefix-literal: baked" CHILD 0= if exit then
   CONSUMER-ARGS
   IMAGE$ 0 s" prefix-literal: exact identity and execution passed" CHILD drop ;


: REFUSE ( ptr u8 n -- )
   CASE-SETUP PRODUCER-ARGS
   ENGINE-CANDIDATE:PATH$ 74
   s" aot-capture: recorded address site outside both window spans" CHILD drop
   ART$ EXISTS? TFALSE ;


: RUN ( -- )
   T-RESET CLEANUP-RESET
   s" global" S\" named=ASCII-UPPER\n" ACCEPT
   s" public" S\" named=PREFIX-MARK:REQ\n" ACCEPT
   s" public-collision" S\" named=PREFIX-MARK:REQ\n" ACCEPT
   s" private" REFUSE
   s" prelude" REFUSE
   s" shadow" REFUSE
   s" nonentry" REFUSE
   CLEANUP-RUN T-REPORT ;

RUN
;package
