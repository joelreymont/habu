\ Checked source capture, file read, and fresh native boot of prefix CODE literals.
require src/lib/string.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/test/subject.f
require test/whitebox-child.f
require test/window-child-args.f

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


\ The producer runs test/native-window-owner-child.f, which reopens the engine's
\ build window: `hb: internal engine word: DECLARATIONS`, exit 70 on the sealed
\ product. So every child runs on the engine test/whitebox-child.f names, in a
\ root of its own so the per-case roots stay free to be thrown away.
: PREPARE ( -- )
   CLEANUP-RESET
   s" aot-prefix-literal" WHITEBOX-CHILD:PROVIDE ;


: CASE-SETUP ( ptr u8 n -- ) {: mode:ptr modeu:n :}
   mode modeu T-LABEL
   mode MODE-TEXT modeu BYTE-COPY modeu MODE-U !
   s" habu-prefix-literal" HB-TMP-MKDIR {: root:ptr u:n :}
   root u CLEANUP-TREE+
   root u s" capture.aot" ART JOIN-PATH ART-U !
   root u s" hb-literal" IMAGE JOIN-PATH IMAGE-U ! ;


: PRODUCER-ARGS ( -- )
   s" test/aot-prefix-literal-producer.f" WINDOW-CHILD-ARGS:PAYLOAD
   PROC-ENV-RESET
   s" HABU_LITERAL_MODE" >LEN MODE$ >LEN PROC-ENV+
   s" HABU_LITERAL_ARTIFACT" >LEN ART$ >LEN PROC-ENV+
   s" HABU_LITERAL_ENGINE" >LEN WHITEBOX-CHILD:ENGINE$ >LEN PROC-ENV+
   WHITEBOX-CHILD:ENV+
   PROC-ENV-INHERIT-MISSING ;


: READER-ARGS ( -- )
   PROC-ARGV-RESET
   s" --load" ARG s" test/aot-prefix-literal-reader.f" ARG s" --" ARG
   ART$ ARG IMAGE$ ARG WHITEBOX-CHILD:ENGINE$ ARG
   WHITEBOX-CHILD:ENV! ;


: CONSUMER-ARGS ( -- )
   PROC-ARGV-RESET
   s" --load" ARG s" test/aot-prefix-literal-consumer.f" ARG
   s" --" ARG MODE$ ARG
   WHITEBOX-CHILD:ENV! ;


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


\ prog names the program the argv loads, for the deadline report.
: CHILD ( ptr u8 n ptr u8 n n ptr u8 n -- bool )
   {: prog:ptr progu:n engine:ptr engineu:n expected:n message:ptr messageu:n :}
   PROC-ARGV-N @ COUNT>N COMMAND-N !
   engine engineu >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN 30000 >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   prog progu OUT outu LEN>N ERR erru LEN>N rc SUBJECT:RC-TIMED-OUT
   OUT outu LEN>N message messageu CONTAINS?
   ERR erru LEN>N message messageu CONTAINS? or {: said:bool :}
   rc expected <> said 0= or if outu erru FAILURE. then
   rc expected T= said TTRUE
   rc expected = said and ;


: ACCEPT ( ptr u8 n ptr u8 n -- ) {: mode:ptr modeu:n name:ptr nameu:n :}
   mode modeu CASE-SETUP PRODUCER-ARGS
   s" test/native-window-owner-child.f" WHITEBOX-CHILD:ENGINE$ 0
   s" prefix-literal: captured" CHILD 0= if exit then
   OUT PROC-OUT-LEN @ name nameu CONTAINS? TTRUE
   OUT PROC-OUT-LEN @ S\" window: 0\n" CONTAINS? TTRUE
   ART$ EXISTS? TTRUE
   READER-ARGS
   s" test/aot-prefix-literal-reader.f" WHITEBOX-CHILD:ENGINE$ 0
   s" prefix-literal: baked" CHILD 0= if exit then
   CONSUMER-ARGS
   s" test/aot-prefix-literal-consumer.f" IMAGE$ 0
   s" prefix-literal: exact identity and execution passed" CHILD drop ;


: REFUSE ( ptr u8 n -- )
   CASE-SETUP PRODUCER-ARGS
   s" test/native-window-owner-child.f" WHITEBOX-CHILD:ENGINE$ 74
   s" aot-capture: recorded address site outside both window spans" CHILD drop
   ART$ EXISTS? TFALSE ;


: CASES ( -- )
   s" global" S\" named=ASCII-UPPER\n" ACCEPT
   s" public" S\" named=PREFIX-MARK:REQ\n" ACCEPT
   s" public-collision" S\" named=PREFIX-MARK:REQ\n" ACCEPT
   s" private" REFUSE
   s" prelude" REFUSE
   s" shadow" REFUSE
   s" nonentry" REFUSE ;

: RUN ( -- )
   T-RESET
   [: PREPARE CASES ;] [: CLEANUP-RUN ;] finally
   T-REPORT ;

RUN
;package
