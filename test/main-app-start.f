\ A captured application runs before stdin, keeps SCRIPT-ARGV's app convention,
\ and takes the same exit hook path on return and uncaught throws.
\ Run with HABU_UNDER_TEST naming the emitted engine.
require lib/test.f
require test/gate-common.f
require lib/engine-candidate.f

package MAIN-APP-START-TEST
private

create SUBJECT FS-PATH-CAP allot
create IMAGE FS-PATH-CAP allot
variable SUBJECT-U
variable IMAGE-U

: SUBJECT$ ( -- ptr u8 n ) SUBJECT SUBJECT-U @ ;
: IMAGE$ ( -- ptr u8 n ) IMAGE IMAGE-U @ ;

: PREPARE ( -- )
   s" habu-main-app" GT-START
   s" subject.f" SUBJECT GT-PATH SUBJECT-U !
   s" application" IMAGE GT-PATH IMAGE-U !
   SUBJECT$
   S\" require src/os/script-argv.f\nrequire lib/string.f\ncreate APP-LF 10 c,\n: APP-HOOK ( -- ) 2 s\" HOOK\" write drop 2 APP-LF 1 write drop ;\nTRUSTED: HOOK-PTR ( -- ptr [ -- ] ) data-base EXIT-HOOK-CELL + ;\n: MAIN ( -- )\n   [: APP-HOOK ;] HOOK-PTR xt!\n   s\" APP \" type SCRIPT-ARGC .\n   0 SCRIPT-ARGV$ 2dup type cr\n   2dup s\" fail\" STR= if 2drop 91 throw then\n   s\" wide\" STR= if -1234 throw then ;\n"
   WRITE-ALL ;

: BUILD ( -- )
   GE-HB-RESET
   s" --" GE-ARG+ IMAGE$ GE-ARG+
   SB-RESET
   S\" require src/habu/app-image.f\ns\" " SB-APPEND
   SUBJECT$ SB-APPEND
   S\" \" required\n' MAIN APP-IMAGE:START!\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n" SB-APPEND
   GE-HB$ SB$ GE-TIMEOUT-MS GE-RUN-STDIN
   s" build application" GE-EXPECT-OK
   IMAGE$ EXECUTABLE? TTRUE ;

: RUN-APP ( ptr u8 n ptr u8 n -- ) {: arg:ptr argu:n input:ptr inu:n :}
   GE-HB-RESET
   s" --" GE-ARG+ arg argu GE-ARG+
   IMAGE$ input inu GE-TIMEOUT-MS GE-RUN-STDIN ;

: RETURN-CASE ( -- )
   s" pass" S\" s\" PIPE\" type cr\n" RUN-APP
   s" app return" GE-EXPECT-OK
   S\" APP 1\npass\nPIPE\n" s" app return" GE-EXPECT-OUT
   S\" HOOK\n" s" app return" GE-EXPECT-ERR ;

\ A saved full engine must retain CREATE's runtime writer when a defining
\ word is compiled and invoked after restore, with its source name operand.
: CREATED-CASE ( -- )
   s" pass"
   S\" 1 set-tier\n: MAKE ( n -- ) create , does> ( -- n ) @ ;\n33 MAKE CREATED\nCREATED . cr\n"
   RUN-APP
   s" app restored create" GE-EXPECT-OK
   S\" APP 1\npass\n33\n\n" s" app restored create" GE-EXPECT-OUT
   S\" HOOK\n" s" app restored create" GE-EXPECT-ERR ;

: BATCH-ERROR ( -- )
   s" pass" S\" NO-SUCH-WORD\n" RUN-APP
   70 s" app batch error" GE-EXPECT-RC
   S\" APP 1\npass\n" s" app batch error" GE-EXPECT-OUT
   s" E-UNDEFINED: NO-SUCH-WORD" s" app batch error" GE-EXPECT-ERR-HAS ;

: THROW-CASE ( -- )
   s" fail" S\" s\" PIPE\" type cr\n" RUN-APP
   91 s" app small throw" GE-EXPECT-RC
   S\" APP 1\nfail\n" s" app small throw" GE-EXPECT-OUT
   S\" HOOK\n" s" app small throw" GE-EXPECT-ERR
   s" wide" S\" s\" PIPE\" type cr\n" RUN-APP
   67 s" app wide throw" GE-EXPECT-RC
   S\" APP 1\nwide\n" s" app wide throw" GE-EXPECT-OUT
   S\" HOOK\nhb: uncaught throw code -1234\n" s" app wide throw" GE-EXPECT-ERR ;

public

: RUN ( -- )
   T-RESET
   PREPARE BUILD RETURN-CASE CREATED-CASE BATCH-ERROR THROW-CASE
   GT-CLEANUP
   T-REPORT ;

RUN

;package
