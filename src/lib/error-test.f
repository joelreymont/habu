\ Run: bin/hb --load src/lib/error-test.f

require src/lib/error.f
require src/lib/string.f
require src/lib/test/assert.f
require src/lib/test/eval.f

-9671 s" the global test code" ERROR:CODE E-ERROR-TEST-GLOBAL

package ERROR-TEST

private

-9670 s" the packaged test code" ERROR:CODE E-PACKAGED
1 63 lshift s" the smallest test code" ERROR:CODE E-MIN
-9681 s" the longest name" ERROR:CODE E-ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRS
s" test/fixtures/error/again.f" included
-9683 s" the fault test code" ERROR:FAULT E-FAULT
-9679 constant UNDECLARED

package PB
-9677 s" the part's test code" ERROR:CODE E-PART
;package


: PACKAGED-CASE ( -- )
   s" a code declared in a package is named PKG:E-X" T-LABEL
   E-PACKAGED -9670 T=
   [: E-PACKAGED throw ;] E-PACKAGED TTHROWSQ
   -9670 ERROR:NAME$ s" ERROR-TEST:E-PACKAGED" T$=
   -9670 ERROR:TEXT$ s" the packaged test code" T$= ;


: GLOBAL-CASE ( -- )
   s" a code declared outside a package is named E-X" T-LABEL
   E-ERROR-TEST-GLOBAL -9671 T=
   -9671 ERROR:NAME$ s" E-ERROR-TEST-GLOBAL" T$=
   -9671 ERROR:TEXT$ s" the global test code" T$= ;


: UNDECLARED-CASE ( -- )
   s" an undeclared code has an empty name and text" T-LABEL
   UNDECLARED ERROR:NAME$ STR:LEN 0 T=
   UNDECLARED ERROR:TEXT$ STR:LEN 0 T= ;


: MIN-CASE ( -- )
   s" the smallest number declared reads back as itself" T-LABEL
   E-MIN 1 63 lshift T=
   1 63 lshift ERROR:NAME$ s" ERROR-TEST:E-MIN" T$= ;


: PART-CASE ( -- )
   s" a code declared in a part is named PKG:PART:E-X" T-LABEL
   -9677 ERROR:NAME$ s" ERROR-TEST:PB:E-PART" T$= ;


: MAX-NAME-CASE ( -- )
   s" a 255-byte name is declared and named" T-LABEL
   -9681 ERROR:NAME$ s" ERROR-TEST:E-ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRS" T$= ;


: AGAIN-CASE ( -- )
   s" the same name declared again in its package is rejected as E-DUPLICATE-DEFINITION" T-LABEL
   REJECTION:CAPTURE
   [: s" test/fixtures/error/again.f" included ;] 70 TTHROWSQ
   REJECTION:BUFFER$ s" error[E-DUPLICATE-DEFINITION]: `E-AGAIN`" STR:CONTAINS? TTRUE
   REJECTION:BUFFER-OFF ;


: FAULT-CASE ( -- )
   s" a code declared by FAULT is named as by CODE, and is a fault" T-LABEL
   E-FAULT -9683 T=
   -9683 ERROR:NAME$ s" ERROR-TEST:E-FAULT" T$=
   -9683 ERROR:TEXT$ s" the fault test code" T$=
   -9683 ERROR:FAULT? TTRUE ;


: NOT-FAULT-CASE ( -- )
   s" a code declared by CODE, or not declared, is no fault" T-LABEL
   -9670 ERROR:FAULT? TFALSE
   UNDECLARED ERROR:FAULT? TFALSE ;


\ The capture holds one record, whose head line is h and help line p.
: ONE-RECORD ( str str -- ) {: h:str p:str :}
   REJECTION:BUFFER$ h STR:CONTAINS? TTRUE
   REJECTION:BUFFER$ p STR:CONTAINS? TTRUE
   REJECTION:BUFFER$ s" error[" STR:COUNT-SUB 1 T=
   REJECTION:BUFFER-OFF ;


: TAKEN-CASE ( -- )
   s" a taken code is rejected naming the constant that holds it" T-LABEL
   REJECTION:CAPTURE
   [: s" test/fixtures/error/taken.f" included ;] 70 TTHROWSQ
   REJECTION:BUFFER$ s"  ^^^^^^^^^^^ declared again" STR:CONTAINS? TTRUE
   s" error[E-DUPLICATE-DEFINITION]: code -9673 is already `ERROR-TEST:E-TAKEN-OLD`"
   s" = help: give `ERROR-TEST:E-TAKEN-NEW` a code its block has not declared" ONE-RECORD ;


: EMPTY-CASE ( -- )
   s" an empty text is rejected at the declaring word" T-LABEL
   REJECTION:CAPTURE
   [: s" test/fixtures/error/empty.f" included ;] 70 TTHROWSQ
   REJECTION:BUFFER$ s"  ^^^^^^^^^^ declared here" STR:CONTAINS? TTRUE
   s" error[E-BAD-DECLARATION]: code -9674 has an empty text"
   s" = help: give the code a text that says what failed" ONE-RECORD ;


: NO-NAME-CASE ( -- )
   s" a code with no name after it is rejected" T-LABEL
   REJECTION:CAPTURE
   [: s" test/fixtures/error/no-name.f" included ;] 70 TTHROWSQ
   s" error[E-MISSING-NAME]: `ERROR:CODE` needs a name"
   s" = help: give `ERROR:CODE` one name, a token with no blanks" ONE-RECORD ;


: LONG-NAME-CASE ( -- )
   s" a 256-byte name is rejected" T-LABEL
   REJECTION:CAPTURE
   [: s" test/fixtures/error/long-name.f" included ;] 70 TTHROWSQ
   s" error[E-BAD-DECLARATION]: the name of a code is 256 bytes, more than the 255 a name holds"
   s" = help: shorten the name to 255 bytes or fewer" ONE-RECORD ;


\ The thrown text keeps its code's row; FILL lays cells past it, where a list
\ walk would read them had the row's storage been cut.
: THROWN-TEXT$ ( -- str )
   s\" -9686 s\" the cut code\" ERROR:CODE E-CUT  1 throw" ;


: AFTER-TEXT$ ( -- str )
   s\" -9687 s\" the code after the cut\" ERROR:CODE E-AFTER" ;


: FILL ( n -- ) 0 ?do 777 , loop ;


: THROWN-CASE ( -- )
   s" a caught failed evaluate leaves the codes before and after it" T-LABEL
   THROWN-TEXT$ TEST-EVAL:RC 1 T=
   12 FILL
   -9670 ERROR:NAME$ s" ERROR-TEST:E-PACKAGED" T$=
   AFTER-TEXT$ TEST-EVAL:RC 0 T=
   -9687 ERROR:NAME$ s" ERROR-TEST:E-AFTER" T$= ;


: ERROR-RUN ( -- )
   PACKAGED-CASE
   GLOBAL-CASE
   UNDECLARED-CASE
   MIN-CASE
   PART-CASE
   MAX-NAME-CASE
   AGAIN-CASE
   FAULT-CASE
   NOT-FAULT-CASE
   TAKEN-CASE
   EMPTY-CASE
   NO-NAME-CASE
   LONG-NAME-CASE
   THROWN-CASE ;

ERROR-RUN

public

\ tail.f opens packages of its own, which a file included inside ERROR-TEST
\ cannot: it runs with no package open.
: TAIL-CASE ( -- )
   s" a taken code declared under the same name in a second package is rejected" T-LABEL
   REJECTION:CAPTURE
   [: s" test/fixtures/error/tail.f" included ;] 70 TTHROWSQ
   s" error[E-DUPLICATE-DEFINITION]: code -9682 is already `ERROR-TAIL-A:E-X`"
   s" = help: give `ERROR-TAIL-B:E-X` a code its block has not declared" ONE-RECORD ;

;package

ERROR-TEST:TAIL-CASE
T-REPORT
