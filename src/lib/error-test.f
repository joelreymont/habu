\ Run: bin/hb --load src/lib/error-test.f

require src/lib/error.f
require src/lib/string.f
require src/lib/process-command.f
require src/lib/engine-candidate.f
require src/lib/test.f
require src/lib/test/ending.f

-9671 s" the global test code" ERROR:CODE E-ERROR-TEST-GLOBAL

package ERROR-TEST

private

-9670 s" the packaged test code" ERROR:CODE E-PACKAGED
201 s" the first 201" ERROR:CODE E-STATUS-OLD
201 s" the second 201" ERROR:CODE E-STATUS-NEW
1 63 lshift s" the smallest test code" ERROR:CODE E-MIN
-9681 s" the longest name" ERROR:CODE E-ABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRSTUVWXYZABCDEFGHIJKLMNOPQRS
s" test/fixtures/error/again.f" included
-9679 constant UNDECLARED
$4E constant DUPLICATE  \ E-DUPLICATE-DEFINITION, as constant throws it
60000 constant TIMEOUT-MS

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
   UNDECLARED ERROR:NAME$ nip 0 T=
   UNDECLARED ERROR:TEXT$ nip 0 T= ;


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
   s" the same name declared again in its package throws E-DUPLICATE-DEFINITION" T-LABEL
   [: s" test/fixtures/error/again.f" included ;] DUPLICATE TTHROWSQ ;


: STATUS-CASE ( -- )
   s" a repeated code in 0..255 is named by its newest declaration" T-LABEL
   E-STATUS-OLD 201 T=
   E-STATUS-NEW 201 T=
   201 ERROR:NAME$ s" ERROR-TEST:E-STATUS-NEW" T$=
   201 ERROR:TEXT$ s" the second 201" T$= ;


\ A refusal ends the process, so each fixture runs in a fresh hb: that exit is
\ the boundary under test.
: FIXTURE-RUN ( CMD:command ptr u8 n -- CMD:command ) {: path:ptr pathu:n :}
   s" --load" CMD:ARG+ path pathu CMD:ARG+
   ENGINE-CANDIDATE:PATH$ TIMEOUT-MS >MS 70 T-RUN-EXITED= ;


: TAKEN-RUN ( CMD:command -- CMD:command )
   s" test/fixtures/error/taken.f" FIXTURE-RUN
   CMD:ERR$ s" ERROR:CODE: -9673 is E-TAKEN-OLD, declared again as E-TAKEN-NEW"
   CONTAINS? TTRUE ;


: TAKEN-CASE ( -- )
   s" a taken code outside 0..255 ends the process naming both constants" T-LABEL
   [: TAKEN-RUN ;] CMD:WITH-COMMAND ;


: TAIL-RUN ( CMD:command -- CMD:command )
   s" test/fixtures/error/tail.f" FIXTURE-RUN
   CMD:ERR$ s" ERROR:CODE: -9682 is ERROR-TAIL-A:E-X, declared again as ERROR-TAIL-B:E-X"
   CONTAINS? TTRUE ;


: TAIL-CASE ( -- )
   s" a taken code declared under the same name in a second package ends the process" T-LABEL
   [: TAIL-RUN ;] CMD:WITH-COMMAND ;


: EMPTY-RUN ( CMD:command -- CMD:command )
   s" test/fixtures/error/empty.f" FIXTURE-RUN
   CMD:ERR$ s" ERROR:CODE: E-EMPTY-TEXT has an empty text" CONTAINS? TTRUE ;


: EMPTY-CASE ( -- )
   s" an empty text ends the process naming the constant" T-LABEL
   [: EMPTY-RUN ;] CMD:WITH-COMMAND ;


: NO-NAME-RUN ( CMD:command -- CMD:command )
   s" test/fixtures/error/no-name.f" FIXTURE-RUN
   CMD:ERR$ s" ERROR:CODE: no name after the code -9675" CONTAINS? TTRUE ;


: NO-NAME-CASE ( -- )
   s" a code with no name after it ends the process" T-LABEL
   [: NO-NAME-RUN ;] CMD:WITH-COMMAND ;


: LONG-NAME-RUN ( CMD:command -- CMD:command )
   s" test/fixtures/error/long-name.f" FIXTURE-RUN
   CMD:ERR$ s" is longer than 255 bytes" CONTAINS? TTRUE ;


: LONG-NAME-CASE ( -- )
   s" a 256-byte name ends the process" T-LABEL
   [: LONG-NAME-RUN ;] CMD:WITH-COMMAND ;


: ERROR-RUN ( -- )
   PACKAGED-CASE
   GLOBAL-CASE
   UNDECLARED-CASE
   STATUS-CASE
   MIN-CASE
   PART-CASE
   MAX-NAME-CASE
   AGAIN-CASE
   TAKEN-CASE
   TAIL-CASE
   EMPTY-CASE
   NO-NAME-CASE
   LONG-NAME-CASE ;

ERROR-RUN
T-REPORT

;package
