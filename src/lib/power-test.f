\ power-test.f - POWER's hold, as the OS itself lists it.
\ Run: bin/hb --load src/lib/power-test.f
\ Failure modes: a hold the OS never lists, or still lists after ALLOW-SLEEP; a
\ name that is not UTF-8 taken; a target without a backend that answers a token.

require src/lib/error.f
require src/lib/string.f
require src/lib/memory.f
require src/lib/fmt.f
require src/lib/test.f
require src/lib/process-command.f
require src/lib/power.f

package POWER-TEST
private

10000 constant PMSET-MS
64 constant NAME-CAP
NAME-CAP BUFFER: NAME
variable NAME-LEN
create BAD-NAME $FF c,

\ The pid keeps two runs at once apart.
: NAME! ( -- )
   s" habu-power-test-" {: p:ptr u:n :}
   p NAME u BYTE-COPY
   getpid FMT:INT$ {: d:ptr du:n :}
   d NAME u + du BYTE-COPY
   u du + NAME-LEN ! ;

: NAME$ ( -- ptr u8 n ) NAME NAME-LEN @ ;

: PMSET ( CMD:command -- CMD:command bool )
   CMD:RESET
   s" -g" CMD:ARG+ s" assertions" CMD:ARG+
   s" /usr/bin/pmset" PMSET-MS >MS 1 CMD:RUN
   CMD:OUT$ NAME$ CONTAINS? ;

: LISTED ( CMD:command -- CMD:command )
   NAME$ POWER:KEEP-AWAKE swap
   PMSET TTRUE
   swap POWER:ALLOW-SLEEP
   PMSET TFALSE ;


: MACOS ( -- )
   NAME!
   [: LISTED ;] CMD:WITH-COMMAND
   [: BAD-NAME 1 POWER:KEEP-AWAKE POWER:ALLOW-SLEEP ;] POWER:E-ASSERTION TTHROWSQ ;

: OTHER ( -- )
   [: s" habu-power-test" POWER:KEEP-AWAKE POWER:ALLOW-SLEEP ;] POWER:E-PLATFORM TTHROWSQ ;


: RUN ( -- )
   T-RESET
   HB-TARGET-MACOS? if MACOS else OTHER then
   T-REPORT ;

RUN
;package
