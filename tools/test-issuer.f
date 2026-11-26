\ test-issuer.f - TEST-ISSUER served on a loopback port for suites that cannot
\ load it. Prints `issuer http://127.0.0.1:<port>` and serves until stdin ends.
\ Usage: bin/hb --load tools/test-issuer.f -- [--client <id> <secret>]...

require src/lib/string.f
require src/lib/fmt.f
require src/lib/span.f
require src/lib/fd.f
require src/lib/aio.f
require src/lib/net/http.f
require src/lib/net/test-issuer.f
require src/os/script-argv.f
require src/core/exit-status.f

package TEST-ISSUER-TOOL
private

$7F000001 constant LOOPBACK
2 constant TWO-WORKERS
500 constant IDLE-MS
$100 constant SINK-CAP

TYPED-VARIABLE CFG HTTP:config

: SERVER-CONFIG ( -- ^HTTP:config )
   LOOPBACK CFG HTTP-CONFIG:ADDRESS !
   0 CFG HTTP-CONFIG:PORT !
   TWO-WORKERS CFG HTTP-CONFIG:WORKERS !
   IDLE-MS >MS CFG HTTP-CONFIG:IDLE !
   CFG ;

SINK-CAP BUFFER: SINK

: USAGE ( -- )
   s" usage: hb --load tools/test-issuer.f -- [--client <id> <secret>]..." EXIT-STATUS:USAGE die ;

: CLIENTS ( -- )
   SCRIPT-ARGC {: argc:n :}
   0 begin dup argc < while
      {: at:n :}
      at 2 + argc >= if USAGE then
      at SCRIPT-ARGV$ s" --client" STR:= 0= if USAGE then
      at 1+ SCRIPT-ARGV$ at 2 + SCRIPT-ARGV$ TEST-ISSUER:CLIENT+
      at 3 +
   repeat drop ;

\ Whether stdin still has bytes; false at its end.
: OPEN? ( -- bool )
   FD:STDIN SINK SINK-CAP SPAN:MAKE FD:READ-EXACT MATCH FD:fill
      full OF true ENDOF
      eof OF drop false ENDOF
   ;MATCH ;

: SERVE ( -- )
   CLIENTS
   TEST-ISSUER:ROUTES
   SERVER-CONFIG HTTP:START
   SB-RESET s" issuer http://127.0.0.1:" SB-APPEND HTTP:PORT FMT:SB-U 10 SB-APPEND-C SB$ type
   begin OPEN? while repeat
   HTTP:STOP ;

SERVE

;package
