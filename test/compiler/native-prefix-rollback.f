\ Loaded before sealing: exercise the actual prefix-boundary extension hooks.
\ test/native-window-capture.f loads it between payload validation's checker
\ preparations and tape-detach's. The MARK below stays taken, so tape-detach's
\ compact against it, as the build's own capture compacts against the mark
\ src/core/lower-cert-seal.f takes.
package PREFIX-ROLLBACK-TEST
using SCHEMA-REG
: ADD-NODE ( -- ) CC-N SCHEMA-CON SCHEMA-ROOT+ drop ;
: CHECK ( -- )
   SCHEMA-N@ {: nodes:n :}
   SCHEMA-ROOT-N@ {: roots:n :}
   CHECKER-BOUND:MARK
   ADD-NODE
   SCHEMA-N@ nodes <= SCHEMA-ROOT-N@ roots <= or if
      s" schema fixture did not grow the registry" 76 die then
   CHECKER-BOUND:REWIND
   SCHEMA-N@ nodes <> SCHEMA-ROOT-N@ roots <> or if
      s" prefix boundary failed to rewind private schema state" 76 die then ;
CHECK
;package
