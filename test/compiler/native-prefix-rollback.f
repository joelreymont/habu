\ Loaded before sealing: exercise the actual prefix-boundary extension hooks.
package PREFIX-ROLLBACK-TEST
using SCHEMA-REG
: CHECK ( -- )
   SCHEMA-N@ {: nodes:n :}
   SCHEMA-ROOT-N@ {: roots:n :}
   CHECKER-BOUND:MARK
   CC-N SCHEMA-CON SCHEMA-ROOT+ drop
   SCHEMA-N@ nodes <= SCHEMA-ROOT-N@ roots <= or if
      s" schema fixture did not grow the registry" 76 die then
   CHECKER-BOUND:REWIND
   SCHEMA-N@ nodes <> SCHEMA-ROOT-N@ roots <> or if
      s" prefix boundary failed to rewind private schema state" 76 die then ;
CHECK
;package
