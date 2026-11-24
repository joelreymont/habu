\ log-first.f - the log program's first file: its top level logs a line, so
\ the build takes LOG's facility, a lock of the building process alone, and
\ leaves a facility of its own held.

require src/lib/log.f
require src/lib/task.f

package FILE-OBJECTS-LOG-FIRST
public
TASK:FACILITY HELD
LOG:WARN s" first" LOG:LINE s" built" LOG:TEXT+ LOG:;LINE
HELD TASK:GET
;package
