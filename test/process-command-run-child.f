\ process-command-run-child.f - child for the CMD:RUN failure row of
\ src/lib/process-command-test.f: a run that exits 3 throws the caller's code,
\ and the child's captured stderr reaches this process's stderr.

require src/lib/test.f
require src/lib/process-command.f

package PROCESS-COMMAND-RUN-CHILD

7 constant CODE

: EXIT3 ( CMD:command -- CMD:command )
   s" -c" CMD:ARG+
   s" printf child-err >&2; exit 3" CMD:ARG+
   s" /bin/sh" 1000 >MS CODE CMD:RUN ;

public

: MAIN ( -- )
   T-RESET
   [: [: EXIT3 ;] CMD:WITH-COMMAND ;] CODE TTHROWSQ
   T-REPORT ;

;package

PROCESS-COMMAND-RUN-CHILD:MAIN
