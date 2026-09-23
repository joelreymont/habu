\ A stripped application runs the process-exit hook on both of its exits. MAIN
\ registers the directory named by argument 0 and then either RETURNS - no
\ `die`, no uncaught throw and no REPL exit, so only the entry's own inline
\ call to the vector (src/habu/aot-lib.f EMIT-EXIT-HOOK) can remove the tree -
\ or, given a second argument, DIES: the engine's BDIE reaches the leaf by a
\ direct BL that the linker relocates through the (LEXITHOOK) engine-helper
\ record, which is the exit a daemon takes.
require lib/fs-mutate.f

package EXIT-HOOK-SUBJECT
public

: RUN ( -- )
   0 SCRIPT-ARGV$ {: path:ptr pathu :}
   path pathu MAKE-DIR
   path pathu CLEANUP-TREE+
   s" exit-hook-subject: ok" type cr
   SCRIPT-ARGC 1 > if s" subject: dying" 7 die then ;

;package

: MAIN ( -- )
   EXIT-HOOK-SUBJECT:RUN ;
