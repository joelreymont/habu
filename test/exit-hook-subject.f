\ A stripped application runs the process-exit hook on both of its exits.
\ test/stripped-image.f links this subject into its application, whose MAIN
\ calls RUN first: RUN makes the directory named by argument 0 and registers it
\ with CLEANUP-TREE+. Given a second argument MAIN then RETURNS - no `die`, no
\ uncaught throw and no REPL exit, so only the entry's own inline call to the
\ vector (src/habu/aot-lib.f EMIT-EXIT-HOOK) can remove the tree. Otherwise MAIN
\ goes on to the other subjects and ends in MEM:UNMAP's `die`: the engine's BDIE
\ reaches the leaf by a direct BL that the linker relocates through the
\ (LEXITHOOK) engine-helper record, which is the exit a daemon takes.
require lib/fs-mutate.f

package EXIT-HOOK-SUBJECT
public

: RUN ( -- )
   0 SCRIPT-ARGV$ {: path:ptr pathu :}
   path pathu MAKE-DIR
   path pathu CLEANUP-TREE+
   s" exit-hook-subject: ok" type cr ;

: RETURN? ( -- bool )
   SCRIPT-ARGC 1 > ;

;package
