\ A stripped application runs the process-exit hook on every one of its exits.
\ test/stripped-image.f links this subject into its application, whose MAIN
\ calls RUN first: RUN makes the directory named by argument 0 and registers it
\ with CLEANUP-TREE+. Given a second argument, a code, MAIN calls FINISH and
\ returns. Code 0 RETURNS - no `die`, no uncaught throw and no REPL exit, so
\ only the entry's own inline call to the vector (src/habu/aot-link-arm.f
\ EMIT-EXIT-HOOK) can remove the tree - and FINISH throws any other code past
\ the entry, to the uncaught throw the image's reporter ends the process on
\ (src/habu/aot-link-arm.f EMIT-UNCAUGHT). Otherwise MAIN goes on to the other
\ subjects and ends in MEM:UNMAP's `die`: the engine's BDIE reaches the leaf by
\ a direct BL that the linker relocates through the (LEXITHOOK) engine-helper
\ record, which is the exit a daemon takes.
require src/lib/string.f
require src/lib/fs-mutate.f

package EXIT-HOOK-SUBJECT
public

: RUN ( -- )
   0 SCRIPT-ARGV$ {: path:ptr pathu :}
   path pathu MAKE-DIR
   path pathu CLEANUP-TREE+
   s" exit-hook-subject: ok" type cr ;

: RETURN? ( -- bool )
   SCRIPT-ARGC 1 > ;

: CODE ( -- n )
   1 SCRIPT-ARGV$ STR>NUMBER? MATCH option
      none OF s" exit-hook-subject: argument 1 is not a code" $4A die ENDOF
      some OF ENDOF
   ;MATCH ;

: FINISH ( -- )
   CODE {: code:n :}
   code 0<> if code throw then ;

;package
