\ A stripped application runs the process-exit hook on every one of its exits.
\ test/stripped-image.f links this subject; MAIN calls RUN first, which makes
\ the directory named by argument 0 and registers it with FS:CLEANUP-TREE+.

\ With a second argument, a code, MAIN calls FINISH. Code 0 returns with no
\ `die`, throw or REPL exit, so only the entry's inline call to the vector
\ (src/habu/aot-link-arm.f EMIT-EXIT-HOOK) can remove the tree; any other code
\ is thrown past the entry to the image's reporter (EMIT-UNCAUGHT). Otherwise
\ MAIN ends in MEM:UNMAP's `die`, a daemon's exit: BDIE reaches the leaf by a
\ direct BL the linker relocates through the (LEXITHOOK) engine-helper record.
require src/lib/string.f
require src/lib/fs-mutate.f

package EXIT-HOOK-SUBJECT
public

: RUN ( -- )
   0 SCRIPT-ARGV$ {: path:ptr pathu :}
   path pathu FS:MAKE-DIR
   path pathu FS:CLEANUP-TREE+
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
