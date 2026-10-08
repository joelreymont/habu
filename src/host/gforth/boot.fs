\ src/host/gforth/boot.fs - the Gforth host's entry (docs/bootstrap.md stage 1):
\   gforth -m 1G src/host/gforth/boot.fs <program.f>   (from the repository root)
\ It loads the kernel files in the native prefix order (src/habu/habu2.f
\ PFX-FILES), checks the primitive bodies against src/habu/prims.f as soon as
\ that file has loaded, runs the program and exits with its code. Control
\ never returns to Gforth's own argument loop.

require ./layout.fs
require ./reader.fs
require ./prims.fs

64 constant USAGE-RC                 \ lib/argv.f usage

: KERNEL ( -- )
   s" src/core/util.f" LOAD-FILE
   s" src/core/cell.f" LOAD-FILE
   s" src/core/pointer-storage.f" LOAD-FILE
   s" src/core/engine-error.f" LOAD-FILE
   s" src/core/exec-vector.f" LOAD-FILE
   s" src/core/checker-fetch-abi.f" LOAD-FILE
   s" src/core/checker-owner-abi.f" LOAD-FILE
   s" src/habu/prims.f" LOAD-FILE  PRIMS-CHECK
   s" src/core/does-clause.f" LOAD-FILE
   s" src/core/checker.f" LOAD-FILE
   s" src/core/engine-error-effects.f" LOAD-FILE
   s" src/core/lower-cert-base.f" LOAD-FILE
   s" src/core/type-schema.f" LOAD-FILE
   s" src/core/type-family.f" LOAD-FILE
   s" src/core/render.f" LOAD-FILE
   s" src/core/sumtype.f" LOAD-FILE
   s" src/core/layout-buffer.f" LOAD-FILE
   s" src/core/layout-valid.f" LOAD-FILE
   s" src/core/check-hook.f" LOAD-FILE ;

: HB-MAIN ( -- )
   next-arg dup 0= if
      2drop s" hb: usage: gforth -m 1G src/host/gforth/boot.fs <program.f>" ERR ERR-NL
      USAGE-RC (bye) then
   KERNEL LOAD-FILE  0 HB-EXIT ;

\ A throw no catch received (habu2.f:11800-11813 LUNCAUGHT): the exit hook,
\ then a code in [1,255] exits as it is; any other is reported and exits
\ RC-REJECT when it is the refusal the checker last rendered, else UNCAUGHT-RC.
: UNCAUGHT ( n -- )
   RUN-EXIT-HOOK  dup 1 256 within if (bye) then
   s" hb: uncaught throw code " ERR dup ERR-N ERR-NL
   REFUSAL-CODE-CELL D@ = if RC-REJECT else UNCAUGHT-RC then (bye) ;

\ The entry is compiled, so a throw out of the program reaches UNCAUGHT
\ whatever state Gforth's compiler was left in, never Gforth's interpreter.
: MAIN ( -- ) ['] HB-MAIN catch UNCAUGHT ;
MAIN
