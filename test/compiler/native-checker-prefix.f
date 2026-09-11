\ Standalone process: compile the checker source with the retained native checker.
package CHECKER-PREFIX-TEST
TRUSTED: RESET ( -- ) 0 set-check 0 set-top-check CHECKER-RESET-SOURCE IMK-NDICT0 @ 1 - seed-ndict! ;
: LOAD ( -- )
   s" src/core/util.f" included
   s" src/core/cell.f" included
   s" src/core/pointer-storage.f" included
   s" src/core/engine-error.f" included
   s" src/core/exec-vector.f" included
   s" src/core/checker.f" included
   s" src/core/engine-error-effects.f" included
   s" src/core/lower-cert-base.f" included
   s" src/core/type-schema.f" included
   s" src/core/type-family.f" included
   s" test/compiler/native-prefix-rollback.f" included
   s" test/compiler/native-checker-storage.f" included ;
: RUN ( -- ) RESET LOAD ;
' RUN
;package
execute
