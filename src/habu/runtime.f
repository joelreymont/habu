\ runtime.f - the Habu loop's installs, one list for every machine:
\ interpreter.f is the engine's `evaluate`, `create` and definition policy,
\ main.f its command line and T0 its compiler, after the declaration cells,
\ bound to the checker's record, and the shadow's `does>` patch. INSTALL stores their xts in the
\ engine's cells; in a build those stores reach the product being written.

require src/compiler/native/shadow.f
require src/habu/t0.f
require src/habu/interpreter.f
require src/habu/main.f

package RUNTIME

public

: INSTALL ( -- )
   ['] NSHADOW:PATCH-DOES data-base NCOMP-DISPATCH:DOES-SHADOW-CELL + xt!
   CHECKER-REG:DECLARATIONS NCOMP-DISPATCH:DECL-A ptr-cell!
   CHECKER-REG:DECLARATIONS NCOMP-DISPATCH:TARGET-DECL-A ptr-cell!
   INTERPRETER:INSTALL-EVALUATE
   INTERPRETER:INSTALL-CREATE
   INTERPRETER:INSTALL-POLICY
   COMMAND-LINE:INSTALL
   COMP:INSTALL ;

;package
