\ runtime.f - the Habu loop's installs, one list for every machine:
\ interpreter.f is the engine's `evaluate`, `create` and definition policy,
\ main.f its command line and T0 its compiler, after the declaration cells,
\ bound to the checker's record, and the shadow's `does>` patch. INSTALL stores their xts in the
\ engine's cells; in a build those stores reach the product being written.
\ The selected target chooses the T0 emitter for live and shadow compilation,
\ and COMP is an engine package: a sealed package cannot be reopened to load one later.

require src/compiler/native/shadow.f
require src/habu/t0-arm64.f
require src/habu/t0-x64.f
require src/habu/t0-wasm.f
require src/habu/t0.f
require src/habu/interpreter.f
require src/habu/main.f

package RUNTIME

EXPORT BOOT:data-base
EXPORT POINTER-STORAGE:RESERVED-PTR-U8-CELL
NCOMP-DISPATCH:DECL-CELL RESERVED-PTR-U8-CELL SOURCE-OWNER-A
NCOMP-DISPATCH:TARGET-DECL-CELL RESERVED-PTR-U8-CELL TARGET-OWNER-A

public

: INSTALL ( -- )
   ['] NSHADOW:PATCH-DOES data-base NCOMP-DISPATCH:DOES-SHADOW-CELL + xt!
   CHECKER-REG:DECLARATIONS SOURCE-OWNER-A ptr-cell!
   CHECKER-REG:DECLARATIONS TARGET-OWNER-A ptr-cell!
   INTERPRETER:INSTALL-EVALUATE
   INTERPRETER:INSTALL-CREATE
   INTERPRETER:INSTALL-POLICY
   COMMAND-LINE:INSTALL
   COMP:INSTALL ;

;package
