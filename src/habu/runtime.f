\ runtime.f - the Habu loop's installs, one list for every machine:
\ interpreter.f is the engine's `evaluate`, `create` and definition policy,
\ main.f its command line and T0 its compiler, after the
\ native compiler's cells (NCOMP:INSTALL). INSTALL stores their xts in the
\ engine's cells; in a build those stores reach the product being written.

require src/compiler/native/compiler.f
require src/habu/t0.f
require src/habu/interpreter.f
require src/habu/main.f

package RUNTIME

public

: INSTALL ( -- )
   NCOMP:INSTALL
   OUTER:INSTALL-EVALUATE
   OUTER:INSTALL-CREATE
   OUTER:INSTALL-POLICY
   COMMAND-LINE:INSTALL
   COMP:INSTALL ;

;package
