\ runtime.f - the Habu loop's installs, one list for every machine: interpret.f
\ is the engine's `evaluate`, definers.f its `create` and definition policy,
\ main.f its command line and T0 its compiler. INSTALL stores their xts in the
\ engine's cells; in a build those stores reach the product being written.

require src/habu/t0.f
require src/habu/definers.f
require src/habu/interpret.f
require src/habu/main.f

package RUNTIME

public

: INSTALL ( -- )
   OUTER:INSTALL-EVALUATE
   OUTER:INSTALL-CREATE
   OUTER:INSTALL-POLICY
   COMMAND-LINE:INSTALL
   COMP:INSTALL ;

;package
