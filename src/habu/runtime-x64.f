\ runtime-x64.f - the x86-64 Habu loop and its installs. native-runtime.f loads
\ it only for linux-x86-64: interpret.f is the engine's `evaluate`, definers.f
\ its `create` and definition policy, main.f its command line; ARM64 keeps the
\ kernel's bodies. CHECKER-REG:SEAL runs SEAL-TARGET, filled here by INSTALL-LOOP;
\ it binds the four cells before the capture seals them.
\ test/prefix-provided-unfilled.f drops the first install line: empty evaluate refused.

require src/habu/definers.f
require src/habu/interpret.f
require src/habu/main.f

package RUNTIME-X64

private

: INSTALL-LOOP ( -- )
   OUTER:INSTALL-EVALUATE
   OUTER:INSTALL-CREATE
   OUTER:INSTALL-POLICY
   ENGINE-MAIN:INSTALL ;

: BIND ( -- ) [: INSTALL-LOOP ;] is CHECKER-REG:SEAL-TARGET ;
BIND

;package
