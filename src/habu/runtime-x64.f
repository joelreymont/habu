\ runtime-x64.f - the x86-64 Habu loop and its installs.
\
\ src/habu/native-runtime.f loads this file only for a linux-x86-64 product.
\ There the Habu loop (src/habu/interpret.f) is the engine's `evaluate`, the
\ definers (src/habu/definers.f) are its `create` and definition policy, and
\ src/habu/main.f is its command line. ARM64 keeps the kernel's evaluate,
\ startup and REPL bodies, so its product captures none of these files.
\
\ CHECKER-REG:SEAL runs CHECKER-REG:SEAL-TARGET on x86-64 only; this file
\ fills it with INSTALL-LOOP, which binds each of the four cells before the
\ capture seals them. test/prefix-provided-unfilled.f removes the first
\ install line here and expects the build to refuse the empty evaluate cell.

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
