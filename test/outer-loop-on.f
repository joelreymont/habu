\ outer-loop-on.f - every file after this one on a `--load` line is read by the
\ interpret loop written in Habu, src/habu/interpret.f OUTER:INTERPRET, instead of
\ the engine's `evaluate`. The switch is the loaded-bytes seam
\ SOURCE-ROOT:INCLUDE-INTERPRET, which src/core/include.f binds to the engine.

require src/habu/interpret.f

package OUTER-LOOP-ON

: OUTER-LOOP-ON ( -- ) [: OUTER:INTERPRET ;] is SOURCE-ROOT:INCLUDE-INTERPRET ;
OUTER-LOOP-ON

;package
