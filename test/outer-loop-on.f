\ outer-loop-on.f - every file after this one on a `--load` line is read by the
\ interpret loop written in Habu instead of the engine's `evaluate-range`. The
\ switch is the loaded-bytes seam SOURCE-ROOT:USE-INTERPRETER, which
\ src/core/include.f RUN-RANGE calls inside LOAD-UNIT's source-unit-run, with
\ the same require rollback and residue check (CALL-UNIT) as the engine's loop.
\ It binds the Habu loop's framed entry, src/habu/interpret.f
\ OUTER:EVAL-FRAMED-RANGE, the one a sealed x86-64 engine reads every loaded
\ file through as its `evaluate-range`, so the switch exercises on ARM64 the
\ entry x86-64 uses.

require src/habu/interpret.f

package OUTER-LOOP-ON

private

: OUTER-LOOP-ON ( -- ) [: OUTER:EVAL-FRAMED-RANGE ;] SOURCE-ROOT:USE-INTERPRETER ;
OUTER-LOOP-ON

;package
