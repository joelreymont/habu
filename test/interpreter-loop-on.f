\ interpreter-loop-on.f - files after it on a --load line are read by
\ INTERPRETER:EVAL-FRAMED-RANGE, so arm64 exercises the entry a sealed x86-64
\ engine reads every loaded file through.

require src/habu/interpreter.f

package INTERPRETER-LOOP-ON

private

: HABU-LOOP-ON ( -- )
   [: INTERPRETER:EVAL-FRAMED-RANGE ;] SOURCE-ROOT:USE-INTERPRETER ;
HABU-LOOP-ON

;package
