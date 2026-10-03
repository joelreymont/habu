\ outer-loop-on.f - every file after this one on a `--load` line is read by the
\ interpret loop written in Habu, src/habu/interpret.f OUTER:INTERPRET, instead of
\ the engine's `evaluate`. The switch is the loaded-bytes seam
\ SOURCE-ROOT:INCLUDE-INTERPRET, which src/core/include.f binds to the engine's
\ loop inside the closed boundary, INCLUDE-EVALUATE. The Habu loop runs inside
\ the same boundary, so a loaded file is a closed program under either loop: its
\ floor is its loader's depth and a cell it leaves is refused E-EVAL-RESIDUE.

require src/habu/interpret.f

package OUTER-LOOP-ON

PTR-VARIABLE SRC-A
variable SRC-U

public

\ The loaded bytes are read before the loop runs, so a load nested in them can
\ store its own. LOADED is immediate: a file an immediate word loads while a
\ definition is open reaches the loop then too, as the engine's loop reads such
\ a file into the definition, where a plain word would compile into it.
: LOADED ( -- )
   SRC-A @ SRC-U @ OUTER:INTERPRET ; immediate
s" OUTER-LOOP-ON:LOADED" 0 parse-imm

private

\ evaluate-closed takes text, so the boundary runs the word that reads the bytes.
: CLOSED ( ptr u8 n -- )
   SRC-U ! SRC-A !
   s" OUTER-LOOP-ON:LOADED" evaluate-closed ;

: OUTER-LOOP-ON ( -- ) [: CLOSED ;] is SOURCE-ROOT:INCLUDE-INTERPRET ;
OUTER-LOOP-ON

;package
