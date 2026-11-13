\ fixed-body.f - COMP:body, which pattern a fixed body uses. A fixed body is the
\ machine code the compiler stamps from a pattern for a word with no source
\ text: a constant pushes its number, create and variable push their data
\ address, defer jumps through its cell, CAST: passes the value through, a
\ field child's offset adds its number to the top cell, an index child's index
\ pops n and adds n times its number to the pointer below; compiled is an
\ ordinary body. The engine's machine code passes the tag as a number through
\ NCOMP-DISPATCH:FIXED-SHADOW-CELL.

require src/lib/prelude.f

package COMP
public
ENUM body DERIVE eq compiled value address defer cast offset index ;ENUM
CAST: BODY>N ( body -- n )
;package
