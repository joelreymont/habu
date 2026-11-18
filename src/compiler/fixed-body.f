split-str
\ fixed-body.f - COMP:body, which pattern a fixed body uses. A fixed body is the
\ machine code the compiler stamps from a pattern for a word with no source
\ text: a constant pushes its number, create and variable push their data
\ address, defer jumps through its cell, CAST: passes the value through, a
\ field child's offset adds its number to the top cell, an index child's index
\ pops n and adds n times its number to the pointer below, a routine pushes
\ its data address and calls its fixed routine, a getter pushes its field's
\ cells read from the base of the view on top and a setter stores them there,
\ a bytes child adds its BUFFER field's offset to the top cell and pushes its
\ byte count; compiled is an ordinary body. A new kind goes last: a kind's
\ number is its order.

require src/lib/prelude.f

package COMP
public
ENUM body DERIVE eq compiled value address deferred cast offset index routine getter setter bytes ;ENUM
;package
