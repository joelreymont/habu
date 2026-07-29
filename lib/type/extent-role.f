\ extent-role.f - the converter surface for the core extent-index substrate.
\
\ Three cell families make up that substrate, and all three are registered by the
\ engine in src/core/type-family.f, in the global (empty) package:
\
\     ix<e>                  one cell, phantom argument e = the extent it indexes
\     extprod<free,inner>    an ordered product former: a folded (B,T) row type
\     redx<e>                an extent index marked as a summation axis
\
\ They live in the engine because the checker has a built-in rule about them:
\ EXT-REDX-BAD-ARG? at SIG-END-PARAM (src/core/checker.f) rejects a declared
\ signature that contracts a FREE (outer / batch) extent or a whole product, and
\ that rule reads the family ids captured right after registration. The engine
\ itself never converts one of these values, so this file - not the engine prefix
\ - carries the converter surface application code calls.
\
\ WHY THE CONVERTERS ARE HERE AND NOT IN package MAKI. Introducing a value into a
\ resolved cell family is authorized only from that family's declaring package
\ (src/core/checker.f CAST-OWNER?, reject code 7135 E-CAST-OWNER). `redx` is
\ declared by the engine in the global package, so a `CAST:` whose OUTPUT is a
\ redx must itself be declared at global scope. maki/extent.f used to declare
\ `CAST: >RED ( ix<e> -- redx<e> )` inside `package MAKI`; that is an application
\ package minting a core-owned nominal, and every load of the file threw 7135.
\ The fix is this file plus the move of `ix` into the same core registration as
\ its two siblings: one owner for the whole substrate, one place its values are
\ introduced. Projections OUT of a family are unrestricted, so IX>N would have
\ been legal anywhere - it lives here to keep the pair in one place.
\
\ This file is a documented core/prelude language surface (the package-first rule
\ in CLAUDE.md), like lib/type/deftype.f: it defines global words on purpose,
\ because the types it converts are global engine types. It has no private state,
\ so it needs no package of its own.
\
\ WHAT IS NOT HERE. The reverse direction - a plain n INTO an extent's index
\ space - stays per-extent and TRUSTED in maki/extent.f (`>#M ( n -- ix<extm> )`),
\ because its runtime range guard is not yet expressible as a checked body. Dot
\ habu-extent-bound-loop-a70a49b3 tracks the author-time binding that would
\ retire it.

\ IX>N ( ix<e> -- n ) : project any extent index back to a plain cell. Generic
\ over the extent, so one word serves every extent, and the direction is always
\ sound because a nominal cell IS a cell.
CAST: IX>N ( ix<e> -- n ) ;

\ >RED ( ix<e> -- redx<e> ) : mark an extent index as a summation (reduction)
\ axis. It cannot launder a free factor into a contraction: a word whose DECLARED
\ signature carries redx over a free extent, or over a whole product, is already a
\ load-time reject by the checker's contraction rule, independent of this cast.
CAST: >RED ( ix<e> -- redx<e> ) ;
