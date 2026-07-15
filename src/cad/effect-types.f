\ effect-types.f - the finite CAD semantic-effect vocabulary and its conservative
\ truth tables (MODEL-CAD-V2-PLAN.md R8; dot habu-define-finite-cad-0bdf52ad,
\ epic habu-v2-types-finite-18bb1b35).
\
\ A CAD operation is legal to duplicate, reorder, fuse, or cache ONLY as far as
\ its SEMANTIC effect allows. A correct Forth stack effect proves nothing about
\ that: a balanced `( -- )` word can read a parameter, mutate device state, or
\ publish an artifact. This file names the finite set of semantic effect atoms so
\ the row algebra (src/cad/effect.f) and the later legality owners
\ (habu-enforce-effect-aware-cf9181b8, habu-persist-cad-semantic-028c0881) can
\ reason over them instead of trusting an author.
\
\ There are ten atoms. `pure` is special: it is the absence of any effect and
\ never appears as a row binding - the unique canonical empty row (PURE in
\ effect.f) carries no atom. The nine effectful atoms bind row entries:
\ parameter-read, state-write, random, host-io, device-launch, atomic,
\ collective, allocation, publication. The set is CLOSED: a general effect
\ calculus is not required, only this finite CAD vocabulary.
\
\ FOUR conservative truth tables answer distinct legality questions over the
\ atoms; `?` predicates take a typed `effect-atom` (COMMUTE? a symmetric pair):
\   DUP-OK?     - may an op with this atom be duplicated / recomputed?
\   CACHEABLE?  - may its result be memoised on resolved inputs?
\   BARRIER?    - is it a conservative reorder / fusion barrier?
\   COMMUTE?    - may two atoms be reordered past each other?
\ Conservative = safe over-approximation: when unsure, forbid. Only PURE and
\ immutable digest-bound parameter reads duplicate/cache/non-barrier; only pure
\ (with anything) and two parameter reads commute. Every other atom or pair is a
\ barrier / non-commuting, so a stateful, random, IO, device, atomic, collective,
\ allocation, or publication op can never be silently treated as pure. For this
\ closed conservative vocabulary DUP-OK?, CACHEABLE?, and the negation of
\ BARRIER? happen to coincide on the {pure, parameter-read} safe set; they stay
\ independent tables because they answer distinct questions and a later
\ resolved-binding refinement (e.g. a digest-bound deterministic device launch)
\ may diverge them.
\
\ No `require`: the package / ENUM / MATCH grammar is in the checker prefix
\ (cf. lib/cad-num-types.f, maki/cad-kinds.f). The finite atom/kind sets are real
\ payloadless sum types, so no raw `n` can stand in for an atom or a slot kind and
\ MATCH stays exhaustive. No production entry loads this file yet; src/cad/effect.f
\ (the row algebra) reopens package CAD-EFFECT and consumes the private code-level
\ tables, and the focused suites are its only other consumers.

\ ---- named refusal codes (throw at the boundary that rejects) -----------------
-5600 constant E-CADEFF-ATOM        \ EMIT of the pure atom, or a decoded atom code outside 1..9
-5601 constant E-CADEFF-KIND        \ a decoded slot-kind code outside 0..3
-5602 constant E-CADEFF-INDEX       \ a negative slot index
-5603 constant E-CADEFF-SITE        \ a negative call-site ordinal (REMAP)
-5604 constant E-CADEFF-MALFORMED   \ a row binding does not decode to a valid effect binding
-5605 constant E-CADEFF-DUPLICATE   \ a direct duplicate binding inserted in one build transaction

package CAD-EFFECT
public

\ ---- the finite semantic effect vocabulary ------------------------------------
ENUM effect-atom
   pure param-read state-write random host-io
   device-launch atomic collective allocation publication
;ENUM

\ ---- the finite semantic slot kinds -------------------------------------------
ENUM slot-kind operand attribute capability capture ;ENUM

\ ---- readable atom constructors (over the generated escaped ctor spellings) ----
: PURE-ATOM ( -- effect-atom )      CAD--EFFECT-EFFECT--ATOM:PURE ;
: PARAM-READ ( -- effect-atom )     CAD--EFFECT-EFFECT--ATOM:PARAM-READ ;
: STATE-WRITE ( -- effect-atom )    CAD--EFFECT-EFFECT--ATOM:STATE-WRITE ;
: RANDOM ( -- effect-atom )         CAD--EFFECT-EFFECT--ATOM:RANDOM ;
: HOST-IO ( -- effect-atom )        CAD--EFFECT-EFFECT--ATOM:HOST-IO ;
: DEVICE-LAUNCH ( -- effect-atom )  CAD--EFFECT-EFFECT--ATOM:DEVICE-LAUNCH ;
: ATOMIC ( -- effect-atom )         CAD--EFFECT-EFFECT--ATOM:ATOMIC ;
: COLLECTIVE ( -- effect-atom )     CAD--EFFECT-EFFECT--ATOM:COLLECTIVE ;
: ALLOCATION ( -- effect-atom )     CAD--EFFECT-EFFECT--ATOM:ALLOCATION ;
: PUBLICATION ( -- effect-atom )    CAD--EFFECT-EFFECT--ATOM:PUBLICATION ;

\ ---- readable slot-kind constructors ------------------------------------------
: OPERAND ( -- slot-kind )     CAD--EFFECT-SLOT--KIND:OPERAND ;
: ATTRIBUTE ( -- slot-kind )   CAD--EFFECT-SLOT--KIND:ATTRIBUTE ;
: CAPABILITY ( -- slot-kind )  CAD--EFFECT-SLOT--KIND:CAPABILITY ;
: CAPTURE ( -- slot-kind )     CAD--EFFECT-SLOT--KIND:CAPTURE ;

private

\ ---- the single atom -> finite code mapping (source of the numeric encoding) ---
\ Codes are the stable identity written into a row's path segments (effect.f). 0 =
\ pure is never a binding; 1..9 are the bindable effectful atoms in declaration
\ order.
: ATOM>CODE ( effect-atom -- n )
   MATCH effect-atom
      pure OF 0 ENDOF               param-read OF 1 ENDOF
      state-write OF 2 ENDOF        random OF 3 ENDOF
      host-io OF 4 ENDOF            device-launch OF 5 ENDOF
      atomic OF 6 ENDOF            collective OF 7 ENDOF
      allocation OF 8 ENDOF        publication OF 9 ENDOF
   ;MATCH ;

: KIND>CODE ( slot-kind -- n )
   MATCH slot-kind
      operand OF 0 ENDOF      attribute OF 1 ENDOF
      capability OF 2 ENDOF   capture OF 3 ENDOF
   ;MATCH ;

\ ---- code validity (consumed by effect.f's EMIT guard and wire classifier) -----
: EFFECT-CODE? ( n -- bool )   dup 1 >= swap 9 <= and ;   \ a bindable effectful atom
: KIND-CODE? ( n -- bool )     dup 0 >= swap 3 <= and ;   \ a slot kind

\ ---- the four conservative truth tables over atom codes -----------------------
: DUP-CODE? ( n -- bool )      dup 0= swap 1 = or ;       \ pure or parameter-read
: CACHE-CODE? ( n -- bool )    dup 0= swap 1 = or ;       \ pure or parameter-read
: BARRIER-CODE? ( n -- bool )  1 > ;                       \ effectful (code >= 2)
: COMMUTE-CODE? ( n n -- bool ) {: a:n b:n :}
   a 0= if 0 0= exit then                                  \ pure commutes with anything
   b 0= if 0 0= exit then
   a 1 = b 1 = and ;                                       \ else only two parameter-reads

public

\ ---- atom-level truth tables (the public finite tables) -----------------------
: DUP-OK? ( effect-atom -- bool )     ATOM>CODE DUP-CODE? ;
: CACHEABLE? ( effect-atom -- bool )  ATOM>CODE CACHE-CODE? ;
: BARRIER? ( effect-atom -- bool )    ATOM>CODE BARRIER-CODE? ;
: COMMUTE? ( effect-atom effect-atom -- bool )
   ATOM>CODE swap ATOM>CODE COMMUTE-CODE? ;

;package
