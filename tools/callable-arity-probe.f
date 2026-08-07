\ callable-arity-probe.f - what the running engine's dictionary offers the native
\ chain, counted rather than argued.
\
\ WHY THIS EXISTS. src/compiler/native/dict.f answers two questions about a
\ spelling - may a call branch there (CALL-TARGET) and how many cells does it
\ move (SPELL-ARITY) - and its header states, as a measurement and not an
\ opinion, how many of the engine's own records answer both and which ones
\ answer only one. That measurement moves whenever either answer changes, and a
\ number written into a comment cannot be re-checked by reading it. So the walk
\ that produced it lives here, runs on demand, and prints the names in the
\ disagreeing buckets rather than only their count. Dot
\ habu-export-the-checker-2bbc831c re-ran it when the checker began publishing
\ cell widths.
\
\ WHAT IT ASKS, AND IN WHOSE SCOPE. Every record from 0 to ndict, by its own
\ spelling, through the same two published entries the chain's elaborator calls.
\ The scope is the probe's own - no package open - which is exactly the scope a
\ top-level caller compiles in, and it means a PRIVATE word of a package nothing
\ has open answers absent here. That is not a defect in the count: an unqualified
\ private spelling is unresolvable for the chain too, and the bucket it lands in
\ says so by name.
\
\ Run: bin/hb --load tools/callable-arity-probe.f

require lib/prelude.f
require lib/errors.f
require src/compiler/native/dict.f

package CALLABLE-ARITY-PROBE

private

variable N-BOTH        \ callable and sized: the chain can compile a call
variable N-CALL-ONLY   \ callable, width not stated
variable N-ARITY-ONLY  \ width stated, not callable
variable N-NEITHER
variable N-RETIRED
variable N-SEEN
variable N-START-BOTH  \ the looser pair dict.f's header quotes: SPELL-START and sized

\ The callable-but-unsized bucket has two causes and they are not the same
\ finding. The checker may hold NO effect for the name - a signature the seal
\ truncated, a word defined before the checker saw it - and then there is no
\ width to state and never was. Or it may hold one whose width it declines to
\ state, which is the fail-closed case src/core/checker.f EFF-ROW-CELLS answers
\ CELLS-NONE for. Only the second is a capability gap, so they are counted apart.
variable N-NO-EFFECT
variable N-UNSIZEABLE

\ One boundary, the shape src/compiler/native/reach.f and the census both use:
\ the checker's effect readers are name-stripped past the seal, so a checked
\ caller reaches them only as compiled calls behind a declared signature.
TRUSTED: HAS-EFFECT? ( ptr u8 n -- bool )
   EFFECT-QUERY ;

\ The listing is capped so a probe on a big image stays readable; the COUNTS are
\ never capped, so a cap reached is visible as a bucket bigger than its listing.
32 constant LIST-MAX
variable N-LISTED

: LIST-RESET ( -- )   0 N-LISTED ! ;

: LIST-ONE ( ptr u8 n -- )
   N-LISTED @ LIST-MAX < if
      ."     " type cr
      N-LISTED @ 1+ N-LISTED !
   else 2drop then ;

: TALLY ( ptr u8 n -- )
   {: a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   N-SEEN @ 1+ N-SEEN !
   a u NDICT:CALL-TARGET 0 <> {: callable:bool :}
   a u NDICT:SPELL-ARITY drop NDICT:ARITY-NONE <> {: sized:bool :}
   a u NDICT:SPELL-START 0 <> sized and if
      N-START-BOTH @ 1+ N-START-BOTH !
      callable 0= if a u LIST-ONE then
   then
   callable sized and if N-BOTH @ 1+ N-BOTH ! exit then
   callable if
      N-CALL-ONLY @ 1+ N-CALL-ONLY !
      a u HAS-EFFECT? if
         N-UNSIZEABLE @ 1+ N-UNSIZEABLE !
         a u LIST-ONE
      else N-NO-EFFECT @ 1+ N-NO-EFFECT ! then
      exit then
   sized if N-ARITY-ONLY @ 1+ N-ARITY-ONLY ! exit then
   N-NEITHER @ 1+ N-NEITHER ! ;

public

: RUN ( -- )
   0 N-BOTH !  0 N-CALL-ONLY !  0 N-ARITY-ONLY !
   0 N-NEITHER !  0 N-RETIRED !  0 N-SEEN !
   0 N-START-BOTH !  0 N-NO-EFFECT !  0 N-UNSIZEABLE !
   ." -- callable / sized, over every dictionary record ----------" cr
   ."   sized, but a call may not branch there:" cr
   LIST-RESET
   0 begin dup ndict@ < while
      dup XREF-REC {: rec:ptr :}
      rec XREF-RETIRED? if N-RETIRED @ 1+ N-RETIRED !
      else rec XREF-NAME$ TALLY then
      1+
   repeat drop
   ." -- counts ---------------------------------------------------" cr
   ."   records          " ndict@ . cr
   ."   retired          " N-RETIRED @ . cr
   ."   asked            " N-SEEN @ . cr
   ."   callable+sized   " N-BOTH @ . cr
   ."   start+sized      " N-START-BOTH @ . cr
   ."   callable only    " N-CALL-ONLY @ . cr
   ."     no effect held " N-NO-EFFECT @ . cr
   ."     unsizeable     " N-UNSIZEABLE @ . cr
   ."   sized only       " N-ARITY-ONLY @ . cr
   ."   neither          " N-NEITHER @ . cr ;

;package

CALLABLE-ARITY-PROBE:RUN
