\ maki/eval-repair-mech-test.f - the mechanical checker-guided repairer suite.
\
\ Each repair class gets a candidate with one known defect of that class; the
\ mechanical loop must reach the probed verdict with the probed round/token
\ counts (real checker verdicts, no author in the loop). add_producer and
\ fix_type are honest UNREPAIRABLE results: their packets carry no insertable
\ producer / typed replacement token (packet-capability gaps tracked by dots).
\ The SAXPY section runs the same surplus-load kernel through the mechanical
\ arm and the author arm (maki/eval-repair.f pattern) for A/B comparison.

require maki/eval-repair-mech.f

T-RESET

\ ---- fixtures ---------------------------------------------------------------
: MRT-REMOVE$ ( -- ptr u8 n )
   s" MRT-REMOVE ( i64 -- i64 ) dup" ;
: MRT-REMOVE2$ ( -- ptr u8 n )
   s" MRT-REMOVE2 ( i64 -- i64 ) dup dup" ;
: MRT-RSTACK$ ( -- ptr u8 n )
   s" MRT-RSTACK ( i64 -- i64 ) >r" ;
: MRT-ADD$ ( -- ptr u8 n )
   s" MRT-ADD ( i64 -- i64 ) drop" ;
: MRT-TYPE$ ( -- ptr u8 n )
   s" MRT-TYPE ( i64 -- i64 ) 0=" ;
: MRT-UNDEF$ ( -- ptr u8 n )
   s" MRT-UNDEF ( i64 -- i64 ) MRT-NO-SUCH-WORD" ;
: MRT-INSURPLUS$ ( -- ptr u8 n )
   s" MRT-INSURPLUS ( i64 -- )" ;
: MRT-CAP$ ( -- ptr u8 n )   \ 10 surplus producers > MECH-MAX-ROUNDS
   s" MRT-CAP ( i64 -- i64 ) dup dup dup dup dup dup dup dup dup dup" ;

\ ---- remove_producer: surplus producer deleted mechanically -> green --------
s" mech remove verdict" T-LABEL    MRT-REMOVE$ EVAL:MECH-REPAIR EVAL:MECH-GREEN T=
s" mech remove green" T-LABEL      EVAL:REPAIR-GREEN? TTRUE
s" mech remove rounds" T-LABEL     EVAL:REPAIR-ROUNDS@ 1 T=
s" mech remove tokens" T-LABEL     EVAL:REPAIR-TOKENS@ 13 T=   \ 7 draft + 6 green
s" mech remove class" T-LABEL      EVAL:MECH-CLASS$ s" remove_producer" T$=
s" mech remove source" T-LABEL     EVAL:MECH-SOURCE$ s" MRT-REMOVE ( i64 -- i64 ) " T$=
s" mech remove certifies" T-LABEL  EVAL:MECH-SOURCE$ EVAL:CHECK-PASSES? TTRUE

\ ---- remove_producer x2: one deletion per checker round -> green ------------
s" mech remove2 verdict" T-LABEL   MRT-REMOVE2$ EVAL:MECH-REPAIR EVAL:MECH-GREEN T=
s" mech remove2 rounds" T-LABEL    EVAL:REPAIR-ROUNDS@ 2 T=
s" mech remove2 tokens" T-LABEL    EVAL:REPAIR-TOKENS@ 21 T=   \ 8 + 7 + 6
s" mech remove2 certifies" T-LABEL EVAL:MECH-SOURCE$ EVAL:CHECK-PASSES? TTRUE

\ ---- fix_return_stack: surplus transfer deleted mechanically -> green -------
s" mech rstack verdict" T-LABEL    MRT-RSTACK$ EVAL:MECH-REPAIR EVAL:MECH-GREEN T=
s" mech rstack rounds" T-LABEL     EVAL:REPAIR-ROUNDS@ 1 T=
s" mech rstack tokens" T-LABEL     EVAL:REPAIR-TOKENS@ 13 T=   \ 7 draft + 6 green
s" mech rstack class" T-LABEL      EVAL:MECH-CLASS$ s" fix_return_stack" T$=
s" mech rstack source" T-LABEL     EVAL:MECH-SOURCE$ s" MRT-RSTACK ( i64 -- i64 ) " T$=
s" mech rstack certifies" T-LABEL  EVAL:MECH-SOURCE$ EVAL:CHECK-PASSES? TTRUE

\ ---- add_producer: packet carries no producer token -> unrepairable ---------
s" mech add verdict" T-LABEL       MRT-ADD$ EVAL:MECH-REPAIR EVAL:MECH-UNREPAIRABLE T=
s" mech add not green" T-LABEL     EVAL:REPAIR-GREEN? TFALSE
s" mech add rounds" T-LABEL        EVAL:REPAIR-ROUNDS@ 1 T=
s" mech add class" T-LABEL         EVAL:MECH-CLASS$ s" add_producer" T$=

\ ---- fix_type: packet carries no typed replacement token -> unrepairable ----
s" mech type verdict" T-LABEL      MRT-TYPE$ EVAL:MECH-REPAIR EVAL:MECH-UNREPAIRABLE T=
s" mech type not green" T-LABEL    EVAL:REPAIR-GREEN? TFALSE
s" mech type rounds" T-LABEL       EVAL:REPAIR-ROUNDS@ 1 T=
s" mech type class" T-LABEL        EVAL:MECH-CLASS$ s" fix_type" T$=

\ ---- classes outside the modeled set fail closed ----------------------------
s" mech undef verdict" T-LABEL     MRT-UNDEF$ EVAL:MECH-REPAIR EVAL:MECH-UNREPAIRABLE T=
s" mech undef class" T-LABEL       EVAL:MECH-CLASS$ s" unknown_rejection" T$=

\ ---- remove_producer flagging the name token (input surplus) fails closed ---
s" mech insurplus verdict" T-LABEL MRT-INSURPLUS$ EVAL:MECH-REPAIR EVAL:MECH-UNREPAIRABLE T=
s" mech insurplus class" T-LABEL   EVAL:MECH-CLASS$ s" remove_producer" T$=
s" mech insurplus rounds" T-LABEL  EVAL:REPAIR-ROUNDS@ 1 T=

\ ---- round cap: more surplus than budget -> capped, not green ---------------
s" mech cap verdict" T-LABEL       MRT-CAP$ EVAL:MECH-REPAIR EVAL:MECH-CAPPED T=
s" mech cap not green" T-LABEL     EVAL:REPAIR-GREEN? TFALSE
s" mech cap rounds" T-LABEL        EVAL:REPAIR-ROUNDS@ EVAL:MECH-MAX-ROUNDS T=
s" mech cap tokens" T-LABEL        EVAL:REPAIR-TOKENS@ 100 T=  \ 16+15+...+9

\ ---- SAXPY surplus-load kernel: mechanical arm vs author arm ----------------
: MRT-SAXPY-BAD$ ( -- ptr u8 n )   \ remove_producer: trailing surplus load
   s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD a SCALE y g LOAD +. y g STORE x g LOAD" ;
: MRT-SAXPY-GREEN$ ( -- ptr u8 n )
   s" K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- ) {: x y a :} x GRID-CTX {: g :} x g LOAD a SCALE y g LOAD +. y g STORE" ;

variable MRT-MECH-R  variable MRT-MECH-T
variable MRT-AUTH-R  variable MRT-AUTH-T

\ mechanical arm: LOAD, then the dangling g and x, deleted one round each
s" mech saxpy verdict" T-LABEL     MRT-SAXPY-BAD$ EVAL:MECH-REPAIR EVAL:MECH-GREEN T=
s" mech saxpy rounds" T-LABEL      EVAL:REPAIR-ROUNDS@ 3 T=
s" mech saxpy certifies" T-LABEL   EVAL:MECH-SOURCE$ EVAL:CHECK-PASSES? TTRUE
EVAL:REPAIR-ROUNDS@ MRT-MECH-R !
EVAL:REPAIR-TOKENS@ MRT-MECH-T !

\ author arm: the packet-guided author removes the surplus load in one round
EVAL:REPAIR-RESET
MRT-SAXPY-BAD$ EVAL:REPAIR-STEP
MRT-SAXPY-GREEN$ EVAL:REPAIR-STEP
s" author saxpy green" T-LABEL     EVAL:REPAIR-GREEN? TTRUE
s" author saxpy rounds" T-LABEL    EVAL:REPAIR-ROUNDS@ 1 T=
EVAL:REPAIR-ROUNDS@ MRT-AUTH-R !
EVAL:REPAIR-TOKENS@ MRT-AUTH-T !

\ the mechanical arm converges but pays more rounds/tokens than the author
s" mech-vs-author rounds" T-LABEL  MRT-MECH-R @ MRT-AUTH-R @ > TTRUE
s" mech-vs-author tokens" T-LABEL  MRT-MECH-T @ MRT-AUTH-T @ > TTRUE

s" == mechanical vs author (SAXPY surplus-load) ==" type cr
s"   mech:   repair-rounds=" type MRT-MECH-R @ . s"  tokens-to-green=" type MRT-MECH-T @ . cr
s"   author: repair-rounds=" type MRT-AUTH-R @ . s"  tokens-to-green=" type MRT-AUTH-T @ . cr

T-REPORT
