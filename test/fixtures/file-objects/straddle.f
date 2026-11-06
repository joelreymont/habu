\ straddle.f - stores an xt into a cell that straddles count.f's TALLY and LATER,
\ which its object cannot place: the build refuses it E-COMP-STATE.
: STRADDLE-TOP ( -- n ) 4 ;
' STRADDLE-TOP FILE-OBJECTS-COUNT:TALLY 4 + xt!
