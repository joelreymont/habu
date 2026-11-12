\ straddle.f - a build's first file: stores an xt into a cell straddling the
\ header's APP-ENTRY:XT-CELL ($43A0) and the next, which its object cannot
\ place: the build refuses it E-COMP-STATE.
: STRADDLE-TOP ( -- n ) 4 ;
' STRADDLE-TOP data-base $43A4 + xt!
