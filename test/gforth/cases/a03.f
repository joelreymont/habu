-1 DIAG-JSON!
: F ( n -- n ) 10 0 ?do dup i = if unloop exit then loop ;
\ G loops around F: F's exit unloops its own frame first, so G's i and loop
\ find G's frame.
: G ( -- n ) 0 4 0 ?do 2 F drop i + loop ;
3 F . 20 F . G .
