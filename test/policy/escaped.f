\ Escaped strings have the same span effect in a body and at top level.
: PART-NAME ( -- ptr u8 n ) S\" Bracket \\ \"Café\"" ;
PART-NAME PDEP:SHOW-NAME
S\" Bracket \\ \"Café\"" PDEP:SHOW-NAME
