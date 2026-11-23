\ Escaped strings have the same span effect in a body and at top level.
: PART-NAME ( -- str ) S\" Bracket \\ \"Café\"" ;
PART-NAME PDEP:SHOW-NAME
S\" Bracket \\ \"Café\"" PDEP:SHOW-NAME
