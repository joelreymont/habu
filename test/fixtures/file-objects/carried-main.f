\ carried-main.f - its top level calls a word whose call is rejected: the
\ build prints that record alone.

: W ( -- ) s" " SOURCE-ROOT:CANON-OS drop drop ;

W

: MAIN ( -- ) ;
