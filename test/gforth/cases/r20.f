\ A catch receives the underdepth (70); the caller's cell is intact, and the
\ program text's own residue is refused at its end.
variable V
: E ( -- ) s" V !" evaluate-closed ;
: T ( -- n ) ['] E catch ;
5 T . cr V @ . cr
