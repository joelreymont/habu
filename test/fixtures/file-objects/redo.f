\ redo.f - one part defines Y and X, which calls Y, undefines both and defines them again,
\ Y answering 2 the second time.

package FILE-OBJECTS-REDO
public
: Y ( -- n ) 1 ;
: X ( -- n ) Y ;
undefine X  undefine Y
: Y ( -- n ) 2 ;
: X ( -- n ) Y ;
;package
