$8 constant CELL
$4C constant CORE-LAYOUT-RC

: CELL-WIDTH-CHECK ( -- )
   1 cells CELL <> if
      s" cell: target width mismatch" CORE-LAYOUT-RC die
   then ;

CELL-WIDTH-CHECK
