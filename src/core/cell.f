\ cell.f - target cell-width constant and load-time invariant.

$8 constant CELL
$4C constant E-CELL-WIDTH

: CELL-WIDTH-CHECK ( -- )
   1 cells CELL <> if
      s" cell: target width mismatch" E-CELL-WIDTH die
   then ;

CELL-WIDTH-CHECK
