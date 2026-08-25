\ diag-state.fs — diagnostic record: filled before a THROW (by unify/checker),
\ read by the formatter (diag.fs). Strings are copied in immediately because a
\ parse-name address dies at the next parse.

128 constant DIAG-BUF
create CUR-WORD-BUF   DIAG-BUF chars allot
create CUR-TOKEN-BUF  DIAG-BUF chars allot
variable CUR-WORD-LEN
variable CUR-TOKEN-LEN
variable DIAG-EXP      \ expected term (type or stack), per error class
variable DIAG-ACT      \ actual term
variable DIAG-CODE     \ THROW code being reported

: CUR-WORD!  ( c-addr u -- )
   DIAG-BUF min  dup CUR-WORD-LEN !  CUR-WORD-BUF swap move ;
: CUR-TOKEN! ( c-addr u -- )
   DIAG-BUF min  dup CUR-TOKEN-LEN !  CUR-TOKEN-BUF swap move ;
: CUR-WORD@   ( -- c-addr u )  CUR-WORD-BUF  CUR-WORD-LEN @ ;
: CUR-TOKEN@  ( -- c-addr u )  CUR-TOKEN-BUF CUR-TOKEN-LEN @ ;

: DIAG-EXP!  ( x -- )  DIAG-EXP ! ;
: DIAG-ACT!  ( x -- )  DIAG-ACT ! ;
: DIAG-CODE! ( n -- )  DIAG-CODE ! ;
: DIAG-EXP@  ( -- x )  DIAG-EXP @ ;
: DIAG-ACT@  ( -- x )  DIAG-ACT @ ;
: DIAG-CODE@ ( -- n )  DIAG-CODE @ ;

\ Convenience: fill expected/actual/code in one shot before THROW.
: DIAG!  ( exp act code -- )  DIAG-CODE!  DIAG-ACT!  DIAG-EXP! ;
