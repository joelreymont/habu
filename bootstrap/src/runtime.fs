\ runtime.fs — runtime implementations for charted combinators that Gforth does
\ not provide, so a charted combinator both TYPE-CHECKS (effect in the DB) and
\ RUNS. Loaded with native `:` (before the override). Effects must match prims.fs.

: DIP  ( …r a xt -- …s a )    swap >r execute r> ;        \ R a [ R -- S ] -- S a
: KEEP ( …r a xt -- …s a )    over >r execute r> ;        \ R a [ R a -- S ] -- S a

: TIMES {: n xt :}        n 0 ?do xt execute loop ;
: BI    {: a p q :}       a p execute a q execute ;
: TRI   {: a p q r :}     a p execute a q execute a r execute ;
: EACH  {: p n xt :}      n 0 ?do p i cells + @ xt execute loop ;
: MAP   {: p n xt :}      n 0 ?do p i cells + dup @ xt execute swap ! loop ;
: FOLD  {: p n acc xt :}  acc n 0 ?do p i cells + @ xt execute loop ;
