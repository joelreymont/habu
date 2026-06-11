\ prim-demo.fs — exercises the expanded prim table / PARSE-SIG. The check hook prints
\ each definition's verdict: -1 well-typed, 0 type error, 1 uncheckable. Expected:
\ -1 -1 -1 0 0 -1 1  then 42.
: HOOK CHECK dup . ; ' HOOK set-check
: T1 swap drop ;
: T2 over nip ;
: T3 dup 1+ + ;
: T4 dup 0= + ;
: T5 0= 1+ ;
: T6 5 dup + ;
: T7 dup 0 < if drop then ;
42 .
