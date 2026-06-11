\ prim-demo.fs — exercises the expanded prim table / PARSE-SIG. The check hook prints
\ each definition's verdict: -1 well-typed, 0 type error (float/int mixes), 1
\ uncheckable (execute is unmodeled). Expected: -1 -1 -1 0 0 -1 1  then 42.
: HOOK CHECK dup . ; ' HOOK set-check
: T1 swap drop ;
: T2 over nip ;
: T3 dup 1+ + ;
: T4 1.5 1 f+ ;
: T5 1.5 0= ;
: T6 5 dup + ;
: T7 dup execute ;
42 .
