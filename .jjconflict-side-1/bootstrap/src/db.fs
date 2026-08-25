\ db.fs — the effect database. Each charted word is an entry in the EFFECTS
\ wordlist whose body holds its scheme string ([len][bytes]). Schemes are stored
\ as canonical text (RENDER-EFFECT), so they survive the per-check arena reset.

wordlist constant EFFECTS

\ Store the scheme string (sa su) into the body of the just-CREATEd word.
: ,SCHEME  ( sa su -- )            \ append [len][bytes] to the current body
   {: sa su :}
   su ,                            \ length cell
   sa  here  su chars dup allot  move ;

: CHART  ( eff c-addr u -- )            \ render eff, store under name c-addr u
   {: eff na nu :}
   eff RENDER-EFFECT {: sa su :}
   get-current >r  EFFECTS set-current
   na nu nextname  create               \ entry in EFFECTS
   r> set-current
   sa su ,SCHEME ;

: EFFECT-OF  ( c-addr u -- sa su | 0 )  \ scheme string for a charted name, or 0
   EFFECTS search-wordlist if
      >body  dup @  swap cell+  swap     ( sa su )
   else 0 then ;
