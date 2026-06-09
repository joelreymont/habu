\ unify.fs — the three mutually-recursive unifiers + the deep occurs seams.
\ Schemes/instantiation (PARSE-SIG) and rendering (GENERALIZE) live in sig.fs.

defer UNIFY-TYPE   ( a b -- )       \ forward (filled below; mutual with UNIFY-ROW)
defer UNIFY-ROW    ( s1 s2 -- )

\ --- occurs (fill the forward.fs seams) ---
\ A type var occurs in a stack if it occurs in any pushed top type.
: OCCURS-TYPE-STACK  ( id s -- f )
   RESOLVE-ROW {: id s :}
   s SROW? if false exit then
   id s STACK-TOP  OCCURS-TYPE
   id s STACK-REST RECURSE  or ;

:noname  ( id t -- f )              \ OCCURS-TYPE: type var id occurs in type t?
   RESOLVE-TYPE {: id t :}
   t TYVAR? if id t TERM>PAYLOAD = exit then
   t TYCON? if false exit then
   t TERM>TAG T-PTR = if id t PTR>INNER OCCURS-TYPE exit then
   t QUOT>EFF {: e :}              \ T-QUOT: descend the four rows
   id e EFF>DIN  OCCURS-TYPE-STACK
   id e EFF>DOUT OCCURS-TYPE-STACK or
   id e EFF>RIN  OCCURS-TYPE-STACK or
   id e EFF>ROUT OCCURS-TYPE-STACK or
; is OCCURS-TYPE

defer OCCURS-ROW-STACK  ( id s -- f )
: OCCURS-ROW-TYPE  ( id t -- f )    \ row var id occurs inside a type? (only via quot/ptr)
   RESOLVE-TYPE {: id t :}
   t TERM>TAG T-PTR = if id t PTR>INNER RECURSE exit then
   t TERM>TAG T-QUOT = if
      t QUOT>EFF {: e :}
      id e EFF>DIN  OCCURS-ROW-STACK
      id e EFF>DOUT OCCURS-ROW-STACK or
      id e EFF>RIN  OCCURS-ROW-STACK or
      id e EFF>ROUT OCCURS-ROW-STACK or
      exit then
   false ;
:noname  ( id s -- f )              \ row var id occurs in stack s (spine or quot tops)
   RESOLVE-ROW {: id s :}
   s SROW? if id s TERM>PAYLOAD = exit then
   id s STACK-TOP  OCCURS-ROW-TYPE
   id s STACK-REST OCCURS-ROW-STACK or
; is OCCURS-ROW-STACK
' OCCURS-ROW-STACK is OCCURS-ROW

\ --- occurs-checked binders ---
: BIND-TYPE  ( id t -- )   2dup OCCURS-TYPE if E-OCCURS throw then swap TV! ;
\ Binding a RIGID row var would extend the declared prefix → underflow/over-arity.
: BIND-ROW   ( id s -- )
   over RV@ RIGID-ROW = if E-UNDERFLOW throw then
   2dup OCCURS-ROW  if E-OCCURS throw then swap RV! ;

\ --- unify ---
: UNIFY-EFFECT  ( e1 e2 -- )
   {: e1 e2 :}
   e1 EFF>DIN  e2 EFF>DIN  UNIFY-ROW
   e1 EFF>DOUT e2 EFF>DOUT UNIFY-ROW
   e1 EFF>RIN  e2 EFF>RIN  UNIFY-ROW
   e1 EFF>ROUT e2 EFF>ROUT UNIFY-ROW ;

:noname  ( a b -- )                 \ UNIFY-TYPE
   RESOLVE-TYPE swap RESOLVE-TYPE swap {: a b :}
   a b = if exit then
   a TYVAR? if a TERM>PAYLOAD b BIND-TYPE exit then
   b TYVAR? if b TERM>PAYLOAD a BIND-TYPE exit then
   a TYCON? b TYCON? and if
      a TERM>PAYLOAD b TERM>PAYLOAD = if exit then
      a b E-MISMATCH DIAG! E-MISMATCH throw then
   a TERM>TAG T-PTR = b TERM>TAG T-PTR = and if
      a PTR>INNER b PTR>INNER UNIFY-TYPE exit then
   a TERM>TAG T-QUOT = b TERM>TAG T-QUOT = and if
      a QUOT>EFF b QUOT>EFF UNIFY-EFFECT exit then
   a b E-MISMATCH DIAG! E-MISMATCH throw
; is UNIFY-TYPE

:noname  ( s1 s2 -- )               \ UNIFY-ROW
   RESOLVE-ROW swap RESOLVE-ROW swap {: a b :}
   a b = if exit then
   a SROW? b SROW? and if            \ two row vars: bind the non-rigid one
      a ROW-RIGID? if  b TERM>PAYLOAD a BIND-ROW   \ a rigid → b := a (throws if b also rigid)
      else             a TERM>PAYLOAD b BIND-ROW   \ a free  → a := b
      then exit
   then
   a SROW? if a TERM>PAYLOAD b BIND-ROW exit then  \ a row, b push → a := b (throws if a rigid)
   b SROW? if b TERM>PAYLOAD a BIND-ROW exit then  \ a push, b row → b := a (throws if b rigid)
   a STACK-TOP  b STACK-TOP  UNIFY-TYPE
   a STACK-REST b STACK-REST UNIFY-ROW
; is UNIFY-ROW
