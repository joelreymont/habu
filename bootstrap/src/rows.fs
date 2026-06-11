\ rows.fs — stack (row) terms + row-variable store (union-find).
\ A stack-term is one cell: (payload << 3) | tag, tag S-ROW | S-PUSH.
\ A stack is a row-var tail plus zero or more pushes:  S-PUSH[rest, top-type].
\ Depends on types.fs (a push stores a TYPE term on top). OCCURS-ROW lives in
\ unify.fs (the seam).

\ --- stack constructors / accessors ---
: SROW?  ( s -- f )   TERM>TAG S-ROW = ;
: SPUSH? ( s -- f )   TERM>TAG S-PUSH = ;
: MK-ROW ( id -- s )  3 lshift S-ROW or ;

\ push: 2-cell arena node [rest-stack, top-type]; payload = node index.
: MK-PUSH ( rest top -- s )
   2 ARENA-ALLOT {: rest top idx :}
   rest idx     ARENA!
   top  idx 1+  ARENA!
   idx 3 lshift S-PUSH or ;
: STACK-REST ( s -- rest )  TERM>PAYLOAD ARENA@ ;
: STACK-TOP  ( s -- top )   TERM>PAYLOAD 1+ ARENA@ ;

\ --- row-variable store (union-find by chasing) ---
create RV-BIND  MAX-RV cells allot   \ RV-BIND[i] = bound stack term, or UNBOUND
variable RV-NEXT                     \ next fresh row-var id

: RV@   ( id -- s )   cells RV-BIND + @ ;
: RV!   ( s id -- )   cells RV-BIND + ! ;
: RV-RESET  ( -- )    0 RV-NEXT ! ;
\ Full clear of the bind array (tests that build raw-id stacks need this; see TV-CLEAR).
: RV-CLEAR  ( -- )    MAX-RV 0 ?do UNBOUND i RV! loop  RV-RESET ;

\ Allocate nr fresh row-var ids, cleared to UNBOUND. Returns the base id.
: RV-ALLOC  ( nr -- base )
   RV-NEXT @ {: nr base :}
   base nr + MAX-RV > if E-TOOMANYVARS throw then
   base nr + RV-NEXT !
   nr 0 ?do  UNBOUND  base i +  RV!  loop
   base ;

\ Chase row-var bindings to the first push-or-(un)bound-row. Shallow.
\ A RIGID row var (the declared prefix) resolves to itself — it cannot be
\ extended, so it terminates the chase like UNBOUND.
: RESOLVE-ROW  ( s -- s' )
   begin
     dup SROW? 0= if exit then        \ a push -> done
     dup TERM>PAYLOAD RV@
     dup UNBOUND = if drop exit then   \ unbound -> done
     dup RIGID-ROW = if drop exit then \ rigid -> done (the row var itself)
     nip
   again ;

\ Is s a RIGID row var (declared, un-extendable)?
: ROW-RIGID?  ( s -- f )
   dup SROW? if TERM>PAYLOAD RV@ RIGID-ROW = else drop false then ;
: RV-RIGID!   ( id -- )   RIGID-ROW swap RV! ;

\ The id of the bottom (tail) row var of a stack.
: TAIL-ROW  ( s -- id )
   begin RESOLVE-ROW dup SROW? 0= while STACK-REST repeat TERM>PAYLOAD ;

\ Number of pushes above the tail row var (the stack's visible depth).
: STACK-ARITY  ( s -- n )
   0 swap begin RESOLVE-ROW dup SROW? 0= while STACK-REST swap 1+ swap repeat drop ;
