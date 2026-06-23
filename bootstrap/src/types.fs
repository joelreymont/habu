\ types.fs — type terms + type-variable store (union-find).
\ A type-term is one cell: (payload << 3) | tag.  Tags/codes in config.fs.
\ Occurs lives in unify.fs (the OCCURS-TYPE seam) — it must descend rows/effects
\ that this layer doesn't know about.

\ --- term constructors / accessors ---
: TERM>TAG      ( t -- tag )   7 and ;
: TERM>PAYLOAD  ( t -- n )     3 rshift ;
: MK-CON   ( code -- t )   3 lshift T-CON or ;
: MK-VAR   ( id -- t )     3 lshift T-VAR or ;
: TYCON?   ( t -- f )      TERM>TAG T-CON = ;
: TYVAR?   ( t -- f )      TERM>TAG T-VAR = ;

\ ptr<inner>: store the inner type in a 1-cell arena node; payload = node index.
: MK-PTR    ( inner -- t )  1 ARENA-ALLOT tuck ARENA!  3 lshift T-PTR or ;
: PTR>INNER ( t -- inner )  TERM>PAYLOAD ARENA@ ;

\ quot<eff>: payload = arena index of the effect node (built by effects-repr).
: MK-QUOT  ( eff -- t )    3 lshift T-QUOT or ;
: QUOT>EFF ( t -- eff )    TERM>PAYLOAD ;

\ --- type-variable store (union-find by chasing) ---
create TV-BIND  MAX-TV cells allot   \ TV-BIND[i] = bound term, or UNBOUND
variable TV-NEXT                     \ next fresh var id (high-water mark)

: TV@   ( id -- t )   cells TV-BIND + @ ;
: TV!   ( t id -- )   cells TV-BIND + ! ;
: TV-RESET  ( -- )    0 TV-NEXT ! ;
\ Full clear of the bind array (production never needs it — TV-ALLOC clears each
\ block — but tests that build raw-id terms must, to avoid stale bindings).
: TV-CLEAR  ( -- )    MAX-TV 0 ?do UNBOUND i TV! loop  TV-RESET ;

\ Allocate nv fresh var ids, cleared to UNBOUND (ids reused across checks, so
\ clearing is mandatory). Returns the base id.
: TV-ALLOC  ( nv -- base )
   TV-NEXT @ {: nv base :}
   base nv + MAX-TV > if E-TOOMANYVARS throw then
   base nv + TV-NEXT !
   nv 0 ?do  UNBOUND  base i +  TV!  loop
   base ;

\ Chase type-var bindings to the first concrete/unbound term. Shallow.
: RESOLVE-TYPE  ( t -- t' )
   begin
     dup TYVAR? 0= if exit then
     dup TERM>PAYLOAD TV@
     dup UNBOUND = if drop exit then
     nip
   again ;
