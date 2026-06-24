\ roles.f — nominal scalar role conversions baked into hb.
\
\ The checker treats roles such as idx/len/fd as distinct nominal cell types.
\ Runtime representation is still one cell, so these conversion bodies are no-op
\ casts. Their effects are pinned by TRUST rows and covered by the engine gate.

0 set-check

s" HB-TARGET-LINUX?" s" -- bool" TRUST
s" HB-TARGET-MACOS?" s" -- bool" TRUST
s" HB-TARGET-KNOWN?" s" -- bool" TRUST

: >IDX ( n -- idx ) ;
s" >IDX" s" n -- idx" TRUST
: IDX>N ( idx -- n ) ;
s" IDX>N" s" idx -- n" TRUST

: >LEN ( n -- len ) ;
s" >LEN" s" n -- len" TRUST
: LEN>N ( len -- n ) ;
s" LEN>N" s" len -- n" TRUST

: >COUNT ( n -- count ) ;
s" >COUNT" s" n -- count" TRUST
: COUNT>N ( count -- n ) ;
s" COUNT>N" s" count -- n" TRUST

: >OFF ( n -- off ) ;
s" >OFF" s" n -- off" TRUST
: OFF>N ( off -- n ) ;
s" OFF>N" s" off -- n" TRUST

: >FD ( n -- fd ) ;
s" >FD" s" n -- fd" TRUST
: FD>N ( fd -- n ) ;
s" FD>N" s" fd -- n" TRUST

: >RC ( n -- rc ) ;
s" >RC" s" n -- rc" TRUST
: RC>N ( rc -- n ) ;
s" RC>N" s" rc -- n" TRUST

: >PID ( n -- pid ) ;
s" >PID" s" n -- pid" TRUST
: PID>N ( pid -- n ) ;
s" PID>N" s" pid -- n" TRUST

: >MS ( n -- ms ) ;
s" >MS" s" n -- ms" TRUST
: MS>N ( ms -- n ) ;
s" MS>N" s" ms -- n" TRUST

: >NS ( n -- ns ) ;
s" >NS" s" n -- ns" TRUST
: NS>N ( ns -- n ) ;
s" NS>N" s" ns -- n" TRUST

: >TOK ( n -- tok ) ;
s" >TOK" s" n -- tok" TRUST
: TOK>N ( tok -- n ) ;
s" TOK>N" s" tok -- n" TRUST

' HOOK set-check
