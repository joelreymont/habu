\ roles.f — nominal scalar role conversions baked into hb.
\
\ The checker treats roles such as idx/len/fd as distinct nominal cell types.
\ Runtime representation is still one cell, so these conversion bodies are no-op
\ casts. Their effects are pinned by TRUST rows and covered by the engine gate.

TRUSTED: HB-TARGET-LINUX? ( -- bool ) ;
TRUSTED: HB-TARGET-MACOS? ( -- bool ) ;
TRUSTED: HB-TARGET-KNOWN? ( -- bool ) ;

TRUSTED: >IDX ( n -- idx ) ;
TRUSTED: IDX>N ( idx -- n ) ;

TRUSTED: >LEN ( n -- len ) ;
TRUSTED: LEN>N ( len -- n ) ;

TRUSTED: >COUNT ( n -- count ) ;
TRUSTED: COUNT>N ( count -- n ) ;

TRUSTED: >OFF ( n -- off ) ;
TRUSTED: OFF>N ( off -- n ) ;

TRUSTED: >FD ( n -- fd ) ;
TRUSTED: FD>N ( fd -- n ) ;

TRUSTED: >RC ( n -- rc ) ;
TRUSTED: RC>N ( rc -- n ) ;

TRUSTED: >PID ( n -- pid ) ;
TRUSTED: PID>N ( pid -- n ) ;

TRUSTED: >MS ( n -- ms ) ;
TRUSTED: MS>N ( ms -- n ) ;

TRUSTED: >NS ( n -- ns ) ;
TRUSTED: NS>N ( ns -- n ) ;

TRUSTED: >TOK ( n -- tok ) ;
TRUSTED: TOK>N ( tok -- n ) ;

TRUSTED: >REG ( n -- reg ) ;
TRUSTED: REG>N ( reg -- n ) ;

TRUSTED: >LABEL ( n -- label ) ;
TRUSTED: LABEL>N ( label -- n ) ;

TRUSTED: >VA ( n -- va ) ;
TRUSTED: VA>N ( va -- n ) ;

TRUSTED: >SYMIDX ( n -- symidx ) ;
TRUSTED: SYMIDX>N ( symidx -- n ) ;
