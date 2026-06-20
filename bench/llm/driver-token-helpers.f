\ driver-token-helpers.f - exact-token source guards for selected drivers.
\
\ Load after bench/llm/drive-stdlib-lib.f.

variable DS-SCAN-IDX
variable DS-SCAN-END
variable DS-SCAN-DEF

: DS-SPACE? ( n -- bool )
   dup STR-SPACE = over STR-TAB = or over STR-LF = or swap STR-CR = or ;

: DS-SKIP-SPACE ( ptr u8 n n -- n ) {: a:ptr u idx :}
   idx begin dup u < while
      dup a + c@ DS-SPACE? if 1+ else exit then
   repeat ;

: DS-SCAN-WORD-END ( ptr u8 n n -- n ) {: a:ptr u idx :}
   idx begin dup u < while
      dup a + c@ DS-SPACE? if exit then
      1+
   repeat ;

: DS-CAND-HAS-WORD? ( ptr u8 n -- bool ) {: needle:ptr needleu :}
   0 DS-SCAN-IDX !
   begin
      DS-CAND$ DS-SCAN-IDX @ DS-SKIP-SPACE DS-SCAN-IDX !
      DS-SCAN-IDX @ DS-CAND$ nip <
   while
      DS-CAND$ DS-SCAN-IDX @ DS-SCAN-WORD-END DS-SCAN-END !
      DS-CAND$ drop DS-SCAN-IDX @ +
      DS-SCAN-END @ DS-SCAN-IDX @ - needle needleu STR= if DS-TRUE exit then
      DS-SCAN-END @ DS-SCAN-IDX !
   repeat
   DS-FALSE ;

: DS-DEF-TOKEN? ( ptr u8 n -- bool ) {: a:ptr u :}
   a u s" :" STR= if DS-TRUE exit then
   a u s" create" STR= if DS-TRUE exit then
   a u s" variable" STR= if DS-TRUE exit then
   a u s" constant" STR= ;

: DS-CAND-DEFINES-PREFIX? ( ptr u8 n -- bool ) {: prefix:ptr prefixu :}
   0 DS-SCAN-IDX !
   0 DS-SCAN-DEF !
   begin
      DS-CAND$ DS-SCAN-IDX @ DS-SKIP-SPACE DS-SCAN-IDX !
      DS-SCAN-IDX @ DS-CAND$ nip <
   while
      DS-CAND$ DS-SCAN-IDX @ DS-SCAN-WORD-END DS-SCAN-END !
      DS-CAND$ drop DS-SCAN-IDX @ + DS-SCAN-END @ DS-SCAN-IDX @ -
      DS-SCAN-DEF @ if
         2dup prefix prefixu STARTS-WITH? if 2drop DS-TRUE exit then
         2drop
         0 DS-SCAN-DEF !
      else
         DS-DEF-TOKEN? if -1 DS-SCAN-DEF ! then
      then
      DS-SCAN-END @ DS-SCAN-IDX !
   repeat
   DS-FALSE ;
