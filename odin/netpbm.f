\ netpbm.f - PGM/PPM (P5/P6) image I/O, ported from Odin's src/netpbm.zig.
\
\ WRITE-P5 emits an 8-bit grayscale P5 image (header + packed rows, dropping row
\ padding). DECODE parses a binary P5/P6 image and yields its luma plane (P6
\ RGB is converted with the Rec.709-ish weights from the Zig source). Pure data;
\ output builds in the lib/string.f SB byte builder. Depends on lib/errors.f
\ lib/string.f lib/fmt.f.

package NETPBM
private
variable POS                            \ parser cursor
variable RIDX                           \ raster byte index
variable WIDTH  variable HEIGHT               \ decoded dimensions

\ ---- P5 writer ------------------------------------------------------------
: P5-HEADER ( n n -- ) {: w:n h:n :}
   s" P5" SB-APPEND  10 SB-APPEND-C
   w SB-U  32 SB-APPEND-C  h SB-U  10 SB-APPEND-C
   s" 255" SB-APPEND  10 SB-APPEND-C ;
: P5-ROWS ( ptr u8 n n n -- ) {: d:ptr w:n h:n step:n :}
   0 begin dup h < while
      dup step * d +  w  SB-APPEND
      1+
   repeat drop ;
public
: WRITE-P5 ( ptr u8 n n n -- ptr u8 n ) {: d:ptr w:n h:n step:n :}
   SB-RESET  w h P5-HEADER  d w h step P5-ROWS  SB$ ;

\ ---- byte classifiers (bounds-safe at idx) --------------------------------
private
: WS? ( n -- bool ) {: c:n :}
   c 32 = c 9 = or c 10 = or c 13 = or c 12 = or c 11 = or ;
: IS-WS? ( ptr u8 n n -- bool ) {: b:ptr n:n idx:n :}
   idx n < if b idx + c@ WS? else 0 0= 0= then ;
: IS-TOK? ( ptr u8 n n -- bool ) {: b:ptr n:n idx:n :}
   idx n < if b idx + c@ WS? 0= else 0 0= 0= then ;
: IS-LINE? ( ptr u8 n n -- bool ) {: b:ptr n:n idx:n :}    \ in bounds and not newline
   idx n < if b idx + c@ 10 = 0= else 0 0= 0= then ;

\ ---- tokenizer (whitespace + # comments) ----------------------------------
: SKIP-COMMENT ( ptr u8 n -- ) {: b:ptr n:n :}
   begin b n POS @ IS-LINE? while POS @ 1+ POS ! repeat ;
: SKIP-ONE ( ptr u8 n -- bool ) {: b:ptr n:n :}          \ skipped something -> continue
   POS @ n >= if 0 0= 0= exit then
   b POS @ + c@ WS? if POS @ 1+ POS ! 0 0= exit then
   b POS @ + c@ 35 = if b n SKIP-COMMENT 0 0= exit then
   0 0= 0= ;
: SKIP-WSC ( ptr u8 n -- ) {: b:ptr n:n :}
   begin b n SKIP-ONE while repeat ;
: TOKEN ( ptr u8 n -- ptr u8 n ) {: b:ptr n:n :}
   b n SKIP-WSC
   POS @ {: start:n :}
   begin b n POS @ IS-TOK? while POS @ 1+ POS ! repeat
   b start +  POS @ start - ;
: SKIP-RASTER-WS ( ptr u8 n -- ) {: b:ptr n:n :}        \ skip the single pre-raster ws
   POS @ n < if b POS @ + c@ WS? if POS @ 1+ POS ! then then ;

\ ---- pixels ---------------------------------------------------------------
: LUMA ( n n n -- n ) {: r:n g:n b:n :}
   r 2126 *  g 7152 * +  b 722 * +  10000 / ;
: EMIT-PIXEL ( ptr u8 n -- ) {: b:ptr ch:n :}
   ch 1 = if
      b RIDX @ + c@ SB-APPEND-C
   else
      b RIDX @ + c@  b RIDX @ 1+ + c@  b RIDX @ 2 + + c@  LUMA SB-APPEND-C
   then
   RIDX @ ch + RIDX ! ;
: EMIT-PIXELS ( ptr u8 n n -- ) {: b:ptr ch:n count:n :}
   0 begin dup count < while
      b ch EMIT-PIXEL
      1+
   repeat drop ;

\ ---- decode ---------------------------------------------------------------
: CHANNELS ( ptr u8 n -- n ) {: ma:ptr mu:n :}          \ 1=P5, 3=P6, 0=unsupported
   ma mu s" P5" STR= if 1 exit then
   ma mu s" P6" STR= if 3 exit then
   0 ;
public
: DECODE ( ptr u8 n -- ptr u8 n bool ) {: b:ptr n:n :}
   SB-RESET  0 POS !
   b n TOKEN CHANNELS {: ch:n :}
   b n TOKEN STR>NUMBER? {: w:n wok:bool :}
   b n TOKEN STR>NUMBER? {: h:n hok:bool :}
   b n TOKEN STR>NUMBER? {: mx:n mxok:bool :}
   b n SKIP-RASTER-WS
   w WIDTH !  h HEIGHT !
   ch 0 =  wok 0= or  hok 0= or  mxok 0= or  mx 1 < or  mx 255 > or
   if SB$ 0 0= 0= exit then
   POS @  w h * ch * +  n >
   if SB$ 0 0= 0= exit then
   POS @ RIDX !
   b ch  w h *  EMIT-PIXELS
   SB$ 0 0= ;

: WIDTH@ ( -- n ) WIDTH @ ;
: HEIGHT@ ( -- n ) HEIGHT @ ;
end-package
