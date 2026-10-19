\ display.f - checked Wasm peer for SVG images and typed-input control.
\ Build with tools/wasm-build.f, entry DISPLAY:TURN.
require src/lib/string.f

package DISPLAY
private

CAST: >BYTES ( n -- ptr u8 )
CAST: >CELLS ( ptr u8 -- ptr n )
CAST: BYTES>N ( ptr u8 -- n )

56 BUFFER: EVENT-BUF
64 BUFFER: MATRIX
36 BUFFER: CORNERS
512 BUFFER: IMAGE
variable IMAGE-U

: FIELD, ( n -- )
   8 0 do dup $FF and emit 8 rshift loop drop ;

: RECORD ( n n n n -- )
   {: kind:n a:n b:n c:n :}
   kind FIELD, a FIELD, b FIELD, c FIELD, ;

: EVENT@ ( n -- n )
   8 * EVENT-BUF + >CELLS @ ;

: SVG ( ptr u8 n -- )
   {: a:ptr u:n :}
   8 a BYTES>N u 0 RECORD ;

: IMAGE+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   a IMAGE IMAGE-U @ + u BYTE-COPY
   u IMAGE-U +! ;

: RED ( -- )
   0 IMAGE-U !
   s" <svg xmlns='http://www.w3.org/2000/svg' width='400' height='100' viewBox='0 0 400 100' onload='parent.svgScript=1'>" IMAGE+
   s" <script>parent.svgScript=2</script><a href='/svg-link'><rect width='400' height='100' fill='#d62828'/></a>" IMAGE+
   s" <image href='/svg-external' width='10' height='10'/></svg>" IMAGE+
   IMAGE IMAGE-U @ SVG ;

: BLUE ( -- )
   s\" <svg xmlns='http://www.w3.org/2000/svg' width='100' height='400' viewBox='0 0 100 400'><rect width='100' height='400' fill='#245bd6'/></svg>" SVG ;

: DRAW ( n -- )
   {: count:n :}
   2 CORNERS BYTES>N count MATRIX BYTES>N RECORD ;

: IS? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   2 EVENT@ u <> if false exit then
   u 0 ?do 1 EVENT@ i + >BYTES c@ a i + c@ <> if false unloop exit then loop
   true ;

: COMMAND ( -- )
   s" red" IS? if RED exit then
   s" blue" IS? if BLUE exit then
   s" zero" IS? if 0 DRAW exit then
   s" draw" IS? if 1 DRAW exit then
   s" broken" IS? if s" <svg>" SVG exit then
   s" on" IS? if 9 1 0 0 RECORD exit then
   s" off" IS? if 9 0 0 0 RECORD exit then
   s" text" IS? if s" still here" {: a:ptr u:n :} 3 a BYTES>N u 0 RECORD exit then
   s" tree" IS? if s" []" {: a:ptr u:n :} 5 a BYTES>N u 0 RECORD exit then
   s" empty" IS? if 8 0 0 0 RECORD exit then
   s" negative" IS? if 8 0 -1 0 RECORD exit then
   s" outside" IS? if 8 -1 1 0 RECORD exit then
   s" past" IS? if 8 $7FFFFFFFFFFFFFFF 1 0 RECORD exit then
   s" svg-reserved" IS? if 8 0 1 1 RECORD exit then
   s" flag" IS? if 9 2 0 0 RECORD exit then
   s" negative-flag" IS? if 9 -1 0 0 RECORD exit then
   s" input-b" IS? if 9 1 1 0 RECORD exit then
   s" input-c" IS? if 9 0 0 1 RECORD exit then ;

public

: TURN ( -- )
   0 EVENT@ 0 = if
      0 EVENT-BUF BYTES>N 0 0 RECORD
      9 0 0 0 RECORD
      RED exit
   then
   0 EVENT@ 4 = if COMMAND then ;

;package
