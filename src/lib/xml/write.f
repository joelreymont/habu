\ write.f - XML escaping.

require src/lib/xml/scalar.f
require src/lib/vector.f

package XML
private
: ESCAPED$ ( n bool -- str )
   {: scalar:n attr:bool :}
   scalar case
      $26 of s" &amp;" endof
      $3C of s" &lt;" endof
      $3E of s" &gt;" endof
      $D of s" &#13;" endof
      $22 of
         attr if s" &quot;" else s" " then
      endof
      $27 of
         attr if s" &apos;" else s" " then
      endof
      $9 of
         attr if s" &#9;" else s" " then
      endof
      $A of
         attr if s" &#10;" else s" " then
      endof
      s" " swap
   endcase ;

: ESCAPED-LEN ( n bool -- n )
   {: scalar:n attr:bool :}
   scalar attr ESCAPED$ STR:LEN
   dup 0= if drop scalar SCALAR-WIDTH then ;

: ESCAPED-SIZE ( str bool -- n )
   {: source:str attr:bool :}
   0 0
   begin dup source STR:LEN < while
      {: total:n cursor:n :}
      source WHOLE cursor SCALAR-AT {: scalar:n next:n :}
      scalar attr ESCAPED-LEN {: added:n :}
      added MAX-SIZE total - > if E-CAPACITY throw then
      total added + next
   repeat
   drop ;

: WRITE-ESCAPED ( n bool ^u8 -- ^u8 )
   {: scalar:n attr:bool destination :}
   scalar attr ESCAPED$ {: escaped:str :}
   escaped STR:LEN 0= if scalar destination PUT-SCALAR exit then
   escaped STR:DATA destination escaped STR:LEN >LEN BYTE-COPY-LEN
   destination escaped STR:LEN + ;

: ESCAPE-WRITE ( str bool ^u8 -- )
   {: source:str attr:bool destination :}
   destination 0
   begin dup source STR:LEN < while
      {: output cursor:n :}
      source WHOLE cursor SCALAR-AT {: scalar:n next:n :}
      scalar attr output WRITE-ESCAPED next
   repeat
   2drop ;

: ESCAPE-INTO ( str bool ^VEC:vector<u8> -- )
   {: source:str attr:bool v:^ :}
   source SPAN-CHECK
   source attr ESCAPED-SIZE {: needed:n :}
   v VEC:COUNT {: at:n :}
   v at needed + source STR:DATA VEC:MAKE-ROOM {: src:^ :}
   v at needed + VEC:ENSURE
   src source STR:LEN >str attr  v VEC:BYTES STR:DATA at +  ESCAPE-WRITE ;

public
\ Each appends the text, escaped, to the vector; the text may lie in it.
: ESCAPE-TEXT ( str ^VEC:vector<u8> -- )
   {: source:str v:^ :}
   source false v ESCAPE-INTO ;

: ESCAPE-ATTR ( str ^VEC:vector<u8> -- )
   {: source:str v:^ :}
   source true v ESCAPE-INTO ;

;package
