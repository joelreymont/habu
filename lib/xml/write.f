require lib/xml/scalar.f

package XML
private
: ESCAPED$ ( n bool -- ptr u8 n )
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
      s" " rot
   endcase ;

: ESCAPED-LEN ( n bool -- n )
   {: scalar:n attr:bool :}
   scalar attr ESCAPED$ nip
   dup 0= if drop scalar SCALAR-WIDTH then ;

: ESCAPED-SIZE ( ptr u8 n bool -- n )
   {: source size:n attr:bool :}
   source size SPAN-CHECK
   0 0
   begin dup size < while
      {: total:n cursor:n :}
      source size cursor SCALAR-AT {: scalar:n next:n :}
      scalar attr ESCAPED-LEN {: added:n :}
      added MAX-SIZE total - > if E-CAPACITY throw then
      total added + next
   repeat
   drop ;

: WRITE-ESCAPED ( n bool ptr u8 -- ptr u8 )
   {: scalar:n attr:bool destination :}
   scalar attr ESCAPED$ {: escaped size:n :}
   size 0= if scalar destination PUT-SCALAR exit then
   escaped destination size >LEN BYTE-COPY-LEN
   destination size + ;

: ESCAPE-WRITE ( ptr u8 n bool ptr u8 -- )
   {: source size:n attr:bool destination :}
   destination 0
   begin dup size < while
      {: output cursor:n :}
      source size cursor SCALAR-AT {: scalar:n next:n :}
      scalar attr output WRITE-ESCAPED next
   repeat
   2drop ;

: ESCAPE-INTO ( ptr u8 n bool ptr u8 n -- n )
   {: source size:n attr:bool destination cap:n :}
   source size attr ESCAPED-SIZE {: needed:n :}
   destination cap SPAN-CHECK
   needed cap > if E-CAPACITY throw then
   source size destination cap OVERLAP? if E-ALIAS throw then
   source size attr destination ESCAPE-WRITE
   needed ;

public
: ESCAPE-TEXT ( ptr u8 n ptr u8 n -- n )
   {: source size:n destination cap:n :}
   source size false destination cap ESCAPE-INTO ;

: ESCAPE-ATTR ( ptr u8 n ptr u8 n -- n )
   {: source size:n destination cap:n :}
   source size true destination cap ESCAPE-INTO ;

;package
