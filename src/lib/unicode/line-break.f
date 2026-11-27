\ line-break.f - text with each mandatory line break, Unicode's line break
\ classes BK, CR, LF and NL, written as one space: CR LF, a lone CR, LF, VT,
\ FF, and the UTF-8 sequences of NEL, U+2028 and U+2029. Every other byte is
\ copied, invalid UTF-8 included, so the text is one line under UAX #14.
\
\ STORAGE CLASS. CALLER-OWNED: no module state; the vector is the caller's.

require src/lib/span.f
require src/lib/vector.f

package UNICODE-LINE-BREAK

private

\ The text after the cursor.
: REST ( str n -- str ) {: s:str at:n :}
   s STR:DATA at + s STR:LEN at - >str ;

\ LF, VT, FF or CR.
: SINGLE? ( n -- bool ) {: b:n :} b STR:LF >= b STR:CR <= and ;

\ The bytes of a NEL, U+2028 or U+2029 the text starts with, else 0.
: MULTI-LEN ( str -- n ) {: r:str :}
   r S\" \xC2\x85" STR:STARTS-WITH? if 2 exit then
   r S\" \xE2\x80\xA8" STR:STARTS-WITH? if 3 exit then
   r S\" \xE2\x80\xA9" STR:STARTS-WITH? if 3 exit then
   0 ;

\ The bytes of the line break a nonempty text starts with, else 0.
: BREAK-LEN ( str -- n ) {: r:str :}
   r STR:DATA c@ SINGLE? if
      r S\" \x0D\x0A" STR:STARTS-WITH? if 2 else 1 then exit
   then
   r MULTI-LEN ;

\ Writes the byte for the cursor's break or byte at out; answers out and the
\ cursor after them.
: STEP ( n n ^u8 str -- n n )
   {: out:n at:n dst:^ s:str :}
   s at REST {: r:str :}
   r BREAK-LEN {: k:n :}
   k 0= if r STR:DATA c@ else STR:SPACE then dst out + c!
   out 1+  at k 1 max + ;

\ Writes the text at the address; answers the bytes written.
: PUT ( ^u8 str -- n ) {: dst:^ s:str :}
   0 0 begin dup s STR:LEN < while dst s STEP repeat drop ;

public

\ Appends the text's one-line form; the text may not lie in the vector, whose
\ growth releases it.
: ONE-LINE+ ( str ^VEC:vector<u8> -- ) {: s:str v:^ :}
   v VEC:COUNT {: at:n :}
   v s STR:LEN VEC:EXTEND SPAN:$ STR:DATA s PUT {: n:n :}
   v at n + VEC:RESIZE ;

;package
