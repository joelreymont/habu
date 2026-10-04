\ lsp-text.f - offsets in a text as the language server counts positions: the
\ line by LF and the character in UTF-16 units.
\
\ TEXT! names the text positions count in. LINE-CHARACTER answers an offset's
\ position through a cursor: moving forward reads only the bytes between,
\ moving back within the line keeps the line, further back starts over; so a
\ run of offsets in order reads each byte of the text once.
\
\ STORAGE CLASS. PROCESS-GLOBAL: the text and its cursor belong to the server's
\ one task.

require lib/utf16.f

package LSP-TEXT

private

10 constant LF

TYPED-VARIABLE TEXT-A ptr u8             \ the text positions count in,
variable TEXT-U
variable CUR-AT                          \ the cursor in it: its offset,
variable CUR-LINE                        \ the line the offset is on,
variable CUR-START                       \ and where that line starts

: CUR-RESET ( -- )
   0 CUR-AT !
   0 CUR-LINE !
   0 CUR-START ! ;

\ The cursor at offset O of the text, O within it.
: CUR-TO ( n -- )
   {: o:n :}
   o CUR-START @ < if CUR-RESET then
   CUR-AT @ begin dup o < while
      TEXT-A @ over + c@ LF = if 1 CUR-LINE +! dup 1+ CUR-START ! then
      1+
   repeat drop
   o CUR-AT ! ;

public

\ Positions count in this text from now on.
: TEXT! ( ptr u8 n -- )
   {: a:ptr u:n :}
   a TEXT-A !
   u TEXT-U !
   CUR-RESET ;

\ The line and UTF-16 character of an offset held to the text: below it is its
\ start, past it its end.
: LINE-CHARACTER ( n -- n n )
   0 max TEXT-U @ min {: o:n :}
   o CUR-TO
   CUR-LINE @
   TEXT-A @ CUR-START @ + o CUR-START @ - UTF16:UNITS ;

;package
