\ lsp-text.f - offsets in a text as the language server counts positions: the
\ line by LF and the character in UTF-16 units.
\
\ TEXT! names the text positions count in, and FILE-TEXT! names a file's text
\ on disk, read into this module's buffer. LINE-CHARACTER answers an offset's
\ position through a cursor: moving forward reads only the bytes between,
\ moving back within the line keeps the line, further back starts over; so a
\ run of offsets in order reads each byte of the text once. OFFSET-AT goes
\ back, from a position to its offset.
\
\ STORAGE CLASS. PROCESS-GLOBAL: the text, the file read and the cursor belong
\ to the server's one task.

require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/fd-io.f
require lib/fs.f
require lib/source.f
require lib/utf16.f
require lib/json-write.f
require tools/lsp-line.f

package LSP-TEXT
using JSON-WRITE
using LSP-LINE

private

10 constant LF

TYPED-VARIABLE TEXT-A ptr u8             \ the text positions count in,
variable TEXT-U
variable CUR-AT                          \ the cursor in it: its offset,
variable CUR-LINE                        \ the line the offset is on,
variable CUR-START                       \ and where that line starts
DYNAMIC-BUFFER DISK u8                   \ a file's text, read from disk,
variable DISK-U                          \ and its length

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

: DISK-ROOM ( n -- ptr u8 )
   DISK-RESERVE 0 DISK ;

\ The file at this path read into DISK, its length in DISK-U, to its end
\ however it grows while it is read: its size, whose refusal (E-FS-STAT) stays
\ a missing file's, is only the first room (lib/source.f READ-WHOLE-SAMPLED).
: SLURP ( ptr u8 n -- ptr u8 n )
   {: f:ptr fu:n :}
   f fu  f fu FILE-SIZE  [: DISK-ROOM ;] SOURCE:READ-WHOLE-SAMPLED DISK-U !
   f fu ;

: ERR ( ptr u8 n -- )
   {: a:ptr u:n :}
   2 >FD a u FD-IO:WRITE-FULL ;

public

\ Positions count in this text from now on.
: TEXT! ( ptr u8 n -- )
   {: a:ptr u:n :}
   a TEXT-A !
   u TEXT-U !
   CUR-RESET ;

\ Positions count in the text of the file at this path, read from disk and
\ taken after the read, which may move it, from now on. A file that cannot be
\ read says so on stderr and counts as empty; it may have been refused before
\ any storage held it.
: FILE-TEXT! ( ptr u8 n -- )
   {: f:ptr fu:n :}
   f fu [: SLURP ;] catch {: code:n :} 2drop
   code 0<> if
      code E-FS-LAST >= code E-FS-FIRST <= and 0= if code throw then
      s" lsp: " ERR f fu ERR
      SB-RESET s" : not read: throw " SB-APPEND code FMT:SB-INT LF SB-APPEND-C
      SB$ ERR
      NULL$ TEXT!
      exit
   then
   0 DISK DISK-U @ TEXT! ;

\ The line and UTF-16 character of an offset held to the text: below it is its
\ start, past it its end.
: LINE-CHARACTER ( n -- n n )
   0 max TEXT-U @ min {: o:n :}
   o CUR-TO
   CUR-LINE @
   TEXT-A @ CUR-START @ + o CUR-START @ - UTF16:UNITS ;

private

: POSITION ( ptr JSON-WRITE:writer n n -- ptr JSON-WRITE:writer )
   {: ln:n ch:n :}
   OBJECT-START
   s" line" ln FIELD-U COMMA
   s" character" ch FIELD-U
   OBJECT-END ;

public

\ The range member from the line and character of its start to those of its
\ end.
: RANGE-AT ( ptr JSON-WRITE:writer n n n n -- ptr JSON-WRITE:writer )
   {: l1:n c1:n l2:n c2:n :}
   s" range" KEY OBJECT-START
   s" start" KEY l1 c1 POSITION COMMA
   s" end" KEY l2 c2 POSITION
   OBJECT-END ;

\ The range member for a line that holds one JSON object with byte_start and
\ byte_end, as the checker's packets do: a missing start is 0, a missing end
\ the start; both are held to the text and the end to no less than the start,
\ so the range is valid whatever the line says.
: RANGE ( ptr JSON-WRITE:writer ptr u8 n -- ptr JSON-WRITE:writer )
   {: a:ptr u:n :}
   a u s" byte_start" INT-MEMBER 0= if drop 0 then {: from:n :}
   a u s" byte_end" INT-MEMBER 0= if drop from then from max {: to:n :}
   from LINE-CHARACTER to LINE-CHARACTER RANGE-AT ;

private

\ Where the line that starts at offset O of the text ends: at its LF, else at
\ the text's end.
: LINE-END ( n -- n )
   {: o:n :}
   TEXT-U @ o ?do
      TEXT-A @ i + c@ LF = if i unloop exit then
   loop
   TEXT-U @ ;

public

\ The offset of a line and UTF-16 character in the text, counted as
\ LINE-CHARACTER counts them: a line past the last is the text's end, and a
\ character past the end of its line that end.
: OFFSET-AT ( n n -- n )
   {: line:n ch:n :}
   0 line 0 ?do
      LINE-END dup TEXT-U @ = if unloop exit then
      1+
   loop
   {: start:n :}
   TEXT-A @ start + start LINE-END start - ch UTF16:OFFSET start + ;

;using
;using
;package
