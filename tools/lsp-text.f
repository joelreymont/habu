\ lsp-text.f - offsets in a text as the language server counts positions: the line
\ by LF and the character in UTF-16 units. LINE-CHARACTER moves a cursor: forward
\ reads only the bytes between, back within the line keeps the line, further back
\ starts over, so offsets in order read each byte once.
\ PROCESS-GLOBAL: the text, the file read and the cursor belong to the server's
\ one task.

require src/lib/errors.f
require src/lib/string.f
require src/lib/fmt.f
require src/lib/fd.f
require src/lib/fs.f
require src/lib/source.f
require src/lib/utf16.f
require src/lib/json-write.f
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
variable DISK-SIZE                       \ metadata captured around that read,
variable DISK-MSEC
variable DISK-MNS
variable DISK-CSEC
variable DISK-CNS
variable DISK-VALID

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
\ a missing file's, is only the first room (src/lib/source.f READ-WHOLE-SAMPLED).
: SLURP ( ptr u8 n -- ptr u8 n )
   {: f:ptr fu:n :}
   f fu FS:FILE-META {: size:n msec:n mns:n csec:n cns:n :}
   f fu  f fu FS:FILE-SIZE  [: DISK-ROOM ;] SOURCE:READ-WHOLE-SAMPLED DISK-U !
   f fu FS:FILE-META {: after:n amsec:n amns:n acsec:n acns:n :}
   size after = msec amsec = and mns amns = and
   csec acsec = and cns acns = and if
      size DISK-SIZE ! msec DISK-MSEC ! mns DISK-MNS !
      csec DISK-CSEC ! cns DISK-CNS !
      true DISK-VALID !
   then
   f fu ;

: ERR ( ptr u8 n -- )
   {: a:ptr u:n :}
   FD:STDERR a u FD:WRITE-FULL ;

public

: TEXT! ( ptr u8 n -- )
   {: a:ptr u:n :}
   a TEXT-A !
   u TEXT-U !
   false DISK-VALID !
   CUR-RESET ;

: TEXT$ ( -- ptr u8 n )  TEXT-A @ TEXT-U @ ;

\ Whether a throw of this code is a file system call's (src/lib/errors.f E-FS-*).
: FS-RC? ( n -- bool )
   {: rc:n :}
   rc E-FS-FIRST <= rc E-FS-LAST >= and ;

private

\ Keep every byte acquired by a completed read, even when its metadata changed.
\ The validity flag separately tells navigation whether the read stayed stable.
\ A file that cannot be read says so on stderr and counts as empty.
: READ-TEXT? ( ptr u8 n -- bool )
   {: f:ptr fu:n :}
   false DISK-VALID !
   f fu [: SLURP ;] catch {: code:n :} 2drop
   code 0<> if
      code FS-RC? 0= if code throw then
      s" lsp: " ERR f fu ERR
      SB-RESET s" : not read: throw " SB-APPEND code FMT:SB-INT LF SB-APPEND-C
      SB$ ERR
      NULL$ TEXT!
      false exit
   then
   DISK-VALID @ 0<> {: valid:bool :}
   0 DISK DISK-U @ TEXT!
   valid DISK-VALID !
   true ;

public

\ Diagnostic positions count in the complete acquired snapshot. Changed
\ metadata grants no freshness: DISK-META and DISK-CURRENT? remain false.
: FILE-SNAPSHOT! ( ptr u8 n -- )
   READ-TEXT? drop ;

\ Navigation requires a stable read. Take the buffer after reading, since
\ growth may move it; a missing or changed file counts as empty text.
: FILE-TEXT! ( ptr u8 n -- )
   {: f:ptr fu:n :}
   f fu READ-TEXT? 0= if exit then
   DISK-VALID @ 0= if
      s" lsp: " ERR f fu ERR s" : changed while read\n" ERR
      NULL$ TEXT!
   then ;

\ Metadata of the most recent file read, valid only when that read stayed
\ stable. TEXT! of an editor buffer clears it.
: DISK-META ( -- n n n n n bool )
   DISK-SIZE @ DISK-MSEC @ DISK-MNS @ DISK-CSEC @ DISK-CNS @ DISK-VALID @ 0<> ;

: DISK-CURRENT? ( ptr u8 n -- bool )
   DISK-VALID @ 0= if 2drop false exit then
   FS:TRY-STAT 0= if false exit then
   FS:STAT-MODE@ FS:S-IFMT and FS:S-IFREG <> if false exit then
   FS:STAT-SIZE@ DISK-SIZE @ =
   FS:STAT-MTIME-SEC@ DISK-MSEC @ = and
   FS:STAT-MTIME-NS@ DISK-MNS @ = and
   FS:STAT-CTIME-SEC@ DISK-CSEC @ = and
   FS:STAT-CTIME-NS@ DISK-CNS @ = and ;

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

: RANGE-VALUE ( ptr JSON-WRITE:writer n n n n -- ptr JSON-WRITE:writer )
   {: l1:n c1:n l2:n c2:n :}
   OBJECT-START
   s" start" KEY l1 c1 POSITION COMMA
   s" end" KEY l2 c2 POSITION
   OBJECT-END ;

: RANGE-AT ( ptr JSON-WRITE:writer n n n n -- ptr JSON-WRITE:writer )
   {: l1:n c1:n l2:n c2:n :}
   s" range" KEY l1 c1 l2 c2 RANGE-VALUE ;

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
