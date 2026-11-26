\ app.f - real Wasm fixture for the turn protocol's rpc, after and read records
\ and its notified, tick, file and chunk events, built by `tools/hb-build.f --
\ --target wasm`, whose entry is MAIN. Each event it reads answers a text record
\ naming its fields; typed text names the record to make. A key event answers
\ key <text> <mods>, so a page control's key turn shows; typed "svg" sends an
\ svg record and "draw" a draw of 0 rows, which removes it, so a page shows
\ what it does while a drawing stands in the canvas's place. Typed "toolbar"
\ sends a toolbar of the buttons Zoom in, Zoom out and Fit, keyed + - f, so a
\ press shows as the key turn of its row's key. Typed "tree" sends
\ a tree of 40 rows with its last pressed, more than a 700 px frame shows. A
\ view/restart notification also tells view/resume {"project":"p2"}.

require src/lib/error.f
require src/lib/string.f
require src/lib/browser/event.f
require test/browser/turn-revision.f

package APP-TURN
private
using BROWSER-EVENT

CAST: >BYTES ( n -- ^u8 )
CAST: BYTES>N ( ^u8 -- n )

256 BUFFER: SPAN
variable SPAN-U
2048 BUFFER: TEXT
variable TEXT-U
\ The last file event's number and size.
variable FILE-NO
variable FILE-SIZE
\ A draw's matrix, all zeros; a draw of 0 rows reads it and draws nothing.
64 BUFFER: MATRIX

: FIELD, ( n -- )
   8 0 do dup $FF and emit 8 rshift loop drop ;


: RECORD ( n n n n -- )
   {: kind:n a:n b:n c:n :}
   kind FIELD, a FIELD, b FIELD, c FIELD, ;


: SAY-C ( n -- )
   TEXT TEXT-U @ + c! TEXT-U @ 1+ TEXT-U ! ;


: DIGITS ( n -- )
   {: n:n :}
   n 10 / {: q:n :}
   q 0 <> if q RECURSE then
   n q 10 * - 48 + SAY-C ;


: SAY ( str -- )
   {: s:str :}
   s STR:LEN 0 ?do s STR:DATA i + c@ SAY-C loop ;


\ Delivered bytes are freed once the turn ends, so the text copies them.
: SAY-AT ( n n -- )
   {: at:n u:n :}
   u 0 ?do at i + >BYTES c@ SAY-C loop ;


: SAY-N ( n -- )  DIGITS 32 SAY-C ;

: TEXT-RECORD ( -- )
   3 TEXT BYTES>N TEXT-U @ 0 RECORD ;

: SPAN-C ( n -- )
   SPAN SPAN-U @ + c! SPAN-U @ 1+ SPAN-U ! ;


: SPAN+ ( str -- )
   {: s:str :}
   s STR:LEN 0 ?do s STR:DATA i + c@ SPAN-C loop ;


: SPAN-CELL ( n -- )
   8 0 do dup $FF and SPAN-C 8 rshift loop drop ;


\ An rpc's span: the method's u64 length, the method, then the params.
: MAKE ( str str -- )
   {: m:str p:str :}
   0 SPAN-U !
   m STR:LEN SPAN-CELL m SPAN+ p SPAN+ ;


: CALL ( -- )
   s" tools/call" s\" {\"name\":\"get_project\"}" MAKE ;

: RPC ( n -- )
   {: id:n :}
   10 SPAN BYTES>N SPAN-U @ id RECORD ;


: READ ( n n n -- )
   {: file:n offset:n length:n :}
   12 file offset length RECORD ;


: IS? ( n n str -- bool )
   {: at:n u:n t:str :}
   u t STR:LEN <> if false exit then
   t STR:LEN 0 ?do at i + >BYTES c@ t STR:DATA i + c@ <> if false unloop exit then loop
   true ;


: SVG ( -- )
   s\" <svg xmlns=\"http://www.w3.org/2000/svg\" width=\"8\" height=\"8\"/>"
   {: s:str :}
   8 s STR:DATA BYTES>N s STR:LEN 0 RECORD ;


\ A tree of rows 1 to 40 under the root, labelled by key, the last pressed.
: TREE ( -- )
   0 TEXT-U !  91 SAY-C
   41 1 do
      i 1 > if 44 SAY-C then
      s\" {\"key\":" SAY  i DIGITS  s\" ,\"parent\":0,\"label\":\"" SAY  i DIGITS  s\" \"}" SAY
   loop
   93 SAY-C  5 TEXT BYTES>N TEXT-U @ 40 RECORD ;


\ The buttons +, - and f, each with an icon.
: TOOLBAR ( -- )
   0 TEXT-U !
   s\" [{\"key\":\"+\",\"label\":\"Zoom in\",\"group\":\"Zoom\"," SAY
   s\" \"icon\":\"M11 4a7 7 0 1 0 0 14a7 7 0 1 0 0-14M20 20l-4-4M8 11h6M11 8v6\"}," SAY
   s\" {\"key\":\"-\",\"label\":\"Zoom out\",\"group\":\"Zoom\"," SAY
   s\" \"icon\":\"M11 4a7 7 0 1 0 0 14a7 7 0 1 0 0-14M20 20l-4-4M8 11h6\"}," SAY
   s\" {\"key\":\"f\",\"label\":\"Fit\",\"group\":\"Zoom\"," SAY
   s\" \"icon\":\"M4 9V4h5M20 9V4h-5M4 15v5h5M20 15v5h-5\"}]" SAY
   18 TEXT BYTES>N TEXT-U @ 0 RECORD ;


: TYPED ( -- )
   1 EVENT@ 2 EVENT@ {: at:n u:n :}
   CALL
   at u s" rpc" IS? if 1 RPC exit then
   at u s" two" IS? if 1 RPC 2 RPC exit then
   at u s" context" IS? if
      s" ui/update-model-context" s\" {\"content\":[{\"type\":\"text\",\"text\":\"seen\"}]}" MAKE
      3 RPC exit then
   at u s" resume" IS? if
      s" view/resume" s\" {\"project\":\"p1\"}" MAKE  4 RPC exit then
   at u s" after" IS? if 11 250 7 0 RECORD exit then
   at u s" negative after" IS? if 11 -1 7 0 RECORD exit then
   at u s" short" IS? if 10 SPAN BYTES>N 7 1 RECORD exit then
   at u s" past" IS? if $C8 SPAN c! 1 RPC exit then
   at u s" empty method" IS? if 0 SPAN c! 1 RPC exit then
   at u s" bad utf8" IS? if $FF SPAN 8 + c! 1 RPC exit then
   at u s" array params" IS? if s" tools/call" s" [1]" MAKE 1 RPC exit then
   at u s" bad json" IS? if s" tools/call" s" {" MAKE 1 RPC exit then
   at u s" zero id" IS? if 0 RPC exit then
   at u s" read start" IS? if FILE-NO @ 0 3 READ exit then
   at u s" read end" IS? if FILE-NO @ FILE-SIZE @ 3 - 3 READ exit then
   at u s" read past" IS? if FILE-NO @ FILE-SIZE @ 2 - 3 READ exit then
   at u s" read unknown" IS? if FILE-NO @ 1+ 0 1 READ exit then
   at u s" svg" IS? if SVG exit then
   at u s" tree" IS? if TREE exit then
   at u s" toolbar" IS? if TOOLBAR exit then
   at u s" draw" IS? if 2 MATRIX BYTES>N 0 MATRIX BYTES>N RECORD exit then ;


\ The u64 little-endian at a delivered address.
: LE@ ( n -- n )
   {: at:n :}
   0  8 0 do  8 lshift  at 7 + i - >BYTES c@ or  loop ;


: SAY-REST ( n n -- )
   {: at:n u:n :}
   u 0 > if 32 SAY-C  at u SAY-AT then ;


\ answered <c> <d> <json>, then a space and the bytes after the
\ JSON when there are any
: ANSWERED ( -- )
   1 EVENT@ {: at:n :}
   at LE@ {: ju:n :}
   0 TEXT-U !
   s" answered " SAY  3 EVENT@ SAY-N  4 EVENT@ SAY-N
   at 8 + ju SAY-AT
   at 8 + ju +  2 EVENT@ 8 - ju -  SAY-REST  TEXT-RECORD ;


\ notified <method> <params>
: NOTIFIED ( -- )
   1 EVENT@ {: at:n :}
   at LE@ {: mu:n :}
   0 TEXT-U !
   s" notified " SAY  at 8 + mu SAY-AT  32 SAY-C
   at 8 + mu +  2 EVENT@ 8 - mu -  SAY-AT  TEXT-RECORD
   at 8 + mu s" view/restart" IS? if
      s" view/resume" s\" {\"project\":\"p2\"}" MAKE  5 RPC then ;


\ key <KeyboardEvent.key> <mods>
: KEYED ( -- )
   0 TEXT-U !
   s" key " SAY  1 EVENT@ 2 EVENT@ SAY-AT  32 SAY-C  6 EVENT@ DIGITS  TEXT-RECORD ;


\ file <name> <size> <number>
: FILE ( -- )
   3 EVENT@ FILE-SIZE !  4 EVENT@ FILE-NO !
   0 TEXT-U !
   s" file " SAY  1 EVENT@ 2 EVENT@ SAY-AT  32 SAY-C
   3 EVENT@ SAY-N  4 EVENT@ DIGITS  TEXT-RECORD ;


\ chunk <file> <offset> <buttons> <each byte in decimal, then a comma>
: CHUNK ( -- )
   1 EVENT@ 2 EVENT@ {: at:n u:n :}
   0 TEXT-U !
   s" chunk " SAY  3 EVENT@ SAY-N  4 EVENT@ SAY-N  5 EVENT@ SAY-N
   u 0 ?do at i + >BYTES c@ DIGITS 44 SAY-C loop
   TEXT-RECORD ;


: TICK ( -- )
   0 TEXT-U !  s" tick " SAY  1 EVENT@ DIGITS  TEXT-RECORD ;


: DISPATCH ( -- )
   0 EVENT@ {: kind:n :}
   kind 0 = if 0 EVENT BYTES>N TURN-REVISION:PROTOCOL 0 RECORD exit then
   kind 4 = if TYPED exit then
   kind 8 = if KEYED exit then
   kind 3 = if ANSWERED exit then
   kind 14 = if NOTIFIED exit then
   kind 15 = if TICK exit then
   kind 16 = if FILE exit then
   kind 17 = if CHUNK exit then ;

public

: TURN ( -- )
   [: DISPATCH ;] HANDLE ;

;using
;package

: MAIN ( -- ) APP-TURN:TURN ;
