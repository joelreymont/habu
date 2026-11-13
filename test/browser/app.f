\ app.f - real Wasm fixture for the turn protocol's rpc, after and read records
\ and its notified, tick, file and chunk events, built by `tools/hb-build.f --
\ --target wasm`, whose entry is MAIN. Each event it reads answers a text record
\ naming its fields; typed text names the record to make.

require src/lib/error.f
require src/lib/string.f
require src/lib/browser/event.f

package APP-TURN
private
using BROWSER-EVENT

CAST: >BYTES ( n -- ptr u8 )
CAST: BYTES>N ( ptr u8 -- n )

256 BUFFER: SPAN
variable SPAN-U
1024 BUFFER: TEXT
variable TEXT-U
\ The last file event's number and size.
variable FILE-NO
variable FILE-SIZE

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


: SAY ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0 ?do a i + c@ SAY-C loop ;


\ Delivered bytes are freed once the turn ends, so the text copies them.
: SAY-AT ( n n -- )
   {: at:n u:n :}
   u 0 ?do at i + >BYTES c@ SAY-C loop ;


: SAY-N ( n -- )  DIGITS 32 SAY-C ;

: TEXT-RECORD ( -- )
   3 TEXT BYTES>N TEXT-U @ 0 RECORD ;

: SPAN-C ( n -- )
   SPAN SPAN-U @ + c! SPAN-U @ 1+ SPAN-U ! ;


: SPAN+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   u 0 ?do a i + c@ SPAN-C loop ;


: SPAN-CELL ( n -- )
   8 0 do dup $FF and SPAN-C 8 rshift loop drop ;


\ An rpc's span: the method's u64 length, the method, then the params.
: MAKE ( ptr u8 n ptr u8 n -- )
   {: m:ptr mu:n p:ptr pu:n :}
   0 SPAN-U !
   mu SPAN-CELL m mu SPAN+ p pu SPAN+ ;


: CALL ( -- )
   s" tools/call" s\" {\"name\":\"get_project\"}" MAKE ;

: RPC ( n -- )
   {: id:n :}
   10 SPAN BYTES>N SPAN-U @ id RECORD ;


: READ ( n n n -- )
   {: file:n offset:n length:n :}
   12 file offset length RECORD ;


: IS? ( n n ptr u8 n -- bool )
   {: at:n u:n t:ptr tu:n :}
   u tu <> if false exit then
   tu 0 ?do at i + >BYTES c@ t i + c@ <> if false unloop exit then loop
   true ;


: TYPED ( -- )
   1 EVENT@ 2 EVENT@ {: at:n u:n :}
   CALL
   at u s" rpc" IS? if 1 RPC exit then
   at u s" two" IS? if 1 RPC 2 RPC exit then
   at u s" context" IS? if
      s" ui/update-model-context" s\" {\"content\":[{\"type\":\"text\",\"text\":\"seen\"}]}" MAKE
      3 RPC exit then
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
   at u s" read unknown" IS? if FILE-NO @ 1+ 0 1 READ exit then ;


\ answered <c> <d> <the response's bytes>
: ANSWERED ( -- )
   0 TEXT-U !
   s" answered " SAY  3 EVENT@ SAY-N  4 EVENT@ SAY-N
   1 EVENT@ 2 EVENT@ SAY-AT  TEXT-RECORD ;


\ The u64 little-endian at a delivered address.
: LE@ ( n -- n )
   {: at:n :}
   0  8 0 do  8 lshift  at 7 + i - >BYTES c@ or  loop ;


\ notified <method> <params>
: NOTIFIED ( -- )
   1 EVENT@ {: at:n :}
   at LE@ {: mu:n :}
   0 TEXT-U !
   s" notified " SAY  at 8 + mu SAY-AT  32 SAY-C
   at 8 + mu +  2 EVENT@ 8 - mu -  SAY-AT  TEXT-RECORD ;


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
   kind 0 = if 0 EVENT BYTES>N PROTOCOL 0 RECORD exit then
   kind 4 = if TYPED exit then
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
