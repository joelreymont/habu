\ E2E of SECRET:AZURE-BIND through the public SECRET words against one
\ loopback HTTP server answering as Azure's metadata service and a Key Vault
\ do, stopped whatever the cases do. Its metadata route answers 400 unless
\ the request has Metadata: true, the api-version and the vault's resource,
\ and a token lasting under BEARER's leeway, so each read asks again; its
\ vault route answers 401 unless that token comes as Bearer with api-version
\ 7.4, then by account. While the tasks read, the vault route holds each
\ answer until two vault requests have been in flight at once, for 5 s at
\ most. Fails on: a metadata origin with a trailing `/` bound; a vault URL
\ with a trailing `/` refused; a value with JSON escapes read wrong or not
\ after the vector's bytes; a 404 not a miss or changing the vector; a 403,
\ or a metadata 400 during a read, not E-ACCESS; a 503, or a metadata 410
\ during a read, not E-UNAVAILABLE; another service, an account with `/` or
\ of 128 bytes not E-CONFIG, or sending a request; a write or erase
\ admitted; an http URL to another host bound, or a refused bind changing
\ the bound store; tasks reading at once given another's value, or never
\ two vault requests in flight at once.
require src/lib/secret/azure.f
require src/lib/secret.f
require src/lib/test.f
require src/lib/aio.f
require src/lib/fmt.f
require src/lib/heap.f
require src/lib/span.f
require src/lib/string.f
require src/lib/task.f
require src/lib/vector.f
require src/lib/net/http.f

package SECRET-AZURE-TEST
private

$7F000001 constant LOOPBACK
4 constant WORKERS
500 constant IDLE-MS
8 constant READERS
8 constant ITEMS                    \ the items each reader reads
READERS ITEMS * constant SLOTS
$20 constant SLOT-CAP
128 constant LONG-U                 \ one past Key Vault's longest name
2 constant OVERLAP                  \ vault requests in flight at once
5000 constant HOLD-MS               \ the longest the vault route holds answers

TYPED-VARIABLE CFG HTTP:config
TYPED-VARIABLE OUT-VEC ^VEC:vector<u8>
TYPED-VARIABLE ORIGIN-VEC ^VEC:vector<u8>
variable ASKS                       \ requests either route has seen
variable IMDS-STATUS                \ the metadata route's status for a good ask
variable IN-FLIGHT                  \ vault requests being answered
variable PEAK                       \ the most IN-FLIGHT has been
variable HOLD-DUE                   \ mono-ns the vault route holds until, or 0
SLOT-CAP SLOTS * BUFFER: ACCOUNTS
SLOTS TYPED-BUFFER ACCOUNT-U n
SLOT-CAP SLOTS * BUFFER: BODIES
SLOTS TYPED-BUFFER BODY-U n
READERS TYPED-BUFFER WRONG n        \ the values each reader read wrong
LONG-U BUFFER: LONG
TASK:MIN-STACK TASK:TASK READER-0
TASK:MIN-STACK TASK:TASK READER-1
TASK:MIN-STACK TASK:TASK READER-2
TASK:MIN-STACK TASK:TASK READER-3
TASK:MIN-STACK TASK:TASK READER-4
TASK:MIN-STACK TASK:TASK READER-5
TASK:MIN-STACK TASK:TASK READER-6
TASK:MIN-STACK TASK:TASK READER-7

: SERVICE$ ( -- str ) s" org.habu.secret-azure-test" ;
: TOKEN-ANSWER$ ( -- str )
   S\" {\"access_token\":\"vault-token\",\"expires_in\":\"30\"}" ;
: ESCAPED-ANSWER$ ( -- str )
   S\" {\"id\":\"escaped\",\"value\":\"a\\\"b\\\\c\\u00e9\\/d\"}" ;
: OUT ( -- ^VEC:vector<u8> ) OUT-VEC @ ;
: OUT$ ( -- str ) OUT VEC:SPAN SPAN:$ ;
: ORIGIN$ ( -- str ) ORIGIN-VEC @ VEC:SPAN SPAN:$ ;
: LONG$ ( -- str ) LONG LONG-U >str ;

: CLEAR-OUT ( -- ) OUT 0 VEC:RESIZE ;
: HOLD ( -- ) CLEAR-OUT s" held" OUT VEC:APPEND ;

\ OUT holds what HOLD put there, and is emptied.
: HELD ( -- ) OUT$ s" held" T$= CLEAR-OUT ;


\ The text copied into the bytes at the offset; the offset past it.
: PUT ( str ^u8 n -- n )
   {: text:str to:^ at:n :}
   text STR:DATA to at + text STR:LEN BYTE-COPY at text STR:LEN + ;

: ACCOUNT-AT ( n -- ^u8 ) SLOT-CAP * ACCOUNTS + ;
: BODY-AT ( n -- ^u8 ) SLOT-CAP * BODIES + ;
: ACCOUNT$ ( n -- str ) {: k:n :} k ACCOUNT-AT k ACCOUNT-U @ >str ;
: BODY$ ( n -- str ) {: k:n :} k BODY-AT k BODY-U @ >str ;

\ The value slot k's body holds, between `{"value":"` and `"}`.
: VALUE$ ( n -- str ) {: k:n :} k BODY-AT 10 + k BODY-U @ 12 - >str ;

\ Slot k: the account item-k and the vault's answer for it, value-k.
: SLOT! ( n -- )
   {: k:n :}
   s" item-" k ACCOUNT-AT 0 PUT {: at:n :}
   k FMT:INT$ k ACCOUNT-AT at PUT k ACCOUNT-U !
   S\" {\"value\":\"value-" k BODY-AT 0 PUT {: b:n :}
   k FMT:INT$ k BODY-AT b PUT {: c:n :}
   S\" \"}" k BODY-AT c PUT k BODY-U ! ;


: SETUP ( -- )
   SLOTS 0 ?do i SLOT! loop
   LONG-U 0 ?do [char] a LONG i + c! loop
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW OUT-VEC !
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW ORIGIN-VEC !
   200 IMDS-STATUS !  0 ASKS ! ;


\ Whether the query's parameter, decoded into the vector, is the text.
: PARAM-IS? ( HTTP:request str str ^VEC:vector<u8> -- bool )
   {: asked:HTTP:request key:str want:str v:^ :}
   v 0 VEC:RESIZE
   asked key v HTTP:PARAM+ 0= if false exit then
   v VEC:SPAN SPAN:$ want STR:= ;

: HEADER-IS? ( HTTP:request str str -- bool )
   {: asked:HTTP:request key:str want:str :}
   asked key HTTP:HEADER-OF$ 0= if drop false exit then
   want STR:= ;

: IMDS-ASKED? ( HTTP:request ^VEC:vector<u8> -- bool )
   {: asked:HTTP:request v:^ :}
   asked s" Metadata" s" true" HEADER-IS? 0= if false exit then
   asked s" api-version" s" 2018-02-01" v PARAM-IS? 0= if false exit then
   asked s" resource" s" https://vault.azure.net" v PARAM-IS? ;

: IMDS-ROUTE ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   1 ASKS atomic-add drop
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: v:^ :}
   asked v IMDS-ASKED? {: ok:bool :}
   v VEC:DELETE
   ok 0= if answer 400 HTTP:STATUS! exit then
   IMDS-STATUS @ 200 <> if answer IMDS-STATUS @ HTTP:STATUS! exit then
   answer TOKEN-ANSWER$ HTTP:BODY! ;


: VAULT-ASKED? ( HTTP:request ^VEC:vector<u8> -- bool )
   {: asked:HTTP:request v:^ :}
   asked s" Authorization" s" Bearer vault-token" HEADER-IS? 0= if
      false exit
   then
   asked s" api-version" s" 7.4" v PARAM-IS? ;

\ The slot whose account is the name, -1 for none.
: SLOT-OF ( str -- n )
   {: name:str :}
   SLOTS 0 ?do name i ACCOUNT$ STR:= if i unloop exit then loop
   -1 ;

: ITEM ( str HTTP:response -- )
   {: name:str answer:HTTP:response :}
   name s" escaped" STR:= if answer ESCAPED-ANSWER$ HTTP:BODY! exit then
   name s" forbidden" STR:= if
      answer 403 HTTP:STATUS!
      answer s" Forbidden: the caller may not get secrets" HTTP:BODY! exit
   then
   name s" busy" STR:= if answer 503 HTTP:STATUS! exit then
   name SLOT-OF {: k:n :}
   k 0 < if answer 404 HTTP:STATUS! exit then
   answer k BODY$ HTTP:BODY! ;

: VAULT-ANSWER ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: v:^ :}
   asked v VAULT-ASKED? {: ok:bool :}
   v VEC:DELETE
   ok 0= if answer 401 HTTP:STATUS! exit then
   asked s" name" HTTP:SEGMENT-OF$ drop answer ITEM ;

: RAISE-PEAK ( n -- )
   {: now:n :}
   begin
      PEAK atomic@ dup now >= if drop exit then
      dup now PEAK atomic-cas =
   until ;

: HOLD-ANSWER ( -- )
   HOLD-DUE atomic@ {: due:n :}
   due 0= if exit then
   begin PEAK atomic@ OVERLAP < mono-ns due < and while
      1 >MS TASK:SLEEP
   repeat ;

: VAULT-ROUTE ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   1 ASKS atomic-add drop
   1 IN-FLIGHT atomic-add 1+ RAISE-PEAK
   [: HOLD-ANSWER asked answer VAULT-ANSWER ;]
   [: -1 IN-FLIGHT atomic-add drop ;] finally ;


: SERVE ( -- )
   LOOPBACK CFG HTTP-CONFIG:ADDRESS !
   0 CFG HTTP-CONFIG:PORT !
   WORKERS CFG HTTP-CONFIG:WORKERS !
   IDLE-MS >MS CFG HTTP-CONFIG:IDLE !
   CFG HTTP:START
   s" http://127.0.0.1:" ORIGIN-VEC @ VEC:APPEND
   HTTP:PORT FMT:INT$ ORIGIN-VEC @ VEC:APPEND ;


: FOUND ( str str -- )
   {: account:str want:str :}
   SERVICE$ account OUT SECRET:READ TTRUE
   OUT$ want T$=
   CLEAR-OUT ;


: READS-ITEM ( str n -- )
   {: account:str code:n :}
   [: SERVICE$ account OUT SECRET:READ drop ;] code TTHROWSQ ;


: TEST-READS ( -- )
   s" a metadata origin with a trailing / is E-CONFIG" T-LABEL
   ORIGIN$ OUT VEC:APPEND  s" /" OUT VEC:APPEND
   [: ORIGIN$ SERVICE$ OUT$ SECRET:AZURE-BIND ;] SECRET:E-CONFIG TTHROWSQ
   CLEAR-OUT
   s" a vault URL with a trailing / binds" T-LABEL
   ORIGIN$ OUT VEC:APPEND  s" /" OUT VEC:APPEND
   OUT$ SERVICE$ ORIGIN$ SECRET:AZURE-BIND  CLEAR-OUT
   s" a value with JSON escapes is appended after the vector's bytes" T-LABEL
   HOLD s" escaped" s\" helda\"b\\c\xC3\xA9/d" FOUND
   s" a 404 is a miss and leaves the vector's bytes" T-LABEL
   HOLD SERVICE$ s" missing" OUT SECRET:READ TFALSE HELD
   s" a 403 is E-ACCESS" T-LABEL
   s" forbidden" SECRET:E-ACCESS READS-ITEM
   s" a 503 is E-UNAVAILABLE" T-LABEL
   s" busy" SECRET:E-UNAVAILABLE READS-ITEM
   s" a metadata 410 during a read is E-UNAVAILABLE" T-LABEL
   410 IMDS-STATUS !
   s" escaped" SECRET:E-UNAVAILABLE READS-ITEM
   s" a metadata 400 during a read is E-ACCESS" T-LABEL
   400 IMDS-STATUS !
   s" escaped" SECRET:E-ACCESS READS-ITEM
   200 IMDS-STATUS ! ;


: TEST-REFUSALS ( -- )
   ASKS @ {: seen:n :}
   s" another service is E-CONFIG with no request" T-LABEL
   [: s" org.habu.other" s" escaped" OUT SECRET:READ drop ;]
   SECRET:E-CONFIG TTHROWSQ
   s" an account with / or of 128 bytes is E-CONFIG with no request" T-LABEL
   s" a/b" SECRET:E-CONFIG READS-ITEM
   LONG$ SECRET:E-CONFIG READS-ITEM
   ASKS @ seen T=
   s" write and erase are E-ACCESS" T-LABEL
   [: SERVICE$ s" escaped" s" new" SECRET:WRITE ;] SECRET:E-ACCESS TTHROWSQ
   [: SERVICE$ s" escaped" SECRET:ERASE drop ;] SECRET:E-ACCESS TTHROWSQ
   s" http to 10.0.0.1 is E-CONFIG and binds nothing" T-LABEL
   [: s" http://10.0.0.1" SERVICE$ ORIGIN$ SECRET:AZURE-BIND ;]
   SECRET:E-CONFIG TTHROWSQ
   s" escaped" s\" a\"b\\c\xC3\xA9/d" FOUND ;


\ Whether slot k reads as its own value into the vector, then emptied.
: OWN? ( ^VEC:vector<u8> n -- bool )
   {: v:^ k:n :}
   SERVICE$ k ACCOUNT$ v SECRET:READ
   v VEC:SPAN SPAN:$ k VALUE$ STR:= and
   v 0 VEC:RESIZE ;

\ Reads reader r's items, counting in WRONG r each value not its own.
: READER ( n -- )
   {: r:n :}
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: v:^ :}
   ITEMS 0 ?do
      v r ITEMS * i + OWN? 0= if 1 r WRONG +! then
   loop
   v VEC:DELETE 0 TASK:RETURN ;

: JOINED ( ^n -- )
   TASK:JOIN MATCH result
      ok OF ENDOF
      err OF throw ENDOF
   ;MATCH 0 T= ;

: READ-ALL ( -- )
   [: 0 READER ;] READER-0 TASK:ACTIVATE
   [: 1 READER ;] READER-1 TASK:ACTIVATE
   [: 2 READER ;] READER-2 TASK:ACTIVATE
   [: 3 READER ;] READER-3 TASK:ACTIVATE
   [: 4 READER ;] READER-4 TASK:ACTIVATE
   [: 5 READER ;] READER-5 TASK:ACTIVATE
   [: 6 READER ;] READER-6 TASK:ACTIVATE
   [: 7 READER ;] READER-7 TASK:ACTIVATE
   READER-0 JOINED READER-1 JOINED READER-2 JOINED READER-3 JOINED
   READER-4 JOINED READER-5 JOINED READER-6 JOINED READER-7 JOINED ;

: TEST-TASKS ( -- )
   s" eight tasks reading eight items each at once read their own" T-LABEL
   READERS 0 ?do 0 i WRONG ! loop
   0 PEAK atomic!
   mono-ns HOLD-MS 1000000 * + HOLD-DUE atomic!
   [: READ-ALL ;] [: 0 HOLD-DUE atomic! ;] finally
   READERS 0 ?do i WRONG @ 0 T= loop
   s" two of their vault requests are in flight at once" T-LABEL
   PEAK atomic@ OVERLAP >= TTRUE ;


: CASES ( -- )
   SERVE TEST-READS TEST-REFUSALS TEST-TASKS ;


: TEST-MAIN ( -- )
   T-RESET SETUP AIO:START
   HTTP:ROUTES-RESET
   s" GET" s" /metadata/identity/oauth2/token" [: IMDS-ROUTE ;] HTTP:ROUTE
   s" GET" s" /secrets/{name}" [: VAULT-ROUTE ;] HTTP:ROUTE
   [: CASES ;] [: HTTP:STOP AIO:STOP ;] finally
   OUT VEC:DELETE ORIGIN-VEC @ VEC:DELETE
   T-REPORT ;
TEST-MAIN
;package
