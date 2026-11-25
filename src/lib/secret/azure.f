\ secret/azure.f - SECRET:AZURE-BIND, a read-only store over an Azure Key
\ Vault's REST API under the host's managed identity. Which store SECRET
\ reads is the host's choice (STORE!); this one serves the one service it
\ was bound for, an item being the vault secret the account names, and
\ throws E-ACCESS for WRITE and ERASE. An account that is not 1 to 127
\ letters, digits or hyphens, Key Vault's secret names, is E-CONFIG before
\ any request. The first value read for an item is held and served from
\ memory after, so a rotated secret takes effect at the next restart; a miss
\ or a failure holds nothing, so the next read asks again. A fetch takes
\ BEARER:IDENTITY-TOKEN's token for the vault from the metadata service at
\ the bound origin, then GETs <vault>/secrets/<account>?api-version=7.4
\ with it as Bearer on a handle of its own: a 200's "value" string is
\ appended, a 404 is a miss, a lost transfer or a status a retry may change
\ is E-UNAVAILABLE, and anything else E-ACCESS, each logged once tagged
\ secret. The token's E-TRANSFER and E-UNAVAILABLE are E-UNAVAILABLE, its
\ E-REJECTED and E-RESPONSE E-ACCESS, which BEARER logged. Contract:
\ src/lib/secret.f. The vault URL is an origin alone, https or http to a
\ loopback IP literal, one trailing `/` allowed; the metadata origin is an
\ origin alone.
\ STORAGE CLASS. PROCESS-WIDE: the vault URL, service and metadata origin,
\ written by AZURE-BIND before use; the held items, each a found item's
\ account and value, under AZURE-LOCK, held for the process's life.
\ CALLER-OWNED: each fetch's staging vector, holding the token's line, the
\ URL and the answer, and its fetched copy, each wiped whatever the fetch
\ does, its CURL handle and its reader's cursor.
require src/lib/secret.f
require src/lib/net/bearer.f
require src/lib/net/curl.f
require src/lib/reason.f
require src/lib/uri.f
require src/lib/json.f
require src/lib/log.f
require src/lib/heap.f
require src/lib/string.f
require src/lib/task.f
require src/lib/vector.f
require src/lib/json/read.f

package SECRET
private
EXPORT URI:SECURE?

127 constant AZURE-NAME-MAX         \ Key Vault's longest secret name
10000 constant AZURE-TIMEOUT-MS     \ protective: the bound on one vault answer
$40000 constant AZURE-ANSWER-CAP    \ protective: a 25 KB value as \u00XX escapes fits
TYPED-VARIABLE AZURE-VAULT ^VEC:vector<u8>
TYPED-VARIABLE AZURE-SERVICE ^VEC:vector<u8>
TYPED-VARIABLE AZURE-IMDS ^VEC:vector<u8>
HEAP AZURE-CURSOR JSON:cursor

\ An item a read found, each vector its own.
STRUCTURE azure-item DERIVE addr
   FIELD account ^VEC:vector<u8>
   FIELD value ^VEC:vector<u8>
;STRUCTURE

\ Null until the first bind; searched in order, as a store serves a handful.
TYPED-VARIABLE AZURE-ITEMS ^VEC:vector<azure-item>
TASK:FACILITY AZURE-LOCK
AZURE-LOCK TASK:FACILITY-INIT

: AZURE-VAULT$ ( -- str ) AZURE-VAULT @ VEC:BYTES ;
: AZURE-SERVICE$ ( -- str ) AZURE-SERVICE @ VEC:BYTES ;
: AZURE-IMDS$ ( -- str ) AZURE-IMDS @ VEC:BYTES ;


\ A secret name Key Vault holds, which a URL path reads as one segment.
: AZURE-NAME? ( str -- bool )
   {: name:str :}
   name STR:LEN 1 < name STR:LEN AZURE-NAME-MAX > or if false exit then
   name STR:LEN 0 ?do
      name STR:DATA i + c@
      dup STR:ALNUM? swap [char] - = or
      0= if unloop false exit then
   loop
   true ;


\ Opens a failed read's ERROR line up to its reason.
: AZURE-LINE ( str -- )
   {: name:str :}
   LOG:ERROR s" secret" LOG:LINE
   s" cannot read " LOG:TEXT+ name LOG:TEXT+ s"  from the vault: " LOG:TEXT+ ;


: AZURE-CURL-FAULT ( str CURL:code -- )
   {: name:str rc:CURL:code :}
   name AZURE-LINE
   s" curl " LOG:TEXT+ rc CURL:CODE>N LOG:INT+
   s" : " LOG:TEXT+ rc CURL:ERROR$ LOG:TEXT+ LOG:;LINE
   rc CURL:LOST? if E-UNAVAILABLE throw then
   E-ACCESS throw ;


: AZURE-SET ( str CURL:status -- )
   {: name:str st:CURL:status :}
   st MATCH CURL:status
      ok OF ENDOF
      failed OF {: rc:CURL:code :} name rc AZURE-CURL-FAULT ENDOF
   ;MATCH ;


: AZURE-AUTH ( ^VEC:vector<u8> -- )
   {: buf:^ :}
   s" Authorization: Bearer " buf VEC:APPEND
   [: AZURE-IMDS$ s" https://vault.azure.net" buf BEARER:IDENTITY-TOKEN ;]
   catch {: code:n :}
   code BEARER:E-TRANSFER = code BEARER:E-UNAVAILABLE = or
   if E-UNAVAILABLE throw then
   code BEARER:E-REJECTED = code BEARER:E-RESPONSE = or
   if E-ACCESS throw then
   code 0<> if code throw then ;


: AZURE-URL ( str ^VEC:vector<u8> -- )
   {: name:str buf:^ :}
   AZURE-VAULT$ buf VEC:APPEND  s" /secrets/" buf VEC:APPEND
   name buf VEC:APPEND  s" ?api-version=7.4" buf VEC:APPEND ;


\ The handle copies each text, so the vector is emptied after each.
: AZURE-ASK ( CURL:handle str ^VEC:vector<u8> -- )
   {: h:CURL:handle name:str buf:^ :}
   name  h buf VEC:BYTES CURL:HEADER+  AZURE-SET
   buf 0 VEC:RESIZE
   name buf AZURE-URL
   name  h buf VEC:BYTES CURL:URL!  AZURE-SET
   buf 0 VEC:RESIZE
   name  h AZURE-TIMEOUT-MS >MS CURL:TIMEOUT!  AZURE-SET ;


: AZURE-DECODE ( ^JSON:cursor ^VEC:vector<u8> ^VEC:vector<u8> -- bool )
   {: c:^ dest:^ buf:^ :}
   c buf VEC:BYTES JSON:READ-INIT
   JSON:READ-NEXT JSON:T-OBJ <> if JSON:READ-CLOSE false exit then
   s" value" JSON:READ-FIND-KEY 0= if JSON:READ-CLOSE false exit then
   JSON:READ-TOKEN JSON:T-STR <> if JSON:READ-CLOSE false exit then
   dest JSON:READ-STR-APPEND
   JSON:READ-CLOSE true ;


: AZURE-PARSED? ( ^VEC:vector<u8> ^VEC:vector<u8> -- bool )
   {: dest:^ buf:^ :}
   AZURE-CURSOR-NEW {: c:^ :}
   [: [: c dest buf AZURE-DECODE ;] REASON:HIDDEN ;]
   [: c JSON:READ-DISPOSE c AZURE-CURSOR-RELEASE ;] finally ;


: AZURE-VALUE ( str ^VEC:vector<u8> ^VEC:vector<u8> -- )
   {: name:str dest:^ buf:^ :}
   [: dest buf AZURE-PARSED? ;] catch {: found code:n :}
   code 0= if found if exit then then
   code 0<> code JSON:INPUT-FAULT? 0= and if code throw then
   name AZURE-LINE
   s" its 200 answer is not JSON holding a value string" LOG:TEXT+ LOG:;LINE
   E-ACCESS throw ;


\ A 2xx answer's text is never logged: it may hold the secret.
: AZURE-ANSWERED ( CURL:http-status str ^VEC:vector<u8> ^VEC:vector<u8>
   -- bool )
   {: st:CURL:http-status name:str dest:^ buf:^ :}
   st CURL:HTTP-STATUS>N {: code:n :}
   code 200 = if name dest buf AZURE-VALUE true exit then
   code 404 = if false exit then
   name AZURE-LINE s" status " LOG:TEXT+ code LOG:INT+
   code 200 < code 299 > or if
      s" : " LOG:TEXT+ buf VEC:BYTES LOG:TEXT+
   then
   LOG:;LINE
   st CURL:UNAVAILABLE? if E-UNAVAILABLE throw then
   E-ACCESS throw ;


: AZURE-PAST-CAP ( str -- )
   AZURE-LINE
   s" status 200 answered past " LOG:TEXT+ AZURE-ANSWER-CAP LOG:INT+
   s"  bytes" LOG:TEXT+ LOG:;LINE
   E-ACCESS throw ;


: AZURE-GET ( CURL:handle str ^VEC:vector<u8> ^VEC:vector<u8> -- bool )
   {: h:CURL:handle name:str dest:^ buf:^ :}
   h name buf AZURE-ASK
   h AZURE-ANSWER-CAP >LEN buf CURL:PERFORM
   MATCH CURL:fetch-result
      response OF drop name dest buf AZURE-ANSWERED ENDOF
      truncated OF drop {: st:CURL:http-status :}
         st CURL:HTTP-STATUS>N 200 = if name AZURE-PAST-CAP then
         st name dest buf AZURE-ANSWERED ENDOF
      failed OF {: rc:CURL:code :} name rc AZURE-CURL-FAULT ENDOF
   ;MATCH ;


\ The vector's room holds a whole answer, so no grow frees the token or the
\ answer unwiped.
: AZURE-FETCH ( str ^VEC:vector<u8> ^VEC:vector<u8> -- bool )
   {: name:str dest:^ buf:^ :}
   buf AZURE-ANSWER-CAP VEC:RESERVE
   buf AZURE-AUTH
   CURL:INIT MATCH CURL:init-result
      ready OF {: h:CURL:handle :}
         [: h name dest buf AZURE-GET ;] [: h CURL:CLEANUP ;] finally ENDOF
      failed OF {: rc:CURL:code :} name rc AZURE-CURL-FAULT ENDOF
   ;MATCH ;


: AZURE-WIPE ( ^VEC:vector<u8> -- ) dup 0 VEC:RESIZE VEC:DELETE ;


: AZURE-STAGED ( str ^VEC:vector<u8> -- bool )
   {: name:str got:^ :}
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: buf:^ :}
   [: name got buf AZURE-FETCH ;] [: buf AZURE-WIPE ;] finally ;


: AZURE-COPY ( str -- ^VEC:vector<u8> )
   {: text:str :}
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: v:^ :}
   text v VEC:APPEND v ;


\ The index of the held item whose account is the name, or -1.
: AZURE-FIND ( str -- n )
   {: name:str :}
   AZURE-ITEMS @ {: items:^ :}
   items VEC:COUNT 0 ?do
      items i VEC:AT AZURE-ITEM-ACCOUNT @ VEC:BYTES name STR:=
      if i unloop exit then
   loop
   -1 ;


\ Appends the held item's value to dest; false when none is held.
: AZURE-SERVED? ( str ^VEC:vector<u8> -- bool )
   {: name:str dest:^ :}
   name AZURE-FIND {: k:n :}
   k 0 < if false exit then
   AZURE-ITEMS @ k VEC:AT AZURE-ITEM-VALUE @ VEC:BYTES dest VEC:APPEND
   true ;


\ The table's room comes first, so the push cannot fail once the copies exist.
: AZURE-HOLD ( str str -- )
   {: name:str value:str :}
   AZURE-ITEMS @ {: items:^ :}
   items  items VEC:COUNT 1+  VEC:RESERVE
   name AZURE-COPY {: account:^ :}
   [: value AZURE-COPY ;] catch {: code:n :}
   code 0<> if account VEC:DELETE code throw then
   {: copy:^ :}
   items VEC:PUSH {: item:^ :}
   account item AZURE-ITEM-ACCOUNT !
   copy item AZURE-ITEM-VALUE ! ;


\ Holds the fetched value unless another task held the item meanwhile;
\ dest gets the held value either way.
: AZURE-KEEP ( str ^VEC:vector<u8> ^VEC:vector<u8> -- )
   {: name:str got:^ dest:^ :}
   name dest AZURE-SERVED? if exit then
   name got VEC:BYTES AZURE-HOLD
   got VEC:BYTES dest VEC:APPEND ;


\ The vault is asked with the lock not held, so reads of other items run
\ at once.
: AZURE-ASKED ( str ^VEC:vector<u8> ^VEC:vector<u8> -- bool )
   {: name:str got:^ dest:^ :}
   name got AZURE-STAGED 0= if false exit then
   AZURE-LOCK TASK:GET
   [: name got dest AZURE-KEEP ;] [: AZURE-LOCK TASK:RELEASE ;] finally
   true ;


: AZURE-READ ( str str ^VEC:vector<u8> -- bool )
   {: svc:str name:str dest:^ :}
   svc AZURE-SERVICE$ STR:= 0= if E-CONFIG throw then
   name AZURE-NAME? 0= if E-CONFIG throw then
   AZURE-LOCK TASK:GET
   [: name dest AZURE-SERVED? ;] [: AZURE-LOCK TASK:RELEASE ;] finally
   if true exit then
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: got:^ :}
   [: name got dest AZURE-ASKED ;] [: got AZURE-WIPE ;] finally ;


: AZURE-WRITE ( str str str -- )
   drop drop drop E-ACCESS throw ;


: AZURE-ERASE ( str str -- bool )
   drop drop E-ACCESS throw ;


: AZURE-TRIMMED ( str -- str )
   {: url:str :}
   url STR:LEN 0 > if
      url STR:DATA url STR:LEN 1- + c@ [char] / = if
         url STR:DATA url STR:LEN 1- >str exit
      then
   then
   url ;


\ Opens a refused bind's ERROR line up to its reason, which AZURE-REFUSED ends.
: AZURE-REFUSING ( str -- )
   {: url:str :}
   LOG:ERROR s" secret" LOG:LINE
   s" cannot bind the Azure Key Vault store at " LOG:TEXT+ url LOG:TEXT+
   s" : " LOG:TEXT+ ;


: AZURE-REFUSED ( str -- ) LOG:TEXT+ LOG:;LINE E-CONFIG throw ;


: AZURE-CHECK ( str str str -- )
   {: url:str svc:str origin:str :}
   url AZURE-TRIMMED URI:ORIGIN? 0= if
      url AZURE-REFUSING s" the vault URL is not an origin alone" AZURE-REFUSED
   then
   url AZURE-TRIMMED SECURE? 0= if
      url AZURE-REFUSING
      s" the vault URL is not https, nor http to a loopback IP literal"
      AZURE-REFUSED
   then
   origin URI:ORIGIN? 0= if
      url AZURE-REFUSING s" the metadata origin is not an origin alone"
      AZURE-REFUSED
   then
   [: svc INPUT ;] catch {: code:n :}
   code E-CONFIG = if
      url AZURE-REFUSING s" the service is empty or not text" AZURE-REFUSED
   then
   code 0<> if code throw then ;


public

\ Binds with STORE! the store over the vault for the service, once, before
\ any read; a refusal binds nothing: E-CONFIG, one ERROR line tagged secret.
: AZURE-BIND ( str str str -- )
   {: url:str svc:str origin:str :}
   url svc origin AZURE-CHECK
   url AZURE-TRIMMED AZURE-COPY {: vault:^ :}
   svc AZURE-COPY {: service:^ :}
   origin AZURE-COPY {: imds:^ :}
   vault AZURE-VAULT !
   service AZURE-SERVICE !
   imds AZURE-IMDS !
   HEAP:ALLOCATOR 0 [type] azure-item VEC:NEW AZURE-ITEMS !
   [: AZURE-READ ;] [: AZURE-WRITE ;] [: AZURE-ERASE ;] STORE! ;

;package
