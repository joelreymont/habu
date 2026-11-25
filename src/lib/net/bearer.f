\ bearer.f - OAuth bearer tokens (RFC 6750) on Habu's HTTP server. A route
\ admits a request whose Authorization carries a compact JWS that one issuer
\ signed RS256 with a key of its JWK Set, whose iss and aud name that issuer
\ and this server, and whose exp and nbf hold (RFC 7515, 7517, 7519). The host
\ states the issuer with ISSUER! before HTTP:START, guards a route with
\ `req res BEARER:ADMIT? if ... then` and reads the caller's claims with CLAIM$
\ and CLAIM-HOLDS?. CALLER? guards a route for a person whose token holds the
\ stated scope or an app APP! lists; PERSON? and CALLER-KEY say which it is.
\ METADATA! names this server's protected-resource metadata
\ (RFC 9728 5.1) and the scope its tokens need (RFC 6750 3) in each 401
\ challenge, and RESOURCE-METADATA answers that metadata's GET (RFC 9728 3),
\ which METADATA-ROUTES routes to it; beside that scope both name
\ offline_access, without which Entra gives a client no refresh token.
\ As a client, TOKEN gets a token from the token endpoint CLIENT! names, for
\ the client itself (RFC 6749 4.4) or for a person (Entra On-Behalf-Of), and
\ REFRESH renews one with a refresh grant (RFC 6749 6); CURRENT keeps the
\ client's own token per scope. IDENTITY-TOKEN appends a managed identity's
\ token for a resource from Azure's metadata service at an origin (IMDS$),
\ kept per origin and resource. Every other URL BEARER fetches or advertises
\ is https, or http to a loopback IP literal for tests.
\ STORAGE CLASS. PROCESS-WIDE: the issuer, the apps APP! lists, the challenges,
\ resource and scope METADATA! states, and the path METADATA-ROUTES routes,
\ written before HTTP serves; the key set, read and replaced under KEYS-LOCK and
\ fetched over CURL outside it by one worker at a time; the counts of workers
\ waiting for that fetch and of fetches returned, under KEYS-LOCK, and the
\ semaphore that wakes them, which ISSUER! sets up; tables indexed by
\ HTTP:WORKER-SLOT holding each request's token and scratch; the client CLIENT!
\ states, written before any TOKEN or REFRESH; and the rows CURRENT keeps, one
\ per scope, and IDENTITY-TOKEN keeps, one per origin and resource, written
\ under ROWS-LOCK when linked and each under its own lock after; CLIENT! empties
\ CURRENT's.
\ TASK-LOCAL: the admitted request, TASK:+USER rows.
\
\ Key lookup: a worker looks for the token's kid in the cached key set under
\ KEYS-LOCK. When the set is empty or lacks the kid, no worker is fetching and
\ no fetch returned within the last REFETCH-MS, that worker becomes the one
\ fetcher: it fetches outside the lock, an accepted set replaces the cached one,
\ and it looks again. A worker that finds the kid missing while a fetch is in
\ flight waits for that fetch, then looks again; the others answer from the set
\ as it stands. A WAKE count may be an earlier fetch's: a waiter whose fetch
\ has not returned counts itself again and waits.

require src/lib/string.f
require src/lib/string-option.f
require src/lib/span.f
require src/lib/memory.f
require src/lib/base64.f
require src/lib/json.f
require src/lib/log.f
require src/lib/task.f
require src/lib/allocator.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/adt/option.f
require src/lib/crypto/evp.f
require src/lib/net/curl.f
require src/lib/net/http.f
require src/lib/reason.f
require src/lib/uri.f
require src/lib/net/http-arena.f
require src/lib/net/http-request.f
require src/lib/net/http-response.f
require src/lib/json/read.f
require src/lib/json/value.f
require src/lib/json/write.f
require src/lib/net/http-router.f

package BEARER
public

-9630 s" no answer from the token endpoint" ERROR:CODE E-TRANSFER
-9631 s" the issuer rejected the request with a 4xx a retry will not change" ERROR:CODE E-REJECTED
-9632 s" a URL is not https (state https, or http only to 127.0.0.0/8 or [::1]), a METADATA! URL is not /.well-known/oauth-protected-resource right after its authority, then nothing or a path with no query or fragment (insert the segment before the resource's path; drop the query and fragment), a METADATA! URL holds a quote, backslash, brace, control or non-ASCII byte (percent-encode it), a scope is not tokens of printable ASCII but quote and backslash one space apart (remove the other bytes and spaces), or an APP! id is empty or holds a space, control or non-ASCII byte (state the app's client id alone)"
   ERROR:CODE E-OPERAND
-9633 s" the token endpoint answered a status a retry may change" ERROR:CODE E-UNAVAILABLE
-9634 s" the token endpoint answered a reply TOKEN or REFRESH cannot use" ERROR:CODE E-RESPONSE
private
EXPORT URI:DOT
EXPORT URI:SECURE?
-9630 constant E-FIRST
-9639 constant E-LAST

60 constant LEEWAY-S                \ seconds of clock skew between issuer and server
$10000 constant KEYS-CAP            \ the largest key set text accepted
5000 constant KEYS-TIMEOUT-MS
1000 constant REFETCH-MS
1000000 constant NS-PER-MS
8 constant EXPONENT-CAP

0 constant ADMITTED
1 constant NO-TOKEN                 \ 401 challenging Bearer
2 constant INVALID                  \ 401 challenging Bearer error="invalid_token"
3 constant UNAVAILABLE              \ 503: no key set, and none could be fetched

PTR-VARIABLE ISSUER-AT   SCRATCH-VARIABLE ISSUER-LEN
PTR-VARIABLE AUDIENCE-AT   SCRATCH-VARIABLE AUDIENCE-LEN
PTR-VARIABLE KEYS-URL-AT   SCRATCH-VARIABLE KEYS-URL-LEN

: ISSUER$ ( -- str ) ISSUER-AT @ ISSUER-LEN @ >str ;
: AUDIENCE$ ( -- str ) AUDIENCE-AT @ AUDIENCE-LEN @ >str ;
: KEYS-URL$ ( -- str ) KEYS-URL-AT @ KEYS-URL-LEN @ >str ;

\ NAMED-LEN bytes at NAMED, an allocation holding METADATA!'s statement: the
\ two challenges naming its URL and scope, CHALLENGES-U bytes, the no-token
\ one's NAMED-U bytes first; then the resource its URL names, RESOURCE-U
\ bytes, none without a URL; then the scope. Empty while neither is stated. A
\ built program starts with the plain challenges: NAMED is a PTR-VARIABLE and
\ the lengths are scratch cells, each written as 0 in the built program.
PTR-VARIABLE NAMED
SCRATCH-VARIABLE NAMED-LEN
SCRATCH-VARIABLE NAMED-U
SCRATCH-VARIABLE CHALLENGES-U
SCRATCH-VARIABLE RESOURCE-U

: NAMED-SPAN ( -- SPAN:span<u8> ) NAMED @ NAMED-LEN @ SPAN:MAKE ;
: CHALLENGES ( -- SPAN:span<u8> ) NAMED-SPAN CHALLENGES-U @ SPAN:TAKE ;

: RESOURCE$ ( -- str )
   NAMED-SPAN CHALLENGES-U @ SPAN:SKIP RESOURCE-U @ SPAN:TAKE SPAN:$ ;

: SCOPE$ ( -- str )
   NAMED-SPAN CHALLENGES-U @ RESOURCE-U @ + SPAN:SKIP SPAN:$ ;

\ The path METADATA-ROUTES gave HTTP, whose routes read it in place; made at
\ its first call, so a built program keeps none.
TYPED-VARIABLE ROUTE-PATH ^VEC:vector<u8> SCRATCH-ONLY

\ The ids APP! lists, one space apart; made at the first APP!, so a built
\ program keeps none.
TYPED-VARIABLE APPS ^VEC:vector<u8> SCRATCH-ONLY

: APPS$ ( -- str )
   APPS @ dup NULL-PTR = if drop s" " exit then
   VEC:BYTES ;

\ The key set's text, under KEYS-LOCK with KEYS-AT, FETCHING, WAITERS, ROUND,
\ KEY-ARRAY and ARRAY-ROUND; FETCHED and ACCEPTED are the fetcher's alone.
KEYS-CAP SCRATCH-BUFFER: KEYS
SCRATCH-VARIABLE KEYS-U
SCRATCH-VARIABLE KEYS-AT            \ when the last fetch returned, a mono-ns; 0 before any
TYPED-VARIABLE FETCHING bool SCRATCH-ONLY
SCRATCH-VARIABLE WAITERS            \ WAKE posts RETURNED owes, one per wait
SCRATCH-VARIABLE ROUND              \ fetches returned
TYPED-VARIABLE KEY-ARRAY JSON:value SCRATCH-ONLY \ KEYS's keys array, as of round ARRAY-ROUND
SCRATCH-VARIABLE ARRAY-ROUND
TASK:SEMAPHORE WAKE
TYPED-VARIABLE FETCHED ^VEC:vector<u8> SCRATCH-ONLY
TYPED-VARIABLE ACCEPTED bool SCRATCH-ONLY \ FETCHED holds a key set
TASK:FACILITY KEYS-LOCK
KEYS-LOCK TASK:FACILITY-INIT

\ A worker's scratch: the decoded segments, no longer than the token and so
\ than the head; a note holding the kid, then the claim CLAIM$ lends or
\ CALLER-KEY appends; the string claim CLAIM-HOLDS? or PERSON? reads, apart
\ from the note; the key.
0 constant SEGMENTS-OFF
HTTP:HEAD-CAP constant NOTE-OFF
HTTP:HEAD-CAP 2 * constant WORDS-OFF
HTTP:HEAD-CAP 3 * constant MODULUS-OFF
MODULUS-OFF CRYPTO:RS256-MAX-BYTES + constant EXPONENT-OFF
EXPONENT-OFF EXPONENT-CAP + constant SCRATCH-BYTES
HTTP:MAX-WORKERS SCRATCH-BYTES * constant SCRATCHES-BYTES
create SCRATCH-ROWS SCRATCHES-BYTES SCRATCH-ALLOT
: SCRATCH ( -- SPAN:span<u8> ) SCRATCH-ROWS SCRATCHES-BYTES SPAN:MAKE ;

\ PRESENTED and SIGNED borrow the request's header; the rest borrow the worker's
\ scratch.
HTTP:MAX-WORKERS TYPED-BUFFER PRESENTED SPAN:span<u8> SCRATCH-ONLY
HTTP:MAX-WORKERS TYPED-BUFFER SIGNED SPAN:span<u8> SCRATCH-ONLY
HTTP:MAX-WORKERS TYPED-BUFFER HEADER JSON:value SCRATCH-ONLY
HTTP:MAX-WORKERS TYPED-BUFFER CLAIMS JSON:value SCRATCH-ONLY
HTTP:MAX-WORKERS TYPED-BUFFER SIGNATURE SPAN:span<u8> SCRATCH-ONLY
HTTP:MAX-WORKERS TYPED-BUFFER KID SPAN:span<u8> SCRATCH-ONLY
HTTP:MAX-WORKERS TYPED-BUFFER MODULUS SPAN:span<u8> SCRATCH-ONLY
HTTP:MAX-WORKERS TYPED-BUFFER EXPONENT SPAN:span<u8> SCRATCH-ONLY

\ The running task's: PASSED is nonzero while FROM, a request's number, stands
\ admitted by ADMIT? and not rejected by CALLER?.
TASK:+USER PASSED n
TASK:+USER FROM n

: SLOT ( -- n ) HTTP:WORKER-SLOT ;
: HEADER@ ( -- JSON:value ) SLOT HEADER @ ;
: CLAIMS@ ( -- JSON:value ) SLOT CLAIMS @ ;

: ROOM ( n n -- SPAN:span<u8> )
   {: off:n len:n :}
   SCRATCH SLOT SCRATCH-BYTES * off + len SPAN:SUB ;

: DECODED ( str SPAN:span<u8> -- SPAN:span<u8> )
   {: s:str room :}
   s room BASE64:DECODE-URL {: got:n :}
   room 0 got SPAN:SUB ;


\ Input's fault: a JSON reader code JSON:INPUT-FAULT? names, or malformed
\ base64url.
: MALFORMED? ( n -- bool )
   {: code:n :}
   code JSON:INPUT-FAULT?
   code E-BASE64-LAST >= code E-BASE64-FIRST <= and or ;

: STEP ( [ bool -- bool ] -- bool )
   false swap catch {: ok code:n :}
   code 0= if ok exit then
   code MALFORMED? if false exit then
   code throw ;


: HAS? ( JSON:value str -- bool )
   JSON:FIND MATCH option
      none OF false ENDOF
      some OF {: v :} true ENDOF
   ;MATCH ;

\ Whether the member is a string equal to the text; the flag when the object
\ lacks it.
: MEMBER-IS? ( JSON:value str str bool -- bool )
   {: obj k:str w:str absent:bool :}
   obj k JSON:FIND MATCH option
      none OF absent ENDOF
      some OF w JSON:STRING= ENDOF
   ;MATCH ;

: INT-OF ( JSON:value -- n bool )
   {: v :}
   v JSON:INTEGER? 0= if 0 false exit then
   v JSON:>N true ;

\ An integer member that fits a cell; false when it is absent or no such
\ number.
: MEMBER-INT ( JSON:value str -- n bool )
   JSON:FIND MATCH option
      none OF 0 false ENDOF
      some OF INT-OF ENDOF
   ;MATCH ;

\ A string decoded into the room, its length; -1 for another kind. A string
\ longer than the room throws E-SPAN-CAPACITY.
: COPIED ( str SPAN:span<u8> -- n )
   {: s:str room :}
   s room SPAN:COPY s STR:LEN ;

: INTO-ROOM ( JSON:value SPAN:span<u8> -- n )
   {: v room :}
   v JSON:STRING? 0= if -1 exit then
   v [: room COPIED ;] JSON:WITH-STRING ;

\ A string member decoded into the room; false when it is absent or no string.
: DECODED$ ( JSON:value str SPAN:span<u8> -- str bool )
   {: room :}
   JSON:FIND MATCH option
      none OF -1 ENDOF
      some OF room INTO-ROOM ENDOF
   ;MATCH {: got:n :}
   got 0< if s" " false exit then
   room SPAN:$ STR:DATA got >str true ;

: NOTE$ ( JSON:value str -- str bool )
   NOTE-OFF HTTP:HEAD-CAP ROOM DECODED$ ;


\ The token of the sole Authorization, when its scheme is Bearer, compared
\ case-insensitively (RFC 6750 2.1).
: TOKEN? ( HTTP:request -- bool )
   {: asked:HTTP:request :}
   asked s" Authorization" HTTP:HEADER-COUNT-OF 1 <> if false exit then
   asked s" Authorization" HTTP:HEADER-OF$ drop {: v:str :}
   v STR:LEN 7 <= if false exit then
   v STR:DATA 6 >str s" Bearer" STR:=CI 0= if false exit then
   v STR:DATA 6 + c@ 32 <> if false exit then
   v STR:DATA 7 + v STR:LEN 7 - SPAN:MAKE SLOT PRESENTED ! true ;

: SEGMENT ( str n -- SPAN:span<u8> )
   {: s:str off:n :}
   s off HTTP:HEAD-CAP off - ROOM DECODED ;

\ The segments decoded into the worker's scratch, true for an object header
\ naming a kid and object claims; a repeated member throws (RFC 7519 §4).
: OPEN ( -- bool )
   SLOT PRESENTED @ SPAN:$ {: s:str :}
   s STR:DATA {: t:^ :}
   s STR:LEN {: u:n :}
   s 0 DOT {: d1:n :}
   d1 u = if false exit then
   s d1 1+ DOT {: d2:n :}
   d2 u = if false exit then
   s d2 1+ DOT u <> if false exit then
   t d1 >str 0 SEGMENT {: head :}
   t d1 + 1+ d2 d1 - 1- >str head SPAN:LEN SEGMENT {: body :}
   t d2 + 1+ u d2 - 1- >str head SPAN:LEN body SPAN:LEN + SEGMENT
      SLOT SIGNATURE !
   t d2 SPAN:MAKE SLOT SIGNED !
   head SPAN:$ JSON:PARSE {: h :}
   h JSON:OBJECT? 0= if false exit then
   body SPAN:$ JSON:PARSE {: c :}
   c JSON:OBJECT? 0= if false exit then
   h SLOT HEADER !
   c SLOT CLAIMS !
   h s" kid" NOTE$ {: k:str named:bool :}
   k STR:DATA k STR:LEN SPAN:MAKE SLOT KID ! named ;


\ A string base64url-decoded into the room; empty for another kind.
: KEY-BYTES ( JSON:value SPAN:span<u8> -- SPAN:span<u8> )
   {: v room :}
   v JSON:STRING? 0= if s" " room DECODED exit then
   v [: room DECODED ;] JSON:WITH-STRING ;

\ A string member base64url-decoded into the room; empty when it is absent or
\ no string.
: KEY-PART ( JSON:value str SPAN:span<u8> -- SPAN:span<u8> )
   {: room :}
   JSON:FIND MATCH option
      none OF s" " room DECODED ENDOF
      some OF room KEY-BYTES ENDOF
   ;MATCH ;

\ Whether a key set's element is the token's key, its n and e then decoded into
\ scratch; a broken one throws, being the issuer's fault, not the caller's.
: PICK? ( JSON:value -- bool )
   {: m :}
   m JSON:OBJECT? 0= if false exit then
   m s" kid" SLOT KID @ SPAN:$ false MEMBER-IS? 0= if false exit then
   m s" kty" s" RSA" false MEMBER-IS? 0= if false exit then
   m s" use" s" sig" true MEMBER-IS? 0= if false exit then
   m s" alg" s" RS256" true MEMBER-IS? 0= if false exit then
   m s" n" MODULUS-OFF CRYPTO:RS256-MAX-BYTES ROOM KEY-PART SLOT MODULUS !
   m s" e" EXPONENT-OFF EXPONENT-CAP ROOM KEY-PART SLOT EXPONENT !
   true ;

\ Under KEYS-LOCK: KEYS changes only with ROUND, so one parse serves a round.
\ The cached set is one a fetch accepted, so its keys member is an array.
: KEY-LIST ( -- JSON:value )
   ARRAY-ROUND @ ROUND @ <> if
      KEYS KEYS-U @ >str JSON:PARSE s" keys" JSON:GET KEY-ARRAY !
      ROUND @ ARRAY-ROUND !
   then KEY-ARRAY @ ;

: SELECT? ( -- bool )
   KEYS-U @ 0= if false exit then
   false KEY-LIST [: PICK? nip dup 0= ;] JSON:EACH-WHILE ;


: OK? ( CURL:status -- bool )
   MATCH CURL:status
      ok OF true ENDOF
      failed OF drop false ENDOF
   ;MATCH ;

: FETCHED-LEN ( CURL:fetch-result -- n )
   MATCH CURL:fetch-result
      response OF {: status:CURL:http-status got:len :}
         status CURL:HTTP-STATUS>N 200 = if got LEN>N else -1 then ENDOF
      truncated OF drop drop -1 ENDOF
      failed OF drop -1 ENDOF
   ;MATCH ;

: TRANSFER ( CURL:handle -- n )
   {: h:CURL:handle :}
   h KEYS-URL$ CURL:URL! OK? 0= if -1 exit then
   h KEYS-TIMEOUT-MS >MS CURL:TIMEOUT! OK? 0= if -1 exit then
   h KEYS-CAP >LEN FETCHED @ CURL:PERFORM FETCHED-LEN ;

: FETCHED$ ( -- str ) FETCHED @ VEC:BYTES ;

: KEY-SET? ( -- bool )
   FETCHED$ JSON:PARSE {: set :}
   set JSON:OBJECT? 0= if false exit then
   set s" keys" JSON:FIND MATCH option
      none OF false ENDOF
      some OF JSON:ARRAY? ENDOF
   ;MATCH ;

\ TRANSFER's answer in place of the second cell, the handle kept for catch.
: TRANSFERRED ( CURL:handle n -- CURL:handle n ) drop dup TRANSFER ;

\ FETCHED is made on the first fetch: a block made at load lives in the
\ loading process, never in a program built from it.
: FETCHED-READY ( -- )
   FETCHED @ NULL-PTR <> if FETCHED @ 0 VEC:RESIZE exit then
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW FETCHED ! ;

\ The issuer's key set appended to the empty FETCHED, at most KEYS-CAP bytes,
\ outside KEYS-LOCK: ACCEPTED when it was answered whole with 200 and is a
\ key set.
: FETCH ( -- )
   false ACCEPTED !
   FETCHED-READY
   CURL:INIT
   MATCH CURL:init-result
      ready OF {: h:CURL:handle :}
         h -1 [: TRANSFERRED ;] catch {: code:n :}
         h CURL:CLEANUP
         code 0<> if 2drop code throw then
         nip ENDOF
      failed OF drop -1 ENDOF
   ;MATCH 0< if exit then
   [: drop KEY-SET? ;] STEP ACCEPTED ! ;

: LOCKED ( R [ R -- S ] -- S )
   KEYS-LOCK TASK:GET [: KEYS-LOCK TASK:RELEASE ;] finally ;

\ Under KEYS-LOCK.
: DUE? ( -- bool )
   KEYS-AT @ 0= if true exit then
   mono-ns KEYS-AT @ - REFETCH-MS NS-PER-MS * >= ;

\ Under KEYS-LOCK: the token's key from the set as it stands, into the worker's
\ scratch: ADMITTED, INVALID for a kid the set lacks, UNAVAILABLE for no set.
: CACHED ( -- n )
   KEYS-U @ 0= if UNAVAILABLE exit then
   SELECT? if ADMITTED else INVALID then ;

ENUM lookup-step
   VARIANT answer FIELD outcome n ;VARIANT
   VARIANT fetch ;VARIANT
   VARIANT wait FIELD round n ;VARIANT
;ENUM

\ Under KEYS-LOCK.
: LOOKUP ( -- lookup-step )
   CACHED {: found:n :}
   found ADMITTED = if found construct lookup-step answer exit then
   FETCHING @ if
      1 WAITERS +! ROUND @ construct lookup-step wait exit
   then
   DUE? 0= if found construct lookup-step answer exit then
   true FETCHING ! construct lookup-step fetch ;

\ Under KEYS-LOCK: FETCHED, at most KEYS-CAP bytes, into KEYS.
: KEEP-SET ( -- )
   FETCHED$ {: src:str :}
   src STR:DATA KEYS src STR:LEN BYTE-COPY src STR:LEN KEYS-U ! ;

\ Under KEYS-LOCK: the fetch has returned, by a throw too.
: RETURNED ( -- )
   ACCEPTED @ if KEEP-SET then
   mono-ns KEYS-AT !
   1 ROUND +!
   false FETCHING !
   WAITERS @ 0 ?do WAKE TASK:SIGNAL-SEM loop
   0 WAITERS ! ;

: SETTLE ( -- ) [: RETURNED ;] LOCKED ;

\ Under KEYS-LOCK.
: ROUND-OVER? ( n -- bool )
   {: r:n :}
   ROUND @ r <> if true exit then
   1 WAITERS +! false ;

: AWAIT ( n -- )
   {: r:n :}
   begin WAKE TASK:WAIT-SEM r [: ROUND-OVER? ;] LOCKED until ;

\ The token's key into the worker's scratch, as CACHED answers.
\ KEYS-TIMEOUT-MS bounds the fetch a waiter waits for.
: KEY ( -- n )
   [: LOOKUP ;] LOCKED MATCH lookup-step
      answer OF ENDOF
      fetch OF
         [: FETCH ;] [: SETTLE ;] finally [: CACHED ;] LOCKED
      ENDOF
      wait OF AWAIT [: CACHED ;] LOCKED ENDOF
   ;MATCH ;


\ Only RS256 is ever verified, which rejects alg none and HS256; a header with
\ crit names extensions this verifier does not understand (RFC 7515 4.1.11).
: RS256? ( -- bool )
   HEADER@ s" alg" s" RS256" false MEMBER-IS? 0= if false exit then
   HEADER@ s" crit" HAS? if false exit then
   SLOT MODULUS @ SPAN:$ SLOT EXPONENT @ SPAN:$
   SLOT SIGNED @ SPAN:$ SLOT SIGNATURE @ SPAN:$ CRYPTO:RS256-VERIFY? ;

\ Whether the value is the string or an array holding it as an element.
: CONTAINS? ( JSON:value str -- bool )
   {: v w:str :}
   v JSON:STRING? if v w JSON:STRING= exit then
   v JSON:ARRAY? 0= if false exit then
   false v [: w JSON:STRING= nip dup 0= ;] JSON:EACH-WHILE ;

: HOLDS? ( JSON:value str str -- bool )
   {: obj k:str w:str :}
   obj k JSON:FIND MATCH option
      none OF false ENDOF
      some OF w CONTAINS? ENDOF
   ;MATCH ;

: AUDIENCE? ( -- bool ) CLAIMS@ s" aud" AUDIENCE$ HOLDS? ;

\ The offset of the first space at or after from, or the end.
: SPACE-AT ( str n -- n )
   {: s:str from:n :}
   s STR:LEN from ?do
      s STR:DATA i + c@ $20 = if i unloop exit then
   loop s STR:LEN ;

\ Whether the name is one of the text's words from the offset on, runs of
\ bytes but spaces.
: WORD-FROM? ( str str n -- bool )
   {: s:str w:str from:n :}
   from begin {: at:n :} at s STR:LEN < while
      s at SPACE-AT {: to:n :}
      to at > if
         s STR:DATA at + to at - >str w STR:= if true exit then
      then
      to 1+
   repeat false ;

\ Appends a word to a list WORD-FROM? reads.
: WORD+ ( str ^VEC:vector<u8> -- )
   {: w:str list:^ :}
   list VEC:COUNT 0 > if s"  " list VEC:APPEND then
   w list VEC:APPEND ;

: WORDS$ ( str -- str bool )
   CLAIMS@ swap WORDS-OFF HTTP:HEAD-CAP ROOM DECODED$ ;

\ A string claim splits into words here, where AUDIENCE? compares aud whole
\ (RFC 7519 4.1.3).
: HELD? ( str str -- bool )
   {: name:str w:str :}
   name WORDS$ if w 0 WORD-FROM? exit then
   drop CLAIMS@ name w HOLDS? ;

: VALID? ( n -- bool )
   {: now:n :}
   CLAIMS@ s" iss" ISSUER$ false MEMBER-IS? 0= if false exit then
   AUDIENCE? 0= if false exit then
   CLAIMS@ s" exp" MEMBER-INT 0= if drop false exit then
   now LEEWAY-S - > 0= if false exit then
   CLAIMS@ s" nbf" HAS? 0= if true exit then
   CLAIMS@ s" nbf" MEMBER-INT 0= if drop false exit then
   now LEEWAY-S + <= ;


: OUTCOME ( HTTP:request -- n )
   TOKEN? 0= if NO-TOKEN exit then
   [: drop OPEN ;] STEP 0= if INVALID exit then
   KEY {: found:n :}
   found ADMITTED <> if found exit then
   RS256? 0= if INVALID exit then
   [: drop epoch-seconds VALID? ;] STEP 0= if INVALID exit then
   ADMITTED ;

: CHALLENGE ( HTTP:response str -- )
   {: answer:HTTP:response value:str :}
   answer 401 HTTP:STATUS!
   answer s" WWW-Authenticate" value HTTP:HEADER! ;

\ The challenge for NO-TOKEN or INVALID.
: CHALLENGE$ ( n -- str )
   {: outcome:n :}
   CHALLENGES {: named :}
   named SPAN:LEN 0= if
      outcome NO-TOKEN = if s" Bearer" exit then
      S\" Bearer error=\"invalid_token\"" exit
   then
   outcome NO-TOKEN = if named NAMED-U @ SPAN:TAKE SPAN:$ exit then
   named NAMED-U @ SPAN:SKIP SPAN:$ ;

: REJECT ( HTTP:response n -- )
   {: answer:HTTP:response outcome:n :}
   outcome UNAVAILABLE = if answer 503 HTTP:STATUS! exit then
   answer outcome CHALLENGE$ CHALLENGE ;

: NAMED-PREFIX ( -- str ) s" Bearer " ;
: INVALID-PREFIX ( -- str ) S\" Bearer error=\"invalid_token\", " ;
: METADATA-NAME ( -- str ) s" resource_metadata" ;
: SCOPE-NAME ( -- str ) s" scope" ;
: SEPARATOR ( -- str ) s" , " ;

: PUT ( str SPAN:span<u8> n -- n )
   {: s:str row off:n :}
   s row off SPAN:SKIP SPAN:COPY
   off s STR:LEN + ;

\ The bytes a parameter `name="value"` takes, from the name's and the value's
\ lengths; none for an empty value, which names nothing.
: PARAM-LEN ( n n -- n )
   {: nu:n vu:n :}
   vu 0= if 0 exit then
   nu vu + 3 + ;


\ Writes nothing for an empty value, as PARAM-LEN measures nothing.
: PARAM ( str str SPAN:span<u8> n -- n )
   {: name:str value:str row off:n :}
   value STR:LEN 0= if off exit then
   name row off PUT {: past-name:n :}
   S\" =\"" row past-name PUT {: past-eq:n :}
   value row past-eq PUT {: past-value:n :}
   S\" \"" row past-value PUT ;


: NAMING ( str str str SPAN:span<u8> n -- n )
   {: pre:str url:str scope:str row off:n :}
   pre row off PUT {: past-pre:n :}
   METADATA-NAME url row past-pre PARAM {: past-url:n :}
   url STR:LEN 0<> scope STR:LEN 0<> and
   if SEPARATOR row past-url PUT else past-url then
   {: past-sep:n :}
   SCOPE-NAME scope row past-sep PARAM ;

: CHALLENGES-LEN ( n n -- n )
   {: uu:n su:n :}
   uu 0= su 0= and if 0 exit then
   METADATA-NAME STR:LEN uu PARAM-LEN
   SCOPE-NAME STR:LEN su PARAM-LEN +
   uu 0<> su 0<> and if SEPARATOR STR:LEN + then {: params:n :}
   NAMED-PREFIX STR:LEN INVALID-PREFIX STR:LEN + params 2 * + ;

\ RFC 9728 3.1: the segment METADATA!'s URL holds between its authority and
\ the resource's path.
: WELL-KNOWN ( -- str ) s" /.well-known/oauth-protected-resource" ;

: RESOURCE-LEN ( n -- n ) dup 0= if exit then WELL-KNOWN STR:LEN - ;

\ The resource the URL names, its bytes but the well-known segment at end.
: RESOURCE+ ( str n SPAN:span<u8> n -- n )
   {: url:str end:n row off:n :}
   url STR:LEN 0= if off exit then
   url STR:DATA end >str row off PUT {: past:n :}
   end WELL-KNOWN STR:LEN + {: from:n :}
   url STR:DATA from + url STR:LEN from - >str row past PUT ;

\ What a stated scope lacks to ask for a refresh token too.
: OFFLINE ( str -- str )
   {: scope:str :}
   scope STR:LEN 0= scope s" offline_access" 0 WORD-FROM? or if s" " exit then
   s"  offline_access" ;

\ METADATA!'s statement as NAMED holds it: the allocation, NAMED-U and
\ CHALLENGES-U; the well-known segment starts at end.
: COMPOSED ( str str n -- SPAN:span<u8> n n )
   {: url:str scope:str end:n :}
   scope OFFLINE {: more:str :}
   scope STR:LEN more STR:LEN + {: su:n :}
   url STR:LEN su CHALLENGES-LEN {: cu:n :}
   cu url STR:LEN RESOURCE-LEN + {: at:n :}
   at su + {: total:n :}
   total 0= if s" " STR:DATA 0 SPAN:MAKE 0 0 exit then
   total MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN {: row :}
   more row scope row at PUT PUT drop
   row at SPAN:SKIP SPAN:$ {: asked:str :}
   NAMED-PREFIX url asked row 0 NAMING {: split:n :}
   INVALID-PREFIX url asked row split NAMING drop
   url end row cu RESOURCE+ drop
   row split cu ;

\ HTTP writes a header value as it is: a quote or backslash would end the
\ quoted resource_metadata early, and a CR or LF would start a line of its own.
\ A brace, no URI byte (RFC 3986 2), would make a named segment of a route.
: QUOTABLE ( str -- )
   {: text:str :}
   text STR:LEN 0 ?do
      text STR:DATA i + c@ {: c :}
      c $22 = c $5C = or c $7B = or c $7D = or c $20 < or c $7E > or if E-OPERAND throw then
   loop ;


\ RFC 6749 3.3: tokens of NQCHAR (%x21, %x23-5B, %x5D-7E), each one space
\ from the next. An empty scope names none.
: SCOPE-TOKENS ( str -- )
   {: text:str :}
   text STR:LEN 0= if exit then
   text STR:DATA c@ $20 =
   text STR:DATA text STR:LEN 1- + c@ $20 = or if E-OPERAND throw then
   text STR:LEN 0 ?do
      text STR:DATA i + c@ {: c :}
      c $20 = if
         text STR:DATA i 1+ + c@ $20 = if E-OPERAND throw then
      else
         c $21 < c $7E > or c $22 = or c $5C = or if E-OPERAND throw then
      then
   loop ;

: ANSWERING? ( -- bool )
   PASSED @ 0= if false exit then
   FROM @ HTTP:>REQUEST HTTP:SERVING? ;

\ E-OPERAND for an http URL whose host is no loopback IP literal; URI:ORIGIN's
\ rejection for one it cannot read.
: SECURE ( str -- )
   SECURE? 0= if E-OPERAND throw then ;

\ The resource's path: nothing, or `/` and no query or fragment.
: PATH? ( str -- bool )
   {: p:str :}
   p STR:LEN 0= if true exit then
   p STR:DATA c@ [char] / <> if false exit then
   p s" ?" STR:CONTAINS? 0= p s" #" STR:CONTAINS? 0= and ;

\ Where the well-known segment starts in METADATA!'s URL, 0 for an empty
\ URL; SECURE's rejection, or E-OPERAND for a URL not of RFC 9728 3.1's form.
: WELL-KNOWN-AT ( str -- n )
   {: url:str :}
   url STR:LEN 0= if 0 exit then
   url SECURE
   url URI:REQUEST-TARGET$ STR:DATA url STR:DATA - {: end:n :}
   url STR:DATA end + url STR:LEN end - >str {: rest:str :}
   rest WELL-KNOWN STR:STARTS-WITH? 0= if E-OPERAND throw then
   rest STR:DATA WELL-KNOWN STR:LEN +
   rest STR:LEN WELL-KNOWN STR:LEN - >str
   PATH? 0= if E-OPERAND throw then
   end ;

: SCOPES+ ( ^JSON:writer str n -- ^JSON:writer )
   {: s:str from:n :}
   from begin {: at:n :}
      s at SPACE-AT {: to:n :}
      s STR:DATA at + to at - >str JSON:WRITE-STRING
      to s STR:LEN <
   while JSON:WRITE-COMMA to 1+ repeat ;

: RESOURCE-BODY ( ^JSON:writer -- ^JSON:writer )
   JSON:WRITE-OBJECT-START
   s" resource" RESOURCE$ JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" authorization_servers" JSON:WRITE-KEY JSON:WRITE-ARRAY-START
   ISSUER$ JSON:WRITE-STRING JSON:WRITE-ARRAY-END
   SCOPE$ {: s:str :}
   s STR:LEN 0 > if
      JSON:WRITE-COMMA s" scopes_supported" JSON:WRITE-KEY
      JSON:WRITE-ARRAY-START s 0 SCOPES+ JSON:WRITE-ARRAY-END
   then
   JSON:WRITE-OBJECT-END ;

public

\ The issuer whose tokens ADMIT? accepts: its iss, the audience this server
\ answers to, and its JWK Set's URL. The texts are borrowed and outlive the
\ server, as literals do. A built program keeps none: it states them at run
\ time. E-HTTP-STATE while HTTP serves; E-OPERAND for an iss or key set URL
\ neither https nor http to a loopback IP literal, URI:ORIGIN's rejection for
\ one it cannot read, either leaving the previous issuer in force.
: ISSUER! ( str str str -- )
   {: iss:str aud:str url:str :}
   HTTP:RUNNING? if E-HTTP-STATE throw then
   iss SECURE url SECURE
   iss STR:DATA ISSUER-AT ! iss STR:LEN ISSUER-LEN !
   aud STR:DATA AUDIENCE-AT ! aud STR:LEN AUDIENCE-LEN !
   url STR:DATA KEYS-URL-AT ! url STR:LEN KEYS-URL-LEN !
   0 KEYS-U ! 0 KEYS-AT !
   WAKE TASK:SEMAPHORE-DESTROY 0 WAKE TASK:SEMAPHORE-INIT ;


\ The URL of this server's protected-resource metadata and the scope of access
\ a token needs here (RFC 6750 3), which each 401 challenge then names as
\ resource_metadata and scope, and RESOURCE-METADATA answers, the scope with
\ offline_access beside it; an empty text names none. The texts are copied, so
\ they need not outlive the call. A built
\ program keeps neither: it challenges plainly until it states METADATA! at
\ run time. E-OPERAND for a URL neither https nor http to a loopback IP
\ literal, or not of the form
\ `<scheme>://<authority>/.well-known/oauth-protected-resource[<path>]`
\ (RFC 9728 3.1), or holding a quote, a backslash, a brace, or a byte below $20
\ or above $7E unencoded, or for a scope that is not space-separated RFC 6749
\ 3.3 scope tokens; URI:ORIGIN's rejection for a URL it cannot read. A rejected
\ text or failed allocation leaves the previous statement in force.
\ E-HTTP-STATE while HTTP serves.
: METADATA! ( str str -- )
   {: url:str scope:str :}
   HTTP:RUNNING? if E-HTTP-STATE throw then
   url QUOTABLE
   url WELL-KNOWN-AT {: end:n :}
   scope SCOPE-TOKENS
   url scope end COMPOSED {: row split:n cu:n :}
   NAMED-SPAN {: was :}
   row SPAN:$ dup STR:LEN NAMED-LEN ! STR:DATA NAMED !
   split NAMED-U ! cu CHALLENGES-U !
   url STR:LEN RESOURCE-LEN RESOURCE-U !
   was MEM:FREE-SPAN ;


\ The route handler a host gives HTTP:ROUTE for the GET of METADATA!'s URL
\ path: 200 with the protected-resource metadata (RFC 9728 2), naming the
\ resource that URL stands for, the issuer as its one authorization server
\ and the challenges' scope tokens as scopes_supported, absent for an empty
\ scope. E-HTTP-STATE before ISSUER! or with no METADATA! URL stated.
: RESOURCE-METADATA ( HTTP:request HTTP:response -- )
   nip {: answer:HTTP:response :}
   ISSUER$ STR:LEN 0= RESOURCE-U @ 0= or if E-HTTP-STATE throw then
   answer 200 HTTP:STATUS!
   answer [: RESOURCE-BODY ;] HTTP:JSON! ;

private

: ROUTE-PATH-READY ( -- )
   ROUTE-PATH @ NULL-PTR <> if exit then
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW ROUTE-PATH ! ;

\ METADATA!'s URL path: RESOURCE$ is that URL less the well-known segment.
: METADATA-PATH ( -- str )
   ROUTE-PATH-READY
   ROUTE-PATH @ {: path:^ :}
   path 0 VEC:RESIZE
   WELL-KNOWN path VEC:APPEND
   RESOURCE$ URI:REQUEST-TARGET$ path VEC:APPEND
   path VEC:BYTES ;

: METADATA-ROUTE ( str -- )
   {: path:str :}
   s" GET" path [: RESOURCE-METADATA ;] HTTP:ROUTE ;

public

\ Adds RESOURCE-METADATA as the GET route of METADATA!'s URL path and, where
\ that path goes on past the well-known segment, of that segment alone too. A
\ second call replaces the text the first one's routes read, so
\ HTTP:ROUTES-RESET comes between. METADATA! comes before it: one stated after
\ leaves the routes at the old path. A built program keeps no route: it adds
\ them at run time. E-HTTP-STATE with no METADATA! URL stated or while HTTP
\ serves.
: METADATA-ROUTES ( -- )
   HTTP:RUNNING? RESOURCE-U @ 0= or if E-HTTP-STATE throw then
   METADATA-PATH {: path:str :}
   path METADATA-ROUTE
   path WELL-KNOWN STR:= if exit then
   WELL-KNOWN METADATA-ROUTE ;


\ Whether the request carries a current bearer token of the issuer's for this
\ server. A rejection answers the response itself, with an empty body: 401
\ challenging Bearer for no, several or a non-Bearer Authorization, or Bearer
\ error="invalid_token" for a token that fails, either one naming METADATA!'s
\ URL and scope once stated; 503 when there is no key set and none
\ could be fetched. A broken key of the issuer's passes its throw.
\ E-HTTP-STATE before ISSUER!.
: ADMIT? ( HTTP:request HTTP:response -- bool )
   {: asked:HTTP:request answer:HTTP:response :}
   ISSUER$ STR:LEN 0= if E-HTTP-STATE throw then
   0 PASSED !
   asked OUTCOME {: outcome:n :}
   outcome ADMITTED <> if answer outcome REJECT false exit then
   asked HTTP:REQUEST>N FROM !
   true bool>n PASSED !
   true ;


\ A string claim of the admitted request this worker answers, decoded; false
\ outside one, or for a claim that is absent or not a string. The answer is
\ borrowed until the worker's next CLAIM$, ADMIT? or CALLER-KEY.
: CLAIM$ ( str -- str bool )
   {: name:str :}
   ANSWERING? 0= if s" " false exit then
   CLAIMS@ name NOTE$ ;

\ Whether a claim of the admitted request this worker answers holds the
\ name: a string claim as one of its space-separated words (RFC 6749 3.3
\ scope), as Entra's scp; an array claim as an element, as Entra's roles;
\ false outside one.
: CLAIM-HOLDS? ( str str -- bool )
   {: name:str w:str :}
   ANSWERING? 0= if false exit then
   name w HELD? ;

private

: NONEMPTY? ( str -- bool )
   {: name:str :}
   CLAIMS@ name JSON:FIND MATCH option
      none OF false ENDOF
      some OF dup JSON:STRING? swap s" " JSON:STRING= 0= and ENDOF
   ;MATCH ;

\ An id a caller's key can hold: not empty, no space, no control byte.
: ID? ( str -- bool )
   {: id:str :}
   id STR:LEN 0= if false exit then
   id STR:LEN 0 ?do
      id STR:DATA i + c@ {: c :}
      c $21 < c $7E > or if unloop false exit then
   loop true ;

: ID-CLAIM? ( str -- bool ) WORDS$ if ID? else drop false then ;

\ A stated scope as Entra writes it in scp: `access_as_user` for
\ `api://<id>/access_as_user`.
: SCP-NAME ( str -- str )
   {: s:str :}
   s STR:LEN begin
      dup 0 > if s STR:DATA over 1- + c@ [char] / <> else false then
   while 1- repeat {: at:n :}
   s STR:DATA at + s STR:LEN at - >str ;

\ offline_access asks for a refresh token; Entra never writes it in scp.
: SCOPE-HELD? ( str -- bool )
   {: scope:str :}
   scope s" offline_access" STR:= if true exit then
   s" scp" scope SCP-NAME HELD? ;

: SCOPES-HELD? ( -- bool )
   SCOPE$ {: s:str :}
   0 begin {: at:n :} at s STR:LEN < while
      s at SPACE-AT {: to:n :}
      s STR:DATA at + to at - >str SCOPE-HELD? 0= if false exit then
      to 1+
   repeat true ;

\ The token's client id: azp, or with no azp a v1 token's appid.
: APP-ID ( -- str )
   CLAIMS@ s" azp" HAS? if s" azp" else s" appid" then {: name:str :}
   CLAIMS@ name NOTE$ drop ;

\ Whether the admitted request's token is a listed app's own: a delegated
\ token names the app too, but has an scp.
: APP? ( -- bool )
   ANSWERING? 0= if false exit then
   s" tid" ID-CLAIM? 0= if false exit then
   CLAIMS@ s" scp" HAS? if false exit then
   APPS$ APP-ID 0 WORD-FROM? ;

: APPS-READY ( -- )
   APPS @ NULL-PTR <> if exit then
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW APPS ! ;

: CLAIM+ ( str ^VEC:vector<u8> -- )
   {: name:str buf:^ :}
   CLAIMS@ name NOTE$ drop buf VEC:APPEND ;

: TENANT+ ( str ^VEC:vector<u8> -- )
   {: kind:str buf:^ :}
   kind buf VEC:APPEND
   s" tid" buf CLAIM+
   s" :" buf VEC:APPEND ;

public

\ Lists an app id that may call as itself, beside those listed before. The text
\ is copied. A built program keeps none: it states them at run time.
\ E-HTTP-STATE while HTTP serves; E-OPERAND for an id that is empty or holds a
\ byte outside $21..$7E.
: APP! ( str -- )
   {: id:str :}
   HTTP:RUNNING? if E-HTTP-STATE throw then
   id ID? 0= if E-OPERAND throw then
   APPS-READY
   id APPS @ WORD+ ;


\ Whether the admitted request this worker answers carries a person's token: a
\ tid and an oid that are nonempty strings with no byte outside $21..$7E, and a
\ nonempty string scp holding every scope METADATA! states, each by the part
\ after its last `/`, offline_access aside. With no scope stated any nonempty
\ scp will do: an app's own token has an oid and no scp. False outside an
\ admitted request.
: PERSON? ( -- bool )
   ANSWERING? 0= if false exit then
   s" tid" ID-CLAIM? s" oid" ID-CLAIM? and
   s" scp" NONEMPTY? and 0= if false exit then
   SCOPES-HELD? ;


\ Whether ADMIT? admits the request and its token is a person's (PERSON?) or a
\ listed app's: such a tid, no scp claim, and an azp, or where the token has no
\ azp an appid, that APP! lists. A rejection answers the response itself:
\ ADMIT?'s, or 403 caller_rejected, after which CLAIM$ and CLAIM-HOLDS? answer
\ false.
: CALLER? ( HTTP:request HTTP:response -- bool )
   {: asked:HTTP:request answer:HTTP:response :}
   asked answer ADMIT? 0= if false exit then
   PERSON? if true exit then
   APP? if true exit then
   0 PASSED !
   answer 403 s" caller_rejected"
   s" the token is neither a person's with this server's scope nor a listed app's"
   HTTP:ERROR! false ;


\ Appends who calls: `entra:<tid>:<oid>` for a person, `app:<tid>:<app id>` for
\ a listed app. E-HTTP-STATE when this worker answers no admitted request of
\ either.
: CALLER-KEY ( ^VEC:vector<u8> -- )
   {: buf:^ :}
   PERSON? if
      s" entra:" buf TENANT+  s" oid" buf CLAIM+ exit
   then
   APP? 0= if E-HTTP-STATE throw then
   s" app:" buf TENANT+  APP-ID buf VEC:APPEND ;


private

10000 constant TOKEN-TIMEOUT-MS     \ protective: the bound on the token endpoint's whole answer
$10000 constant TOKEN-CAP           \ protective: the largest token answer read

TYPED-VARIABLE ENDPOINT SPAN:span<u8> SCRATCH-ONLY
TYPED-VARIABLE CLIENT-ID SPAN:span<u8> SCRATCH-ONLY
TYPED-VARIABLE SECRET SPAN:span<u8> SCRATCH-ONLY

private

ENUM kept-state empty kept failed ;ENUM

\ A kept token: CURRENT's for a scope, or, when identity is set,
\ IDENTITY-TOKEN's for the resource in scope from the IMDS at origin. state
\ says whether one is kept or the last ask failed; it is asked again once
\ life seconds have passed since the epoch second before the kept token's
\ ask; asks counts the asks ended, and region holds the row's lock.
STRUCTURE row DERIVE addr
   FIELD next ^row
   FIELD identity bool
   FIELD state kept-state
   FIELD since n
   FIELD life n
   FIELD code n
   FIELD asks n
   FIELD region SPAN:span<u8>
   FIELD origin ^VEC:vector<u8>
   FIELD scope ^VEC:vector<u8>
   FIELD token ^VEC:vector<u8>
   FIELD answer ^VEC:vector<u8>
;STRUCTURE

TYPED-VARIABLE NEWEST ^row SCRATCH-ONLY

\ Held to find, link or walk the rows, never across an ask.
TASK:FACILITY ROWS-LOCK
ROWS-LOCK TASK:FACILITY-INIT

: GUARD ( SPAN:span<u8> -- ^n ) SPAN:$ STR:DATA CELL-VIEW ;

: LISTED ( R [ R -- S ] -- S )
   ROWS-LOCK TASK:GET [: ROWS-LOCK TASK:RELEASE ;] finally ;

\ Empties CURRENT's rows. The walk holds ROWS-LOCK: IDENTITY-TOKEN may link
\ a row while CLIENT! runs.
: DROP-KEPT ( -- )
   [: NEWEST @
      begin dup NULL-PTR <> while
         dup ROW-IDENTITY @ 0= if
            dup ROW-TOKEN @ 0 VEC:RESIZE
            construct kept-state empty over ROW-STATE !
         then
         ROW-NEXT @
      repeat drop ;] LISTED ;

public

\ How TOKEN asks: as the client itself (client credentials), or for the person
\ whose token is the assertion (Entra On-Behalf-Of).
ENUM grant
   VARIANT own ;VARIANT
   VARIANT for-person FIELD assertion ^u8 FIELD len n ;VARIANT
;ENUM

\ The token endpoint's URL, and the client id and secret this process presents
\ there, written before any TOKEN, REFRESH or CURRENT, not while one runs; each
\ kept token is dropped. An empty secret states a public client, which
\ presents none (RFC 6749 2.3.1). The texts are borrowed and outlive every
\ TOKEN and REFRESH, as literals do. A built program keeps none: it states them
\ at run time. E-OPERAND for a URL neither https nor http to a loopback IP
\ literal, URI:ORIGIN's rejection for one it cannot read, either leaving the
\ previous client in force.
: CLIENT! ( str str str -- )
   {: url:str id:str secret:str :}
   url SECURE
   url STR:DATA url STR:LEN SPAN:MAKE ENDPOINT !
   id STR:DATA id STR:LEN SPAN:MAKE CLIENT-ID !
   secret STR:DATA secret STR:LEN SPAN:MAKE SECRET !
   DROP-KEPT ;

private

\ The value must not lie in the vector: each append before it is read may move
\ the block.
: PAIR+ ( str str ^VEC:vector<u8> -- )
   {: k:str v:str buf:^ :}
   buf VEC:COUNT 0 > if [char] & buf VEC:PUSH c! then
   k buf VEC:APPEND
   [char] = buf VEC:PUSH c!
   v URI:FORM-LEN {: n:n :}
   v buf n VEC:EXTEND URI:FORM-ENCODE drop ;

: CREDENTIALS+ ( ^VEC:vector<u8> -- )
   {: buf:^ :}
   s" client_id" CLIENT-ID @ SPAN:$ buf PAIR+
   SECRET @ SPAN:LEN 0= if exit then
   s" client_secret" SECRET @ SPAN:$ buf PAIR+ ;

\ What the vector held is wiped, and it has room for twice TOKEN-CAP, an answer
\ and ISSUED's decode room: no secret or answer sits in storage a grow releases.
: EMPTIED ( ^VEC:vector<u8> -- )
   {: buf:^ :}
   buf 0 VEC:RESIZE  buf TOKEN-CAP 2 * VEC:RESERVE ;

: FORM ( str grant ^VEC:vector<u8> -- )
   {: scope:str g buf:^ :}
   g MATCH grant
      own OF
         buf EMPTIED
         s" grant_type" s" client_credentials" buf PAIR+
         buf CREDENTIALS+
         s" scope" scope buf PAIR+ ENDOF
      for-person OF {: a:^ au:n :}
         buf EMPTIED
         s" grant_type" s" urn:ietf:params:oauth:grant-type:jwt-bearer" buf PAIR+
         buf CREDENTIALS+
         s" assertion" a au >str buf PAIR+
         s" scope" scope buf PAIR+
         s" requested_token_use" s" on_behalf_of" buf PAIR+ ENDOF
   ;MATCH ;

: REFRESH-FORM ( str str ^VEC:vector<u8> -- )
   {: scope:str r:str buf:^ :}
   buf EMPTIED
   s" grant_type" s" refresh_token" buf PAIR+
   buf CREDENTIALS+
   s" refresh_token" r buf PAIR+
   s" scope" scope buf PAIR+ ;

: OPEN-LINE ( -- )  LOG:ERROR s" bearer" LOG:LINE ;

: CURL-FAULT ( CURL:code -- )
   {: rc:CURL:code :}
   OPEN-LINE
   s" token request failed: curl " LOG:TEXT+  rc CURL:CODE>N LOG:INT+
   s" : " LOG:TEXT+  rc CURL:ERROR$ LOG:TEXT+
   LOG:;LINE ;

: SET? ( CURL:status -- bool )
   MATCH CURL:status
      ok OF true ENDOF
      failed OF CURL-FAULT false ENDOF
   ;MATCH ;

\ The vector's form set as the request's body, then wiped from the vector
\ since the secret is in it; whether every setting took, logging the first
\ that did not.
: REQUESTED? ( CURL:handle ^VEC:vector<u8> -- bool )
   {: h:CURL:handle buf:^ :}
   h buf VEC:BYTES CURL:BODY!
   buf 0 VEC:RESIZE
   SET? 0= if false exit then
   h ENDPOINT @ SPAN:$ CURL:URL! SET? 0= if false exit then
   h s" Content-Type: application/x-www-form-urlencoded" CURL:HEADER+ SET?
   0= if false exit then
   h s" Accept: application/json" CURL:HEADER+ SET? 0= if false exit then
   h TOKEN-TIMEOUT-MS >MS CURL:TIMEOUT! SET? ;

: STATUS-ERROR ( CURL:http-status -- n )
   {: status:CURL:http-status :}
   status CURL:UNAVAILABLE? if E-UNAVAILABLE exit then
   status CURL:HTTP-STATUS>N {: code:n :}
   code 400 >= code 500 < and if E-REJECTED exit then
   E-RESPONSE ;

\ The vector is empty with EMPTIED's room: the answer is appended to it, at
\ most TOKEN-CAP bytes, so the block never moves.
: RECEIVED ( CURL:handle ^VEC:vector<u8> [ CURL:http-status -- n ]
   -- option<CURL:http-status> )
   {: h:CURL:handle buf:^ map :}
   h TOKEN-CAP >LEN buf CURL:PERFORM
   MATCH CURL:fetch-result
      response OF drop OPTION:SOME ENDOF
      truncated OF
         drop {: status:CURL:http-status :}
         buf 0 VEC:RESIZE
         OPEN-LINE
         s" token answer " LOG:TEXT+  status CURL:HTTP-STATUS>N LOG:INT+
         s"  past " LOG:TEXT+  TOKEN-CAP LOG:INT+  s"  bytes" LOG:TEXT+
         LOG:;LINE
         status map execute throw ENDOF
      failed OF buf 0 VEC:RESIZE CURL-FAULT OPTION:NONE ENDOF
   ;MATCH ;

: EXCHANGE ( ^VEC:vector<u8> [ CURL:handle -- bool ]
   [ CURL:http-status -- n ] -- option<CURL:http-status> )
   {: buf:^ set map :}
   CURL:INIT
   MATCH CURL:init-result
      ready OF {: h:CURL:handle :}
         [: h set execute
            if h buf map RECEIVED else OPTION:NONE then ;] catch
         {: got code:n :}
         h CURL:CLEANUP
         code 0<> if buf 0 VEC:RESIZE code throw then
         got ENDOF
      failed OF CURL-FAULT buf 0 VEC:RESIZE OPTION:NONE ENDOF
   ;MATCH ;

\ The access_token string of a token answer decoded into room, its length;
\ -1 when the answer has no access_token string.
: ACCESS-TOKEN ( JSON:value SPAN:span<u8> -- n )
   {: answer room :}
   answer s" access_token" JSON:FIND MATCH option
      none OF -1 ENDOF
      some OF room INTO-ROOM ENDOF
   ;MATCH ;

: SECONDS ( JSON:value -- n )
   {: v :}
   v JSON:STRING? if
      v [: STR:PARSE-POS MATCH option
         none OF -1 ENDOF
         some OF ENDOF
      ;MATCH ;] JSON:WITH-STRING exit
   then
   v INT-OF 0= if drop -1 then ;

\ The positive expires_in of a token answer, a JSON integer or a digit
\ string (IMDS and Entra's v1 endpoint send one), -1 when it lacks one.
: LIFETIME ( JSON:value -- n )
   s" expires_in" JSON:FIND MATCH option
      none OF -1 ENDOF
      some OF SECONDS ENDOF
   ;MATCH {: secs:n :}
   secs 0 > if secs else -1 then ;

\ The refresh_token string of a token answer decoded into room, its length;
\ 0 when the answer lacks it, -1 when it is no string.
: RENEWAL ( JSON:value SPAN:span<u8> -- n )
   {: answer room :}
   answer s" refresh_token" JSON:FIND MATCH option
      none OF 0 ENDOF
      some OF room INTO-ROOM ENDOF
   ;MATCH ;

\ ACCESS-TOKEN's, LIFETIME's and RENEWAL's answers in place of the last three
\ cells, the texts and flag kept for catch. RENEWAL decodes into the room past
\ the access token, and only when renewing after an access token: else 0.
: KEPT ( SPAN:span<u8> SPAN:span<u8> bool n n n -- SPAN:span<u8> SPAN:span<u8> bool n n n )
   drop 2drop {: text room renew:bool :}
   text room renew
   text SPAN:$ JSON:PARSE {: answer :}
   answer JSON:OBJECT? 0= if -1 -1 -1 exit then
   answer room ACCESS-TOKEN {: got:n :}
   got answer LIFETIME
   renew got 0 >= and if answer room got SPAN:SKIP RENEWAL else 0 then ;

\ A 2xx answer that cannot be used, logged without the answer, which can
\ hold a token.
: UNUSABLE ( n str -- )
   {: status:n what:str :}
   OPEN-LINE s" token answer " LOG:TEXT+  status LOG:INT+
   s"  " LOG:TEXT+  what LOG:TEXT+  LOG:;LINE
   E-RESPONSE throw ;

: PARSED ( n SPAN:span<u8> SPAN:span<u8> bool -- n n n )
   {: status:n text room renew:bool :}
   text room renew -1 -1 -1 [: [: KEPT ;] REASON:HIDDEN ;] catch {: code:n :}
   code 0= if {: text room renew:bool got:n secs:n kept:n :} got secs kept exit then
   2drop 2drop 2drop
   code MALFORMED? if
      status s" is not JSON naming each member once" UNUSABLE
   then
   code throw ;

\ The 2xx answer's access_token and, when renewing, its refresh_token, empty
\ when absent, decoded into the vector past the answer's bytes, the refresh
\ token right after the access token, and its expires_in. They are disjoint
\ parts of the answer and a decoded string never outgrows its text, so both
\ fit.
: ISSUED ( n ^VEC:vector<u8> bool -- str str n )
   {: status:n buf:^ renew:bool :}
   buf VEC:COUNT {: u:n :}
   buf u VEC:EXTEND {: room :}
   status buf VEC:SPAN u SPAN:TAKE room renew PARSED
   {: got:n secs:n kept:n :}
   got 0< if status s" lacks an access_token string" UNUSABLE then
   kept 0< if status s" lacks a refresh_token string" UNUSABLE then
   secs 0< if
      status s" lacks a positive integer expires_in" UNUSABLE
   then
   room SPAN:$ STR:DATA {: at:^ :}
   at got >str at got + kept >str secs ;

: ANSWERED ( n ^VEC:vector<u8> -- )
   {: status:n buf:^ :}
   OPEN-LINE s" token request answered " LOG:TEXT+  status LOG:INT+
   s" : " LOG:TEXT+  buf VEC:BYTES LOG:TEXT+  LOG:;LINE ;

: STATED ( -- ) ENDPOINT @ SPAN:LEN 0= if E-HTTP-STATE throw then ;

: ASKED ( ^VEC:vector<u8> [ CURL:handle -- bool ]
   [ CURL:http-status -- n ] -- n )
   {: buf:^ set map :}
   buf set map EXCHANGE MATCH option
      none OF E-TRANSFER throw ENDOF
      some OF ENDOF
   ;MATCH {: status:CURL:http-status :}
   status CURL:HTTP-STATUS>N {: code:n :}
   code 200 >= code 300 < and if code exit then
   code buf ANSWERED
   status map execute throw ;

\ The IMDS request for the resource set on the handle, its URL built in the
\ vector, then emptied to EMPTIED's room for RECEIVED; whether every setting
\ took, logging the first that did not. IMDS must bypass proxies.
: IMDS-SET? ( CURL:handle str str ^VEC:vector<u8> -- bool )
   {: h:CURL:handle o:str res:str buf:^ :}
   buf EMPTIED  o buf VEC:APPEND
   s" /metadata/identity/oauth2/token?api-version=2018-02-01&resource="
   buf VEC:APPEND  res buf URI:QUERY-ENCODE
   h buf VEC:BYTES CURL:URL!  buf 0 VEC:RESIZE
   SET? 0= if false exit then
   h s" Metadata: true" CURL:HEADER+ SET? 0= if false exit then
   h s" " CURL:PROXY! SET? 0= if false exit then
   h TOKEN-TIMEOUT-MS >MS CURL:TIMEOUT! SET? ;

\ 404 and 410 are E-UNAVAILABLE: Azure answers them while IMDS updates.
: IMDS-ERROR ( CURL:http-status -- n )
   {: status:CURL:http-status :}
   status CURL:HTTP-STATUS>N {: code:n :}
   code 404 = code 410 = or if E-UNAVAILABLE exit then
   status STATUS-ERROR ;

\ The resource's token from the IMDS at the origin, borrowed from the vector
\ as TOKEN's, then its expires_in.
: IMDS-TOKEN ( str str ^VEC:vector<u8> -- str n )
   {: o:str res:str buf:^ :}
   buf [: o res buf IMDS-SET? ;] [: IMDS-ERROR ;] ASKED
   buf false ISSUED
   {: a:str r:str secs:n :}
   a secs ;

public

\ An access token for the scope (for Entra a resource's `api://.../.default`)
\ from the endpoint CLIENT! names, borrowed from the vector until the caller
\ writes to it again, then the answer's expires_in: the token's lifetime in
\ seconds, positive. E-TRANSFER for no answer; E-UNAVAILABLE for a status
\ CURL:UNAVAILABLE? accepts; E-REJECTED for another 4xx, the vector then
\ holding the issuer's answer, empty past TOKEN-CAP; E-RESPONSE for a 2xx
\ that is not JSON naming each member once or lacks an access_token string or
\ a positive integer expires_in, a number or a digit string, or any other
\ status. Each of those failures first logs one line tagged bearer, never a
\ 2xx answer's text. A scope or assertion must not lie in the vector.
\ E-HTTP-STATE before CLIENT!.
: TOKEN ( str grant ^VEC:vector<u8> -- str n )
   {: scope:str g buf:^ :}
   STATED
   scope g buf FORM
   buf [: buf REQUESTED? ;] [: STATUS-ERROR ;] ASKED
   buf false ISSUED {: a:str r:str secs:n :}
   a secs ;


\ A new access token for the scope from the endpoint CLIENT! names, by the
\ refresh grant with the refresh token (RFC 6749 6), then the answer's refresh
\ token, empty when it has none (the old one then stays the one to present),
\ then expires_in. Both texts are borrowed from the vector until the caller
\ writes to it again. It fails as TOKEN does, and E-RESPONSE for a 2xx whose
\ refresh_token is not a string. A scope or refresh token must not lie in the
\ vector.
: REFRESH ( str str ^VEC:vector<u8> -- str str n )
   {: scope:str r:str buf:^ :}
   STATED
   scope r buf REFRESH-FORM
   buf [: buf REQUESTED? ;] [: STATUS-ERROR ;] ASKED
   buf true ISSUED ;

private

: ROW-ORIGIN$ ( ^row -- str ) ROW-ORIGIN @ VEC:BYTES ;
: ROW-SCOPE$ ( ^row -- str ) ROW-SCOPE @ VEC:BYTES ;
: ROW-KEPT$ ( ^row -- str ) ROW-TOKEN @ VEC:BYTES ;

\ Under ROWS-LOCK: the row of the kind, origin and scope, else null.
: FOUND ( bool str str -- ^row )
   {: id:bool o:str scope:str :}
   NEWEST @
   begin dup NULL-PTR <> while
      dup ROW-IDENTITY @ bool>n id bool>n =
      over ROW-ORIGIN$ o STR:= and
      over ROW-SCOPE$ scope STR:= and if exit then
      ROW-NEXT @
   repeat ;

: EMPTY ( -- ^VEC:vector<u8> ) HEAP:ALLOCATOR 0 [type] u8 VEC:NEW ;

\ The row's lock made ready in its zeroed region and its vectors made, the
\ origin's and scope's holding them; a throw leaves what was made to UNMADE.
: MADE ( bool str str ^row -- )
   {: id:bool o:str scope:str r:^ :}
   id r ROW-IDENTITY !
   TASK:FACILITY-BYTES MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN r ROW-REGION !
   0 r ROW-REGION @ SPAN:FILL
   r ROW-REGION @ GUARD TASK:FACILITY-INIT
   EMPTY r ROW-ORIGIN !  EMPTY r ROW-SCOPE !
   EMPTY r ROW-TOKEN !  EMPTY r ROW-ANSWER !
   o r ROW-ORIGIN @ VEC:APPEND
   scope r ROW-SCOPE @ VEC:APPEND ;

\ Frees what MADE made of an unlinked row, and the row.
: UNMADE ( ^row -- )
   {: r:^ :}
   r ROW-ORIGIN @ VEC:DELETE  r ROW-SCOPE @ VEC:DELETE
   r ROW-TOKEN @ VEC:DELETE
   r ROW-ANSWER @ VEC:DELETE  r ROW-REGION @ MEM:FREE-SPAN
   HEAP:ALLOCATOR r ALLOC:DELETE ;

\ Under ROWS-LOCK: a new row for the kind, origin and scope, linked newest
\ first and never freed; a throw leaves nothing allocated.
: LINKED ( bool str str -- ^row )
   {: id:bool o:str scope:str :}
   HEAP:ALLOCATOR [type] row ALLOC:NEW {: r:^ :}
   [: id o scope r MADE ;] catch {: code:n :}
   code 0<> if r UNMADE code throw then
   NEWEST @ r ROW-NEXT !
   r NEWEST !  r ;

\ Under ROWS-LOCK.
: ROW-FOR ( bool str str -- ^row )
   {: id:bool o:str scope:str :}
   id o scope FOUND dup NULL-PTR = if
      drop id o scope LINKED
   then ;

: FAILED? ( ^row -- bool )
   ROW-STATE @ MATCH kept-state
      empty OF false ENDOF
      kept OF false ENDOF
      failed OF true ENDOF
   ;MATCH ;

\ Under the row's lock: whether its token is to be asked for.
: STALE? ( ^row -- bool )
   {: r:^ :}
   r ROW-STATE @ MATCH kept-state
      empty OF true ENDOF
      kept OF epoch-seconds r ROW-SINCE @ - r ROW-LIFE @ >= ENDOF
      failed OF true ENDOF
   ;MATCH ;

\ Under the row's lock: a failed ask that ended after HELD read the asks
\ count throws its code to the task that waited on it.
: WAITED ( ^row n -- )
   {: r:^ seen:n :}
   r ROW-ASKS @ seen > 0= if exit then
   r FAILED? if r ROW-CODE @ throw then ;

\ The row's token and expires_in: the IMDS's for an identity row, else
\ TOKEN's with own.
: ISSUED-FOR ( ^row -- str n )
   {: r:^ :}
   r ROW-IDENTITY @ if
      r ROW-ORIGIN$ r ROW-SCOPE$ r ROW-ANSWER @ IMDS-TOKEN
   else
      r ROW-SCOPE$ BEARER-GRANT:OWN r ROW-ANSWER @ TOKEN
   then ;

\ Under the row's lock: the answer's token in place of the old one, kept
\ for its expires_in less LEEWAY-S, counted from before the ask.
: KEEP ( ^row -- )
   {: r:^ :}
   epoch-seconds {: asked:n :}
   r ISSUED-FOR {: a:str secs:n :}
   r ROW-TOKEN @ {: kept:^ :}
   kept 0 VEC:RESIZE  a kept VEC:APPEND
   asked r ROW-SINCE !  secs LEEWAY-S - r ROW-LIFE !
   construct kept-state kept r ROW-STATE ! ;

\ Under the row's lock: one more ask ended, failed with the code unless it is
\ 0; asks is stored atomic! for HELD's read outside the lock.
: ENDED ( ^row n -- )
   {: r:^ code:n :}
   code 0<> if
      code r ROW-CODE !  construct kept-state failed r ROW-STATE !
   then
   r ROW-ASKS @ 1+ r ROW-ASKS atomic! ;

\ Under the row's lock.
: FRESH ( ^row -- )
   {: r:^ :}
   r STALE? 0= if exit then
   [: r KEEP ;] catch {: code:n :}
   r ROW-ANSWER @ 0 VEC:RESIZE
   r code ENDED
   code 0<> if code throw then ;

\ Runs the quotation on the row's token under the row's lock, asked first
\ when stale; a waiter on a failed ask throws its code.
: HELD ( ^row [ str -- ] -- )
   {: r:^ q :}
   r ROW-ASKS atomic@ {: seen:n :}
   r ROW-REGION @ GUARD TASK:GET
   [: r seen WAITED r FRESH r ROW-KEPT$ q execute ;]
   [: r ROW-REGION @ GUARD TASK:RELEASE ;] finally ;

public

\ Runs the quotation on an access token for the scope, asked as the client
\ itself (TOKEN with own) from the endpoint CLIENT! names. A token is kept
\ per scope and asked again once LEEWAY-S or less of its expires_in
\ remains, counted in epoch seconds from before the ask, so one that lasts
\ LEEWAY-S or less is asked for on every call. Callable from any task, many
\ at once: tasks on one scope ask one at a time; one that waited on an ask
\ that failed throws that ask's code, and a call that begins after it asks
\ again. The text is borrowed for the quotation only, under the scope's
\ lock: the quotation copies what it keeps, does no I/O and calls no
\ CURRENT. A failed ask throws TOKEN's code. The replaced token and the
\ endpoint's answer are wiped. E-HTTP-STATE before CLIENT!.
: CURRENT ( str [ str -- ] -- )
   {: scope:str q :}
   STATED
   false s" " scope [: ROW-FOR ;] LISTED q HELD ;

\ Azure's metadata service (IMDS); production passes it to IDENTITY-TOKEN.
: IMDS$ ( -- str ) s" http://169.254.169.254" ;

\ Appends to the vector the managed identity's token for the resource (for
\ Azure PostgreSQL `https://ossrdbms-aad.database.windows.net`) from the
\ IMDS at the origin, kept per origin and resource as CURRENT keeps a
\ scope's: asked again once LEEWAY-S or less of its expires_in remains, one
\ task at a time, a task that waited on a failed ask throwing its code.
\ Callable from any task. E-TRANSFER for no answer; E-UNAVAILABLE for 404,
\ 410 and a status CURL:UNAVAILABLE? accepts; E-REJECTED for another 4xx;
\ E-RESPONSE as TOKEN. Each failed ask first logs one line tagged bearer,
\ never a 2xx answer's text. The caller wipes the vector (`0 VEC:RESIZE`)
\ when done.
: IDENTITY-TOKEN ( str str ^VEC:vector<u8> -- )
   {: o:str res:str dest:^ :}
   true o res [: ROW-FOR ;] LISTED
   [: dest VEC:APPEND ;] HELD ;

;package
