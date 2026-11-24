split-str
\ bearer.f - OAuth bearer tokens (RFC 6750) on Habu's HTTP server. A route
\ admits a request whose Authorization carries a compact JWS that one issuer
\ signed RS256 with a key of its JWK Set, whose iss and aud name that issuer
\ and this server, and whose exp and nbf hold (RFC 7515, 7517, 7519). The host
\ states the issuer with ISSUER! before HTTP:START, guards a route with
\ `req res BEARER:ADMIT? if ... then` and reads the caller's claims with CLAIM$
\ and CLAIM-HOLDS?. METADATA! names this server's protected-resource metadata
\ (RFC 9728 5.1) and the scope its tokens need (RFC 6750 3) in each 401
\ challenge, and RESOURCE-METADATA answers that metadata's GET (RFC 9728 3).
\ As a client, TOKEN gets a token from the token endpoint CLIENT! names, for
\ the client itself (RFC 6749 4.4) or for a person (Entra On-Behalf-Of), and
\ REFRESH renews one with a refresh grant (RFC 6749 6); CURRENT keeps the
\ client's own token per scope. IDENTITY-TOKEN appends a managed identity's
\ token for a resource from Azure's metadata service at an origin (IMDS$),
\ kept per origin and resource. Every other URL BEARER fetches or advertises
\ is https, or http to a loopback IP literal for tests.
\ STORAGE CLASS. PROCESS-WIDE: the issuer, and the challenges, resource and
\ scope METADATA! states, written before HTTP serves; the key set, read and
\ replaced under KEYS-LOCK and fetched over CURL outside it by one worker at a
\ time; the counts of workers waiting for that fetch and of fetches returned,
\ under KEYS-LOCK, and the semaphore that wakes them, which ISSUER! sets up;
\ tables indexed by HTTP:WORKER-SLOT holding each request's token and
\ scratch; the client CLIENT! states, written before any TOKEN or REFRESH;
\ and the rows CURRENT keeps, one per scope, and IDENTITY-TOKEN keeps, one
\ per origin and resource, written under ROWS-LOCK when linked and each under
\ its own lock after; CLIENT! empties CURRENT's.
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
require src/lib/uri.f

package BEARER
public

-9630 s" no answer from the token endpoint" ERROR:CODE E-TRANSFER
-9631 s" the issuer refused with a 4xx a retry will not change" ERROR:CODE E-REFUSED
-9632 s" a URL is not https (state https, or http only to 127.0.0.0/8 or [::1]), a METADATA! URL is not /.well-known/oauth-protected-resource right after its authority, then nothing or a path with no query or fragment (insert the segment before the resource's path; drop the query and fragment), a METADATA! URL holds a quote, backslash, control or non-ASCII byte (percent-encode it), or a scope is not tokens of printable ASCII but quote and backslash one space apart (remove the other bytes and spaces)"
   ERROR:CODE E-OPERAND
-9633 s" the token endpoint answered a status a retry may change" ERROR:CODE E-UNAVAILABLE
-9634 s" the token endpoint answered a reply TOKEN or REFRESH cannot use" ERROR:CODE E-RESPONSE
private
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

: ISSUER ( -- SPAN:span<u8> ) ISSUER-AT @ ISSUER-LEN @ SPAN:MAKE ;
: AUDIENCE ( -- SPAN:span<u8> ) AUDIENCE-AT @ AUDIENCE-LEN @ SPAN:MAKE ;
: KEYS-URL ( -- SPAN:span<u8> ) KEYS-URL-AT @ KEYS-URL-LEN @ SPAN:MAKE ;

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

\ The key set's text, under KEYS-LOCK with KEYS-AT, FETCHING, WAITERS, ROUND,
\ KEY-ARRAY and ARRAY-ROUND; FETCHED and ACCEPTED are the fetcher's alone.
KEYS-CAP BUFFER: KEYS
variable KEYS-U
variable KEYS-AT                    \ when the last fetch returned, a mono-ns; 0 before any
TYPED-VARIABLE FETCHING bool
variable WAITERS                    \ WAKE posts RETURNED owes, one per wait
variable ROUND                      \ fetches returned
TYPED-VARIABLE KEY-ARRAY JSON:value \ KEYS's keys array, as of round ARRAY-ROUND
variable ARRAY-ROUND
TASK:SEMAPHORE WAKE
TYPED-VARIABLE FETCHED ^VEC:vector<u8>
TYPED-VARIABLE ACCEPTED bool        \ FETCHED holds a key set
TASK:FACILITY KEYS-LOCK
KEYS-LOCK TASK:FACILITY-INIT

\ A worker's scratch: the decoded segments, no longer than the token and so
\ than the head; a note holding the kid, then CLAIM$'s answer; the string
\ claim CLAIM-HOLDS? splits, apart from the note CLAIM$ lends; the key.
0 constant SEGMENTS-OFF
HTTP:HEAD-CAP constant NOTE-OFF
HTTP:HEAD-CAP 2 * constant WORDS-OFF
HTTP:HEAD-CAP 3 * constant MODULUS-OFF
MODULUS-OFF CRYPTO:RS256-MAX-BYTES + constant EXPONENT-OFF
EXPONENT-OFF EXPONENT-CAP + constant SCRATCH-BYTES
HTTP:MAX-WORKERS SCRATCH-BYTES * SPAN-BUFFER: SCRATCH

\ PRESENTED and SIGNED borrow the request's header; the rest borrow the worker's
\ scratch.
HTTP:MAX-WORKERS TYPED-BUFFER PRESENTED SPAN:span<u8>
HTTP:MAX-WORKERS TYPED-BUFFER SIGNED SPAN:span<u8>
HTTP:MAX-WORKERS TYPED-BUFFER HEADER JSON:value
HTTP:MAX-WORKERS TYPED-BUFFER CLAIMS JSON:value
HTTP:MAX-WORKERS TYPED-BUFFER SIGNATURE SPAN:span<u8>
HTTP:MAX-WORKERS TYPED-BUFFER KID SPAN:span<u8>
HTTP:MAX-WORKERS TYPED-BUFFER MODULUS SPAN:span<u8>
HTTP:MAX-WORKERS TYPED-BUFFER EXPONENT SPAN:span<u8>

\ The running task's: PASSED is nonzero once ADMIT? admitted FROM, a request's
\ number.
TASK:+USER PASSED n
TASK:+USER FROM n

: SLOT ( -- n ) HTTP:WORKER-SLOT ;
: HEADER@ ( -- JSON:value ) SLOT HEADER @ ;
: CLAIMS@ ( -- JSON:value ) SLOT CLAIMS @ ;

: ROOM ( n n -- SPAN:span<u8> )
   {: off:n len:n :}
   SCRATCH SLOT SCRATCH-BYTES * off + len SPAN:SUB ;

: DECODED ( str SPAN:span<u8> -- SPAN:span<u8> )
   {: a:^ u:n room :}
   a u room BASE64:DECODE-URL {: got:n :}
   room 0 got SPAN:SUB ;


\ Input's fault: malformed JSON or base64url, or a repeated member (JSON:E-MEMBER).
\ No input raises E-JSON-READ-STATE or JSON:E-READ-SOURCE, so they pass.
: MALFORMED? ( n -- bool )
   {: code:n :}
   code E-JSON-READ-STATE = code JSON:E-READ-SOURCE = or if false exit then
   code E-JSON-READ-LAST >= code E-JSON-READ-FIRST <= and
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
   {: obj k:^ ku:n w:^ wu:n absent:bool :}
   obj k ku JSON:FIND MATCH option
      none OF absent ENDOF
      some OF w wu JSON:STRING= ENDOF
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
: INTO-ROOM ( JSON:value SPAN:span<u8> -- n )
   {: v room :}
   v JSON:STRING? 0= if -1 exit then
   v [: tuck room SPAN:COPY ;] JSON:WITH-STRING ;

\ A string member decoded into the room; false when it is absent or no string.
: DECODED$ ( JSON:value str SPAN:span<u8> -- str bool )
   {: room :}
   JSON:FIND MATCH option
      none OF -1 ENDOF
      some OF room INTO-ROOM ENDOF
   ;MATCH {: got:n :}
   got 0< if s" " false exit then
   room SPAN:$ drop got true ;

: NOTE$ ( JSON:value str -- str bool )
   NOTE-OFF HTTP:HEAD-CAP ROOM DECODED$ ;


\ The token of the sole Authorization, when its scheme is Bearer, compared
\ case-insensitively (RFC 6750 2.1).
: TOKEN? ( HTTP:request -- bool )
   {: asked:HTTP:request :}
   asked s" Authorization" HTTP:HEADER-COUNT-OF 1 <> if false exit then
   asked s" Authorization" HTTP:HEADER-OF$ drop {: v:^ vu:n :}
   vu 7 <= if false exit then
   v 6 s" Bearer" STR:=CI 0= if false exit then
   v 6 + c@ 32 <> if false exit then
   v 7 + vu 7 - SPAN:MAKE SLOT PRESENTED ! true ;

: DOT ( str n -- n )
   {: t:^ u:n from:n :}
   u from ?do t i + c@ [char] . = if i unloop exit then loop u ;

: SEGMENT ( str n -- SPAN:span<u8> )
   {: a:^ u:n off:n :}
   a u off HTTP:HEAD-CAP off - ROOM DECODED ;

\ The segments decoded into the worker's scratch, true for an object header
\ naming a kid and object claims; a repeated member throws (RFC 7519 §4).
: OPEN ( -- bool )
   SLOT PRESENTED @ SPAN:$ {: t:^ u:n :}
   t u 0 DOT {: d1:n :}
   d1 u = if false exit then
   t u d1 1+ DOT {: d2:n :}
   d2 u = if false exit then
   t u d2 1+ DOT u <> if false exit then
   t d1 0 SEGMENT {: head :}
   t d1 + 1+ d2 d1 - 1- head SPAN:LEN SEGMENT {: body :}
   t d2 + 1+ u d2 - 1- head SPAN:LEN body SPAN:LEN + SEGMENT SLOT SIGNATURE !
   t d2 SPAN:MAKE SLOT SIGNED !
   head SPAN:$ JSON:PARSE {: h :}
   h JSON:OBJECT? 0= if false exit then
   body SPAN:$ JSON:PARSE {: c :}
   c JSON:OBJECT? 0= if false exit then
   h SLOT HEADER !
   c SLOT CLAIMS !
   h s" kid" NOTE$ {: k:^ ku:n named:bool :}
   k ku SPAN:MAKE SLOT KID ! named ;


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
      KEYS KEYS-U @ JSON:PARSE s" keys" JSON:GET KEY-ARRAY !
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
   h KEYS-URL SPAN:$ CURL:URL! OK? 0= if -1 exit then
   h KEYS-TIMEOUT-MS >MS CURL:TIMEOUT! OK? 0= if -1 exit then
   h KEYS-CAP >LEN FETCHED @ CURL:PERFORM FETCHED-LEN ;

: FETCHED$ ( -- str ) FETCHED @ VEC:SPAN SPAN:$ ;

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
   FETCHED$ {: src:^ u:n :}
   src KEYS u BYTE-COPY u KEYS-U ! ;

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


\ Only RS256 is ever verified, which refuses alg none and HS256; a header with
\ crit names extensions this verifier does not understand (RFC 7515 4.1.11).
: RS256? ( -- bool )
   HEADER@ s" alg" s" RS256" false MEMBER-IS? 0= if false exit then
   HEADER@ s" crit" HAS? if false exit then
   SLOT MODULUS @ SPAN:$ SLOT EXPONENT @ SPAN:$
   SLOT SIGNED @ SPAN:$ SLOT SIGNATURE @ SPAN:$ CRYPTO:RS256-VERIFY? ;

\ Whether the value is the string or an array holding it as an element.
: CONTAINS? ( JSON:value str -- bool )
   {: v w:^ wu:n :}
   v JSON:STRING? if v w wu JSON:STRING= exit then
   v JSON:ARRAY? 0= if false exit then
   false v [: w wu JSON:STRING= nip dup 0= ;] JSON:EACH-WHILE ;

: HOLDS? ( JSON:value str str -- bool )
   {: obj k:^ ku:n w:^ wu:n :}
   obj k ku JSON:FIND MATCH option
      none OF false ENDOF
      some OF w wu CONTAINS? ENDOF
   ;MATCH ;

: AUDIENCE? ( -- bool ) CLAIMS@ s" aud" AUDIENCE SPAN:$ HOLDS? ;

\ The offset of the first space at or after from, or the end.
: SPACE-AT ( str n -- n )
   {: t:^ u:n from:n :}
   u from ?do t i + c@ $20 = if i unloop exit then loop u ;

\ Whether the name is one of the text's words from the offset on, runs of
\ bytes but spaces.
: WORD-FROM? ( str str n -- bool )
   {: t:^ u:n w:^ wu:n from:n :}
   from begin {: at:n :} at u < while
      t u at SPACE-AT {: to:n :}
      to at > if t at + to at - w wu STR:= if true exit then then
      to 1+
   repeat false ;

\ A string claim splits into words here, where AUDIENCE? compares aud whole
\ (RFC 7519 4.1.3).
: HELD? ( str str -- bool )
   {: name:^ nu:n w:^ wu:n :}
   WORDS-OFF HTTP:HEAD-CAP ROOM {: room :}
   CLAIMS@ name nu room DECODED$ if w wu 0 WORD-FROM? exit then
   2drop CLAIMS@ name nu w wu HOLDS? ;

: VALID? ( n -- bool )
   {: now:n :}
   CLAIMS@ s" iss" ISSUER SPAN:$ false MEMBER-IS? 0= if false exit then
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
   {: answer:HTTP:response value:^ vu:n :}
   answer 401 HTTP:STATUS!
   answer s" WWW-Authenticate" value vu HTTP:HEADER! ;

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

: REFUSE ( HTTP:response n -- )
   {: answer:HTTP:response outcome:n :}
   outcome UNAVAILABLE = if answer 503 HTTP:STATUS! exit then
   answer outcome CHALLENGE$ CHALLENGE ;

: NAMED-PREFIX ( -- str ) s" Bearer " ;
: INVALID-PREFIX ( -- str ) S\" Bearer error=\"invalid_token\", " ;
: METADATA-NAME ( -- str ) s" resource_metadata" ;
: SCOPE-NAME ( -- str ) s" scope" ;
: SEPARATOR ( -- str ) s" , " ;

: PUT ( str SPAN:span<u8> n -- n )
   {: a:^ u:n row off:n :}
   a u row off SPAN:SKIP SPAN:COPY
   off u + ;

\ The bytes a parameter `name="value"` takes, from the name's and the value's
\ lengths; none for an empty value, which names nothing.
: PARAM-LEN ( n n -- n )
   {: nu:n vu:n :}
   vu 0= if 0 exit then
   nu vu + 3 + ;


\ Writes nothing for an empty value, as PARAM-LEN measures nothing.
: PARAM ( str str SPAN:span<u8> n -- n )
   {: name:^ nu:n value:^ vu:n row off:n :}
   vu 0= if off exit then
   name nu row off PUT {: past-name:n :}
   S\" =\"" row past-name PUT {: past-eq:n :}
   value vu row past-eq PUT {: past-value:n :}
   S\" \"" row past-value PUT ;


: NAMING ( str str str SPAN:span<u8> n -- n )
   {: pre:^ pu:n url:^ uu:n scope:^ su:n row off:n :}
   pre pu row off PUT {: past-pre:n :}
   METADATA-NAME url uu row past-pre PARAM {: past-url:n :}
   uu 0<> su 0<> and if SEPARATOR row past-url PUT else past-url then
   {: past-sep:n :}
   SCOPE-NAME scope su row past-sep PARAM ;

: CHALLENGES-LEN ( n n -- n )
   {: uu:n su:n :}
   uu 0= su 0= and if 0 exit then
   METADATA-NAME nip uu PARAM-LEN SCOPE-NAME nip su PARAM-LEN +
   uu 0<> su 0<> and if SEPARATOR nip + then {: params:n :}
   NAMED-PREFIX nip INVALID-PREFIX nip + params 2 * + ;

\ RFC 9728 3.1: the segment METADATA!'s URL holds between its authority and
\ the resource's path.
: WELL-KNOWN ( -- str ) s" /.well-known/oauth-protected-resource" ;

: RESOURCE-LEN ( n -- n ) dup 0= if exit then WELL-KNOWN nip - ;

\ The resource the URL names, its bytes but the well-known segment at end.
: RESOURCE+ ( str n SPAN:span<u8> n -- n )
   {: url:^ uu:n end:n row off:n :}
   uu 0= if off exit then
   url end row off PUT {: past:n :}
   end WELL-KNOWN nip + {: from:n :}
   url from + uu from - row past PUT ;

\ METADATA!'s statement as NAMED holds it: the allocation, NAMED-U and
\ CHALLENGES-U; the well-known segment starts at end.
: COMPOSED ( str str n -- SPAN:span<u8> n n )
   {: url:^ uu:n scope:^ su:n end:n :}
   uu su CHALLENGES-LEN {: cu:n :}
   cu uu RESOURCE-LEN + su + {: total:n :}
   total 0= if s" " SPAN:MAKE 0 0 exit then
   total MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN {: row :}
   NAMED-PREFIX url uu scope su row 0 NAMING {: split:n :}
   INVALID-PREFIX url uu scope su row split NAMING drop
   url uu end row cu RESOURCE+ {: past:n :}
   scope su row past PUT drop
   row split cu ;

\ HTTP writes a header value as it is: a quote or backslash would end the
\ quoted resource_metadata early, and a CR or LF would start a line of its own.
: QUOTABLE ( str -- )
   {: text u:n :}
   u 0 ?do
      text i + c@ {: c :}
      c $22 = c $5C = or c $20 < or c $7E > or if E-OPERAND throw then
   loop ;


\ RFC 6749 3.3: tokens of NQCHAR (%x21, %x23-5B, %x5D-7E), each one space
\ from the next. An empty scope names none.
: SCOPE-TOKENS ( str -- )
   {: text u:n :}
   u 0= if exit then
   text c@ $20 = text u 1- + c@ $20 = or if E-OPERAND throw then
   u 0 ?do
      text i + c@ {: c :}
      c $20 = if
         text i 1+ + c@ $20 = if E-OPERAND throw then
      else
         c $21 < c $7E > or c $22 = or c $5C = or if E-OPERAND throw then
      then
   loop ;

: ANSWERING? ( -- bool )
   PASSED @ 0= if false exit then
   FROM @ HTTP:>REQUEST HTTP:SERVING? ;

: V6-LOOPBACK ( -- str ) s" [::1]" ;

\ Decimal 0 to 255 without a leading zero, which a resolver may read as
\ octal.
: OCTET? ( str -- bool )
   {: a:^ u:n :}
   u 1 < u 3 > or if false exit then
   a u STR:DIGITS? 0= if false exit then
   u 1 > a c@ [char] 0 = and if false exit then
   a u s" 255" STR:DIGITS<= ;

\ A dotted-quad IPv4 literal in 127.0.0.0/8.
: LOOPBACK-V4? ( str -- bool )
   {: h:^ hu:n :}
   h hu s" 127." STR:STARTS-WITH? 0= if false exit then
   h hu 4 DOT {: d1:n :}
   h hu d1 1+ DOT {: d2:n :}
   d2 hu >= if false exit then
   h hu d2 1+ DOT hu <> if false exit then
   h 4 + d1 4 - OCTET?
   h d1 + 1+ d2 d1 - 1- OCTET? and
   h d2 + 1+ hu d2 - 1- OCTET? and ;

\ E-OPERAND for an http URL whose host is no loopback IP literal; URI:ORIGIN's
\ rejection for one it cannot read.
: SECURE ( str -- )
   URI:ORIGIN drop {: s:^ su:n h:^ hu:n :}
   s su s" https" STR:=CI if exit then
   h hu V6-LOOPBACK STR:= if exit then
   h hu LOOPBACK-V4? 0= if E-OPERAND throw then ;

\ The resource's path: nothing, or `/` and no query or fragment.
: PATH? ( str -- bool )
   {: p:^ pu:n :}
   pu 0= if true exit then
   p c@ [char] / <> if false exit then
   p pu s" ?" STR:CONTAINS? 0= p pu s" #" STR:CONTAINS? 0= and ;

\ Where the well-known segment starts in METADATA!'s URL, 0 for an empty
\ URL; SECURE's rejection, or E-OPERAND for a URL not of RFC 9728 3.1's form.
: WELL-KNOWN-AT ( str -- n )
   {: url:^ uu:n :}
   uu 0= if 0 exit then
   url uu SECURE
   url uu URI:REQUEST-TARGET$ drop url - {: end:n :}
   url end + uu end - {: rest:^ ru:n :}
   rest ru WELL-KNOWN STR:STARTS-WITH? 0= if E-OPERAND throw then
   rest WELL-KNOWN nip + ru WELL-KNOWN nip - PATH? 0= if E-OPERAND throw then
   end ;

: SCOPES+ ( ^JSON:writer str n -- ^JSON:writer )
   {: s:^ su:n from:n :}
   from begin {: at:n :}
      s su at SPACE-AT {: to:n :}
      s at + to at - JSON:WRITE-STRING
      to su <
   while JSON:WRITE-COMMA to 1+ repeat ;

: RESOURCE-BODY ( ^JSON:writer -- ^JSON:writer )
   JSON:WRITE-OBJECT-START
   s" resource" RESOURCE$ JSON:WRITE-FIELD-S JSON:WRITE-COMMA
   s" authorization_servers" JSON:WRITE-KEY JSON:WRITE-ARRAY-START
   ISSUER SPAN:$ JSON:WRITE-STRING JSON:WRITE-ARRAY-END
   SCOPE$ {: s:^ su:n :}
   su 0 > if
      JSON:WRITE-COMMA s" scopes_supported" JSON:WRITE-KEY
      JSON:WRITE-ARRAY-START s su 0 SCOPES+ JSON:WRITE-ARRAY-END
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
   {: iss:^ iu:n aud:^ au:n url:^ uu:n :}
   HTTP:RUNNING? if E-HTTP-STATE throw then
   iss iu SECURE url uu SECURE
   iss ISSUER-AT ! iu ISSUER-LEN !
   aud AUDIENCE-AT ! au AUDIENCE-LEN !
   url KEYS-URL-AT ! uu KEYS-URL-LEN !
   0 KEYS-U ! 0 KEYS-AT !
   WAKE TASK:SEMAPHORE-DESTROY 0 WAKE TASK:SEMAPHORE-INIT ;


\ The URL of this server's protected-resource metadata and the scope of access
\ a token needs here (RFC 6750 3), which each 401 challenge then names as
\ resource_metadata and scope, and RESOURCE-METADATA answers; an empty text
\ names none. The texts are copied, so they need not outlive the call. A built
\ program keeps neither: it challenges plainly until it states METADATA! at
\ run time. E-OPERAND for a URL neither https nor http to a loopback IP
\ literal, or not of the form
\ `<scheme>://<authority>/.well-known/oauth-protected-resource[<path>]`
\ (RFC 9728 3.1), or holding a quote, a backslash, or a byte below $20 or above
\ $7E unencoded, or for a scope that is not space-separated RFC 6749 3.3 scope
\ tokens; URI:ORIGIN's rejection for a URL it cannot read. A refused text or
\ allocation leaves the previous statement in force. E-HTTP-STATE while HTTP
\ serves.
: METADATA! ( str str -- )
   {: url:^ uu:n scope:^ su:n :}
   HTTP:RUNNING? if E-HTTP-STATE throw then
   url uu QUOTABLE
   url uu WELL-KNOWN-AT {: end:n :}
   scope su SCOPE-TOKENS
   url uu scope su end COMPOSED {: row split:n cu:n :}
   NAMED-SPAN {: was :}
   row SPAN:$ NAMED-LEN ! NAMED ! split NAMED-U ! cu CHALLENGES-U !
   uu RESOURCE-LEN RESOURCE-U !
   was MEM:FREE-SPAN ;


\ The route handler a host gives HTTP:ROUTE for the GET of METADATA!'s URL
\ path: 200 with the protected-resource metadata (RFC 9728 2), naming the
\ resource that URL stands for, the issuer as its one authorization server
\ and METADATA!'s scope tokens as scopes_supported, absent for an empty
\ scope. E-HTTP-STATE before ISSUER! or with no METADATA! URL stated.
: RESOURCE-METADATA ( HTTP:request HTTP:response -- )
   nip {: answer:HTTP:response :}
   ISSUER SPAN:LEN 0= RESOURCE-U @ 0= or if E-HTTP-STATE throw then
   answer 200 HTTP:STATUS!
   answer [: RESOURCE-BODY ;] HTTP:JSON! ;


\ Whether the request carries a current bearer token of the issuer's for this
\ server. A refusal answers the response itself, with an empty body: 401
\ challenging Bearer for no, several or a non-Bearer Authorization, or Bearer
\ error="invalid_token" for a token that fails, either one naming METADATA!'s
\ URL and scope once stated; 503 when there is no key set and none
\ could be fetched. A broken key of the issuer's passes its throw.
\ E-HTTP-STATE before ISSUER!.
: ADMIT? ( HTTP:request HTTP:response -- bool )
   {: asked:HTTP:request answer:HTTP:response :}
   ISSUER SPAN:LEN 0= if E-HTTP-STATE throw then
   0 PASSED !
   asked OUTCOME {: outcome:n :}
   outcome ADMITTED <> if answer outcome REFUSE false exit then
   asked HTTP:REQUEST>N FROM !
   true bool>n PASSED !
   true ;


\ A string claim of the admitted request this worker answers, decoded; false
\ outside one, or for a claim that is absent or not a string. The answer is
\ borrowed until the worker's next CLAIM$ or ADMIT?.
: CLAIM$ ( str -- str bool )
   {: name:^ nu:n :}
   ANSWERING? 0= if s" " false exit then
   CLAIMS@ name nu NOTE$ ;

\ Whether a claim of the admitted request this worker answers holds the
\ name: a string claim as one of its space-separated words (RFC 6749 3.3
\ scope), as Entra's scp; an array claim as an element, as Entra's roles;
\ false outside one.
: CLAIM-HOLDS? ( str str -- bool )
   {: name:^ nu:n w:^ wu:n :}
   ANSWERING? 0= if false exit then
   name nu w wu HELD? ;


private

10000 constant TOKEN-TIMEOUT-MS     \ protective: the bound on the token endpoint's whole answer
$10000 constant TOKEN-CAP           \ protective: the largest token answer read

TYPED-VARIABLE ENDPOINT SPAN:span<u8>
TYPED-VARIABLE CLIENT-ID SPAN:span<u8>
TYPED-VARIABLE SECRET SPAN:span<u8>

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

TYPED-VARIABLE NEWEST ^row

\ Held to find, link or walk the rows, never across an ask.
TASK:FACILITY ROWS-LOCK
ROWS-LOCK TASK:FACILITY-INIT

: GUARD ( SPAN:span<u8> -- ^n ) SPAN:$ drop CELL-VIEW ;

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
\ TOKEN and REFRESH, as literals do. E-OPERAND for a URL neither https nor
\ http to a loopback IP literal, URI:ORIGIN's rejection for one it cannot
\ read, either leaving the previous client in force.
: CLIENT! ( str str str -- )
   {: url:^ uu:n id:^ iu:n secret:^ su:n :}
   url uu SECURE
   url uu SPAN:MAKE ENDPOINT !
   id iu SPAN:MAKE CLIENT-ID !
   secret su SPAN:MAKE SECRET !
   DROP-KEPT ;

private

\ The value must not lie in the vector: each append before it is read may move
\ the block.
: PAIR+ ( str str ^VEC:vector<u8> -- )
   {: k:^ ku:n v:^ vu:n buf:^ :}
   buf VEC:COUNT 0 > if [char] & buf VEC:PUSH c! then
   k ku buf VEC:APPEND
   [char] = buf VEC:PUSH c!
   v vu URI:FORM-LEN {: n:n :}
   v vu buf n VEC:EXTEND URI:FORM-ENCODE drop ;

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
   {: scope:^ su:n g buf:^ :}
   g MATCH grant
      own OF
         buf EMPTIED
         s" grant_type" s" client_credentials" buf PAIR+
         buf CREDENTIALS+
         s" scope" scope su buf PAIR+ ENDOF
      for-person OF {: a:^ au:n :}
         buf EMPTIED
         s" grant_type" s" urn:ietf:params:oauth:grant-type:jwt-bearer" buf PAIR+
         buf CREDENTIALS+
         s" assertion" a au buf PAIR+
         s" scope" scope su buf PAIR+
         s" requested_token_use" s" on_behalf_of" buf PAIR+ ENDOF
   ;MATCH ;

: REFRESH-FORM ( str str ^VEC:vector<u8> -- )
   {: scope:^ su:n r:^ ru:n buf:^ :}
   buf EMPTIED
   s" grant_type" s" refresh_token" buf PAIR+
   buf CREDENTIALS+
   s" refresh_token" r ru buf PAIR+
   s" scope" scope su buf PAIR+ ;

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
   h buf VEC:SPAN SPAN:$ CURL:BODY!
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
   code 400 >= code 500 < and if E-REFUSED exit then
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
   {: status:n what:^ wu:n :}
   OPEN-LINE s" token answer " LOG:TEXT+  status LOG:INT+
   s"  " LOG:TEXT+  what wu LOG:TEXT+  LOG:;LINE
   E-RESPONSE throw ;

: PARSED ( n SPAN:span<u8> SPAN:span<u8> bool -- n n n )
   {: status:n text room renew:bool :}
   text room renew -1 -1 -1 [: KEPT ;] catch {: code:n :}
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
   room SPAN:$ drop {: at:^ :}
   at got at got + kept secs ;

: ANSWERED ( n ^VEC:vector<u8> -- )
   {: status:n buf:^ :}
   OPEN-LINE s" token request answered " LOG:TEXT+  status LOG:INT+
   s" : " LOG:TEXT+  buf VEC:SPAN SPAN:$ LOG:TEXT+  LOG:;LINE ;

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
   {: h:CURL:handle o:^ ou:n res:^ ru:n buf:^ :}
   buf EMPTIED  o ou buf VEC:APPEND
   s" /metadata/identity/oauth2/token?api-version=2018-02-01&resource="
   buf VEC:APPEND  res ru buf URI:QUERY-ENCODE
   h buf VEC:SPAN SPAN:$ CURL:URL!  buf 0 VEC:RESIZE
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
   {: o:^ ou:n res:^ ru:n buf:^ :}
   buf [: o ou res ru buf IMDS-SET? ;] [: IMDS-ERROR ;] ASKED
   buf false ISSUED
   {: a:^ u:n r:^ rn:n secs:n :}
   a u secs ;

public

\ An access token for the scope (for Entra a resource's `api://.../.default`)
\ from the endpoint CLIENT! names, borrowed from the vector until the caller
\ writes to it again, then the answer's expires_in: the token's lifetime in
\ seconds, positive. E-TRANSFER for no answer; E-UNAVAILABLE for a status
\ CURL:UNAVAILABLE? accepts; E-REFUSED for another 4xx, the vector then
\ holding the issuer's answer, empty past TOKEN-CAP; E-RESPONSE for a 2xx
\ that is not JSON naming each member once or lacks an access_token string or
\ a positive integer expires_in, a number or a digit string, or any other
\ status. Each of those failures first logs one line tagged bearer, never a
\ 2xx answer's text. A scope or assertion must not lie in the vector.
\ E-HTTP-STATE before CLIENT!.
: TOKEN ( str grant ^VEC:vector<u8> -- str n )
   {: scope:^ su:n g buf:^ :}
   STATED
   scope su g buf FORM
   buf [: buf REQUESTED? ;] [: STATUS-ERROR ;] ASKED
   buf false ISSUED {: a:^ u:n r:^ ru:n secs:n :}
   a u secs ;


\ A new access token for the scope from the endpoint CLIENT! names, by the
\ refresh grant with the refresh token (RFC 6749 6), then the answer's refresh
\ token, empty when it has none (the old one then stays the one to present),
\ then expires_in. Both texts are borrowed from the vector until the caller
\ writes to it again. It fails as TOKEN does, and E-RESPONSE for a 2xx whose
\ refresh_token is not a string. A scope or refresh token must not lie in the
\ vector.
: REFRESH ( str str ^VEC:vector<u8> -- str str n )
   {: scope:^ su:n r:^ ru:n buf:^ :}
   STATED
   scope su r ru buf REFRESH-FORM
   buf [: buf REQUESTED? ;] [: STATUS-ERROR ;] ASKED
   buf true ISSUED ;

private

: ROW-ORIGIN$ ( ^row -- str ) ROW-ORIGIN @ VEC:SPAN SPAN:$ ;
: ROW-SCOPE$ ( ^row -- str ) ROW-SCOPE @ VEC:SPAN SPAN:$ ;
: ROW-KEPT$ ( ^row -- str ) ROW-TOKEN @ VEC:SPAN SPAN:$ ;

\ Under ROWS-LOCK: the row of the kind, origin and scope, else null.
: FOUND ( bool str str -- ^row )
   {: id:bool o:^ ou:n scope:^ su:n :}
   NEWEST @
   begin dup NULL-PTR <> while
      dup ROW-IDENTITY @ bool>n id bool>n =
      over ROW-ORIGIN$ o ou STR:= and
      over ROW-SCOPE$ scope su STR:= and if exit then
      ROW-NEXT @
   repeat ;

: EMPTY ( -- ^VEC:vector<u8> ) HEAP:ALLOCATOR 0 [type] u8 VEC:NEW ;

\ The row's lock made ready in its zeroed region and its vectors made, the
\ origin's and scope's holding them; a throw leaves what was made to UNMADE.
: MADE ( bool str str ^row -- )
   {: id:bool o:^ ou:n scope:^ su:n r:^ :}
   id r ROW-IDENTITY !
   TASK:FACILITY-BYTES MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN r ROW-REGION !
   0 r ROW-REGION @ SPAN:FILL
   r ROW-REGION @ GUARD TASK:FACILITY-INIT
   EMPTY r ROW-ORIGIN !  EMPTY r ROW-SCOPE !
   EMPTY r ROW-TOKEN !  EMPTY r ROW-ANSWER !
   o ou r ROW-ORIGIN @ VEC:APPEND
   scope su r ROW-SCOPE @ VEC:APPEND ;

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
   {: id:bool o:^ ou:n scope:^ su:n :}
   HEAP:ALLOCATOR [type] row ALLOC:NEW {: r:^ :}
   [: id o ou scope su r MADE ;] catch {: code:n :}
   code 0<> if r UNMADE code throw then
   NEWEST @ r ROW-NEXT !
   r NEWEST !  r ;

\ Under ROWS-LOCK.
: ROW-FOR ( bool str str -- ^row )
   {: id:bool o:^ ou:n scope:^ su:n :}
   id o ou scope su FOUND dup NULL-PTR = if
      drop id o ou scope su LINKED
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
   r ISSUED-FOR {: a:^ u:n secs:n :}
   r ROW-TOKEN @ {: kept:^ :}
   kept 0 VEC:RESIZE  a u kept VEC:APPEND
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
   {: scope:^ su:n q :}
   STATED
   false s" " scope su [: ROW-FOR ;] LISTED q HELD ;

\ Azure's metadata service (IMDS); production passes it to IDENTITY-TOKEN.
: IMDS$ ( -- str ) s" http://169.254.169.254" ;

\ Appends to the vector the managed identity's token for the resource (for
\ Azure PostgreSQL `https://ossrdbms-aad.database.windows.net`) from the
\ IMDS at the origin, kept per origin and resource as CURRENT keeps a
\ scope's: asked again once LEEWAY-S or less of its expires_in remains, one
\ task at a time, a task that waited on a failed ask throwing its code.
\ Callable from any task. E-TRANSFER for no answer; E-UNAVAILABLE for 404,
\ 410 and a status CURL:UNAVAILABLE? accepts; E-REFUSED for another 4xx;
\ E-RESPONSE as TOKEN. Each failed ask first logs one line tagged bearer,
\ never a 2xx answer's text. The caller wipes the vector (`0 VEC:RESIZE`)
\ when done.
: IDENTITY-TOKEN ( str str ^VEC:vector<u8> -- )
   {: o:^ ou:n res:^ ru:n dest:^ :}
   true o ou res ru [: ROW-FOR ;] LISTED
   [: dest VEC:APPEND ;] HELD ;

;package
