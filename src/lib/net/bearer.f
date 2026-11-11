\ bearer.f - OAuth bearer tokens (RFC 6750) on Habu's HTTP server. A route
\ admits a request whose Authorization carries a compact JWS that one issuer
\ signed RS256 with a key of its JWK Set, whose iss and aud name that issuer
\ and this server, and whose exp and nbf hold (RFC 7515, 7517, 7519). The host
\ states the issuer with ISSUER! before HTTP:START, guards a route with
\ `req res BEARER:ADMIT? if ... then` and reads the caller's string claims with
\ CLAIM$. METADATA! names this server's protected-resource metadata (RFC 9728
\ 5.1) and the scope its tokens need (RFC 6750 3) in each 401 challenge. As a
\ client, TOKEN gets a token from the token endpoint CLIENT! names, for the
\ client itself (RFC 6749 4.4) or for a person (Entra On-Behalf-Of), and
\ REFRESH renews one with a refresh grant (RFC 6749 6).
\ STORAGE CLASS. PROCESS-WIDE: the issuer and the challenges naming the
\ metadata and scope, written before HTTP serves; the key set, read and
\ replaced under KEYS-LOCK and fetched over CURL outside it by one worker at a
\ time; tables indexed by HTTP:WORKER-SLOT holding each request's token and
\ scratch; the client CLIENT! states, written before any TOKEN or REFRESH; and
\ the cursor that reads a token answer, under ANSWER-LOCK.
\ TASK-LOCAL: the admitted request, TASK:+USER rows.

require src/lib/string.f
require src/lib/span.f
require src/lib/memory.f
require src/lib/base64.f
require src/lib/json.f
require src/lib/log.f
require src/lib/task.f
require src/lib/adt/option.f                  \ an exchange's status, none without an answer
require src/lib/crypto/evp.f
require src/lib/net/curl.f
require src/lib/net/http.f

package BEARER
public

-9630 constant E-TRANSFER           \ no answer from the token endpoint
-9631 constant E-REFUSED            \ the issuer refused with a 4xx CURL:UNAVAILABLE? does not accept
-9632 constant E-OPERAND            \ a METADATA! text holding a byte its quoted challenge value cannot carry, or a scope that is not space-separated scope tokens
-9633 constant E-UNAVAILABLE        \ the token endpoint answered a status CURL:UNAVAILABLE? accepts
-9634 constant E-RESPONSE           \ a reply TOKEN or REFRESH cannot use
private
-9630 constant E-FIRST
-9639 constant E-LAST

60 constant LEEWAY-S                \ seconds of clock skew between issuer and server
$10000 constant KEYS-CAP            \ the largest key set text accepted
5000 constant KEYS-TIMEOUT-MS
1000 constant REFETCH-MS            \ at most one fetch a window, after the last returned
1000000 constant NS-PER-MS
8 constant EXPONENT-CAP

0 constant ADMITTED
1 constant NO-TOKEN                 \ 401 challenging Bearer
2 constant INVALID                  \ 401 challenging Bearer error="invalid_token"
3 constant UNAVAILABLE              \ 503: no key set, and none could be fetched

TYPED-VARIABLE ISSUER SPAN:span<u8>
TYPED-VARIABLE AUDIENCE SPAN:span<u8>
TYPED-VARIABLE KEYS-URL SPAN:span<u8>

\ NAMED-LEN bytes at NAMED, an allocation holding the two challenges naming
\ METADATA!'s URL and scope, the no-token one's NAMED-U bytes first; empty while
\ neither is stated. A built program starts with the plain challenges: NAMED is
\ a PTR-VARIABLE and NAMED-LEN and NAMED-U are scratch cells, each written as 0
\ in the product.
PTR-VARIABLE NAMED
variable NAMED-LEN   NAMED-LEN scratch-cell-mark
variable NAMED-U   NAMED-U scratch-cell-mark

: NAMED-SPAN ( -- SPAN:span<u8> ) NAMED @ NAMED-LEN @ SPAN:MAKE ;

\ The key set's text, under KEYS-LOCK with KEYS-AT and FETCHING; the lock is
\ never held across the network. FETCHED is the flagged fetcher's alone.
KEYS-CAP BUFFER: KEYS
variable KEYS-U
variable KEYS-AT                    \ when the last fetch returned, a mono-ns; 0 before any
TYPED-VARIABLE FETCHING bool        \ a worker is fetching
KEYS-CAP BUFFER: FETCHED
variable FETCHED-U
TYPED-VARIABLE ACCEPTED bool        \ FETCHED holds a key set of FETCHED-U bytes
TASK:FACILITY KEYS-LOCK
KEYS-LOCK TASK:FACILITY-INIT

\ A worker's scratch: the decoded segments, no longer than the token and so
\ than the head; a note holding the kid, then CLAIM$'s answer; the key.
0 constant SEGMENTS-OFF
HTTP:HEAD-CAP constant NOTE-OFF
HTTP:HEAD-CAP 2 * constant MODULUS-OFF
MODULUS-OFF CRYPTO:RS256-MAX-BYTES + constant EXPONENT-OFF
EXPONENT-OFF EXPONENT-CAP + constant SCRATCH-BYTES
HTTP:MAX-WORKERS SCRATCH-BYTES * SPAN-BUFFER: SCRATCH

\ A row to each HTTP worker; PRESENTED and SIGNED borrow the request's header.
HTTP:MAX-WORKERS TYPED-BUFFER READER JSON:cursor
HTTP:MAX-WORKERS TYPED-BUFFER OUTER JSON:cursor       \ the key set's, while READER reads a key
HTTP:MAX-WORKERS TYPED-BUFFER PRESENTED SPAN:span<u8>
HTTP:MAX-WORKERS TYPED-BUFFER SIGNED SPAN:span<u8>
HTTP:MAX-WORKERS TYPED-BUFFER HEADER SPAN:span<u8>
HTTP:MAX-WORKERS TYPED-BUFFER CLAIMS SPAN:span<u8>
HTTP:MAX-WORKERS TYPED-BUFFER SIGNATURE SPAN:span<u8>
HTTP:MAX-WORKERS TYPED-BUFFER KID SPAN:span<u8>
HTTP:MAX-WORKERS TYPED-BUFFER MODULUS SPAN:span<u8>
HTTP:MAX-WORKERS TYPED-BUFFER EXPONENT SPAN:span<u8>

\ The running task's: PASSED is nonzero once ADMIT? admitted FROM, a request's
\ number (HTTP:REQUEST>N).
TASK:#USER
CELL TASK:+USER PASSED
CELL TASK:+USER FROM
drop

: SLOT ( -- n ) HTTP:WORKER-SLOT ;
: CURSOR ( -- ptr JSON:cursor ) SLOT READER ;
: OUTER-CURSOR ( -- ptr JSON:cursor ) SLOT OUTER ;
: HEADER$ ( -- ptr u8 n ) SLOT HEADER @ SPAN:$ ;
: CLAIMS$ ( -- ptr u8 n ) SLOT CLAIMS @ SPAN:$ ;

: ROOM ( n n -- SPAN:span<u8> )
   {: off:n len:n :}
   SCRATCH SLOT SCRATCH-BYTES * off + len SPAN:SUB ;

: DECODED ( ptr u8 n SPAN:span<u8> -- SPAN:span<u8> )
   {: a:ptr u:n room :}
   a u room BASE64:DECODE-URL {: got:n :}
   room 0 got SPAN:SUB ;


\ Malformed JSON or base64url: the token's fault, or a fetched text's. No input
\ raises E-JSON-READ-STATE or JSON:E-READ-SOURCE, so they are BEARER's own misuse and pass.
: MALFORMED? ( n -- bool )
   {: code:n :}
   code E-JSON-READ-STATE = code JSON:E-READ-SOURCE = or if false exit then
   code E-JSON-READ-LAST >= code E-JSON-READ-FIRST <= and
   code E-BASE64-LAST >= code E-BASE64-FIRST <= and or ;

\ One step over untrusted text: malformed is false, any other throw passes.
: STEP ( [ bool -- bool ] -- bool )
   false swap catch {: ok:bool code:n :}
   code 0= if ok exit then
   code MALFORMED? if false exit then
   code throw ;


\ The worker's reader on an object text at a member's value, and its token
\ kind, -1 when the member is absent; the caller reads and closes it.
: MEMBER ( ptr u8 n ptr u8 n -- JSON:reader n )
   {: t:ptr tu:n k:ptr ku:n :}
   CURSOR t tu JSON:READ-INIT JSON:READ-NEXT drop
   k ku JSON:READ-FIND-KEY 0= if -1 exit then
   JSON:READ-TOKEN ;

: KIND-OF ( ptr u8 n ptr u8 n -- n ) MEMBER {: kind:n :} JSON:READ-CLOSE kind ;

\ Whether an object text's member is a string decoding to the wanted bytes; an
\ absent one answers the given flag.
: MEMBER-IS? ( ptr u8 n ptr u8 n ptr u8 n bool -- bool )
   {: t:ptr tu:n k:ptr ku:n w:ptr wu:n absent:bool :}
   t tu k ku MEMBER {: kind:n :}
   kind -1 = if JSON:READ-CLOSE absent exit then
   kind JSON:T-STR <> if JSON:READ-CLOSE false exit then
   w wu JSON:READ-STR-EQ? {: same:bool :} JSON:READ-CLOSE same ;

\ An object text's string member as sent, escapes encoded; empty when absent or
\ not a string.
: MEMBER$ ( ptr u8 n ptr u8 n -- ptr u8 n )
   MEMBER JSON:T-STR <> if JSON:READ-CLOSE s" " exit then
   JSON:READ-SPAN$ {: a:ptr u:n :} JSON:READ-CLOSE a u ;

\ An object text's integer member; false when absent or not an integer.
: MEMBER-INT ( ptr u8 n ptr u8 n -- n bool )
   MEMBER JSON:T-INT <> if JSON:READ-CLOSE 0 false exit then
   JSON:READ-INT {: v:n :} JSON:READ-CLOSE v true ;

\ An object text's string member decoded into the worker's note; false when
\ absent or not a string.
: NOTE$ ( ptr u8 n ptr u8 n -- ptr u8 n bool )
   NOTE-OFF HTTP:HEAD-CAP ROOM {: note :}
   MEMBER JSON:T-STR <> if JSON:READ-CLOSE s" " false exit then
   note SPAN:$ JSON:READ-STR {: got:n :} JSON:READ-CLOSE
   note SPAN:$ drop got true ;


\ The token of the sole Authorization, when its scheme is Bearer, compared
\ case-insensitively (RFC 6750 2.1).
: TOKEN? ( HTTP:request -- bool )
   {: asked:HTTP:request :}
   asked s" Authorization" HTTP:HEADER-COUNT-OF 1 <> if false exit then
   asked s" Authorization" HTTP:HEADER-OF$ drop {: v:ptr vu:n :}
   vu 7 <= if false exit then
   v 6 s" Bearer" STR=CI 0= if false exit then
   v 6 + c@ 32 <> if false exit then
   v 7 + vu 7 - SPAN:MAKE SLOT PRESENTED ! true ;

\ The index of the first dot at or after from, else the text's length.
: DOT ( ptr u8 n n -- n )
   {: t:ptr u:n from:n :}
   u from ?do t i + c@ [char] . = if i unloop exit then loop u ;

\ A segment decoded into the scratch's segments at off.
: SEGMENT ( ptr u8 n n -- SPAN:span<u8> )
   {: a:ptr u:n off:n :}
   a u off HTTP:HEAD-CAP off - ROOM DECODED ;

\ Whether a text is one JSON object; malformed JSON throws the reader's code.
: OBJECT? ( SPAN:span<u8> -- bool )
   {: text :}
   CURSOR text SPAN:$ JSON:READ-INIT
   JSON:READ-NEXT JSON:T-OBJ = if JSON:READ-FINISH true exit then
   JSON:READ-CLOSE false ;

\ The token's three segments decoded into the worker's scratch: a header and
\ claims that are JSON objects repeating no member name (RFC 7519 §4), the
\ header naming a kid.
: OPEN ( -- bool )
   SLOT PRESENTED @ SPAN:$ {: t:ptr u:n :}
   t u 0 DOT {: d1:n :}
   d1 u = if false exit then
   t u d1 1+ DOT {: d2:n :}
   d2 u = if false exit then
   t u d2 1+ DOT u <> if false exit then
   t d1 0 SEGMENT {: head :}
   t d1 + 1+ d2 d1 - 1- head SPAN:LEN SEGMENT {: body :}
   t d2 + 1+ u d2 - 1- head SPAN:LEN body SPAN:LEN + SEGMENT SLOT SIGNATURE !
   t d2 SPAN:MAKE SLOT SIGNED !
   head SLOT HEADER !
   body SLOT CLAIMS !
   head OBJECT? 0= if false exit then
   body OBJECT? 0= if false exit then
   HEADER$ JSON:READ-REPEATS? if false exit then
   CLAIMS$ JSON:READ-REPEATS? if false exit then
   HEADER$ s" kid" NOTE$ {: k:ptr ku:n named:bool :}
   k ku SPAN:MAKE SLOT KID ! named ;


\ A key set member for the token's kid: kty RSA, and use sig and alg RS256 when
\ stated. Its n and e are decoded into the worker's scratch; ones that will not
\ decode throw, since a key the issuer publishes broken is not the caller's.
: PICK? ( ptr u8 n -- bool )
   {: m:ptr mu:n :}
   m mu s" kid" SLOT KID @ SPAN:$ false MEMBER-IS? 0= if false exit then
   m mu s" kty" s" RSA" false MEMBER-IS? 0= if false exit then
   m mu s" use" s" sig" true MEMBER-IS? 0= if false exit then
   m mu s" alg" s" RS256" true MEMBER-IS? 0= if false exit then
   m mu s" n" MEMBER$ MODULUS-OFF CRYPTO:RS256-MAX-BYTES ROOM DECODED SLOT MODULUS !
   m mu s" e" MEMBER$ EXPONENT-OFF EXPONENT-CAP ROOM DECODED SLOT EXPONENT !
   true ;

\ One element of the keys array: 1 the token's key, 0 another, -1 past the last.
: KEY-ELEMENT ( JSON:reader -- JSON:reader n )
   JSON:READ-NEXT {: kind:n :}
   kind JSON:T-ARR-END = if -1 exit then
   kind JSON:T-OBJ = if JSON:READ-VALUE-SPAN$ PICK? if 1 else 0 then exit then
   kind JSON:T-ARR = if JSON:READ-VALUE-SPAN$ 2drop then
   0 ;

\ The token's key from the cached set, decoded into the worker's scratch.
: SELECT? ( -- bool )
   KEYS-U @ 0= if false exit then
   OUTER-CURSOR KEYS KEYS-U @ JSON:READ-INIT
   JSON:READ-NEXT drop s" keys" JSON:READ-FIND-KEY drop
   begin KEY-ELEMENT dup 0= while drop repeat
   {: found:n :} JSON:READ-CLOSE found 0 > ;


: OK? ( CURL:status -- bool )
   MATCH CURL:status
      ok OF true ENDOF
      failed OF drop false ENDOF
   ;MATCH ;

\ A whole 200 answer's length, else -1.
: FETCHED-LEN ( CURL:fetch-result -- n )
   MATCH CURL:fetch-result
      response OF {: status:CURL:http-status got:len :}
         status CURL:HTTP-STATUS>N 200 = if got LEN>N else -1 then ENDOF
      truncated OF drop drop -1 ENDOF
      failed OF drop -1 ENDOF
   ;MATCH ;

: TRANSFER ( CURL:handle -- n )
   {: h:CURL:handle :}
   h KEYS-URL @ SPAN:$ CURL:URL! OK? 0= if -1 exit then
   h KEYS-TIMEOUT-MS >MS CURL:TIMEOUT! OK? 0= if -1 exit then
   h FETCHED KEYS-CAP >LEN CURL:PERFORM FETCHED-LEN ;

\ A fetched text whose value is an object with a keys array, well formed
\ throughout.
: KEY-SET? ( -- bool )
   OUTER-CURSOR FETCHED FETCHED-U @ JSON:READ-INIT
   JSON:READ-NEXT JSON:T-OBJ <> if JSON:READ-CLOSE false exit then
   s" keys" JSON:READ-FIND-KEY 0= if JSON:READ-CLOSE false exit then
   JSON:READ-TOKEN JSON:T-ARR <> if JSON:READ-CLOSE false exit then
   JSON:READ-FINISH true ;

\ TRANSFER's answer in place of the second cell, the handle kept for catch.
: TRANSFERRED ( CURL:handle n -- CURL:handle n ) drop dup TRANSFER ;

\ The issuer's key set into FETCHED, outside KEYS-LOCK: ACCEPTED when it was
\ answered whole with 200 and is a key set.
: FETCH ( -- )
   false ACCEPTED !
   CURL:INIT
   MATCH CURL:init-result
      ready OF {: h:CURL:handle :}
         h -1 [: TRANSFERRED ;] catch {: code:n :}
         h CURL:CLEANUP
         code 0<> if 2drop code throw then
         nip ENDOF
      failed OF drop -1 ENDOF
   ;MATCH FETCHED-U !
   FETCHED-U @ 0< if exit then
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

\ Under KEYS-LOCK: the cached answer, and whether this worker becomes the one
\ fetcher, for a set that is empty or lacks the kid, once a REFETCH-MS window.
: LOOKUP ( -- n bool )
   CACHED {: found:n :}
   found ADMITTED = FETCHING @ or DUE? 0= or if found false exit then
   true FETCHING ! found true ;

\ Under KEYS-LOCK: the fetch has returned, by a throw too; an accepted set
\ replaces the cached one.
: RETURNED ( -- )
   ACCEPTED @ if FETCHED KEYS FETCHED-U @ BYTE-COPY FETCHED-U @ KEYS-U ! then
   mono-ns KEYS-AT !
   false FETCHING ! ;

: SETTLE ( -- ) [: RETURNED ;] LOCKED ;

\ The token's key into the worker's scratch, as CACHED answers. While one
\ worker fetches, the others answer from the set as it stands.
: KEY ( -- n )
   [: LOOKUP ;] LOCKED {: found:n fetcher:bool :}
   fetcher 0= if found exit then
   [: FETCH ;] [: SETTLE ;] finally
   [: CACHED ;] LOCKED ;


\ Only RS256 is ever verified, which refuses alg none and HS256; a header with
\ crit names extensions this verifier does not understand (RFC 7515 4.1.11).
: RS256? ( -- bool )
   HEADER$ s" alg" s" RS256" false MEMBER-IS? 0= if false exit then
   HEADER$ s" crit" KIND-OF -1 <> if false exit then
   SLOT MODULUS @ SPAN:$ SLOT EXPONENT @ SPAN:$
   SLOT SIGNED @ SPAN:$ SLOT SIGNATURE @ SPAN:$ CRYPTO:RS256-VERIFY? ;

\ One element of an aud array: 1 the audience, 0 another, -1 past the last.
: AUDIENCE-ELEMENT ( JSON:reader -- JSON:reader n )
   JSON:READ-NEXT {: kind:n :}
   kind JSON:T-ARR-END = if -1 exit then
   kind JSON:T-STR = if AUDIENCE @ SPAN:$ JSON:READ-STR-EQ? if 1 else 0 then exit then
   kind JSON:T-OBJ = kind JSON:T-ARR = or if JSON:READ-VALUE-SPAN$ 2drop then
   0 ;

\ aud is the audience or an array holding it.
: AUDIENCE? ( -- bool )
   CLAIMS$ s" aud" MEMBER {: kind:n :}
   kind JSON:T-STR = if
      AUDIENCE @ SPAN:$ JSON:READ-STR-EQ? {: same:bool :} JSON:READ-CLOSE same exit
   then
   kind JSON:T-ARR <> if JSON:READ-CLOSE false exit then
   begin AUDIENCE-ELEMENT dup 0= while drop repeat
   {: found:n :} JSON:READ-CLOSE found 0 > ;

\ The claims hold at now: iss is the issuer, aud holds the audience, exp is
\ later and nbf, when stated, not later, each by LEEWAY-S (RFC 7519 4.1).
: VALID? ( n -- bool )
   {: now:n :}
   CLAIMS$ s" iss" ISSUER @ SPAN:$ false MEMBER-IS? 0= if false exit then
   AUDIENCE? 0= if false exit then
   CLAIMS$ s" exp" MEMBER-INT 0= if drop false exit then
   now LEEWAY-S - > 0= if false exit then
   CLAIMS$ s" nbf" KIND-OF -1 = if true exit then
   CLAIMS$ s" nbf" MEMBER-INT 0= if drop false exit then
   now LEEWAY-S + <= ;


: OUTCOME ( HTTP:request -- n )
   TOKEN? 0= if NO-TOKEN exit then
   [: drop OPEN ;] STEP 0= if INVALID exit then
   KEY {: found:n :}
   found ADMITTED <> if found exit then
   RS256? 0= if INVALID exit then
   [: drop epoch-seconds VALID? ;] STEP 0= if INVALID exit then
   ADMITTED ;

: CHALLENGE ( HTTP:response ptr u8 n -- )
   {: answer:HTTP:response value:ptr vu:n :}
   answer 401 HTTP:STATUS!
   answer s" WWW-Authenticate" value vu HTTP:HEADER! ;

\ The challenge for NO-TOKEN or INVALID.
: CHALLENGE$ ( n -- ptr u8 n )
   {: outcome:n :}
   NAMED-SPAN {: named :}
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

: NAMED-PREFIX ( -- ptr u8 n ) s" Bearer " ;
: INVALID-PREFIX ( -- ptr u8 n ) S\" Bearer error=\"invalid_token\", " ;
: METADATA-NAME ( -- ptr u8 n ) s" resource_metadata" ;
: SCOPE-NAME ( -- ptr u8 n ) s" scope" ;
: SEPARATOR ( -- ptr u8 n ) s" , " ;

\ The bytes copied into the span at off; answers the offset past them.
: PUT ( ptr u8 n SPAN:span<u8> n -- n )
   {: a:ptr u:n row off:n :}
   a u row off SPAN:SKIP SPAN:COPY
   off u + ;

\ The bytes a parameter `name="value"` takes, from the name's and the value's
\ lengths; none for an empty value, which names nothing.
: PARAM-LEN ( n n -- n )
   {: nu:n vu:n :}
   vu 0= if 0 exit then
   nu vu + 3 + ;


\ `name="value"` written into the span at off; answers the offset past it.
\ Writes nothing for an empty value, as PARAM-LEN measures nothing.
: PARAM ( ptr u8 n ptr u8 n SPAN:span<u8> n -- n )
   {: name:ptr nu:n value:ptr vu:n row off:n :}
   vu 0= if off exit then
   name nu row off PUT {: past-name:n :}
   S\" =\"" row past-name PUT {: past-eq:n :}
   value vu row past-eq PUT {: past-value:n :}
   S\" \"" row past-value PUT ;


\ The prefix, then resource_metadata for a nonempty URL and scope for a
\ nonempty scope, joined by SEPARATOR when both are present, written into the
\ span at off; answers the offset past them.
: NAMING ( ptr u8 n ptr u8 n ptr u8 n SPAN:span<u8> n -- n )
   {: pre:ptr pu:n url:ptr uu:n scope:ptr su:n row off:n :}
   pre pu row off PUT {: past-pre:n :}
   METADATA-NAME url uu row past-pre PARAM {: past-url:n :}
   uu 0<> su 0<> and if SEPARATOR row past-url PUT else past-url then
   {: past-sep:n :}
   SCOPE-NAME scope su row past-sep PARAM ;

\ The challenges naming the URL and scope, in a new allocation, and the
\ no-token one's length; an empty span when both are empty.
: COMPOSED ( ptr u8 n ptr u8 n -- SPAN:span<u8> n )
   {: url:ptr uu:n scope:ptr su:n :}
   uu 0= su 0= and if s" " SPAN:MAKE 0 exit then
   METADATA-NAME nip uu PARAM-LEN SCOPE-NAME nip su PARAM-LEN +
   uu 0<> su 0<> and if SEPARATOR nip + then {: params:n :}
   NAMED-PREFIX nip INVALID-PREFIX nip + params 2 * +
   MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN {: row :}
   NAMED-PREFIX url uu scope su row 0 NAMING {: split:n :}
   INVALID-PREFIX url uu scope su row split NAMING drop
   row split ;

\ HTTP writes a header value as it is: a quote or backslash would end the
\ quoted resource_metadata early, and a CR or LF would start a line of its own.
: QUOTABLE ( ptr u8 n -- )
   {: text u:n :}
   u 0 ?do
      text i + c@ {: c :}
      c $22 = c $5C = or c $20 < or c $7F = or if E-OPERAND throw then
   loop ;


\ RFC 6749 3.3: tokens of NQCHAR (%x21, %x23-5B, %x5D-7E), each one space
\ from the next. An empty scope names none.
: SCOPE-TOKENS ( ptr u8 n -- )
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

public

\ The issuer whose tokens ADMIT? accepts: its iss, the audience this server
\ answers to, and its JWK Set's URL. The texts are borrowed and outlive the
\ server, as literals do. E-HTTP-STATE while HTTP serves.
: ISSUER! ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: iss:ptr iu:n aud:ptr au:n url:ptr uu:n :}
   HTTP:RUNNING? if E-HTTP-STATE throw then
   iss iu SPAN:MAKE ISSUER !
   aud au SPAN:MAKE AUDIENCE !
   url uu SPAN:MAKE KEYS-URL !
   0 KEYS-U ! 0 KEYS-AT ! ;


\ The URL of this server's protected-resource metadata and the scope of access
\ a token needs here (RFC 6750 3), which each 401 challenge then names as
\ resource_metadata and scope; an empty text names none. The texts are copied,
\ so they need not outlive the call. A built program keeps neither: it
\ challenges plainly until it states METADATA! at run time.
\ E-OPERAND for a URL holding a quote, a backslash, a control byte below $20 or
\ $7F, which the quoted challenge value cannot carry, or a scope that is not
\ space-separated RFC 6749 3.3 scope tokens. A refused text or allocation
\ leaves the previous challenges in force. E-HTTP-STATE while HTTP serves.
: METADATA! ( ptr u8 n ptr u8 n -- )
   {: url:ptr uu:n scope:ptr su:n :}
   HTTP:RUNNING? if E-HTTP-STATE throw then
   url uu QUOTABLE
   scope su SCOPE-TOKENS
   url uu scope su COMPOSED {: row split:n :}
   NAMED-SPAN {: was :}
   row SPAN:$ NAMED-LEN ! NAMED ! split NAMED-U !
   was SPAN:LEN 0 > if was MEM:FREE-SPAN then ;


\ Whether the request carries a current bearer token of the issuer's for this
\ server. A refusal answers the response itself, with an empty body: 401
\ challenging Bearer for no, several or a non-Bearer Authorization, or Bearer
\ error="invalid_token" for a token that fails, either one naming METADATA!'s
\ URL and scope once stated; 503 when there is no key set and none
\ could be fetched. A broken key of the issuer's passes its throw.
\ E-HTTP-STATE before ISSUER!.
: ADMIT? ( HTTP:request HTTP:response -- bool )
   {: asked:HTTP:request answer:HTTP:response :}
   ISSUER @ SPAN:LEN 0= if E-HTTP-STATE throw then
   0 PASSED !
   asked OUTCOME {: outcome:n :}
   outcome ADMITTED <> if answer outcome REFUSE false exit then
   asked HTTP:REQUEST>N FROM !
   true bool>n PASSED !
   true ;


\ A string claim of the admitted request this worker answers, decoded; false
\ outside one, or for a claim that is absent or not a string. The answer is
\ borrowed until the worker's next CLAIM$ or ADMIT?.
: CLAIM$ ( ptr u8 n -- ptr u8 n bool )
   {: name:ptr nu:n :}
   PASSED @ 0= if s" " false exit then
   FROM @ HTTP:>REQUEST HTTP:SERVING? 0= if s" " false exit then
   CLAIMS$ name nu NOTE$ ;


\ Getting a token.

10000 constant TOKEN-TIMEOUT-MS     \ protective: the bound on the token endpoint's whole answer
$10000 constant TOKEN-CAP           \ protective: the largest token answer read

TYPED-VARIABLE ENDPOINT SPAN:span<u8>
TYPED-VARIABLE CLIENT-ID SPAN:span<u8>
TYPED-VARIABLE SECRET SPAN:span<u8>

\ One cursor for every task, held under ANSWER-LOCK only while it reads a
\ token answer, which takes no I/O.
TYPED-VARIABLE ANSWER-CURSOR JSON:cursor
TASK:FACILITY ANSWER-LOCK
ANSWER-LOCK TASK:FACILITY-INIT

public

\ How TOKEN asks: as the client itself (client credentials), or for the person
\ whose token is the assertion (Entra On-Behalf-Of).
ENUM grant
   VARIANT own ;VARIANT
   VARIANT for-person FIELD assertion ptr u8 FIELD len n ;VARIANT
;ENUM

\ The token endpoint's URL, and the client id and secret this process presents
\ there, written before any TOKEN or REFRESH, not while one runs. An empty
\ secret states a public client, which presents none (RFC 6749 2.3.1). The
\ texts are borrowed and outlive every TOKEN and REFRESH, as literals do.
: CLIENT! ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: url:ptr uu:n id:ptr iu:n secret:ptr su:n :}
   url uu SPAN:MAKE ENDPOINT !
   id iu SPAN:MAKE CLIENT-ID !
   secret su SPAN:MAKE SECRET ! ;

private

\ RFC 3986's unreserved bytes, which a form value carries as they are.
: UNRESERVED? ( n -- bool )
   {: c:n :}
   c [char] A >= c [char] Z <= and
   c [char] a >= c [char] z <= and or
   c [char] 0 >= c [char] 9 <= and or
   c [char] - = or c [char] . = or c [char] _ = or c [char] ~ = or ;

: HEX-DIGIT ( n -- n ) dup 10 < if [char] 0 + else 10 - [char] A + then ;

\ A byte of a form value appended to the BUF, percent-encoded unless
\ unreserved (RFC 6749 Appendix B).
: ENCODED+ ( n ptr n -- )
   {: c:n buf:ptr :}
   c UNRESERVED? if c buf BUF:APPEND-BYTE exit then
   [char] % buf BUF:APPEND-BYTE
   c 4 rshift HEX-DIGIT buf BUF:APPEND-BYTE
   c 15 and HEX-DIGIT buf BUF:APPEND-BYTE ;

\ name=value appended to the BUF's form, after an & unless it is the first.
: PAIR+ ( ptr u8 n ptr u8 n ptr n -- )
   {: k:ptr ku:n v:ptr vu:n buf:ptr :}
   buf BUF:LEN@ BUF:BLEN>N 0 > if [char] & buf BUF:APPEND-BYTE then
   k ku BUF:N>BLEN buf BUF:APPEND-SPAN
   [char] = buf BUF:APPEND-BYTE
   vu 0 ?do v i + c@ buf ENCODED+ loop ;

\ The client's id, and its secret unless it is a public client.
: CREDENTIALS+ ( ptr n -- )
   {: buf:ptr :}
   s" client_id" CLIENT-ID @ SPAN:$ buf PAIR+
   SECRET @ SPAN:LEN 0= if exit then
   s" client_secret" SECRET @ SPAN:$ buf PAIR+ ;

\ A text TOKEN or REFRESH reads must not lie in the BUF it writes.
: OUTSIDE ( ptr u8 n ptr n -- ) BUF:BORROWED? if E-BUF-BOUNDS throw then ;

\ Room for a form within TOKEN-CAP: its secret never sits in storage a grow releases.
: EMPTIED ( ptr n -- ) {: buf:ptr :} buf BUF:CLEAR buf TOKEN-CAP BUF:N>BLEN BUF:ENSURE ;

\ The form TOKEN posts, written into the BUF: client credentials (RFC 6749
\ 4.4.2) or On-Behalf-Of (RFC 7523 2.1 with Entra's requested_token_use).
: FORM ( ptr u8 n grant ptr n -- )
   {: scope:ptr su:n g buf:ptr :}
   scope su buf OUTSIDE
   g MATCH grant
      own OF
         buf EMPTIED
         s" grant_type" s" client_credentials" buf PAIR+
         buf CREDENTIALS+
         s" scope" scope su buf PAIR+ ENDOF
      for-person OF {: a:ptr au:n :}
         a au buf OUTSIDE
         buf EMPTIED
         s" grant_type" s" urn:ietf:params:oauth:grant-type:jwt-bearer" buf PAIR+
         buf CREDENTIALS+
         s" assertion" a au buf PAIR+
         s" scope" scope su buf PAIR+
         s" requested_token_use" s" on_behalf_of" buf PAIR+ ENDOF
   ;MATCH ;

\ The form REFRESH posts, written into the BUF (RFC 6749 6).
: REFRESH-FORM ( ptr u8 n ptr u8 n ptr n -- )
   {: scope:ptr su:n r:ptr ru:n buf:ptr :}
   scope su buf OUTSIDE
   r ru buf OUTSIDE
   buf EMPTIED
   s" grant_type" s" refresh_token" buf PAIR+
   buf CREDENTIALS+
   s" refresh_token" r ru buf PAIR+
   s" scope" scope su buf PAIR+ ;

: WIPE ( ptr n -- )
   {: buf:ptr :}
   0 buf BUF:SPAN SPAN:FILL
   buf BUF:CLEAR ;

: OPEN-LINE ( -- )  LOG:ERROR s" bearer" LOG:LINE ;

: CURL-FAULT ( CURL:code -- )
   {: rc:CURL:code :}
   OPEN-LINE
   s" token request failed: curl " LOG:TEXT+  rc CURL:CODE>N LOG:INT+
   s" : " LOG:TEXT+  rc CURL:ERROR$ LOG:TEXT+
   LOG:;LINE ;

\ Whether the setting took; logs one that did not.
: SET? ( CURL:status -- bool )
   MATCH CURL:status
      ok OF true ENDOF
      failed OF CURL-FAULT false ENDOF
   ;MATCH ;

\ The BUF's form set as the request's body, then wiped from the BUF since the
\ secret is in it; whether every setting took, logging the first that did not.
: REQUESTED? ( CURL:handle ptr n -- bool )
   {: h:CURL:handle buf:ptr :}
   h buf BUF:SPAN$ BUF:BLEN>N CURL:BODY!
   buf WIPE
   SET? 0= if false exit then
   h ENDPOINT @ SPAN:$ CURL:URL! SET? 0= if false exit then
   h s" Content-Type: application/x-www-form-urlencoded" CURL:HEADER+ SET?
   0= if false exit then
   h s" Accept: application/json" CURL:HEADER+ SET? 0= if false exit then
   h TOKEN-TIMEOUT-MS >MS CURL:TIMEOUT! SET? ;

\ No answer, typed: EXCHANGE's catch drops it after a throw, where a bare
\ OPTION:NONE is E-REJECTED at the 2drop (rc 70).
: UNANSWERED ( -- option<CURL:http-status> ) OPTION:NONE ;

\ The error TOKEN and REFRESH throw for an answer they cannot use, by the
\ answer's status.
: STATUS-ERROR ( CURL:http-status -- n )
   {: status:CURL:http-status :}
   status CURL:UNAVAILABLE? if E-UNAVAILABLE exit then
   status CURL:HTTP-STATUS>N {: code:n :}
   code 400 >= code 500 < and if E-REFUSED exit then
   E-RESPONSE ;

\ The answer read into the BUF, which then holds exactly its bytes: its HTTP
\ status, none for a failed transfer, which it logs. An answer past TOKEN-CAP
\ is logged and throws its status's STATUS-ERROR.
: RECEIVED ( CURL:handle ptr n -- option<CURL:http-status> )
   {: h:CURL:handle buf:ptr :}
   buf TOKEN-CAP BUF:N>BLEN BUF:GROW {: room :}
   h room SPAN:$ >LEN CURL:PERFORM
   buf BUF:CLEAR
   MATCH CURL:fetch-result
      response OF {: status:CURL:http-status got:len :}
         room SPAN:$ drop got LEN>N BUF:N>BLEN buf BUF:APPEND-SPAN
         status OPTION:SOME ENDOF
      truncated OF
         drop {: status:CURL:http-status :}
         OPEN-LINE
         s" token answer " LOG:TEXT+  status CURL:HTTP-STATUS>N LOG:INT+
         s"  past " LOG:TEXT+  TOKEN-CAP LOG:INT+  s"  bytes" LOG:TEXT+
         LOG:;LINE
         status STATUS-ERROR throw ENDOF
      failed OF CURL-FAULT UNANSWERED ENDOF
   ;MATCH ;

: POST ( CURL:handle ptr n -- option<CURL:http-status> )
   {: h:CURL:handle buf:ptr :}
   h buf REQUESTED? if h buf RECEIVED else UNANSWERED then ;

\ POST's status in place of the third cell, the handle and BUF kept for catch.
: POSTED ( CURL:handle ptr n option<CURL:http-status> -- CURL:handle ptr n option<CURL:http-status> )
   drop {: h:CURL:handle buf:ptr :} h buf h buf POST ;

\ The form posted and the answer read into the BUF: its HTTP status, none when
\ there is no answer, which is logged. The handle is cleaned up on every exit,
\ and a throw wipes the BUF too.
: EXCHANGE ( ptr n -- option<CURL:http-status> )
   {: buf:ptr :}
   CURL:INIT
   MATCH CURL:init-result
      ready OF {: h:CURL:handle :}
         h buf UNANSWERED [: POSTED ;] catch {: code:n :}
         code 0<> if 2drop drop h CURL:CLEANUP buf WIPE code throw then
         nip nip h CURL:CLEANUP ENDOF
      failed OF CURL-FAULT buf WIPE UNANSWERED ENDOF
   ;MATCH ;

\ Under ANSWER-LOCK: the answer's reader on a JSON object text at a member's
\ value, and its token kind, -1 when the text is no object or lacks the member;
\ the caller reads and closes it.
: ANSWER-MEMBER ( SPAN:span<u8> ptr u8 n -- JSON:reader n )
   {: text k:ptr ku:n :}
   ANSWER-CURSOR text SPAN:$ JSON:READ-INIT
   JSON:READ-NEXT JSON:T-OBJ <> if -1 exit then
   k ku JSON:READ-FIND-KEY 0= if -1 exit then
   JSON:READ-TOKEN ;

\ Under ANSWER-LOCK: the access_token string of a JSON object text decoded
\ into room, its length; -1 when the text is no such object.
: ACCESS-TOKEN ( SPAN:span<u8> SPAN:span<u8> -- n )
   {: text room :}
   text s" access_token" ANSWER-MEMBER JSON:T-STR <> if JSON:READ-CLOSE -1 exit then
   room SPAN:$ JSON:READ-STR {: got:n :} JSON:READ-CLOSE got ;

\ Under ANSWER-LOCK: the positive integer expires_in of a JSON object text, -1
\ when the text is no such object.
: LIFETIME ( SPAN:span<u8> -- n )
   s" expires_in" ANSWER-MEMBER JSON:T-INT <> if JSON:READ-CLOSE -1 exit then
   JSON:READ-INT {: secs:n :} JSON:READ-CLOSE
   secs 0 > if secs else -1 then ;

\ Under ANSWER-LOCK: the refresh_token string of a JSON object text decoded
\ into room, its length; 0 when the text lacks it, -1 when it is no string.
: RENEWAL ( SPAN:span<u8> SPAN:span<u8> -- n )
   {: text room :}
   text s" refresh_token" ANSWER-MEMBER {: kind:n :}
   kind -1 = if JSON:READ-CLOSE 0 exit then
   kind JSON:T-STR <> if JSON:READ-CLOSE -1 exit then
   room SPAN:$ JSON:READ-STR {: got:n :} JSON:READ-CLOSE got ;

\ ACCESS-TOKEN's, LIFETIME's and RENEWAL's answers in place of the last three
\ cells, the texts and flag kept for catch. RENEWAL decodes into the room past
\ the access token, and only when renewing after an access token: else 0.
: KEPT ( SPAN:span<u8> SPAN:span<u8> bool n n n -- SPAN:span<u8> SPAN:span<u8> bool n n n )
   drop 2drop {: text room renew:bool :}
   text room renew
   text room ACCESS-TOKEN {: got:n :}
   got text LIFETIME
   renew got 0 >= and if text room got SPAN:SKIP RENEWAL else 0 then ;

\ Under ANSWER-LOCK: KEPT's three answers, each -1 for malformed JSON.
: PARSED ( SPAN:span<u8> SPAN:span<u8> bool -- n n n )
   -1 -1 -1 [: KEPT ;] catch {: code:n :}
   code 0= if {: text room renew:bool got:n secs:n kept:n :} got secs kept exit then
   2drop 2drop 2drop
   code MALFORMED? if -1 -1 -1 exit then
   code throw ;

: PARSING ( R [ R -- S ] -- S )
   ANSWER-LOCK TASK:GET [: ANSWER-LOCK TASK:RELEASE ;] finally ;

\ A 2xx answer lacking a member, logged without the answer, which can hold a
\ token.
: LACKS ( n ptr u8 n -- )
   {: status:n what:ptr wu:n :}
   OPEN-LINE s" token answer " LOG:TEXT+  status LOG:INT+
   s"  lacks " LOG:TEXT+  what wu LOG:TEXT+  LOG:;LINE
   E-RESPONSE throw ;

\ The 2xx answer's access_token and, when renewing, its refresh_token, empty
\ when absent, decoded into the BUF past the answer's bytes, the refresh token
\ right after the access token, and its expires_in. They are disjoint parts of
\ the answer and a decoded string never outgrows its text, so both fit.
: ISSUED ( n ptr n bool -- ptr u8 n ptr u8 n n )
   {: status:n buf:ptr renew:bool :}
   buf BUF:LEN@ {: u:NUM:byte-len :}
   buf u BUF:GROW {: room :}
   buf BUF:SPAN u BUF:BLEN>N SPAN:TAKE room renew [: PARSED ;] PARSING
   {: got:n secs:n kept:n :}
   got 0< if status s" an access_token string" LACKS then
   kept 0< if status s" a refresh_token string" LACKS then
   secs 0< if status s" a positive integer expires_in" LACKS then
   room SPAN:$ drop {: at:ptr :}
   at got at got + kept secs ;

\ The issuer's status and answer for a non-2xx.
: ANSWERED ( n ptr n -- )
   {: status:n buf:ptr :}
   OPEN-LINE s" token request answered " LOG:TEXT+  status LOG:INT+
   s" : " LOG:TEXT+  buf BUF:SPAN$ BUF:BLEN>N LOG:TEXT+  LOG:;LINE ;

\ E-HTTP-STATE before CLIENT!.
: STATED ( -- ) ENDPOINT @ SPAN:LEN 0= if E-HTTP-STATE throw then ;

\ The BUF's form posted, the BUF then holding the answer: its 2xx status;
\ any other outcome throws as TOKEN states.
: ASKED ( ptr n -- n )
   {: buf:ptr :}
   buf EXCHANGE MATCH option
      none OF E-TRANSFER throw ENDOF
      some OF ENDOF
   ;MATCH {: status:CURL:http-status :}
   status CURL:HTTP-STATUS>N {: code:n :}
   code 200 >= code 300 < and if code exit then
   code buf ANSWERED
   status STATUS-ERROR throw ;

public

\ An access token for the scope (for Entra a resource's `api://.../.default`)
\ from the endpoint CLIENT! names, borrowed from the BUF until the caller writes
\ to it again, then the answer's expires_in: the token's lifetime in seconds,
\ positive. E-TRANSFER for no answer; E-UNAVAILABLE for a status
\ CURL:UNAVAILABLE? accepts; E-REFUSED for another 4xx, the BUF then holding
\ the issuer's answer, empty past TOKEN-CAP; E-RESPONSE for a 2xx without an
\ access_token string or a positive integer expires_in, or any other status.
\ Each of those failures first logs one line tagged bearer, never a 2xx
\ answer's text. A scope or assertion lying in the BUF throws E-BUF-BOUNDS.
\ E-HTTP-STATE before CLIENT!.
: TOKEN ( ptr u8 n grant ptr n -- ptr u8 n n )
   {: scope:ptr su:n g buf:ptr :}
   STATED
   scope su g buf FORM
   buf ASKED buf false ISSUED {: a:ptr u:n r:ptr ru:n secs:n :}
   a u secs ;


\ A new access token for the scope from the endpoint CLIENT! names, by the
\ refresh grant with the refresh token (RFC 6749 6), then the answer's refresh
\ token, empty when it has none (the old one then stays the one to present),
\ then expires_in. Both texts are borrowed from the BUF until the caller writes
\ to it again. It fails as TOKEN does, and E-RESPONSE for a 2xx whose
\ refresh_token is not a string; a scope or refresh token lying in the BUF
\ throws E-BUF-BOUNDS.
: REFRESH ( ptr u8 n ptr u8 n ptr n -- ptr u8 n ptr u8 n n )
   {: scope:ptr su:n r:ptr ru:n buf:ptr :}
   STATED
   scope su r ru buf REFRESH-FORM
   buf ASKED buf true ISSUED ;

;package
