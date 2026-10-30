\ bearer.f - OAuth bearer tokens (RFC 6750) on Habu's HTTP server. A route
\ admits a request whose Authorization carries a compact JWS that one issuer
\ signed RS256 with a key of its JWK Set, whose iss and aud name that issuer
\ and this server, and whose exp and nbf hold (RFC 7515, 7517, 7519). The host
\ states the issuer with ISSUER! before HTTP:START, guards a route with
\ `req res BEARER:ADMIT? if ... then` and reads the caller's string claims with
\ CLAIM$. As a client, TOKEN gets a token from the token endpoint CLIENT! names,
\ for the client itself (RFC 6749 4.4) or for a person (Entra On-Behalf-Of).
\ STORAGE CLASS. PROCESS-WIDE: the issuer, written before HTTP serves; the key
\ set, read and replaced under KEYS-LOCK and fetched over CURL outside it by
\ one worker at a time; tables indexed by HTTP:WORKER-SLOT holding each
\ request's token and scratch; the client CLIENT! states, written before any
\ TOKEN; and the cursor that reads a token answer, under ANSWER-LOCK.
\ TASK-LOCAL: the admitted request, TASK:+USER rows.

require src/lib/string.f
require src/lib/span.f
require src/lib/base64.f
require src/lib/json-read.f
require src/lib/task.f
require src/lib/crypto/evp.f
require src/lib/net/curl.f
require src/lib/net/http.f

package BEARER
private

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
HTTP:MAX-WORKERS TYPED-BUFFER READER JR:cursor
HTTP:MAX-WORKERS TYPED-BUFFER OUTER JR:cursor       \ the key set's, while READER reads a key
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
: CURSOR ( -- ptr JR:cursor ) SLOT READER ;
: OUTER-CURSOR ( -- ptr JR:cursor ) SLOT OUTER ;
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
\ raises E-JR-STATE or JR:E-SOURCE, so they are BEARER's own misuse and pass.
: MALFORMED? ( n -- bool )
   {: code:n :}
   code E-JR-STATE = code JR:E-SOURCE = or if false exit then
   code E-JR-LAST >= code E-JR-FIRST <= and
   code E-BASE64-LAST >= code E-BASE64-FIRST <= and or ;

\ One step over untrusted text: malformed is false, any other throw passes.
: STEP ( [ bool -- bool ] -- bool )
   false swap catch {: ok:bool code:n :}
   code 0= if ok exit then
   code MALFORMED? if false exit then
   code throw ;


\ The worker's reader on an object text at a member's value, and its token
\ kind, -1 when the member is absent; the caller reads and closes it.
: MEMBER ( ptr u8 n ptr u8 n -- JR:reader n )
   {: t:ptr tu:n k:ptr ku:n :}
   CURSOR t tu JR:INIT JR:NEXT drop
   k ku JR:FIND-KEY 0= if -1 exit then
   JR:TOKEN ;

: KIND-OF ( ptr u8 n ptr u8 n -- n ) MEMBER {: kind:n :} JR:CLOSE kind ;

\ Whether an object text's member is a string decoding to the wanted bytes; an
\ absent one answers the given flag.
: MEMBER-IS? ( ptr u8 n ptr u8 n ptr u8 n bool -- bool )
   {: t:ptr tu:n k:ptr ku:n w:ptr wu:n absent:bool :}
   t tu k ku MEMBER {: kind:n :}
   kind -1 = if JR:CLOSE absent exit then
   kind JR:T-STR <> if JR:CLOSE false exit then
   w wu JR:STR-EQ? {: same:bool :} JR:CLOSE same ;

\ An object text's string member as sent, escapes encoded; empty when absent or
\ not a string.
: MEMBER$ ( ptr u8 n ptr u8 n -- ptr u8 n )
   MEMBER JR:T-STR <> if JR:CLOSE s" " exit then
   JR:SPAN$ {: a:ptr u:n :} JR:CLOSE a u ;

\ An object text's integer member; false when absent or not an integer.
: MEMBER-INT ( ptr u8 n ptr u8 n -- n bool )
   MEMBER JR:T-INT <> if JR:CLOSE 0 false exit then
   JR:INT {: v:n :} JR:CLOSE v true ;

\ An object text's string member decoded into the worker's note; false when
\ absent or not a string.
: NOTE$ ( ptr u8 n ptr u8 n -- ptr u8 n bool )
   NOTE-OFF HTTP:HEAD-CAP ROOM {: note :}
   MEMBER JR:T-STR <> if JR:CLOSE s" " false exit then
   note SPAN:$ JR:STR {: got:n :} JR:CLOSE
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
   CURSOR text SPAN:$ JR:INIT
   JR:NEXT JR:T-OBJ = if JR:FINISH true exit then
   JR:CLOSE false ;

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
   HEADER$ JR:REPEATS? if false exit then
   CLAIMS$ JR:REPEATS? if false exit then
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
: KEY-ELEMENT ( JR:reader -- JR:reader n )
   JR:NEXT {: kind:n :}
   kind JR:T-ARR-END = if -1 exit then
   kind JR:T-OBJ = if JR:VALUE-SPAN$ PICK? if 1 else 0 then exit then
   kind JR:T-ARR = if JR:VALUE-SPAN$ 2drop then
   0 ;

\ The token's key from the cached set, decoded into the worker's scratch.
: SELECT? ( -- bool )
   KEYS-U @ 0= if false exit then
   OUTER-CURSOR KEYS KEYS-U @ JR:INIT
   JR:NEXT drop s" keys" JR:FIND-KEY drop
   begin KEY-ELEMENT dup 0= while drop repeat
   {: found:n :} JR:CLOSE found 0 > ;


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
   OUTER-CURSOR FETCHED FETCHED-U @ JR:INIT
   JR:NEXT JR:T-OBJ <> if JR:CLOSE false exit then
   s" keys" JR:FIND-KEY 0= if JR:CLOSE false exit then
   JR:TOKEN JR:T-ARR <> if JR:CLOSE false exit then
   JR:FINISH true ;

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
: AUDIENCE-ELEMENT ( JR:reader -- JR:reader n )
   JR:NEXT {: kind:n :}
   kind JR:T-ARR-END = if -1 exit then
   kind JR:T-STR = if AUDIENCE @ SPAN:$ JR:STR-EQ? if 1 else 0 then exit then
   kind JR:T-OBJ = kind JR:T-ARR = or if JR:VALUE-SPAN$ 2drop then
   0 ;

\ aud is the audience or an array holding it.
: AUDIENCE? ( -- bool )
   CLAIMS$ s" aud" MEMBER {: kind:n :}
   kind JR:T-STR = if
      AUDIENCE @ SPAN:$ JR:STR-EQ? {: same:bool :} JR:CLOSE same exit
   then
   kind JR:T-ARR <> if JR:CLOSE false exit then
   begin AUDIENCE-ELEMENT dup 0= while drop repeat
   {: found:n :} JR:CLOSE found 0 > ;

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

: REFUSE ( HTTP:response n -- )
   {: answer:HTTP:response outcome:n :}
   outcome UNAVAILABLE = if answer 503 HTTP:STATUS! exit then
   outcome NO-TOKEN = if answer s" Bearer" CHALLENGE exit then
   answer S\" Bearer error=\"invalid_token\"" CHALLENGE ;

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


\ Whether the request carries a current bearer token of the issuer's for this
\ server. A refusal answers the response itself, with an empty body: 401
\ challenging Bearer for no, several or a non-Bearer Authorization, or Bearer
\ error="invalid_token" for a token that fails; 503 when there is no key set
\ and none could be fetched. A broken key of the issuer's passes its throw.
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
TYPED-VARIABLE ANSWER-CURSOR JR:cursor
TASK:FACILITY ANSWER-LOCK
ANSWER-LOCK TASK:FACILITY-INIT

public

\ How TOKEN asks: as the client itself (client credentials), or for the person
\ whose token is the assertion (Entra On-Behalf-Of).
ENUM grant
   VARIANT own ;VARIANT
   VARIANT for-person FIELD assertion ptr u8 FIELD len n ;VARIANT
;ENUM

-9630 constant E-TRANSFER           \ no answer from the token endpoint, a 5xx, or a 2xx without an access_token string
-9631 constant E-REFUSED            \ the issuer refused with a 4xx; the BUF holds its answer
private
-9630 constant E-FIRST
-9639 constant E-LAST
public

\ The token endpoint's URL, and the client id and secret this process presents
\ there, written before any TOKEN, not while a TOKEN runs. The texts are
\ borrowed and outlive every TOKEN, as literals do.
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

: CREDENTIALS+ ( ptr n -- )
   {: buf:ptr :}
   s" client_id" CLIENT-ID @ SPAN:$ buf PAIR+
   s" client_secret" SECRET @ SPAN:$ buf PAIR+ ;

\ A text TOKEN reads must not lie in the BUF it writes.
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

: WIPE ( ptr n -- )
   {: buf:ptr :}
   0 buf BUF:SPAN SPAN:FILL
   buf BUF:CLEAR ;

\ The BUF's form set as the request's body, then wiped from the BUF since the
\ secret is in it; whether every setting took.
: REQUESTED? ( CURL:handle ptr n -- bool )
   {: h:CURL:handle buf:ptr :}
   h buf BUF:SPAN$ BUF:BLEN>N CURL:BODY! OK?
   buf WIPE
   h ENDPOINT @ SPAN:$ CURL:URL! OK? and
   h s" Content-Type: application/x-www-form-urlencoded" CURL:HEADER+ OK? and
   h s" Accept: application/json" CURL:HEADER+ OK? and
   h TOKEN-TIMEOUT-MS >MS CURL:TIMEOUT! OK? and ;

\ The answer read into the BUF, which then holds exactly its bytes: its HTTP
\ status, -1 for a failed transfer or one past TOKEN-CAP.
: RECEIVED ( CURL:handle ptr n -- n )
   {: h:CURL:handle buf:ptr :}
   buf TOKEN-CAP BUF:N>BLEN BUF:GROW {: room :}
   h room SPAN:$ >LEN CURL:PERFORM
   buf BUF:CLEAR
   MATCH CURL:fetch-result
      response OF {: status:CURL:http-status got:len :}
         room SPAN:$ drop got LEN>N BUF:N>BLEN buf BUF:APPEND-SPAN
         status CURL:HTTP-STATUS>N ENDOF
      truncated OF drop drop -1 ENDOF
      failed OF drop -1 ENDOF
   ;MATCH ;

: POST ( CURL:handle ptr n -- n )
   {: h:CURL:handle buf:ptr :}
   h buf REQUESTED? if h buf RECEIVED else -1 then ;

\ POST's status in place of the third cell, the handle and BUF kept for catch.
: POSTED ( CURL:handle ptr n n -- CURL:handle ptr n n )
   drop {: h:CURL:handle buf:ptr :} h buf h buf POST ;

\ The form posted and the answer read into the BUF: its HTTP status, -1 when
\ there is none. The handle is cleaned up on every exit, and a throw wipes the
\ BUF too.
: EXCHANGE ( ptr n -- n )
   {: buf:ptr :}
   CURL:INIT
   MATCH CURL:init-result
      ready OF {: h:CURL:handle :}
         h buf -1 [: POSTED ;] catch {: code:n :}
         code 0<> if 2drop drop h CURL:CLEANUP buf WIPE code throw then
         nip nip h CURL:CLEANUP ENDOF
      failed OF drop buf WIPE -1 ENDOF
   ;MATCH ;

\ Under ANSWER-LOCK: the access_token string of a JSON object text decoded
\ into room, its length; -1 when the text is no such object.
: ACCESS-TOKEN ( SPAN:span<u8> SPAN:span<u8> -- n )
   {: text room :}
   ANSWER-CURSOR text SPAN:$ JR:INIT
   JR:NEXT JR:T-OBJ <> if JR:CLOSE -1 exit then
   s" access_token" JR:FIND-KEY 0= if JR:CLOSE -1 exit then
   JR:TOKEN JR:T-STR <> if JR:CLOSE -1 exit then
   room SPAN:$ JR:STR {: got:n :} JR:CLOSE got ;

\ ACCESS-TOKEN's answer in place of the third cell, the texts kept for catch.
: KEPT ( SPAN:span<u8> SPAN:span<u8> n -- SPAN:span<u8> SPAN:span<u8> n )
   drop {: text room :} text room text room ACCESS-TOKEN ;

\ Under ANSWER-LOCK: ACCESS-TOKEN, and -1 for malformed JSON too.
: PARSED ( SPAN:span<u8> SPAN:span<u8> -- n )
   -1 [: KEPT ;] catch {: code:n :}
   code 0= if {: text room got:n :} got exit then
   2drop drop
   code MALFORMED? if -1 exit then
   code throw ;

: PARSING ( R [ R -- S ] -- S )
   ANSWER-LOCK TASK:GET [: ANSWER-LOCK TASK:RELEASE ;] finally ;

\ The 2xx answer's access_token decoded into the BUF past the answer's bytes,
\ which a decoded string never outgrows. E-TRANSFER for an answer without one.
: ISSUED ( ptr n -- ptr u8 n )
   {: buf:ptr :}
   buf BUF:LEN@ {: u:NUM:byte-len :}
   buf u BUF:GROW {: room :}
   buf BUF:SPAN u BUF:BLEN>N SPAN:TAKE room [: PARSED ;] PARSING {: got:n :}
   got 0< if E-TRANSFER throw then
   room SPAN:$ drop got ;

public

\ An access token for the scope (for Entra a resource's `api://.../.default`)
\ from the endpoint CLIENT! names, borrowed from the BUF until the caller writes
\ to it again. E-TRANSFER for no answer, a 5xx, or a 2xx without an
\ access_token string; E-REFUSED for a 4xx, the BUF then holding the issuer's
\ answer. A scope or assertion lying in the BUF throws E-BUF-BOUNDS.
\ E-HTTP-STATE before CLIENT!.
: TOKEN ( ptr u8 n grant ptr n -- ptr u8 n )
   {: scope:ptr su:n g buf:ptr :}
   ENDPOINT @ SPAN:LEN 0= if E-HTTP-STATE throw then
   scope su g buf FORM
   buf EXCHANGE {: status:n :}
   status 200 >= status 300 < and if buf ISSUED exit then
   status 400 >= status 500 < and if E-REFUSED throw then
   E-TRANSFER throw ;

;package
