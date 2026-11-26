\ web-session.f - a person's browser signed in to Habu's HTTP server through
\ the OpenID Connect authorization code flow (RFC 6749 4.1, PKCE by RFC 7636,
\ OpenID Connect Core 3.1), held by a session cookie with the __Host- prefix
\ (RFC 6265bis 4.1.3.2). The host states BEARER:ISSUER! and BEARER:CLIENT! for
\ a confidential client, fills a config and calls START before HTTP:START, and
\ guards a route with `req res v WEB-SESSION:ADMIT? if ... then`, or a page with
\ ADMIT-PAGE?.
\ config: authorize, the issuer's authorization endpoint URL; redirect, the
\ URI the issuer sends the browser back to, an origin and a path, which this
\ server routes; scope, the scope of access asked for beside `openid
\ offline_access`, empty for none; idle, how long a session may go unused.
\ START ( ^config -- ) checks the texts as BEARER checks its own, adds the
\ GET route of the redirect URI's path, the callback, then drops every
\ session. The config's texts are borrowed and outlive the server, as
\ literals do. E-HTTP-STATE while HTTP serves or before BEARER:ISSUER! and
\ CLIENT!; BEARER:E-OPERAND, or URI:ORIGIN's rejection, for an authorize URL
\ or a redirect URI neither https nor http to a loopback IP literal or
\ holding a byte outside $21..$7E, a redirect URI holding a query, fragment
\ or brace, an authorize URL holding a fragment, a scope that is not RFC 6749
\ 3.3 scope tokens one space apart, or an idle under 1 ms; each rejection's
\ REASON:TEXT$ names the setting, the value and the fix; HTTP:E-ROUTE for a
\ path a GET route already covers, a second START's on the same path included.
\ Each rejection leaves the previous config and its sessions in force; after
\ HTTP:ROUTES-RESET a START adds the callback again.
\ ADMIT? ( HTTP:request HTTP:response ^VEC:vector<u8> -- bool ): true leaves
\ the person's current access token as the vector's whole content; false has
\ answered 403 origin_rejected for a request whose Origin is not the redirect
\ URI's origin, 401 signed_out for one with no session, or 503 when a stale
\ session's renewal could not reach the issuer or read its answer, or is
\ another guard's and its token expired, and leaves the vector empty.
\ ADMIT-PAGE? ( same -- bool ) guards a page alike, answering in place of
\ the 401 the 302 that sends the browser to the issuer to sign in.
\ END ( HTTP:request HTTP:response -- ) drops the cookie's session and adds
\ the Set-Cookie that clears the cookie; status and body stay the route's.
\ All three run on an HTTP worker.
\ A sign-in: the 302 names a fresh verifier's S256 challenge as nonce and
\ code_challenge, and as state the challenge, `.` and the page's path and query
\ in base64url, and sets the cookie `__Host-signin-<challenge>` holding the
\ verifier for 600 seconds. The callback takes a state only from the browser
\ holding the cookie of its challenge whose verifier hashes to it, else 400
\ no_sign_in. With the issuer's error it clears the cookie and answers 403
\ sign_in_rejected after one log line; with a code it redeems it
\ (BEARER:REDEEM), and makes a session, sets `__Host-session=<id>` and sends the
\ browser back to the page on success, answers 400 sign_in_failed for E-REJECTED
\ and 502 for E-RESPONSE or a token already stale with no refresh token, each
\ clearing the cookie, and 503 for E-TRANSFER and E-UNAVAILABLE, keeping it. A
\ target that does not start with `/` or holds a byte outside $21..$7E, or whose
\ state would pass an eighth of a request head (HTTP:HEAD-CAP), is `/`. A new
\ attempt clears all but the newest three it was sent, so a browser that waits
\ for each page holds at most four; pages loaded at once each add one of 101
\ bytes.
\ A session holds the SHA-256 of its id, never the id, the access and refresh
\ tokens, since, the epoch second before the ask, life, expires_in less
\ BEARER's LEEWAY-S, used, a mono-ns, and renewal, settled or asking. Stale
\ is epoch-seconds >= since + life. The guard that finds a stale session
\ settled makes it asking and renews it by its refresh token (BEARER:REFRESH)
\ holding no lock: success replaces the tokens, the refresh token when one is
\ answered, and admits; E-REJECTED drops the session, answered as none;
\ E-TRANSFER, E-UNAVAILABLE and E-RESPONSE answer 503 and keep it; however
\ the ask ends, the session, when still held, is settled again. A guard that
\ finds a stale session asking admits by its access token until since +
\ expires_in, then answers 503. A stale session with no refresh token is
\ dropped. A session unused longer than idle is dropped by the guard that
\ finds it, and every such session at each callback that makes one; an
\ asking session is dropped alike.
\ Every header value written is base64url, a checked setting, a target the
\ rule above admits or a query value URI:QUERY-ENCODE wrote.
\ STORAGE CLASS. PROCESS-WIDE: the config's borrowed texts and SCOPE-ASKED,
\ the scope composed from them in a vector the first START makes and the
\ process keeps, written before HTTP serves; the sessions, a heap list read
\ and changed under TABLE-LOCK, each session's renewal included, no pointer to
\ one kept across its release; and rows indexed by HTTP:WORKER-SLOT, a scratch
\ region and three vectors whose slot's first use makes them and the process
\ keeps: a text composed or read, an ask's answer and the refresh token
\ presented, the last two cut to 0 however the ask ends.

require src/lib/string.f
require src/lib/span.f
require src/lib/memory.f
require src/lib/base64.f
require src/lib/log.f
require src/lib/task.f
require src/lib/allocator.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/reason.f
require src/lib/fmt.f
require src/lib/entropy.f
require src/lib/uri.f
require src/lib/net/http.f
require src/lib/net/http-arena.f
require src/lib/net/http-request.f
require src/lib/net/http-response.f
require src/lib/net/http-router.f
require src/lib/net/bearer.f

package WEB-SESSION
private
EXPORT BEARER:SECURE
EXPORT BEARER:SCOPE-TOKENS
EXPORT BEARER:REDEEMABLE
EXPORT BEARER:LEEWAY-S
EXPORT BEARER:CLIENT-ID
EXPORT BEARER:SETTING
EXPORT BEARER:FOUND-BYTE
EXPORT HTTP:COOKIE-NEXT

$20 constant RAW-BYTES              \ a secret's random bytes; a SHA-256 digest
43 constant SECRET-LEN              \ RAW-BYTES in unpadded base64url
48 constant SECRET-ROOM             \ SECRET-LEN rounded up to cells
1000000 constant NS-PER-MS
$7B constant LBRACE
$7D constant RBRACE

public

STRUCTURE config DERIVE addr
   FIELD authorize str
   FIELD redirect str
   FIELD scope str
   FIELD idle ms
;STRUCTURE

private

TYPED-VARIABLE SETTINGS config SCRATCH-ONLY

: AUTHORIZE$ ( -- str ) SETTINGS WEB--SESSION-CONFIG:AUTHORIZE @ ;
: BACK$ ( -- str ) SETTINGS WEB--SESSION-CONFIG:REDIRECT @ ;

\ The scope a sign-in and a refresh ask for.
TYPED-VARIABLE SCOPE-ASKED ^VEC:vector<u8> SCRATCH-ONLY

: SIGNIN-PREFIX ( -- str ) s" __Host-signin-" ;
: SESSION-NAME ( -- str ) s" __Host-session" ;
: LAX ( -- str ) s" ; Secure; HttpOnly; SameSite=Lax" ;

\ A sign-in's state: the challenge, `.` and the target.
HTTP:HEAD-CAP 8 / constant STATE-MOST

ENUM renewal settled asking ;ENUM

STRUCTURE session DERIVE addr
   FIELD next ^session
   BUFFER digest RAW-BYTES
   FIELD access ^VEC:vector<u8>
   FIELD refresh ^VEC:vector<u8>
   FIELD since n
   FIELD life n
   FIELD used n                     \ a mono-ns
   FIELD renewal renewal
;STRUCTURE

TYPED-VARIABLE NEWEST ^session SCRATCH-ONLY

\ A look at the table for a session: its token given, none, held stale and now
\ this guard's to renew, or held stale, another guard's to renew, expired.
ENUM found fresh absent stale renewing ;ENUM

\ A guard's answer: the session's token given; none held; the request's
\ Origin rejected; or a stale session's renewal not done.
ENUM guard admitted absent rejected unavailable ;ENUM

\ Held to find, link, drop or read a session, never across an ask.
TASK:FACILITY TABLE-LOCK
TABLE-LOCK TASK:FACILITY-INIT

: TABLED ( R [ R -- S ] -- S )
   TABLE-LOCK TASK:GET [: TABLE-LOCK TASK:RELEASE ;] finally ;

\ A worker's scratch: SHA-256's context, a digest, random bytes, a secret
\ (a verifier or a session id), the state's challenge and one worked out.
0 constant CTX-OFF
SHA256-CTX-BYTES constant DIGEST-OFF
DIGEST-OFF RAW-BYTES + constant RAW-OFF
RAW-OFF RAW-BYTES + constant SECRET-OFF
SECRET-OFF SECRET-ROOM + constant CHALLENGE-OFF
CHALLENGE-OFF SECRET-ROOM + constant HASHED-OFF
HASHED-OFF SECRET-ROOM + constant SCRATCH-BYTES
HTTP:MAX-WORKERS SCRATCH-BYTES * constant SCRATCHES-BYTES
create SCRATCH-ROWS SCRATCHES-BYTES SCRATCH-ALLOT

: SLOT ( -- n ) HTTP:WORKER-SLOT ;

: ROOM ( n n -- SPAN:span<u8> )
   {: off:n len:n :}
   SCRATCH-ROWS SCRATCHES-BYTES SPAN:MAKE
   SLOT SCRATCH-BYTES * off + len SPAN:SUB ;

: ROOM$ ( n n -- str ) ROOM SPAN:$ ;

: SECRET$ ( -- str ) SECRET-OFF SECRET-LEN ROOM$ ;
: CHALLENGE$ ( -- str ) CHALLENGE-OFF SECRET-LEN ROOM$ ;
: DIGEST ( -- SPAN:span<u8> ) DIGEST-OFF RAW-BYTES ROOM ;

: EMPTY ( -- ^VEC:vector<u8> ) HEAP:ALLOCATOR 0 [type] u8 VEC:NEW ;

: MINE ( ^^VEC:vector<u8> -- ^VEC:vector<u8> )
   {: cell:^ :}
   cell @ NULL-PTR = if EMPTY cell ! then
   cell @ ;

\ A worker's vector, for a text it composes or reads from the request.
HTTP:MAX-WORKERS TYPED-BUFFER NOTES ^VEC:vector<u8> SCRATCH-ONLY

: NOTE ( -- ^VEC:vector<u8> ) SLOT NOTES MINE dup 0 VEC:RESIZE ;

\ A worker's vectors for an ask: the issuer's answer, and the copy of the
\ refresh token presented.
HTTP:MAX-WORKERS TYPED-BUFFER ANSWERS ^VEC:vector<u8> SCRATCH-ONLY
HTTP:MAX-WORKERS TYPED-BUFFER RENEWALS ^VEC:vector<u8> SCRATCH-ONLY

: ANSWER ( -- ^VEC:vector<u8> ) SLOT ANSWERS MINE ;
: RENEWAL ( -- ^VEC:vector<u8> ) SLOT RENEWALS MINE ;

\ The cell's eight bytes, low first.
: CELL-BYTES! ( n ^u8 -- )
   {: v:n at :}
   8 0 ?do v i 8 * rshift $FF and at i + c! loop ;

\ RAW-BYTES random bytes in unpadded base64url, into the secret's room.
: DRAWN ( -- str )
   RAW-OFF RAW-BYTES ROOM {: raw :}
   RAW-BYTES 8 / 0 ?do
      ENTROPY:RANDOM raw SPAN:$ STR:DATA i 8 * + CELL-BYTES!
   loop
   raw SPAN:$ SECRET-OFF SECRET-LEN ROOM BASE64:ENCODE-URL drop
   SECRET$ ;

\ The text's SHA-256, in the digest's room.
: HASHED ( str -- SPAN:span<u8> )
   {: t:str :}
   CTX-OFF SHA256-CTX-BYTES ROOM$ STR:DATA t DIGEST SPAN:$ STR:DATA SHA256-IN
   DIGEST ;

\ The text's S256 challenge (RFC 7636 4.2) into the room at the offset.
: CHALLENGE ( str n -- str )
   {: t:str off:n :}
   t HASHED SPAN:$ off SECRET-LEN ROOM BASE64:ENCODE-URL drop
   off SECRET-LEN ROOM$ ;

: URL-BYTE? ( n -- bool )
   {: c:n :}
   c [char] A >= c [char] Z <= and
   c [char] a >= c [char] z <= and or
   c [char] 0 >= c [char] 9 <= and or
   c [char] - = or c [char] _ = or ;

: URL-SAFE? ( str -- bool )
   {: s:str :}
   s STR:LEN 0 ?do
      s STR:DATA i + c@ URL-BYTE? 0= if false unloop exit then
   loop true ;

\ A verifier, a challenge or a session id as this module draws them.
: SECRET? ( str -- bool )
   {: s:str :}
   s STR:LEN SECRET-LEN <> if false exit then
   s URL-SAFE? ;

\ Unpadded base64url's length for n bytes, and the bytes of n characters.
: URL-LEN ( n -- n ) 4 * 2 + 3 / ;

: BYTES-LEN ( n -- n )
   {: u:n :}
   u 3 and {: k:n :}
   u 4 / 3 * k 0<> if k 1- + then ;

\ A target a sign-in returns to: `/`, then bytes $21 to $7E alone.
: TARGET? ( str -- bool )
   {: t:str :}
   t STR:LEN 0= if false exit then
   t STR:DATA c@ [char] / <> if false exit then
   t STR:LEN 0 ?do
      t STR:DATA i + c@ {: c:n :}
      c $21 < c $7E > or if false unloop exit then
   loop true ;

\ Where the redirect URI's path starts: its origin's length.
: ORIGIN-LEN ( -- n )
   BACK$ {: back:str :}
   back URI:REQUEST-TARGET$ STR:DATA back STR:DATA - ;

\ The redirect URI's path, which the callback is routed on; RFC 9112 sends
\ an empty one as `/`.
: CALLBACK-PATH ( str -- str )
   URI:REQUEST-TARGET$ {: p:str :}
   p STR:LEN 0= if s" /" exit then
   p ;

\ Under TABLE-LOCK.
: LINKED ( ^session -- )
   {: row:^ :}
   NEWEST @ row SESSION-NEXT !
   row NEWEST ! ;

\ Frees a session whose vectors are made or null, wiping its tokens.
: FREED ( ^session -- )
   {: row:^ :}
   row SESSION-ACCESS @ dup NULL-PTR <> if dup 0 VEC:RESIZE VEC:DELETE
   else drop then
   row SESSION-REFRESH @ dup NULL-PTR <> if dup 0 VEC:RESIZE VEC:DELETE
   else drop then
   HEAP:ALLOCATOR row ALLOC:DELETE ;

\ Under TABLE-LOCK: the session the cell links taken out and freed.
: UNLINKED ( ^^session -- )
   {: link:^ :}
   link @ {: row:^ :}
   row SESSION-NEXT @ link !
   row FREED ;

\ Under TABLE-LOCK: the cell linking the session of the digest, holding null
\ when none has it.
: LINK-OF ( SPAN:span<u8> -- ^^session )
   {: d :}
   NEWEST
   begin dup @ NULL-PTR <> while
      dup @ SESSION-DIGEST SPAN:$ d SPAN:$ STR:= if exit then
      @ SESSION-NEXT
   repeat ;

: DROP-ALL ( -- )
   [: begin NEWEST @ NULL-PTR <> while NEWEST UNLINKED repeat ;] TABLED ;

: STALE? ( ^session -- bool )
   {: row:^ :}
   epoch-seconds row SESSION-SINCE @ - row SESSION-LIFE @ >= ;

\ Unused longer than idle, compared without idle × NS-PER-MS, which
\ overflows.
: IDLE? ( ^session -- bool )
   {: row:^ :}
   mono-ns row SESSION-USED @ - 1- NS-PER-MS /
   SETTINGS WEB--SESSION-CONFIG:IDLE @ MS>N >= ;

\ Under TABLE-LOCK: the session used now and its access token appended to
\ the vector.
: USED ( ^session ^VEC:vector<u8> -- )
   {: row:^ dest:^ :}
   mono-ns row SESSION-USED !
   row SESSION-ACCESS @ VEC:BYTES dest VEC:APPEND ;

\ Whether the access token is inside the expires_in it was answered with.
: UNEXPIRED? ( ^session -- bool )
   {: row:^ :}
   epoch-seconds row SESSION-SINCE @ - row SESSION-LIFE @ LEEWAY-S + < ;

\ Under TABLE-LOCK, a stale settled session the cell links: stale, its refresh
\ token copied into RENEWAL, then asking; absent, dropped, with none.
: CLAIMED ( ^^session -- found )
   {: link:^ :}
   link @ {: row:^ :}
   row SESSION-REFRESH @ VEC:COUNT 0= if
      link UNLINKED construct found absent exit
   then
   row SESSION-REFRESH @ VEC:BYTES RENEWAL VEC:APPEND
   construct renewal asking row SESSION-RENEWAL !
   construct found stale ;

\ Under TABLE-LOCK, a stale session another guard renews: fresh, used now,
\ while its token is unexpired; renewing after.
: WAITED ( ^session ^VEC:vector<u8> -- found )
   {: row:^ dest:^ :}
   row UNEXPIRED? 0= if construct found renewing exit then
   row dest USED
   construct found fresh ;

\ Under TABLE-LOCK, the session of the digest: fresh, used now; absent when
\ none is held or an idle one was dropped; a stale one as CLAIMED or WAITED.
: HELD ( SPAN:span<u8> ^VEC:vector<u8> -- found )
   {: d dest:^ :}
   d LINK-OF {: link:^ :}
   link @ NULL-PTR = if construct found absent exit then
   link @ IDLE? if link UNLINKED construct found absent exit then
   link @ STALE? 0= if link @ dest USED construct found fresh exit then
   link @ SESSION-RENEWAL @ MATCH renewal
      settled OF link CLAIMED ENDOF
      asking OF link @ dest WAITED ENDOF
   ;MATCH ;

\ The vector's content replaced by the text, the old bytes wiped.
: REPLACED ( str ^VEC:vector<u8> -- )
   {: t:str v:^ :}
   v 0 VEC:RESIZE
   t v VEC:APPEND ;

\ Under TABLE-LOCK: REFRESH's tokens and expires_in, asked at since, put in
\ the session of the digest, used now: admitted; absent when it is gone.
: RENEWED ( str str n n SPAN:span<u8> ^VEC:vector<u8> -- guard )
   {: a:str r:str secs:n since:n d dest:^ :}
   d LINK-OF @ {: row:^ :}
   row NULL-PTR = if construct guard absent exit then
   a row SESSION-ACCESS @ REPLACED
   r STR:LEN 0 > if r row SESSION-REFRESH @ REPLACED then
   since row SESSION-SINCE !
   secs LEEWAY-S - row SESSION-LIFE !
   row dest USED
   construct guard admitted ;

\ Under TABLE-LOCK: the session of the digest dropped when it is held.
: DROPPED ( SPAN:span<u8> -- )
   LINK-OF {: link:^ :}
   link @ NULL-PTR <> if link UNLINKED then ;

\ The scope asked for, raw.
: SCOPE$ ( -- str ) SCOPE-ASKED @ VEC:BYTES ;

\ RENEWAL presented for the digest's session: RENEWED's answer; absent,
\ the session dropped, or unavailable, the session kept, by REFRESH's code.
: REFRESHED ( SPAN:span<u8> ^VEC:vector<u8> -- guard )
   {: d dest:^ :}
   epoch-seconds {: since:n :}
   [: SCOPE$ RENEWAL VEC:BYTES ANSWER BEARER:REFRESH
      since d dest [: RENEWED ;] TABLED ;] catch {: code:n :}
   code 0= if exit then
   drop
   code BEARER:E-REJECTED = if
      d [: DROPPED ;] TABLED construct guard absent exit
   then
   code BEARER:E-TRANSFER = code BEARER:E-UNAVAILABLE = or
   code BEARER:E-RESPONSE = or if construct guard unavailable exit then
   code throw ;

\ Under TABLE-LOCK.
: SETTLED ( SPAN:span<u8> -- )
   LINK-OF @ {: row:^ :}
   row NULL-PTR <> if construct renewal settled row SESSION-RENEWAL ! then ;

\ REFRESHED's answer, holding no lock; however it ends, ANSWER and RENEWAL cut
\ to 0, then the session, when still held, settled.
: RENEWAL-OF ( SPAN:span<u8> ^VEC:vector<u8> -- guard )
   {: d dest:^ :}
   [: d dest REFRESHED ;]
   [: ANSWER 0 VEC:RESIZE  RENEWAL 0 VEC:RESIZE  d [: SETTLED ;] TABLED ;]
   finally ;

\ The session of the request's cookie, its token appended when admitted, a
\ stale one as HELD finds it; absent for no cookie, a bad shape or unknown id.
: ADMISSION ( HTTP:request ^VEC:vector<u8> -- guard )
   {: asked:HTTP:request dest:^ :}
   asked SESSION-NAME HTTP:COOKIE-OF$ 0= if
      drop construct guard absent exit
   then
   {: id:str :}
   id SECRET? 0= if construct guard absent exit then
   id HASHED {: d :}
   d dest [: HELD ;] TABLED
   MATCH found
      fresh OF construct guard admitted ENDOF
      absent OF construct guard absent ENDOF
      stale OF d dest RENEWAL-OF ENDOF
      renewing OF construct guard unavailable ENDOF
   ;MATCH ;

\ Under TABLE-LOCK: every idle session dropped.
: SWEPT ( -- )
   NEWEST
   begin dup @ NULL-PTR <> while
      dup @ IDLE? if dup UNLINKED else @ SESSION-NEXT then
   repeat drop ;

\ The session the request's cookie names dropped, if one is held.
: ENDED ( HTTP:request -- )
   {: asked:HTTP:request :}
   asked SESSION-NAME HTTP:COOKIE-OF$ 0= if drop exit then
   {: id:str :}
   id SECRET? 0= if exit then
   id HASHED {: d :}
   [: d DROPPED ;] TABLED ;

\ Whether the request's page, if it names one, is of the redirect URI's
\ origin. A browser sends one Origin; several name no one page.
: FROM-HERE? ( HTTP:request -- bool )
   {: asked:HTTP:request :}
   asked s" Origin" HTTP:HEADER-COUNT-OF {: count:n :}
   count 0= if true exit then
   count 1 > if false exit then
   asked s" Origin" HTTP:HEADER-OF$ drop {: o:str :}
   o URI:ORIGIN? 0= if false exit then
   o BACK$ URI:SAME-ORIGIN? ;

: ALLOWED? ( HTTP:request HTTP:response -- bool )
   {: asked:HTTP:request answer:HTTP:response :}
   asked FROM-HERE? if true exit then
   answer 403 s" origin_rejected" s" a page of this origin may not call"
   HTTP:ERROR! false ;

\ name=value after `&`, the value query-encoded.
: PARAM ( str str ^VEC:vector<u8> -- )
   {: k:str v:str buf:^ :}
   [char] & buf VEC:PUSH c!
   k buf VEC:APPEND
   [char] = buf VEC:PUSH c!
   v buf URI:QUERY-ENCODE ;

: SCOPE+ ( ^VEC:vector<u8> -- )
   {: buf:^ :}
   s" &scope=" buf VEC:APPEND
   SCOPE$ buf URI:QUERY-ENCODE ;

\ The request's path, then `?` and its query when it has one.
: TARGET+ ( HTTP:request ^VEC:vector<u8> -- )
   {: asked:HTTP:request buf:^ :}
   asked HTTP:PATH$ buf VEC:APPEND
   asked HTTP:QUERY$ {: q:str :}
   q STR:LEN 0 > if [char] ? buf VEC:PUSH c! q buf VEC:APPEND then ;

\ The target the request's page returns to, in the vector, `/` when the rule
\ rejects it.
: RETURN-TO ( HTTP:request ^VEC:vector<u8> -- )
   {: asked:HTTP:request buf:^ :}
   asked buf TARGET+
   buf VEC:COUNT URL-LEN SECRET-LEN + 1+ STATE-MOST > 0=
   buf VEC:BYTES TARGET? and if exit then
   buf 0 VEC:RESIZE  s" /" buf VEC:APPEND ;

\ The authorization request for the challenge and the request's page, in the
\ worker's vector (RFC 6749 4.1.1, RFC 7636 4.3, OpenID Connect Core 3.1.2.1).
: AUTHORIZE-URL ( HTTP:request str -- str )
   {: asked:HTTP:request c:str :}
   NOTE {: buf:^ :}
   asked buf RETURN-TO
   buf VEC:COUNT {: t:n :}
   AUTHORIZE$ {: url:str :}
   url buf VEC:APPEND
   url s" ?" STR:CONTAINS? if [char] & else [char] ? then buf VEC:PUSH c!
   s" response_type=code" buf VEC:APPEND
   s" client_id" CLIENT-ID @ SPAN:$ buf PARAM
   s" redirect_uri" BACK$ buf PARAM
   buf SCOPE+
   s" &state=" buf VEC:APPEND  c buf VEC:APPEND  [char] . buf VEC:PUSH c!
   buf t URL-LEN VEC:EXTEND {: room :}
   buf VEC:BYTES STR:DATA t >str room BASE64:ENCODE-URL drop
   s" nonce" c buf PARAM  s" code_challenge" c buf PARAM
   s" &code_challenge_method=S256" buf VEC:APPEND
   buf VEC:BYTES {: all:str :}
   all STR:DATA t + all STR:LEN t - >str ;

\ The challenge's attempt cookie, holding the verifier alone, in the worker's
\ vector.
: ATTEMPT-COOKIE ( str str -- str )
   {: v:str c:str :}
   NOTE {: buf:^ :}
   SIGNIN-PREFIX buf VEC:APPEND  c buf VEC:APPEND
   [char] = buf VEC:PUSH c!  v buf VEC:APPEND
   s" ; Path=/; Max-Age=600" buf VEC:APPEND  LAX buf VEC:APPEND
   buf VEC:BYTES ;

\ The Set-Cookie that clears the challenge's attempt cookie.
: CLEARED ( HTTP:response str -- )
   {: answer:HTTP:response c:str :}
   NOTE {: buf:^ :}
   SIGNIN-PREFIX buf VEC:APPEND  c buf VEC:APPEND
   s" =; Path=/; Max-Age=0" buf VEC:APPEND  LAX buf VEC:APPEND
   answer s" Set-Cookie" buf VEC:BYTES HTTP:HEADER! ;

\ How many attempt cookies the request's one Cookie header holds, the first n,
\ the oldest (RFC 6265 5.4), cleared; 0 without exactly one Cookie header.
: CLEARED-FIRST ( HTTP:request HTTP:response n -- n )
   {: asked:HTTP:request answer:HTTP:response most:n :}
   asked s" cookie" HTTP:HEADER-COUNT-OF 1 <> if 0 exit then
   asked s" cookie" HTTP:HEADER-OF$ drop {: h:str :}
   SIGNIN-PREFIX STR:LEN {: p:n :}
   0 0 begin {: held:n start:n :}
      h start COOKIE-NEXT {: name:str value:str next:n found:bool :}
      found 0= if held exit then
      name SIGNIN-PREFIX STR:STARTS-WITH? if
         name STR:DATA p + name STR:LEN p - >str {: c:str :}
         c SECRET? if
            held most < if answer c CLEARED then
            held 1+
         else held then
      else held then
      next
   again ;

\ The 302 that sends the browser to sign in, with its attempt cookie, the
\ request's attempt cookies but the newest three cleared.
: ATTEMPT ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   DRAWN {: v:str :}
   v CHALLENGE-OFF CHALLENGE {: c:str :}
   answer 302 HTTP:STATUS!
   answer s" Location" asked c AUTHORIZE-URL HTTP:HEADER!
   asked answer asked answer 0 CLEARED-FIRST 3 - CLEARED-FIRST drop
   answer s" Set-Cookie" v c ATTEMPT-COOKIE HTTP:HEADER! ;

\ The guards' shared part, the vector emptied first: rejected when the
\ Origin check answered 403, else ADMISSION's answer.
: GUARDED ( HTTP:request HTTP:response ^VEC:vector<u8> -- guard )
   {: asked:HTTP:request answer:HTTP:response dest:^ :}
   dest 0 VEC:RESIZE
   asked answer ALLOWED? 0= if construct guard rejected exit then
   asked dest ADMISSION ;

: NO-SIGN-IN ( HTTP:response -- )
   400 s" no_sign_in" s" this browser started no sign-in of that state"
   HTTP:ERROR! ;

\ Whether the request's state is a challenge this module draws, `.` and more;
\ true copies the challenge into its room.
: STATE? ( HTTP:request -- bool )
   {: asked:HTTP:request :}
   NOTE {: buf:^ :}
   asked s" state" buf HTTP:PARAM+ 0= if false exit then
   buf VEC:BYTES {: s:str :}
   s STR:LEN SECRET-LEN > 0= if false exit then
   s STR:DATA SECRET-LEN + c@ [char] . <> if false exit then
   s STR:DATA SECRET-LEN >str {: c:str :}
   c SECRET? 0= if false exit then
   c CHALLENGE-OFF SECRET-LEN ROOM SPAN:COPY true ;

\ The value of the attempt cookie the challenge names.
: ATTEMPT-OF ( HTTP:request str -- str bool )
   {: asked:HTTP:request c:str :}
   NOTE {: buf:^ :}
   SIGNIN-PREFIX buf VEC:APPEND  c buf VEC:APPEND
   asked buf VEC:BYTES HTTP:COOKIE-OF$ ;

\ Whether the attempt cookie's value is a verifier whose challenge is the
\ state's.
: PROVES? ( str str -- bool )
   {: value:str c:str :}
   value SECRET? 0= if false exit then
   value HASHED-OFF CHALLENGE c STR:= ;

\ The vector's base64url text at the offset, of the length, decoded and
\ appended; nothing for a text DECODE-URL rejects.
: DECODED+ ( n n ^VEC:vector<u8> -- )
   {: off:n len:n buf:^ :}
   buf VEC:BYTES STR:DATA off + len >str {: t:str :}
   t URL-SAFE? len 3 and 1 <> and 0= if exit then
   buf VEC:COUNT {: at:n :}
   buf len BYTES-LEN VEC:EXTEND {: room :}
   [: buf VEC:BYTES STR:DATA off + len >str room BASE64:DECODE-URL drop ;]
   catch {: code:n :}
   code 0= if exit then
   buf at VEC:RESIZE
   code E-BASE64-PAD <> if code throw then ;

\ The redirect URI's origin and the target after the request's state's `.`,
\ `/` when the rule rejects it, in the worker's vector.
: RETURN-URL ( HTTP:request -- str )
   {: asked:HTTP:request :}
   NOTE {: buf:^ :}
   asked s" state" buf HTTP:PARAM+ drop
   buf VEC:COUNT {: k:n :}
   k STATE-MOST > if k else SECRET-LEN 1+ k min then {: from:n :}
   ORIGIN-LEN {: o:n :}
   BACK$ STR:DATA o >str buf VEC:APPEND
   from k from - buf DECODED+
   k o + {: at:n :}
   buf VEC:BYTES STR:DATA at + buf VEC:COUNT at - >str TARGET? 0= if
      buf at VEC:RESIZE  s" /" buf VEC:APPEND
   then
   buf VEC:BYTES {: all:str :}
   all STR:DATA k + all STR:LEN k - >str ;

\ The session cookie of the id in the secret's room.
: SESSION-COOKIE ( -- str )
   NOTE {: buf:^ :}
   SESSION-NAME buf VEC:APPEND  [char] = buf VEC:PUSH c!
   SECRET$ buf VEC:APPEND
   s" ; Path=/" buf VEC:APPEND  LAX buf VEC:APPEND
   buf VEC:BYTES ;

\ The Set-Cookie that clears the session cookie.
: SESSION-CLEARED ( -- str )
   NOTE {: buf:^ :}
   SESSION-NAME buf VEC:APPEND
   s" =; Path=/; Max-Age=0" buf VEC:APPEND  LAX buf VEC:APPEND
   buf VEC:BYTES ;

\ The query's error, in the worker's vector, true when the issuer sent one.
: ISSUER-ERROR? ( HTTP:request -- bool )
   {: asked:HTTP:request :}
   asked s" error" NOTE HTTP:PARAM+ ;

: REJECTED-LINE ( -- )
   LOG:ERROR s" web-session" LOG:LINE
   s" the issuer answered a sign-in with error " LOG:TEXT+
   SLOT NOTES @ VEC:BYTES LOG:TEXT+ LOG:;LINE ;

: STALE-LINE ( -- )
   LOG:ERROR s" web-session" LOG:LINE
   s" a sign-in's access token is stale at once and has no refresh token"
   LOG:TEXT+ LOG:;LINE ;

\ The query's code, in the worker's vector; empty without one, which the
\ issuer rejects.
: CODE$ ( HTTP:request -- str )
   {: asked:HTTP:request :}
   NOTE {: buf:^ :}
   asked s" code" buf HTTP:PARAM+ drop
   buf VEC:BYTES ;

\ Fills a session whose vectors are null: the digest's room, the tokens and
\ the times.
: FILLED ( str str n n ^session -- )
   {: a:str r:str since:n life:n row:^ :}
   DIGEST SPAN:$ row SESSION-DIGEST SPAN:COPY
   EMPTY row SESSION-ACCESS !  a row SESSION-ACCESS @ VEC:APPEND
   EMPTY row SESSION-REFRESH !  r row SESSION-REFRESH @ VEC:APPEND
   since row SESSION-SINCE !  life row SESSION-LIFE !
   mono-ns row SESSION-USED ! ;

\ A session for the tokens; a throw leaves nothing allocated.
: MADE ( str str n n -- ^session )
   {: a:str r:str since:n life:n :}
   HEAP:ALLOCATOR [type] session ALLOC:NEW {: row:^ :}
   NULL-PTR row SESSION-NEXT !
   NULL-PTR row SESSION-ACCESS !  NULL-PTR row SESSION-REFRESH !
   construct renewal settled row SESSION-RENEWAL !
   [: a r since life row FILLED ;] catch {: code:n :}
   code 0<> if row FREED code throw then
   row ;

\ REDEEM's answer kept as a session, linked under TABLE-LOCK after the idle
\ ones are dropped; E-RESPONSE for a token stale at once with none to renew it.
: KEPT ( str str n n -- )
   {: a:str r:str secs:n since:n :}
   secs LEEWAY-S - {: life:n :}
   epoch-seconds since - life >= r STR:LEN 0= and if
      STALE-LINE BEARER:E-RESPONSE throw
   then
   a r since life MADE {: row:^ :}
   [: SWEPT row LINKED ;] TABLED ;

\ The code redeemed with the verifier for the challenge as nonce, holding no
\ lock, ANSWER cut to 0 however it ends.
: REDEEMED ( str str str -- )
   {: code:str v:str c:str :}
   epoch-seconds {: since:n :}
   [: code BACK$ v c ANSWER BEARER:REDEEM since KEPT ;]
   [: ANSWER 0 VEC:RESIZE ;] finally ;

\ The browser sent back to the page its state names, signed in.
: SIGNED-IN ( HTTP:request HTTP:response str -- )
   {: asked:HTTP:request answer:HTTP:response c:str :}
   answer s" Set-Cookie" SESSION-COOKIE HTTP:HEADER!
   answer c CLEARED
   answer 302 HTTP:STATUS!
   answer s" Location" asked RETURN-URL HTTP:HEADER! ;

\ A failed redeem's answer; a code no row names throws on.
: FAILED ( HTTP:response str n -- )
   {: answer:HTTP:response c:str rc:n :}
   rc BEARER:E-TRANSFER = rc BEARER:E-UNAVAILABLE = or if
      answer 503 HTTP:STATUS! exit
   then
   rc BEARER:E-REJECTED = if
      answer c CLEARED
      answer 400 s" sign_in_failed" s" the issuer rejected the sign-in's code"
      HTTP:ERROR! exit
   then
   rc BEARER:E-RESPONSE = if answer c CLEARED answer 502 HTTP:STATUS! exit then
   rc throw ;

\ The callback of an attempt this browser proved, the cookie's verifier given.
: RETURNED ( HTTP:request HTTP:response str -- )
   {: asked:HTTP:request answer:HTTP:response value:str :}
   CHALLENGE$ {: c:str :}
   asked ISSUER-ERROR? if
      REJECTED-LINE answer c CLEARED
      answer 403 s" sign_in_rejected" s" the issuer did not sign the person in"
      HTTP:ERROR! exit
   then
   DRAWN HASHED drop
   asked CODE$ {: code:str :}
   [: code value c REDEEMED ;] catch {: rc:n :}
   rc 0= if asked answer c SIGNED-IN exit then
   answer c rc FAILED ;

\ The redirect URI's route: the issuer's answer to an attempt (RFC 6749
\ 4.1.2), taken only from the browser whose cookie proves its state's challenge.
: CALLBACK ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   asked STATE? 0= if answer NO-SIGN-IN exit then
   asked CHALLENGE$ ATTEMPT-OF 0= if drop answer NO-SIGN-IN exit then
   {: value:str :}
   value CHALLENGE$ PROVES? 0= if answer NO-SIGN-IN exit then
   asked answer value RETURNED ;

: BRACE? ( n -- bool )
   {: c:n :}
   c LBRACE = c RBRACE = or ;

\ The fix for a byte BARE rejects.
: BARE-FIX ( n -- str )
   BRACE? if s" percent-encode it" exit then
   s" drop the query and the fragment" ;

\ E-OPERAND naming the redirect URI and its first `?`, `#` or brace, which
\ the callback's route would read as a named segment.
: BARE ( str str -- )
   {: back:str name:str :}
   back STR:LEN 0 ?do
      back STR:DATA i + c@ {: c:n :}
      c [char] ? = c [char] # = or c BRACE? or if
         back name SETTING REASON:EXPECTED
         s" an origin and a path, with no query, fragment or brace"
         REASON:APPEND
         c i FOUND-BYTE c BARE-FIX REASON:APPEND
         BEARER:E-OPERAND REASON:STOP
      then
   loop ;

\ E-OPERAND naming the authorize URL and its `#`: AUTHORIZE-URL's query
\ would follow the fragment.
: UNFRAGMENTED ( str str -- )
   {: url:str name:str :}
   url STR:LEN 0 ?do
      url STR:DATA i + c@ [char] # = if
         url name SETTING REASON:EXPECTED
         s" a URL with no fragment" REASON:APPEND
         [char] # i FOUND-BYTE s" drop the fragment" REASON:APPEND
         BEARER:E-OPERAND REASON:STOP
      then
   loop ;

\ HTTP writes a header value as it is: a CR or LF in a URL a Location names
\ would start a line of its own. E-OPERAND names the setting and the byte.
: PRINTABLE ( str str -- )
   {: url:str name:str :}
   url STR:LEN 0 ?do
      url STR:DATA i + c@ {: c:n :}
      c $21 < c $7E > or if
         url name SETTING REASON:EXPECTED s" bytes $21 to $7E" REASON:APPEND
         c i FOUND-BYTE s" percent-encode it" REASON:APPEND
         BEARER:E-OPERAND REASON:STOP
      then
   loop ;

: CHECKED ( str str str -- )
   {: url:str back:str scope:str :}
   url s" WEB-SESSION:START's authorize URL" SECURE
   url s" WEB-SESSION:START's authorize URL" PRINTABLE
   url s" WEB-SESSION:START's authorize URL" UNFRAGMENTED
   back s" WEB-SESSION:START's redirect URI" SECURE
   back s" WEB-SESSION:START's redirect URI" PRINTABLE
   back s" WEB-SESSION:START's redirect URI" BARE
   scope s" WEB-SESSION:START's scope" SCOPE-TOKENS ;

\ E-OPERAND for an idle under 1 ms, which would make every session idle at
\ once.
: IDLE-CHECKED ( n -- )
   {: idle:n :}
   idle 0 > if exit then
   REASON:OPEN s" WEB-SESSION:START's idle " REASON:APPEND
   idle FMT:INT$ REASON:APPEND s"  ms: " REASON:APPEND
   REASON:EXPECTED s" at least 1 ms" REASON:APPEND
   REASON:FIX s" give how long a session may go unused, 1 ms or more"
   REASON:APPEND
   BEARER:E-OPERAND REASON:STOP ;

: SCOPE-COMPOSED ( str -- )
   {: s:str :}
   SCOPE-ASKED MINE {: buf:^ :}
   buf 0 VEC:RESIZE
   s STR:LEN 0 > if s buf VEC:APPEND s"  " buf VEC:APPEND then
   s" openid offline_access" buf VEC:APPEND ;

public

: START ( ^config -- )
   {: cfg:^config :}
   HTTP:RUNNING? if E-HTTP-STATE throw then
   REDEEMABLE
   cfg WEB--SESSION-CONFIG:AUTHORIZE @ {: url:str :}
   cfg WEB--SESSION-CONFIG:REDIRECT @ {: back:str :}
   cfg WEB--SESSION-CONFIG:SCOPE @ {: scope:str :}
   url back scope CHECKED
   cfg WEB--SESSION-CONFIG:IDLE @ MS>N IDLE-CHECKED
   s" GET" back CALLBACK-PATH [: CALLBACK ;] HTTP:ROUTE
   url SETTINGS WEB--SESSION-CONFIG:AUTHORIZE !
   back SETTINGS WEB--SESSION-CONFIG:REDIRECT !
   scope SCOPE-COMPOSED
   cfg WEB--SESSION-CONFIG:IDLE @ SETTINGS WEB--SESSION-CONFIG:IDLE !
   DROP-ALL ;

: ADMIT? ( HTTP:request HTTP:response ^VEC:vector<u8> -- bool )
   {: asked:HTTP:request answer:HTTP:response dest:^ :}
   asked answer dest GUARDED
   MATCH guard
      admitted OF true ENDOF
      rejected OF false ENDOF
      absent OF
         answer 401 s" signed_out" s" no session is signed in" HTTP:ERROR!
         false
      ENDOF
      unavailable OF answer 503 HTTP:STATUS! false ENDOF
   ;MATCH ;

: ADMIT-PAGE? ( HTTP:request HTTP:response ^VEC:vector<u8> -- bool )
   {: asked:HTTP:request answer:HTTP:response dest:^ :}
   asked answer dest GUARDED
   MATCH guard
      admitted OF true ENDOF
      rejected OF false ENDOF
      absent OF asked answer ATTEMPT false ENDOF
      unavailable OF answer 503 HTTP:STATUS! false ENDOF
   ;MATCH ;

: END ( HTTP:request HTTP:response -- )
   {: asked:HTTP:request answer:HTTP:response :}
   asked ENDED
   answer s" Set-Cookie" SESSION-CLEARED HTTP:HEADER! ;

;package
