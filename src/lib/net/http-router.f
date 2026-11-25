\ http-router.f - routes keyed on a method and a path pattern with named
\ segments, as in /items/{id}. A pattern segment written {name} matches one
\ nonempty path segment and binds its decoded text; every other segment
\ matches itself.
\ The router answers the two rejections that belong to it: 404 when no pattern
\ matches the path and 405, with Allow, when one does but not for this method.
require src/lib/net/http-response.f
require src/lib/string.f
require src/lib/span.f
require src/lib/net/http-arena.f
require src/lib/net/http-request.f

package HTTP

private

$7B constant LBRACE
$7D constant RBRACE

\ A route added may move the columns under a worker reading them.
DYNAMIC-BUFFER ROUTE-METHOD-A ^u8
DYNAMIC-BUFFER ROUTE-METHOD-N n
DYNAMIC-BUFFER ROUTE-PATTERN-A ^u8
DYNAMIC-BUFFER ROUTE-PATTERN-N n
DYNAMIC-BUFFER ROUTE-HANDLER [ request response -- ]
DYNAMIC-BUFFER ROUTE-CAP n       \ -1 inherits BODY-CAP

SCRATCH-VARIABLE RUNNING
SCRATCH-VARIABLE ROUTE-COUNT


: ROUTE-METHOD$ ( n -- str ) {: route:n :}
   route ROUTE-METHOD-A @ route ROUTE-METHOD-N @ >str ;


: ROUTE-PATTERN$ ( n -- str ) {: route:n :}
   route ROUTE-PATTERN-A @ route ROUTE-PATTERN-N @ >str ;


: ROUTE-BODY-CAP ( n -- n ) {: route:n :}
   route 0 < if BODY-CAP exit then
   route ROUTE-CAP @ {: cap:n :}
   cap 0 < if BODY-CAP exit then
   cap ;


\ ---- segments ---------------------------------------------------------------

: SEG-END ( str n -- n ) {: bytes:str from:n :}
   bytes STR:LEN from ?do
      bytes STR:DATA i + c@ SLASH = if i unloop exit then
   loop
   bytes STR:LEN ;


: NAMED? ( str -- bool ) {: bytes:str :}
   bytes STR:LEN 2 < if false exit then
   bytes STR:DATA c@ LBRACE <> if false exit then
   bytes STR:DATA bytes STR:LEN + 1- c@ RBRACE = ;


: SEGMENT-NAME$ ( str -- str ) {: bytes:str :}
   bytes STR:DATA 1+ bytes STR:LEN 2 - >str ;


\ The escapes were validated when the request line was parsed, so a percent
\ always carries the two hex digits this reads.
: COPY-SEGMENT ( str n -- ) {: bytes:str idx:n :}
   0
   begin
      {: at:n :}
      at bytes STR:LEN >= if exit then
      bytes STR:DATA at + c@ PERCENT = at 3 + bytes STR:LEN <= and if
         bytes STR:DATA at 1+ + c@ STR:HEX-DIGIT-VALUE HEX-BASE *
         bytes STR:DATA at 2 + + c@ STR:HEX-DIGIT-VALUE + idx SEGMENT+C
         at 3 +
      else
         bytes STR:DATA at + c@ idx SEGMENT+C
         at 1+
      then
   again ;


: BIND-SEGMENT ( str n -- ) {: bytes:str idx:n :}
   idx SEGMENT-OPEN {: start:n :}
   bytes idx COPY-SEGMENT
   start idx SEGMENT-CLOSE ;


\ ---- matching ---------------------------------------------------------------

\ Select a route from the validated path without copying named segments. The
\ request's body limit is checked before a selected route binds them.
: SEGMENTS-MATCH? ( n n -- bool ) {: route:n idx:n :}
   route ROUTE-PATTERN$ {: pat:str :}
   idx SLOT-PATH$ {: path:str :}
   pat STR:LEN path STR:LEN {: patlen:n pathlen:n :}
   0 0
   begin
      {: patat:n pathat:n :}
      patat patlen >= pathat pathlen >= and if true exit then
      patat patlen >= pathat pathlen >= or if false exit then
      pat patat 1+ SEG-END {: patend:n :}
      path pathat 1+ SEG-END {: pathend:n :}
      pat STR:DATA patat 1+ + patend patat - 1- >str {: pseg:str :}
      path STR:DATA pathat 1+ + pathend pathat - 1- >str {: hseg:str :}
      pseg NAMED? if
         hseg STR:LEN 0= if false exit then
      else
         pseg hseg STR:= 0= if false exit then
      then
      patend pathend
   again ;


\ The selected pattern already matches this path. Copy only its named
\ segments once the body has been accepted, just before the handler runs.
: BIND-ROUTE ( n n -- ) {: route:n idx:n :}
   route ROUTE-PATTERN$ {: pat:str :}
   idx SLOT-PATH$ {: path:str :}
   0 idx SEG-U!
   0 idx SEGMENT-N!
   0 0
   begin
      {: patat:n pathat:n :}
      patat pat STR:LEN >= if exit then
      pat patat 1+ SEG-END {: patend:n :}
      path pathat 1+ SEG-END {: pathend:n :}
      pat STR:DATA patat 1+ + patend patat - 1- >str NAMED? if
         path STR:DATA pathat 1+ + pathend pathat - 1- >str
         idx BIND-SEGMENT
      then
      patend pathend
   again ;


: METHOD-MATCH? ( n n -- bool ) {: route:n idx:n :}
   idx SLOT-METHOD$ route ROUTE-METHOD$ STR:= ;


: FIND-ROUTE ( n -- n ) {: idx:n :}
   ROUTE-COUNT @ 0 ?do
      i idx SEGMENTS-MATCH? if
         i idx METHOD-MATCH? if i unloop exit then
      then
   loop
   -1 ;


: PATH-MATCHED? ( n -- bool ) {: idx:n :}
   ROUTE-COUNT @ 0 ?do
      i idx SEGMENTS-MATCH? if true unloop exit then
   loop
   false ;


: NAMED-INDEX ( n str -- n ) {: route:n name:str :}
   route ROUTE-PATTERN$ {: pat:str :}
   0 0
   begin
      {: at:n count:n :}
      at pat STR:LEN >= if -1 exit then
      pat at 1+ SEG-END {: end:n :}
      pat STR:DATA at 1+ + end at - 1- >str {: seg:str :}
      seg NAMED? if
         seg SEGMENT-NAME$ name STR:= if count exit then
         end count 1+
      else
         end count
      then
   again ;


\ ---- the rejections the router owns -------------------------------------------

: ALLOW-HEADER ( n -- ) {: idx:n :}
   s" Allow" idx HDR+
   COLON idx HDR+C
   SP idx HDR+C
   0
   ROUTE-COUNT @ 0 ?do
      i idx SEGMENTS-MATCH? if
         dup 0 > if CH-COMMA idx HDR+C SP idx HDR+C then
         i ROUTE-METHOD$ idx HDR+
         1+
      then
   loop
   drop
   CR idx HDR+C
   LF idx HDR+C ;


: NOT-FOUND ( n -- ) {: idx:n :}
   idx RESPONSE-OF 404 s" not_found" s" no route answers this path" ERROR! ;


: NOT-ALLOWED ( n -- ) {: idx:n :}
   idx ALLOW-HEADER
   idx RESPONSE-OF 405 s" method_not_allowed"
   s" this path does not answer that method" ERROR! ;


: ROUTE-RESERVE ( n -- ) {: routes:n :}
   routes ROUTE-METHOD-A-RESERVE
   routes ROUTE-METHOD-N-RESERVE
   routes ROUTE-PATTERN-A-RESERVE
   routes ROUTE-PATTERN-N-RESERVE
   routes ROUTE-HANDLER-RESERVE
   routes ROUTE-CAP-RESERVE ;


: ADD-ROUTE ( str str n [ request response -- ] -- )
   {: method:str pattern:str cap:n handler :}
   RUNNING @ 0 <> if E-STATE throw then
   method STR:LEN 0 <= if E-ROUTE throw then
   pattern STR:LEN 0 <= if E-ROUTE throw then
   pattern STR:DATA c@ SLASH <> if E-ROUTE throw then
   ROUTE-COUNT @ 1+ ROUTE-RESERVE
   ROUTE-COUNT @ {: at:n :}
   method STR:DATA at ROUTE-METHOD-A !
   method STR:LEN at ROUTE-METHOD-N !
   pattern STR:DATA at ROUTE-PATTERN-A !
   pattern STR:LEN at ROUTE-PATTERN-N !
   cap at ROUTE-CAP !
   handler at ROUTE-HANDLER !
   at 1+ ROUTE-COUNT ! ;


public

\ One route. The method, the pattern and the handler are the caller's and must
\ outlive the server, which a literal and a definition both do. A built program
\ keeps no route: it adds them at run time.
: ROUTE ( str str [ request response -- ] -- )
   {: method:str pattern:str handler :}
   method pattern -1 handler ADD-ROUTE ;


\ An explicit byte limit for this route, including zero for an empty body. A
\ built program keeps no route: it adds them at run time.
: ROUTE-CAPPED ( str str n [ request response -- ] -- )
   {: method:str pattern:str cap:n handler :}
   cap 0 < if E-CAPACITY throw then
   method pattern cap handler ADD-ROUTE ;


: ROUTES-RESET ( -- )
   RUNNING @ 0 <> if E-STATE throw then
   0 ROUTE-COUNT ! ;


\ The text a named pattern segment bound, decoded. False when this request
\ matched no route, or that route names no such segment.
: SEGMENT-OF$ ( request str -- str bool )
   {: subject:request name:str :}
   subject SLOT-OF-REQUEST {: idx:n :}
   idx ROUTE-AT@ {: route:n :}
   route 0 < if idx SEG-BUF 0 SPAN:TAKE SPAN:$ false exit then
   route name NAMED-INDEX {: at:n :}
   at 0 < if idx SEG-BUF 0 SPAN:TAKE SPAN:$ false exit then
   at idx SEGMENT-N@ >= if idx SEG-BUF 0 SPAN:TAKE SPAN:$ false exit then
   idx at SEGMENT$ true ;


;package
