\ http-router.f - routes keyed on a method and a path pattern with named
\ segments, as in /items/{id}. A pattern segment written {name} matches one
\ nonempty path segment and binds its decoded text; every other segment
\ matches itself.
\
\ The router answers the two refusals that belong to it: 404 when no pattern
\ matches the path and 405, with Allow, when one does but not for this method.
require lib/net/http-response.f
require lib/string.f
require lib/span.f

package HTTP

private

$20 constant MAX-ROUTES
$7B constant LBRACE
$7D constant RBRACE

MAX-ROUTES TYPED-BUFFER ROUTE-METHOD-A ptr u8
MAX-ROUTES TYPED-BUFFER ROUTE-METHOD-N n
MAX-ROUTES TYPED-BUFFER ROUTE-PATTERN-A ptr u8
MAX-ROUTES TYPED-BUFFER ROUTE-PATTERN-N n
MAX-ROUTES TYPED-BUFFER ROUTE-HANDLER [ request response -- ]

\ Routes are registered before any task is live and only read afterwards.
variable ROUTE-COUNT


: ROUTE-BOUNDED ( n -- n ) {: route:n :}
   route 0 < if E-ROUTE throw then
   route ROUTE-COUNT @ >= if E-ROUTE throw then
   route ;


: ROUTE-METHOD$ ( n -- ptr u8 n ) {: route:n :}
   route ROUTE-BOUNDED ROUTE-METHOD-A @ route ROUTE-METHOD-N @ ;


: ROUTE-PATTERN$ ( n -- ptr u8 n ) {: route:n :}
   route ROUTE-BOUNDED ROUTE-PATTERN-A @ route ROUTE-PATTERN-N @ ;


\ ---- segments ---------------------------------------------------------------

: SEG-END ( ptr u8 n n -- n ) {: bytes:ptr len:n from:n :}
   len from ?do
      bytes i + c@ SLASH = if i unloop exit then
   loop
   len ;


: NAMED? ( ptr u8 n -- bool ) {: bytes:ptr len:n :}
   len 2 < if false exit then
   bytes c@ LBRACE <> if false exit then
   bytes len + 1- c@ RBRACE = ;


: SEGMENT-NAME$ ( ptr u8 n -- ptr u8 n ) {: bytes:ptr len:n :}
   bytes 1+ len 2 - ;


\ The escapes were validated when the request line was parsed, so a percent
\ always carries the two hex digits this reads.
: COPY-SEGMENT ( ptr u8 n n -- ) {: bytes:ptr len:n idx:n :}
   0
   begin
      {: at:n :}
      at len >= if exit then
      bytes at + c@ PERCENT = at 3 + len <= and if
         bytes at 1+ + c@ HEX-VALUE HEX-BASE *
         bytes at 2 + + c@ HEX-VALUE + idx SEGMENT+C
         at 3 +
      else
         bytes at + c@ idx SEGMENT+C
         at 1+
      then
   again ;


: BIND-SEGMENT ( ptr u8 n n -- ) {: bytes:ptr len:n idx:n :}
   idx SEGMENT-OPEN {: start:n :}
   bytes len idx COPY-SEGMENT
   start idx SEGMENT-CLOSE ;


\ ---- matching ---------------------------------------------------------------

\ Walks both paths one segment at a time, binding what the pattern names. The
\ bindings of a pattern that then fails are dropped by the next attempt.
: SEGMENTS-MATCH? ( n n -- bool ) {: route:n idx:n :}
   route ROUTE-PATTERN$ {: pat:ptr patlen:n :}
   idx SLOT-PATH$ {: path:ptr pathlen:n :}
   0 idx SEG-U!
   0 idx SEGMENT-N!
   0 0
   begin
      {: patat:n pathat:n :}
      patat patlen >= pathat pathlen >= and if true exit then
      patat patlen >= pathat pathlen >= or if false exit then
      pat patlen patat 1+ SEG-END {: patend:n :}
      path pathlen pathat 1+ SEG-END {: pathend:n :}
      pat patat 1+ + patend patat - 1- {: pa:ptr pu:n :}
      path pathat 1+ + pathend pathat - 1- {: ha:ptr hu:n :}
      pa pu NAMED? if
         hu 0= if false exit then
         ha hu idx BIND-SEGMENT
      else
         pa pu ha hu STR= 0= if false exit then
      then
      patend pathend
   again ;


: METHOD-MATCH? ( n n -- bool ) {: route:n idx:n :}
   idx SLOT-METHOD$ route ROUTE-METHOD$ STR= ;


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


: NAMED-INDEX ( n ptr u8 n -- n ) {: route:n name:ptr namelen:n :}
   route ROUTE-PATTERN$ {: pat:ptr patlen:n :}
   0 0
   begin
      {: at:n count:n :}
      at patlen >= if -1 exit then
      pat patlen at 1+ SEG-END {: end:n :}
      pat at 1+ + end at - 1- {: pa:ptr pu:n :}
      pa pu NAMED? if
         pa pu SEGMENT-NAME$ name namelen STR= if count exit then
         end count 1+
      else
         end count
      then
   again ;


\ ---- the refusals the router owns -------------------------------------------

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


public

\ One route. The method, the pattern and the handler are the caller's and must
\ outlive the server, which a literal and a definition both do.
: ROUTE ( ptr u8 n ptr u8 n [ request response -- ] -- )
   {: method:ptr methodlen:n pattern:ptr patternlen:n handler :}
   ROUTE-COUNT @ MAX-ROUTES >= if E-ROUTE throw then
   methodlen 0 <= if E-ROUTE throw then
   patternlen 0 <= if E-ROUTE throw then
   pattern c@ SLASH <> if E-ROUTE throw then
   ROUTE-COUNT @ {: at:n :}
   method at ROUTE-METHOD-A !
   methodlen at ROUTE-METHOD-N !
   pattern at ROUTE-PATTERN-A !
   patternlen at ROUTE-PATTERN-N !
   handler at ROUTE-HANDLER !
   at 1+ ROUTE-COUNT ! ;


: ROUTES-RESET ( -- )
   0 ROUTE-COUNT ! ;


\ The text a named pattern segment bound, decoded. False when this request
\ matched no route, or that route names no such segment.
: SEGMENT-OF$ ( request ptr u8 n -- ptr u8 n bool )
   {: subject:request name:ptr namelen:n :}
   subject SLOT-OF-REQUEST {: idx:n :}
   idx ROUTE-AT@ {: route:n :}
   route 0 < if idx SEG-BUF 0 SPAN:TAKE SPAN:$ false exit then
   route name namelen NAMED-INDEX {: at:n :}
   at 0 < if idx SEG-BUF 0 SPAN:TAKE SPAN:$ false exit then
   at idx SEGMENT-N@ >= if idx SEG-BUF 0 SPAN:TAKE SPAN:$ false exit then
   idx at SEGMENT$ true ;


;package
