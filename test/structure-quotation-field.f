\ A private route record carries a typed handler through construction, storage,
\ buffer growth, field projection and checked dispatch.
require lib/test.f
require test/checker-assert.f

package QUOT-FIELD-TEST

STRUCTURE request 0
   FIELD id n
;STRUCTURE

STRUCTURE response 0
   FIELD id n
;STRUCTURE

STRUCTURE route-text 0
   FIELD base ptr u8
   FIELD len n
;STRUCTURE

STRUCTURE route 0 DERIVE addr
   FIELD method route-text
   FIELD pattern route-text
   FIELD handler [ request response -- ]
;STRUCTURE

STRUCTURE route-box 0
   FIELD item route
;STRUCTURE

STRUCTURE route-mixed 0
   FIELD before [ request response -- ]
   FIELD text route-text
   FIELD after [ request response -- ]
;STRUCTURE

DYNAMIC-BUFFER ROUTES route
TYPED-VARIABLE SAVED-BOX route-box
variable SEEN

: EARLY ( request response -- )
   {: req:request res:response :}
   req REQUEST-UNMAKE res RESPONSE-UNMAKE + SEEN ! ;

: LATE ( request response -- )
   {: req:request res:response :}
   req REQUEST-UNMAKE res RESPONSE-UNMAKE + 1000 + SEEN ! ;

: ADD ( n -- ) {: idx:n :}
   idx 1+ ROUTES-RESERVE
   s" GET" ROUTE-TEXT-MAKE
   s" /item" ROUTE-TEXT-MAKE
   idx 64 < if [: EARLY ;] else [: LATE ;] then
   ROUTE-MAKE idx ROUTES ! ;

: DISPATCH ( n -- ) {: idx:n :}
   7 REQUEST-MAKE 13 RESPONSE-MAKE
   idx ROUTES ROUTE-HANDLER @ execute ;

: STORE-SAVED ( -- )
   s" GET" ROUTE-TEXT-MAKE
   s" /item" ROUTE-TEXT-MAKE
   [: EARLY 77 SEEN ! ;] ROUTE-MAKE ROUTE-BOX-MAKE SAVED-BOX ! ;

: CHECK-BOX ( route-box -- )
   ROUTE-BOX-UNMAKE ROUTE-UNMAKE
   {: method:route-text pattern:route-text handler :}
   method ROUTE-TEXT-UNMAKE s" GET" T$=
   pattern ROUTE-TEXT-UNMAKE s" /item" T$=
   7 REQUEST-MAKE 13 RESPONSE-MAKE handler execute
   SEEN @ 77 T= ;

: CHECK-ROW ( n -- ) {: idx:n :}
   idx ROUTES @ ROUTE-UNMAKE
   {: method:route-text pattern:route-text handler :}
   method ROUTE-TEXT-UNMAKE 3 T= drop
   pattern ROUTE-TEXT-UNMAKE 5 T= drop
   7 REQUEST-MAKE 13 RESPONSE-MAKE handler execute ;

: CHECK-MIXED ( -- )
   [: EARLY ;] s" GET" ROUTE-TEXT-MAKE [: LATE ;] ROUTE-MIXED-MAKE
   ROUTE-MIXED-UNMAKE {: before text:route-text after :}
   text ROUTE-TEXT-UNMAKE 3 T= drop
   7 REQUEST-MAKE 13 RESPONSE-MAKE before execute
   SEEN @ 20 T=
   7 REQUEST-MAKE 13 RESPONSE-MAKE after execute
   SEEN @ 1020 T= ;

: RUN ( -- )
   T-RESET
   s" BAD-MAKE ( route-text route-text [ n -- n ] -- route ) ROUTE-MAKE"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" BAD-FIELD ( [ n -- n ] ptr route -- ) ROUTE-HANDLER !"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" BAD-EXEC ( n n n -- ) ROUTES ROUTE-HANDLER @ execute"
      CHECK-QUIET-CANDIDATE! 0 T=
   CHECK-MIXED
   129 0 ?do i ADD loop
   0 CHECK-ROW SEEN @ 20 T=
   128 CHECK-ROW SEEN @ 1020 T=
   0 DISPATCH SEEN @ 20 T=
   128 DISPATCH SEEN @ 1020 T=
   STORE-SAVED SAVED-BOX @ CHECK-BOX
   ROUTES-RELEASE
   T-REPORT ;

RUN
;package
