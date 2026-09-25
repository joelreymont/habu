\ A real inferred quotation with live exceptional rows cannot be exported as
\ an ordinary four-row effect while those exceptional rows are not portable.
s" src/habu/layout.f" provided
s" src/core/checker-owner-abi.f" provided
require src/habu/aot-arm.f

package PAYLOAD-EXCEPTION-TEST
public

TRUSTED: NONVACUOUS ( -- )
   s" PAYLOAD-THROW-PROVIDER" CHECKER-FIND-ACTIVE-SYM
   USIG-NEWEST dup 0= IF 79 throw THEN 1- E-PTR
   E-DOUT@ E-PTR EN.A @ E-PTR {: quotation:ptr :}
   quotation EN.TAG @ EN-QUOT <> IF 79 throw THEN
   quotation EN.E @ 0= IF 79 throw THEN
   quotation EN.G @ 0= quotation EN.H @ 0= or IF 79 throw THEN
   s" exceptional quotation rows are present" type cr ;

;package
AOT-ARM:WINDOW-OPEN
: PAYLOAD-THROW-PROVIDER [: 1+ -99 throw ;] ;
PAYLOAD-EXCEPTION-TEST:NONVACUOUS
AOT-ARM:WINDOW-CLOSE
s" exception-bearing payload was incorrectly accepted" 79 die
