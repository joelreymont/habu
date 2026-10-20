\ Native engine builder support saved before any target window opens.
1 set-tier
require tools/native-build-support-deps.f

package NATIVE-BUILD-SUPPORT
private

create SAVED-ROOT FS-PATH-CAP allot
variable SAVED-ROOT-U

\ Compile the ordinary driver under the same guard as the source entry.
: LOAD ( -- )
   s" tools/native-build-core.f" required ;

' LOAD
;package
EXECUTABLE-BUILD:WITH

package NATIVE-BUILD-SUPPORT
private

: RESTORE-ROOT ( -- )
   SAVED-ROOT-U @ 0 > if
      SAVED-ROOT SAVED-ROOT-U @ SOURCE-ROOT:ENGINE!
   then
   FS-PATH-CAP 0 ?do 0 SAVED-ROOT i + c! loop
   0 SAVED-ROOT-U ! ;

public

: ORIGIN ( n n -- n ) code-origin ;

: SAVE-ROOT ( -- )
   SOURCE-ROOT:ENGINE$ {: a:ptr u:n :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   a SAVED-ROOT u BYTE-COPY
   u SAVED-ROOT-U ! ;

: RUN ( -- )
   RESTORE-ROOT
   \ Target callbacks own their native scopes; the transient writer has none.
   ['] ORIGIN false NATIVE-BUILD:RUN ;

;package

' NATIVE-BUILD-SUPPORT:RUN APP-IMAGE:START!
