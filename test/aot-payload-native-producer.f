\ The preparation opened the window; its retained driver then opened the
\ compiler's literal pool before loading these real native definitions.

package PAYLOAD-NATIVE
public

STRUCTURE pair 0
   FIELD left n
   FIELD right n
;STRUCTURE

: BUMP ( n -- n ) 1+ ;

\ An unchecked native row is still useful ABI metadata after import.
0 set-check
: ABI-ONLY ( n -- n ) ;
TRUSTED: ASSERTED ( n -- n ) ;
' LOWER-CERT-HOOK:HOOK set-check


: PAIR-SUM ( pair -- n ) PAYLOAD--NATIVE-PAIR:UNMAKE + ;

\ Abstract views are capturable code effects; this window creates no live view.
: VIEW-COPY ( read-view<p,q,a> -- read-view<p,q,a> read-view<p,q,a> ) dup ;
: VIEW-WRAP ( read-view<p,q,a> -- option<read-view<p,q,a>> ) OPTION:SOME ;
: VIEW-UNWRAP ( option<read-view<p,q,a>> -- read-view<p,q,a> )
   MATCH option
      none OF 1 throw ENDOF
      some OF ENDOF
   ;MATCH ;

private
variable QUOTE-SLOT
public
: QUOTE-STORE ( [ R -- R ] -- ) QUOTE-SLOT xt! ;

;package

AOT-ARM:WINDOW-CLOSE

package PAYLOAD-NATIVE-PRODUCER
create KEY 32 allot
create FSHA-CTX SHA256-FILE-CTX-BYTES allot   \ this fixture's file-digest context

: EQ ( n n -- ) <> if 79 throw then ;


: RUN ( -- )
   AOT-ARM:B0 @ AOT-ARM:B1 @ code-origin 1 EQ
   PRE-R @ PRE-D @ AOT-CAPTURE:PRELUDE-MARK
   AOT-ARM:WINDOW$ AOT-CAPTURE:CAPTURE
   AOT-IDENT:RESET
   s" test/aot-payload-native-prepare.f" AOT-IDENT:PATH+
   s" test/aot-payload-native-producer.f" AOT-IDENT:PATH+
   FSHA-CTX s" HABU_PAYLOAD_TEST_ENGINE" GETENV KEY SHA256-FILE-IN 0 EQ
   KEY s" HABU_PAYLOAD_TEST_ARTIFACT" GETENV AOT-FILE:WRITE
   s" native graph artifact written" type cr ;

RUN
;package
