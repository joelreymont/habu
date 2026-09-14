\ Loaded only by the freshly emitted image; the producer has already exited.
package PREFIX-LITERAL-CONSUMER
public

\ Identity is separate from execution: a different callable can share a result.
TRUSTED: SCALAR-XT ( [ n -- n ] -- n ) ;
TRUSTED: QUERY-XT ( [ -- n ] -- n ) ;


: SAME-XT ( n n -- )
   <> if s" prefix-literal: resolved a different code entry" 79 die then ;


: SAME-VALUE ( n n -- )
   <> if s" prefix-literal: execution returned a different value" 79 die then ;


\ Source composition selects the holder's concrete quotation effect. Every
\ generated CHECK body is checked normally by the fresh image.
TRUSTED: CHECK-SOURCE ( ptr u8 n -- ) evaluate ;


: RUN ( -- )
   SCRIPT-ARGC 1 <> if 79 throw then
   0 SCRIPT-ARGV$ s" global" CORE-STR= if
      s" package LITERAL-CONSUMER using PREFIX-LITERAL-CONSUMER using LITERAL-WINDOW : CHECK ( -- ) HOLDER SCALAR-XT ['] ASCII-UPPER SCALAR-XT SAME-XT 97 HOLDER execute 65 SAME-VALUE 48 HOLDER execute 48 SAME-VALUE ; CHECK ;package" CHECK-SOURCE
   else
      s" package LITERAL-CONSUMER using PREFIX-LITERAL-CONSUMER using LITERAL-WINDOW : CHECK ( -- ) HOLDER QUERY-XT ['] PREFIX-MARK:REQ QUERY-XT SAME-XT HOLDER execute PREFIX-MARK:REQ SAME-VALUE ; CHECK ;package" CHECK-SOURCE
   then
   s" prefix-literal: exact identity and execution passed" type cr ;

;package

PREFIX-LITERAL-CONSUMER:RUN
