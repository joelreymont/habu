\ conditional.f - `[if]` and `[then]` choose text at a file's top level, as a
\ target's files are chosen:
\ `HB-TARGET-MACOS? [if] require src/lib/power/macos.f [then]`. A false answer
\ skips the tokens through the matching `[then]`, counting the `[if]`s nested
\ there, those in comments and strings too. Immediate, as `require` is: a body
\ rejects `[if]` for its effect, and a `[then]` there does nothing. Loads after
\ src/core/rejection.f, whose record an unclosed `[if]` prints, and before
\ src/core/checker.f, which loads before the source loader, so the checker can
\ choose its own text.

\ The `[if]` token is kept at entry: parse-name moves the token cells.
: [if] ( bool -- )
   if exit then
   data-base TKA-CELL + 0 ptr-field @  data-base TKL-CELL + @
   {: ta:^ tu:n :}
   1 begin dup 0 > while
      parse-name dup 0= if
         s" E-UNTERMINATED-IF" REJECTION:OPEN
         s" `[if]` has no matching `[then]` before the source ends"
         REJECTION:EMIT-STR
         ta tu REJECTION:AT  s" opened here" REJECTION:EMIT-STR
         REJECTION:HELP  s" close it with `[then]`" REJECTION:EMIT-STR
         INCLUDE-IO-RC REJECTION:STOP
      then
      2dup s" [if]" CORE-STR=CI if 2drop 1 + else
         s" [then]" CORE-STR=CI if 1 - then
      then
   repeat drop ;
immediate

: [then] ( -- ) ;
immediate
