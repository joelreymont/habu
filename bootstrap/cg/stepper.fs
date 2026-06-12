\ stepper.fs — a single-step debugger for Forth: evaluate a snippet ONE TOKEN at a
\ time, printing the token and the resulting data stack after each step. The "stepper"
\ debugging tool — interleaves with the .s inspector. Pure gforth host words (evaluate,
\ .s); the per-step logic carries typed signatures. Use: s" 5 dup * 3 +" STEP
variable SP-A  variable SP-U  variable SP-I

: ST-AT   ( -- c )  SP-A @ SP-I @ + c@ ;                  \ current char

: ST-END? ( -- f )  SP-I @ SP-U @ >= ;

: ST-SKIP ( -- )  begin ST-END? 0= ST-AT bl = and while 1 SP-I +! repeat ;

: ST-SCAN ( -- a u )  SP-A @ SP-I @ +  begin ST-END? 0= ST-AT bl <> and while 1 SP-I +! repeat
   SP-A @ SP-I @ +  over - ;

: STEP ( a u -- )
   SP-U !  SP-A !  0 SP-I !
   cr ." stepping:" cr
   begin  ST-SKIP  ST-END? 0=  while
      ST-SCAN  2dup ."   " type  ['] evaluate catch if  ."  !! error" 2drop  else  then
      ."   => "  .s  cr
   repeat ;
