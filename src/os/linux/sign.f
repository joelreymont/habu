\ sign.f -- Linux executables do not need a post-link code-signing pass.

: SET-SIGID ( ptr u8 n -- )
   2drop ;

: CODESIG2 ( img -- img ) ;
