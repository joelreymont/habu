\ sign.f -- Linux executables do not need a post-link code-signing pass.

: SET-SIGID ( ptr u8 n -- )  2drop ;
s" SET-SIGID" s" ptr u8 n --" TRUST

: CODESIG2 ( img -- img ) ;
s" CODESIG2" s" img -- img" TRUST
