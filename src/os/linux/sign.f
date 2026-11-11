\ sign.f -- Linux executables do not need a post-link code-signing pass.

package ELF
public
: SET-SIGID ( ptr u8 n -- )
   2drop ;

: CODESIG2 ( img -- img ) ;
;package
