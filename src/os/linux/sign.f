\ sign.f -- Linux executables do not need a post-link code-signing pass.

package ELF
public
: SET-SIGID ( str -- )
   drop ;

: CODESIG2 ( img -- img ) ;
;package
