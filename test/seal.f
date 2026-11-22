split-str
\ seal.f - seal regressions: a sealed package, and every engine package hb's
\ build seals, rejects a definition into it, under --load and in a SUBJECT fork.
\ Run: bin/hb --load src/lib/error.f src/lib/string.f src/lib/test.f src/lib/memory.f src/lib/fs.f
\   src/lib/fs-mutate.f src/lib/process.f src/lib/process-argv.f test/seal.f

require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
require src/lib/test/ending.f
require src/lib/memory.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/test/subject.f
require src/lib/engine-candidate.f

package SEAL-SUITE

2048 constant SLV-CAP

variable SLV-ROOT-U
variable SLV-CHILD-U
variable SLV-OUT-U
variable SLV-ERR-U
variable SLV-EXITED                 \ bool: child completed by exit
variable SLV-RC

create SLV-ROOT-BUF PATH-CAP allot
create SLV-CHILD-BUF PATH-CAP allot
create SLV-OUT SLV-CAP allot
create SLV-ERR SLV-CAP allot
create SLV-EMPTY 1 allot            \ zero-length stdin

: SLV-COPY! ( str ^u8 ^n -- ) {: src:^ u:n dst:^ lenp:^ :}
   u PATH-CAP > if E-FS-CAPACITY throw then
   src dst u BYTE-COPY
   u lenp ! ;

: SLV-ROOT ( -- str )
   SLV-ROOT-BUF SLV-ROOT-U @ ;

: SLV-CHILD ( -- str )
   SLV-CHILD-BUF SLV-CHILD-U @ ;

: SLV-LF ( -- )
   10 SB-APPEND-C ;

: SLV-STORE! ( len len PROCESS:ending str -- ) {: outu:len erru:len oc src:^ u:n :}
   erru LEN>N SLV-ERR-U !  outu LEN>N SLV-OUT-U !
   oc MATCH PROCESS:ending
     exited OF SLV-RC ! 0 0= SLV-EXITED ! ENDOF
     signaled OF SLV-RC ! 0 0= 0= SLV-EXITED ! ENDOF
     timeout OF src u SLV-OUT SLV-OUT-U @ SLV-ERR SLV-ERR-U @ T-TIMED-OUT ENDOF
   ;MATCH ;

: SLV-RUN-LOAD ( str -- ) {: src:^ u:n :}
   SLV-CHILD src u FS:WRITE-ALL
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   SLV-CHILD >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$  SLV-EMPTY 0  SLV-OUT SLV-CAP >LEN
   SLV-ERR SLV-CAP >LEN  PROCESS:WAIT-FOREVER  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   src u SLV-STORE! ;

\ Fixtures run in SUBJECT forks of the sealed candidate.
: SLV-SUBJECT ( str -- ) {: src:^ u:n :}
   src u SLV-OUT SLV-CAP >LEN SLV-ERR SLV-CAP >LEN
   PROCESS:WAIT-FOREVER SUBJECT:RUN src u SLV-STORE! ;

: SLV-ERR$ ( -- str )  SLV-ERR SLV-ERR-U @ ;

\ Publishing into a protected WID is rejected with exit 70 and an
\ E-SEALED-PACKAGE record naming the word it would add.
: SLV-ASSERT-PROT-PUBLISH ( str -- ) {: prose:^ proseu:n :}
   SLV-EXITED @ TTRUE
   SLV-RC @ 70 T=
   SLV-ERR$ s" error[E-SEALED-PACKAGE]" CONTAINS? TTRUE
   SLV-ERR$ prose proseu CONTAINS? TTRUE ;

: SLV-LINE ( str -- )
   SB-APPEND SLV-LF ;

\ Package SP with A in its public wordlist, sealed and left open.
: SLV-SP-SEALED ( -- )
   SB-RESET
   s" package SP" SLV-LINE  s" public" SLV-LINE  s" : A ( -- ) ;" SLV-LINE
   s" seal-package" SLV-LINE ;

: SLV-SP-COLON$ ( -- str )
   SLV-SP-SEALED  s" ;package" SLV-LINE  s" : SP:B ( -- ) ;" SLV-LINE  SB$ ;

: SLV-SP-VAR$ ( -- str )
   SLV-SP-SEALED  s" ;package" SLV-LINE  s" variable SP:V" SLV-LINE  SB$ ;

: SLV-SP-INSIDE$ ( -- str )
   SLV-SP-SEALED  s" : C ( -- ) ;" SLV-LINE  s" ;package" SLV-LINE  SB$ ;

: SLV-SP-EXPORT$ ( -- str )
   SB-RESET
   s" package SP2" SLV-LINE  s" public" SLV-LINE  s" : Z ( -- ) ;" SLV-LINE
   s" ;package" SLV-LINE
   s" package SP" SLV-LINE  s" public" SLV-LINE  s" : A ( -- ) ;" SLV-LINE
   s" seal-package" SLV-LINE  s" EXPORT SP2:Z" SLV-LINE  s" ;package" SLV-LINE  SB$ ;

\ A sealed package's record refuses a definition, a variable and an export into its
\ wordlists, and a bare name inside it, naming the name token as written.
: SLV-SEALED-DEFINE ( -- )
   s" : SP:B after SP's seal-package -> E-SEALED-PACKAGE naming SP:B, exit 70" T-LABEL
   SLV-SP-COLON$ SLV-RUN-LOAD s" `SP:B` would change a sealed package" SLV-ASSERT-PROT-PUBLISH
   s" variable SP:V after SP's seal-package -> E-SEALED-PACKAGE naming SP:V, exit 70" T-LABEL
   SLV-SP-VAR$ SLV-SUBJECT s" `SP:V` would change a sealed package" SLV-ASSERT-PROT-PUBLISH
   s" : C inside SP after its seal-package -> E-SEALED-PACKAGE naming C, exit 70" T-LABEL
   SLV-SP-INSIDE$ SLV-SUBJECT s" `C` would change a sealed package" SLV-ASSERT-PROT-PUBLISH
   s" EXPORT SP2:Z into SP after its seal-package -> E-SEALED-PACKAGE naming SP2:Z, exit 70" T-LABEL
   SLV-SP-EXPORT$ SLV-SUBJECT s" `SP2:Z` would change a sealed package" SLV-ASSERT-PROT-PUBLISH ;

\ hb's build seals every engine package, MACHO with no seal-package in its source.
: SLV-ENGINE-SEALED ( -- )
   s" package MACHO in hb -> E-SEALED-PACKAGE naming MACHO, exit 70" T-LABEL
   s" package MACHO ;package" SLV-RUN-LOAD s" `MACHO` would change a sealed package" SLV-ASSERT-PROT-PUBLISH ;

: SLV-PREPARE ( -- )
   FS:CLEANUP-RESET
   s" habu-seal" FS:MKTEMP-DIR {: a:^ u:n :}
   a u SLV-ROOT-BUF SLV-ROOT-U SLV-COPY!
   SLV-ROOT FS:CLEANUP-TREE+
   SLV-ROOT s" forge.f" SLV-CHILD-BUF FS:JOIN-PATH SLV-CHILD-U ! ;

: SLV-CLEANUP ( -- )
   FS:CLEANUP-RUN
   SLV-ROOT FS:EXISTS? TFALSE ;

public

: RUN ( -- )
   T-RESET
   SLV-PREPARE
   SLV-SEALED-DEFINE
   SLV-ENGINE-SEALED
   SLV-CLEANUP
   T-REPORT
   s" seal-test: ok" type cr ;

;package

SEAL-SUITE:RUN
