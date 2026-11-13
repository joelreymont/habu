\ seal.f - seal regressions: post-seal cp!, FORGET, HIDE and record writes
\ refuse engine code and records (exit ENGINE-ERROR:SEAL-VIOLATION), and a sealed
\ package refuses a definition into it, under --load and in an evaluated source.
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
30000 constant SLV-TIMEOUT-MS
ENGINE-ERROR:SEAL-VIOLATION constant SLV-SEAL-RC

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

: SLV-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: src:ptr u:n dst:ptr lenp:ptr :}
   u PATH-CAP > if E-FS-CAPACITY throw then
   src dst u BYTE-COPY
   u lenp ! ;

: SLV-ROOT ( -- ptr u8 n )
   SLV-ROOT-BUF SLV-ROOT-U @ ;

: SLV-CHILD ( -- ptr u8 n )
   SLV-CHILD-BUF SLV-CHILD-U @ ;

: SLV-LF ( -- )
   10 SB-APPEND-C ;

\ Legit FORGET marks live in the code region (>= DATA-START, in the DBASE region),
\ so the cp!/ndict! guards must leave a real save-mark/roll-back/redefine intact.
: SLV-FORGET-FORGE$ ( -- ptr u8 n )
   SB-RESET
   s" : SLF-A ( -- n ) 1 ;" SB-APPEND SLV-LF
   s" cp@ ndict@" SB-APPEND SLV-LF                 \ save code/dict marks
   s" : SLF-B ( -- n ) 2 ;" SB-APPEND SLV-LF       \ define past the marks
   s" ndict! cp!" SB-APPEND SLV-LF                 \ FORGET: roll both back
   s" : SLF-C ( -- n ) SLF-A 3 + ;" SB-APPEND SLV-LF
   s" SLF-C . cr" SB-APPEND SLV-LF
   SB$ ;

\ Sealed-dictionary truncation guard: FORGET-DEFS-FROM and HIDE-DEFS-FROM
\ truncate ndict (FORGET also rewinds CP) to a name's dict index. An index below
\ the seal-time watermark SEAL-NDICT-CELL (SEAL-CAPTURE, at the end of the
\ engine source) would retire engine definitions and rewind CP into engine code,
\ so post-seal source is refused (ENGINE-ERROR:SEAL-VIOLATION). `WORDS` is an
\ engine word (xref.f), well below the watermark.
: SLV-FORGET-ENGINE-FORGE$ ( -- ptr u8 n )
   SB-RESET
   S\" s\" WORDS\" FORGET-DEFS-FROM" SB-APPEND SLV-LF
   SB$ ;

: SLV-HIDE-ENGINE-FORGE$ ( -- ptr u8 n )
   SB-RESET
   S\" s\" WORDS\" HIDE-DEFS-FROM" SB-APPEND SLV-LF
   SB$ ;

\ The engine-prefix tail: src/os/script-argv.f loads after xref.f, and
\ SEAL-CAPTURE sits at the true engine-prefix end, so the tail traps like any
\ engine definition.
: SLV-FORGET-TAIL-FORGE$ ( -- ptr u8 n )
   SB-RESET
   S\" s\" SCRIPT-ARGV$\" FORGET-DEFS-FROM" SB-APPEND SLV-LF
   SB$ ;

: SLV-HIDE-TAIL-FORGE$ ( -- ptr u8 n )
   SB-RESET
   S\" s\" SCRIPT-ARGV$\" HIDE-DEFS-FROM" SB-APPEND SLV-LF
   SB$ ;

\ Public ndict! may forget user definitions, but it cannot retire the sealed
\ engine. The attack continues from already compiled code and would evaluate a
\ reserved-package publication attempt if the dictionary sink failed open.
: SLV-NDICT-CONTINUATION-FORGE$ ( -- ptr u8 n )
   SB-RESET
   s" TRUSTED: SLF-NDICT-ATTACK ( -- )" SB-APPEND SLV-LF
   s"    0 ndict!" SB-APPEND SLV-LF
   S\"    s\" package TFAM public : SLF-PWN ( -- ) ; ;package\" evaluate ;" SB-APPEND SLV-LF
   s" SLF-NDICT-ATTACK" SB-APPEND SLV-LF
   SB$ ;

\ record-facts! refuses a record below the floor: record 0 is a kernel word's.
: SLV-RECFACTS-FORGE$ ( -- ptr u8 n )
   SB-RESET
   s" : SLF-RF ( -- ) 0 0 record-facts! ; SLF-RF" SB-APPEND SLV-LF
   SB$ ;

\ Direct checker signature-registry truncation bypasses the FORGET/HIDE index
\ guard but would forget an engine word's checker signature so it could be
\ redefined (a spoof). CHECKER-USIGS-TRUNCATE-FROM refuses any post-seal call.
: SLV-USIG-TRUNC-FORGE$ ( -- ptr u8 n )
   SB-RESET
   S\" s\" WORDS\" CHECKER-USIGS-TRUNCATE-FROM" SB-APPEND SLV-LF
   SB$ ;

\ A user mark defined after the seal is above the watermark, so FORGET/HIDE of
\ it stays a working round-trip (retire the user tail, redefine).
: SLV-FORGET-USER-FORGE$ ( -- ptr u8 n )
   SB-RESET
   s" : SLF-UMARK ( -- ) ;" SB-APPEND SLV-LF
   s" : SLF-UA ( -- n ) 1 ;" SB-APPEND SLV-LF
   S\" s\" SLF-UMARK\" FORGET-DEFS-FROM" SB-APPEND SLV-LF
   s" : SLF-UB ( -- n ) 2 ;" SB-APPEND SLV-LF
   s" SLF-UB . cr" SB-APPEND SLV-LF
   SB$ ;

: SLV-HIDE-USER-FORGE$ ( -- ptr u8 n )
   SB-RESET
   s" : SLF-UMARK ( -- ) ;" SB-APPEND SLV-LF
   s" : SLF-UA ( -- n ) 1 ;" SB-APPEND SLV-LF
   S\" s\" SLF-UMARK\" HIDE-DEFS-FROM" SB-APPEND SLV-LF
   SB$ ;

: SLV-STORE! ( len len PROCESS:ending ptr u8 n -- ) {: outu:len erru:len oc src:ptr u:n :}
   erru LEN>N SLV-ERR-U !  outu LEN>N SLV-OUT-U !
   oc MATCH PROCESS:ending
     exited OF SLV-RC ! 0 0= SLV-EXITED ! ENDOF
     signaled OF SLV-RC ! 0 0= 0= SLV-EXITED ! ENDOF
     timeout OF src u SLV-OUT SLV-OUT-U @ SLV-ERR SLV-ERR-U @ T-TIMED-OUT ENDOF
   ;MATCH ;

: SLV-RUN-LOAD ( ptr u8 n -- ) {: src:ptr u:n :}
   SLV-CHILD src u FS:WRITE-ALL
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   SLV-CHILD >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN  SLV-EMPTY 0 >LEN  SLV-OUT SLV-CAP >LEN
   SLV-ERR SLV-CAP >LEN  SLV-TIMEOUT-MS >MS  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   src u SLV-STORE! ;

\ Fixtures run in SUBJECT forks of the sealed candidate.
: SLV-SUBJECT ( ptr u8 n -- ) {: src:ptr u:n :}
   src u SLV-OUT SLV-CAP >LEN SLV-ERR SLV-CAP >LEN
   SLV-TIMEOUT-MS >MS SUBJECT:RUN src u SLV-STORE! ;

: SLV-ASSERT-SEAL ( -- )
   SLV-EXITED @ TTRUE
   SLV-RC @ SLV-SEAL-RC T= ;

: SLV-ASSERT-OK ( -- )
   SLV-EXITED @ TTRUE
   SLV-RC @ 0 T= ;

: SLV-ERR$ ( -- ptr u8 n )  SLV-ERR SLV-ERR-U @ ;

\ prot-wid-add refuses an id at or above PROT-WID-MAX, the protected-WID
\ bitmap's bound, and exits 84 rather than writing outside it, printing the id.
: SLV-ASSERT-PWID-OOR ( ptr u8 n -- ) {: want:ptr wantu:n :}
   SLV-EXITED @ TTRUE
   SLV-RC @ ENGINE-ERROR:SEAL-PACKAGE T=
   SLV-ERR$ want wantu CONTAINS? TTRUE ;

: SLV-PWID-BOUND ( -- )
   s" prot-wid-add refuses an id at the bitmap bound" T-LABEL
   s" PROT-WID-MAX prot-wid-add" SLV-RUN-LOAD
   s" E-SEAL-PACKAGE habu: wid at or above PROT-WID-MAX: 0000000000002000 lr "
   SLV-ASSERT-PWID-OOR
   s" and prints the id past it, not the bound" T-LABEL
   s" PROT-WID-MAX 1+ prot-wid-add" SLV-RUN-LOAD
   s" E-SEAL-PACKAGE habu: wid at or above PROT-WID-MAX: 0000000000002001 lr "
   SLV-ASSERT-PWID-OOR ;

\ Publishing into a protected WID dies 84 with the line
\ `E-SEALED-PACKAGE habu: <path>[:<line>]: <prose>`, its path the loading file's.
: SLV-ASSERT-PROT-PUBLISH ( ptr u8 n -- ) {: prose:ptr proseu:n :}
   SLV-EXITED @ TTRUE
   SLV-RC @ ENGINE-ERROR:SEAL-PACKAGE T=
   SLV-ERR$ s" E-SEALED-PACKAGE habu: " CONTAINS? TTRUE
   SLV-ERR$ prose proseu CONTAINS? TTRUE ;

\ The two engine-reserved OWNER-API wordlists are permanently protected, so a
\ checked program cannot forge a record into them with set-current. Without this,
\ `2 set-current : X ;` would stamp X with OWNER-API-PRI-WID, the marker that
\ seals a record as a system-private engine helper and hides it from raw word
\ searches.
: SLV-OWNER-PRI-FORGE$ ( -- ptr u8 n )       \ publish into the private engine-API wordlist (helper marker)
   SB-RESET
   s" 2 set-current : OWNERFORGE ( -- ) ;" SB-APPEND SLV-LF
   SB$ ;

: SLV-OWNER-PUB-FORGE$ ( -- ptr u8 n )       \ publish into the public engine-API wordlist
   SB-RESET
   s" 1 set-current : OWNERFORGE ( -- ) ;" SB-APPEND SLV-LF
   SB$ ;

: SLV-OWNER-FORGE ( -- )
   s" : X ; via 2 set-current into OWNER-API-PRI-WID -> labeled exit 84" T-LABEL
   SLV-OWNER-PRI-FORGE$ SLV-SUBJECT s" 'OWNERFORGE' is sealed" SLV-ASSERT-PROT-PUBLISH
   s" : X ; via 1 set-current into OWNER-API-PUB-WID -> labeled exit 84" T-LABEL
   SLV-OWNER-PUB-FORGE$ SLV-SUBJECT s" 'OWNERFORGE' is sealed" SLV-ASSERT-PROT-PUBLISH ;

: SLV-LINE ( ptr u8 n -- )
   SB-APPEND SLV-LF ;

\ Package SP with A in its public wordlist, sealed and left open.
: SLV-SP-SEALED ( -- )
   SB-RESET
   s" package SP" SLV-LINE  s" public" SLV-LINE  s" : A ( -- ) ;" SLV-LINE
   s" seal-package" SLV-LINE ;

: SLV-SP-COLON$ ( -- ptr u8 n )
   SLV-SP-SEALED  s" ;package" SLV-LINE  s" : SP:B ( -- ) ;" SLV-LINE  SB$ ;

: SLV-SP-VAR$ ( -- ptr u8 n )
   SLV-SP-SEALED  s" ;package" SLV-LINE  s" variable SP:V" SLV-LINE  SB$ ;

: SLV-SP-INSIDE$ ( -- ptr u8 n )
   SLV-SP-SEALED  s" : C ( -- ) ;" SLV-LINE  s" ;package" SLV-LINE  SB$ ;

: SLV-SP-EXPORT$ ( -- ptr u8 n )
   SB-RESET
   s" package SP2" SLV-LINE  s" public" SLV-LINE  s" : Z ( -- ) ;" SLV-LINE
   s" ;package" SLV-LINE
   s" package SP" SLV-LINE  s" public" SLV-LINE  s" : A ( -- ) ;" SLV-LINE
   s" seal-package" SLV-LINE  s" EXPORT SP2:Z" SLV-LINE  s" ;package" SLV-LINE  SB$ ;

\ A sealed package's record refuses a definition, a variable and an export into its
\ wordlists, and a bare name inside it, naming the name token as written.
: SLV-SEALED-DEFINE ( -- )
   s" : SP:B after SP's seal-package -> E-SEALED-PACKAGE, 'SP:B' is sealed, exit 84" T-LABEL
   SLV-SP-COLON$ SLV-RUN-LOAD s" 'SP:B' is sealed" SLV-ASSERT-PROT-PUBLISH
   s" variable SP:V after SP's seal-package -> E-SEALED-PACKAGE, 'SP:V' is sealed, exit 84" T-LABEL
   SLV-SP-VAR$ SLV-SUBJECT s" 'SP:V' is sealed" SLV-ASSERT-PROT-PUBLISH
   s" : C inside SP after its seal-package -> E-SEALED-PACKAGE, 'C' is sealed, exit 84" T-LABEL
   SLV-SP-INSIDE$ SLV-SUBJECT s" 'C' is sealed" SLV-ASSERT-PROT-PUBLISH
   s" EXPORT SP2:Z into SP after its seal-package -> E-SEALED-PACKAGE, 'SP2:Z' is sealed, exit 84" T-LABEL
   SLV-SP-EXPORT$ SLV-SUBJECT s" 'SP2:Z' is sealed" SLV-ASSERT-PROT-PUBLISH ;

\ hb's build seals every engine package, MACHO with no seal-package in its source.
: SLV-ENGINE-SEALED ( -- )
   s" package MACHO in hb -> E-SEALED-PACKAGE, 'MACHO' is sealed, exit 84" T-LABEL
   s" package MACHO ;package" SLV-RUN-LOAD s" 'MACHO' is sealed" SLV-ASSERT-PROT-PUBLISH ;

: SLV-PREPARE ( -- )
   FS:CLEANUP-RESET
   s" habu-seal" FS:MKTEMP-DIR {: a:ptr u:n :}
   a u SLV-ROOT-BUF SLV-ROOT-U SLV-COPY!
   SLV-ROOT FS:CLEANUP-TREE+
   SLV-ROOT s" forge.f" SLV-CHILD-BUF FS:JOIN-PATH SLV-CHILD-U ! ;

: SLV-CLEANUP ( -- )
   FS:CLEANUP-RUN
   SLV-ROOT FS:EXISTS? TFALSE ;

: SLV-NEGATIVES-TRUNCATE ( -- )
   s" FORGET-DEFS-FROM of an engine def traps" T-LABEL
   SLV-FORGET-ENGINE-FORGE$ SLV-SUBJECT SLV-ASSERT-SEAL
   s" HIDE-DEFS-FROM of an engine def traps" T-LABEL
   SLV-HIDE-ENGINE-FORGE$ SLV-SUBJECT SLV-ASSERT-SEAL
   s" direct CHECKER-USIGS-TRUNCATE-FROM traps" T-LABEL
   SLV-USIG-TRUNC-FORGE$ SLV-SUBJECT SLV-ASSERT-SEAL
   s" FORGET-DEFS-FROM of the script-argv tail traps" T-LABEL
   SLV-FORGET-TAIL-FORGE$ SLV-SUBJECT SLV-ASSERT-SEAL
   s" HIDE-DEFS-FROM of the script-argv tail traps" T-LABEL
   SLV-HIDE-TAIL-FORGE$ SLV-SUBJECT SLV-ASSERT-SEAL
   s" public ndict! continuation cannot cross the engine watermark" T-LABEL
   SLV-NDICT-CONTINUATION-FORGE$ SLV-SUBJECT SLV-ASSERT-SEAL
   s" record-facts! of a sealed record traps" T-LABEL
   SLV-RECFACTS-FORGE$ SLV-SUBJECT SLV-ASSERT-SEAL ;

: SLV-POSITIVES ( -- )
   s" legit cp!/ndict! FORGET round-trip still works" T-LABEL
   SLV-FORGET-FORGE$ SLV-SUBJECT SLV-ASSERT-OK
   s" FORGET-DEFS-FROM of a post-seal user mark still works" T-LABEL
   SLV-FORGET-USER-FORGE$ SLV-SUBJECT SLV-ASSERT-OK
   s" HIDE-DEFS-FROM of a post-seal user mark still works" T-LABEL
   SLV-HIDE-USER-FORGE$ SLV-SUBJECT SLV-ASSERT-OK ;

public

: RUN ( -- )
   T-RESET
   SLV-PREPARE
   SLV-NEGATIVES-TRUNCATE
   SLV-POSITIVES
   SLV-PWID-BOUND
   SLV-OWNER-FORGE
   SLV-SEALED-DEFINE
   SLV-ENGINE-SEALED
   SLV-CLEANUP
   T-REPORT
   s" seal-test: ok" type cr ;

;package

SEAL-SUITE:RUN
