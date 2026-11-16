\ seal.f - seal regressions: post-seal FORGET and HIDE of engine words are
\ rejected (E-SEAL-VIOLATION, status 70), post-seal cp! and record writes stop
\ with ENGINE-ERROR:SEAL-VIOLATION, and a sealed package rejects a definition
\ into it, under --load and in an evaluated source.
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

: SLV-COPY! ( ^u8 n ^u8 ^n -- ) {: src:^ u:n dst:^ lenp:^ :}
   u PATH-CAP > if E-FS-CAPACITY throw then
   src dst u BYTE-COPY
   u lenp ! ;

: SLV-ROOT ( -- ^u8 n )
   SLV-ROOT-BUF SLV-ROOT-U @ ;

: SLV-CHILD ( -- ^u8 n )
   SLV-CHILD-BUF SLV-CHILD-U @ ;

: SLV-LF ( -- )
   10 SB-APPEND-C ;

\ Legit FORGET marks live in the code region (>= DATA-START, in the DBASE region),
\ so the cp!/ndict! guards must leave a real save-mark/roll-back/redefine intact.
: SLV-FORGET-FORGE$ ( -- ^u8 n )
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
\ so post-seal source is rejected (E-SEAL-VIOLATION). `WORDS` is an
\ engine word (xref.f), well below the watermark.
: SLV-FORGET-ENGINE-FORGE$ ( -- ^u8 n )
   SB-RESET
   S\" s\" WORDS\" XREF:FORGET-DEFS-FROM" SB-APPEND SLV-LF
   SB$ ;

: SLV-HIDE-ENGINE-FORGE$ ( -- ^u8 n )
   SB-RESET
   S\" s\" WORDS\" XREF:HIDE-DEFS-FROM" SB-APPEND SLV-LF
   SB$ ;

\ The engine-prefix tail: src/os/script-argv.f loads after xref.f, and
\ SEAL-CAPTURE sits at the true engine-prefix end, so the tail is rejected like
\ any engine definition.
: SLV-FORGET-TAIL-FORGE$ ( -- ^u8 n )
   SB-RESET
   S\" s\" SCRIPT-ARGV$\" XREF:FORGET-DEFS-FROM" SB-APPEND SLV-LF
   SB$ ;

: SLV-HIDE-TAIL-FORGE$ ( -- ^u8 n )
   SB-RESET
   S\" s\" SCRIPT-ARGV$\" XREF:HIDE-DEFS-FROM" SB-APPEND SLV-LF
   SB$ ;

\ Public ndict! may forget user definitions, but it cannot retire the sealed
\ engine. The attack continues from already compiled code and would evaluate a
\ reserved-package publication attempt if the dictionary sink failed open.
: SLV-NDICT-CONTINUATION-FORGE$ ( -- ^u8 n )
   SB-RESET
   s" TRUSTED: SLF-NDICT-ATTACK ( -- )" SB-APPEND SLV-LF
   s"    0 ndict!" SB-APPEND SLV-LF
   S\"    s\" package TFAM public : SLF-PWN ( -- ) ; ;package\" evaluate ;" SB-APPEND SLV-LF
   s" SLF-NDICT-ATTACK" SB-APPEND SLV-LF
   SB$ ;

\ record-facts! refuses a record below the floor: record 0 is a kernel word's.
: SLV-RECFACTS-FORGE$ ( -- ^u8 n )
   SB-RESET
   s" : SLF-RF ( -- ) 0 0 record-facts! ; SLF-RF" SB-APPEND SLV-LF
   SB$ ;

\ A user mark defined after the seal is above the watermark, so FORGET/HIDE of
\ it stays a working round-trip (retire the user tail, redefine).
: SLV-FORGET-USER-FORGE$ ( -- ^u8 n )
   SB-RESET
   s" : SLF-UMARK ( -- ) ;" SB-APPEND SLV-LF
   s" : SLF-UA ( -- n ) 1 ;" SB-APPEND SLV-LF
   S\" s\" SLF-UMARK\" XREF:FORGET-DEFS-FROM" SB-APPEND SLV-LF
   s" : SLF-UB ( -- n ) 2 ;" SB-APPEND SLV-LF
   s" SLF-UB . cr" SB-APPEND SLV-LF
   SB$ ;

: SLV-HIDE-USER-FORGE$ ( -- ^u8 n )
   SB-RESET
   s" : SLF-UMARK ( -- ) ;" SB-APPEND SLV-LF
   s" : SLF-UA ( -- n ) 1 ;" SB-APPEND SLV-LF
   S\" s\" SLF-UMARK\" XREF:HIDE-DEFS-FROM" SB-APPEND SLV-LF
   SB$ ;

: SLV-STORE! ( len len PROCESS:ending ^u8 n -- ) {: outu:len erru:len oc src:^ u:n :}
   erru LEN>N SLV-ERR-U !  outu LEN>N SLV-OUT-U !
   oc MATCH PROCESS:ending
     exited OF SLV-RC ! 0 0= SLV-EXITED ! ENDOF
     signaled OF SLV-RC ! 0 0= 0= SLV-EXITED ! ENDOF
     timeout OF src u SLV-OUT SLV-OUT-U @ SLV-ERR SLV-ERR-U @ T-TIMED-OUT ENDOF
   ;MATCH ;

: SLV-RUN-LOAD ( ^u8 n -- ) {: src:^ u:n :}
   SLV-CHILD src u FS:WRITE-ALL
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   SLV-CHILD >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN  SLV-EMPTY 0 >LEN  SLV-OUT SLV-CAP >LEN
   SLV-ERR SLV-CAP >LEN  PROCESS:WAIT-FOREVER  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   src u SLV-STORE! ;

\ Fixtures run in SUBJECT forks of the sealed candidate.
: SLV-SUBJECT ( ^u8 n -- ) {: src:^ u:n :}
   src u SLV-OUT SLV-CAP >LEN SLV-ERR SLV-CAP >LEN
   PROCESS:WAIT-FOREVER SUBJECT:RUN src u SLV-STORE! ;

: SLV-ASSERT-RC ( n -- ) {: rc:n :}
   SLV-EXITED @ TTRUE
   SLV-RC @ rc T= ;

: SLV-ASSERT-SEAL ( -- )
   SLV-SEAL-RC SLV-ASSERT-RC ;

: SLV-ERR$ ( -- ^u8 n )  SLV-ERR SLV-ERR-U @ ;

\ The child's record opens with the line L and holds the help line H.
: SLV-ASSERT-REJECT ( ^u8 n ^u8 n -- )
   {: l:^ lu:n h:^ hu:n :}
   CHECKER-REJECT-RC SLV-ASSERT-RC
   SLV-ERR$ l lu STARTS-WITH? TTRUE
   SLV-ERR$ h hu CONTAINS? TTRUE ;

: SLV-FORGET-HELP$ ( -- ^u8 n )
   S\" = help: pass a word this program defined, as in `s\" MY-WORD\" XREF:FORGET-DEFS-FROM`\n" ;

: SLV-HIDE-HELP$ ( -- ^u8 n )
   S\" = help: pass a word this program defined, as in `s\" MY-WORD\" XREF:HIDE-DEFS-FROM`\n" ;

: SLV-ASSERT-OK ( -- )
   SLV-EXITED @ TTRUE
   SLV-RC @ 0 T= ;

\ prot-wid-add refuses an id at or above PROT-WID-MAX, the protected-WID
\ bitmap's bound, and exits 84 rather than writing outside it, printing the id.
: SLV-ASSERT-PWID-OOR ( ^u8 n -- ) {: want:^ wantu:n :}
   SLV-EXITED @ TTRUE
   SLV-RC @ ENGINE-ERROR:SEAL-WORDLIST T=
   SLV-ERR$ want wantu CONTAINS? TTRUE ;

: SLV-PWID-BOUND ( -- )
   s" prot-wid-add refuses an id at the bitmap bound" T-LABEL
   s" PROT-WID-MAX prot-wid-add" SLV-RUN-LOAD
   s" E-SEAL-WORDLIST habu: wid at or above PROT-WID-MAX: 0x0000000000002000 lr 0x"
   SLV-ASSERT-PWID-OOR
   s" and prints the id past it, not the bound" T-LABEL
   s" PROT-WID-MAX 1+ prot-wid-add" SLV-RUN-LOAD
   s" E-SEAL-WORDLIST habu: wid at or above PROT-WID-MAX: 0x0000000000002001 lr 0x"
   SLV-ASSERT-PWID-OOR ;

\ Publishing into a protected WID is rejected with exit 70 and an
\ E-SEALED-PACKAGE record naming the word it would add.
: SLV-ASSERT-PROT-PUBLISH ( ^u8 n -- ) {: prose:^ proseu:n :}
   SLV-EXITED @ TTRUE
   SLV-RC @ CHECKER-REJECT-RC T=
   SLV-ERR$ s" error[E-SEALED-PACKAGE]" CONTAINS? TTRUE
   SLV-ERR$ prose proseu CONTAINS? TTRUE ;

\ The two engine-reserved OWNER-API wordlists are permanently protected, so a
\ checked program cannot forge a record into them with set-current. Without this,
\ `2 set-current : X ;` would stamp X with OWNER-API-PRI-WID, the marker that
\ seals a record as a system-private engine helper and hides it from raw word
\ searches.
: SLV-OWNER-PRI-FORGE$ ( -- ^u8 n )       \ publish into the private engine-API wordlist (helper marker)
   SB-RESET
   s" 2 set-current : OWNERFORGE ( -- ) ;" SB-APPEND SLV-LF
   SB$ ;

: SLV-OWNER-PUB-FORGE$ ( -- ^u8 n )       \ publish into the public engine-API wordlist
   SB-RESET
   s" 1 set-current : OWNERFORGE ( -- ) ;" SB-APPEND SLV-LF
   SB$ ;

: SLV-OWNER-FORGE ( -- )
   s" : X ; via 2 set-current into OWNER-API-PRI-WID -> labeled exit 70" T-LABEL
   SLV-OWNER-PRI-FORGE$ SLV-SUBJECT s" `OWNERFORGE` would change a sealed package" SLV-ASSERT-PROT-PUBLISH
   s" : X ; via 1 set-current into OWNER-API-PUB-WID -> labeled exit 70" T-LABEL
   SLV-OWNER-PUB-FORGE$ SLV-SUBJECT s" `OWNERFORGE` would change a sealed package" SLV-ASSERT-PROT-PUBLISH ;

: SLV-LINE ( ^u8 n -- )
   SB-APPEND SLV-LF ;

\ Package SP with A in its public wordlist, sealed and left open.
: SLV-SP-SEALED ( -- )
   SB-RESET
   s" package SP" SLV-LINE  s" public" SLV-LINE  s" : A ( -- ) ;" SLV-LINE
   s" seal-package" SLV-LINE ;

: SLV-SP-COLON$ ( -- ^u8 n )
   SLV-SP-SEALED  s" ;package" SLV-LINE  s" : SP:B ( -- ) ;" SLV-LINE  SB$ ;

: SLV-SP-VAR$ ( -- ^u8 n )
   SLV-SP-SEALED  s" ;package" SLV-LINE  s" variable SP:V" SLV-LINE  SB$ ;

: SLV-SP-INSIDE$ ( -- ^u8 n )
   SLV-SP-SEALED  s" : C ( -- ) ;" SLV-LINE  s" ;package" SLV-LINE  SB$ ;

: SLV-SP-EXPORT$ ( -- ^u8 n )
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

: SLV-NEGATIVES-TRUNCATE ( -- )
   s" FORGET-DEFS-FROM of an engine def is rejected" T-LABEL
   SLV-FORGET-ENGINE-FORGE$ SLV-SUBJECT
   S\" error[E-SEAL-VIOLATION]: `WORDS` is an engine definition, which `XREF:FORGET-DEFS-FROM` cannot remove\n"
   SLV-FORGET-HELP$ SLV-ASSERT-REJECT
   s" HIDE-DEFS-FROM of an engine def is rejected" T-LABEL
   SLV-HIDE-ENGINE-FORGE$ SLV-SUBJECT
   S\" error[E-SEAL-VIOLATION]: `WORDS` is an engine definition, which `XREF:HIDE-DEFS-FROM` cannot remove\n"
   SLV-HIDE-HELP$ SLV-ASSERT-REJECT
   s" FORGET-DEFS-FROM of the script-argv tail is rejected" T-LABEL
   SLV-FORGET-TAIL-FORGE$ SLV-SUBJECT
   S\" error[E-SEAL-VIOLATION]: `SCRIPT-ARGV$` is an engine definition, which `XREF:FORGET-DEFS-FROM` cannot remove\n"
   SLV-FORGET-HELP$ SLV-ASSERT-REJECT
   s" HIDE-DEFS-FROM of the script-argv tail is rejected" T-LABEL
   SLV-HIDE-TAIL-FORGE$ SLV-SUBJECT
   S\" error[E-SEAL-VIOLATION]: `SCRIPT-ARGV$` is an engine definition, which `XREF:HIDE-DEFS-FROM` cannot remove\n"
   SLV-HIDE-HELP$ SLV-ASSERT-REJECT
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
