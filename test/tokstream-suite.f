\ tokstream-suite.f - adversarial fixtures for tools/tokstream.f, the comment-only
\ diff instrument (dot habu-promote-the-comment-1f30b9f3).
\
\ The instrument's whole claim is one equality: a comment-only edit leaves the
\ stream byte-identical, and anything else moves it. Each case writes a fixture to
\ ONE path, runs the real tool through its real entry point
\ (`bin/hb --load tools/tokstream.f -- <path>`) in a child, and compares the
\ captured stream against the base run's. The path never changes between runs, so
\ the F record is constant and the comparison is about content alone.
\
\ The base fixture is built to fool a text search: `--` and an `s" ..."` opener
\ live inside a `\` line comment, a stack effect is spelled inside a string
\ literal, and `(CMP)` is a `(`-initial WORD rather than a comment opener. The
\ comment-strip variant also reflows the surviving source across different lines,
\ because the stream is a token stream and whitespace is not content.
\
\ Run: bin/hb --load test/tokstream-suite.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f

package TOKSTREAM-TEST

65536 constant CAP                    \ the at-cap case captures 64 fixture streams
10000 constant TIMEOUT-MS
64 constant USAGE-RC                  \ ARGV:E-USAGE, the over-cap refusal
65 constant OVER-CAP                  \ one past lib/argv.f ARGV-MAX

variable ROOT-U
variable SRC-U
variable SECOND-U
variable OUT-U
variable ERR-U
variable BASE-U
variable RC
variable EXITED

create ROOT-BUF FS-PATH-CAP allot
create SRC-BUF FS-PATH-CAP allot
create SECOND-BUF FS-PATH-CAP allot
create OUT CAP allot
create ERR CAP allot
create BASE CAP allot
create EMPTY 1 allot

: ROOT$ ( -- ptr u8 n )    ROOT-BUF ROOT-U @ ;
: SRC$ ( -- ptr u8 n )     SRC-BUF SRC-U @ ;
: SECOND$ ( -- ptr u8 n )  SECOND-BUF SECOND-U @ ;
: OUT$ ( -- ptr u8 n )     OUT OUT-U @ ;
: BASE$ ( -- ptr u8 n )    BASE BASE-U @ ;

: HB$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" >LEN PROC-ENV-DEFAULT$? if LEN>N exit then
   2drop
   s" HABU_UNDER_TEST" GETENV dup 0= if
      2drop s" bin/hb" exit
   then ;

\ --- fixture sources ----------------------------------------------------------
\ Every variant below differs from BASE by exactly one edit, so a case that fails
\ names the edit that moved the stream.

: LN ( ptr u8 n -- )   SB-APPEND 10 SB-APPEND-C ;

: BODY ( -- )                                 \ the code every variant shares
   s" package TSF" LN
   s" public" LN
   s" : (CMP) ( n n -- n ) - ;" LN
   s" : ONE ( n -- n ) dup + ;" LN
   S\" : TWO ( -- ) s\" ( n -- n )\" type ;" LN
   s" ;package" LN ;

: BASE-SRC$ ( -- ptr u8 n )
   SB-RESET
   S\" \\ a line comment carrying -- and s\" hello\" where neither is code" LN
   s" ( a plain paren comment )" LN
   s" ( a signature-shaped comment -- with a dash )" LN
   s" .( loud )" LN
   BODY
   SB$ ;

\ Comment-only: the two droppable comments go, the surviving source is reflowed
\ across new lines. The stream must not move.
: STRIP-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" ( a signature-shaped comment -- with a dash )" LN
   s" .( loud )" LN
   s" package TSF" LN
   s" public" LN
   s" : (CMP) ( n n -- n )" LN
   s" - ;" LN
   s" : ONE ( n -- n )" LN
   s" dup" LN
   s" + ;" LN
   S\" : TWO ( -- ) s\" ( n -- n )\" type ;" LN
   s" ;package" LN
   SB$ ;

: HEAD ( -- )                                 \ BASE's comment block, unchanged
   S\" \\ a line comment carrying -- and s\" hello\" where neither is code" LN
   s" ( a plain paren comment )" LN
   s" ( a signature-shaped comment -- with a dash )" LN
   s" .( loud )" LN ;

: CODE-SRC$ ( -- ptr u8 n )                   \ `+` becomes `*`
   SB-RESET HEAD
   s" package TSF" LN
   s" public" LN
   s" : (CMP) ( n n -- n ) - ;" LN
   s" : ONE ( n -- n ) dup * ;" LN
   S\" : TWO ( -- ) s\" ( n -- n )\" type ;" LN
   s" ;package" LN
   SB$ ;

: SIG-SRC$ ( -- ptr u8 n )                    \ ONE's stack effect gains an input
   SB-RESET HEAD
   s" package TSF" LN
   s" public" LN
   s" : (CMP) ( n n -- n ) - ;" LN
   s" : ONE ( n n -- n ) dup + ;" LN
   S\" : TWO ( -- ) s\" ( n -- n )\" type ;" LN
   s" ;package" LN
   SB$ ;

: PAYLOAD-SRC$ ( -- ptr u8 n )                \ only the string's body changes
   SB-RESET HEAD
   s" package TSF" LN
   s" public" LN
   s" : (CMP) ( n n -- n ) - ;" LN
   s" : ONE ( n -- n ) dup + ;" LN
   S\" : TWO ( -- ) s\" ( n -- x )\" type ;" LN
   s" ;package" LN
   SB$ ;

: PRINT-SRC$ ( -- ptr u8 n )                  \ the `.( )` printing comment goes
   SB-RESET
   S\" \\ a line comment carrying -- and s\" hello\" where neither is code" LN
   s" ( a plain paren comment )" LN
   s" ( a signature-shaped comment -- with a dash )" LN
   BODY
   SB$ ;

: ORDER-SRC$ ( -- ptr u8 n )                  \ ONE and TWO swap places
   SB-RESET HEAD
   s" package TSF" LN
   s" public" LN
   s" : (CMP) ( n n -- n ) - ;" LN
   S\" : TWO ( -- ) s\" ( n -- n )\" type ;" LN
   s" : ONE ( n -- n ) dup + ;" LN
   s" ;package" LN
   SB$ ;

: DUP-SRC$ ( -- ptr u8 n )                    \ ONE appears twice
   SB-RESET HEAD
   s" package TSF" LN
   s" public" LN
   s" : (CMP) ( n n -- n ) - ;" LN
   s" : ONE ( n -- n ) dup + ;" LN
   s" : ONE ( n -- n ) dup + ;" LN
   S\" : TWO ( -- ) s\" ( n -- n )\" type ;" LN
   s" ;package" LN
   SB$ ;

\ --- child runs ---------------------------------------------------------------

: STORE! ( len len outcome -- )
   MATCH outcome
     exited OF RC ! 0 0= EXITED ! ENDOF
     signaled OF RC ! 0 0= 0= EXITED ! ENDOF
     timeout OF 0 RC ! 0 0= 0= EXITED ! ENDOF
   ;MATCH
   LEN>N ERR-U !  LEN>N OUT-U ! ;

: ARGV-HEAD ( -- )
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" tools/tokstream.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+ ;

: SPAWN ( -- )
   HB$ >LEN  EMPTY 0 >LEN  OUT CAP >LEN  ERR CAP >LEN  TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE-OUTCOME
   STORE! ;

: WRITE-SRC ( ptr u8 n -- )   SRC$ 2swap WRITE-ALL ;

\ Write the variant, run the tool on the one fixture path, require a clean exit.
: RUN-SRC ( ptr u8 n -- )
   WRITE-SRC
   ARGV-HEAD  SRC$ >LEN PROC-ARGV+
   SPAWN
   EXITED @ TTRUE  RC @ 0 T= ;

: BASE! ( -- )
   BASE-SRC$ RUN-SRC
   OUT BASE OUT-U @ BYTE-COPY
   OUT-U @ BASE-U ! ;

\ --- cases --------------------------------------------------------------------

: MOVES ( ptr u8 n ptr u8 n -- ) {: la:ptr lu:n a:ptr u:n :}
   la lu T-LABEL
   a u RUN-SRC
   OUT$ BASE$ T$<> ;

: CASE-IDENTICAL ( -- )
   s" comment strip plus reflow leaves the stream identical" T-LABEL
   STRIP-SRC$ RUN-SRC
   OUT$ BASE$ T$= ;

: CASE-MOVERS ( -- )
   s" a code-token edit moves the stream" CODE-SRC$ MOVES
   s" a stack-signature edit moves the stream" SIG-SRC$ MOVES
   s" a string-payload edit moves the stream" PAYLOAD-SRC$ MOVES
   s" removing a .( ) printing comment moves the stream" PRINT-SRC$ MOVES
   s" reordering two definitions moves the stream" ORDER-SRC$ MOVES
   s" duplicating a definition moves the stream" DUP-SRC$ MOVES ;

\ A file holding nothing but droppable comments: every byte of it is material the
\ sweep may remove, so its F record is the ONLY record it contributes.
: SILENT-SRC$ ( -- ptr u8 n )
   SB-RESET
   S\" \\ nothing here is a code token" LN
   s" ( not even this one )" LN
   SB$ ;

\ The F record's reason for existing: a file that leaves the sweep must show as a
\ missing record, not as an empty diff. The second file contributes no C, L or S
\ record at all, so ONLY its F record can distinguish the two streams - drop the F
\ record and the two runs are byte-identical.
: CASE-FILE-DROP ( -- )
   s" dropping a comment-only file from the sweep moves the stream" T-LABEL
   BASE-SRC$ WRITE-SRC
   SECOND$ SILENT-SRC$ WRITE-ALL
   ARGV-HEAD  SRC$ >LEN PROC-ARGV+  SECOND$ >LEN PROC-ARGV+
   SPAWN
   EXITED @ TTRUE  RC @ 0 T=
   OUT BASE OUT-U @ BYTE-COPY  OUT-U @ BASE-U !     \ the two-file stream
   ARGV-HEAD  SRC$ >LEN PROC-ARGV+
   SPAWN
   EXITED @ TTRUE  RC @ 0 T=
   OUT$ BASE$ T$<> ;

\ Over the ARGV cap the tool must refuse LOUDLY. An empty stream compares equal to
\ another empty stream, so a silent refusal would read as "comment-only": the exit
\ code is the only thing standing between a sweep and a false pass.
: CASE-OVER-CAP ( -- )
   s" past the path cap: exit 64, empty stream, named on stderr" T-LABEL
   BASE-SRC$ WRITE-SRC
   ARGV-HEAD
   OVER-CAP 0 ?do SRC$ >LEN PROC-ARGV+ loop
   SPAWN
   EXITED @ TTRUE
   RC @ USAGE-RC T=
   OUT-U @ 0 T=
   ERR ERR-U @ s" too many positional arguments" CONTAINS? TTRUE ;

\ One under the cap still runs, so the refusal is the cap and not the tool giving
\ up on a long list.
: CASE-AT-CAP ( -- )
   s" at the path cap the sweep still runs" T-LABEL
   BASE-SRC$ WRITE-SRC
   ARGV-HEAD
   OVER-CAP 1- 0 ?do SRC$ >LEN PROC-ARGV+ loop
   SPAWN
   EXITED @ TTRUE
   RC @ 0 T=
   OUT-U @ 0 T<> ;

: PREPARE ( -- )
   CLEANUP-RESET
   s" habu-tokstream" TMPDIR-MKDIR {: a:ptr u:n :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   a ROOT-BUF u BYTE-COPY  u ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT$ s" fixture.f" SRC-BUF JOIN-PATH SRC-U !
   ROOT$ s" second.f" SECOND-BUF JOIN-PATH SECOND-U ! ;

: CLEANUP ( -- )
   CLEANUP-RUN
   ROOT$ EXISTS? TFALSE ;

T-RESET
PREPARE
BASE!
CASE-IDENTICAL
CASE-MOVERS
CASE-FILE-DROP
CASE-OVER-CAP
CASE-AT-CAP
CLEANUP
T-REPORT

;package

s" tokstream-suite: ok" type cr
