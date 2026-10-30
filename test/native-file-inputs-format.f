\ A truncated answer in a query row must refuse before reading past its bytes.
\ A source row's stat stands for its bytes; only a changed stat hashes them.
\ A row recorded after its file moved off the bytes read keeps no stat.

require src/lib/test.f
require src/lib/le.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require tools/native-file-inputs.f

package NFILE-INPUTS-FORMAT-TEST

create ROW 72 allot
create SOURCE-PATH PATH-CAP allot
variable SOURCE-U
variable OUTER-CALLS
variable INNER-CALLS
4096 constant CAP
create OUT CAP allot
create ERR CAP allot
create SROWS 256 allot
variable SROWS-U

PROCESS-SYMBOLS
FUNCTION: MKFIFO-CALL mkfifo ( ptr u8 n -- i32 ) ;FUNCTION

: SOURCE$ ( -- ptr u8 n ) SOURCE-PATH SOURCE-U @ ;

: OUTER-CANON ( ptr u8 n -- ptr u8 n bool )
   1 OUTER-CALLS +! SOURCE-ROOT:CANON-OS ;
: INNER-CANON ( ptr u8 n -- ptr u8 n bool )
   1 INNER-CALLS +! SOURCE-ROOT:CANON-OS ;
: SCOPE-FAIL ( -- )
   SOURCE$ SOURCE-INPUT:CANON 2drop drop
   E-NUNIT-PROFILE throw ;

: CALLBACK-SCOPE ( -- )
   0 OUTER-CALLS ! 0 INNER-CALLS !
   [: OUTER-CANON ;] [: SOURCE-ROOT:READ-OS ;] SOURCE-INPUT:USE
   [: [: INNER-CANON ;] [: SOURCE-ROOT:READ-OS ;]
      [: SCOPE-FAIL ;] SOURCE-INPUT:WITH ;]
      E-NUNIT-PROFILE TTHROWSQ
   INNER-CALLS @ 1 T=
   OUTER-CALLS @ 0 T=
   SOURCE$ SOURCE-INPUT:CANON 2drop drop
   OUTER-CALLS @ 1 T=
   SOURCE-INPUT:RESET ;

: FIFO-CHILD ( -- )
   1 SCRIPT-ARGV$ {: path:ptr pathu:n :}
   path SOURCE-PATH pathu BYTE-COPY pathu SOURCE-U !
   T-RESET
   NFILE-INPUTS:RESET
   SOURCE$ s" " NFILE-INPUTS:ADD-CONTENT
   SOURCE$ FS:REMOVE-FILE
   SOURCE$ FS:>PATHZ 384 MKFIFO-CALL 0 T=
   [: NFILE-INPUTS:BYTES$ NFILE-INPUTS:CHECK ;]
      E-NUNIT-PROFILE TTHROWSQ
   NFILE-INPUTS:CLOSE
   T-REPORT ;

: FIFO-STALE ( -- )
   PROC-ARGV-ENV-RESET
   PROC-ENV-INHERIT-MISSING
   s" --load" >LEN PROC-ARGV+
   s" test/native-file-inputs-format.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" fifo" >LEN PROC-ARGV+
   SOURCE$ >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN s" " >LEN
   OUT CAP >LEN ERR CAP >LEN 10000 >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME PROC-OUTCOME>DEADLINE-RC RC>N
      {: outu:len erru:len rc:n :}
   rc 0<> if OUT outu LEN>N type ERR erru LEN>N type then
   rc 0 T= ;

\ SROWS holds a copy of the recorded source row, to change its stat or digest.
: SROWS$ ( -- ptr u8 n ) SROWS SROWS-U @ ;
: SROWS-COPY ( -- )
   NFILE-INPUTS:BYTES$ {: rows:ptr rowsu:n :}
   rowsu 256 > if T-FAIL exit then
   rows SROWS rowsu BYTE-COPY rowsu SROWS-U ! ;

: SOURCE-CASE ( -- )
   s" a source row whose stat matches is fresh without hashing" T-LABEL
   SOURCE$ s" : A 1 ;" FS:WRITE-ALL
   NFILE-INPUTS:RESET
   SOURCE$ s" : A 1 ;" NFILE-INPUTS:ADD-SOURCE
   SROWS-COPY
   32 0 ?do 0 SROWS 64 SOURCE-U @ + + i + c! loop
   SROWS$ NFILE-INPUTS:FRESH? TTRUE
   s" a changed stat over equal bytes is fresh" T-LABEL
   SROWS-COPY
   SROWS 32 + LE:U64@ 1+ SROWS 32 + LE:U64!
   SROWS$ NFILE-INPUTS:FRESH? TTRUE
   s" changed bytes are stale" T-LABEL
   SOURCE$ s" : A 22 ;" FS:WRITE-ALL
   NFILE-INPUTS:BYTES$ NFILE-INPUTS:FRESH? TFALSE
   s" a truncated source row is malformed, not stale" T-LABEL
   [: SROWS SROWS-U @ 1- NFILE-INPUTS:FRESH? drop ;] E-NUNIT-FILE TTHROWSQ
   s" a source row recorded after its file moved off the bytes read is stale" T-LABEL
   NFILE-INPUTS:RESET
   SOURCE$ s" : A 1 ;" NFILE-INPUTS:ADD-SOURCE
   NFILE-INPUTS:BYTES$ NFILE-INPUTS:FRESH? TFALSE ;

public
: MAIN ( -- )
   SCRIPT-ARGC 0 > if
      0 SCRIPT-ARGV$ s" fifo" STR= if FIFO-CHILD exit then
   then
   T-RESET
   s" FILE input query truncation" T-LABEL
   72 0 ?do 0 ROW i + c! loop
   2 ROW LE:U64!       \ query row
   3 ROW 8 + LE:U64!   \ path length
   3 ROW 16 + LE:U64!  \ answer length
   s" abc" drop ROW 64 + 3 BYTE-COPY
   s" xy" drop ROW 67 + 2 BYTE-COPY
   [: ROW 69 NFILE-INPUTS:FORMAT ;] E-NUNIT-FILE TTHROWSQ
   s" native-input-content" FS:MKTEMP-DIR {: root:ptr rootu:n :}
   root rootu s" source.f" SOURCE-PATH FS:JOIN-PATH SOURCE-U !
   SOURCE$ s" : A 1 ;" FS:WRITE-ALL
   NFILE-INPUTS:RESET
   SOURCE$ s" " NFILE-INPUTS:ADD-CONTENT
   NFILE-INPUTS:BYTES$ NFILE-INPUTS:CHECK
   SOURCE$ s" : A 2 ;" FS:WRITE-ALL
   [: NFILE-INPUTS:BYTES$ NFILE-INPUTS:CHECK ;] E-NUNIT-PROFILE TTHROWSQ
   s" a scoped source callback returns to its caller after a throw" T-LABEL
   CALLBACK-SCOPE
   SOURCE-CASE
   s" a recorded content path replaced by a FIFO is stale" T-LABEL
   FIFO-STALE
   NFILE-INPUTS:CLOSE
   root rootu FS:REMOVE-TREE
   T-REPORT ;

;package

NFILE-INPUTS-FORMAT-TEST:MAIN
