\ content-length-test.f - Content-Length framing over real pipes, through lib/content-length.f.
\ Run: bin/hb --load lib/content-length-test.f
\
\ The failure list the module was written against, each line with its test:
\ - two messages back to back, then end of file on the boundary: both bodies
\   exact, then NONE; nothing at all: NONE ................. TEST-BOUNDARY
\ - end of file inside a header line, after a whole header line, inside a
\   body: TRUNCATED, and the reader is spent ................. TEST-TRUNCATED
\ - no Content-Length; a value with letters, a sign, nothing, an inner blank;
\   a value past a cell, with and without leading zeros; a value over the
\   maximum; two Content-Length lines; a line with no colon, no name before it,
\   or a name with a blank or a parenthesis; a Content-Type charset other than
\   UTF-8, bare or quoted; an LF alone, first or last; a CR inside a line; a
\   line over LINE-CAP held whole, or still growing past it: MALFORMED, and the
\   reader is spent ......................................... TEST-MALFORMED
\ - the name in any case, no blank or blanks and tabs around the value,
\   leading zeros, more of them than a cell has digits, other headers before
\   and after, a name of every token punctuation byte, a Content-Type with no
\   charset or with utf-8 or utf8 in any case, bare or quoted, a body of zero
\   bytes, a line of exactly LINE-CAP bytes: read as written ... TEST-FORMS
\ - a body far longer than the pipe and the reader's buffer, from a writer
\   that blocks, then a message after it .................... TEST-BIG
\ - a header and body arriving in pieces, each its own read .... TEST-PIECES
\ - a reader never bound, a buffer under LINE-CAP + 2, a negative maximum,
\   BODY with no body pending, NEXT-LENGTH with one pending: STATE; a span
\   shorter than the body: E-SPAN-CAPACITY .................. TEST-MISUSE
\ - a body exactly the reader's maximum: read; one byte over: MALFORMED
\   ......................................................... TEST-MAXIMUM
\ - a read the kernel refuses, before a header or inside a body the buffer did
\   not hold: E-FS-IO, and the reader is spent ............. TEST-READ-ERROR
\ - SEND: the exact header and body, an empty body, a body built in SB, read
\   back by a reader; a pipe with no reader, SIGPIPE disarmed: E-FS-IO
\   ......................................................... TEST-SEND

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/span.f
require lib/memory.f
require lib/process.f
require lib/process-fork.f
require lib/task.f
require lib/test/outcome.f
require lib/fd-io.f
require lib/fmt.f
require lib/adt/option.f
require lib/content-length.f

package CONTENT-LENGTH-TEST
using CONTENT-LENGTH
using SPAN
using FD-IO
using MEM

$1000000 constant MAX-BODY        \ the reader's maximum: 16 MiB
$100000 constant BIG              \ 1 MiB: far past a pipe and the reader's buffer
251 constant PATTERN-MOD          \ prime, so no power-of-two chunk realigns the pattern
50 constant PIECE-MS              \ how long the piecemeal writer waits between pieces
3000 constant LONG                \ a body longer than one read into the reader's buffer

TYPED-VARIABLE R CONTENT-LENGTH:reader
TYPED-VARIABLE FRESH CONTENT-LENGTH:reader      \ never bound
2048 SPAN-BUFFER: INBUF           \ the reader's buffer
64 SPAN-BUFFER: SMALL             \ the small bodies
TYPED-VARIABLE BIG-BODY SPAN:span<u8>
variable FEED-FD                  \ the read end the reader is bound to
variable CHILD-FD                 \ the pipe end a forked writer writes
variable CHILD-CODE

\ ---- fixtures ---------------------------------------------------------------

: BIND-ON ( fd n -- )
   {: f:fd max:n :}
   R f INBUF max BIND ;

\ A pipe that holds these bytes and then ends; a reader taking bodies up to the
\ maximum reads its read end.
: FEED-MAX ( ptr u8 n n -- )
   {: a:ptr u:n max:n :}
   PIPE-PAIR {: r:fd w:fd :}
   w a u WRITE-FULL
   w FD>N close
   r FD>N FEED-FD !
   r max BIND-ON ;

: FEED ( ptr u8 n -- )
   MAX-BODY FEED-MAX ;

: DONE ( -- )
   FEED-FD @ close ;

\ The next body length, or -1 for the end on a boundary.
: NEXT-LEN ( -- n )
   R NEXT-LENGTH MATCH option
      none OF -1 ENDOF
      some OF ENDOF
   ;MATCH ;

\ The pending body of u bytes, read into SMALL.
: BODY$ ( n -- ptr u8 n )
   {: u:n :}
   R SMALL BODY
   SMALL $ drop u ;

: SPENT ( -- )
   [: NEXT-LEN drop ;] E-CONTENT-LENGTH-STATE TTHROWSQ ;

: PATTERN! ( SPAN:span<u8> -- )
   $ {: p:ptr u:n :}
   u 0 ?do i PATTERN-MOD mod p i + c! loop ;

: PATTERN? ( SPAN:span<u8> -- bool )
   $ {: p:ptr u:n :}
   u 0 ?do p i + c@ i PATTERN-MOD mod <> if false unloop exit then loop
   true ;

: CHILD-EXIT ( n -- )
   s" " rot die ;

\ The rest of a forked child's life: its part runs under catch, and the exit
\ code is 1 for a throw, else the code the part chose.
: IN-CHILD ( [ -- ] -- )
   catch {: code:n :}
   code 0<> if 1 CHILD-EXIT then
   CHILD-CODE @ CHILD-EXIT ;

\ Runs a writer part in a forked child on a fresh pipe and opens the reader on
\ the read end; the child's pid comes back for the case to reap.
: FORKED ( [ -- ] -- pid )
   {: part :}
   PIPE-PAIR {: r:fd w:fd :}
   w FD>N CHILD-FD !
   0 CHILD-CODE !
   PROC-FORK:CHECKED {: pid:pid :}
   pid PID>N 0= if r FD>N close part IN-CHILD then
   w FD>N close
   r FD>N FEED-FD !
   r MAX-BODY BIND-ON
   pid ;

\ ---- cases ------------------------------------------------------------------

: TEST-BOUNDARY ( -- )
   s\" Content-Length: 2\r\n\r\n{}Content-Length: 5\r\n\r\nhello" FEED
   NEXT-LEN 2 T=  2 BODY$ s" {}" T$=
   NEXT-LEN 5 T=  5 BODY$ s" hello" T$=
   NEXT-LEN -1 T=
   DONE
   s" " FEED
   NEXT-LEN -1 T=
   DONE ;

: TRUNCATED-HEADER ( ptr u8 n -- )
   FEED
   [: NEXT-LEN drop ;] E-CONTENT-LENGTH-TRUNCATED TTHROWSQ
   SPENT
   DONE ;

: TEST-TRUNCATED ( -- )
   s" Content-Len" TRUNCATED-HEADER
   s\" Content-Length: 2\r\n" TRUNCATED-HEADER
   s\" Content-Length: 10\r\n\r\n{}" FEED
   NEXT-LEN 10 T=
   [: 10 BODY$ 2drop ;] E-CONTENT-LENGTH-TRUNCATED TTHROWSQ
   SPENT
   DONE ;

: MALFORMED ( ptr u8 n -- )
   FEED
   [: NEXT-LEN drop ;] E-CONTENT-LENGTH-MALFORMED TTHROWSQ
   SPENT
   DONE ;

\ A header line of an X-Pad name and u - 7 bytes of value: u bytes in all,
\ ended by CR LF, then a Content-Length line and a body.
$1000 constant PAD-CAP
PAD-CAP BUFFER: PAD-BUF
TYPED-VARIABLE PAD-LEN len

: PAD-LINE$ ( n -- ptr u8 n )
   {: u:n :}
   PAD-LEN BUF-RESET
   s" X-Pad: " PAD-BUF PAD-CAP PAD-LEN BUF-APPEND
   u 7 - 0 ?do [char] a PAD-BUF PAD-CAP PAD-LEN BUF-APPEND-C loop
   s\" \r\nContent-Length: 2\r\n\r\n{}" PAD-BUF PAD-CAP PAD-LEN BUF-APPEND
   PAD-BUF PAD-LEN BUF-LEN@ ;

: TEST-MALFORMED ( -- )
   s\" Content-Type: application/vscode-jsonrpc\r\n\r\n{}" MALFORMED
   s\" Content-Length: abc\r\n\r\n" MALFORMED
   s\" Content-Length: -2\r\n\r\n{}" MALFORMED
   s\" Content-Length: +2\r\n\r\n{}" MALFORMED
   s\" Content-Length:\r\n\r\n" MALFORMED
   s\" Content-Length: 1 2\r\n\r\n" MALFORMED
   s\" Content-Length: 99999999999999999999\r\n\r\n" MALFORMED
   s\" Content-Length: 0099999999999999999999\r\n\r\n" MALFORMED
   s\" Content-Length: 16777217\r\n\r\n" MALFORMED
   s\" Content-Length: 2\r\ncontent-length: 2\r\n\r\n{}" MALFORMED
   s\" Content-Length 2\r\n\r\n{}" MALFORMED
   s\" : 2\r\nContent-Length: 2\r\n\r\n{}" MALFORMED
   s\" Bad Name: a\r\nContent-Length: 2\r\n\r\n{}" MALFORMED
   s\" X(y): a\r\nContent-Length: 2\r\n\r\n{}" MALFORMED
   s\" Content-Type: application/vscode-jsonrpc; charset=latin1\r\nContent-Length: 2\r\n\r\n{}" MALFORMED
   s\" Content-Type: application/json;Charset=\"UTF-16\"\r\nContent-Length: 2\r\n\r\n{}" MALFORMED
   s\" Content-Length: 2\n\n{}" MALFORMED
   s\" \nContent-Length: 2\r\n\r\n{}" MALFORMED
   s\" Content-Length: 2\r\r\n\r\n{}" MALFORMED
   LINE-CAP 1+ PAD-LINE$ MALFORMED
   3000 PAD-LINE$ MALFORMED ;

: FORM ( ptr u8 n ptr u8 n -- )
   {: a:ptr u:n want:ptr wu:n :}
   a u FEED
   NEXT-LEN wu T=
   wu BODY$ want wu T$=
   NEXT-LEN -1 T=
   DONE ;

: TEST-FORMS ( -- )
   s\" content-length: 2\r\n\r\n{}" s" {}" FORM
   s\" CONTENT-LENGTH:2\r\n\r\n{}" s" {}" FORM
   s\" Content-Length: \t 007 \t\r\n\r\nabcdefg" s" abcdefg" FORM
   s\" Content-Length: 00000000000000000002\r\n\r\n{}" s" {}" FORM
   s\" Content-Type: x\r\nContent-Length: 3\r\nX-Other: y\r\n\r\nabc" s" abc" FORM
   s\" X-A!#$%&'*+.^_`|~9: a\r\nContent-Length: 2\r\n\r\n{}" s" {}" FORM
   s\" Content-Type: application/vscode-jsonrpc; charset=utf-8\r\nContent-Length: 2\r\n\r\n{}" s" {}" FORM
   s\" Content-Type: application/json; foo=bar;CHARSET=\"UTF8\"\r\nContent-Length: 2\r\n\r\n{}" s" {}" FORM
   LINE-CAP PAD-LINE$ s" {}" FORM
   s\" Content-Length: 0\r\n\r\nContent-Length: 1\r\n\r\nz" FEED
   NEXT-LEN 0 T=  0 BODY$ nip 0 T=
   NEXT-LEN 1 T=  1 BODY$ s" z" T$=
   NEXT-LEN -1 T=
   DONE ;

\ ---- a body longer than everything between writer and reader ---------------

: BIG-WRITER ( -- )
   CHILD-FD @ >FD s\" Content-Length: 1048576\r\n\r\n" WRITE-FULL
   CHILD-FD @ >FD BIG-BODY @ $ WRITE-FULL
   CHILD-FD @ >FD s\" Content-Length: 2\r\n\r\n{}" WRITE-FULL ;

: TEST-BIG ( -- )
   BIG-BODY @ PATTERN!
   [: BIG-WRITER ;] FORKED {: pid:pid :}
   0 BIG-BODY @ FILL
   NEXT-LEN BIG T=
   R BIG-BODY @ BODY
   BIG-BODY @ PATTERN? TTRUE
   NEXT-LEN 2 T=  2 BODY$ s" {}" T$=
   NEXT-LEN -1 T=
   DONE
   pid PROC-WAIT-OUTCOME 0 T-OUTCOME-EXITED= ;

\ ---- a message in pieces ----------------------------------------------------

: PIECE ( ptr u8 n -- )
   {: a:ptr u:n :}
   CHILD-FD @ >FD a u WRITE-FULL
   PIECE-MS >MS TASK:SLEEP ;

: PIECE-WRITER ( -- )
   s" Content-" PIECE
   s\" Length: 3\r" PIECE
   s\" \n\r" PIECE
   s\" \nab" PIECE
   s" c" PIECE ;

: TEST-PIECES ( -- )
   [: PIECE-WRITER ;] FORKED {: pid:pid :}
   NEXT-LEN 3 T=
   3 BODY$ s" abc" T$=
   NEXT-LEN -1 T=
   DONE
   pid PROC-WAIT-OUTCOME 0 T-OUTCOME-EXITED= ;

\ ---- misuse -----------------------------------------------------------------

: FRESH-NEXT ( -- )
   FRESH NEXT-LENGTH MATCH option none OF ENDOF some OF drop ENDOF ;MATCH ;

: SHORT-BIND ( -- )
   R 0 >FD INBUF LINE-CAP 1+ TAKE MAX-BODY BIND ;

: NEGATIVE-BIND ( -- )
   R 0 >FD INBUF -1 BIND ;

: TEST-MISUSE ( -- )
   [: FRESH-NEXT ;] E-CONTENT-LENGTH-STATE TTHROWSQ
   [: SHORT-BIND ;] E-CONTENT-LENGTH-STATE TTHROWSQ
   [: NEGATIVE-BIND ;] E-CONTENT-LENGTH-STATE TTHROWSQ
   s\" Content-Length: 5\r\n\r\nhello" FEED
   [: 0 BODY$ 2drop ;] E-CONTENT-LENGTH-STATE TTHROWSQ
   NEXT-LEN 5 T=
   [: NEXT-LEN drop ;] E-CONTENT-LENGTH-STATE TTHROWSQ
   [: R SMALL 4 TAKE BODY ;] E-SPAN-CAPACITY TTHROWSQ
   5 BODY$ s" hello" T$=
   NEXT-LEN -1 T=
   DONE ;

\ A message whose body is LONG bytes of b.
: LONG$ ( -- ptr u8 n )
   SB-RESET  s" Content-Length: " SB-APPEND  LONG FMT:SB-U  s\" \r\n\r\n" SB-APPEND
   PAD-LEN BUF-RESET
   SB$ PAD-BUF PAD-CAP PAD-LEN BUF-APPEND
   LONG 0 ?do [char] b PAD-BUF PAD-CAP PAD-LEN BUF-APPEND-C loop
   PAD-BUF PAD-LEN BUF-LEN@ ;

: LONG-BODY ( -- )
   R BIG-BODY @ LONG TAKE BODY ;

\ The reader's descriptor is closed under it, before a header and inside a body
\ the buffer did not hold: the kernel refuses the read, and the reader is spent.
: TEST-READ-ERROR ( -- )
   s\" Content-Length: 2\r\n\r\n{}" FEED
   DONE
   [: NEXT-LEN drop ;] E-FS-IO TTHROWSQ
   SPENT
   LONG$ FEED
   NEXT-LEN LONG T=
   DONE
   [: LONG-BODY ;] E-FS-IO TTHROWSQ
   SPENT ;

\ A reader whose maximum is a body's exact length takes it; one byte more is over.
: TEST-MAXIMUM ( -- )
   s\" Content-Length: 5\r\n\r\nhello" 5 FEED-MAX
   NEXT-LEN 5 T=  5 BODY$ s" hello" T$=
   NEXT-LEN -1 T=
   DONE
   s\" Content-Length: 6\r\n\r\nhello!" 5 FEED-MAX
   [: NEXT-LEN drop ;] E-CONTENT-LENGTH-MALFORMED TTHROWSQ
   DONE ;

\ ---- SEND -------------------------------------------------------------------

: SENT$ ( fd n -- ptr u8 n )
   {: r:fd u:n :}
   r SMALL u TAKE READ-EXACT MATCH FD-IO:fill
      full OF ENDOF
      eof OF drop 1 0 T= ENDOF
   ;MATCH
   SMALL $ drop u ;

: NO-READER ( -- )
   CHILD-FD @ >FD s" lost" SEND ;

: TEST-SEND ( -- )
   PIPE-PAIR {: r:fd w:fd :}
   w s" hello" SEND
   r 26 SENT$ s\" Content-Length: 5\r\n\r\nhello" T$=
   w s" " SEND
   r 21 SENT$ s\" Content-Length: 0\r\n\r\n" T$=
   SB-RESET s" {}" SB-APPEND
   w SB$ SEND
   r 23 SENT$ s\" Content-Length: 2\r\n\r\n{}" T$=
   w s" {}" SEND
   w s" " SEND
   w FD>N close
   r FD>N FEED-FD !
   r MAX-BODY BIND-ON
   NEXT-LEN 2 T=  2 BODY$ s" {}" T$=
   NEXT-LEN 0 T=  0 BODY$ nip 0 T=
   NEXT-LEN -1 T=
   DONE
   PIPE-PAIR {: r2:fd w2:fd :}
   w2 FD-NOSIGPIPE!
   r2 FD>N close
   w2 FD>N CHILD-FD !
   [: NO-READER ;] E-FS-IO TTHROWSQ
   w2 FD>N close ;

: TEST-MAIN ( -- )
   BIG BYTES-ALLOC-LEN ALLOC-SPAN BIG-BODY !
   T-RESET
   TEST-BOUNDARY
   TEST-TRUNCATED
   TEST-MALFORMED
   TEST-FORMS
   TEST-BIG
   TEST-PIECES
   TEST-MISUSE
   TEST-MAXIMUM
   TEST-READ-ERROR
   TEST-SEND
   BIG-BODY @ FREE-SPAN
   T-REPORT
   s" content-length-test: ok" type cr ;

TEST-MAIN

;using
;using
;using
;using
;package
