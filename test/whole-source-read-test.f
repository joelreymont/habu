\ whole-source-read-test.f - a whole-source read (src/lib/source.f
\ READ-WHOLE-SAMPLED, which READ-WHOLE-FILE and every reader that takes a size
\ itself call) reads the file to its end from any size sample: a short one, as
\ when the file grew after its size was taken, and a long one, as when it
\ shrank. The language server counts a file it cannot read as empty
\ (test/whole-source-read-child.f, run in a fork child).

require src/lib/string.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/source.f
require src/lib/test/subject.f
require test/whole-source-read-child.f

package WHOLE-SOURCE-READ-TEST
private

$10000 constant IO-CAP
\ Many first rooms (src/lib/source.f SOURCE-ROOM-MIN): read from a short
\ sample, the room doubles five times.
100000 constant TEXT-LEN

create ROOT PATH-CAP allot      variable ROOT-U
create SUBJECT PATH-CAP allot   variable SUBJECT-U
create MISSING PATH-CAP allot   variable MISSING-U
create TEXT TEXT-LEN allot
create OUT IO-CAP allot
create ERR IO-CAP allot
DYNAMIC-BUFFER READ-BUF u8

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;
: SUBJECT$ ( -- ptr u8 n ) SUBJECT SUBJECT-U @ ;
: MISSING$ ( -- ptr u8 n ) MISSING MISSING-U @ ;

: READ-ROOM ( n -- ptr u8 )
   READ-BUF-RESERVE 0 READ-BUF ;


\ The subject is TEXT, lines of 63 letters; missing.f beside it is never
\ written.
: SETUP ( -- )
   FS:CLEANUP-RESET
   s" habu-whole-source-read" FS:MKTEMP-DIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY  u ROOT-U !
   ROOT$ FS:CLEANUP-TREE+
   ROOT$ s" subject.f" SUBJECT FS:JOIN-PATH SUBJECT-U !
   ROOT$ s" missing.f" MISSING FS:JOIN-PATH MISSING-U !
   TEXT-LEN 0 ?do
      i 64 mod 63 = if $0a else i 26 mod [char] a + then TEXT i + c!
   loop
   SUBJECT$ TEXT TEXT-LEN FS:WRITE-ALL ;


\ The bytes are taken after the read, which may move the buffer.
: SAMPLE-CASE ( n ptr u8 n -- )
   {: first:n label:ptr labelu:n :}
   SUBJECT$ first [: READ-ROOM ;] SOURCE:READ-WHOLE-SAMPLED {: u:n :}
   label labelu T-LABEL
   0 READ-BUF u TEXT TEXT-LEN STR= TTRUE ;


\ The server publishes a packet about a file that does not exist, its first
\ read of it: the file is named on standard error and its text counts as empty.
: LSP-MISSING-CASE ( -- )
   SUBJECT$ MISSING$ WHOLE-SOURCE-READ-CHILD:CASE!
   s" WHOLE-SOURCE-READ-CHILD:MAIN"
   OUT IO-CAP >LEN ERR IO-CAP >LEN PROCESS:WAIT-FOREVER SUBJECT:RUN
   PROCESS:ENDING>RC RC>N
   {: outu:len erru:len rc:n :}
   s" the language server counts a file it cannot read as empty" T-LABEL
   rc 0 T=
   ERR erru LEN>N s" missing.f: not read: throw " CONTAINS? TTRUE
   s" ... and still publishes the packet, ranged at the start" T-LABEL
   OUT outu LEN>N S\" \"range\":{\"start\":{\"line\":0,\"character\":0},\"end\":{\"line\":0,\"character\":0}}" CONTAINS? TTRUE ;

public

: WHOLE-SOURCE-READ-TEST-MAIN ( -- )
   T-RESET
   SETUP
   1 s" a whole-source read reads past a short size sample" SAMPLE-CASE
   TEXT-LEN 2 * s" ... and to the file's end short of a long one" SAMPLE-CASE
   LSP-MISSING-CASE
   READ-BUF-RELEASE
   FS:CLEANUP-RUN
   T-REPORT
   s" whole-source-read-test: ok" type cr ;

;package

WHOLE-SOURCE-READ-TEST:WHOLE-SOURCE-READ-TEST-MAIN
