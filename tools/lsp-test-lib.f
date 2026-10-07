\ lsp-test-lib.f - the language server end to end: tools/lsp.f run as a
\ client runs it, one child per conversation.
\ Run: bin/hb --load tools/lsp-test.f
\
\ A conversation is written to the server's stdin whole, and the input then
\ ends; its stdout, stderr and exit code are read after. Every frame the server
\ writes must be `Content-Length: N`, CR LF twice and N bytes, and the replies
\ come in the order of the messages they answer. The rest run over pipes the
\ test holds: answers-at-once keeps the input open, stdout-closed closes the
\ output first, and each diagnostics conversation is held turn by turn, the
\ messages of a turn written together once the server has answered the last
\ and every publish read as it comes, since the server checks a document only
\ when no input waits. The directory the test prints after `artifact:` keeps
\ every conversation byte for byte, NAME.in, NAME.out and NAME.err, and its
\ exit code in `exits`: `bin/hb --load tools/lsp.f < NAME.in` replays one,
\ though a held one's turns then come at once. A conversation whose wait ran
\ out keeps what was read before it.
\
\ How the server could fail, and the conversation that would show it:
\
\ Lifecycle
\ - initialize answers other than its capabilities: positions in utf-16, open
\   and close notifications, Full sync, workspace and document symbols,
\   document highlights, document links without resolve, the name habu .. lifecycle
\ - shutdown answers other than a null result; exit after it exits other than
\   0 or writes to stderr ............................................ lifecycle
\ - a reply held until more input comes, or the end of input .. answers-at-once
\ - exit without shutdown exits 0, after initialize or as the first message
\   ............................................. exit-without-shutdown, exit-first
\ - input ending before exit exits 0 or says nothing ........ eof-before-exit
\ - input ending after shutdown exits other than 0 or says something
\   ...................................................... eof-after-shutdown
\ - a request before initialize served, or answered without its id; a
\   notification before it applied ........................ before-initialize
\ - initialize served twice ................................. initialize-twice
\ - a request after shutdown served; a notification after it applied
\   .......................................................... after-shutdown
\
\ Envelope
\ - an unknown request answered other than -32601; an unknown notification
\   answered ................................................. unknown-method
\ - an id not echoed byte for byte: 0, -7, 1.5e3, 30 digits, a string with
\   escapes, null ....................................................... ids
\ - a body that is not JSON, or empty, answered other than -32700 with id
\   null, or stopping the server .............................. body-not-json
\ - an array, a scalar, jsonrpc "1.0" or a method that is a number answered
\   other than -32600 with the id the message carries; a client's response
\   answered ................................................ invalid-request
\
\ Document sync
\ - open, change and close of a document: the close publishes anything but an
\   empty list for its URI ................................................ sync
\ - a URI matched or echoed as its JSON text instead of decoded (`\/`), or its
\   percent escapes decoded in the echo ................................... sync
\ - a document opened twice held twice, so two closes publish twice ...... sync
\ - a change or close of a document not open applied or silent; a URI that is
\   not a file URI opened; a change with a range applied under Full sync . sync
\ - params absent, of the wrong kind or with a version past a cell, or a URI
\   naming a path that holds a NUL, answered, applied or stopping the server;
\   each must log one line ................................... malformed-params
\
\ Framing
\ - a header form the transport takes refused: a lower-case name, Content-Type
\   before and after, blanks and tabs around the value, leading zeros
\   ............................................................ frame-forms
\ - a framing fault survived or reported without its code: no Content-Length,
\   a value that is no number, a line ended by LF alone, a line over LINE-CAP,
\   a length over the maximum, end of input in a header and in a body; each is
\   its own conversation ........................................... fault-*
\ - a request read before a framing fault not answered before the fault
\   stops the server ..................................... fault-after-request
\ - a 700 KB body, written in many pieces, refused, cut or its document lost:
\   its last line's diagnostic proves the whole text checked ...... big-frame
\ - a closed stdout ending the server by SIGPIPE instead of exit 1 and the
\   throw that stopped it .................................... stdout-closed
\
\ Diagnostics, each checked against the test's own CHECK:VERIFY-BYTES of the
\ same text and path, whose packets, in order, must be the diagnostics' data
\ - an opened document's list not published with its version, or a diagnostic
\   whose range is not its packet's byte offsets in lines and UTF-16 units, or
\   whose severity, code, source, message or data is not the packet's
\   ...................................................... diagnostics-open
\ - a refusal after an undefined word not listed, or a definition that uses
\   the undefined one as declared listed ................... diagnostics-open
\ - a require of a file of the tree not resolved, or a changed document not
\   checked again .................................... diagnostics-require
\ - a change that came while another waited checked on its own, or a version
\   other than the newest published ................. diagnostics-supersede
\ - a save leaving an open document unchecked, or documents waiting together
\   not checked in turn ................................... save-dirties-all
\ - a close publishing anything but an empty list without a version, or a
\   document opened and closed in one turn checked ...................... close
\ - a file the engine provides checked, or its list kept from the client
\   ........................................................ engine-provided
\ - a character counted in bytes instead of UTF-16 units ........ utf16-column
\ - a dependency's packets not published at its canonical file URI with
\   positions in its text on disk, its list not withdrawn once its packets are
\   gone, or published over the list of the open document that holds it
\   ............................................................. dependency
\ - a dependency's list not published again from disk once the document that
\   held it open closes ................................... dependency-close
\ - a dependency's list at its canonical URI kept once the client, naming it
\   through a symlink, opens it fixed ..................... dependency-symlink
\ - a dependency two documents require withdrawn while one still does, not
\   published again by that one's check, or kept once neither does
\   ...................................................... dependency-shared
\ - one document's words seen by another's check ................ two-in-turn
\ - a check that ended without a verdict and wrote no packet publishing, or
\   not saying how it ended ................................ incomplete, held
\ - the packets a check wrote before a later statement ended it without a
\   verdict not published ................................ incomplete-packets
\ - a definition never ended, at which the verifier stops, not said refused
\   with the verifier's prose, or its stop not published at it ...... unended
\ - a definer with nothing after it, at which the verifier stops, not said
\   refused, or the missing name not published at it ........... missing-name
\ - a packet placed at the name of the definition it refuses, a public word
\   whose private twin moves other cells, not published at that name
\   ......................................................... shadowed-arity
\ - a warning, which carries no verdict, published with a severity or not at
\   the name of the definition whose effect it did not record .. not-recorded
\ - a duplicate definition, of the document's own word or of one a file it
\   requires defines, not published at the name defined again
\   ......................................... duplicate, duplicate-of-required
\ - a definition deferred to the run, then one never ended: the deferral or
\   the stop's record not published, or the document not said refused
\   ....................................................... deferred-unended
\
\ Workspace symbols
\ - a definition whose word holds the query, in an open document or a file one
\   requires, missing, or answered with another name, kind, package or range
\   than its line's, or at another URI than the one the client opened its
\   document by or its file's canonical one; a query compared with case
\   ........................................................ workspace-symbol
\ - a definition two open documents' checks reached listed twice, or one a
\   document's last check no longer retains still listed .... workspace-symbol
\ - a file two open documents' checks reached answered from the earlier check,
\   or not from the other once the later one's document closes
\   ........................................................ workspace-symbol
\ - params without a string query answered other than -32602 .. workspace-symbol
\ - a file a later check read with no definition left answered from an
\   earlier check, or still empty once the later one's document closes
\   .................................................. workspace-symbol-empty
\ - a redeclaration whose effect the registrar refused listed beside the
\   declaration it retained ........................ workspace-symbol-retained
\ - a redeclaration too wide to record listed or its refusal not published;
\   the declaration retained before it, a definition after it, an identical
\   redeclaration the registrar retained, or a refused body whose signature
\   the checker kept not listed; a malformed declaration with none before it
\   listed ....................................... workspace-symbol-redeclared
\ - a definition in a file a check reads twice listed twice
\   ........................................... workspace-symbol-reinclude
\ - an open document's definitions answered from another document's check,
\   which read the file from disk, or that check's definitions there placed
\   through the document's text ............ workspace-symbol-open-dependency
\ - a document in a directory whose name is 1, 2, 3 or 4 bytes long stopping the
\   server: its global words, two bytes each, put the empty package each of
\   their definition lines names at every even offset of the bytes the server
\   keeps them in, one of them where those bytes fill their room .. path-length-N
\
\ Document symbols
\ - a definition the document's last completed check retained in it missing,
\   or answered other than as the flat SymbolInformation workspace symbols
\   give it: its name, kind and location at the document's URI over the
\   token that declared it; a definition of another open document or of a
\   file the document requires listed; a document with no definition answered
\   other than an empty list; a document not open, or params naming none,
\   answered other than -32602 ................................. document-symbol
\ - a request in the turn of a change answered before the check of the
\   changed text published, or still listing a definition the change removed;
\   a request with a change of its document read behind it answered other
\   than -32801, or that change not applied and checked after it; a client
\   whose initialize supports hierarchical document symbols answered other
\   than that flat list ........................... document-symbol-after-change
\ - a request after a change whose check did not complete answered with the
\   definitions of the text before it, which workspace symbols still list
\   ................................................. document-symbol-incomplete
\ - an open document's outline answered from a later check of another
\   document, which read its file from disk; with its file open again by
\   another URI and other text, either document's outline answered with the
\   other's definitions or at the other's URI
\   ......................................... workspace-symbol-open-dependency
\
\ Go to definition
\ - a use in an open document, at its first character or its last, or after a
\   character UTF-16 counts as one unit and UTF-8 as two bytes, not answered
\   with its declaring token's range at the URI the client opened the document
\   by; the character after a use, or a declaring token, answered with a
\   Location; a negative character answered other than -32602 .... definition
\ - a use of a dependency on disk only, its global or, qualified, its public
\   word, not answered with the declaring token's range in its text on disk
\   at its canonical file URI ............................ definition-dependency
\ - a use of a dependency open with its lines swapped, unsaved, not answered
\   with its declaring token's range in the text on disk the check read, at
\   the URI the client opened it by ................. definition-dependency-open
\ - a use in a document whose file is open by another URI too not answered at
\   the URI the client opened the document by
\   ......................................... workspace-symbol-open-dependency
\ - a position in a document not open answered other than -32602
\   ................................................... definition-not-open
\ - a use asked about with the document's opening, answered before the check
\   it started published, or not with the declaration's
\   ................................................ definition-before-check
\ - a use asked about in the turn of a change that moved it and its
\   declaration, answered before the check of the changed text published, or
\   not with the declaration's new range ............. definition-after-change
\ - a use asked about after a change whose check did not complete answered
\   from the uses of the text before it, or the definitions of the last check
\   that completed not listed .......................... definition-incomplete
\
\ Hover
\ - a use, at its first character or its last, not answered with its
\   declaration's kind, word, effect, package, file relative to the working
\   directory and 1-based line as a fenced habu block, over the use .... hover
\ - a use of a dependency on disk, its global or, qualified, its public word,
\   not answered with its declaration there ............... hover-dependency
\ - a use of a dependency open with its lines swapped, unsaved, not answered
\   with its declaration in the text on disk, which the check read
\   ...................................................... hover-dependency-open
\ - a declaring token not answered with its definition, over it
\   ...................................................... hover-definition
\ - a use of either converter a DEFTYPE declares, both at its name's token,
\   in either case or package-qualified, not answered with that converter's
\   own definition ........................................... hover-deftype
\ - a comment, the space before a use or a stack comment answered other
\   than null ................................................ hover-none
\ - a client whose hovers take plain text answered other than with the
\   lines unfenced ...................................... hover-plaintext
\ - a position in a document not open answered other than -32602
\   ........................................................ hover-not-open
\ - a use asked about in the turn of a change that moved it and its
\   declaration answered before the check of the changed text, or not with
\   the declaration's new line ...................... hover-after-change
\
\ Document highlight
\ - a use, at its first character or its last, or the token declaring its
\   word, in an open document whose last check was verified, not answered
\   with that token and every use of the word in the document, each a Text
\   highlight over its range, the last after a character UTF-16 counts as one
\   unit and UTF-8 as two bytes; a token no use names not answered with
\   itself; the character after a use answered with any highlight; a negative
\   character answered other than -32602 ........................ highlight
\ - a use of either converter a DEFTYPE declares, both at its name's token,
\   in either case or package-qualified, answered with the other converter's
\   uses; that shared token, which selects neither word, answered other than
\   null ................................................. highlight-deftype
\ - a use of a dependency on disk, its global or, qualified, its public word,
\   answered with its declaration, which is outside the document, or without
\   the use ........................................... highlight-dependency
\ - a position in a document not open answered other than -32602
\   ..................................................... highlight-not-open
\ - a use asked about in the turn of a change that moved it and its
\   declaration answered before the check of the changed text published, or
\   from the uses of the text before it; a request with a change of its
\   document read behind it answered other than -32801 . highlight-after-change
\ - a use in a document whose last check was refused, or deferred at a
\   top-level word that parses, answered other than null: neither verdict
\   says the check reported every use; go to definition at that byte not
\   answered from the same check ......................... highlight-unproved
\ - a use asked about after a change whose check did not complete answered
\   other than null ................................... highlight-incomplete
\ - a use of a word whose token, in a file included into two packages, into
\   one package's public section and its private one, or again after
\   `undefine`, declares the other's word too, answered other than null: no
\   use's spelling tells them apart ...................... highlight-included
\ - a use of either of two words whose spellings differ only in case, which
\   one check declared at one token by reading its file twice, the file
\   changed on disk between the readings, answered other than null: Habu
\   compares names without case, so neither spelling tells them apart. No
\   conversation can change a file between a check's two readings of it
\   without racing the check, so this one runs the store and the highlight in
\   the test's own process, on the lines a verified check of such a race
\   stated .......................................... highlight-case-reread
\
\ Completion
\ - a cursor in a body not answered, in a list that is not incomplete, with
\   the words of the document and of a dependency that would bind there, each
\   with its declared effect and the document's with its package, or with an
\   engine word's, without either, or with each converter of a DEFTYPE, which
\   share one token, with its own effect, qualified or not ...... completion
\ - a position in a document not open answered other than -32602
\   ................................................... completion-not-open
\ - a cursor asked about in the turn of a change answered before the check of
\   the changed text, or not from it .............. completion-after-change
\ - a bare prefix, or a package's name and an edge colon, answered without
\   a qualified spelling a qualified prefix offers there, or with another
\   detail ................................................ completion-qualified
\
\ Document links
\ - a loader of an open document whose file its last completed check read not
\   linked over its operand, after a character UTF-16 counts as one unit and
\   UTF-8 as two bytes, to the file's canonical file URI with no tooltip; one
\   whose file was held, the engine's or required before, linked otherwise
\   than there with a tooltip naming the file as hover does; a loader whose
\   file is not there linked ................................. document-link
\ - a loader's file open, through a symlink, not linked at the URI the client
\   opened it by, or linked there before it opened ...... document-link-open
\ - a document not open, or params naming none, answered other than -32602
\   .................................................. document-link-not-open
\ - links asked with the document's opening answered before the check it
\   started published ............................ document-link-before-check
\ - links asked in the turn of a change that moved a loader answered before
\   the check of the changed text, or not at its new place; asked with a
\   change read behind them, answered other than -32801
\   .............................................. document-link-after-change
\ - links asked after a change whose check did not complete answered from the
\   text before it, or from the loads that check reported
\   ................................................ document-link-incomplete
\
\ Cancellation
\ - a request with a cancel of it read behind it answered other than -32800,
\   or its work done: two completions and a cancel of the first publish
\   twice .................................................. cancel-completion
\ - a hover cancelled by its string id spelt another way served
\   ............................................................. cancel-hover
\ - a completion with a change of its document read behind it answered other
\   than -32801, or the change not applied and checked after it
\   ......................................................... content-modified
\ - a cancel of an id answered, never sent, of another kind or of a message
\   that is not a request answered, or cancelling a request ..... cancel-stray
\ - a request held when shutdown is read not answered first, a cancel of it
\   read past shutdown not counted, or a request after shutdown answered other
\   than -32600 .............................................. cancel-shutdown
\
\ Not proven here: a packet line that is not JSON, that names no file, or that
\ names its file by a relative path, goes to stderr, but the checker writes
\ none of them - every packet names its file, and the verifier names every
\ file by the canonical absolute path the closure gave it - so no conversation
\ can make one. Nor can one make a check outlast LSP-CHECK's deadline or
\ overflow the verifier's capture.

require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/test.f
require lib/test/outcome.f
require lib/test/subject.f
require lib/span.f
require lib/memory.f
require lib/byte-buffer.f
require lib/fd-io.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/engine-candidate.f
require lib/uri.f
require lib/content-length.f
require lib/json-read.f
require lib/json-rpc.f
require lib/json-write.f
require tools/check-verify-core.f
require tools/lsp-docs.f
require tools/lsp-text.f
require tools/lsp-defs.f
require tools/lsp-highlight.f
require tools/lsp-core.f

package LSP-TEST
using BUF

\ ---- storage ------------------------------------------------------------------

\ What a conversation proves is what the server writes and how it exits, and
\ none of that changes when the host is busy, so the time one run of the server
\ may take is a deadlock guard and nothing else: a server that is merely slow
\ must never reach it. The capture bounds a conversation run through it whole.
\ A conversation over pipes the test holds bounds each wait for output against
\ one deadline after its reads start: CONVERSATION-MS, or CHECKED-MS for a
\ diagnostics conversation, whose server checks documents and whose test checks
\ them too inside that bound. The wait for the server's exit once it has ended
\ stdout and stderr is not bounded. Past its bound, any kind kills the server
\ and fails by name.
\
\ Measured 2026-10-02 on a 12-core machine with six copies of this test running
\ at once at a load average of 87 to 91: the diagnostics conversations took 528
\ to 1230 ms, the heaviest big-frame, and every other one timed 418 to 793 ms;
\ answers-at-once and stdout-closed, untimed, carry no more than lifecycle. The
\ conversations added since, dependency-close, dependency-symlink and
\ dependency-shared, took 660 to 1111 ms in single runs at a load average of
\ about 140, when big-frame took up to 1289 ms. CONVERSATION-MS is ten times
\ its busiest measurement and CHECKED-MS about six times big-frame's 1230 to
\ 1289 ms, and neither is larger. The one bounds 27 runs (the 24 conversations
\ CONVERSE runs, answers-at-once, stdout-closed and TEST-EXIT-TIMEOUT's child)
\ and the other 81 conversations. A server that blocks spends none of the
\ row's CPU budget (test/suite-budget.f CPU-MS), so all 108 could reach their
\ bounds, each failing by name, well inside the row's hang guard (ROW-MS). One
\ that spins to its bound spends that much of the budget's 360 s, so at most
\ 45 such runs fail by name before the budget ends the row.
8000 constant CONVERSATION-MS
8000 constant CHECKED-MS

$10000 constant OUT-CAP                  \ the most a conversation writes to stdout
$1000 constant ERR-CAP                   \ and to stderr
32000 constant BIG-LINES                 \ big-frame's document, 22 bytes a line escaped
512 constant PIPE-ATOM                   \ PIPE_BUF: the room POLLOUT promises a writer

OUT-CAP SPAN-BUFFER: OUT-SPAN
ERR-CAP SPAN-BUFFER: ERR-SPAN
variable OUT-LEN                         \ the bytes the server wrote to stdout,
variable OUT-AT                          \ of them the bytes read as frames,
variable ERR-LEN                         \ and the bytes it wrote to stderr

create LABEL-B HDR-BYTES allot           \ the conversation's name
create IN-B HDR-BYTES allot              \ its input
create WANT-B HDR-BYTES allot            \ the stderr it must write
create MSG-B HDR-BYTES allot             \ a message or a reply being composed
create PAR-B HDR-BYTES allot             \ a notification's params
create DIR-B HDR-BYTES allot             \ the artifact directory
create EXP-B HDR-BYTES allot             \ a publish expected
create PATH-B HDR-BYTES allot            \ a document's path
create TXT-B HDR-BYTES allot             \ a generated document

TYPED-VARIABLE IN-W fd                   \ a held conversation's ends of the server's stdin,
TYPED-VARIABLE OUT-R fd                  \ stdout
TYPED-VARIABLE ERR-R fd                  \ and stderr,
variable DEADLINE                        \ the mono-ns time its waits run out,
variable SENT                            \ and the bytes of its input written

FS-PATH-CAP 3 * 7 + SPAN-BUFFER: URI-SPAN  \ a file URI: file:// and each byte escaped
create JR-ST JR:STORAGE-BYTES allot      \ JR storage for reading a packet
TYPED-VARIABLE HL-W JSON-WRITE:writer    \ a highlight written in this process

: READY ( ptr a -- )  256 N>BLEN INIT ;

: LABEL$ ( -- ptr u8 n )  LABEL-B SPAN$ BLEN>N ;
: IN$ ( -- ptr u8 n )  IN-B SPAN$ BLEN>N ;
: WANT$ ( -- ptr u8 n )  WANT-B SPAN$ BLEN>N ;
: MSG$ ( -- ptr u8 n )  MSG-B SPAN$ BLEN>N ;
: PAR$ ( -- ptr u8 n )  PAR-B SPAN$ BLEN>N ;
: DIR$ ( -- ptr u8 n )  DIR-B SPAN$ BLEN>N ;
: IN+ ( ptr u8 n -- )  N>BLEN IN-B APPEND-SPAN ;
: WANT+ ( ptr u8 n -- )  N>BLEN WANT-B APPEND-SPAN ;
: MSG+ ( ptr u8 n -- )  N>BLEN MSG-B APPEND-SPAN ;
: PAR+ ( ptr u8 n -- )  N>BLEN PAR-B APPEND-SPAN ;
: EXP$ ( -- ptr u8 n )  EXP-B SPAN$ BLEN>N ;
: EXP+ ( ptr u8 n -- )  N>BLEN EXP-B APPEND-SPAN ;
: TXT$ ( -- ptr u8 n )  TXT-B SPAN$ BLEN>N ;
: OUT$ ( -- ptr u8 n )  OUT-SPAN SPAN:$ drop OUT-LEN @ ;
: ERR$ ( -- ptr u8 n )  ERR-SPAN SPAN:$ drop ERR-LEN @ ;

\ A number's decimal text, in SB.
: INT$ ( n -- ptr u8 n )  SB-RESET FMT:SB-INT SB$ ;

\ ---- the input ----------------------------------------------------------------

\ The header LSP frames a body of N bytes with, in SB.
: HEAD$ ( n -- ptr u8 n )
   SB-RESET
   s" Content-Length: " SB-APPEND
   FMT:SB-U
   s\" \r\n\r\n" SB-APPEND
   SB$ ;

\ A body, framed. It never lives in SB, where the header is built.
: FRAMED ( ptr u8 n -- )
   {: a:ptr u:n :}
   u HEAD$ IN+
   a u IN+ ;

\ A body framed in another header form: BEFORE, the length, AFTER, the body.
: FORM ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: a:ptr u:n b:ptr bu:n c:ptr cu:n :}
   b bu IN+
   u INT$ IN+
   c cu IN+
   a u IN+ ;

\ A request with no params, from its id's JSON text and its method.
: ASK$ ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: i:ptr iu:n m:ptr mu:n :}
   MSG-B CLEAR
   s\" {\"jsonrpc\":\"2.0\",\"id\":" MSG+ i iu MSG+
   s\" ,\"method\":\"" MSG+ m mu MSG+ s\" \"}" MSG+
   MSG$ ;

: ASK ( ptr u8 n ptr u8 n -- )  ASK$ FRAMED ;

\ A notification, from its method and its params' JSON text, none when empty.
: TELL$ ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: m:ptr mu:n p:ptr pu:n :}
   MSG-B CLEAR
   s\" {\"jsonrpc\":\"2.0\",\"method\":\"" MSG+ m mu MSG+ s\" \"" MSG+
   pu 0 > if s\" ,\"params\":" MSG+ p pu MSG+ then
   s" }" MSG+
   MSG$ ;

: TELL ( ptr u8 n ptr u8 n -- )  TELL$ FRAMED ;

: INITIALIZE$ ( -- ptr u8 n )
   s\" {\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"capabilities\":{}}}" ;

: INITIALIZE ( -- )  INITIALIZE$ FRAMED ;
: SHUTDOWN ( -- )  s" 2" s" shutdown" ASK ;
: EXIT-NOTE ( -- )  s" exit" s" " TELL ;

: SIGN-OFF ( -- )
   SHUTDOWN
   EXIT-NOTE ;

\ The documents, each by its URI's JSON text.
: URI-A ( -- ptr u8 n )  s\" \"file:///habu-lsp-test/a.f\"" ;
: URI-B ( -- ptr u8 n )  s\" \"file:///habu-lsp-test/a%20b.f\"" ;

\ didOpen params, in PAR, from the JSON text of a URI, a version and a text.
: OPEN-PARAMS ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: u:ptr uu:n v:ptr vu:n t:ptr tu:n :}
   PAR-B CLEAR
   s\" {\"textDocument\":{\"uri\":" PAR+ u uu PAR+
   s\" ,\"languageId\":\"habu\",\"version\":" PAR+ v vu PAR+
   s\" ,\"text\":" PAR+ t tu PAR+ s" }}" PAR+ ;

\ didChange params, in PAR, from the JSON text of a URI and of contentChanges.
: CHANGE-PARAMS ( ptr u8 n ptr u8 n -- )
   {: u:ptr uu:n c:ptr cu:n :}
   PAR-B CLEAR
   s\" {\"textDocument\":{\"uri\":" PAR+ u uu PAR+
   s\" ,\"version\":2},\"contentChanges\":" PAR+ c cu PAR+ s" }" PAR+ ;

: OPEN-DOC ( ptr u8 n -- )
   s" 1" s\" \": A ( -- ) ;\\n\"" OPEN-PARAMS
   s" textDocument/didOpen" PAR$ TELL ;

\ Two whole texts, the last one the document's.
: CHANGE-DOC ( ptr u8 n -- )
   s\" [{\"text\":\"x\"},{\"text\":\": B ( -- ) ;\\n\"}]" CHANGE-PARAMS
   s" textDocument/didChange" PAR$ TELL ;

\ A change of a range, which Full sync never sends.
: RANGE-CHANGE ( ptr u8 n -- )
   s\" [{\"range\":{\"start\":{\"line\":0,\"character\":0},\"end\":{\"line\":0,\"character\":1}},\"text\":\"y\"}]"
   CHANGE-PARAMS
   s" textDocument/didChange" PAR$ TELL ;

: CLOSE-DOC ( ptr u8 n -- )
   {: u:ptr uu:n :}
   PAR-B CLEAR
   s\" {\"textDocument\":{\"uri\":" PAR+ u uu PAR+ s" }}" PAR+
   s" textDocument/didClose" PAR$ TELL ;

\ The diagnostics conversations' documents, each by its path: a file of the
\ artifact directory, on disk only when a case writes it there, or of the tree.
: FIXTURE ( ptr u8 n -- ptr u8 n )
   {: a:ptr u:n :}
   DIR$ N>BLEN PATH-B REPLACE
   s" /" N>BLEN PATH-B APPEND-SPAN
   a u N>BLEN PATH-B APPEND-SPAN
   PATH-B SPAN$ BLEN>N ;

: IN-TREE ( ptr u8 n -- ptr u8 n )
   {: a:ptr u:n :}
   SOURCE-ROOT:CWD$ N>BLEN PATH-B REPLACE
   s" /" N>BLEN PATH-B APPEND-SPAN
   a u N>BLEN PATH-B APPEND-SPAN
   PATH-B SPAN$ BLEN>N ;

: A-PATH ( -- ptr u8 n )  s" a.f" FIXTURE ;
: B-PATH ( -- ptr u8 n )  s" b.f" FIXTURE ;

\ A path's file URI, in URI-SPAN.
: URI-OF ( ptr u8 n -- ptr u8 n )
   URI-SPAN URI:PATH>FILE {: n:n :}
   URI-SPAN SPAN:$ drop n ;

\ A path's file URI as a JSON string, in PAR.
: URI+ ( ptr u8 n -- )
   URI-OF {: a:ptr u:n :}
   s\" \"" PAR+ a u PAR+ s\" \"" PAR+ ;

\ The byte at A as JSON string text, in PAR: a quote, a backslash and LF
\ escaped; the texts here hold no other control byte.
: ESCAPED+ ( ptr u8 -- )
   {: a:ptr :}
   a c@ {: c:n :}
   c 10 = if s" \n" PAR+ exit then
   c 34 = if s\" \\\"" PAR+ exit then
   c 92 = if s" \\" PAR+ exit then
   a 1 PAR+ ;

\ A document's text as a JSON string, in PAR.
: TEXT+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   s\" \"" PAR+
   u 0 ?do a i + ESCAPED+ loop
   s\" \"" PAR+ ;

\ didOpen of the document at PATH with this text and version.
: OPENS ( ptr u8 n ptr u8 n n -- )
   {: p:ptr pu:n t:ptr tu:n v:n :}
   PAR-B CLEAR
   s\" {\"textDocument\":{\"uri\":" PAR+ p pu URI+
   s\" ,\"languageId\":\"habu\",\"version\":" PAR+ v INT$ PAR+
   s\" ,\"text\":" PAR+ t tu TEXT+ s" }}" PAR+
   s" textDocument/didOpen" PAR$ TELL ;

\ didChange of the document at PATH to this whole text and version.
: CHANGES ( ptr u8 n ptr u8 n n -- )
   {: p:ptr pu:n t:ptr tu:n v:n :}
   PAR-B CLEAR
   s\" {\"textDocument\":{\"uri\":" PAR+ p pu URI+
   s\" ,\"version\":" PAR+ v INT$ PAR+
   s\" },\"contentChanges\":[{\"text\":" PAR+ t tu TEXT+ s" }]}" PAR+
   s" textDocument/didChange" PAR$ TELL ;

\ Params naming the document at PATH alone, in PAR.
: NAMING ( ptr u8 n -- )
   PAR-B CLEAR
   s\" {\"textDocument\":{\"uri\":" PAR+ URI+ s" }}" PAR+ ;

: SAVES ( ptr u8 n -- )  NAMING s" textDocument/didSave" PAR$ TELL ;
: CLOSES ( ptr u8 n -- )  NAMING s" textDocument/didClose" PAR$ TELL ;

\ ---- the stderr expected ------------------------------------------------------

\ The line for a notification left unapplied: its method and the throw.
: IGNORED ( ptr u8 n n -- )
   {: m:ptr mu:n code:n :}
   s\" lsp: ignored \"" WANT+ m mu WANT+ s\" \": throw " WANT+
   code INT$ WANT+ s\" \n" WANT+ ;

\ The line the server stops on: the throw that stopped it.
: STOPPED ( n -- )
   s" lsp: stopped by throw " WANT+ INT$ WANT+ s\" \n" WANT+ ;

: CUT-SHORT ( -- )  s\" lsp: input ended before exit\n" WANT+ ;

\ A notification the server must leave unapplied with this code.
: UNAPPLIED ( ptr u8 n ptr u8 n n -- )
   {: m:ptr mu:n p:ptr pu:n code:n :}
   m mu p pu TELL
   m mu code IGNORED ;

\ The line about the check of the document at PATH, these words after its URI,
\ then the prose of the test's own check, ended by LF.
: SAID ( ptr u8 n ptr u8 n -- )
   {: p:ptr pu:n w:ptr wu:n :}
   s" lsp: " WANT+ p pu URI-OF WANT+ s" : " WANT+ w wu WANT+ s\" \n" WANT+
   CHECK:VERIFY-LOG$ {: a:ptr u:n :}
   u 0= if exit then
   a u WANT+
   a u + 1- c@ 10 <> if s\" \n" WANT+ then ;

\ The lines about a check that completed with this verdict: none unless the
\ test's own check wrote prose.
: COMPLETED ( ptr u8 n ptr u8 n -- )
   CHECK:VERIFY-LOG$ nip 0= if 2drop 2drop exit then
   SAID ;

\ ---- running the server -------------------------------------------------------

\ Starts a conversation of this name, with no input and no stderr expected.
: CONVERSATION ( ptr u8 n -- )
   N>BLEN LABEL-B REPLACE
   IN-B CLEAR
   WANT-B CLEAR
   0 OUT-LEN !
   0 OUT-AT !
   0 ERR-LEN ! ;

\ The engine to run, with the server's command line and environment staged.
: STAGED ( -- ptr u8 len )
   ENGINE-CANDIDATE:PATH$ >LEN
   s" --load" >LEN PROC-ARGV+
   s" tools/lsp.f" >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING ;

\ The conversation's artifact file with this suffix, in SB.
: ARTIFACT$ ( ptr u8 n -- ptr u8 n )
   {: x:ptr xu:n :}
   SB-RESET
   DIR$ SB-APPEND s" /" SB-APPEND LABEL$ SB-APPEND x xu SB-APPEND
   SB$ ;

\ Keeps the conversation: its input, output and stderr, and its exit code.
: ARCHIVE ( n -- )
   {: rc:n :}
   s" .in" ARTIFACT$ IN$ WRITE-ALL
   s" .out" ARTIFACT$ OUT$ WRITE-ALL
   s" .err" ARTIFACT$ ERR$ WRITE-ALL
   MSG-B CLEAR
   LABEL$ MSG+ s"  " MSG+ rc INT$ MSG+ s\" \n" MSG+
   SB-RESET DIR$ SB-APPEND s" /exits" SB-APPEND
   SB$ MSG$ APPEND-FILE ;

\ The server ended so, and must have exited with this code; past its deadline,
\ the conversation and what the server wrote are printed.
: EXITED ( outcome n -- )
   {: r want:n :}
   LABEL$ T-LABEL IN$ OUT$ ERR$ r want T-OUTCOME-EXITED=
   r PROC-OUTCOME>RC RC>N ARCHIVE ;

\ Exercise this adapter's timeout arm without waiting for a slow server.
: TIMEOUT-EXIT-PROBE ( -- )
   s" timed-out conversation" CONVERSATION
   s" timeout input" IN+
   s" timeout stdout" dup OUT-LEN ! OUT-SPAN SPAN:COPY
   s" timeout stderr" dup ERR-LEN ! ERR-SPAN SPAN:COPY
   OUTCOME:TIMEOUT 0 EXITED ;

: TEST-EXIT-TIMEOUT ( -- )
   s" LSP timeout reports the conversation and both streams" T-LABEL
   s" package LSP-TEST TIMEOUT-EXIT-PROBE ;package" {: src:ptr srcu:n :}
   src srcu OUT-SPAN SPAN:$ >LEN ERR-SPAN SPAN:$ >LEN
   CONVERSATION-MS >MS SUBJECT:RUN {: outu:len erru:len r :}
   src srcu OUT-SPAN SPAN:$ drop outu LEN>N
   ERR-SPAN SPAN:$ drop erru LEN>N r UNCAUGHT-RC T-OUTCOME-EXITED=
   OUT-SPAN SPAN:$ drop outu LEN>N s" timed-out conversation" CONTAINS? TTRUE
   OUT-SPAN SPAN:$ drop outu LEN>N s" timeout input" CONTAINS? TTRUE
   OUT-SPAN SPAN:$ drop outu LEN>N s" timeout stdout" CONTAINS? TTRUE
   OUT-SPAN SPAN:$ drop outu LEN>N s" timeout stderr" CONTAINS? TTRUE
   ERR-SPAN SPAN:$ drop erru LEN>N s" uncaught throw code -2502" CONTAINS? TTRUE ;

: TOOK ( n -- )
   {: ns:n :}
   SB-RESET
   s" lsp-test: " SB-APPEND LABEL$ SB-APPEND s"  " SB-APPEND
   ns 1000000 / FMT:SB-U s"  ms" SB-APPEND
   SB$ type cr ;

\ Fails the conversation, under its name and this reason, unless the flag holds.
: HOLDS ( bool ptr u8 n -- )
   {: ok:bool r:ptr ru:n :}
   SB-RESET LABEL$ SB-APPEND s" : " SB-APPEND r ru SB-APPEND
   SB$ T-LABEL ok TTRUE ;

\ Runs the server on the conversation's input and reads all it writes. It must
\ exit with RC. The capture writes the input in pieces of PROC-STDIN-CHUNK-CAP,
\ and one piece lands in the empty pipe whole, so the server reads every
\ message of a conversation no larger before it would look for a document to
\ check: none of these conversations expects a check.
: CONVERSE ( n -- )
   {: rc:n :}
   IN$ nip PROC-STDIN-CHUNK-CAP <= s" input past one write" HOLDS
   mono-ns {: t0:n :}
   STAGED IN$ >LEN OUT-SPAN SPAN:$ >LEN ERR-SPAN SPAN:$ >LEN CONVERSATION-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME {: o:len e:len r :}
   mono-ns t0 - TOOK
   o LEN>N OUT-LEN !
   e LEN>N ERR-LEN !
   r rc EXITED ;

\ ---- the output ---------------------------------------------------------------

: UNREAD ( -- ptr u8 n )  OUT-SPAN SPAN:$ drop OUT-AT @ +  OUT-LEN @ OUT-AT @ - ;

: ALIKE ( ptr u8 n ptr u8 n -- )  LABEL$ T-LABEL T$= ;

\ The length a frame's header names, from the output at the frame: the number
\ between the name and CR LF twice, or -1.
: FRAME-LEN ( ptr u8 n -- n )
   {: a:ptr u:n :}
   s" Content-Length: " nip {: name:n :}
   a u s\" \r\n\r\n" FIND-SUB MATCH option
      none OF -1 ENDOF
      some OF IDX>N ENDOF
   ;MATCH {: end:n :}
   end name < if -1 exit then
   a name + end name - STR>NUMBER? MATCH option
      none OF -1 ENDOF
      some OF ENDOF
   ;MATCH ;

\ Output not framed as LSP frames it fails the conversation and is passed over.
: MISFRAMED ( -- ptr u8 n )
   false s" output not framed as LSP frames it" HOLDS
   OUT-LEN @ OUT-AT !
   OUT-SPAN SPAN:$ drop 0 ;

\ The next frame's body. The server frames a message as `Content-Length: N`, CR
\ LF twice and N bytes, the length without sign or leading zeros.
: NEXT-BODY ( -- ptr u8 n )
   UNREAD {: a:ptr u:n :}
   a u FRAME-LEN {: len:n :}
   len 0 < if MISFRAMED exit then
   len HEAD$ {: h:ptr hu:n :}
   a u h hu STARTS-WITH? 0= if MISFRAMED exit then
   hu len + u > if MISFRAMED exit then
   hu len + OUT-AT +!
   a hu + len ;

\ The next frame is this body.
: HEARD ( ptr u8 n -- )
   {: a:ptr u:n :}
   NEXT-BODY a u ALIKE ;

\ The next frame is an error reply to the request with this id's JSON text,
\ with this code. Its message is prose: only that it is a string is read.
: REFUSED ( ptr u8 n n -- )
   {: i:ptr iu:n code:n :}
   NEXT-BODY {: b:ptr bu:n :}
   MSG-B CLEAR
   s\" {\"jsonrpc\":\"2.0\",\"id\":" MSG+ i iu MSG+ s\" ,\"error\":{\"code\":" MSG+
   code INT$ MSG+ s\" ,\"message\":\"" MSG+
   b bu MSG$ nip min MSG$ ALIKE
   b bu s\" \"}}" ENDS-WITH? LABEL$ T-LABEL TTRUE ;

: CAPABILITIES ( -- )
   MSG-B CLEAR
   s\" {\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"capabilities\":{\"positionEncoding\":\"utf-16\"," MSG+
   s\" \"textDocumentSync\":{\"openClose\":true,\"change\":1,\"save\":{\"includeText\":false}}," MSG+
   s\" \"completionProvider\":{},\"definitionProvider\":true," MSG+
   s\" \"hoverProvider\":true,\"documentHighlightProvider\":true," MSG+
   s\" \"documentSymbolProvider\":true," MSG+
   s\" \"documentLinkProvider\":{}," MSG+
   s\" \"workspaceSymbolProvider\":true}," MSG+
   s\" \"serverInfo\":{\"name\":\"habu\"}}}" MSG+
   MSG$ HEARD ;

: NULL-RESULT ( ptr u8 n -- )
   {: i:ptr iu:n :}
   MSG-B CLEAR
   s\" {\"jsonrpc\":\"2.0\",\"id\":" MSG+ i iu MSG+ s\" ,\"result\":null}" MSG+
   MSG$ HEARD ;

\ The empty diagnostics list published for a document, by the JSON text of the
\ URI the server must echo.
: PUBLISHED ( ptr u8 n -- )
   {: u:ptr uu:n :}
   MSG-B CLEAR
   s\" {\"jsonrpc\":\"2.0\",\"method\":\"textDocument/publishDiagnostics\",\"params\":{\"uri\":" MSG+
   u uu MSG+ s\" ,\"diagnostics\":[]}}" MSG+
   MSG$ HEARD ;

\ Nothing is left past the frames read, and stderr is what was expected.
: ENDS ( -- )
   UNREAD s" " ALIKE
   ERR$ WANT$ ALIKE ;

\ ---- conversations over pipes the test holds -----------------------------------

\ Starts the server with these descriptors as its stdin, stdout and stderr.
: SPAWN ( fd fd fd -- pid )
   {: i:fd o:fd e:fd :}
   STAGED i o e PROC-SPAWN-ARGV-ENV-IO ;

\ Waits until the descriptor holds a byte or its end; past the deadline, a
\ mono-ns time, it throws E-PROC-TIMEOUT.
: AWAIT ( fd n -- )
   PROC-LEFT-MS POLL-IN-OR-TIMEOUT drop ;

\ Reads the byte past the span's first N once the descriptor holds it: whether
\ one came before the end. A wait promises one byte, and READ-EXACT reads until
\ its span is full, so each read asks for one.
: BYTE ( n fd SPAN:span<u8> n -- bool )
   {: got:n f:fd dst deadline:n :}
   f deadline AWAIT
   f dst got SPAN:SKIP 1 SPAN:TAKE FD-IO:READ-EXACT MATCH FD-IO:fill
      full OF true ENDOF
      eof OF drop false ENDOF
   ;MATCH ;

\ Reads a descriptor into the span, past the bytes the cell counts, until its
\ end or the span is full, keeping the count in the cell. Each wait ends at the
\ deadline.
: DRAIN ( fd SPAN:span<u8> ptr n n -- )
   {: f:fd dst len:ptr deadline:n :}
   len @ begin
      dup len !
      dup dst SPAN:LEN < if dup f dst deadline BYTE else false then
   while 1+ repeat drop ;

\ The server's outcome once its output is read. A wait that ran out kills it,
\ and the outcome is a timeout, as a capture's is; any other throw goes on.
: SETTLED ( n pid -- outcome )
   {: code:n pid :}
   code 0= if pid PROC-WAIT-OUTCOME exit then
   code E-PROC-TIMEOUT <> if code throw then
   pid SIGKILL PROC-KILL-RAW drop
   pid PROC-WAIT-STATUS drop
   OUTCOME:TIMEOUT ;

\ The reply's first byte while the input stays open; the input then ends,
\ whether the byte came or the wait ran out.
: FIRST-BYTE ( fd fd n -- )
   {: in-w:fd out-r:fd deadline:n :}
   out-r deadline [: 2dup AWAIT ;] catch {: code:n :}
   2drop
   in-w FD>N close
   code 0<> if code throw then ;

\ answers-at-once's reads, against one deadline: the reply's first byte, then
\ stdout and stderr to their ends.
: HEAR-AT-ONCE ( fd fd fd -- fd fd fd )
   {: in-w:fd out-r:fd err-r:fd :}
   CONVERSATION-MS >MS PROC-DEADLINE-AT {: deadline:n :}
   in-w out-r deadline FIRST-BYTE
   out-r OUT-SPAN OUT-LEN deadline DRAIN
   err-r ERR-SPAN ERR-LEN deadline DRAIN
   in-w out-r err-r ;

\ stdout-closed's read: stderr to its end.
: HEAR-ERR ( fd -- fd )
   dup ERR-SPAN ERR-LEN CONVERSATION-MS >MS PROC-DEADLINE-AT DRAIN ;

\ The reply to initialize starts while the input stays open: a server that
\ waited for more input, or for its end, would never answer a client waiting
\ for that reply.
: TEST-ANSWERS-AT-ONCE ( -- )
   s" answers-at-once" CONVERSATION
   INITIALIZE
   CUT-SHORT
   PIPE-PAIR {: in-r:fd in-w:fd :}
   PIPE-PAIR {: out-r:fd out-w:fd :}
   PIPE-PAIR {: err-r:fd err-w:fd :}
   in-w FD-CLOEXEC!
   in-w FD-NOSIGPIPE!
   out-r FD-CLOEXEC!
   err-r FD-CLOEXEC!
   in-r out-w err-w SPAWN {: pid :}
   in-r FD>N close
   out-w FD>N close
   err-w FD>N close
   in-w IN$ FD-IO:WRITE-FULL
   in-w out-r err-r [: HEAR-AT-ONCE ;] catch {: code:n :}
   drop 2drop
   out-r FD>N close
   err-r FD>N close
   code pid SETTLED 1 EXITED
   CAPABILITIES
   ENDS ;

\ The server's stdout has no reader. Its input is in the pipe whole and ended,
\ and a write with no reader fails at once, so nothing can hold it up.
: TEST-STDOUT-CLOSED ( -- )
   s" stdout-closed" CONVERSATION
   INITIALIZE
   SIGN-OFF
   E-FS-IO STOPPED
   PIPE-PAIR {: in-r:fd in-w:fd :}
   PIPE-PAIR {: out-r:fd out-w:fd :}
   PIPE-PAIR {: err-r:fd err-w:fd :}
   in-w IN$ FD-IO:WRITE-FULL
   in-w FD>N close
   out-r FD>N close
   err-r FD-CLOEXEC!
   in-r out-w err-w SPAWN {: pid :}
   in-r FD>N close
   out-w FD>N close
   err-w FD>N close
   err-r [: HEAR-ERR ;] catch {: code:n :}
   drop
   err-r FD>N close
   code pid SETTLED 1 EXITED
   ENDS ;

\ ---- conversations ------------------------------------------------------------

: TEST-LIFECYCLE ( -- )
   s" lifecycle" CONVERSATION
   INITIALIZE
   s" initialized" s" {}" TELL
   SIGN-OFF
   0 CONVERSE
   CAPABILITIES
   s" 2" NULL-RESULT
   ENDS ;

\ exit-first: the initialize after exit is never read.
: TEST-EXIT-WITHOUT-SHUTDOWN ( -- )
   s" exit-without-shutdown" CONVERSATION
   INITIALIZE
   EXIT-NOTE
   1 CONVERSE
   CAPABILITIES
   ENDS
   s" exit-first" CONVERSATION
   EXIT-NOTE
   INITIALIZE
   1 CONVERSE
   ENDS ;

: TEST-EOF-BEFORE-EXIT ( -- )
   s" eof-before-exit" CONVERSATION
   INITIALIZE
   CUT-SHORT
   1 CONVERSE
   CAPABILITIES
   ENDS ;

: TEST-EOF-AFTER-SHUTDOWN ( -- )
   s" eof-after-shutdown" CONVERSATION
   INITIALIZE
   SHUTDOWN
   0 CONVERSE
   CAPABILITIES
   s" 2" NULL-RESULT
   ENDS ;

\ The document opened before initialize is not open after it.
: TEST-BEFORE-INITIALIZE ( -- )
   s" before-initialize" CONVERSATION
   s" 7" s" shutdown" ASK
   URI-A OPEN-DOC
   URI-A CLOSE-DOC
   INITIALIZE
   URI-A CLOSE-DOC
   s" textDocument/didClose" LSP:E-LSP-NOT-OPEN IGNORED
   SIGN-OFF
   0 CONVERSE
   s" 7" LSP:NOT-INITIALIZED REFUSED
   CAPABILITIES
   s" 2" NULL-RESULT
   ENDS ;

: TEST-INITIALIZE-TWICE ( -- )
   s" initialize-twice" CONVERSATION
   INITIALIZE
   s" 3" s" initialize" ASK
   SIGN-OFF
   0 CONVERSE
   CAPABILITIES
   s" 3" JSON-RPC:INVALID-REQUEST REFUSED
   s" 2" NULL-RESULT
   ENDS ;

\ The close of a document open before shutdown publishes nothing after it.
: TEST-AFTER-SHUTDOWN ( -- )
   s" after-shutdown" CONVERSATION
   INITIALIZE
   URI-A OPEN-DOC
   SHUTDOWN
   s" 3" s" initialize" ASK
   s" 4" s" shutdown" ASK
   URI-A CLOSE-DOC
   EXIT-NOTE
   0 CONVERSE
   CAPABILITIES
   s" 2" NULL-RESULT
   s" 3" JSON-RPC:INVALID-REQUEST REFUSED
   s" 4" JSON-RPC:INVALID-REQUEST REFUSED
   ENDS ;

: TEST-UNKNOWN-METHOD ( -- )
   s" unknown-method" CONVERSATION
   INITIALIZE
   s" 3" s" habu/unknown" ASK
   s" habu/unknown" s" {}" TELL
   SIGN-OFF
   0 CONVERSE
   CAPABILITIES
   s" 3" JSON-RPC:METHOD-NOT-FOUND REFUSED
   s" 2" NULL-RESULT
   ENDS ;

\ A string id with a quote, a backslash, \u and \/ escaped.
: ESCAPED-ID ( -- ptr u8 n )  s\" \"a\\\"b\\\\\\u00e9\\/c\"" ;
: LONG-ID ( -- ptr u8 n )  s" 123456789012345678901234567890" ;

: TEST-IDS ( -- )
   s" ids" CONVERSATION
   INITIALIZE
   s" 0" s" habu/unknown" ASK
   s" -7" s" habu/unknown" ASK
   s" 1.5e3" s" habu/unknown" ASK
   LONG-ID s" habu/unknown" ASK
   ESCAPED-ID s" habu/unknown" ASK
   s" null" s" habu/unknown" ASK
   SIGN-OFF
   0 CONVERSE
   CAPABILITIES
   s" 0" JSON-RPC:METHOD-NOT-FOUND REFUSED
   s" -7" JSON-RPC:METHOD-NOT-FOUND REFUSED
   s" 1.5e3" JSON-RPC:METHOD-NOT-FOUND REFUSED
   LONG-ID JSON-RPC:METHOD-NOT-FOUND REFUSED
   ESCAPED-ID JSON-RPC:METHOD-NOT-FOUND REFUSED
   s" null" JSON-RPC:METHOD-NOT-FOUND REFUSED
   s" 2" NULL-RESULT
   ENDS ;

: TEST-BODY-NOT-JSON ( -- )
   s" body-not-json" CONVERSATION
   INITIALIZE
   s\" {\"jsonrpc\":" FRAMED
   s" " FRAMED
   SIGN-OFF
   0 CONVERSE
   CAPABILITIES
   s" null" JSON-RPC:PARSE-ERROR REFUSED
   s" null" JSON-RPC:PARSE-ERROR REFUSED
   s" 2" NULL-RESULT
   ENDS ;

: TEST-INVALID-REQUEST ( -- )
   s" invalid-request" CONVERSATION
   INITIALIZE
   s" [1]" FRAMED
   s" 5" FRAMED
   s\" {\"jsonrpc\":\"1.0\",\"id\":3,\"method\":\"habu/unknown\"}" FRAMED
   s\" {\"jsonrpc\":\"2.0\",\"id\":4,\"method\":7}" FRAMED
   s\" {\"jsonrpc\":\"2.0\",\"id\":5,\"result\":null}" FRAMED
   s\" {\"jsonrpc\":\"2.0\",\"id\":6,\"error\":{\"code\":1,\"message\":\"m\"}}" FRAMED
   SIGN-OFF
   0 CONVERSE
   CAPABILITIES
   s" null" JSON-RPC:INVALID-REQUEST REFUSED
   s" null" JSON-RPC:INVALID-REQUEST REFUSED
   s" 3" JSON-RPC:INVALID-REQUEST REFUSED
   s" 4" JSON-RPC:INVALID-REQUEST REFUSED
   s" 2" NULL-RESULT
   ENDS ;

\ The document opened under its URI escaped is changed and closed under it
\ plain, and the close echoes the URI decoded with its percent escapes kept. A
\ second open replaces the first, so one close publishes and the next is
\ refused.
: TEST-SYNC ( -- )
   s" sync" CONVERSATION
   INITIALIZE
   s\" \"file:\\/\\/\\/habu-lsp-test\\/a.f\"" OPEN-DOC
   URI-A CHANGE-DOC
   URI-A CLOSE-DOC
   URI-B OPEN-DOC
   URI-B CLOSE-DOC
   URI-A OPEN-DOC
   URI-A OPEN-DOC
   URI-A CLOSE-DOC
   URI-A CLOSE-DOC
   s" textDocument/didClose" LSP:E-LSP-NOT-OPEN IGNORED
   URI-A CHANGE-DOC
   s" textDocument/didChange" LSP:E-LSP-NOT-OPEN IGNORED
   s\" \"untitled:Untitled-1\"" OPEN-DOC
   s" textDocument/didOpen" E-URI-SCHEME IGNORED
   s\" \"untitled:Untitled-1\"" CLOSE-DOC
   s" textDocument/didClose" LSP:E-LSP-NOT-OPEN IGNORED
   URI-A OPEN-DOC
   URI-A RANGE-CHANGE
   s" textDocument/didChange" LSP:E-LSP-PARAMS IGNORED
   URI-A CLOSE-DOC
   SIGN-OFF
   0 CONVERSE
   CAPABILITIES
   URI-A PUBLISHED
   URI-B PUBLISHED
   URI-A PUBLISHED
   URI-A PUBLISHED
   s" 2" NULL-RESULT
   ENDS ;

\ didOpen params the server cannot read, each logged.
: BAD-OPENS ( -- )
   s" textDocument/didOpen" s" " LSP:E-LSP-PARAMS UNAPPLIED
   s" textDocument/didOpen" s" [1]" LSP:E-LSP-PARAMS UNAPPLIED
   s" textDocument/didOpen" s" {}" LSP:E-LSP-PARAMS UNAPPLIED
   s" 1" s" 1" s\" \"\"" OPEN-PARAMS
   s" textDocument/didOpen" PAR$ LSP:E-LSP-PARAMS UNAPPLIED
   URI-A s" 1.5" s\" \"\"" OPEN-PARAMS
   s" textDocument/didOpen" PAR$ LSP:E-LSP-PARAMS UNAPPLIED
   URI-A LONG-ID s\" \"\"" OPEN-PARAMS
   s" textDocument/didOpen" PAR$ E-JR-NUMBER UNAPPLIED
   URI-A s" 1" s" null" OPEN-PARAMS
   s" textDocument/didOpen" PAR$ LSP:E-LSP-PARAMS UNAPPLIED
   s\" \"file:///habu-lsp-test/a%00.f\"" s" 1" s\" \"\"" OPEN-PARAMS
   s" textDocument/didOpen" PAR$ E-PATH-RANGE UNAPPLIED ;

\ didChange and didClose params the server cannot read, each logged.
: BAD-CHANGES ( -- )
   URI-A s" []" CHANGE-PARAMS
   s" textDocument/didChange" PAR$ LSP:E-LSP-PARAMS UNAPPLIED
   URI-A s" [1]" CHANGE-PARAMS
   s" textDocument/didChange" PAR$ LSP:E-LSP-PARAMS UNAPPLIED
   URI-A s" [{}]" CHANGE-PARAMS
   s" textDocument/didChange" PAR$ LSP:E-LSP-PARAMS UNAPPLIED
   s" textDocument/didClose" s\" {\"textDocument\":{}}" LSP:E-LSP-PARAMS UNAPPLIED ;

\ None of them stops the server, and the document open among them stays open.
: TEST-MALFORMED-PARAMS ( -- )
   s" malformed-params" CONVERSATION
   INITIALIZE
   BAD-OPENS
   URI-A OPEN-DOC
   BAD-CHANGES
   URI-A CLOSE-DOC
   SIGN-OFF
   0 CONVERSE
   CAPABILITIES
   URI-A PUBLISHED
   s" 2" NULL-RESULT
   ENDS ;

\ A lower-case name; Content-Type before, blanks and tabs around the value;
\ leading zeros, Content-Type after.
: TEST-FRAME-FORMS ( -- )
   s" frame-forms" CONVERSATION
   INITIALIZE$ s" content-length: " s\" \r\n\r\n" FORM
   s" 2" s" shutdown" ASK$
   s\" Content-Type: application/vscode-jsonrpc; charset=utf-8\r\nContent-Length: \t " s\" \t \r\n\r\n" FORM
   s" exit" s" " TELL$
   s" Content-Length: 000" s\" \r\nContent-Type: application/vscode-jsonrpc; charset=utf-8\r\n\r\n" FORM
   0 CONVERSE
   CAPABILITIES
   s" 2" NULL-RESULT
   ENDS ;

\ The server stops on the fault's code, and nothing after the fault is read.
: FAULTED ( n -- )
   STOPPED
   1 CONVERSE
   ENDS ;

\ A header line of LINE-CAP + 1 bytes.
: LONG-LINE ( -- )
   s" X-Pad: " IN+
   CONTENT-LENGTH:LINE-CAP 6 - 0 ?do s" a" IN+ loop
   s\" \r\n\r\n" IN+ ;

: TEST-MALFORMED-FRAMES ( -- )
   s" fault-no-length" CONVERSATION
   s\" Content-Type: application/vscode-jsonrpc; charset=utf-8\r\n\r\n{}" IN+
   INITIALIZE
   E-CONTENT-LENGTH-MALFORMED FAULTED
   s" fault-not-a-number" CONVERSATION
   s\" Content-Length: abc\r\n\r\n" IN+
   INITIALIZE
   E-CONTENT-LENGTH-MALFORMED FAULTED
   s" fault-lf-alone" CONVERSATION
   s\" Content-Length: 2\n\n{}" IN+
   INITIALIZE
   E-CONTENT-LENGTH-MALFORMED FAULTED
   s" fault-long-line" CONVERSATION
   LONG-LINE
   INITIALIZE
   E-CONTENT-LENGTH-MALFORMED FAULTED
   s" fault-over-maximum" CONVERSATION
   s" Content-Length: " IN+ LSP:MAX-BODY 1+ INT$ IN+ s\" \r\n\r\n" IN+
   INITIALIZE
   E-CONTENT-LENGTH-MALFORMED FAULTED
   s" fault-after-request" CONVERSATION
   INITIALIZE
   s\" Content-Length: abc\r\n\r\n" IN+
   E-CONTENT-LENGTH-MALFORMED STOPPED
   1 CONVERSE
   CAPABILITIES
   ENDS ;

: TEST-TRUNCATED-FRAMES ( -- )
   s" fault-eof-in-header" CONVERSATION
   s\" Content-Length: 5\r\n" IN+
   E-CONTENT-LENGTH-TRUNCATED FAULTED
   s" fault-eof-in-body" CONVERSATION
   s\" Content-Length: 5\r\n\r\n{}" IN+
   E-CONTENT-LENGTH-TRUNCATED FAULTED ;

\ ---- diagnostics conversations, held turn by turn --------------------------------

\ Waits until the server's stdin takes a write; past DEADLINE it throws
\ E-PROC-TIMEOUT.
: WRITABLE ( -- )
   IN-W @ POLLOUT PROC-PFD!
   1 DEADLINE @ PROC-LEFT-MS MS>N DEADLINE @ PROC-POLL-RESTART {: rc:n :}
   rc 0 < if E-PROC-OUTPUT throw then
   rc 0= if E-PROC-TIMEOUT throw then ;

\ Writes a turn: the input not yet written, in one write. Each turn but the
\ first waits for output the server writes only once it has read all it was
\ sent, so the pipe is empty and takes one piece whole: the turn's messages
\ reach the server together, and no check runs between them.
: SAY ( -- )
   IN$ {: a:ptr u:n :}
   u SENT @ - {: left:n :}
   left PROC-STDIN-CHUNK-CAP <= s" turn past one write" HOLDS
   WRITABLE
   IN-W @ a SENT @ + left FD-IO:WRITE-FULL
   u SENT ! ;

\ Writes the next piece of the input, PIPE-ATOM bytes at most, once the pipe
\ has room for it.
: PIECE ( -- )
   IN$ {: a:ptr u:n :}
   u SENT @ - PIPE-ATOM min {: k:n :}
   WRITABLE
   IN-W @ a SENT @ + k FD-IO:WRITE-FULL
   k SENT +! ;

\ Writes the input not yet written piece by piece, so a server that stopped
\ reading cannot hold the test past DEADLINE.
: STREAM ( -- )
   begin SENT @ IN$ nip < while PIECE repeat ;

\ Whether the output not yet read as frames holds a whole frame.
: WHOLE? ( -- bool )
   UNREAD {: a:ptr u:n :}
   a u FRAME-LEN {: len:n :}
   len 0 < if false exit then
   len HEAD$ nip len + u <= ;

\ Reads stdout until the output not yet read as frames holds a whole frame,
\ stdout ends or the output fills its span.
: HEAR ( -- )
   begin
      WHOLE? 0= OUT-LEN @ OUT-SPAN SPAN:LEN < and if
         OUT-LEN @ OUT-R @ OUT-SPAN DEADLINE @ BYTE
      else false then
   while 1 OUT-LEN +! repeat ;

\ Reads stderr until it holds as many bytes as the conversation must write
\ there, or ends: a check that publishes nothing is over once it has said why.
: LOGGED ( -- )
   begin
      ERR-LEN @ WANT$ nip < ERR-LEN @ ERR-SPAN SPAN:LEN < and if
         ERR-LEN @ ERR-R @ ERR-SPAN DEADLINE @ BYTE
      else false then
   while 1 ERR-LEN +! repeat ;

\ stdout and stderr read on to their ends.
: DRAINED ( -- )
   OUT-R @ OUT-SPAN OUT-LEN DEADLINE @ DRAIN
   ERR-R @ ERR-SPAN ERR-LEN DEADLINE @ DRAIN ;

\ The conversation's turns, then shutdown and exit.
: FINISHED ( [ -- ] -- [ -- ] )
   dup execute
   SIGN-OFF
   SAY ;

\ Holds the conversation of these turns with the server, every wait against
\ one deadline CHECKED-MS from its start. Shutdown and exit end it, stdout and
\ stderr are read to their ends, and the server must exit 0 with the null
\ result to shutdown its last frame. A throw kills the server and fails the
\ conversation by name.
: HELD ( [ -- ] -- )
   {: turns :}
   PIPE-PAIR {: in-r:fd in-w:fd :}
   PIPE-PAIR {: out-r:fd out-w:fd :}
   PIPE-PAIR {: err-r:fd err-w:fd :}
   in-w FD-CLOEXEC!
   in-w FD-NOSIGPIPE!
   out-r FD-CLOEXEC!
   err-r FD-CLOEXEC!
   in-r out-w err-w SPAWN {: pid :}
   in-r FD>N close
   out-w FD>N close
   err-w FD>N close
   in-w IN-W !
   out-r OUT-R !
   err-r ERR-R !
   0 SENT !
   mono-ns {: t0:n :}
   CHECKED-MS >MS PROC-DEADLINE-AT DEADLINE !
   turns [: FINISHED ;] catch {: code:n :}
   drop
   in-w FD>N close
   code 0= if [: DRAINED ;] catch else code then {: why:n :}
   out-r FD>N close
   err-r FD>N close
   mono-ns t0 - TOOK
   why 0= if 0 else E-PROC-TIMEOUT then pid SETTLED {: r :}
   SB-RESET LABEL$ SB-APPEND s" : throw" SB-APPEND SB$ T-LABEL
   why 0 T=
   r 0 EXITED
   why 0<> if exit then
   s" 2" NULL-RESULT
   ENDS ;

\ Holds the conversation of this name and these turns.
: TALK ( ptr u8 n [ -- ] -- )
   {: a:ptr u:n turns :}
   a u CONVERSATION
   turns HELD ;

\ ---- the diagnostics expected ----------------------------------------------------

variable PACKET-NEXT                     \ where the check's next packet starts

\ The test's own check of the text as the file at PATH, whose packets and prose
\ the server's check of them must publish and write, given what is left of the
\ conversation's time. The words below read it.
: CHECKS ( ptr u8 n ptr u8 n -- )
   0 PACKET-NEXT !
   DEADLINE @ PROC-LEFT-MS CHECK:VERIFY-BYTES MATCH CHECK:verdict
      verified OF ENDOF
      refused OF ENDOF
      engine-provided OF ENDOF
      held OF ENDOF
      incomplete OF PROC-OUTCOME>RC drop ENDOF
      deferred OF ENDOF
   ;MATCH ;

\ The check's packet AT bytes into its output: the line from there, empty
\ past the last.
: PACKET-FROM$ ( n -- ptr u8 n )
   CHECK:VERIFY-OUT$ {: at:n a:ptr u:n :}
   at u min {: from:n :}
   a from + u from - {: p:ptr pu:n :}
   p pu 10 INDEX-OF MATCH option
      none OF p pu ENDOF
      some OF IDX>N p swap ENDOF
   ;MATCH ;

\ The raw text of a top-level member of the packet, a string's between its
\ quotes, and whether it is a string.
: MEMBER$ ( ptr u8 n ptr u8 n -- ptr u8 n bool )
   {: a:ptr u:n k:ptr ku:n :}
   JR-ST JR:STORAGE-BYTES a u JR:INIT
   JR:NEXT drop
   k ku JR:FIND-KEY 0= if JR:CLOSE NULL$ false exit then
   JR:TOKEN >r JR:SPAN$ rot JR:CLOSE r> JR:T-STR = ;

\ The message the packet's diagnostic carries: the packet's message, or else
\ its code, its suggestion when not empty, and its expected and actual, each on
\ a line of its own.
: MESSAGE+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   s\" \"" EXP+
   a u s" message" MEMBER$ if EXP+ s\" \"" EXP+ exit then
   2drop
   a u s" code" MEMBER$ drop EXP+
   a u s" suggestion" MEMBER$ over 0 > and if s" \n" EXP+ EXP+ else 2drop then
   a u s" expected" MEMBER$ if s" \nexpected: " EXP+ EXP+ else 2drop then
   a u s" actual" MEMBER$ if s" \nactual: " EXP+ EXP+ else 2drop then
   s\" \"" EXP+ ;

\ The publish the next frame must be, for the document or the file at PATH,
\ with this version, or without one for -1: begun, up to its list's bracket.
: EXPECT ( ptr u8 n n -- )
   {: p:ptr pu:n v:n :}
   EXP-B CLEAR
   s\" {\"jsonrpc\":\"2.0\",\"method\":\"textDocument/publishDiagnostics\",\"params\":{\"uri\":\"" EXP+
   p pu URI-OF EXP+ s\" \"," EXP+
   v 0 >= if s\" \"version\":" EXP+ v INT$ EXP+ s" ," EXP+ then
   s\" \"diagnostics\":[" EXP+ ;

\ The diagnostic the list holds next, begun: its range, from line L1,
\ character C1 to line L2, character C2.
: RANGE+ ( n n n n -- )
   {: l1:n c1:n l2:n c2:n :}
   EXP$ s" [" ENDS-WITH? 0= if s" ," EXP+ then
   s\" {\"range\":{\"start\":{\"line\":" EXP+ l1 INT$ EXP+
   s\" ,\"character\":" EXP+ c1 INT$ EXP+
   s\" },\"end\":{\"line\":" EXP+ l2 INT$ EXP+
   s\" ,\"character\":" EXP+ c2 INT$ EXP+
   s\" }}" EXP+ ;

\ The check's next packet for this code, advancing to the following packet.
: PACKET-FOR ( ptr u8 n -- ptr u8 n )
   {: c:ptr cu:n :}
   PACKET-NEXT @ PACKET-FROM$ {: a:ptr u:n :}
   SB-RESET LABEL$ SB-APPEND s" : a packet for " SB-APPEND c cu SB-APPEND SB$ T-LABEL
   u 0 > TTRUE
   u 0= if a u exit then
   PACKET-NEXT @ u + 1+ PACKET-NEXT !
   a u ;

\ The diagnostic ended: this code, then its packet's message and data.
: CODED+ ( ptr u8 n ptr u8 n -- )
   {: c:ptr cu:n a:ptr u:n :}
   s\" ,\"code\":\"" EXP+ c cu EXP+
   s\" \",\"source\":\"habu\",\"message\":" EXP+ a u MESSAGE+
   s\" ,\"data\":" EXP+ a u EXP+ s" }" EXP+ ;

\ The diagnostic the list holds next: the check's next packet, from line L1,
\ character C1 to line L2, character C2, with this severity and code.
: DIAG+ ( n n n n n ptr u8 n -- )
   {: l1:n c1:n l2:n c2:n sev:n c:ptr cu:n :}
   c cu PACKET-FOR {: a:ptr u:n :}
   u 0= if exit then
   l1 c1 l2 c2 RANGE+
   s\" ,\"severity\":" EXP+ sev INT$ EXP+
   c cu a u CODED+ ;

\ DIAG+ for a packet with no verdict, whose diagnostic has no severity.
: UNRATED+ ( n n n n ptr u8 n -- )
   {: l1:n c1:n l2:n c2:n c:ptr cu:n :}
   c cu PACKET-FOR {: a:ptr u:n :}
   u 0= if exit then
   l1 c1 l2 c2 RANGE+
   c cu a u CODED+ ;

\ The next frame is the publish expected.
: PUBLISHES ( -- )
   s" ]}}" EXP+
   HEAR
   EXP$ HEARD ;

\ ---- diagnostics ------------------------------------------------------------------

\ F's drop takes the only cell its effect leaves: E-MISMATCH at bytes 15-19.
: TEXT-F ( -- ptr u8 n )  s" : F ( n -- n ) drop ;" ;

\ The list of the document at PATH holding F's packet, at this version.
: F-LISTED ( ptr u8 n n -- )
   {: p:ptr pu:n v:n :}
   TEXT-F p pu CHECKS
   p pu s" refused" COMPLETED
   p pu v EXPECT 0 15 0 19 1 s" E-MISMATCH" DIAG+ PUBLISHES ;

\ The first turn: initialize and A opened as F.
: F-OPENED ( -- )
   INITIALIZE
   A-PATH TEXT-F 1 OPENS
   SAY
   HEAR CAPABILITIES
   A-PATH 1 F-LISTED ;

\ An undefined word refuses LSPT-U1. LSPT-U2 uses its declared effect and
\ certifies; LSPT-U3 calls it with nothing on the stack and LSPT-U4 leaves a
\ cell short, and each is refused.
: TEXT-U ( -- ptr u8 n )
   s\" : LSPT-U1 ( n -- n ) NOPE ;\n: LSPT-U2 ( n -- n ) LSPT-U1 1 + ;\n: LSPT-U3 ( -- n ) LSPT-U1 ;\n: LSPT-U4 ( n -- n ) drop ;\n" ;

\ A opened as F, then changed to U: every refusal listed, in order.
: OPEN-TURNS ( -- )
   F-OPENED
   A-PATH TEXT-U 2 CHANGES
   SAY
   TEXT-U A-PATH CHECKS
   A-PATH s" refused" COMPLETED
   A-PATH 2 EXPECT
   0 21 0 25 1 s" E-UNDEFINED" DIAG+
   2 19 2 26 1 s" E-INPUT-UNDERFLOW" DIAG+
   3 21 3 25 1 s" E-MISMATCH" DIAG+
   PUBLISHES ;

\ Three changes in one turn: the last alone is checked.
: SUPERSEDE-TURNS ( -- )
   F-OPENED
   A-PATH s" : S ( n -- n ) ;" 2 CHANGES
   A-PATH s" y" 3 CHANGES
   A-PATH s\" ( v4 )\n: S ( n -- n ) drop ;" 4 CHANGES
   SAY
   s\" ( v4 )\n: S ( n -- n ) drop ;" A-PATH CHECKS
   A-PATH s" refused" COMPLETED
   A-PATH 4 EXPECT 1 15 1 19 1 s" E-MISMATCH" DIAG+ PUBLISHES ;

\ X has an unknown signature type and an undefined word: the load refuses the
\ word, so the diagnostic is E-UNDEFINED at NOPE, not at zz.
: TEXT-X ( -- ptr u8 n )  s" : X ( n -- zz ) NOPE ;" ;

: ONE-REFUSAL-TURNS ( -- )
   F-OPENED
   A-PATH TEXT-X 2 CHANGES
   SAY
   TEXT-X A-PATH CHECKS
   A-PATH s" refused" COMPLETED
   A-PATH 2 EXPECT 0 16 0 20 1 s" E-UNDEFINED" DIAG+ PUBLISHES ;

: REQUIRES-FMT ( -- ptr u8 n )  s\" require lib/fmt.f\n: G ( n -- ) FMT:SB-U ;\n" ;
: UNDERFLOWS-FMT ( -- ptr u8 n )  s\" require lib/fmt.f\n: G ( -- ) FMT:SB-U ;\n" ;

\ A requires a file of the tree: checked in A's load context, it verifies, and
\ changed to call that file's word without its input, it is refused.
: REQUIRE-TURNS ( -- )
   INITIALIZE
   A-PATH REQUIRES-FMT 1 OPENS
   SAY
   HEAR CAPABILITIES
   REQUIRES-FMT A-PATH CHECKS
   A-PATH s" verified" COMPLETED
   A-PATH 1 EXPECT PUBLISHES
   A-PATH UNDERFLOWS-FMT 2 CHANGES
   SAY
   UNDERFLOWS-FMT A-PATH CHECKS
   A-PATH s" refused" COMPLETED
   A-PATH 2 EXPECT 1 11 1 19 1 s" E-INPUT-UNDERFLOW" DIAG+ PUBLISHES ;

\ A and B, waiting together, are checked in turn, and a save checks both again.
: SAVE-TURNS ( -- )
   INITIALIZE
   A-PATH TEXT-F 1 OPENS
   B-PATH TEXT-F 1 OPENS
   SAY
   HEAR CAPABILITIES
   A-PATH 1 F-LISTED
   B-PATH 1 F-LISTED
   A-PATH SAVES
   SAY
   A-PATH 1 F-LISTED
   B-PATH 1 F-LISTED ;

\ A close publishes an empty list without a version. A opened and closed in one
\ turn is never checked: that list is all it gets.
: CLOSE-TURNS ( -- )
   F-OPENED
   A-PATH CLOSES
   SAY
   A-PATH -1 EXPECT PUBLISHES
   A-PATH TEXT-F 1 OPENS
   A-PATH CLOSES
   SAY
   A-PATH -1 EXPECT PUBLISHES ;

\ The engine provides lib/string.f, so nothing is checked whatever its text.
: ENGINE-TURNS ( -- )
   INITIALIZE
   s" lib/string.f" IN-TREE TEXT-F 1 OPENS
   SAY
   HEAR CAPABILITIES
   s" lib/string.f" IN-TREE 1 EXPECT PUBLISHES ;

\ The emoji before H's drop is four bytes and two UTF-16 units.
: TEXT-H ( -- ptr u8 n )  s" ( 😀 ) : H ( n -- n ) drop ;" ;

: UTF16-TURNS ( -- )
   INITIALIZE
   A-PATH TEXT-H 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-H A-PATH CHECKS
   A-PATH s" refused" COMPLETED
   A-PATH 1 EXPECT 0 22 0 26 1 s" E-MISMATCH" DIAG+ PUBLISHES ;

: BIG-PATH ( -- ptr u8 n )  s" big.f" FIXTURE ;

\ big-frame's document, about 700 KB: BIG-LINES comment lines, then F.
: BIG-TEXT ( -- )
   TXT-B CLEAR
   BIG-LINES 0 ?do s\" ( twenty-byte line )\n" N>BLEN TXT-B APPEND-SPAN loop
   TEXT-F N>BLEN TXT-B APPEND-SPAN ;

\ The document is written in many pieces; F's packet on its last line proves
\ it taken whole and checked.
: BIG-TURNS ( -- )
   INITIALIZE
   SAY
   HEAR CAPABILITIES
   BIG-TEXT
   BIG-PATH TXT$ 1 OPENS
   STREAM
   TXT$ BIG-PATH CHECKS
   BIG-PATH s" refused" COMPLETED
   BIG-PATH 1 EXPECT BIG-LINES 15 BIG-LINES 19 1 s" E-MISMATCH" DIAG+ PUBLISHES ;

: S-PATH ( -- ptr u8 n )  s" s.f" FIXTURE ;
: DEP-PATH ( -- ptr u8 n )  s" dep.f" FIXTURE ;
: TEXT-S ( -- ptr u8 n )  s\" require dep.f\n: S ( -- ) ;\n" ;
: DEP-BROKEN ( -- ptr u8 n )  s\" : D ( n -- n ) drop ;\n" ;
: DEP-FIXED ( -- ptr u8 n )  s\" : D ( n -- n ) ;\n" ;

\ dep.f on disk holds this text.
: DEP-WRITTEN ( ptr u8 n -- )
   {: a:ptr u:n :}
   DEP-PATH a u WRITE-ALL ;

\ The path the packets name dep.f by: its canonical one.
: DEP-CANON ( -- ptr u8 n )  DEP-PATH SOURCE-ROOT:CANONICAL drop ;

\ The empty list of the document at PATH at this version, after the test's
\ own check of this text there, with this verdict.
: LISTED ( ptr u8 n ptr u8 n n ptr u8 n -- )
   {: t:ptr tu:n p:ptr pu:n v:n w:ptr wu:n :}
   t tu p pu CHECKS
   p pu w wu COMPLETED
   p pu v EXPECT PUBLISHES ;

\ The list at PATH, with this version or none for -1, holding the broken D's
\ packet, which the test's last check found first.
: D-LISTED ( ptr u8 n n -- )
   EXPECT 0 15 0 19 1 s" E-MISMATCH" DIAG+ PUBLISHES ;

\ S requires dep.f, which is not open. Broken on disk, dep.f gets a list of its
\ own; fixed and saved, S's check withdraws it. Opened with its fix while
\ broken on disk, its own check's list stands, and S's check, which reads the
\ disk, publishes nothing over it: shutdown's reply comes next.
: DEPENDENCY-TURNS ( -- )
   DEP-BROKEN DEP-WRITTEN
   INITIALIZE
   S-PATH TEXT-S 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-S S-PATH 1 s" refused" LISTED
   DEP-CANON -1 D-LISTED
   DEP-FIXED DEP-WRITTEN
   S-PATH SAVES
   SAY
   TEXT-S S-PATH 1 s" verified" LISTED
   DEP-CANON -1 EXPECT PUBLISHES
   DEP-BROKEN DEP-WRITTEN
   DEP-PATH DEP-FIXED 1 OPENS
   SAY
   DEP-FIXED DEP-PATH CHECKS
   DEP-PATH s" verified" COMPLETED
   DEP-PATH 1 EXPECT PUBLISHES
   S-PATH TEXT-S 2 CHANGES
   SAY
   TEXT-S S-PATH 2 s" refused" LISTED ;

\ dep.f, broken on disk, opened and then closed: its close publishes its empty
\ list, and S's check, which left dep.f to that list while it was open,
\ publishes dep.f's list from disk again.
: DEP-CLOSE-TURNS ( -- )
   DEP-BROKEN DEP-WRITTEN
   INITIALIZE
   S-PATH TEXT-S 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-S S-PATH 1 s" refused" LISTED
   DEP-CANON -1 D-LISTED
   DEP-PATH DEP-BROKEN 1 OPENS
   SAY
   DEP-BROKEN DEP-PATH CHECKS
   DEP-PATH s" refused" COMPLETED
   DEP-PATH 1 D-LISTED
   DEP-PATH CLOSES
   SAY
   DEP-PATH -1 EXPECT PUBLISHES
   TEXT-S S-PATH 1 s" refused" LISTED
   DEP-CANON -1 D-LISTED ;

: LINK-S ( -- ptr u8 n )  s" link/s.f" FIXTURE ;
: LINK-DEP ( -- ptr u8 n )  s" link/dep.f" FIXTURE ;
: REAL-DEP ( -- ptr u8 n )  s" real/dep.f" FIXTURE ;

\ The path the packets name dep.f under real by.
: REAL-CANON ( -- ptr u8 n )  REAL-DEP SOURCE-ROOT:CANONICAL drop ;

\ The client names S and dep.f through link, a symlink to real, and the
\ packets name dep.f by its canonical path, under real. Broken on disk, dep.f
\ gets a list at that canonical URI. Fixed on disk and opened through link, it
\ has its own list at the link URI, and S's check after a save withdraws the
\ canonical one.
: DEP-SYMLINK-TURNS ( -- )
   s" real" FIXTURE MAKE-DIR
   s" real" s" link" FIXTURE MAKE-SYMLINK
   REAL-DEP DEP-BROKEN WRITE-ALL
   INITIALIZE
   LINK-S TEXT-S 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-S LINK-S 1 s" refused" LISTED
   REAL-CANON -1 D-LISTED
   REAL-DEP DEP-FIXED WRITE-ALL
   LINK-DEP DEP-FIXED 1 OPENS
   SAY
   DEP-FIXED LINK-DEP 1 s" verified" LISTED
   LINK-S SAVES
   SAY
   TEXT-S LINK-S 1 s" verified" LISTED
   REAL-CANON -1 EXPECT PUBLISHES
   DEP-FIXED LINK-DEP 1 s" verified" LISTED ;

: T-PATH ( -- ptr u8 n )  s" t.f" FIXTURE ;
: TEXT-T ( -- ptr u8 n )  s\" require dep.f\n: T ( -- ) ;\n" ;
: ALONE-S ( -- ptr u8 n )  s\" : S ( -- ) ;\n" ;
: ALONE-T ( -- ptr u8 n )  s\" : T ( -- ) ;\n" ;

\ S and T both require dep.f, broken on disk, and each one's check publishes
\ dep.f's list. S dropping its require leaves that list to T, whose check
\ publishes it again; T dropping its require too withdraws it.
: DEP-SHARED-TURNS ( -- )
   DEP-BROKEN DEP-WRITTEN
   INITIALIZE
   S-PATH TEXT-S 1 OPENS
   T-PATH TEXT-T 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-S S-PATH 1 s" refused" LISTED
   DEP-CANON -1 D-LISTED
   TEXT-T T-PATH 1 s" refused" LISTED
   DEP-CANON -1 D-LISTED
   S-PATH ALONE-S 2 CHANGES
   SAY
   ALONE-S S-PATH 2 s" verified" LISTED
   TEXT-T T-PATH 1 s" refused" LISTED
   DEP-CANON -1 D-LISTED
   T-PATH ALONE-T 2 CHANGES
   SAY
   ALONE-T T-PATH 2 s" verified" LISTED
   DEP-CANON -1 EXPECT PUBLISHES ;

: DEFINES-A ( -- ptr u8 n )  s" : LSPT-ONLY-A ( -- ) ;" ;
: CALLS-A ( -- ptr u8 n )  s" : LSPT-B ( -- ) LSPT-ONLY-A ;" ;

\ B calls the word only A defines: each check sees its own document alone.
: TWO-TURNS ( -- )
   INITIALIZE
   A-PATH DEFINES-A 1 OPENS
   B-PATH CALLS-A 1 OPENS
   SAY
   HEAR CAPABILITIES
   DEFINES-A A-PATH CHECKS
   A-PATH s" verified" COMPLETED
   A-PATH 1 EXPECT PUBLISHES
   CALLS-A B-PATH CHECKS
   B-PATH s" refused" COMPLETED
   B-PATH 1 EXPECT 0 16 0 27 1 s" E-UNDEFINED" DIAG+ PUBLISHES ;

\ A definition never ended: the verifier stops at its opener and refuses the
\ document, and the stop's record, the one `check.f --verify-only` writes, is
\ published there.
: UNENDED ( -- ptr u8 n )  s" : H6 ( n -- n ) 1" ;

: UNENDED-TURNS ( -- )
   INITIALIZE
   A-PATH UNENDED 1 OPENS
   SAY
   HEAR CAPABILITIES
   UNENDED A-PATH CHECKS
   A-PATH s" refused" COMPLETED
   A-PATH 1 EXPECT 0 0 0 1 1 s" E-STATEMENT-THROW" DIAG+ PUBLISHES ;

\ A definer with nothing after it: the verifier stops at it and refuses the
\ document, and the stop's record, the nominal pass's packet that
\ `check.f --verify-only` writes, is published there.
: MISSING-NAME ( -- ptr u8 n )  s" :" ;

: MISSING-NAME-TURNS ( -- )
   INITIALIZE
   A-PATH MISSING-NAME 1 OPENS
   SAY
   HEAR CAPABILITIES
   MISSING-NAME A-PATH CHECKS
   A-PATH s" refused" COMPLETED
   A-PATH 1 EXPECT 0 0 0 1 1 s" E-MISSING-NAME" DIAG+ PUBLISHES ;

\ One `using` more than the checker holds open at once (CK-USE-MAX): the
\ verifier dies in CHECKER-USING (`76 die`) and exits without a verdict, where
\ `--load` refuses that using as ENGINE-ERROR:USING-OVERFLOW. A bare
\ `generates:`, which this case read before, stops the verifier at the reader
\ now (7187, E-MISSING-NAME).
CK-USE-MAX 1 + constant OVER-USINGS

: OVER-USINGS+ ( -- )
   OVER-USINGS 0 ?do s\" using SOURCE-ROOT\n" N>BLEN TXT-B APPEND-SPAN loop ;

: OVER-USING-TEXT ( -- )
   TXT-B CLEAR  OVER-USINGS+ ;

: INCOMPLETE-TURNS ( -- )
   INITIALIZE
   OVER-USING-TEXT
   A-PATH TXT$ 1 OPENS
   SAY
   HEAR CAPABILITIES
   TXT$ A-PATH CHECKS
   A-PATH s" not checked: exit 76" SAID
   LOGGED ;

\ H1 is refused, then one `using` more than the checker holds open: the
\ verifier dies in CHECKER-USING (`76 die`) after writing H1's packet, which is
\ published. A bare `generates:`, which this case read before, stops the
\ verifier at the reader now (7187, E-MISSING-NAME).
: REFUSED-OVER-TEXT ( -- )
   TXT-B CLEAR
   s\" : H1 ( n -- n ) NOSUCHWORD ;\n" N>BLEN TXT-B APPEND-SPAN
   OVER-USINGS+ ;

: INCOMPLETE-PACKET-TURNS ( -- )
   INITIALIZE
   REFUSED-OVER-TEXT
   A-PATH TXT$ 1 OPENS
   SAY
   HEAR CAPABILITIES
   TXT$ A-PATH CHECKS
   A-PATH s" not checked: exit 76" SAID
   A-PATH 1 EXPECT 0 16 0 26 1 s" E-UNDEFINED" DIAG+ PUBLISHES ;

\ The verifier's own image holds tools/check-verify-child.f.
: HELD-PATH ( -- ptr u8 n )  s" tools/check-verify-child.f" IN-TREE ;

: HELD-TURNS ( -- )
   INITIALIZE
   HELD-PATH TEXT-F 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-F HELD-PATH CHECKS
   HELD-PATH s" not checked: the verifier's image holds it" SAID
   LOGGED ;

\ The public W moves other cells than the private W, whose tail it shares: the
\ packet is at the public definition's name. The refusal refuses W alone: the
\ check goes on, and M's extra cell on line 7 is published beside it.
: SHADOWS ( -- ptr u8 n )
   s\" package LSPT-Q\nprivate\n: W ( n -- ) drop ;\npublic\n: W ( -- ) ;\n;package\n: LSPT-M ( -- ) 1 ;\n" ;

: SHADOWED-TURNS ( -- )
   INITIALIZE
   A-PATH SHADOWS 1 OPENS
   SAY
   HEAR CAPABILITIES
   SHADOWS A-PATH CHECKS
   A-PATH s" refused" COMPLETED
   A-PATH 1 EXPECT 4 2 4 3 1 s" E-SHADOWED-ARITY" DIAG+
   6 16 6 17 1 s" E-MISMATCH" DIAG+ PUBLISHES ;

\ LSPT-WIDE's inferred effect has more type variables than a record holds: the
\ check verifies it and warns at its name that the effect is not recorded.
: WIDENS ( -- ptr u8 n )
   s\" defer LSPT-V14 ( -- a b c d e g h i j k l m o p )\n: LSPT-WIDE LSPT-V14 LSPT-V14 ;\n" ;

: NOT-RECORDED-TURNS ( -- )
   INITIALIZE
   A-PATH WIDENS 1 OPENS
   SAY
   HEAR CAPABILITIES
   WIDENS A-PATH CHECKS
   A-PATH s" verified" COMPLETED
   A-PATH 1 EXPECT 1 2 1 11 s" W-EFFECT-NOT-RECORDED" UNRATED+ PUBLISHES ;

\ U's bare LSPT-W resolves in both used packages: E-USING-AMBIGUOUS, at the
\ token on line 10. The refusal refuses U alone: the check goes on, and M's
\ extra cell on line 11 is published beside it.
: TWO-USED ( -- ptr u8 n )
   s\" package LSPT-UA\npublic\n: LSPT-W ( n -- ) drop ;\n;package\npackage LSPT-UB\npublic\n: LSPT-W ( n -- ) drop ;\n;package\nusing LSPT-UA\nusing LSPT-UB\n: LSPT-U ( -- ) 1 LSPT-W ;\n: LSPT-M ( -- ) 1 ;\n;using\n;using\n" ;

: TWO-USED-TURNS ( -- )
   INITIALIZE
   A-PATH TWO-USED 1 OPENS
   SAY
   HEAR CAPABILITIES
   TWO-USED A-PATH CHECKS
   A-PATH s" refused" COMPLETED
   A-PATH 1 EXPECT 10 18 10 24 1 s" E-USING-AMBIGUOUS" DIAG+
   11 16 11 17 1 s" E-MISMATCH" DIAG+ PUBLISHES ;

\ U's bare LSPT-SW names the global and the used public both:
\ E-USING-SHADOW-GLOBAL at the token on line 6, and M's extra cell on line 7.
: GLOBAL-USED ( -- ptr u8 n )
   s\" : LSPT-SW ( n -- ) drop ;\npackage LSPT-SP\npublic\n: LSPT-SW ( n -- ) drop ;\n;package\nusing LSPT-SP\n: LSPT-SU ( -- ) 1 LSPT-SW ;\n: LSPT-M ( -- ) 1 ;\n;using\n" ;

: GLOBAL-USED-TURNS ( -- )
   INITIALIZE
   A-PATH GLOBAL-USED 1 OPENS
   SAY
   HEAR CAPABILITIES
   GLOBAL-USED A-PATH CHECKS
   A-PATH s" refused" COMPLETED
   A-PATH 1 EXPECT 6 19 6 26 1 s" E-USING-SHADOW-GLOBAL" DIAG+
   7 16 7 17 1 s" E-MISMATCH" DIAG+ PUBLISHES ;

\ The same refusals in does> clauses each refuse their definer alone:
\ E-USING-SHADOW-GLOBAL at DS's bare LSPT-SW on line 12, E-USING-AMBIGUOUS at
\ DM's bare LSPT-DV on line 13, and M's extra cell on line 14.
: CLAUSE-USED ( -- ptr u8 n )
   s\" : LSPT-SW ( n -- ) drop ;\npackage LSPT-DA\npublic\n: LSPT-SW ( n -- ) drop ;\n: LSPT-DV ( n -- ) drop ;\n;package\npackage LSPT-DB\npublic\n: LSPT-DV ( n -- ) drop ;\n;package\nusing LSPT-DA\nusing LSPT-DB\n: LSPT-DS ( n -- ) create , does> ( -- ) @ LSPT-SW ;\n: LSPT-DM ( n -- ) create , does> ( -- ) @ LSPT-DV ;\n: LSPT-M ( -- ) 1 ;\n;using\n;using\n" ;

: CLAUSE-USED-TURNS ( -- )
   INITIALIZE
   A-PATH CLAUSE-USED 1 OPENS
   SAY
   HEAR CAPABILITIES
   CLAUSE-USED A-PATH CHECKS
   A-PATH s" refused" COMPLETED
   A-PATH 1 EXPECT 12 43 12 50 1 s" E-USING-SHADOW-GLOBAL" DIAG+
   13 43 13 50 1 s" E-USING-AMBIGUOUS" DIAG+
   14 16 14 17 1 s" E-MISMATCH" DIAG+ PUBLISHES ;

\ DA defined twice: the packet at the second DA, line 1, characters 2-4.
: TEXT-DUP ( -- ptr u8 n )  s\" : DA ( -- n ) 1 ;\n: DA ( -- n ) 2 ;\n" ;

: DUPLICATE-TURNS ( -- )
   INITIALIZE
   A-PATH TEXT-DUP 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-DUP A-PATH CHECKS
   A-PATH s" refused" COMPLETED
   A-PATH 1 EXPECT 1 2 1 4 1 s" E-DUPLICATE-DEFINITION" DIAG+ PUBLISHES ;

\ The document defines again the word the file it requires defines: the packet
\ is the document's, at its LSPT-DD, line 1, characters 2-9.
: DD-DEP ( -- ptr u8 n )  s" dd-dep.f" FIXTURE ;
: TEXT-DD ( -- ptr u8 n )  s\" require dd-dep.f\n: LSPT-DD ( -- n ) 2 ;\n" ;

: DUP-REQUIRED-TURNS ( -- )
   DD-DEP s\" : LSPT-DD ( -- n ) 1 ;\n" WRITE-ALL
   INITIALIZE
   A-PATH TEXT-DD 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-DD A-PATH CHECKS
   A-PATH s" refused" COMPLETED
   A-PATH 1 EXPECT 1 2 1 9 1 s" E-DUPLICATE-DEFINITION" DIAG+ PUBLISHES ;

\ A top-level word no scope defines, at bytes 2-12; then the line after a word
\ that parses its own input, deferred to the run where the word stands.
: UNDEFINED-AT-TOP ( -- ptr u8 n )  s\" 1 NOSUCHWORD drop\n" ;
: DEFERRED-AT-TOP ( -- ptr u8 n )  s\" : GRAB ( -- ) parse-name 2drop ;\nGRAB x\n" ;

: TOP-LEVEL-TURNS ( -- )
   INITIALIZE
   A-PATH UNDEFINED-AT-TOP 1 OPENS
   SAY
   HEAR CAPABILITIES
   UNDEFINED-AT-TOP A-PATH CHECKS
   A-PATH s" refused" COMPLETED
   A-PATH 1 EXPECT 0 2 0 12 1 s" E-UNDEFINED-TOP-LEVEL" DIAG+ PUBLISHES
   A-PATH DEFERRED-AT-TOP 2 CHANGES
   SAY
   DEFERRED-AT-TOP A-PATH CHECKS
   A-PATH s" deferred" COMPLETED
   A-PATH 2 EXPECT 1 0 1 4 3 s" W-CHECK-DEFERRED" DIAG+ PUBLISHES ;

\ A body naming a word only the text `evaluate` renders may define, deferred to
\ the run at that name.
: DEFERRED-IN-BODY ( -- ptr u8 n )  s\" s\" : G ( -- n ) 2 ;\" evaluate\n: F ( -- n ) NOSUCH ;\n" ;

: DEFINITION-TURNS ( -- )
   INITIALIZE
   A-PATH DEFERRED-IN-BODY 1 OPENS
   SAY
   HEAR CAPABILITIES
   DEFERRED-IN-BODY A-PATH CHECKS
   A-PATH s" deferred" COMPLETED
   A-PATH 1 EXPECT 1 13 1 19 3 s" W-CHECK-DEFERRED" DIAG+ PUBLISHES ;

\ That deferral, then a definition never ended: the verifier stops at its
\ opener and refuses the document, and both the deferral and the stop's record
\ are published.
: DEFERRED-UNENDED-TEXT ( -- )
   TXT-B CLEAR
   DEFERRED-IN-BODY N>BLEN TXT-B APPEND-SPAN
   UNENDED N>BLEN TXT-B APPEND-SPAN ;

: DEFERRED-UNENDED-TURNS ( -- )
   INITIALIZE
   DEFERRED-UNENDED-TEXT
   A-PATH TXT$ 1 OPENS
   SAY
   HEAR CAPABILITIES
   TXT$ A-PATH CHECKS
   A-PATH s" refused" COMPLETED
   A-PATH 1 EXPECT
   1 13 1 19 3 s" W-CHECK-DEFERRED" DIAG+
   2 0 2 1 1 s" E-STATEMENT-THROW" DIAG+ PUBLISHES ;

\ ---- workspace symbols ------------------------------------------------------------

: SYM-A-PATH ( -- ptr u8 n )  s" sym-a.f" FIXTURE ;
: SYM-B-PATH ( -- ptr u8 n )  s" sym-b.f" FIXTURE ;
: SYM-DEP-PATH ( -- ptr u8 n )  s" sym-dep.f" FIXTURE ;
: SYM-DEP-CANON ( -- ptr u8 n )  SYM-DEP-PATH SOURCE-ROOT:CANONICAL drop ;
: TEXT-SYM-A ( -- ptr u8 n )  s\" require sym-dep.f\n: QUX-A ( -- n ) 1 ;\n: ZED ( -- n ) 4 ;\n" ;
: TEXT-SYM-A2 ( -- ptr u8 n )  s\" require sym-dep.f\n: QUX-AA ( -- n ) 1 ;\n" ;
: TEXT-SYM-B ( -- ptr u8 n )  s\" require sym-dep.f\n: QUX-B ( -- n ) 2 ;\n3 constant QUX-K\n" ;
: TEXT-SYM-DEP ( -- ptr u8 n )
   s\" package QD\n: QUX-P ( -- n ) 5 ;\n: QE:QUX-Q ( -- n ) 6 ;\npublic\n: QUX-DEP ( -- n ) 3 ;\nEXPORT QE:QUX-Q\n;package\nvariable QUX-V\n" ;

\ workspace/symbol, by its id's JSON text, with these params' JSON text.
: SYMBOLS-ASK ( ptr u8 n ptr u8 n -- )
   {: i:ptr iu:n p:ptr pu:n :}
   MSG-B CLEAR
   s\" {\"jsonrpc\":\"2.0\",\"id\":" MSG+ i iu MSG+
   s\" ,\"method\":\"workspace/symbol\",\"params\":" MSG+ p pu MSG+ s" }" MSG+
   MSG$ FRAMED ;

\ The answer to the request with this id's JSON text, begun in MSG, up to its
\ list's bracket.
: SYMBOLS-START ( ptr u8 n -- )
   {: i:ptr iu:n :}
   MSG-B CLEAR
   s\" {\"jsonrpc\":\"2.0\",\"id\":" MSG+ i iu MSG+ s\" ,\"result\":[" MSG+ ;

\ The symbol the answer lists next: its name, kind and URI, the range from
\ character C1 to C2 of line L, and its package, none when empty.
: SYMBOL+ ( ptr u8 n n ptr u8 n n n n ptr u8 n -- )
   {: w:ptr wu:n k:n u:ptr uu:n l:n c1:n c2:n p:ptr pu:n :}
   MSG$ s" [" ENDS-WITH? 0= if s" ," MSG+ then
   s\" {\"name\":\"" MSG+ w wu MSG+
   s\" \",\"kind\":" MSG+ k INT$ MSG+
   s\" ,\"location\":{\"uri\":\"" MSG+ u uu MSG+
   s\" \",\"range\":{\"start\":{\"line\":" MSG+ l INT$ MSG+
   s\" ,\"character\":" MSG+ c1 INT$ MSG+
   s\" },\"end\":{\"line\":" MSG+ l INT$ MSG+
   s\" ,\"character\":" MSG+ c2 INT$ MSG+ s" }}}" MSG+
   pu 0 > if s\" ,\"containerName\":\"" MSG+ p pu MSG+ s\" \"" MSG+ then
   s" }" MSG+ ;

\ The next frame is the answer begun.
: SYMBOLS-END ( -- )
   s" ]}" MSG+
   HEAR
   MSG$ HEARD ;

\ textDocument/documentSymbol, by its id's JSON text, of the document opened
\ from this path.
: OUTLINE-ASK ( ptr u8 n ptr u8 n -- )
   {: i:ptr iu:n p:ptr pu:n :}
   MSG-B CLEAR
   s\" {\"jsonrpc\":\"2.0\",\"id\":" MSG+ i iu MSG+
   s\" ,\"method\":\"textDocument/documentSymbol\",\"params\":{\"textDocument\":{\"uri\":\"" MSG+
   p pu URI-OF MSG+ s\" \"}}}" MSG+
   MSG$ FRAMED ;

\ A request by this method and id's JSON text at character C of line L of
\ the document opened from this path.
: AT-ASK ( ptr u8 n ptr u8 n ptr u8 n n n -- )
   {: m:ptr mu:n i:ptr iu:n p:ptr pu:n l:n c:n :}
   MSG-B CLEAR
   s\" {\"jsonrpc\":\"2.0\",\"id\":" MSG+ i iu MSG+
   s\" ,\"method\":\"" MSG+ m mu MSG+
   s\" \",\"params\":{\"textDocument\":{\"uri\":\"" MSG+
   p pu URI-OF MSG+
   s\" \"},\"position\":{\"line\":" MSG+ l INT$ MSG+
   s\" ,\"character\":" MSG+ c INT$ MSG+ s" }}}" MSG+
   MSG$ FRAMED ;

\ textDocument/definition, by its id's JSON text, at character C of line L of
\ the document opened from this path.
: DEFINITION-ASK ( ptr u8 n ptr u8 n n n -- )
   {: i:ptr iu:n p:ptr pu:n l:n c:n :}
   s" textDocument/definition" i iu p pu l c AT-ASK ;

\ The next frame answers the request with this id's JSON text with one
\ Location: this URI, from character C1 to C2 of line L.
: LOCATED ( ptr u8 n ptr u8 n n n n -- )
   {: i:ptr iu:n u:ptr uu:n l:n c1:n c2:n :}
   MSG-B CLEAR
   s\" {\"jsonrpc\":\"2.0\",\"id\":" MSG+ i iu MSG+
   s\" ,\"result\":[{\"uri\":\"" MSG+ u uu MSG+
   s\" \",\"range\":{\"start\":{\"line\":" MSG+ l INT$ MSG+
   s\" ,\"character\":" MSG+ c1 INT$ MSG+
   s\" },\"end\":{\"line\":" MSG+ l INT$ MSG+
   s\" ,\"character\":" MSG+ c2 INT$ MSG+ s" }}}]}" MSG+
   HEAR
   MSG$ HEARD ;

\ The dependency's symbols, each under the package the checker recorded it
\ in, folded: QUX-P, private in QD, bare; QE:QUX-Q, public in QE, by its
\ tail; QUX-DEP, public in QD, qualified by the package as recorded; QE:QUX-Q
\ re-exported public in QD, by its tail under QD; and the global QUX-V, bare,
\ in none.
: DEP-SYMBOLS+ ( -- )
   s" QUX-P" 12 SYM-DEP-CANON URI-OF 1 2 7 s" qd" SYMBOL+
   s" qe:QUX-Q" 12 SYM-DEP-CANON URI-OF 2 2 10 s" qe" SYMBOL+
   s" qd:QUX-DEP" 12 SYM-DEP-CANON URI-OF 4 2 9 s" qd" SYMBOL+
   s" qd:QUX-Q" 12 SYM-DEP-CANON URI-OF 5 7 15 s" qd" SYMBOL+
   s" QUX-V" 13 SYM-DEP-CANON URI-OF 7 9 14 s" " SYMBOL+ ;

\ The dependency changed on disk: QUX-NEW where QUX-DEP was.
: TEXT-SYM-DEP2 ( -- ptr u8 n )
   s\" package QD\npublic\n: QUX-NEW ( -- n ) 3 ;\n;package\nvariable QUX-V\n" ;

: DEP2-SYMBOLS+ ( -- )
   s" qd:QUX-NEW" 12 SYM-DEP-CANON URI-OF 2 2 9 s" qd" SYMBOL+
   s" QUX-V" 13 SYM-DEP-CANON URI-OF 4 9 14 s" " SYMBOL+ ;

\ Two open documents require sym-dep.f, on disk only. A query in another case
\ lists each definition whose word holds it once, the dependency's at its
\ canonical file URI and positions in its text on disk, the documents' at the
\ URIs the client opened them by, check after check, oldest first; the word
\ ZED is not one. A word public in a package is named qualified by it, any
\ other bare. A changed document's check replaces its definitions. A query
\ holding a colon lists as well each word a caller spells qualified whose
\ package is the text before the colon, whole, and whose tail holds the text
\ after it, in another case: QUX-DEP and the re-exported QUX-Q, not QD's
\ private QUX-P nor QE's QUX-Q; QD:QE fits no name, so lists none.
\ A query that is no string, or none, is -32602. A file both documents'
\ checks reached is listed once, from the later check: sym-dep.f changed on
\ disk and B checked again, B's QUX-NEW and not A's QUX-DEP; B closed, A's
\ QUX-DEP again, answered before A's next check.
: SYMBOL-TURNS ( -- )
   SYM-DEP-PATH TEXT-SYM-DEP WRITE-ALL
   INITIALIZE
   SYM-A-PATH TEXT-SYM-A 1 OPENS
   SYM-B-PATH TEXT-SYM-B 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-SYM-A SYM-A-PATH 1 s" verified" LISTED
   TEXT-SYM-B SYM-B-PATH 1 s" verified" LISTED
   s" 3" s\" {\"query\":\"qux\"}" SYMBOLS-ASK
   SAY
   s" 3" SYMBOLS-START
   s" QUX-A" 12 SYM-A-PATH URI-OF 1 2 7 s" " SYMBOL+
   DEP-SYMBOLS+
   s" QUX-B" 12 SYM-B-PATH URI-OF 1 2 7 s" " SYMBOL+
   s" QUX-K" 14 SYM-B-PATH URI-OF 2 11 16 s" " SYMBOL+
   SYMBOLS-END
   SYM-A-PATH TEXT-SYM-A2 2 CHANGES
   SAY
   TEXT-SYM-A2 SYM-A-PATH 2 s" verified" LISTED
   s" 4" s\" {\"query\":\"Qux-\"}" SYMBOLS-ASK
   SAY
   s" 4" SYMBOLS-START
   s" QUX-B" 12 SYM-B-PATH URI-OF 1 2 7 s" " SYMBOL+
   s" QUX-K" 14 SYM-B-PATH URI-OF 2 11 16 s" " SYMBOL+
   DEP-SYMBOLS+
   s" QUX-AA" 12 SYM-A-PATH URI-OF 1 2 8 s" " SYMBOL+
   SYMBOLS-END
   s" 5" s\" {\"query\":\"QD:qux\"}" SYMBOLS-ASK
   SAY
   s" 5" SYMBOLS-START
   s" qd:QUX-DEP" 12 SYM-DEP-CANON URI-OF 4 2 9 s" qd" SYMBOL+
   s" qd:QUX-Q" 12 SYM-DEP-CANON URI-OF 5 7 15 s" qd" SYMBOL+
   SYMBOLS-END
   s" 10" s\" {\"query\":\"QD:qe\"}" SYMBOLS-ASK
   SAY
   s" 10" SYMBOLS-START SYMBOLS-END
   s" 6" s\" {\"query\":7}" SYMBOLS-ASK
   s" 7" s" workspace/symbol" ASK
   SAY
   HEAR s" 6" -32602 REFUSED
   HEAR s" 7" -32602 REFUSED
   SYM-DEP-PATH TEXT-SYM-DEP2 WRITE-ALL
   SYM-B-PATH TEXT-SYM-B 2 CHANGES
   SAY
   TEXT-SYM-B SYM-B-PATH 2 s" verified" LISTED
   s" 8" s\" {\"query\":\"qux\"}" SYMBOLS-ASK
   SAY
   s" 8" SYMBOLS-START
   s" QUX-AA" 12 SYM-A-PATH URI-OF 1 2 8 s" " SYMBOL+
   DEP2-SYMBOLS+
   s" QUX-B" 12 SYM-B-PATH URI-OF 1 2 7 s" " SYMBOL+
   s" QUX-K" 14 SYM-B-PATH URI-OF 2 11 16 s" " SYMBOL+
   SYMBOLS-END
   SYM-B-PATH CLOSES
   s" 9" s\" {\"query\":\"qux\"}" SYMBOLS-ASK
   SAY
   SYM-B-PATH -1 EXPECT PUBLISHES
   s" 9" SYMBOLS-START
   DEP-SYMBOLS+
   s" QUX-AA" 12 SYM-A-PATH URI-OF 1 2 8 s" " SYMBOL+
   SYMBOLS-END
   TEXT-SYM-A2 SYM-A-PATH 2 s" verified" LISTED ;

\ The dependency emptied on disk.
: TEXT-SYM-DEP3 ( -- ptr u8 n )  s\" \\ no definition remains\n" ;

\ Two open documents require sym-dep.f, on disk only, which then loses every
\ definition, and B is checked again: B's check read it, so none of its
\ definitions is listed, not even A's older ones; B closed, A's again,
\ answered before A's next check.
: EMPTY-TURNS ( -- )
   SYM-DEP-PATH TEXT-SYM-DEP WRITE-ALL
   INITIALIZE
   SYM-A-PATH TEXT-SYM-A 1 OPENS
   SYM-B-PATH TEXT-SYM-B 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-SYM-A SYM-A-PATH 1 s" verified" LISTED
   TEXT-SYM-B SYM-B-PATH 1 s" verified" LISTED
   SYM-DEP-PATH TEXT-SYM-DEP3 WRITE-ALL
   SYM-B-PATH TEXT-SYM-B 2 CHANGES
   SAY
   TEXT-SYM-B SYM-B-PATH 2 s" verified" LISTED
   s" 3" s\" {\"query\":\"qux\"}" SYMBOLS-ASK
   SAY
   s" 3" SYMBOLS-START
   s" QUX-A" 12 SYM-A-PATH URI-OF 1 2 7 s" " SYMBOL+
   s" QUX-B" 12 SYM-B-PATH URI-OF 1 2 7 s" " SYMBOL+
   s" QUX-K" 14 SYM-B-PATH URI-OF 2 11 16 s" " SYMBOL+
   SYMBOLS-END
   SYM-B-PATH CLOSES
   s" 4" s\" {\"query\":\"qux\"}" SYMBOLS-ASK
   SAY
   SYM-B-PATH -1 EXPECT PUBLISHES
   s" 4" SYMBOLS-START
   DEP-SYMBOLS+
   s" QUX-A" 12 SYM-A-PATH URI-OF 1 2 7 s" " SYMBOL+
   SYMBOLS-END
   TEXT-SYM-A SYM-A-PATH 1 s" verified" LISTED ;

\ NAV-RETAIN declared, then declared again with a bare ptr, an effect the
\ registrar refuses.
: TEXT-RETAINED ( -- ptr u8 n )
   s\" TRUSTED: NAV-RETAIN ( -- n ) 1 ;\nTRUSTED: NAV-RETAIN ( -- ptr ) ;\n" ;

\ NAV-WIDE declared, then declared again taking 256 cells, a row too wide to
\ record, which the registrar refuses; NAV-SAME declared twice with one effect,
\ both retained; NAV-ODD malformed with nothing declared before it; NAV-KEPT's
\ body refused with its signature kept; NAV-LATER after them.
: REDECLARED-TEXT ( -- )
   TXT-B CLEAR
   s\" TRUSTED: NAV-WIDE ( n -- ) drop ;\nTRUSTED: NAV-WIDE (" N>BLEN TXT-B APPEND-SPAN
   256 0 ?do s"  n" N>BLEN TXT-B APPEND-SPAN loop
   s\"  -- ) ;\nTRUSTED: NAV-SAME ( -- n ) 1 ;\nTRUSTED: NAV-SAME ( -- n ) 1 ;\n" N>BLEN TXT-B APPEND-SPAN
   s\" TRUSTED: NAV-ODD ( -- ptr ) ;\n: NAV-KEPT ( -- n n ) 8 ;\n: NAV-LATER ( -- n ) 2 ;\n" N>BLEN TXT-B APPEND-SPAN ;

: RETAINED-PATH ( -- ptr u8 n )  s" retained.f" FIXTURE ;

: REDECLARED-PATH ( -- ptr u8 n )  s" redeclared.f" FIXTURE ;

\ The refused redeclaration keeps no record, so a query lists NAV-RETAIN once,
\ where the declaration the registrar retained names it.
: RETAINED-TURNS ( -- )
   INITIALIZE
   RETAINED-PATH TEXT-RETAINED 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-RETAINED RETAINED-PATH CHECKS
   RETAINED-PATH s" refused" COMPLETED
   RETAINED-PATH 1 EXPECT 1 9 1 19 1 s" E-BAD-STORED-SIGNATURE" DIAG+ PUBLISHES
   s" 3" s\" {\"query\":\"nav-retain\"}" SYMBOLS-ASK
   SAY
   s" 3" SYMBOLS-START
   s" NAV-RETAIN" 12 RETAINED-PATH URI-OF 0 9 19 s" " SYMBOL+
   SYMBOLS-END ;

\ Each refusal is published, and a query lists NAV-WIDE where the declaration
\ the registrar retained names it, NAV-SAME at each declaration, NAV-KEPT and
\ NAV-LATER, and not NAV-ODD.
: REDECLARED-TURNS ( -- )
   REDECLARED-TEXT
   INITIALIZE
   REDECLARED-PATH TXT$ 1 OPENS
   SAY
   HEAR CAPABILITIES
   TXT$ REDECLARED-PATH CHECKS
   REDECLARED-PATH s" refused" COMPLETED
   REDECLARED-PATH 1 EXPECT
   1 9 1 17 1 s" E-BAD-STORED-SIGNATURE" DIAG+
   4 9 4 16 1 s" E-BAD-STORED-SIGNATURE" DIAG+
   5 22 5 23 1 s" E-MISMATCH" DIAG+
   PUBLISHES
   s" 3" s\" {\"query\":\"nav\"}" SYMBOLS-ASK
   SAY
   s" 3" SYMBOLS-START
   s" NAV-WIDE" 12 REDECLARED-PATH URI-OF 0 9 17 s" " SYMBOL+
   s" NAV-SAME" 12 REDECLARED-PATH URI-OF 2 9 17 s" " SYMBOL+
   s" NAV-SAME" 12 REDECLARED-PATH URI-OF 3 9 17 s" " SYMBOL+
   s" NAV-KEPT" 12 REDECLARED-PATH URI-OF 5 2 10 s" " SYMBOL+
   s" NAV-LATER" 12 REDECLARED-PATH URI-OF 6 2 11 s" " SYMBOL+
   SYMBOLS-END ;

\ rep.f includes rep-dep.f, on disk only, undefines its word and includes it
\ again.
: REP-DEP-PATH ( -- ptr u8 n )  s" rep-dep.f" FIXTURE ;
: REP-DEP-CANON ( -- ptr u8 n )  REP-DEP-PATH SOURCE-ROOT:CANONICAL drop ;
: REP-PATH ( -- ptr u8 n )  s" rep.f" FIXTURE ;
: TEXT-REP-DEP ( -- ptr u8 n )  s\" : REV-SHARED ( -- n ) 7 ;\n" ;
: TEXT-REP ( -- ptr u8 n )
   s\" include rep-dep.f\nundefine REV-SHARED\ninclude rep-dep.f\n" ;

\ The check reads rep-dep.f twice, and a query lists REV-SHARED once.
: REINCLUDE-TURNS ( -- )
   REP-DEP-PATH TEXT-REP-DEP WRITE-ALL
   INITIALIZE
   REP-PATH TEXT-REP 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-REP REP-PATH 1 s" verified" LISTED
   s" 3" s\" {\"query\":\"rev-shared\"}" SYMBOLS-ASK
   SAY
   s" 3" SYMBOLS-START
   s" REV-SHARED" 12 REP-DEP-CANON URI-OF 0 2 12 s" " SYMBOL+
   SYMBOLS-END ;

\ od-dep.f on disk, and the other text its open document holds, whose first
\ line is longer in bytes than in characters.
: OD-DEP-PATH ( -- ptr u8 n )  s" od-dep.f" FIXTURE ;
: OD-PATH ( -- ptr u8 n )  s" od.f" FIXTURE ;
: TEXT-OD-DISK ( -- ptr u8 n )  s\" ( disk )\n: REV-DISK ( -- n ) 7 ;\n" ;
: TEXT-OD-EDIT ( -- ptr u8 n )
   s\" ( unsaved é🙂 comment )\n: REV-EDIT ( -- n ) 8 ;\n" ;
: TEXT-OD ( -- ptr u8 n )  s\" require od-dep.f\n: REV-A ( -- n ) 1 ;\n" ;

\ od-twin.f, a symlink to od-dep.f, and the other text its open document
\ holds, whose definitions are on other lines than od-dep.f's.
: OD-TWIN-PATH ( -- ptr u8 n )  s" od-twin.f" FIXTURE ;
: TEXT-OD-TWIN ( -- ptr u8 n )
   s\" \\ twin\n\n: REV-TWIN ( -- n ) 9 ;\n: REV-USE ( -- n ) REV-TWIN ;\n" ;

\ od-dep.f open with text its disk file lacks, then od.f, which requires the
\ file, open: a query lists REV-EDIT where the document places it, from its
\ own check, and REV-A, and not REV-DISK, which od.f's check read from disk;
\ od-dep.f's outline, after od.f's check, is REV-EDIT alone. The file open
\ again through od-twin.f with its own text: each document's outline is its
\ own definitions, at the URI the client opened it by, and a use in od-twin.f
\ is at its declaration there.
\ od-twin.f closed, and both documents checked again; then od-dep.f closed,
\ REV-DISK where the file on disk places it, answered before od.f's next
\ check.
: OPEN-DEP-TURNS ( -- )
   OD-DEP-PATH TEXT-OD-DISK WRITE-ALL
   INITIALIZE
   OD-DEP-PATH TEXT-OD-EDIT 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-OD-EDIT OD-DEP-PATH 1 s" verified" LISTED
   OD-PATH TEXT-OD 1 OPENS
   SAY
   TEXT-OD OD-PATH 1 s" verified" LISTED
   s" 3" s\" {\"query\":\"rev-\"}" SYMBOLS-ASK
   s" 4" OD-DEP-PATH OUTLINE-ASK
   SAY
   s" 3" SYMBOLS-START
   s" REV-EDIT" 12 OD-DEP-PATH URI-OF 1 2 10 s" " SYMBOL+
   s" REV-A" 12 OD-PATH URI-OF 1 2 7 s" " SYMBOL+
   SYMBOLS-END
   s" 4" SYMBOLS-START
   s" REV-EDIT" 12 OD-DEP-PATH URI-OF 1 2 10 s" " SYMBOL+
   SYMBOLS-END
   s" od-dep.f" OD-TWIN-PATH MAKE-SYMLINK
   OD-TWIN-PATH TEXT-OD-TWIN 1 OPENS
   SAY
   TEXT-OD-TWIN OD-TWIN-PATH 1 s" verified" LISTED
   s" 5" OD-TWIN-PATH OUTLINE-ASK
   s" 6" OD-DEP-PATH OUTLINE-ASK
   s" 7" OD-TWIN-PATH 3 19 DEFINITION-ASK
   SAY
   s" 5" SYMBOLS-START
   s" REV-TWIN" 12 OD-TWIN-PATH URI-OF 2 2 10 s" " SYMBOL+
   s" REV-USE" 12 OD-TWIN-PATH URI-OF 3 2 9 s" " SYMBOL+
   SYMBOLS-END
   s" 6" SYMBOLS-START
   s" REV-EDIT" 12 OD-DEP-PATH URI-OF 1 2 10 s" " SYMBOL+
   SYMBOLS-END
   s" 7" OD-TWIN-PATH URI-OF 2 2 10 LOCATED
   OD-TWIN-PATH CLOSES
   SAY
   OD-TWIN-PATH -1 EXPECT PUBLISHES
   TEXT-OD-EDIT OD-DEP-PATH 1 s" verified" LISTED
   TEXT-OD OD-PATH 1 s" verified" LISTED
   OD-DEP-PATH CLOSES
   s" 8" s\" {\"query\":\"rev-\"}" SYMBOLS-ASK
   SAY
   OD-DEP-PATH -1 EXPECT PUBLISHES
   s" 8" SYMBOLS-START
   s" REV-DISK" 12 OD-DEP-PATH URI-OF 1 2 10 s" " SYMBOL+
   s" REV-A" 12 OD-PATH URI-OF 1 2 7 s" " SYMBOL+
   SYMBOLS-END
   TEXT-OD OD-PATH 1 s" verified" LISTED ;

\ path-length's document: the global word Q, then a global word of two bytes on
\ each line after it, QA to Q9, XA to X9 and ZA to Z9.
: LENGTH-TEXT ( -- )
   TXT-B CLEAR
   s\" : Q ( -- ) ;\n" N>BLEN TXT-B APPEND-SPAN
   3 0 ?do
      36 0 ?do
         s" : " N>BLEN TXT-B APPEND-SPAN
         s" QXZ" drop j + c@ TXT-B APPEND-BYTE
         s" ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" drop i + c@ TXT-B APPEND-BYTE
         s\"  ( -- ) ;\n" N>BLEN TXT-B APPEND-SPAN
      loop
   loop ;

variable LENGTH-N                        \ the length of its directory's name

: LENGTH-DIR ( -- ptr u8 n )  s" dddd" drop LENGTH-N @ FIXTURE ;

: LENGTH-PATH ( -- ptr u8 n )
   LENGTH-DIR 2drop
   s" /g.f" N>BLEN PATH-B APPEND-SPAN
   PATH-B SPAN$ BLEN>N ;

\ The document is checked and published, and Z9, its last word, answered.
: LENGTH-TURNS ( -- )
   INITIALIZE
   LENGTH-PATH TXT$ 1 OPENS
   SAY
   HEAR CAPABILITIES
   TXT$ LENGTH-PATH 1 s" verified" LISTED
   s" 3" s\" {\"query\":\"z9\"}" SYMBOLS-ASK
   SAY
   s" 3" SYMBOLS-START
   s" Z9" 12 LENGTH-PATH URI-OF 108 2 4 s" " SYMBOL+
   SYMBOLS-END ;

\ ---- go to definition --------------------------------------------------------

: DEF-A-PATH ( -- ptr u8 n )  s" def-a.f" FIXTURE ;
: DEF-B-PATH ( -- ptr u8 n )  s" def-b.f" FIXTURE ;
: DEF-DEP-PATH ( -- ptr u8 n )  s" def-dep.f" FIXTURE ;
: DEF-DEP-CANON ( -- ptr u8 n )  DEF-DEP-PATH SOURCE-ROOT:CANONICAL drop ;

\ DEF-ONE, its use in DEF-TWO, and its use in DEF-THREE after a character of
\ one UTF-16 unit and two bytes.
: TEXT-DEF-A ( -- ptr u8 n )
   s\" : DEF-ONE ( -- n ) 1 ;\n: DEF-TWO ( -- n ) DEF-ONE 1 + ;\n: DEF-THREE ( -- n ) s\" é\" 2drop DEF-ONE ;\n" ;

\ def-dep.f, on disk only: DEF-PUB, public in package DD, and the global
\ DEF-DEP.
: TEXT-DEF-DEP ( -- ptr u8 n )
   s\" package DD\npublic\n: DEF-PUB ( -- n ) 3 ;\n;package\n: DEF-DEP ( -- n ) 4 ;\n" ;

\ Uses of def-dep.f's global and, qualified, of its public word.
: TEXT-DEF-B ( -- ptr u8 n )
   s\" require def-dep.f\n: DEF-USE ( -- n ) DEF-DEP DD:DEF-PUB + ;\n" ;

\ The next frame answers the request with this id's JSON text with no
\ Location.
: NOWHERE ( ptr u8 n -- )
   {: i:ptr iu:n :}
   MSG-B CLEAR
   s\" {\"jsonrpc\":\"2.0\",\"id\":" MSG+ i iu MSG+ s\" ,\"result\":[]}" MSG+
   HEAR
   MSG$ HEARD ;

\ DEF-ONE's uses, at the first and last character of the one in DEF-TWO and
\ the first of the one in DEF-THREE, its byte one past its character, each
\ answered with DEF-ONE's declaring token at the URI the document was opened
\ by; the character after the use and DEF-TWO's declaring token with none; a
\ negative character -32602.
: DEF-TURNS ( -- )
   INITIALIZE
   DEF-A-PATH TEXT-DEF-A 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-DEF-A DEF-A-PATH 1 s" verified" LISTED
   s" 3" DEF-A-PATH 1 19 DEFINITION-ASK
   s" 4" DEF-A-PATH 1 25 DEFINITION-ASK
   s" 5" DEF-A-PATH 2 33 DEFINITION-ASK
   s" 6" DEF-A-PATH 1 26 DEFINITION-ASK
   s" 7" DEF-A-PATH 1 2 DEFINITION-ASK
   s" 8" DEF-A-PATH 1 -1 DEFINITION-ASK
   SAY
   s" 3" DEF-A-PATH URI-OF 0 2 9 LOCATED
   s" 4" DEF-A-PATH URI-OF 0 2 9 LOCATED
   s" 5" DEF-A-PATH URI-OF 0 2 9 LOCATED
   s" 6" NOWHERE
   s" 7" NOWHERE
   HEAR s" 8" -32602 REFUSED ;

\ The uses of def-dep.f's global and, qualified, of its public word, each
\ answered with its declaring token in the text on disk at the file's
\ canonical URI.
: DEF-DEP-TURNS ( -- )
   DEF-DEP-PATH TEXT-DEF-DEP WRITE-ALL
   INITIALIZE
   DEF-B-PATH TEXT-DEF-B 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-DEF-B DEF-B-PATH 1 s" verified" LISTED
   s" 3" DEF-B-PATH 1 19 DEFINITION-ASK
   s" 4" DEF-B-PATH 1 27 DEFINITION-ASK
   SAY
   s" 3" DEF-DEP-CANON URI-OF 4 2 9 LOCATED
   s" 4" DEF-DEP-CANON URI-OF 2 2 9 LOCATED ;

\ def-od-dep.f on disk, and the text its open document holds, its two lines
\ swapped: DEF-DEPB's token there has the bytes DEF-DEPA's has on disk, and
\ DEF-DEPA's token, after DEF-DEPB's longer line, those of no token on disk.
: DEF-OD-DEP-PATH ( -- ptr u8 n )  s" def-od-dep.f" FIXTURE ;
: DEF-OD-DEP-CANON ( -- ptr u8 n )  DEF-OD-DEP-PATH SOURCE-ROOT:CANONICAL drop ;
: DEF-OD-PATH ( -- ptr u8 n )  s" def-od.f" FIXTURE ;
: TEXT-DEF-OD-DISK ( -- ptr u8 n )
   s\" : DEF-DEPA ( -- n ) 1 ;\n: DEF-DEPB ( -- n n ) 2 3 ;\n" ;
: TEXT-DEF-OD-EDIT ( -- ptr u8 n )
   s\" : DEF-DEPB ( -- n n ) 2 3 ;\n: DEF-DEPA ( -- n ) 1 ;\n" ;
: TEXT-DEF-OD ( -- ptr u8 n )
   s\" require def-od-dep.f\n: DEF-USE ( -- n ) DEF-DEPA DEF-DEPB + + ;\n" ;

\ def-od-dep.f open with its lines swapped, unsaved, then def-od.f, which
\ requires it and whose check reads it from disk: the uses of DEF-DEPA and
\ DEF-DEPB, each answered with its declaring token's range in the text on
\ disk, at the URI the client opened def-od-dep.f by.
: DEF-OPEN-DEP-TURNS ( -- )
   DEF-OD-DEP-PATH TEXT-DEF-OD-DISK WRITE-ALL
   INITIALIZE
   DEF-OD-DEP-PATH TEXT-DEF-OD-EDIT 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-DEF-OD-EDIT DEF-OD-DEP-PATH 1 s" verified" LISTED
   DEF-OD-PATH TEXT-DEF-OD 1 OPENS
   SAY
   TEXT-DEF-OD DEF-OD-PATH 1 s" verified" LISTED
   s" 3" DEF-OD-PATH 1 19 DEFINITION-ASK
   s" 4" DEF-OD-PATH 1 28 DEFINITION-ASK
   SAY
   s" 3" DEF-OD-DEP-PATH URI-OF 0 2 10 LOCATED
   s" 4" DEF-OD-DEP-PATH URI-OF 1 2 10 LOCATED ;

\ A position in a document the client never opened: -32602.
: DEF-NOT-OPEN-TURNS ( -- )
   INITIALIZE
   s" 3" DEF-A-PATH 1 19 DEFINITION-ASK
   SAY
   HEAR CAPABILITIES
   HEAR s" 3" -32602 REFUSED ;

\ A use asked about with the document's opening, before the server checked
\ it: the request checks it first, so its list comes before the answer, the
\ declaration's.
: DEF-BEFORE-CHECK-TURNS ( -- )
   INITIALIZE
   DEF-A-PATH TEXT-DEF-A 1 OPENS
   s" 3" DEF-A-PATH 1 19 DEFINITION-ASK
   SAY
   HEAR CAPABILITIES
   TEXT-DEF-A DEF-A-PATH 1 s" verified" LISTED
   s" 3" DEF-A-PATH URI-OF 0 2 9 LOCATED ;

\ TEXT-DEF-A two lines down.
: TEXT-DEF-A-MOVED ( -- ptr u8 n )
   s\" \\ two lines\n\\ above\n: DEF-ONE ( -- n ) 1 ;\n: DEF-TWO ( -- n ) DEF-ONE 1 + ;\n: DEF-THREE ( -- n ) s\" é\" 2drop DEF-ONE ;\n" ;

\ DEF-ONE's use in DEF-TWO asked about at its new place in the turn of the
\ change that moved it and DEF-ONE two lines down, with no check between
\ them: the request checks the changed text first, so its list comes before
\ the answer, DEF-ONE's new declaring token.
: DEF-AFTER-CHANGE-TURNS ( -- )
   INITIALIZE
   DEF-A-PATH TEXT-DEF-A 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-DEF-A DEF-A-PATH 1 s" verified" LISTED
   DEF-A-PATH TEXT-DEF-A-MOVED 2 CHANGES
   s" 3" DEF-A-PATH 3 19 DEFINITION-ASK
   SAY
   TEXT-DEF-A-MOVED DEF-A-PATH 2 s" verified" LISTED
   s" 3" DEF-A-PATH URI-OF 2 2 9 LOCATED ;

\ DEF-ONE and DEF-TWO, used in that order.
: TEXT-DEF-C ( -- ptr u8 n )
   s\" : DEF-ONE ( -- n ) 1 ;\n: DEF-TWO ( -- n ) 2 ;\n: DEF-SUM ( -- n ) DEF-ONE DEF-TWO + ;\n" ;

\ TEXT-DEF-C with its uses swapped, then one `using` more than the checker
\ holds open, at which the verifier dies (`76 die`) and exits without a
\ verdict as in incomplete. A bare `generates:`, which this case read before,
\ is refused at the reader now (7187, E-MISSING-NAME).
: TEXT-DEF-C-SWAPPED ( -- ptr u8 n )
   TXT-B CLEAR
   s\" : DEF-ONE ( -- n ) 1 ;\n: DEF-TWO ( -- n ) 2 ;\n: DEF-SUM ( -- n ) DEF-TWO DEF-ONE + ;\n"
   N>BLEN TXT-B APPEND-SPAN
   OVER-USINGS+
   TXT$ ;

\ DEF-TWO's use, at the bytes of DEF-ONE's before the change, asked about in
\ the turn of a change whose check did not complete: the uses of the text
\ before it would answer DEF-ONE's declaring token, so the answer is no
\ Location; the definitions of the last completed check are still listed.
: DEF-INCOMPLETE-TURNS ( -- )
   INITIALIZE
   DEF-A-PATH TEXT-DEF-C 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-DEF-C DEF-A-PATH 1 s" verified" LISTED
   DEF-A-PATH TEXT-DEF-C-SWAPPED 2 CHANGES
   s" 3" DEF-A-PATH 2 19 DEFINITION-ASK
   s" 4" s\" {\"query\":\"def-\"}" SYMBOLS-ASK
   SAY
   TEXT-DEF-C-SWAPPED DEF-A-PATH CHECKS
   DEF-A-PATH s" not checked: exit 76" SAID
   s" 3" NOWHERE
   s" 4" SYMBOLS-START
   s" DEF-ONE" 12 DEF-A-PATH URI-OF 0 2 9 s" " SYMBOL+
   s" DEF-TWO" 12 DEF-A-PATH URI-OF 1 2 9 s" " SYMBOL+
   s" DEF-SUM" 12 DEF-A-PATH URI-OF 2 2 9 s" " SYMBOL+
   SYMBOLS-END
   LOGGED ;

\ ---- document symbols --------------------------------------------------------

: SYM-NONE-PATH ( -- ptr u8 n )  s" sym-none.f" FIXTURE ;
: TEXT-SYM-NONE ( -- ptr u8 n )  s\" \\ no definition\n" ;

\ Three open documents, A and B requiring sym-dep.f, on disk only, and one
\ with no definition. Each outline lists its own document's definitions in
\ source order, each as workspace symbols give it, and none of the other
\ document's or of sym-dep.f, which both checks read; the document with none
\ answers an empty list. A document never opened, and params naming no
\ document, are -32602.
: OUTLINE-TURNS ( -- )
   SYM-DEP-PATH TEXT-SYM-DEP WRITE-ALL
   INITIALIZE
   SYM-A-PATH TEXT-SYM-A 1 OPENS
   SYM-B-PATH TEXT-SYM-B 1 OPENS
   SYM-NONE-PATH TEXT-SYM-NONE 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-SYM-A SYM-A-PATH 1 s" verified" LISTED
   TEXT-SYM-B SYM-B-PATH 1 s" verified" LISTED
   TEXT-SYM-NONE SYM-NONE-PATH 1 s" verified" LISTED
   s" 3" SYM-A-PATH OUTLINE-ASK
   s" 4" SYM-B-PATH OUTLINE-ASK
   s" 5" SYM-NONE-PATH OUTLINE-ASK
   s" 6" DEF-A-PATH OUTLINE-ASK
   s" 7" s" textDocument/documentSymbol" ASK
   SAY
   s" 3" SYMBOLS-START
   s" QUX-A" 12 SYM-A-PATH URI-OF 1 2 7 s" " SYMBOL+
   s" ZED" 12 SYM-A-PATH URI-OF 2 2 5 s" " SYMBOL+
   SYMBOLS-END
   s" 4" SYMBOLS-START
   s" QUX-B" 12 SYM-B-PATH URI-OF 1 2 7 s" " SYMBOL+
   s" QUX-K" 14 SYM-B-PATH URI-OF 2 11 16 s" " SYMBOL+
   SYMBOLS-END
   s" 5" SYMBOLS-START SYMBOLS-END
   HEAR s" 6" -32602 REFUSED
   HEAR s" 7" -32602 REFUSED ;

\ initialize from a client that supports hierarchical document symbols.
: HIERARCHICAL-CLIENT ( -- )
   MSG-B CLEAR
   s\" {\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"capabilities\":" MSG+
   s\" {\"textDocument\":{\"documentSymbol\":{\"hierarchicalDocumentSymbolSupport\":true}}}}}" MSG+
   MSG$ FRAMED ;

\ From a client that supports hierarchical document symbols, answered the same
\ flat list. A's outline asked in the turn of the change that removed QUX-A
\ and ZED: the request checks the changed text first, so its list comes before
\ the answer, QUX-AA alone. Asked again with a change of A read behind it, the
\ request is -32801 and the change is applied and checked after it; asked once
\ more, the restored definitions are answered without a check.
: OUTLINE-AFTER-CHANGE-TURNS ( -- )
   SYM-DEP-PATH TEXT-SYM-DEP WRITE-ALL
   HIERARCHICAL-CLIENT
   SYM-A-PATH TEXT-SYM-A 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-SYM-A SYM-A-PATH 1 s" verified" LISTED
   SYM-A-PATH TEXT-SYM-A2 2 CHANGES
   s" 3" SYM-A-PATH OUTLINE-ASK
   SAY
   TEXT-SYM-A2 SYM-A-PATH 2 s" verified" LISTED
   s" 3" SYMBOLS-START
   s" QUX-AA" 12 SYM-A-PATH URI-OF 1 2 8 s" " SYMBOL+
   SYMBOLS-END
   s" 4" SYM-A-PATH OUTLINE-ASK
   SYM-A-PATH TEXT-SYM-A 3 CHANGES
   SAY
   HEAR s" 4" -32801 REFUSED
   TEXT-SYM-A SYM-A-PATH 3 s" verified" LISTED
   s" 5" SYM-A-PATH OUTLINE-ASK
   SAY
   s" 5" SYMBOLS-START
   s" QUX-A" 12 SYM-A-PATH URI-OF 1 2 7 s" " SYMBOL+
   s" ZED" 12 SYM-A-PATH URI-OF 2 2 5 s" " SYMBOL+
   SYMBOLS-END ;

\ The outline asked in the turn of a change whose check did not complete: the
\ definitions of the text before it are not of the document's text, so the
\ answer is an empty list, while workspace symbols still list them.
: OUTLINE-INCOMPLETE-TURNS ( -- )
   INITIALIZE
   DEF-A-PATH TEXT-DEF-C 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-DEF-C DEF-A-PATH 1 s" verified" LISTED
   DEF-A-PATH TEXT-DEF-C-SWAPPED 2 CHANGES
   s" 3" DEF-A-PATH OUTLINE-ASK
   s" 4" s\" {\"query\":\"def-\"}" SYMBOLS-ASK
   SAY
   TEXT-DEF-C-SWAPPED DEF-A-PATH CHECKS
   DEF-A-PATH s" not checked: exit 76" SAID
   s" 3" SYMBOLS-START SYMBOLS-END
   s" 4" SYMBOLS-START
   s" DEF-ONE" 12 DEF-A-PATH URI-OF 0 2 9 s" " SYMBOL+
   s" DEF-TWO" 12 DEF-A-PATH URI-OF 1 2 9 s" " SYMBOL+
   s" DEF-SUM" 12 DEF-A-PATH URI-OF 2 2 9 s" " SYMBOL+
   SYMBOLS-END
   LOGGED ;

\ ---- hover -------------------------------------------------------------------

\ initialize from a client whose hovers take the formats this JSON array
\ text lists.
: INITIALIZE-AS ( ptr u8 n -- )
   {: f:ptr fu:n :}
   MSG-B CLEAR
   s\" {\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"capabilities\":{\"textDocument\":{\"hover\":{\"contentFormat\":" MSG+
   f fu MSG+ s" }}}}}" MSG+
   MSG$ FRAMED ;

: MARKDOWN-CLIENT ( -- )  s\" [\"markdown\",\"plaintext\"]" INITIALIZE-AS ;
: PLAIN-CLIENT ( -- )  s\" [\"plaintext\"]" INITIALIZE-AS ;

\ textDocument/hover, by its id's JSON text, at character C of line L of the
\ document opened from this path.
: HOVER-ASK ( ptr u8 n ptr u8 n n n -- )
   {: i:ptr iu:n p:ptr pu:n l:n c:n :}
   s" textDocument/hover" i iu p pu l c AT-ASK ;

: HV+ ( ptr u8 n -- )  N>BLEN TXT-B APPEND-SPAN ;

\ A path as a hover names its file: relative to the working directory, which
\ the server shares, when the file lies inside it, else the path.
: WHERE+ ( ptr u8 n -- )  SOURCE-ROOT:CWD$ SOURCE-ROOT:RELATIVE HV+ ;

\ The next frame answers the request with this id's JSON text with a hover of
\ this kind, whose value TXT-B holds, over characters C1 to C2 of line L.
: HOVERED ( ptr u8 n ptr u8 n n n n -- )
   {: i:ptr iu:n k:ptr ku:n l:n c1:n c2:n :}
   PAR-B CLEAR TXT$ TEXT+
   MSG-B CLEAR
   s\" {\"jsonrpc\":\"2.0\",\"id\":" MSG+ i iu MSG+
   s\" ,\"result\":{\"contents\":{\"kind\":\"" MSG+ k ku MSG+
   s\" \",\"value\":" MSG+ PAR$ MSG+
   s\" },\"range\":{\"start\":{\"line\":" MSG+ l INT$ MSG+
   s\" ,\"character\":" MSG+ c1 INT$ MSG+
   s\" },\"end\":{\"line\":" MSG+ l INT$ MSG+
   s\" ,\"character\":" MSG+ c2 INT$ MSG+ s" }}}}" MSG+
   HEAR
   MSG$ HEARD ;

\ hover-a.f, in the tree, the working directory the server shares, never on
\ disk: HV-ONE, private in package HV, and its use in HV-TWO.
: HOVER-A-PATH ( -- ptr u8 n )  s" hover-a.f" IN-TREE ;
: TEXT-HOVER-A ( -- ptr u8 n )
   s\" package HV\n: HV-ONE ( -- n ) 1 ;\n: HV-TWO ( -- n ) HV-ONE 1 + ;\n;package\n" ;

\ TEXT-HOVER-A two lines down.
: TEXT-HOVER-A-MOVED ( -- ptr u8 n )
   s\" \\ two lines\n\\ above\npackage HV\n: HV-ONE ( -- n ) 1 ;\n: HV-TWO ( -- n ) HV-ONE 1 + ;\n;package\n" ;

\ HV-ONE's lines, declared on this 1-based line.
: HV-ONE-LINES ( n -- )
   {: line:n :}
   s\" : HV-ONE ( -- n )\n\\ package hv, hover-a.f:" HV+
   SB-RESET line FMT:SB-INT SB$ HV+ ;

\ HV-ONE's lines as Markdown.
: HV-ONE-MD ( n -- )
   TXT-B CLEAR s\" ```habu\n" HV+ HV-ONE-LINES s\" \n```" HV+ ;

\ HV-ONE's use, at its first character and its last, answered with HV-ONE's
\ kind, word, effect, package and place, over the use.
: HOVER-TURNS ( -- )
   MARKDOWN-CLIENT
   HOVER-A-PATH TEXT-HOVER-A 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-HOVER-A HOVER-A-PATH 1 s" verified" LISTED
   s" 3" HOVER-A-PATH 2 18 HOVER-ASK
   s" 4" HOVER-A-PATH 2 23 HOVER-ASK
   SAY
   2 HV-ONE-MD s" 3" s" markdown" 2 18 24 HOVERED
   2 HV-ONE-MD s" 4" s" markdown" 2 18 24 HOVERED ;

\ The uses of def-dep.f's global and, qualified, of its public word, each
\ answered with its declaration in the file on disk.
: HOVER-DEP-TURNS ( -- )
   DEF-DEP-PATH TEXT-DEF-DEP WRITE-ALL
   MARKDOWN-CLIENT
   DEF-B-PATH TEXT-DEF-B 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-DEF-B DEF-B-PATH 1 s" verified" LISTED
   s" 3" DEF-B-PATH 1 19 HOVER-ASK
   s" 4" DEF-B-PATH 1 27 HOVER-ASK
   SAY
   TXT-B CLEAR s\" ```habu\n: DEF-DEP ( -- n )\n\\ " HV+ DEF-DEP-CANON WHERE+ s\" :5\n```" HV+
   s" 3" s" markdown" 1 19 26 HOVERED
   TXT-B CLEAR s\" ```habu\n: DEF-PUB ( -- n )\n\\ package dd, " HV+ DEF-DEP-CANON WHERE+ s\" :3\n```" HV+
   s" 4" s" markdown" 1 27 37 HOVERED ;

\ def-od-dep.f open with its lines swapped, unsaved, then def-od.f, which
\ requires it and whose check reads it from disk: the uses of DEF-DEPA and
\ DEF-DEPB, each answered with its declaration in the text on disk.
: HOVER-OPEN-DEP-TURNS ( -- )
   DEF-OD-DEP-PATH TEXT-DEF-OD-DISK WRITE-ALL
   MARKDOWN-CLIENT
   DEF-OD-DEP-PATH TEXT-DEF-OD-EDIT 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-DEF-OD-EDIT DEF-OD-DEP-PATH 1 s" verified" LISTED
   DEF-OD-PATH TEXT-DEF-OD 1 OPENS
   SAY
   TEXT-DEF-OD DEF-OD-PATH 1 s" verified" LISTED
   s" 3" DEF-OD-PATH 1 19 HOVER-ASK
   s" 4" DEF-OD-PATH 1 28 HOVER-ASK
   SAY
   TXT-B CLEAR s\" ```habu\n: DEF-DEPA ( -- n )\n\\ " HV+ DEF-OD-DEP-CANON WHERE+ s\" :1\n```" HV+
   s" 3" s" markdown" 1 19 27 HOVERED
   TXT-B CLEAR s\" ```habu\n: DEF-DEPB ( -- n n )\n\\ " HV+ DEF-OD-DEP-CANON WHERE+ s\" :2\n```" HV+
   s" 4" s" markdown" 1 28 36 HOVERED ;

\ HV-ONE's declaring token, at its first character and its last, answered
\ with HV-ONE, over the token.
: HOVER-DEF-TURNS ( -- )
   MARKDOWN-CLIENT
   HOVER-A-PATH TEXT-HOVER-A 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-HOVER-A HOVER-A-PATH 1 s" verified" LISTED
   s" 3" HOVER-A-PATH 1 2 HOVER-ASK
   s" 4" HOVER-A-PATH 1 7 HOVER-ASK
   SAY
   2 HV-ONE-MD s" 3" s" markdown" 1 2 8 HOVERED
   2 HV-ONE-MD s" 4" s" markdown" 1 2 8 HOVERED ;

\ hover-cvt.f, in the tree, never on disk: DEFTYPE CVT-T, public in package
\ HV-P, whose converters >CVT-T and CVT-T>N the check states at CVT-T's token;
\ a use of each in upper case and in lower case, and qualified ones outside HV-P.
: HOVER-CVT-PATH ( -- ptr u8 n )  s" hover-cvt.f" IN-TREE ;
: TEXT-HOVER-CVT ( -- ptr u8 n )
   s\" require lib/type/deftype.f\npackage HV-P\npublic\nDEFTYPE CVT-T\n: HV-CVT ( -- n ) 5 >CVT-T CVT-T>N >cvt-t cvt-t>n ;\n;package\n: HV-Q ( -- n ) 5 HV-P:>CVT-T HV-P:CVT-T>N ;\n" ;

\ The lines of CVT-T's converter W, of effect E, as Markdown.
: CVT-MD ( ptr u8 n ptr u8 n -- )
   {: w:ptr wu:n e:ptr eu:n :}
   TXT-B CLEAR s\" ```habu\nDEFTYPE " HV+ w wu HV+
   s"  ( " HV+ e eu HV+ s\"  )\n\\ package hv-p, hover-cvt.f:4\n```" HV+ ;

\ The use of each converter, in either case and qualified, answered with its
\ own definition, over the use: the two share one token, so the token alone
\ tells neither. A qualified use's whole spelling is neither converter's word,
\ so the server compares its tail, the bytes after its colon.
: HOVER-DEFTYPE-TURNS ( -- )
   MARKDOWN-CLIENT
   HOVER-CVT-PATH TEXT-HOVER-CVT 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-HOVER-CVT HOVER-CVT-PATH 1 s" verified" LISTED
   s" 3" HOVER-CVT-PATH 4 20 HOVER-ASK
   s" 4" HOVER-CVT-PATH 4 27 HOVER-ASK
   s" 5" HOVER-CVT-PATH 4 35 HOVER-ASK
   s" 6" HOVER-CVT-PATH 4 42 HOVER-ASK
   s" 7" HOVER-CVT-PATH 6 18 HOVER-ASK
   s" 8" HOVER-CVT-PATH 6 30 HOVER-ASK
   SAY
   s" >CVT-T" s" n -- cvt-t" CVT-MD s" 3" s" markdown" 4 20 26 HOVERED
   s" CVT-T>N" s" cvt-t -- n" CVT-MD s" 4" s" markdown" 4 27 34 HOVERED
   s" >CVT-T" s" n -- cvt-t" CVT-MD s" 5" s" markdown" 4 35 41 HOVERED
   s" CVT-T>N" s" cvt-t -- n" CVT-MD s" 6" s" markdown" 4 42 49 HOVERED
   s" >CVT-T" s" n -- cvt-t" CVT-MD s" 7" s" markdown" 6 18 29 HOVERED
   s" CVT-T>N" s" cvt-t -- n" CVT-MD s" 8" s" markdown" 6 30 42 HOVERED ;

\ A comment, the space before a use and a stack comment: null.
: HOVER-NONE-TURNS ( -- )
   MARKDOWN-CLIENT
   DEF-A-PATH TEXT-DEF-A-MOVED 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-DEF-A-MOVED DEF-A-PATH 1 s" verified" LISTED
   s" 3" DEF-A-PATH 0 3 HOVER-ASK
   s" 4" DEF-A-PATH 3 18 HOVER-ASK
   s" 5" DEF-A-PATH 3 12 HOVER-ASK
   SAY
   HEAR s" 3" NULL-RESULT
   HEAR s" 4" NULL-RESULT
   HEAR s" 5" NULL-RESULT ;

\ A client whose hovers take plain text only: HV-ONE's lines, unfenced.
: HOVER-PLAIN-TURNS ( -- )
   PLAIN-CLIENT
   HOVER-A-PATH TEXT-HOVER-A 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-HOVER-A HOVER-A-PATH 1 s" verified" LISTED
   s" 3" HOVER-A-PATH 2 18 HOVER-ASK
   SAY
   TXT-B CLEAR 2 HV-ONE-LINES
   s" 3" s" plaintext" 2 18 24 HOVERED ;

\ A position in a document the client never opened: -32602.
: HOVER-NOT-OPEN-TURNS ( -- )
   MARKDOWN-CLIENT
   s" 3" HOVER-A-PATH 2 18 HOVER-ASK
   SAY
   HEAR CAPABILITIES
   HEAR s" 3" -32602 REFUSED ;

\ HV-ONE's use asked about at its new place in the turn of the change that
\ moved it and HV-ONE two lines down: the request checks the changed text
\ first, so its list comes before the answer, HV-ONE on its new line.
: HOVER-AFTER-CHANGE-TURNS ( -- )
   MARKDOWN-CLIENT
   HOVER-A-PATH TEXT-HOVER-A 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-HOVER-A HOVER-A-PATH 1 s" verified" LISTED
   HOVER-A-PATH TEXT-HOVER-A-MOVED 2 CHANGES
   s" 3" HOVER-A-PATH 4 18 HOVER-ASK
   SAY
   TEXT-HOVER-A-MOVED HOVER-A-PATH 2 s" verified" LISTED
   4 HV-ONE-MD s" 3" s" markdown" 4 18 24 HOVERED ;

\ ---- document highlight ------------------------------------------------------

\ textDocument/documentHighlight, by its id's JSON text, at character C of
\ line L of the document opened from this path.
: HIGHLIGHT-ASK ( ptr u8 n ptr u8 n n n -- )
   {: i:ptr iu:n p:ptr pu:n l:n c:n :}
   s" textDocument/documentHighlight" i iu p pu l c AT-ASK ;

\ The highlight the answer SYMBOLS-START began lists next: a Text one from
\ character C1 to C2 of line L.
: HIGHLIGHT+ ( n n n -- )
   {: l:n c1:n c2:n :}
   MSG$ s" [" ENDS-WITH? 0= if s" ," MSG+ then
   s\" {\"range\":{\"start\":{\"line\":" MSG+ l INT$ MSG+
   s\" ,\"character\":" MSG+ c1 INT$ MSG+
   s\" },\"end\":{\"line\":" MSG+ l INT$ MSG+
   s\" ,\"character\":" MSG+ c2 INT$ MSG+ s\" }},\"kind\":1}" MSG+ ;

\ The next frame answers the request with this id's JSON text with DEF-ONE's
\ declaring token and its two uses, in TEXT-DEF-A moved this many lines down.
: DEF-ONE-LIT ( ptr u8 n n -- )
   {: i:ptr iu:n down:n :}
   i iu SYMBOLS-START
   down 2 9 HIGHLIGHT+
   down 1+ 19 26 HIGHLIGHT+
   down 2 + 33 40 HIGHLIGHT+
   SYMBOLS-END ;

\ DEF-ONE's use in DEF-TWO, at its first character and its last, and DEF-ONE's
\ declaring token, each answered with that token and both uses of DEF-ONE;
\ DEF-TWO's token, which no use names, with itself alone; the character after
\ the use with none; a negative character -32602.
: HIGHLIGHT-TURNS ( -- )
   INITIALIZE
   DEF-A-PATH TEXT-DEF-A 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-DEF-A DEF-A-PATH 1 s" verified" LISTED
   s" 3" DEF-A-PATH 1 19 HIGHLIGHT-ASK
   s" 4" DEF-A-PATH 1 25 HIGHLIGHT-ASK
   s" 5" DEF-A-PATH 0 3 HIGHLIGHT-ASK
   s" 6" DEF-A-PATH 1 2 HIGHLIGHT-ASK
   s" 7" DEF-A-PATH 1 26 HIGHLIGHT-ASK
   s" 8" DEF-A-PATH 1 -1 HIGHLIGHT-ASK
   SAY
   s" 3" 0 DEF-ONE-LIT
   s" 4" 0 DEF-ONE-LIT
   s" 5" 0 DEF-ONE-LIT
   s" 6" SYMBOLS-START 1 2 9 HIGHLIGHT+ SYMBOLS-END
   s" 7" SYMBOLS-START SYMBOLS-END
   HEAR s" 8" -32602 REFUSED ;

\ A use of >CVT-T and a qualified use of CVT-T>N, each answered with CVT-T's
\ token and the uses of its own converter, in either case and qualified, and
\ none of the other's: the check states both converters at that one token. The
\ token itself selects neither word, so it answers null.
: HIGHLIGHT-DEFTYPE-TURNS ( -- )
   INITIALIZE
   HOVER-CVT-PATH TEXT-HOVER-CVT 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-HOVER-CVT HOVER-CVT-PATH 1 s" verified" LISTED
   s" 3" HOVER-CVT-PATH 4 20 HIGHLIGHT-ASK
   s" 4" HOVER-CVT-PATH 6 30 HIGHLIGHT-ASK
   s" 5" HOVER-CVT-PATH 3 9 HIGHLIGHT-ASK
   SAY
   s" 3" SYMBOLS-START
   3 8 13 HIGHLIGHT+ 4 20 26 HIGHLIGHT+ 4 35 41 HIGHLIGHT+ 6 18 29 HIGHLIGHT+
   SYMBOLS-END
   s" 4" SYMBOLS-START
   3 8 13 HIGHLIGHT+ 4 27 34 HIGHLIGHT+ 4 42 49 HIGHLIGHT+ 6 30 42 HIGHLIGHT+
   SYMBOLS-END
   HEAR s" 5" NULL-RESULT ;

\ The uses of def-dep.f's global and, qualified, of its public word, each
\ answered with itself alone: the declarations are in the file on disk, not in
\ the document.
: HIGHLIGHT-DEP-TURNS ( -- )
   DEF-DEP-PATH TEXT-DEF-DEP WRITE-ALL
   INITIALIZE
   DEF-B-PATH TEXT-DEF-B 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-DEF-B DEF-B-PATH 1 s" verified" LISTED
   s" 3" DEF-B-PATH 1 19 HIGHLIGHT-ASK
   s" 4" DEF-B-PATH 1 27 HIGHLIGHT-ASK
   SAY
   s" 3" SYMBOLS-START 1 19 26 HIGHLIGHT+ SYMBOLS-END
   s" 4" SYMBOLS-START 1 27 37 HIGHLIGHT+ SYMBOLS-END ;

\ A position in a document the client never opened: -32602.
: HIGHLIGHT-NOT-OPEN-TURNS ( -- )
   INITIALIZE
   s" 3" DEF-A-PATH 1 19 HIGHLIGHT-ASK
   SAY
   HEAR CAPABILITIES
   HEAR s" 3" -32602 REFUSED ;

\ DEF-ONE's use asked about at its new place in the turn of the change that
\ moved it and DEF-ONE two lines down: the request checks the changed text
\ first, so its list comes before the answer, from the moved text. Asked again
\ with a change back read behind it, the request is -32801 and the change is
\ applied and checked after it; asked once more, the uses of the restored text
\ are answered without a check.
: HIGHLIGHT-AFTER-CHANGE-TURNS ( -- )
   INITIALIZE
   DEF-A-PATH TEXT-DEF-A 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-DEF-A DEF-A-PATH 1 s" verified" LISTED
   DEF-A-PATH TEXT-DEF-A-MOVED 2 CHANGES
   s" 3" DEF-A-PATH 3 19 HIGHLIGHT-ASK
   SAY
   TEXT-DEF-A-MOVED DEF-A-PATH 2 s" verified" LISTED
   s" 3" 2 DEF-ONE-LIT
   s" 4" DEF-A-PATH 3 19 HIGHLIGHT-ASK
   DEF-A-PATH TEXT-DEF-A 3 CHANGES
   SAY
   HEAR s" 4" -32801 REFUSED
   TEXT-DEF-A DEF-A-PATH 3 s" verified" LISTED
   s" 5" DEF-A-PATH 1 19 HIGHLIGHT-ASK
   SAY
   s" 5" 0 DEF-ONE-LIT ;

\ hu.f, refused for HU-BAD, whose body leaves the cell HU-ONE gives, and
\ hd.f, deferred at the top-level PN, which parses: neither verdict says the
\ check reported every use in the document.
: HU-PATH ( -- ptr u8 n )  s" hu.f" FIXTURE ;
: HD-PATH ( -- ptr u8 n )  s" hd.f" FIXTURE ;
: TEXT-HU ( -- ptr u8 n )  s\" : HU-ONE ( -- n ) 1 ;\n: HU-BAD ( -- ) HU-ONE ;\n" ;
: TEXT-HD ( -- ptr u8 n )
   s\" : HD-ONE ( -- n ) 1 ;\n: HD-TWO ( -- n ) HD-ONE ;\n: PN ( -- ) parse-name 2drop ;\nPN anything\n" ;

\ The use of HU-ONE in the refused document and of HD-ONE in the deferred
\ one, each answered null, while go to definition at the same byte answers
\ its declaration from the same check.
: HIGHLIGHT-UNPROVED-TURNS ( -- )
   INITIALIZE
   HU-PATH TEXT-HU 1 OPENS
   HD-PATH TEXT-HD 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-HU HU-PATH CHECKS
   HU-PATH s" refused" COMPLETED
   HU-PATH 1 EXPECT 1 16 1 22 1 s" E-MISMATCH" DIAG+ PUBLISHES
   TEXT-HD HD-PATH CHECKS
   HD-PATH s" deferred" COMPLETED
   HD-PATH 1 EXPECT 3 0 3 2 3 s" W-CHECK-DEFERRED" DIAG+ PUBLISHES
   s" 3" HU-PATH 1 16 HIGHLIGHT-ASK
   s" 4" HU-PATH 1 16 DEFINITION-ASK
   s" 5" HD-PATH 1 18 HIGHLIGHT-ASK
   s" 6" HD-PATH 1 18 DEFINITION-ASK
   SAY
   HEAR s" 3" NULL-RESULT
   s" 4" HU-PATH URI-OF 0 2 8 LOCATED
   HEAR s" 5" NULL-RESULT
   s" 6" HD-PATH URI-OF 0 2 8 LOCATED ;

\ DEF-ONE's use asked about in the turn of a change whose check did not
\ complete: no check of the document's text was verified, so the answer is
\ null, not the uses of the text before it.
: HIGHLIGHT-INCOMPLETE-TURNS ( -- )
   INITIALIZE
   DEF-A-PATH TEXT-DEF-C 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-DEF-C DEF-A-PATH 1 s" verified" LISTED
   DEF-A-PATH TEXT-DEF-C-SWAPPED 2 CHANGES
   s" 3" DEF-A-PATH 2 19 HIGHLIGHT-ASK
   SAY
   TEXT-DEF-C-SWAPPED DEF-A-PATH CHECKS
   DEF-A-PATH s" not checked: exit 76" SAID
   HEAR s" 3" NULL-RESULT
   LOGGED ;

\ hl-common.f, on disk only, defines HL-K in the package and section that
\ include it. hl-twice.f includes it into HLP's public section and HLQ's,
\ hl-vis.f into HLV's public section and then its private one, hl-undef.f
\ into the globals, then again after `undefine HL-K`, and each uses both
\ words: the check states two words at HL-K's one token, of two packages, of
\ one package under two visibilities, then of one wordlist one after the
\ other.
: HL-COMMON-PATH ( -- ptr u8 n )  s" hl-common.f" FIXTURE ;
: HL-TWICE-PATH ( -- ptr u8 n )  s" hl-twice.f" FIXTURE ;
: HL-VIS-PATH ( -- ptr u8 n )  s" hl-vis.f" FIXTURE ;
: HL-UNDEF-PATH ( -- ptr u8 n )  s" hl-undef.f" FIXTURE ;
: TEXT-HL-COMMON ( -- ptr u8 n )  s\" : HL-K ( -- n ) 1 ;\n" ;
: TEXT-HL-TWICE ( -- ptr u8 n )
   s\" package HLP\npublic\ninclude hl-common.f\n;package\npackage HLQ\npublic\ninclude hl-common.f\n;package\n: HL-BOTH ( -- n ) HLP:HL-K HLQ:HL-K + ;\n" ;
: TEXT-HL-VIS ( -- ptr u8 n )
   s\" package HLV\npublic\ninclude hl-common.f\nprivate\ninclude hl-common.f\n: HL-IN ( -- n ) HL-K ;\n;package\n: HL-OUT ( -- n ) HLV:HL-K ;\n" ;
: TEXT-HL-UNDEF ( -- ptr u8 n )
   s\" include hl-common.f\n: HL-U1 ( -- n ) HL-K ;\nundefine HL-K\ninclude hl-common.f\n: HL-U2 ( -- n ) HL-K ;\n" ;

\ HLP:HL-K and HLQ:HL-K, the private HL-K and the public HLV:HL-K, then the
\ first global HL-K and the second, each answered null: the token their uses
\ target declares two words, and no use is known to be of the one it names
\ rather than of the other.
: HIGHLIGHT-INCLUDED-TURNS ( -- )
   HL-COMMON-PATH TEXT-HL-COMMON WRITE-ALL
   INITIALIZE
   HL-TWICE-PATH TEXT-HL-TWICE 1 OPENS
   HL-VIS-PATH TEXT-HL-VIS 1 OPENS
   HL-UNDEF-PATH TEXT-HL-UNDEF 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-HL-TWICE HL-TWICE-PATH 1 s" verified" LISTED
   TEXT-HL-VIS HL-VIS-PATH 1 s" verified" LISTED
   TEXT-HL-UNDEF HL-UNDEF-PATH 1 s" verified" LISTED
   s" 3" HL-TWICE-PATH 8 19 HIGHLIGHT-ASK
   s" 4" HL-TWICE-PATH 8 28 HIGHLIGHT-ASK
   s" 5" HL-VIS-PATH 5 17 HIGHLIGHT-ASK
   s" 6" HL-VIS-PATH 7 18 HIGHLIGHT-ASK
   s" 7" HL-UNDEF-PATH 1 17 HIGHLIGHT-ASK
   s" 8" HL-UNDEF-PATH 4 17 HIGHLIGHT-ASK
   SAY
   HEAR s" 3" NULL-RESULT
   HEAR s" 4" NULL-RESULT
   HEAR s" 5" NULL-RESULT
   HEAR s" 6" NULL-RESULT
   HEAR s" 7" NULL-RESULT
   HEAR s" 8" NULL-RESULT ;

\ hl-case.f, on disk, as the second of one check's two readings of it read it:
\ hl-reread.f includes it into P's public section, then into Q's, and uses
\ P:K and Q:k, and between the readings the file changed from
\ `: K ( -- n ) 1 ;` to this text. The lines are the ones
\ tools/check-verify-child.f stated for a verified check of that race: the two
\ files, K of p and k of q both at hl-case.f's bytes 2 to 3, and the two uses,
\ here at hl-reread.f's bytes 107 to 110 and 111 to 114.
: HL-CASE-PATH ( -- ptr u8 n )  s" hl-case.f" FIXTURE ;
: HL-REREAD-PATH ( -- ptr u8 n )  s" hl-reread.f" FIXTURE ;
: TEXT-HL-CASE ( -- ptr u8 n )  s\" : k ( -- n ) 2 ;\n" ;
: TEXT-HL-REREAD ( -- ptr u8 n )
   s\" package P\npublic\ninclude hl-case.f\n;package\npackage Q\npublic\ninclude hl-case.f\n;package\n: HL-BOTH ( -- n ) P:K Q:k + ;\n" ;

\ The definition of word W of package P at hl-case.f's bytes 2 to 3, a line
\ in PAR.
: HL-CASE-DEF+ ( ptr u8 n ptr u8 n -- )
   {: w:ptr wu:n p:ptr pu:n :}
   s\" {\"kind\":\":\",\"class\":\"word\",\"word\":\"" PAR+ w wu PAR+
   s\" \",\"package\":\"" PAR+ p pu PAR+
   s\" \",\"visibility\":\"public\",\"effect\":\"-- n\",\"file\":" PAR+
   HL-CASE-PATH TEXT+ s\" ,\"byte_start\":2,\"byte_end\":3}\n" PAR+ ;

\ A use from byte FROM to byte TO of hl-reread.f of the token at hl-case.f's
\ bytes 2 to 3, a line in PAR.
: HL-CASE-USE+ ( n n -- )
   {: from:n to:n :}
   s\" {\"byte_start\":" PAR+ from INT$ PAR+
   s\" ,\"byte_end\":" PAR+ to INT$ PAR+
   s\" ,\"file\":" PAR+ HL-CASE-PATH TEXT+
   s\" ,\"target_start\":2,\"target_end\":3}\n" PAR+ ;

\ The highlight at this line and character of the document in this slot, its
\ byte found as the server finds a request's, must be null; it fails under
\ this label.
: HL-NULL-AT ( n n n ptr u8 n -- )
   {: slot:n line:n ch:n l:ptr lu:n :}
   slot LSP-DOCS:DOC-TEXT$ LSP-TEXT:TEXT!
   line ch LSP-TEXT:OFFSET-AT {: at:n :}
   l lu T-LABEL
   HL-W MSG-B JSON-WRITE:OPEN-BUF slot at LSP-HIGHLIGHT:ANSWER
   JSON-WRITE:$ s" null" T$=
   HL-W JSON-WRITE:CLOSE ;

\ The check's lines kept for hl-reread.f, open in this slot, as the server
\ keeps a completed check's, then the highlight asked at P:K and at Q:k.
: HL-REREAD-ASK ( n -- )
   {: slot:n :}
   PAR-B CLEAR
   s\" {\"file\":" PAR+ HL-REREAD-PATH TEXT+
   s\" }\n{\"file\":" PAR+ HL-CASE-PATH TEXT+ s\" }\n" PAR+
   PAR$ nip {: files:n :}
   s" K" s" p" HL-CASE-DEF+
   s" k" s" q" HL-CASE-DEF+
   PAR$ nip {: defs:n :}
   107 110 HL-CASE-USE+
   111 114 HL-CASE-USE+
   PAR$ {: a:ptr u:n :}
   a files  a files + defs files -  a defs + u defs -  slot true
   LSP-DEFS:DEFS-KEEP
   slot 8 20 s" highlight-case-reread: P:K" HL-NULL-AT
   slot 8 24 s" highlight-case-reread: Q:k" HL-NULL-AT ;

: HIGHLIGHT-CASE-REREAD ( -- )
   LSP-DEFS:DEFS-PREPARE
   HL-CASE-PATH TEXT-HL-CASE WRITE-ALL
   HL-REREAD-PATH URI-OF 1 HL-REREAD-PATH TEXT-HL-REREAD LSP-DOCS:DOC-OPEN
   HL-REREAD-PATH URI-OF LSP-DOCS:DOC-FIND MATCH option
      some OF HL-REREAD-ASK ENDOF
      none OF s" highlight-case-reread: open" T-LABEL false TTRUE ENDOF
   ;MATCH ;

\ ---- completion --------------------------------------------------------------

: CMP-A-PATH ( -- ptr u8 n )  s" cmpl-a.f" FIXTURE ;
: CMP-DEP-PATH ( -- ptr u8 n )  s" cmpl-dep.f" FIXTURE ;

\ cmpl-dep.f, on disk only: the global CMPL-DEP.
: TEXT-CMP-DEP ( -- ptr u8 n )  s\" : CMPL-DEP ( n -- n ) 4 + ;\n" ;

\ In package CMPA, CMPL-ONE, then a body using it, cmpl-dep.f's global CMPL-DEP
\ and the engine's swap; then, global, DEFTYPE CMPL-T, whose converters
\ >CMPL-T and CMPL-T>N the check states at CMPL-T's token, and a use of each;
\ then DEFTYPE CMPQ-T, public in package CMPQ, and a qualified use of each of
\ its converters outside CMPQ.
: TEXT-CMP-A ( -- ptr u8 n )
   s\" require cmpl-dep.f\npackage CMPA\n: CMPL-ONE ( -- n ) 1 ;\n: CMPL-TWO ( -- n ) CMPL-ONE CMPL-DEP 1 2 swap 2drop ;\n;package\nrequire lib/type/deftype.f\nDEFTYPE CMPL-T\n: CMPL-CVT ( -- n ) 5 >CMPL-T CMPL-T>N ;\npackage CMPQ\npublic\nDEFTYPE CMPQ-T\n;package\n: CMPQ-USE ( -- n ) 5 CMPQ:>CMPQ-T CMPQ:CMPQ-T>N ;\n" ;

\ TEXT-CMP-A with CMPL-ONE renamed CMPL-NEW, the body's bytes unmoved.
: TEXT-CMP-A-NEW ( -- ptr u8 n )
   s\" require cmpl-dep.f\npackage CMPA\n: CMPL-NEW ( -- n ) 5 ;\n: CMPL-TWO ( -- n ) CMPL-NEW CMPL-DEP 1 2 swap 2drop ;\n;package\nrequire lib/type/deftype.f\nDEFTYPE CMPL-T\n: CMPL-CVT ( -- n ) 5 >CMPL-T CMPL-T>N ;\npackage CMPQ\npublic\nDEFTYPE CMPQ-T\n;package\n: CMPQ-USE ( -- n ) 5 CMPQ:>CMPQ-T CMPQ:CMPQ-T>N ;\n" ;

\ textDocument/completion, by its id's JSON text, at character C of line L of
\ the document opened from this path.
: COMPLETION-ASK ( ptr u8 n ptr u8 n n n -- )
   {: i:ptr iu:n p:ptr pu:n l:n c:n :}
   s" textDocument/completion" i iu p pu l c AT-ASK ;

\ The answer to the request with this id's JSON text, begun in MSG, up to its
\ items' bracket.
: ITEMS-START ( ptr u8 n -- )
   {: i:ptr iu:n :}
   MSG-B CLEAR
   s\" {\"jsonrpc\":\"2.0\",\"id\":" MSG+ i iu MSG+
   s\" ,\"result\":{\"isIncomplete\":false,\"items\":[" MSG+ ;

\ The item the answer lists next: its label, then its detail, none when empty,
\ each as JSON text.
: ITEM+ ( ptr u8 n ptr u8 n -- )
   {: w:ptr wu:n d:ptr du:n :}
   MSG$ s" [" ENDS-WITH? 0= if s" ," MSG+ then
   s\" {\"label\":\"" MSG+ w wu MSG+ s\" \"" MSG+
   du 0 > if s\" ,\"detail\":\"" MSG+ d du MSG+ s\" \"" MSG+ then
   s" }" MSG+ ;

\ The next frame is the answer begun.
: ITEMS-END ( -- )
   s" ]}}" MSG+
   HEAR
   MSG$ HEARD ;

\ At CMPL in CMPL-TWO's body, the document's CMPL-ONE and the dependency's
\ CMPL-DEP, each with its declared effect and CMPL-ONE with its package; at
\ swa the engine's swap, without either; at >CMPL and at CMPL-T>, each
\ converter with its own effect, which the token they share does not tell;
\ and at CMPQ:>CMPQ and at CMPQ:CMPQ-T>, each of CMPQ's converters, its
\ spelling qualified, with its own effect and package. Each request checks the
\ document with its cursor, as its turn, so the document's list comes before
\ each answer.
: CMP-TURNS ( -- )
   CMP-DEP-PATH TEXT-CMP-DEP WRITE-ALL
   INITIALIZE
   CMP-A-PATH TEXT-CMP-A 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-CMP-A CMP-A-PATH 1 s" verified" LISTED
   s" 3" CMP-A-PATH 3 24 COMPLETION-ASK
   s" 4" CMP-A-PATH 3 45 COMPLETION-ASK
   s" 5" CMP-A-PATH 7 27 COMPLETION-ASK
   s" 6" CMP-A-PATH 7 37 COMPLETION-ASK
   s" 7" CMP-A-PATH 12 32 COMPLETION-ASK
   s" 8" CMP-A-PATH 12 47 COMPLETION-ASK
   SAY
   TEXT-CMP-A CMP-A-PATH 1 s" verified" LISTED
   s" 3" ITEMS-START
   s" CMPL-DEP" s" ( n -- n )" ITEM+
   s" CMPL-ONE" s" ( -- n ) \\ package cmpa" ITEM+
   ITEMS-END
   TEXT-CMP-A CMP-A-PATH 1 s" verified" LISTED
   s" 4" ITEMS-START
   s" swap" s" " ITEM+
   ITEMS-END
   TEXT-CMP-A CMP-A-PATH 1 s" verified" LISTED
   s" 5" ITEMS-START
   s" >CMPL-T" s" ( n -- cmpl-t )" ITEM+
   ITEMS-END
   TEXT-CMP-A CMP-A-PATH 1 s" verified" LISTED
   s" 6" ITEMS-START
   s" CMPL-T>N" s" ( cmpl-t -- n )" ITEM+
   ITEMS-END
   TEXT-CMP-A CMP-A-PATH 1 s" verified" LISTED
   s" 7" ITEMS-START
   s" CMPQ:>CMPQ-T" s" ( n -- cmpq-t ) \\ package cmpq" ITEM+
   ITEMS-END
   TEXT-CMP-A CMP-A-PATH 1 s" verified" LISTED
   s" 8" ITEMS-START
   s" CMPQ:CMPQ-T>N" s" ( cmpq-t -- n ) \\ package cmpq" ITEM+
   ITEMS-END ;

\ A position in a document the client never opened: -32602.
: CMP-NOT-OPEN-TURNS ( -- )
   INITIALIZE
   s" 3" CMP-A-PATH 2 24 COMPLETION-ASK
   SAY
   HEAR CAPABILITIES
   HEAR s" 3" -32602 REFUSED ;

\ CMPL asked about in the turn of the change that renamed CMPL-ONE: the
\ request checks the changed text, so its list comes before the answer,
\ which offers CMPL-NEW and no CMPL-ONE.
: CMP-AFTER-CHANGE-TURNS ( -- )
   CMP-DEP-PATH TEXT-CMP-DEP WRITE-ALL
   INITIALIZE
   CMP-A-PATH TEXT-CMP-A 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-CMP-A CMP-A-PATH 1 s" verified" LISTED
   CMP-A-PATH TEXT-CMP-A-NEW 2 CHANGES
   s" 3" CMP-A-PATH 3 24 COMPLETION-ASK
   SAY
   TEXT-CMP-A-NEW CMP-A-PATH 2 s" verified" LISTED
   s" 3" ITEMS-START
   s" CMPL-DEP" s" ( n -- n )" ITEM+
   s" CMPL-NEW" s" ( -- n ) \\ package cmpa" ITEM+
   ITEMS-END ;

\ In CMPQ-USE's body, at CMPQ, the package's name, a bare prefix, and at
\ CMPQ:, its name and an edge colon, each answer holds every qualified
\ spelling that the qualified prefixes CMPQ:> and CMPQ:C there offer, with
\ the same detail: the client's filter of the first answer as the token grows
\ to either keeps what a later request would offer.
: CMP-QUALIFIED-TURNS ( -- )
   CMP-DEP-PATH TEXT-CMP-DEP WRITE-ALL
   INITIALIZE
   CMP-A-PATH TEXT-CMP-A 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-CMP-A CMP-A-PATH 1 s" verified" LISTED
   s" 3" CMP-A-PATH 12 26 COMPLETION-ASK
   s" 4" CMP-A-PATH 12 27 COMPLETION-ASK
   s" 5" CMP-A-PATH 12 28 COMPLETION-ASK
   s" 6" CMP-A-PATH 12 41 COMPLETION-ASK
   SAY
   TEXT-CMP-A CMP-A-PATH 1 s" verified" LISTED
   s" 3" ITEMS-START
   s" CMPQ:>CMPQ-T" s" ( n -- cmpq-t ) \\ package cmpq" ITEM+
   s" CMPQ:CMPQ-T>N" s" ( cmpq-t -- n ) \\ package cmpq" ITEM+
   ITEMS-END
   TEXT-CMP-A CMP-A-PATH 1 s" verified" LISTED
   s" 4" ITEMS-START
   s" CMPQ:>CMPQ-T" s" ( n -- cmpq-t ) \\ package cmpq" ITEM+
   s" CMPQ:CMPQ-T>N" s" ( cmpq-t -- n ) \\ package cmpq" ITEM+
   ITEMS-END
   TEXT-CMP-A CMP-A-PATH 1 s" verified" LISTED
   s" 5" ITEMS-START
   s" CMPQ:>CMPQ-T" s" ( n -- cmpq-t ) \\ package cmpq" ITEM+
   ITEMS-END
   TEXT-CMP-A CMP-A-PATH 1 s" verified" LISTED
   s" 6" ITEMS-START
   s" CMPQ:CMPQ-T>N" s" ( cmpq-t -- n ) \\ package cmpq" ITEM+
   ITEMS-END ;

\ ---- document links ----------------------------------------------------------

: LK-PATH ( -- ptr u8 n )  s" lk.f" FIXTURE ;
: LK-DEP-PATH ( -- ptr u8 n )  s" lk-dep.f" FIXTURE ;
: LK-INC-PATH ( -- ptr u8 n )  s" lk-inc.f" FIXTURE ;
: LK-REAL-DEP ( -- ptr u8 n )  s" lk-real/lk-dep.f" FIXTURE ;
: LK-LINK-DEP ( -- ptr u8 n )  s" lk-link/lk-dep.f" FIXTURE ;
: LK-LINK-DOC ( -- ptr u8 n )  s" lk-link/lk.f" FIXTURE ;

\ The path the load lines name the file at this path by.
: LK-CANON ( ptr u8 n -- ptr u8 n )  SOURCE-ROOT:CANONICAL drop ;

: TEXT-LK-LOADED ( -- ptr u8 n )  s\" \\ loaded\n" ;
: TEXT-LK-ONE ( -- ptr u8 n )  s\" require lk-dep.f\n" ;
: TEXT-LK-MOVED ( -- ptr u8 n )  s\" \\ moved\nrequire lk-dep.f\n" ;

\ After a comment holding a character UTF-16 counts as one unit and UTF-8 as
\ two bytes, lk-dep.f required; lib/string.f required, which the engine
\ provides; lk-dep.f required again by a string; lk-inc.f included; and
\ lk-gone.f, which is not there, required, where the check stops.
: TEXT-LK ( -- ptr u8 n )
   s\" ( é ) require lk-dep.f\nrequire lib/string.f\ns\" lk-dep.f\" required\ninclude lk-inc.f\nrequire lk-gone.f\n" ;

\ TEXT-LK-ONE, then one `using` more than the checker holds open, at which the
\ verifier dies, its load reported, and exits without a verdict.
: TEXT-LK-OVER ( -- ptr u8 n )
   TXT-B CLEAR
   TEXT-LK-ONE N>BLEN TXT-B APPEND-SPAN
   OVER-USINGS+
   TXT$ ;

\ textDocument/documentLink, by its id's JSON text, of the document opened
\ from this path.
: LINKS-ASK ( ptr u8 n ptr u8 n -- )
   {: i:ptr iu:n p:ptr pu:n :}
   MSG-B CLEAR
   s\" {\"jsonrpc\":\"2.0\",\"id\":" MSG+ i iu MSG+
   s\" ,\"method\":\"textDocument/documentLink\",\"params\":{\"textDocument\":{\"uri\":\"" MSG+
   p pu URI-OF MSG+ s\" \"}}}" MSG+
   MSG$ FRAMED ;

\ The answer to the request with this id's JSON text, begun in MSG.
: LINKS-START ( ptr u8 n -- )
   {: i:ptr iu:n :}
   MSG-B CLEAR
   s\" {\"jsonrpc\":\"2.0\",\"id\":" MSG+ i iu MSG+ s\" ,\"result\":[" MSG+ ;

\ The link the answer holds next, begun: over characters C1 to C2 of line L,
\ to the file URI of the path at P.
: LINK+ ( n n n ptr u8 n -- )
   {: l:n c1:n c2:n p:ptr pu:n :}
   MSG$ s" [" ENDS-WITH? 0= if s" ," MSG+ then
   s\" {\"range\":{\"start\":{\"line\":" MSG+ l INT$ MSG+
   s\" ,\"character\":" MSG+ c1 INT$ MSG+
   s\" },\"end\":{\"line\":" MSG+ l INT$ MSG+
   s\" ,\"character\":" MSG+ c2 INT$ MSG+
   s\" }},\"target\":\"" MSG+ p pu URI-OF MSG+ s\" \"" MSG+ ;

\ The link of a loader that read the file at P: no tooltip.
: READ-LINK+ ( n n n ptr u8 n -- )  LINK+ s" }" MSG+ ;

\ The link of a loader that held the file at P: its tooltip names the file as
\ a hover does.
: HELD-LINK+ ( n n n ptr u8 n -- )
   {: l:n c1:n c2:n p:ptr pu:n :}
   l c1 c2 p pu LINK+
   TXT-B CLEAR
   s" held " HV+ p pu WHERE+
   s" : this loader did not read it; the engine provides it or it was already recorded." HV+
   PAR-B CLEAR TXT$ TEXT+
   s\" ,\"tooltip\":" MSG+ PAR$ MSG+ s" }" MSG+ ;

\ The answer ended: the next frame is it.
: LINKS-END ( -- )
   s" ]}" MSG+
   HEAR
   MSG$ HEARD ;

\ Each loader of TEXT-LK the check reached and whose file it selected is a
\ link over its operand, in the order of the text: lk-dep.f's first require
\ and lk-inc.f's include read their files, linked with no tooltip; the
\ engine's lib/string.f and lk-dep.f's second require are held, each with
\ its tooltip, lib/string.f named relative to the working directory, which
\ the server shares, and lk-dep.f, outside it, by its path. lk-gone.f, which
\ the check could not read, has its E-MISSING-SOURCE and no link.
: LINK-TURNS ( -- )
   LK-DEP-PATH TEXT-LK-LOADED WRITE-ALL
   LK-INC-PATH TEXT-LK-LOADED WRITE-ALL
   INITIALIZE
   LK-PATH TEXT-LK 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-LK LK-PATH CHECKS
   LK-PATH s" refused" COMPLETED
   LK-PATH 1 EXPECT 4 0 4 7 1 s" E-MISSING-SOURCE" DIAG+ PUBLISHES
   s" 3" LK-PATH LINKS-ASK
   SAY
   s" 3" LINKS-START
   0 14 22 LK-DEP-PATH LK-CANON READ-LINK+
   1 8 20 s" lib/string.f" IN-TREE LK-CANON HELD-LINK+
   2 3 11 LK-DEP-PATH LK-CANON HELD-LINK+
   3 8 16 LK-INC-PATH LK-CANON READ-LINK+
   LINKS-END ;

\ lk.f, opened through lk-link, a symlink to lk-real, requires lk-dep.f, on
\ disk there: its link is at lk-dep.f's canonical file URI, under lk-real,
\ until lk-dep.f is opened through lk-link, and then at the URI the client
\ opened it by. That open leaves lk.f's check as it was, and lk-dep.f is
\ checked after the links are answered.
: LINK-OPEN-TURNS ( -- )
   s" lk-real" FIXTURE MAKE-DIR
   s" lk-real" s" lk-link" FIXTURE MAKE-SYMLINK
   LK-REAL-DEP TEXT-LK-LOADED WRITE-ALL
   INITIALIZE
   LK-LINK-DOC TEXT-LK-ONE 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-LK-ONE LK-LINK-DOC 1 s" verified" LISTED
   s" 3" LK-LINK-DOC LINKS-ASK
   SAY
   s" 3" LINKS-START 0 8 16 LK-REAL-DEP LK-CANON READ-LINK+ LINKS-END
   LK-LINK-DEP TEXT-LK-LOADED 1 OPENS
   s" 4" LK-LINK-DOC LINKS-ASK
   SAY
   s" 4" LINKS-START 0 8 16 LK-LINK-DEP READ-LINK+ LINKS-END
   TEXT-LK-LOADED LK-LINK-DEP 1 s" verified" LISTED ;

\ A document the client never opened, and params naming no document: -32602.
: LINK-NOT-OPEN-TURNS ( -- )
   INITIALIZE
   s" 3" LK-PATH LINKS-ASK
   s" 4" s" textDocument/documentLink" ASK
   SAY
   HEAR CAPABILITIES
   HEAR s" 3" -32602 REFUSED
   HEAR s" 4" -32602 REFUSED ;

\ Links asked with the document's opening, before the server checked it: the
\ request checks it first, so its list comes before the answer.
: LINK-BEFORE-CHECK-TURNS ( -- )
   LK-DEP-PATH TEXT-LK-LOADED WRITE-ALL
   INITIALIZE
   LK-PATH TEXT-LK-ONE 1 OPENS
   s" 3" LK-PATH LINKS-ASK
   SAY
   HEAR CAPABILITIES
   TEXT-LK-ONE LK-PATH 1 s" verified" LISTED
   s" 3" LINKS-START 0 8 16 LK-DEP-PATH LK-CANON READ-LINK+ LINKS-END ;

\ Links asked in the turn of a change that moved the loader a line down: the
\ request checks the changed text first, so its list comes before the answer,
\ the link at the loader's new place. Asked with a change of the document read
\ behind them, they are -32801, and the change is applied and checked after.
: LINK-AFTER-CHANGE-TURNS ( -- )
   LK-DEP-PATH TEXT-LK-LOADED WRITE-ALL
   INITIALIZE
   LK-PATH TEXT-LK-ONE 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-LK-ONE LK-PATH 1 s" verified" LISTED
   LK-PATH TEXT-LK-MOVED 2 CHANGES
   s" 3" LK-PATH LINKS-ASK
   SAY
   TEXT-LK-MOVED LK-PATH 2 s" verified" LISTED
   s" 3" LINKS-START 1 8 16 LK-DEP-PATH LK-CANON READ-LINK+ LINKS-END
   s" 4" LK-PATH LINKS-ASK
   LK-PATH TEXT-LK-ONE 3 CHANGES
   SAY
   HEAR s" 4" -32801 REFUSED
   TEXT-LK-ONE LK-PATH 3 s" verified" LISTED ;

\ Links asked in the turn of a change whose check did not complete: neither
\ the load lines of the text before it nor the one that check reported before
\ it stopped are of a text a check completed on, so the answer is an empty
\ list.
: LINK-INCOMPLETE-TURNS ( -- )
   LK-DEP-PATH TEXT-LK-LOADED WRITE-ALL
   INITIALIZE
   LK-PATH TEXT-LK-ONE 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-LK-ONE LK-PATH 1 s" verified" LISTED
   LK-PATH TEXT-LK-OVER 2 CHANGES
   s" 3" LK-PATH LINKS-ASK
   SAY
   TEXT-LK-OVER LK-PATH CHECKS
   LK-PATH s" not checked: exit 76" SAID
   s" 3" LINKS-START LINKS-END
   LOGGED ;

\ ---- cancellation ------------------------------------------------------------

\ A cancel of the request with this id's JSON text.
: CANCELS ( ptr u8 n -- )
   {: i:ptr iu:n :}
   PAR-B CLEAR
   s\" {\"id\":" PAR+ i iu PAR+ s" }" PAR+
   s" $/cancelRequest" PAR$ TELL ;

\ Two completions and a cancel of the first, written together: the first is
\ answered -32800 and runs no check, so the document's list is published once,
\ by the second's check, before its answer.
: CANCEL-CMP-TURNS ( -- )
   CMP-DEP-PATH TEXT-CMP-DEP WRITE-ALL
   INITIALIZE
   CMP-A-PATH TEXT-CMP-A 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-CMP-A CMP-A-PATH 1 s" verified" LISTED
   s" 3" CMP-A-PATH 3 24 COMPLETION-ASK
   s" 4" CMP-A-PATH 3 45 COMPLETION-ASK
   s" 3" CANCELS
   SAY
   HEAR s" 3" -32800 REFUSED
   TEXT-CMP-A CMP-A-PATH 1 s" verified" LISTED
   s" 4" ITEMS-START
   s" swap" s" " ITEM+
   ITEMS-END ;

\ A hover whose id is a string and a cancel naming that string spelt another
\ way, written together: -32800, the id echoed as it came.
: CANCEL-HOVER-TURNS ( -- )
   MARKDOWN-CLIENT
   HOVER-A-PATH TEXT-HOVER-A 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-HOVER-A HOVER-A-PATH 1 s" verified" LISTED
   s\" \"h\\/3\"" HOVER-A-PATH 4 18 HOVER-ASK
   s\" \"h/3\"" CANCELS
   SAY
   HEAR s\" \"h\\/3\"" -32800 REFUSED ;

\ A completion and then a change of its document, written together: the
\ completion asked about the text the change replaces, so it is answered
\ -32801 and runs no check; the change is applied after it and its text is
\ checked.
: MODIFIED-TURNS ( -- )
   CMP-DEP-PATH TEXT-CMP-DEP WRITE-ALL
   INITIALIZE
   CMP-A-PATH TEXT-CMP-A 1 OPENS
   SAY
   HEAR CAPABILITIES
   TEXT-CMP-A CMP-A-PATH 1 s" verified" LISTED
   s" 3" CMP-A-PATH 3 24 COMPLETION-ASK
   CMP-A-PATH TEXT-CMP-A-NEW 2 CHANGES
   SAY
   HEAR s" 3" -32801 REFUSED
   TEXT-CMP-A-NEW CMP-A-PATH 2 s" verified" LISTED ;

\ Cancels read behind request 4 that name no request held: request 3's id,
\ answered the turn before, an id never sent, the string "4", and the id of an
\ invalid message, which is no request. None is answered, request 4 and the
\ invalid message are answered as they would be, and a cancel of request 5
\ behind it still counts: the server goes on.
: STRAY-TURNS ( -- )
   INITIALIZE
   s" 3" s" habu/unknown" ASK
   SAY
   HEAR CAPABILITIES
   HEAR s" 3" -32601 REFUSED
   s" 4" s" habu/unknown" ASK
   s" 3" CANCELS
   s" 9" CANCELS
   s\" \"4\"" CANCELS
   s\" {\"jsonrpc\":\"1.0\",\"id\":6,\"method\":\"habu/unknown\"}" FRAMED
   s" 6" CANCELS
   s" 5" s" habu/unknown" ASK
   s" 5" CANCELS
   SAY
   HEAR s" 4" -32601 REFUSED
   HEAR s" 6" -32600 REFUSED
   HEAR s" 5" -32800 REFUSED ;

\ A request, shutdown, a cancel of the request, a request after shutdown and
\ a cancel of it, and exit, read together: the held request is answered
\ before shutdown, -32800, its cancel counting though read past shutdown;
\ shutdown is answered null, the request after it -32600, since a server shut
\ down takes no cancel, and exit after shutdown exits 0.
: TEST-CANCEL-SHUTDOWN ( -- )
   s" cancel-shutdown" CONVERSATION
   INITIALIZE
   s" 3" s" habu/unknown" ASK
   SHUTDOWN
   s" 3" CANCELS
   s" 4" s" habu/unknown" ASK
   s" 4" CANCELS
   EXIT-NOTE
   0 CONVERSE
   CAPABILITIES
   s" 3" -32800 REFUSED
   s" 2" NULL-RESULT
   s" 4" -32600 REFUSED
   ENDS ;

: TEST-CANCELS ( -- )
   TEST-CANCEL-SHUTDOWN
   s" cancel-completion" [: CANCEL-CMP-TURNS ;] TALK
   s" cancel-hover" [: CANCEL-HOVER-TURNS ;] TALK
   s" content-modified" [: MODIFIED-TURNS ;] TALK
   s" cancel-stray" [: STRAY-TURNS ;] TALK ;

: TEST-COMPLETIONS ( -- )
   s" completion" [: CMP-TURNS ;] TALK
   s" completion-not-open" [: CMP-NOT-OPEN-TURNS ;] TALK
   s" completion-after-change" [: CMP-AFTER-CHANGE-TURNS ;] TALK
   s" completion-qualified" [: CMP-QUALIFIED-TURNS ;] TALK ;

: TEST-LINKS ( -- )
   s" document-link" [: LINK-TURNS ;] TALK
   s" document-link-open" [: LINK-OPEN-TURNS ;] TALK
   s" document-link-not-open" [: LINK-NOT-OPEN-TURNS ;] TALK
   s" document-link-before-check" [: LINK-BEFORE-CHECK-TURNS ;] TALK
   s" document-link-after-change" [: LINK-AFTER-CHANGE-TURNS ;] TALK
   s" document-link-incomplete" [: LINK-INCOMPLETE-TURNS ;] TALK ;

: TEST-HOVERS ( -- )
   s" hover" [: HOVER-TURNS ;] TALK
   s" hover-dependency" [: HOVER-DEP-TURNS ;] TALK
   s" hover-dependency-open" [: HOVER-OPEN-DEP-TURNS ;] TALK
   s" hover-definition" [: HOVER-DEF-TURNS ;] TALK
   s" hover-deftype" [: HOVER-DEFTYPE-TURNS ;] TALK
   s" hover-none" [: HOVER-NONE-TURNS ;] TALK
   s" hover-plaintext" [: HOVER-PLAIN-TURNS ;] TALK
   s" hover-not-open" [: HOVER-NOT-OPEN-TURNS ;] TALK
   s" hover-after-change" [: HOVER-AFTER-CHANGE-TURNS ;] TALK ;

: TEST-HIGHLIGHTS ( -- )
   s" highlight" [: HIGHLIGHT-TURNS ;] TALK
   s" highlight-deftype" [: HIGHLIGHT-DEFTYPE-TURNS ;] TALK
   s" highlight-dependency" [: HIGHLIGHT-DEP-TURNS ;] TALK
   s" highlight-not-open" [: HIGHLIGHT-NOT-OPEN-TURNS ;] TALK
   s" highlight-after-change" [: HIGHLIGHT-AFTER-CHANGE-TURNS ;] TALK
   s" highlight-unproved" [: HIGHLIGHT-UNPROVED-TURNS ;] TALK
   s" highlight-incomplete" [: HIGHLIGHT-INCOMPLETE-TURNS ;] TALK
   s" highlight-included" [: HIGHLIGHT-INCLUDED-TURNS ;] TALK
   HIGHLIGHT-CASE-REREAD ;

: TEST-DEFINITIONS ( -- )
   s" definition" [: DEF-TURNS ;] TALK
   s" definition-dependency" [: DEF-DEP-TURNS ;] TALK
   s" definition-dependency-open" [: DEF-OPEN-DEP-TURNS ;] TALK
   s" definition-not-open" [: DEF-NOT-OPEN-TURNS ;] TALK
   s" definition-before-check" [: DEF-BEFORE-CHECK-TURNS ;] TALK
   s" definition-after-change" [: DEF-AFTER-CHANGE-TURNS ;] TALK
   s" definition-incomplete" [: DEF-INCOMPLETE-TURNS ;] TALK ;

: TEST-SYMBOLS ( -- )
   s" workspace-symbol" [: SYMBOL-TURNS ;] TALK
   s" workspace-symbol-empty" [: EMPTY-TURNS ;] TALK
   s" workspace-symbol-retained" [: RETAINED-TURNS ;] TALK
   s" workspace-symbol-redeclared" [: REDECLARED-TURNS ;] TALK
   s" workspace-symbol-reinclude" [: REINCLUDE-TURNS ;] TALK
   s" workspace-symbol-open-dependency" [: OPEN-DEP-TURNS ;] TALK
   LENGTH-TEXT
   5 1 ?do
      i LENGTH-N !
      LENGTH-DIR MAKE-DIR
      SB-RESET s" path-length-" SB-APPEND i FMT:SB-INT
      SB$ [: LENGTH-TURNS ;] TALK
   loop ;

: TEST-OUTLINES ( -- )
   s" document-symbol" [: OUTLINE-TURNS ;] TALK
   s" document-symbol-after-change" [: OUTLINE-AFTER-CHANGE-TURNS ;] TALK
   s" document-symbol-incomplete" [: OUTLINE-INCOMPLETE-TURNS ;] TALK ;

\ A FUNCTION: whose group is empty stores a signature that does not parse: the
\ packet is at its name F, line 1, characters 10-11.
: EMPTY-FFI ( -- ptr u8 n )  s\" require lib/ffi-abi.f\nFUNCTION: F getpid ( )\n" ;

: STORED-SIGNATURE-TURNS ( -- )
   INITIALIZE
   A-PATH EMPTY-FFI 1 OPENS
   SAY
   HEAR CAPABILITIES
   EMPTY-FFI A-PATH CHECKS
   A-PATH s" refused" COMPLETED
   A-PATH 1 EXPECT 1 10 1 11 1 s" E-BAD-STORED-SIGNATURE" DIAG+ PUBLISHES ;

\ A TRUSTED: definition named as the colon definition `undefine` removed before
\ it stores a signature that does not parse: the packet is at its name X, line
\ 2, characters 9-10.
: REPLACED-TRUSTED ( -- ptr u8 n )  s\" : X ( -- ) ;\nundefine X\nTRUSTED: X ( -- zz ) ;\n" ;

: REPLACED-SIGNATURE-TURNS ( -- )
   INITIALIZE
   A-PATH REPLACED-TRUSTED 1 OPENS
   SAY
   HEAR CAPABILITIES
   REPLACED-TRUSTED A-PATH CHECKS
   A-PATH s" refused" COMPLETED
   A-PATH 1 EXPECT 2 9 2 10 1 s" E-BAD-STORED-SIGNATURE" DIAG+ PUBLISHES ;

\ A defer whose name has two qualifiers asks a record for a malformed name: the
\ packet is at its name DIAG:MAL:NAME, line 0, characters 6-19.
: MALFORMED-DEFER ( -- ptr u8 n )  s\" defer DIAG:MAL:NAME ( -- )\n" ;

: MALFORMED-RECORD-TURNS ( -- )
   INITIALIZE
   A-PATH MALFORMED-DEFER 1 OPENS
   SAY
   HEAR CAPABILITIES
   MALFORMED-DEFER A-PATH CHECKS
   A-PATH s" refused" COMPLETED
   A-PATH 1 EXPECT
   0 6 0 19 1 s" E-BAD-QUALIFIED-RECORD" DIAG+
   0 6 0 19 1 s" E-STATEMENT-THROW" DIAG+ PUBLISHES ;

\ A colon definition CHECK rejects for its undefined NOPE still asks a record
\ for its malformed name: that packet follows the body's, at the name P:Q:R,
\ line 0, characters 2-7.
: MALFORMED-REJECTED ( -- ptr u8 n )  s\" : P:Q:R ( -- ) NOPE ;\n" ;

: MALFORMED-REJECTED-TURNS ( -- )
   INITIALIZE
   A-PATH MALFORMED-REJECTED 1 OPENS
   SAY
   HEAR CAPABILITIES
   MALFORMED-REJECTED A-PATH CHECKS
   A-PATH s" refused" COMPLETED
   A-PATH 1 EXPECT
   0 15 0 19 1 s" E-UNDEFINED" DIAG+
   0 2 0 7 1 s" E-BAD-QUALIFIED-RECORD" DIAG+
   0 20 0 21 1 s" E-STATEMENT-THROW" DIAG+ PUBLISHES ;

: TEST-DIAGNOSTICS ( -- )
   s" diagnostics-open" [: OPEN-TURNS ;] TALK
   s" diagnostics-require" [: REQUIRE-TURNS ;] TALK
   s" diagnostics-supersede" [: SUPERSEDE-TURNS ;] TALK
   s" diagnostics-one-refusal" [: ONE-REFUSAL-TURNS ;] TALK
   s" save-dirties-all" [: SAVE-TURNS ;] TALK
   s" close" [: CLOSE-TURNS ;] TALK
   s" engine-provided" [: ENGINE-TURNS ;] TALK
   s" utf16-column" [: UTF16-TURNS ;] TALK
   s" big-frame" [: BIG-TURNS ;] TALK
   s" dependency" [: DEPENDENCY-TURNS ;] TALK
   s" dependency-close" [: DEP-CLOSE-TURNS ;] TALK
   s" dependency-symlink" [: DEP-SYMLINK-TURNS ;] TALK
   s" dependency-shared" [: DEP-SHARED-TURNS ;] TALK
   s" two-in-turn" [: TWO-TURNS ;] TALK
   s" unended" [: UNENDED-TURNS ;] TALK
   s" missing-name" [: MISSING-NAME-TURNS ;] TALK
   s" incomplete" [: INCOMPLETE-TURNS ;] TALK
   s" incomplete-packets" [: INCOMPLETE-PACKET-TURNS ;] TALK
   s" held" [: HELD-TURNS ;] TALK
   s" shadowed-arity" [: SHADOWED-TURNS ;] TALK
   s" not-recorded" [: NOT-RECORDED-TURNS ;] TALK
   s" using-ambiguous" [: TWO-USED-TURNS ;] TALK
   s" using-shadow" [: GLOBAL-USED-TURNS ;] TALK
   s" using-clause" [: CLAUSE-USED-TURNS ;] TALK
   s" duplicate" [: DUPLICATE-TURNS ;] TALK
   s" duplicate-of-required" [: DUP-REQUIRED-TURNS ;] TALK
   s" top-level" [: TOP-LEVEL-TURNS ;] TALK
   s" stored-signature" [: STORED-SIGNATURE-TURNS ;] TALK
   s" replaced-signature" [: REPLACED-SIGNATURE-TURNS ;] TALK
   s" malformed-record" [: MALFORMED-RECORD-TURNS ;] TALK
   s" malformed-rejected" [: MALFORMED-REJECTED-TURNS ;] TALK
   s" deferred-definition" [: DEFINITION-TURNS ;] TALK
   s" deferred-unended" [: DEFERRED-UNENDED-TURNS ;] TALK ;

public

: TEST ( -- )
   LABEL-B READY
   IN-B READY
   WANT-B READY
   MSG-B READY
   PAR-B READY
   DIR-B READY
   EXP-B READY
   PATH-B READY
   TXT-B READY
   s" habu-lsp-test" HB-TMP-MKDIR N>BLEN DIR-B REPLACE
   T-RESET
   TEST-EXIT-TIMEOUT
   TEST-LIFECYCLE
   TEST-ANSWERS-AT-ONCE
   TEST-EXIT-WITHOUT-SHUTDOWN
   TEST-EOF-BEFORE-EXIT
   TEST-EOF-AFTER-SHUTDOWN
   TEST-BEFORE-INITIALIZE
   TEST-INITIALIZE-TWICE
   TEST-AFTER-SHUTDOWN
   TEST-UNKNOWN-METHOD
   TEST-IDS
   TEST-BODY-NOT-JSON
   TEST-INVALID-REQUEST
   TEST-SYNC
   TEST-MALFORMED-PARAMS
   TEST-FRAME-FORMS
   TEST-MALFORMED-FRAMES
   TEST-TRUNCATED-FRAMES
   TEST-STDOUT-CLOSED
   TEST-DIAGNOSTICS
   TEST-SYMBOLS
   TEST-OUTLINES
   TEST-DEFINITIONS
   TEST-HOVERS
   TEST-HIGHLIGHTS
   TEST-COMPLETIONS
   TEST-LINKS
   TEST-CANCELS
   SB-RESET s" artifact: " SB-APPEND DIR$ SB-APPEND SB$ type cr
   T-REPORT ;

;using
;package
