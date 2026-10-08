\ check-verify-test-lib.f - CHECK:VERIFY-BYTES and `tools/check.f --verify-only`.
\ Run: bin/hb --load tools/check-verify-test.f
\
\ The operation is called here directly, as a language server calls it, and the
\ command line is run as a child. Fixtures are files under a fresh HB_TMP
\ directory; the subject's bytes are handed over in memory. What can go wrong,
\ and the case that sees it:
\
\   a word of an earlier check, or of this process, reaches the subject   two-in-turn
\   the subject's top-level code runs, or a dependency's                  no-run
\   the copy on disk is checked instead of the bytes, or positions
\   are counted in it                                                     buffer-not-disk
\   a dependency that requires PATH back meets the copy on disk           requires-back
\   a require of an absent PATH falls back to another file                absent-path
\   an included child lowers the source using floor, then leaves an
\   import visible after clean return                                     load-using-floor
\   a dependency's packets are dropped, or name the subject               failing-dependency
\   an engine-provided path is verified because of its bytes              engine-provided
\   the verifier's own source is verified in the image that holds it      held
\   a child that ends without a result reads as a verdict                 no-result, deadline
\   a child that dies drops the packets it made before                    no-result
\   a statement the source leaves open is no refusal, or is not placed
\   at its opener in its file, or its record not among the packets        open-stop
\   an engine table row (EPRIM:, EPPRIM:) is read token by token or past
\   its first closer, or one the source ends inside is not refused at its
\   opener by the record check.f writes for a row                         engine-rows
\   a closure the verifier cannot follow, a string or locals group never
\   closed, a dynamic loader path, a missing or unreadable required
\   file, stops with no status line                  disc-stop, closure-line
\   a stop drops the packets made before it, or names another file than
\   the one it is in, a dependency's or the subject's                     stop-after-packet
\   a nested duplicate drops earlier all-errors packets                   duplicate-after-packet
\   a duplicate is no packet, or not the record --all-errors writes; the
\   scan stops at it; --all-errors goes past it               cli-duplicate,
\                                    cli-duplicate-goes-on, duplicate-in-dependency
\   a name the definer generates is no packet                             duplicate-made
\   a child that dies after a duplicate drops its record                  duplicate-then-dies
\   a complete answer followed by a failed process exit leaks framing or
\   invents a duplicate, or loses two identical real duplicates   unclean-answer
\   a closure that cannot be followed reads as verified, or is no packet at
\   the form that stops it                          missing-dependency, loader-form
\   a loader call in a body, which runs only when the body does, reads its
\   file, is refused for its path or defines its words for definitions after
\   it; a valid dynamic form is refused before effect checking             body-uncalled
\   an immediate loader in a body leaves its file unread                  body-immediate
\   a loader word's name stored, deferred, exported, defined, undefined or
\   retired is refused at the name, or one the global wordlist holds is no
\   duplicate                                                             reserved-names
\   a loader form whose word the file defined or retired before it is
\   followed or refused elsewhere, or a use the walk reads past, a package
\   name or a retirement in a body is refused                             loader-use
\   a word given a library storage definer's spelling, here or in a
\   required file, takes the next token as its name                       storage-binding
\   a loader form's fault drops the packets made before it, is not the
\   last, leaks a line that is no packet, or loses the child's code and
\   place                                                                 fault-after-packet
\   provided reads its file, or takes a form no loader takes              provided-meta
\   script-required stops the verifier without a packet at the loader     script-loader
\   a literal whose escape the checker refuses gives a loader its path    bad-escape
\   a tree file whose computed loads are all in bodies is refused, from
\   the tree root or from outside the tree                                body-computed
\   a closure wider than a fixed table is refused                         wide-closure
\   output past 4 MiB is cut short, or puts prose on --verify-only's
\   stderr                                              whole-output, cli-whole-output
\   --verify-only drops stdin's closure, names the subject otherwise
\   than the operation, or writes prose on stderr                         cli-file, cli-stdin
\   --stdin-path or --verify-only is taken where it means nothing         cli-usage
\   a definition line reaches --verify-only's output                      cli-file
\   a retained definition has no line, or a name other than as written,
\   a package or visibility other than the checker's record of it, a span
\   other than the token that declared it, or a name a definer generates
\   or a family has one before the checker reports it                     definitions
\   a refused body without a kept signature has a line, or one with it,
\   or a deferred body, has none, or one after an undeclared parser has
\   one                                                                   definitions-refused
\   a name defined again after undefine loses a line                      definitions-undefine
\   a dependency's definitions are dropped, or named by the subject       definitions-dependency
\   a file a check read has no file line, or several for the same bytes,
\   or one it did not read has one; a file line's digest is other than
\   the SHA-256 of the bytes the check read of the file                   files
\   the subject's file line states its copy on disk, not the bytes        identity-subject
\   two paths that hold the same bytes share one file line                identity-paths
\   a dependency rewritten between two checks keeps its file line's
\   digest, though a span in it names another word in each                identity-swap
\   a file line is keyed by its path alone, by its digest alone, by the
\   path's last bytes alone, or by a path or bytes its caller reuses; the
\   pre-pass writes one                                                   file-line-keys
\   a top-level loader of the subject that reached its file has no load
\   line, a span other than its operand's (the token after include or
\   require, a string loader's literal as written, escapes and all), a
\   path other than the canonical one, or read for a held file or held
\   for a read one; provided, a comment or a dependency's loader has one  loads
\   a file a loader cannot read has a load line, or its packet moves      loads-unread
\   a file read for a loader loses its line when its scan then stops      loads-scan-fail
\   an include of the subject's own path, read from the given bytes,
\   has no line                                                           loads-supplied
\   a body's immediate loader, or one after a word that may read on, has
\   a line                                                                loads-none
\   load lines outlive the next check, or check.f's pre-pass has one      loads-lifetime
\   a use bound to a located declaration has no line, a range other than
\   its token's, or a target other than the token that declared it: a
\   body's call, a quotation's, ['], is, a top-level call and tick, an
\   export operand, a generates: definer                                  uses
\   a use binds other than the declaration its scope selects: private
\   over global, a used public, a qualified name, an export's alias       uses
\   a use of a dependency's or its dependency's declaration names another
\   file; a use inside a dependency, or of a word the engine provides,
\   has a line; a use line reaches --verify-only's output                 uses
\   an ambiguous, shadowed or undefined name has a target, or the scan
\   binds nothing after them                                              uses-refused
\   a use binds other than the declaration undefine left visible          uses-order
\   a use of a refused body's kept signature has no target, or a use of
\   a refused signature type, which retains nothing, has one              uses-recovery
\   a use of a body the checker defers has no target, or the name only
\   its run defines has one                                               uses-deferred
\   two declarations at one file span from separate includes share a use's
\   identity, or a qualified/bare call selects the other package; DEFTYPE's
\   two generated converters at one span merge                              uses-visit
\   a refused duplicate takes the uses after it                           uses-duplicate
\   a declaration loses or moves its location when the store or the
\   location table grows                                                  uses-growth
\   a string literal before a call of a word a names: row declares binds
\   other than what XREF-FIND finds by its bytes, at other than the bytes
\   between its quotes, or nothing before a definer's call; a computed
\   operand, a caller with no row of its own or an escape the bytes do not
\   hold as written binds                                                 named-uses
\   a cursor's word is missing, or names a declaration other than the one
\   it binds: a body's prefix, an engine word the body has not used, a
\   word a TRUSTED: body before it declares; or the body's own word, or a
\   word declared after it, is offered                                    cands-body
\   a top-level cursor in a blank, at a prefix or at the end offers
\   nothing                                                               cands-top
\   a cursor in a name, a signature, a comment, a string, a TRUSTED: body,
\   a type's name, a deferred stretch or a body the scan never reaches
\   offers a word                                                         cands-none
\   a tick or is target offers a local                                    cands-targets
\   two locals that differ only in case merge, or lose their spelling     cands-locals
\   a used public is offered before its using or after it, or a name two
\   used publics export or one shadows a global with                      cands-refused
\   a word undefine retired is offered, or its redefinition with the
\   bytes of the first                                                    cands-order
\   a word binds other than its scope selects, or names another file:
\   private over global, a used public, a qualified public, a qualified
\   export's tail; a cursor is placed in a dependency's bytes             cands-scopes
\   the cursor changes the verdict, packets, definitions, uses or files,
\   or --verify-only writes a candidate line                              cands-observer
\   a source path exceeds the CLI's slot and leaks engine text on stderr cli-path-capacity
\   the child runs on the working directory's bin/hb, or is that
\   directory's tools/check-verify-child.f                                check-test: file-load-context
\   a top-level token the load refuses as unresolvable verifies, is
\   refused elsewhere than where it was read, or stops the scan           top-undefined
\   an ambiguous or shadowed name at top level or after ' verifies       top-ambiguous, top-shadow, top-tick
\   a name resolves before the statement that defines it                  top-order
\   a number out of range, or a token only shaped like one, verifies      top-number
\   a compile-only keyword resolves at top level                          top-keyword
\   a retired engine name or compiler-only axiom verifies as a top word    top-retired, top-axiom
\   a definition into a sealed package, or its reopening, verifies or is
\   refused elsewhere or otherwise than the load refuses it, a duplicate
\   there as sealed; a retirement there, or a definition into a package
\   left open, is refused                                                 sealed
\   a qualified name the load refuses verifies, or is refused otherwise
\   than a body refuses it                                                top-qualified
\   a parsing word's operand is resolved, or the stretch after it is
\   resolved, passed over in silence or answered verified                 top-deferred
\   a dependency's deferred stretch hides a caller token, or is forgotten top-nested-deferred
\   a declared parsing word's operand is read as code, a definer or `:`,
\   or the check stops at it or before what follows                       top-parses-bound
\   a through form's operands are read as code across lines, or a
\   prefix token spelled like an end closes it                            top-parses-through
\   a row binds a spelling: a defer, a resident word, a wrapper, a
\   package's own parses: or a retired binding is bounded, or an EXPORT
\   loses it                                                              top-parses-opaque
\   a loader, definer or package word a row bounds is dispatched, or a
\   loader, definition or loader's name after the window is not checked   top-parses-loader
\   a loader after a word no usable row bounds is read or refused, in
\   its file or in a file that loaded it                                  top-opaque-loader
\   a malformed or misplaced row loads, or is refused for another reason  top-parses-row
\   a names: row with no target, naming no word or naming a word that
\   takes no string verifies or loads, or a body calling its declarer or
\   the checker words behind it is admitted                               names-row
\   a declarer loses its identity, the binding query raises or answers a
\   refused name, a rolled-back row survives, a lost field falls back     top-parses-whitebox
\   the tokens after a word that renders source are deferred, though it
\   reads none of them, or a name its text may define, bare or
\   qualified, is refused                                                 top-renders
\   the operand or product of a word that defines a name it reads when it
\   runs - its body calls create, or calls such a word - is refused, or
\   the check goes on past one no row bounds                              top-create
\   a TRUSTED: word is taken to do other than the calls its body binds do,
\   or than anything when one of them binds to nothing                    top-trusted
\   kernel: is not taken as : is, or its refused body ends the scan       top-kernel
\   an open package's own qualified name the engine finds in the global
\   wordlist is refused, or that fallback reaches past the package or
\   misses the row of the word it finds                                   top-package-tail
\   a name a deferred word may define is refused after its call           top-defer-word
\   a deferred word followed only by comments is answered verified        top-defer-comment
\   a definition left to the run is answered verified, or not reported
\   where the checker's judgment of it stops                              def-deferred
\   a name the load accepts at top level is refused                       top-accepted
\   a type made by reached source rendering or nominal registration is
\   refused as absent; an unrelated absence or later error is lost         type-deferred
\
\ `measure` prints the time of one check of a one-definition subject and of
\ tools/check-core.f, whose closure is over thirty files.

require lib/errors.f
require lib/string.f
require lib/fmt.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-cwd.f
require lib/engine-candidate.f
require test/cold-engine.f
require tools/json.f
require tools/check-verify-core.f

package CHECK-VERIFY-TEST

\ A deadlock guard, never reached by a check that is merely slow: one takes
\ tens of milliseconds, and a loaded host stretches a child to seconds.
60000 constant GUARD-MS
\ Shorter than any engine takes to start.
1 constant SHORT-MS
\ What a child of this test may write on either stream: check.f's packets from
\ a full capture of the verifier's output fit.
$800000 constant CAP
\ What the verifier child's stdout once held at most: CHECK's fixed capture.
$400000 constant OLD-CAPTURE
\ big.f's definitions, each a packet of some 500 bytes: more in all than
\ OLD-CAPTURE.
10000 constant BIG-DEFS

create ROOT FS-PATH-CAP allot
create AT FS-PATH-CAP allot
create SUBJ FS-PATH-CAP allot
variable ROOT-U
variable AT-U
variable SUBJ-U
DYNAMIC-BUFFER OUT u8
DYNAMIC-BUFFER ERR u8
DYNAMIC-BUFFER FILE-BYTES u8
DYNAMIC-BUFFER GEN u8                   \ generated source
DYNAMIC-BUFFER ALL-ERR u8               \ --all-errors' stderr, to compare
variable GEN-U
variable START-NS
variable CLI-OUT-U


: COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;


: ROOT$ ( -- ptr u8 n )
   ROOT ROOT-U @ ;


\ NAME in the fixture directory, canonical as every packet spells it.
: AT$ ( ptr u8 n -- ptr u8 n )
   ROOT$ 2swap AT JOIN-PATH AT-U !
   AT AT-U @ ;


: FIXTURE ( ptr u8 n ptr u8 n -- ) {: name:ptr nameu:n src:ptr srcu:n :}
   name nameu AT$ src srcu WRITE-ALL ;


\ Check SRC as the fixture NAME, which keeps its spelling in SUBJ.
: CHECK-AS ( ptr u8 n ptr u8 n n -- CHECK:verdict ) {: src:ptr srcu:n name:ptr nameu:n ms:n :}
   name nameu AT$ SUBJ SUBJ-U COPY!
   src srcu SUBJ SUBJ-U @ [: drop ;] ms >MS CHECK:VERIFY-BYTES ;


: SUBJ$ ( -- ptr u8 n )
   SUBJ SUBJ-U @ ;


\ A tree file's canonical path, kept in SUBJ.
: TREE$ ( ptr u8 n -- ptr u8 n )
   SOURCE-ROOT:CANONICAL drop SUBJ SUBJ-U COPY!
   SUBJ$ ;


\ A file's bytes, in FILE-BYTES.
: TREE-BYTES ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   a u FILE-SIZE {: size:n :}
   size 1 max FILE-BYTES-RESERVE
   a u 0 FILE-BYTES size READ-ALL drop
   0 FILE-BYTES size ;


\ 0 verified, 1 refused, 2 engine-provided, 3 held, 4 incomplete, 5 deferred.
: KIND ( CHECK:verdict -- n )
   MATCH CHECK:verdict
      verified OF 0 ENDOF
      refused OF 1 ENDOF
      engine-provided OF 2 ENDOF
      held OF 3 ENDOF
      incomplete OF {: status :} 4 ENDOF
      deferred OF 5 ENDOF
   ;MATCH ;


: EXPECT-KIND ( CHECK:verdict n ptr u8 n -- ) {: v want:n label:ptr labelu:n :}
   label labelu T-LABEL v KIND want T=
   v KIND want = if exit then
   s" log: " type CHECK:VERIFY-LOG$ type cr ;


\ How a child that gave no result ended: the exit code, -1 for a signal, -2
\ for the deadline, and -3 for a verdict, which is no incomplete one.
: STATUS ( CHECK:verdict -- n )
   MATCH CHECK:verdict
      incomplete OF
         MATCH outcome
            exited OF ENDOF
            signaled OF drop -1 ENDOF
            timeout OF -2 ENDOF
         ;MATCH
      ENDOF
      verified OF -3 ENDOF
      refused OF -3 ENDOF
      engine-provided OF -3 ENDOF
      held OF -3 ENDOF
      deferred OF -3 ENDOF
   ;MATCH ;


\ The value under KEY in packet NODE when it is of KIND, else -1.
: VALUE ( n ptr u8 n n -- n ) {: node:n key:ptr keyu:n kind:n :}
   node 0 < if -1 exit then
   node key keyu JSON-GET {: v:n :}
   v 0 < if -1 exit then
   v JSON-KIND kind = if v exit then
   -1 ;


: STRING$ ( n ptr u8 n -- ptr u8 n )
   J-STR VALUE dup 0 < if drop s" " exit then
   JSON-STRING$ ;


: NUMBER$ ( n ptr u8 n -- ptr u8 n )
   J-NUM VALUE dup 0 < if drop s" " exit then
   JSON-NUMBER$ ;


\ The first packet in LINES whose string KEY is VALUE, or -1.
: PACKET ( ptr u8 n ptr u8 n ptr u8 n -- n ) {: lines:ptr linesu:n key:ptr keyu:n val:ptr valu:n :}
   lines linesu JSONL-START
   begin
      JSONL-NEXT-OBJECT
      dup 0 < if exit then
      dup key keyu STRING$ val valu STR= 0=
   while
      drop
   repeat ;


\ How many JSON objects LINES holds.
: OBJECTS ( ptr u8 n -- n )
   JSONL-START 0
   begin JSONL-NEXT-OBJECT 0 >= while 1+ repeat ;


\ Every line is a JSON object: the strict reader refuses a line that is not
\ JSON, E-JSON-SYNTAX, or not an object, E-JSON-TYPE.
: ALL-JSON? ( ptr u8 n -- bool )
   JSONL-START-STRICT
   [: begin JSONL-NEXT-OBJECT 0 < until ;] catch {: rc:n :}
   rc 0= if true exit then
   rc E-JSON-SYNTAX = rc E-JSON-TYPE = or if false exit then
   rc throw ;


\ ---- the operation ---------------------------------------------------------

: DEP$SRC ( -- ptr u8 n )
   s\" : CVT-SEVEN ( -- n ) 7 ;\n" ;

: BAD-DEP$SRC ( -- ptr u8 n )
   s\" : CVT-EIGHT ( -- n ) 8 ;\n: CVT-BROKEN ( -- n n ) 8 ;\n" ;

\ A refused definition, then one left open, where the verifier stops.
: OPEN-DEP$SRC ( -- ptr u8 n )
   s\" : CVT-BAD-DEP ( -- n n ) 8 ;\n: CVT-OPEN-DEP ( -- n ) 1\n" ;

\ What the files on disk hold where a case checks other bytes as them: a clean
\ definition on line 1, and a string discovery refuses to follow.
: POS$DISK ( -- ptr u8 n )
   s\" : CVT-POS ( n -- n ) 1 + ;\n" ;

: LOOP$DISK ( -- ptr u8 n )
   s\" s\" never closed\n" ;

: BACK$SRC ( -- ptr u8 n )
   s\" require loop.f\n: CVT-BACK ( -- n ) 5 ;\n" ;

\ The fixture directory has no README.md; the tree root, the fallback, has one.
: BACK-README$SRC ( -- ptr u8 n )
   s\" require README.md\n: CVT-BACK-README ( -- n ) 2 ;\n" ;

\ The marker each file's top-level code writes when it runs.
: MARK$ ( ptr u8 n -- ptr u8 n ) {: name:ptr nameu:n :}
   SB-RESET
   s" require lib/fs.f" SB-APPEND $0a SB-APPEND-C
   s" s" SB-APPEND $22 SB-APPEND-C $20 SB-APPEND-C
   name nameu AT$ SB-APPEND
   $22 SB-APPEND-C s"  s" SB-APPEND $22 SB-APPEND-C s"  ran" SB-APPEND
   $22 SB-APPEND-C s"  WRITE-ALL" SB-APPEND $0a SB-APPEND-C
   SB$ ;

: MARK-DEP$SRC ( -- ptr u8 n )
   s" marker-dep" MARK$ ;

: MARK$SRC ( -- ptr u8 n )
   s" marker" MARK$ 2drop
   s\" require mark-dep.f\n: CVT-MARKED ( -- n ) 3 ;\n" SB-APPEND
   SB$ ;

: GEN+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   GEN-U @ u + GEN-RESERVE
   a GEN-U @ GEN u BYTE-COPY
   GEN-U @ u + GEN-U ! ;

: GEN-C+ ( n -- )
   {: c:n :}
   GEN-U @ 1+ GEN-RESERVE
   c GEN-U @ GEN c!
   GEN-U @ 1+ GEN-U ! ;

: GEN-N+ ( n -- )
   dup 10 >= if dup 10 / RECURSE then
   10 mod $30 + GEN-C+ ;

\ big.f: CVT-B<I> for each I, every one a value short.
: BIG$SRC ( -- ptr u8 n )
   0 GEN-U !
   BIG-DEFS 0 ?do
      s" : CVT-B" GEN+ i GEN-N+ s\"  ( -- n n ) 8 ;\n" GEN+
   loop
   0 GEN GEN-U @ ;

\ DA defined twice, the second DA at line 2, column 3, bytes 20-22; and LATE,
\ refused after it.
: DUP$SRC ( -- ptr u8 n )
   s\" : DA ( -- n ) 1 ;\n: DA ( -- n ) 2 ;\n" ;

: DUP-LATE$SRC ( -- ptr u8 n )
   s\" : DA ( -- n ) 1 ;\n: DA ( -- n ) 2 ;\n: LATE ( n -- n ) drop ;\n" ;

\ A refused definition, then one `using` more than the checker holds open at
\ once (CK-USE-MAX, the engine's USE-MAX).
CK-USE-MAX 1 + constant OVER-USINGS

: OVER-USING$SRC ( -- ptr u8 n )
   0 GEN-U !
   s\" : CVT-REFUSED ( -- n n ) 8 ;\n" GEN+
   OVER-USINGS 0 ?do s\" using SOURCE-ROOT\n" GEN+ loop
   0 GEN GEN-U @ ;

\ What same-a.f and same-b.f both hold.
: SAME$SRC ( -- ptr u8 n )
   s\" \\ the same bytes at two paths\n" ;

: FIXTURES ( -- )
   s" cvt" HB-TMP-MKDIR SOURCE-ROOT:CANONICAL drop ROOT ROOT-U COPY!
   s" dep.f" DEP$SRC FIXTURE
   s" bad-dep.f" BAD-DEP$SRC FIXTURE
   s" open-dep.f" OPEN-DEP$SRC FIXTURE
   s" pos.f" POS$DISK FIXTURE
   s" loop.f" LOOP$DISK FIXTURE
   s" back.f" BACK$SRC FIXTURE
   s" back-readme.f" BACK-README$SRC FIXTURE
   s" mark-dep.f" MARK-DEP$SRC FIXTURE
   s" order-dep.f" s\" package CVT-ORDER public\n: VALUE ( -- n ) 7 ;\n;package\n" FIXTURE
   s" order-late.f" s\" package CVT-LATE public\n: EARLY ( -- n ) CVT-ORDER:VALUE ;\n;package\nrequire order-dep.f\n" FIXTURE
   s" order-context-dep.f" s\" package CVT-CONTEXT public\n: USE ( -- n ) VALUE ;\n;package\n" FIXTURE
   s" order-context.f" s\" require order-dep.f\nusing CVT-ORDER\nrequire order-context-dep.f\n;using\n" FIXTURE
   s" order-package-dep.f" s\" : CVT-PKG-VALUE ( -- n ) 3 ;\n" FIXTURE
   s" files-mid.f" s\" require files-nested.f\n" FIXTURE
   s" files-nested.f" s\" : CVT-NESTED ( -- n ) 4 ;\n" FIXTURE
   s" files-inc.f" s\" \\ no definition\n" FIXTURE
   s" files-unread.f" s\" : CVT-UNREAD ( -- n ) 5 ;\n" FIXTURE
   s" same-a.f" SAME$SRC FIXTURE
   s" same-b.f" SAME$SRC FIXTURE
   s" ld-dep.f" s\" require ld-nest.f\n" FIXTURE
   s" ld-nest.f" s\" \\ loaded\n" FIXTURE
   s" ld-inc.f" s\" \\ loaded\n" FIXTURE
   s" ld-str-inc.f" s\" \\ loaded\n" FIXTURE
   s" ld-str-req.f" s\" \\ loaded\n" FIXTURE
   s" ld-script.f" s\" \\ loaded\n" FIXTURE
   s" ld-esc.f" s\" \\ loaded\n" FIXTURE
   s" ld-body.f" s\" \\ loaded\n" FIXTURE
   s" ld-open.f" s\" : CVT-LDOPEN ( -- n ) 1\n" FIXTURE
   s" order-package.f" s\" package CVT-PKG public\nrequire order-package-dep.f\n;package\n: CVT-PKG-USE ( -- n ) CVT-PKG:CVT-PKG-VALUE ;\n" FIXTURE
   s" order-floor-child.f" s\" ;package\nusing CVT-FLOOR-CHILD\n" FIXTURE
   s" order-floor-include.f" s\" package CVT-FLOOR-BASE public\n: BASE-VALUE ( -- n ) 11 ;\n;package\npackage CVT-FLOOR-CHILD public\n: CHILD-VALUE ( -- n ) 29 ;\n;package\npackage CVT-FLOOR-PARENT\nusing CVT-FLOOR-BASE\ninclude order-floor-child.f\npackage CVT-FLOOR-AFTER public\n: AFTER-VALUE ( -- n ) CHILD-VALUE ;\n;package\n" FIXTURE
   s" order-floor-require.f" s\" package CVT-FLOOR-BASE public\n: BASE-VALUE ( -- n ) 11 ;\n;package\npackage CVT-FLOOR-CHILD public\n: CHILD-VALUE ( -- n ) 29 ;\n;package\npackage CVT-FLOOR-PARENT\nusing CVT-FLOOR-BASE\nrequire order-floor-child.f\npackage CVT-FLOOR-AFTER public\n: AFTER-VALUE ( -- n ) CHILD-VALUE ;\n;package\n" FIXTURE
   s" order-double.f" s\" include order-dep.f\ninclude order-dep.f\n" FIXTURE
   s" dup.f" DUP$SRC FIXTURE
   s" dup-late.f" DUP-LATE$SRC FIXTURE
   s" big.f" BIG$SRC FIXTURE ;


\ The second check must see neither what the first declared nor a word of this
\ process.
: TWO-IN-TURN ( -- )
   s\" : CVT-ONE ( -- n ) 1 ;\n" s" one.f" GUARD-MS CHECK-AS
   0 s" two-in-turn: the first" EXPECT-KIND
   s\" : CVT-TWO ( -- n ) CVT-ONE ;\n" s" two.f" GUARD-MS CHECK-AS
   1 s" two-in-turn: the second" EXPECT-KIND
   CHECK:VERIFY-OUT$ s" token" s" CVT-ONE" PACKET {: p:n :}
   s" two-in-turn: CVT-ONE is undefined" T-LABEL p s" code" STRING$ s" E-UNDEFINED" T$=
   s" two-in-turn: in the second" T-LABEL p s" file" STRING$ SUBJ$ T$=
   s\" : CVT-OWN ( -- ptr u8 n ) CHECK:VERIFY-OUT$ ;\n" s" own.f" GUARD-MS CHECK-AS
   1 s" two-in-turn: a word of this process" EXPECT-KIND
   CHECK:VERIFY-OUT$ s" token" s" CHECK:VERIFY-OUT$" PACKET {: q:n :}
   s" two-in-turn: it is undefined" T-LABEL q s" code" STRING$ s" E-UNDEFINED" T$= ;


\ Neither the subject's top-level code nor its dependency's runs; loaded, both do.
: NO-RUN ( -- )
   MARK$SRC s" mark.f" GUARD-MS CHECK-AS
   0 s" no-run: verified" EXPECT-KIND
   s" no-run: the subject wrote nothing" T-LABEL s" marker" AT$ EXISTS? TFALSE
   s" no-run: its dependency wrote nothing" T-LABEL s" marker-dep" AT$ EXISTS? TFALSE
   s" mark.f" MARK$SRC FIXTURE
   PROC-ARGV-ENV-RESET
   s" --load" >LEN PROC-ARGV+
   s" mark.f" AT$ >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ >LEN 0 OUT CAP >LEN 0 ERR CAP >LEN GUARD-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME {: outu:len erru:len o :}
   s" no-run: a load runs both" T-LABEL o MATCH outcome
      exited OF ENDOF
      signaled OF drop -1 ENDOF
      timeout OF -2 ENDOF
   ;MATCH 0 T=
   s" no-run: the loaded subject wrote its marker" T-LABEL s" marker" AT$ EXISTS? TTRUE
   s" no-run: the loaded dependency wrote its marker" T-LABEL s" marker-dep" AT$ EXISTS? TTRUE ;


\ The disk copy is clean; the bytes put a rejected definition on line 4.
: BUFFER-NOT-DISK ( -- )
   s\" \n\n\n: CVT-POS ( n -- n ) drop ;\n" s" pos.f" GUARD-MS CHECK-AS
   1 s" buffer-not-disk: the bytes are checked" EXPECT-KIND
   CHECK:VERIFY-OUT$ s" code" s" E-MISMATCH" PACKET {: p:n :}
   s" buffer-not-disk: the packet names PATH" T-LABEL p s" file" STRING$ SUBJ$ T$=
   s" buffer-not-disk: line in the bytes" T-LABEL p s" line" NUMBER$ s" 4" T$=
   s" buffer-not-disk: byte in the bytes" T-LABEL p s" byte_start" NUMBER$ s" 24" T$= ;


\ back.f requires loop.f, whose disk copy discovery refuses; the bytes are clean.
: REQUIRES-BACK ( -- )
   s\" require back.f\n: CVT-LOOP ( -- n ) CVT-BACK ;\n" s" loop.f" GUARD-MS CHECK-AS
   0 s" requires-back: the dependency meets the bytes" EXPECT-KIND ;


\ PATH is absent: a require of it, the subject's own or a dependency's, meets
\ the bytes, never the tree root's README.md.
: ABSENT-PATH ( -- )
   s\" require README.md\n: CVT-SELF ( -- n ) 1 ;\n" s" README.md" GUARD-MS CHECK-AS
   0 s" absent-path: the subject requires itself" EXPECT-KIND
   s\" require back-readme.f\n: CVT-ABSENT ( -- n ) CVT-BACK-README ;\n"
   s" README.md" GUARD-MS CHECK-AS
   0 s" absent-path: a dependency requires it back" EXPECT-KIND ;


: FAILING-DEPENDENCY ( -- )
   s\" require bad-dep.f\n: CVT-USE8 ( -- n ) CVT-EIGHT ;\n" s" use8.f" GUARD-MS CHECK-AS
   1 s" failing-dependency: refused" EXPECT-KIND
   CHECK:VERIFY-OUT$ s" word" s" cvt-broken" PACKET {: p:n :}
   s" failing-dependency: the packet names the dependency" T-LABEL
   p s" file" STRING$ s" bad-dep.f" AT$ T$=
   s" failing-dependency: at its line" T-LABEL p s" line" NUMBER$ s" 2" T$=
   s" failing-dependency: none names the subject" T-LABEL
   CHECK:VERIFY-OUT$ s" file" SUBJ$ PACKET 0 < TTRUE ;


\ Bytes no loader would accept, as the engine's own lib/string.f.
: ENGINE-PROVIDED ( -- )
   s\" : STR= ( -- ) drop ;\ns\" never closed\n" s" lib/string.f" TREE$
   [: drop ;] GUARD-MS >MS CHECK:VERIFY-BYTES
   2 s" engine-provided: whatever the bytes hold" EXPECT-KIND
   s" engine-provided: no packet" T-LABEL CHECK:VERIFY-OUT$ nip 0 T= ;


: HELD ( -- )
   s" src/habu/verify-source.f" TREE-BYTES s" src/habu/verify-source.f" TREE$
   [: drop ;] GUARD-MS >MS CHECK:VERIFY-BYTES
   3 s" held: the verifier's own source" EXPECT-KIND ;


\ Where the second line of the given text starts.
: LINE-TWO ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 begin dup u < if a over + c@ 10 <> else false then while 1+ repeat 1+ ;


\ An open definition stops the verifier at its opener (7155, a code private to
\ src/habu/verify-source.f): a refusal that says where, and the stop's record,
\ the one `check.f --verify-only` writes, in VERIFY-OUT$ for a client to
\ publish.
: OPEN-STOP ( -- )
   s\" : CVT-OPEN ( -- n ) 1\n" s" open.f" GUARD-MS CHECK-AS {: v :}
   v 1 s" open-stop: refused" EXPECT-KIND
   s" open-stop: the code" T-LABEL CHECK:VERIFY-STOP 7155 T=
   s" open-stop: the subject" T-LABEL CHECK:VERIFY-STOP-SUBJECT? TTRUE
   s" open-stop: its name" T-LABEL CHECK:VERIFY-STOPPED$ SUBJ$ T$=
   s" open-stop: at the opener" T-LABEL CHECK:VERIFY-STOP-AT 0 T=
   CHECK:VERIFY-OUT$ s" code" s" E-STATEMENT-THROW" PACKET {: p:n :}
   s" open-stop: its record names the subject" T-LABEL p s" file" STRING$ SUBJ$ T$=
   s" open-stop: with the code" T-LABEL p s" throw_code" NUMBER$ s" 7155" T$=
   s" open-stop: at the opener's line" T-LABEL p s" line" NUMBER$ s" 1" T$=
   s" open-stop: and column" T-LABEL p s" column" NUMBER$ s" 1" T$= ;


\ The verifier stops at a closure it cannot follow: SRC, checked as the fixture
\ NAME, is refused, and its status line, the one `check.f --verify-only` prints
\ on stdout, is the line the xt builds, the file the verifier stopped in and
\ why.
: STOP-LINE ( ptr u8 n ptr u8 n ptr u8 n [ -- ] -- )
   {: src:ptr srcu:n name:ptr nameu:n label:ptr labelu:n line :}
   src srcu name nameu GUARD-MS CHECK-AS {: v :}
   v 1 label labelu EXPECT-KIND
   SB-RESET
   line execute
   $0a SB-APPEND-C
   label labelu T-LABEL CHECK:VERIFY-LOG$ SB$ T$= ;

\ A string or a locals group the bytes never close stops the verifier at it,
\ after the packets it made before, in the file it is in: the subject or a
\ file the subject requires.
: DISC-STOP ( -- )
   s\" : CVT-STR ( -- ) s\" abc ;\n" s" disc.f"
   s" disc-stop: an open string's status line"
   [: SUBJ$ SB-APPEND
      s" : discovery rejected: unterminated string or locals group" SB-APPEND ;]
   STOP-LINE
   s\" : CVT-LOC ( n -- n ) {: a\n" s" disc.f"
   s" disc-stop: an open locals group's status line"
   [: SUBJ$ SB-APPEND
      s" : discovery rejected: unterminated string or locals group" SB-APPEND ;]
   STOP-LINE
   s\" : CVT-DISC-BAD ( -- n n ) 8 ;\ns\" never closed\n" s" disc.f"
   s" disc-stop: after a refused definition"
   [: SUBJ$ SB-APPEND
      s" : discovery rejected: unterminated string or locals group" SB-APPEND ;]
   STOP-LINE
   s" disc-stop: the packet made first is kept" T-LABEL
   CHECK:VERIFY-OUT$ s" word" s" cvt-disc-bad" PACKET 0 >= TTRUE
   CHECK:VERIFY-OUT$ s" code" s" E-UNTERMINATED-STRING" PACKET {: r:n :}
   s" disc-stop: the stop's record in the subject" T-LABEL r s" file" STRING$ SUBJ$ T$=
   s" disc-stop: at the opener's line" T-LABEL r s" line" NUMBER$ s" 2" T$=
   s\" require loop.f\n" s" disc-dep.f"
   s" disc-stop: a dependency's status line"
   [: s" loop.f" AT$ SB-APPEND
      s" : discovery rejected: unterminated string or locals group" SB-APPEND ;]
   STOP-LINE
   s" disc-stop: not in the subject" T-LABEL CHECK:VERIFY-STOP-SUBJECT? TFALSE
   s" disc-stop: in the dependency" T-LABEL CHECK:VERIFY-STOPPED$ s" loop.f" AT$ T$=
   s" disc-stop: at its opener" T-LABEL CHECK:VERIFY-STOP-AT 0 T= ;

\ A loader path discovery cannot follow, and a required file that is not there
\ or that the file system will not read (the census's dyn-loader.f and
\ missing-require.f, and a file whose mode forbids reading it), each in the
\ subject.
: CLOSURE-LINE ( -- )
   s\" : H1 ( n -- n ) 1 + ;\ns\" x.f\" 2dup + drop included\n" s" dyn-loader.f"
   s" closure-line: a dynamic loader path's status line"
   [: SUBJ$ SB-APPEND
      s" : discovery rejected: dynamic (non-literal) loader path" SB-APPEND ;]
   STOP-LINE
   s\" require nosuch-lib.f\n: H1 ( n -- n ) 1 + ;\n" s" missing-require.f"
   s" closure-line: a missing source's status line"
   [: s" nosuch-lib.f" AT$ SB-APPEND s" : no such source" SB-APPEND ;]
   STOP-LINE
   s" locked-line.f" s\" \\ locked\n" FIXTURE
   s" locked-line.f" AT$ 0 CHMOD-MODE
   s\" require locked-line.f\n" s" unread-line.f"
   s" closure-line: an unreadable source's status line"
   [: s" cannot read " SB-APPEND s" locked-line.f" AT$ SB-APPEND ;]
   STOP-LINE ;


\ The verifier dies after it refused a definition: no result line, its exit and
\ its words, and the packet it made first is kept. The input's last `using` is
\ one past what the checker holds open: CHECKER-USING's `76 die`
\ (src/core/checker.f), which `--load` refuses as ENGINE-ERROR:USING-OVERFLOW.
\ The bare `generates:` this case read before no longer kills the verifier: it
\ stops it at the reader (7187, E-MISSING-NAME).
: NO-RESULT ( -- )
   OVER-USING$SRC s" over-using.f" GUARD-MS CHECK-AS {: v :}
   v 4 s" no-result: incomplete" EXPECT-KIND
   s" no-result: the child's exit" T-LABEL v STATUS 76 T=
   s" no-result: the child's words" T-LABEL
   CHECK:VERIFY-LOG$ s" checker: using stack overflow" CONTAINS? TTRUE
   CHECK:VERIFY-OUT$ s" word" s" cvt-refused" PACKET {: p:n :}
   s" no-result: the packet made first" T-LABEL p s" file" STRING$ SUBJ$ T$= ;


: DEADLINE ( -- )
   DEP$SRC s" dep.f" SHORT-MS CHECK-AS {: v :}
   v 4 s" deadline: incomplete" EXPECT-KIND
   s" deadline: timed out" T-LABEL v STATUS -2 T= ;


\ The verifier stops at an open definition after it refused one: the packet it
\ made first is kept, and the stop is at the opener in the file it is in, a
\ dependency's and then the subject's.
: STOP-AFTER-PACKET ( -- )
   s\" require open-dep.f\n: CVT-OPEN-USE ( -- n ) 1 ;\n" s" open-use.f" GUARD-MS CHECK-AS {: v :}
   v 1 s" stop-after-packet: refused" EXPECT-KIND
   CHECK:VERIFY-OUT$ s" word" s" cvt-bad-dep" PACKET {: p:n :}
   s" stop-after-packet: the dependency's packet" T-LABEL
   p s" file" STRING$ s" open-dep.f" AT$ T$=
   s" stop-after-packet: in the dependency" T-LABEL
   CHECK:VERIFY-STOP-SUBJECT? TFALSE
   CHECK:VERIFY-STOPPED$ s" open-dep.f" AT$ T$=
   s" stop-after-packet: at its opener" T-LABEL
   CHECK:VERIFY-STOP-AT OPEN-DEP$SRC LINE-TWO T=
   s\" : CVT-BAD ( -- n n ) 8 ;\n: CVT-OPEN ( -- n ) 1\n" {: own:ptr ownu:n :}
   own ownu s" open-own.f" GUARD-MS CHECK-AS {: w :}
   w 1 s" stop-after-packet: the subject's, refused" EXPECT-KIND
   CHECK:VERIFY-OUT$ s" word" s" cvt-bad" PACKET {: q:n :}
   s" stop-after-packet: the subject's packet" T-LABEL q s" file" STRING$ SUBJ$ T$=
   s" stop-after-packet: in the subject" T-LABEL CHECK:VERIFY-STOP-SUBJECT? TTRUE
   s" stop-after-packet: at the subject's opener" T-LABEL
   CHECK:VERIFY-STOP-AT own ownu LINE-TWO T= ;


\ A require of a file that is not there refuses the subject, with a packet at
\ the loader word that names it, in the file that holds the word.
: MISSING-DEPENDENCY ( -- )
   s\" \\ lost\nrequire cvt-missing.f\n" s" lost.f" GUARD-MS CHECK-AS
   1 s" missing-dependency: refused" EXPECT-KIND
   CHECK:VERIFY-OUT$ s" code" s" E-MISSING-SOURCE" PACKET {: p:n :}
   s" missing-dependency: in the subject" T-LABEL p s" file" STRING$ SUBJ$ T$=
   s" missing-dependency: at the loader word" T-LABEL p s" token" STRING$ s" require" T$=
   s" missing-dependency: its line" T-LABEL p s" line" NUMBER$ s" 2" T$=
   s" missing-dependency: its column" T-LABEL p s" column" NUMBER$ s" 1" T$= ;


\ Why a loader form ends the verification, after the file it is in.
: DYNAMIC$ ( -- ptr u8 n )
   s" discovery rejected: dynamic (non-literal) loader path" ;

: OPENER$ ( -- ptr u8 n )
   s" discovery rejected: unsupported string opener before a loader word" ;

: CAPACITY$ ( -- ptr u8 n )
   s" discovery rejected: capacity exceeded" ;

: SHADOWED$ ( -- ptr u8 n )
   s" discovery rejected: loader word used after the file defined its name" ;

: RETIRED$ ( -- ptr u8 n )
   s" discovery rejected: loader word used after the file retired it" ;


\ The first packet of the last check with CODE is in the subject, at TOKEN on
\ LINE and COLUMN.
: AT-TOKEN ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: label:ptr labelu:n code:ptr codeu:n tok:ptr toku:n line:ptr lineu:n col:ptr colu:n :}
   CHECK:VERIFY-OUT$ s" code" code codeu PACKET {: p:n :}
   label labelu T-LABEL p s" file" STRING$ SUBJ$ T$=
   label labelu T-LABEL p s" token" STRING$ tok toku T$=
   label labelu T-LABEL p s" line" NUMBER$ line lineu T$=
   label labelu T-LABEL p s" column" NUMBER$ col colu T$= ;


\ The last check's status line names the subject and WHY.
: SUBJ-STATUS ( ptr u8 n ptr u8 n -- ) {: label:ptr labelu:n why:ptr whyu:n :}
   SB-RESET SUBJ$ SB-APPEND s" : " SB-APPEND why whyu SB-APPEND $0a SB-APPEND-C
   label labelu T-LABEL CHECK:VERIFY-LOG$ SB$ T$= ;


\ The last check's status line names the fixture NAME, which is not there.
: MISSING-STATUS ( ptr u8 n ptr u8 n -- ) {: label:ptr labelu:n name:ptr nameu:n :}
   SB-RESET name nameu AT$ SB-APPEND s\" : no such source\n" SB-APPEND
   label labelu T-LABEL CHECK:VERIFY-LOG$ SB$ T$= ;


\ VERIFY-OUT$'s last line, without its newline.
: LAST-LINE ( -- ptr u8 n )
   CHECK:VERIFY-OUT$ {: a:ptr u:n :}
   u 0= if a 0 exit then
   u 1 - {: end:n :}
   end begin dup 0 > if a over 1 - + c@ 10 <> else false then while 1 - repeat
   {: start:n :}
   a start + end start - ;


: VERIFIED ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu:n name:ptr nameu:n :}
   src srcu name nameu GUARD-MS CHECK-AS 0 name nameu EXPECT-KIND ;


\ SRC, checked as the fixture NAME, is refused at the loader form TOKEN on LINE
\ and COLUMN of the subject, and its status line names the subject and WHY.
: LOADER-REFUSED ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n name:ptr nameu:n tok:ptr toku:n line:ptr lineu:n col:ptr colu:n why:ptr whyu:n :}
   src srcu name nameu GUARD-MS CHECK-AS 1 name nameu EXPECT-KIND
   name nameu s" E-LOADER-FORM" tok toku line lineu col colu AT-TOKEN
   name nameu why whyu SUBJ-STATUS ;


\ A loader form discovery cannot follow, in a file the subject requires: a
\ packet at that form, in that file. A path a `c"` string gives at the top
\ level of the subject: a packet at its loader word, and the opener status
\ line.
: LOADER-FORM ( -- )
   s" dyn-dep.f" s\" \\ dyn\nPATH$ included\n" FIXTURE
   s\" require dyn-dep.f\n" s" dyn-use.f" GUARD-MS CHECK-AS
   1 s" loader-form: refused" EXPECT-KIND
   CHECK:VERIFY-OUT$ s" code" s" E-LOADER-FORM" PACKET {: p:n :}
   s" loader-form: in the dependency" T-LABEL p s" file" STRING$ s" dyn-dep.f" AT$ T$=
   s" loader-form: at the loader word" T-LABEL p s" token" STRING$ s" included" T$=
   s" loader-form: its line" T-LABEL p s" line" NUMBER$ s" 2" T$=
   s" loader-form: its column" T-LABEL p s" column" NUMBER$ s" 7" T$=
   s\" c\" x.f\" required\n" s" top-opener.f"
   s" required" s" 1" s" 9" OPENER$ LOADER-REFUSED ;


\ A comment between a literal path and its loader word is skipped as the loader
\ skips it: a subject that loads one file across a `( … )` comment and another
\ across a `\` comment, and uses what both define, is verified. A file that is
\ not there is refused at the loader word that names it, a comment between them
\ or not.
: COMMENTED-LITERAL ( -- )
   s" lit-paren.f" s\" : CVT-PAREN ( -- n ) 1 ;\n" FIXTURE
   s" lit-line.f" s\" : CVT-LINE ( -- n ) 2 ;\n" FIXTURE
   s\" s\" lit-paren.f\" ( kept ) required\ns\" lit-line.f\" \\ kept\nrequired\n: CVT-LIT ( -- n ) CVT-PAREN CVT-LINE + ;\n"
   s" lit.f" GUARD-MS CHECK-AS
   0 s" commented-literal: verified" EXPECT-KIND
   s\" s\" cvt-lit-gone.f\" ( kept ) required\n" s" lit-gone.f" GUARD-MS CHECK-AS
   1 s" commented-literal: a missing one refused" EXPECT-KIND
   CHECK:VERIFY-OUT$ s" code" s" E-MISSING-SOURCE" PACKET {: p:n :}
   s" commented-literal: in the subject" T-LABEL p s" file" STRING$ SUBJ$ T$=
   s" commented-literal: at the loader word" T-LABEL p s" token" STRING$ s" required" T$=
   s" commented-literal: its line" T-LABEL p s" line" NUMBER$ s" 1" T$=
   s" commented-literal: its column" T-LABEL p s" column" NUMBER$ s" 29" T$= ;


\ A require of a file the file system will not read refuses the subject, with
\ a packet at the loader word that names it.
: UNREADABLE-DEPENDENCY ( -- )
   s" locked-dep.f" s\" \\ locked\n" FIXTURE
   s" locked-dep.f" AT$ 0 CHMOD-MODE
   s\" \\ unread\nrequire locked-dep.f\n" s" unread.f" GUARD-MS CHECK-AS
   1 s" unreadable-dependency: refused" EXPECT-KIND
   CHECK:VERIFY-OUT$ s" code" s" E-UNREADABLE-SOURCE" PACKET {: p:n :}
   s" unreadable-dependency: in the subject" T-LABEL p s" file" STRING$ SUBJ$ T$=
   s" unreadable-dependency: at the loader word" T-LABEL p s" token" STRING$ s" require" T$=
   s" unreadable-dependency: its line" T-LABEL p s" line" NUMBER$ s" 2" T$= ;


\ A literal path within the 1024 bytes `require` takes that resolves past them
\ refuses the subject, with a packet at the loader word.
: LONG-RESOLVED ( -- )
   0 GEN-U !
   s" require " GEN+
   496 0 ?do s" a/" GEN+ loop
   s\" absent.f\n" GEN+
   0 GEN GEN-U @ s" long.f" GUARD-MS CHECK-AS
   1 s" long-resolved: refused" EXPECT-KIND
   CHECK:VERIFY-OUT$ s" code" s" E-LOADER-FORM" PACKET {: p:n :}
   s" long-resolved: in the subject" T-LABEL p s" file" STRING$ SUBJ$ T$=
   s" long-resolved: at the loader word" T-LABEL p s" token" STRING$ s" require" T$=
   s" long-resolved: its line" T-LABEL p s" line" NUMBER$ s" 1" T$= ;


\ A closure wider than a fixed table held: the subject and WIDE-N files it
\ requires.
130 constant WIDE-N

: WIDE-NAME ( n -- ptr u8 n )
   SB-RESET s" cvt-w" SB-APPEND FMT:SB-INT s" .f" SB-APPEND SB$ ;

: WIDE-CLOSURE ( -- )
   0 GEN-U !
   WIDE-N 0 ?do
      i WIDE-NAME s\" \\ one of many\n" FIXTURE
      s" require " GEN+ i WIDE-NAME GEN+ s\" \n" GEN+
   loop
   s\" : CVT-WIDE ( -- n ) 1 ;\n" GEN+
   0 GEN GEN-U @ s" wide.f" GUARD-MS CHECK-AS
   0 s" wide-closure: verified" EXPECT-KIND ;


\ More packets than OLD-CAPTURE held: every one, each complete, the last
\ definition's among them.
: WHOLE-OUTPUT ( -- )
   BIG$SRC s" big.f" GUARD-MS CHECK-AS 1 s" whole-output: refused" EXPECT-KIND
   CHECK:VERIFY-OUT$ {: out:ptr outu:n :}
   s" whole-output: past the old capture" T-LABEL outu OLD-CAPTURE > TTRUE
   s" whole-output: each complete" T-LABEL out outu ALL-JSON? TTRUE
   s" whole-output: every packet" T-LABEL out outu OBJECTS BIG-DEFS T=
   s" whole-output: the last" T-LABEL
   out outu s" word" s" cvt-b9999" PACKET 0 >= TTRUE ;


\ ---- loader calls ------------------------------------------------------------

\ A loader call in a body loads when the body runs, and the verifier runs
\ none: the file is not read, so one that is not there, one that defines a word
\ again or one behind a target test refuses nothing, and the words it defines
\ are not there for the definitions after the body. The ordinary checker
\ judges a dynamic argument and rejects an invalid stack effect.
: BODY-UNCALLED ( -- )
   s\" : CVT-BODY-MISSING ( -- ) s\" nosuch.f\" included ;\n" s" body-missing.f" VERIFIED
   s" dupdep.f" s\" : CVT-DUPW ( -- n ) 1 ;\n" FIXTURE
   s\" : CVT-DUPW ( -- n ) 2 ;\n: CVT-LOADS ( -- ) s\" dupdep.f\" included ;\n"
   s" body-dup.f" VERIFIED
   s\" : CVT-GUARDED ( -- ) HB-TARGET-MACOS? if s\" nosuch.f\" included then ;\n"
   s" body-guarded.f" VERIFIED
   s\" : CVT-LOADS7 ( -- ) s\" dep.f\" required ;\n: CVT-USE7 ( -- n ) CVT-SEVEN ;\n"
   s" body-later.f" GUARD-MS CHECK-AS 1 s" body-uncalled: its words are not there" EXPECT-KIND
   s" body-uncalled: at the use" s" E-UNDEFINED" s" CVT-SEVEN" s" 2" s" 21" AT-TOKEN
   s\" : CVT-INC ( -- ) s\" files-inc.f\" included ;\ns\" dep.f\" required\n: CVT-USE-INC ( -- n ) CVT-SEVEN ;\n"
   s" body-files.f" VERIFIED
   s" body-uncalled: the body's file is not read" T-LABEL
   CHECK:VERIFY-FILES$ s" file" s" files-inc.f" AT$ PACKET 0 < TTRUE
   s" body-uncalled: the top level's is" T-LABEL
   CHECK:VERIFY-FILES$ s" file" s" dep.f" AT$ PACKET 0 >= TTRUE
   s\" : CVT-ARGV-LOAD ( -- ) 0 SCRIPT-ARGV$ included ;\n" s" body-argv.f" VERIFIED
   s\" : CVT-ENTRY ( ptr u8 n -- ) {: entry:ptr eu:n :} entry eu included ;\n"
   s" body-entry.f" VERIFIED
   s\" : CVT-DYN ( ptr u8 n -- ) included ;\n" s" body-dyn.f" VERIFIED
   s\" : CVT-DYN-REQ ( ptr u8 n -- ) required ;\n" s" body-dyn-req.f" VERIFIED
   s\" : CVT-DYN-PROV ( ptr u8 n -- ) provided ;\n" s" body-dyn-prov.f" VERIFIED
   s\" : CVT-BAD ( -- ) 0 included ;\n" s" body-bad.f" GUARD-MS CHECK-AS
   1 s" body-uncalled: invalid effect refused" EXPECT-KIND
   s" body-uncalled: effect at included" s" E-INPUT-UNDERFLOW" s" included" s" 1" s" 20" AT-TOKEN
   s\" : CVT-OPENER ( -- ) c\" x.f\" included ;\n" s" body-opener.f" GUARD-MS CHECK-AS
   1 s" body-uncalled: counted string effect refused" EXPECT-KIND
   s" body-uncalled: effect at counted string" s" E-MISMATCH" s" included" s" 1" s" 29" AT-TOKEN
   0 GEN-U !
   s\" : CVT-LONG ( -- ) s\" " GEN+
   1025 0 ?do s" a" GEN+ loop
   s\" \" included ;\n" GEN+
   0 GEN GEN-U @ s" body-long.f" VERIFIED ;


\ include and require in a body are immediate: the engine refuses the body
\ (E-UNMODELED-IMMEDIATE) and loads nothing, but the verifier reads their file
\ there, a superset. One that is not there is refused at the loader word; one
\ that is defines its words for the definitions after the body, which the
\ checker refuses for the immediate.
: BODY-IMMEDIATE ( -- )
   s\" : CVT-IMM-MISSING ( -- n ) include nosuch.f 1 ;\n" s" imm-missing.f" GUARD-MS CHECK-AS
   1 s" body-immediate: a missing file" EXPECT-KIND
   s" body-immediate: at the loader word" s" E-MISSING-SOURCE" s" include" s" 1" s" 28" AT-TOKEN
   s" body-immediate: its status line" s" nosuch.f" MISSING-STATUS
   s" imm.f" s\" : CVT-IMM ( -- n ) 5 ;\n" FIXTURE
   s\" : CVT-IMM-LOADS ( -- n ) include imm.f 1 ;\n: CVT-IMM-USE ( -- n ) CVT-IMM ;\n"
   s" imm-loads.f" GUARD-MS CHECK-AS 1 s" body-immediate: refused for the immediate" EXPECT-KIND
   s" body-immediate: the file's word is there" T-LABEL
   CHECK:VERIFY-OUT$ s" code" s" E-UNDEFINED" PACKET 0 < TTRUE ;


\ A loader word's name is a name like any other. A definition, storage, a
\ deferred word or an export that takes one verifies, as each loads, in a
\ package, whose wordlist does not hold the global loader; so do `undefine` and
\ UNDEFINE-IF-DEFINED of one. At top level the global wordlist holds it
\ already: the load refuses a second definition as the duplicate, exit 78, and
\ so does the check, at the name. A package's name, or a using's, names no word.
: RESERVED-NAMES ( -- )
   s\" package CVT-RES\nprivate\nvariable REQUIRED\n;package\n" s" res-variable.f" VERIFIED
   s\" package CVT-RES\ncreate INCLUDED\n;package\n" s" res-create.f" VERIFIED
   s\" package CVT-RES\nTYPED-VARIABLE PROVIDED n\n;package\n" s" res-typed.f" VERIFIED
   s\" package CVT-RES\n1 constant INCLUDE\n;package\n" s" res-constant.f" VERIFIED
   s\" package CVT-RES\ndefer included ( -- )\n;package\n" s" res-defer.f" VERIFIED
   s\" package CVT-EXP\npublic\nEXPORT required\n;package\n" s" res-export.f" VERIFIED
   s\" package CVT-RES\n: include ( -- ) ;\n;package\n" s" res-colon.f" VERIFIED
   s\" package CVT-RES\nTRUSTED: require ( -- ) ;\n;package\n" s" res-trusted.f" VERIFIED
   s\" undefine required\n" s" res-undefine.f" VERIFIED
   s\" s\" required\" UNDEFINE-IF-DEFINED\n" s" res-retire.f" VERIFIED
   s\" : include ( -- ) ;\n" s" res-global.f" GUARD-MS CHECK-AS
   1 s" res-global.f" EXPECT-KIND
   s" res-global.f" s" E-DUPLICATE-DEFINITION" s" include" s" 1" s" 3" AT-TOKEN
   s\" package REQUIRED\npublic\n: CVT-RQ ( -- n ) 1 ;\n;package\nusing REQUIRED\n: CVT-RQ-USE ( -- n ) CVT-RQ ;\n;using\n"
   s" res-package.f" VERIFIED ;


\ Past a definition or retirement of a loader word's name, the walk cannot tell
\ which word the spelling names, so a loader form it reads with that word, at
\ top level or in a body, is refused at the word: E-DISC-SHADOW when the file
\ defined the name, before E-DISC-RETIRE when it retired it. A top-level
\ UNDEFINE-IF-DEFINED with no path literal before it may retire any loader word.
\ A use the walk reads past, a call in a body, is no loader form; another loader
\ word, a package, using or export name, and a retirement in a body that only
\ its run would make, replace nothing.
: LOADER-USE ( -- )
   s\" undefine require\n: require ( -- ) parse-name required ;\nrequire ld-inc.f\n"
   s" use-colon.f" s" require" s" 3" s" 1" SHADOWED$ LOADER-REFUSED
   s\" package CVT-SH\nkernel: include ( -- ) ;\n;package\ninclude ld-inc.f\n"
   s" use-kernel.f" s" include" s" 4" s" 1" SHADOWED$ LOADER-REFUSED
   s\" package CVT-SH\n: required ( ptr u8 n -- ) 2drop ;\n;package\n: L ( -- ) s\" ld-inc.f\" required ;\n"
   s" use-body.f" s" required" s" 4" s" 25" SHADOWED$ LOADER-REFUSED
   s\" package CVT-SH\n: include ( -- ) ;\n;package\n: L ( -- ) include ld-inc.f ;\n"
   s" use-body-include.f" s" include" s" 4" s" 12" SHADOWED$ LOADER-REFUSED
   s\" package CVT-RES\nprivate\nvariable REQUIRED\n;package\ns\" ld-inc.f\" required\n"
   s" use-variable.f" s" required" s" 5" s" 14" SHADOWED$ LOADER-REFUSED
   s\" undefine required\ns\" ld-inc.f\" required\n"
   s" use-undefine.f" s" required" s" 2" s" 14" RETIRED$ LOADER-REFUSED
   s\" s\" required\" UNDEFINE-IF-DEFINED\n: L ( -- ) s\" ld-inc.f\" required ;\n"
   s" use-retire-body.f" s" required" s" 2" s" 25" RETIRED$ LOADER-REFUSED
   s\" s\" CVT-NONE\" 2dup 2drop UNDEFINE-IF-DEFINED\nrequire ld-inc.f\n"
   s" use-retire-any.f" s" require" s" 2" s" 1" RETIRED$ LOADER-REFUSED
   s\" package CVT-SH\nprivate\nvariable REQUIRED\n: L ( -- n ) REQUIRED @ ;\n;package\nrequire ld-inc.f\n"
   s" use-read-past.f" VERIFIED
   s\" package REQUIRED\n;package\nusing REQUIRED\n;using\npackage CVT-EXP\npublic\nEXPORT required\n;package\ns\" ld-inc.f\" required\n"
   s" use-package.f" VERIFIED
   s\" : R ( -- ) s\" required\" UNDEFINE-IF-DEFINED ;\n: U ( ptr u8 n -- ) undefine required ;\ns\" ld-inc.f\" required\n"
   s" use-body-retire.f" VERIFIED ;


\ A library storage definer reads a name only as the word it is. A package's
\ word of its spelling, in the file or in one it requires, and a global word
\ an `undefine` of it made room for, are calls, and the require after each
\ loads its file: every source here loads. Read as the library's definer, the
\ require is the name and the path an undefined word.
: STORAGE-BINDING ( -- )
   s\" package CVT-BIND\n: LAYOUT-BUFFER ( -- ) ;\n: DEFER-LAYOUT-BUFFER ( -- ) ;\n: TYPED-BUFFER ( -- ) ;\n: TYPED-VARIABLE ( -- ) ;\n: DYNAMIC-BUFFER ( -- ) ;\n: PTR-VARIABLE ( -- ) ;\n: PERSISTED-PTR-VARIABLE ( -- ) ;\n: PTR-U8-TABLE ( -- ) ;\n: PERSISTED-PTR-U8-TABLE-VARIABLE ( -- ) ;\n: RESERVED-PTR-U8-CELL ( -- ) ;\nLAYOUT-BUFFER require ld-inc.f\nDEFER-LAYOUT-BUFFER require ld-inc.f\nTYPED-BUFFER require ld-inc.f\nTYPED-VARIABLE require ld-inc.f\nDYNAMIC-BUFFER require ld-inc.f\nPTR-VARIABLE require ld-inc.f\nPERSISTED-PTR-VARIABLE require ld-inc.f\nPTR-U8-TABLE require ld-inc.f\nPERSISTED-PTR-U8-TABLE-VARIABLE require ld-inc.f\nRESERVED-PTR-U8-CELL require ld-inc.f\n;package\n"
   s" bind-package.f" VERIFIED
   s" bind-cx-x.f" s\" undefine PTR-VARIABLE\n: PTR-VARIABLE ( -- ) ;\n" FIXTURE
   s\" require bind-cx-x.f\nPTR-VARIABLE require ld-inc.f\n" s" bind-cx.f" VERIFIED
   s" bind-cp-x.f" s\" package CVT-BIND\n: PTR-VARIABLE ( -- ) ;\n;package\n" FIXTURE
   s\" require bind-cp-x.f\npackage CVT-BIND\nPTR-VARIABLE require ld-inc.f\n;package\n" s" bind-cp.f" VERIFIED
   s" bind-cu-x.f" s\" undefine PTR-VARIABLE\npackage CVT-BIND\npublic\n: PTR-VARIABLE ( -- ) ;\n;package\n" FIXTURE
   s\" require bind-cu-x.f\nusing CVT-BIND\nPTR-VARIABLE require ld-inc.f\n;using\n" s" bind-cu.f" VERIFIED ;


\ A loader form's fault is the verifier's, met where its walk reaches the form:
\ the packets it made before are kept, the fault's is the last, every line is
\ a packet, and the stop is the child's, its code at the loader word.
: FAULT-AFTER-PACKET ( -- )
   s\" : CVT-FAP-BAD ( -- n n ) 8 ;\nrequire nosuch.f\n" s" fap-missing.f" GUARD-MS CHECK-AS
   1 s" fault-after-packet: refused" EXPECT-KIND
   s" fault-after-packet: the packet made first" T-LABEL
   CHECK:VERIFY-OUT$ s" word" s" cvt-fap-bad" PACKET 0 >= TTRUE
   s" fault-after-packet: the fault" s" E-MISSING-SOURCE" s" require" s" 2" s" 1" AT-TOKEN
   s" fault-after-packet: the fault last" T-LABEL LAST-LINE s" E-MISSING-SOURCE" CONTAINS? TTRUE
   s" fault-after-packet: only packets" T-LABEL CHECK:VERIFY-OUT$ ALL-JSON? TTRUE
   s" fault-after-packet: its status line" s" nosuch.f" MISSING-STATUS
   s\" : CVT-FAP ( -- n n ) 8 ;\n: CVT-FAP-PATH ( -- ptr u8 n ) s\" x.f\" ;\nCVT-FAP-PATH included\n"
   s" fap-dynamic.f" GUARD-MS CHECK-AS 1 s" fault-after-packet: a dynamic path" EXPECT-KIND
   s" fault-after-packet: its packet made first" T-LABEL
   CHECK:VERIFY-OUT$ s" word" s" cvt-fap" PACKET 0 >= TTRUE
   s" fault-after-packet: the form" s" E-LOADER-FORM" s" included" s" 3" s" 14" AT-TOKEN
   s" fault-after-packet: the child's code" T-LABEL CHECK:VERIFY-STOP E-DISC-DYNAMIC T=
   s" fault-after-packet: in the subject" T-LABEL CHECK:VERIFY-STOP-SUBJECT? TTRUE
   \ 79 is the byte of `included`.
   s" fault-after-packet: at the loader word" T-LABEL CHECK:VERIFY-STOP-AT 79 T=
   s" fault-after-packet: the status line" DYNAMIC$ SUBJ-STATUS ;


\ provided records a path as loaded and reads no file: one that is not there
\ refuses nothing. A path no literal gives, or one no loader word takes, is
\ refused.
: PROVIDED-META ( -- )
   s\" s\" nosuch.f\" provided\n" s" prov-missing.f" VERIFIED
   s\" : CVT-PROV$ ( -- ptr u8 n ) s\" x.f\" ;\nCVT-PROV$ provided\n" s" prov-dynamic.f"
   s" provided" s" 2" s" 11" DYNAMIC$ LOADER-REFUSED
   s\" s\" \" provided\n" s" prov-empty.f" s" provided" s" 1" s" 6" CAPACITY$ LOADER-REFUSED ;


\ script-required, which resolves its path from the working directory, reads
\ its file as the other loaders do: a path no literal gives is refused at the
\ loader word, and so is a file that is not there.
: SCRIPT-LOADER ( -- )
   s\" : CVT-SR$ ( -- ptr u8 n ) s\" dep.f\" ;\nCVT-SR$ script-required\n" s" script-dynamic.f"
   s" script-required" s" 2" s" 9" DYNAMIC$ LOADER-REFUSED
   s\" s\" cvt-nosuch-sr.f\" script-required\n" s" script-missing.f" GUARD-MS CHECK-AS
   1 s" script-loader: a missing file" EXPECT-KIND
   s" script-loader: at the loader word" s" E-MISSING-SOURCE" s" script-required" s" 1" s" 21"
   AT-TOKEN ;


\ A body literal whose escape the checker refuses gives no path: the loader
\ word after it is refused for nothing more than the literal.
: BAD-ESCAPE ( -- )
   s\" : CVT-ESC ( -- ) s\\\" a\\k\" included ;\n" s" bad-escape.f" GUARD-MS CHECK-AS
   1 s" bad-escape: refused" EXPECT-KIND
   s" bad-escape: no loader form" T-LABEL
   CHECK:VERIFY-OUT$ s" code" s" E-LOADER-FORM" PACKET 0 < TTRUE
   s" bad-escape: no missing source" T-LABEL
   CHECK:VERIFY-OUT$ s" code" s" E-MISSING-SOURCE" PACKET 0 < TTRUE ;


\ ---- the command line ------------------------------------------------------

\ The stderr length and exit status of HOST given IN on stdin.
: CLI-HOST ( ptr u8 n ptr u8 n -- n n ) {: in:ptr inu:n host:ptr hostu:n :}
   host hostu >LEN in inu >LEN 0 OUT CAP >LEN 0 ERR CAP >LEN GUARD-MS >MS
   RUN-ARGV-ENV-STDIN-CAPTURE
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: o:len e:len :} o LEN>N CLI-OUT-U ! e LEN>N 0 ENDOF
      err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :} o LEN>N CLI-OUT-U ! e LEN>N c RC>N ENDOF
   ;MATCH ;

: CLI ( ptr u8 n -- n n )
   ENGINE-CANDIDATE:PATH$ CLI-HOST ;

: COLD-CLI ( ptr u8 n -- n n )
   COLD-ENGINE:PATH$ CLI-HOST ;


: CLI-START ( -- )
   PROC-ARGV-ENV-RESET
   s" --load" >LEN PROC-ARGV+
   s" tools/check.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   PROC-ENV-INHERIT-MISSING ;


: ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;


: CLI-FILE ( -- )
   CLI-START s" --verify-only" ARG+ s" dep.f" AT$ ARG+
   s" " CLI {: erru:n rc:n :}
   s" cli-file: verified" T-LABEL rc 0 T=
   s" cli-file: nothing on stderr" T-LABEL erru 0 T=
   s" cli-file: no definition line" T-LABEL
   0 OUT CLI-OUT-U @ s\" \"visibility\"" CONTAINS? TFALSE
   s" pos.f" s\" \n\n\n: CVT-POS ( n -- n ) drop ;\n" FIXTURE
   CLI-START s" --verify-only" ARG+ s" pos.f" AT$ ARG+
   s" " CLI {: erru2:n rc2:n :}
   s" cli-file: refused" T-LABEL rc2 70 T=
   s" cli-file: only packets on stderr" T-LABEL 0 ERR erru2 ALL-JSON? TTRUE
   0 ERR erru2 s" code" s" E-MISMATCH" PACKET {: p:n :}
   s" cli-file: named as the operation names it" T-LABEL p s" file" STRING$ s" pos.f" AT$ T$=
   s" pos.f" POS$DISK FIXTURE ;


\ stdin's closure resolves against --stdin-path's directory; the path need not exist.
: CLI-STDIN ( -- )
   CLI-START s" --verify-only" ARG+ s" --stdin-path" ARG+ s" new.f" AT$ ARG+
   s\" require dep.f\n: CVT-NEW ( -- n ) CVT-SEVEN ;\n" CLI {: erru:n rc:n :}
   s" cli-stdin: the closure is followed" T-LABEL rc 0 T=
   s" cli-stdin: nothing on stderr" T-LABEL erru 0 T=
   CLI-START s" --json-errors" ARG+ s" --verify-only" ARG+ s" --stdin-path" ARG+ s" new.f" AT$ ARG+
   s\" require dep.f\n: CVT-NEW ( -- n ) CVT-SEVEN drop ;\n" CLI {: erru2:n rc2:n :}
   s" cli-stdin: refused" T-LABEL rc2 70 T=
   s" cli-stdin: only packets on stderr" T-LABEL 0 ERR erru2 ALL-JSON? TTRUE
   0 ERR erru2 s" code" s" E-MISMATCH" PACKET {: p:n :}
   s" cli-stdin: named by the path" T-LABEL p s" file" STRING$ s" new.f" AT$ T$= ;


: CLI-USAGE ( -- )
   CLI-START s" --stdin-path" ARG+ s" new.f" AT$ ARG+
   s" : CVT-X ( -- ) ;" CLI {: erru:n rc:n :}
   s" cli-usage: --stdin-path without --verify-only" T-LABEL rc 64 T=
   s" cli-usage: default mode explains on stderr" T-LABEL
   0 ERR erru s" usage: tools/check.f" CONTAINS? TTRUE
   CLI-START s" --verify-only" ARG+
   s" : CVT-X ( -- ) ;" CLI {: erru2:n rc2:n :}
   s" cli-usage: stdin without a path" T-LABEL rc2 64 T=
   s" cli-usage: verify mode keeps stderr empty" T-LABEL erru2 0 T=
   s" cli-usage: verify mode explains on stdout" T-LABEL
   0 OUT CLI-OUT-U @ s" usage: tools/check.f" CONTAINS? TTRUE
   CLI-START s" --verify-only" ARG+ s" --source-list" ARG+ s" dep.f" AT$ ARG+
   s" " CLI {: erru3:n rc3:n :}
   s" cli-usage: a source list" T-LABEL rc3 64 T=
   s" cli-usage: source list keeps stderr empty" T-LABEL erru3 0 T=
   CLI-START s" --verify-only" ARG+ s" --stdin-path" ARG+
   s" " CLI {: erru4:n rc4:n :}
   s" cli-usage: missing option argument" T-LABEL rc4 64 T=
   s" cli-usage: missing argument keeps stderr empty" T-LABEL erru4 0 T=
   CLI-START s" --stdin-path" ARG+ s" new.f" AT$ ARG+ s" --verify-only" ARG+
   s" : CVT-X ( -- ) ;" CLI {: erru5:n rc5:n :}
   s" cli-usage: later verify option is honored" T-LABEL rc5 0 T=
   s" cli-usage: later verify option keeps stderr empty" T-LABEL erru5 0 T=
   CLI-START s" --stdin-path" ARG+ s" --verify-only" ARG+
   s" : CVT-X ( -- ) ;" CLI {: erru6:n rc6:n :}
   s" cli-usage: option-shaped path is a value" T-LABEL rc6 64 T=
   s" cli-usage: option-shaped value leaves default stderr" T-LABEL
   0 ERR erru6 s" usage: tools/check.f" CONTAINS? TTRUE
   CLI-START s" --" ARG+ s" --verify-only" ARG+
   s" " CLI {: erru7:n rc7:n :}
   s" cli-usage: option after separator is a file" T-LABEL rc7 66 T=
   s" cli-usage: separator leaves default stderr" T-LABEL
   0 ERR erru7 s" check.f: no such source" CONTAINS? TTRUE
   CLI-START s" --unknown" ARG+ s" --verify-only" ARG+
   s" " CLI {: erru8:n rc8:n :}
   s" cli-usage: earlier bad option is usage" T-LABEL rc8 64 T=
   s" cli-usage: later verify option routes early failure" T-LABEL erru8 0 T=
   s" cli-usage: earlier bad option explained" T-LABEL
   0 OUT CLI-OUT-U @ s" usage: tools/check.f" CONTAINS? TTRUE ;


: CLI-EARLY-FAILS ( -- )
   CLI-START s" --verify-only" ARG+ s" cvt-missing.f" AT$ ARG+
   s" " CLI {: erru:n rc:n :}
   s" cli-early-fails: missing file rc" T-LABEL rc 66 T=
   s" cli-early-fails: missing file stderr empty" T-LABEL erru 0 T=
   s" cli-early-fails: missing file explanation" T-LABEL
   0 OUT CLI-OUT-U @ s" check.f: no such source" CONTAINS? TTRUE ;

\ The verifier reads standard input whole, however large: the definition that
\ ends a source of $180000 bytes, past a megabyte, is checked there.
$180000 constant LARGE-STDIN-LEN

: LARGE-STDIN-DEF$ ( -- ptr u8 n )
   s" : CVT-BIG ( -- n ) ;" ;

: CLI-LARGE-STDIN ( -- )
   LARGE-STDIN-LEN GEN-RESERVE
   LARGE-STDIN-DEF$ {: d:ptr du:n :}
   LARGE-STDIN-LEN du - {: fill:n :}
   fill 0 ?do $0a i GEN c! loop
   d fill GEN du BYTE-COPY
   CLI-START s" --json-errors" ARG+ s" --verify-only" ARG+
   s" --stdin-path" ARG+ s" new.f" AT$ ARG+
   0 GEN LARGE-STDIN-LEN CLI {: erru:n rc:n :}
   s" cli-large-stdin: refused at its end" T-LABEL rc 70 T=
   s" cli-large-stdin: only packets on stderr" T-LABEL 0 ERR erru ALL-JSON? TTRUE
   0 ERR erru s" code" s" E-MISMATCH" PACKET {: p:n :}
   s" cli-large-stdin: named by the path" T-LABEL
   p s" file" STRING$ s" new.f" AT$ T$= ;


: LONG-PATH$ ( -- ptr u8 n )
   FS-PATH-CAP 1+ GEN-RESERVE
   FS-PATH-CAP 1+ 0 do 97 i GEN c! loop
   0 GEN FS-PATH-CAP 1+ ;

\ A path argument longer than a path slot is a usage error, 64: apart from a
\ refusal (70) and from an uncaught throw (67).
: CLI-PATH-CAPACITY ( -- )
   LONG-PATH$ {: path:ptr pathu:n :}
   CLI-START s" --verify-only" ARG+ path pathu ARG+
   s" " CLI {: erru:n rc:n :}
   s" cli-path-capacity: verify FILE status" T-LABEL rc 64 T=
   s" cli-path-capacity: verify FILE stderr" T-LABEL erru 0 T=
   s" cli-path-capacity: verify FILE explanation" T-LABEL
   0 OUT CLI-OUT-U @ s" check.f: source path exceeds capacity" CONTAINS? TTRUE
   CLI-START path pathu ARG+
   s" " CLI {: erru2:n rc2:n :}
   s" cli-path-capacity: ordinary FILE status" T-LABEL rc2 64 T=
   s" cli-path-capacity: ordinary FILE explanation" T-LABEL
   0 ERR erru2 s" check.f: source path exceeds capacity" CONTAINS? TTRUE
   CLI-START s" --verify-only" ARG+ s" --stdin-path" ARG+ path pathu ARG+
   s" : CVT-X ( -- ) ;" CLI {: erru3:n rc3:n :}
   s" cli-path-capacity: verify stdin path status" T-LABEL rc3 64 T=
   s" cli-path-capacity: verify stdin path stderr" T-LABEL erru3 0 T=
   s" cli-path-capacity: verify stdin path explanation" T-LABEL
   0 OUT CLI-OUT-U @ s" check.f: source path exceeds capacity" CONTAINS? TTRUE
   CLI-START s" --stdin-path" ARG+ path pathu ARG+
   s" : CVT-X ( -- ) ;" CLI {: erru4:n rc4:n :}
   s" cli-path-capacity: ordinary stdin path status" T-LABEL rc4 64 T=
   s" cli-path-capacity: ordinary stdin path explanation" T-LABEL
   0 ERR erru4 s" check.f: source path exceeds capacity" CONTAINS? TTRUE ;


: CLI-WHOLE-OUTPUT ( -- )
   CLI-START s" --verify-only" ARG+ s" big.f" AT$ ARG+
   s" " CLI {: erru:n rc:n :}
   s" cli-whole-output: refused" T-LABEL rc 70 T=
   s" cli-whole-output: only packets on stderr" T-LABEL 0 ERR erru ALL-JSON? TTRUE
   s" cli-whole-output: every packet on stderr" T-LABEL 0 ERR erru OBJECTS BIG-DEFS T= ;


\ --deadline-ms reaches the verifier's child, and the line names the deadline.
: CLI-DEADLINE ( -- )
   CLI-START s" --deadline-ms" ARG+ s" 1" ARG+ s" --verify-only" ARG+ s" dep.f" AT$ ARG+
   s" " CLI {: erru:n rc:n :}
   s" cli-deadline: unavailable" T-LABEL rc 69 T=
   s" cli-deadline: nothing on stderr" T-LABEL erru 0 T=
   s" cli-deadline: the line names the deadline" T-LABEL
   0 OUT CLI-OUT-U @ s" check.f: the verifier did not complete: deadline of 1 ms passed" CONTAINS? TTRUE ;

\ The pre-pass runs in the same child, under the same deadline, and a check
\ writes its line on stderr.
: CLI-DEADLINE-PREPASS ( -- )
   CLI-START s" --deadline-ms" ARG+ s" 1" ARG+ s" dep.f" AT$ ARG+
   s" " CLI {: erru:n rc:n :}
   s" cli-deadline-prepass: unavailable" T-LABEL rc 69 T=
   s" cli-deadline-prepass: nothing on stdout" T-LABEL CLI-OUT-U @ 0 T=
   s" cli-deadline-prepass: the line names the deadline" T-LABEL
   0 ERR erru s" check.f: the verifier did not complete: deadline of 1 ms passed" CONTAINS? TTRUE ;


\ These cases compare the real loader with both CHECK entry points. An ordered
\ preload lets EARLY see a later file, loses the using inherited by USE, and
\ verifies an included file only once. None has a top-level runtime action.
: NATIVE-RC ( ptr u8 n -- n )
   PROC-ARGV-ENV-RESET
   s" --load" ARG+
   ARG+
   PROC-ENV-INHERIT-MISSING
   s" " CLI nip ;

: CHECK-RC ( ptr u8 n bool bool -- n )
   {: path:ptr pathu:n verify:bool all:bool :}
   CLI-START
   all if s" --all-errors" ARG+ then
   verify if s" --verify-only" ARG+ then
   path pathu ARG+
   s" " CLI nip ;

: LOAD-ORDER ( -- )
   s" order-late.f" AT$ {: path:ptr pathu:n :}
   s" load-order: native refuses early reference" T-LABEL path pathu NATIVE-RC 70 T=
   s" load-order: verify-only refuses early reference" T-LABEL path pathu true false CHECK-RC 70 T=
   s" load-order: ordinary refuses early reference" T-LABEL path pathu false false CHECK-RC 70 T=
   s" load-order: all-errors refuses early reference" T-LABEL path pathu false true CHECK-RC 70 T= ;

: LOAD-CONTEXT ( -- )
   s" order-context.f" AT$ {: path:ptr pathu:n :}
   s" load-context: native inherits using" T-LABEL path pathu NATIVE-RC 0 T=
   s" load-context: verify-only inherits using" T-LABEL path pathu true false CHECK-RC 0 T=
   s" load-context: ordinary inherits using" T-LABEL path pathu false false CHECK-RC 0 T=
   s" load-context: all-errors inherits using" T-LABEL path pathu false true CHECK-RC 0 T= ;

: LOAD-PACKAGE ( -- )
   s" order-package.f" AT$ {: path:ptr pathu:n :}
   s" load-package: native inherits package" T-LABEL path pathu NATIVE-RC 0 T=
   s" load-package: verify-only inherits package" T-LABEL path pathu true false CHECK-RC 0 T=
   s" load-package: ordinary inherits package" T-LABEL path pathu false false CHECK-RC 0 T=
   s" load-package: all-errors inherits package" T-LABEL path pathu false true CHECK-RC 0 T= ;

: LOAD-USING-FLOOR ( -- )
   s" order-floor-include.f" AT$ {: path:ptr pathu:n :}
   s" load-using-floor: native include refuses stale import" T-LABEL path pathu NATIVE-RC 70 T=
   s" load-using-floor: verify-only include refuses stale import" T-LABEL path pathu true false CHECK-RC 70 T=
   s" load-using-floor: ordinary include refuses stale import" T-LABEL path pathu false false CHECK-RC 70 T=
   s" load-using-floor: all-errors include refuses stale import" T-LABEL path pathu false true CHECK-RC 70 T=
   s" order-floor-require.f" AT$ {: req:ptr requ:n :}
   s" load-using-floor: native require refuses stale import" T-LABEL req requ NATIVE-RC 70 T=
   s" load-using-floor: verify-only require refuses stale import" T-LABEL req requ true false CHECK-RC 70 T=
   s" load-using-floor: ordinary require refuses stale import" T-LABEL req requ false false CHECK-RC 70 T=
   s" load-using-floor: all-errors require refuses stale import" T-LABEL req requ false true CHECK-RC 70 T= ;

: LOAD-REPEAT ( -- )
   s" order-double.f" AT$ {: path:ptr pathu:n :}
   s" load-repeat: native sees second include" T-LABEL path pathu NATIVE-RC 78 T=
   s" load-repeat: verify-only sees second include" T-LABEL path pathu true false CHECK-RC 70 T=
   s" load-repeat: ordinary sees second include" T-LABEL path pathu false false CHECK-RC 78 T=
   s" load-repeat: all-errors sees second include" T-LABEL path pathu false true CHECK-RC 78 T= ;


\ The exit status of check.f, given FLAG unless it is empty, on the tree's
\ tools/source-discovery.f, run from the fixture directory, outside the tree,
\ with check.f and the subject named by absolute paths.
: OUTSIDE-RC ( ptr u8 n -- n ) {: flag:ptr flagu:n :}
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --load" ARG+
   s" tools/check.f" SOURCE-ROOT:CANONICAL drop ARG+
   s" --" ARG+
   flagu 0 > if flag flagu ARG+ then
   s" tools/source-discovery.f" SOURCE-ROOT:CANONICAL drop ARG+
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ >LEN ROOT$ >LEN 0 OUT CAP >LEN 0 ERR CAP >LEN GUARD-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE 2drop 0 ENDOF
      err OF PCAP-FAILED:UNMAKE RC>N nip nip ENDOF
   ;MATCH ;

\ tools/source-discovery.f loads by computed paths, every one in a body
\ (SD-CALL-ACT): calls no check makes, so the file verifies from the tree root
\ and from outside the tree, in the operation and in each mode of check.f.
: BODY-COMPUTED ( -- )
   s" tools/source-discovery.f" TREE-BYTES s" tools/source-discovery.f" TREE$
   [: drop ;] GUARD-MS >MS CHECK:VERIFY-BYTES
   0 s" body-computed: the operation" EXPECT-KIND
   SUBJ$ {: path:ptr pathu:n :}
   s" body-computed: verify-only from the root" T-LABEL path pathu true false CHECK-RC 0 T=
   s" body-computed: a check from the root" T-LABEL path pathu false false CHECK-RC 0 T=
   s" body-computed: all errors from the root" T-LABEL path pathu false true CHECK-RC 0 T=
   s" body-computed: verify-only from outside" T-LABEL s" --verify-only" OUTSIDE-RC 0 T=
   s" body-computed: a check from outside" T-LABEL s" " OUTSIDE-RC 0 T=
   s" body-computed: all errors from outside" T-LABEL s" --all-errors" OUTSIDE-RC 0 T= ;

: DUPLICATE-AFTER-PACKET ( -- )
   s" order-errors.f"
   s\" package CVT-ORDER-ERR public\n: CVT-ORDER-BAD ( -- n ) drop ;\n;package\nrequire order-dups.f\n" FIXTURE
   s" order-dups.f"
   s\" package CVT-ORDER-DUP public\n: SAME ( -- n ) 1 ;\n: SAME ( -- n ) 2 ;\n;package\n" FIXTURE
   CLI-START s" --all-errors" ARG+ s" --json-errors" ARG+ s" order-errors.f" AT$ ARG+
   s" " CLI {: erru:n rc:n :}
   s" duplicate-after-packet: original exit" T-LABEL rc 78 T=
   s" duplicate-after-packet: JSON packets only" T-LABEL 0 ERR erru ALL-JSON? TTRUE
   0 ERR erru JSONL-START-STRICT
   s" duplicate-after-packet: earlier rejection first" T-LABEL
   JSONL-NEXT-OBJECT s" word" STRING$ s" cvt-order-bad" T$=
   s" duplicate-after-packet: duplicate second" T-LABEL
   JSONL-NEXT-OBJECT s" code" STRING$ s" E-DUPLICATE-DEFINITION" T$=
   s" duplicate-after-packet: both packets retained" T-LABEL JSONL-NEXT-OBJECT -1 T= ;


\ ---- a duplicate ------------------------------------------------------------
\
\ A duplicate is the record --all-errors writes for it, at the name defined
\ again: the second DA, at line 2, column 3, bytes 20-22. The scan goes past it
\ to the definitions after it, as past any refused definition, where
\ --all-errors stops at it as the load does.

\ include order-dep.f twice: the second defines VALUE again, in order-dep.f.
: DUPLICATE-IN-DEPENDENCY ( -- )
   s\" include order-dep.f\ninclude order-dep.f\n" s" double.f" GUARD-MS CHECK-AS
   1 s" duplicate-in-dependency: refused" EXPECT-KIND
   CHECK:VERIFY-OUT$ s" code" s" E-DUPLICATE-DEFINITION" PACKET {: p:n :}
   s" duplicate-in-dependency: names the dependency" T-LABEL
   p s" file" STRING$ s" order-dep.f" AT$ T$=
   s" duplicate-in-dependency: the word" T-LABEL p s" word" STRING$ s" VALUE" T$=
   s" duplicate-in-dependency: its line there" T-LABEL p s" line" NUMBER$ s" 2" T$=
   s" duplicate-in-dependency: its column there" T-LABEL p s" column" NUMBER$ s" 3" T$= ;


\ The definer generates GD-RESERVE, which the line before defined: there is no
\ written name to place, so the record is the placeholder, and the scan stops.
: MADE$SRC ( -- ptr u8 n )
   s\" : GD-RESERVE ( n -- ) drop ;\nDYNAMIC-BUFFER GD u8\n" ;


: DUPLICATE-MADE ( -- )
   MADE$SRC s" made.f" GUARD-MS CHECK-AS
   1 s" duplicate-made: refused" EXPECT-KIND
   CHECK:VERIFY-OUT$ s" code" s" E-DUPLICATE-DEFINITION" PACKET {: p:n :}
   s" duplicate-made: the placeholder" T-LABEL
   p s" word" STRING$ s" duplicate-definition" T$=
   s" duplicate-made: names PATH" T-LABEL p s" file" STRING$ SUBJ$ T$=
   s" duplicate-made: the scan stopped" T-LABEL
   CHECK:VERIFY-LOG$ s" verification stopped by throw 78" CONTAINS? TTRUE ;


\ An open definition after a duplicate stops at its opener, with the duplicate's
\ record still before the result.
: DUPLICATE-THEN-STOPS ( -- )
   s\" : DA ( -- n ) 1 ;\n: DA ( -- n ) 2 ;\n: CVT-OPEN ( -- n ) 1\n" s" dup-open.f" GUARD-MS CHECK-AS {: v :}
   v 1 s" duplicate-then-stops: refused" EXPECT-KIND
   s" duplicate-then-stops: the open definition" T-LABEL CHECK:VERIFY-STOP 7155 T=
   s" duplicate-then-stops: in the subject" T-LABEL CHECK:VERIFY-STOP-SUBJECT? TTRUE
   s" duplicate-then-stops: the subject path" T-LABEL CHECK:VERIFY-STOPPED$ SUBJ$ T$=
   CHECK:VERIFY-OUT$ s" code" s" E-DUPLICATE-DEFINITION" PACKET {: p:n :}
   s" duplicate-then-stops: the duplicate's record" T-LABEL p s" line" NUMBER$ s" 2" T$= ;


\ This executable Habu fixture forwards the real verifier child's complete
\ output, then fails its own process. For unclean-first-dup.f it forwards it
\ only through the first duplicate line, for unclean-two-dups.f through the
\ second, as a child that died there. Its shebang makes it an engine candidate
\ while leaving the verifier and its packet writer on the normal load path.
: UNCLEAN-FIXTURE ( -- )
   0 GEN-U !
   s" #!" GEN+ ENGINE-CANDIDATE:PATH$ GEN+ s\"  --load\n" GEN+
   S\" require lib/process.f\nrequire lib/process-argv.f\nrequire lib/process-env.f\nrequire lib/engine-id.f\nrequire lib/string.f\npackage CVT-UNCLEAN\nDYNAMIC-BUFFER SRC u8\nDYNAMIC-BUFFER OUT u8\nDYNAMIC-BUFFER ERR u8\nvariable SRC-U\n" GEN+
   S\" : ARGS ( -- )\n   PROC-ARGV-ENV-RESET\n   s\" --load\" >LEN PROC-ARGV+\n   s\" tools/check-verify-child.f\" >LEN PROC-ARGV+\n   s\" --\" >LEN PROC-ARGV+\n   SCRIPT-ARGC 0 ?do i SCRIPT-ARGV$ >LEN PROC-ARGV+ loop\n   PROC-ENV-INHERIT-MISSING ;\n" GEN+
   S\" : INPUT ( -- )\n   0 SRC-U !\n   begin SRC-U @ 4096 < while\n      0 SRC-U @ SRC 4096 SRC-U @ - read\n      dup 0< if s\" read failed\" 74 die then\n      dup 0= if drop exit then\n      SRC-U +!\n   repeat ;\n" GEN+
   S\" : LINE-END ( n n -- n )\n   {: at:n u:n :}\n   u at ?do i OUT c@ 10 = if i 1+ unloop exit then loop u ;\n" GEN+
   S\" : DUP-END ( n n -- n )\n   {: u:n :}\n   begin\n      {: at:n :}\n      at u >= if u exit then\n      at u LINE-END\n      {: end:n :}\n      at OUT end at - s\" check-verify: duplicate \" STARTS-WITH? if end exit then\n      end\n   again ;\npublic\n: MAIN ( -- )\n   4096 SRC-RESERVE 65536 OUT-RESERVE 65536 ERR-RESERVE\n   INPUT ARGS\n   ENGINE-ID:PATH$ >LEN 0 SRC SRC-U @ >LEN\n   0 OUT 65536 >LEN 0 ERR 65536 >LEN 60000 >MS\n   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME {: outu:len erru:len o :}\n   o MATCH outcome\n      exited OF 0<> if s\" real verifier failed\" 74 die then ENDOF\n      signaled OF drop s\" real verifier signaled\" 74 die ENDOF\n      timeout OF s\" real verifier timed out\" 74 die ENDOF\n   ;MATCH\n   SCRIPT-ARGC 1- SCRIPT-ARGV$ s\" unclean-first-dup.f\" CONTAINS?\n   if 0 outu LEN>N DUP-END else\n      SCRIPT-ARGC 1- SCRIPT-ARGV$ s\" unclean-two-dups.f\" CONTAINS?\n      if 0 outu LEN>N DUP-END outu LEN>N DUP-END else outu LEN>N then\n   then\n   0 OUT swap type\n   s\" completed child output\" 79 die ;\n;package\nCVT-UNCLEAN:MAIN\n" GEN+
   s" unclean.f" 0 GEN GEN-U @ FIXTURE
   s" unclean.f" AT$ CHMOD-X ;


: UNCLEAN-ANSWER ( -- )
   UNCLEAN-FIXTURE
   s" HABU_UNDER_TEST" >LEN s" unclean.f" AT$ >LEN PROC-ENV-DEFAULT+
   DEP$SRC s" unclean-verified.f" GUARD-MS CHECK-AS {: verified :}
   verified 4 s" unclean-answer: verified frame is incomplete" EXPECT-KIND
   s" unclean-answer: verified process exit" T-LABEL verified STATUS 79 T=
   s" unclean-answer: verified frame is not a packet" T-LABEL CHECK:VERIFY-OUT$ nip 0 T=
   BAD-DEP$SRC s" unclean-refused.f" GUARD-MS CHECK-AS {: refused :}
   refused 4 s" unclean-answer: refused frame is incomplete" EXPECT-KIND
   s" unclean-answer: refused packet remains" T-LABEL
   CHECK:VERIFY-OUT$ s" code" s" E-MISMATCH" PACKET 0 >= TTRUE
   s" unclean-answer: refused frame is not a packet" T-LABEL CHECK:VERIFY-OUT$ ALL-JSON? TTRUE
   s" src/habu/verify-source.f" TREE-BYTES s" src/habu/verify-source.f" TREE$
   [: drop ;] GUARD-MS >MS CHECK:VERIFY-BYTES {: held :}
   held 4 s" unclean-answer: held frame is incomplete" EXPECT-KIND
   s" unclean-answer: held frame is not a packet" T-LABEL CHECK:VERIFY-OUT$ nip 0 T=
   s\" : CVT-OPEN ( -- n ) 1\n\\" s" unclean-open.f" GUARD-MS CHECK-AS {: stopped :}
   stopped 4 s" unclean-answer: stopped frame is incomplete" EXPECT-KIND
   s" unclean-answer: stopped 7155 is not a duplicate" T-LABEL CHECK:VERIFY-OUT$ nip 0 T=
   s\" : DA ( -- n ) 1 ;\n: DA ( -- n ) 2 ;\n: CVT-OPEN ( -- n ) 1\n\\"
   s" unclean-dup-open.f" GUARD-MS CHECK-AS {: dup-open :}
   dup-open 4 s" unclean-answer: duplicate and stop are incomplete" EXPECT-KIND
   CHECK:VERIFY-OUT$ JSONL-START-STRICT
   s" unclean-answer: genuine duplicate before 7155" T-LABEL
   JSONL-NEXT-OBJECT s" code" STRING$ s" E-DUPLICATE-DEFINITION" T$=
   s" unclean-answer: terminal 7155 adds no packet" T-LABEL JSONL-NEXT-OBJECT -1 T=
   MADE$SRC s" unclean-made.f" GUARD-MS CHECK-AS {: dup-stop :}
   dup-stop 4 s" unclean-answer: duplicate stop is incomplete" EXPECT-KIND
   CHECK:VERIFY-OUT$ JSONL-START-STRICT
   s" unclean-answer: genuine duplicate before stop 78" T-LABEL
   JSONL-NEXT-OBJECT s" code" STRING$ s" E-DUPLICATE-DEFINITION" T$=
   s" unclean-answer: terminal 78 adds no packet" T-LABEL JSONL-NEXT-OBJECT -1 T=
   DUP$SRC s" unclean-first-dup.f" GUARD-MS CHECK-AS {: first-dup :}
   first-dup 4 s" unclean-answer: packet before death is incomplete" EXPECT-KIND
   CHECK:VERIFY-OUT$ JSONL-START-STRICT
   s" unclean-answer: sole code-78 packet survives" T-LABEL
   JSONL-NEXT-OBJECT s" code" STRING$ s" E-DUPLICATE-DEFINITION" T$=
   s" unclean-answer: sole code-78 packet is last" T-LABEL JSONL-NEXT-OBJECT -1 T=
   s\" include order-dep.f\ninclude order-dep.f\ninclude order-dep.f\n"
   s" unclean-two-dups.f" GUARD-MS CHECK-AS {: two-dups :}
   two-dups 4 s" unclean-answer: two real duplicates are incomplete" EXPECT-KIND
   CHECK:VERIFY-OUT$ {: packets:ptr packetu:n :}
   packets packetu 10 0 SPLIT-NEXT {: first:ptr firstu:n next:n found:bool :}
   s" unclean-answer: first duplicate has a line" T-LABEL found TTRUE
   s" unclean-answer: duplicate records are byte-identical" T-LABEL
   first firstu packets packetu 10 next SPLIT-NEXT 2drop STR= TTRUE
   CHECK:VERIFY-OUT$ JSONL-START-STRICT
   s" unclean-answer: first identical duplicate survives" T-LABEL
   JSONL-NEXT-OBJECT s" code" STRING$ s" E-DUPLICATE-DEFINITION" T$=
   s" unclean-answer: second identical duplicate survives" T-LABEL
   JSONL-NEXT-OBJECT s" code" STRING$ s" E-DUPLICATE-DEFINITION" T$=
   s" unclean-answer: only two duplicate packets" T-LABEL JSONL-NEXT-OBJECT -1 T=
   s" unclean-pre.f" AT$ SUBJ SUBJ-U COPY!
   DUP$SRC SUBJ$ s" unclean-pre.f" GUARD-MS >MS CHECK:PREVERIFY-BYTES
   MATCH result
      ok OF drop false ENDOF
      err OF MATCH outcome
         exited OF 79 = ENDOF
         signaled OF drop false ENDOF
         timeout OF false ENDOF
      ;MATCH ENDOF
   ;MATCH
   s" unclean-answer: preverify remains incomplete" T-LABEL TTRUE
   s" unclean-answer: preverify stop is framing" T-LABEL CHECK:VERIFY-OUT$ nip 0 T=
   PROC-ENV-DEFAULT-RESET ;


\ --verify-only writes, byte for byte, the record --all-errors writes, and no
\ prose: the scan went past the duplicate.
: CLI-DUPLICATE ( -- )
   CLI-START s" --json-errors" ARG+ s" --all-errors" ARG+ s" dup.f" AT$ ARG+
   s" " CLI {: allu:n allrc:n :}
   s" cli-duplicate: --all-errors exits as the load" T-LABEL allrc 78 T=
   allu 1 max ALL-ERR-RESERVE
   0 ERR 0 ALL-ERR allu BYTE-COPY
   CLI-START s" --verify-only" ARG+ s" dup.f" AT$ ARG+
   s" " CLI {: erru:n rc:n :}
   s" cli-duplicate: refused" T-LABEL rc 70 T=
   s" cli-duplicate: the record --all-errors writes" T-LABEL
   0 ERR erru 0 ALL-ERR allu T$=
   0 ERR erru s" code" s" E-DUPLICATE-DEFINITION" PACKET {: p:n :}
   s" cli-duplicate: line 2" T-LABEL p s" line" NUMBER$ s" 2" T$=
   s" cli-duplicate: column 3" T-LABEL p s" column" NUMBER$ s" 3" T$=
   s" cli-duplicate: byte 20" T-LABEL p s" byte_start" NUMBER$ s" 20" T$=
   s" cli-duplicate: to byte 22" T-LABEL p s" byte_end" NUMBER$ s" 22" T$=
   s" cli-duplicate: no prose" T-LABEL CLI-OUT-U @ 0 T= ;


\ Under --all-errors --verify-only LATE's refusal follows the duplicate's;
\ plain --all-errors stops at the duplicate.
: CLI-DUPLICATE-GOES-ON ( -- )
   CLI-START s" --all-errors" ARG+ s" --verify-only" ARG+ s" dup-late.f" AT$ ARG+
   s" " CLI {: erru:n rc:n :}
   s" cli-duplicate-goes-on: refused" T-LABEL rc 70 T=
   0 ERR erru JSONL-START-STRICT
   s" cli-duplicate-goes-on: the duplicate first" T-LABEL
   JSONL-NEXT-OBJECT s" code" STRING$ s" E-DUPLICATE-DEFINITION" T$=
   s" cli-duplicate-goes-on: LATE's refusal next" T-LABEL
   JSONL-NEXT-OBJECT s" code" STRING$ s" E-MISMATCH" T$=
   s" cli-duplicate-goes-on: nothing else" T-LABEL JSONL-NEXT-OBJECT -1 T=
   CLI-START s" --json-errors" ARG+ s" --all-errors" ARG+ s" dup-late.f" AT$ ARG+
   s" " CLI {: erru2:n rc2:n :}
   s" cli-duplicate-goes-on: --all-errors exits as the load" T-LABEL rc2 78 T=
   0 ERR erru2 JSONL-START-STRICT
   s" cli-duplicate-goes-on: --all-errors reports the duplicate" T-LABEL
   JSONL-NEXT-OBJECT s" code" STRING$ s" E-DUPLICATE-DEFINITION" T$=
   s" cli-duplicate-goes-on: and stops there" T-LABEL JSONL-NEXT-OBJECT -1 T= ;


\ ---- top-level tokens -------------------------------------------------------

\ Each subject below is checked as top.f. What `bin/hb --load` does with the
\ same text is in the comment above each case.

\ How many packets LINES holds.
: PACKETS ( ptr u8 n -- n )
   JSONL-START
   0 begin
      JSONL-NEXT-OBJECT 0 >=
   while
      1+
   repeat ;


\ The packet at INDEX in LINES, counted from 0, or -1.
: NTH-PACKET ( ptr u8 n n -- n )
   {: lines:ptr linesu:n idx:n :}
   lines linesu JSONL-START
   idx 0 ?do JSONL-NEXT-OBJECT drop loop
   JSONL-NEXT-OBJECT ;


: TOP-CHECK ( ptr u8 n -- CHECK:verdict )
   s" top.f" GUARD-MS CHECK-AS ;


\ The packet for TOKEN: a record of CODE in the subject on LINE at COLUMN,
\ with no definition's name. LABEL stays set for the caller's next assertion.
: TOP-PACKET ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- n )
   {: label:ptr labelu:n tok:ptr toku:n code:ptr codeu:n line:ptr lineu:n col:ptr colu:n :}
   CHECK:VERIFY-OUT$ s" token" tok toku PACKET {: p:n :}
   label labelu T-LABEL p s" code" STRING$ code codeu T$=
   label labelu T-LABEL p s" file" STRING$ SUBJ$ T$=
   label labelu T-LABEL p s" line" NUMBER$ line lineu T$=
   label labelu T-LABEL p s" column" NUMBER$ col colu T$=
   label labelu T-LABEL p s" word" J-STR VALUE 0 < TTRUE
   label labelu T-LABEL
   p ;


\ A refused check whose only packet is located at TOKEN.
: REFUSED-AT ( CHECK:verdict ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: v label:ptr labelu:n tok:ptr toku:n code:ptr codeu:n line:ptr lineu:n col:ptr colu:n :}
   v 1 label labelu EXPECT-KIND
   label labelu T-LABEL CHECK:VERIFY-OUT$ PACKETS 1 T=
   label labelu tok toku code codeu line lineu col colu TOP-PACKET
   s" verdict" STRING$ s" rejected" T$= ;


\ Loaded: E-UNDEFINED: NOSUCHWORD, exit 70. The scan goes on past the token:
\ a refused definition after it is a second packet, in source order.
: TOP-UNDEFINED ( -- )
   s\" 1 NOSUCHWORD drop\n: CVT-AFTER ( -- n ) 1 ;\n" TOP-CHECK
   s" top-undefined" s" NOSUCHWORD" s" E-UNDEFINED-TOP-LEVEL" s" 1" s" 3" REFUSED-AT
   CHECK:VERIFY-OUT$ s" token" s" NOSUCHWORD" PACKET {: p:n :}
   s" top-undefined: its first byte" T-LABEL p s" byte_start" NUMBER$ s" 2" T$=
   s" top-undefined: past its last" T-LABEL p s" byte_end" NUMBER$ s" 12" T$=
   s\" 1 NOSUCHWORD drop\n: CVT-SHORT ( -- n n ) 8 ;\n" TOP-CHECK
   1 s" top-undefined: then a refused definition" EXPECT-KIND
   s" top-undefined: two packets" T-LABEL CHECK:VERIFY-OUT$ PACKETS 2 T=
   s" top-undefined: the token's first" T-LABEL
   CHECK:VERIFY-OUT$ 0 NTH-PACKET s" token" STRING$ s" NOSUCHWORD" T$=
   s" top-undefined: the definition's second" T-LABEL
   CHECK:VERIFY-OUT$ 1 NTH-PACKET s" word" STRING$ s" cvt-short" T$= ;


\ A using refusal refuses the token as it refuses a body's definition, and the
\ scan goes on past it, as past a refused definition: a refused definition after
\ it is a second packet, in source order, and no throw stopped the scan.
: GOES-ON ( CHECK:verdict ptr u8 n ptr u8 n -- )
   {: v label:ptr labelu:n tok:ptr toku:n :}
   v 1 label labelu EXPECT-KIND
   label labelu T-LABEL CHECK:VERIFY-OUT$ PACKETS 2 T=
   label labelu T-LABEL CHECK:VERIFY-OUT$ 0 NTH-PACKET s" token" STRING$ tok toku T$=
   label labelu T-LABEL CHECK:VERIFY-OUT$ 1 NTH-PACKET s" word" STRING$ s" cvt-short" T$=
   label labelu T-LABEL CHECK:VERIFY-LOG$ s" verification stopped by throw" CONTAINS? TFALSE ;


\ Two used packages export AW. Loaded: exit 94.
: TOP-AMBIGUOUS ( -- )
   s\" package CVT-UA public : AW ( -- n ) 1 ; ;package\npackage CVT-UC public : AW ( -- n ) 2 ; ;package\nusing CVT-UA using CVT-UC AW drop ;using ;using\n" TOP-CHECK
   s" top-ambiguous" s" AW" s" E-USING-AMBIGUOUS" s" 3" s" 27" REFUSED-AT
   s\" package CVT-UA public : AW ( -- n ) 1 ; ;package\npackage CVT-UC public : AW ( -- n ) 2 ; ;package\nusing CVT-UA using CVT-UC AW drop ;using ;using\n: CVT-SHORT ( -- n n ) 8 ;\n" TOP-CHECK
   s" top-ambiguous: goes on" s" AW" GOES-ON ;


\ A global CVT-MW and a used public of that name. Loaded: exit 105.
: SHADOW$ ( ptr u8 n -- ptr u8 n )
   SB-RESET
   s\" : CVT-MW ( -- n ) 1 ;\npackage CVT-USG public : CVT-MW ( -- n ) 2 ; ;package\nusing CVT-USG " SB-APPEND
   SB-APPEND
   s\" CVT-MW drop ;using\n" SB-APPEND
   SB$ ;


: TOP-SHADOW ( -- )
   s" " SHADOW$ TOP-CHECK
   s" top-shadow" s" CVT-MW" s" E-USING-SHADOW-GLOBAL" s" 3" s" 15" REFUSED-AT
   s\" : CVT-MW ( -- n ) 1 ;\npackage CVT-USG public : CVT-MW ( -- n ) 2 ; ;package\nusing CVT-USG CVT-MW drop ;using\n: CVT-SHORT ( -- n n ) 8 ;\n" TOP-CHECK
   s" top-shadow: goes on" s" CVT-MW" GOES-ON ;


\ Loaded: E-UNDEFINED: NOSUCH, exit 70; the shadowed name, exit 105.
: TOP-TICK ( -- )
   s\" ' NOSUCH drop\n" TOP-CHECK
   s" top-tick: undefined" s" NOSUCH" s" E-UNDEFINED-TOP-LEVEL" s" 1" s" 3" REFUSED-AT
   s" ' " SHADOW$ TOP-CHECK
   s" top-tick: shadowed" s" CVT-MW" s" E-USING-SHADOW-GLOBAL" s" 3" s" 17" REFUSED-AT ;


\ The engine still has a dictionary entry after `undefine`, but its load
\ refuses a bare call or tick, including an open package's global-tail retry.
: TOP-RETIRED ( -- )
   s\" undefine dup\n' dup drop\n" TOP-CHECK
   s" top-retired: bare tick" s" dup" s" E-UNDEFINED-TOP-LEVEL" s" 2" s" 3" REFUSED-AT
   s\" undefine dup\ndup\n" TOP-CHECK
   s" top-retired: bare call" s" dup" s" E-UNDEFINED-TOP-LEVEL" s" 2" s" 1" REFUSED-AT
   s\" undefine dup\npackage CVT-RT\n' CVT-RT:dup drop\n;package\n" TOP-CHECK
   s" top-retired: open tail" s" CVT-RT:dup" s" E-UNDEFINED-TOP-LEVEL" s" 3" s" 3" REFUSED-AT
   s\" undefine dup\n: dup ( n -- n n )\n   {: x:n :}\n   x x ;\n' dup drop\npackage CVT-RT\n' CVT-RT:dup drop\n;package\n"
   TOP-CHECK
   0 s" top-retired: redeclared word" EXPECT-KIND
   s" top-retired: no packet after declaration" T-LABEL CHECK:VERIFY-OUT$ PACKETS 0 T= ;


\ A published prefix primitive can leave a dictionary entry behind after
\ `undefine`. Its qualified spelling must not bind for a call or a tick.
: TOP-RETIRED-PUBLIC ( -- )
   s" retired-public-call.f"
   s\" undefine CHECKER-OWNER-ABI:HEADER-BYTES\nCHECKER-OWNER-ABI:HEADER-BYTES drop\n" FIXTURE
   s" retired-public-tick.f"
   s\" undefine CHECKER-OWNER-ABI:HEADER-BYTES\n' CHECKER-OWNER-ABI:HEADER-BYTES drop\n" FIXTURE
   s" retired-public-call.f" AT$ {: call:ptr callu:n :}
   s" top-retired-public: call load" T-LABEL call callu NATIVE-RC 70 T=
   s" top-retired-public: call verify" T-LABEL call callu true false CHECK-RC 70 T=
   s" top-retired-public: call check" T-LABEL call callu false false CHECK-RC 70 T=
   s" retired-public-tick.f" AT$ {: tick:ptr ticku:n :}
   s" top-retired-public: tick load" T-LABEL tick ticku NATIVE-RC 70 T=
   s" top-retired-public: tick verify" T-LABEL tick ticku true false CHECK-RC 70 T=
   s" top-retired-public: tick check" T-LABEL tick ticku false false CHECK-RC 70 T= ;


\ IR-ID's wordlists are protected (src/compiler/ir/id.f, which the engine
\ bakes). The load refuses a definition into one at its name, before a colon
\ definition's body, and the package's reopening at the package's name, exit
\ 84 (ENGINE-ERROR:SEAL-PACKAGE). SRC, as the fixture NAME, loads with exit 84,
\ and the check stops at TOK, on LINE at COLUMN, with that code.
: SEALED-AT ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n name:ptr nameu:n tok:ptr toku:n line:ptr lineu:n col:ptr colu:n :}
   name nameu src srcu FIXTURE
   name nameu T-LABEL name nameu AT$ NATIVE-RC 84 T=
   src srcu name nameu GUARD-MS CHECK-AS
   name nameu tok toku s" E-STATEMENT-THROW" line lineu col colu REFUSED-AT
   name nameu T-LABEL CHECK:VERIFY-OUT$ 0 NTH-PACKET s" throw_code" NUMBER$ s" 84" T$= ;

\ A deferred word and a cast read their signature before the seal is asked: an
\ unclosed one in SRC, as the fixture NAME, ends the load with exit 76, and the
\ check stops at the definer TOK, on line 2, for the signature.
: SEALED-SIGNATURE ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n name:ptr nameu:n tok:ptr toku:n :}
   name nameu src srcu FIXTURE
   name nameu T-LABEL name nameu AT$ NATIVE-RC 76 T=
   src srcu name nameu GUARD-MS CHECK-AS
   name nameu tok toku s" E-STATEMENT-THROW" s" 2" s" 1" REFUSED-AT
   name nameu T-LABEL CHECK:VERIFY-OUT$ 0 NTH-PACKET s" throw_code" NUMBER$ s" 7157" T$= ;

\ SRC, as the fixture NAME, loads and verifies.
: UNSEALED ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu:n name:ptr nameu:n :}
   name nameu src srcu FIXTURE
   name nameu T-LABEL name nameu AT$ NATIVE-RC 0 T=
   src srcu name nameu GUARD-MS CHECK-AS 0 name nameu EXPECT-KIND ;

\ The load asks a definition's name for a duplicate before it asks the seal
\ (src/habu/habu2.f EMIT-QUALIFY-DEF, then EMIT-STORE-DEF-NAME). SRC, as the
\ fixture NAME, defines IR-ID's public COUNT again and loads with exit 78, and
\ the check refuses it once, a duplicate at the name, on LINE at COLUMN.
: SEALED-DUPLICATE ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n name:ptr nameu:n line:ptr lineu:n col:ptr colu:n :}
   name nameu src srcu FIXTURE
   name nameu T-LABEL name nameu AT$ NATIVE-RC 78 T=
   src srcu name nameu GUARD-MS CHECK-AS 1 name nameu EXPECT-KIND
   name nameu T-LABEL CHECK:VERIFY-OUT$ PACKETS 1 T=
   CHECK:VERIFY-OUT$ 0 NTH-PACKET {: p:n :}
   name nameu T-LABEL p s" code" STRING$ s" E-DUPLICATE-DEFINITION" T$=
   name nameu T-LABEL p s" token" STRING$ s" IR-ID:COUNT" T$=
   name nameu T-LABEL p s" file" STRING$ SUBJ$ T$=
   name nameu T-LABEL p s" line" NUMBER$ line lineu T$=
   name nameu T-LABEL p s" column" NUMBER$ col colu T$= ;

\ Each definer the scan reads, and `package` of a package the checker lists as
\ sealed; the command line exits 70 for the refusal. A duplicate there is
\ refused as a duplicate by each definer whose scan asks the seal after its
\ own duplicate test. A retirement in a sealed package loads, and so does a
\ definition into a package the source leaves open, qualified or after
\ reopening it.
: SEALED ( -- )
   s\" require src/compiler/ir/id.f\n: IR-ID:CVT-SZ ( -- n ) 1 ;\n"
   s" sealed-colon.f" s" IR-ID:CVT-SZ" s" 2" s" 3" SEALED-AT
   s" sealed-colon.f" T-LABEL s" sealed-colon.f" AT$ true false CHECK-RC 70 T=
   s\" require src/compiler/ir/id.f\ntrusted: IR-ID:CVT-ST ( -- n ) 1 ;\n"
   s" sealed-trusted.f" s" IR-ID:CVT-ST" s" 2" s" 10" SEALED-AT
   s\" require src/compiler/ir/id.f\nvariable IR-ID:CVT-SV\n"
   s" sealed-variable.f" s" IR-ID:CVT-SV" s" 2" s" 10" SEALED-AT
   s\" require src/compiler/ir/id.f\n4 BUFFER: IR-ID:CVT-SU\n"
   s" sealed-created.f" s" IR-ID:CVT-SU" s" 2" s" 11" SEALED-AT
   s\" require src/compiler/ir/id.f\ndefer IR-ID:CVT-SD ( -- )\n"
   s" sealed-defer.f" s" IR-ID:CVT-SD" s" 2" s" 7" SEALED-AT
   s\" require src/compiler/ir/id.f\nCAST: IR-ID:CVT-SC ( n -- ptr u8 )\n"
   s" sealed-cast.f" s" IR-ID:CVT-SC" s" 2" s" 7" SEALED-AT
   s\" require src/compiler/ir/id.f\nDYNAMIC-BUFFER IR-ID:CVT-SB u8\n"
   s" sealed-buffer.f" s" IR-ID:CVT-SB" s" 2" s" 16" SEALED-AT
   s\" require src/compiler/ir/id.f\nBEGIN-STRUCTURE IR-ID:CVT-SS\nEND-STRUCTURE\n"
   s" sealed-structure.f" s" IR-ID:CVT-SS" s" 2" s" 17" SEALED-AT
   s\" require src/compiler/ir/id.f\nBEGIN-STRUCTURE CVT-SQ\nCELL +FIELD IR-ID:CVT-SF\nEND-STRUCTURE\n"
   s" sealed-field.f" s" IR-ID:CVT-SF" s" 3" s" 13" SEALED-AT
   s\" require src/compiler/ir/id.f\nPROCESS-SYMBOLS\nFUNCTION: IR-ID:CVT-SN fstat ( n ptr u8 -- i32 )\n;FUNCTION\n"
   s" sealed-function.f" s" IR-ID:CVT-SN" s" 3" s" 11" SEALED-AT
   s\" require src/compiler/ir/id.f\npackage IR-ID\n: CVT-SZ ( -- n ) 1 ;\n;package\n"
   s" sealed-reopen.f" s" IR-ID" s" 2" s" 9" SEALED-AT
   s\" package type\n;package\n"
   s" sealed-listed.f" s" type" s" 1" s" 9" SEALED-AT
   s\" require src/compiler/ir/id.f\nvariable IR-ID:COUNT\n"
   s" sealed-dup-variable.f" s" 2" s" 10" SEALED-DUPLICATE
   s\" require src/compiler/ir/id.f\n1 constant IR-ID:COUNT\n"
   s" sealed-dup-constant.f" s" 2" s" 12" SEALED-DUPLICATE
   s\" require src/compiler/ir/id.f\ncreate IR-ID:COUNT 4 allot\n"
   s" sealed-dup-create.f" s" 2" s" 8" SEALED-DUPLICATE
   s\" require src/compiler/ir/id.f\nPTR-VARIABLE IR-ID:COUNT\n"
   s" sealed-dup-pointer.f" s" 2" s" 14" SEALED-DUPLICATE
   s\" require src/compiler/ir/id.f\n: CVT-MK ( n -- ) create , does> ( -- n ) @ ;\n5 CVT-MK IR-ID:COUNT\n"
   s" sealed-dup-learned.f" s" 3" s" 10" SEALED-DUPLICATE
   s\" require src/compiler/ir/id.f\n4 BUFFER: IR-ID:COUNT\n"
   s" sealed-dup-created.f" s" 2" s" 11" SEALED-DUPLICATE
   s\" require src/compiler/ir/id.f\ntrusted: IR-ID:COUNT ( n -- ) create , does> ( -- n ) @ ;\n"
   s" sealed-dup-trusted.f" s" 2" s" 10" SEALED-DUPLICATE
   s\" require src/compiler/ir/id.f\ndefer IR-ID:COUNT ( -- n )\n"
   s" sealed-dup-defer.f" s" 2" s" 7" SEALED-DUPLICATE
   s\" require src/compiler/ir/id.f\nBEGIN-STRUCTURE IR-ID:COUNT\nEND-STRUCTURE\n"
   s" sealed-dup-structure.f" s" 2" s" 17" SEALED-DUPLICATE
   s\" require src/compiler/ir/id.f\nBEGIN-STRUCTURE CVT-SQD\nCELL +FIELD IR-ID:COUNT\nEND-STRUCTURE\n"
   s" sealed-dup-field.f" s" 3" s" 13" SEALED-DUPLICATE
   s\" require src/compiler/ir/id.f\nPROCESS-SYMBOLS\nFUNCTION: IR-ID:COUNT fstat ( n ptr u8 -- i32 )\n;FUNCTION\n"
   s" sealed-dup-function.f" s" 3" s" 11" SEALED-DUPLICATE
   s\" require src/compiler/ir/id.f\ndefer IR-ID:CVT-SE ( -- n\n"
   s" sealed-defer-signature.f" s" defer" SEALED-SIGNATURE
   s\" require src/compiler/ir/id.f\nCAST: IR-ID:CVT-SC ( n --\n"
   s" sealed-cast-signature.f" s" CAST:" SEALED-SIGNATURE
   s\" require src/compiler/ir/id.f\nundefine IR-ID:SPAN-LOCAL\n"
   s" sealed-undefine.f" UNSEALED
   s\" package CVT-SO\npublic\n: A ( -- n ) 1 ;\n;package\n: CVT-SO:B ( -- n ) 2 ;\npackage CVT-SO\n: C ( -- n ) 3 ;\n;package\n"
   s" sealed-open.f" UNSEALED ;


\ Return-stack keywords have checker axioms for compiled bodies but no
\ interpret or tick binding. A declaration read from source still binds.
: TOP-AXIOM ( -- )
   s\" ' >r drop\n" TOP-CHECK
   s" top-axiom: tick" s" >r" s" E-UNDEFINED-TOP-LEVEL" s" 1" s" 3" REFUSED-AT
   s\" >r\n" TOP-CHECK
   s" top-axiom: call" s" >r" s" E-UNDEFINED-TOP-LEVEL" s" 1" s" 1" REFUSED-AT
   s\" : CVT-SCANNED ( -- n ) 7 ;\n' CVT-SCANNED drop\nCVT-SCANNED drop\n"
   TOP-CHECK
   0 s" top-axiom: scanned declaration" EXPECT-KIND
   s" top-axiom: no packet" T-LABEL CHECK:VERIFY-OUT$ PACKETS 0 T= ;


\ A cold prefix publishes this PPRIM package word without a user record.
\ Its scanner runs in the cold host, and both forms also use the product CLI.
: TOP-COLD-PRIM ( -- )
   s" prim-call.f" s\" CHECKER-OWNER-ABI:HEADER-BYTES drop\n" FIXTURE
   s" prim-tick.f" s\" ' CHECKER-OWNER-ABI:HEADER-BYTES drop\n" FIXTURE
   s" prim-scan.f"
   s\" require src/habu/verify-source.f\ns\" CHECKER-OWNER-ABI:HEADER-BYTES drop\" s\" prim-call.f\" VERIFY:SOURCE-COMPOSE-IN-SCOPE\ns\" ' CHECKER-OWNER-ABI:HEADER-BYTES drop\" s\" prim-tick.f\" VERIFY:SOURCE-COMPOSE-IN-SCOPE\n" FIXTURE
   s" prim-call.f" AT$ {: call:ptr callu:n :}
   s" top-cold-prim: call loads" T-LABEL
   PROC-ARGV-ENV-RESET s" --load" ARG+ call callu ARG+
   PROC-ENV-INHERIT-MISSING s" " COLD-CLI nip 0 T=
   s" top-cold-prim: call verifies" T-LABEL
   call callu true false CHECK-RC 0 T=
   s" prim-tick.f" AT$ {: tick:ptr ticku:n :}
   s" top-cold-prim: tick loads" T-LABEL
   PROC-ARGV-ENV-RESET s" --load" ARG+ tick ticku ARG+
   PROC-ENV-INHERIT-MISSING s" " COLD-CLI nip 0 T=
   s" top-cold-prim: tick verifies" T-LABEL
   tick ticku true false CHECK-RC 0 T=
   s" prim-scan.f" AT$ {: scan:ptr scanu:n :}
   s" top-cold-prim: cold scanner" T-LABEL
   PROC-ARGV-ENV-RESET s" --load" ARG+ scan scanu ARG+
   PROC-ENV-INHERIT-MISSING s" " COLD-CLI nip 0 T= ;


\ The engine's table (src/habu/prims.f) opens a row with EPRIM: or EPPRIM:,
\ which parse the row's name, EPPRIM: its package first, so a row named
\ `create` or `:` is a row and its closer no name. Both closers write the open
\ row, so either ends either row, and the definition after a row the other one
\ ends is code, as its use shows. Each fixture defines the words as prims.f
\ does. SRC, as the fixture NAME, loads and verifies with no packet.
: ENGINE-ROWS-READ ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu:n name:ptr nameu:n :}
   name nameu src srcu FIXTURE
   name nameu T-LABEL name nameu AT$ NATIVE-RC 0 T=
   src srcu name nameu VERIFIED
   name nameu T-LABEL CHECK:VERIFY-OUT$ PACKETS 0 T= ;

\ A row SRC ends inside, after the words on lines 1-5, stops the verifier at
\ its opener TOK, on line 6 at column 3, with the record check.f writes for a
\ primitive-axiom row that does not close.
: ENGINE-ROW-OPEN ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n name:ptr nameu:n tok:ptr toku:n :}
   src srcu name nameu GUARD-MS CHECK-AS
   name nameu tok toku s" E-MALFORMED-REGISTRY-ROW" s" 6" s" 3" REFUSED-AT
   name nameu T-LABEL CHECK:VERIFY-STOP 7189 T= ;

: ENGINE-ROWS ( -- )
   s\" : ROW-NAME ( -- ) parse-name 2drop ;\n: EPRIM: ( -- ) ROW-NAME ;\n: EPRIM; ( -- ) ;\n: EPPRIM: ( -- ) parse-name 2drop ROW-NAME ;\n: ECLOSE-PRIVATE ( -- ) ;\nEPRIM: create EPRIM;\nEPRIM: : EPRIM;\nEPRIM: CVT-EM ECLOSE-PRIVATE\n: CVT-EB ( -- ) ;\nCVT-EB\n"
   s" engine-rows-bare.f" ENGINE-ROWS-READ
   s\" : ROW-NAME ( -- ) parse-name 2drop ;\n: EPRIM: ( -- ) ROW-NAME ;\n: EPRIM; ( -- ) ;\n: EPPRIM: ( -- ) parse-name 2drop ROW-NAME ;\n: ECLOSE-PRIVATE ( -- ) ;\nEPPRIM: CVT-ER create ECLOSE-PRIVATE\nEPPRIM: CVT-ER : ECLOSE-PRIVATE\nEPPRIM: CVT-ER CVT-EM EPRIM;\n: CVT-EP ( -- ) ;\nCVT-EP\n"
   s" engine-rows-package.f" ENGINE-ROWS-READ
   s\" : ROW-NAME ( -- ) parse-name 2drop ;\n: EPRIM: ( -- ) ROW-NAME ;\n: EPRIM; ( -- ) ;\n: EPPRIM: ( -- ) parse-name 2drop ROW-NAME ;\n: ECLOSE-PRIVATE ( -- ) ;\n  EPRIM: CVT-EO\n"
   s" engine-row-bare-open.f" s" EPRIM:" ENGINE-ROW-OPEN
   s\" : ROW-NAME ( -- ) parse-name 2drop ;\n: EPRIM: ( -- ) ROW-NAME ;\n: EPRIM; ( -- ) ;\n: EPPRIM: ( -- ) parse-name 2drop ROW-NAME ;\n: ECLOSE-PRIVATE ( -- ) ;\n  EPPRIM: CVT-ER CVT-EO\n"
   s" engine-row-package-open.f" s" EPPRIM:" ENGINE-ROW-OPEN ;


\ `undefine` retires the global dictionary entry before the used public binds.
: TOP-RETIRED-IMPORT ( -- )
   s" retired-call.f"
   s\" undefine dup\npackage CVT-RI public : dup ( -- n ) 7 ; ;package\nusing CVT-RI dup drop ;using\n" FIXTURE
   s" retired-tick.f"
   s\" undefine dup\npackage CVT-RI public : dup ( -- n ) 7 ; ;package\nusing CVT-RI ' dup drop ;using\n" FIXTURE
   s" retired-call.f" AT$ {: call:ptr callu:n :}
   s" top-retired-import: call loads" T-LABEL call callu NATIVE-RC 0 T=
   s" top-retired-import: call verifies" T-LABEL call callu true false CHECK-RC 0 T=
   s" top-retired-import: call checks" T-LABEL call callu false false CHECK-RC 0 T=
   s" retired-tick.f" AT$ {: tick:ptr ticku:n :}
   s" top-retired-import: tick loads" T-LABEL tick ticku NATIVE-RC 0 T=
   s" top-retired-import: tick verifies" T-LABEL tick ticku true false CHECK-RC 0 T=
   s" top-retired-import: tick checks" T-LABEL tick ticku false false CHECK-RC 0 T= ;


\ Loaded: E-UNDEFINED: CVT-FWD, exit 70.
: TOP-ORDER ( -- )
   s\" CVT-FWD drop\n: CVT-FWD ( -- n ) 1 ;\n" TOP-CHECK
   s" top-order" s" CVT-FWD" s" E-UNDEFINED-TOP-LEVEL" s" 1" s" 1" REFUSED-AT ;


\ Loaded: E-UNDEFINED for each, exit 70: the engine's reader refuses a decimal
\ out of range, and the other two are no number.
: TOP-NUMBER ( -- )
   s\" 99999999999999999999 drop\n" TOP-CHECK
   s" top-number: out of range" s" 99999999999999999999" s" E-UNDEFINED-TOP-LEVEL" s" 1" s" 1" REFUSED-AT
   s\" 1. drop\n" TOP-CHECK
   s" top-number: a point and no digits" s" 1." s" E-UNDEFINED-TOP-LEVEL" s" 1" s" 1" REFUSED-AT
   s\" $GG drop\n" TOP-CHECK
   s" top-number: no hexadecimal digit" s" $GG" s" E-UNDEFINED-TOP-LEVEL" s" 1" s" 1" REFUSED-AT ;


\ Loaded: E-UNDEFINED: if, exit 70.
: TOP-KEYWORD ( -- )
   s\" if\n" TOP-CHECK
   s" top-keyword" s" if" s" E-UNDEFINED-TOP-LEVEL" s" 1" s" 1" REFUSED-AT ;


\ Loaded: E-UNDEFINED for each, exit 70. A body refuses the malformed name, two
\ qualifiers, as E-BAD-QUALIFIED, and the top level as E-BAD-QUALIFIED-TOP-LEVEL.
: TOP-QUALIFIED ( -- )
   s\" CVT-A:B:C drop\n" TOP-CHECK
   s" top-qualified: malformed" s" CVT-A:B:C" s" E-BAD-QUALIFIED-TOP-LEVEL" s" 1" s" 1" REFUSED-AT
   s\" CVT-NOPKG:CVT-W drop\n" TOP-CHECK
   s" top-qualified: no such package" s" CVT-NOPKG:CVT-W" s" E-UNDEFINED-TOP-LEVEL" s" 1" s" 1" REFUSED-AT
   s\" package CVT-QP public : CVT-QW ( -- n ) 1 ; ;package\nCVT-QP:CVT-NOSUCH drop\n" TOP-CHECK
   s" top-qualified: no such public" s" CVT-QP:CVT-NOSUCH" s" E-UNDEFINED-TOP-LEVEL" s" 2" s" 1" REFUSED-AT ;


\ The tokens a word that parses its own input reads cannot be known without
\ running it. Loaded, CVT-GRAB takes NOSUCH, and NOSUCH2 is E-UNDEFINED, exit
\ 70; alone, the first two lines load. Undeclared, CVT-GRAB is deferred to the
\ run where it stands and the scan discovers nothing after it, since what it
\ reads may be any of the rest. A `parses:` row bounds what it reads, and a
\ token past its operand is resolved again.
: TOP-DEFERRED ( -- )
   s\" : CVT-GRAB ( -- ) parse-name 2drop ;\nCVT-GRAB NOSUCH\n: CVT-AFTER ( -- n ) 1 ;\nNOSUCH2 drop\n" TOP-CHECK
   5 s" top-deferred: undeclared, deferred" EXPECT-KIND
   s" top-deferred: undeclared, one packet" T-LABEL CHECK:VERIFY-OUT$ PACKETS 1 T=
   s" top-deferred: undeclared, the word" s" CVT-GRAB" s" W-CHECK-DEFERRED" s" 2" s" 1" TOP-PACKET
   s" verdict" STRING$ s" deferred" T$=
   s\" : CVT-GRAB ( -- ) parse-name 2drop ;\nparses: CVT-GRAB 1\nCVT-GRAB NOSUCH\n: CVT-AFTER ( -- n ) 1 ;\nNOSUCH2 drop\n" TOP-CHECK
   1 s" top-deferred: refused past the operand" EXPECT-KIND
   s" top-deferred: two packets" T-LABEL CHECK:VERIFY-OUT$ PACKETS 2 T=
   s" top-deferred: nothing names the operand" T-LABEL
   CHECK:VERIFY-OUT$ s" token" s" NOSUCH" PACKET 0 < TTRUE
   s" top-deferred: the call" s" CVT-GRAB" s" W-CHECK-DEFERRED" s" 3" s" 1" TOP-PACKET
   s" verdict" STRING$ s" deferred" T$=
   s" top-deferred: past the operand" s" NOSUCH2" s" E-UNDEFINED-TOP-LEVEL" s" 5" s" 1" TOP-PACKET
   s" verdict" STRING$ s" rejected" T$=
   s\" : CVT-GRAB ( -- ) parse-name 2drop ;\nCVT-GRAB NOSUCH\n" TOP-CHECK
   5 s" top-deferred: alone, deferred" EXPECT-KIND
   s" top-deferred: alone, one packet" T-LABEL CHECK:VERIFY-OUT$ PACKETS 1 T=
   s" top-deferred: alone, the stretch" s" CVT-GRAB" s" W-CHECK-DEFERRED" s" 2" s" 1" TOP-PACKET
   s" verdict" STRING$ s" deferred" T$= ;


\ A file the subject loads scans its own deferred word. Its warning
\ contributes to the composed verdict. Undeclared, the dependency's GRAB may
\ read any of the rest and change the scope the caller goes on in, so the
\ caller discovers nothing after it either. With GRAB's operand declared the
\ caller resumes with its own top-level state and refuses the malformed
\ qualified token.
: TOP-NESTED-DEFERRED ( -- )
   s" nested-dep.f" s\" : GRAB ( -- ) parse-name 2drop ;\nGRAB x\n" FIXTURE
   s\" s\" nested-dep.f\" required\nQ:R:S\n" TOP-CHECK
   5 s" top-nested-deferred: undeclared, deferred" EXPECT-KIND
   s" top-nested-deferred: undeclared, one packet" T-LABEL CHECK:VERIFY-OUT$ PACKETS 1 T=
   s" top-nested-deferred: undeclared, dependency warning" T-LABEL
   CHECK:VERIFY-OUT$ s" token" s" GRAB" PACKET s" code" STRING$ s" W-CHECK-DEFERRED" T$=
   s" nested-dep.f" s\" : GRAB ( -- ) parse-name 2drop ;\nparses: GRAB 1\nGRAB x\n" FIXTURE
   s\" s\" nested-dep.f\" required\nQ:R:S\n" TOP-CHECK
   1 s" top-nested-deferred: refused" EXPECT-KIND
   s" top-nested-deferred: two packets" T-LABEL CHECK:VERIFY-OUT$ PACKETS 2 T=
   s" top-nested-deferred: dependency warning" T-LABEL
   CHECK:VERIFY-OUT$ s" token" s" GRAB" PACKET {: p:n :}
   p s" code" STRING$ s" W-CHECK-DEFERRED" T$=
   s" top-nested-deferred: dependency path" T-LABEL
   p s" file" STRING$ s" nested-dep.f" AT$ T$=
   s" top-nested-deferred: caller refusal" s" Q:R:S" s" E-BAD-QUALIFIED-TOP-LEVEL" s" 2" s" 1" TOP-PACKET
   s" verdict" STRING$ s" rejected" T$=
   s\" s\" nested-dep.f\" required\n7 drop\n" TOP-CHECK
   5 s" top-nested-deferred: warning survives" EXPECT-KIND
   s" top-nested-deferred: only warning" T-LABEL CHECK:VERIFY-OUT$ PACKETS 1 T= ;


\ A deferred check whose only packet is the call TOKEN on LINE at COLUMN.
: DEFERRED-ONLY ( CHECK:verdict ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: v label:ptr labelu:n tok:ptr toku:n line:ptr lineu:n col:ptr colu:n :}
   v 5 label labelu EXPECT-KIND
   label labelu T-LABEL CHECK:VERIFY-OUT$ PACKETS 1 T=
   label labelu tok toku s" W-CHECK-DEFERRED" line lineu col colu TOP-PACKET
   s" verdict" STRING$ s" deferred" T$= ;


\ The deferred stretch packet IDX holds, counted from 0: TOKEN on LINE at COLUMN.
: DEFERRED-NTH ( ptr u8 n n ptr u8 n ptr u8 n ptr u8 n -- )
   {: label:ptr labelu:n idx:n tok:ptr toku:n line:ptr lineu:n col:ptr colu:n :}
   CHECK:VERIFY-OUT$ idx NTH-PACKET {: p:n :}
   label labelu T-LABEL p s" token" STRING$ tok toku T$=
   label labelu T-LABEL p s" code" STRING$ s" W-CHECK-DEFERRED" T$=
   label labelu T-LABEL p s" line" NUMBER$ line lineu T$=
   label labelu T-LABEL p s" column" NUMBER$ col colu T$=
   label labelu T-LABEL p s" verdict" STRING$ s" deferred" T$= ;


\ A refused check of two packets: the call TOKEN deferred at the start of
\ LINE, then the definition WORD refused with CODE.
: BOUND-REFUSED ( CHECK:verdict ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: v label:ptr labelu:n tok:ptr toku:n line:ptr lineu:n word:ptr wordu:n code:ptr codeu:n :}
   v 1 label labelu EXPECT-KIND
   label labelu T-LABEL CHECK:VERIFY-OUT$ PACKETS 2 T=
   label labelu tok toku s" W-CHECK-DEFERRED" line lineu s" 1" TOP-PACKET drop
   CHECK:VERIFY-OUT$ 1 NTH-PACKET {: p:n :}
   label labelu T-LABEL p s" word" STRING$ word wordu T$=
   label labelu T-LABEL p s" code" STRING$ code codeu T$= ;


\ Loaded, PN reads deftype, variable or `:` and OKW loads; F loads, and G is
\ E-MISMATCH, exit 70. Declared, the operand is data: PN is deferred at its
\ call, nothing names its operand, and the check goes on past it, so V is
\ discovered, F certifies and G is refused. Undeclared, PN may read any of the
\ rest, and the check discovers nothing after it.
: TOP-PARSES-BOUND ( -- )
   s\" : PN ( -- ) parse-name 2drop ;\nparses: PN 1\nPN deftype\n: OKW ( n -- n ) 1 + ;\n" TOP-CHECK
   s" top-parses-bound: deftype" s" PN" s" 3" s" 1" DEFERRED-ONLY
   s\" : PN ( -- ) parse-name 2drop ;\nparses: PN 1\nPN variable\n: OKW ( n -- n ) 1 + ;\n" TOP-CHECK
   s" top-parses-bound: variable" s" PN" s" 3" s" 1" DEFERRED-ONLY
   s\" : PN ( -- ) parse-name 2drop ;\nparses: PN 1\nPN :\n: OKW ( n -- n ) 1 + ;\n" TOP-CHECK
   s" top-parses-bound: colon" s" PN" s" 3" s" 1" DEFERRED-ONLY
   s\" : PN ( -- ) parse-name 2drop ;\nparses: PN 1\nPN foo\nvariable V\n: F ( -- n ) V @ ;\n: G ( -- n ) V @ V @ ;\n" TOP-CHECK
   s" top-parses-bound: past the operand" s" PN" s" 3" s" g" s" E-MISMATCH" BOUND-REFUSED
   s\" : PN ( -- ) parse-name 2drop ;\nPN foo\nvariable V\n: G ( -- n ) V @ V @ ;\n" TOP-CHECK
   s" top-parses-bound: undeclared" s" PN" s" 2" s" 1" DEFERRED-ONLY ;


\ Loaded, BLK reads through ;BLK and G is E-MISMATCH, exit 70. Declared, the
\ definer, comment and string openers it reads are data, across lines, and the
\ check resumes after the end; undeclared, BLK is deferred and nothing after it
\ is discovered. SU reads one name, then through the first exact end: a name
\ spelled like an end is the prefix, and either listed end closes it.
: TOP-PARSES-THROUGH ( -- )
   s\" require lib/string.f\n: BLK? ( ptr u8 n -- bool ) s\" ;BLK\" STR= ;\n: BLK ( -- ) begin parse-name dup 0= if 2drop exit then BLK? until ;\nparses-through: BLK 0 ( ;BLK )\nBLK deftype\n   foo\n;BLK\n: OKW ( n -- n ) 1 + ;\n: G ( -- n ) 1 2 ;\n" TOP-CHECK
   s" top-parses-through: definer" s" BLK" s" 5" s" g" s" E-MISMATCH" BOUND-REFUSED
   s\" require lib/string.f\n: BLK? ( ptr u8 n -- bool ) s\" ;BLK\" STR= ;\n: BLK ( -- ) begin parse-name dup 0= if 2drop exit then BLK? until ;\nBLK deftype\n   foo\n;BLK\n: G ( -- n ) 1 2 ;\n" TOP-CHECK
   s" top-parses-through: undeclared" s" BLK" s" 4" s" 1" DEFERRED-ONLY
   s\" require lib/string.f\n: BLK? ( ptr u8 n -- bool ) s\" ;BLK\" STR= ;\n: BLK ( -- ) begin parse-name dup 0= if 2drop exit then BLK? until ;\nparses-through: BLK 0 ( ;BLK )\nBLK ( \\ s\" .(\n  : X ;BLK\n: G ( -- n ) 1 2 ;\n" TOP-CHECK
   s" top-parses-through: openers" s" BLK" s" 5" s" g" s" E-MISMATCH" BOUND-REFUSED
   s\" require lib/string.f\n: SU? ( ptr u8 n -- bool )\n   {: a:ptr u:n :}\n   a u s\" ;SU\" STR=  a u s\" P:;SU\" STR=  or ;\n: SU ( -- ) parse-name 2drop begin parse-name dup 0= if 2drop exit then SU? until ;\nparses-through: SU 1 ( ;SU P:;SU )\nSU ;SU a b ;SU\nSU name c P:;SU\n: G ( -- n ) 1 2 ;\n" TOP-CHECK
   1 s" top-parses-through: suite" EXPECT-KIND
   s" top-parses-through: suite, three packets" T-LABEL CHECK:VERIFY-OUT$ PACKETS 3 T=
   s" top-parses-through: suite, first end" T-LABEL
   CHECK:VERIFY-OUT$ 0 NTH-PACKET s" line" NUMBER$ s" 7" T$=
   s" top-parses-through: suite, second end" T-LABEL
   CHECK:VERIFY-OUT$ 1 NTH-PACKET s" line" NUMBER$ s" 8" T$=
   s" top-parses-through: suite, resumed" T-LABEL
   CHECK:VERIFY-OUT$ 2 NTH-PACKET s" word" STRING$ s" g" T$= ;


\ A row binds the selected definition, never its spelling. Loaded, the unset
\ DW throws, exit 76; each other call reads its operand and G is E-MISMATCH,
\ exit 70. A defer stays opaque under a row, and so does the resident
\ parse-name, whose row no source states; a wrapper of a declared word
\ inherits no row, a package's own `parses:` is an ordinary parser, and a row
\ dies with the binding `undefine` retires, though the new PN reuses its
\ symbol. Each is deferred, and nothing after it is discovered. An EXPORT
\ copies the row to its alias, and a row may target the original declarer:
\ the check goes on past both.
: TOP-PARSES-OPAQUE ( -- )
   s\" defer DW ( -- )\nparses: DW 1\nDW :\n: G ( -- n ) 1 2 ;\n" TOP-CHECK
   s" top-parses-opaque: defer" s" DW" s" 3" s" 1" DEFERRED-ONLY
   s\" parse-name foo\n: G ( -- n ) 1 2 ;\n" TOP-CHECK
   s" top-parses-opaque: resident" s" parse-name" s" 1" s" 1" DEFERRED-ONLY
   s\" : W ( -- ) parse-name 2drop ;\n: WR ( -- ) W ;\nparses: W 1\nWR :\n: G ( -- n ) 1 2 ;\n" TOP-CHECK
   s" top-parses-opaque: wrapper" s" WR" s" 4" s" 1" DEFERRED-ONLY
   s\" package Q\n: parses: ( -- ) parse-name 2drop parse-name 2drop ;\n: PN ( -- ) parse-name 2drop ;\nparses: PN 1\n;package\n: G ( -- n ) 1 2 ;\n" TOP-CHECK
   s" top-parses-opaque: package declarer" s" parses:" s" 4" s" 1" DEFERRED-ONLY
   s\" : PN ( -- ) parse-name 2drop ;\nparses: PN 1\nundefine PN\n: PN ( -- ) parse-name 2drop ;\nPN :\n: G ( -- n ) 1 2 ;\n" TOP-CHECK
   s" top-parses-opaque: redefined" s" PN" s" 5" s" 1" DEFERRED-ONLY
   s\" package Q\n: PN ( -- ) parse-name 2drop ;\nparses: PN 1\npublic\nEXPORT PN\n;package\nQ:PN :\n: G ( -- n ) 1 2 ;\n" TOP-CHECK
   s" top-parses-opaque: export" s" Q:PN" s" 7" s" g" s" E-MISMATCH" BOUND-REFUSED
   s\" parses: parses: 2\n: PN ( -- ) parse-name 2drop ;\nparses: PN 1\nPN :\n: G ( -- n ) 1 2 ;\n" TOP-CHECK
   s" top-parses-opaque: declarer row" s" PN" s" 4" s" g" s" E-MISMATCH" BOUND-REFUSED ;


\ Loaded, PN takes `required`, `include nosuch.f` or `;package` as its operand
\ and OKW loads; BLK takes every token through ;BLK, a loader and its path, a
\ definer, package words and `:`, and the `s" nosuch.f" required` after it
\ cannot open, exit 74. Past PN's operand, `s" dep.f" required` loads
\ CVT-SEVEN for X, `require nosuch.f` cannot open, exit 74, G is E-MISMATCH,
\ exit 70, and `: required` defines a loader's name again, exit 78. Declared,
\ the operand is the parser's: no loader, definer or package word in it loads,
\ refuses or opens anything, and the check goes on after the window, so the
\ loader, the definition or the loader's name there is checked.
: TOP-PARSES-LOADER ( -- )
   s\" : PN ( -- ) parse-name 2drop ;\nparses: PN 1\nPN required\n: OKW ( n -- n ) 1 + ;\n" TOP-CHECK
   s" top-parses-loader: required" s" PN" s" 3" s" 1" DEFERRED-ONLY
   s\" : PN ( -- ) parse-name 2drop parse-name 2drop ;\nparses: PN 2\nPN include nosuch.f\n: OKW ( n -- n ) 1 + ;\n" TOP-CHECK
   s" top-parses-loader: include" s" PN" s" 3" s" 1" DEFERRED-ONLY
   s\" : PN ( -- ) parse-name 2drop ;\nparses: PN 1\nPN ;package\n: OKW ( n -- n ) 1 + ;\n" TOP-CHECK
   s" top-parses-loader: ;package" s" PN" s" 3" s" 1" DEFERRED-ONLY
   s\" require lib/string.f\n: BLK? ( ptr u8 n -- bool ) s\" ;BLK\" STR= ;\n: BLK ( -- ) begin parse-name dup 0= if 2drop exit then BLK? until ;\nparses-through: BLK 0 ( ;BLK )\nBLK require nosuch.f s\" x.f\" included\n   variable package ;package using : X ;BLK\ns\" nosuch.f\" required\n" TOP-CHECK
   1 s" top-parses-loader: through" EXPECT-KIND
   s" top-parses-loader: through, two packets" T-LABEL CHECK:VERIFY-OUT$ PACKETS 2 T=
   s" top-parses-loader: through, the call" s" BLK" s" W-CHECK-DEFERRED" s" 5" s" 1" TOP-PACKET drop
   s" top-parses-loader: through, the loader after" s" E-MISSING-SOURCE" s" required" s" 7" s" 14" AT-TOKEN
   s\" : PN ( -- ) parse-name 2drop ;\nparses: PN 1\nPN required\ns\" dep.f\" required\n: X ( -- n ) CVT-SEVEN ;\n" TOP-CHECK
   s" top-parses-loader: a load after" s" PN" s" 3" s" 1" DEFERRED-ONLY
   s\" : PN ( -- ) parse-name 2drop ;\nparses: PN 1\nPN required\nrequire nosuch.f\n" TOP-CHECK
   1 s" top-parses-loader: a missing file after" EXPECT-KIND
   s" top-parses-loader: a missing file after, two packets" T-LABEL CHECK:VERIFY-OUT$ PACKETS 2 T=
   s" top-parses-loader: a missing file after, the call" s" PN" s" W-CHECK-DEFERRED" s" 3" s" 1" TOP-PACKET drop
   s" top-parses-loader: a missing file after" s" E-MISSING-SOURCE" s" require" s" 4" s" 1" AT-TOKEN
   s\" : PN ( -- ) parse-name 2drop ;\nparses: PN 1\nPN include\n: G ( -- n ) 1 2 ;\n" TOP-CHECK
   s" top-parses-loader: a definition after" s" PN" s" 3" s" g" s" E-MISMATCH" BOUND-REFUSED
   s\" : PN ( -- ) parse-name 2drop ;\nparses: PN 1\nPN required\n: required ( -- ) ;\n" TOP-CHECK
   s" top-parses-loader: a loader's name after" s" PN" s" 3" s" required" s" E-DUPLICATE-DEFINITION" BOUND-REFUSED ;


\ A deferred check whose only packet is the call TOKEN on LINE of the fixture
\ DEP, a file the subject loads.
: DEP-DEFERRED-ONLY ( CHECK:verdict ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: v label:ptr labelu:n tok:ptr toku:n dep:ptr depu:n line:ptr lineu:n :}
   v 5 label labelu EXPECT-KIND
   label labelu T-LABEL CHECK:VERIFY-OUT$ PACKETS 1 T=
   CHECK:VERIFY-OUT$ s" token" tok toku PACKET {: p:n :}
   label labelu T-LABEL p s" code" STRING$ s" W-CHECK-DEFERRED" T$=
   label labelu T-LABEL p s" file" STRING$ dep depu AT$ T$=
   label labelu T-LABEL p s" line" NUMBER$ line lineu T$= ;


\ Loaded, PN takes `required`, or `include` and `nosuch.f`, and OKW loads;
\ past PN, the redefined PN or GRAB, which take `required`, `require nosuch.f`
\ cannot open, exit 74; DW, unset, throws, exit 76. A word that may read on
\ with no row, or with a row it cannot use, a defer's or one its binding's
\ retirement ended, may read any of the rest: it is deferred, and the check
\ reads, loads and refuses nothing after it, in its file and in each file that
\ loaded it. A dependency whose row bounds GRAB is gone past, and the subject's
\ missing file is refused.
: TOP-OPAQUE-LOADER ( -- )
   s\" : PN ( -- ) parse-name 2drop ;\nPN required\n: OKW ( n -- n ) 1 + ;\n" TOP-CHECK
   s" top-opaque-loader: required" s" PN" s" 2" s" 1" DEFERRED-ONLY
   s\" : PN ( -- ) parse-name 2drop parse-name 2drop ;\nPN include nosuch.f\n: OKW ( n -- n ) 1 + ;\n" TOP-CHECK
   s" top-opaque-loader: include" s" PN" s" 2" s" 1" DEFERRED-ONLY
   s\" : PN ( -- ) parse-name 2drop ;\nPN required\nrequire nosuch.f\n: G ( -- n ) 1 2 ;\n" TOP-CHECK
   s" top-opaque-loader: no row" s" PN" s" 2" s" 1" DEFERRED-ONLY
   s\" : PN ( -- ) parse-name 2drop ;\nparses: PN 1\nundefine PN\n: PN ( -- ) parse-name 2drop ;\nPN required\nrequire nosuch.f\n: G ( -- n ) 1 2 ;\n" TOP-CHECK
   s" top-opaque-loader: retired row" s" PN" s" 5" s" 1" DEFERRED-ONLY
   s\" defer DW ( -- )\nparses: DW 1\nDW include\nrequire nosuch.f\n: G ( -- n ) 1 2 ;\n" TOP-CHECK
   s" top-opaque-loader: defer" s" DW" s" 3" s" 1" DEFERRED-ONLY
   s" opq-dep.f" s\" : GRAB ( -- ) parse-name 2drop ;\nGRAB required\n" FIXTURE
   s\" require opq-dep.f\nrequire nosuch.f\n: G ( -- n ) 1 2 ;\n" TOP-CHECK
   s" top-opaque-loader: a dependency's" s" GRAB" s" opq-dep.f" s" 2" DEP-DEFERRED-ONLY
   s" opq-defer-dep.f" s\" defer DW ( -- )\nparses: DW 1\nDW include\n" FIXTURE
   s\" require opq-defer-dep.f\nrequire nosuch.f\n: G ( -- n ) 1 2 ;\n" TOP-CHECK
   s" top-opaque-loader: a dependency's defer" s" DW" s" opq-defer-dep.f" s" 3" DEP-DEFERRED-ONLY
   s" row-dep.f" s\" : GRAB ( -- ) parse-name 2drop ;\nparses: GRAB 1\nGRAB required\n" FIXTURE
   s\" require row-dep.f\nrequire nosuch.f\n" TOP-CHECK
   1 s" top-opaque-loader: a bounded dependency" EXPECT-KIND
   s" top-opaque-loader: a bounded dependency, two packets" T-LABEL CHECK:VERIFY-OUT$ PACKETS 2 T=
   s" top-opaque-loader: a bounded dependency's call" T-LABEL
   CHECK:VERIFY-OUT$ s" token" s" GRAB" PACKET s" file" STRING$ s" row-dep.f" AT$ T$=
   s" top-opaque-loader: the subject goes on" s" E-MISSING-SOURCE" s" require" s" 2" s" 1" AT-TOKEN ;


\ A refused row: its one packet E-PARSES-ROW at TOKEN, with repair CLASS.
: ROW-REFUSED ( CHECK:verdict ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: v label:ptr labelu:n tok:ptr toku:n line:ptr lineu:n col:ptr colu:n class:ptr classu:n :}
   v label labelu tok toku s" E-PARSES-ROW" line lineu col colu REFUSED-AT
   label labelu T-LABEL
   CHECK:VERIFY-OUT$ 0 NTH-PACKET s" repair_class" STRING$ class classu T$= ;


\ Loaded, each row below throws E-PARSES-ROW, exit 67, naming its reason. A
\ target that names no word, a malformed, ambiguous or shadow-refused one, and
\ a live word that reads no source are fix_parses_row at the target; a count
\ that is missing, not a number or negative, and a delimiter list that is
\ empty, unopened or unclosed are fix_parses_syntax, at the keyword when no
\ target follows it. A checked body calling a declarer or the registrar is
\ E-UNSAFE; loaded, it is refused, exit 70.
: TOP-PARSES-ROW ( -- )
   s\" : PN ( -- ) parse-name 2drop ;\nparses: NOSUCH 1\n" TOP-CHECK
   s" top-parses-row: unknown" s" NOSUCH" s" 2" s" 9" s" fix_parses_row" ROW-REFUSED
   s\" : PN ( -- ) parse-name 2drop ;\nparses: A::B 1\n" TOP-CHECK
   s" top-parses-row: malformed" s" A::B" s" 2" s" 9" s" fix_parses_row" ROW-REFUSED
   s\" package A1\npublic\n: PQ ( -- ) parse-name 2drop ;\n;package\npackage A2\npublic\n: PQ ( -- ) parse-name 2drop ;\n;package\nusing A1\nusing A2\nparses: PQ 1\n;using\n;using\n" TOP-CHECK
   s" top-parses-row: ambiguous" s" PQ" s" 11" s" 9" s" fix_parses_row" ROW-REFUSED
   s\" package SH\npublic\n: DUP ( n -- n n ) dup ;\n;package\nusing SH\nparses: DUP 1\n;using\n" TOP-CHECK
   s" top-parses-row: shadow" s" DUP" s" 6" s" 9" s" fix_parses_row" ROW-REFUSED
   s\" : NP ( -- ) ;\nparses: NP 1\n" TOP-CHECK
   s" top-parses-row: reads none" s" NP" s" 2" s" 9" s" fix_parses_row" ROW-REFUSED
   s\" : PN ( -- ) parse-name 2drop ;\nparses: PN\n" TOP-CHECK
   s" top-parses-row: no count" s" PN" s" 2" s" 9" s" fix_parses_syntax" ROW-REFUSED
   s\" : PN ( -- ) parse-name 2drop ;\nparses: PN x\n" TOP-CHECK
   s" top-parses-row: count" s" PN" s" 2" s" 9" s" fix_parses_syntax" ROW-REFUSED
   s\" : PN ( -- ) parse-name 2drop ;\nparses: PN -1\n" TOP-CHECK
   s" top-parses-row: negative" s" PN" s" 2" s" 9" s" fix_parses_syntax" ROW-REFUSED
   s\" : PN ( -- ) parse-name 2drop ;\nparses-through: PN 0 ( )\n" TOP-CHECK
   s" top-parses-row: empty list" s" PN" s" 2" s" 17" s" fix_parses_syntax" ROW-REFUSED
   s\" : PN ( -- ) parse-name 2drop ;\nparses-through: PN 0 ;E\n" TOP-CHECK
   s" top-parses-row: unopened" s" PN" s" 2" s" 17" s" fix_parses_syntax" ROW-REFUSED
   s\" : PN ( -- ) parse-name 2drop ;\nparses-through: PN 0 ( ;E\n" TOP-CHECK
   s" top-parses-row: unclosed" s" PN" s" 2" s" 17" s" fix_parses_syntax" ROW-REFUSED
   s\" : PN ( -- ) parse-name 2drop ;\nparses:\n" TOP-CHECK
   s" top-parses-row: no target" s" parses:" s" 2" s" 1" s" fix_parses_syntax" ROW-REFUSED
   s\" : W ( -- ) parses: ;\n" TOP-CHECK
   1 s" top-parses-row: declarer in a body" EXPECT-KIND
   s" top-parses-row: declarer in a body" T-LABEL
   CHECK:VERIFY-OUT$ s" token" s" parses:" PACKET s" code" STRING$ s" E-UNSAFE" T$=
   s\" : W2 ( -- ) checker-parses-row ;\n" TOP-CHECK
   1 s" top-parses-row: registrar in a body" EXPECT-KIND
   s" top-parses-row: registrar in a body" T-LABEL
   CHECK:VERIFY-OUT$ s" token" s" checker-parses-row" PACKET s" code" STRING$ s" E-UNSAFE" T$= ;


\ A names: row whose target names no word, or a word whose input row does not
\ end in the string pair ( ptr u8 n ) a lookup takes, is E-NAMES-ROW at the
\ target, fix_names_row, and one with no target at the keyword,
\ fix_names_syntax; loaded, either target exits 67. A checked body calling the
\ declarer or either word the verifier reaches the checker through is E-UNSAFE.
: NAMES-ROW ( -- )
   s\" : CVT-LOOK ( ptr u8 n -- ) 2drop ;\nnames: CVT-NOSUCH\n" TOP-CHECK
   s" names-row: undefined" s" CVT-NOSUCH" s" E-NAMES-ROW" s" 2" s" 8" REFUSED-AT
   s" names-row: undefined" T-LABEL
   CHECK:VERIFY-OUT$ 0 NTH-PACKET s" repair_class" STRING$ s" fix_names_row" T$=
   s\" : CVT-LOOK ( ptr u8 n -- ) 2drop ;\nnames:\n" TOP-CHECK
   s" names-row: no target" s" names:" s" E-NAMES-ROW" s" 2" s" 1" REFUSED-AT
   s" names-row: no target" T-LABEL
   CHECK:VERIFY-OUT$ 0 NTH-PACKET s" repair_class" STRING$ s" fix_names_syntax" T$=
   s\" names: dup\n" TOP-CHECK
   s" names-row: takes no string" s" dup" s" E-NAMES-ROW" s" 1" s" 8" REFUSED-AT
   s" names-row: takes no string" T-LABEL
   CHECK:VERIFY-OUT$ 0 NTH-PACKET s" repair_class" STRING$ s" fix_names_row" T$=
   s\" : CVT-ROOK ( -- n ) 1 ;\nnames: CVT-ROOK\n" TOP-CHECK
   s" names-row: no inputs" s" CVT-ROOK" s" E-NAMES-ROW" s" 2" s" 8" REFUSED-AT
   s" names-row: no inputs" T-LABEL
   CHECK:VERIFY-OUT$ 0 NTH-PACKET s" repair_class" STRING$ s" fix_names_row" T$=
   s" names-row.f" s\" names: CVT-NOSUCH\n" FIXTURE
   s" names-row: loaded, exit 67" T-LABEL
   PROC-ARGV-ENV-RESET s" --load" ARG+ s" names-row.f" AT$ ARG+
   PROC-ENV-INHERIT-MISSING s" " CLI nip 67 T=
   s" names-none.f" s\" : CVT-ROOK ( -- n ) 1 ;\nnames: CVT-ROOK\n" FIXTURE
   s" names-row: no inputs, loaded, exit 67" T-LABEL
   PROC-ARGV-ENV-RESET s" --load" ARG+ s" names-none.f" AT$ ARG+
   PROC-ENV-INHERIT-MISSING s" " CLI nip 67 T=
   s\" : W ( -- ) names: ;\n" TOP-CHECK
   1 s" names-row: declarer in a body" EXPECT-KIND
   s" names-row: declarer in a body" T-LABEL
   CHECK:VERIFY-OUT$ s" token" s" names:" PACKET s" code" STRING$ s" E-UNSAFE" T$=
   s\" : W2 ( -- ) checker-names-row ;\n" TOP-CHECK
   1 s" names-row: registrar in a body" EXPECT-KIND
   s" names-row: registrar in a body" T-LABEL
   CHECK:VERIFY-OUT$ s" token" s" checker-names-row" PACKET s" code" STRING$ s" E-UNSAFE" T$=
   s\" : W3 ( -- ) checker-named-use ;\n" TOP-CHECK
   1 s" names-row: use publisher in a body" EXPECT-KIND
   s" names-row: use publisher in a body" T-LABEL
   CHECK:VERIFY-OUT$ s" token" s" checker-named-use" PACKET s" code" STRING$ s" E-UNSAFE" T$= ;


\ The rows' checker side, from loaded files that ask the live checker owner
\ (CHECKER-OWNER-ABI) as the verifier does. Each file dies, exit 76, naming the
\ first fact that fails, and prints NAME: ok after the last. A call of either
\ declarer is a word the run reads source for (2) and its tick's context is
\ unknown (-1). After the hook each declarer is its original binding: its
\ intrinsic id and PARSES. A word calling parse-name parses with no id, and
\ parse-name, a primitive, has no visible record a row could be keyed by.
\ The selected-binding query answers 0 0 0 for an unknown, a malformed, a
\ retired, a shadowed and an ambiguous name, and raises nothing. A scope's rows
\ go with its rollback, though the next scope's PN reuses the binding's
\ symbol and record. An owner record that ends before the query's field is
\ refused with E-NCOMP-OWNER, with no fallback.
\ Load the fixture NAME: it exits 0 and prints OK. Its stderr's length.
: WB-LOAD ( ptr u8 n ptr u8 n -- n )
   {: name:ptr nameu:n ok:ptr oku:n :}
   name nameu AT$ {: f:ptr fu:n :}
   PROC-ARGV-ENV-RESET s" --load" ARG+ f fu ARG+
   PROC-ENV-INHERIT-MISSING s" " CLI {: erru:n rc:n :}
   rc 0 T=
   0 OUT CLI-OUT-U @ ok oku CONTAINS? TTRUE
   erru ;


: TOP-PARSES-WHITEBOX ( -- )
   s" parses-top.f" s\" package CVT-WB\n: ASSERT ( bool ptr u8 n -- )\n   {: ok:bool m:ptr mu:n :}\n   ok if exit then m mu 76 die ;\ns\" parses:\" true data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ CHECKER-OWNER-ABI:VERIFY-TOP-OFF + @ execute 2 = s\" parses: call\" ASSERT\ns\" parses:\" false data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ CHECKER-OWNER-ABI:VERIFY-TOP-OFF + @ execute -1 = s\" parses: tick\" ASSERT\ns\" parses-through:\" true data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ CHECKER-OWNER-ABI:VERIFY-TOP-OFF + @ execute 2 = s\" parses-through: call\" ASSERT\ns\" parses-through:\" false data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ CHECKER-OWNER-ABI:VERIFY-TOP-OFF + @ execute -1 = s\" parses-through: tick\" ASSERT\ns\" top: ok\" type cr\n;package\n" FIXTURE
   s" top-parses-whitebox: top" T-LABEL s" parses-top.f" s" top: ok" WB-LOAD drop
   s" parses-binding.f" s\" package SH\npublic\n: DUP ( n -- n n ) dup ;\n;package\npackage A1\npublic\n: PQ ( -- ) parse-name 2drop ;\n;package\npackage A2\npublic\n: PQ ( -- ) parse-name 2drop ;\n;package\npackage CVT-WB\n: ASSERT ( bool ptr u8 n -- )\n   {: ok:bool m:ptr mu:n :}\n   ok if exit then m mu 76 die ;\n: BOUND ( n n n n ptr u8 n -- )\n   {: sym:n eff:n ctl:n want:n m:ptr mu:n :}\n   sym 0 <> m mu ASSERT\n   eff 0 <> m mu ASSERT\n   ctl CHECKER-OWNER-ABI:BINDING-ID-MASK and CHECKER-OWNER-ABI:BINDING-ID-SHIFT rshift want = m mu ASSERT\n   ctl CHECKER-OWNER-ABI:BINDING-PARSES and 0 <> m mu ASSERT ;\n: PRIM ( n n n ptr u8 n -- )\n   {: sym:n eff:n ctl:n m:ptr mu:n :}\n   sym 0 <> m mu ASSERT\n   eff 0= m mu ASSERT\n   ctl CHECKER-OWNER-ABI:BINDING-PARSES and 0 <> m mu ASSERT ;\n: NONE ( n n n ptr u8 n -- )\n   {: sym:n eff:n ctl:n m:ptr mu:n :}\n   sym 0= m mu ASSERT\n   eff 0= m mu ASSERT\n   ctl 0= m mu ASSERT ;\ns\" parses:\" data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ CHECKER-OWNER-ABI:VERIFY-TOP-BINDING-OFF + @ execute CHECKER-OWNER-ABI:BINDING-PARSES-ID s\" parses:\" BOUND\ns\" parses-through:\" data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ CHECKER-OWNER-ABI:VERIFY-TOP-BINDING-OFF + @ execute CHECKER-OWNER-ABI:BINDING-THROUGH-ID s\" parses-through:\" BOUND\ns\" parse-name\" data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ CHECKER-OWNER-ABI:VERIFY-TOP-BINDING-OFF + @ execute s\" parse-name\" PRIM\n: PN ( -- ) parse-name 2drop ;\ns\" PN\" data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ CHECKER-OWNER-ABI:VERIFY-TOP-BINDING-OFF + @ execute 0 s\" PN\" BOUND\ns\" A1:PQ\" data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ CHECKER-OWNER-ABI:VERIFY-TOP-BINDING-OFF + @ execute 0 s\" A1:PQ\" BOUND\ns\" NOSUCH\" data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ CHECKER-OWNER-ABI:VERIFY-TOP-BINDING-OFF + @ execute s\" unknown\" NONE\ns\" A::B\" data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ CHECKER-OWNER-ABI:VERIFY-TOP-BINDING-OFF + @ execute s\" malformed\" NONE\nundefine PN\ns\" PN\" data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ CHECKER-OWNER-ABI:VERIFY-TOP-BINDING-OFF + @ execute s\" retired\" NONE\nusing SH\ns\" DUP\" data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ CHECKER-OWNER-ABI:VERIFY-TOP-BINDING-OFF + @ execute s\" shadowed\" NONE\n;using\nusing A1\nusing A2\ns\" PQ\" data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ CHECKER-OWNER-ABI:VERIFY-TOP-BINDING-OFF + @ execute s\" ambiguous\" NONE\n;using\n;using\ns\" binding: ok\" type cr\n;package\n" FIXTURE
   s" top-parses-whitebox: binding" T-LABEL s" parses-binding.f" s" binding: ok" WB-LOAD drop
   s" parses-nested.f" s\" require src/habu/verify-source.f\nVERIFY:REPORT-DEFERRALS\ns\" CHECKER-SCOPE-START-NEUTRAL\" s\" --\" TRUST\ns\" CHECKER-SCOPE-DONE\" s\" --\" TRUST\npackage CVT-WB\n: ASSERT ( bool ptr u8 n -- )\n   {: ok:bool m:ptr mu:n :}\n   ok if exit then m mu 76 die ;\n: RUN1 ( -- n ) CHECKER-SCOPE-START-NEUTRAL [: s\\\" : PN ( -- ) parse-name 2drop ;\\nparses: PN 1\\n\" s\" a.f\" VERIFY:SOURCE-COMPOSE-IN-SCOPE ;] catch CHECKER-SCOPE-DONE ;\n: RUN2 ( -- n ) CHECKER-SCOPE-START-NEUTRAL [: s\\\" : PN ( -- ) parse-name 2drop ;\\nPN :\\n: G ( -- n ) 1 2 ;\\n\" s\" b.f\" VERIFY:SOURCE-COMPOSE-IN-SCOPE ;] catch CHECKER-SCOPE-DONE ;\n: RUN3 ( -- n ) CHECKER-SCOPE-START-NEUTRAL [: s\\\" : PN ( -- ) parse-name 2drop ;\\nparses: PN 1\\nPN :\\n: G ( -- n ) 1 2 ;\\n\" s\" c.f\" VERIFY:SOURCE-COMPOSE-IN-SCOPE ;] catch CHECKER-SCOPE-DONE ;\nRUN1 0 = s\" declared\" ASSERT\nRUN2 0 = s\" rolled back: G unchecked\" ASSERT\nRUN3 70 = s\" declared again: G refused\" ASSERT\ns\" nested: ok\" type cr\n;package\n" FIXTURE
   s" top-parses-whitebox: nested" T-LABEL s" parses-nested.f" s" nested: ok" WB-LOAD {: erru:n :}
   s" top-parses-whitebox: rolled back, deferred" T-LABEL
   0 ERR erru s\" \"token\":\"PN\",\"file\":\"b.f\",\"line\":2,\"column\":1" CONTAINS? TTRUE
   s" top-parses-whitebox: declared again, deferred" T-LABEL
   0 ERR erru s\" \"token\":\"PN\",\"file\":\"c.f\",\"line\":3,\"column\":1" CONTAINS? TTRUE
   s" parses-field.f" s\" require src/habu/verify-source.f\npackage CVT-WB\n: ASSERT ( bool ptr u8 n -- )\n   {: ok:bool m:ptr mu:n :}\n   ok if exit then m mu 76 die ;\n: RUN ( -- n ) [: s\\\" : PN ( -- ) parse-name 2drop ;\\nparses: PN 1\\n\" s\" a.f\" VERIFY:SOURCE-COMPOSE-IN-SCOPE ;] catch ;\ndata-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ CELL - CELL-VIEW @\nCHECKER-OWNER-ABI:VERIFY-TOP-BINDING-OFF data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ CELL - CELL-VIEW !\nRUN E-NCOMP-OWNER = s\" the missing field refuses\" ASSERT\ndata-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ CELL - CELL-VIEW !\ns\" field: ok\" type cr\n;package\n" FIXTURE
   s" top-parses-whitebox: field" T-LABEL s" parses-field.f" s" field: ok" WB-LOAD drop ;


\ A body naming a word only the run can define is deferred where the checker's
\ judgment of it stops: a name the evaluated text may define, and a create
\ caller's product. The renderer's call is deferred too. Loaded, each refuses
\ that name (E-UNDEFINED, exit 70), so neither is answered verified. A refusal
\ anywhere keeps the file refused. A create caller no row bounds may read any
\ of the rest, so the check discovers nothing after its call.
: DEF-DEFERRED ( -- )
   s\" s\" : CVT-EG ( -- n ) 2 ;\" evaluate\n: CVT-EF ( -- n ) CVT-NOSUCH ;\n" TOP-CHECK
   5 s" def-deferred: evaluate, deferred" EXPECT-KIND
   s" def-deferred: evaluate, two packets" T-LABEL CHECK:VERIFY-OUT$ PACKETS 2 T=
   s" def-deferred: at the name" s" CVT-NOSUCH" s" W-CHECK-DEFERRED" s" 2" s" 19" TOP-PACKET
   s" verdict" STRING$ s" deferred" T$=
   s\" : CVT-MK ( -- ) create 0 , ;\nparses: CVT-MK 1\nCVT-MK CVT-FOO\n: CVT-UF ( -- n ) CVT-FOO @ ;\n" TOP-CHECK
   5 s" def-deferred: create caller, deferred" EXPECT-KIND
   s" def-deferred: create caller, two packets" T-LABEL CHECK:VERIFY-OUT$ PACKETS 2 T=
   s" def-deferred: the call" s" CVT-MK" s" W-CHECK-DEFERRED" s" 3" s" 1" TOP-PACKET
   s" verdict" STRING$ s" deferred" T$=
   s" def-deferred: at the product" s" CVT-FOO" s" W-CHECK-DEFERRED" s" 4" s" 19" TOP-PACKET
   s" verdict" STRING$ s" deferred" T$=
   s\" : CVT-MK ( -- ) create 0 , ;\nCVT-MK CVT-FOO\n: CVT-UF ( -- n ) CVT-FOO @ ;\n" TOP-CHECK
   s" def-deferred: undeclared create caller, the stop" s" CVT-MK" s" 2" s" 1" DEFERRED-ONLY
   s\" : CVT-E ( -- n ) CVT-NOPE ;\ns\" : CVT-EG ( -- n ) 2 ;\" evaluate\n: CVT-EF ( -- n ) CVT-NOSUCH ;\n" TOP-CHECK
   1 s" def-deferred: after a refusal, refused" EXPECT-KIND
   s" def-deferred: after a refusal, three packets" T-LABEL CHECK:VERIFY-OUT$ PACKETS 3 T=
   s" def-deferred: the refusal" T-LABEL
   CHECK:VERIFY-OUT$ s" token" s" CVT-NOPE" PACKET s" code" STRING$ s" E-UNDEFINED" T$=
   s" def-deferred: the deferral" s" CVT-NOSUCH" s" W-CHECK-DEFERRED" s" 3" s" 19" TOP-PACKET
   s" verdict" STRING$ s" deferred" T$= ;

\ The fresh verifier child captures tier 0 before requiring its tooling. A
\ top-level tier switch is executed only by check.f's existing subject run.
: TRUSTED-TICK-ORDER ( -- )
   s\" : CVT-GATE ( -- ) drop ['] patch32 drop CVT-NOT-DEFINED ;\n" TOP-CHECK
   1 s" trusted-tick-order: static tier 0 refusal" EXPECT-KIND
   CHECK:VERIFY-OUT$ s" word" s" cvt-gate" PACKET {: p:n :}
   s" trusted-tick-order: gate code" T-LABEL p s" code" STRING$ s" E-CAP-TRUSTED" T$=
   s" trusted-tick-order: target token" T-LABEL p s" token" STRING$ s" patch32" T$=
   s" trusted-tick-order: target line" T-LABEL p s" line" NUMBER$ s" 1" T$=
   s" trusted-tick-order: target column" T-LABEL p s" column" NUMBER$ s" 28" T$=
   s" trusted-tick-order: rejected verdict" T-LABEL
   p s" verdict" STRING$ s" rejected" T$=
   s\" 1 set-tier\n: CVT-GATE ( -- ) drop ['] patch32 drop ;\n" TOP-CHECK
   5 s" trusted-tick-order: dynamic tier is deferred" EXPECT-KIND
   CHECK:VERIFY-OUT$ s" token" s" patch32" PACKET {: warning:n :}
   s" trusted-tick-order: warning code" T-LABEL
   warning s" code" STRING$ s" W-CHECK-DEFERRED" T$=
   s" trusted-tick-order: warning line" T-LABEL
   warning s" line" NUMBER$ s" 2" T$=
   s" trusted-tick-order: warning column" T-LABEL
   warning s" column" NUMBER$ s" 28" T$=
   s" trusted-tick-order: pre-pass continues to the run" T-LABEL
   s\" 1 set-tier\n: CVT-GATE ( -- ) drop ['] patch32 drop ;\n"
   SUBJ$ s" dynamic-tier.f" GUARD-MS >MS CHECK:PREVERIFY-BYTES
   MATCH result
      ok OF 0= ENDOF
      err OF drop false ENDOF
   ;MATCH TTRUE
   s" dynamic-tier.f" s\" 1 set-tier\n: CVT-GATE ( -- ) drop ['] patch32 drop ;\n" FIXTURE
   CLI-START s" --json-errors" ARG+ s" dynamic-tier.f" AT$ ARG+
   s" " CLI {: erru:n rc:n :}
   s" trusted-tick-order: single live run exits at the checker" T-LABEL rc 70 T=
   s" trusted-tick-order: live stderr is packets" T-LABEL
   0 ERR erru ALL-JSON? TTRUE
   0 ERR erru s" token" s" drop" PACKET {: live:n :}
   s" trusted-tick-order: live tier 1 underflow" T-LABEL
   live s" code" STRING$ s" E-INPUT-UNDERFLOW" T$=
   s" trusted-tick-order: live source line" T-LABEL
   live s" line" NUMBER$ s" 2" T$=
   s" trusted-tick-order: live source column" T-LABEL
   live s" column" NUMBER$ s" 19" T$=
   s" 1 set-tier : CVT-A ( -- ) drop ['] patch32 drop ; : CVT-B ( -- ) CVT-A ;" TOP-CHECK
   5 s" trusted-tick-order: unknown body leaves dependents to the run" EXPECT-KIND
   s" trusted-tick-order: only the uncertain tick is reported" T-LABEL
   CHECK:VERIFY-OUT$ PACKETS 1 T=
   CHECK:VERIFY-OUT$ s" token" s" patch32" PACKET s" code" STRING$
   s" W-CHECK-DEFERRED" T$=
   s" 0 set-tier package CVT-TU public : patch32 ( -- n ) 1 ; ;package using CVT-TU : CVT-SH ( -- ) ['] patch32 drop ; ;using" TOP-CHECK
   1 s" trusted-tick-order: current tick resolution beats uncertainty" EXPECT-KIND
   CHECK:VERIFY-OUT$ s" token" s" patch32" PACKET s" code" STRING$
   s" E-USING-SHADOW-GLOBAL" T$=
   s" : CVT-DOES ( -- ) drop create does> ( -- ) drop ['] patch32 drop ;" TOP-CHECK
   1 s" trusted-tick-order: does clause gate precedes parent check" EXPECT-KIND
   CHECK:VERIFY-OUT$ s" word" s" cvt-does;does" PACKET s" code" STRING$
   s" E-CAP-TRUSTED" T$=
   s" : CVT-DOES ( -- ) CVT-MISSING create does> ( -- ) ['] patch32 drop ;" TOP-CHECK
   1 s" trusted-tick-order: tier 0 parent compile refusal wins" EXPECT-KIND
   CHECK:VERIFY-OUT$ s" word" s" cvt-does" PACKET s" code" STRING$
   s" E-UNDEFINED" T$=
   s" : CVT-DOES ( -- ) ['] patch32 drop create does> ( -- ) CVT-MISSING ;" TOP-CHECK
   1 s" trusted-tick-order: parent gate precedes later clause refusal" EXPECT-KIND
   CHECK:VERIFY-OUT$ s" word" s" cvt-does" PACKET s" code" STRING$
   s" E-CAP-TRUSTED" T$=
   s" 0 set-tier : CVT-DOES ( -- ) ['] patch32 drop create does> ( -- ) CVT-MISSING ;" TOP-CHECK
   5 s" trusted-tick-order: unknown parent tick defers clause" EXPECT-KIND
   CHECK:VERIFY-OUT$ s" token" s" patch32" PACKET s" code" STRING$
   s" W-CHECK-DEFERRED" T$=
   s" 0 set-tier : CVT-DOES ( -- ) ['] patch32 drop create does> CVT-MISSING ;" TOP-CHECK
   5 s" trusted-tick-order: unknown parent stops before malformed clause" EXPECT-KIND
   s" trusted-tick-order: unknown parent has one warning" T-LABEL
   CHECK:VERIFY-OUT$ PACKETS 1 T=
   CHECK:VERIFY-OUT$ s" token" s" patch32" PACKET s" code" STRING$
   s" W-CHECK-DEFERRED" T$=
   S\" s\" : CVT-GENERATED ( -- ) ;\" evaluate : CVT-DOES ( -- ) create does> ( -- ) drop CVT-GENERATED ;" TOP-CHECK
   5 s" trusted-tick-order: clause warning retains its pin after parent scan" EXPECT-KIND
   CHECK:VERIFY-OUT$ s" token" s" CVT-GENERATED" PACKET {: clause-warning:n :}
   s" trusted-tick-order: clause warning span" T-LABEL
   clause-warning s" line" NUMBER$ s" 1" T$=
   clause-warning s" column" NUMBER$ s" 82" T$=
   clause-warning s" code" STRING$
   s" W-CHECK-DEFERRED" T$=
   S\" s\" : CVT-GENERATED ( -- ) ;\" evaluate\n: CVT-DOES ( -- ) CVT-GENERATED create does> ( -- ) CVT-GENERATED ;" TOP-CHECK
   5 s" trusted-tick-order: parent and clause both defer" EXPECT-KIND
   s" trusted-tick-order: one warning for the definition, one for the renderer" T-LABEL
   CHECK:VERIFY-OUT$ PACKETS 2 T=
   CHECK:VERIFY-OUT$ s" token" s" CVT-GENERATED" PACKET {: parent-warning:n :}
   s" trusted-tick-order: parent warning wins" T-LABEL
   parent-warning s" line" NUMBER$ s" 2" T$=
   parent-warning s" column" NUMBER$ s" 19" T$=
   parent-warning s" code" STRING$ s" W-CHECK-DEFERRED" T$=
   s" 0 set-tier : CVT-DOES ( -- ) drop create does> ( -- ) drop ['] patch32 drop ;" TOP-CHECK
   5 s" trusted-tick-order: unknown does clause defers parent check" EXPECT-KIND
   CHECK:VERIFY-OUT$ s" token" s" patch32" PACKET s" code" STRING$
   s" W-CHECK-DEFERRED" T$=
   s" 0 set-tier : CVT-DOES ( -- ) CVT-MISSING create does> ( -- ) ['] patch32 drop ;" TOP-CHECK
   1 s" trusted-tick-order: earlier parent compile refusal wins" EXPECT-KIND
   CHECK:VERIFY-OUT$ s" word" s" cvt-does" PACKET s" code" STRING$
   s" E-UNDEFINED" T$=
   s" : CVT-PRIOR ( -- ) ['] patch32 drop ; using CVT-TU : CVT-LATER ( -- ) ['] patch32 drop ; ;using" TOP-CHECK
   1 s" trusted-tick-order: later using cannot replace earlier gate" EXPECT-KIND
   CHECK:VERIFY-OUT$ s" word" s" cvt-prior" PACKET s" code" STRING$
   s" E-CAP-TRUSTED" T$= ;


\ Bytes the load accepts verify, with no packet.
: LOADS-CLEAN ( ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n label:ptr labelu:n :}
   src srcu TOP-CHECK 0 label labelu EXPECT-KIND
   label labelu T-LABEL CHECK:VERIFY-OUT$ PACKETS 0 T= ;


\ A word that renders source reads only the text it renders: loaded, `: R ( -- )
\ s" char" evaluate-closed ; R X` stops at the rendered `char`, exit 74, as it
\ does with no X. So the tokens after `evaluate` resolve as any others do, and
\ only a name the text may define is the run's: its stretch opens at CVT-EV.
\ The text may define a package too, so a qualified name is the run's as a bare
\ one is, whether `evaluate` renders it or a word that reaches the loader under
\ `catch` (test/using-test.f). Each subject loads, exit 0. A reached renderer
\ itself leaves source unchecked even when every later token resolves, so it
\ is deferred at its token, a definer's call a definer arm takes among them.
: TOP-RENDERS ( -- )
   s\" s\" : CVT-EV ( -- n ) 1 ;\" evaluate 1 drop\n" TOP-CHECK
   s" top-renders: at the renderer" s" evaluate" s" 1" s" 27" DEFERRED-ONLY
   s\" s\" : CVT-EV ( -- n ) 1 ;\" evaluate CVT-EV drop\n" TOP-CHECK
   5 s" top-renders: a product, deferred" EXPECT-KIND
   s" top-renders: two packets" T-LABEL CHECK:VERIFY-OUT$ PACKETS 2 T=
   s" top-renders: the renderer first" 0 s" evaluate" s" 1" s" 27" DEFERRED-NTH
   s" top-renders: then the product" 1 s" CVT-EV" s" 1" s" 36" DEFERRED-NTH
   s\" s\" package CVT-RP public : CVT-RW ( -- n ) 1 ; ;package\" evaluate\nCVT-RP:CVT-RW drop\n" TOP-CHECK
   5 s" top-renders: a qualified product, deferred" EXPECT-KIND
   s" top-renders: qualified, two packets" T-LABEL CHECK:VERIFY-OUT$ PACKETS 2 T=
   s" top-renders: qualified, the renderer first" 0 s" evaluate" s" 1" s" 58" DEFERRED-NTH
   s" top-renders: at the qualified product" 1 s" CVT-RP:CVT-RW" s" 2" s" 1" DEFERRED-NTH
   s\" require lib/prelude.f\nTYPED-VARIABLE CVT-UA ptr u8   variable CVT-UU\n: CVT-UGO ( -- ) CVT-UA @ CVT-UU @ INCLUDE-EVALUATE ;\n: CVT-UCATCH ( ptr u8 n -- n ) CVT-UU ! CVT-UA ! [: CVT-UGO ;] catch ;\ns\" package CVT-UQ public : CVT-UR ( -- n ) 42 ; ;package\" CVT-UCATCH drop\nCVT-UQ:CVT-UR drop\n"
   TOP-CHECK
   5 s" top-renders: under catch, deferred" EXPECT-KIND
   s" top-renders: under catch, two packets" T-LABEL CHECK:VERIFY-OUT$ PACKETS 2 T=
   s" top-renders: under catch, the renderer first" 0 s" CVT-UCATCH" s" 5" s" 59" DEFERRED-NTH
   s" top-renders: under catch, at the product" 1 s" CVT-UQ:CVT-UR" s" 6" s" 1" DEFERRED-NTH
   s\" : CVT-LD ( n -- ) create , s\" : CVT-LZ ( -- n ) 3 ;\" evaluate-closed does> ( -- n ) @ ;\n1 CVT-LD CVT-LX\nCVT-LX drop\n" TOP-CHECK
   s" top-renders: a learned definer's call" s" CVT-LD" s" 2" s" 3" DEFERRED-ONLY ;


\ A source whose type is made at load time must really load with the same
\ engine the verifier child runs. Keep the disk and buffer bytes identical.
: TYPE-LOAD-CHECK ( ptr u8 n ptr u8 n ptr u8 n -- CHECK:verdict )
   {: src:ptr srcu:n name:ptr nameu:n label:ptr labelu:n :}
   name nameu src srcu FIXTURE
   label labelu T-LABEL name nameu AT$ NATIVE-RC 0 T=
   src srcu name nameu GUARD-MS CHECK-AS ;

: TYPE-DEFERRED-PACKET ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: label:ptr labelu:n tok:ptr toku:n line:ptr lineu:n col:ptr colu:n :}
   CHECK:VERIFY-OUT$ s" token" tok toku PACKET {: p:n :}
   label labelu T-LABEL p s" code" STRING$ s" W-CHECK-DEFERRED" T$=
   label labelu T-LABEL p s" file" STRING$ SUBJ$ T$=
   label labelu T-LABEL p s" line" NUMBER$ line lineu T$=
   label labelu T-LABEL p s" column" NUMBER$ col colu T$=
   label labelu T-LABEL p s" verdict" STRING$ s" deferred" T$= ;

\ All four sources load. The verifier cannot execute their reached renderer or
\ original registrar, so the declaration depending on the new type is
\ deferred at that type, and a renderer's call is deferred at itself.
: TYPE-DEFERRED-DECLS ( -- )
   s\" s\" STRUCTURE cbx 0 FIELD v n ;STRUCTURE\" INCLUDE-EVALUATE\n: CVT-G ( cbx -- cbx ) ;\n"
   s" type-sig-rendered.f" s" type-deferred: signature loads" TYPE-LOAD-CHECK
   5 s" type-deferred: signature defers" EXPECT-KIND
   s" type-deferred: signature has two packets" T-LABEL CHECK:VERIFY-OUT$ PACKETS 2 T=
   s" type-deferred: signature renderer" s" INCLUDE-EVALUATE" s" 1" s" 42" TYPE-DEFERRED-PACKET
   s" type-deferred: signature type" s" cbx" s" 2" s" 11" TYPE-DEFERRED-PACKET
   s\" s\" STRUCTURE cbx 0 FIELD v n ;STRUCTURE\" INCLUDE-EVALUATE\nSTRUCTURE cob 0 FIELD b cbx ;STRUCTURE\n"
   s" type-field-rendered.f" s" type-deferred: field loads" TYPE-LOAD-CHECK
   5 s" type-deferred: field defers" EXPECT-KIND
   s" type-deferred: field has two packets" T-LABEL CHECK:VERIFY-OUT$ PACKETS 2 T=
   s" type-deferred: field type" s" cbx" s" 2" s" 25" TYPE-DEFERRED-PACKET
   s\" s\" STRUCTURE cbx 1 FIELD v a ;STRUCTURE\" INCLUDE-EVALUATE\nSTRUCTURE cob 0 FIELD b cbx<n> ;STRUCTURE\n"
   s" type-generic-rendered.f" s" type-deferred: generic field loads" TYPE-LOAD-CHECK
   5 s" type-deferred: generic field defers" EXPECT-KIND
   s" type-deferred: generic field has two packets" T-LABEL CHECK:VERIFY-OUT$ PACKETS 2 T=
   s" type-deferred: generic field type" s" cbx<n>" s" 2" s" 25" TYPE-DEFERRED-PACKET
   s\" s\" chz\" s\" 0 VARIANT first ;VARIANT VARIANT second ;VARIANT\" CHECKER-DEFSUM\n: CVT-F ( -- chz ) CONSTRUCT chz first ;\n"
   s" type-sum-registered.f" s" type-deferred: sum loads" TYPE-LOAD-CHECK
   5 s" type-deferred: sum defers" EXPECT-KIND
   s" type-deferred: sum has one packet" T-LABEL CHECK:VERIFY-OUT$ PACKETS 1 T=
   s" type-deferred: sum signature type" s" chz" s" 2" s" 14" TYPE-DEFERRED-PACKET

   s\" : CVT-REG ( -- ) s\" chz\" s\" 0 VARIANT first ;VARIANT VARIANT second ;VARIANT\" CHECKER-DEFSUM ;\nCVT-REG\n: CVT-F ( -- chz ) CONSTRUCT chz first ;\n"
   s" type-registrar-wrapper.f" s" type-deferred: registrar wrapper loads" TYPE-LOAD-CHECK
   5 s" type-deferred: registrar wrapper defers" EXPECT-KIND
   s" type-deferred: registrar wrapper type" s" chz" s" 3" s" 14" TYPE-DEFERRED-PACKET

   s\" s\" STRUCTURE cbx 0 FIELD v n ;STRUCTURE\" INCLUDE-EVALUATE\nTYPED-VARIABLE CVT-ST cbx\n"
   s" type-storage-rendered.f" s" type-deferred: storage loads" TYPE-LOAD-CHECK
   5 s" type-deferred: storage defers" EXPECT-KIND
   s" type-deferred: storage has two packets" T-LABEL CHECK:VERIFY-OUT$ PACKETS 2 T=
   s" type-deferred: stored type" s" cbx" s" 2" s" 23" TYPE-DEFERRED-PACKET

   s\" s\" STRUCTURE cbx 0 FIELD v n ;STRUCTURE\" INCLUDE-EVALUATE\nTYPED-VARIABLE CVT-ST cbx\n: CVT-USE ( -- ) CVT-ST drop ;\n"
   s" type-storage-use.f" s" type-deferred: storage dependency loads" TYPE-LOAD-CHECK
   5 s" type-deferred: storage dependency defers" EXPECT-KIND
   s" type-deferred: storage dependency has three packets" T-LABEL CHECK:VERIFY-OUT$ PACKETS 3 T=
   s" type-deferred: storage dependency type" s" cbx" s" 2" s" 23" TYPE-DEFERRED-PACKET
   s" type-deferred: storage dependency word" s" CVT-ST" s" 3" s" 18" TYPE-DEFERRED-PACKET

   s\" s\" STRUCTURE cbx 0 FIELD v n ;STRUCTURE\" INCLUDE-EVALUATE\nVALUE-RECORD CVT-VR x cbx END-VALUE-RECORD\n"
   s" type-record-rendered.f" s" type-deferred: record loads" TYPE-LOAD-CHECK
   5 s" type-deferred: record defers" EXPECT-KIND
   s" type-deferred: record has two packets" T-LABEL CHECK:VERIFY-OUT$ PACKETS 2 T=
   s" type-deferred: record field" s" cbx" s" 2" s" 23" TYPE-DEFERRED-PACKET

   s\" s\" STRUCTURE cbx 0 FIELD v n ;STRUCTURE\" INCLUDE-EVALUATE\nSUMTYPE cvs 0 VARIANT one cbx ;VARIANT ;SUMTYPE\n"
   s" type-variant-rendered.f" s" type-deferred: variant loads" TYPE-LOAD-CHECK
   5 s" type-deferred: variant defers" EXPECT-KIND
   s" type-deferred: variant has two packets" T-LABEL CHECK:VERIFY-OUT$ PACKETS 2 T=
   s" type-deferred: variant payload" s" cbx" s" 2" s" 27" TYPE-DEFERRED-PACKET

   s\" s\" STRUCTURE cbx 0 FIELD v n ;STRUCTURE\" INCLUDE-EVALUATE\nPRODUCT cvp 0 FIELD f cbx ;PRODUCT\n"
   s" type-product-rendered.f" s" type-deferred: product loads" TYPE-LOAD-CHECK
   5 s" type-deferred: product defers" EXPECT-KIND
   s" type-deferred: product has two packets" T-LABEL CHECK:VERIFY-OUT$ PACKETS 2 T=
   s" type-deferred: product field" s" cbx" s" 2" s" 23" TYPE-DEFERRED-PACKET

   s\" s\" STRUCTURE cbx 1 FIELD v a ;STRUCTURE\" INCLUDE-EVALUATE\nSUMTYPE cvs 0 VARIANT one cbx<n> ;VARIANT ;SUMTYPE\n"
   s" type-variant-generic.f" s" type-deferred: applied payload loads" TYPE-LOAD-CHECK
   5 s" type-deferred: applied payload defers" EXPECT-KIND
   s" type-deferred: applied payload has two packets" T-LABEL CHECK:VERIFY-OUT$ PACKETS 2 T=
   s" type-deferred: applied payload type" s" cbx" s" 2" s" 27" TYPE-DEFERRED-PACKET

   s\" s\" STRUCTURE cbx 0 FIELD v n ;STRUCTURE\" INCLUDE-EVALUATE\nSUMTYPE cvs 0 VARIANT one [ cbx -- n ] ;VARIANT ;SUMTYPE\n"
   s" type-variant-quote.f" s" type-deferred: quoted payload loads" TYPE-LOAD-CHECK
   5 s" type-deferred: quoted payload defers" EXPECT-KIND
   s" type-deferred: quoted payload has two packets" T-LABEL CHECK:VERIFY-OUT$ PACKETS 2 T=
   s" type-deferred: quoted payload type" s" cbx" s" 2" s" 29" TYPE-DEFERRED-PACKET

   s\" s\" STRUCTURE cbx 0 FIELD v n ;STRUCTURE\" INCLUDE-EVALUATE\nPRODUCT cvp 0 FIELD f ptr cbx ;PRODUCT\n"
   s" type-product-pointer.f" s" type-deferred: pointer field loads" TYPE-LOAD-CHECK
   5 s" type-deferred: pointer field defers" EXPECT-KIND
   s" type-deferred: pointer field has two packets" T-LABEL CHECK:VERIFY-OUT$ PACKETS 2 T=
   s" type-deferred: pointer field type" s" cbx" s" 2" s" 27" TYPE-DEFERRED-PACKET

   s\" s\" NEWTYPE cbx 0\" INCLUDE-EVALUATE\nCAST: CVT-C ( n -- cbx )\n"
   s" type-cast-rendered.f" s" type-deferred: cast loads" TYPE-LOAD-CHECK
   5 s" type-deferred: cast defers" EXPECT-KIND
   s" type-deferred: cast has two packets" T-LABEL CHECK:VERIFY-OUT$ PACKETS 2 T=
   s" type-deferred: cast type" s" cbx" s" 2" s" 20" TYPE-DEFERRED-PACKET

   s\" s\" package CVT-L public DEFLINEAR CVT-L:tok ;package\" INCLUDE-EVALUATE\npackage CVT-L LINEAR: CVT-M ( n -- CVT-L:tok ) ;package\n"
   s" type-linear-rendered.f" s" type-deferred: linear loads" TYPE-LOAD-CHECK
   5 s" type-deferred: linear defers" EXPECT-KIND
   s" type-deferred: linear has two packets" T-LABEL CHECK:VERIFY-OUT$ PACKETS 2 T=
   s" type-deferred: linear type" s" CVT-L:tok" s" 2" s" 36" TYPE-DEFERRED-PACKET

   s\" s\" STRUCTURE cbx 0 FIELD v n ;STRUCTURE\" INCLUDE-EVALUATE\ndefer CVT-D ( cbx -- cbx )\n"
   s" type-defer-rendered.f" s" type-deferred: defer loads" TYPE-LOAD-CHECK
   5 s" type-deferred: defer signature defers" EXPECT-KIND
   s" type-deferred: defer has two packets" T-LABEL CHECK:VERIFY-OUT$ PACKETS 2 T=
   s" type-deferred: defer type" s" cbx" s" 2" s" 15" TYPE-DEFERRED-PACKET

   s\" s\" STRUCTURE cbx 0 FIELD v n ;STRUCTURE\" INCLUDE-EVALUATE\nSUMTYPE cvs 0 VARIANT one cbx ;VARIANT VARIANT one n ;VARIANT ;SUMTYPE\n" TOP-CHECK
   1 s" type-deferred: duplicate variant refuses after missing type" EXPECT-KIND
   s" type-deferred: duplicate variant stays rejected" T-LABEL
   CHECK:VERIFY-OUT$ s" token" s" one" PACKET s" verdict" STRING$ s" rejected" T$=

   s\" s\" STRUCTURE cbx 0 FIELD v n ;STRUCTURE\" INCLUDE-EVALUATE\nPRODUCT cvp 0 FIELD f cbx FIELD f n ;PRODUCT\n" TOP-CHECK
   1 s" type-deferred: duplicate field refuses after missing type" EXPECT-KIND
   s" type-deferred: duplicate field stays rejected" T-LABEL
   CHECK:VERIFY-OUT$ s" token" s" f" PACKET s" verdict" STRING$ s" rejected" T$=

   s\" s\" STRUCTURE cbx 0 FIELD v n ;STRUCTURE\" INCLUDE-EVALUATE\nSUMTYPE cvs 0 VARIANT one cbx<n ;VARIANT ;SUMTYPE\n" TOP-CHECK
   1 s" type-deferred: malformed applied payload refuses" EXPECT-KIND
   s" type-deferred: malformed applied payload stays rejected" T-LABEL
   CHECK:VERIFY-OUT$ s" token" s" ;VARIANT" PACKET s" verdict" STRING$ s" rejected" T$=

   s\" s\" STRUCTURE cbx 0 FIELD v n ;STRUCTURE\" INCLUDE-EVALUATE\nSTRUCTURE cknown 1 FIELD v a ;STRUCTURE\nSUMTYPE cvs 0 VARIANT one cbx<cknown> ;VARIANT ;SUMTYPE\n" TOP-CHECK
   1 s" type-deferred: known nested arity refuses" EXPECT-KIND
   s" type-deferred: known nested arity stays rejected" T-LABEL
   CHECK:VERIFY-OUT$ s" token" s" cknown" PACKET s" verdict" STRING$ s" rejected" T$= ;

: TYPE-DEFERRED-CONTROLS ( -- )
   s\" package CVT-SH private\n: INCLUDE-EVALUATE ( -- ) ;\nINCLUDE-EVALUATE\n: CVT-G ( cbx -- cbx ) ;\n;package\n" TOP-CHECK
   1 s" type-deferred: shadowed nonproducer refuses" EXPECT-KIND
   s" type-deferred: shadowed nonproducer code" T-LABEL
   CHECK:VERIFY-OUT$ s" token" s" cbx" PACKET s" code" STRING$ s" E-UNKNOWN-SIGNATURE-TYPE" T$=

   s\" package CVT-A private\ns\" cbx\" s\" 0 VARIANT first ;VARIANT\" CHECKER-DEFSUM\n;package\n: CVT-G ( CVT-B:cbx -- CVT-B:cbx ) ;\n" TOP-CHECK
   1 s" type-deferred: private registrar cannot supply another package" EXPECT-KIND
   s" type-deferred: other package type refuses" T-LABEL
   CHECK:VERIFY-OUT$ s" token" s" CVT-B:cbx" PACKET s" code" STRING$ s" E-UNKNOWN-SIGNATURE-TYPE" T$=

   s\" : CVT-G ( cbx -- cbx ) ;\n" TOP-CHECK
   1 s" type-deferred: no producer refuses" EXPECT-KIND
   s" type-deferred: no producer code" T-LABEL
   CHECK:VERIFY-OUT$ s" token" s" cbx" PACKET s" code" STRING$ s" E-UNKNOWN-SIGNATURE-TYPE" T$=
   s\" : CVT-RENDER ( -- ) s\" STRUCTURE cbx 0 FIELD v n ;STRUCTURE\" INCLUDE-EVALUATE ;\n: CVT-G ( cbx -- cbx ) ;\n" TOP-CHECK
   1 s" type-deferred: uncalled renderer refuses" EXPECT-KIND
   s" type-deferred: uncalled renderer code" T-LABEL
   CHECK:VERIFY-OUT$ s" token" s" cbx" PACKET s" code" STRING$ s" E-UNKNOWN-SIGNATURE-TYPE" T$=
   s\" : CVT-RENDER ( -- ) s\" STRUCTURE cbx 0 FIELD v n ;STRUCTURE\" INCLUDE-EVALUATE ;\n' CVT-RENDER drop\n: CVT-G ( cbx -- cbx ) ;\n" TOP-CHECK
   1 s" type-deferred: ticked renderer refuses" EXPECT-KIND
   s" type-deferred: ticked renderer code" T-LABEL
   CHECK:VERIFY-OUT$ s" token" s" cbx" PACKET s" code" STRING$ s" E-UNKNOWN-SIGNATURE-TYPE" T$=

   s\" s\" STRUCTURE cbx 0 FIELD v n ;STRUCTURE\" INCLUDE-EVALUATE\n: CVT-G ( cbx -- cbx ) ;\n: CVT-BAD ( n -- n n ) drop ;\n" TOP-CHECK
   1 s" type-deferred: later mismatch refuses" EXPECT-KIND
   s" type-deferred: later mismatch has three packets" T-LABEL CHECK:VERIFY-OUT$ PACKETS 3 T=
   s" type-deferred: prior type" s" cbx" s" 2" s" 11" TYPE-DEFERRED-PACKET
   s" type-deferred: later mismatch code" T-LABEL
   CHECK:VERIFY-OUT$ s" token" s" drop" PACKET s" code" STRING$ s" E-MISMATCH" T$=

   s\" s\" STRUCTURE cbx 0 FIELD v n ;STRUCTURE\" INCLUDE-EVALUATE\npackage CVT-A public STRUCTURE amb 0 FIELD v n ;STRUCTURE ;package\npackage CVT-B public STRUCTURE amb 0 FIELD v n ;STRUCTURE ;package\n: CVT-BAD ( amb -- amb ) ;\n" TOP-CHECK
   1 s" type-deferred: ambiguous family refuses" EXPECT-KIND
   s" type-deferred: ambiguous family stays rejected" T-LABEL
   CHECK:VERIFY-OUT$ s" token" s" amb" PACKET s" verdict" STRING$ s" rejected" T$=
   s\" s\" STRUCTURE cbx 1 FIELD v a ;STRUCTURE\" INCLUDE-EVALUATE\n: CVT-BAD ( cbx<n -- cbx<n ) ;\n" TOP-CHECK
   1 s" type-deferred: unclosed generic refuses" EXPECT-KIND
   s" type-deferred: unclosed generic stays rejected" T-LABEL
   CHECK:VERIFY-OUT$ s" token" s" --" PACKET s" verdict" STRING$ s" rejected" T$= ;

: TYPE-DEFERRED-TRUSTED ( -- )
   s\" s\" STRUCTURE cbx 0 FIELD v n ;STRUCTURE\" INCLUDE-EVALUATE\nTRUSTED: CVT-T ( cbx -- cbx ) create does> ( -- n ) drop 7 ;\n: CVT-NEXT ( -- n ) 1 ;\n"
   s" type-trusted-rendered.f" s" type-deferred: trusted loads" TYPE-LOAD-CHECK
   5 s" type-deferred: trusted signature defers" EXPECT-KIND
   s" type-deferred: trusted has two packets" T-LABEL CHECK:VERIFY-OUT$ PACKETS 2 T=
   s" type-deferred: trusted signature type" s" cbx" s" 2" s" 18" TYPE-DEFERRED-PACKET
   s" type-deferred: trusted parent absent" T-LABEL
   CHECK:VERIFY-DEFS$ s" word" s" CVT-T" PACKET 0 < TTRUE
   s" type-deferred: trusted clause absent" T-LABEL
   CHECK:VERIFY-DEFS$ s" word" s" CVT-T;does" PACKET 0 < TTRUE
   s" type-deferred: following declaration present" T-LABEL
   CHECK:VERIFY-DEFS$ s" word" s" CVT-NEXT" PACKET 0 >= TTRUE ;

: TYPE-DEFERRED-DIRECT ( -- )
   s\" s\" cbx\" CHECKER-DEFLINEAR\n: CVT-KEEP ( cbx -- cbx ) ;\n"
   s" type-linear-direct.f" s" type-deferred: direct linear loads" TYPE-LOAD-CHECK
   5 s" type-deferred: direct linear defers" EXPECT-KIND
   s" type-deferred: direct linear type" s" cbx" s" 2" s" 14" TYPE-DEFERRED-PACKET
   s\" s\" cbx\" s\" f n\" CHECKER-DEFRECORD\n: CVT-KEEP ( cbx -- cbx ) ;\n"
   s" type-record-direct.f" s" type-deferred: direct record loads" TYPE-LOAD-CHECK
   5 s" type-deferred: direct record defers" EXPECT-KIND
   s" type-deferred: direct record type" s" cbx" s" 2" s" 14" TYPE-DEFERRED-PACKET ;

: TYPE-DEFERRED-REVIEW ( -- )
   s\" s\" STRUCTURE cbx 0 FIELD v n ;STRUCTURE\" INCLUDE-EVALUATE\nSUMTYPE cvs 0 VARIANT one a ;VARIANT ;SUMTYPE\n" TOP-CHECK
   1 s" type-deferred: known payload letter arity refuses" EXPECT-KIND
   s" type-deferred: known payload letter stays rejected" T-LABEL
   CHECK:VERIFY-OUT$ s" token" s" a" PACKET s" verdict" STRING$ s" rejected" T$=

   s\" s\" STRUCTURE cbx 0 FIELD v n ;STRUCTURE\" INCLUDE-EVALUATE\nSUMTYPE cvs 0 VARIANT one n ;VARIANT ;SUMTYPE\n" TOP-CHECK
   5 s" type-deferred: known payload scalar has no type uncertainty" EXPECT-KIND
   s" type-deferred: known payload scalar has only the renderer's packet" T-LABEL
   CHECK:VERIFY-OUT$ PACKETS 1 T=

   s\" s\" STRUCTURE cbx 0 FIELD v n ;STRUCTURE\" INCLUDE-EVALUATE\nSUMTYPE cvs 0 VARIANT one bool ;VARIANT ;SUMTYPE\n" TOP-CHECK
   5 s" type-deferred: known payload bool has no type uncertainty" EXPECT-KIND
   s" type-deferred: known payload bool has only the renderer's packet" T-LABEL
   CHECK:VERIFY-OUT$ PACKETS 1 T=

   s\" s\" STRUCTURE cbx 0 FIELD v n ;STRUCTURE\" INCLUDE-EVALUATE\nSTRUCTURE bad 0 FIELD x cbx FIELD x n ;STRUCTURE\n" TOP-CHECK
   1 s" type-deferred: duplicate structure field refuses" EXPECT-KIND
   s" type-deferred: duplicate structure field stays rejected" T-LABEL
   CHECK:VERIFY-OUT$ s" token" s" x" PACKET s" verdict" STRING$ s" rejected" T$=

   s\" s\" STRUCTURE cbx 0 FIELD v n ;STRUCTURE\" INCLUDE-EVALUATE\nENUM bad 0 VARIANT one FIELD x cbx FIELD x n ;VARIANT ;ENUM\n" TOP-CHECK
   1 s" type-deferred: duplicate enum field refuses" EXPECT-KIND
   s" type-deferred: duplicate enum field stays rejected" T-LABEL
   CHECK:VERIFY-OUT$ s" token" s" x" PACKET s" verdict" STRING$ s" rejected" T$=

   s\" s\" STRUCTURE cbx 0 FIELD v n ;STRUCTURE\" INCLUDE-EVALUATE\nSTRUCTURE bad 0 FIELD BAD cbx ;STRUCTURE\n" TOP-CHECK
   1 s" type-deferred: reserved structure field refuses" EXPECT-KIND
   s" type-deferred: reserved structure field stays rejected" T-LABEL
   CHECK:VERIFY-OUT$ s" token" s" BAD" PACKET s" verdict" STRING$ s" rejected" T$=

   s\" package CVT-P private STRUCTURE hidden 0 FIELD v n ;STRUCTURE ;package\ns\" STRUCTURE cbx 0 FIELD v n ;STRUCTURE\" INCLUDE-EVALUATE\n: CVT-BAD ( CVT-P:hidden -- CVT-P:hidden ) ;\n" TOP-CHECK
   1 s" type-deferred: other private family refuses" EXPECT-KIND
   s" type-deferred: other private family code" T-LABEL
   CHECK:VERIFY-OUT$ s" token" s" CVT-P:hidden" PACKET s" code" STRING$ s" E-UNKNOWN-SIGNATURE-TYPE" T$=

   s\" s\" STRUCTURE cbx 0 FIELD v n ;STRUCTURE\" INCLUDE-EVALUATE\nCAST: CVT-BAD ( n n -- cbx )\n" TOP-CHECK
   1 s" type-deferred: cast known row shape refuses" EXPECT-KIND
   s" type-deferred: cast known row shape stays rejected" T-LABEL
   CHECK:VERIFY-OUT$ s" verdict" s" rejected" PACKET 0 >= TTRUE

   s\" s\" NEWTYPE cbx 0\" INCLUDE-EVALUATE\nCAST: CVT-BAD ( R n -- S cbx )\n" TOP-CHECK
   1 s" type-deferred: cast known tail mismatch refuses" EXPECT-KIND
   s" type-deferred: cast known tail mismatch stays rejected" T-LABEL
   CHECK:VERIFY-OUT$ s" verdict" s" rejected" PACKET 0 >= TTRUE

   s\" s\" STRUCTURE cbx 0 FIELD v n ;STRUCTURE\" INCLUDE-EVALUATE\n0 TYPED-BUFFER CVT-BAD cbx\n" TOP-CHECK
   1 s" type-deferred: buffer known bad count refuses" EXPECT-KIND
   s" type-deferred: buffer known bad count stays rejected" T-LABEL
   CHECK:VERIFY-OUT$ s" verdict" s" rejected" PACKET 0 >= TTRUE ;

\ A reached renderer makes the type at load time. TRUST changes a seen word's
\ effect, and a view cast depends on that type too: the quiet scan must not
\ publish an unresolved row or use the old effect to certify the next body.
\ The complete sources below load-refuse at that next body, exit 70.
: TYPE-DEFERRED-ROWS ( -- )
   s\" require lib/type/deftype.f\n: CVT-MAKE ( -- ) s\" DEFTYPE CVT-NOM\" evaluate-closed ;\nCVT-MAKE\n: CVT-K ( -- n ) 1 ;\ns\" CVT-K\" s\" -- cvt-nom\" TRUST\n: CVT-AFTER ( -- n ) CVT-K ;\n"
   {: src:ptr srcu:n :}
   s" type-trust-rendered.f" src srcu FIXTURE
   s" type-deferred: asserted effect changes at load" T-LABEL
   s" type-trust-rendered.f" AT$ NATIVE-RC 70 T=
   src srcu TOP-CHECK
   5 s" type-deferred: asserted effect defers" EXPECT-KIND
   s" type-deferred: renderer, assertion and dependent body" T-LABEL CHECK:VERIFY-OUT$ PACKETS 3 T=
   s" type-deferred: assertion type" s" cvt-nom" s" 5" s" 17" TYPE-DEFERRED-PACKET
   s" type-deferred: old effect is unavailable" T-LABEL
   CHECK:VERIFY-OUT$ s" token" s" CVT-K" PACKET s" code" STRING$ s" W-CHECK-DEFERRED" T$=

   s\" require lib/type/deftype.f\npackage CVT-VIEW\n: CVT-MAKE ( -- ) s\" DEFTYPE CVT-NOM\" evaluate-closed ;\nCVT-MAKE\nCAST: CVT-UNPACK ( read-view<p,q,cvt-nom> -- ptr u8 n )\n: CVT-AFTER ( read-view<p,q,n> -- ptr u8 n ) CVT-UNPACK ;\n;package\n"
   {: vsrc:ptr vsrcu:n :}
   s" type-view-rendered.f" vsrc vsrcu FIXTURE
   s" type-deferred: view element differs at load" T-LABEL
   s" type-view-rendered.f" AT$ NATIVE-RC 70 T=
   vsrc vsrcu TOP-CHECK
   5 s" type-deferred: view cast defers" EXPECT-KIND
   s" type-deferred: renderer, view cast and dependent body" T-LABEL CHECK:VERIFY-OUT$ PACKETS 3 T=
   s" type-deferred: unresolved view cast is absent" T-LABEL
   CHECK:VERIFY-DEFS$ s" word" s" CVT-UNPACK" PACKET 0 < TTRUE

   s\" require lib/type/deftype.f\n: CVT-MAKE ( -- ) s\" DEFTYPE CVT-NOM\" evaluate-closed ;\nCVT-MAKE\n: CVT-K ( -- n ) 1 ;\ns\" CVT-K\" s\" -- cvt-nom ptr\" TRUST\n" TOP-CHECK
   1 s" type-deferred: invalid assertion grammar refuses" EXPECT-KIND
   s" type-deferred: invalid assertion adds no deferral to the renderer's" T-LABEL CHECK:VERIFY-OUT$ PACKETS 2 T=
   s" type-deferred: invalid assertion code" T-LABEL
   CHECK:VERIFY-OUT$ 1 NTH-PACKET s" code" STRING$ s" E-BAD-STORED-SIGNATURE" T$=

   s\" require lib/type/deftype.f\n: CVT-MAKE ( -- ) s\" DEFTYPE CVT-NOM\" evaluate-closed ;\nCVT-MAKE\n: CVT-K ( -- n ) 1 ;\ns\" CVT-K\" s\" -- cvt-nom\" TRUST\ns\" CVT-K\" s\" -- n\" TRUST\n: CVT-RESTORED ( -- n ) CVT-K ;\n"
   TOP-CHECK
   5 s" type-deferred: known assertion restores checking" EXPECT-KIND
   s" type-deferred: restored assertion has only the renderer's and original warnings" T-LABEL CHECK:VERIFY-OUT$ PACKETS 2 T=
   s" type-deferred: restored caller is recorded" T-LABEL
   CHECK:VERIFY-DEFS$ s" word" s" CVT-RESTORED" PACKET 0 >= TTRUE

   s\" require lib/type/deftype.f\n: CVT-MAKE ( -- ) s\" DEFTYPE CVT-NOM\" evaluate-closed ;\nCVT-MAKE\n: CVT-K ( -- n ) 1 ;\ns\" CVT-K\" s\" -- cvt-nom\" TRUST\ns\" CVT-K\" s\" -- n\" TRUST\n: CVT-BAD ( -- bool ) CVT-K ;\n"
   TOP-CHECK
   1 s" type-deferred: restored assertion rejects mismatch" EXPECT-KIND
   s" type-deferred: restored caller mismatch" T-LABEL
   CHECK:VERIFY-OUT$ s" word" s" cvt-bad" PACKET s" code" STRING$ s" E-MISMATCH" T$=

   s\" require lib/type/deftype.f\n: CVT-MAKE ( -- ) s\" DEFTYPE CVT-NOM\" evaluate-closed ;\nCVT-MAKE\n: CVT-K ( -- n ) 1 ;\ns\" CVT-K\" s\" -- cvt-nom\" TRUST\npackage CVT-OTHER\n: CVT-K ( -- n ) 2 ;\n: CVT-KEPT ( -- n ) CVT-K ;\n;package\n"
   TOP-CHECK
   5 s" type-deferred: another binding stays checkable" EXPECT-KIND
   s" type-deferred: another binding has only the renderer's and original warnings" T-LABEL CHECK:VERIFY-OUT$ PACKETS 2 T=

   s\" require lib/type/deftype.f\n: CVT-MAKE ( -- ) s\" DEFTYPE CVT-NOM\" evaluate-closed ;\nCVT-MAKE\n: CVT-K ( -- n ) 1 ;\ns\" CVT-K\" s\" -- cvt-nom\" TRUST\n: CVT-TICK ( -- [ -- n ] ) ['] CVT-K ;\n' CVT-K drop\n"
   TOP-CHECK
   5 s" type-deferred: ticks keep no old effect" EXPECT-KIND
   s" type-deferred: renderer, assertion and both ticks" T-LABEL CHECK:VERIFY-OUT$ PACKETS 4 T=

   s\" require lib/type/deftype.f\n: CVT-MAKE ( -- ) s\" DEFTYPE CVT-NOM\" evaluate-closed ;\nCVT-MAKE\ndefer CVT-K ( -- n )\ns\" CVT-K\" s\" -- cvt-nom\" TRUST\n: CVT-SET ( [ -- n ] -- ) is CVT-K ;\n"
   TOP-CHECK
   5 s" type-deferred: deferred setter keeps no old effect" EXPECT-KIND
   s" type-deferred: renderer, assertion and deferred setter" T-LABEL CHECK:VERIFY-OUT$ PACKETS 3 T=

   s\" require lib/type/deftype.f\n: CVT-MAKE ( -- ) s\" DEFTYPE CVT-NOM\" evaluate-closed ;\nCVT-MAKE\n: CVT-PN ( -- ) parse-name 2drop ;\ns\" CVT-PN\" s\" -- cvt-nom\" TRUST\nCVT-PN : CVT-PAYLOAD ( -- n ) 1 ;\n" TOP-CHECK
   5 s" type-deferred: parser still owns its operands" EXPECT-KIND
   s" type-deferred: parser payload is not a declaration" T-LABEL
   CHECK:VERIFY-DEFS$ s" word" s" CVT-PAYLOAD" PACKET 0 < TTRUE

   s\" require lib/type/deftype.f\n: CVT-MAKE ( -- ) s\" DEFTYPE CVT-NOM\" evaluate-closed ;\nCVT-MAKE\ns\" patch32\" s\" n n -- cvt-nom\" TRUST\n: CVT-BAD ( n n -- ) patch32 ;\n" TOP-CHECK
   1 s" type-deferred: trusted-only call still refuses" EXPECT-KIND
   s" type-deferred: trusted-only call code" T-LABEL
   CHECK:VERIFY-OUT$ s" word" s" cvt-bad" PACKET s" code" STRING$ s" E-CAP-TRUSTED" T$= ;

: TYPE-LONG-FIELDS ( -- )
   33 0 DO
      s" FIELD f" GEN+
      252 0 DO 97 GEN-C+ LOOP
      i 10 < IF 48 GEN-C+ THEN i GEN-N+
      s"  n" GEN+  10 GEN-C+
   LOOP ;

: TYPE-LONG-LIVE ( -- )
   0 GEN-U !
   s" STRUCTURE cvtlong 0" GEN+  10 GEN-C+
   TYPE-LONG-FIELDS
   s" ;STRUCTURE" GEN+  10 GEN-C+
   s" type-structure-long.f" 0 GEN GEN-U @ FIXTURE
   s" type-deferred: long live structure loads" T-LABEL
   s" type-structure-long.f" AT$ NATIVE-RC 0 T=

   0 GEN-U !
   s" ENUM cvtlong 0 VARIANT one" GEN+  10 GEN-C+
   TYPE-LONG-FIELDS
   s" ;VARIANT ;ENUM" GEN+  10 GEN-C+
   s" type-enum-long.f" 0 GEN GEN-U @ FIXTURE
   s" type-deferred: long live enum loads" T-LABEL
   s" type-enum-long.f" AT$ NATIVE-RC 0 T= ;

: TYPE-DEFERRED ( -- )
   TYPE-DEFERRED-DECLS TYPE-DEFERRED-CONTROLS TYPE-DEFERRED-TRUSTED
   TYPE-DEFERRED-DIRECT TYPE-DEFERRED-REVIEW TYPE-DEFERRED-ROWS TYPE-LONG-LIVE ;


\ A deferred stretch at TOKEN on LINE at COLUMN.
: DEFERRED-AT ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: label:ptr labelu:n tok:ptr toku:n line:ptr lineu:n col:ptr colu:n :}
   label labelu tok toku s" W-CHECK-DEFERRED" line lineu col colu TOP-PACKET
   s" verdict" STRING$ s" deferred" T$= ;


\ A word whose body calls `create` reads a name when it runs and defines it, and
\ so does a word that calls such a word, though its own check is the run's
\ (CVT-MK, deferred at CVT-RA:CVT-RSEVEN, which rendered text defines). Loaded,
\ each subject makes CVT-Q or CVT-Y, exit 0. With a row the call is the run's
\ and its operand is consumed, and each use of its product after it is the
\ run's too. With none, it may read any of the rest: the check discovers
\ nothing after the call.
: TOP-CREATE ( -- )
   s\" : CVT-MKS ( n -- ) create , ;\nparses: CVT-MKS 1\n5 CVT-MKS CVT-Q\nCVT-Q drop\n: CVT-AFTER ( -- n ) 1 ;\nCVT-Q drop\n" TOP-CHECK
   5 s" top-create: deferred" EXPECT-KIND
   s" top-create: three packets" T-LABEL CHECK:VERIFY-OUT$ PACKETS 3 T=
   s" top-create: the call" s" CVT-MKS" s" 3" s" 3" DEFERRED-AT
   s" top-create: its product next" s" CVT-Q" s" 4" s" 1" DEFERRED-AT
   CHECK:VERIFY-OUT$ 2 NTH-PACKET {: p:n :}
   s" top-create: its product after it" T-LABEL p s" token" STRING$ s" CVT-Q" T$=
   s" top-create: its product after it" T-LABEL p s" code" STRING$ s" W-CHECK-DEFERRED" T$=
   s" top-create: its product after it" T-LABEL p s" line" NUMBER$ s" 6" T$=
   s" top-create: its product after it" T-LABEL p s" column" NUMBER$ s" 1" T$=
   s" top-create: its product after it" T-LABEL p s" verdict" STRING$ s" deferred" T$=
   s\" : CVT-MKS ( n -- ) create , ;\n5 CVT-MKS CVT-Q\nCVT-Q drop\n: CVT-AFTER ( -- n ) 1 ;\nCVT-Q drop\n" TOP-CHECK
   s" top-create: undeclared, the stop" s" CVT-MKS" s" 2" s" 3" DEFERRED-ONLY
   s\" package CVT-RA public : CVT-RMAKE ( -- ) s\" : CVT-RSEVEN ( -- n ) 7 ;\" INCLUDE-EVALUATE ; CVT-RMAKE ;package\n: CVT-DEFR ( n -- ) create , does> ( -- n ) @ ;\n: CVT-MK ( n -- ) CVT-RA:CVT-RSEVEN + CVT-DEFR ;\n5 CVT-MK CVT-Y\n"
   TOP-CHECK
   5 s" top-create: through a word the run checks" EXPECT-KIND
   s" top-create: three packets, through a word" T-LABEL CHECK:VERIFY-OUT$ PACKETS 3 T=
   s" top-create: the renderer's call" s" CVT-RMAKE" s" 1" s" 91" DEFERRED-AT
   s" top-create: that word's body" s" CVT-RA:CVT-RSEVEN" s" 3" s" 19" DEFERRED-AT
   s" top-create: at that word" s" CVT-MK" s" 4" s" 3" DEFERRED-AT ;


\ A deferred check of two packets: CVT-TL's call on TLINE at TCOL, then the
\ row's name TOKEN on LINE at COLUMN.
: TL-DEFERRED-AT ( CHECK:verdict ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: v label:ptr labelu:n tok:ptr toku:n line:ptr lineu:n col:ptr colu:n tline:ptr tlineu:n tcol:ptr tcolu:n :}
   v 5 label labelu EXPECT-KIND
   label labelu T-LABEL CHECK:VERIFY-OUT$ PACKETS 2 T=
   label labelu 0 s" CVT-TL" tline tlineu tcol tcolu DEFERRED-NTH
   label labelu 1 tok toku line lineu col colu DEFERRED-NTH ;


\ A refused check of two packets: the row's name TOKEN unresolved on LINE at
\ COLUMN, and CVT-TL's call deferred on TLINE at TCOL.
: TL-REFUSED-AT ( CHECK:verdict ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: v label:ptr labelu:n tok:ptr toku:n line:ptr lineu:n col:ptr colu:n tline:ptr tlineu:n tcol:ptr tcolu:n :}
   v 1 label labelu EXPECT-KIND
   label labelu T-LABEL CHECK:VERIFY-OUT$ PACKETS 2 T=
   label labelu tok toku s" E-TRUST-UNRESOLVED" line lineu col colu TOP-PACKET
   s" verdict" STRING$ s" rejected" T$=
   label labelu s" CVT-TL" s" W-CHECK-DEFERRED" tline tlineu tcol tcolu TOP-PACKET
   s" verdict" STRING$ s" deferred" T$= ;


\ A `trust` row naming a word a statement before it renders: loaded, CVT-TL
\ defines CVT-TDW in the wordlist current when it runs, from the text it
\ evaluates or the file it includes, the row finds it there and each subject
\ loads, exit 0, but CVT-TDWX names no word, E-TRUST-UNRESOLVED, exit 70. The
\ scan never sees CVT-TDW, since it runs no word and so reads no file a body
\ loads, and the statement marked the wordlist the row's record lands in - the
\ open section, or PKG's public one for PKG:TAIL - where a product and a
\ misspelt name look alike, so the row is the run's at its name. It records
\ nothing: each use after it is the run's too. The statement is the run's at
\ its call.
\ A row naming a word the scan has seen is checked as ever: the last subject is
\ E-BAD-STORED-SIGNATURE loaded, exit 70.
: TOP-TRUST ( -- )
   s\" : CVT-TL ( -- ) s\" : CVT-TDW ( -- n ) 1 ;\" evaluate-closed ;\nCVT-TL\ns\" CVT-TDW\" s\" -- n\" TRUST\n: CVT-TU ( -- n ) CVT-TDW ;\nCVT-TDW drop\n"
   TOP-CHECK
   5 s" top-trust: a product's row, deferred" EXPECT-KIND
   s" top-trust: four packets" T-LABEL CHECK:VERIFY-OUT$ PACKETS 4 T=
   s" top-trust: at the statement" 0 s" CVT-TL" s" 2" s" 1" DEFERRED-NTH
   s" top-trust: at the row's name" 1 s" CVT-TDW" s" 3" s" 4" DEFERRED-NTH
   s" top-trust: a use in a body" 2 s" CVT-TDW" s" 4" s" 19" DEFERRED-NTH
   s" top-trust: a use at top level" 3 s" CVT-TDW" s" 5" s" 1" DEFERRED-NTH
   s\" package CVT-TP\n: CVT-TL ( -- ) s\" : CVT-TDW ( -- n ) 1 ;\" evaluate-closed ;\nCVT-TL\ns\" CVT-TDW\" s\" -- n\" TRUST\n;package\npackage CVT-TP s\" CVT-TDW\" s\" -- n\" TRUST ;package\n"
   TOP-CHECK
   5 s" top-trust: private, deferred" EXPECT-KIND
   s" top-trust: private, three packets" T-LABEL CHECK:VERIFY-OUT$ PACKETS 3 T=
   s" top-trust: private, at the statement" 0 s" CVT-TL" s" 3" s" 1" DEFERRED-NTH
   s" top-trust: in the private section" 1 s" CVT-TDW" s" 4" s" 4" DEFERRED-NTH
   s" top-trust: in that section reopened" 2 s" CVT-TDW" s" 6" s" 19" DEFERRED-NTH
   s\" package CVT-TP public\n: CVT-TL ( -- ) s\" : CVT-TDW ( -- n ) 1 ;\" evaluate-closed ;\nCVT-TL\ns\" CVT-TDW\" s\" -- n\" TRUST\n;package\n"
   TOP-CHECK s" top-trust: in the public section" s" CVT-TDW" s" 4" s" 4" s" 3" s" 1" TL-DEFERRED-AT
   s\" package CVT-TQ public : CVT-TL ( -- ) s\" : CVT-TDW ( -- n ) 1 ;\" evaluate-closed ; CVT-TL ;package\ns\" CVT-TQ:CVT-TDW\" s\" -- n\" TRUST\n"
   TOP-CHECK s" top-trust: qualified, in its public" s" CVT-TQ:CVT-TDW" s" 2" s" 4" s" 1" s" 84" TL-DEFERRED-AT
   s\" : CVT-TL ( -- ) s\" : CVT-TDW ( -- n ) 1 ;\" evaluate-closed ;\nCVT-TL\ns\" CVT-TDWX\" s\" -- n\" TRUST\n"
   TOP-CHECK s" top-trust: a misspelt name" s" CVT-TDWX" s" 3" s" 4" s" 2" s" 1" TL-DEFERRED-AT
   s" trust-dep.f" s\" : CVT-TDW ( -- n ) 1 ;\n" FIXTURE
   s\" : CVT-TL ( -- ) s\" trust-dep.f\" included ;\nCVT-TL\ns\" CVT-TDW\" s\" -- n\" TRUST\n"
   TOP-CHECK s" top-trust: a called body's loader" s" CVT-TDW" s" 3" s" 4" s" 2" s" 1" TL-DEFERRED-AT
   s\" : CVT-TL ( -- ) s\" : CVT-TDW ( -- n ) 1 ;\" evaluate-closed ;\nCVT-TL\n: CVT-TK ( -- n ) 1 ;\ns\" CVT-TK\" s\" -- n\" TRUST\n: CVT-TU ( -- n ) CVT-TK ;\n"
   TOP-CHECK s" top-trust: a seen word's row, deferred only at the statement" s" CVT-TL" s" 2" s" 1" DEFERRED-ONLY
   s\" : CVT-TL ( -- ) s\" : CVT-TDW ( -- n ) 1 ;\" evaluate-closed ;\nCVT-TL\n: CVT-TK ( -- n ) 1 ;\ns\" CVT-TK\" s\" -- ptr\" TRUST\n: CVT-TU ( -- n ) CVT-TK ;\n"
   TOP-CHECK
   1 s" top-trust: a seen word's bad row" EXPECT-KIND
   s" top-trust: a seen word's bad row, two packets" T-LABEL CHECK:VERIFY-OUT$ PACKETS 2 T=
   s" top-trust: a seen word's bad row, its code" T-LABEL
   CHECK:VERIFY-OUT$ 1 NTH-PACKET s" code" STRING$ s" E-BAD-STORED-SIGNATURE" T$= ;


\ The rows the scan still refuses where the load does, each E-TRUST-UNRESOLVED,
\ exit 70: a row before the statement that renders its word, a row naming a
\ word only an uncalled body's loader defines, whose file neither the load nor
\ the scan reads and for which no statement marks a wordlist, a row whose record
\ lands in a wordlist no statement marked - a package's section under a global
\ mark, the public section under a private mark, the global wordlist under a
\ package's mark or a used public's, PKG's public wordlist under a global mark -
\ and a name no word can have: a malformed one, or one holding a blank or a tab,
\ which no token holds, the mark's own spelling among them (checker.f
\ UNSEEN-MARK$). A statement that runs CVT-TL is deferred at its call.
: TOP-TRUST-REFUSED ( -- )
   s\" : CVT-TL ( -- ) s\" : CVT-TDW ( -- n ) 1 ;\" evaluate-closed ;\ns\" CVT-TDW\" s\" -- n\" TRUST\nCVT-TL\n" TOP-CHECK
   s" top-trust: before the statement" s" CVT-TDW" s" 2" s" 4" s" 3" s" 1" TL-REFUSED-AT
   s" trust-dep.f" s\" : CVT-TDW ( -- n ) 1 ;\n" FIXTURE
   s\" : CVT-TL ( -- ) s\" trust-dep.f\" included ;\ns\" CVT-TDW\" s\" -- n\" TRUST\n" TOP-CHECK
   s" top-trust: an uncalled body's loader" s" CVT-TDW" s" E-TRUST-UNRESOLVED" s" 2" s" 4" REFUSED-AT
   s\" : CVT-TL ( -- ) s\" : CVT-TDW ( -- n ) 1 ;\" evaluate-closed ;\nCVT-TL\npackage CVT-TP s\" CVT-TDW\" s\" -- n\" TRUST ;package\n"
   TOP-CHECK s" top-trust: a package's row" s" CVT-TDW" s" 3" s" 19" s" 2" s" 1" TL-REFUSED-AT
   s\" package CVT-TP : CVT-TL ( -- ) s\" : CVT-TDW ( -- n ) 1 ;\" evaluate-closed ; CVT-TL\npublic s\" CVT-TDW\" s\" -- n\" TRUST ;package\n"
   TOP-CHECK s" top-trust: a public row" s" CVT-TDW" s" 2" s" 11" s" 1" s" 77" TL-REFUSED-AT
   s\" package CVT-TP : CVT-TL ( -- ) s\" : CVT-TDW ( -- n ) 1 ;\" evaluate-closed ; CVT-TL ;package\ns\" CVT-TDW\" s\" -- n\" TRUST\n"
   TOP-CHECK s" top-trust: a global row" s" CVT-TDW" s" 2" s" 4" s" 1" s" 77" TL-REFUSED-AT
   s\" package CVT-TQ public : CVT-TL ( -- ) s\" : CVT-TDW ( -- n ) 1 ;\" evaluate-closed ; CVT-TL ;package\nusing CVT-TQ s\" CVT-TDW\" s\" -- n\" TRUST ;using\n"
   TOP-CHECK s" top-trust: a used public's mark" s" CVT-TDW" s" 2" s" 17" s" 1" s" 84" TL-REFUSED-AT
   s\" package CVT-TQ public : CVT-TK ( -- n ) 1 ; ;package\n: CVT-TL ( -- ) s\" : CVT-TDW ( -- n ) 1 ;\" evaluate-closed ;\nCVT-TL\ns\" CVT-TQ:CVT-TDW\" s\" -- n\" TRUST\n"
   TOP-CHECK s" top-trust: a qualified row" s" CVT-TQ:CVT-TDW" s" 4" s" 4" s" 3" s" 1" TL-REFUSED-AT
   s\" package CVT-TQ public : CVT-TL ( -- ) s\" : CVT-TDW ( -- n ) 1 ;\" evaluate-closed ; CVT-TL ;package\ns\" CVT-TQ:CVT-TDW:X\" s\" -- n\" TRUST\n"
   TOP-CHECK s" top-trust: a malformed qualified name" s" CVT-TQ:CVT-TDW:X" s" 2" s" 4" s" 1" s" 84" TL-REFUSED-AT
   s\" : CVT-TL ( -- ) s\" : CVT-TDW ( -- n ) 1 ;\" evaluate-closed ;\nCVT-TL\ns\" CVT-A:B:C\" s\" -- n\" TRUST\n"
   TOP-CHECK s" top-trust: a malformed name" s" CVT-A:B:C" s" 3" s" 4" s" 2" s" 1" TL-REFUSED-AT
   s\" : CVT-TL ( -- ) s\" : CVT-TDW ( -- n ) 1 ;\" evaluate-closed ;\nCVT-TL\ns\" CVT-TDW X\" s\" -- n\" TRUST\n"
   TOP-CHECK s" top-trust: a name holding a blank" s" CVT-TDW X" s" 3" s" 4" s" 2" s" 1" TL-REFUSED-AT
   s\" : CVT-TL ( -- ) s\" : CVT-TDW ( -- n ) 1 ;\" evaluate-closed ;\nCVT-TL\ns\" unseen products\" s\" -- n\" TRUST\n"
   TOP-CHECK s" top-trust: the mark's own spelling" s" unseen products" s" 3" s" 4" s" 2" s" 1" TL-REFUSED-AT
   s\" : CVT-TL ( -- ) s\" : CVT-TDW ( -- n ) 1 ;\" evaluate-closed ;\nCVT-TL\ns\" CVT-TDW\tX\" s\" -- n\" TRUST\n"
   TOP-CHECK s" top-trust: a name holding a tab" s\" CVT-TDW\tX" s" 3" s" 4" s" 2" s" 1" TL-REFUSED-AT ;


\ A word a top-level `create` makes pushes its address when it runs: it reads
\ and defines nothing, though `create` does both, and neither does a word whose
\ calls reach only such words (lib/test.f T= reaches lib/string.f's
\ STR-MIN-I64$ through FMT:.INT). Both come from the engine. Loaded, NOSUCH is
\ E-UNDEFINED, exit 70.
: TOP-DATA-WORD ( -- )
   s\" require lib/string.f\nSTR-MAX-I64$ drop NOSUCH\n" TOP-CHECK
   s" top-data-word: the word" s" NOSUCH" s" E-UNDEFINED-TOP-LEVEL" s" 2" s" 19" REFUSED-AT
   s\" require lib/test.f\n1 1 T= NOSUCH\n" TOP-CHECK
   s" top-data-word: a word that calls it" s" NOSUCH" s" E-UNDEFINED-TOP-LEVEL" s" 2" s" 8" REFUSED-AT ;


\ A TRUSTED: body is asserted, never checked, but what it may do when it runs
\ is what the calls it binds may do, as for a checked body. Loaded, CVT-EVW's
\ text defines CVT-EVY, CVT-TP takes NOSUCH, and CVT-TU runs, exit 0. CVT-HID
\ comes out of rendered text, so the call to it binds to nothing the checker
\ holds and may do anything. CVT-TB's calls do neither: loaded, NOSUCH is
\ E-UNDEFINED, exit 70.
: TOP-TRUSTED ( -- )
   s\" TRUSTED: CVT-EVW ( ptr u8 n -- ) evaluate ;\ns\" : CVT-EVY ( -- n ) 2 ;\" CVT-EVW\nCVT-EVY drop\n" TOP-CHECK
   5 s" top-trusted: renders" EXPECT-KIND
   s" top-trusted: renders, two packets" T-LABEL CHECK:VERIFY-OUT$ PACKETS 2 T=
   s" top-trusted: renders, at its call" s" CVT-EVW" s" 2" s" 28" DEFERRED-AT
   s" top-trusted: its product" s" CVT-EVY" s" 3" s" 1" DEFERRED-AT
   s\" TRUSTED: CVT-TP ( -- ) parse-name 2drop ;\nCVT-TP NOSUCH\n" TOP-CHECK
   5 s" top-trusted: parses" EXPECT-KIND
   s" top-trusted: parses, one packet" T-LABEL CHECK:VERIFY-OUT$ PACKETS 1 T=
   s" top-trusted: its stretch" s" CVT-TP" s" 2" s" 1" DEFERRED-AT
   s\" s\" : CVT-HID ( -- ) ;\" evaluate\nTRUSTED: CVT-TU ( -- ) CVT-HID ;\nCVT-TU 1 drop\n" TOP-CHECK
   5 s" top-trusted: binds to nothing" EXPECT-KIND
   s" top-trusted: binds to nothing, two packets" T-LABEL CHECK:VERIFY-OUT$ PACKETS 2 T=
   s" top-trusted: binds to nothing, the renderer's call" s" evaluate" s" 1" s" 24" DEFERRED-AT
   s" top-trusted: that stretch" s" CVT-TU" s" 3" s" 1" DEFERRED-AT
   s\" TRUSTED: CVT-TB ( -- n ) 1 dup drop ;\nCVT-TB NOSUCH\n" TOP-CHECK
   s" top-trusted: does neither" s" NOSUCH" s" E-UNDEFINED-TOP-LEVEL" s" 2" s" 8" REFUSED-AT ;


\ `kernel:` is the engine's synonym for `:`. Loaded, the first subject runs,
\ exit 0. The second's body is refused, exit 70; checked, the scan goes on past
\ it as past a `:` definition's, to refuse NOSUCH.
: TOP-KERNEL ( -- )
   s\" KERNEL: CVT-TKI ( n -- n ) 1+ ;\n8 CVT-TKI drop\n" s" top-kernel: a definition" LOADS-CLEAN
   s\" KERNEL: CVT-KB ( -- n ) ;\nNOSUCH drop\n" TOP-CHECK
   1 s" top-kernel: a refused body" EXPECT-KIND
   s" top-kernel: two packets" T-LABEL CHECK:VERIFY-OUT$ PACKETS 2 T=
   s" top-kernel: the body's first" T-LABEL
   CHECK:VERIFY-OUT$ 0 NTH-PACKET s" word" STRING$ s" cvt-kb" T$=
   s" top-kernel: then the token" s" NOSUCH" s" E-UNDEFINED-TOP-LEVEL" s" 2" s" 1" TOP-PACKET
   s" verdict" STRING$ s" rejected" T$= ;


\ The engine's top-level find takes an open package's own qualified name that
\ its public wordlist lacks to the global wordlist, for `'` too, and the word
\ it finds there may define what a bare call to it would: a renderer's text
\ (CVT-OFP:evaluate) or an unlearned create caller (CVT-OFP:CVT-OFMK) makes
\ CVT-OFQ, so a later use of it is the run's. The row of the word it finds
\ bounds the qualified call, and with none the check discovers nothing after
\ it. Loaded, the first subject and the last three run, exit 0; a tail no
\ wordlist holds, or the name once the package is closed, is E-UNDEFINED, exit
\ 70.
: TOP-PACKAGE-TAIL ( -- )
   s\" : CVT-OFG ( -- n ) 7 ;\npackage CVT-OFP\n: CVT-OFL ( -- n ) 1 ;\nCVT-OFP:CVT-OFG drop\n' CVT-OFP:CVT-OFG drop\n;package\n"
   s" top-package-tail: in its package" LOADS-CLEAN
   s\" : CVT-OFG ( -- n ) 7 ;\npackage CVT-OFP\n: CVT-OFL ( -- n ) 1 ;\nCVT-OFP:CVT-NOSUCH drop\n;package\n" TOP-CHECK
   s" top-package-tail: no such tail" s" CVT-OFP:CVT-NOSUCH" s" E-UNDEFINED-TOP-LEVEL" s" 4" s" 1" REFUSED-AT
   s\" : CVT-OFG ( -- n ) 7 ;\npackage CVT-OFP\n: CVT-OFL ( -- n ) 1 ;\n;package\nCVT-OFP:CVT-OFG drop\n" TOP-CHECK
   s" top-package-tail: closed" s" CVT-OFP:CVT-OFG" s" E-UNDEFINED-TOP-LEVEL" s" 5" s" 1" REFUSED-AT
   s\" package CVT-OFP\ns\" : CVT-OFQ ( -- n ) 7 ;\" CVT-OFP:evaluate\nCVT-OFQ drop\n;package\n" TOP-CHECK
   5 s" top-package-tail: a renderer" EXPECT-KIND
   s" top-package-tail: a renderer, two packets" T-LABEL CHECK:VERIFY-OUT$ PACKETS 2 T=
   s" top-package-tail: its call" s" CVT-OFP:evaluate" s" 2" s" 28" DEFERRED-AT
   s" top-package-tail: its product" s" CVT-OFQ" s" 3" s" 1" DEFERRED-AT
   s\" : CVT-OFMK ( -- ) create ;\nparses: CVT-OFMK 1\npackage CVT-OFP\nCVT-OFP:CVT-OFMK CVT-OFQ\n: CVT-AFTER ( -- ) ;\nCVT-OFQ drop\n;package\n" TOP-CHECK
   5 s" top-package-tail: a create caller" EXPECT-KIND
   s" top-package-tail: a create caller, two packets" T-LABEL CHECK:VERIFY-OUT$ PACKETS 2 T=
   s" top-package-tail: its call" s" CVT-OFP:CVT-OFMK" s" 4" s" 1" DEFERRED-AT
   s" top-package-tail: its product after it" s" CVT-OFQ" s" 6" s" 1" DEFERRED-AT
   s\" : CVT-OFMK ( -- ) create ;\npackage CVT-OFP\nCVT-OFP:CVT-OFMK CVT-OFQ\n: CVT-AFTER ( -- ) ;\nCVT-OFQ drop\n;package\n" TOP-CHECK
   s" top-package-tail: an undeclared create caller, the stop" s" CVT-OFP:CVT-OFMK" s" 3" s" 1" DEFERRED-ONLY ;


\ A deferred word may do anything when it runs, define the name it reads among
\ them. Loaded, CVT-D runs CVT-MKC, which makes CVT-DQ, exit 0. The check
\ reports CVT-D at its call and discovers nothing after it, so the use of that
\ name is never refused.
: TOP-DEFER-WORD ( -- )
   s\" defer CVT-D ( -- )\n: CVT-MKC ( -- ) create ;\n: CVT-SET ( -- ) ['] CVT-MKC is CVT-D ;\nCVT-SET\nCVT-D CVT-DQ\n: CVT-AFTER ( -- n ) 1 ;\nCVT-DQ drop\n"
   TOP-CHECK
   5 s" top-defer-word: deferred" EXPECT-KIND
   s" top-defer-word: one packet" T-LABEL CHECK:VERIFY-OUT$ PACKETS 1 T=
   s" top-defer-word: the call" s" CVT-D" s" 5" s" 1" DEFERRED-AT
   s" top-defer-word: the name after it" T-LABEL
   CHECK:VERIFY-OUT$ s" token" s" CVT-DQ" PACKET 0 < TTRUE ;


\ A deferred word may read whatever follows it, a comment among them, so it is
\ reported at its call whatever follows, and the check discovers nothing after
\ it. Loaded, CVT-CD runs CVT-NOP, exit 0.
: CVT-CD$ ( ptr u8 n -- ptr u8 n ) {: tail:ptr tailu:n :}
   SB-RESET
   s\" defer CVT-CD ( -- )\n: CVT-NOP ( -- ) ;\n: CVT-CSET ( -- ) ['] CVT-NOP is CVT-CD ;\nCVT-CSET\nCVT-CD " SB-APPEND
   tail tailu SB-APPEND
   s\" \n: CVT-AFTER ( -- n ) 1 ;\n" SB-APPEND
   SB$ ;

: TOP-DEFER-COMMENT ( -- )
   s" \ a note" CVT-CD$ TOP-CHECK
   s" top-defer-comment: a line comment" s" CVT-CD" s" 5" s" 1" DEFERRED-ONLY
   s" ( a note )" CVT-CD$ TOP-CHECK
   s" top-defer-comment: a comment in parentheses" s" CVT-CD" s" 5" s" 1" DEFERRED-ONLY
   s" .( a note)" CVT-CD$ TOP-CHECK
   5 s" top-defer-comment: a print, deferred" EXPECT-KIND
   s" top-defer-comment: one packet" T-LABEL CHECK:VERIFY-OUT$ PACKETS 1 T=
   s" top-defer-comment: at the word" s" CVT-CD" s" 5" s" 1" DEFERRED-AT ;


\ Each subject loads, exit 0.
: TOP-ACCEPTED ( -- )
   s\" variable CVT-V\n5 CVT-V !\n7 constant CVT-K\nCVT-K drop\ncreate CVT-B 16 allot\nCVT-B drop\n: CVT-DEF ( n -- ) create , does> ( -- n ) @ ;\n3 CVT-DEF CVT-W\nCVT-W drop\n"
   s" top-accepted: created names" LOADS-CLEAN
   s\" TYPED-VARIABLE CVT-TV n\nCVT-TV drop\n" s" top-accepted: a typed variable" LOADS-CLEAN
   s\" require lib/string.f\n16 BUFFER: CVT-BF\nCVT-BF drop\n" s" top-accepted: a buffer" LOADS-CLEAN
   s\" require lib/type/deftype.f\nDEFTYPE CVT-COLOR\n5 >CVT-COLOR drop\n"
   s" top-accepted: a DEFTYPE conversion" LOADS-CLEAN
   s\" require dep.f\nCVT-SEVEN drop\n" s" top-accepted: a required file's word" LOADS-CLEAN
   s\" package CVT-PB\n: CVT-HIDDEN ( -- n ) 1 ;\nCVT-HIDDEN drop\n;package\n"
   s" top-accepted: a private word in its package" LOADS-CLEAN
   s\" package CVT-PU public\n: CVT-SHOWN ( -- n ) 1 ;\n;package\nusing CVT-PU CVT-SHOWN drop ;using\n"
   s" top-accepted: a used public" LOADS-CLEAN
   s\" char NOSUCH drop\n" s" top-accepted: char's operand" LOADS-CLEAN
   s\" 1.5 drop $FF drop -3 drop 0 drop\n" s" top-accepted: numbers" LOADS-CLEAN
   s\" : CVT-MIXED ( -- n ) 1 ;\ncvt-mixed drop\n" s" top-accepted: another case" LOADS-CLEAN
   s\" package CVT-QA public : CVT-QW ( -- n ) 1 ; ;package\nCVT-QA:CVT-QW drop\n"
   s" top-accepted: a qualified public" LOADS-CLEAN
   s\" s\" a\" 2drop .\" x\" cr\n" s" top-accepted: strings" LOADS-CLEAN ;


\ ---- the definitions --------------------------------------------------------

PTR-VARIABLE DEFS-SRC-A                 \ the bytes a definitions case checked
variable DEFS-SRC-U
variable DEF-NODE                       \ the definition line DEF found


: DEFS-SRC$ ( -- ptr u8 n )
   DEFS-SRC-A @ DEFS-SRC-U @ ;


\ Check the generated source as the fixture NAME, kept for the spans its
\ definition lines are read against.
: DEFS-CHECK ( ptr u8 n -- CHECK:verdict )
   {: name:ptr nameu:n :}
   0 GEN DEFS-SRC-A !  GEN-U @ DEFS-SRC-U !
   DEFS-SRC$ name nameu GUARD-MS CHECK-AS ;


\ The first definition line of the last check naming WORD as written; -1 for
\ none.
: DEF ( ptr u8 n -- )
   {: w:ptr wu:n :}
   CHECK:VERIFY-DEFS$ s" word" w wu PACKET DEF-NODE ! ;


\ Where the first LEAD in TEXT ends, -1 for none.
: END-IN ( ptr u8 n ptr u8 n -- n )
   {: a:ptr u:n b:ptr v:n :}
   u v - 1 + 0 max 0 ?do
      a i + v b v STR= if i v + unloop exit then
   loop
   -1 ;


\ Where the first LEAD in the checked bytes ends, -1 for none.
: LEAD-END ( ptr u8 n -- n )
   {: b:ptr v:n :}
   DEFS-SRC$ b v END-IN ;


: DEF-STR ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: label:ptr labelu:n key:ptr keyu:n want:ptr wantu:n :}
   label labelu T-LABEL DEF-NODE @ key keyu STRING$ want wantu T$= ;


: DEF-NUM ( ptr u8 n ptr u8 n n -- )
   {: label:ptr labelu:n key:ptr keyu:n want:n :}
   label labelu T-LABEL DEF-NODE @ key keyu NUMBER$
   SB-RESET want FMT:SB-INT SB$ T$= ;


\ The line DEF found: declared by KIND, recorded in PKG with visibility VIS.
: DEF-WHO ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: label:ptr labelu:n kind:ptr kindu:n pkg:ptr pkgu:n vis:ptr visu:n :}
   label labelu T-LABEL DEF-NODE @ 0 >= TTRUE
   label labelu s" kind" kind kindu DEF-STR
   label labelu s" package" pkg pkgu DEF-STR
   label labelu s" visibility" vis visu DEF-STR ;


\ Its effect, EFF, absent when empty.
: DEF-EFF ( ptr u8 n ptr u8 n -- )
   {: label:ptr labelu:n eff:ptr effu:n :}
   effu 0= if
      label labelu T-LABEL DEF-NODE @ s" effect" J-STR VALUE 0 < TTRUE
      exit
   then
   label labelu s" effect" eff effu DEF-STR ;


\ Its span, the U bytes the first LEAD in the checked bytes ends with, in the
\ fixture FILE.
: DEF-SPAN ( ptr u8 n ptr u8 n n ptr u8 n -- )
   {: label:ptr labelu:n lead:ptr leadu:n u:n file:ptr fileu:n :}
   lead leadu LEAD-END
   {: end:n :}
   label labelu s" file" file fileu AT$ DEF-STR
   label labelu s" byte_start" end u - DEF-NUM
   label labelu s" byte_end" end DEF-NUM ;


\ A definer's names in packages: a private, a public, a qualified name defined
\ in another package's private section, which lands in its qualifier's public
\ wordlist, and an export of it under its tail. Each is named as written.
: DEFS-PACKAGES ( -- )
   0 GEN-U !
   s\" package CVT-DQ\n;package\npackage CVT-DR\n: CVT-DQ:CVT-Y ( -- n ) 5 ;\n;package\n" GEN+
   s\" package CVT-DP\nCAST: CVT-CAST ( n -- ptr u8 )\n: HELPER ( n -- n ) 2 * ;\npublic\n" GEN+
   s\" : CVT-COUNT ( -- n ) 5 HELPER ;\nEXPORT CVT-DQ:CVT-Y\n;package\n" GEN+ ;


\ The global definers, each a line, two that generate names, a user's own
\ definer and the word it creates, and a family.
: DEFS-GLOBALS ( -- )
   s\" : CVT-G ( -- n ) 1 ;\n1 constant CVT-K\nvariable CVT-V\ndefer CVT-DF ( -- n )\n" GEN+
   s\" TRUSTED: CVT-TR ( -- n ) 3 ;\nDYNAMIC-BUFFER CVT-DB u8\nDEFTYPE CVT-T\nNEWTYPE cvt-nt 0\n" GEN+
   s\" : CVT-MK ( n -- ) create , does> ( -- n ) @ ;\n7 CVT-MK CVT-M\n" GEN+ ;


: DEFINITIONS ( -- )
   DEFS-PACKAGES DEFS-GLOBALS
   s" defs.f" DEFS-CHECK 0 s" definitions: verified" EXPECT-KIND
   s" CVT-DQ:CVT-Y" DEF
   s" definitions: Q:Y" s" :" s" cvt-dq" s" public" DEF-WHO
   s" definitions: Q:Y" s" class" s" word" DEF-STR
   s" definitions: Q:Y" s" -- n" DEF-EFF
   s" definitions: Q:Y" s" : CVT-DQ:CVT-Y" 12 s" defs.f" DEF-SPAN
   s" CVT-CAST" DEF
   s" definitions: cast" s" CAST:" s" cvt-dp" s" private" DEF-WHO
   s" definitions: cast" s" class" s" word" DEF-STR
   s" definitions: cast" s" n -- ptr u8" DEF-EFF
   s" definitions: cast" s" CAST: CVT-CAST" 8 s" defs.f" DEF-SPAN
   \ The name as written, which the checker records folded as helper.
   s" HELPER" DEF
   s" definitions: private" s" :" s" cvt-dp" s" private" DEF-WHO
   s" definitions: private" s" class" s" word" DEF-STR
   s" definitions: private" s" n -- n" DEF-EFF
   s" definitions: private" s" : HELPER" 6 s" defs.f" DEF-SPAN
   s" CVT-COUNT" DEF
   s" definitions: public" s" :" s" cvt-dp" s" public" DEF-WHO
   s" definitions: public" s" class" s" word" DEF-STR
   s" definitions: public" s" -- n" DEF-EFF
   s" definitions: public" s" : CVT-COUNT" 9 s" defs.f" DEF-SPAN
   CHECK:VERIFY-DEFS$ s" kind" s" EXPORT" PACKET DEF-NODE !
   s" definitions: export" s" EXPORT" s" cvt-dp" s" public" DEF-WHO
   s" definitions: export" s" class" s" export" DEF-STR
   s" definitions: export" s" word" s" CVT-DQ:CVT-Y" DEF-STR
   s" definitions: export" s" " DEF-EFF
   s" definitions: export" s" EXPORT CVT-DQ:CVT-Y" 12 s" defs.f" DEF-SPAN
   s" CVT-G" DEF
   s" definitions: global" s" :" s" " s" global" DEF-WHO
   s" definitions: global" s" class" s" word" DEF-STR
   s" definitions: global" s" -- n" DEF-EFF
   s" definitions: global" s" : CVT-G" 5 s" defs.f" DEF-SPAN
   s" CVT-K" DEF
   s" definitions: constant" s" constant" s" " s" global" DEF-WHO
   s" definitions: constant" s" class" s" constant" DEF-STR
   s" definitions: constant" s" constant CVT-K" 5 s" defs.f" DEF-SPAN
   s" CVT-V" DEF
   s" definitions: variable" s" variable" s" " s" global" DEF-WHO
   s" definitions: variable" s" class" s" storage" DEF-STR
   s" definitions: variable" s" variable CVT-V" 5 s" defs.f" DEF-SPAN
   s" CVT-DF" DEF
   s" definitions: defer" s" defer" s" " s" global" DEF-WHO
   s" definitions: defer" s" class" s" word" DEF-STR
   s" definitions: defer" s" -- n" DEF-EFF
   s" definitions: defer" s" defer CVT-DF" 6 s" defs.f" DEF-SPAN
   s" CVT-TR" DEF
   s" definitions: trusted" s" TRUSTED:" s" " s" global" DEF-WHO
   s" definitions: trusted" s" class" s" word" DEF-STR
   s" definitions: trusted" s" -- n" DEF-EFF
   s" definitions: trusted" s" TRUSTED: CVT-TR" 6 s" defs.f" DEF-SPAN
   s" CVT-DB" DEF
   s" definitions: dynamic buffer" s" DYNAMIC-BUFFER" s" " s" global" DEF-WHO
   s" definitions: dynamic buffer" s" class" s" storage" DEF-STR
   s" definitions: dynamic buffer" s" " DEF-EFF
   s" definitions: dynamic buffer" s" DYNAMIC-BUFFER CVT-DB" 6 s" defs.f" DEF-SPAN
   s" CVT-DB-RESERVE" DEF
   s" definitions: no generated reserve yet" T-LABEL DEF-NODE @ 0 < TTRUE
   s" CVT-DB-RELEASE" DEF
   s" definitions: no generated release yet" T-LABEL DEF-NODE @ 0 < TTRUE
   s" >CVT-T" DEF
   s" definitions: deftype in" s" DEFTYPE" s" " s" global" DEF-WHO
   s" definitions: deftype in" s" class" s" word" DEF-STR
   s" definitions: deftype in" s" n -- cvt-t" DEF-EFF
   s" definitions: deftype in" s" DEFTYPE CVT-T" 5 s" defs.f" DEF-SPAN
   s" CVT-T>N" DEF
   s" definitions: deftype out" s" DEFTYPE" s" " s" global" DEF-WHO
   s" definitions: deftype out" s" class" s" word" DEF-STR
   s" definitions: deftype out" s" cvt-t -- n" DEF-EFF
   s" definitions: deftype out" s" DEFTYPE CVT-T" 5 s" defs.f" DEF-SPAN
   s" CVT-MK" DEF
   s" definitions: definer" s" :" s" " s" global" DEF-WHO
   s" definitions: definer" s" class" s" word" DEF-STR
   \ A word a user's own create definer makes is storage, whatever its
   \ does> clause answers.
   s" CVT-M" DEF
   s" definitions: created" s" CVT-MK" s" " s" global" DEF-WHO
   s" definitions: created" s" class" s" storage" DEF-STR
   s" definitions: created" s" -- n" DEF-EFF
   s" definitions: created" s" CVT-MK CVT-M" 5 s" defs.f" DEF-SPAN
   \ A family name has no recording symbol: its line waits on the checker
   \ reporting each name a registrar publishes, the Habu owner's Q1 lane.
   s" cvt-nt" DEF
   s" definitions: no family line yet" T-LABEL DEF-NODE @ 0 < TTRUE
   s" definitions: one line each" T-LABEL CHECK:VERIFY-DEFS$ OBJECTS 15 T=
   s" definitions: packets unchanged" T-LABEL CHECK:VERIFY-OUT$ nip 0 T= ;


\ A refused body keeps its line when the checker kept its signature, and has
\ none without one; a body the checker defers has its line. After a create
\ caller no row bounds, the check discovers nothing: a body there has none.
: DEFINITIONS-REFUSED ( -- )
   0 GEN-U !
   s\" : CVT-BAD ( -- n n ) 8 ;\n: CVT-NOSIG 0 if 1 then ;\n" GEN+
   s\" : CVT-MKS ( n -- ) create , ;\nparses: CVT-MKS 1\n5 CVT-MKS CVT-Q\n: CVT-USEQ ( -- n ) CVT-Q @ ;\n" GEN+
   s" defs-refused.f" DEFS-CHECK 1 s" definitions-refused: refused" EXPECT-KIND
   s" CVT-BAD" DEF
   s" definitions-refused: signature kept" s" :" s" " s" global" DEF-WHO
   s" definitions-refused: signature kept" s" -- n n" DEF-EFF
   s" definitions-refused: signature kept" s" : CVT-BAD" 7 s" defs-refused.f" DEF-SPAN
   s" CVT-NOSIG" DEF
   s" definitions-refused: no signature, no line" T-LABEL DEF-NODE @ 0 < TTRUE
   s" CVT-USEQ" DEF
   s" definitions-refused: deferred" s" :" s" " s" global" DEF-WHO
   s" definitions-refused: deferred" s" -- n" DEF-EFF
   s" definitions-refused: deferred" s" : CVT-USEQ" 8 s" defs-refused.f" DEF-SPAN
   0 GEN-U !
   s\" : CVT-MKS ( n -- ) create , ;\n5 CVT-MKS CVT-Q\n: CVT-USEQ ( -- n ) CVT-Q @ ;\n" GEN+
   s" defs-stop.f" DEFS-CHECK 5 s" definitions-refused: undeclared, deferred" EXPECT-KIND
   s" CVT-MKS" DEF
   s" definitions-refused: before the stop" s" n --" DEF-EFF
   s" CVT-USEQ" DEF
   s" definitions-refused: after the stop, no line" T-LABEL DEF-NODE @ 0 < TTRUE ;


\ A name defined again after undefine has both lines, in order.
: DEFINITIONS-UNDEFINE ( -- )
   0 GEN-U !
   s\" : CVT-R ( -- n ) 1 ;\nundefine CVT-R\n: CVT-R ( -- n n ) 2 3 ;\n" GEN+
   s" defs-undefine.f" DEFS-CHECK 0 s" definitions-undefine: verified" EXPECT-KIND
   s" definitions-undefine: two lines" T-LABEL CHECK:VERIFY-DEFS$ OBJECTS 2 T=
   s" CVT-R" DEF
   s" definitions-undefine: the first first" s" -- n" DEF-EFF
   s" definitions-undefine: the first first" s" : CVT-R" 5 s" defs-undefine.f" DEF-SPAN
   CHECK:VERIFY-DEFS$ s" effect" s" -- n n" PACKET DEF-NODE !
   s" definitions-undefine: the second" s" :" s" " s" global" DEF-WHO
   s" definitions-undefine: the second" s" word" s" CVT-R" DEF-STR
   s" definitions-undefine: the second" s\" undefine CVT-R\n: CVT-R" 5 s" defs-undefine.f" DEF-SPAN ;


\ A dependency's definitions have their lines, named by its file, before the
\ subject's.
: DEFINITIONS-DEPENDENCY ( -- )
   0 GEN-U !
   s\" require dep.f\n: CVT-USE7 ( -- n ) CVT-SEVEN ;\n" GEN+
   s" defs-dep.f" DEFS-CHECK 0 s" definitions-dependency: verified" EXPECT-KIND
   s" definitions-dependency: the dependency's first" T-LABEL
   CHECK:VERIFY-DEFS$ JSONL-START JSONL-NEXT-OBJECT s" word" STRING$ s" CVT-SEVEN" T$=
   s" CVT-SEVEN" DEF
   s" definitions-dependency: the dependency's" s" :" s" " s" global" DEF-WHO
   s" definitions-dependency: the dependency's" s" -- n" DEF-EFF
   s" definitions-dependency: the dependency's" s" file" s" dep.f" AT$ DEF-STR
   s" definitions-dependency: the dependency's" s" byte_start" 2 DEF-NUM
   s" definitions-dependency: the dependency's" s" byte_end" 11 DEF-NUM
   s" CVT-USE7" DEF
   s" definitions-dependency: the subject's" s" :" s" " s" global" DEF-WHO
   s" definitions-dependency: the subject's" s" : CVT-USE7" 8 s" defs-dep.f" DEF-SPAN ;


32 constant DIGEST-U                    \ a SHA-256 digest's bytes
64 constant HEX-U                       \ the digest as a file line states it
create SHA-CTX SHA256-CTX-BYTES allot
create DIGEST DIGEST-U allot
create DIGEST-HEX HEX-U allot


\ The SHA-256 of the bytes, as a file line states it.
: SHA$ ( ptr u8 n -- ptr u8 n )
   {: a:ptr u:n :}
   SHA-CTX a u DIGEST SHA256-IN
   DIGEST DIGEST-HEX SHA256>HEX
   DIGEST-HEX HEX-U ;


\ SHA$ of the bytes on disk of the fixture NAME.
: DISK-SHA$ ( ptr u8 n -- ptr u8 n )
   AT$ TREE-BYTES SHA$ ;


\ The next file line names the fixture NAME and states the digest SHA.
: NEXT-FILE ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: label:ptr labelu:n name:ptr nameu:n sha:ptr shau:n :}
   JSONL-NEXT-OBJECT {: node:n :}
   label labelu T-LABEL node s" file" STRING$ name nameu AT$ T$=
   label labelu T-LABEL node s" sha256" STRING$ sha shau T$= ;


\ Each file a check read has one file line, in the order it started them: the
\ subject, a file it requires, the file that one requires, and a file it
\ includes twice and so reads twice, the same bytes each time. Each line states
\ the SHA-256 of the bytes the check read, the subject's the bytes checked. A
\ file required again, one the image holds and one only a comment names have
\ none.
: FILES ( -- )
   0 GEN-U !
   s\" require files-mid.f\nrequire files-mid.f\ninclude files-inc.f\ninclude files-inc.f\n" GEN+
   s\" require src/habu/verify-source.f\n\\ files-unread.f\n: CVT-FILES ( -- n ) 1 ;\n" GEN+
   s" files.f" DEFS-CHECK 0 s" files: verified" EXPECT-KIND
   s" files: four lines" T-LABEL CHECK:VERIFY-FILES$ OBJECTS 4 T=
   CHECK:VERIFY-FILES$ JSONL-START
   s" files: the subject" s" files.f" DEFS-SRC$ SHA$ NEXT-FILE
   s" files: the required file" s" files-mid.f" 2dup DISK-SHA$ NEXT-FILE
   s" files: the file it requires" s" files-nested.f" 2dup DISK-SHA$ NEXT-FILE
   s" files: the included file" s" files-inc.f" 2dup DISK-SHA$ NEXT-FILE ;


\ loop.f's copy on disk is a string never closed: the subject's file line
\ states the bytes checked as loop.f, and back.f's require of it reads nothing.
: IDENTITY-SUBJECT ( -- )
   0 GEN-U !
   s\" require back.f\n: CVT-LOOP ( -- n ) CVT-BACK ;\n" GEN+
   s" loop.f" DEFS-CHECK 0 s" identity-subject: verified" EXPECT-KIND
   s" identity-subject: two lines" T-LABEL CHECK:VERIFY-FILES$ OBJECTS 2 T=
   CHECK:VERIFY-FILES$ JSONL-START
   s" identity-subject: the bytes checked" s" loop.f" DEFS-SRC$ SHA$ NEXT-FILE ;


\ same-a.f and same-b.f hold the same bytes: after the subject's, each has its
\ own file line, with the same digest.
: IDENTITY-PATHS ( -- )
   0 GEN-U !
   s\" require same-a.f\nrequire same-b.f\n: CVT-SAME ( -- n ) 1 ;\n" GEN+
   s" same.f" DEFS-CHECK 0 s" identity-paths: verified" EXPECT-KIND
   CHECK:VERIFY-FILES$ JSONL-START JSONL-NEXT-OBJECT drop
   s" identity-paths: one path" s" same-a.f" SAME$SRC SHA$ NEXT-FILE
   s" identity-paths: the other" s" same-b.f" SAME$SRC SHA$ NEXT-FILE ;


\ file-line-keys.f, loaded into the child after its pre-pass, gives the child's
\ own FILE-LINE five reads through one path buffer and one bytes buffer: P
\ alpha, P alpha, P bravo, P alpha, Q alpha.
: KEYS$SRC ( -- ptr u8 n )
   0 GEN-U !
   s\" package CHECK-VERIFY-CHILD\ncreate KEY-PATH 8 allot\ncreate KEY-BYTES 5 allot\n" GEN+
   s\" : KEY-READ ( ptr u8 n ptr u8 n -- )\n   {: f:ptr fu:n a:ptr u:n :}\n" GEN+
   s\"    f KEY-PATH fu BYTE-COPY\n   a KEY-BYTES u BYTE-COPY\n   KEY-PATH fu KEY-BYTES u FILE-LINE ;\n" GEN+
   s\" : KEY-READS ( -- )\n   s\" /cvt/p.f\" s\" alpha\" KEY-READ\n   s\" /cvt/p.f\" s\" alpha\" KEY-READ\n" GEN+
   s\"    s\" /cvt/p.f\" s\" bravo\" KEY-READ\n   s\" /cvt/p.f\" s\" alpha\" KEY-READ\n" GEN+
   s\"    s\" /cvt/q.f\" s\" alpha\" KEY-READ ;\nKEY-READS\n;package\n" GEN+
   0 GEN GEN-U @ ;


\ The file line FILE-LINE writes for the path F read as the bytes A, onto SB.
: KEY-LINE+ ( ptr u8 n ptr u8 n -- )
   {: f:ptr fu:n a:ptr u:n :}
   s\" check-verify: file {\"file\":\"" SB-APPEND
   f fu SB-APPEND
   s\" \",\"sha256\":\"" SB-APPEND
   a u SHA$ SB-APPEND
   s\" \"}\n" SB-APPEND ;


\ Exactly P alpha, P bravo and Q alpha have file lines, in that order, after
\ the pre-pass's result, which comes with none: a key of the path alone, of
\ the digest alone or of the path's last bytes alone, or one that keeps the
\ caller's buffers in place of their bytes, writes other lines. Two reads of
\ one file in one check give it different bytes only when a disk write comes
\ between them, which nothing here can schedule: that is untested.
: FILE-LINE-KEYS ( -- )
   s" file-line-keys.f" KEYS$SRC FIXTURE
   PROC-ARGV-ENV-RESET
   s" --load" ARG+
   s" tools/check-verify-child.f" TREE$ ARG+
   s" file-line-keys.f" AT$ ARG+
   s" --" ARG+
   s" file-line-subject.f" AT$ ARG+
   s" file-line-keys" ARG+
   PROC-ENV-INHERIT-MISSING
   s\" \n" CLI nip
   s" file-line-keys: loaded" T-LABEL 0 T=
   SB-RESET
   s\" check-verify: verified\n" SB-APPEND
   s" /cvt/p.f" s" alpha" KEY-LINE+
   s" /cvt/p.f" s" bravo" KEY-LINE+
   s" /cvt/q.f" s" alpha" KEY-LINE+
   s" file-line-keys: a line per path and bytes" T-LABEL
   0 OUT CLI-OUT-U @ SB$ T$= ;


\ ---- the loads --------------------------------------------------------------

\ The next load line of CHECK:VERIFY-LOADS$, once JSONL-START has started it:
\ the load of PATH whose operand is the U bytes the first LEAD in the checked
\ bytes ends with, and what became of it, OUTCOME.
: LOAD-NEXT ( ptr u8 n ptr u8 n n ptr u8 n ptr u8 n -- )
   {: label:ptr labelu:n lead:ptr leadu:n u:n path:ptr pathu:n out:ptr outu:n :}
   JSONL-NEXT-OBJECT  lead leadu LEAD-END
   {: node:n end:n :}
   label labelu T-LABEL node 0 >= TTRUE
   label labelu T-LABEL node s" byte_start" NUMBER$
   SB-RESET end u - FMT:SB-INT SB$ T$=
   label labelu T-LABEL node s" byte_end" NUMBER$
   SB-RESET end FMT:SB-INT SB$ T$=
   label labelu T-LABEL node s" path" STRING$ path pathu T$=
   label labelu T-LABEL node s" outcome" STRING$ out outu T$= ;


: LOAD-COUNT ( ptr u8 n n -- )
   {: label:ptr labelu:n want:n :}
   label labelu T-LABEL CHECK:VERIFY-LOADS$ OBJECTS want T= ;


\ `s" ROOT/NAME" script-required`, NAME in the fixture directory:
\ script-required resolves from the working directory, so its path is
\ absolute.
: SCRIPT-REQUIRED+ ( ptr u8 n -- )
   {: name:ptr nameu:n :}
   s\" s\" " GEN+  ROOT$ GEN+  s" /" GEN+  name nameu GEN+
   s\" \" script-required\n" GEN+ ;


\ Each top-level loader of the subject that reached its file has a load line,
\ in the subject's order: where its operand stands (the token after include or
\ require, past any blanks and line breaks, a string loader's literal between
\ its quotes as written, escapes and all, not the word, nor the literal before
\ it), the canonical path, and read, the file acquired for it, or held, a
\ require of a path held already: required before under another spelling, the
\ engine's, provided. provided itself, of a path held or not, a comment and the
\ loader in a file the subject loads have none, though that file is read.
: LOADS ( -- )
   0 GEN-U !
   s\" require ld-dep.f\ns\" ld-dep.f\" provided\ninclude\n   ld-inc.f\n" GEN+
   s\" s\" ld-str-inc.f\" included\ns\" ld-str-req.f\" required\n" GEN+
   s" ld-script.f" SCRIPT-REQUIRED+
   s\" .\" x\" s\\\" ld-esc\\x2ef\" required\n" GEN+
   s\" require ./ld-dep.f\nrequire lib/string.f\n" GEN+
   s" ./ld-script.f" SCRIPT-REQUIRED+
   s\" s\" ld-prov.f\" provided\nrequire ld-prov.f\n\\ require ld-inc.f\n" GEN+
   s" loads.f" DEFS-CHECK 0 s" loads: verified" EXPECT-KIND
   s" loads: ten lines" 10 LOAD-COUNT
   s" loads: each a JSON object" T-LABEL CHECK:VERIFY-LOADS$ ALL-JSON? TTRUE
   s" loads: the dependency's file read" T-LABEL
   CHECK:VERIFY-FILES$ s" file" s" ld-nest.f" AT$ PACKET 0 >= TTRUE
   CHECK:VERIFY-LOADS$ JSONL-START
   s" loads: require" s" require ld-dep.f" 8 s" ld-dep.f" AT$ s" read" LOAD-NEXT
   s" loads: include" s\" include\n   ld-inc.f" 8 s" ld-inc.f" AT$ s" read" LOAD-NEXT
   s" loads: included" s" ld-str-inc.f" 12 s" ld-str-inc.f" AT$ s" read" LOAD-NEXT
   s" loads: required" s" ld-str-req.f" 12 s" ld-str-req.f" AT$ s" read" LOAD-NEXT
   s" loads: script-required" s" /ld-script.f" ROOT-U @ 12 +
   s" ld-script.f" AT$ s" read" LOAD-NEXT
   s" loads: escaped" s\" ld-esc\\x2ef" 11 s" ld-esc.f" AT$ s" read" LOAD-NEXT
   s" loads: required again" s" require ./ld-dep.f" 10 s" ld-dep.f" AT$ s" held" LOAD-NEXT
   s" loads: the engine's" s" lib/string.f" 12 s" lib/string.f" TREE$ s" held" LOAD-NEXT
   s" loads: script-required again" s" /./ld-script.f" ROOT-U @ 14 +
   s" ld-script.f" AT$ s" held" LOAD-NEXT
   s" loads: provided" s" require ld-prov.f" 9 s" ld-prov.f" AT$ s" held" LOAD-NEXT ;


\ A file a loader cannot read has no load line: the check stops at the loader
\ word with the packet and status line it always had, and the loader after the
\ stop is never reached.
: LOADS-UNREAD ( -- )
   s\" require ld-gone.f\nrequire ld-dep.f\n" s" loads-gone.f" GUARD-MS CHECK-AS
   1 s" loads-unread: refused" EXPECT-KIND
   s" loads-unread: no line" 0 LOAD-COUNT
   s" loads-unread: at the loader word" s" E-MISSING-SOURCE" s" require" s" 1" s" 1" AT-TOKEN
   s" loads-unread: its status line" s" ld-gone.f" MISSING-STATUS ;


\ A file read for a loader keeps its read line when its own scan then stops.
: LOADS-SCAN-FAIL ( -- )
   0 GEN-U !
   s\" require ld-open.f\n" GEN+
   s" loads-open.f" DEFS-CHECK 1 s" loads-scan-fail: refused" EXPECT-KIND
   s" loads-scan-fail: one line" 1 LOAD-COUNT
   CHECK:VERIFY-LOADS$ JSONL-START
   s" loads-scan-fail: read" s" require ld-open.f" 9 s" ld-open.f" AT$ s" read" LOAD-NEXT
   s" loads-scan-fail: the stop is the file's" T-LABEL
   CHECK:VERIFY-STOPPED$ s" ld-open.f" AT$ T$= ;


\ An include of the subject's own path reads the bytes given for it, there
\ being no file: read. In that second visit the buffer's name is free again,
\ but a word the buffer makes is not, so the file stops there, as its load
\ does, and that visit's loader is never reached.
: LOADS-SUPPLIED ( -- )
   0 GEN-U !
   s\" DYNAMIC-BUFFER CVT-LDSELF u8\nundefine CVT-LDSELF\ninclude loads-self.f\n" GEN+
   s" loads-self.f" DEFS-CHECK 1 s" loads-supplied: refused" EXPECT-KIND
   s" loads-supplied: one line" 1 LOAD-COUNT
   CHECK:VERIFY-LOADS$ JSONL-START
   s" loads-supplied: read" s" include loads-self.f" 12 SUBJ$ s" read" LOAD-NEXT ;


\ A loader no top-level statement of the subject makes has no line, though it
\ loads: an immediate require in a body, loaded once the definition ends, here
\ after a provided, which reports nothing. After a call of a word that may read
\ on, nothing is loaded.
: LOADS-NONE ( -- )
   s\" s\" ld-p2.f\" provided\n: CVT-LDBODY ( -- n ) require ld-body.f 1 ;\n"
   s" loads-body.f" GUARD-MS CHECK-AS 1 s" loads-none: body, refused" EXPECT-KIND
   s" loads-none: body, no line" 0 LOAD-COUNT
   s" loads-none: body, its file read" T-LABEL
   CHECK:VERIFY-FILES$ s" file" s" ld-body.f" AT$ PACKET 0 >= TTRUE
   s\" : CVT-GRAB ( -- ) parse-name 2drop ;\nCVT-GRAB x\nrequire ld-dep.f\n"
   s" loads-opaque.f" GUARD-MS CHECK-AS 5 s" loads-none: opaque, deferred" EXPECT-KIND
   s" loads-none: opaque, no line" 0 LOAD-COUNT ;


\ The load lines are the last check's: the next check's replace them, none for
\ a check that loads nothing, and check.f's pre-pass has none.
: LOADS-LIFETIME ( -- )
   s\" require ld-dep.f\n" s" loads-again.f" GUARD-MS CHECK-AS
   0 s" loads-lifetime: verified" EXPECT-KIND
   s" loads-lifetime: its line" 1 LOAD-COUNT
   s\" : CVT-LDNONE ( -- n ) 1 ;\n" s" loads-nothing.f" GUARD-MS CHECK-AS
   0 s" loads-lifetime: nothing loaded, verified" EXPECT-KIND
   s" loads-lifetime: nothing loaded, no line" 0 LOAD-COUNT
   s\" require ld-dep.f\n" s" loads-again.f" GUARD-MS CHECK-AS
   0 s" loads-lifetime: again, verified" EXPECT-KIND
   s" loads-lifetime: the pre-pass" T-LABEL
   s\" require ld-dep.f\n" SUBJ$ s" loads-again.f" GUARD-MS >MS CHECK:PREVERIFY-BYTES
   MATCH result
      ok OF 0= ENDOF
      err OF drop false ENDOF
   ;MATCH TTRUE
   s" loads-lifetime: the pre-pass, no line" 0 LOAD-COUNT ;


\ ---- the uses ---------------------------------------------------------------

variable USE-NODE                       \ the use line USE-FROM found


\ The first use line of the last check whose use starts at AT; -1 for none.
: USE-FROM ( n -- )
   {: at:n :}
   CHECK:VERIFY-USES$ JSONL-START
   begin
      JSONL-NEXT-OBJECT
      dup 0 < if USE-NODE ! exit then
      dup s" byte_start" NUMBER$ SB-RESET at FMT:SB-INT SB$ STR= 0=
   while
      drop
   repeat
   USE-NODE ! ;


: USE-NUM ( ptr u8 n ptr u8 n n -- )
   {: label:ptr labelu:n key:ptr keyu:n want:n :}
   label labelu T-LABEL USE-NODE @ key keyu NUMBER$
   SB-RESET want FMT:SB-INT SB$ T$= ;


\ The use line of the U bytes the first LEAD in the checked bytes ends with.
: USE ( ptr u8 n ptr u8 n n -- )
   {: label:ptr labelu:n lead:ptr leadu:n u:n :}
   lead leadu LEAD-END
   {: end:n :}
   end u - USE-FROM
   label labelu T-LABEL USE-NODE @ 0 >= TTRUE
   label labelu s" byte_end" end USE-NUM ;


\ No use line starts where the U bytes the first LEAD in the checked bytes
\ end.
: NO-USE ( ptr u8 n ptr u8 n n -- )
   {: label:ptr labelu:n lead:ptr leadu:n u:n :}
   lead leadu LEAD-END u - USE-FROM
   label labelu T-LABEL USE-NODE @ 0 < TTRUE ;


\ The declaration the use line USE found names: the U bytes the first LEAD in
\ TEXT ends with, TEXT the fixture FILE's bytes.
: USE-TARGET ( ptr u8 n ptr u8 n ptr u8 n n ptr u8 n -- )
   {: label:ptr labelu:n text:ptr textu:n lead:ptr leadu:n u:n file:ptr fileu:n :}
   text textu lead leadu END-IN
   {: end:n :}
   label labelu T-LABEL USE-NODE @ s" file" STRING$ file fileu AT$ T$=
   label labelu s" target_start" end u - USE-NUM
   label labelu s" target_end" end USE-NUM ;


: USE-COUNT ( ptr u8 n n -- )
   {: label:ptr labelu:n want:n :}
   label labelu T-LABEL CHECK:VERIFY-USES$ OBJECTS want T= ;


: USES-NEST$SRC ( -- ptr u8 n )
   s\" : CVT-UNEST ( -- n ) 3 ;\n" ;

: USES-DEP$SRC ( -- ptr u8 n )
   s\" require uses-nested.f\npackage CVT-UD\n: CVT-HELP ( n -- n ) 1 + ;\npublic\n: CVT-UONE ( -- n ) 1 CVT-HELP ;\n: CVT-UTWO ( -- n ) 2 ;\n;package\n: CVT-UGLOBAL ( -- n ) CVT-UNEST ;\n" ;


\ uses-dep.f, which requires uses-nested.f.
: USES-DEPS ( -- )
   s" uses-nested.f" USES-NEST$SRC FIXTURE
   s" uses-dep.f" USES-DEP$SRC FIXTURE ;


\ The subject's uses where each source occurrence binds, across scopes, its
\ dependency and that dependency's own: each line names the token that
\ declared what it binds. The uses inside the dependencies, and of words the
\ engine provides, have none.
: USES ( -- )
   USES-DEPS
   0 GEN-U !
   s\" require uses-dep.f\n: CVT-A ( -- n ) 1 ;\n: CVT-B ( -- n ) CVT-A 1 + ;\ndefer CVT-HOOK ( -- n )\n" GEN+
   s\" : CVT-SET ( -- ) [: CVT-B ;] is CVT-HOOK ;\n: CVT-TICK ( -- n ) ['] CVT-A execute ;\n" GEN+
   s\" : CVT-MAKER ( n -- ) drop ;\ngenerates: CVT-MAKER ( -- n )\nCVT-A drop\n' CVT-B drop\n" GEN+
   s\" package CVT-UP\n: CVT-A ( -- n ) 2 ;\n: CVT-PRIV ( -- n ) CVT-A ;\npublic\n" GEN+
   s\" : CVT-PUB ( -- n ) CVT-PRIV ;\nEXPORT CVT-UD:CVT-UTWO\n;package\n" GEN+
   s\" : CVT-QUAL ( -- n ) CVT-UD:CVT-UONE CVT-UP:CVT-PUB + CVT-UP:CVT-UTWO + ;\n" GEN+
   s\" using CVT-UD\n: CVT-USED ( -- n ) CVT-UONE ;\n;using\n" GEN+
   s\" : CVT-DEP ( -- n ) CVT-UGLOBAL CVT-UNEST + STR-SPACE + ;\n" GEN+
   s" uses.f" DEFS-CHECK 0 s" uses: verified" EXPECT-KIND
   s" uses: body call" s" : CVT-B ( -- n ) CVT-A" 5 USE
   s" uses: body call" DEFS-SRC$ s" : CVT-A" 5 s" uses.f" USE-TARGET
   s" uses: quotation call" s" [: CVT-B" 5 USE
   s" uses: quotation call" DEFS-SRC$ s" : CVT-B" 5 s" uses.f" USE-TARGET
   s" uses: is" s" ;] is CVT-HOOK" 8 USE
   s" uses: is" DEFS-SRC$ s" defer CVT-HOOK" 8 s" uses.f" USE-TARGET
   s" uses: [']" s" ['] CVT-A" 5 USE
   s" uses: [']" DEFS-SRC$ s" : CVT-A" 5 s" uses.f" USE-TARGET
   s" uses: generates:" s" generates: CVT-MAKER" 9 USE
   s" uses: generates:" DEFS-SRC$ s" : CVT-MAKER" 9 s" uses.f" USE-TARGET
   s" uses: top-level call" s\" \nCVT-A" 5 USE
   s" uses: top-level call" DEFS-SRC$ s" : CVT-A" 5 s" uses.f" USE-TARGET
   s" uses: top-level tick" s" ' CVT-B" 5 USE
   s" uses: top-level tick" DEFS-SRC$ s" : CVT-B" 5 s" uses.f" USE-TARGET
   s" uses: private over global" s" : CVT-PRIV ( -- n ) CVT-A" 5 USE
   s" uses: private over global" DEFS-SRC$ s\" package CVT-UP\n: CVT-A" 5 s" uses.f" USE-TARGET
   s" uses: private over global identity" T-LABEL
   USE-NODE @ s" decl_name" STRING$ s" cvt-a" T$=
   USE-NODE @ s" package" STRING$ s" cvt-up" T$=
   USE-NODE @ s" visibility" STRING$ s" private" T$=
   s" uses: private" s" : CVT-PUB ( -- n ) CVT-PRIV" 8 USE
   s" uses: private" DEFS-SRC$ s" : CVT-PRIV" 8 s" uses.f" USE-TARGET
   s" uses: private identity" T-LABEL
   USE-NODE @ s" decl_name" STRING$ s" cvt-priv" T$=
   USE-NODE @ s" package" STRING$ s" cvt-up" T$=
   USE-NODE @ s" visibility" STRING$ s" private" T$=
   s" uses: export operand" s" EXPORT CVT-UD:CVT-UTWO" 15 USE
   s" uses: export operand" USES-DEP$SRC s" : CVT-UTWO" 8 s" uses-dep.f" USE-TARGET
   s" uses: export operand identity" T-LABEL
   USE-NODE @ s" decl_name" STRING$ s" cvt-utwo" T$=
   USE-NODE @ s" package" STRING$ s" cvt-ud" T$=
   USE-NODE @ s" visibility" STRING$ s" public" T$=
   s" uses: qualified, dependency" s" CVT-UD:CVT-UONE" 15 USE
   s" uses: qualified, dependency" USES-DEP$SRC s" : CVT-UONE" 8 s" uses-dep.f" USE-TARGET
   s" uses: qualified" s" CVT-UP:CVT-PUB" 14 USE
   s" uses: qualified" DEFS-SRC$ s" : CVT-PUB" 7 s" uses.f" USE-TARGET
   s" uses: export alias" s" CVT-UP:CVT-UTWO" 15 USE
   s" uses: export alias" DEFS-SRC$ s" EXPORT CVT-UD:CVT-UTWO" 15 s" uses.f" USE-TARGET
   s" uses: export alias identity" T-LABEL
   USE-NODE @ s" decl_name" STRING$ s" cvt-utwo" T$=
   USE-NODE @ s" package" STRING$ s" cvt-up" T$=
   USE-NODE @ s" visibility" STRING$ s" public" T$=
   s" uses: used public" s" : CVT-USED ( -- n ) CVT-UONE" 8 USE
   s" uses: used public" USES-DEP$SRC s" : CVT-UONE" 8 s" uses-dep.f" USE-TARGET
   s" uses: dependency global" s" : CVT-DEP ( -- n ) CVT-UGLOBAL" 11 USE
   s" uses: dependency global" USES-DEP$SRC s" : CVT-UGLOBAL" 11 s" uses-dep.f" USE-TARGET
   s" uses: nested dependency" s" CVT-UGLOBAL CVT-UNEST" 9 USE
   s" uses: nested dependency" USES-NEST$SRC s" : CVT-UNEST" 9 s" uses-nested.f" USE-TARGET
   s" uses: engine-provided" s" STR-SPACE" 9 NO-USE
   s" uses: primitive" s" : CVT-MAKER ( n -- ) drop" 4 NO-USE
   s" uses: one line each" 16 USE-COUNT
   s" uses.f" DEFS-SRC$ FIXTURE
   CLI-START s" --verify-only" ARG+ s" uses.f" AT$ ARG+
   s" " CLI {: erru:n rc:n :}
   s" uses: cli verified" T-LABEL rc 0 T=
   s" uses: no use line on the cli" T-LABEL
   0 OUT CLI-OUT-U @ s\" \"target_start\"" CONTAINS?
   0 ERR erru s\" \"target_start\"" CONTAINS? or TFALSE ;


\ A string literal just before a call of a word a names: row declares binds
\ what XREF-FIND finds by the bytes between its quotes, at those bytes: a bare
\ name the global, never the open package's private twin; PKG:TAIL a public
\ alone; a word defined before the call. A computed operand, a caller with no
\ row of its own and an escape the bytes do not hold as written bind nothing,
\ not even the word spelled as that escape is written.
\ The row is a use of its word, which binds at top level too, and before a
\ definer's call the scan takes with the name after it: a `generates:` or
\ `does>` definer, a wrapper of one and a wrapper of a resident definer.
: NAMED-USES ( -- )
   0 GEN-U !
   s\" : CVT-ROOK ( -- n ) 1 ;\n: CVT-LOOK ( ptr u8 n -- ) 2drop ;\nnames: CVT-LOOK\n" GEN+
   s\" : CVT-SEEK ( -- ) s\" CVT-ROOK\" CVT-LOOK ;\n: CVT-HID ( -- n ) 5 ;\npackage CVT-NP\n" GEN+
   s\" : CVT-HID ( -- n ) 2 ;\n: CVT-PRIV ( -- ) s\" CVT-HID\" CVT-LOOK ;\n" GEN+
   s\" : CVT-QHID ( -- ) s\" CVT-NP:CVT-HID\" CVT-LOOK ;\npublic\n: CVT-PUB ( -- n ) 3 ;\n;package\n" GEN+
   s\" : CVT-QUAL ( -- ) s\" CVT-NP:CVT-PUB\" CVT-LOOK ;\n" GEN+
   s\" : CVT-COMP ( ptr u8 n -- ) s\" CVT-ROOK\" 2drop CVT-LOOK ;\n" GEN+
   s\" : CVT-THRU ( ptr u8 n -- ) CVT-LOOK ;\n: CVT-VIA ( -- ) s\" CVT-ROOK\" CVT-THRU ;\n" GEN+
   s\" : CVT-R\\x4fOK ( -- n ) 6 ;\n: CVT-ESC ( -- ) s\\\" CVT-R\\x4fOK\" CVT-LOOK ;\n" GEN+
   s\" : CVT-PLAIN ( -- ) s\\\" CVT-ROOK\" CVT-LOOK ;\n" GEN+
   s\" : CVT-EARLY ( -- ) s\" CVT-LATER\" CVT-LOOK ;\n: CVT-LATER ( -- n ) 4 ;\n" GEN+
   s\" s\" CVT-ROOK\" CVT-LOOK\n" GEN+
   s\" : CVT-NALIAS ( ptr u8 n -- ) 2drop parse-name 2drop s\" : CVT-NAX ( -- n ) 7 ;\" evaluate-closed ;\n" GEN+
   s\" generates: CVT-NALIAS ( -- n )\nnames: CVT-NALIAS\ns\" CVT-LATER\" CVT-NALIAS CVT-NAX\n" GEN+
   s\" : CVT-NDOES ( ptr u8 n -- ) 2drop create 0 , does> ( -- n ) @ ;\nnames: CVT-NDOES\n" GEN+
   s\" s\" CVT-HID\" CVT-NDOES CVT-NDX\n" GEN+
   s\" : CVT-NWRAP ( ptr u8 n -- ) CVT-NDOES ;\nnames: CVT-NWRAP\ns\" CVT-NP:CVT-PUB\" CVT-NWRAP CVT-NWX\n" GEN+
   s\" : CVT-NBUF ( ptr u8 n -- ) 2drop 16 BUFFER: ;\nnames: CVT-NBUF\ns\" CVT-LOOK\" CVT-NBUF CVT-NBX\n" GEN+
   s" named-uses.f" DEFS-CHECK 0 s" names: verified" EXPECT-KIND
   s" names: literal body" s\" CVT-SEEK ( -- ) s\" CVT-ROOK" 8 USE
   s" names: literal body" DEFS-SRC$ s" : CVT-ROOK" 8 s" named-uses.f" USE-TARGET
   s" names: a bare name is the global" s\" CVT-PRIV ( -- ) s\" CVT-HID" 7 USE
   s" names: a bare name is the global" DEFS-SRC$ s" : CVT-HID" 7 s" named-uses.f" USE-TARGET
   s" names: qualified names a public only" s\" CVT-QHID ( -- ) s\" CVT-NP:CVT-HID" 14 NO-USE
   s" names: qualified" s\" CVT-QUAL ( -- ) s\" CVT-NP:CVT-PUB" 14 USE
   s" names: qualified" DEFS-SRC$ s" : CVT-PUB" 7 s" named-uses.f" USE-TARGET
   s" names: computed operand" s\" CVT-COMP ( ptr u8 n -- ) s\" CVT-ROOK" 8 NO-USE
   s" names: caller without a row" s\" CVT-VIA ( -- ) s\" CVT-ROOK" 8 NO-USE
   s" names: escaped bytes differ" s\" CVT-ESC ( -- ) s\\\" CVT-R\\x4fOK" 11 NO-USE
   s" names: escaped spelling, plain bytes" s\" CVT-PLAIN ( -- ) s\\\" CVT-ROOK" 8 USE
   s" names: escaped spelling, plain bytes" DEFS-SRC$ s" : CVT-ROOK" 8 s" named-uses.f" USE-TARGET
   s" names: defined later" s\" CVT-EARLY ( -- ) s\" CVT-LATER" 9 NO-USE
   s" names: top level" s\" \ns\" CVT-ROOK" 8 USE
   s" names: top level" DEFS-SRC$ s" : CVT-ROOK" 8 s" named-uses.f" USE-TARGET
   s" names: row is a use of W" s" names: CVT-LOOK" 8 USE
   s" names: row is a use of W" DEFS-SRC$ s" : CVT-LOOK" 8 s" named-uses.f" USE-TARGET
   s" names: a generates: definer's call" s\" \ns\" CVT-LATER" 9 USE
   s" names: a generates: definer's call" DEFS-SRC$ s" : CVT-LATER" 9 s" named-uses.f" USE-TARGET
   s" names: a does> definer's call" s\" \ns\" CVT-HID" 7 USE
   s" names: a does> definer's call" DEFS-SRC$ s" : CVT-HID" 7 s" named-uses.f" USE-TARGET
   s" names: a definer wrapper's call" s\" \ns\" CVT-NP:CVT-PUB" 14 USE
   s" names: a definer wrapper's call" DEFS-SRC$ s" : CVT-PUB" 7 s" named-uses.f" USE-TARGET
   s" names: a resident definer's wrapper" s\" \ns\" CVT-LOOK" 8 USE
   s" names: a resident definer's wrapper" DEFS-SRC$ s" : CVT-LOOK" 8 s" named-uses.f" USE-TARGET
   s" names: one line each" 27 USE-COUNT ;


\ Two real includes of one file declare K at the same byte span in distinct
\ packages. Qualified calls and each package's using scope select different
\ declarations even though the shared source location is identical.
: VISIT-COMMON$SRC ( -- ptr u8 n )
   s\" : K ( -- n ) 1 ;\n" ;

\ Missing or nonnumeric visit is not a declaration identity.
: DECL-VISIT ( n -- n )
   s" decl_visit" NUMBER$ STR>NUMBER? MATCH option
      none OF -1 ENDOF
      some OF ENDOF
   ;MATCH ;

: VISIT-USE ( ptr u8 n ptr u8 n n n ptr u8 n -- )
   {: label:ptr labelu:n lead:ptr leadu:n u:n visit:n pkg:ptr pkgu:n :}
   label labelu lead leadu u USE
   label labelu T-LABEL USE-NODE @ DECL-VISIT visit T=
   label labelu T-LABEL USE-NODE @ s" decl_name" STRING$ s" k" T$=
   label labelu T-LABEL USE-NODE @ s" package" STRING$ pkg pkgu T$=
   label labelu T-LABEL USE-NODE @ s" visibility" STRING$ s" public" T$= ;

: USES-VISIT ( -- )
   s" visit-common.f" VISIT-COMMON$SRC FIXTURE
   0 GEN-U !
   s\" require lib/type/deftype.f\n" GEN+
   s\" package P\npublic\ninclude visit-common.f\n;package\npackage Q\npublic\ninclude visit-common.f\n;package\n" GEN+
   s\" : BOTH ( -- n ) P:K Q:K + ;\nusing P\n: P-BARE ( -- n ) K ;\n;using\nusing Q\n: Q-BARE ( -- n ) K ;\n;using\n" GEN+
   s\" DEFTYPE CVT-ID-T\n: CVT-ROUND ( -- n ) 1 >CVT-ID-T CVT-ID-T>N ;\n" GEN+
   s" uses-visit.f" 0 GEN GEN-U @ FIXTURE
   s" uses-visit.f" AT$ {: path:ptr pathu:n :}
   s" uses-visit: native loads" T-LABEL path pathu NATIVE-RC 0 T=
   s" uses-visit.f" DEFS-CHECK 0 s" uses-visit: verified" EXPECT-KIND
   CHECK:VERIFY-DEFS$ s" package" s" p" PACKET DEF-NODE !
   s" uses-visit: P declaration" s" :" s" p" s" public" DEF-WHO
   s" uses-visit: P declaration" s" word" s" K" DEF-STR
   s" uses-visit: P source" T-LABEL DEF-NODE @ s" file" STRING$ s" visit-common.f" AT$ T$=
   s" uses-visit: P span" s" byte_start" 2 DEF-NUM
   s" uses-visit: P span" s" byte_end" 3 DEF-NUM
   CHECK:VERIFY-DEFS$ s" package" s" q" PACKET DEF-NODE !
   s" uses-visit: Q declaration" s" :" s" q" s" public" DEF-WHO
   s" uses-visit: Q declaration" s" word" s" K" DEF-STR
   s" uses-visit: Q source" T-LABEL DEF-NODE @ s" file" STRING$ s" visit-common.f" AT$ T$=
   s" uses-visit: Q span" s" byte_start" 2 DEF-NUM
   s" uses-visit: Q span" s" byte_end" 3 DEF-NUM
   CHECK:VERIFY-DEFS$ s" package" s" p" PACKET {: pd:n :}
   s" uses-visit: P canonical tail" T-LABEL pd s" decl_name" STRING$ s" k" T$=
   pd DECL-VISIT {: pv:n :}
   CHECK:VERIFY-DEFS$ s" package" s" q" PACKET {: qd:n :}
   s" uses-visit: Q canonical tail" T-LABEL qd s" decl_name" STRING$ s" k" T$=
   qd DECL-VISIT {: qv:n :}
   s" uses-visit: P visit" T-LABEL pv 0 > TTRUE
   s" uses-visit: Q visit" T-LABEL qv 0 > TTRUE
   s" uses-visit: distinct visits" T-LABEL pv qv T<>
   s" uses-visit: qualified P" s" P:K" 3 pv s" p" VISIT-USE
   s" uses-visit: qualified P" VISIT-COMMON$SRC s" : K" 1 s" visit-common.f" USE-TARGET
   s" uses-visit: qualified Q" s" Q:K" 3 qv s" q" VISIT-USE
   s" uses-visit: qualified Q" VISIT-COMMON$SRC s" : K" 1 s" visit-common.f" USE-TARGET
   s" uses-visit: bare P" s" P-BARE ( -- n ) K" 1 pv s" p" VISIT-USE
   s" uses-visit: bare P" VISIT-COMMON$SRC s" : K" 1 s" visit-common.f" USE-TARGET
   s" uses-visit: bare Q" s" Q-BARE ( -- n ) K" 1 qv s" q" VISIT-USE
   s" uses-visit: bare Q" VISIT-COMMON$SRC s" : K" 1 s" visit-common.f" USE-TARGET
   CHECK:VERIFY-DEFS$ s" word" s" >CVT-ID-T" PACKET DEF-NODE !
   s" uses-visit: converter in" s" DEFTYPE" s" " s" global" DEF-WHO
   s" uses-visit: converter in" s" decl_name" s" >cvt-id-t" DEF-STR
   s" uses-visit: converter in" s" DEFTYPE CVT-ID-T" 8 s" uses-visit.f" DEF-SPAN
   DEF-NODE @ DECL-VISIT {: iv:n :}
   CHECK:VERIFY-DEFS$ s" word" s" CVT-ID-T>N" PACKET DEF-NODE !
   s" uses-visit: converter out" s" DEFTYPE" s" " s" global" DEF-WHO
   s" uses-visit: converter out" s" decl_name" s" cvt-id-t>n" DEF-STR
   s" uses-visit: converter out" s" DEFTYPE CVT-ID-T" 8 s" uses-visit.f" DEF-SPAN
   DEF-NODE @ DECL-VISIT {: ov:n :}
   s" uses-visit: converter visit" T-LABEL iv 0 > TTRUE
   s" uses-visit: shared converter visit" T-LABEL iv ov T=
   s" uses-visit: converter in use" s" >CVT-ID-T" 9 USE
   s" uses-visit: converter in target" DEFS-SRC$ s" DEFTYPE CVT-ID-T" 8 s" uses-visit.f" USE-TARGET
   s" uses-visit: converter in binding" T-LABEL USE-NODE @ DECL-VISIT iv T=
   s" uses-visit: converter in name" T-LABEL USE-NODE @ s" decl_name" STRING$ s" >cvt-id-t" T$=
   s" uses-visit: converter in package" T-LABEL USE-NODE @ s" package" STRING$ s" " T$=
   s" uses-visit: converter in visibility" T-LABEL USE-NODE @ s" visibility" STRING$ s" global" T$=
   s" uses-visit: converter out use" s" CVT-ID-T>N" 10 USE
   s" uses-visit: converter out target" DEFS-SRC$ s" DEFTYPE CVT-ID-T" 8 s" uses-visit.f" USE-TARGET
   s" uses-visit: converter out binding" T-LABEL USE-NODE @ DECL-VISIT ov T=
   s" uses-visit: converter out name" T-LABEL USE-NODE @ s" decl_name" STRING$ s" cvt-id-t>n" T$=
   s" uses-visit: converter out package" T-LABEL USE-NODE @ s" package" STRING$ s" " T$=
   s" uses-visit: converter out visibility" T-LABEL USE-NODE @ s" visibility" STRING$ s" global" T$=
   s" uses-visit: six calls" 6 USE-COUNT ;


\ Exporting a private K creates a public K in the same package. The operand
\ and earlier bare use keep the private identity; the qualified use selects
\ the public export, despite the same package and tail.
: USES-VISIBILITY ( -- )
   0 GEN-U !
   s\" package CVT-TWIN\n: K ( -- n ) 1 ;\n: PRIVATE-USE ( -- n ) K ;\npublic\nEXPORT K\n;package\n: PUBLIC-USE ( -- n ) CVT-TWIN:K ;\n" GEN+
   s" uses-visibility.f" 0 GEN GEN-U @ FIXTURE
   s" uses-visibility: native loads" T-LABEL
   s" uses-visibility.f" AT$ NATIVE-RC 0 T=
   s" uses-visibility.f" DEFS-CHECK 0 s" uses-visibility: verified" EXPECT-KIND
   CHECK:VERIFY-DEFS$ s" visibility" s" private" PACKET {: pd:n :}
   s" uses-visibility: private declaration" T-LABEL pd s" decl_name" STRING$ s" k" T$=
   pd s" package" STRING$ s" cvt-twin" T$=
   pd s" visibility" STRING$ s" private" T$=
   pd DECL-VISIT {: pv:n :}
   CHECK:VERIFY-DEFS$ s" kind" s" EXPORT" PACKET {: ed:n :}
   s" uses-visibility: public declaration" T-LABEL ed s" decl_name" STRING$ s" k" T$=
   ed s" package" STRING$ s" cvt-twin" T$=
   ed s" visibility" STRING$ s" public" T$=
   ed DECL-VISIT {: ev:n :}
   s" uses-visibility: declaration visits" T-LABEL pv 0 > ev 0 > and TTRUE
   s" uses-visibility: bare" s" PRIVATE-USE ( -- n ) K" 1 USE
   s" uses-visibility: bare target" DEFS-SRC$ s" : K" 1 s" uses-visibility.f" USE-TARGET
   s" uses-visibility: bare identity" T-LABEL USE-NODE @ DECL-VISIT pv T=
   USE-NODE @ s" decl_name" STRING$ s" k" T$=
   USE-NODE @ s" package" STRING$ s" cvt-twin" T$=
   USE-NODE @ s" visibility" STRING$ s" private" T$=
   s" uses-visibility: export operand" s" EXPORT K" 1 USE
   s" uses-visibility: export operand target" DEFS-SRC$ s" : K" 1 s" uses-visibility.f" USE-TARGET
   s" uses-visibility: export operand identity" T-LABEL USE-NODE @ DECL-VISIT pv T=
   USE-NODE @ s" decl_name" STRING$ s" k" T$=
   USE-NODE @ s" package" STRING$ s" cvt-twin" T$=
   USE-NODE @ s" visibility" STRING$ s" private" T$=
   s" uses-visibility: qualified" s" CVT-TWIN:K" 10 USE
   s" uses-visibility: qualified target" DEFS-SRC$ s" EXPORT K" 1 s" uses-visibility.f" USE-TARGET
   s" uses-visibility: qualified identity" T-LABEL USE-NODE @ DECL-VISIT ev T=
   USE-NODE @ s" decl_name" STRING$ s" k" T$=
   USE-NODE @ s" package" STRING$ s" cvt-twin" T$=
   USE-NODE @ s" visibility" STRING$ s" public" T$=
   s" uses-visibility: three uses" 3 USE-COUNT ;

\ Amending an effect keeps its declaration; re-including the same token after
\ undefine publishes another declaration lifetime in the same namespace.
: USES-VISIT-LIFETIME ( -- )
   s" visit-common.f" VISIT-COMMON$SRC FIXTURE
   0 GEN-U !
   s\" package P\npublic\ninclude visit-common.f\n: BEFORE ( -- n ) K ;\ns\" K\" s\" -- n\" TRUST\n: AFTER ( -- n ) K ;\nundefine K\ninclude visit-common.f\n: REPLACED ( -- n ) K ;\n;package\n" GEN+
   s" uses-lifetime.f" 0 GEN GEN-U @ FIXTURE
   s" uses-lifetime: native loads" T-LABEL
   s" uses-lifetime.f" AT$ NATIVE-RC 0 T=
   s" uses-lifetime.f" DEFS-CHECK
   0 s" uses-lifetime: verified" EXPECT-KIND
   s" uses-lifetime: declarations" T-LABEL CHECK:VERIFY-DEFS$ OBJECTS 5 T=
   CHECK:VERIFY-DEFS$ JSONL-START
   JSONL-NEXT-OBJECT DECL-VISIT {: first:n :}
   JSONL-NEXT-OBJECT drop
   JSONL-NEXT-OBJECT drop
   JSONL-NEXT-OBJECT DECL-VISIT {: later:n :}
   s" uses-lifetime: first visit" T-LABEL first 0 > TTRUE
   s" uses-lifetime: later visit" T-LABEL later 0 > TTRUE
   s" uses-lifetime: reinclude changes visit" T-LABEL first later T<>
   s" uses-lifetime: before amendment" s" BEFORE ( -- n ) K" 1 USE
   s" uses-lifetime: before amendment" T-LABEL USE-NODE @ DECL-VISIT first T=
   s" uses-lifetime: amendment preserves visit" s" AFTER ( -- n ) K" 1 USE
   s" uses-lifetime: amendment preserves visit" T-LABEL USE-NODE @ DECL-VISIT first T=
   s" uses-lifetime: replacement visit" s" REPLACED ( -- n ) K" 1 USE
   s" uses-lifetime: replacement visit" T-LABEL USE-NODE @ DECL-VISIT later T=
   s" uses-lifetime: three uses" 3 USE-COUNT ;


\ A name the checker refuses binds nothing: one two usings both export, one a
\ using shadows a global with, one nothing defines. The scan goes on, and the
\ global binds once the using closes.
: USES-REFUSED ( -- )
   0 GEN-U !
   s\" package CVT-RA\npublic\n: CVT-SAME ( -- n ) 1 ;\n: CVT-SHADOW ( -- n ) 2 ;\n: CVT-SLOT ( -- n ) 5 ;\n;package\n" GEN+
   s\" package CVT-RB\npublic\n: CVT-SAME ( -- n ) 3 ;\n;package\n: CVT-SHADOW ( -- n ) 4 ;\n" GEN+
   s\" using CVT-RA\nusing CVT-RB\n: CVT-AMBIG ( -- n ) CVT-SAME ;\n;using\n;using\n" GEN+
   s\" defer CVT-SLOT ( -- n )\nusing CVT-RA\n: CVT-SHADOWED ( -- n ) CVT-SHADOW ;\n: CVT-TICK ( -- ) ['] CVT-SHADOW drop ;\n: CVT-IS ( [ -- n ] -- ) is CVT-SLOT ;\n;using\n" GEN+
   s\" : CVT-UNKNOWN ( -- n ) CVT-NOWHERE ;\n: CVT-AFTER ( -- n ) CVT-SHADOW ;\n: CVT-IS-AFTER ( [ -- n ] -- ) is CVT-SLOT ;\n" GEN+
   s" uses-refused.f" DEFS-CHECK 1 s" uses-refused: refused" EXPECT-KIND
   s" uses-refused: ambiguous" s" CVT-AMBIG ( -- n ) CVT-SAME" 8 NO-USE
   s" uses-refused: shadowed" s" CVT-SHADOWED ( -- n ) CVT-SHADOW" 10 NO-USE
   s" uses-refused: tick target" s" CVT-TICK ( -- ) ['] CVT-SHADOW" 10 NO-USE
   s" uses-refused: is target" s" CVT-IS ( [ -- n ] -- ) is CVT-SLOT" 8 NO-USE
   s" uses-refused: undefined" s" CVT-NOWHERE" 11 NO-USE
   s" uses-refused: the global after" s" CVT-AFTER ( -- n ) CVT-SHADOW" 10 USE
   s" uses-refused: the global after" DEFS-SRC$ s\" ;package\n: CVT-SHADOW" 10 s" uses-refused.f" USE-TARGET
   s" uses-refused: is after" s" CVT-IS-AFTER ( [ -- n ] -- ) is CVT-SLOT" 8 USE
   s" uses-refused: is after" DEFS-SRC$ s" defer CVT-SLOT" 8 s" uses-refused.f" USE-TARGET
   s" uses-refused: two lines" 2 USE-COUNT ;


\ A use binds the declaration visible where it stands: the first before
\ undefine retires it, the second after.
: USES-ORDER ( -- )
   0 GEN-U !
   s\" : CVT-W ( -- n ) 1 ;\n: CVT-EARLY ( -- n ) CVT-W ;\nundefine CVT-W\n" GEN+
   s\" : CVT-W ( -- n ) 2 ;\n: CVT-LATE ( -- n ) CVT-W ;\nCVT-W drop\n" GEN+
   s" uses-order.f" DEFS-CHECK 0 s" uses-order: verified" EXPECT-KIND
   s" uses-order: before undefine" s" CVT-EARLY ( -- n ) CVT-W" 5 USE
   s" uses-order: before undefine" DEFS-SRC$ s" : CVT-W" 5 s" uses-order.f" USE-TARGET
   s" uses-order: undefine binds nothing" s" undefine CVT-W" 5 NO-USE
   s" uses-order: after" s" CVT-LATE ( -- n ) CVT-W" 5 USE
   s" uses-order: after" DEFS-SRC$ s\" undefine CVT-W\n: CVT-W" 5 s" uses-order.f" USE-TARGET
   s" uses-order: top level after" s\" \nCVT-W" 5 USE
   s" uses-order: top level after" DEFS-SRC$ s\" undefine CVT-W\n: CVT-W" 5 s" uses-order.f" USE-TARGET
   s" uses-order: three lines" 3 USE-COUNT ;


\ A refused body keeps its declared signature, and its uses bind it. A refused
\ signature type retains nothing, so a use of it is undefined and binds
\ nothing.
: USES-RECOVERY ( -- )
   0 GEN-U !
   s\" : CVT-BAD ( -- n ) ;\n: CVT-USE-BAD ( -- n ) CVT-BAD 1 + ;\n" GEN+
   s\" : CVT-GONE ( -- cvt-no-type ) 1 ;\n: CVT-KEPT ( -- n ) 2 ;\n" GEN+
   s\" : CVT-USE-KEPT ( -- n ) CVT-KEPT CVT-GONE + ;\n" GEN+
   s" uses-recovery.f" DEFS-CHECK 1 s" uses-recovery: refused" EXPECT-KIND
   s" uses-recovery: kept signature" s" CVT-USE-BAD ( -- n ) CVT-BAD" 7 USE
   s" uses-recovery: kept signature" DEFS-SRC$ s" : CVT-BAD" 7 s" uses-recovery.f" USE-TARGET
   s" uses-recovery: after a refusal" s" CVT-USE-KEPT ( -- n ) CVT-KEPT" 8 USE
   s" uses-recovery: after a refusal" DEFS-SRC$ s" : CVT-KEPT" 8 s" uses-recovery.f" USE-TARGET
   s" uses-recovery: a refused type retains nothing" s" CVT-KEPT CVT-GONE" 8 NO-USE
   s" uses-recovery: two lines" 2 USE-COUNT ;


\ A body the checker defers to the run (src/core/checker.f CHECK-VERDICT 2: it
\ names a word only a rendering statement in scope can define) keeps its
\ declared signature, and its uses bind it; the name only the run defines
\ binds nothing.
: USES-DEFERRED ( -- )
   0 GEN-U !
   s\" package CVT-UF\n: CVT-RENDER ( -- ) s\" : CVT-MADE ( -- n ) 7 ;\" INCLUDE-EVALUATE ;\nCVT-RENDER\n" GEN+
   s\" : CVT-DEFERRED ( -- n ) CVT-MADE ;\n: CVT-USE ( -- n ) CVT-DEFERRED ;\n;package\n" GEN+
   s" uses-deferred.f" DEFS-CHECK 5 s" uses-deferred: deferred" EXPECT-KIND
   s" uses-deferred: the renderer" s\" \nCVT-RENDER" 10 USE
   s" uses-deferred: the renderer" DEFS-SRC$ s" : CVT-RENDER" 10 s" uses-deferred.f" USE-TARGET
   s" uses-deferred: the deferred body" s" CVT-USE ( -- n ) CVT-DEFERRED" 12 USE
   s" uses-deferred: the deferred body" DEFS-SRC$ s" : CVT-DEFERRED" 12 s" uses-deferred.f" USE-TARGET
   s" uses-deferred: the run's name" s" CVT-DEFERRED ( -- n ) CVT-MADE" 8 NO-USE
   s" uses-deferred: two lines" 2 USE-COUNT ;


\ A duplicate definition is refused and retains nothing: the uses after it
\ bind the first.
: USES-DUPLICATE ( -- )
   0 GEN-U !
   s\" : CVT-D ( -- n ) 1 ;\n: CVT-D ( -- n ) 2 ;\n: CVT-DUSE ( -- n ) CVT-D ;\nCVT-D drop\n" GEN+
   s" uses-duplicate.f" DEFS-CHECK 1 s" uses-duplicate: refused" EXPECT-KIND
   s" uses-duplicate: body" s" CVT-DUSE ( -- n ) CVT-D" 5 USE
   s" uses-duplicate: body" DEFS-SRC$ s" : CVT-D" 5 s" uses-duplicate.f" USE-TARGET
   s" uses-duplicate: top level" s\" \nCVT-D" 5 USE
   s" uses-duplicate: top level" DEFS-SRC$ s" : CVT-D" 5 s" uses-duplicate.f" USE-TARGET
   s" uses-duplicate: two lines" 2 USE-COUNT ;


\ The checker's record store and its location table both start with room for
\ fewer declarations than this subject makes, and grow by copying
\ (src/core/checker.f USIGS-GROW, ARENA-ROWS-ENSURE), which keeps every
\ record's offset: the first and the last declaration keep their own
\ locations. An engine bakes its store with one to two 64 KiB grains of room
\ (USIGS-PERSIST-CAP), the child's own load takes some 36 KB of it, and the
\ table starts at 64 rows. These declarations take some 170 KB: in the child
\ on an unsealed engine the store grew from 1,835,008 to 3,670,016 bytes.
3000 constant GROWTH-DEFS

: USES-GROWTH ( -- )
   0 GEN-U !
   s\" : CVT-GFIRST ( -- n ) 1 ;\n" GEN+
   GROWTH-DEFS 0 ?do
      s" : CVT-G" GEN+ i GEN-N+ s\"  ( -- n ) 1 ;\n" GEN+
   loop
   s\" : CVT-GLAST ( -- n ) 2 ;\n: CVT-GUSE ( -- n ) CVT-GFIRST CVT-GLAST + ;\n" GEN+
   s" uses-growth.f" DEFS-CHECK 0 s" uses-growth: verified" EXPECT-KIND
   s" uses-growth: the first declaration" s" CVT-GUSE ( -- n ) CVT-GFIRST" 10 USE
   s" uses-growth: the first declaration" DEFS-SRC$ s" : CVT-GFIRST" 10 s" uses-growth.f" USE-TARGET
   s" uses-growth: the last declaration" s" CVT-GFIRST CVT-GLAST" 9 USE
   s" uses-growth: the last declaration" DEFS-SRC$ s" : CVT-GLAST" 9 s" uses-growth.f" USE-TARGET
   s" uses-growth: two lines" 2 USE-COUNT ;


: SWAP-A$SRC ( -- ptr u8 n )
   s\" : DEF-DEPA ( -- n ) 1 ;\n: DEF-DEPB ( -- n n ) 2 3 ;\n" ;

\ SWAP-A$SRC's two definitions swapped.
: SWAP-B$SRC ( -- ptr u8 n )
   s\" : DEF-DEPB ( -- n n ) 2 3 ;\n: DEF-DEPA ( -- n ) 1 ;\n" ;


\ swap-dep.f rewritten between two checks of the same subject, its two
\ definitions swapped: the second check's use of DEF-DEPB names the span the
\ first check's use of DEF-DEPA named, bytes 2 to 10 of swap-dep.f, and only
\ the digests on swap-dep.f's file lines tell the two declarations apart.
: IDENTITY-SWAP ( -- )
   s" swap-dep.f" SWAP-A$SRC FIXTURE
   0 GEN-U !
   s\" require swap-dep.f\n: CVT-SWAP-A ( -- n ) DEF-DEPA ;\n: CVT-SWAP-B ( -- n n ) DEF-DEPB ;\n" GEN+
   s" swap.f" DEFS-CHECK 0 s" identity-swap: the first check" EXPECT-KIND
   s" identity-swap: DEF-DEPA" s" ) DEF-DEPA" 8 USE
   s" identity-swap: DEF-DEPA" SWAP-A$SRC s" : DEF-DEPA" 8 s" swap-dep.f" USE-TARGET
   CHECK:VERIFY-FILES$ JSONL-START JSONL-NEXT-OBJECT drop
   s" identity-swap: the first bytes" s" swap-dep.f" SWAP-A$SRC SHA$ NEXT-FILE
   s" swap-dep.f" SWAP-B$SRC FIXTURE
   s" swap.f" DEFS-CHECK 0 s" identity-swap: the second check" EXPECT-KIND
   s" identity-swap: DEF-DEPB" s" ) DEF-DEPB" 8 USE
   s" identity-swap: DEF-DEPB at DEF-DEPA's span" SWAP-A$SRC s" : DEF-DEPA" 8 s" swap-dep.f" USE-TARGET
   CHECK:VERIFY-FILES$ JSONL-START JSONL-NEXT-OBJECT drop
   s" identity-swap: the second bytes" s" swap-dep.f" SWAP-B$SRC SHA$ NEXT-FILE ;


\ ---- the candidates ---------------------------------------------------------

variable CAND-NODE                      \ the candidate line CAND found
DYNAMIC-BUFFER SEEN u8                  \ an unarmed check's outputs, then an armed one's
variable SEEN-U


\ Check the generated source as the fixture NAME with its cursor at byte AT.
: CANDS-AT ( ptr u8 n n -- CHECK:verdict )
   {: name:ptr nameu:n at:n :}
   0 GEN DEFS-SRC-A !  GEN-U @ DEFS-SRC-U !
   name nameu AT$ SUBJ SUBJ-U COPY!
   DEFS-SRC$ SUBJ$ at [: drop ;] GUARD-MS >MS CHECK:VERIFY-BYTES-AT ;


\ CANDS-AT where the first LEAD in the generated source ends.
: CANDS ( ptr u8 n ptr u8 n -- CHECK:verdict )
   {: name:ptr nameu:n lead:ptr leadu:n :}
   0 GEN GEN-U @ lead leadu END-IN
   {: at:n :}
   name nameu at CANDS-AT ;


\ The candidate line of the last check spelling WORD as written; -1 for none.
: CAND ( ptr u8 n -- )
   {: w:ptr wu:n :}
   CHECK:VERIFY-CANDIDATES$ s" word" w wu PACKET CAND-NODE ! ;


: CAND-NUM ( ptr u8 n ptr u8 n n -- )
   {: label:ptr labelu:n key:ptr keyu:n want:n :}
   label labelu T-LABEL CAND-NODE @ key keyu NUMBER$
   SB-RESET want FMT:SB-INT SB$ T$= ;


\ The candidate CAND found is declared by the U bytes the first LEAD in TEXT
\ ends with, TEXT the fixture FILE's bytes.
: CAND-TARGET ( ptr u8 n ptr u8 n ptr u8 n n ptr u8 n -- )
   {: label:ptr labelu:n text:ptr textu:n lead:ptr leadu:n u:n file:ptr fileu:n :}
   text textu lead leadu END-IN
   {: end:n :}
   label labelu T-LABEL CAND-NODE @ 0 >= TTRUE
   label labelu T-LABEL CAND-NODE @ s" file" STRING$ file fileu AT$ T$=
   label labelu s" target_start" end u - CAND-NUM
   label labelu s" target_end" end CAND-NUM ;


\ The candidate CAND found has no declaration in the files checked: no file and
\ no target.
: CAND-BARE ( ptr u8 n -- )
   {: label:ptr labelu:n :}
   label labelu T-LABEL CAND-NODE @ 0 >= TTRUE
   label labelu T-LABEL CAND-NODE @ s" file" J-STR VALUE 0 < TTRUE
   label labelu T-LABEL CAND-NODE @ s" target_start" J-NUM VALUE 0 < TTRUE ;


\ CAND found no line.
: NO-CAND ( ptr u8 n -- )
   {: label:ptr labelu:n :}
   label labelu T-LABEL CAND-NODE @ 0 < TTRUE ;


: CAND-COUNT ( ptr u8 n n -- )
   {: label:ptr labelu:n want:n :}
   label labelu T-LABEL CHECK:VERIFY-CANDIDATES$ OBJECTS want T= ;


\ A body token's prefix offers each word that binds there with the declaration
\ it binds, and an engine word the body has not used yet with none. A TRUSTED:
\ body read before the cursor's body does not take the cursor, and a word is
\ not offered in its own body.
: CANDS-BODY ( -- )
   0 GEN-U !
   s\" : CVT-KA ( -- n ) 1 ;\nTRUSTED: CVT-KT ( -- n ) 2 ;\n: CVT-KB ( -- n ) CVT-K 1 + ;\n: CVT-KLATE ( -- n ) 3 ;\n" GEN+
   s" cands-body.f" s" ( -- n ) CVT-K" CANDS 1 s" cands-body: refused" EXPECT-KIND
   s" CVT-KA" CAND
   s" cands-body: a word declared before" DEFS-SRC$ s" : CVT-KA" 6 s" cands-body.f" CAND-TARGET
   s" CVT-KT" CAND
   s" cands-body: the trusted word" DEFS-SRC$ s" TRUSTED: CVT-KT" 6 s" cands-body.f" CAND-TARGET
   s" CVT-KB" CAND
   s" cands-body: not the body's own word" NO-CAND
   s" CVT-KLATE" CAND
   s" cands-body: not a word declared after" NO-CAND
   s" cands-body: no other word" 2 CAND-COUNT
   0 GEN-U !
   s\" : CVT-KE ( n -- n n ) du ;\n" GEN+
   s" cands-engine.f" s" ) du" CANDS drop
   s" dup" CAND
   s" cands-body: an engine word not yet taken" CAND-BARE ;


\ A top-level cursor offers the words that bind there: in the blank before a
\ token, at a prefix, at the end of the file, and in the blanks before a
\ comment the file ends with, which belong to its end.
: CANDS-TOP ( -- )
   0 GEN-U !
   s\" : CVT-KA ( -- n ) 1 ;\nCVT-KA drop  CVT-KA drop\nCVT-K" GEN+
   s" cands-top.f" s" drop " CANDS drop
   s" CVT-KA" CAND
   s" cands-top: the blank before a token" DEFS-SRC$ s" : CVT-KA" 6 s" cands-top.f" CAND-TARGET
   s" cands-top.f" s\" drop\nCVT-K" CANDS drop
   s" CVT-KA" CAND
   s" cands-top: a prefix" DEFS-SRC$ s" : CVT-KA" 6 s" cands-top.f" CAND-TARGET
   0 GEN-U !
   s\" : CVT-KA ( -- n ) 1 ;\n" GEN+
   s" cands-end.f" GEN-U @ CANDS-AT drop
   s" CVT-KA" CAND
   s" cands-top: the end of the file" DEFS-SRC$ s" : CVT-KA" 6 s" cands-end.f" CAND-TARGET
   0 GEN-U !
   s\" : CVT-KA ( -- n ) 1 ;\n \\ hello wor\n" GEN+
   s" cands-end-comment.f" s\" ;\n" CANDS drop
   s" CVT-KA" CAND
   s" cands-top: the blanks before a closed comment that ends the file" DEFS-SRC$ s" : CVT-KA" 6 s" cands-end-comment.f" CAND-TARGET ;


\ Nothing is offered where no word binds: a definition's name, its signature,
\ a comment, a string, a TRUSTED: body, a type's name, the stretch a deferring
\ top-level word leaves to the run, a body the scan never reaches, or the end
\ of a comment or string that the file ends in before it closes.
: CANDS-NONE ( -- )
   0 GEN-U !
   s\" : CVT-KA ( -- n ) 1 ;\n: CVT-KN ( -- n ) 1 ;\n" GEN+
   s" cands-none.f" s" : CVT-KN" CANDS drop
   s" cands-none: a definition's name" 0 CAND-COUNT
   s" cands-none.f" s" CVT-KN ( -- n" CANDS drop
   s" cands-none: a signature" 0 CAND-COUNT
   0 GEN-U !
   s\" : CVT-KA ( -- n ) 1 ;\n\\ CVT-K\n: CVT-KS ( -- ptr u8 n ) s\" CVT-K\" ;\n" GEN+
   s\" TRUSTED: CVT-KT ( -- n ) CVT-K ;\nDEFTYPE CVT-K\n" GEN+
   s" cands-skip.f" s" \ CVT-K" CANDS drop
   s" cands-none: a comment" 0 CAND-COUNT
   s" cands-skip.f" s\" s\" CVT-K" CANDS drop
   s" cands-none: a string" 0 CAND-COUNT
   s" cands-skip.f" s" ( -- n ) CVT-K" CANDS drop
   s" cands-none: a TRUSTED: body" 0 CAND-COUNT
   s" cands-skip.f" s" DEFTYPE CVT-K" CANDS drop
   s" cands-none: a type's name" 0 CAND-COUNT
   0 GEN-U !
   s\" : CVT-KA ( -- n ) 1 ;\ndefer CVT-KD ( -- n )\nCVT-KD CVT-K\n" GEN+
   s" cands-deferred.f" s" CVT-KD CVT-K" CANDS drop
   s" cands-none: a deferred stretch" 0 CAND-COUNT
   0 GEN-U !
   s\" : CVT-KA ( -- n ) 1 ;\n: CVT-KA ( -- n ) CVT-K ;\n" GEN+
   s" cands-dup.f" s" ) CVT-K" CANDS drop
   s" cands-none: a body after a duplicate" 0 CAND-COUNT
   0 GEN-U !
   s\" : CVT-KA ( -- n ) 1 ;\n\\ hello wor" GEN+
   s" cands-open-line.f" GEN-U @ CANDS-AT drop
   s" cands-none: the end of an open line comment" 0 CAND-COUNT
   0 GEN-U !
   s\" : CVT-KA ( -- n ) 1 ;\n( hello wor" GEN+
   s" cands-open-paren.f" GEN-U @ CANDS-AT drop
   s" cands-none: the end of an open paren comment" 0 CAND-COUNT
   \ Pins the product answer: the core refuses an open s" (E-DISC-UNTERM) before the child, unlike \ and (.
   0 GEN-U !
   s\" : CVT-KA ( -- n ) 1 ;\ns\" hello wor" GEN+
   s" cands-open-string.f" GEN-U @ CANDS-AT drop
   s" cands-none: the end of an open string" 0 CAND-COUNT ;


\ A tick or `is` target offers the words that bind there, never a local.
: CANDS-TARGETS ( -- )
   0 GEN-U !
   s\" : CVT-KA ( -- n ) 1 ;\ndefer CVT-KD ( -- n )\n" GEN+
   s\" : CVT-KT ( n -- n ) {: CVT-KL:n :} ['] CVT-K drop CVT-KL ;\n" GEN+
   s\" : CVT-KI ( [ -- n ] n -- ) {: CVT-KL:n :} is CVT-K ;\n" GEN+
   s" cands-target.f" s" ['] CVT-K" CANDS drop
   s" CVT-KA" CAND
   s" cands-targets: a tick target" DEFS-SRC$ s" : CVT-KA" 6 s" cands-target.f" CAND-TARGET
   s" CVT-KL" CAND
   s" cands-targets: no local for a tick" NO-CAND
   s" cands-target.f" s" is CVT-K" CANDS drop
   s" CVT-KD" CAND
   s" cands-targets: an is target" DEFS-SRC$ s" defer CVT-KD" 6 s" cands-target.f" CAND-TARGET
   s" CVT-KL" CAND
   s" cands-targets: no local for is" NO-CAND ;


\ Two locals that differ only in case are each offered as declared.
: CANDS-LOCALS ( -- )
   0 GEN-U !
   s\" : CVT-KC ( n n -- n ) {: cvt-kx:n CVT-KX:n :} cvt-kx CVT-K ;\n" GEN+
   s" cands-locals.f" s" cvt-kx CVT-K" CANDS drop
   s" cvt-kx" CAND
   s" cands-locals: the lower-case local" CAND-BARE
   s" CVT-KX" CAND
   s" cands-locals: the upper-case local" CAND-BARE ;


\ A name the scope does not select is not offered, while a global beside it
\ is: a used public before its using and after it, one two used publics both
\ export, and one a used public shadows a global with.
: CANDS-REFUSED ( -- )
   0 GEN-U !
   s\" : CVT-SOLO ( -- n ) 9 ;\npackage CVT-RA\npublic\n: CVT-SAME ( -- n ) 1 ;\n: CVT-SHADOW ( -- n ) 2 ;\n;package\n" GEN+
   s\" package CVT-RB\npublic\n: CVT-SAME ( -- n ) 3 ;\n;package\n: CVT-SHADOW ( -- n ) 4 ;\n" GEN+
   s\" : CVT-KBEFORE ( -- n ) CVT-S ;\nusing CVT-RA\nusing CVT-RB\n: CVT-KAMBIG ( -- n ) CVT-S ;\n;using\n;using\n" GEN+
   s\" using CVT-RA\n: CVT-KSHADOW ( -- n ) CVT-S ;\n;using\n: CVT-KAFTER ( -- n ) CVT-S ;\n" GEN+
   s" cands-refused.f" s" CVT-KBEFORE ( -- n ) CVT-S" CANDS drop
   s" CVT-SOLO" CAND
   s" cands-refused: the global before the using" DEFS-SRC$ s" : CVT-SOLO" 8 s" cands-refused.f" CAND-TARGET
   s" CVT-SAME" CAND
   s" cands-refused: a public before its using" NO-CAND
   s" cands-refused.f" s" CVT-KAMBIG ( -- n ) CVT-S" CANDS drop
   s" CVT-SOLO" CAND
   s" cands-refused: the global beside two usings" DEFS-SRC$ s" : CVT-SOLO" 8 s" cands-refused.f" CAND-TARGET
   s" CVT-SAME" CAND
   s" cands-refused: two used publics" NO-CAND
   s" cands-refused.f" s" CVT-KSHADOW ( -- n ) CVT-S" CANDS drop
   s" CVT-SOLO" CAND
   s" cands-refused: the global inside the using" DEFS-SRC$ s" : CVT-SOLO" 8 s" cands-refused.f" CAND-TARGET
   s" CVT-SHADOW" CAND
   s" cands-refused: a used public over a global" NO-CAND
   s" cands-refused.f" s" CVT-KAFTER ( -- n ) CVT-S" CANDS drop
   s" CVT-SOLO" CAND
   s" cands-refused: the global after the using" DEFS-SRC$ s" : CVT-SOLO" 8 s" cands-refused.f" CAND-TARGET
   s" CVT-SAME" CAND
   s" cands-refused: a public after its using" NO-CAND ;


\ A word undefine retires is not offered after it, while a word beside it is,
\ and its redefinition is offered with the later declaration.
: CANDS-ORDER ( -- )
   0 GEN-U !
   s\" : CVT-KEEP ( -- n ) 0 ;\n: CVT-KW ( -- n ) 1 ;\nundefine CVT-KW\n: CVT-KG ( -- n ) CVT-K ;\n" GEN+
   s\" : CVT-KW ( -- n ) 2 ;\n: CVT-KH ( -- n ) CVT-K ;\n" GEN+
   s" cands-order.f" s" CVT-KG ( -- n ) CVT-K" CANDS drop
   s" CVT-KEEP" CAND
   s" cands-order: the word beside it" DEFS-SRC$ s" : CVT-KEEP" 8 s" cands-order.f" CAND-TARGET
   s" CVT-KW" CAND
   s" cands-order: undefined" NO-CAND
   s" cands-order.f" s" CVT-KH ( -- n ) CVT-K" CANDS drop
   s" CVT-KW" CAND
   s" cands-order: the redefinition" DEFS-SRC$ s\" CVT-K ;\n: CVT-KW" 6 s" cands-order.f" CAND-TARGET ;


\ Each word as its scope selects it, with the declaration it binds in its own
\ file: the open package's private word over a dependency's global of that
\ name, a used public, a dependency's qualified public, a qualified export's
\ tail. A cursor in the subject's comment is not placed at a dependency's token
\ at the same byte.
: CANDS-SCOPES ( -- )
   USES-DEPS
   0 GEN-U !
   s\" require uses-dep.f\npackage CVT-KP\n: CVT-UGLOBAL ( -- n ) 7 ;\n: CVT-KQ ( -- n ) CVT-UG ;\n;package\n" GEN+
   s" cands-private.f" s" ) CVT-UG" CANDS drop
   s" CVT-UGLOBAL" CAND
   s" cands-scopes: the private word over the global" DEFS-SRC$ s\" package CVT-KP\n: CVT-UGLOBAL" 11 s" cands-private.f" CAND-TARGET
   s" cands-scopes: offered once" 1 CAND-COUNT
   0 GEN-U !
   s\" require uses-dep.f\nusing CVT-UD\n: CVT-KU ( -- n ) CVT-UO ;\n;using\n: CVT-KV ( -- n ) CVT-UD:CVT-UO ;\n" GEN+
   s" cands-used.f" s" ) CVT-UO" CANDS drop
   s" CVT-UONE" CAND
   s" cands-scopes: a used public" USES-DEP$SRC s" : CVT-UONE" 8 s" uses-dep.f" CAND-TARGET
   s" cands-used.f" s" CVT-UD:CVT-UO" CANDS drop
   s" CVT-UD:CVT-UONE" CAND
   s" cands-scopes: a qualified public" USES-DEP$SRC s" : CVT-UONE" 8 s" uses-dep.f" CAND-TARGET
   0 GEN-U !
   s\" require uses-dep.f\npackage CVT-KX\npublic\nEXPORT CVT-UD:CVT-UTWO\n;package\n: CVT-KY ( -- n ) CVT-KX:CVT-UT ;\n" GEN+
   s" cands-export.f" s" CVT-KX:CVT-UT" CANDS drop
   s" CVT-KX:CVT-UTWO" CAND
   s" cands-scopes: a qualified export's tail" DEFS-SRC$ s" EXPORT CVT-UD:CVT-UTWO" 15 s" cands-export.f" CAND-TARGET
   USES-DEP$SRC s" 1 CVT-HE" END-IN
   {: at:n :}
   0 GEN-U !
   s\" require uses-dep.f\n\\ " GEN+
   at 0 ?do s" x" GEN+ loop
   s\" \n: CVT-KZ ( -- n ) 1 ;\n" GEN+
   s" cands-ident.f" at CANDS-AT drop
   s" cands-scopes: not a dependency's token" 0 CAND-COUNT ;


\ The last check's verdict and outputs onto SEEN, each ended by a NUL.
: SEEN+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   SEEN-U @ u + 1+ SEEN-RESERVE
   a SEEN-U @ SEEN u BYTE-COPY
   0 SEEN-U @ u + SEEN c!
   SEEN-U @ u + 1+ SEEN-U ! ;

: SEEN-CHECK+ ( CHECK:verdict -- )
   KIND {: k:n :}
   SEEN-U @ 1+ SEEN-RESERVE
   k $30 + SEEN-U @ SEEN c!
   SEEN-U @ 1+ SEEN-U !
   CHECK:VERIFY-OUT$ SEEN+  CHECK:VERIFY-DEFS$ SEEN+
   CHECK:VERIFY-USES$ SEEN+  CHECK:VERIFY-FILES$ SEEN+ ;


\ The cursor only observes: the same bytes checked with and without it give the
\ same verdict, packets, definitions, uses and files, byte for byte; and
\ --verify-only, which has no cursor, writes no candidate line.
: CANDS-OBSERVER ( -- )
   USES-DEPS
   0 GEN-U !
   s\" require uses-dep.f\n: CVT-KA ( -- n ) 1 ;\n: CVT-KB ( -- n ) CVT-KA CVT-UGLOBAL + ;\n" GEN+
   s\" : CVT-KR ( -- n ) CVT-KA CVT-NOPE ;\n: CVT-KC ( -- n ) CVT-KA CVT-KB + ;\n" GEN+
   0 SEEN-U !
   s" cands-observe.f" DEFS-CHECK SEEN-CHECK+
   SEEN-U @
   {: half:n :}
   s" cands-observe.f" s" CVT-KA CVT-KB" CANDS SEEN-CHECK+
   s" CVT-KB" CAND
   s" cands-observer: the cursor offered" DEFS-SRC$ s" : CVT-KB" 6 s" cands-observe.f" CAND-TARGET
   s" cands-observer: the same verdict and outputs" T-LABEL
   0 SEEN half  half SEEN SEEN-U @ half -  T$=
   s" cands-observe.f" DEFS-SRC$ FIXTURE
   CLI-START s" --verify-only" ARG+ s" cands-observe.f" AT$ ARG+
   s" " CLI drop
   {: erru:n :}
   s" cands-observer: no candidate line on the cli" T-LABEL
   0 OUT CLI-OUT-U @ s" candidate" CONTAINS?
   0 ERR erru s" candidate" CONTAINS? or TFALSE ;


\ ---- the measurement -------------------------------------------------------

: MS. ( -- )
   mono-ns START-NS @ - PROC-NS-PER-MS / FMT:.INT s"  ms" type ;


: MEASURE ( -- )
   mono-ns START-NS !
   DEP$SRC s" dep.f" GUARD-MS CHECK-AS KIND
   s" measure: one-definition file " type MS. s" , outcome " type FMT:.INT cr
   s" tools/check-core.f" TREE-BYTES s" tools/check-core.f" TREE$
   mono-ns START-NS !
   [: drop ;] GUARD-MS >MS CHECK:VERIFY-BYTES KIND
   s" measure: tools/check-core.f " type MS. s" , outcome " type FMT:.INT cr ;


: RUN-CASE ( ptr u8 n [ -- ] -- ) {: label:ptr labelu:n q :}
   mono-ns START-NS !
   q execute
   s" case " type label labelu type s"  (" type MS. s" )" type cr ;

public

: MAIN ( -- )
   T-RESET
   CAP OUT-RESERVE
   CAP ERR-RESERVE
   FIXTURES
   s" two-in-turn" [: TWO-IN-TURN ;] RUN-CASE
   s" no-run" [: NO-RUN ;] RUN-CASE
   s" buffer-not-disk" [: BUFFER-NOT-DISK ;] RUN-CASE
   s" requires-back" [: REQUIRES-BACK ;] RUN-CASE
   s" absent-path" [: ABSENT-PATH ;] RUN-CASE
   s" failing-dependency" [: FAILING-DEPENDENCY ;] RUN-CASE
   s" engine-provided" [: ENGINE-PROVIDED ;] RUN-CASE
   s" held" [: HELD ;] RUN-CASE
   s" open-stop" [: OPEN-STOP ;] RUN-CASE
   s" disc-stop" [: DISC-STOP ;] RUN-CASE
   s" closure-line" [: CLOSURE-LINE ;] RUN-CASE
   s" no-result" [: NO-RESULT ;] RUN-CASE
   s" deadline" [: DEADLINE ;] RUN-CASE
   s" stop-after-packet" [: STOP-AFTER-PACKET ;] RUN-CASE
   s" missing-dependency" [: MISSING-DEPENDENCY ;] RUN-CASE
   s" loader-form" [: LOADER-FORM ;] RUN-CASE
   s" commented-literal" [: COMMENTED-LITERAL ;] RUN-CASE
   s" body-uncalled" [: BODY-UNCALLED ;] RUN-CASE
   s" body-immediate" [: BODY-IMMEDIATE ;] RUN-CASE
   s" reserved-names" [: RESERVED-NAMES ;] RUN-CASE
   s" loader-use" [: LOADER-USE ;] RUN-CASE
   s" storage-binding" [: STORAGE-BINDING ;] RUN-CASE
   s" fault-after-packet" [: FAULT-AFTER-PACKET ;] RUN-CASE
   s" provided-meta" [: PROVIDED-META ;] RUN-CASE
   s" script-loader" [: SCRIPT-LOADER ;] RUN-CASE
   s" bad-escape" [: BAD-ESCAPE ;] RUN-CASE
   s" unreadable-dependency" [: UNREADABLE-DEPENDENCY ;] RUN-CASE
   s" long-resolved" [: LONG-RESOLVED ;] RUN-CASE
   s" wide-closure" [: WIDE-CLOSURE ;] RUN-CASE
   s" whole-output" [: WHOLE-OUTPUT ;] RUN-CASE
   s" definitions" [: DEFINITIONS ;] RUN-CASE
   s" definitions-refused" [: DEFINITIONS-REFUSED ;] RUN-CASE
   s" definitions-undefine" [: DEFINITIONS-UNDEFINE ;] RUN-CASE
   s" definitions-dependency" [: DEFINITIONS-DEPENDENCY ;] RUN-CASE
   s" files" [: FILES ;] RUN-CASE
   s" identity-subject" [: IDENTITY-SUBJECT ;] RUN-CASE
   s" identity-paths" [: IDENTITY-PATHS ;] RUN-CASE
   s" identity-swap" [: IDENTITY-SWAP ;] RUN-CASE
   s" file-line-keys" [: FILE-LINE-KEYS ;] RUN-CASE
   s" loads" [: LOADS ;] RUN-CASE
   s" loads-unread" [: LOADS-UNREAD ;] RUN-CASE
   s" loads-scan-fail" [: LOADS-SCAN-FAIL ;] RUN-CASE
   s" loads-supplied" [: LOADS-SUPPLIED ;] RUN-CASE
   s" loads-none" [: LOADS-NONE ;] RUN-CASE
   s" loads-lifetime" [: LOADS-LIFETIME ;] RUN-CASE
   s" uses" [: USES ;] RUN-CASE
   s" named-uses" [: NAMED-USES ;] RUN-CASE
   s" uses-visit" [: USES-VISIT ;] RUN-CASE
   s" uses-visibility" [: USES-VISIBILITY ;] RUN-CASE
   s" uses-visit-lifetime" [: USES-VISIT-LIFETIME ;] RUN-CASE
   s" uses-refused" [: USES-REFUSED ;] RUN-CASE
   s" uses-order" [: USES-ORDER ;] RUN-CASE
   s" cands-body" [: CANDS-BODY ;] RUN-CASE
   s" cands-top" [: CANDS-TOP ;] RUN-CASE
   s" cands-none" [: CANDS-NONE ;] RUN-CASE
   s" cands-targets" [: CANDS-TARGETS ;] RUN-CASE
   s" cands-locals" [: CANDS-LOCALS ;] RUN-CASE
   s" cands-refused" [: CANDS-REFUSED ;] RUN-CASE
   s" cands-order" [: CANDS-ORDER ;] RUN-CASE
   s" cands-scopes" [: CANDS-SCOPES ;] RUN-CASE
   s" cands-observer" [: CANDS-OBSERVER ;] RUN-CASE
   s" uses-recovery" [: USES-RECOVERY ;] RUN-CASE
   s" uses-deferred" [: USES-DEFERRED ;] RUN-CASE
   s" uses-duplicate" [: USES-DUPLICATE ;] RUN-CASE
   s" uses-growth" [: USES-GROWTH ;] RUN-CASE
   s" cli-file" [: CLI-FILE ;] RUN-CASE
   s" cli-stdin" [: CLI-STDIN ;] RUN-CASE
   s" cli-usage" [: CLI-USAGE ;] RUN-CASE
   s" cli-early-fails" [: CLI-EARLY-FAILS ;] RUN-CASE
   s" cli-large-stdin" [: CLI-LARGE-STDIN ;] RUN-CASE
   s" cli-path-capacity" [: CLI-PATH-CAPACITY ;] RUN-CASE
   s" cli-whole-output" [: CLI-WHOLE-OUTPUT ;] RUN-CASE
   s" cli-deadline" [: CLI-DEADLINE ;] RUN-CASE
   s" cli-deadline-prepass" [: CLI-DEADLINE-PREPASS ;] RUN-CASE
   s" load-order" [: LOAD-ORDER ;] RUN-CASE
   s" load-context" [: LOAD-CONTEXT ;] RUN-CASE
   s" load-package" [: LOAD-PACKAGE ;] RUN-CASE
   s" load-using-floor" [: LOAD-USING-FLOOR ;] RUN-CASE
   s" load-repeat" [: LOAD-REPEAT ;] RUN-CASE
   s" body-computed" [: BODY-COMPUTED ;] RUN-CASE
   s" duplicate-after-packet" [: DUPLICATE-AFTER-PACKET ;] RUN-CASE
   s" duplicate-in-dependency" [: DUPLICATE-IN-DEPENDENCY ;] RUN-CASE
   s" duplicate-made" [: DUPLICATE-MADE ;] RUN-CASE
   s" duplicate-then-stops" [: DUPLICATE-THEN-STOPS ;] RUN-CASE
   s" unclean-answer" [: UNCLEAN-ANSWER ;] RUN-CASE
   s" cli-duplicate" [: CLI-DUPLICATE ;] RUN-CASE
   s" cli-duplicate-goes-on" [: CLI-DUPLICATE-GOES-ON ;] RUN-CASE
   s" top-undefined" [: TOP-UNDEFINED ;] RUN-CASE
   s" top-ambiguous" [: TOP-AMBIGUOUS ;] RUN-CASE
   s" top-shadow" [: TOP-SHADOW ;] RUN-CASE
   s" top-tick" [: TOP-TICK ;] RUN-CASE
   s" top-retired" [: TOP-RETIRED ;] RUN-CASE
   s" top-retired-public" [: TOP-RETIRED-PUBLIC ;] RUN-CASE
   s" sealed" [: SEALED ;] RUN-CASE
   s" top-axiom" [: TOP-AXIOM ;] RUN-CASE
   s" top-cold-prim" [: TOP-COLD-PRIM ;] RUN-CASE
   s" engine-rows" [: ENGINE-ROWS ;] RUN-CASE
   s" top-retired-import" [: TOP-RETIRED-IMPORT ;] RUN-CASE
   s" top-order" [: TOP-ORDER ;] RUN-CASE
   s" top-number" [: TOP-NUMBER ;] RUN-CASE
   s" top-keyword" [: TOP-KEYWORD ;] RUN-CASE
   s" top-qualified" [: TOP-QUALIFIED ;] RUN-CASE
   s" top-deferred" [: TOP-DEFERRED ;] RUN-CASE
   s" top-nested-deferred" [: TOP-NESTED-DEFERRED ;] RUN-CASE
   s" top-parses-bound" [: TOP-PARSES-BOUND ;] RUN-CASE
   s" top-parses-through" [: TOP-PARSES-THROUGH ;] RUN-CASE
   s" top-parses-opaque" [: TOP-PARSES-OPAQUE ;] RUN-CASE
   s" top-parses-loader" [: TOP-PARSES-LOADER ;] RUN-CASE
   s" top-opaque-loader" [: TOP-OPAQUE-LOADER ;] RUN-CASE
   s" top-parses-row" [: TOP-PARSES-ROW ;] RUN-CASE
   s" names-row" [: NAMES-ROW ;] RUN-CASE
   s" top-parses-whitebox" [: TOP-PARSES-WHITEBOX ;] RUN-CASE
   s" top-renders" [: TOP-RENDERS ;] RUN-CASE
   s" type-deferred" [: TYPE-DEFERRED ;] RUN-CASE
   s" def-deferred" [: DEF-DEFERRED ;] RUN-CASE
   s" trusted-tick-order" [: TRUSTED-TICK-ORDER ;] RUN-CASE
   s" top-create" [: TOP-CREATE ;] RUN-CASE
   s" top-trust" [: TOP-TRUST ;] RUN-CASE
   s" top-trust-refused" [: TOP-TRUST-REFUSED ;] RUN-CASE
   s" top-data-word" [: TOP-DATA-WORD ;] RUN-CASE
   s" top-trusted" [: TOP-TRUSTED ;] RUN-CASE
   s" top-kernel" [: TOP-KERNEL ;] RUN-CASE
   s" top-package-tail" [: TOP-PACKAGE-TAIL ;] RUN-CASE
   s" top-defer-word" [: TOP-DEFER-WORD ;] RUN-CASE
   s" top-defer-comment" [: TOP-DEFER-COMMENT ;] RUN-CASE
   s" top-accepted" [: TOP-ACCEPTED ;] RUN-CASE
   MEASURE
   ROOT$ REMOVE-TREE
   T-REPORT
   s" check-verify-test: ok" type cr ;

;package
