\ judge/chain.f - compiling one canonical corpus definition through the native
\ chain, and recording what the chain did with it. One concern: the chain's
\ answer about one definition - the routine it published, or the refusal it
\ made.
\
\ WHAT IS DIFFERENT FROM THE COLUMN THIS REPLACES. The migrated column of the
\ old comparison held a hand-retyped copy of every subject and, beside it, a
\ hand-written list naming the two subjects the chain cannot compile. Both are
\ statements somebody typed. Here the text comes from the corpus file
\ (tools/judge/src.f) and the refusal comes from the compiler: a definition is
\ handed to the migration entry, and what this file records is either that it
\ published or the exact code it was refused with. A row that stops being
\ refused becomes a compiled row on the next run, with no list to take a name
\ out of, and a row that starts being refused becomes a refused row with its own
\ code rather than a silently shorter column.
\
\ ONE REGISTER BUDGET, AND WHY IT IS THE ARCHITECTURE'S. A migration states how
\ many scratch general registers its routine may use. The old column chose eight
\ for straight-line subjects and eighteen for subjects with loops in them, which
\ made every row's number partly a fact about that choice: a refusal under a
\ budget somebody picked is a fact about the budget. This file states EIGHTEEN
\ for every routine, which is the whole of what the machine has - the pool starts
\ at x0 and src/compiler/a64-effect.f refuses x18 in any routine's register set,
\ because the platform reserves it - so a refusal here is a fact about the
\ compiler and a size here is what the compiler does with everything it has.
\ Measured: every subject of tools/codegen-compare-corpus4.f compiles to the
\ same bytes at eighteen as the old column's per-row choice produced, and the
\ two it refuses are refused at every budget from one to eighteen.
\
\ NOTHING IS STAGED ABOUT A CALL. The migration entry has a way to hand it a
\ callee's spelling, entry address and arity, and this file does not use it:
\ src/compiler/native/hir-word.f RESOLVE-CALLABLE builds that row from the
\ engine's own answers when the elaborator meets a name it has no opinion about,
\ resolving the entry in the order the engine resolves the body that wrote the
\ name and taking the arity from the effect the CHECKER accepted. A caller that
\ stated those facts would be a second authority for them, going stale the
\ instant a callee is retired, and would also cap a body at the four callees the
\ staging list holds. So a subject that calls is migrated exactly like one that
\ does not, and the callees are published first because a call is compiled
\ against a callee that already exists - which is the same order the corpus file
\ itself is written in, since the engine compiled it in that order too.
\
\ WHERE THE DERIVED WORDS LAND. Wherever the caller's current wordlist points
\ when PUBLISH runs, which for a tool run at the top level is the global one.
\ That is what lets a bare derived name be resolved afterwards, both by the
\ chain compiling the next subject and by the readers that take this one's size
\ off its dictionary record. A derived name that already resolves is refused
\ rather than published over: the reader that then asked for its size would be
\ answered about whichever of the two the dictionary reached first, and a
\ measurement must not have that question.

require lib/errors.f
require lib/prelude.f
require lib/string.f
require src/compiler/native/dict.f
require src/compiler/native/migrate.f
require tools/codegen-tail-probe.f
require tools/judge/src.f

package JUDGE-CHAIN

private

96 constant NAME-MAX              \ bytes of one derived word name, qualifier and all
48 constant QUAL-MAX              \ bytes of the package qualifier a corpus is opened under
16 constant SUFFIX-MAX            \ bytes of the suffix derived names carry

create NAME NAME-MAX allot
variable NAME-U

create SUFFIX SUFFIX-MAX allot
variable SUFFIX-U

variable STAGED                   \ the definition PUBLISH is about to compile

: NAME! ( ptr u8 n -- ) {: a:ptr u:n :}
   u NAME-MAX > if E-JUDGE-CHAIN-CAP throw then
   a NAME u STR-LEN BYTE-COPY-LEN
   u NAME-U ! ;

: NAME+ ( ptr u8 n -- ) {: a:ptr u:n :}
   NAME-U @ u + NAME-MAX > if E-JUDGE-CHAIN-CAP throw then
   a  NAME NAME-U @ +  u STR-LEN BYTE-COPY-LEN
   NAME-U @ u + NAME-U ! ;

public

\ x0..x17: the whole general pool, x18 being platform-reserved. Stated once,
\ here, because it is a fact about the machine and not a choice about a row.
18 constant REGS

\ The suffix every derived word carries. It is settable because the judge
\ measures one corpus at a time and their derived words share one dictionary: a
\ suffix per corpus is what keeps two corpora that spell a subject the same way
\ from publishing over each other. There is no default that hides the choice.
: SUFFIX! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= if E-JUDGE-CHAIN-SUFFIX throw then
   u SUFFIX-MAX > if E-JUDGE-CHAIN-CAP throw then
   a SUFFIX u STR-LEN BYTE-COPY-LEN
   u SUFFIX-U ! ;

: SUFFIX$ ( -- ptr u8 n )
   SUFFIX-U @ 0= if E-JUDGE-CHAIN-SUFFIX throw then
   SUFFIX SUFFIX-U @ ;

private

\ What definition k's derived word is called, unqualified: the spelling the
\ migration publishes it under, in whatever wordlist is current then.
: WORD$ ( n -- ptr u8 n ) {: k:n :}
   k JUDGE-SRC:NAME$ NAME!
   SUFFIX$ NAME+
   NAME NAME-U @ ;

\ Does a name denote a routine a call may branch to? The chain's own question,
\ asked through the chain's own resolver, so what counts as published here is
\ what counts as published to the compiler.
: PUBLISHED? ( ptr u8 n -- bool )
   NDICT:CALL-TARGET 0<> ;

private

\ ---- which migration entry a body needs --------------------------------------
\ A body that names one of the corpus file's own storage words is a different
\ migration from one that does not: the address that word pushes is the
\ ENGINE's to answer, so the chain is handed the spelling and asks
\ src/compiler/native/dict.f for it while it declares the row. Measured: corpus
\ 1's CELL-BUMP, which WRITES its cell, is refused with E-A64RAV-DKEEP under
\ the plain entry and compiles under this one; corpus 2's TV-NEXT?, which only
\ READS its table, compiles under either. The distinction is not one this file
\ tries to draw - a body that names storage gets the entry that can express
\ storage, whichever way it uses it.
\
\ AND A BODY THAT NAMES TWO IS REFUSED HERE. The entry takes ONE spelling.
\ Handing it the first of two would compile a body whose other storage word
\ resolved to whatever the scope happened to hold, which is a measurement of a
\ program nobody wrote. There is no shape for it, so it is a named refusal.

: MIGRATE ( -- )
   STAGED @ {: k:n :}
   k JUDGE-SRC:USES {: uses:n :}
   uses 1 > if E-JUDGE-CHAIN-DATA throw then
   k SUFFIX$ JUDGE-SRC:TEXT$ {: sa:ptr su:n :}
   uses 0= if
      sa su k JUDGE-SRC:IN k JUDGE-SRC:OUT REGS NMIGRATE:DEFINE exit
   then
   sa su
   k 0 JUDGE-SRC:USE@ JUDGE-SRC:DATA-NAME$
   k JUDGE-SRC:IN
   k JUDGE-SRC:OUT
   REGS NMIGRATE:DEFINE-DATA ;

public

\ Compile definition k of the loaded corpus source through the native chain and
\ publish the result as its derived word. Answers 0 when the chain compiled it,
\ and otherwise the code the chain refused it with - which is a measurement of
\ the compiler, taken here, and not a name on a list.
\
\ A definition already published is left alone and answered 0: the callees of
\ two subjects overlap, and publishing one twice would be refused by the name
\ check below for a reason that is about this file rather than about the
\ compiler.
: PUBLISH ( n -- n ) {: k:n :}
   k WORD$ PUBLISHED? if 0 exit then
   k STAGED !
   [: MIGRATE ;] catch ;

private

\ ---- publishing what a subject calls, first ----------------------------------
\ A call is compiled against a callee that already exists, so the definitions a
\ subject reaches have to be published before it is. They are found by
\ tools/judge/src.f, which read them off the body, and they all sit EARLIER in
\ the corpus file than the subject does - the engine compiled that file top to
\ bottom, so a corpus cannot name a definition it has not reached yet. One
\ descending sweep therefore marks the whole transitive set, and a callee at or
\ after its caller is refused rather than published in an order that cannot
\ work.

64 constant MARK-MAX
create MARK MARK-MAX cells allot

: MARK-CLEAR ( -- )
   MARK-MAX 0 ?do 0 MARK i cells + ! loop ;

: MARKED? ( n -- bool ) {: k:n :}
   MARK k cells + @ 0<> ;

: MARK! ( n -- ) {: k:n :}
   1 MARK k cells + ! ;

\ Mark every definition k reaches, k included.
: MARK-DEPS ( n -- ) {: k:n :}
   JUDGE-SRC:DEFS MARK-MAX > if E-JUDGE-CHAIN-CAP throw then
   MARK-CLEAR
   k MARK!
   k 1+ 0 ?do
      k i - {: at:n :}
      at MARKED? if
         at JUDGE-SRC:CALLS 0 ?do
            at i JUDGE-SRC:CALL@ {: c:n :}
            c at >= if E-JUDGE-CHAIN-ORDER throw then
            c MARK!
         loop
      then
   loop ;

public

\ Publish everything definition k calls, and then k itself, answering the code
\ the chain refused K with - or 0. A DEPENDENCY the chain refuses is not
\ answered as k's refusal: it is a different fact, and reporting it as k's would
\ record the allocator declining a callee as the subject being declined. It is
\ thrown instead, so the run says which.
: PUBLISH-CALLING ( n -- n ) {: k:n :}
   k MARK-DEPS
   k 0 ?do
      i MARKED? if
         i PUBLISH 0<> if E-JUDGE-CHAIN-DEP throw then
      then
   loop
   k PUBLISH ;

\ ---- reading a derived word's record -----------------------------------------
\ A derived word is published where its corpus's own package points, so that a
\ body naming that corpus's private storage compiles at all. The reader that
\ takes a size off a dictionary record does NOT walk the open package's
\ wordlists - it resolves a spelling as written - so a size is asked for under
\ the qualified name. The qualifier is the corpus's package, stated once by the
\ caller that opened it, and there is no default: a wrong or absent qualifier
\ would answer about some other word or about none, and both are worse than
\ refusing.

create QUAL QUAL-MAX allot
variable QUAL-U

public

: QUALIFIER! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= if E-JUDGE-CHAIN-QUALIFIER throw then
   u QUAL-MAX > if E-JUDGE-CHAIN-CAP throw then
   a QUAL u STR-LEN BYTE-COPY-LEN
   u QUAL-U ! ;

\ Definition k's derived word under the qualifier its corpus was opened with.
: QUALIFIED$ ( n -- ptr u8 n ) {: k:n :}
   QUAL-U @ 0= if E-JUDGE-CHAIN-QUALIFIER throw then
   QUAL QUAL-U @ NAME!
   k JUDGE-SRC:NAME$ NAME+
   SUFFIX$ NAME+
   NAME NAME-U @ ;

\ How many bytes of machine code the derived word is, read off the word's own
\ dictionary record by the one reader the comparison already uses for the
\ engine's words, so the two columns' byte counts are the same kind of number.
: SIZE ( n -- n ) {: k:n :}
   k QUALIFIED$ NTAILPROBE:CODE-BYTES ;

;package
