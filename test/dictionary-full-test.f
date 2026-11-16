\ dictionary-full-test.f - a definition, a storage declaration, a does> clause,
\ a package, a qualified head's namespace, an EXPORT, a declaration and a
\ replayed definition that find no dictionary record left are rejected as
\ E-DICTIONARY-FULL, placed at the name they make, a does> clause at its
\ `does>`, a namespace at its qualified head, an EXPORT at its operand, a
\ declaration where it ran out of room and a replayed definition at its `:`,
\ and the load exits REJECTION:DICT-FULL-RC. A full dictionary stops the load,
\ so each case is its own child hb. Each child fills the dictionary to a set
\ number of free records counted from its own ndict@, so no case depends on how
\ many records the engine uses at boot.
require test/gate-common.f
require src/lib/engine-candidate.f
require src/core/rejection.f

package DICTIONARY-FULL-TEST
private

\ Lines 1-3 of each child: LEAVE-FREE ( n -- ) creates words until n records
\ are free.
: PRELUDE$ ( -- ^u8 n )
   S\" require src/lib/fmt.f\ncreate NB 32 allot\n: LEAVE-FREE ( n -- ) DICT-CAP ndict@ - swap - 0 ?do s\" create F\" NB swap BYTE-COPY  i FMT:INT$ {: a:^ u:n :}  a NB 8 + u BYTE-COPY  NB 8 u + evaluate-closed loop ;\n" ;

\ Runs the prelude, then the case's lines from line 4.
: RUN-CASE ( ^u8 n -- ) {: a:^ u:n :}
   GE-SRC-RESET  PRELUDE$ GE-SRC+  a u GE-SRC+
   GE-HB-RESET
   GE-HB$ GE-SRC-BUF GE-SRC-U @ PROCESS:WAIT-FOREVER MS>N GE-RUN-STDIN ;

: EXPECT ( ^u8 n ^u8 n ^u8 n -- )
   {: src:^ srcu:n want:^ wantu:n label:^ labelu:n :}
   src srcu RUN-CASE
   want wantu label labelu GE-EXPECT-ERR
   REJECTION:DICT-FULL-RC label labelu GE-EXPECT-RC
   s" PASS: " type label labelu type cr ;

\ MK and its does> clause take the last two records, so OVER1 finds none.
: DEFINITION ( -- )
   S\" 2 LEAVE-FREE\n: MK ( n -- ) create , does> ( -- n ) @ ;\n: OVER1 ;\n"
   S\" error[E-DICTIONARY-FULL]: the dictionary has no record left for `OVER1`\n --> <input>:6:3\n  |\n6 | : OVER1 ;\n  |   ^^^^^ no record left\n  = help: define fewer words in this load\n\n"
   s" a definition after a does> definer fills the dictionary" EXPECT ;

: DOES-CLAUSE ( -- )
   S\" 1 LEAVE-FREE\n: MK ( n -- ) create , does> ( -- n ) @ ;\n"
   S\" error[E-DICTIONARY-FULL]: the dictionary has no record left for `MK`\n --> <input>:5:24\n  |\n5 | : MK ( n -- ) create , does> ( -- n ) @ ;\n  |                        ^^^^^ no record left\n  = help: define fewer words in this load\n\n"
   s" a does> clause with no record left" EXPECT ;

: FULL-PACKAGE ( -- )
   S\" 0 LEAVE-FREE\npackage FOO\n"
   S\" error[E-DICTIONARY-FULL]: the dictionary has no record left for `FOO`\n --> <input>:5:9\n  |\n5 | package FOO\n  |         ^^^ no record left\n  = help: define fewer words in this load\n\n"
   s" a package in a full dictionary" EXPECT ;

: NAMESPACE ( -- )
   S\" 0 LEAVE-FREE\n: NEWNS:W ( -- ) ;\n"
   S\" error[E-DICTIONARY-FULL]: the dictionary has no record left for `NEWNS`\n --> <input>:5:3\n  |\n5 | : NEWNS:W ( -- ) ;\n  |   ^^^^^^^ no record left\n  = help: define fewer words in this load\n\n"
   s" a qualified head in a full dictionary" EXPECT ;

: EXPORTED ( -- )
   S\" package BAR public\n: W ( -- ) ;\n;package\npackage FOO public\n0 LEAVE-FREE\nEXPORT BAR:W\n"
   S\" error[E-DICTIONARY-FULL]: the dictionary has no record left for `W`\n --> <input>:9:8\n  |\n9 | EXPORT BAR:W\n  |        ^^^^^ no record left\n  = help: define fewer words in this load\n\n"
   s" an EXPORT in a full dictionary" EXPECT ;

\ A declaration makes all its words or none; the record names the first.
: STRUCTURE-DECL ( -- )
   S\" 2 LEAVE-FREE\nSTRUCTURE pt DERIVE addr FIELD x n FIELD y n ;STRUCTURE\n"
   S\" error[E-DICTIONARY-FULL]: the dictionary has no record left for `PT:MAKE`\n --> <input>:5:46\n  |\n5 | STRUCTURE pt DERIVE addr FIELD x n FIELD y n ;STRUCTURE\n  |                                              ^^^^^^^^^^ no record left\n  = help: define fewer words in this load\n\n"
   s" a structure that does not fit" EXPECT ;

: SUMTYPE-DECL ( -- )
   S\" 2 LEAVE-FREE\nSUMTYPE shape VARIANT circle n ;VARIANT VARIANT square n ;VARIANT ;SUMTYPE\n"
   S\" error[E-DICTIONARY-FULL]: the dictionary has no record left for `SHAPE:CIRCLE`\n --> <input>:5:67\n  |\n5 | SUMTYPE shape VARIANT circle n ;VARIANT VARIANT square n ;VARIANT ;SUMTYPE\n  |                                                                   ^^^^^^^^ no record left\n  = help: define fewer words in this load\n\n"
   s" a sumtype that does not fit" EXPECT ;

: STORAGE-DECL ( -- )
   S\" 0 LEAVE-FREE\nTYPED-VARIABLE FOO n\n"
   S\" error[E-DICTIONARY-FULL]: the dictionary has no record left for `FOO`\n --> <input>:5:16\n  |\n5 | TYPED-VARIABLE FOO n\n  |                ^^^ no record left\n  = help: define fewer words in this load\n\n"
   s" a storage declaration in a full dictionary" EXPECT ;

\ RPL replays RA and RB while OUTER is parked: RA takes OUTER's slot, the one
\ free record, so RB finds none.
: REPLAY-PARKED ( -- )
   S\" require src/habu/verify-source.f\n: RPL ( -- ) [: s\" : RA ( -- ) ; : RB ( -- ) ;\" VERIFY:SOURCE-BUF-IN-SCOPE ;] CHECK:SCOPED ; immediate\ns\" RPL\" 0 parse-imm\n1 LEAVE-FREE\n: OUTER ( -- ) RPL ;\n"
   S\" error[E-DICTIONARY-FULL]: the dictionary has no record left for `rb`\n --> <input>:1:15\n  |\n1 | : RA ( -- ) ; : RB ( -- ) ;\n  |               ^ no record left\n  = word: rb\n  = help: define fewer words in this load\n\n"
   s" a replay that takes a parked definition's record" EXPECT ;

DEFINITION
DOES-CLAUSE
FULL-PACKAGE
NAMESPACE
EXPORTED
STRUCTURE-DECL
SUMTYPE-DECL
STORAGE-DECL
REPLAY-PARKED

;package
