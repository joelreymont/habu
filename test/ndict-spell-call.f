\ ndict-spell-call.f - one resolution answers what three used to.
\
\ NDICT:SPELL-CALL runs EFFECT-QUERY once and reads the latched rows three ways.
\ NDICT:SPELL-ARITY, NDICT:SPELL-GLUE and NDICT:SPELL-RET-NEUTRAL? each run it
\ themselves. The combined reader is therefore checked the only way that means
\ anything: against those three, over a corpus of words with different shapes,
\ rather than against a table of expected numbers this file would be free to
\ agree with by writing them down twice.
\
\ THE CORPUS IS GLOBAL AND ITS ARITIES ARE PINNED. A package-private word is not
\ reachable by a qualified spelling - the qualified leg resolves through the
\ package's PUBLIC wordlist - so a corpus of private words would answer
\ ARITY-NONE for every entry and the agreement case would pass without ever
\ comparing a populated row. The shapes case below pins the arity of each entry
\ for exactly that reason: it fails if the corpus ever stops denoting words.
\
\ THE CONTRACT WHEN THERE IS NOTHING TO SAY is asserted separately and exactly.
\ A name the checker holds no sizeable effect for answers ARITY-NONE twice,
\ GLUE-NONE and false - one refusal a caller tests once, in the first value.
\ The separate readers do not agree there and are not meant to: SPELL-GLUE keeps
\ computing and answers GLUE-UNKNOWN for an unsizeable row, which a caller that
\ already saw ARITY-NONE never looks at. Pinning the sentinel row here is what
\ keeps that difference deliberate instead of latent.

require lib/test.f

: SPCALL-PLAIN ( n -- n )
   1 + ;

: SPCALL-TWO-IN ( n n -- n )
   + ;

: SPCALL-NONE-IN ( -- n )
   7 ;

: SPCALL-WIDE-OUT ( n -- ptr u8 n )
   drop s" abc" ;

: SPCALL-WIDE-IN ( ptr u8 n -- n )
   nip ;

package SPCALL-TEST

private

\ Every value of the row must come from the same query as the three readers.
: AGREE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u NDICT:SPELL-ARITY {: in0:n out0:n :}
   a u NDICT:SPELL-GLUE nip {: glue0:n :}
   a u NDICT:SPELL-RET-NEUTRAL? {: neu0:bool :}
   a u NDICT:SPELL-CALL {: in1:n out1:n glue1:n neu1:bool :}
   in1 in0 T=
   out1 out0 T=
   in0 NDICT:ARITY-NONE = if exit then
   glue1 glue0 T=
   neu1 if neu0 TTRUE else neu0 TFALSE then ;

: CORPUS ( -- )
   s" SPCALL-PLAIN" AGREE
   s" SPCALL-TWO-IN" AGREE
   s" SPCALL-NONE-IN" AGREE
   s" SPCALL-WIDE-OUT" AGREE
   s" SPCALL-WIDE-IN" AGREE
   s" +" AGREE
   s" dup" AGREE
   s" drop" AGREE
   s" swap" AGREE
   s" @" AGREE ;

: CORPUS-CASE ( -- )
   s" one query answers what three queries answered" T-LABEL
   CORPUS ;

: IN-OF ( ptr u8 n -- n )
   NDICT:SPELL-CALL {: in:n out:n glue:n neu:bool :} in ;

: OUT-OF ( ptr u8 n -- n )
   NDICT:SPELL-CALL {: in:n out:n glue:n neu:bool :} out ;

: GLUE-OF ( ptr u8 n -- n )
   NDICT:SPELL-CALL {: in:n out:n glue:n neu:bool :} glue ;

\ The corpus really does denote words, and really does cover more than one
\ shape. Without this the agreement case can pass on rows that are all absent.
: SHAPES-CASE ( -- )
   s" the corpus denotes words and covers several shapes" T-LABEL
   s" SPCALL-PLAIN" IN-OF 1 T=
   s" SPCALL-PLAIN" OUT-OF 1 T=
   s" SPCALL-TWO-IN" IN-OF 2 T=
   s" SPCALL-TWO-IN" OUT-OF 1 T=
   s" SPCALL-NONE-IN" IN-OF 0 T=
   s" SPCALL-NONE-IN" OUT-OF 1 T=
   s" SPCALL-WIDE-OUT" IN-OF 1 T=
   s" SPCALL-WIDE-OUT" OUT-OF 2 T=
   s" SPCALL-WIDE-IN" IN-OF 2 T=
   s" SPCALL-WIDE-IN" OUT-OF 1 T= ;

\ GLUE IS ABOUT CELLS OF ONE VALUE, and none of the corpus has any: `ptr u8 n`
\ is a pointer and a count, two terms a caller may reorder freely, so every row
\ here answers GLUE-NONE. That is worth pinning rather than assuming - a row
\ that started carrying glue would mean the cell/term correspondence had moved
\ underneath the readers. A genuinely glued row needs a declared layout family;
\ the agreement case above is what covers glue for one if it ever joins the
\ corpus, because it compares this reader's glue against SPELL-GLUE's own.
: GLUE-CASE ( -- )
   s" every row in this corpus is cells of separate values" T-LABEL
   s" SPCALL-WIDE-OUT" GLUE-OF NDICT:GLUE-NONE T=
   s" SPCALL-WIDE-IN" GLUE-OF NDICT:GLUE-NONE T=
   s" SPCALL-PLAIN" GLUE-OF NDICT:GLUE-NONE T= ;

: ABSENT-CASE ( -- )
   s" a name the checker holds no effect for answers the sentinel row" T-LABEL
   s" SPCALL-NO-SUCH-WORD-ANYWHERE" NDICT:SPELL-CALL
   {: in:n out:n glue:n neu:bool :}
   in NDICT:ARITY-NONE T=
   out NDICT:ARITY-NONE T=
   glue NDICT:GLUE-NONE T=
   neu TFALSE ;

public

: RUN ( -- )
   T-RESET
   CORPUS-CASE  T-NEXT
   SHAPES-CASE  T-NEXT
   GLUE-CASE  T-NEXT
   ABSENT-CASE  T-NEXT
   T-REPORT ;

;package

SPCALL-TEST:RUN
