\ heap.f — the HEAP definer.
\
\ `HEAP NAME t` declares two words for records of `t` that each live on their
\ own: NAME-NEW ( -- ptr t ) answers a fresh zero-filled record and
\ NAME-RELEASE ( ptr t -- ) unmaps it; a pointer kept across RELEASE is stale, and
\ releasing it again can unmap whatever was mapped there since. Nothing registers a
\ record: release it before an image save.
\ Each record is its own OS mapping, so the OS rounds it up to a page. The width
\ is the declared type's, fixed per declaration.
\ Checked code reaches DYNAMIC-STORAGE:RECORD-NEW and RECORD-RELEASE only from
\ these generated words (checker.f ARENA-CALL-REFUSE?).

package HEAP-DECL

private

\ Appends `: NAME<suffix>` and answers the span of the word's name.
: HEAD, ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: name:ptr nameu:n sfx:ptr sfxu:n :}
   s" : " LBUF-APP
   LBUF-GEN-U @ {: start:n :}
   name nameu LBUF-APP  sfx sfxu LBUF-APP
   LBUF-GEN start +  LBUF-GEN-U @ start - ;


\ NEW's and RELEASE's text, built before either is evaluated so a refusal
\ while building defines nothing, then the spans of their names.
: SOURCE ( ptr u8 n ptr u8 n n -- ptr u8 n ptr u8 n ptr u8 n ptr u8 n )
   {: name:ptr nameu:n type:ptr typeu:n width:n :}
   LBUF-CLEAR
   name nameu s" -NEW" HEAD, {: pna:ptr pnu:n :}
   s"  ( -- ptr " LBUF-APP type typeu LBUF-APP s"  ) " LBUF-APP
   width LBUF-DEC,  s"  DYNAMIC-STORAGE:RECORD-NEW ; " LBUF-APP
   LBUF-GEN-U @ {: relat:n :}
   name nameu s" -RELEASE" HEAD, {: rna:ptr rnu:n :}
   s"  ( ptr " LBUF-APP type typeu LBUF-APP s"  -- ) " LBUF-APP
   width LBUF-DEC,  s"  DYNAMIC-STORAGE:RECORD-RELEASE ;" LBUF-APP
   LBUF-GEN relat
   LBUF-GEN relat +  LBUF-GEN-U @ relat -
   pna pnu  rna rnu ;

public

: RUN ( -- )
   STORAGE-LINE-TOKEN {: name:ptr nameu:n :}
   nameu 0= if s" HEAP" CHECKER-STORAGE-NAME-REFUSE exit then
   STORAGE-PARSE-TYPE {: type:ptr typeu:n :}
   name nameu CHECKER-LBUF-NAME-OK? 0= if exit then
   name nameu s" -NEW" LBUF-SUFFIX-OK? 0= if exit then
   name nameu s" -RELEASE" LBUF-SUFFIX-OK? 0= if exit then
   type typeu CHECKER-DYNAMIC-INFO 0= if
      drop name nameu type typeu CHECKER-STORAGE-TYPE-REFUSE exit
   then {: width:n :}
   name nameu type typeu width SOURCE
   {: new:ptr newu:n rel:ptr relu:n pna:ptr pnu:n rna:ptr rnu:n :}
   new newu pna pnu LBUF-EVAL! drop
   rel relu rna rnu LBUF-EVAL! drop ;

;package

: HEAP ( -- ) HEAP-DECL:RUN ;

PRIM: HEAP PRIM;
