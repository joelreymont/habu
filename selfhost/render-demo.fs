\ render-demo.fs — check + render the inferred residual stack of several words, each
\ followed by '|'. Expected: "a a |a b |a b a |a b c |n |". Needs checker.fs + render.fs.
: SHOW {: a u :} a u CHECK drop RENDER 124 EMIT1 ;
: GO  s" dup" SHOW  s" swap" SHOW  s" over" SHOW  s" rot" SHOW  s" dup *" SHOW ;
GO
