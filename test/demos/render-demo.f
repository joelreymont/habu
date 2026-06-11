\ render-demo.fs — check + render the inferred residual stack of several words, each
\ followed by '|'. Expected: "a a |a b |a b a |a b c |n |". Needs checker.fs + render.fs.
: SHOW {: a u :} a u CHECK drop RENDER 124 EMIT1 ;   \ first token = word name
: GO  s" w dup" SHOW  s" w swap" SHOW  s" w over" SHOW  s" w rot" SHOW  s" w dup *" SHOW ;
GO
