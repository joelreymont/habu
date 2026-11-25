\ text-main.f - requires m.f for text-b.f's call; text-a.f's text requires it
\ too.
require sib.f
require m.f
include text-a.f
include text-b.f
