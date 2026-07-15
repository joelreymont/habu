\ structures-effects.f - retired pre-hook structure effect rows.
\ No boot path loads this file; hard deletion owns its final removal.

s" STRUCT-BYTE+" s" ptr a n -- ptr u8" TRUST
s" BEGIN-STRUCTURE" s" -- ptr a n" TRUST
s" +FIELD" s" ptr a n n -- ptr a n" TRUST
s" PTR-FIELD:" s" ptr a n -- ptr a n" TRUST
s" CFIELD:" s" ptr a n -- ptr a n" TRUST
s" END-STRUCTURE" s" ptr a n --" TRUST
