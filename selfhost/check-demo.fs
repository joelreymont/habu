: HOOK CHECK dup . ;
' HOOK set-check
: SQ dup * ;
: BAD dup 0= + ;
: BR dup 0 < IF negate THEN ;
7 SQ .
