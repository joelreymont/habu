\ path.f - the engine's C paths: one capacity, one copier, one shared scratch.
\
\ PATH-CAP is the tree's one path capacity, in bytes without the NUL: a path one
\ layer accepts every layer accepts. PATHZ copies (a,u) into the room bytes at d
\ and appends the NUL, so src/core and src/habu write no terminator of their
\ own. A path holding a 0 byte, which the OS would read short, is E-PATH-NUL.
\ Every guard throws, never dies: every file reader opens through here and a
\ caller must get a catchable refusal. The negative check is the memory guard:
\ for u < 0 the copy loop never runs and `0 d u + c!` writes before the buffer.
\ The checker models PATHZ and PATH0 with a stack effect only, so it does not
\ see these throws.
7134 constant E-PATH-RANGE   \ path length negative, or no room for it and its NUL
7165 constant E-PATH-NUL     \ a path holds a 0 byte, which would end it early
: PATHZ ( ptr u8 n ptr u8 n -- )
   {: a:ptr u d:ptr room :}
   u 0 < IF E-PATH-RANGE throw THEN
   u room >= IF E-PATH-RANGE throw THEN
   0 BEGIN dup u < WHILE
      dup a + c@  dup 0= IF E-PATH-NUL throw THEN  over d + c!  1 +
   REPEAT drop  0 d u + c! ;
create PZB PATH-CAP 1 + allot

: PATH0 ( ptr u8 n -- ptr u8 )
   {: a:ptr u :}
   a u PZB PATH-CAP 1 + PATHZ  PZB ;     \ shared scratch
