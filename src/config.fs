\ config.fs — caf constants: limits, term tags, type codes, THROW codes.
\ No magic numbers elsewhere; everything symbolic/tunable lives here.
\ Naming: TC-* type codes avoid gforth built-ins (CELL CHAR collide).

\ --- Limits (tuning defaults; not spec-derived). Exceeding any -> THROW. ---
4096 constant MAX-TV         \ type-variable slots
2048 constant MAX-RV         \ row-variable slots
 256 constant MAX-DEPTH      \ symbolic stack depth
262144 constant ARENA-SIZE    \ per-check heap, in cells

\ --- Term tags (low 3 bits of a type-term cell) ---
0 constant T-CON            \ concrete type; payload = type code
1 constant T-VAR            \ type variable; payload = type-var id
2 constant T-PTR            \ ptr<inner>; payload = arena index of inner type
3 constant T-QUOT           \ quot<effect>; payload = arena index of effect node

\ --- Stack-term tags (separate namespace, low 3 bits of a stack cell) ---
1 constant S-ROW            \ row variable; payload = row-var id
2 constant S-PUSH           \ push node; payload = arena index of [rest, top]
\ (nonzero so a stack-term cell is never 0 = the UNBOUND sentinel; row id 0
\  must still encode to a nonzero cell.)

\ --- Type codes (T-CON payloads). UNBOUND doubles as the "no binding"
\ sentinel: code 0 is never a valid concrete type. ---
0 constant UNBOUND
-1 constant RIGID-ROW       \ row var bound to RIGID = un-extendable (declared prefix)
1 constant TC-I64
2 constant TC-U8
3 constant TC-U32
4 constant TC-CELL
5 constant TC-BOOL
6 constant TC-CHAR
7 constant TC-STR
8 constant TC-ADDR
9 constant TC-F64           \ IEEE-754 double (one data-stack cell; FP ops use D-regs)

\ --- THROW codes (private range) ---
-2000 constant E-UNDERFLOW
-2001 constant E-MISMATCH
-2002 constant E-ARITY
-2003 constant E-UNKNOWN
-2004 constant E-UNSAFE
-2005 constant E-UNCHECKED
-2006 constant E-OCCURS
-2007 constant E-BADTYPE
-2008 constant E-DEPTH
-2009 constant E-TOOMANYVARS
-2010 constant E-ARENA
-2011 constant E-BRANCH
-2012 constant E-LOOP
-2013 constant E-QUOT
-2014 constant E-LOCAL
-2015 constant E-RECURSE
