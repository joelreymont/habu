# USE TYPES - MANDATORY

**Habu has a powerful ADT system. USE IT.**

## Core Principle

**Every dispatch on variant data MUST use typed ADTs with exhaustiveness checking.**

When you see code dispatching on tags/types/variants using `ecase`, `cond`, or `case`:
1. STOP
2. Define an ADT with `deftype`
3. Use `match` for exhaustiveness-checked dispatch

## Forbidden Patterns

```lisp
;; WRONG - runtime failure on missing case
(ecase (car ir)
  (:ir-lit ...)
  (:ir-add ...)
  ;; Forgot :ir-sub - discovers at runtime!
  )

;; WRONG - silent fallback
(case tag
  (0 'cons)
  (2 'symbol)
  (t 'unknown))  ; HIDES BUGS
```

## Required Patterns

```lisp
;; RIGHT - define the type
(deftype ir-node :prefix ir
  (lit value)
  (add left right)
  (sub left right)
  ...)

;; RIGHT - exhaustive match (compile-time error if case missing)
(match ir-node node
  (ir-lit (value) ...)
  (ir-add (left right) ...)
  (ir-sub (left right) ...))
```

## When Adding New Variants

1. Add variant to `deftype`
2. Compiler errors show ALL places that need updating
3. Fix each `match` - the type system guides you

## Benefits

- **Compile-time exhaustiveness**: Missing cases are errors, not runtime surprises
- **Self-documenting**: ADT definition shows all variants in one place
- **Refactoring safety**: Add/remove variants and compiler tells you what broke
- **No string comparisons**: Symbols, not strings

## Applies To

- IR nodes (ir-lit, ir-add, ir-if, ...)
- TAC instructions (tac-lit, tac-add, tac-call, ...)
- Type tags (cons, symbol, string, ...)
- Any enum-like dispatch

**If you're writing `ecase (car x)` or `case (car x)`, you're doing it wrong.**
