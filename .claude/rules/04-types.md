# Habu Type System

## Tagged Value Representation

```
value
├── tagged-value (any tagged Habu value)
│   ├── tagged-fixnum (bit0=1, actual = val >> 1)
│   └── tagged-ptr (bit0=0, tag in bits 1-3)
│       ├── nil-ptr (value 0)
│       ├── cons-ptr (tag 0)
│       ├── symbol-ptr (tag 2)
│       ├── vector-ptr (tag 4)
│       ├── string-ptr (tag 6)
│       ├── closure-ptr (tag 8)
│       └── keyword-ptr (tag 10)
├── untagged-int (raw machine integer)
└── untagged-ptr (raw machine pointer)
```

## ADT Pattern

Define types with `deftype`, dispatch with exhaustive `match`:

```lisp
(deftype ir-node :prefix ir
  (lit value)
  (add left right)
  (sub left right))

(match ir-node node
  (ir-lit (value) ...)
  (ir-add (left right) ...)
  (ir-sub (left right) ...))
```

Adding a variant to `deftype` → compiler errors show all `match` sites needing update.

## Memory Layouts

```lisp
(deftype vector-header :record
  (length untagged-int))    ; length is UNTAGGED at offset 0

(deftype string-layout :record
  (length tagged-fixnum)
  (data (array byte)))
```
