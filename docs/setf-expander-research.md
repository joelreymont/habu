# Common Lisp SETF Expander Mechanism Research

## Overview

The CL setf expander mechanism allows extending setf to handle complex places beyond built-in types.

## define-setf-expander

**Purpose**: Define how setf should expand a custom place form.

**Syntax**:
```lisp
(define-setf-expander access-fn lambda-list
  [[declaration* | documentation]]
  form*)
```

**Key Features**:
1. **Lambda-list**: Supports full destructuring (not just simple args)
2. **Environment**: Defined in lexical environment where form appears
3. **Implicit block**: Body wrapped in block named `access-fn`
4. **Compile-time availability**: Must be evaluable at compile time if used later in file

**Return Values** (from expander body, same as get-setf-expansion):
The expander must return 5 values:
1. **vars**: List of temporary variable symbols
2. **vals**: List of forms to compute values for temps
3. **store-vars**: List of variables to hold new values (usually one)
4. **writer-form**: Form that performs the update
5. **reader-form**: Form that reads current value

## get-setf-expansion

**Purpose**: Retrieve the 5-value expansion for a place.

**Syntax**:
```lisp
(get-setf-expansion place &optional environment)
```

**Returns**: 5 values as above

## Example Pattern

```lisp
;; Define custom place
(define-setf-expander my-place (obj slot)
  (let ((obj-temp (gensym "OBJ"))
        (slot-temp (gensym "SLOT"))
        (store-temp (gensym "STORE")))
    (values
     ;; vars
     (list obj-temp slot-temp)
     ;; vals  
     (list obj slot)
     ;; store-vars
     (list store-temp)
     ;; writer-form
     `(set-my-place ,obj-temp ,slot-temp ,store-temp)
     ;; reader-form
     `(get-my-place ,obj-temp ,slot-temp))))
```

## Difference from defsetf

- **define-setf-expander**: Full control, lambda-list is destructured against place form
- **defsetf**: Simpler, lambda-list is for values of subforms

## Implementation Plan for Habu

1. Need registry: `*setf-expanders*` hash table mapping symbols → expander functions
2. `get-setf-expansion`: Look up expander, call it, or use built-in expansion
3. `define-setf-expander`: Store expander in registry
4. Refactor current setf macro to use get-setf-expansion

## Sources

- [CLHS define-setf-expander](http://clhs.lisp.se/Body/m_defi_3.htm)
- [CLHS get-setf-expansion](http://clhs.lisp.se/Body/f_get_se.htm)
- [Tutorial: define-setf-expander for beginners](https://blog.cneufeld.ca/2014/01/the-less-familiar-parts-of-lisp-for-beginners-define-setf-expander/)
