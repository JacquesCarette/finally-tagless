Domains: the leaf module. Does not depend on monads.
Domains_sig.ml -- signatures, parameterized over code repr
Domains_code.ml -- implementation for rep = code
Domains_direct.ml -- implementation for rep = thunk

Code: domain-independent code generation stuff
Uses monads, does not depend on domain.

coderep.mli -- signature
code.ml  -- implementation for MetaOCaml code
code_direct.ml -- implementation for thunk


