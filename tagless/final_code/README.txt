The code accompanying the paper

	Finally Tagless, Partially Evaluated:
	Tagless Staged Interpreters for Simpler Typed Languages

Copyright (c) 2007, Jacques Carette, Chung-chieh Shan, Oleg Kiselyov


Overview of the files

Warm-up: simple tagless (and, for comparison, tag-full) interpreters
They are all in (Meta)OCaml.

tagless_interp1.ml	Tagfull and tagless (staged) interpreters
			for simply typed lambda-calculus with booleans,
			and deBruijn encoding of variables

tagless_interp2.ml	Tagless staged interpreter for simply typed lambda-
			calculus with deBruijn indices and booleans,
			integers, pairs, references, sequencing and let.
			
tagless_interp21.ml	The same as above but using higher-order 
			abstract syntax rather than deBruijn indices

Warm up: simple typed tagless partial evaluators

tagless_pe3.ml		For the language of tagless_interp2.ml
tagless_pe4.ml		The same with CPS, distinguishing pure from
			potentially effectful computations


The main development: code described in the paper

	In (Meta)Ocaml:

incope.ml	        Tagless typed interpreters, compilers, 
			partial evaluators, CBV and CBN CPS
			interpreters and transformers,
			continuation- and state-passing interpreter

	In Haskell:

Incope.hs		Tagless typed interpreters, (byte) compilers,
			partial evaluator, HOAS bytecode compiler,
			various self-interpreters.
			Byte-compilers use GADT to emulate safe
			staging.
			Tagless partial evaluator is given in two
			versions: with GADT and without
			

incope1.hs		Tagless interpreter, compiler, partial evaluator.
			The partial evaluator is implemented with
			typeclasses instead of GADTs.



Last updated: November 2007.

