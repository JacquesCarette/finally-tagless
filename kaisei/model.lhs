Here we define an AST for our modeling language, and then a translator
to a probabilistic model.  Next should be a translation to a 'sampler'
which actually draws sample from the model.

The language is based on BUGS, to a certain extent.  It is structured as
a sequence of declarations of either distributions (generators) and 
deterministic computations.  Both distributions and computations can
depend on parameter(s), so that we can easily define families of 
distributions.

The 'computations' are divided into 3 categories:
\begin{enumerate}
\item explicit distribution generators
\item dependent generators (i.e. which depend on other distributions)
\item combinators, namely iterators (over, partition) and application
\end{enumerate}

The embedding below is ``very deep''.  This is really bad terminology, since
this means that the language uses very very little of the surrounding (Haskell)
type system for safety.  A ``shallow'' embedding would be much more 
preferable, but at this point much harder to achieve.

\begin{code}
module Model where

import Data.Map as M

type Model a b = [ Decl a b ]
data Decl a b = Dist (Assign a b) | Let (Assign a b)
data Assign a b = A {name :: Name, value :: Comp a b}

type Name = (String, [ String ])
type VarName = String
type Class = VarName

data Distr = 
    Beta [Integer] 
  | Normal Float Float
  | Cat Name
  deriving Show
data DepDistr a b =
    NormalM (Comp a b) Float

data Comp a b = 
    D Distr

  | DepD (DepDistr a b)

  | Over (Comp a b) VarName Class
  | Partition a (Model a b)
  | Apply Name Name
\end{code}

We also have a language of PDF expressions.  They really correspond to the
``inner expression'' of a multiple integral (which integrates to $1$ since it
is a PDF).

\begin{code}
type PDF = [ Term ]
data Term =
    Di Name Distr
  | Dp Term VarName Class
  deriving Show
\end{code}

And now we can translate from one to the other:

\begin{code}
type Env = M.Map Name Distr
type EPDF = (Env, PDF)

trans :: Model a b -> PDF
trans m = snd $ foldr trans1 (env,[]) m
  where env = M.empty

trans1 :: Decl a b -> EPDF -> EPDF
trans1 (Dist d) (e,p) = tAssign d e p
trans1 (Let d) (e,p) = error "Let not implemented yet"

tAssign :: (Assign a b) -> Env -> PDF -> EPDF
tAssign (A n val) e p = (e, (tComp n val):p)

tComp :: Name -> (Comp a b) -> Term
tComp n (D d) = Di n d
tComp _ (DepD _) = error "Dependent product NIY"
tComp n (Over c s cl) = Dp (tComp n c) s cl
tComp _ (Partition _ _) = error "Partition NIY"
tComp _ (Apply _ _) = error "Apply NIY"
\end{code}
