\documentclass[t,utf8]{beamer}
\setcounter{errorcontextlines}{100}
\usepackage[T1]{fontenc}
\usepackage{charter}
\usepackage{eulervm}
\usepackage{comment}

\usefonttheme{serif}
\makeatletter
\setbeamertemplate{frametitle}
{\vskip-2pt
  \@@tempdima=\textwidth%
  \advance\@@tempdima by\beamer@@leftmargin%
  \advance\@@tempdima by\beamer@@rightmargin%
  \begin{beamercolorbox}[sep=0.3cm,left,wd=\the\@@tempdima]{frametitle}
    {\usebeamerfont{frametitle}\smash[b]{\insertframetitle}\par}%
    {%
      \ifx\insertframesubtitle\@@empty%
      \else%
      {\usebeamerfont{framesubtitle}\usebeamercolor[fg]{framesubtitle}\smash[b]{\insertframesubtitle\par}}%
      \fi
    }%
  \end{beamercolorbox}%
}
\setbeamertemplate{block begin}
{
  \par\vskip\medskipamount%
  \ifx\insertblocktitle\@@empty\else
    \begin{beamercolorbox}[colsep*=.75ex]{block title}
      \usebeamerfont*{block title}\smash[b]{\insertblocktitle}%
    \end{beamercolorbox}%
    {\parskip0pt\par}%
    \ifbeamercolorempty[bg]{block title}
      {}
      {\ifbeamercolorempty[bg]{block body}{}{\nointerlineskip\vskip-0.5pt}}%
  \fi
  \usebeamerfont{block body}%
  \begin{beamercolorbox}[colsep*=.75ex,vmode]{block body}%
    \ifbeamercolorempty[bg]{block body}{\vskip-.25ex}{\vskip-.75ex}\vbox{}%
}
\makeatother
\setbeamertemplate{sidebar right}{\vfill\llap{\usebeamerfont{framesubtitle}\insertframenumber\hskip\jot}\vskip\jot}
\usecolortheme{crane}
\setbeamercolor{alerted text}{fg=red!80!craneorange!80!fg}
%\setbeamercolor{highlight}{bg=yellow!80!craneorange!80!bg}
\newcommand<>{\highlight}[1]{\text{\fboxsep=0pt \only#2{\colorbox{yellow}}{\strut$#1$}}}
\newcommand<>{\Overbrace}[2]{\alt#3{\mathord{\overbrace{#2}^{\text{\small #1}}}}{\vphantom{\overbrace{#2}^{\text{\small #1}}}#2}}
\newcommand<>{\Underbrace}[2]{\alt#3{\mathord{\underbrace{#2}_{\text{\small #1}}}}{\vphantom{\underbrace{#2}_{\text{\small #1}}}#2}}

%include lhs2TeX.fmt
%include lhs2TeX.sty
\makeatletter
\renewcommand{\PT@@resetandcr}[1][0pt]%
  {\ifx\PT@@currentcol\PT@@lastcol
   \else
     \ifx\PT@@currentcol\PT@@nullcol
       \edef\PT@@currentcol{\Head{\Tail\PT@@sortedlist}}%
     \fi
     \edef\PT@@currentcol@@{\StripColumn\PT@@currentcol}%
     \edef\PT@@lastcol@@
       {\StripColumn\PT@@lastcol}%
     \PT@@typeout@@{adding implicit fromto from \PT@@currentcol@@
                                  \space to \PT@@lastcol@@}%
     \expandafter\expandafter\expandafter\fromto
       \expandafter\expandafter\expandafter{%
         \expandafter\expandafter\expandafter\PT@@currentcol@@
           \expandafter}\expandafter{\PT@@lastcol@@}{}%
   \fi
   \PT@@typeout@@{Next line ...}%
   \PT@@resetcolumn\@@newline[#1]} % CHANGED THIS LINE from polytable
\makeatother
%format forall a = "\Forall " a
%format . = "\circ "
%format r = "\Varid{r}"
%format f = "\Varid{f}"
%format e = "\Varid{e}"
%format es = "\Varid{es}"
%format m = "\Varid{m}"
%format t = "\Varid{t}"
%format tm = "\Varid{tm}"
%format sg = "\Varid{sg}"
%format r1
%format f1
%format f2
%format es1
%format es2
%format e0
%format e1
%format e2
%format e3
%format m0
%format m1
%format m2
%format m3
%format t1
%format t2
%format t3
%format tm1
%format tm2
%format sg0
%format sg1
%format sg11
%format sg12
%format sg2
%format sg3
%format ... = "\quad\ldots\quad"
%format =<< = "\rbind"
%format >>- = "\bindi"
%format >> = "\bindN"
%format ==== = "\simeq"
%format === = "\quad\simeq\quad"
%format ===/ = "\quad\not\simeq\quad"
%format * = "\times "
%format observe_sr = "\Varid{observe}_{\Varid{sr}}"
\def\Forall#1\circ{\forall#1.\:}

%format |> = "\llap{\structure{$\blacktriangleright$ }}"
%format failure = "\text{\textbf{failure}} "
%format diverge = "\text{\textbf{diverge}} "

%subst code a    	= "\unskip\begingroup\parskip\abovedisplayskip\csname @@par\endcsname\noindent\advance\leftskip\mathindent\('n\begin{pboxed}\SaveRestoreHook'n" a "\ColumnHook'n\end{pboxed}'n\)\parskip\belowdisplayskip\csname @@par\endcsname\noindent\endgroup\resethooks'n"

\renewcommand{\bind}{\mathbin{>\mkern-6mu>\mkern-2mu=}}
\newcommand{\rbind}{\mathbin{=\mkern-2mu<\mkern-6mu<}}
\newcommand{\bindi}{\mathbin{>\mkern-6mu>\mkern-6mu-}}
\newcommand{\bindN}{\mathbin{>\mkern-6mu>}}
\renewcommand{\plus}{\mathbin{+\mkern-4mu+}}

\title{Backtracking, interleaving, and terminating monad transformers}
\subtitle{Functional pearl}
\author{Oleg Kiselyov\inst{1} \and Chung-chieh Shan\inst{2} \and Daniel P. Friedman\inst{3} \and Amr Sabry\inst{3}}
\institute{\inst{1}FNMOC \inst{2}Harvard University \inst{2}Rutgers University \inst{3}Indiana University}
\date{ICFP 2005}
\keywords{continuations, control delimiters, Haskell, logic programming, Prolog, streams}

\begin{document}
\divide\abovedisplayskip 2
\divide\belowdisplayskip 2
\divide\abovedisplayshortskip 2
\divide\belowdisplayshortskip 2
\blanklineskip\abovedisplayskip

\begin{comment}
\begin{code}
module Talk where
import Monad hiding (guard)
\end{code}
\end{comment}

\begin{frame}
\titlepage
\end{frame}

\section{Introduction}

\begin{frame}
\frametitle{Search}

Combinators for computations that yield solutions:

\onslide<2->

\alert<3>{Conjoin} and \alert<4>{disjoin} searches (Wadler FPCA 1985).

\alert<5>{Failure} backtracks to the last choice point.

\begin{spec}
factor :: Int -> [(Int, Int)]
factor n = [(i, j) |  i  <- [2{-"\mathinner{\highlight<4>{"-}..{-"}}"-}n-1]{-"\highlight<3>{"-},{-"}"-}
                      j  <- [2{-"\mathinner{\highlight<4>{"-}..{-"}}"-}n-1]{-"\highlight<3>{"-},{-"}"-}
                      () <- guard(n == i * j)]

guard :: Bool -> [()]
guard True   = [()]
guard False  = {-"\highlight<5>{"-}[]{-"}"-}

|> factor 15
[(3,5),(5,3)]
\end{spec}

\onslide<6->
Applications: parsing, nondeterminism, transactions, pattern combinators, failure handling, \dots.

\end{frame}

\begin{frame}
\frametitle{Search with a monad}

\alert<2>{The |Monad| type-class} provides an interface for conjunction.

\alert<3>{The |MonadPlus| type-class} adds an interface for disjunction.

\begin{spec}
range :: MonadPlus m => Int -> Int -> m Int
range min max =  guard (min <= max) {-"\mathbin{\highlight<2>{"-}>>={-"}}"-} \() ->
                 {-"\highlight<3>{"-}mplus{-"}"-} ({-"\highlight<2>{"-}return{-"}"-} min) (range (min + 1) max)

factor :: MonadPlus m => Int -> m (Int, Int)
factor n =  range 2 (n - 1)     {-"\mathbin{\highlight<2>{"-}>>={-"}}"-} \i ->
            range 2 (n - 1)     {-"\mathbin{\highlight<2>{"-}>>={-"}}"-} \j ->
            guard (n == i * j)  {-"\mathbin{\highlight<2>{"-}>>={-"}}"-} \() -> {-"\highlight<2>{"-}return{-"}"-} (i,j)

guard :: MonadPlus m => Bool -> m ()
guard True   = {-"\highlight<2>{"-}return{-"}"-} ()
guard False  = {-"\highlight<3>{"-}mzero{-"}"-}
\end{spec}

\onslide<4->
The same interface can be implemented in multiple ways:
a~kind of structured denotational semantics.

\end{frame}

\begin{frame}
\frametitle{Search with a monad transformer}

A \emph{monad transformer}~|T| maps monads to monads, respecting the monad
operations.
%format * = "\star "
\begin{spec}
T        :: (* -> *) -> (* -> *)

lift     :: Monad m => m a -> T m a
observe  :: Monad m => T m a -> m a     -- get first solution
\end{spec}
%format * = "\times "
We can add backtracking to any monad, such as |IO|.

The examples below let |m| be the identity monad.
\end{frame}

\begin{frame}
\frametitle{Hinze (ICFP 2000) \only<2->{\alert<2>{\smash{versus this paper}}}}

``Deriving backtracking monad transformers''
\pause
\begin{description}[Transformers]
    \item[Deriving]
        \only<2->{Hughes's techniques (AFP 1995) work for basic backtracking,
            but creativity is required for |cut|.}
        \only<2->{\alert<2>{\\ We require creativity outright.}}
    \item[Backtracking]
        \only<2->{Hinze provides depth-first search only.}
        \only<2->{\alert<2>{\\ We add fair disjunction and conjunction.}}
    \item[Monad]
        \only<2->{Hinze implements |cut| by pattern-matching on contexts.}
        \only<2->{\alert<2>{\\ We separate |cut| into conditional and pruning
            (Naish).  Our implementation runs deterministic code at full speed
            given delimited control.}}
    \item[Transformers]
        \only<2->{Hinze's interface allows observing the first solution or (if
            finitely many) all solutions.}
        \only<2->{\alert<2>{\\ Our interface allows observing arbitrary initial
            solutions out of possibly infinitely many.}}
\end{description}
\end{frame}

\begin{frame}
\frametitle{Outline}
\tableofcontents

No logical variables or unification here (but see Kanren project)
\end{frame}

\section{Basic backtracking}

\begin{frame}
\frametitle{Basic backtracking}

\begin{spec}
composite n =  range 2 (n - 1) >>= \i ->
               guard (n `mod` i == 0)

|> observe (composite 5)
failure
|> observe (composite 15)
()
\end{spec}

\pause
\begin{block}{Stream implementation}
\begin{spec}
return a        =  [a]
mzero           =  []
mplus           =  (++)
observe         =  head
[]     >>= k    =  mzero
(a:m)  >>= k    =  mplus (k a) (m >>= k)
\end{spec}
\end{block}
\end{frame}

\section{A more expressive interface; a stream-based implementation}

\begin{frame}
\frametitle{Once (pruning)}

Because |composite 15| succeeds twice, the conjunction
\begin{spec}
|>  composite 15 >>= \() ->
    composite 15 >>= \() ->
    composite 15 >>= \() ->
    composite 5
\end{spec}
takes exponential time to fail.
\pause
For efficiency, redefine
\begin{spec}
composite n =  {-"\highlight{"-}once{-"}"-}(  range 2 (n - 1) >>= \i ->
                                              guard (n `mod` i == 0)  )
\end{spec}

\pause
\begin{block}{Stream implementation}
\begin{spec}
once []     = []
once (a:m)  = [a]
\end{spec}
\end{block}
But any effect for the tail~|m| must not take place.
\end{frame}

\begin{frame}
\frametitle{Soft-cut (conditional)}

\begin{overprint}
\onslide<1>
\begin{spec}
prime n =
\end{spec}
\onslide<2->
\begin{spec}
prime n = {-"\highlight{"-}ifte{-"}"-}  (composite n)
                                        (\() -> mzero)
                                        (return ())
\end{spec}
Commit to |mzero| as soon as the condition |composite n| succeeds.
\end{overprint}
\onslide<2->
\begin{spec}
|> observe (prime 5)
()
|> observe (prime 15)
failure
\end{spec}
\onslide<3->
\begin{block}{Stream implementation}
\begin{spec}
ifte []  thn els  = els
ifte m   thn els  = m >>= thn
\end{spec}
\end{block}
But any effect for the head of~|m| must take place only one time.
\end{frame}

\begin{frame}
\frametitle{An infinite number of solutions}

\begin{spec}
from n       =  mplus (return n) (from (n + 1))

composite n  =  from 2 >>= \i ->
                from 2 >>= \j -> guard (n == i * j)
\end{spec}
\pause
This new definition doesn't even work on composite numbers.
\begin{spec}
|> observe (composite 15)
diverge
\end{spec}
\end{frame}

\begin{frame}
\frametitle{An infinite number of solutions}

Need to interleave (dovetail) |i| and~|j| solutions (Seres and Spivey).  Redefine
\begin{spec}
composite n  =  from 2 {-"\mathbin{\highlight{"-}>>-{-"}}"-} \i ->
                from 2 {-"\mathbin{\highlight{"-}>>-{-"}}"-} \j -> guard (n == i * j)

|> observe (composite 15)
()
\end{spec}

\pause
\begin{block}{Stream implementation}
Fair disjunction
\begin{spec}
    interleave []     m'  =  m'
    interleave (a:m)  m'  =  a:(interleave m' m)
\end{spec}
Fair conjunction
\begin{spec}
    []     >>- k = mzero
    (a:m)  >>- k = interleave (k a) (m >>- k)
\end{spec}
\end{block}
\end{frame}

\begin{frame}
\frametitle{Observation}

\begin{spec}
observe  :: Monad m => T m a -> m a     -- get first solution
\end{spec}

\pause
We don't just want the first solution.
\begin{itemize}
    \item All solutions (Prolog |bagof|)
    \item First $n$ solutions
    \item Some desired prefix of solutions
\end{itemize}
We want to observe a backtracking computation like a stream.

\pause
\begin{block}{A subsuming deconstruction interface}
\begin{spec}
msplit :: Monad m => T m a -> T m (Maybe (a, T m a))
\end{spec}
\end{block}
\pause
\begin{block}{Stream implementation}
\begin{spec}
    msplit []     = [Nothing]
    msplit (a:m)  = [Just (a,m)]
\end{spec}
\end{block}
But any effect for each solution must take place just on demand.
\end{frame}

\section{More efficient implementations using continuations}

\begin{frame}
\frametitle{Implementations using delimited continuations}

\begin{block}{CPS implementation}
\begin{spec}
    type T m a = forall w. {-"\Overbrace<2->{\llap{\alert<2>{success continuation }}$c$}{"-}(a -> {-"\Underbrace<2->{\llap{\alert<2>{failure continuation }}$f$}{\vphantom{()}"-}m w{-"}"-} -> m w){-"}"-} -> {-"\Overbrace<3->{\alert<3>{answer type}}{\Underbrace<2->{$f$}{\vphantom{()}"-}m w{-"}"-} -> m w{-"}"-}
\end{spec}
\onslide<4->
Deterministic code uses just the success continuation~$c$ and so runs as fast
as ordinary code using delimited control.
\end{block}

\onslide<5->
Several perspectives:
\begin{itemize}
    \item Hinze's context-passing implementation, without |cut|
    \item The Church encoding of streams (Böhm and Berarducci); the
        continuation monad transformer applied to streams (Danvy, Grobauer, and
        Rhiger); related to streams by logical relations (Wand and Vaillancourt)
    \item Cooperative multithreading among disjuncts
\end{itemize}
\end{frame}

\begin{frame}
\frametitle{CPS implementation: Basic backtracking}

|return| and |>>=| are exactly as in the continuation monad.

\begin{block}{}
\begin{spec}
return :: a -> T m a
return a     =  \c -> c a

(>>=) :: T m a -> (a -> T m b) -> T m b
m >>= k      =  \c -> m (\a -> k a c)

mzero :: T m a
mzero        =  \c f -> f

mplus :: T m a -> T m a -> T m a
mplus m1 m2  =  \c f -> m1 c (m2 c f)

lift :: Monad m => m a -> T m a
lift m       =  \c f -> m >>= \a -> c a f
\end{spec}
\end{block}
\end{frame}

\begin{frame}
\frametitle{CPS implementation: |msplit|}

From the Church encoding: |reflect| is like $\Varid{in}$; |msplit| is like
$\Varid{out}$.
\begin{block}{}
\begin{spec}
reflect :: Maybe (a, T m a) -> T m a
reflect Nothing        =  mzero
reflect (Just (a, m))  =  mplus (return a) m

msplit :: Monad m => T m a -> T m (Maybe (a, T m a))
msplit m = lift (m c f) where
    c a f  =  return (Just (a, (lift f >>= reflect)))
    f      =  return Nothing
\end{spec}
\end{block}
But take care of effects, so the fold in |msplit| incurs effects on demand.
\end{frame}

\begin{frame}
\frametitle{Implementation using delimited control}

Essentially the CPS implementation in direct style.
\begin{itemize}
    \item Disjunction (|mplus|) forks a new thread.
    \item Failure (|mzero|) kills the current thread.
    \item A scheduler operates at each prompt to service these backtracking
        requests.
\end{itemize}
It is easy to enhance the scheduler (search strategy).
\end{frame}

\section{Conclusion}

\begin{frame}
\frametitle{Conclusion}

\textbf{Search with a monad transformer}
\smallskip

\alert<2>{An interface that extends |MonadPlus| beyond backtracking}
\begin{itemize}
    \item Once (pruning): |once|
    \item Soft-cut (conditional): |ifte|
    \item Fair disjunction: |interleave|
    \item Fair conjunction: |>>-|
    \item Flexible observation
\end{itemize}
\alert<3>{Implementations using delimited continuations}
\begin{itemize}
    \item A subsuming deconstruction interface: |msplit|
    \item Deterministic code runs at full speed
    \item Future work: more sophisticated scheduling, for completeness and
        termination
\end{itemize}
\onslide<4->
Extended example in paper:
Tic Tac Toe using pruning and conditional for heuristics
\end{frame}

\end{document}
