A test model: draw some eggs from two kinds, where we have 'different'
means (and standard deviation 1) for the weight.

\begin{code}
module Eggs where

import Model

theta = ("theta", [])
r = "R"
cs = "c"
c = (cs,[])
mu_c = ("mu",[cs])
x = ("X",[])
xe = ("x",[])

egg_model n = [
   Dist $ A theta $ D (Beta [3,2]),
   Dist $ A mu_c (Over (D $ Normal 0.0 1.0) cs r),
   Dist $ A x (Partition n [
       Dist . A c . D $ Cat theta,
       Dist $ A xe $ DepD $ NormalM (Apply mu_c c) 1 ] ) ]

pdf = trans (egg_model 20)
\end{code}
