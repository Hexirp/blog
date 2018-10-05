###############
Haskellでの等式
###############

Haskell での等式に興味がわいたので調べてみただけの記事です。

****
概観
****

==== ======== ======== ========== ====== =========
名前 多型か？ 軽性     返り値     約振り 定義箇所
==== ======== ======== ========== ====== =========
(~#) hetero   unlifted #          nomial GHC.Prim
(~~) hetero   lifted   Constriant nomial GHC.Types
(~)  homo     lifted   Constriant nomial Data.Type.Equality
(:~:) homo    lifted   *          nomial Data.Type.Equality
. . . . . .
(~R#) hetero unlifted # repr. GHC.Prim
Coercible homo lifted Constriant repr. GHC.Types
Coecion homo lifted * repr. Data.Type.Coercion
. . . . . .
(~P#) hetero unlifted phantom GHC.Prim
