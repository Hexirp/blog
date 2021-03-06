#########################
Ghosts of Departed Proofs
#########################

``head`` みたいな全ての入力に対して出力があるとは限らない関数を定義したいときに
どうしますか？ 普通は部分関数にするか ``Maybe`` なんなりの失敗を表現できる
何かを使って定義しますよね。でも、部分関数は苛立たしい実行時エラーを引き
起こします。Maybe など安全な方法を取っても、「なんでこれ失敗するの！？」と
いうような時があります。原因を調べるためには、ドキュメントやソースコードを
調べる必要があり無駄な苦労をすることがあります。即ち、どちらの選択肢も
理想的ではありません。

この問題を解決する方法の一つが "Ghosts of Departed Proofs" です。核となる発想は
幽霊型の newtype ラッパーを使うことで、余分な型引数によって高度な事前条件を
オーバーヘッドなしに Haskell の型システムで表現できることであり、ユーザーは
幽霊型によって表現された事前条件を証明することで、引数が正しいものであると
コンパイラに主張します。

"Ghosts of Departed Proof" は Haskell 2010 に対する簡易な拡張のみを使って、
依存型 (dependent type) や篩型 (refinement type) のいくつかの利点を得られると
いう点で、他の方法よりも優位性を持っています。この記事では、リストやマップや
共有メモリ領域やその他へ不変条件を課する例を紹介することで "Ghosts of Departed
Proofs" を段階的に説明していきたいと思います。

******
幽霊型
******

.. code-block:: haskell

 -- 幽霊型でない。
 data Either a b = Left a | Right b

 -- 幽霊型である。
 data D a b = D a Int
 newtype E a = E [Bool]

幽霊型は、その内部で使われていない余分な型引数を持つ型のことです。上の
``Either`` は ``a`` と ``b`` という二つの型引数を持っていて、どちらも使われて
いますので幽霊型ではありません。一方で ``D`` は ``b`` が使われていないので
幽霊型です。また ``E`` も ``a`` が使われていないので幽霊型です。

******************
ソート済みのリスト
******************

.. code-block:: Haskell

 -- `sortBy` は、渡された比較関数によってリストをソートする。
 sortBy :: (a -> a -> Ordering) -> [a] -> [a]
 sortBy = ...

 -- `mergeBy` は、渡された比較関数によってリストをマージする。
 --
 -- `mergeBy comp xs ys` と書いたとき `xs` と `ys` は `comp` によって
 -- ソートされている必要がある。
 mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
 mergeBy comp = go
  where
   go [] ys = ys
   go xs [] = xs
   go (x:xs) (y:ys) = case comp x y of
    GT -> y : go (x:xs) ys
    _  -> x : go xs (y:ys)

このとき、ユーザーは ``mergeBy`` がソートされたリストだけを受け付けること、
そして二つのリストが渡された比較関数によってソートされていなければならない
ことに注意しないといけません。そうすることはかなり大変なことです。

.. code-block:: Haskell

 module Named (Named, type (~~), name) where

 import Data.Coerce

 newtype Named name a = Named a
 type a ~~ name = Named name a

 --      a -> (exists name. (a ~~ name))
 name :: a -> (forall name. (a ~~ name) -> t) -> t
 name x k = k (coerce x)
