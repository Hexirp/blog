########################
Traversable のための圏論
########################

Haskell の型クラス ``Traversable`` は知っていますか？ ``Foldable`` の進化版に
して、lawful であることが美しく感じて、私が好きな型クラスの一つです。

Haskell は、比較的に圏論と関係深い言語です。そもそもの ``Functor`` とか
``Monad`` が圏論と関連しているし、kan-extension なんてパッケージが存在します。

Haskell で圏論を表現するのと反対に、圏論で Haskell の型クラスを解釈することも
行われています。

この記事では ``Traversable`` の圏論的な解釈を紹介します。

**************************
特殊な自然変換を持った関手
**************************

``Traversable`` は ``Functor`` です。さらに ``sequenceA :: Applicative f => t
(f a) -> f (t a)`` を考えます。

Applicative transformer は Applicative の間の自然変換です。Functor では
任意の ``f :: forall a. F a -> G a`` が自然変換になりましたが、Applicative では
そうでありません。List から ZipList へそのまま変換するものが例外です。
``a`` を Applicative transformation とします。

.. code-block:: haskell

 a . sequenceA === sequenceA . fmap a
 fmap sequenceA . sequenceA === sequenceA
 sequenceA === fmap sequenceA

****************
ある圏の自己関手
****************

圏 ``C`` を考えましょう。

* 対象: カインドが ``*`` である型
* 射: `a, b :: *`` に対して、任意の ``Applicative`` である ``F`` を取って、
  ``a -> F a``

恒等射と合成はこのように実装されます。

.. code-block:: haskell

 id :: Applicative f => a -> f a
 id = pure

 (.)
   :: (Applicative f, Applicative g)
   => (b -> g c)
   -> (a -> f b)
   -> (a -> Compose f g c)
 g . f = Compose . fmap g . f

``Traversable`` は、この圏の自己関手です。
