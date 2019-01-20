########################
Traversable のための圏論
########################

Haskell の ``Traversable`` 型クラスは、私が好きな型クラスの一つです。
``Foldable`` 型クラスには ``Foldable`` 則がないという問題があります（私は
`独習 Scalaz`_ で知りました）。すなわち無法者 (lawless) だというのです。
しかし ``Traversable`` はどうでしょうか？ ``Foldable`` と根を同じくしながらも
``Traversable`` 則を持っています。このことに私はいたく感動しました。ですから、
私は ``Traversable`` 型クラスが好きです。

話を変えましょう。Haskell は圏論と関係深い言語です。代表的なのは ``Functor`` や
``Monad`` という概念でしょう。断らないといけないのは、Haskell と圏論の関係に
ついては様々な考え方があることです。「Haskell は圏論を知らなくてもできる」と
いう意見や、「Haskell と圏論を結びつけるな」という過激な意見もあります。

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
