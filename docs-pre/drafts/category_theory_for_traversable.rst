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
ついては様々な考え方があり、誤った考え方を修正しようとする人が多いことです。
それは良いことだとも言えますが、もしあなたがこれについて間違ったことをどこかに
書いてしまったら、あなたが間違ったことを書いてしまったことが、他の人たちへ
一瞬で広まっていきます。自分が間違ってしまったという事を広められたくない人は
注意するべきです。

話を戻すと、最初に出した例は型クラスでしたね。\ ``Closed`` など、\ ``Functor``
型クラスに圏論に由来する何らかの条件を付けた型クラスが多くあります。また、
``Contravariant`` は圏論の「反変関手」という概念を型クラスで表現したものです。
さらに ``Profunctor`` や ``Bifunctor`` など派生した型クラスが大量にあります。
圏論は「圏」についての分野ですが、そのものずばり ``Category`` という型クラスも
あります。さらに、それの ``Arrow`` という派生は ``Monad`` に代わるもう一つの
プログラミングの抽象化として知っておくべきです。

「Traversable」と「圏論」という二つの話が出ました。この記事は、この二つの
結び付きを解説するものです。

******************
Traversable の定義
******************

定義をおさらいしましょう。ソースコードから抜き出したのが下のです。

.. code-block:: haskell

 -- | Functors representing data structures that can be traversed from
 -- left to right.
 --
 -- A definition of 'traverse' must satisfy the following laws:
 --
 -- [/naturality/]
 --   @t . 'traverse' f = 'traverse' (t . f)@
 --   for every applicative transformation @t@
 --
 -- [/identity/]
 --   @'traverse' Identity = Identity@
 --
 -- [/composition/]
 --   @'traverse' (Compose . 'fmap' g . f) = Compose . 'fmap' ('traverse' g) . 'traverse' f@
 --
 -- A definition of 'sequenceA' must satisfy the following laws:
 --
 -- [/naturality/]
 --   @t . 'sequenceA' = 'sequenceA' . 'fmap' t@
 --   for every applicative transformation @t@
 --
 -- [/identity/]
 --   @'sequenceA' . 'fmap' Identity = Identity@
 --
 -- [/composition/]
 --   @'sequenceA' . 'fmap' Compose = Compose . 'fmap' 'sequenceA' . 'sequenceA'@
 --
 -- where an /applicative transformation/ is a function
 --
 -- @t :: (Applicative f, Applicative g) => f a -> g a@
 --
 -- preserving the 'Applicative' operations, i.e.
 --
 --  * @t ('pure' x) = 'pure' x@
 --
 --  * @t (x '<*>' y) = t x '<*>' t y@
 --
 -- and the identity functor @Identity@ and composition of functors @Compose@
 -- are defined as
 --
 -- >   newtype Identity a = Identity a
 -- >
 -- >   instance Functor Identity where
 -- >     fmap f (Identity x) = Identity (f x)
 -- >
 -- >   instance Applicative Identity where
 -- >     pure x = Identity x
 -- >     Identity f <*> Identity x = Identity (f x)
 -- >
 -- >   newtype Compose f g a = Compose (f (g a))
 -- >
 -- >   instance (Functor f, Functor g) => Functor (Compose f g) where
 -- >     fmap f (Compose x) = Compose (fmap (fmap f) x)
 -- >
 -- >   instance (Applicative f, Applicative g) => Applicative (Compose f g) where
 -- >     pure x = Compose (pure (pure x))
 -- >     Compose f <*> Compose x = Compose ((<*>) <$> f <*> x)
 --
 -- (The naturality law is implied by parametricity.)
 --
 -- Instances are similar to 'Functor', e.g. given a data type
 --
 -- > data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
 --
 -- a suitable instance would be
 --
 -- > instance Traversable Tree where
 -- >    traverse f Empty = pure Empty
 -- >    traverse f (Leaf x) = Leaf <$> f x
 -- >    traverse f (Node l k r) = Node <$> traverse f l <*> f k <*> traverse f r
 --
 -- This is suitable even for abstract types, as the laws for '<*>'
 -- imply a form of associativity.
 --
 -- The superclass instances should satisfy the following:
 --
 --  * In the 'Functor' instance, 'fmap' should be equivalent to traversal
 --    with the identity applicative functor ('fmapDefault').
 --
 --  * In the 'Foldable' instance, 'Data.Foldable.foldMap' should be
 --    equivalent to traversal with a constant applicative functor
 --    ('foldMapDefault').
 --
 class (Functor t, Foldable t) => Traversable t where
     {-# MINIMAL traverse | sequenceA #-}

     -- | Map each element of a structure to an action, evaluate these actions
     -- from left to right, and collect the results. For a version that ignores
     -- the results see 'Data.Foldable.traverse_'.
     traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
     {-# INLINE traverse #-}  -- See Note [Inline default methods]
     traverse f = sequenceA . fmap f

     -- | Evaluate each action in the structure from left to right, and
     -- collect the results. For a version that ignores the results
     -- see 'Data.Foldable.sequenceA_'.
     sequenceA :: Applicative f => t (f a) -> f (t a)
     {-# INLINE sequenceA #-}  -- See Note [Inline default methods]
     sequenceA = traverse id

     -- | Map each element of a structure to a monadic action, evaluate
     -- these actions from left to right, and collect the results. For
     -- a version that ignores the results see 'Data.Foldable.mapM_'.
     mapM :: Monad m => (a -> m b) -> t a -> m (t b)
     {-# INLINE mapM #-}  -- See Note [Inline default methods]
     mapM = traverse

     -- | Evaluate each monadic action in the structure from left to
     -- right, and collect the results. For a version that ignores the
     -- results see 'Data.Foldable.sequence_'.
     sequence :: Monad m => t (m a) -> m (t a)
     {-# INLINE sequence #-}  -- See Note [Inline default methods]
     sequence = sequenceA

必要な関数は ``traversal`` または ``sequenceA`` で、どっちにもいくつかの条件が
必要になるという一般的な型クラスですね。条件を読み解くと ``Functor`` とは違い
条件を記述するために新しい定義をしています——Applicative transformer と
Identity と Compose です。これが必要なわけは圏論で考えることで分かります。

******************
Applicative と圏論
******************

``Applicative`` 型クラスは、圏論のモノイダル関手 (monoidal functor) と結び付け
られます。圏論のモノイダル関手は C というモノイダル圏から D というモノイダル
圏への自然な条件を満たす関手ですが、Haskell の ``Applicative`` は特殊化されて
いて、Hask という Haskell の型を対象として、型の間の関数を射とする圏を、
``(,)`` と ``()`` によってモノイダル圏にしたもの、それからそれへのモノイダル
関手です。

さて、二つの関手の間には自然変換が考えられます。同じように二つのモノイダル
関手の間にある自然変換のようなものが考えられます。二つの圏の間の関手が、
二つのモノイダル圏の間のモノイダル関手と発展したように、その何かは自然変換を
発展させたものであることが期待されます。つまり、自然変換に何かモノイダル関手の
構造と合うような条件を付けたものになってほしいのです。

実際にその通りになり、上のソースコードから抜き出すと、下のように定義されて
います。さっきまでの話は難しかったかと思いますが、Haskell のソースコードで
表現するとこういう風になるものなんだと分かっていれば大丈夫です。

.. code-block:: haskell

 -- ~~~
 -- where an /applicative transformation/ is a function
 --
 -- @t :: (Applicative f, Applicative g) => f a -> g a@
 --
 -- preserving the 'Applicative' operations, i.e.
 --
 --  * @t ('pure' x) = 'pure' x@
 --
 --  * @t (x '<*>' y) = t x '<*>' t y@
 -- ~~~

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
