---
title: Traversable のための圏論
description: Haskell の Traversable を圏論で形式化する二通りの方法について。
canonical: 'https://hexirp.github.io/blog/articles/category_theory_for_traversable.html'
type: article
...

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

.. _独習 Scalaz: http://eed3si9n.com/learning-scalaz/ja/Lawless-typeclasses.html

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
Identity と Compose です。Traversable を圏論で表現するときは、これらの
概念が重要になってきます。

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
発展させたものであることが予測されます。つまり、自然変換に何かモノイダル関手の
構造と合うような条件を付けたものになると考えられます。

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

ちなみに ``Functor f`` と ``Functor g`` の間の自然変換は ``t :: f a -> g a`` と
いう型を持ちますが、このような型を持つ関数は、驚くべきことに必ず自然変換に
なります。モノイダル関手ではそういうことはありません（リストから ``ZipList``
への関数が反例になります）。

----

関手には、恒等関手があり関手の合成があります。ゆえに圏になります。それが圏と
関手の圏と呼ばれるものです。Haskell では恒等関手と関手の合成はこのように
書かれます。つまり ``Identity`` が恒等関手であり二つの関手 ``f`` と ``g`` を
合成した関手が ``Compose f g`` です。

.. code-block:: haskell

 newtype Identity a = Identity a

 newtype Compose f g a = Compose (f (g a))

 instance Functor Identity where
     fmap f (Identity a) = Identity (f a)

 instance (Functor f, Functor g) => Functor (Compose f g) where
     fmap f (Compose a) = Compose (fmap (fmap f) a)

モノイダル関手には、恒等モノイダル関手がありモノイダル関手の合成があります。
より分かりやすくいうと、恒等関手はモノイダルであり、二つのモノイダル関手の
合成はモノイダル関手になります。

.. code-block:: haskell

 instance Applicative Identity where
     pure x = Identity x
     Identity f <*> Identity x = Identity (f x)

 instance (Applicative f, Applicative g) => Applicative (Compose f g) where
     pure x = Compose (pure (pure x))
     Compose f <*> Compose x = Compose ((<*>) <$> f <*> x)

ちなみに、モナドには合成がありません。モナド変換子は、もし合成があったのならば
必要なかったでしょう。この話は本題と逸れますが、面白いので調べてみてください。

さて、ここまで applicative transformer と Identity と Compose の圏論の
結びつきを解説してきました。Traversable を圏論で考えるときに、これらの定義が
効いてくるのです。

**************************
特殊な自然変換を持った関手
**************************

``Traversable`` は ``Functor`` のうち特殊な条件を満たすものでしたから、
圏論でも特殊な関手として表現できるでしょう。では、どのように特殊なのでしょう
か。

核になるのは ``sequenceA`` です。つまり、\ ``sequenceA`` が任意の
applicative な関手に対しての自然変換として見れることを使います。

関手 T が、traversable であるとは、
 任意の applicative な関手 F に対して d という TF から FT の自然変換が存在し、
 ほにゃららという条件を満たすことである。

この「ほにゃらら」は、上の Traversable のソースコードにある Traversable 則を
圏論的に書き表したものです。Traversable 則は以下の通りです。ただし ``d`` で
``sequenceA`` を置き換えました。

naturality
 ``t . d = d . fmap t`` for every applicative transformation ``t``

identity
 ``d . fmap Identity = Identity``

composition
 ``d . fmap Compose = Compose . fmap d . d``

まず、これは何と何が等しいと言っているのでしょうか？ 関数と関数が等しいことを
表すと考えてもいいかもしれませんが、両辺とも自然変換になっています（applicative
transfomation ではありません）。さらに言えば関手と自然変換の圏の上で考えた方が
いいでしょう。可換図式が書けます！

参考にした論文では "A traversable functor is a functor 'T : C -> C' equipped
with a distributive law 'd_F : TF -> FT' for T over the action of App on C by
evaluation." と書かれていて action という言葉を使って定義されているのですが、
よく分からなかったので直後に書いてある明示的な則を載せました。

可換図式は下のようになります。ただし、\ ``d_F`` は F に対する d を表します
（任意の applicative な関手に対する自然変換だったのでした）。

.. code-block:: text

 naturality:

           d_F
     TF ---------> FT

     |             |
     |             |
  Ta |             | aT
     |             |
     v             v

     TG ---------> GT
           d_G

            for every morphism 'a' in 'App'

 composition:

              d_(FG)
   TFG -------------------> FGT

   |                        ^
   |                        |
   +---------> FTG ---------+
     (d_F)G         F(d_G)

 identity:

       d_I
  TI ---------+
              |
  |           v
  |
  +---------> IT
       id

****************
ある圏の自己関手
****************

`特殊な自然変換を持った関手`_ 節では ``sequenceA`` を使って形式化しました。
では、\ ``traverse`` を使った形式化もあるのでしょうか？ あります。

とある圏を考えましょう。対象は Haskell の型です。\ ``A`` から ``B`` への射は
任意の ``Applicative`` である ``F`` に対して ``A -> f B`` です。スライス圏と
同じような感じだと思ってください。この定義はちょっと非直感的なので Haskell で
書き下しましょう。

.. code-block:: haskell

 -- 対象は A :: * である。

 -- 対象 A, B に対して、任意の F において Applicative F であるならば
 -- f : A -> F B は射である。
 data C :: * -> * -> * where
  Mk_C :: forall f. Applicative f => (a -> f b) -> C a b

恒等射と合成はこのように実装されます。

.. code-block:: haskell

 id :: C a a
 id = Mk_C (pure :: a -> Identity a)

 (.)
   :: C b c
   -> C a b
   -> C a c
 (Mk_C g) . (Mk_C f) = C (Compose . fmap g . f)

``Traversable`` は、この圏の特殊な自己関手です。つまり、この関手は、このような
型クラスとして説明されます。

.. code-block:: haskell

 class Traversable t where
   tmap :: C a b -> C (t a) (t b)

そして、引数が含む存在量化された型 ``f`` が、返り値が含む存在量化された
型 ``g`` と一致する条件が課されます。もちろん関手であるための条件もです。

これでうまくいくことの説明は読者への演習問題とします（めんどくさいだけ）。

********
終わりに
********

Traversable の圏論での形式化を二通り紹介しました。どっちにしても、楽しいです
よね！

ちなみに「ある圏の自己関手」での定義方法は自分の思いつきです。誰かが先に考えて
いると思いますが（と予防線を張る）。

追記 (2019/01/24): いました (https://duplode.github.io/posts/traversable-a-remix.html) 。

****
出典
****

* Traversable の定義 - https://hackage.haskell.org/package/base-4.12.0.0/docs/src/Data.Traversable.html
* 特殊な自然変換を持った関手 - `Categories of Optics`_ の Definition 4.6.2

.. _Categories of Optics: https://arxiv.org/abs/1809.00738


.. role:: strike
