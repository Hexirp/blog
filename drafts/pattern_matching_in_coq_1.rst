##########################
Coq でのパターンマッチング
##########################

.. highlight:: coq

Coq は、代数的データ型 (ADT / Algebraic data type) を使えます。さらに、一般化
代数的データ型 (GADT / Generalized algebraic data type) も使えます。さらに、
さらに、依存型もモリモリ使えます。たいへん良い言語ですね。しかしながら、
それが複雑化して、パターンマッチングをどうやればいいのか、それで何が起こるの
か、どう型付けされるのか、素朴に理解できませんでした。この記事は、それらを理解
したいがためのメモです。Haskell を知っているが Coq はよくわからない人
（＝自分）のために書いてあります。

************************
普通のパターンマッチング
************************

まずは、軽いジャブから行きましょう。これらは代数的データ型の範囲であり、
Haskell でも同じことが出来ます。

::

 (** 曜日を表す。 *)
 Inductive day : Type :=
 | monday : day
 | tuesday : day
 | wednesday : day
 | thursday : day
 | friday : day
 | saturday : day
 | sunday : day
 .

 (** 次の平日を返す。 *)
 Definition next_weekday : day -> day :=
  fun d : day =>
   match d with
   | monday => tuesday
   | tuesday => wednesday
   | wednesday => thursday
   | thursday => friday
   | friday => monday
   | saturday => monday
   | sunday => monday
   end
 .

 (** 自然数を表す。 *)
 Inductive nat : Type :=
 | O : nat
 | S : nat -> nat
 .

 (** 前の自然数を返す。 *)
 Definition pred : nat -> nat :=
  fun n : nat =>
   match n with
   | O => O
   | S np => np
   end
 .

Haskell と違うところが出てくるのはここからです。Haskell では（通常）型を関数へ
明示的に渡すことはなく、何もやらなくても型推論されます。例えば、\ ``Just :: a
-> Maybe a`` の真の姿は ``Just :: forall a. a -> Maybe a`` です。\ ``a`` は
ありとあらゆる型でもある、すなわち多相であるということですね。ここらあたりは
入門で語られているでしょうから飛ばします（本当に語られていたのかどうか
覚えていないけど）。さらに先へ進みましょう。\ ``a`` は ``Just`` の引数です。
つまり ``Just 1`` は、最初に ``Int`` が渡されて次に ``1`` が渡されています。

``Just`` に ``Int`` が渡された後に ``"string"`` が渡されたときはどうなるの
でしょうか？ そもそも、そういうことをやろうとすると計算する前からエラーに
なります。この辺りは不思議に思うかもしれません。「普通では関数の引数にそれぞれ
何を渡しても良いのに、先に何を渡されているのかによって型エラーになるのかが
変わるのがよくわからない」とか「プログラムの結果によって渡される型が変わったり
したらどうなるんだ？」とか考えたりするかもしれませんが、そういうことを含めて
全て型検査されるので大丈夫だと納得するしかないと思います。

この渡されている型というのは仮定上の物ではなく、きちんと Haskell の内部に
存在するのですが、普段は推論されて自動的に適したものが渡されるので見えません。
しかしながら存在します。Haskell はコンパイルする途中でどんどん単純な言語へ
変換されていくのですが、その一番目である Core は型が引数として渡されている
様子がはっきり明示的に書かれています。また、明示的に渡せるようになる
言語拡張もあります。\ ``TypeApplications`` という名前であり、オンにしたら
``Just @Int 1`` というように頭にアットマークを付けることで型を引数として渡せる
ようになります。

Coq は言語の特徴として型と値の区別がない言語（完全依存型言語という呼び方を今
考えた）です。型と値の区別がないというのはちょっと強く言いすぎかもしれません
が。とにかく ``Just Int 1`` というように普通の値を渡すときと何ら変わりなく
型を渡せます。その代わり、デフォルトでは型が推論されて自動的に渡されたりは
しません。Haskell で ``Just 1`` の ``1`` が推論されて勝手に渡されたりしないのと
同じように。もちろん、\ ``Int`` が渡されることは後に ``1`` が渡されることから
分かるなど、そのままでは無駄な記述が出てきますが、暗黙引数 (implicit arguments)
というシステムがあるので、その仕組みを使って推論させることが出来ます。この
仕組みは型に対してのみならず値にも適用できます。Scala の暗黙引数 (implicit
parameters) と同じような感じですね。この仕組みを使って型クラスが再現されるのも
同じです。

型を値として渡せるだけではありません。パターンマッチングの時のパターンを書く
時も、引数として扱う必要があり、\ ``Just`` の場合にマッチさせたいときは
``Just ? ?`` とのように書かないといけませんが、この場合はパターンマッチする値の
型が ``Maybe A`` であれば ``Just A ?`` であることが分かるため、\ ``Just _ ?``
とアンダースコアで書きます。これは、省略しているのではなく\ **強制**\ です。
どのような場合にこういう書き方が強制されるのか？ それは ``Inductive`` （または
``CoInductive``\ ）で定義する際に、名前が付いた引数として書かれているときです。
下の ``prod`` においては ``A : Type`` と ``B : Type`` です。これが型ではなく
値を取る引数でも同じことです。この制限にはどのような意味があるのかという疑問に
ついては、冗長な部分を取り除くためだけではなく、また別の答えもあるのですが、
まだ上手く説明できる自信がないので書きません。

::

 (** 直積を表す。 *)
 Inductive prod (A : Type) (B : Type) : Type :=
 | pair : A -> B -> prod A B
 .

 (** [pair] の型を確かめる。 *)
 Check pair : forall A B : Type, A -> B -> prod A B.

 (** 直積の左右を入れ替える。 *)
 Definition swap : forall A B : Type, prod A B -> prod B A :=
  fun A B : Type =>
   fun x : prod A B =>
    match x with
    | pair _ _ x1 x2 => pair B A x2 x1
    end
 .

 (** [swap] の間違った定義。 *)
 Fail Definition swap' : forall A B : Type, prod A B -> prod B A :=
  fun A B : Type :=
   fun x : prod A B =>
    match x with
    | pair xB xA x1 x2 => pair B A x2 x1
    end
 .

この Coq の書き方ではアンダースコア (``_``) に無視したい場合と書けない場合の
二つの意味が出てきてしまうので、筋が通らないように感じます。ただ、Coq では組み
立てるときの ``Just Int 1`` の ``Int`` が暗黙引数で省略できるのと同じように、
パターンでの ``Just _ a`` の ``_`` も暗黙引数で省略できるので、実用上では困った
ことはないです。

::

 Coq < Inductive prod (A : Type) (B : Type) : Type := pair : A -> B -> prod A B .
 prod is defined
 prod_rect is defined
 prod_ind is defined
 prod_rec is defined

 Coq < Check pair .
 pair
      : forall A B : Type, A -> B -> prod A B

 Coq < Definition swap : forall A B : Type, prod A B -> prod B A .
 1 subgoal

   ============================
   forall A B : Type, prod A B -> prod B A

 swap < Proof.

 swap < refine (fun A B => _) .
 1 subgoal

   A : Type
   B : Type
   ============================
   prod A B -> prod B A

 swap < refine (fun x => _) .
 1 subgoal

   A : Type
   B : Type
   x : prod A B
   ============================
   prod B A

 swap < refine (match x with pair xA xB x1 x2 => _ end) .
 Toplevel input, characters 26-28:
 > refine (match x with pair xA xB x1 x2 => _ end) .
 >                           ^^
 Error: The parameters do not bind in patterns; they must be replaced by '_'.

 swap < refine (match x with pair _ _ x1 x2 => _ end) .
 1 subgoal

   A : Type
   B : Type
   x : prod A B
   x1 : A
   x2 : B
   ============================
   prod B A

 swap < refine (pair x2 x1) .
 Toplevel input, characters 13-15:
 > refine (pair x2 x1) .
 >              ^^
 Error:
 Ltac call to "refine (uconstr)" failed.
 In environment
 A : Type
 B : Type
 x : prod A B
 x1 : A
 x2 : B
 The term "x2" has type "B" while it is expected to have type "Type".

 swap < refine (pair A B x1 x2) .
 Toplevel input, characters 8-22:
 > refine (pair A B x1 x2) .
 >         ^^^^^^^^^^^^^^
 Error:
 Ltac call to "refine (uconstr)" failed.
 In environment
 A : Type
 B : Type
 x : prod A B
 x1 : A
 x2 : B
 The term "pair A B x1 x2" has type "prod A B"
 while it is expected to have type "prod B A".

 swap < refine (pair B A x2 x1) .
 No more subgoals.

 swap < Defined.
 swap is defined

 Coq < Quit.

****
GADT
****

GADT は、今までの理解からもう一歩踏み出さないといけません。ADT の素朴な理解は
「型 ``A`` は ``X`` か ``Y`` のどちらかである」というぐらいでしょう。

.. code-block:: haskell

 data A = X | Y

パラメータがあっても、同じように「\ ``List a`` は ``Nil`` か ``Cons`` のどちら
かである」という考え方が出来るでしょう。言い換えれば「\ ``a`` がどのように
変わっても型の構造自体は変わらない」という風になります。

.. code-block:: haskell

 data List a = Nil | Cons a (List a)

それでは、このような型ではどうなるのでしょう？

.. code-block:: haskell

 data B a where
  BB :: Bool           -> B Bool
  BI :: Int            -> B Int
  BE :: B Int -> B Int -> B Bool

``B a`` は ``BB`` か ``BI`` か ``BE`` のどちらかなのか？ 違います。

``a`` の値によって異なるというのが答えです。\ ``B a`` というひとまとまりの
型ではなくて ``B Bool``, ``B Int``, ``B Char`` というようにそれぞれで構造が
違っていて、\ ``B Bool`` は ``BB`` か ``BE`` であり ``B Int`` は ``BI`` ただ
一つであり ``B Char`` は値が存在しません。引数としてどのような型を受け取るかに
よって構造そのものが変化するのです。

こんなのどうやって実装するんでしょうか。 ``List a`` はどんな型に対しても同じ
だったから型消去すればそれだけで済むはずでした（私は Java からプログラミングを
始めたので型消去が一番にイメージされます）。これでは型消去したら、たとえ
Haskell のプログラムで書けなくとも ``B Char`` の型を持つ ``BB`` とかが
内部表現で書けてしまうはずです。この質問に対する答えは「細かいことは気に
しない」が一番良いでしょう。ただ世界に ``B`` と ``BB`` とかが放り込まれて、
それが世界の基盤でどう表現されるのかは考えない、という考え方です。また、
内部表現で型が合わない異常な値が書けても Haskell のプログラミングの内部で
書けなければそれでよしとします。

ちょっと横道にそれてしまいますが、罠として ``GADT`` 風の表記（これは
``GADTSyntax`` 拡張により単体で使える）では、上に書いてある型の引数の名前は
何にも意味がない、というのがあります。下の４つの定義は全て等価です。

.. code-block:: haskell

 data List a where
  Nil :: List a
  Cons :: a -> List a -> List a

 data List b where
  Nil :: List a
  Cons :: a -> List a -> List a

 data List b where
  Nil :: List a
  Cons :: b -> List b -> List b

 data List hoge where
  Nil :: List huga
  Cons :: piyo -> List piyo -> List piyo

さらに GADT がモジュールを使って再現できるという話のせいで、私は混乱したことが
あります。これが内部表現なのか？ しかし、この定義はパターンマッチングの際に
破綻します。パターンマッチングをするときの専用の関数を作らないといけないのです
が、その時に ``B a`` が ``BB`` で構築されている、よって ``a`` は ``Bool``
であるという推論が表現できないのです。

.. code-block:: haskell

 module B (B, bb, bi, be) where

  data B a = BB Bool | BI Int | BE (B Int) (B Int)

  bb :: Bool -> B Bool
  bb = BB

  bi :: Int -> B Int
  bi = BI

  be :: B Int -> B Int -> B Bool
  be = BE

もし ADT の範囲でどうにか表現したいのであれば、\ ``a`` が変わることによって
変化する構造を一か所だけに押し込めることで可能になります。そのために今まで
なかった新しい型が必要になります。それは等式型というものです。\ ``(:~:)`` は
両辺の型が等しいことを表す型です。\ ``a :~: b`` は ``a``, ``b`` が等しくない
とき空で、\ ``a``, ``b`` が等しいとき ``Refl`` というただ一つの値を持ちます。
``a`` が ``Bool`` だったときは ``Bool :~: Int`` の値は存在しないため、
``BI`` を使うことはできません。

.. code-block:: haskell

 data B a = BB (a :~: Bool) Bool
          | BI (a :~: Int) Int
          | BE (a :~: Bool) (B Int) (B Int)

Coq でも GADT は書くことが出来ます。むしろ Coq で証明を表すために必須です。
「GADT で証明する」というテーマの記事は読んだことありませんか？ ああいう風に
GADT と証明というものは密接に関係しています。

上での例を Coq に翻訳したものが下の例です。\ ``prod`` に型引数があるのに対して
``B`` の型引数はありません。あるいは Haskell で ``f x = ...`` を ``f = \x ->
...`` と書くのと似たような感じでしょうか。それがなんであるかによって取りえる
構造そのものが変わる型引数は、明示的に名前を付けることが出来ません。それぞれの
構築子が返す型に与えられている引数の中で、構築子によって変化する引数は固定され
ていません。この辺りは、自由に名前を付けられてその名前が何の意味も持たない
Haskell よりも分かりやすいです。

::

 (** 適当な GADT を使うための定義。

     [Int] は Coq で表すのが難しいので [nat] で代替する。 *)
 Inductive B : Type -> Type :=
 | BB : bool -> B bool
 | BI : nat -> B nat
 | BE : B nat -> B nat -> B bool
 .

前に書いた等式型で表す書き方と同じように書くこともできます。

::

 (** もう一つの [B] 。 *)
 Inductive B (A : Type) : Type :=
 | BB : A = bool -> bool -> B A
 | BI : A = nat -> nat -> B A
 | BE : A = bool -> B nat -> B nat -> B A
 .

Coq ではデフォルトで GADT みたいな書き方をします。たとえ ADT で表せる型であって
でもです。一貫性があってよい仕様だと思います。

ちなみに GADT は「ADT に全称量化と存在量化と等式型を加えたもの」として定義され
ます。全称量化は全称型と、存在量化は存在型とも呼ばれます。

******
依存型
******

Coq では、さっきまで見てきた GADT に限らず依存型も書くことが出来ます。つまり
``A -> Type`` という型を持つ値（ただし ``A`` は ``Type`` ではない）を記述する
ことが出来ます。型ではないのかと混乱したことがありますが、\ ``Type`` という
型を持つものだけが型ということ、または ``x : T`` という式の ``T`` として書ける
ものが型だと考えます。

``forall n : nat, P n`` という型を持つ値を作りたいとしましょう。全称型は
関数型を一般化したものなので、同じように ``fun n => _`` と書けます。あとは、
``P n`` という型を持つ値を作らないといけません。ここで ``n`` に対して場合分け
したくなったとします（ここで ``n : nat`` という値がラムダ抽象によって環境に
加えられています）。

下のように書けばいいのです。一番目の空白は ``n`` が ``O`` だと分かっている
ので ``P O`` の型を持つ値で埋め、二番目の空白は ``n`` が ``S np`` だと分かって
いるので ``P (S np)`` で埋めます。

::

 match n with
 | O => _ (* 1 *)
 | S np => _ (* 2 *)
 end

くだんの ``A -> Type`` が帰納的に定義されている場合もあります。

::

 Inductive Vec (A : Type) : nat -> Type :=
 | Nil : Vec A O
 | Cons : forall n, A -> Vec A n -> Vec A (S n)
 .

この型が引数になっている場合のパターンマッチングはどうすればいいのでしょうか？
具体的な式として書き下してみると ``forall n, Vec A n -> B n`` みたいな場合
です。まず、下のようにすると ``n : nat`` と ``x : Vec A n`` が環境に入って
パターンマッチングできるようになります。

::

 fun (n : nat) (x : Vec A n) => _

そして ``x`` に対してパターンマッチングします。

これだけでうまく行く場合もありますが、そうでない場合もあります。一番目の空白は
``x`` が ``Nil`` なので ``n`` は ``O`` であることが分かっているはずなので、
埋めるべき値の型は ``B O`` となっていてほしいのですが、そのまま ``B n`` と
なっていたりすることがあります。

::

 fun (n : nat) (x : Vec A n) =>
  match x with
  | Nil _ => _ (* 1 *)
  | Cons _ np xv xs => _ (* 2 *)
  end

これを避けるために ``B n`` の ``n`` は、\ ``x : Vec A n`` に対する場合分けに
よって具体的にどういう値なのかわかるということを、コンパイラに知らせないと
いけません。

``x : Vec A n`` をパターンマッチングするとき ``n`` に関する情報が得られます。
それを ``n'`` と表すために ``in Vec _ n'`` と書いて ``return B n'`` と
使います。

::

 fun (n : nat) (x : Vec A n) =>
  match x in Vec _ n' return B n' with
  | Nil _ => _ (* 1 *)
  | Cons _ np xv xs => _ (* 2 *)
  end


もうちょっと難しくなるのが ``n`` に条件が付いているような場合です。つまり
``forall n, P n -> Vec A n -> B n`` のような場合です。こういう時は、
``forall n, Vec A n -> P n -> B n`` みたいに入れ替えてパターンマッチングすれば
いいです。上でのパターンマッチングでいう ``B n`` は、この時 ``P n -> B n``
です。

もし、そのままパターンマッチングしたら、このように書くことになるでしょう。

::

 fun (n : nat) (H : P n) (x : Vec A n) =>
  match x in Vec a n' return B n' with
  | Nil _ => _ (* 1 *)
  | Cons _ np a xs => _ (* 1 *)
  end

この時 ``_1 : B 0``, ``_2 : B (S np)`` です。いざ、条件 ``H : P n`` を使って
証明しようとしたら、例えば一番目の場合では ``P 0`` であることが分かりません。
こういった困った事態は、このようにして回避できます。

::

 fun (n : nat) (H' : P n) (x : Vec A n) =>
  match x in Vec a n' return P n' -> B n' with
  | Nil _ => fun (H : P 0) => _ (* 1 *)
  | Cons _ np a xs => fun (H : P (S np)) => _ (* 2 *)
  end H'

このように ``H`` をパターンマッチングに巻き込むことでできます。

なら ``Vec A 1 -> B`` は？一般化された ``forall n, n = 1 -> Vec A n -> B`` に
変換した後にパターンマッチングすればオーケーです。上でのパターンマッチングで
いう ``B n`` は、この時 ``n = 1 -> B`` です。

実のところ ``Vec A n -> B`` でパターンマッチしたとき、それぞれの枝での ``n`` は
``0`` とか ``S np`` とかに置き換えられますが、それらは ``n`` と切り離されていて
なにも関係がないように振る舞います。

::

 Definition Vec_match
   (A : Type)
   (B : forall n : nat, Vec A n -> Type)
   (o : B 0 (Nil A))
   (s : forall (np : nat) (x : A) (xs : Vec A np),
           B (S np) (Cons A np x xs))
   (n : nat)
   (x : Vec A n)
   : B n x
   :=
     match x as x' in Vec _ n' return B n' x' with
     | Nil _ => o
     | Cons _ np x xs => s np x xs
     end
   .

パターンマッチングの部分だけ取り出した関数はこれです。上で ``Vec A n`` に当たる
``n`` が、その直前で全称量化により導入された引数であればうれしいといった訳は、
ゴールが ``forall n, Vec A n -> B n`` という形で、この関数がそのまま適用できる
形 (``Vec_match A (fun n _ => B n) o s``) になっているからです。

この場合のような ``n`` が決まっているときも、上の関数だけで表せます。つまり
``Vec_match A (fun _ _ => B) o s 1`` とするのです。しかし、これでは ``1`` で
あるという情報が消えてしまう、というのがポイントです。

対応したパターンマッチングはこのようになります。

::

 match x in Vec _ n' return n' = 1 -> B with
 | Nil _ => _
 | Cons _ np x xs => _
 end

Coq のパターンマッチングの式は ``as`` や ``in`` キーワードで新しい引数を
導入するので、\ ``forall n, Vec A n -> n = 1 -> B`` に書かれている全称量化に
より導入された ``n`` を、それで代用できます。返り値が関数になっていますが、
``n'`` は外側からは ``1`` として見えているので、\ ``eq_refl`` を渡してやれば
元に戻せます。

なら ``forall m n, Vec A (m + n) -> B m n`` は？新しい引数 ``o`` を取って、
``forall o, Vec A o -> forall m n, o = m + n -> B m n`` とすればいいのです。
上でのパターンマッチングでいう ``B o`` ( ``o`` は上での ``n`` と考える）は、
この時 ``forall m n, o = m + n -> B m n`` です。

上のやり方と同じようにできます。

::

 match x in Vec _ o return o = m + n -> B m n with ... end

これらを統一的に取り扱う方法はないのでしょうか？

もう一つ ``forall n, P n -> Vec A n -> B n`` を ``n : nat`` と ``H : P n`` まで
``intro`` した後に「あ！やりすぎた！」ということはないでしょうか？本来は上の
ように変形すべきだったというときです。これが十数個の引数とそれらの依存関係と
合わさると活力が奪われること間違いなしです。

こういう時、そのまま簡単に進む方法はないのでしょうか？
