##########################
Coq でのパターンマッチング
##########################

.. highlight:: coq

Coq は代数的データ型 (ADT / Algebraic data type) のようにして新しい型を定義
でき、さらに一般化代数的データ型 (GADT / Generalized algebraic data type) や
依存型もモリモリ使える、という言語です。

それが複雑化して、パターンマッチングの時に何が起こるのか素朴に理解できない
ものになってしまっています。この記事は、それらを理解したいがためのメモです。

自分に対して書いているのか他人に対して書いているのかよく分からないので注意を。
Haskell を知っているが Coq はよくわからない人（＝自分）のために書いてあります。

************************
普通のパターンマッチング
************************

まずは、軽いジャブから行きましょう。これらは代数的データ型の範囲です。

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

Haskell でも同じようなことが出来ます。

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

ここで、ちょっと変化が出ます。

Haskell では型を明示的に渡すことはありません（通常は）が Coq では型を普通の値と
同じように扱うことが出来るので、パターンマッチングの時も型を気にかけないと
いけません。\ ``pair _ _ x1 x2`` と長々と書かないといけないってことです。

組み立てるときも同じことで ``pair A B x1 x2`` と書かないといけません。もちろん
Coq には Implicit arguments って仕組みがあって、ここでいう型の部分とかの
省略したいところを省略できる仕組みがあります。

要するに、Coq では型もパターンマッチングの時に取り出せる一つの値です。
とはいえ、ここは ``A``, ``B`` が ``prod`` の引数として「明示」されてるので、
自明に ``xB = B``, ``xA = A`` と分かるため「いらないよ！」ってエラーになります。

****
GADT
****

GADT は、今までの理解からもう一歩踏み出さないといけません。ADT の素朴な理解は
型 ``A`` は ``X`` か ``Y`` のどちらかである、というぐらいでしょう。

.. code-block:: haskell

 data A = X | Y

パラメータがあっても同じで ``List a`` は ``Nil`` か ``Cons`` のどちらかである、
という考え方が出来るでしょう。

.. code-block:: haskell

 data List a = Nil | Cons a (List a)

じゃあ、こんな型は何なんでしょうか。

.. code-block:: haskell

 data B a where
  BB :: Bool           -> B Bool
  BI :: Int            -> B Int
  BE :: B Int -> B Int -> B Bool

``B a`` は ``BB`` か ``BI`` か ``BE`` のどちらかなのでしょうか。違います。
なら何なのでしょうか。\ ``a`` の値によって異なるというのが答えです。

``B a`` というひとまとまりの型ではなくて ``B Bool``, ``B Int``, ``B Char`` と
いうようにそれぞれの型で異なります。\ ``B Bool`` は ``BB`` か ``BE`` であり 
``B Int`` は ``BI`` であり ``B Char`` は値が存在しません。

こんなのどうやって実装するんでしょうか。\ ``List a`` はどんな型に対しても
同じだったから型消去すればそれだけで済むはずでしたが、これでは型消去したら
たとえ Haskell のプログラムで書けなくとも ``B Char`` の型を持つ ``BB`` とかが
内部表現で書けてしまうはずです。

この質問に対する答えは「細かいことは気にしない」が一番良いでしょう。ただ世界に
``B`` と ``BB`` とかが放り込まれて、それが世界の基盤でどう表現されるのかは
考えない、というイメージで乗り切りました。

また、罠として、\ ``GADT`` 風の表記（これは ``GADTSyntax`` 拡張により単体で
使える）では、上に書いてある型の引数の名前は何にも意味がない、というのも
あります。

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

これらの定義はすべて等価です。

さらに GADT がモジュールを使って再現できるという話もありました。私はそれを見て
混乱しました。

.. code-block:: haskell

 module B (B, bb, bi, be) where

  data B a = BB Bool | BI Int | BE (B Int) (B Int)

  bb :: Bool -> B Bool
  bb = BB

  bi :: Int -> B Int
  bi = BI

  be :: B Int -> B Int -> B Bool
  be = BE

これが内部表現なのか、と混乱しましたが、この定義はパターンマッチングの際に
破綻します。\ ``BB`` にマッチしたとき、\ ``a`` が ``Bool`` であることが
分からないのです。

結局、正しい ADT での表現はこういうものになります。

.. code-block:: haskell

 data B a = BB (a :~: Bool) Bool
          | BI (a :~: Int) Int
          | BE (a :~: Bool) (B Int) (B Int)

``(:~:)`` は両辺の型が等しいことを表す型です。つまり「 GADT は ADT に全称量化、
存在量化、等式を加えたもの」です。

Coq の話に戻りましょう。普通に定義できます。

::

 (** 適当な GADT を使うための定義。

     [Int] は Coq で表すのが難しいので [nat] で代替する。 *)
 Inductive B : Type -> Type :=
 | BB : bool -> B bool
 | BI : nat -> B nat
 | BE : B nat -> B nat -> B bool
 .

``prod`` の型引数とは違って ``B`` の型引数はありません。あるいは Haskell で
``f x = ...`` を ``f = \x -> ...`` と書くのと似たような感じでしょうか。
型引数によって、取りえる構造そのものが変わるためです。この辺り Haskell より
分かりやすいです。

::

 (** もう一つの [B] 。 *)
 Inductive B (A : Type) : Type :=
 | BB : A = bool -> bool -> B A
 | BI : A = nat -> nat -> B A
 | BE : A = bool -> B nat -> B nat -> B A
 .

こう書くこともできます。さっき書いた書き方と同じですね。ところで Coq では
デフォルトで GADT みたいな書き方をしますが、一貫性があってよい仕様だと
思います。

******
依存型
******

Coq ではそれだけではなく依存型も書くことが出来ます。つまり ``A -> Type`` という
型を持つ値（ただし ``A`` は ``Type`` ではない）を記述することが出来ます。

``forall n : nat, P n`` という型を持つ値を作りたいとしましょう。全称量化は
関数型を一般化したものなので、同じように ``fun n => _`` と書けます。あとは、
``P n`` という型を作らないといけません。ここで ``n`` に対して場合分けしたく
なったとします。（ここで ``n : nat`` という値がラムダ抽象によって環境に
加えられている）。

::

 match n with
 | O => _ (* 1 *)
 | S np => _ (* 2 *)
 end

こう書けばオーケーです。一番目の空白は ``n`` が ``O`` だと分かっているので、
``P O`` の型を持つ値で埋めればよく、二番目の空白は ``n`` が ``S np`` だと
分かっているので、\ ``P (S np)`` で埋めればよいのです。

実のところ、どこの部分を場合分けして、どの部分を場合分けしないかの選択が
より複雑な証明では必要になります。例えば ``Q n n`` の一番目の所だけ場合分け
したいなど。

::

 match n as n' return Q n' n with
 | O => _ (* 1 *)
 | S np => _ (* 2 *)
 end

こういう時は ``n`` が、こういう引数 ``n'`` として場合分けされて、返される型は
``Q n' n`` として返されますよ、って書けばよいのです。一番目の空白は ``Q O n``
で、二番目の空白は ``Q (S np) n`` です。

くだんの ``A -> Type`` が帰納的に定義されている場合もあります。

::

 Inductive Vec (A : Type) : nat -> Type :=
 | Nil : Vec A O
 | Cons : forall n, A -> Vec A n -> Vec A (S n)
 .

この型が引数になっている場合のパターンマッチングはどうすればいいんでしょうか。
つまり ``forall n, Vec A n -> B n`` みたいな場合です。ここでは ``Vec A n`` の
``n`` に当たるのが、その直前で全称量化により導入された ``n`` なので簡単です。

::

 fun (n : nat) (x : Vec A n) =>
  match x in Vec _ n' return B n' with
  | Nil => _ (* 1 *)
  | Cons a xs => _ (* 2 *)
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
  | Nil => _ (* 1 *)
  | Cons a xs => _ (* 1 *)
  end

この時 ``_1 : B 0``, ``_2 : B (S np)`` です。いざ、条件 ``H : P n`` を使って
証明しようとしたら、例えば一番目の場合では ``P 0`` であることが分かりません。
こういった困った事態は、このようにして回避できます。

::

 fun (n : nat) (H' : P n) (x : Vec A n) =>
  match x in Vec a n' return P n' -> B n' with
  | Nil => fun (H : P 0) => _ (* 1 *)
  | Cons a xs => fun (H : P (S np)) => _ (* 2 *)
  end H'

このように ``H`` をパターンマッチングに巻き込むことでできます。

なら ``Vec A 1 -> B`` は？一般化された ``forall n, n = 1 -> Vec A n -> B`` に
変換した後にパターンマッチングすればオーケーです。上でのパターンマッチングで
いう ``B n`` は、この時 ``n = 1 -> B`` です。

なら ``forall m n, Vec A (m + n) -> B m n`` は？新しい引数 ``o`` を取って、
``forall o, Vec A o -> forall m n, o = m + n -> B m n`` とすればいいのです。
上でのパターンマッチングでいう ``B o`` ( ``o`` は上での ``n`` と考える）は、
この時 ``forall m n, o = m + n -> B m n`` です。

これらを統一的に取り扱う方法はないのでしょうか？

もう一つ ``forall n, P n -> Vec A n -> B n`` を ``n : nat`` と ``H : P n`` まで
``intro`` した後に「あ！やりすぎた！」ということはないでしょうか？本来は上の
ように変形すべきだったというときです。これが十数個の引数とそれらの依存関係と
合わさると活力が奪われること間違いなしです。

こういう時、そのまま簡単に進む方法はないのでしょうか？

*********
inversion
*********

``inversion`` タクティックは非自明な動きをするタクティックとして槍玉に
あげられています。（調査対象：自分）。

しかし ``simpl`` タクティックや ``intro`` タクティック、\ ``subst`` 
タクティック、\ ``discriminate`` タクティックを勝手に適用することを除けば、
基本的なアイデアは先ほど書いたものだけです。

即ち、ゴール ``Vec A M -> B`` があったとき、ゴールを
``forall n, Vec A n -> n = M -> B`` と書き換えてやり、普通の
パターンマッチングを行うことです。その結果、ゴールは二つのゴールに分かれ、
``0 = M -> B`` と ``forall np, A -> Vec A np -> S np = M -> B`` となります。

例えば ``forall m n, Vec A m -> Vec A n -> Vec A (m + n)`` を定義することを
考えましょう。まずは "intro" しましょう。

.. code-block:: none

 A : Type
 m : nat
 n : nat
 x : Vec A m
 y : Vec A n
 ------------------------
 Vec A (m + n)

この時 "inversion" によって以下の場合に分けることが出来ます。

.. code-block:: none

 A : Type
 m : nat
 n : nat
 y : Vec A n
 H : 0 = m
 ------------------------
 Vec A (m + n)
