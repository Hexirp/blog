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
  BB :: Bool -> B Bool
  BI :: Int -> B Int
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
