#################################
Coqの依存型に対するパターンマッチ
#################################

.. highlight:: coq

パターンマッチは代数的データ型に付随する機能であり、Haskellでお馴染みです。
Haskell使いの私には嬉しいことにCoqも代数的データ型を持っていますので、
もちろんパターンマッチも存在します。
さらに、Coqには数学的な証明を記述するためのもっと強力な型システムがあり、
依存型や一般化された代数的データ型を含みます。
もちろん、依存型や一般化された代数的データ型へのパターンマッチも可能です。
しかし、それがどういう型付けをされるのか私はなかなか理解できませんでした。
ここにメモすることで分かったことを整理していきたいと思います。

********************
普通のパターンマッチ
********************

まずは単純な例です。
いわゆる列挙型と呼ばれるもので、ほとんどのプログラミング言語にあります。

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

普通にパターンマッチできます。

::

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

****************
引数を持つ構築子
****************

ここで、 ``S`` は値を受け取るように定義されています。
すると、パターンマッチする時に包まれた値を取り出すことが出来ます。

::

 (** 自然数を表す。 *)
 Inductive nat : Type :=
 | O : nat
 | S : nat -> nat
 .

取り出した値（ここでは ``S np`` の ``np`` ）には名前を付けることが出来ます。
そして、パターンの右の式で使うことが出来ます。

::

 (** 前の自然数を返す。 *)
 Definition pred : nat -> nat :=
  fun n : nat =>
   match n with
   | O => O
   | S np => np
   end
 .

********************
引数を持つ型の構築子
********************

ここで、 ``prod`` は引数を持っています。
すると、その構築子である ``pair`` も ``prod`` の引数を受け取るようになります。

::

 (** 直積を表す。 *)
 Inductive prod (A : Type) (B : Type) : Type :=
 | pair : A -> B -> prod A B
 .


``pair`` の型を見てみると分かりますが、全称量化が含まれています。
そのため、 何を渡すかで型が変わるちょっとした依存型になっています。
型の引数を全て渡してしまえば ``pair A B : A -> B -> pair A B`` のようになり、
定義した元々の型と同じになります。

::

 (** [pair] の型を確かめる。 *)
 Check pair : forall A B : Type, A -> B -> prod A B.

このようにしてパターンマッチできます。

::

 (** 直積の左右を入れ替える。 *)
 Definition swap : forall A B : Type, prod A B -> prod B A :=
  fun A B : Type =>
   fun x : prod A B =>
    match x with
    | pair _ _ x1 x2 => pair B A x2 x1
    end
 .

構築子が受け取る型の引数（ここでは ``pair A B x y`` の ``A`` と ``B`` ）には
パターンマッチの時に名前を **付けられません** 。

::

 (** [swap] の間違った定義。 *)
 Fail Definition swap' : forall A B : Type, prod A B -> prod B A :=
  fun A B : Type :=
   fun x : prod A B =>
    match x with
    | pair xB xA x1 x2 => pair B A x2 x1
    end
 .

名前が付けられなくても
型から ``A`` と ``B`` が渡されていることが分かっているので問題はありません。
より詳細は後で説明します。
