#################################
Coqの依存型に対するパターンマッチ
#################################

.. highlight:: coq

パターンマッチはHaskellでお馴染みの機能であり、Coqにも存在する。
また、Coqには依存型がありパターンマッチの時にも関わってくる。
しかし、それがどういう法則で動くのかをなかなか理解できなかった。
ここにメモすることで分かったことを整理したいと思う。

********************
普通のパターンマッチ
********************

まずは単純な例から始める。

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

このようにしてパターンマッチを行うことが出来る。

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

``S`` は引数を持っている。

::

  (** 自然数を表す。 *)
  Inductive nat : Type :=
  | O : nat
  | S : nat -> nat
  .

構築子が持つ引数（ここでは ``S np`` の ``np`` ）には
パターンマッチの時に名前を付けることが出来る。

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

``prod`` は引数を持っている。

::

  (** 直積を表す。 *)
  Inductive prod (A : Type) (B : Type) : Type :=
  | pair : A -> B -> prod A B
  .

``pair`` は生成する型の引数も元々の引数に加えて持つことになる。
``pair A B x y`` のようになり、 ``A`` と ``B`` が型の引数で
``x`` と ``y`` が元々の引数である。

::

  (** [pair] の型を確かめる。 *)
  Check pair : forall A B : Type, A -> B -> prod A B.

このようにしてパターンマッチできる。

::

  (** 直積の左右を入れ替える。 *)
  Definition swap : forall A B : Type, prod A B -> prod B A :=
    fun A B : Type =>
      fun x : prod A B =>
        match x with
        | pair _ _ x1 x2 => pair B A x2 x1
        end
  .

型の引数から引き継がれた構築子の引数（ここでは ``pair A B x y`` の ``A`` と ``B`` ）には
パターンマッチの時に名前を付けては **いけない** 。

::

  (** [swap] の間違った定義。 *)
  Fail Definition swap' : forall A B : Type, prod A B -> prod B A :=
    fun A B : Type :=
      fun x : prod A B =>
        match x with
        | pair xB xA x1 x2 => pair B A x2 x1
        end
  .

この理由は記事の本題に関するものなので後で説明する。
