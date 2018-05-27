#################################
Coqの依存型に対するパターンマッチ
#################################

.. highlight:: coq

パターンマッチはHaskellでお馴染みの機能である。
Coqにもあるのだが、私は依存型が絡むときのパターンマッチをなかなか理解できなかった。
一度ここにメモすることで分かったことを整理したいと思う。

********************
普通のパターンマッチ
********************

まずは単純な例を挙げる。

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

ここで、 ``S`` は引数を持っている。

::

  (** 自然数を表す。 *)
  Inductive nat : Type :=
  | O : nat
  | S : nat -> nat
  .

構築子が持つ引数、ここでは [S np] の [np] に名前を付けることが出来る。

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

ここで、 ``prod`` は引数を持っている。

::

  (** 直積を表す。 *)
  Inductive prod (A : Type) (B : Type) : Type :=
  | pair : A -> B -> prod A B
  .

``pair`` はそれ自身が持っている引数に加えて、それが生成する型の引数も持つことになる。

::

  (** [pair] の型を確かめる。 *)
  Check pair : forall A B : Type, A -> B -> prod A B.

また、このように定義された ``prod'`` も引数を持っている。

::

  (** 直積を表す。 もう一つの [prod] である。 *)
  Inductive prod' : Type -> Type -> Type :=
  | pair' : forall A B : Type, A -> B -> prod' A B
  .

この二つの表記には、パターンマッチの際に関する違いがある。
