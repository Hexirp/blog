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

パターンマッチは構築子が持つ引数を束縛しなければならない。

::

  (** 前の自然数を返す。 *)
  Definition pred : nat -> nat :=
    fun n : nat =>
      match n with
      | O => O
      | S np => np
  .
