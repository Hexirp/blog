##########################
Coq でのパターンマッチング
##########################

.. highlight:: coq

Coq は代数的データ型のようにして新しい型を定義でき、
さらに一般化代数的データ型 (GADTs) や依存型もモリモリ使える、という言語です。

それが複雑化して素朴に理解できないものになってしまっています。
この記事は、それらを理解したいがための場合分けと帰納法についてのメモです。
自分に対して書いているのか他人に対して書いているのかよく分からないので注意を。

********************
普通のパターンマッチ
********************

まずは、軽いジャブから行きましょう。これらは一般化データ型の範囲です。

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

Haskell に慣れている人なら、きっと何をやっているのか分かってくれるでしょう。

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

Haskell では型を明示的に渡すことはありませんが、
Coq では型を普通の値と同じように扱うことが出来るので、
パターンマッチングの時も型を気にかけないといけません。
``pair _ _ x1 x2`` と長々と書かないといけないってことです。

組み立てるときも同じことで ``pair A B x1 x2`` と書かないといけません。
もちろん Coq には Implicit arguments って仕組みがあって、
ここでいう型の部分とかの省略したいところを省略できる仕組みがあります。

要するに、Coq では型もパターンマッチングの時に取り出せる一つの値です。
とはいえ、ここは ``A``, ``B`` が ``prod`` の引数として明示されてるので、
自明に ``xB = B``, ``xA = A`` と分かるため「いらないよ！」ってエラーになります。

**************************
値に依存する型を持つ構築子
**************************

ここで、 ``ex_pair`` はその型に全称量化を使われているため
はっきりとした依存型です。
また、 ``ex`` 自体は依存型ではありませんが
その引数である ``P`` は型 ``A`` の値を受け取って型を返すため依存型です。

::

 Inductive ex (A : Type) (P : A -> Type) : Type :=
 | ex_pair : forall a : A, P a -> ex A P
 .

ここまで一度もCoqのタクティックを使っていないことに気が付いていますか？
それは今までやってきたことが定理証明よりも関数定義に近いからです。
ある定理を証明したい時はその証明方法は問題にならないことが多いのですが、
関数を定義したい場合は定義方法が問題になります。
例えば ``inversion`` タクティックを使って定義した関数が
どのような定義を持つのか予測できる人はほとんどいないでしょう。
それでも、関数定義においても有用であるのが ``refine`` です。

証明モードは ``Definition foo : Foo := _.`` のアンダースコア部を
様々なタクティックを使って徐々に組み立てていくものです。
``refine term`` は ``term`` をそのままアンダースコア部にあてはめます。
ただし、 ``term`` にはアンダースコアが含まれていてもよく、
その場合は次にそのアンダースコア部を組み立てていくことになります。

::

 Definition ex_swap : forall A B : Type, forall P : A -> B -> Type,
   ex A (fun a : A => ex B (fun b : B => P a b)) ->
   ex B (fun b : B => ex A (fun a : A => P a b)).
 Proof.
  refine (
   fun (A B : Type) => _
  ).
  refine (
   fun (P : A -> B -> Type) => _
  ).
  refine (
   fun (x : ex A (fun a : A => ex B (fun b : B => P a b))) => _
  ).
  refine (
   match x with
   | ex_pair _ _ a aH => _
   end
  ).
  refine (
   match aH with
   | ex_pair _ _ b bH => _
   end
  ).
  refine (
   ex_pair B (fun b : B => ex A (fun a : A => P a b)) b _
  ).
  refine (
   ex_pair A (fun a : A => P a b) a _
  ).
  refine (
   bH
  ).
 Defined.

``refine`` の後にアンダースコア部を埋めていくとき、
そこから見えるべき値が環境に追加されています。
ここの時が分かりやすいでしょう。

::

 refine (
  fun (A B : Type) => _
 ).

ここでは、最後に一つアンダースコアが含まれています。
ここからは ``A : Type`` と ``B : Type`` が見えるべきです。
そして、このアンダースコアが次に埋めていくべきもの、すなわちゴールです。
ゴールはアンダースコアだということが分かり切っているので、
その型だけが表示されます。

前：

.. code-block:: none

 1 subgoal
 ______________________________________(1/1)
 forall (A B : Type) (P : A -> B -> Type),
 ex A (fun a : A => ex B (fun b : B => P a b)) ->
 ex B (fun b : B => ex A (fun a : A => P a b))

後：

.. code-block:: none

 1 subgoal
 A : Type
 B : Type
 ______________________________________(1/1)
 forall P : A -> B -> Type,
 ex A (fun a : A => ex B (fun b : B => P a b)) ->
 ex B (fun b : B => ex A (fun a : A => P a b))

また、パターンマッチの時も同じです。取り出した値は見えるべきです。

::

 refine (
  match x with
  | ex_pair _ _ a aH => _
  end
 ).

ここでいえば、 ``a`` と ``aH`` は
右側のアンダースコア部から見えるべきだということになります。
名前が付けられないがゆえに置かれた左側のアンダースコアと混同しないように
気を付けてください。

前：

.. code-block:: none

 1 subgoal
 A : Type
 B : Type
 P : A -> B -> Type
 x : ex A (fun a : A => ex B (fun b : B => P a b))
 ______________________________________(1/1)
 ex B (fun b : B => ex A (fun a : A => P a b))

後：

.. code-block:: none

 1 subgoal
 A : Type
 B : Type
 P : A -> B -> Type
 x : ex A (fun a : A => ex B (fun b : B => P a b))
 a : A
 aH : ex B (fun b : B => P a b)
 ______________________________________(1/1)
 ex B (fun b : B => ex A (fun a0 : A => P a0 b))

``x`` へのパターンマッチで ``a`` と ``aH`` が取り出されました。
``aH`` の型は ``ex_pair`` の型通り ``a`` に依存しています。
