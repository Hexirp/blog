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
  fun A B : Type =>
    fun x : prod A B =>
      match x with
      | pair xB xA x1 x2 => pair B A x2 x1
      end
.
