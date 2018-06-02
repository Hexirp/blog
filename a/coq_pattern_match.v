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

Inductive ex (A : Type) (P : A -> Type) : Type :=
| ex_pair : forall a : A, P a -> ex A P
.

Definition ex_swap (A B : Type) (P : A -> B -> Type) :
  ex A (fun a => ex B (fun b => P a b)) ->
  ex B (fun b => ex A (fun a => P a b))
:=
 fun x =>
  match x with
  | ex_pair _ _ a aH =>
   match aH with
   | ex_pair _ _ b bH => ex_pair _ _ b (ex_pair _ _ a bH)
   end
  end
.
