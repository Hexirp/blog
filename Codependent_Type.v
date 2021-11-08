(** 余帰納法の原理を証明します。 *)

Declare ML Module "ltac_plugin".

(** [ltac_plugin] を読み込みます。 *)

Declare ML Module "ssrmatching_plugin".

(** [ssrmatching_plugin] を読み込みます。 *)

Declare ML Module "ssreflect_plugin".

(** [ssreflect_plugin] を読み込みます。 *)

Global Set Default Proof Mode "Classic".

(** 対話的な証明を行う時のモードを [Classic] にセットします。 *)

Global Set Default Goal Selector "all".

(** ゴールのセレクタのデフォルトを [all] にします。 *)

Global Unset Elimination Schemes.

(** [Elimination Schemes] をオフにします。 *)

Global Set Universe Polymorphism.

(** [Universe Polymorphism] をオンにします。 *)

Global Set Polymorphic Inductive Cumulativity.

(** [Polymorphic Inductive Cumulativity] をオンにします。 *)

Inductive Unit@{i | } : Type@{i} := Unit_value : Unit.

(** 単一型を定義します。 *)

Definition
  Unit_induction@{i j | }
      (P : Unit@{i} -> Type@{j})
      (constructor_value : P Unit_value@{i})
      (x : Unit@{i})
    : P x
:=
  match x as x_ return P x_ with Unit_value => constructor_value end
.

(** 単一型の帰納法の原理を定義します。 *)

Inductive Void@{i | } : Type@{i} :=.

(** 虚空型を定義します。 *)

Definition
  Void_induction@{i j | }
      (P : Void@{i} -> Type@{j})
      (x : Void@{i})
    : P x
:=
  match x as x_ return P x_ with end
.

(** 虚空型の帰納法の原理を定義します。 *)

Inductive
  Product@{i | } (A : Type@{i}) (B : Type@{i}) : Type@{i}
:=
  Product_pair : A -> B -> Product A B
.

(** 直積型を定義します。 *)

Definition
  Product_induction@{i j | }
      (A : Type@{i})
      (B : Type@{i})
      (P : Product@{i} A B -> Type@{j})
      (
        constructor_pair
          :
            forall (x_0 : A) (x_1 : B), P (Product_pair@{i} A B x_0 x_1)
      )
      (x : Product@{i} A B)
    : P x
:=
  match x as x_ return P x_ with
    Product_pair _ _ x_0 x_1 => constructor_pair x_0 x_1
  end
.

(** 直積型の帰納法の原理を定義します。 *)

Definition
  Product_first@{i | } (A : Type@{i}) (B : Type@{i}) (x : Product@{i} A B) : A
:=
  Product_induction@{i i}
    A
    B
    (fun _ : Product@{i} A B => A)
    (fun (x_0 : A) (_ : B) => x_0)
    x
.

Definition
  Product_second@{i | } (A : Type@{i}) (B : Type@{i}) (x : Product@{i} A B) : B
:=
  Product_induction@{i i}
    A
    B
    (fun _ : Product@{i} A B => B)
    (fun (_ : A) (x_1 : B) => x_1)
    x
.

Inductive
  Dependent_Sum@{i j | } (A : Type@{i}) (B : A -> Type@{j}) : Type@{max(i,j)}
:=
  Dependent_Sum_pair : forall x_0 : A, B x_0 -> Dependent_Sum A B
.

Definition
  Dependent_Sum_induction@{i j k | }
      (A : Type@{i})
      (B : A -> Type@{j})
      (P : Dependent_Sum@{i j} A B -> Type@{k})
      (
        constructor_pair
          :
            forall (x_0 : A) (x_1 : B x_0),
              P (Dependent_Sum_pair@{i j} A B x_0 x_1)
      )
      (x : Dependent_Sum@{i j} A B)
    : P x
:=
  match x as x_ return P x_ with
    Dependent_Sum_pair _ _ x_0 x_1 => constructor_pair x_0 x_1
  end
.

Definition
  Dependent_Sum_first@{i j | }
      (A : Type@{i})
      (B : A -> Type@{j})
      (x : Dependent_Sum@{i j} A B)
    : A
:=
  Dependent_Sum_induction@{i j i}
    A
    B
    (fun _ : Dependent_Sum@{i j} A B => A)
    (fun (x_0 : A) (_ : B x_0) => x_0)
    x
.

Definition
  Dependent_Sum_second@{i j | }
      (A : Type@{i})
      (B : A -> Type@{j})
      (x : Dependent_Sum@{i j} A B)
    : B (Dependent_Sum_first@{i j} A B x)
:=
  Dependent_Sum_induction@{i j j}
    A
    B
    (
      fun x_ : Dependent_Sum@{i j} A B =>
        B (Dependent_Sum_first@{i j} A B x_)
    )
    (fun (x_0 : A) (x_1 : B x_0) => x_1)
    x
.

Definition
  Dependent_Product@{i j | } (A : Type@{i}) (B : A -> Type@{j})
    : Type@{max(i,j)}
:=
  forall x : A, B x
.

Definition
  Function@{i | } (A : Type@{i}) (B : Type@{i}) : Type@{i}
:=
  A -> B
.

Definition
  Dependent_Function@{i j | } (A : Type@{i}) (B : A -> Type@{j})
    : Type@{max(i,j)}
:=
  forall x : A, B x
.

Inductive
  Path@{i | } (A : Type@{i}) (a : A) : A -> Type@{i}
:=
  Path_id : Path A a a
.

Definition
  Path_induction@{i j | }
      (A : Type@{i})
      (a : A)
      (P : forall e : A, Path@{i} A a e -> Type@{j})
      (constructor_id : P a (Path_id@{i} A a))
      (e : A)
      (x : Path@{i} A a e)
    : P e x
:=
  match x as x_ in Path _ _ e_ return P e_ x_ with
    Path_id _ _ => constructor_id
  end
.

Definition
  Dependent_Path@{i j | }
      (A : Type@{i})
      (P : A -> Type@{j})
      (a : A)
      (e : A)
      (p : Path@{i} A a e)
      (b : P a)
      (o : P e)
    : Type@{j}
:=
  Path@{j}
    (P e)
    (
      Path_induction@{i j}
        A
        a
        (fun (e_ : A) (_ : Path@{i} A a e_) => P e_)
        b
        e
        p
    )
    o
.

Definition
  Dependent_Path_ap@{i j | }
      (A : Type@{i})
      (P : A -> Type@{j})
      (a : A)
      (e : A)
      (f : forall x : A, P x)
      (p : Path@{i} A a e)
    :
      Dependent_Path@{i j}
        A
        P
        a
        e
        p
        (f a)
        (f e)
:=
  Path_induction@{i j}
    A
    a
    (
      fun (e_ : A) (p_ : Path@{i} A a e_) =>
        Dependent_Path@{i j}
          A
          P
          a
          e_
          p_
          (f a)
          (f e_)
    )
    (Path_id@{j} (P a) (f a))
    e
    p
.

Definition Universe@{i s_i | i < s_i} : Type@{s_i} := Type@{i}.

Definition lift@{i s_i | i < s_i} (A : Type@{i}) : Type@{s_i} := A.

(* ここから依存型の定義をする。 *)

Definition
  Dependent_Type@{i s_i | i < s_i} (A : Type@{i}) : Type@{s_i}
:=
  A -> Type@{i}
.

Definition
  Dependent_Type_Wrapper@{i s_i | i < s_i}
      (A : Type@{i})
      (P : Dependent_Type@{i s_i} A)
    : Type@{s_i}
:=
  Dependent_Sum@{i i} A P
.

Definition
  display@{i s_i | i < s_i}
      (A : Type@{i})
      (P : Dependent_Type@{i s_i} A)
      (x : Dependent_Type_Wrapper@{i s_i} A P)
    : A
:=
  Dependent_Sum_first@{i i} A P x
.

Definition
  Dependent_Type_map@{i s_i | i < s_i}
      (A : Type@{i})
      (X : Type@{i})
      (f : X -> A)
      (P : Dependent_Type@{i s_i} A)
    : Dependent_Type@{i s_i} X
:=
  fun x : X => P (f x)
.

Definition
  substitute@{i s_i | i < s_i}
      (A : Type@{i})
      (P : Dependent_Type@{i s_i} A)
      (X : Type@{i})
      (f : X -> A)
      (
        x
          :
            Dependent_Type_Wrapper@{i s_i}
              X
              (Dependent_Type_map@{i s_i} A X f P)
      )
    : Dependent_Type_Wrapper@{i s_i} A P
:=
  Dependent_Sum_induction@{i i s_i}
    X
    (Dependent_Type_map@{i s_i} A X f P)
    (
      fun
        _
          :
            Dependent_Sum@{i i}
              X
              (Dependent_Type_map@{i s_i} A X f P)
      =>
        Dependent_Type_Wrapper@{i s_i} A P
    )
    (
      fun
        (x_0 : X)
        (x_1 : Dependent_Type_map@{i s_i} A X f P x_0)
      =>
        Dependent_Sum_pair@{i i} A P (f x_0) (x_1)
    )
    x
.

(* 余依存型の定義に必要なので押し出しの定義をする。 *)

Axiom
  Push_out@{i | }
    :
      forall
        (A : Type@{i})
        (B : Type@{i})
        (C : Type@{i})
      ,
        (A -> B) -> (A -> C) -> Type@{i}
.

Axiom
  Push_out_left@{i | }
    :
      forall
        (A : Type@{i})
        (B : Type@{i})
        (C : Type@{i})
        (f : A -> B)
        (g : A -> C)
      ,
        B -> Push_out@{i} A B C f g
.

Axiom
  Push_out_right@{i | }
    :
      forall
        (A : Type@{i})
        (B : Type@{i})
        (C : Type@{i})
        (f : A -> B)
        (g : A -> C)
      ,
        C -> Push_out@{i} A B C f g
.

Axiom
  Push_out_path@{i | }
    :
      forall
        (A : Type@{i})
        (B : Type@{i})
        (C : Type@{i})
        (f : A -> B)
        (g : A -> C)
        (x_a : A)
      ,
        Path@{i}
          (Push_out@{i} A B C f g)
          (Push_out_left@{i} A B C f g (f x_a))
          (Push_out_right@{i} A B C f g (g x_a))
.

Axiom
  Push_out_induction@{i j | }
    :
      forall
        (A : Type@{i})
        (B : Type@{i})
        (C : Type@{i})
        (f : A -> B)
        (g : A -> C)
        (P : Push_out@{i} A B C f g -> Type@{j})
        (
          constructor_left
            :
              forall x_b : B, P (Push_out_left@{i} A B C f g x_b)
        )
        (
          constructor_right
            :
              forall x_c : C, P (Push_out_right@{i} A B C f g x_c)
        )
        (
          constructor_path
            :
              forall x_a : A,
                Dependent_Path@{i j}
                  (Push_out@{i} A B C f g)
                  P
                  (Push_out_left@{i} A B C f g (f x_a))
                  (Push_out_right@{i} A B C f g (g x_a))
                  (Push_out_path@{i} A B C f g x_a)
                  (constructor_left (f x_a))
                  (constructor_right (g x_a))
        )
        (x : Push_out@{i} A B C f g)
      ,
        P x
.

Axiom
  Push_out_induction_beta_left@{i j | }
    :
      forall
        (A : Type@{i})
        (B : Type@{i})
        (C : Type@{i})
        (f : A -> B)
        (g : A -> C)
        (P : Push_out@{i} A B C f g -> Type@{j})
        (
          constructor_left
            :
              forall x_b : B, P (Push_out_left@{i} A B C f g x_b)
        )
        (
          constructor_right
            :
              forall x_c : C, P (Push_out_right@{i} A B C f g x_c)
        )
        (
          constructor_path
            :
              forall x_a : A,
                Dependent_Path@{i j}
                  (Push_out@{i} A B C f g)
                  P
                  (Push_out_left@{i} A B C f g (f x_a))
                  (Push_out_right@{i} A B C f g (g x_a))
                  (Push_out_path@{i} A B C f g x_a)
                  (constructor_left (f x_a))
                  (constructor_right (g x_a))
        )
        (x_b : B)
      ,
        Path@{j}
          (P (Push_out_left@{i} A B C f g x_b))
          (
            Push_out_induction@{i j}
              A
              B
              C
              f
              g
              P
              constructor_left
              constructor_right
              constructor_path
              (Push_out_left@{i} A B C f g x_b)
          )
          (constructor_left x_b)
.

Axiom
  Push_out_induction_beta_right@{i j | }
    :
      forall
        (A : Type@{i})
        (B : Type@{i})
        (C : Type@{i})
        (f : A -> B)
        (g : A -> C)
        (P : Push_out@{i} A B C f g -> Type@{j})
        (
          constructor_left
            :
              forall x_b : B, P (Push_out_left@{i} A B C f g x_b)
        )
        (
          constructor_right
            :
              forall x_c : C, P (Push_out_right@{i} A B C f g x_c)
        )
        (
          constructor_path
            :
              forall x_a : A,
                Dependent_Path@{i j}
                  (Push_out@{i} A B C f g)
                  P
                  (Push_out_left@{i} A B C f g (f x_a))
                  (Push_out_right@{i} A B C f g (g x_a))
                  (Push_out_path@{i} A B C f g x_a)
                  (constructor_left (f x_a))
                  (constructor_right (g x_a))
        )
        (x_c : C)
      ,
        Path@{j}
          (P (Push_out_right@{i} A B C f g x_c))
          (
            Push_out_induction@{i j}
              A
              B
              C
              f
              g
              P
              constructor_left
              constructor_right
              constructor_path
              (Push_out_right@{i} A B C f g x_c)
          )
          (constructor_right x_c)
.

Axiom
  Push_out_induction_beta_path@{i j | }
    :
      forall
        (A : Type@{i})
        (B : Type@{i})
        (C : Type@{i})
        (f : A -> B)
        (g : A -> C)
        (P : Push_out@{i} A B C f g -> Type@{j})
        (
          constructor_left
            :
              forall x_b : B, P (Push_out_left@{i} A B C f g x_b)
        )
        (
          constructor_right
            :
              forall x_c : C, P (Push_out_right@{i} A B C f g x_c)
        )
        (
          constructor_path
            :
              forall x_a : A,
                Dependent_Path@{i j}
                  (Push_out@{i} A B C f g)
                  P
                  (Push_out_left@{i} A B C f g (f x_a))
                  (Push_out_right@{i} A B C f g (g x_a))
                  (Push_out_path@{i} A B C f g x_a)
                  (constructor_left (f x_a))
                  (constructor_right (g x_a))
        )
        (x_a : A)
      ,
        Path@{j}
          (
            Dependent_Path@{i j}
              (Push_out@{i} A B C f g)
              P
              (Push_out_left@{i} A B C f g (f x_a))
              (Push_out_right@{i} A B C f g (g x_a))
              (Push_out_path@{i} A B C f g x_a)
              (constructor_left (f x_a))
              (constructor_right (g x_a))
          )
          (
            Path_induction@{j j}
              (P (Push_out_left@{i} A B C f g (f x_a)))
              (
                Push_out_induction@{i j}
                  A
                  B
                  C
                  f
                  g
                  P
                  constructor_left
                  constructor_right
                  constructor_path
                  (Push_out_left@{i} A B C f g (f x_a))
              )
              (
                fun
                  (e_ : P (Push_out_left@{i} A B C f g (f x_a)))
                  (
                    _
                      :
                        Path@{j}
                          (P (Push_out_left@{i} A B C f g (f x_a)))
                          (
                            Push_out_induction@{i j}
                              A
                              B
                              C
                              f
                              g
                              P
                              constructor_left
                              constructor_right
                              constructor_path
                              (Push_out_left@{i} A B C f g (f x_a))
                          )
                          e_
                  )
                =>
                  Dependent_Path@{i j}
                    (Push_out@{i} A B C f g)
                    P
                    (Push_out_left@{i} A B C f g (f x_a))
                    (Push_out_right@{i} A B C f g (g x_a))
                    (Push_out_path@{i} A B C f g x_a)
                    e_
                    (constructor_right (g x_a))
              )
              (
                Path_induction@{j j}
                  (P (Push_out_right@{i} A B C f g (g x_a)))
                  (
                    Push_out_induction@{i j}
                      A
                      B
                      C
                      f
                      g
                      P
                      constructor_left
                      constructor_right
                      constructor_path
                      (Push_out_right@{i} A B C f g (g x_a))
                  )
                  (
                    fun
                      (e_ : P (Push_out_right@{i} A B C f g (g x_a)))
                      (
                        _
                          :
                            Path@{j}
                              (P (Push_out_right@{i} A B C f g (g x_a)))
                              (
                                Push_out_induction@{i j}
                                  A
                                  B
                                  C
                                  f
                                  g
                                  P
                                  constructor_left
                                  constructor_right
                                  constructor_path
                                  (Push_out_right@{i} A B C f g (g x_a))
                              )
                              e_
                      )
                    =>
                      Dependent_Path@{i j}
                        (Push_out@{i} A B C f g)
                        P
                        (Push_out_left@{i} A B C f g (f x_a))
                        (Push_out_right@{i} A B C f g (g x_a))
                        (Push_out_path@{i} A B C f g x_a)
                        (
                          Push_out_induction@{i j}
                            A
                            B
                            C
                            f
                            g
                            P
                            constructor_left
                            constructor_right
                            constructor_path
                            (Push_out_left@{i} A B C f g (f x_a))
                        )
                        e_
                  )
                  (
                    Dependent_Path_ap@{i j}
                      (Push_out@{i} A B C f g)
                      P
                      (Push_out_left@{i} A B C f g (f x_a))
                      (Push_out_right@{i} A B C f g (g x_a))
                      (
                        Push_out_induction@{i j}
                          A
                          B
                          C
                          f
                          g
                          P
                          constructor_left
                          constructor_right
                          constructor_path
                      )
                      (Push_out_path@{i} A B C f g x_a)
                  )
                  (constructor_right (g x_a))
                  (
                    Push_out_induction_beta_right@{i j}
                      A
                      B
                      C
                      f
                      g
                      P
                      constructor_left
                      constructor_right
                      constructor_path
                      (g x_a)
                  )
              )
              (constructor_left (f x_a))
              (
                Push_out_induction_beta_left@{i j}
                  A
                  B
                  C
                  f
                  g
                  P
                  constructor_left
                  constructor_right
                  constructor_path
                  (f x_a)
              )
          )
          (constructor_path x_a)
.
