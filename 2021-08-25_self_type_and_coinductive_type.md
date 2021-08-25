# 自分型と余帰納型

自分型を使って余帰納型をエンコーディングします。

## 2021-08-18

`(P -> A) -> (P -> P) -> P -> Stream A`

self-typing で coinductive type を表現する方法は？

```
P : Stream A -> Type
case_constructor : pi (x_p : A) (x_s : Stream A), P x_s -> P (constructor A x_p x_s)
 - - - - -
P x
```

これじゃ、矛盾を生成できてしまう。なぜならば、 `P _ = Void` の時に……

```
P : Stream A -> Type
case_constructor : pi (x_p : A) (x_s : Stream A), P x_s /\ P (constructor A x_p x_s)
 - - - - -
P x
```

このようにしてみたらどうだろうか？ `_ -> _` と `_ /\ _` は双対だから良いかもしれない。

## 2021-08-24

```
P : Stream A -> Type
case_constructor : pi (x_p : A) (x_s : Stream A), P x_s /\ P (constructor A x_p x_s)
 - - - - -
P x
```

ここで、依存性を取り除くと……

```
P : Type
case_constructor : A -> Stream A -> P /\ P
 - - - - -
P
```

こうなって……

はい、だめ。


こうなったら、全部ひっくり返して

```
P : Stream A -> Type
case_constructor : sigma (x_p : A) (x_s : Stream A), P x_s /\ P (constructor A x_p x_s)
 - - - - -
P x
```

これはどうですかね？

だめっすね

```
P : Stream A -> Type
case_constructor : P x_s -> sigma (x_p : A) (x_s : Stream A), P (constructor A x_p x_s)
 - - - - -
P x
```

これは？

だめです

```
P : Stream A -> Type
case_constructor : P (constructor A x_p x_s) -> sigma (x_p : A) (x_s : Stream A), P x_s
 - - - - -
P x
```

これは？

良い線を行ってそうですね　これは……


何をやりたいのか？

self-typing で coinductive type を実装する。


```
P : Stream A -> Type
case_constructor : pi (x_p : A) (x_s : Stream A), P x_s -> P (constructor A x_p x_s)
 - - - - -
P x
```

これでは全然ダメである。

```
P x
 - - - - -
P : Stream A -> Type
case_constructor : pi (x_p : A) (x_s : Stream A), P x_s -> P (constructor A x_p x_s)
```

はい、だめ。

```
P : Stream A -> Type
P x
 - - - - -
case_constructor : pi (x_p : A) (x_s : Stream A), P x_s -> P (constructor A x_p x_s)
```

だめ。

うーん……

```
P : Natural_Number -> Type
construct_zero : P zero
construct_successor : pi n_p : Natural_Number, P n_p -> P (successor n_p)
 - - - - -
P x
```

これは self type を使った時の普通……

```
P : Natural_Number -> Type
construct_zero : P zero
construct_successor : pi n_p : Natural_Number, P n_p -> P (successor n_p)
 - - - - -
pi x : Natural_Number, P x
```

これは普通の帰納法……

この `pi x : Natural_Number, P x` のところが大事なんだよね……

```
P : Type
destruct_head : P -> A
destruct_tail : P -> P
 - - - - -
P -> Stream A
```

ここの P が依存型にすることが出来ないから問題なんだよね……

前は依存型の圏論的双対だったり `P : Type -> Stream A` だったりに活路を求めたけど……それでもダメだったんだよね。依存型の圏論的双対は何か空虚なものになるみたいだしね……

`sigma x : Stream A, P x` とかかな……

でも、これは上手く行かなさそうだし……

問題は、 `P -> Stream A` の `Stream A` が共変の位置にあることなんだよね……

`Stream A -> P` だったら簡単に依存積に変換できるのに……

`sigma x : Stream A, P x`

`~ P \/ Stream A`

あー、どれもダメだなあ

シークエント計算の右辺で `x : A, B x` というのが書けないかな？ これは or になるんだよね？

共変と反変といえば……

`(lambda A : Type => A -> Type) : Type -> Type` は、反変なのにモナドになるんだよね

その理屈は…… `lambda (f : A -> P B) (g : B -> P C) => lambda (x : A) (z : C) => sigma y : B, f x y /\ f y z` ということ。

関係の合成と同じ。

依存和は共変と反変をひっくりかえす働きを持つ。

となるなら……

`P -> Stream A` は……

`(lambda (f : A -> B) (x : A) (y : B) => Path (f x) y) : (A -> B) -> (A -> B -> Type)` ということで……

```
P : Stream A -> Type
destruct_head : pi x : Stream A, P x -> A
destruct_tail : pi x : Stream A, P x -> P (tail x)
 - - - - -
sigma x : Stream A, P x
```

いやいや、これじゃだめだ！

`destruct_head` が、なにかおかしい。

それに、 self-type で表現できない。

```
P : Type
Q : Type -> Type -> Type
destruct_head : P -> A
destruct_tail : P -> P
 - - - - -
Q P (Stream A)
```

これもダメだ。

```
P : Type
destruct_head : P -> A
destruct_tail : P -> P
 - - - - -
P -> Stream A -> Type
```

これかな？

これだな？

結末の値に `(A -> B) -> (A -> B -> Type)` を適用した

そんで……

```
P : Stream A -> Type
destruct_head : P -> A
destruct_tail : P -> P
 - - - - -
pi x : Stream A, P x -> Type
```

こんな感じに依存型化する。

`destruct` が依存型化できないな……

```
P : Type
destruct_head : P -> A -> Type
destruct_tail : P -> P -> Type
 - - - - -
P -> Stream A -> Type
```

こうして……

```
P : Stream A -> Type
destruct_head : pi x : Stream A, P x -> A -> Type
destruct_tail : pi x : Stream A, P x -> P (tail x) -> Type
 - - - - -
pi x : Stream A, P x -> Type
```

こうすると。

そんで……

```
P : Stream A -> Type
destruct_head : pi x : Stream A, P x -> A -> Type
destruct_tail : pi x : Stream A, P x -> P (tail x) -> Type
 - - - - -
P x -> Type
```

self-type 化して、こうすると。

これらの関数が持つべき性質を考察してみよう。

`P A = A -> Type` として、 `P` をモナドとして捉えて、この先は `map` などを使うことにしよう。

```
Stream.corecursion : pi A : Type, pi P : Type, (P -> A -> Type) -> (P -> P -> Type) -> (P -> Stream A -> Type)

map Stream.head (Stream.corecursion A P destruct_head destruct_tail x) = destruct_head x
map Stream.tail (Stream.corecursion A P destruct_head destruct_tail x) = Stream.corecursion A P destruct_head destruct_tail (destruct_tail x)

Stream.coinduction : pi A : Type, pi P : Stream A -> Type, (pi x : Stream A, P x -> A -> Type) -> (pi x : Stream A, P x -> P (tail x) -> Type) -> (pi x : Stream A, P x -> Type)
```

……うーん？

なんか上手く行かないなあ？

色々と大変なことになってるもんなあ。

```
Stream.corecursion A P destruct_head destruct_tail x y = Path (A -> Type) (destruct_head x) (pure (head y)) /\ Stream.corecursion A P destruct_head destruct_tail (destruct_tail x) (tail y)

Stream.coinduction A P destruct_head destruct_tail y x = Path (A -> Type) (destruct_head y x) (pure (head y)) /\ Stream.coinduction A P destruct_head destruct_tail (tail y) (destruct_tail y x)
```

こうなるのかあ……

つまり…… `Stream.coinduction` は…… `(a : A) -> B a -> Type` という関係で…… `B a` から `A` への関数を表現しているという……訳が分からないものを返すのか。

## 2021-08-25

あ、

`sigma y : Stream A, P y` を展開して

`pi Q : Type, (pi (y : Stream A) (x : P y), Q) -> Q` にして

`pi Q : Type, (P y -> Q) -> Q` にすれば良かったのかな？

いや、よくはないかな？

でも、よさそう
