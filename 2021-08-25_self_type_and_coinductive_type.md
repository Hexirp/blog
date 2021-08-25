# 自分型と余帰納型

自分型を使って余帰納型をエンコーディングします。

## エンコーディング

帰納型は様々な方法でエンコーディングすることが出来ます。

### チャーチ・エンコーディング

チャーチ・エンコーディング (Church encoding) は、帰納型を**型無し**ラムダ計算にエンコーディングします。概略は [Wikipedia](https://en.wikipedia.org/wiki/Church_encoding) を見たら把握できると思います。

具体的に自然数をエンコーディングしてみます。

```
zero = λ x. λ f. x
successor = λ n. λ x. λ f. f (n x f)
```

### ベーム・ベラルドゥッチ・エンコーディング

ベーム・ベラルドゥッチ・エンコーディング (Böhm–Berarducci encoding) は、帰納型を System F にエンコーディングします。

System F は、次の特徴を持つラムダ計算の体系です。

* ランク n の全称量化が使える。
* 依存型はない。

チャーチ・エンコーディングに型をつけたやつだと思っていれば良いです。 Haskell の表記で書くと `[a]` が `forall r. a -> (a -> r -> r) -> r` になるやつです。 Haskell では計算量を改善したりモナド変換子化したりするときに使われるやつです。

具体的に自然数をエンコーディングしてみます。

```
Natural_Number = ∀ r. r -> (r -> r) -> r
zero = Λ r. λ x. λ f. x
successor = λ n. Λ r. λ x. λ f. f (n r x f)
```

「型が付いた！ これで勝ちだ！」という訳にはいきません。ベーム・ベラルドゥッチ・エンコーディングには次のような問題があります。

* 後者関数の計算量が `O(n)` になる。
  * 普通なら構築子を一枚だけ剥げばよいだけなので `O(1)` だよね。
* 帰納原理が使えない。

後者関数の計算量が `O(n)` になるのは、パリゴット・エンコーディングを使えば解決できます。

帰納原理が使えないのは、これが例え System F に依存型を追加した体系 (System F<sub>ω</sub> など) においてベーム・ベラルドゥッチ・エンコーディングを使っても、ペアノ算術でおなじみの `∀ P. P 0 -> (∀ n. P n -> P (n + 1)) -> ∀ n. P n` という数学的帰納法を依存型に適応したやつを証明できないということです。このことは証明されているので、ベーム・ベラルドゥッチ・エンコーディングを使う限りはどうしようもありません。

### パリゴット・エンコーディング

パリゴット・エンコーディング (Parigot encoding) は、チャーチ・エンコーディング（またはベーム・ベラルドゥッチ・エンコーディング）を少しだけ捻ったやつです。

具体的に自然数をエンコーディングしてみます。

```
Natural_Number = ∀ r. r -> (Natural_Number -> r -> r) -> r
zero = Λ r. λ x. λ f. x
successor = λ n. Λ r. λ x. λ f. f n (n r x f)
-- Natural_Number should be the least fixed point.
```

パリゴット・エンコーディングを使った自然数においては後者関数が `O(1)` で実装できることは明かでしょう。

パリゴット・エンコーディングは、そのままでは自然数 `n` のエンコーディングの結果の項の大きさのオーダーが `n` に関する指数関数になるという欠点を持っています。しかし、これは部分項を共有することが出来れば解決できます。部分項を共有したまま簡約できるような方法としては、最適簡約 (optimal reduction) などがあります。また、最近、私が注目している対称的相互作用計算 (symmetric interaction calculus) も使えるかもしれません。

### 帰納原理

帰納原理 (induction principle) は、それぞれの帰納型に対して存在する命題のことです。 Coq では、 `Inductive` コマンドを使って帰納型を定義するたびに Coq IDE の中で `***_ind is defined` というメッセージが出てくるのを見ることになります。この `***_ind is defined` が帰納原理というやつです。

たとえば、自然数に対する帰納原理を書き下すと、次のようになります。

```
∀ P : Natural_Number -> Type,
∀ construct_zero : P zero,
∀ construct_successor : ∀ n_p : Natural_Number, P n_p -> P (successor n_p),
(∀ x : Natural_Number, P x)
```

帰納原理は面白いことにパリゴット・エンコーディングを包摂しています。どういうことかというと、 `P _ = A` と定義した上で計算してみると、次のようになるのです。

```
∀ construct_zero : P zero,
∀ construct_successor : ∀ n_p : Natural_Number, P n_p -> P (successor n_p),
(∀ x : Natural_Number, P x)

∀ construct_zero : A     ,
∀ construct_successor : ∀ n_p : Natural_Number, A     -> A                ,
(∀ x : Natural_Number, A  )

∀ _              : A     ,
∀ _                   : ∀ _   : Natural_Number, A     -> A                ,
(∀ _ : Natural_Number, A  )

∀ _ : A,
∀ _ : ∀ _ : Natural_Number, A -> A,
(∀ _ : Natural_Number, A)

∀ _ : A,
∀ _ : Natural_Number -> A -> A,
(Natural_Number -> A)

A ->
(Natural_Number -> A -> A) ->
(Natural_Number -> A)

A -> (Natural_Number -> A -> A) -> (Natural_Number -> A)
```

このような帰納原理はベーム・ベラルドゥッチ・エンコーディングとも見た目が似ています。そのため、次のようにすればよいと思うかもしれません。

```
Natural_Number
  =
    ∀ P : Natural_Number -> Type,
    ∀ construct_zero : P zero,
    ∀ construct_successor : ∀ n_p : Natural_Number, P n_p -> P (successor n_p),
    (∀ x : Natural_Number, P x)
```

しかし、これがいろいろとおかしいことは明らかです。まず、

## 自分型とは

自分型 (self type) は、自分の型に自分自身が現れるような値を型付けします。

たとえば、ある値 `x` に対して `x : P x` という型付けが成り立つとしましょう。また、別の値に対して `y : P y` という型付けが成り立つとします。 `x` と `y` の両方に対して自分型を使って `ι t. P t` という型を与えることが出来ます。

自分型は `ι x. T` と書きます。 `T` の中では `x` を参照できます。

## なぜ自分型なのか

## 自分型で余帰納型をエンコーディングする

## 参考文献

1. [Self Types for Dependently Typed Lambda Encodings](https://dblp.org/rec/conf/rta/FuS14.html)
  1. [PDF version](https://homepage.divms.uiowa.edu/~astump/papers/fu-stump-rta-tlca-14.pdf)

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
