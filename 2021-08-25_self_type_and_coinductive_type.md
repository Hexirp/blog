# 自分型と余帰納型

自分型を使って余帰納型をエンコーディングします。

## 前提知識

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
Π P : Natural_Number -> Type.
Π construct_zero : P zero.
Π construct_successor : Π n_p : Natural_Number. P n_p -> P (successor n_p).
(Π x : Natural_Number. P x)
```

帰納原理は面白いことにパリゴット・エンコーディングを包摂しています。どういうことかというと、 `P _ := A` と定義した上で計算してみると、次のようになるのです。

```
Π construct_zero : P zero.
Π construct_successor : Π n_p : Natural_Number. P n_p -> P (successor n_p).
(Π x : Natural_Number. P x)

Π construct_zero : A     .
Π construct_successor : Π n_p : Natural_Number. A     -> A                .
(Π x : Natural_Number. A  )

Π _              : A     .
Π _                   : Π _   : Natural_Number. A     -> A                .
(Π _ : Natural_Number. A  )

Π _ : A.
Π _ : Π _ : Natural_Number. A -> A.
(Π _ : Natural_Number. A)

Π _ : A.
Π _ : Natural_Number -> A -> A.
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
    Π P : Natural_Number -> Type.
    Π construct_zero : P zero.
    Π construct_successor : Π n_p : Natural_Number. P n_p -> P (successor n_p).
    (Π x : Natural_Number. P x)
```

パリゴット・エンコーディングを含む上に帰納法の原理も直接的に導出できて良しです。しかし、これがいろいろとおかしいことは明らかです。 `Natural_Number` は ω 個の値を持たないといけませんが、（おそらくは）これでは 1 個の値だけになってしまいます。

帰納型を帰納原理つきで実装するには、どうすればよいのか。 Coq や Agda では、あるいは帰納的構築計算 (Calculus of Inductive Constructions) では直接的に体系へ追加する方法を使っています。

### 組み込みの帰納型

体系へ帰納型を組み込むためには、次の用意がなければなりません。

* 帰納型の定義の方法
* 構築子の定義の方法
* 帰納法を行う方法

Coq では、これらが直接的に用意されていて分かりやすいです。 `Inductive` コマンドで帰納型と構築子の定義を行うことが出来ます。そして、帰納法を `fix` 式で行うことが出来ます。この `fix` 式を使って帰納原理は定義されます。

しかし、この方法にはデメリットがあります。

* 項の構成要素が増える。
* 型付けの規則を上手く与えないと HoTT (Homotopy Type Theory) との非互換性を組み込んでしまったりする。
* 簡約の規則を上手く与えないと型の保存性 (type preservation) が簡単に失われる。

一方で、ベーム・ベラルドゥッチ・エンコーディングは、表現力には欠けるものの、全てを単純な構成要素で表現することが出来ます。これに自分型を組み合わせると、両方のメリットを取り入れることが出来るのです。

## 自分型とは

自分型 (self type) は、自分の型に自分自身が現れるような値を型付けします。

たとえば、ある値 `x` に対して `x : P x` という型付けが成り立つとしましょう。また、別の値に対して `y : P y` という型付けが成り立つとします。 `x` と `y` の両方に対して自分型を使って `ι t. P t` という型を与えることが出来ます。

自分型は `ι x. T` と書きます。 `T` の中では `x` を参照できます。型付けの規則は次のようになります。

```
Γ ⊢ t : T [ x := t ]
Γ ⊢ (ι x : T) : Type
 - - - - -             (SelfGen)
Γ ⊢ t : ι x : T

Γ ⊢ t : ι x : T
 - - - - -            (SelfInst)
Γ ⊢ t : T [ x := t ]
```

## 自分型で帰納型をエンコーディングする

自分型で帰納型をエンコーディングすることが出来ます。

```
Natural_Number
  =
    Π P : Natural_Number -> Type.
    Π construct_zero : P zero.
    Π construct_successor : Π n_p : Natural_Number. P n_p -> P (successor n_p).
    (Π x : Natural_Number. P x)
```

これは失敗したものです。

```
zero : Natural_Number
zero = λ r. λ x. λ f. x
```

しかし、無理矢理当てはめてみればどうでしょうか？ この `zero` に対して無理矢理型を押し付けてみると、どんなことが起きるでしょうか？

```
r := P : Natural_Number -> Type
x := construct_zero : P zero
f := construct_successor : Π n_p : Natural_Number. P n_p -> P (successor n_p)
```

まず、当然ながら上記のような型になります。そして、次のような不整合が発生します。

```
x : P zero
```

本来は、ここで `Π x_ : Natural_Number. P x_` を返さなければなりません。ですが、最後の型は `P zero` になってしまっています。

```
one : Natural_Number
one = λ r. λ x. λ f. f zero x
```

これにも同じようにしましょう。

```
r := P : Natural_Number -> Type
x := construct_zero : P zero
f := construct_successor : Π n_p : Natural_Number. P n_p -> P (successor n_p)
```

こうなります。そして、次のようになります。

```
f zero x : P (successor zero)
```

ここでも、本来は `Π x_ : Natural_Number. P x_` を返さなければなりません。しかし、 `P (successor zero)` になっている、つまり `P one` になってしまっています。

まとめましょう。

```
zero
  :
    Π P : Natural_Number -> Type.
    Π construct_zero : P zero.
    Π construct_successor : Π n_p : Natural_Number. P n_p -> P (successor n_p).
    P zero
one
  :
    Π P : Natural_Number -> Type.
    Π construct_zero : P zero.
    Π construct_successor : Π n_p : Natural_Number. P n_p -> P (successor n_p).
    P one
```

おや？ **自分自身**が型に現れていますね。

そう、自然数 `n` は次のように型付けすることが出来ます。

```
n
  :
    Π P : Natural_Number -> Type.
    Π construct_zero : P zero.
    Π construct_successor : Π n_p : Natural_Number. P n_p -> P (successor n_p).
    P n
```

これを自分型で閉じます。

```
x
  :
    ι n.
    Π P : Natural_Number -> Type.
    Π construct_zero : P zero.
    Π construct_successor : Π n_p : Natural_Number. P n_p -> P (successor n_p).
    P n
```

これが**自分型を使った自然数のエンコーディング**です。

```
Natural_Number
  =
    ι n.
    Π P : Natural_Number -> Type.
    Π construct_zero : P zero.
    Π construct_successor : Π n_p : Natural_Number. P n_p -> P (successor n_p).
    P n
zero = λ P. λ x. λ f. x
successor = λ n. λ P. λ x. λ f. f n (n P x f)
```

上記のようになります。

他の帰納型についても同じようなエンコーディングが可能です。詳細は論文を読むか [Yatima 言語](https://github.com/yatima-inc/yatima)で遊んで把握するかしてください。ちなみに、このヤチマは、あのグレッグ・イーガンの『ディアスポラ』の主人公たるヤチマに由来しています。

## 自分型で余帰納型をエンコーディングする

自分型で余帰納型をエンコーディングするには困難があります。

余帰納型の代表たる `Stream` 型を取り上げます。 `Stream` 型は `Π P : Type. (P -> A) -> (P -> P) -> (P -> Stream A)` という関数で特徴づけられています。帰納型の方では `Natural_Number -> P` が最終的に返すものでした。同じように余帰納型では `P -> Stream A` が最後に返すものです。矢印の方向が反対になっている訳ですね。これは帰納型と余帰納型が双対であるためです。

```
Π P : Type.
Π destruct_head : P -> A.
Π destruct_tail : P -> P.
(P -> Stream A)
```

## 参考文献

1. [Self Types for Dependently Typed Lambda Encodings](https://dblp.org/rec/conf/rta/FuS14.html)
  1. [PDF link](https://homepage.divms.uiowa.edu/~astump/papers/fu-stump-rta-tlca-14.pdf)
2. [Programming with Proofs: A Second Order Type Theory](https://dl.acm.org/doi/10.5555/645387.651559)
  1. [PDF link](https://link.springer.com/content/pdf/10.1007%2F3-540-19027-9_10.pdf)

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
