# 自分型で余帰納型をエンコーディングする

自分型を使って余帰納型をエンコーディングします。

## 帰納型

帰納型 (inductive type) は、その値が帰納的に定義された型のことです。

例えば、自然数の型 `Natural_Number` は次のように定義されます。

* `zero : Natural_Number` である。
* `n_p : Natural_Number` であるならば `successor n_p : Natural_Number` である。
* これらの条件を満たす最小の型である。

ここに出てきた `zero` と `successor` を構築子 (constructor) と呼びます。また、値に関して帰納法を行うことが出来ます。

## チャーチ・エンコーディング

チャーチ・エンコーディング (Church encoding) は、帰納型を**型無し**ラムダ計算にエンコーディングします。概略は [Wikipedia](https://en.wikipedia.org/wiki/Church_encoding) を見たら把握できると思います。

具体的に自然数をエンコーディングしてみます。

```
zero = λ x. λ f. x
successor = λ n. λ x. λ f. f (n x f)
```

## ベーム・ベラルドゥッチ・エンコーディング

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

## パリゴット・エンコーディング

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

## 帰納原理

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

## 組み込みの帰納型

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

## 自分型

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

## 自分型による帰納型のエンコーディング

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

## 余帰納型

余帰納型 (coinductive type) は、その値が余帰納的に定義された型です。

たとえば、 `A` のストリームの型 `Stream A` は次のように定義されます。

* `x : Stream A` であるならば、 `head x : A` である。
* `x : Stream A` であるならば、 `tail x : Stream A` である。
* これらの条件を満たす最大の型である。

ここに出てきた `head` と `tail` は分解子 (destructor) と呼ぶことにします。また、値に関して余帰納法を行うことが出来ます。

## 自分型による余帰納型のエンコーディング（１）

余帰納型の代表たる `Stream` 型を取り上げます。非依存型に対する余帰納法を考えると次のようになります。

```
Π P : Type.
Π destruct_head : P -> A.
Π destruct_tail : P -> P.
(P -> Stream A)
```

帰納型の方では `Natural_Number -> P` が最終的に返すものでした。同じように余帰納型では `P -> Stream A` が最後に返すものです。矢印の方向が反対になっている訳ですね。これは帰納型と余帰納型が圏論的に双対であるためです。

しかし、これを依存型へと拡張させようとすると、途端に行き詰ってしまいます。帰納型の方では `Natural_Number -> P` を `Π x : Natural_Number. P x` に書き換えて引数の型も適切に書き換えると依存型に対応した帰納法が得られます。これは `A -> B` が `Π _ : A. B` と等しいことを利用しています。ところが、余帰納型では同じ方法が使えないのです。

自分型で帰納型をエンコーディングした時は `Π x : Natural_Numbber. P x` を `P x` に書き換えていました。しかし、余帰納型では前述の通り同じ手法が使えません。自分型で余帰納型をエンコーディングするのには、どうすればよいのでしょうか。

この問題については、私が Coq を始めた時から疑問に思っていました。帰納型に依存型での帰納法があるのに、余帰納型に依存型での余帰納法がないのは、なんでだろうと。

`P : Natural_Number -> Type` での矢印を単純に逆にして `P : Type -> Stream A` にしてみたり、 [余帰納型には**余**依存型での余帰納法があるのではないかと考えてみたり](https://twitter.com/hexirp_prixeh/status/1033573680582807555)、具体的に余依存型を計算してみたら情報を余り持たないことが分かったり、依存積 `Π` の双対は依存和 `Σ` なので、 `Σ x : Stream A. P x` が正しいのではないかと考えてみたり、色々と頭を捻っていました。

結局のところは `P -> Stream A` の `Stream A` が反変ではないため一番左側に括りだせないのがダメなのです。これを何とか出来れば……

共変と反変を入れ替える…… 反変を共変にする…… そんな例があることに思い当たりました。

## 冪モナド

`P X := X -> Type` という関数を考えます。 `P : Type -> Type` です。ただし、宇宙レベル (universe level) に注意しなければなりませんが。

実は、これはモナドになるのです。なぜならば、 `X -> Type` は冪集合の類似物と見なすことが出来て、集合の圏において冪集合の操作は冪集合モナドというものになるからです。

具体的に実装を与えましょう。

```
P := λ X. X -> Type

return = λ A. λ x. λ y. Path A x y
bind = λ A. λ B. λ p. λ q. λ y. Σ x : A. p x * q x y
```

また、 `map _ _ f x = bind _ _ x (λ x. return _ (f x))` としてファンクターの実装も与えることが出来ます。ちなみに、 `P` のクライスリ圏での合成 (Haskell での `>=>`) は関係の合成と一致します。

ここで `P X` の `X` は反変でした。それなのにモナドにすることが出来ました。つまり、 `P` は共変と反変の壁を崩すことが出来るのです。

ここでの `P` の正しい名前を知らないので冪モナド (power monad) と呼ぶことにしましょう。また、 `P` は名前が短すぎるので `Power` とします。

## 自分型による余帰納型のエンコーディング（２）

冪モナドを使って共変と反変をひっくりかえします。

```
Π P : Type.
Π destruct_head : P -> Power A.
Π destruct_tail : P -> Power P.
(P -> Power (Stream A))
```

`P -> Power (Stream A)` での `Stream A` は反変です。

```
Π P : Type.
Π destruct_head : P -> A -> Type.
Π destruct_tail : P -> P -> Type.
(P -> Stream A -> Type)
```

`Power` の定義を展開してみます。

```
Π P : Stream A -> Type.
Π destruct_head : Π x : Stream A. P x -> A -> Type.
Π destruct_tail : Π x : Stream A. P x -> P (tail x) -> Type.
(Π y : Stream A. P y -> Type)
```

すると、このように `P` を依存型に書き換えることが出来ます。 `Π y : Stream A. P y -> Type` での `Stream A` は引数ではなく**返り値**であるため `y` を使ってあります。この関数を `coinduction` と呼ぶことにしましょう。

```
ι y.
Π P : Stream A -> Type.
Π destruct_head : Π x : Stream A. P x -> A -> Type.
Π destruct_tail : Π x : Stream A. P x -> P (tail x) -> Type.
(P y -> Type)
```

`y : Stream A` 自分だと考えて自分型で閉じます。これが `Stream A` の自分型によるエンコーディングと言えましょう。

```
ι y.
Π P : Stream A -> Type.
Π destruct_head : Π x : Stream A. P x -> Power A.
Π destruct_tail : Π x : Stream A. P x -> Power (P (tail x)).
(Power (P y))
```

ただし、上記のように冪モナドが覆い被さった形になります。 `Power` を剥がす手段が存在しない以上、これは不完全と言わざるを得ません。

それはそうと、 `coinduction` は、どんな挙動をするのか考えてみましょう。

```
  coinduction A P destruct_head destruct_tail y x
=
    Path (A -> Type) (destruct_head y x) (return (head y))
  *
    coinduction A P destruct_head destruct_tail (tail y) (destruct_tail y x)
```

上記のようになります。このような条件を満たす述語は余帰納法を使って定義できます。

`coinduction` は、最終的には `Π y : Stream A. P y -> Type` という関係を返します。これは関数を表す関係であって、 `x : P y` が定まれば `y : Stream A` が定まるというものです。なんというか、これが余依存型の正体だとしたら、情報を余り持たないという結果が出たのも納得です。

## 自分型による余帰納型のエンコーディング（３）

依存型に対する余帰納法を冪モナドを使って無理矢理求めました。ここでは冪モナドを除去できないか試して見ます。

```
Π P : Stream A -> Type.
Π destruct_head : Π x : Stream A. P x -> A -> Type.
Π destruct_tail : Π x : Stream A. P x -> P (tail x) -> Type.
(Π y : Stream A. P y -> Type)
```

冪モナド版の `coinduction` 関数の型です。

```
Π P : Stream A -> Type.
Π destruct_head : Π x : Stream A. P x -> A -> Type.
Π destruct_tail : Π x : Stream A. P x -> P (tail x) -> Type.
((Σ y : Stream A. P y) -> Type)
```

`Π` と `Σ` の間で変換をします。

```
Π P : Stream A -> Type.
Π destruct_head : Π x : Stream A. P x -> Power A.
Π destruct_tail : Π x : Stream A. P x -> Power (P (tail x)).
(Power (Σ y : Stream A. P y))
```

`Power` で式を畳みます。

```
Π P : Stream A -> Type.
Π destruct_head : Π x : Stream A. P x -> Σ y : A. Path A (head x) y.
Π destruct_tail : Π x : Stream A. P x -> P (tail x).
(Σ y : Stream A. P y)
```

`Power` を取り除きます。ここで注意すべきなのは `Power A` を `Σ y : A. Path A (head x) y` に置き換えた所です。これは、さっきの冪モナド版の `coinduction` の計算で `Path (A -> Type) (destruct_head y x) (return (head y))` という結果になっていたからですね。

以前に依存積を依存和に置き換えればいいのではと考えていたのと同じ結果になりました。ただし、以前は `Σ y : A. Path A (head x) y` の所に気付けていなかったため、妙な違和感があって先に進めなかったのです。

さて、ここでの `y : Stream A` を自分型で閉じたいところですが、やっぱり不可能です。なので、これでおしまいです。

## 自分型による余帰納型のエンコーディング（４）

余帰納型は帰納型で表現することが出来ます。そのため、余帰納型を帰納型でエンコーディングした後に、帰納型を自分型でエンコーディングします。

## まとめ

依存型に関する余帰納法は可能です。たとえば、次のようになります。

```
Π P : Stream A -> Type.
Π destruct_head : Π x : Stream A. P x -> Σ y : A. Path A (head x) y.
Π destruct_tail : Π x : Stream A. P x -> P (tail x).
(Σ y : Stream A. P y)
```

自分型による余帰納法のエンコーディングは、冪モナドを使えば無理矢理実現することが可能です。たとえば、次のようになります。

```
ι y.
Π P : Stream A -> Type.
Π destruct_head : Π x : Stream A. P x -> Power A.
Π destruct_tail : Π x : Stream A. P x -> Power (P (tail x)).
(Power (P y))
```

冪モナドを使わない方法は、余帰納型を帰納型でエンコーディングした後に、帰納型を自分型でエンコーディングすることしかなさそうです。

## 参考文献

1. [Self Types for Dependently Typed Lambda Encodings](https://dblp.org/rec/conf/rta/FuS14.html)
  1. [PDF link](https://homepage.divms.uiowa.edu/~astump/papers/fu-stump-rta-tlca-14.pdf)
2. [Programming with Proofs: A Second Order Type Theory](https://dl.acm.org/doi/10.5555/645387.651559)
  1. [PDF link](https://link.springer.com/content/pdf/10.1007%2F3-540-19027-9_10.pdf)
