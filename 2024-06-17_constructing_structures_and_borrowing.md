# 構造体の構築と借用

今、私は自作言語を作りながら Rust を学んでいます。 Rust は、手続き型な書き方をできるにも拘らず、型システムが柔軟で関数型のような書き方もできるというのが、面白い使い心地だと感じています。しかしながら、そんな中、長らく悩んでいた事柄があったので、それを書き留めておきます。

## 発端

私は、次のような型を定義しました。

```rc
struct VariableName { string: String }
```

その後に、私は関数を実装していました。その中で、 `x: &String` と `set: HashSet<VariableName>` があり、それらを使って `set` は `VariableName { string: *x }` と同じ値を含んでいるのかどうか、それを判定したい場面がありました。

まず、私が試したのは `set.contains(&VariableName { string: *x })` というように外してみることでした。しかし、これは [E0507](https://doc.rust-lang.org/stable/error_codes/E0507.html) というエラーを起こしました。 `*x` をムーブしてしまうのがいけないようです。

次に、私はムーブに関する問題が起きていることから、 `Clone::clone` 関数を使用することを考察してみることにしました。とりあえずコストを減らそうとするのは後回しにしようということで、 `Clone::clone` 関数を使うことを選択しました。そうすると `set.contains(&VariableName { string: x.clone() })` と書けるようになり、とりあえずは動く実装をすることが出来ました。

しかし、 `Clone::clone` 関数は、 `String` 型の場合は、丸ごとコピーするため、コストがかかりそうです。このため、私は `Clone::clone` 関数を使わない実装をすることが出来ないのかを疑問に思いました。

最終的には、これは出来ないということを理解することが出来ましたが、そこに辿り着くまでに時間が掛かりました。その経緯を書き留めておきます。

----

本題から逸れますが、実際のコードが気になる方のために書いておくと、この問題に直面した時のコードは [LINK](https://github.com/Hexirp/Labda/blob/461556610cc2331db9657b1f9de57f12d2478f80/src/lambda_calculus.rs#L59-L69) です。このコードは既に `Clone:clone` 関数を使わないようにしながらも問題を迂回できるように書き換えています。私が書き換えた後のコードは [LINK](https://github.com/Hexirp/Labda/blob/9e99f4bf39dd1f8190cb9cd92fb2629d4de3fe38/src/lambda_calculus.rs#L59-L67) です。

## 一般化

私は、この問題を、「 `T` 型を `S` 型がフィールドとして含むとき、 `&T` を `&S` へ変換することが出来るかどうか」という問題に一般化しました。つまり、たとえば、次に書いた `foo` 関数と `bar` 関数と `baz` 関数を実装できるかどうか、ということです。

```rc
struct VariableName { string: String }

fn foo<'a>(x: &'a String) -> &'a VariableName {
    &VariableName { string: *x }
}

struct Bar<T> { bar: T, num: i32 }

fn bar<'a,T>(x: &'a T) -> &'a Bar<T> {
    &Bar { bar: *x, num: 0 }
}

fn baz<'a,T>(x: &'a T) -> &'a Option<T> {
    &Option::Some(*x)
}
```

## 私の思考

私は、「 `T` 型を `S` 型がフィールドとして含むとき、 `&T` を `&S` へ変換することが出来るかどうか」の答えは、「できる」だと考えました。その理由は、「それぞれのフィールドが `'a` ライフタイムの間は存続するとき、それらを束ねた構造体も `'a` ライフタイムの間は存続する」のだと私は考えたためです。たとえば、 `foo` 関数については、「 `x` はライフタイム `'a` の間は存続するため、 `VariableName { string: *x }` もライフタイム `'a` の間は存続する」のだと私は考えていました。

しかしながら、これが正しいのかどうか、色々と調べても、よくわかりませんでした。その代わり、 kmdreko が投稿した回答 ([LINK](https://stackoverflow.com/questions/71026147/reference-to-an-option-versus-option-of-a-reference/71027236#71027236)) が説明している方法を使うことで、 while 式を再帰へ書き換えることになりますが、目的の処理を記述することが出来ました。しかしながら、私は再帰も性能面で不安があると考えて別の方法を求めました。

## コミュニティ

私は、自分の力で解決できなさそうだと感じて、公開の場で質問することにしました。そこで Stack Overflow で "Convert a reference to a reference to a value wrapped in a structure with the value of the reference" ([LINK](https://stackoverflow.com/questions/78613964/convert-a-reference-to-a-reference-to-a-value-wrapped-in-a-structure-with-the-va)) を投稿しました。しかし、これは私の聞き方が悪かったせいで、私が意図したものとは異なる別の問題として解釈されてしまいました。

また、 Twitter で「わからないことがある」という呟き [LINK](https://x.com/hexirp_prixeh/status/1800165271023624546) をしたところ、 KawamoYurase さんが「関数の内部で作成した参照を、関数の外側へ出すことは出来ない」ということを教えてくれました。このため、 `foo` 関数と `bar` 関数と `baz` 関数を実装することは出来ないということです。しかし、これらの関数を正しく書くことが出来ない理由にはなっても、最初に説明した処理を書くことが出来ない理由にはなりません。私の一般化が誤っていたということです。

しかし、既に `String` ではなく `VariableName` を保持するという方法で `Clone::clone` 関数も再帰も使わない実装をすることが出来ています。そのため、「 `x: &String` と `set: HashSet<VariableName>` があり、それらを使って `set` は `VariableName { string: *x }` と同じ値を含んでいるのかどうか、それを判定したい時に、 `Clone::clone` 関数を使わずにすることが出来るのかどうか」という問題を棚上げすることにしました。

## 解決

ところが、数日前のこと、ふとした時に解決してしまいました。そもそもの間違いは、私が思考に用いていたモデルの誤りです。具体的には、私は `VariableName { string: x }` という値があるとき、「 `VariableName { string: _ }` が `x` を参照している」というモデルを前提にしていました。しかし、実際には、そうではないのです。つまり、 `VariableName { string: x }` という値があるとき、それは `x` がリニアに並んでいるのです。

本当に正しいモデルを前提にして考えてみると、 `x` を記録するメモリの領域を拡張して `VariableName { string: x }` を作ることは出来ません。 `x` を記録するメモリの領域の周囲に隙間があるかどうかは保証することが出来ないからです。その一方で、 `VariableName { string: x }` を記録するメモリの領域を収縮して `x` と見なすことは出来ます。

Rust は、 Haskell のように algebraic data types を扱うことが出来ますが、それは Haskell とは異なりメモリと強く結びついています。そのことを忘れてしまったがゆえに、ずっと私は悩んでいたのです。
