# 構造体の構築と借用

今、私は自作言語を作りながら Rust を学んでいます。 Rust は、手続き型な書き方をできるにも拘らず、型システムが柔軟で関数型のような書き方もでき、面白い使い心地です。しかしながら、そんな型システムの限界と見えるような状態に遭遇したので、それについて書き留めます。

## 経緯

私は、次のような型を定義しました。

```rc
struct VariableName { string: String }
```

その後に、私は関数を実装していました。その中で、 `x: &String` と `set: HashSet<VariableName>` があり、それらを使い `set.contains(&VariableName { string: x })` という風に判定したい場面がありました。

当然のことながら、 `set.contains(&VariableName { string: x })` は、そのままでは型が合いません。なぜかというと、フィールド `string` の型は `String` であり、変数 `x` の型は `&String` であるためです。

まず、私が試したのは `set.contains(&VariableName { string: *x })` というように外してみることでした。しかし、これは [E0507](https://doc.rust-lang.org/stable/error_codes/E0507.html) となりました。ここで、 `x` を `VariableName` 構造体の構築の際に含めると、それは move して消費してしまうということです。

次に、私は move 関連で問題が起きていることから、早く実装を進めたいこともあり、コストがかかることを知りながらも `clone` 関数を使うことを選択しました。これにより、 `set.contains(&VariableName { string: x.clone() })` として動く実装をすることができました。

しかし、後から見返して `clone` 関数を使わない実装が出来るのではないかと思うようになりました。

これが出来るのか出来ないのか、この問題に私は直面しました。

----

本題から逸れますが、実際のコードが気になる方のために書いておくと、この問題に直面した時のコードは [LINK](https://github.com/Hexirp/Labda/blob/461556610cc2331db9657b1f9de57f12d2478f80/src/lambda_calculus.rs#L59-L69) であり、私は此のコードを既に此の記事で説明する問題を迂回できるように書き換えており、その後のコードは [LINK](https://github.com/Hexirp/Labda/blob/9e99f4bf39dd1f8190cb9cd92fb2629d4de3fe38/src/lambda_calculus.rs#L59-L67) です。

## 問題

この問題を一般化すると、次のような形になります。

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

これらの関数を、正しく安全に書くことが出来るかどうか、ということです。

## 私の考え方

私は、これらの関数を正しく安全に書くことが出来るのではないかと考えていました。その理由は、それらのフィールドのライフタイムが全て `'a` である構造体であれば、その全体のライフタイムも `'a` となることが自然であると思っていたからです。たとえば、 `'static` については、これが当てはまります。次のコードについて、それらのフィールドが全て `'static` であるため、その全体のライフタイムも `'static` であると推論することが出来ます。

```rc
struct Bar<T> { bar: T, num: i32 }

fn bar() -> &'static Bar<i32> {
    &Bar { bar: 0, num: 0 }
}
```

しかしながら、正しく安全に書く方法を見つけることが出来ませんでしたし、これらの関数を正しく安全に書くことが出来ない理由も分かりませんでした。

## 解決

そこで、 Twitter で呟いたり Stack Overflow で質問したりしました。その呟きは [LINK](https://x.com/hexirp_prixeh/status/1800165271023624546) であり、その質問は [LINK](https://stackoverflow.com/questions/78613964/convert-a-reference-to-a-reference-to-a-value-wrapped-in-a-structure-with-the-va) です。

一つ目の結論としては、ある関数の中で構造体を作成したとき、そのライフタイムは関数の最初から最後までが上限となるため、上記の関数を正しく安全に書くことが出来ないということです。つまり、 `'static` なライフタイムの時に関数の外側へ出せたのは恐らく特殊な処理だということです。

しかし、上記の関数を正しく書くことが出来ない理由にはなっても、最初に説明した処理を書くことが出来ない理由にはなりません。

それを解決したのは、たまたま Twitter で見つけた "[The Inconceivable Types of Rust: How to Make Self-Borrows Safe](https://blog.polybdenum.com/2024/06/07/the-inconceivable-types-of-rust-how-to-make-self-borrows-safe.html)" という記事を読んだことでした。

二つ目の結論としては、現在の Rust は move を前提にしており、特に構造体の構築においても move を前提にしているため、それを回避することはできないということです。
