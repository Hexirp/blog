# elm-update-builder

[arrowM/elm-update-builder](https://package.elm-lang.org/packages/arowM/elm-update-builder/latest/) とは何なのか、私からの視点で解釈したものをメモしておきます。これは、 elm-update-builder を分かりやすく解説するものではありません。あくまでも、私から見た elm-update-builder を記述しているだけです。

## 型

`Update model msg` は、内部では `modek -> (model, Cmd msg)` となっています。

[elm/browser](https://package.elm-lang.org/packages/elm/browser/latest/) では `update : msg -> model -> (model, Cmd msg)` というフィールドを持ったレコード型が使われています。 `Update` 型は、 `msg -> model -> (model, Cmd msg)` の中の `model -> (model, Cmd msg)` を切り出したものです。 `Update` 型を使うと、 `update : msg -> Update model msg` になります。

## 構造

`Update model msg` は `Update.batch` を使ってモノイドと見なせます。それは、 `Cmd msg` を `Cmd.batch` を使ってモノイドとして見なしたものを使った Writer モナドを使った Kreisli 圏においての `model` から `model` 自身への endomorphism を射の合成でモノイドとしてみなしたものに一致します。ただし、 endomorphism を射の合成としてモノイドと見なす時の合成の順序は `f >> g` です。

すなわち、具体的には次のように書き下せます。 ただし、 `Cmd.then cmd cmd_ = Cmd.batch [cmd, cmd_]` であると考えてください。

```elm
none : Update model msg
none = Update (\model -> (model, Cmd.none))

then : Update model msg -> Update model msg -> Update model msg
then (Update update) (Update update_) = Update (\model -> let (model_, cmd) = update model in let (model__, cmd_) = update_ model_ in (model__, Cmd.then cmd cmd_))
```

このような構造を持っていることに気付くと、 `Update` 型がどのように動作するのか思い浮かべることが出来ます。 `model` 型の値が操作を積み重ねられながら上から下へ受け渡されていき、その度に `Cmd msg` 型の作用が積み重なっていく様子を。

`Update model msg` が `Update.batch` を使ってモノイドと見なせることが分かると、様々な都合の良い性質を持つことが分かります。たとえば、次のようにいくら積み重ねても変なことにならずに直観的に動くことが分かります。

```elm
foo : Update model msg
foo
  =
    Update.batch
      [
        a
      ,
        b
      ,
        Update.batch
          [
            c
          ,
            d
          ,
            Update.batch
              [
                e
              ]
          ,
            f
          ]
      ,
        g
      ,
        h
      ,
        i
      ]

baa : Update model msg
baa
  =
    Update.batch
      [
        a
      ,
        b
      ,
        c
      ,
        d
      ,
        e
      ,
        f
      ,
        g
      ,
        h
      ,
        i
      ]

-- foo と baa は等しい。
```

## まとめ

[arrowM/elm-update-builder](https://package.elm-lang.org/packages/arowM/elm-update-builder/latest/) は、次の二つの点で面白そうなライブラリだと感じました。

* アイデアが単純である。アイデアが単純なものは、設計も単純になり、汎用性が高くなりやすいです。
* モノイドとして解釈できる。モノイドは、コードの組み換えをしても処理の内容が変わらないという保証を与えてくれます。
