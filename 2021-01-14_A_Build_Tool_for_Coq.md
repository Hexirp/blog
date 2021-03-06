# Coq のためのビルドツール

Coq では `coqc` がファイルのコンパイルを担っている。しかし、それ以上の構造についてはごちゃごちゃしている。ビルドツールとしては `make` や `opam` などを使うらしいが、前者はビルドの過程を一から書きたい私にとっては拒否反応があるし、後者は始めてみるものだから拒否反応がある。

そこで手前みそながら新しいビルドツールを構想してみたい。

## 基本構造

`make` に似ているシステムを採用する。

```haskell
data Job = Job !Text (IO ())
```

要するに、核はこれだけである。最初のフィールドがジョブの名前を指し、二番目のフィールドが実際に実行する内容である。

むろん、「依存関係の記述はどうするのか？」というような疑問があるに違いない。これらはジョブの生成の時に様々な特殊な方法を用意して解決する。

ジョブの中でジョブを追加することは出来ないと定める。そのような設計を選択する。

## 依存関係

あるジョブ `foo` を実行することは、それが推移的に依存する全てのジョブを実行することに等しい。これは `IO ()` の中でジョブの実行を行ってやることで再現できる。

これはジョブに外付けで含まれるシステムとなる。ジョブの中で動的に依存関係を生成できるが、そのようなことは出来ないようにシステムを設計する。 selective functor を使って、 dry run と両立可能な場合分けを実現する。
