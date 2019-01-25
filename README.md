# hexirp-blog

Hexirp のブログ。

## LICENSE

[LICENSE](LICENSE) applies to all files **except docs-pre/ and docs/**.

## 構造

ブログの元だね (docs-pre) と、それを Hakyll によってコンパイルする
プログラムからなる。Hakyll のバグがなければ Windows でも Linux でも
ビルドできるはず。はやく https://github.com/jaspervdj/hakyll/pull/664 が
マージされてほしい。for-windows ブランチは Windows 用で、独自にバグを
直した Hakyll (さっきのプルリクのやつ) を使う。

master ブランチは CI によるコミット以外は禁止。source にコミットすると CI が
走って、コンパイル結果が master にデプロイされる。コミットは慎重に。develop は
普段の開発用。

## 書き方

以下のメタデータが必要である。Pandoc Markdown の `yaml_metadata_block` 拡張を
使って記述する。

* title - ページのタイトル
* description - ページの説明。一行に収まらない場合は YAML の複数行書くための
  記法を使える。
* canonical - ページの標準的な URL 。例えば、このブログと Qiita に重複投稿
  したときに、このデータに Qiita の URL をセットすることで、スパムと見なされて
  Google 検索結果から消えることを防げる。
* type - ページのタイプ。トップページだけ website で、記事は article 。

日本語の文章中に英語などの単語の間を空ける言語を挿入するときは、その間で間隔を
あけるように。大量の例外があるので、自分の美意識に従って処理。
