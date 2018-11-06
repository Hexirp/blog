# hexirp-blog

Hexirp のブログのソースコードである。

ブログの元だねと、それを Hakyll によってコンパイルするプログラムからなる。
Hakyll のバグがなければ Windows でも Linux でもビルドできるはず。はやく
https://github.com/jaspervdj/hakyll/pull/664 がマージされてほしい。
for-windows ブランチは Windows 用で、独自にバグを直した Hakyll (上のやつ) を
使う。

## LICENSE

[LICENSE](LICENSE) applies to all files **except docs-pre/ and docs/**.

## ページ

以下のフィールドが必要である。

```yaml
title: ページのタイトル
description: ページの説明
canonical: 正しい記事のURL、Qiita などに重複投稿しているならばそれへのリンク
type: ページのタイプ、(website | article) 、ホームページ以外は article
```

英字と和字の間に半角スペースを入れる。一般化すると、文章中に英語などの
単語の間を空ける言語を挿入するときは、間隔をあける。
