---
title: このブログの制作記録
...

####################
このブログの制作記録
####################

このブログを作ったときに起こった出来事を書き留めていきます。

******
Hakyll
******

このブログに記事が一つ出来たら GitHub Pages で公開しようと考えていました。
ついさっき、記事が一つ完成したので、取り掛かり始めました。

Haskell でブログといったら `Hakyll`_ でしょう。とても有名なライブラリです。
Haskell で書かれた静的なウェブサイトのためのコンパイラ、Hakyll は
Jekyll のHaskell版であり、markdown なんなりで書かれたものをHTMLへ変換し、
テンプレートを使ってヘッダーなどを付け加え、一ページとしてコンパイルできます。

最初の一歩、Hakyll を依存関係へ加えてやりました。何も関数は使いません。
しかし、Hakyllの大量の依存関係のせいでビルドに約1時間かかってしまったあげく、
最終的に Windows のみで発生するらしい GHC そのもののバグで失敗してしまいました。

.. code-block:: text

 Configuring hakyll-4.12.3.0...
 Preprocessing library for hakyll-4.12.3.0..
 Building library for hakyll-4.12.3.0..
 [ 1 of 53] Compiling Data.List.Extended ( lib\Data\List\Extended.hs, .stack-work\dist\5c8418a7\build\Data\List\Extended.o )
 [ 2 of 53] Compiling Data.Yaml.Extended ( lib\Data\Yaml\Extended.hs, .stack-work\dist\5c8418a7\build\Data\Yaml\Extended.o )
 [ 3 of 53] Compiling Hakyll.Core.Configuration ( lib\Hakyll\Core\Configuration.hs, .stack-work\dist\5c8418a7\build\Hakyll\Core\Configuration.o )
 [ 4 of 53] Compiling Hakyll.Core.Identifier ( lib\Hakyll\Core\Identifier.hs, .stack-work\dist\5c8418a7\build\Hakyll\Core\Identifier.o )
 [ 5 of 53] Compiling Hakyll.Core.Identifier.Pattern.Internal ( lib\Hakyll\Core\Identifier\Pattern\Internal.hs, .stack-work\dist\5c8418a7\build\Hakyll\Core\Identifier\Pattern\Internal.o )
 [ 6 of 53] Compiling Hakyll.Core.Identifier.Pattern ( lib\Hakyll\Core\Identifier\Pattern.hs, .stack-work\dist\5c8418a7\build\Hakyll\Core\Identifier\Pattern.o )
 [ 7 of 53] Compiling Hakyll.Core.Dependencies ( lib\Hakyll\Core\Dependencies.hs, .stack-work\dist\5c8418a7\build\Hakyll\Core\Dependencies.o )
 [ 8 of 53] Compiling Hakyll.Core.Logger ( lib\Hakyll\Core\Logger.hs, .stack-work\dist\5c8418a7\build\Hakyll\Core\Logger.o )
 [ 9 of 53] Compiling Hakyll.Core.Metadata ( lib\Hakyll\Core\Metadata.hs, .stack-work\dist\5c8418a7\build\Hakyll\Core\Metadata.o )
 [10 of 53] Compiling Hakyll.Core.Store ( lib\Hakyll\Core\Store.hs, .stack-work\dist\5c8418a7\build\Hakyll\Core\Store.o )
 [11 of 53] Compiling Hakyll.Core.Util.File ( lib\Hakyll\Core\Util\File.hs, .stack-work\dist\5c8418a7\build\Hakyll\Core\Util\File.o )
 [12 of 53] Compiling Hakyll.Core.Provider.Internal ( lib\Hakyll\Core\Provider\Internal.hs, .stack-work\dist\5c8418a7\build\Hakyll\Core\Provider\Internal.o )
 [13 of 53] Compiling Hakyll.Core.Provider.Metadata ( lib\Hakyll\Core\Provider\Metadata.hs, .stack-work\dist\5c8418a7\build\Hakyll\Core\Provider\Metadata.o )
 [14 of 53] Compiling Hakyll.Core.Provider.MetadataCache ( lib\Hakyll\Core\Provider\MetadataCache.hs, .stack-work\dist\5c8418a7\build\Hakyll\Core\Provider\MetadataCache.o )
 [15 of 53] Compiling Hakyll.Core.Provider ( lib\Hakyll\Core\Provider.hs, .stack-work\dist\5c8418a7\build\Hakyll\Core\Provider.o )
 [16 of 53] Compiling Hakyll.Core.Util.Parser ( lib\Hakyll\Core\Util\Parser.hs, .stack-work\dist\5c8418a7\build\Hakyll\Core\Util\Parser.o )
 [17 of 53] Compiling Hakyll.Core.Util.String ( lib\Hakyll\Core\Util\String.hs, .stack-work\dist\5c8418a7\build\Hakyll\Core\Util\String.o )
 [18 of 53] Compiling Hakyll.Core.Routes ( lib\Hakyll\Core\Routes.hs, .stack-work\dist\5c8418a7\build\Hakyll\Core\Routes.o )
 [19 of 53] Compiling Hakyll.Core.Compiler.Internal ( lib\Hakyll\Core\Compiler\Internal.hs, .stack-work\dist\5c8418a7\build\Hakyll\Core\Compiler\Internal.o )
 [20 of 53] Compiling Hakyll.Core.Item ( lib\Hakyll\Core\Item.hs, .stack-work\dist\5c8418a7\build\Hakyll\Core\Item.o )
 [21 of 53] Compiling Hakyll.Core.Compiler.Require ( lib\Hakyll\Core\Compiler\Require.hs, .stack-work\dist\5c8418a7\build\Hakyll\Core\Compiler\Require.o )
 [22 of 53] Compiling Hakyll.Core.Compiler ( lib\Hakyll\Core\Compiler.hs, .stack-work\dist\5c8418a7\build\Hakyll\Core\Compiler.o )
 [23 of 53] Compiling Hakyll.Core.UnixFilter ( lib\Hakyll\Core\UnixFilter.hs, .stack-work\dist\5c8418a7\build\Hakyll\Core\UnixFilter.o )
 [24 of 53] Compiling Hakyll.Core.Writable ( lib\Hakyll\Core\Writable.hs, .stack-work\dist\5c8418a7\build\Hakyll\Core\Writable.o )
 [25 of 53] Compiling Hakyll.Core.Item.SomeItem ( lib\Hakyll\Core\Item\SomeItem.hs, .stack-work\dist\5c8418a7\build\Hakyll\Core\Item\SomeItem.o )
 [26 of 53] Compiling Hakyll.Core.Rules.Internal ( lib\Hakyll\Core\Rules\Internal.hs, .stack-work\dist\5c8418a7\build\Hakyll\Core\Rules\Internal.o )
 [27 of 53] Compiling Hakyll.Core.Runtime ( lib\Hakyll\Core\Runtime.hs, .stack-work\dist\5c8418a7\build\Hakyll\Core\Runtime.o )
 [28 of 53] Compiling Hakyll.Core.Rules ( lib\Hakyll\Core\Rules.hs, .stack-work\dist\5c8418a7\build\Hakyll\Core\Rules.o )
 [29 of 53] Compiling Hakyll.Core.File ( lib\Hakyll\Core\File.hs, .stack-work\dist\5c8418a7\build\Hakyll\Core\File.o )
 [30 of 53] Compiling Hakyll.Preview.Poll ( lib\Hakyll\Preview\Poll.hs, .stack-work\dist\5c8418a7\build\Hakyll\Preview\Poll.o )

 lib\Hakyll\Preview\Poll.hs:21:1: warning: [-Wunused-imports]
     The import of ‘throw’ from module ‘Control.Exception’ is redundant
    |
 21 | import           Control.Exception              (IOException, throw, try)
    | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
 [31 of 53] Compiling Hakyll.Preview.Server ( lib\Hakyll\Preview\Server.hs, .stack-work\dist\5c8418a7\build\Hakyll\Preview\Server.o )
 [32 of 53] Compiling Hakyll.Web.CompressCss ( lib\Hakyll\Web\CompressCss.hs, .stack-work\dist\5c8418a7\build\Hakyll\Web\CompressCss.o )
 [33 of 53] Compiling Hakyll.Web.Html  ( lib\Hakyll\Web\Html.hs, .stack-work\dist\5c8418a7\build\Hakyll\Web\Html.o )
 [34 of 53] Compiling Hakyll.Web.Html.RelativizeUrls ( lib\Hakyll\Web\Html\RelativizeUrls.hs, .stack-work\dist\5c8418a7\build\Hakyll\Web\Html\RelativizeUrls.o )
 [35 of 53] Compiling Hakyll.Web.Pandoc.Binary ( lib\Hakyll\Web\Pandoc\Binary.hs, .stack-work\dist\5c8418a7\build\Hakyll\Web\Pandoc\Binary.o )
 [36 of 53] Compiling Hakyll.Web.Pandoc.FileType ( lib\Hakyll\Web\Pandoc\FileType.hs, .stack-work\dist\5c8418a7\build\Hakyll\Web\Pandoc\FileType.o )
 [37 of 53] Compiling Hakyll.Web.Pandoc ( lib\Hakyll\Web\Pandoc.hs, .stack-work\dist\5c8418a7\build\Hakyll\Web\Pandoc.o )
 [38 of 53] Compiling Hakyll.Web.Pandoc.Biblio ( lib\Hakyll\Web\Pandoc\Biblio.hs, .stack-work\dist\5c8418a7\build\Hakyll\Web\Pandoc\Biblio.o )
 [39 of 53] Compiling Hakyll.Web.Redirect ( lib\Hakyll\Web\Redirect.hs, .stack-work\dist\5c8418a7\build\Hakyll\Web\Redirect.o )
 [40 of 53] Compiling Hakyll.Web.Template.Context ( lib\Hakyll\Web\Template\Context.hs, .stack-work\dist\5c8418a7\build\Hakyll\Web\Template\Context.o )
 [41 of 53] Compiling Hakyll.Web.Tags  ( lib\Hakyll\Web\Tags.hs, .stack-work\dist\5c8418a7\build\Hakyll\Web\Tags.o )
 [42 of 53] Compiling Hakyll.Web.Paginate ( lib\Hakyll\Web\Paginate.hs, .stack-work\dist\5c8418a7\build\Hakyll\Web\Paginate.o )
 [43 of 53] Compiling Hakyll.Web.Template.Internal.Element ( lib\Hakyll\Web\Template\Internal\Element.hs, .stack-work\dist\5c8418a7\build\Hakyll\Web\Template\Internal\Element.o )
 [44 of 53] Compiling Hakyll.Web.Template.Internal.Trim ( lib\Hakyll\Web\Template\Internal\Trim.hs, .stack-work\dist\5c8418a7\build\Hakyll\Web\Template\Internal\Trim.o )
 [45 of 53] Compiling Hakyll.Web.Template.Internal ( lib\Hakyll\Web\Template\Internal.hs, .stack-work\dist\5c8418a7\build\Hakyll\Web\Template\Internal.o )
 [46 of 53] Compiling Hakyll.Web.Template ( lib\Hakyll\Web\Template.hs, .stack-work\dist\5c8418a7\build\Hakyll\Web\Template.o )
 [47 of 53] Compiling Hakyll.Web.Template.List ( lib\Hakyll\Web\Template\List.hs, .stack-work\dist\5c8418a7\build\Hakyll\Web\Template\List.o )
 [48 of 53] Compiling Hakyll.Web.Feed  ( lib\Hakyll\Web\Feed.hs, .stack-work\dist\5c8418a7\build\Hakyll\Web\Feed.o )
 Access violation in generated code when executing data at 0000000103644510

こんなバグです。

* https://github.com/jaspervdj/hakyll/issues/613
* https://ghc.haskell.org/trac/ghc/ticket/13112

hakyllをビルドしているときのメモリの様子を見ていると90%を超えているため、
ただメモリが足りないためかもしれません。どっちにしても解決は困難です。

手元のパソコンでできない以上 TravisCI でやるしかないのですが、
仕様上メモリに加えて時間制限もネックになってしまいます。
依存するライブラリ群を徐々にビルドさせることにしました。

\ ``stack dot``\ を使って依存関係をグラフに変換して穴が開くほど見つめると、
この逆の順序でビルドさせるのがよさそうに見えます。

* hakyll
* pandoc
* wai-app-static
* wai-extra
* http-conduit
* tls

**************
デプロイの方法
**************

ちょっとわき道にそれて、生成したものをどうやってサイトにするかを考えます。
GitHub Pages を使うといってもどこを参照するかでいろいろオプションがあります。
gh-pages ブランチ以下のソースや master そのものを使う方法もあるのですが、
master のdocsフォルダ以下を読み込ませる方法でいきます。

開発は develop ブランチで行います。開発途中の物が公開されるのは困るため、
また、安全のために、sourceブランチにマージしないとデプロイされないようにします。

サイトのソースは docs-pre に置いて、Hakyll の設定で docs へと変換させます。

デプロイは TravisCI で行います。Hakyll を使うときはメモリの問題もろもろで
CircleCI など別のものが一般的らしいのですが、TravisCI のシンプルさが好きなため、
しばらくは TravisCI で頑張ってみることにします。

************
テンプレート
************

話を戻します。TravisCI に依存関係をキャッシュさせるのは成功しました。

次に始めたのはテンプレート作りです。HTMLやCSSについて必死に調べて、
Grid Layout とか Flexbox を活用した結果、なんかそれっぽいものが完成しました。

文章にすると、グリッドで上下に区切って、上側に細いヘッダーを作り、
ヘッダーの一番左にアイコンを置いて、その右に Flexbox で等間隔にリンクを置いて、
下側は記事にして、左右に余白を作って、見出しの下にラインを入れるという形です。

**************
Hakyllのビルド
**************

TravisCI上でビルド出来るとはいえ、Hakyll を手元のPCで動作させたい。
色々手段は考えられます。以下に思いついた手段と試してみた結果を載せます。

そもそものバグを直す
 コンパイラという深いところで発生するバグ、しかしもメモリがらみです。
 今の私の技術ではとてもとても歯が立ちそうにありません。

 せめて情報を集めることでバグが早く直るようにしたかったのですが、
 stackの\ ``ghc-options=(options)``\ を渡して情報を出力させようとしても、
 情報が出力されないのかされているのかよくわかりませんでした。

GHCのメモリ使用量を減らす
 \ ``Limiting GHC Memory``\ とかいうキーワードで検索したりしたのですが、
 ほとんど情報は見つかりません。どうやら無理なようです。

 Stack Overflowに投稿された「GHCのメモリ使用量を制限する方法は？」という質問に
 まったく回答が付いていないのが哀愁を感じさせました。

分割コンパイル
 Hakyll そのものを複数回に分けてコンパイル出来たらメモリ使用量も減るのでは、
 という考えを持ったのですが、普通にはできないようです。
 Hakyll に手を入れて、直接的に複数のライブラリに分けるのなら可能です。

フラグを操作してビルドするモジュールを減らす
 Hakyll はたくさんフラグを持っていて、Pandoc を使わなかったり、
 プレビュー用のWebサーバーを使わなかったり、というときにフラグをオフにして、
 無駄なコンパイルがされないように出来ます。

 出来るだけフラグをオフにしたのですが、それでも無理でした。

ここで、有望そうなのは Hakyll を複数のライブラリに分けることでした。

************
Hakyllの分割
************

コンパイルが失敗するのは ``Hakyll.Core.***`` のコンパイルが終わってから、
``Hakyll.Web.***`` に入り始めたあたりなので、そこで分割することにしました。

Hakyll のコア部である ``Hakyll.Core.***`` だけのライブラリとして、
hakyll-core というライブラリを作り、元々のライブラリをそれに依存させます。

まず、hakyll-core というフォルダを作って、\ ``Hakyll.Core.***`` を入れました。
``.`` にある Hakyll のそのほかの部分と ``hakyll-core/`` にあるコア部、
その二つのライブラリに分けて複数ライブラリを含むリポジトリとする作戦です。

その後、cabalファイルもコピーしてやり、その他のファイルを分配し始めたのですが、
どのファイルがいるのかいらないのかよくわからず進みませんでした。

そこで、テストやサンプルなどは全て消した方がいいと思い直しました。
ただビルドを通したいだけなので、無駄なものは消した方がいいことになります。

全てを巻き戻したあとに、ライブラリでないものをすべて削除しました。
さらに、cabalファイルもそれに合わせて色々削除したり変更したりしました。
また、後々困りそうなので Paths_hakyll を使わないようにコードを書き換え、
分割したときにインポートができるように隠されたモジュールを全て公開しました。

その後、フォルダ構造をルートフォルダに一つのライブラリがある形から、
hakyll-core と hakyll それぞれに一つのフォルダが割り当てられる形にしました。

この時点で一回ビルドして、ファイルがないというエラーを見つけました。
本来は必要なのに消しすぎたということなので戻そうとしたのですが、
なぜか認識されませんでした。これは Windows のみで起こるバグでした。
(https://github.com/jaspervdj/hakyll/pull/645)

そして、メモリを使いすぎて落ちることなく、ビルドが成功しました！

ここまでの作業は `1abdee...2487d2`_ で見れます。

.. _1abdee...2487d2: https://github.com/jaspervdj/hakyll/compare/1abdeee743d65d96c6f469213ca6e7ea823340a7...2487d2ca77606da20986165ee57b3de22e311a02

************
Hakyllの修正
************

実行しようとしたらこのようなエラーが出てしまいました。

.. code-block:: text

 $ stack exec -- hexirp-blog-exe build
 Initialising...
   Creating store...
   Creating provider...
   Running rules...
 Checking for out-of-date items
 Compiling
   [ERROR] docs-pre\articles/coq_pattern_match.rst: hGetContents: invalid argument

パスがおかしくなってファイルを取得できていません。

修正するために、実行の流れを辿ってみました。

最初に呼び出す関数は ``hakyllWith`` です。
その後、色々なオプション付きの似たような関数を辿り、
``invokeCommands`` にたどり着きました。
ここで、渡したオプションに応じて呼ばれる関数が変わります。
この時に渡したオプションは build だったので、
それに応じて呼ばれる関数は ``Commands.build`` だと考えます。

この ``build`` は ``run`` の簡単なラップで、
``run`` は store の生成、provider の生成、rule の設定DSLの実行を行い、
その結果を ``build`` （さっきとは別）の初期状態として、実行します。
store は途中ファイルのキャッシュを担い、provider はサイトのソースを表します。

パスの問題に対処するには、おそらく、パスを読み込むときか、
それを使って処理するときのどっちかを直さないといけません。

本質的な方を直したいので、まず provider を生成する ``newProvider`` を見ました。
それは internal な方の ``newProvider`` を呼び出して後処理をするだけした。
それは生成するときに ``getRecursiveContent`` でコンテンツを取得して、
``getResourceInfo`` で日時情報を取得しその情報で色々しています。

WindowsとLinuxはパスの区切りが違います。\ ``\`` と ``/`` です。
もし、ファイルパスを文字列で直接書けばどちらかにしか対応できません。
そこで、\ ``System.FilePath`` は ``(<\>)`` 演算子を用意しています。
これは二つの文字列をパスの区切りを挟んで結合する単純な演算子ですが、
WindowsかLinuxかのどっちでコンパイルするかでパスの区切りが変わります。
よって、\ ``(<\>)`` を使っている関数は安全ということになります。

すぐ真下に定義があったため初めに目が留まったのは ``getResourceInfo`` で、
``docs-pre\articles/coq_pattern_match.rst`` というパスを生成する物でした。
私はここまで ``providerDirectory`` に設定した ``docs-pre`` が使われている、
そのことに着目してそれが渡される関数の定義を次々に追ってきましたが、
そのコードは問題はないように見えます。

行き詰ったように思えましたが、\ ``toFilePath`` を見てひらめきました。
hakyllは内部で ``Identifier`` という型でファイルパスで扱っています。
これはきれいなファイルパスというようなもので、その変換時に問題がありました。
つまり、\ ``fromFilePath`` が直接 ``/`` をパス区切りに使っていたのです。
(https://github.com/jaspervdj/hakyll/blob/1abdeee743d65d96c6f469213ca6e7ea823340a7/lib/Hakyll/Core/Identifier.hs#L67)

直してやると、エラーは出なくなったのですが、ファイルが認識されません。

.. note::

 プルリクエストは送り終えています。
 (https://github.com/jaspervdj/hakyll/pull/649)

************
規則の書き方
************

Hakyll は規則をまとめて ``hakyll`` 系関数に渡してやって実行するという形です。
規則は ``Route`` （どのファイルに出力するか）とか、\ ``Compiler``
（どうやって変換するか）とか、色々織り込めます。

その規則を一部のファイルだけに適用することを表す ``match`` という関数、
それが受け取るパスの書き方に問題があり、Windowsで実行するのならば、
パス区切りに ``\`` を使わないといけないということでした。
修正してみたのですが、まだファイルが認識されません。

``match pattern rule`` と書いたとき、引数となるパターンは独自の型です。
しかし、\ ``IsStrng`` のインスタンスがあるため、文字列の形で書けます。
この時に裏で走るのは\ ``fromGlob``\ という関数です。

さて、このGlob記法にはエスケープが含まれていて、\ ``\`` という文字です。
つまり、\ ``\`` そのものを使いたいときは ``\\`` と書かないといけない。
さらにHaskellのソースコードにこれを書くために ``\\\\`` と書かないといけない。

修正したら無事ファイルが認識されてコンパイルできました。

******************
テンプレートの適用
******************

テンプレートが適用されていないので生成物はのっぺりな状態です。
テンプレートは先ほど作っているので、これを適用させるコードを書きました。

これも ``match`` と同じような罠があります。ヘルパー関数を作っておきました。
パスをリストで表すので何回もバックスラッシュを書く必要がなく、
Windows でも Linux でも同じように書いて使えます。

さてコンパイルしようとしたところ、
「テンプレートの穴が開いているところは埋められなければならない」
このルールに引っかかって動作させることが出来ませんでした。

穴は文脈から埋められるのですが、その文脈を作るのが難しいのです。
実は、Pandocで定義される\ `yaml_metadata_block`_\ で文脈を定義して、
それを取り出すのが定石なのですが、これが好きではないため、
代替手段を作ろうとしたのですが出来ませんでした。

仕方ないのでとりあえずデフォルト値で埋めときました。

.. _yaml_metadata_block: https://pandoc.org/MANUAL.html#extension-yaml_metadata_block

*********************
stackのresolverの更新
*********************

resolver は早め早めに更新しないと後で困ります。
しかし、更新したら変なエラーが出てしまいました。
(https://travis-ci.org/Hexirp/blog/builds/400810238)

* https://github.com/jaspervdj/hakyll/issues/629
* https://github.com/commercialhaskell/stack/issues/4071
* https://github.com/commercialhaskell/stack/pull/4111

つまり、最後のプルリクエストでこのエラーは解決しています。
しかし、その変更点はまだリリースされている stack に含まれていません！
resolver の更新はしばらく待つ必要がありそうです。

********
デプロイ
********

デプロイのソースを実際に作りました。

まず、source ブランチでビルドしたものを master にプッシュしようとしましたが、
これでは master ブランチから source ブランチへのコミットが辿れず、
どれだけコミットをしても芝生が生えません。

さらに加えて source ブランチを master ブランチにマージすることにしました。
master ブランチからマージするとき全てを source ブランチと同じにしたいのですが、
``-s ours`` はあれども ``-s theirs`` というオプションはありません。
結局 source側から ``-s ours`` を使ってマージすることにしました。
さらに、コマンドが失敗したら終わりにしたいから ``set -eu`` したり、
``&> /dev/null`` というようにトークンを使うコマンドの出力を、
/dev/null送りの刑に処したりいろいろありましたが出来ました。

TravisCIのビルドがなぜかキャッシュを読み込まず、
一からライブラリをビルドしようとしていて落ちました。

********************
TravisCIのキャッシュ
********************

原因はデフォルトブランチを master から develop に置き換えていたことでした。
このため、デフォルトブランチを元に戻さないといけませんでした。

TravisCIは通常のコミットに対するビルドの時、
第一にそのブランチに付随するキャッシュを読み込もうとします。
それがなかったら次にデフォルトブランチのキャッシュを読み込もうとします。
（今までは何となく派生元ブランチだと思っていました）

そして、masterブランチにはあらかじめ用意して置いたキャッシュがあります。
今までのすべてのビルドはこのキャッシュを読み込んでいたのでした。
そして、developブランチのキャッシュは存在しなかったため、
置き換えたときにビルドが失敗するようになったのです。
しかし、私は全てのブランチにキャッシュがあるから大丈夫だと思っていました。

ないのにあると思っていたわけは何か。

| あるブランチでのビルドが、別のブランチのキャッシュを読み込んでビルドしたとき、
| そのキャッシュは改めてそのブランチのキャッシュとして追加される。

こんな風に思っていたためでした。

つまり、masterブランチのキャッシュが soruceブランチ、developブランチ、
そのほかのキャッシュとして伝道されていくイメージでした。

しかし、そのビルドにおいてキャッシュに変更がないとき、
そのブランチのキャッシュとして追加されません。

developブランチのキャッシュは存在しないままでした。
