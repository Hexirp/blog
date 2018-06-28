##################
このブログの作り方
##################

このブログを作った記録です。

******
Hakyll
******

私は記事が一つ出来たらGitHub Pagesで公開しようと考えていました。

ついさっき、記事が一つ完成したので作業を始めました。
Haskellで書かれた静的なウェブサイトのためのコンパイラ、
\ `hakyll`_\ が色々なところで使われていて定番といえるため。それを使います。

まず、依存関係に加えたのですがビルドに約1時間かかってしまった上に、
最終的にWindowsで発生するGHCそのもののバグで失敗してしまいました。

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

* https://github.com/jaspervdj/hakyll/issues/613
* https://ghc.haskell.org/trac/ghc/ticket/13112

hakyllをビルドしているときのメモリの様子を見ていると90%を超えています。
そのためただメモリが足りないためかもしれません。

TravisCIでやるとメモリに加えて時間もネックになってしまいそうなので、
\ ``stack dot``\ を使って依存関係をグラフに変換した後、
特に依存関係が多いライブラリを探すと、
この逆の順序でビルドさせるのがよさそうだと思います。

* hakyll
* pandoc
* wai-app-static
* wai-extra
* http-conduit
* tls

**************
デプロイの方法
**************

GitHub Pagesを使います。gh-pagesやmasterそのものを使う方法もあるのですが、
masterのdocsフォルダの内部を参照する方法でいきます。

開発はdevelopブランチで行いますが、開発途中の物が公開されるのは困ります。
安全のために、surceブランチにマージしないとデプロイされないようにします。

サイトのソースはdocs-preに置いて、docsへと変換させます。

************
テンプレート
************

上で書いたことの作業を進めていて、
TravisCIに依存関係をキャッシュさせるのは成功しました。

次に始めたのはテンプレート作りです。HTMLやCSSについて必死に調べて、
Grid LayoutとかFlexboxを活用した結果、なんかそれっぽいものが完成しました。

グリッドで上下に区切って、上側に細いヘッダーを作り、
ヘッダーの一番左にアイコンを置いて、その右にFlexboxで等間隔にリンクを置いて、
下側は記事にして、左右に余白を作って、見出しの下にラインを入れただけです。

**************
Hakyllのビルド
**************

いい加減、Hakyllを手元のWindowsが入ってるPCで動作させたいです。
手はいろいろ思いつきました。

そもそものバグを直す
 コンパイラという深いところで発生するバグ、しかしもメモリがらみなので、
 今の私の技術では無理でした。

 せめて情報を集めることでバグが早く直るように、貢献しようとしたのですが、
 stackの\ ``ghc-options=(options)``\ を渡して情報を出力させようとしても、
 情報が出力されないのかされているのかよくわかりませんでした。

GHCのメモリ使用量を減らす
 \ ``Limiting GHC Memory``\ とかいうキーワードで検索したりしたのですが、
 ほとんど情報は見つからず、無理なようです。

 Stack Overflowに投稿された「GHCのメモリ使用量を制限する方法は？」という質問に
 まったく回答が付いていないのが哀愁を感じさせました。

分割コンパイル
 Hakyllそのものを複数回に分けてコンパイル出来たらメモリ使用量も減るのでは、
 という考えを持ったのですが、普通にはできないようです。

 Hakyllに手を入れて、直接的に複数のライブラリに分けることを決めました。

フラグを操作してビルドするモジュールを減らす
 Hakyllはたくさんフラグを持っていて、Pandocを使わなかったり、
 プレビュー用のWebサーバーを使わなかったり、というときにフラグをオフにして、
 無駄なコンパイルがされないように出来ます。

 出来るだけフラグをオフにしたのですが、それでも無理でした。

ここで、有望そうなのはHakyllを複数のライブラリに分けることでした。

************
Hakyllの分割
************

コンパイルが失敗するのは\ ``Hakyll.Core.***``\ が終わってから、
\ ``Hakyll.Web.***``\ に入り始めたあたりなので、
そのあたりで分割することにしました。

まず、hakyll-coreというサブフォルダを作って、
\ ``Hakyll.Core``\ 以下のモジュールを全てそこに移動しました。
その後、cabalファイルもコピーしていらない設定を削除し始めたのですが、
どのファイルがいるのかいらないのかよくわからず進みませんでした。
そのため、テストとか実行ファイルとかWebサイトのサンプルは、
全て消した方がいいと思い直しました。ビルドしたいだけなのでいらないのです。

編集する前にリポジトリの状態を戻すコミットをした後、
ライブラリでないものをすべて削除してcabalファイルもそれに合わせて編集しました。
さらに、Paths_hakyllを消して、隠されたモジュールを全部公開しました。
後々困りそうなので先に処理しました。

その後、そもそものフォルダ構造をルートフォルダに一つのライブラリがある形から、
hakyll-coreとhakyllという二つのフォルダに二つのライブラリがある形にしました。

この時点で一回ビルドして、ファイルがないというエラーを見つけました。
本来は必要なのに消しすぎたということなので戻そうとしたのですが、
なぜか認識されませんでした。これはバグでした。
（\ https://github.com/jaspervdj/hakyll/pull/645\ ）

そして、メモリを使いすぎて落ちることなく、ビルドが成功しました！

ここまでの作業は\ https://github.com/jaspervdj/hakyll/compare/1abdeee743d65d96c6f469213ca6e7ea823340a7...2487d2ca77606da20986165ee57b3de22e311a02\ で見れます。
