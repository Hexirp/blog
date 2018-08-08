---
title: Haskellでの絵
...

#############
Haskellでの絵
#############

ただの Haskell で Graphics をやろうとして起こったことを書き留める日記のような、
出来事や起こった問題とその解決方法を並べて書いていくものです。

**********
事の始まり
**********

いくら理論寄りを自負していたとしても「ウィンドウを出すプログラム」を
一度も作ったことがないのは一端のプログラマーとしてやばいのでは。
そう思ってグラフィックスプログラムを始めることにしました。

何を作るか。私はライフゲームを作ることを目標にしました。
何よりもルールが単純。それでいて出来るものはとても複雑で豊かかつ、視覚的。
初めてのグラフィックスとしては申し分ないのではという考えです。

****************
ライブラリの選択
****************

何のライブラリを使おうか。

グラフィックスのライブラリは抽象化されていないラッパーがほとんどで
組み合わせることが出来ないため、どれか一つ選ばないといけません。
私はグラフィックスに関しては初心者で、信念とかないので他の人に聞いてみました。

`Haskell-jp`_ は日本で Haskell をする人のための集まりです。
自由に質問が出来る Slackチーム、および redditチャンネルを持っています。
今回は人が多い Slack を使いました。（議論になりそうな質問は reddit を推奨）

返ってきた答えは `gloss`_ を薦めるものでした。私も以前から知っていました。
二重振り子のシミュレーションを表示するのに使われたりしているのを見ていたからで、
というか答えた人がそのシミュレーションを作った方でした。

簡単な特徴：

* そこまで本格的ではなく真面目に使うには至らない
* 簡単にシミュレーションやゲームをウィンドウに表示できる
* 主にシミュレーションの結果を表示する用途が多いらしい

さっそく使ってみることにします。

.. _Haskell-jp: https://haskell.jp/
.. _gloss: https://hackage.haskell.org/package/gloss

********************************
gloss のサンプルが起こしたエラー
********************************

gloss のチュートリアルにあった円を表示するサンプルをコンパイルして実行すると、
このようなエラーが出て何も表示されないまま終了してしまいました。
その時のソースコードはこのような状態 (`ceaa10`_) でした。

.. code-block:: text

 hasga-exe.EXE: user error (unknown GLUT entry glutInit)

調べてみると `gloss のホームページ`_\ のQ&Aに対処法が載っていました。

 | **Q: On Windows, when I try to run a gloss program it says**
   ``user error (unknown GLUT entry glutInit)``\ **.**
 | A1: You need to install ``glut32.dll``.
   Copy it into ``\Windows\System32`` (for 32-bit installs)
   or ``\Windows\SysWOW64`` (for 64-bit installs).
   Alternatively, you can just copy it into the same directory
   as the Main.exe executable you are trying to run.

要約すると「 ``glut32.dll`` をインストールして何某のフォルダに入れるか
実行ファイルと同じところに置かないといけない」という感じです。

まず、\ ``glut32.dll`` がいったい何なのか調べてみると `GLUT`_ のものでした。
さらに、どうしてこれが必要になるのか調べると、\ `gloss`_ が依存する
`GLUT (haskell)`_ が GLUT を FFI で使っているためでした。

ならば、言う通り glut をインストールすればいいのかと思いましたが
今は開発が止まっていて `freeglut`_ に引き継がれているようです。
GLUT (haskell) もまた freeglut を使えるようになっていました。

つまるところ、freeglut をインストールすればいいわけです。
しかし、そのインストール方法は自分でコンパイルすることでした。

必要な物：

* `Microsoft Visual Studio`_
* `CMake`_

CMake を使ってビルドするらしいです。一部の分野ではこれが普通なのでしょうか。
しかし、stack や gradle というぬるま湯で育った私にはめんどくさく感じられました。

.. _ceaa10:
 https://github.com/Hexirp/hasga/tree/ceaa10c76b078ab856b22c9f98a08dbef1c8c15a
.. _gloss のホームページ: http://gloss.ouroborus.net/
.. _OpenGL Utility Toolkit: https://ja.wikipedia.org/wiki/OpenGL_Utility_Toolkit
.. _GLUT: https://www.opengl.org/resources/libraries/glut/
.. _GLUT (haskell): https://hackage.haskell.org/package/GLUT
.. _freeglut: http://freeglut.sourceforge.net/
.. _Microsoft Visual Studio: https://visualstudio.microsoft.com/
.. _CMake: https://cmake.org/

****************************
他のプロジェクトで出たエラー
****************************

頭を切り替えて他のプロジェクトをやろうとしたらこんなエラーが出てきました。

.. code-block:: text

 primitive-0.6.3.0: using precompiled package
 ghc-pkg: cannot find package microlens-0.4.8.3
 ghc-pkg: cannot find package mtl-2.2.2
 ghc-pkg: cannot find package stm-2.4.5.0
 ghc-pkg: cannot find package text-1.2.3.0
 async-2.1.1.1: using precompiled package
 hashable-1.2.7.0: using precompiled package
 ghc-pkg: cannot find package transformers-compat-0.5.1.4
 ghc-pkg: cannot find package unliftio-core-0.1.1.0
 exceptions-0.8.3: using precompiled package
 ghc-pkg: cannot find package primitive-0.6.3.0
 ghc-pkg: cannot find package async-2.1.1.1
 vector-0.12.0.1: using precompiled package
 ghc-pkg: cannot find package hashable-1.2.7.0
 typed-process-0.2.2.0: using precompiled package
 unliftio-0.2.7.0: using precompiled package

stack の resolver を変更してのビルド時、今までのキャッシュはそのまま使えません。
パッケージの内容が変更されるためですが、キャッシュが再利用されるときがあります。
すなわち stack は、あるパッケージが依存するパッケージに変更がないのならば、
キャッシュに残っている古いそのパッケージのビルドをそのまま使います。
その時に起こるようです。このエラーを解決するために色々調べました。
起こる事象からキャッシュが壊れているんじゃないかと予想はつきました。
すると最終手段は stack のルートフォルダを消してしまうことだと考えられます。

それはできるだけ取りたくないため、回避するためにいろいろ調べていると
`完全なリビルド`_\ という記事を見つけました。

それに記載されているこのようなコマンドを打ち込んだら直りました。

.. code-block:: bash

 rm -rf $(stack path --stack-root)/precompiled
 rm -rf $(stack path --stack-root)/snapshots

それ自体にかなりの時間がかかりますし、キャッシュが削除されるため
これを入力した後のコンパイルはさらに時間がかかります。

.. note::

 後で分かったことなのですが、このエラーが出ていてもコンパイルは可能なようです。
 なので、直さないといけないという訳ではありません。ビビっていただけでした……。

 また、TravisCI の上でも再現しているので Windows 固有の現象だったり、
 アンチウイルスソフトによるものだという線はなくなっています。

.. _完全なリビルド: https://haskell.e-bigmoon.com/stack/tips/full-rebuild.html

**************************
バックエンドに glfw を使う
**************************

`gloss`_ のドキュメントを見ているとこんな記述を見つけました。

+---------------------+----------------------------+----------+-----------+
| Name                | Description                | Default  | Type      |
+=====================+============================+==========+===========+
| ``glut``            | Enable the GLUT backend    | Enabled  | Automatic |
+---------------------+----------------------------+----------+-----------+
| ``glfw``            | Enable the GLFW backend    | Disabled | Automatic |
+---------------------+----------------------------+----------+-----------+
| ``explicitbackend`` | Expose versions of display | Disabled | Automatic |
|                     | and friends that allow you |          |           |
|                     | to choose what window      |          |           |
|                     | manager backend to use.    |          |           |
+---------------------+----------------------------+----------+-----------+

``glfw`` フラグを有効にすると、GLUT の代わりに `GLFW`_ を使うということです。
GLFW は GLUT の開発が止まった後に主流になったライブラリであるようです。

なんと、Windowsのためのプリコンパイルバイナリが配布されています！
つまり、自分でコンパイルする必要がないということです。

さっそくそのフラグをセットしました。「まず TravisCI 上で」ということで
ビルドさせてみたのですが、GLUT パッケージがコンパイルできませんでした。
`有志による修正版`_ を使ったらビルドは通ったんですが気力をそがれました。

どこかで、Cabal のフラグというシステムは使っていけないというのを見たんですが、
実際に体験することになるとは思いませんでした。フラグそれぞれの組み合わせ全てで
テストすることは現実的に不可能であり、コンパイルできないコードが入り込みます。

.. _GLFW: http://www.glfw.org/
.. _有志による修正版: https://github.com/benl23x5/gloss/pull/41

****
sdl2
****

`とある記事`_ をきっかけとして `sdl2`_ を知りました。調べてみると
使われているCライブラリを stack だけでインストールできるといいます！
（\ `Windows SDL2 is now almost painless via stack`_\ ）

どうやら stack はここ（\ `Index of /mingw/x86_64/`_\ ）にあるものを
インストールできるようです。（サンドボックスの中で！）初耳でした。
そして、とうとう Haskell でウィンドウを表示させることが出来ました！
ソースコードはこの状態 (`27b3ce`_\) でした。

今までが嘘かのようにすんなりいったので感動しました。

.. _sdl2: https://hackage.haskell.org/package/sdl2
.. _とある記事: https://myuon.github.io/posts/refluxible-library/
.. _Windows SDL2 is now almost painless via stack:
 https://www.reddit.com/r/haskellgamedev/comments/4jpthu/
.. _Index of /mingw/x86_64/: http://repo.msys2.org/mingw/x86_64/
.. _27b3ce:
 https://github.com/Hexirp/hasga/tree/27b3cee11f149fb1191b50f285cf1ff0011c5fcb

**************************
gloss でも同じようにできた
**************************

上記のインストール方法ですが、\ `Index of /mingw/x86_64/`_ の中に freeglut が
あったため、こんな風にインストールしてみたら gloss でも使えました。
（ソースコードは `e8fdcf`_\ ）

.. code-block:: bash

 stack exec -- pacman -S mingw-w64-x86_64-freeglut

便利なのでこのインストール方法について知っていることを書き出しておきます。

原理
 Windows版の stack は内部に MSYS2 というソフトを持っている。
 Windows で shell をやるためのソフトであり、
 stack はこれをサンドボックス環境としている。
 おそらく、\ ``stack exec -- <command>`` としたときに、
 そのコマンドがこの環境の中で実行されるのだと思う。

 MSYS2 はもっと深く shell の動作を模擬することが出来て、
 例えば、ライブラリのインストールを行う ``pacman`` が使える。
 そして、実際にそのサンドボックス環境にライブラリがインストールされる。
 インストールできるライブラリは多分 http://repo.msys2.org/msys/x86_64/ や
 http://repo.msys2.org/mingw/x86_64/ にあるもの。

やり方
 最初に ``stack exec -- pacman -Syu`` を実行して既存のライブラリを更新する。
 失敗することがあるが、再実行すれば出来るはずである。
 次に欲しいライブラリをインストールする。
 ``stack exec -- pacman -S <library>`` を実行する。

注意点
 gloss の場合は freeglut が実行時に必要になるため、
 作ったソフトウェアを ``stack install`` して実行しても
 freeglut はサンドボックス環境にしかないためおそらくエラーになる。
 つまり、この方法では ``stack exec`` を使ってしか実行できない。
 また、gloss を使って作ったゲームは、exe を配布しても
 それぞれのプレイヤーが freeglut をインストールする必要がある。

 さらに、sdl2 はこのインストール形式に対して特別に対応を行っているのに対して、
 gloss が使う GLUT は対応していないので「エラーがあるけど一応動く」状態になり、
 下のようなメッセージが終了するたびに表示される。

 .. code-block:: text

  freeglut (hasga-exe.EXE): fgPlatformInitialize: CreateDC failed, Screen size info may be incorrect
  This is quite likely caused by a bad '-display' parameter
