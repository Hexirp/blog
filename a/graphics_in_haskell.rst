#############
Haskellでの絵
#############

ただの、HaskellでGraphicsをやってみる話です。

**********
事の始まり
**********

いくら理論寄りを自負していたとしても、
ウィンドウを出すプログラムを一度も作ったことがないのは、
一端のプログラマーとしてやばいかな？

そう思ってグラフィックスプログラムを始めることにしました。

何を作るかという問題もあります。私はライフゲームに目を付けました。
何よりもルールが単純。それでいて出来るものはとても複雑で豊かかつ、視覚的。
初めてのグラフィックスとしては申し分ないのではという考えです。

****************
ライブラリの選択
****************

初めにライブラリを選ばないといけないのですが、
私はグラフィックスに関しては初心者なので、
普通に他の人に聞いてみました。

\ `Haskell-jp`_\ は日本でHaskellをする人のための集まりで、
自由に質問が出来できるSlackチーム、およびredditチャンネルを持っています。
今回は人が多いSlackを使いました。（議論になりそうなものはredditを推奨のこと）

答えは\ `gloss`_\ でした。私も以前から知っていたライブラリです。

簡単な特徴：

* そこまで本格的ではなく真面目に使うには至らない
* 簡単にシミュレーションやゲームをウィンドウに表示できる
* 主にシミュレーションの結果を表示する用途が多いらしい

さっそく使ってみることにします。

.. _Haskell-jp: https://haskell.jp/
.. _gloss: https://hackage.haskell.org/package/gloss

*******************************
glossのサンプルが起こしたエラー
*******************************

円を表示するサンプルを入力してコンパイルして実行すると、
このようなエラーが出て何も表示されないまま終了しました。
ソースコードはこの状態（\ `ceaa10`_\ ）でした。

.. code-block:: text

 hasga-exe.EXE: user error (unknown GLUT entry glutInit)

調べてみると\ `glossのホームページ`_\ のQ&Aに対処法が載っていました。

 | **Q: On Windows, when I try to run a gloss program it says**
   ``user error (unknown GLUT entry glutInit)``\ **.**
 | A1: You need to install ``glut32.dll``.
   Copy it into ``\Windows\System32`` (for 32-bit installs)
   or ``\Windows\SysWOW64`` (for 64-bit installs).
   Alternatively, you can just copy it into the same directory
   as the Main.exe executable you are trying to run.

「\ ``glut32.dll``\ をインストールして、
システムに関わっていそうな何某のフォルダに入れるか、
実行ファイルと同じところに置かないといけない」

まず、\ ``glut32.dll``\ がいったい何なのか調べてみると、
\ `GLUT`_\ のものでした。

どうしてこれが必要になるのか調べると、
\ `gloss`_\ が依存する\ `GLUT (haskell)`_\ のせいでした。

ならば、言う通りglutをインストールすればいいのかと思いましたが、
今は開発が止まっていて\ `freeglut`_\ に引き継がれているようです。
GLUT (haskell)もまたfreeglutを利用できます。

つまるところ、freeglutをインストールすればいいわけです。
しかし、そのインストールのために自分でコンパイルする必要がありました。

必要な物：

* `Microsoft Visual Studio`_
* `CMake`_

CMakeを使ってビルドするだけです。

昔は全ての分野で、そして今でも一部の分野ではこれが普通なのでしょう。
しかし、stackやgradleというぬるま湯で育った私にはめんどくさく感じられました。

.. _ceaa10:
 https://github.com/Hexirp/hasga/tree/ceaa10c76b078ab856b22c9f98a08dbef1c8c15a
.. _glossのホームページ: http://gloss.ouroborus.net/
.. _OpenGL Utility Toolkit: https://ja.wikipedia.org/wiki/OpenGL_Utility_Toolkit
.. _GLUT: https://www.opengl.org/resources/libraries/glut/
.. _GLUT (haskell): https://hackage.haskell.org/package/GLUT
.. _freeglut: http://freeglut.sourceforge.net/
.. _Microsoft Visual Studio: https://visualstudio.microsoft.com/
.. _CMake: https://cmake.org/

****************************
他のプロジェクトで出たエラー
****************************

他のプロジェクトをやろうとしたら、おかしなことにこんなエラーが出てきました。

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

このエラーを解決するために色々調べました。
キャッシュが壊れているんじゃないかと予想はついたので、
最終手段はstackのルートフォルダを消してしまうことだと考えました。

それはできるだけ取りたくないので、回避するためにいろいろ調べていたら、
\ `完全なリビルド`_\ という記事を見つけました。

それに記載されているこのようなコマンドを打ち込んだら直りました。

.. code-block:: bash

 rm -rf $(stack path --stack-root)/precompiled
 rm -rf $(stack path --stack-root)/snapshots

それ自体にかなりの時間がかかりますし、キャッシュがないため、
これを入力した後のコンパイルはさらに時間がかかりますので注意してください。

.. note::

 後で分かったことなのですが、私がびびっていただけでした。
 このエラーが出ていてもコンパイルは可能なようです。
 なので、特に直さないといけないという訳ではないようです。

 また、TravisCIでも再現しているのでWindows固有の現象だったり、
 アンチウイルスソフトによるものだという線はなくなっています。

.. _完全なリビルド: https://haskell.e-bigmoon.com/stack/tips/full-rebuild.html

************************
バックエンドにglfwを使う
************************

\ `gloss`_\ のドキュメントを見ているとこんな記述を見つけました。

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

\ ``glfw``\ フラグを有効にすると、
GLUTの代わりに\ `GLFW`_\ を使うということです。
GLFWはGLUTの開発が止まった後の主流になったライブラリのようです。

なん、Windowsのためのプリコンパイルバイナリが配布されていました！
つまり、自分でコンパイルする必要がないということです。

さっそく、そのフラグをセットして、
まずTravisCI上で、ということでプッシュしたら、
なんとコンパイルできませんでした。
\ `有志による修正版`_\ を使ったらビルドは通ったんですが、
ここまで来るまでに気力をそがれました。

どこかで、Cabalのフラグというシステムは使っていけないというのを見たんですが、
ここで実例を見ることになると思いませんでした。
最初は\ ``gloss-glfw``\ という風に分けていたらしいから、
そのままでよかったと思います。

.. _GLFW: http://www.glfw.org/
.. _有志による修正版: https://github.com/benl23x5/gloss/pull/41

****
sdl2
****

\ `とある記事`_\ で\ `sdl2`_\ を見つけました。
READMEを見てみると、使われているCライブラリを、
stackだけでインストールできるといいます！
（\ `Windows SDL2 is now almost painless via stack`_\ ）

どうやらstackはここ（\ `Index of /mingw/x86_64/`_\ ）にあるものを
インストールできるようです。（sandboxの中で！）
そして、とうとうHaskellでウィンドウを表示させることが出来ました！
（ソースコードは\ `27b3ce`_\ )

今までが嘘かのようにすんなりいったので感動するしかありませんでした。

.. _sdl2: https://hackage.haskell.org/package/sdl2
.. _とある記事: https://myuon.github.io/posts/refluxible-library/
.. _Windows SDL2 is now almost painless via stack:
 https://www.reddit.com/r/haskellgamedev/comments/4jpthu/
.. _Index of /mingw/x86_64/: http://repo.msys2.org/mingw/x86_64/
.. _27b3ce:
 https://github.com/Hexirp/hasga/tree/27b3cee11f149fb1191b50f285cf1ff0011c5fcb

*************************
glossでも同じようにできた
*************************

\ `sdl2`_\ で可能なCライブラリのインストール方法ですが、
\ `Index of /mingw/x86_64/`_\ の中にfreeglutがあったので、
こんな風にインストールしてみたらglossでも出来ちゃいました。
（ソースコードは\ `e8fdcf`_\ ）

.. code-block:: bash

 stack exec -- pacman -S mingw-w64-x86_64-freeglut

かなり有用であると思われますので、
このインストール方法について詳しく書き留めます。
参考にするのならば自己責任でお願いします。

原理
 Windows版のstackは内部にMSYS2というソフトを持っている。
 Windowsでshellをやるためのソフトで、
 stackはこれをサンドボックス環境としている。
 おそらく、\ ``stack exec -- <command>``\ としたときに、
 この環境の中で実行されるのだと思う。

 ここで、MSYS2はもっと深くshellの動作を模擬することが出来て、
 例えば、ライブラリのインストールを行う\ ``pacman``\ が使える。
 そして、実際にそのサンドボックス環境にライブラリがインストールされる。
 インストールできるライブラリは http://repo.msys2.org/msys/x86_64/ や
 http://repo.msys2.org/mingw/x86_64/ にあるものだと思う。

やり方
 最初にライブラリの更新をする。
 \ ``stack exec -- pacman -Syu``\ を実行する。
 私の場合はこれが失敗して何度か実行する必要があった。

 次に、欲しいライブラリをインストールする。
 \ ``stack exec -- pacman -S <library>``\ を実行する。

注意点
 glossの場合はCライブラリが実行時に必要になるので\ ``stack install``\ しても、
 freeglutはサンドボックス環境にしかないため実行できないと思う。

 また、sdl2はこのインストール形式に対して特別に対応を行っているのに対して、
 glossが使うGLUTは対応していないので「エラーがあるけど一応動く」という状態に
 なってしまう。

この手順はMSYS2について調べればもっとよくできると思うし、
エラーが起きたとしてもMSYS2についての知識があれば対応できると思いますので、
MSYS2についてもっと調べてみたいと思います。
