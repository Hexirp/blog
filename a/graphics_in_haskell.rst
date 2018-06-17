#############
Haskellでの絵
#############

HaskellでGraphics Libraryを探して苦労する話です。

.. warning::

 この記事はイライラを昇華したものです。

**********
事の始まり
**********

「そうだ、ライフゲームを作ろう。」

まず、私自身いくら理論寄りを自負していたとしても、
ウィンドウを出すプログラムを一度も作ったことがないのは、
一端のプログラマーとしてやばいかなと思っていたことが始まりです。

そこで、私はライフゲームに目を付けました。
何よりもルールが単純。それでいて出来るものはとても複雑で豊か。
初めてのグラフィックスとしては申し分ないのではという考えでした。

*****
gloss
*****

まず、ライブラリに何を使おうか考える必要があります。
これは普通にコミュニティに聞いてみることにしました。

\ `Haskell-jp`_\ は日本でHaskellをする人のためのコミュニティで、
自由に質問が出来できるSlackチーム、およびredditチャンネルを持っているのですが、
今回は人が多い方のSlackを使いました。（議論になりそうなものはredditを推奨）

.. _Haskell-jp: https://haskell.jp/

答えは\ `gloss`_\ 、私も以前から知っていたライブラリです。
そこまで本格的ではなく真面目に使うには至らない部分はあるものの、
簡単にシミュレーションやゲームをウィンドウに表示できることを、
私はよさそうに思っていて、回答者もまた推していました。

さあ、さっそく使おうと円を表示するサンプルを入力してコンパイルすると、
こんなエラーが……。ソースコードはこの状態（\ `ceaa10c76b`_\ ）でした。

.. _ceaa10c76b: https://github.com/Hexirp/hasga/tree/ceaa10c76b078ab856b22c9f98a08dbef1c8c15a

.. code-block:: none

 hasga-exe.EXE: user error (unknown GLUT entry glutInit)

調べてみると公式のQ&Aに対処法が載っていました。
\ ``glut32.dll``\ をインストールしてとあるフォルダに入れるか、
実行ファイルと同じところに置けといいます。

それがいったい何なのか調べてみると、\ `OpenGL Utility Toolkit`_\ のことで、 
\ `gloss`_\ が依存する\ `GLUT`_\ のバインディング先のようでした。
今は開発が止まっているので\ `GLUT`_\ もまた\ `freeglut`_\ という、
後続？派生？ライブラリを使っています。

.. _OpenGL Utility Toolkit: https://ja.wikipedia.org/wiki/OpenGL_Utility_Toolkit
.. _GLUT: http://hackage.haskell.org/package/GLUT

つまるところ、\ `freeglut`_\ をインストールすればいいわけです。
しかし、そのインストールのために自分でビルドする必要がありました。
Microsoft Visual Studioをインストールして、
CMakeをインストールして、それをビルドする必要があり、
もしかしたらこれが普通なのかもしれないのですが、
私にはめんどくさく感じられました。

疲れたので一回休み。

気分転換に他のプロジェクトをやろうとしたら、
おかしなことにこんなエラーがたくさん出てきたのです。

.. code-block:: none

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

いろいろ調べていたら見つけたこんな如何にもというようなコマンド
\ ``rm -rf $(stack path --stack-root)/precompiled``\ で直りました。
（元ソース\ `完全なリビルド`_\ では三番目の手法にあたるもので、
二つのコマンドなのですが一番最初が重要な雰囲気なので抜き出しました。
もしこれでだめだったらもう一つのコマンドも試してみてください。）

.. _完全なリビルド: https://haskell.e-bigmoon.com/stack/tips/full-rebuild.html

疲れたので一回休み。

\ `gloss`_\ のドキュメントをぶらぶらしているとこんな記述を見つけました。
フラグ\ ``glfw``\ 、これを有効にすると\ `glut`_\ の代わりに\ `glfw`_ \を使う。
\ `glfw`_\ は\ `glut`_\ に取って代わっているというライブラリです。
なんと、Windowsのためのプリコンパイルバイナリが配布されていました！
そのフラグをセットしてさっそくビルドするとなんとコンパイルできない。

\ `修正版`_\ を使ってビルドを通すために色々した結果が\ `このエラー`_\ でした。

.. _修正版: https://github.com/benl23x5/gloss/pull/41
.. _このエラー: https://travis-ci.org/Hexirp/hasga/builds/393054588

疲れたので一回休み。

.. _gloss: http://hackage.haskell.org/package/gloss
.. _glut: https://www.opengl.org/resources/libraries/glut/
.. _freeglut: http://freeglut.sourceforge.net/
.. _glfw: http://www.glfw.org/
