# 初めて Lua を使った

MediaWiki にはテンプレートを Lua で書く機能がある。 [Scribunto](https://www.mediawiki.org/wiki/Extension:Scribunto) 拡張で提供される機能だ。分岐や繰り返しなどは、元々のテンプレートの範囲でも出来なくもないが、非常に読みづらいものになってしまう。

そのため、出典を示すために使うテンプレートを Lua で書こうというプロジェクトを始めた。[巨大数 Wiki](https://kyodaisuu.fandom.com/ja/Main_page) で使用するためのものである。

実際のソースコードは [citation.lua](https://github.com/Hexirp/mwcite/blob/75402bcc0fff52db0703c4731f355ded7e8b22b2/citation.lua) のようになった。手続き的であるが、それほど非直感的な点はなく、普通の言語といった印象である。

これからも気が向いたら開発を進めていきたい。
