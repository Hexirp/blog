###############
Haskellでの等式
###############

Haskell での等式に興味がわいたので調べてみただけの記事です。

****
概観
****

一覧というものはかっこいいですよね！

+-----------+------+--------+--------+---------+------------------+------------+
| 名前      | 型？ | 持上？ | 多型？ | 約振り  | TyCon に         | 定義箇所   |
|           | T/C  | L/U    |        |         | 組み込み？       |            |
+===========+======+========+========+=========+==================+============+
| (~#)      | T    | U      | hetero | nominal | eqPrimTyCon      | GHC.Prim   |
| (~~)      | C    | L      | hetero | nominal | hEqTyCon         | GHC.Types  |
| (~)       | C    | L      | homo   | nominal | eqTyCon          | "Equality" |
| (:~:)     | T    | L      | homo   | nominal | (not built-in)   | "Equality" |
| (:~~:)    | T    | L      | hetero | nominal | (not built-in)   | "Equality" |
|           |      |        |        |         |                  |            |
| (~R#)     | T    | U      | hetero | repr    | eqReprPrimTy     | GHC.Prim   |
| Coercible | C    | L      | homo   | repr    | coercibleTyCon   | GHC.Types  |
| Coercion  | T    | L      | homo   | repr    | (not built-in)   | "Coercion" |
|           |      |        |        |         |                  |            |
| (~P#)     | T    | U      | hetero | phantom | eqPhantPrimTyCon | GHC.Prim   |
+-----------+------+--------+--------+---------+------------------+------------+

まず、それぞれの行に書かれている性質を解説しましょう。

名前
====

それぞれの等式を表す型の名前です。二項演算子は括弧をつけていますが、
特別な構文であるため実際には括弧をつけて関数に変換できないものもあります。
