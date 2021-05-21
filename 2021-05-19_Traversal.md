# Traversal 型クラスの二通りの形式化

## Traversal とは？

## 形式化とは？

## Hask 圏とは？

Hask 圏の対象は、 `Int`, `Maybe Int`, `[Bool]`, ... のような Haskell の型です。 `Maybe` などは Hask 圏の対象ではありません。

Hask 圏の射は、 Haskell の関数です。具体的に、対象 `a` から対象の `b` への射は、 `a -> b` という型の関数です。たとえば、 `not : Bol -> Bool` は `Bool` から `Bool` への射です。

## Applicative の形式的な定義をする。

## 1 番目の方法: Traversal を特別な自然変換を持つ関手として形式化する。

## 2 番目の方法: Traversal を特別な圏の自己関手として形式化する。
