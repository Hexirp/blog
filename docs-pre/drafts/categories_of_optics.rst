####################
Categories of Optics
####################

Optics は「光学機器」という意味で、Lens, Prism, Iso, ... などの総称であること
から名付けられた。

Lens のセマンティックは、どんどん進化している。その最新版であるらしい論文、
Categories of Optics を読んでみた。上層を掠め取ることしかできなかった。

つまり、このようなことである。

.. code-block:: haskell

 type Lens s t a b = exists m, (s -> (m, a), (m, b) -> t)

*****
Prism
*****

...

***
Iso
***

...

*********
Traversal
*********

...
