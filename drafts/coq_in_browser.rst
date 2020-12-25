---
title: ブラウザで Coq
description: jsCoq を利用してブラウザで Coq をやります。ここで。
canonical: https://hexirp.github.io/articles/coq_in_browser.html
type: article
...

.. raw:: html

 <script
  src="https://x80.org/rhino-hott/ui-js/jscoq-loader.js"
  type="text/javascript" \>
 <script type="text/javascript">
  loadJsCoq(https://x80.org/rhino-hott/ui-js/)
   .then( () => new CoqManager ($list_of_ids, [$options]) );
 </script>

##############
ブラウザで Coq
##############
