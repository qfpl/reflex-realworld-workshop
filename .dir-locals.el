;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((markdown-mode
  (markdown-command . "pandoc -c file:///home/bkolera/src/github/qfpl/reflex-realworld-workshop/docs.css  --lua-filter=/home/bkolera/src/github/qfpl/reflex-realworld-workshop/links-to-html.lua --from gfm --metadata pagetitle=\"Workshop Docs\" -t html5 --mathjax --highlight-style pygments --standalone")))
