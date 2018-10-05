(clear-abbrev-table global-abbrev-table)

;; common auto correction like abbrevs
(define-abbrev-table 'global-abbrev-table
  '(
  ("abbout" "about")
  ("abotu" "about")
  ("abouta" "about a")
  ("aboutit" "about it")
  ("aboutthe" "about the")
  ("namepsace" "namespace")
  ("inlcude" "include")
  ("ustd" "using namespace std;")

  ;; emacs regex
  ("8d" "\\([0-9]+?\\)")
  ("8str" "\\([^\"]+?\\)\"")

  ("8inf" "∞")
  ("8smly" "☺")
  ("8luv" "♥")
  ("8dia" "♦")
  ("8spade" "♠")
  ("8club" "♣")
  ("8ge" "≥")
  ("envv" "environment variable")
  ("eenv" "environment")
  ("tf" "testing framework")
  ("retrun" "return")
  ("reutrn" "return")
  ("messges" "messages")
  ("messge" "message")

))


;; stop asking whether to save newly added abbrev when quitting emacs
(setq save-abbrevs nil)

;; turn on abbrev mode globally
(setq-default abbrev-mode t)
