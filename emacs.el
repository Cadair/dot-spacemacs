;; Configuration specific to the "emacs" hostname
;; I run a server which lets me access my org mode files with emacsclient over TCP

(setq server-use-tcp t)
(setq server-host emacs)
