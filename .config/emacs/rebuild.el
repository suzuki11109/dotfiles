;; -*- lexical-binding: t; -*-

(load "~/.config/emacs/init.el")

(package-refresh-contents)
(package-upgrade-all)

(let ((files
       (directory-files-recursively
        package-user-dir
        "\\.el\\'")))
  (dolist (file files)
    (native-compile file)))

(kill-emacs 0)
