;; -*- lexical-binding: t; -*-

(load "~/.config/emacs/init.el")

(message "Processing Elpaca queues...")
(elpaca-process-queues)
(elpaca-wait)

(when (fboundp 'elpaca-update-all)
  (message "Updating packages...")
  (elpaca-update-all)
  (elpaca-process-queues)
  (elpaca-wait))

(let ((builds-dir
       (expand-file-name "elpaca/builds"
                         user-emacs-directory)))

  (message "Compiling packages from %s" builds-dir)

  (dolist (file
           (directory-files-recursively
            builds-dir
            "\\.el\\'"))
    (condition-case err
        (native-compile file)
      (error
       (message "Failed %s: %S" file err)))))

(message "Finished.")
(kill-emacs 0)
