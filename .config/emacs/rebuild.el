(load user-init-file)

(require 'elpaca)

(message "Processing Elpaca queues...")
(elpaca-process-queues)
(elpaca-wait)

(message "Updating packages...")
(elpaca-update-all)
(elpaca-process-queues)
(elpaca-wait)

(let* ((elpaca-dir
        (expand-file-name "elpaca/repos" user-emacs-directory))
       (files
        (directory-files-recursively
         elpaca-dir
         "\\.el\\'")))

  (message "Native compiling %d files..." (length files))

  (dolist (file files)
    (condition-case err
        (native-compile file)
      (error
       (message "Failed %s: %s" file err)))))

(message "Finished.")
(kill-emacs 0)
