(setq virthualenv nil)
(setq virthualenv-path-backup nil)
(setq virthualenv-exec-path-backup nil)

(defun virthualenv-read-file (fpath)
  (with-temp-buffer
    (insert-file-contents fpath)
    (buffer-string)))

(defun virthualenv-activate (dir)
  "Activate the Virtual Haskell Environment in DIR"
  (interactive "Dvirthualenv directory: ")
  (when (equal (char-list-to-string (last (string-to-list dir)))
               "/")
    (setq dir (subseq dir
                      0
                      (- (length dir) 1))))
  (let* ((virthualenv-dir (concat dir "/.virthualenv/"))
         (path-var-prependix-location (concat virthualenv-dir "path_var_prependix"))
         (ghc-package-path-var-location (concat virthualenv-dir "ghc_package_path_var"))
         (path-var-prependix (virthualenv-read-file path-var-prependix-location))
         (ghc-package-path-var (virthualenv-read-file ghc-package-path-var-location))
         (new-path-var (concat path-var-prependix ":" (getenv "PATH")))
         (exec-path-prependix (split-string path-var-prependix ":")))
    (setq virthualenv-path-backup (getenv "PATH"))
    (setenv "PATH" new-path-var)
    (setq virthualenv-exec-path-backup exec-path)
    (setq exec-path (append exec-path-prependix exec-path))
    (setenv "GHC_PACKAGE_PATH" ghc-package-path-var)
    (setq virthualenv dir)))

(defun virthualenv-deactivate ()
  "Deactivate the Virtual Haskell Environment"
  (interactive)
  (setenv "PATH" virthualenv-path-backup)
  (setq exec-path virthualenv-exec-path-backup)
  (setenv "GHC_PACKAGE_PATH" nil)
  (setq virthualenv nil)
  (setq virthualenv-path-backup nil)
  (setq virthualenv-exec-path-backup nil))

(provide 'virthualenv)
