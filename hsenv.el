(setq hsenv-activated nil)
(setq hsenv nil)
(setq hsenv-path-backup nil)
(setq hsenv-exec-path-backup nil)

(defun hsenv-read-file (fpath)
  (with-temp-buffer
    (insert-file-contents fpath)
    (buffer-string)))

(defun hsenv-activate (dir env_name)
  "Activate the Virtual Haskell Environment named ENV_NAME in project directory DIR"
  (interactive "Dproject directory: \nsenvironment name: ")
  (when (string-match "^.*/$" dir)
    (setq dir (substring dir 0 -1)))
  (let* ((hsenv-dir (concat dir "/.hsenv_" env_name "/"))
         (path-var-prependix-location (concat hsenv-dir "path_var_prependix"))
         (ghc-package-path-var-location (concat hsenv-dir "ghc_package_path_var"))
         (path-var-prependix (hsenv-read-file path-var-prependix-location))
         (ghc-package-path-var (hsenv-read-file ghc-package-path-var-location))
         (new-path-var (concat path-var-prependix ":" (getenv "PATH")))
         (exec-path-prependix (split-string path-var-prependix ":")))
    (setq hsenv-path-backup (getenv "PATH"))
    (setenv "PATH" new-path-var)
    (setq hsenv-exec-path-backup exec-path)
    (setq exec-path (append exec-path-prependix exec-path))
    (setenv "GHC_PACKAGE_PATH" ghc-package-path-var)
    (setq hsenv-activated t)))

(defun hsenv-deactivate ()
  "Deactivate the Virtual Haskell Environment"
  (interactive)
  (when hsenv-activated
    (setenv "PATH" hsenv-path-backup)
    (setq exec-path hsenv-exec-path-backup)
    (setenv "GHC_PACKAGE_PATH" nil)
    (setq hsenv nil)
    (setq hsenv-path-backup nil)
    (setq hsenv-exec-path-backup nil)
    (setq hsenv-activated nil)))

(provide 'hsenv)
