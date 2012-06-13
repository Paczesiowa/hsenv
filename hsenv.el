(setq hsenv-activated nil)
(setq hsenv nil)
(setq hsenv-path-backup nil)
(setq hsenv-exec-path-backup nil)

(defun hsenv-read-file (fpath)
  (with-temp-buffer
    (insert-file-contents fpath)
    (buffer-string)))

(defun hsenv-activate-environment (hsenv-dir)
  "Activate the Virtual Haskell Environment in directory HSENV-DIR"
  (let* ((path-var-prependix-file (concat hsenv-dir "path_var_prependix"))
         (ghc-package-path-var-file (concat hsenv-dir "ghc_package_path_var")))
    (if (and (file-accessible-directory-p hsenv-dir)
             (file-readable-p path-var-prependix-file)
             (file-readable-p ghc-package-path-var-file))
        (let* ((path-var-prependix (hsenv-read-file path-var-prependix-file))
               (ghc-package-path-var (hsenv-read-file ghc-package-path-var-file))
               (new-path-var (concat path-var-prependix ":" (getenv "PATH")))
               (exec-path-prependix (split-string path-var-prependix ":")))
          (if (not hsenv-activated)
              (progn
                (setq hsenv-path-backup (getenv "PATH"))
                (setenv "PATH" new-path-var)
                (setq hsenv-exec-path-backup exec-path)
                (setq exec-path (append exec-path-prependix exec-path))
                (setenv "GHC_PACKAGE_PATH" ghc-package-path-var)
                (setq hsenv hsenv-dir)
                (setq hsenv-activated t))
            (message "An hsenv is already activated (%s)." hsenv)))
    (message "The environment you provided is not a valid hsenv directory (%s)." hsenv-dir))))

(defun hsenv-activate-default-environment (dir)
  "Activate the Virtual Haskell Environment named after DIR in directory DIR"
  (setq dir (file-name-as-directory dir))
  (let* ((dir-name (directory-file-name dir))
         (env-name (file-relative-name dir-name (file-name-directory dir-name)))
         (hsenv-dir-name (concat dir ".hsenv_" env-name))
         (hsenv-dir (file-name-as-directory hsenv-dir-name)))
    (hsenv-activate-environment hsenv-dir)))

(defun hsenv-activate (&optional specify-env)
  "Activate a Virtual Haskell Environment"
  (interactive "P")
  (if specify-env
      (let* ((dir (read-directory-name "Project directory:"))
             (candidates (file-expand-wildcards (concat dir ".hsenv_*"))))
        (if candidates
            (hsenv-activate-environment (completing-read "Environment:" candidates)))
        (message "Directory (%s) does not contain any hsenv environments."))
    (let ((dir (read-directory-name "Project directory:")))
      (hsenv-activate-default-environment dir))))

(defun hsenv-deactivate ()
  "Deactivate the Virtual Haskell Environment"
  (interactive)
  (if hsenv-activated
      (progn
        (setenv "PATH" hsenv-path-backup)
        (setq exec-path hsenv-exec-path-backup)
        (setenv "GHC_PACKAGE_PATH" nil)
        (setq hsenv nil)
        (setq hsenv-path-backup nil)
        (setq hsenv-exec-path-backup nil)
        (setq hsenv-activated nil))
    (message "No hsenv currently activated.")))

(provide 'hsenv)
