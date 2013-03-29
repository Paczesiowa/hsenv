(defvar hsenv-active-environment nil)

(defconst hsenv-path-prepend-file "path_var_prependix")
(defconst hsenv-ghc-package-path-file "ghc_package_path_var")

(defun hsenv-valid-dirp (hsenv-dir)
  (let ((valid (and (file-accessible-directory-p hsenv-dir)
                    (file-readable-p
                     (concat hsenv-dir hsenv-path-prepend-file))
                    (file-readable-p
                     (concat hsenv-dir hsenv-ghc-package-path-file)))))
    (when (not valid)
      (message "The environment you provided is not a valid hsenv directory (%s)."
               hsenv-dir))
    valid))

(defun hsenv-is-not-active ()
  (let ((is-not-active (not hsenv-active-environment)))
    (when (not is-not-active)
      (message "An hsenv is already activated (%s)."
               (assoc-default 'dir hsenv-active-environment)))
    is-not-active))

(defun hsenv-is-active ()
  (let ((is-active hsenv-active-environment))
    (when (not is-active)
      (message "No hsenv currently activated."))
    is-active))

(defun hsenv-read-file-content (hsenv-dir file)
  (with-temp-buffer
    (insert-file-contents (concat hsenv-dir file))
    (buffer-string)))

(defun hsenv-activate-environment (hsenv-dir)
  "Activate the Virtual Haskell Environment in directory HSENV-DIR"
  (when (and (hsenv-valid-dirp hsenv-dir)
             (hsenv-is-not-active))
    ; Create an hsenv active environment and backup paths
    (setq hsenv-active-environment (list `(path-backup . ,(getenv "PATH"))
                                         `(exec-path-backup . ,exec-path)
                                         `(dir . ,hsenv-dir)))
    ; Prepend paths
    (let* ((path-prepend (hsenv-read-file-content hsenv-dir
                                                  hsenv-path-prepend-file)))
      (setenv "PATH" (concat  path-prepend ":" (getenv "PATH")))
      (setq exec-path (append (split-string path-prepend ":") exec-path)))
    ; Set ghc-package
    (setenv "GHC_PACKAGE_PATH"
            (hsenv-read-file-content hsenv-dir hsenv-ghc-package-path-file))
    (message "Environment activated: %s" hsenv-dir)))

(defun hsenv-env-name-from-dir (directory)
  "Return the name of an environment based on DIRECTORY."
  (let ((offs (string-match "[.]hsenv_\\([^\\/]*\\)$" directory)))
    (cond
     (offs
      (substring directory (+ 7 offs)))
     ((string-match "[.]hsenv$" directory)
      "(default)")
     (t
      (error "Not an hsenv directory %s" directory)))))

;;; Tests:
;; (and (equal "foo" (hsenv-env-name-from-dir "/home/bar/baz/.hsenv_foo"))
;;      (equal "foo" (hsenv-env-name-from-dir "/home/bar/.hsenv_boo/baz/.hsenv_foo"))
;;      (equal "(default)"
;;             (hsenv-env-name-from-dir "/home/bar/.hsenv_boo/baz/.hsenv")))

(defun hsenv-make-env (directory)
  (cons (hsenv-env-name-from-dir directory) directory))

(defun hsenv-env-name (env)
  (car env))

(defun hsenv-env-dir (env)
  (cdr env))

(defun hsenv-deactivate ()
  "Deactivate the Virtual Haskell Environment"
  (interactive)
  (when (hsenv-is-active)
    ; Restore paths
    (setenv "PATH" (assoc-default 'path-backup hsenv-active-environment))
    (setq exec-path (assoc-default 'exec-path-backup hsenv-active-environment))
    ; Destroy the hsenv active environment
    (let ((old-dir (cdr (assoc 'dir hsenv-active-environment))))
      (setq hsenv-active-environment nil)
      (message "Environment deactivated: %s" old-dir))))

(defun hsenv-activate-dir (dir)
  (let ((environments (hsenv-list-environments dir)))
    (if (null environments)
        (message "Directory %s does not contain any hsenv." dir)
      (let* ((env-name
              (if (= 1 (length environments))
                  (hsenv-env-name (car environments))
                (completing-read "Environment:"
                                 (mapcar #'hsenv-env-name environments))))
             (env (assoc env-name environments)))
        (let* ((hsenv-dir-name (hsenv-env-dir env))
               (hsenv-dir (file-name-as-directory hsenv-dir-name)))
          (hsenv-activate-environment hsenv-dir))))))

(defun hsenv-list-environments (dir)
  "Returns an assoc list of all environments avaliable in DIR.

The assoc list contains pairs of the form (NAME . DIRECTORY)."
  (let ((hsenv-dirs (append (file-expand-wildcards (concat dir ".hsenv"))
			    (file-expand-wildcards (concat dir ".hsenv_*")))))
    (mapcar #'hsenv-make-env hsenv-dirs)))

(defun hsenv-activate (&optional select-dir)
  "Activate a Virtual Haskell Environment"
  (interactive "P")
  (if (or select-dir
          (null (hsenv-list-environments default-directory)))
      (hsenv-activate-dir (read-directory-name "Directory:"))
    (hsenv-activate-dir default-directory)))

(provide 'hsenv)
