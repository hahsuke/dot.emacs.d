(require 'cl-lib)

(defun set-drvfs-alist ()
  (interactive)
  (setq drvfs-alist
        (mapcar (lambda (x)
                  (when (string-match "\\(.*\\)|\\(.*?\\)/?$" x)
                    (cons (match-string 1 x) (match-string 2 x))))
                (split-string (concat
                               ;; //wsl$ or //wsl.localhost パス情報の追加
                               (when (or (not (string-match "Microsoft" (shell-command-to-string "uname -v")))
                                         (>= (string-to-number (nth 1 (split-string operating-system-release "-"))) 18362))
                                 (concat "/|" (shell-command-to-string "wslpath -m /")))
                               (shell-command-to-string
                                (concat
                                 "mount | grep -E 'type (drvfs|cifs)' | sed -r 's/(.*) on (.*) type (drvfs|cifs) .*/\\2\\|\\1/' | sed 's!\\\\!/!g';"
                                 "mount | grep 'aname=drvfs;' | sed -r 's/.* on (.*) type 9p .*;path=([^;]*);.*/\\1|\\2/' | sed 's!\\\\!/!g' | sed 's!|UNC/!|//!' | sed \"s!|UNC\\(.\\)!|//\\$(printf '%o' \\\\\\'\\1)!\" | sed 's/.*/echo \"&\"/' | sh")))
                              "\n" t))))

(set-drvfs-alist)

(defconst windows-path-style-regexp "\\`\\(.*/\\)?\\([a-zA-Z]:\\\\.*\\|[a-zA-Z]:/.*\\|\\\\\\\\.*\\|//.*\\)")

(defun windows-path-convert-file-name (name)
  (setq name (replace-regexp-in-string windows-path-style-regexp "\\2" name t nil))
  (setq name (replace-regexp-in-string "\\\\" "/" name))
  (let ((case-fold-search t))
    (cl-loop for (mountpoint . source) in drvfs-alist
             if (string-match (concat "^\\(" (regexp-quote source) "\\)\\($\\|/\\)") name)
             return (replace-regexp-in-string "^//" "/" (replace-match mountpoint t t name 1))
             finally return name)))

(defun windows-path-run-real-handler (operation args)
  "Run OPERATION with ARGS."
  (let ((inhibit-file-name-handlers
         (cons 'windows-path-map-drive-hook-function
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation args)))

(defun windows-path-map-drive-hook-function (operation name &rest args)
  "Run OPERATION on cygwin NAME with ARGS."
  (windows-path-run-real-handler
   operation
   (cons (windows-path-convert-file-name name)
         (if (stringp (car args))
             (cons (windows-path-convert-file-name (car args))
                   (cdr args))
           args))))

(add-to-list 'file-name-handler-alist
             (cons windows-path-style-regexp
                   'windows-path-map-drive-hook-function))
