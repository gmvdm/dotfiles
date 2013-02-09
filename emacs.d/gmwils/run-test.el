;; Run tests based on the current file

;; TODO(gmwils): Run cucumber if editing features
;; TODO(gmwils): use different test runners based on extension (eg. rspec, py, etc)
;; TODO(gmwils): if file is ROOT/a/b/file, then try ROOT/test/a/b/test_file

(defun string/ends-with (s ending)
  "return non-nil if string S ends with ENDING."
  (let ((elength (length ending)))
            (string= (substring s (- 0 elength)) ending)))

(defun find-test-file (f)
  "find the equivalent test file in the current project"
  (let ((test-dir (concat (textmate-find-project-root) "/test/"))
        (filename (file-name-nondirectory (file-name-sans-extension f)))
        (ext (file-name-extension f)))
    (concat test-dir filename "_test." ext)))

(defun test-file-p (f)
  "return non-nil if file is a test file"
  (string/ends-with (file-name-sans-extension f) "test"))

(defun test-file-name (f)
  "return a test file or nil if none found."
  (if (or (eq f nil) (test-file-p f))
      f
      (find-test-file f)))

(defun run-test-from-file (f)
  "given a file, run tests on it"
  (let ((base-dir (textmate-find-project-root))
        (virtenv (file-name-nondirectory (getenv "VIRTUAL_ENV"))))
    (if (and (not (eq f nil)) (file-readable-p f))
        (shell-command (concat "("
                               "cd " base-dir "; "
                               "source ~/.virtualenvs/" virtenv "/bin/activate; "
                               "PYTHONPATH=\"" base-dir ";$PYTHONPATH\" py.test " f ")"))
        (message "Unable to run test for file %s" f))))

(defun run-test ()
  "run tests based on the current buffer"
  (interactive)
  (save-buffer)
  (run-test-from-file (test-file-name (buffer-file-name))))

(global-set-key (kbd "C-t") 'run-test)

(provide 'run-test)
