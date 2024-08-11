;; -*- coding: utf-8; -*-

(require 'async)
(require 'subr-x) ;; for `string-empty-p`

(defgroup msg-blame nil
  "A minor mode to show git blame information in messages."
  :group 'tools
  :prefix "msg-blame-")

(defcustom msg-blame-idle-time 1.0
  "Time in seconds of idle before showing git blame information."
  :type 'number
  :group 'msg-blame)

(defcustom msg-blame-author-icon "👤"
  "Icon used to display the author's name."
  :type 'string
  :group 'msg-blame)

(defcustom msg-blame-date-format "%Y-%m-%d"
  "Format for displaying the date."
  :type 'string
  :group 'msg-blame)

(defcustom msg-blame-no-commit-message "No commit information available."
  "Message to show when no commit information is found."
  :type 'string
  :group 'msg-blame)

(defun msg-blame--enable ()
  "Enable msg-blame functionality."
  (add-hook 'post-command-hook #'msg-blame--post-command nil t))

(defun msg-blame--disable ()
  "Disable msg-blame functionality."
  (cancel-function-timers #'msg-blame--check-and-blame)
  (setq msg-blame--last-line nil))

(defvar-local msg-blame--last-line nil
  "Cache the last line number that was processed.")

(defun msg-blame--start-timer ()
  "Start the idle timer for msg-blame."
  (run-with-idle-timer msg-blame-idle-time t #'msg-blame--check-and-blame))

(defun msg-blame--check-and-blame ()
  "Check if we need to run git blame and do so if necessary."
  (let ((current-line (line-number-at-pos))
        (file (buffer-file-name)))
    (when (and msg-blame-mode
               file
               (not (equal current-line msg-blame--last-line)))
      (setq msg-blame--last-line current-line)
      (msg-blame--async-blame file current-line))))

(defun msg-blame--async-blame (file line)
  "Asynchronously get git blame information for FILE at LINE."
  (async-start
   `(lambda ()
      (let ((default-directory ,(convert-standard-filename (file-name-directory file)))
            (coding-system-for-read 'utf-8)
            (coding-system-for-write 'utf-8))
        (with-temp-buffer
          ;; 限制 git blame 搜索深度为单行，避免过多计算
          (when (zerop (call-process "git" nil t nil
                                     "blame"
                                     "-L" ,(format "%d,%d" line line)
                                     "--porcelain"
                                     ,(convert-standard-filename (file-name-nondirectory file))))
            (buffer-string)))))
   (lambda (output)
     (msg-blame--handle-blame-output output))))

(defun msg-blame--handle-blame-output (output)
  "Handle the OUTPUT of the git blame command."
  (if (not (string-empty-p output))
      (let* ((author (msg-blame--extract-info output "^author "))
             (date (msg-blame--extract-info output "^author-time "))
             (summary (msg-blame--extract-info output "^summary ")))
        (message "%s %s %s %s"
                 msg-blame-author-icon
                 (string-trim author)
                 (format-time-string msg-blame-date-format (seconds-to-time (string-to-number (string-trim date))))
                 (string-trim summary)))
    (message "%s" msg-blame-no-commit-message)))

(defun msg-blame--extract-info (output regex)
  "Extract information from the OUTPUT using the REGEX."
  (when (string-match (concat regex "\\(.*\\)") output)
    (match-string 1 output)))

(defun msg-blame--turn-on ()
  "Enable `msg-blame-mode' in the current buffer."
  (when (and (buffer-file-name)
             (vc-backend (buffer-file-name)))
    (msg-blame-mode 1)))

;;;###autoload
(define-minor-mode msg-blame-mode
  "Minor mode to show git blame information in messages."
  :lighter " MsgBlame"
  :group 'msg-blame
  (if msg-blame-mode
      (msg-blame--start-timer)
    (msg-blame--disable)))

;;;###autoload
(define-globalized-minor-mode global-msg-blame-mode
  msg-blame-mode
  msg-blame--turn-on)

(provide 'msg-blame)

;;; msg-blame.el ends here
