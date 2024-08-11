;; -*- coding: utf-8; -*-

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

;;;###autoload
(define-minor-mode msg-blame-mode
  "Minor mode to show git blame information in messages."
  :lighter " MsgBlame"
  :group 'msg-blame
  (if msg-blame-mode
      (msg-blame--enable)
    (msg-blame--disable)))

;;;###autoload
(define-globalized-minor-mode global-msg-blame-mode
  msg-blame-mode
  msg-blame--turn-on)

(defun msg-blame--turn-on ()
  "Enable `msg-blame-mode' in the current buffer."
  (when (and (buffer-file-name)
             (vc-backend (buffer-file-name)))
    (msg-blame-mode 1)))

(defun msg-blame--enable ()
  "Enable msg-blame functionality."
  (add-hook 'post-command-hook #'msg-blame--post-command nil t))

(defun msg-blame--disable ()
  "Disable msg-blame functionality."
  (remove-hook 'post-command-hook #'msg-blame--post-command t))

(require 'async)

(defun msg-blame--post-command ()
  "Handle post command logic for msg-blame."
  (when (and msg-blame-mode
             (sit-for msg-blame-idle-time))
    (let ((line (line-number-at-pos))
          (file (buffer-file-name)))
      (msg-blame--async-blame file line))))

(defun msg-blame--async-blame (file line)
  "Asynchronously get git blame information for FILE at LINE."
  (async-start
   `(lambda ()
      (with-temp-buffer
        (when (zerop (call-process "git" nil t nil "blame" "-L" ,(format "%d,%d" line line) "--porcelain" ,file))
          (buffer-string))))
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
                 author
                 (format-time-string msg-blame-date-format (seconds-to-time (string-to-number date)))
                 summary))
    (message "%s" msg-blame-no-commit-message)))

(defun msg-blame--extract-info (output regex)
  "Extract information from the OUTPUT using the REGEX."
  (when (string-match (concat regex "\\(.*\\)") output)
    (match-string 1 output)))

(provide 'msg-blame)

;;; msg-blame.el ends here
