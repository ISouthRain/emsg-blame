;; -*- coding: utf-8; -*-

(require 'async)
(require 'subr-x) ;; for `string-empty-p`
;; (require 'posframe)

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

(defcustom msg-blame-display-method 'message
  "Method to display git blame information. Options are 'message or 'posframe."
  :type '(choice (const :tag "Message" message)
                 (const :tag "Posframe" posframe))
  :group 'msg-blame)

(defun msg-blame--enable ()
  "Enable msg-blame functionality."
  (add-hook 'post-command-hook #'msg-blame--post-command nil t))

(defun msg-blame--disable ()
  "Disable msg-blame functionality."
  (cancel-function-timers #'msg-blame--check-and-blame)
  (setq msg-blame--last-line nil)
  (when (posframe-workable-p)
    (posframe-hide "*msg-blame-posframe*")))

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
             (summary (msg-blame--extract-info output "^summary "))
             (relative-time (msg-blame--format-relative-time date)))
        (if (eq msg-blame-display-method 'posframe)
            (progn
              (msg-blame--display (format "Author: %s \nDate: %s \n%s"
                                          (string-trim author)
                                          relative-time
                                          (string-trim summary))))
          (msg-blame--display (format "%s %s %s <%s>"
                                      msg-blame-author-icon
                                      (string-trim author)
                                      relative-time
                                      (string-trim summary))))
        )
    (msg-blame--display msg-blame-no-commit-message)))

(defun msg-blame--extract-info (output regex)
  "Extract information from the OUTPUT using the REGEX."
  (when (string-match (concat regex "\\(.*\\)") output)
    (match-string 1 output)))

(defun msg-blame--format-relative-time (date)
  "Format relative time for display."
  (let* ((date (seconds-to-time (string-to-number (string-trim date))))
         (now (current-time))
         (diff (float-time (time-subtract now date)))
         (seconds-in-minute 60)
         (seconds-in-hour (* 60 seconds-in-minute))
         (seconds-in-day (* 24 seconds-in-hour))
         (seconds-in-month (* 30 seconds-in-day))
         (seconds-in-year (* 365 seconds-in-day)))
    (cond
     ((< diff seconds-in-minute) "刚刚")
     ((< diff seconds-in-hour) (format "%d分钟前" (floor (/ diff seconds-in-minute))))
     ((< diff seconds-in-day) (format "%d小时前" (floor (/ diff seconds-in-hour))))
     ((< diff seconds-in-month) (format "%d天前" (floor (/ diff seconds-in-day))))
     ((< diff (* 1 seconds-in-month)) (format "%d个月前" (floor (/ diff seconds-in-month))))
     (t (format-time-string msg-blame-date-format date)))))

(defun msg-blame--display (message-text)
  "Display the MESSAGE-TEXT according to `msg-blame-display-method`."
  (if (eq msg-blame-display-method 'posframe)
      (msg-blame--display-posframe message-text)
    (message "%s" message-text)))

(defun msg-blame--display-posframe (message-text)
  "Display the MESSAGE-TEXT using posframe at point."
  (posframe-show "*msg-blame-posframe*"
               :string message-text
               :timeout 5
               :max-width 40
               :left-fringe 5
               :right-fringe 5
               :position (point)
               :poshandler #'posframe-poshandler-frame-top-right-corner
               :border-width 5;; 外边框大小
               :border-color "#ed98cc" ;; 边框颜色
               )
  )

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
