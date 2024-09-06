;; -*- coding: utf-8; -*-

(require 'async)
(require 'subr-x) ;; for `string-empty-p`
;; (require 'posframe)

(defgroup emsg-blame nil
  "A minor mode to show git blame information in messages."
  :group 'tools
  :prefix "emsg-blame-")

(defcustom emsg-blame-idle-time 1.0
  "Time in seconds of idle before showing git blame information."
  :type 'number
  :group 'emsg-blame)

(defcustom emsg-blame-author-icon "👤"
  "Icon used to display the author's name."
  :type 'string
  :group 'emsg-blame)

(defcustom emsg-blame-date-format "%Y-%m-%d"
  "Format for displaying the date."
  :type 'string
  :group 'emsg-blame)

(defcustom emsg-blame-no-commit-message "`emsg-blame` No commit information available."
  "Message to show when no commit information is found."
  :type 'string
  :group 'emsg-blame)

(defcustom emsg-blame-display-method 'message
  "Method to display git blame information. Options are 'message or 'posframe."
  :type '(choice (const :tag "Message" message)
                 (const :tag "Posframe" posframe))
  :group 'emsg-blame)

(defun emsg-blame--enable ()
  "Enable emsg-blame functionality."
  (add-hook 'post-command-hook #'emsg-blame--post-command nil t))

(defun emsg-blame--disable ()
  "Disable emsg-blame functionality."
  (cancel-function-timers #'emsg-blame--check-and-blame)
  (setq emsg-blame--last-line nil)
  (when (posframe-workable-p)
    (posframe-hide "*emsg-blame-posframe*")))

(defvar-local emsg-blame--last-line nil
  "Cache the last line number that was processed.")

(defun emsg-blame--start-timer ()
  "Start the idle timer for emsg-blame."
  (run-with-idle-timer emsg-blame-idle-time t #'emsg-blame--check-and-blame))

(defun emsg-blame--check-and-blame ()
  "Check if we need to run git blame and do so if necessary."
  (let ((current-line (line-number-at-pos))
        (file (buffer-file-name)))
    (when (and emsg-blame-mode
               file
               (not (equal current-line emsg-blame--last-line)))
      (setq emsg-blame--last-line current-line)
      (emsg-blame--async-blame file current-line))))

(defun emsg-blame--async-blame (file line)
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
     (emsg-blame--handle-blame-output output))))

(defun emsg-blame--handle-blame-output (output)
  "Handle the OUTPUT of the git blame command."
  ;; 检查 output 是否为 nil 或空字符串
  (if (and output (not (string-empty-p output)))
      (let* ((author (emsg-blame--extract-info output "^author "))
             (date (emsg-blame--extract-info output "^author-time "))
             (summary (emsg-blame--extract-info output "^summary "))
             ;; 检查 author 和 date 是否成功提取
             (relative-time (if date (emsg-blame--format-relative-time date) "Unknown date")))
        (if (eq emsg-blame-display-method 'posframe)
            ;; 使用 posframe 显示信息
            (emsg-blame--display (format "Author: %s\nDate: %s\n%s"
                                         (or (string-trim author) "Unknown author")
                                         relative-time
                                         (or (string-trim summary) "No summary")))
          ;; 使用 message 显示信息
          (emsg-blame--display (format "%s %s %s <%s>"
                                       emsg-blame-author-icon
                                       (or (string-trim author) "Unknown author")
                                       relative-time
                                       (or (string-trim summary) "No summary")))))
    ;; 当 output 为 nil 或为空时，显示无提交信息
    (emsg-blame--display emsg-blame-no-commit-message)))

(defun emsg-blame--extract-info (output regex)
  "Extract information from the OUTPUT using the REGEX."
  ;; 确保 output 不是 nil，并且成功匹配
  (when (and output (string-match (concat regex "\\(.*\\)") output))
    (match-string 1 output)))

(defun emsg-blame--format-relative-time (date)
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
     (t (format-time-string emsg-blame-date-format date)))))

(defun emsg-blame--display (message-text)
  "Display the MESSAGE-TEXT according to `emsg-blame-display-method`."
  (if (eq emsg-blame-display-method 'posframe)
      (emsg-blame--display-posframe message-text)
    (message "%s" message-text)))

(defun emsg-blame--display-posframe (message-text)
  "Display the MESSAGE-TEXT using posframe at point."
  (posframe-show "*emsg-blame-posframe*"
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

(defun emsg-blame--turn-on ()
  "Enable `emsg-blame-mode' in the current buffer."
  (when (and (buffer-file-name)
             (vc-backend (buffer-file-name)))
    (emsg-blame-mode 1)))

;;;###autoload
(define-minor-mode emsg-blame-mode
  "Minor mode to show git blame information in messages."
  :lighter " MsgBlame"
  :group 'emsg-blame
  (if emsg-blame-mode
      (emsg-blame--start-timer)
    (emsg-blame--disable)))

;;;###autoload
(define-globalized-minor-mode global-emsg-blame-mode
  emsg-blame-mode
  emsg-blame--turn-on)

(provide 'emsg-blame)

;;; emsg-blame.el ends here
