;; -*- coding: utf-8; -*-

;;; emsg-blame.el --- View git blame in Emacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.
;; License: GPL-3.0-or-later

;; Author: ISouthRain
;; Version: 0.1
;; Package-Requires: ((emacs "24.2"))
;; Keywords: blame
;; URL: https://github.com/ISouthRain/emsg-blame

;;; Commentary:
;;
;; This package is quickly view git blame information of the current file line in Emacs in real time.

;;; Code:
(require 'async)
(require 'subr-x)

(defgroup emsg-blame nil
  "A minor mode to show git blame information in messages."
  :group 'tools
  :prefix "emsg-blame-")

(defcustom emsg-blame-idle-time 0.5
  "Time in seconds of idle before showing git blame information."
  :type 'number
  :group 'emsg-blame)

(defcustom emsg-blame-date-format "%Y-%m-%d %H:%M:%S"
  "Format for displaying the date."
  :type 'string
  :group 'emsg-blame)

(defcustom emsg-blame-data-pretty t
  "Toggle pretty time display."
  :type 'boolean
  :group 'emsg-blame)

(defcustom emsg-blame-no-commit-message "`emsg-blame` Output: No commit information available."
  "Message to show when no commit information is found."
  :type 'string
  :group 'emsg-blame)

(defcustom emsg-blame-display #'emsg-blame--display-message
  "emsg-blame to display function."
  :type '(choice (const nil)
                 function)
  :group 'emsg-blame)

(defcustom emsg-blame-i18n-lang "English"
  "Local language environment for `emsg-blame`.
Possible values include:
- \"English\" (default)
- \"Chinese\"
- \"French\"
- \"Russian\"
This setting determines the language used for displaying time information."
  :type '(choice (const :tag "English" "English")
                 (const :tag "Chinese" "Chinese")
                 (const :tag "French" "French")
                 (const :tag "Russian" "Russian"))
  :group 'emsg-blame)

(defun emsg-blame--get-time-descriptions ()
  "Return time description strings based on the current language environment."
  (let ((lang (or emsg-blame-i18n-lang "English")))  ;; 默认英文
    (cond
     ;; 使用 `member` 检查语言环境
     ((member lang '("Chinese" "Chinese Simplified"))
      (list "刚刚"
            "%d分钟前"
            "%d小时前"
            "%d天前"
            "%d个月前"
            "%d年前"))

     ((member lang '("French"))
      (list "à l'instant"
            "il y a %d minutes"
            "il y a %d heures"
            "il y a %d jours"
            "il y a %d mois"
            "il y a %d ans"))

     ((member lang '("Russian"))
      (list "только что"
            "%d минут назад"
            "%d часов назад"
            "%d дней назад"
            "%d месяцев назад"
            "%d лет назад"))

     ;; 默认英文
     (t (list "Just Now"
              "%d minutes ago"
              "%d hours ago"
              "%d days ago"
              "%d months ago"
              "%d years ago")))))

(defvar emsg-blame--commit-author ""
  "emsg-blame Commit Author.")

(defvar emsg-blame--commit-date ""
  "emsg-blame Commit Date.")

(defvar emsg-blame--commit-summary ""
  "emsg-blame Commit Summary.")

(defun emsg-blame--display-message ()
  "emsg-blame Default display function."
  (message " %s %s <%s> " emsg-blame--commit-author emsg-blame--commit-date emsg-blame--commit-summary))

(defvar-local emsg-blame--last-line nil
  "Cache the last line number that was processed.")

(defun emsg-blame--enable ()
  "Enable emsg-blame functionality."
  (add-hook 'post-command-hook #'emsg-blame--post-command nil t))

(defun emsg-blame--disable ()
  "Disable emsg-blame functionality."
  (cancel-function-timers #'emsg-blame--check-and-blame)
  (setq emsg-blame--last-line nil))

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

;; TODO: Known issues: Non-ascii filenames are not supported, but non-ascii folders are supported
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
        ;; Assign variable; format is used to remove unknown symbols, such as ^M
        (setq emsg-blame--commit-author (format "%s" (string-trim author))
              emsg-blame--commit-date (format "%s" relative-time)
              emsg-blame--commit-summary (format "%s" (string-trim summary))
              )
        ;; To display function.
        (when (functionp emsg-blame-display)
          (funcall emsg-blame-display))
        )
    ;; 当 output 为 nil 或为空时，显示无提交信息
    (emsg-blame--no-commit-display emsg-blame-no-commit-message)))

(defun emsg-blame--extract-info (output regex)
  "Extract information from the OUTPUT using the REGEX."
  ;; 确保 output 不是 nil，并且成功匹配
  (when (and output (string-match (concat regex "\\(.*\\)") output))
    (match-string 1 output)))

(defun emsg-blame--format-relative-time (date)
  "Format relative time for display based on the selected language and the value of `emsg-blame-data-pretty`."
  (let* ((date (seconds-to-time (string-to-number (string-trim date))))
         (now (current-time))
         (diff (float-time (time-subtract now date)))
         (seconds-in-minute 60)
         (seconds-in-hour (* 60 seconds-in-minute))
         (seconds-in-day (* 24 seconds-in-hour))
         (seconds-in-month (* 30 seconds-in-day))
         (seconds-in-year (* 365 seconds-in-day))
         ;; 获取时间描述
         (descriptions (emsg-blame--get-time-descriptions))
         (just-now (nth 0 descriptions))
         (minutes-ago (nth 1 descriptions))
         (hours-ago (nth 2 descriptions))
         (days-ago (nth 3 descriptions))
         (months-ago (nth 4 descriptions))
         (years-ago (nth 5 descriptions)))

    ;; 如果 `emsg-blame-data-pretty` 为 nil，直接显示绝对时间
    (if (not emsg-blame-data-pretty)
        (format-time-string emsg-blame-date-format date)
      ;; 否则显示相对时间
      (cond
       ((< diff seconds-in-minute) just-now)
       ((< diff seconds-in-hour) (format minutes-ago (floor (/ diff seconds-in-minute))))
       ((< diff seconds-in-day) (format hours-ago (floor (/ diff seconds-in-hour))))
       ((< diff seconds-in-month) (format days-ago (floor (/ diff seconds-in-day))))
       ((< diff seconds-in-year) (format months-ago (floor (/ diff seconds-in-month))))
       (t (format years-ago (floor (/ diff seconds-in-year)))))))
  )

(defun emsg-blame--no-commit-display (message-text)
  "Display the MESSAGE-TEXT according to `emsg-blame-no-commit-message`."
    (message "%s" message-text))

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
