;; -*- coding: utf-8; -*-

;;; emsg-blame.el --- View git blame in Emacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.
;; License: GPL-3.0-or-later

;; Author: ISouthRain
;; Version: 0.2
;; Package-Requires: ((emacs "24.2") (async "1.8"))
;; Keywords: blame
;; URL: https://github.com/ISouthRain/emsg-blame

;;; Commentary:
;;
;; This package is quickly view git blame information of the current file line in Emacs in real time.

;;; Code:
(require 'async)
(require 'subr-x)
(require 'hl-line)

(defgroup emsg-blame nil
  "A minor mode to show git blame information."
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

(defcustom emsg-blame-display #'emsg-blame--display-message
  "emsg-blame to display function."
  :type '(choice (const nil)
                 function)
  :group 'emsg-blame)

(defcustom emsg-blame-background nil
  "Toggle display diff overlay background."
  :type 'boolean
  :group 'emsg-blame)

(defvar emsg-blame-background-color nil
  "Customizable background color for diff overlays.
If set to a color value (e.g., hex code or color name), it will be used as the background color for overlays that highlight differences.
If set to nil, the default `hl-line` background color will be used instead.")

(defcustom emsg-blame-i18n-date-now "Just Now"
  "String for 'Just Now' in the current language."
  :type 'string
  :group 'emsg-blame)

(defcustom emsg-blame-i18n-date-min "%d minutes ago"
  "String for '%d minutes ago' in the current language."
  :type 'string
  :group 'emsg-blame)

(defcustom emsg-blame-i18n-date-hour "%d hours ago"
  "String for '%d hours ago' in the current language."
  :type 'string
  :group 'emsg-blame)

(defcustom emsg-blame-i18n-date-day "%d days ago"
  "String for '%d days ago' in the current language."
  :type 'string
  :group 'emsg-blame)

(defcustom emsg-blame-i18n-date-month "%d months ago"
  "String for '%d months ago' in the current language."
  :type 'string
  :group 'emsg-blame)

(defcustom emsg-blame-i18n-date-year "%d years ago"
  "String for '%d years ago' in the current language."
  :type 'string
  :group 'emsg-blame)

(defvar emsg-blame-debug nil
  "Toggle emsg-blame debug log output.")
(defvar emsg-blame-debug-log "*emsg-blame-log*"
  "emsg-blame debug output buffer name.")

(defvar emsg-blame-current-file ""
  "emsg-blame to git blame filename.")

(defvar emsg-blame-current-file-buffer-name ""
  "emsg-blame to git blame filename buffer.")

(defvar emsg-blame--commit-head ""
  "emsg-blame Commit HEAD.")

(defvar emsg-blame--commit-author ""
  "emsg-blame Commit Author.")

(defvar emsg-blame--commit-date ""
  "emsg-blame Commit Date.")

(defvar emsg-blame--commit-summary ""
  "emsg-blame Commit Summary.")

(defvar-local emsg-blame--last-line nil
  "Cache the last line number that was processed.")

(defvar emsg-blame--git-show-overlay-line nil
  "List of overlays used for displaying `+` symbols at the beginning of lines.")

(defun emsg-blame--display-message ()
  "emsg-blame Default display function."
  (message " %s %s <%s> " emsg-blame--commit-author emsg-blame--commit-date emsg-blame--commit-summary))

(defun emsg-blame-debug-output (txt)
  (when emsg-blame-debug
    (with-current-buffer (get-buffer-create emsg-blame-debug-log)
      (goto-char (point-max))
      (insert txt)
      (insert "\n")))
  )

(defun emsg-blame--enable ()
  "Enable emsg-blame functionality."
  (add-hook 'post-command-hook #'emsg-blame--post-command nil t))

(defun emsg-blame--disable ()
  "Disable emsg-blame functionality."
  (cancel-function-timers #'emsg-blame--git-blame-check)
  (emsg-blame--git-show-overlay-clear-line)
  (setq emsg-blame--last-line nil))

(defun emsg-blame--start-timer ()
  "Start the idle timer for emsg-blame."
  (run-with-idle-timer emsg-blame-idle-time t #'emsg-blame--git-blame-check))

(defun emsg-blame--git-blame-check ()
  "Check if we need to run git blame and do so if necessary."
  (let ((current-line (line-number-at-pos))
        (file (buffer-file-name))
        (buffer (buffer-name)))
    (when (and emsg-blame-mode
               file
               (not (equal current-line emsg-blame--last-line)))
      (setq emsg-blame--last-line current-line)
      (setopt emsg-blame-current-file file
              emsg-blame-current-file-buffer-name buffer)
      (emsg-blame-debug-output "--------------------------------------------\n\nStart executing `git blame`..\n")
      (emsg-blame--git-show-overlay-clear-line);; Clear all overlays line.
      (emsg-blame--git-blame-async file current-line)
      )
    )
  )

;; TODO: Known issues: Non-ascii filenames are not supported, but non-ascii folders are supported
(defun emsg-blame--git-blame-async (file line)
  "Asynchronously get git blame information for FILE at LINE."
  (emsg-blame-debug-output (format "command: \n git blame -L %s,%s --incremental %s\n" line line file))
  (async-start
   `(lambda ()
      (let ((default-directory ,(convert-standard-filename (file-name-directory file)))
            (coding-system-for-read 'utf-8)
            (coding-system-for-write 'utf-8))
        (with-temp-buffer
          (when (zerop (call-process "git" nil t nil
                                     "blame"
                                     "-L" ,(format "%d,%d" line line)
                                     "--incremental"
                                     ,(convert-standard-filename (file-name-nondirectory file))))
            (buffer-string)))))
   (lambda (output)
     (emsg-blame-debug-output (concat "`git blame` return success..\n\n" output "\n"))
     (emsg-blame--git-blame-get-commit-info output))))

(defun emsg-blame--git-show-overlay-add-line ()
  "Use `overlay` to add highlight to each line."
  (let ((start (line-beginning-position))
        (end (line-beginning-position 2))
        (bg-color (or emsg-blame-background-color
                      (face-attribute 'hl-line :background nil 'default))))
    (let ((new-overlay (make-overlay start end)))
      (overlay-put new-overlay 'face `(:background ,bg-color))
      (push new-overlay emsg-blame--git-show-overlay-line))))

(defun emsg-blame--git-show-overlay-clear-line ()
  "Clear all `emsg-blame--git-show-overlay-line`."
  (dolist (overlay emsg-blame--git-show-overlay-line)
    (delete-overlay overlay))
  (setq emsg-blame--git-show-overlay-line nil))

;; Keno issues: If the filename of the current file has changed in a commit,
;; it will not be displayed here due to Git limitations.
;; Test command: git --no-pager show current-head current-filename
;; TODO: It should be handled as async separately, rather than in emsg-blame--git-blame-async.
(defun emsg-blame--git-show-async (head)
  "Retrieve and process the output of `git show` for the specified HEAD.
This function uses an asynchronous process to run `git show` and handles the output.

HEAD is the commit hash or reference to retrieve the git show output for.
It also processes the output to filter and clean up lines for display in the `*emsg-blame--git-show-txt*` buffer."
  (emsg-blame-debug-output (format "Command:\n git --no-pager show %s %s\n" head emsg-blame-current-file))
  (let ((git-output nil)
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8))
    ;; Use a temporary buffer to execute the git show command
    (with-temp-buffer
      (when (zerop (call-process "git" nil t nil
                                 "--no-pager"
                                 "show"
                                 (format "%s" head)
                                 (format "%s" emsg-blame-current-file)))
        (setq git-output (buffer-string)))
      ;; Create or switch to the *emsg-blame--git-show-txt* buffer
      (with-current-buffer (get-buffer-create "*emsg-blame--git-show-txt*")
        (erase-buffer)  ;; Clear the buffer contents
        (insert git-output))
      )
    ;; Process the content of the *emsg-blame--git-show-txt* buffer
    (with-current-buffer "*emsg-blame--git-show-txt*"
      (let ((lines (split-string (buffer-string) "\n" t)))  ;; Split buffer content into lines
        (erase-buffer)  ;; Clear the current buffer
        (dolist (line lines)
          (when (or (string-prefix-p "+" line)
                    (string-prefix-p "@@" line))
            (insert (substring line 1) "\n")
            )))  ;; Remove the leading + or @ from each line and insert it into the buffer
      (goto-char (point-min)) ;; Move the cursor to the beginning of the buffer
      )
    (emsg-blame-debug-output "Extraction `git show` successful!!\n\nStart comparing... \n\n")
    )
  )

(defun emsg-blame--git-show-comper-buffer ()
  "Compare the content of `*emsg-blame--git-show-txt*` buffer with the current buffer."
  (let ((git-show-buffer (get-buffer "*emsg-blame--git-show-txt*"))
        (buffers (get-buffer emsg-blame-current-file-buffer-name)))
    (if (not git-show-buffer)
        (message "Buffer *emsg-blame--git-show-txt* not found.")
      (let ((lines (split-string (with-current-buffer git-show-buffer (buffer-string)) "\n" t)))
        (with-current-buffer buffers
          (save-excursion
            (let ((start-point (point-min)))
              ;; TODO: @@block@@block matching should be used instead of per-line matching, as per-line matching is not accurate enough.
              (dolist (line lines)
                (goto-char start-point)
                (when (re-search-forward (concat "^" (regexp-quote line) "$") nil t)
                  ;; add overlay
                  (emsg-blame--git-show-overlay-add-line)
                  ;; Update start-point for next search
                  (setq start-point (point))))))))))
  (emsg-blame-debug-output "Comparison buffer completed.")
  )

(defun emsg-blame--git-blame-get-commit-info (output)
  "Handle the OUTPUT of the git blame command."
  ;; Check if output is nil or empty string
  (if (and output (not (string-empty-p output)))
      (let* ((head-temp (emsg-blame--git-blame-extract-info output "^"))
             (head (substring head-temp 0 (min 40 (length head-temp))))
             (author (emsg-blame--git-blame-extract-info output "^author "))
             (date (emsg-blame--git-blame-extract-info output "^author-time "))
             (summary (emsg-blame--git-blame-extract-info output "^summary "))
             ;; Check if author and date were extracted successfully
             (relative-time (if date (emsg-blame--git-blame-format-relative-time date) "Unknown date")))
        ;; Assign variable; format is used to remove unknown symbols, such as ^M
        (setq emsg-blame--commit-author (format "%s" (string-trim author))
              emsg-blame--commit-date (format "%s" relative-time)
              emsg-blame--commit-summary (format "%s" (string-trim summary))
              emsg-blame--commit-head (format "%s" (string-trim head))
              )
        ;; To display function.
        (when (functionp emsg-blame-display)
          (funcall emsg-blame-display))
        ;; To display diff overlay background.
        (when (and emsg-blame-background
                   (not (string= emsg-blame--commit-head "0000000000000000000000000000000000000000")))
          (emsg-blame-debug-output "With the correct `head` number, start executing highlighting...\n")
          (emsg-blame--git-show-async emsg-blame--commit-head)
          (emsg-blame--git-show-comper-buffer);; Compare and add overlay lines.
          )
        )
    ))

(defun emsg-blame--git-blame-extract-info (output regex)
  "Extract information from the OUTPUT using the REGEX."
  ;; Make sure output is not nil and the match is successful
  (when (and output (string-match (concat regex "\\(.*\\)") output))
    (match-string 1 output)))

(defun emsg-blame--git-blame-format-relative-time (date)
  "Format relative time for display based on the selected language and the value of `emsg-blame-data-pretty`."
  (let* ((date (seconds-to-time (string-to-number (string-trim date))))
         (now (current-time))
         (diff (float-time (time-subtract now date)))
         (seconds-in-minute 60)
         (seconds-in-hour (* 60 seconds-in-minute))
         (seconds-in-day (* 24 seconds-in-hour))
         (seconds-in-month (* 30 seconds-in-day))
         (seconds-in-year (* 365 seconds-in-day))
         (just-now emsg-blame-i18n-date-now)
         (minutes-ago emsg-blame-i18n-date-min)
         (hours-ago emsg-blame-i18n-date-hour)
         (days-ago emsg-blame-i18n-date-day)
         (months-ago emsg-blame-i18n-date-month)
         (years-ago emsg-blame-i18n-date-year))

    ;; If `emsg-blame-data-pretty` is nil, display the absolute time directly
    (if (not emsg-blame-data-pretty)
        (format-time-string emsg-blame-date-format date)
      ;; Otherwise display relative time
      (cond
       ((< diff seconds-in-minute) just-now)
       ((< diff seconds-in-hour) (format minutes-ago (floor (/ diff seconds-in-minute))))
       ((< diff seconds-in-day) (format hours-ago (floor (/ diff seconds-in-hour))))
       ((< diff seconds-in-month) (format days-ago (floor (/ diff seconds-in-day))))
       ((< diff seconds-in-year) (format months-ago (floor (/ diff seconds-in-month))))
       (t (format years-ago (floor (/ diff seconds-in-year)))))))
  )

(defun emsg-blame--turn-on ()
  "Enable `emsg-blame-mode' in the current buffer."
  (when (and (buffer-file-name)
             (vc-backend (buffer-file-name)))
    (emsg-blame-mode 1)))

;;;###autoload
(define-minor-mode emsg-blame-mode
  "Minor mode to show git blame information in messages."
  :lighter " "
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
