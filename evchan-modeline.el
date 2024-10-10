;;; evchan-modeline --- Custom mode-line for my opinionated setting -*- lexical-binding: t -*-

;; Copyright (C) 2024 Anho Ki

;; Author: Anho Ki
;; Maintainer: Anho Ki
;; URL: https://github.com/kyano/evchan-modeline
;; Version: 0.0.2
;; Package-Requires: ((emacs "29.1") (all-the-icons "6.0.0"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Custom mode-line for my opinionated setting

;;; Code:

(require 'battery)
(require 'subr-x)
(require 'time)
(require 'vc-hooks)
(require 'all-the-icons)

(defsubst evchan-modeline/buffer-identification ()
  "Generate a string for `mode-line-buffer-identification' with the icon."

  (let ((icon (all-the-icons-icon-for-buffer)))
    (unless (stringp icon)
      (setq icon (all-the-icons-octicons "file")))
    (list (propertize
           (format "%s %%12b" icon)
           'face 'mode-line-buffer-id
           'help-echo (purecopy "Buffer name
mouse-1: Previous buffer\nmouse-3: Next buffer")
           'mouse-face 'mode-line-highlight
           'local-map 'mode-line-buffer-identification-keymap))))

(defsubst evchan-modeline/modified ()
  "Generate a string for `mode-line-modified' with the icons."

  (list (propertize
         (format "%s" (all-the-icons-vscode-codicons
                       (if buffer-read-only "lock" "unlock")
                       :face (when buffer-read-only 'error)))
         'help-echo 'mode-line-read-only-help-echo
         'local-map (purecopy (make-mode-line-mouse-map
                               'mouse-1
                               #'mode-line-toggle-read-only))
         'mouse-face 'mode-line-highlight)
        (propertize
         (if buffer-file-name
             (format "%s" (all-the-icons-vscode-codicons
                           "save"
                           :face (when (buffer-modified-p) 'error)))
           "%1+")
         'help-echo 'mode-line-modified-help-echo
         'local-map (purecopy (make-mode-line-mouse-map
                               'mouse-1 #'mode-line-toggle-modified))
         'mouse-face 'mode-line-highlight)))

(setq-default mode-line-format
              (list "%e"
                    'mode-line-front-space
                    'mode-line-mule-info
                    'mode-line-client
                    'mode-line-modified
                    'mode-line-remote
                    'mode-line-window-dedicated
                    'mode-line-frame-identification
                    'mode-line-buffer-identification
                    "   "
                    'mode-line-position
                    'mode-line-format-right-align
                    '(project-mode-line project-mode-line-format)
                    '(vc-mode vc-mode)
                    "  "
                    'mode-line-modes
                    'mode-line-misc-info
                    'mode-line-end-spaces))

(defun evchan-modeline/battery-mode-line-update (data)
  "Generate battery status string for mode-line.

DATA is from `battery-update-funtions' so please refer the original doc string."

  (let* ((line-status (alist-get ?L data))
         (battery-status-symbol (alist-get ?b data))
         (load-percentage (string-to-number (alist-get ?p data)))
         (icon-name (if (string= battery-status-symbol "!")
                        "battery_alert"
                      (if (or (string= line-status "AC")
                              (string= battery-status-symbol "+"))
                          "battery_charging_"
                        "battery_")))
         (icon-face (cond ((string= battery-status-symbol "!") 'battery-load-critical)
                          ((string= battery-status-symbol "-") 'battery-load-low)
                          ((string= battery-status-symbol "+") 'success)
                          (t 'default))))
    (unless (string= battery-status-symbol "!")
      (cond
       ((<= load-percentage 20) (setf icon-name (concat icon-name "20")))
       ((<= load-percentage 30) (setf icon-name (concat icon-name "30")))
       ((<= load-percentage 50) (setf icon-name (concat icon-name "50")))
       ((<= load-percentage 60) (setf icon-name (concat icon-name "60")))
       ((<= load-percentage 80) (setf icon-name (concat icon-name "80")))
       ((<= load-percentage 90) (setf icon-name (concat icon-name "90")))
       (t (setf icon-name (concat icon-name "full")))))
    (setq battery-mode-line-string
          (format "%s%s%%"
                  (all-the-icons-material-icons icon-name
                                                :face icon-face
                                                :style 'twotone)
                  load-percentage))))

(defun evchan-modeline/vc-mode-line (_file &optional backend)
  "Add an icon to `vc-mode' string.

When BACKEND is `Git', it adds the special icon."

  (when (stringp vc-mode)
    (let* ((vc-mode-trimmed (string-trim-left vc-mode))
           (properties (text-properties-at 0 vc-mode-trimmed)))
      (setq vc-mode
            (concat
             " "
             (apply #'propertize
                    (format "%s %s"
                            (if (string= (symbol-name backend) "Git")
                                (all-the-icons-devopicons "git")
                              (all-the-icons-octicons "workflow"))
                            vc-mode-trimmed)
                    properties))))))

(defun evchan-modeline/display-time-update--load ()
  "Copied from `time.el'."

  (if (null display-time-load-average)
      ""
    (condition-case ()
        ;; Do not show values less than
        ;; `display-time-load-average-threshold'.
        (if (> (* display-time-load-average-threshold 100)
               (nth display-time-load-average (load-average)))
            ""
          ;; The load average number is mysterious, so
          ;; provide some help.
          (let ((str (format "%03d"
                             (nth display-time-load-average
                                  (load-average)))))
            (propertize
             (concat (substring str 0 -2) "." (substring str -2))
             'local-map (make-mode-line-mouse-map
                         'mouse-2 'display-time-next-load-average)
             'mouse-face 'mode-line-highlight
             'help-echo (concat
                         "System load average for past "
                         (if (= 0 display-time-load-average)
                             "1 minute"
                           (if (= 1 display-time-load-average)
                               "5 minutes"
                             "15 minutes"))
                         "; mouse-2: next"))))
      (error ""))))

(dolist (buf (buffer-list))
  (with-current-buffer buf
    (setq-local mode-line-buffer-identification
                (evchan-modeline/buffer-identification))
    (setq-local mode-line-modified
                (evchan-modeline/modified))))
(add-hook 'after-change-major-mode-hook
          #'(lambda ()
              (setq-local mode-line-buffer-identification
                          (evchan-modeline/buffer-identification))))
(add-hook 'post-command-hook
          #'(lambda ()
              (setq-local mode-line-modified
                          (evchan-modeline/modified))))

(when (and battery-status-function battery-mode-line-format display-battery-mode)
  (add-to-list 'battery-update-functions
               #'evchan-modeline/battery-mode-line-update)
  (battery-update))

(advice-add 'vc-mode-line
            :after #'evchan-modeline/vc-mode-line)

(let ((new-forms))
  (dolist (elem (reverse display-time-string-forms))
    (if (eq elem 'load)
        (progn
          (push #'(evchan-modeline/display-time-update--load) new-forms)
          (push (format " %s" (all-the-icons-material-icons "memory" :style 'twotone))
                new-forms))
      (push elem new-forms)))
  (push
   #'(let* ((hour (string-to-number (format-time-string "%I")))
            (icon-name (format "time-%d" hour)))
       (format "%s" (all-the-icons-weather-icons icon-name)))
   new-forms)
  (setq display-time-string-forms new-forms))
(display-time-update)

(provide 'evchan-modeline)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; evchan-modeline.el ends here
