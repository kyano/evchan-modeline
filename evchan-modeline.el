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
(require 'calendar)
(require 'json)
(require 'solar)
(require 'subr-x)
(require 'tab-bar)
(require 'time)
(require 'timer)
(require 'url)
(require 'url-http)
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

(defun evchan-modeline/weather-icon-elem (icon-type &optional day night)
  "Make an element of association list `evchan-modeline/weather-icon-alist'.

When no optional arguments are given, this returns `(ICON-TYPE
. (ICON-TYPE ICON-TYPE))'.  When ICON-TYPE and DAY are given, this
returns `(ICON-TYPE . (day-DAY night-alt-DAY))'.  And all arguments are
given, this returns `(ICON-TYPE . (DAY NIGHT))'.


Please note that the icon name prefix is attached only when ICON and
DAY, 2 arguments are given."

  (if night `(,icon-type . (,day ,night))
    (if day `(,icon-type . (,(format "day-%s" day) ,(format "night-alt-%s" day)))
      `(,icon-type . (,icon-type ,icon-type)))))

(defconst evchan-modeline/weather-icon-alist
  `(,(evchan-modeline/weather-icon-elem "fog")
    ,(evchan-modeline/weather-icon-elem "clearsky" "day-sunny" "night-clear")
    ,(evchan-modeline/weather-icon-elem "fair" "cloudy")
    ,(evchan-modeline/weather-icon-elem "partlycloudy" "cloudy")
    ,(evchan-modeline/weather-icon-elem "cloudy")
    ,(evchan-modeline/weather-icon-elem "lightrain" "sprinkle")
    ,(evchan-modeline/weather-icon-elem "rain" "rain")
    ,(evchan-modeline/weather-icon-elem "heavyrain" "hail")
    ,(evchan-modeline/weather-icon-elem "lightrainshowers" "showers")
    ,(evchan-modeline/weather-icon-elem "rainshowers" "showers")
    ,(evchan-modeline/weather-icon-elem "heavyrainshowers" "showers" "showers")
    ,(evchan-modeline/weather-icon-elem "lightrainandthunder" "thunderstorm")
    ,(evchan-modeline/weather-icon-elem "rainandthunder" "thunderstorm")
    ,(evchan-modeline/weather-icon-elem "heavyrainandthunder" "thunderstorm" "thunderstorm")
    ,(evchan-modeline/weather-icon-elem "lightrainshowersandthunder" "storm-showers")
    ,(evchan-modeline/weather-icon-elem "rainshowersandthunder" "storm-showers")
    ,(evchan-modeline/weather-icon-elem "heavyrainshowersandthunder" "storm-showers" "storm-showers")
    ,(evchan-modeline/weather-icon-elem "lightsleet" "sleet")
    ,(evchan-modeline/weather-icon-elem "sleet" "sleet")
    ,(evchan-modeline/weather-icon-elem "heavysleet" "sleet" "sleet")
    ,(evchan-modeline/weather-icon-elem "lightsleetshowers" "rain-mix")
    ,(evchan-modeline/weather-icon-elem "sleetshowers" "rain-mix")
    ,(evchan-modeline/weather-icon-elem "heavysleetshowers" "rain-mix" "rain-mix")
    ,(evchan-modeline/weather-icon-elem "lightsleetandthunder" "sleet-storm")
    ,(evchan-modeline/weather-icon-elem "sleetandthunder" "sleet-storm")
    ,(evchan-modeline/weather-icon-elem "heavysleetandthunder" "sleet-storm")
    ,(evchan-modeline/weather-icon-elem "lightssleetshowersandthunder" "sleet-storm")
    ,(evchan-modeline/weather-icon-elem "sleetshowersandthunder" "sleet-storm")
    ,(evchan-modeline/weather-icon-elem "heavysleetshowersandthunder" "sleet-storm")
    ,(evchan-modeline/weather-icon-elem "lightsnow" "snow")
    ,(evchan-modeline/weather-icon-elem "snow" "snow" "snow")
    ,(evchan-modeline/weather-icon-elem "heavysnow" "snowflake-cold" "snowflake-cold")
    ,(evchan-modeline/weather-icon-elem "lightsnowshowers" "snow")
    ,(evchan-modeline/weather-icon-elem "snowshowers" "snow" "snow")
    ,(evchan-modeline/weather-icon-elem "heavysnowshowers" "snowflake-cold" "snowflake-cold")
    ,(evchan-modeline/weather-icon-elem "lightsnowandthunder" "snow-thunderstorm")
    ,(evchan-modeline/weather-icon-elem "snowandthunder" "snow-thunderstorm")
    ,(evchan-modeline/weather-icon-elem "heavysnowandthunder" "snow-thunderstorm")
    ,(evchan-modeline/weather-icon-elem "lightssnowshowersandthunder"  "snow-thunderstorm")
    ,(evchan-modeline/weather-icon-elem "snowshowersandthunder" "snow-thunderstorm")
    ,(evchan-modeline/weather-icon-elem "heavysnowshowersandthunder" "snow-thunderstorm")))

(defvar evchan-modeline/weather-icon nil)
(defvar evchan-modeline/weather-temperature nil)

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
                          (t nil))))
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
          (format "%s%s%% "
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

(defun evchan-modeline/weather-icon-name (code daytime)
  "Generate a weather icon name for `all-the-icons', with CODE and DAYTIME."

  (let* ((weather-code (car (split-string code "_")))
         (icon-list (cdr (assoc weather-code evchan-modeline/weather-icon-alist))))
    (if icon-list
        (if daytime (nth 0 icon-list)
          (nth 1 icon-list))
      "celsius")))

(defvar evchan-modeline/weather-data)

(defun evchan-modeline/json-weather-data (date)
  "Return the weather data in JSON format.

DATE must be in the format of `%Y-%m-%d'"

  (let* ((properties (assoc 'properties evchan-modeline/weather-data))
         (units (assoc 'units (assoc 'meta properties)))
         (timeseries (cdr (assoc 'timeseries properties)))
         (idx 0)
         (ret (list)))
    (while (< idx (length timeseries))
      (let* ((ts (aref timeseries idx))
             (timestamp (format-time-string "%Y-%m-%dT%H:%M:%S%:z" (encode-time (parse-time-string (cdr (assoc 'time ts))))))
             (data (assoc 'data ts))
             (details (assoc 'details (assoc 'instant data)))
             (next-1-hours (assoc 'next_1_hours data))
             (symbol-code (cdr (assoc 'symbol_code (assoc 'summary next-1-hours))))
             (precipitation-amount (cdr (assoc 'precipitation_amount (assoc 'details next-1-hours)))))
        (when (string-match (format "^%sT" date) timestamp)
          (cl-pushnew `(,timestamp . ((symbol_code . ,(if symbol-code symbol-code nil))
                                      (air_pressure_at_sea_level . ,(format "%.1f%s"
                                                                            (cdr (assoc 'air_pressure_at_sea_level details))
                                                                            (cdr (assoc 'air_pressure_at_sea_level units))))
                                      (air_temperature . ,(format "%.1f%s"
                                                                  (cdr (assoc 'air_temperature details))
                                                                  (if (string= "celsius" (cdr (assoc 'air_temperature units)))
                                                                      "℃"
                                                                    "℉")))
                                      (cloud_area_fraction . ,(format "%.1f%s"
                                                                      (cdr (assoc 'cloud_area_fraction details))
                                                                      (cdr (assoc 'cloud_area_fraction units))))
                                      (precipitation_amount . ,(if precipitation-amount
                                                                   (format "%.1f%s"
                                                                           precipitation-amount
                                                                           (cdr (assoc 'precipitation_amount units)))
                                                                 nil))
                                      (relative_humidity . ,(format "%.1f%s"
                                                                    (cdr (assoc 'relative_humidity details))
                                                                    (cdr (assoc 'relative_humidity units))))
                                      (wind_from_direction . ,(format "%.1f%s"
                                                                      (cdr (assoc 'wind_from_direction details))
                                                                      (if (string= "degrees" (cdr (assoc 'wind_from_direction units)))
                                                                          "°"
                                                                        " radian")))
                                      (wind_speed . ,(format "%.1f%s"
                                                             (cdr (assoc 'wind_speed details))
                                                             (cdr (assoc 'wind_speed units))))))
                      ret
                      :test #'equal))
        (setq idx (1+ idx))))
    (json-encode ret)))

(defun evchan-modeline/update-weather ()
  "Fetch the weather data from `met.no' and save them to the variables."

  (when (and calendar-latitude
             calendar-longitude)
    (let ((url-user-agent "Evchan Modeline for Emacs github.com/kyano/evchan-modeline")
          (url-request-method "GET"))
      (defvar url-http-end-of-headers)
      (condition-case ()
          (url-retrieve
           (format "https://api.met.no/weatherapi/locationforecast/2.0/compact?lat=%s&lon=%s"
                   calendar-latitude
                   calendar-longitude)
           (lambda (status)
             (unless (plist-member status 'error)
               (let* ((weather-data
                       (json-read-from-string
                        (buffer-substring-no-properties (marker-position url-http-end-of-headers)
                                                        (point-max))))
                      (current-weather
                       (assoc 'data (aref (cdr (assoc 'timeseries (assoc 'properties weather-data))) 0)))
                      (code (cdr (assoc 'symbol_code (assoc 'summary (assoc 'next_1_hours current-weather)))))
                      (temperature (cdr (assoc 'air_temperature (assoc 'details (assoc 'instant current-weather)))))
                      (sunrise-sunset (solar-sunrise-sunset (calendar-current-date)))
                      (sunrise (car (car sunrise-sunset)))
                      (sunset (car (nth 1 sunrise-sunset)))
                      (current-time (decode-time))
                      (hour (nth 2 current-time))
                      (minute (nth 1 current-time))
                      (time-of-the-day (+ hour (/ minute (float 60))))
                      (daytime (and (> time-of-the-day sunrise) (< time-of-the-day sunset)))
                      (weather-icon-name (evchan-modeline/weather-icon-name code daytime)))
                 (setq evchan-modeline/weather-data weather-data)
                 (setq evchan-modeline/weather-icon
                       (all-the-icons-weather-icons weather-icon-name)
                       evchan-modeline/weather-temperature
                       (format "%s°C" temperature)))))
           nil t)
        (error nil))))
  (run-with-timer 900 nil #'evchan-modeline/update-weather))

(defun evchan-modeline/display-weather ()
  "Display the weather icon and the current temperature."

  (if (and evchan-modeline/weather-icon
           evchan-modeline/weather-temperature)
      (format "%s%s "
              evchan-modeline/weather-icon
              evchan-modeline/weather-temperature)
    ""))

(defun evchan-modeline/tab-bar-tab-name ()
  "Custom function for `tab-bar-tab-name-function'."

  (let ((buffer (window-buffer (or (minibuffer-selected-window)
                                   (and (window-minibuffer-p)
                                        (get-mru-window))))))
    (with-current-buffer buffer
      (let ((svg-icon (all-the-icons-icon-for-buffer)))
        (unless (stringp svg-icon)
          (setf svg-icon (all-the-icons-octicons "file"
                                                 :face 'all-the-icons-dsilver)))
        (let* ((face (get-text-property 0 'face svg-icon))
               (icon (propertize (format "%s " svg-icon) 'face face)))
          (concat " " icon (buffer-name buffer)))))))

(dolist (buf (buffer-list))
  (with-current-buffer buf
    (setq-local mode-line-buffer-identification
                (evchan-modeline/buffer-identification)
                mode-line-modified
                (evchan-modeline/modified))))
(add-hook 'after-change-major-mode-hook
          #'(lambda ()
              (setq-local mode-line-buffer-identification
                          (evchan-modeline/buffer-identification))))
(add-hook 'post-command-hook
          #'(lambda ()
              (setq-local mode-line-modified
                          (evchan-modeline/modified))))

(add-to-list 'battery-update-functions
             #'evchan-modeline/battery-mode-line-update)
(when (and battery-status-function battery-mode-line-format)
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

(setq tab-bar-tab-name-function
      #'evchan-modeline/tab-bar-tab-name)

(add-to-list 'global-mode-string
             '(t (:eval (evchan-modeline/display-weather))))
(evchan-modeline/update-weather)

(provide 'evchan-modeline)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; evchan-modeline.el ends here
