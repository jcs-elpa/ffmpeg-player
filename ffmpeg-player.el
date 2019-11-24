;;; ffmpeg-player.el --- Play video using ffmpeg.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shen, Jen-Chieh
;; Created date 2019-11-20 13:28:20

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Play video using ffmpeg.
;; Keyword: video ffmpeg buffering images
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3") (s "1.12.0"))
;; URL: https://github.com/jcs090218/ffmpeg-player

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Play video using ffmpeg.
;;

;;; Code:

(require 's)


(defgroup ffmpeg-player nil
  "Play video using ffmpeg."
  :prefix "ffmpeg-player-"
  :group 'tool
  :link '(url-link :tag "Github" "https://github.com/jcs090218/ffmpeg-player"))


(defcustom ffmpeg-player-images-directory (format "%s%s"
                                                  user-emacs-directory
                                                  "ffmpeg-player/images/")
  "Directory that stores video images."
  :type 'string
  :group 'ffmpeg-player)

(defcustom ffmpeg-player-buffer-name "*ffmpeg-player* : %s"
  "Buffer name of the video player."
  :type 'string
  :group 'ffmpeg-player)

(defcustom ffmpeg-player-image-prefix "snap"
  "Prefix when output images."
  :type 'string
  :group 'ffmpeg-player)

(defcustom ffmpeg-player-image-extension "jpg"
  "Image extension when output from ffmpeg."
  :type 'string
  :group 'ffmpeg-player)

(defcustom ffmpeg-player-fixed-id "%09d"
  "Fixed id for the images."
  :type 'string
  :group 'ffmpeg-player)

(defconst ffmpeg-player--command-video-to-images
  "ffmpeg -i \"%s\" \"%s%s%s.%s\""
  "Command that convert video to image source.")

(defconst ffmpeg-player--command-video-to-audio
  ""
  "Command that convert video to audio source.")

(defconst ffmpeg-player--command-kill-process
  ""
  "Command that kill ffmpeg process.")


(defvar ffmpeg-player--frame-regexp nil
  "Frame regular expression for matching length.")

(defvar ffmpeg-player--current-fps 0.0 "Current FPS that are being used.")
(defvar ffmpeg-player--first-frame-time 0.2 "Time to check if the first frame exists.")
(defvar ffmpeg-player--buffer-time 0.0 "Time to update buffer frame, calculate with FPS.")

(defvar ffmpeg-player--start-time 0.0 "Record video start time.")
(defvar ffmpeg-player--video-timer 0.0 "Time to record delta time.")

(defvar ffmpeg-player--frame-index 0 "Current frame index/counter.")

(defvar ffmpeg-player--first-frame-timer nil "Timer that find out the first frame.")

(defvar ffmpeg-player--buffer nil "Buffer that displays video.")
(defvar ffmpeg-player--buffer-timer nil "Timer that will update the image buffer.")


;;; Command

(defun ffmpeg-player--form-command (path source)
  "From the command by needed parameters.
PATH is the input video file.  SOURCE is the output image directory."
  (format ffmpeg-player--command-video-to-images
          path
          source
          ffmpeg-player-image-prefix
          ffmpeg-player-fixed-id
          ffmpeg-player-image-extension))

(defun ffmpeg-player--kill-ffmpeg ()
  "Kill ffmpeg by command."
  (shell-command ffmpeg-player--command-kill-process))

;;; Util

(defun ffmpeg-player--safe-path (path)
  "Check if safe PATH."
  (unless (file-exists-p path) (setq path (expand-file-name path)))
  (if (file-exists-p path) path nil))

(defun ffmpeg-player--clean-video-images ()
  "Clean up all video images."
  (delete-directory (expand-file-name ffmpeg-player-images-directory) t))

(defun ffmpeg-player--ensure-video-directory-exists ()
  "Ensure the video directory exists so we can put our image files."
  (unless (file-directory-p (expand-file-name ffmpeg-player-images-directory))
    (make-directory (expand-file-name ffmpeg-player-images-directory) t)))

;;; Buffer

(defun ffmpeg-player--buffer-name (path)
  "Return current ffmpeg play buffer name by PATH."
  (format ffmpeg-player-buffer-name (f-filename path)))

(defun ffmpeg-player--create-video-buffer (path)
  "Create a new video buffer with PATH."
  (let* ((name (ffmpeg-player--buffer-name path))
         (buf (if (get-buffer name) (get-buffer name) (generate-new-buffer name))))
    (setq ffmpeg-player--buffer buf)
    (with-current-buffer buf (ffmpeg-player-mode))
    (ffmpeg-player--update-frame-by-string "[Nothing to display yet...]")
    buf))

(defun ffmpeg-player--buffer-alive-p ()
  "Check if the video buffer alive."
  (buffer-name (get-buffer ffmpeg-player--buffer)))

;;; First frame

(defun ffmpeg-player--set-first-frame-timer ()
  "Set the first frame timer task."
  (ffmpeg-player--kill-first-frame-timer)
  (setq ffmpeg-player--first-frame-timer
        (run-with-timer ffmpeg-player--first-frame-time nil 'ffmpeg-player--check-first-frame)))

(defun ffmpeg-player--kill-first-frame-timer ()
  "Kill the first frame timer.
Information about first frame timer please see variable `ffmpeg-player--first-frame-timer'."
  (when (timerp ffmpeg-player--first-frame-timer)
    (cancel-timer ffmpeg-player--first-frame-timer)
    (setq ffmpeg-player--first-frame-timer nil)))

(defun ffmpeg-player--form-file-extension-regexp ()
  "Form regular expression for search image file."
  (format "\\.%s$" ffmpeg-player-image-extension))

(defun ffmpeg-player--check-first-frame ()
  "Core function to check first frame image is ready."
  (let ((images (directory-files (expand-file-name ffmpeg-player-images-directory) nil (ffmpeg-player--form-file-extension-regexp)))
        (first-frame nil))
    (if (not images)
        (ffmpeg-player--set-first-frame-timer)
      (setq first-frame (nth 0 images))
      (setq first-frame (s-replace ffmpeg-player-image-prefix "" first-frame))
      (setq first-frame (s-replace-regexp (ffmpeg-player--form-file-extension-regexp) "" first-frame))
      (setq ffmpeg-player--frame-regexp (format "%s%sd" "%0" (length first-frame)))
      (ffmpeg-player--resolve-fps)
      (setq ffmpeg-player--start-time (float-time))
      (ffmpeg-player--update-frame))))

;;; Frame

(defun ffmpeg-player--set-buffer-timer ()
  "Set the buffer timer task."
  (ffmpeg-player--kill-buffer-timer)
  (setq ffmpeg-player--buffer-timer
        (run-with-timer ffmpeg-player--buffer-time nil 'ffmpeg-player--update-frame)))

(defun ffmpeg-player--kill-buffer-timer ()
  "Kill the buffer timer."
  (when (timerp ffmpeg-player--buffer-timer)
    (cancel-timer ffmpeg-player--buffer-timer)
    (setq ffmpeg-player--buffer-timer nil)))

(defun ffmpeg-player--form-frame-filename ()
  "Form the frame filename."
  (format "%s%s.%s"
          ffmpeg-player-image-prefix
          (format ffmpeg-player--frame-regexp ffmpeg-player--frame-index)
          ffmpeg-player-image-extension))

(defun ffmpeg-player--update-frame-by-image-path (path)
  "Update the frame by image PATH."
  (if (not (ffmpeg-player--buffer-alive-p))
      (ffmpeg-player--clean-up)
    (with-current-buffer ffmpeg-player--buffer
      (erase-buffer)
      (insert-image-file path))))

(defun ffmpeg-player--update-frame-by-string (str)
  "Update the frame by STR."
  (if (not (ffmpeg-player--buffer-alive-p))
      (ffmpeg-player--clean-up)
    (with-current-buffer ffmpeg-player--buffer
      (erase-buffer)
      (insert str))))

(defun ffmpeg-player--update-frame ()
  "Core logic to update frame."
  (if (not (ffmpeg-player--buffer-alive-p))
      (user-error "[WARNING] Display buffer no longer alived")
    ;; Calculate the time passed.
    (setq ffmpeg-player--video-timer (- (float-time) ffmpeg-player--start-time))
    ;; Calculate the frame index.
    (setq ffmpeg-player--frame-index (ceiling (* ffmpeg-player--current-fps ffmpeg-player--video-timer)))
    ;; Start refresh image display.
    (let ((frame-file (concat ffmpeg-player-images-directory (ffmpeg-player--form-frame-filename))))
      (if (file-exists-p frame-file)
          (progn
            (ffmpeg-player--update-frame-by-image-path frame-file)
            (ffmpeg-player--set-buffer-timer))
        (ffmpeg-player--update-frame-by-string "[Done display...]")))))

;;; Core

(defun ffmpeg-player--output-p ()
  "Check if output available."
  (not (string-empty-p
        (with-current-buffer (get-buffer "*Async Shell Command*")
          (buffer-string)))))

(defun ffmpeg-player--resolve-fps ()
  "Resolve FPS."
  (while (not (ffmpeg-player--output-p)))  ; ATTENTION: Make it hang.
  (setq ffmpeg-player--current-fps
        (with-current-buffer (get-buffer "*Async Shell Command*")
          (goto-char (point-min))
          (let ((end-pt -1))
            (search-forward "fps,")
            (search-backward " ")
            (setq end-pt (1- (point)))
            (search-backward " ")
            (substring (buffer-string) (point) end-pt))))
  (setq ffmpeg-player--current-fps (string-to-number ffmpeg-player--current-fps))
  (setq ffmpeg-player--buffer-time (/ 1.0 ffmpeg-player--current-fps)))

(defun ffmpeg-player--clean-up ()
  "Reset/Clean up some variable before we play a new video."
  (ffmpeg-player--kill-first-frame-timer)
  (ffmpeg-player--kill-buffer-timer)
  (setq ffmpeg-player--buffer nil)
  (setq ffmpeg-player--start-time 0.0)
  (setq ffmpeg-player--video-timer 0.0)
  (setq ffmpeg-player--current-fps 0.0)
  (setq ffmpeg-player--frame-index 0)
  (setq ffmpeg-player--frame-regexp nil))

(defun ffmpeg-player--video (path)
  "Play the video with PATH."
  (setq path (ffmpeg-player--safe-path path))
  (if (not path)
      (user-error "[ERROR] Input video file doesn't exists: %s" path)
    (ffmpeg-player--clean-video-images)
    (ffmpeg-player--ensure-video-directory-exists)
    (ffmpeg-player--clean-up)
    (async-shell-command (ffmpeg-player--form-command path (expand-file-name ffmpeg-player-images-directory)))
    (ffmpeg-player--create-video-buffer path)
    (switch-to-buffer-other-window ffmpeg-player--buffer)
    (ffmpeg-player--check-first-frame)))

(define-derived-mode ffmpeg-player-mode fundamental-mode "ffmpeg-player"
  "Major mode for player ffmpeg video."
  :group 'ffmpeg-player
  (buffer-disable-undo)
  )


(provide 'ffmpeg-player)
;;; ffmpeg-player.el ends here
