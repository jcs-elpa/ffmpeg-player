;;; ffmpeg-player.el --- Play video using ffmpeg.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shen, Jen-Chieh
;; Created date 2019-11-20 13:28:20

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Play video using ffmpeg.
;; Keyword: video ffmpeg buffering images
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4") (s "1.12.0") (f "0.20.0"))
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

(require 'f)
(require 's)


(defgroup ffmpeg-player nil
  "Play video using ffmpeg."
  :prefix "ffmpeg-player-"
  :group 'tool
  :link '(url-link :tag "Github" "https://github.com/jcs090218/ffmpeg-player"))


(defcustom ffmpeg-player-images-directory (expand-file-name
                                           (format "%s%s"
                                                   user-emacs-directory
                                                   "ffmpeg-player/images/"))
  "Directory that stores video images."
  :type 'string
  :group 'ffmpeg-player)

(defcustom ffmpeg-player-buffer-name "*ffmpeg-player* : %s"
  "Buffer name of the video player."
  :type 'string
  :group 'ffmpeg-player)

(defcustom ffmpeg-player-loop t
  "Loop when the video end."
  :type 'boolean
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


(defvar ffmpeg-player--pause nil
  "Flag for pausing video.")
(defvar ffmpeg-player--pause-by-frame-not-ready nil
  "Flag to check if pause by frame is not ready yet.")

(defvar ffmpeg-player--frame-regexp nil
  "Frame regular expression for matching length.")

(defvar ffmpeg-player--current-duration 0.0 "Current video length/duration.")

(defvar ffmpeg-player--current-fps 0.0 "Current FPS that are being used.")
(defvar ffmpeg-player--first-frame-time 0.2 "Time to check if the first frame exists.")
(defvar ffmpeg-player--buffer-time 0.0 "Time to update buffer frame, calculate with FPS.")

(defvar ffmpeg-player--video-timer 0.0 "Time to record delta time.")

(defvar ffmpeg-player--last-time 0.0
  "Record last time for each frame, used for calculate delta time.")
(defvar ffmpeg-player--delta-time 0.0
  "Record delta time for each frame.")

(defvar ffmpeg-player--frame-index 0 "Current frame index/counter.")

(defvar ffmpeg-player--first-frame-timer nil "Timer that find out the first frame.")

(defvar ffmpeg-player--buffer nil "Buffer that displays video.")
(defvar ffmpeg-player--buffer-timer nil "Timer that will update the image buffer.")

(defvar ffmpeg-player--resolve-clip-info-timer nil "Timer that try to resolve FPS.")
(defvar ffmpeg-player--resolve-clip-info-time 0.2 "Time to check if fps could be resolved.")


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

;;; Util

(defun ffmpeg-player--safe-path (path)
  "Check if safe PATH."
  (unless (file-exists-p path) (setq path (expand-file-name path)))
  (if (file-exists-p path) path nil))

(defun ffmpeg-player--clean-video-images ()
  "Clean up all video images."
  (delete-directory ffmpeg-player-images-directory t))

(defun ffmpeg-player--ensure-video-directory-exists ()
  "Ensure the video directory exists so we can put our image files."
  (unless (file-directory-p ffmpeg-player-images-directory)
    (make-directory ffmpeg-player-images-directory t)))

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

;;; Delta Time

(defun ffmpeg-player--calc-delta-time ()
  "Calculate the delta time."
  (if ffmpeg-player--pause
      (setq ffmpeg-player--delta-time 0.0)
    (setq ffmpeg-player--delta-time (- (float-time) ffmpeg-player--last-time)))
  (setq ffmpeg-player--last-time (float-time)))

;;; Resolve FPS

(defun ffmpeg-player--set-resolve-clip-info-timer ()
  "Set the resolve clip information timer task."
  (ffmpeg-player--kill-resolve-clip-info-timer)
  (setq ffmpeg-player--resolve-clip-info-timer
        (run-with-timer ffmpeg-player--resolve-clip-info-time nil 'ffmpeg-player--check-resolve-clip-info)))

(defun ffmpeg-player--kill-resolve-clip-info-timer ()
  "Kill the resolve clip information timer."
  (when (timerp ffmpeg-player--resolve-clip-info-timer)
    (cancel-timer ffmpeg-player--resolve-clip-info-timer)
    (setq ffmpeg-player--resolve-clip-info-timer nil)))

(defun ffmpeg-player--output-p ()
  "Check if output available."
  (save-window-excursion
    (switch-to-buffer (get-buffer "*Async Shell Command*"))
    (not (string-empty-p (buffer-string)))))

(defun ffmpeg-player--get-fps ()
  "Get the FPS from async shell command output buffer."
  (with-current-buffer (get-buffer "*Async Shell Command*")
    (goto-char (point-min))
    (let ((end-pt -1))
      (search-forward "fps,")
      (search-backward " ")
      (setq end-pt (1- (point)))
      (search-backward " ")
      (substring (buffer-string) (point) end-pt))))

(defun ffmpeg-player--get-duration ()
  "Get the duration from async shell command output buffer."
  (with-current-buffer (get-buffer "*Async Shell Command*")
    (goto-char (point-min))
    (let ((start-pt -1))
      (search-forward "Duration: ")
      (setq start-pt (1- (point)))
      (search-forward ",")
      (substring (buffer-string) start-pt (- (point) 2)))))

(defun ffmpeg-player--string-to-float-time (str-time)
  "Convert STR-TIME to float time."
  (let* ((split-time (split-string str-time ":"))
         (time-arg (1- (length split-time)))
         (float-time 0.0))
    (dolist (st split-time)
      (setq st (string-to-number st))
      (setq float-time (+ float-time (* st (expt 60 time-arg))))
      (setq time-arg (1- time-arg)))
    float-time))

(defun ffmpeg-player--resolve-clip-info ()
  "Resolve clip's information."
  (setq ffmpeg-player--current-duration
        (ffmpeg-player--string-to-float-time (ffmpeg-player--get-duration)))
  (setq ffmpeg-player--current-fps (ffmpeg-player--get-fps))
  (setq ffmpeg-player--current-fps (string-to-number ffmpeg-player--current-fps))
  (setq ffmpeg-player--buffer-time (/ 1.0 ffmpeg-player--current-fps)))

(defun ffmpeg-player--check-resolve-clip-info ()
  "Check if resolved clip inforamtion."
  (if (not (ffmpeg-player--output-p))
      (progn
        (message "[INFO] Waiting to resolve clip information")
        (ffmpeg-player--set-resolve-clip-info-timer))
    (ffmpeg-player--resolve-clip-info)
    (ffmpeg-player--check-first-frame)))

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
  (if (f-empty? ffmpeg-player-images-directory)
      (ffmpeg-player--set-first-frame-timer)
    (let ((images (directory-files ffmpeg-player-images-directory nil (ffmpeg-player--form-file-extension-regexp)))
          (first-frame nil))
      (setq first-frame (nth 0 images))
      (setq first-frame (s-replace ffmpeg-player-image-prefix "" first-frame))
      (setq first-frame (s-replace-regexp (ffmpeg-player--form-file-extension-regexp) "" first-frame))
      (setq ffmpeg-player--frame-regexp (format "%s%sd" "%0" (length first-frame)))
      (setq ffmpeg-player--last-time (float-time))
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

(defun ffmpeg-player--update-frame-index ()
  "Calculate then update the frame index by time."
  ;; Calculate the time passed.
  (setq ffmpeg-player--video-timer (+ ffmpeg-player--video-timer ffmpeg-player--delta-time))
  ;; Calculate the frame index.
  (setq ffmpeg-player--frame-index (ceiling (* ffmpeg-player--current-fps ffmpeg-player--video-timer))))

(defun ffmpeg-player--update-frame-image ()
  "Refresh image display."
  (if (ffmpeg-player--done-playing-p)
      (if (not ffmpeg-player-loop)
          (ffmpeg-player--update-frame-by-string "[INFO] Done display...")
        )
    (let ((frame-file (concat ffmpeg-player-images-directory (ffmpeg-player--form-frame-filename))))
      (if (not (file-exists-p frame-file))
          (progn
            (setq ffmpeg-player--pause-by-frame-not-ready t)
            (ffmpeg-player-pause)
            (ffmpeg-player--update-frame-by-string "[INFO] Frame not ready"))
        (when ffmpeg-player--pause-by-frame-not-ready
          (ffmpeg-player-unpause)
          (setq ffmpeg-player--pause-by-frame-not-ready nil))
        (ffmpeg-player--update-frame-by-image-path frame-file))
      (ffmpeg-player--set-buffer-timer))))

(defun ffmpeg-player--update-frame ()
  "Core logic to update frame."
  (if (not (ffmpeg-player--buffer-alive-p))
      (user-error "[WARNING] Display buffer no longer alived")
    (ffmpeg-player--calc-delta-time)
    (ffmpeg-player--update-frame-index)
    (ffmpeg-player--update-frame-image)))

;;; Core

(defun ffmpeg-player--done-playing-p ()
  "Check if done playing the clip."
  (<= ffmpeg-player--current-duration ffmpeg-player--video-timer))

(defun ffmpeg-player--clean-up ()
  "Reset/Clean up some variable before we play a new video."
  (ffmpeg-player--kill-first-frame-timer)
  (ffmpeg-player--kill-resolve-clip-info-timer)
  (ffmpeg-player--kill-buffer-timer)
  (setq ffmpeg-player--buffer nil)
  (progn  ; Clean delta time
    (setq ffmpeg-player--last-time 0.0)
    (setq ffmpeg-player--delta-time 0.0))
  (setq ffmpeg-player--video-timer 0.0)
  (setq ffmpeg-player--current-duration 0.0)
  (setq ffmpeg-player--current-fps 0.0)
  (setq ffmpeg-player--frame-index 0)
  (setq ffmpeg-player--frame-regexp nil)
  (progn  ; User settings
    (ffmpeg-player-unpause)))

(defun ffmpeg-player--video (path)
  "Play the video with PATH."
  (setq path (ffmpeg-player--safe-path path))
  (if (not path)
      (user-error "[ERROR] Input video file doesn't exists: %s" path)
    ;;(ffmpeg-player--clean-video-images)
    (ffmpeg-player--ensure-video-directory-exists)
    (ffmpeg-player--clean-up)
    (async-shell-command (ffmpeg-player--form-command path ffmpeg-player-images-directory))
    (ffmpeg-player--create-video-buffer path)
    (switch-to-buffer-other-window ffmpeg-player--buffer)
    (ffmpeg-player--check-resolve-clip-info)))

;; Mode

(defun ffmpeg-player-unpause ()
  "Unpause the video."
  (interactive)
  (setq ffmpeg-player--pause nil))

(defun ffmpeg-player-pause ()
  "Pause the video."
  (interactive)
  (setq ffmpeg-player--pause t))

(defun ffmpeg-player-pause-or-unpause ()
  "Pause or unpause video."
  (interactive)
  (if ffmpeg-player--pause (ffmpeg-player-unpause) (ffmpeg-player-pause)))

(defun ffmpeg-player--move-timeline (n)
  "Move video timeline by N seconds."
  (setq ffmpeg-player--video-timer (+ ffmpeg-player--video-timer n))
  (cond ((< ffmpeg-player--video-timer 0.0)
         (setq ffmpeg-player--video-timer 0.0))
        ((> ffmpeg-player--video-timer ffmpeg-player--current-duration)
         (setq ffmpeg-player--video-timer ffmpeg-player--current-duration))))

(defun ffmpeg-player-backward-10 ()
  "Backward time 10 seconds."
  (interactive)
  (ffmpeg-player--move-timeline -10.0))

(defun ffmpeg-player-forward-10 ()
  "Forward time 10 seconds."
  (interactive)
  (ffmpeg-player--move-timeline 10.0))


(defvar ffmpeg-player-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") #'ffmpeg-player-pause-or-unpause)
    (define-key map (kbd "<left>") #'ffmpeg-player-backward-10)
    (define-key map (kbd "<right>") #'ffmpeg-player-forward-10)
    map)
  "Keymap used in `ffmpeg-player-mode'.")

(define-derived-mode ffmpeg-player-mode fundamental-mode "ffmpeg-player"
  "Major mode for play ffmpeg video."
  :group 'ffmpeg-player
  (buffer-disable-undo)
  (use-local-map ffmpeg-player-mode-map))


(ffmpeg-player--video (expand-file-name "./test/1.avi"))

(provide 'ffmpeg-player)
;;; ffmpeg-player.el ends here
