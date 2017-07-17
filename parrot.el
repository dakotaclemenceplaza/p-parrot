(defun start-parrot (&optional arg)
  "Party Parrot"
  (interactive "sf for fast, s for slow, g for goth: ")
  (setq goth nil)
  (setq slow nil)
  (setq speed 0.07)
  (cond ((string= arg "f") (setq speed 0.03))
	((string= arg "s") (setq slow 1))
	((string= arg "g") (setq goth 1)))
  ;frame length 
  (setq length 1018 frame 0)
  (switch-to-buffer "Parrot")
  ;window sizes
  (setq winh (window-height) winw (window-width))
  (while 1
    (erase-buffer)
    (draw-parrot frame)
    ;make more newlines for text
    (newline (- (/ winh 2) 5))
    ;put text before the bottom end 
    (goto-char (point-min))
    (forward-line (- winh 5))
    (insert (make-string (- (/ winw 2) 5) ? ))
    (insert "C-g to stop")
    ;colorize
    (if goth
	(add-face-text-property (point-min) (point-max) '(:foreground "#787B80" :weight bold))
      (colorize-appropriate frame))
    ;show everything
    (redisplay) 
    ;keeping track of frames
    (if (and slow (= frame 4)) (sleep-for 1))
    (if (= frame 9)
	(setq frame 0)
      (setq frame (1+ frame))
      (sleep-for speed))))

(defun draw-parrot (frame &optional position)
  (let ((parrotframe (read-frame-from-file frame))
	(draw-frame
	 (lambda (line-from-frame)
	   (insert (make-string (- (/ winw 2) 25) ? ))
	   (insert line-from-frame)
	   (newline))))
    (newline (- (/ winh 2) 10))
    (mapc draw-frame parrotframe)))

(defun read-frame-from-file (frame)
  (let* ((start (* frame length))
	 (end (+ start length)))
    (with-temp-buffer
      (insert-file-contents "parrotframes" nil start end)
      (split-string (buffer-string) "\n" t))))

;function for color
;choose-color holds a lambda which takes a color argument depending on frame
(defun colorize-appropriate (start)
  (let ((choose-color
	 (lambda (color)
	   (add-face-text-property (point-min) (point-max) `(:foreground ,color)))))
    (cond ((or (= start 0) (= start 10)) (funcall choose-color "#FDFA00"))
	  ((= start 1) (funcall choose-color "#00F800"))
	  ((= start 2) (funcall choose-color "#00A4A2"))
	  ((= start 3) (funcall choose-color "#4179B2"))
          ((= start 4) (funcall choose-color "#555AB4"))
	  ((= start 5) (funcall choose-color "#C22E9C"))
	  ((= start 6) (funcall choose-color "#E02C98"))
	  ((= start 7) (funcall choose-color "#FE40FD"))
	  ((= start 8) (funcall choose-color "#E07898")))))

;for test
;(start-parrot)
