(defun start-parrot (&optional arg)
  "Party Parrot"
  (interactive "sf for fast, s for slow, g for goth: ")
  (setq goth nil slow nil speed 0.07)
  (cond ((string= arg "f") (setq speed 0.03))
	((string= arg "s") (setq slow 1))
	((string= arg "g") (setq goth 1)))
  ;frame length and count
  (setq length 1018 frame 0)
  (switch-to-buffer "Parrot")
  ;window sizes
  (setq winh (window-height) winw (window-width))
  (while 1
    (erase-buffer)
    (draw-parrot)
    ;make more newlines for text
    (newline (- (/ winh 2) 5))
    ;put text before the bottom end 
    (goto-char (point-min))
    (forward-line (- winh 5))
    (insert (make-string (- (/ winw 2) 5) ? ))
    (insert "C-g to stop")
    ;colorize
    (colorize-appropriate)
    ;show everything
    (redisplay) 
    ;keeping track of frames
    (if (and slow (= frame 4)) (sleep-for 1))
    (if (= frame 9)
	(setq frame 0)
      (setq frame (1+ frame)))
    (sleep-for speed)))

(defun draw-parrot (&optional position)
  (let ((parrotframe (read-frame-from-file))
	(draw-frame
	 (lambda (line-from-frame)
	   (insert (make-string (- (/ winw 2) 25) ? ))
	   (insert line-from-frame)
	   (newline))))
    (newline (- (/ winh 2) 10))
    (mapc draw-frame parrotframe)))

(defun read-frame-from-file ()
  (let ((start (* frame length)))
    (with-temp-buffer
      (insert-file-contents "~/p-parrot/parrotframes" nil start (+ start length))
      (split-string (buffer-string) "\n" t))))

;function for color
;choose-color holds a lambda which takes a color argument depending on frame
(defun colorize-appropriate ()
  (if goth
      (add-face-text-property (point-min) (point-max) '(:foreground "#787B80" :weight bold))
    (let ((choose-color
	   (lambda (color)
	     (add-face-text-property (point-min) (point-max) `(:foreground ,color)))))
      (cond ((or (= frame 0) (= frame 9)) (funcall choose-color "#FDFA00"))
	    ((= frame 1) (funcall choose-color "#00F800"))
	    ((= frame 2) (funcall choose-color "#00A4A2"))
	    ((= frame 3) (funcall choose-color "#4179B2"))
	    ((= frame 4) (funcall choose-color "#555AB4"))
	    ((= frame 5) (funcall choose-color "#C22E9C"))
	    ((= frame 6) (funcall choose-color "#E02C98"))
	    ((= frame 7) (funcall choose-color "#FE40FD"))
	    ((= frame 8) (funcall choose-color "#E07898"))))))

;for test
;(start-parrot)
