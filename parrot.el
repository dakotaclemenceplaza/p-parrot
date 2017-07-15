(defun start-parrot (&optional arg)
  "Party Parrot"
  (interactive "sf for fast, s for slow: ")
  (setq slow nil)
  (cond ((string= arg "f") (setq speed 0.03))
	((string= arg "s") (setq slow 1 speed 0.07))
	(t (setq speed 0.07)))
  ;frame length
  (setq length 1018 start 0 end (+ start length))
  (switch-to-buffer "Parrot")
  ;window sizes
  (setq winh (window-height) winw (window-width))
  (while 1
    (erase-buffer)
    ;go down with newlines
    (newline (- (/ winh 2) 10))
    (insert-file-contents "parrotframes" nil start end)
    ;make parrot appear at the middle
    (setq parheight 19)
    (while (> parheight 0)
      ;insert empty strings to move parrot in the middle
      (insert (make-string (- (/ winw 2) 25) ? ))
      (forward-line)
      (setq parheight (- parheight 1)))
    ;make more newlines for text
    (newline (- (/ winh 2) 10))
    ;put text before the bottom end 
    (goto-char (point-min))
    (forward-line (- winh 5))
    (insert (make-string (- (/ winw 2) 5) ? ))
    (insert "C-g to stop")

    (colorize-appropriate start)

    ;show everything
    (redisplay)
    ;keeping track of frames
    (if (= start 4072)
	(if slow (sleep-for 1)))
    (if (= start 9162)
	(setq start 0 end (+ start length))
      (setq start (+ start length) end (+ end length)))
    (sleep-for speed)))

(defun colorize-appropriate (start)
  (let ((choose-color
	 (lambda (color)
	   (add-face-text-property (point-min) (point-max) `(:foreground ,color)))))
    (cond ((or (= start 0) (= start 9162)) (funcall choose-color "#FDFA00"))
	  ((= start 1018) (funcall choose-color "#00F800"))
	  ((= start 2036) (funcall choose-color "#00A4A2"))
	  ((= start 3054) (funcall choose-color "#4179B2"))
          ((= start 4072) (funcall choose-color "#555AB4"))
	  ((= start 5090) (funcall choose-color "#C22E9C"))
	  ((= start 6108) (funcall choose-color "#E02C98"))
	  ((= start 7126) (funcall choose-color "#FE40FD"))
	  ((= start 8144) (funcall choose-color "#E07898")))))

;for test
(start-parrot)
