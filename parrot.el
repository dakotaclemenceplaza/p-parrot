(defun start-parrot (&optional arg)
  "Party Parrot"
  (interactive "sf for fast, s for slow, g for goth, w for wave: ")
  (setq goth nil slow nil wave nil speed 0.07 frame 0)
  (cond ((string= arg "f") (setq speed 0.03))
	((string= arg "s") (setq slow 1))
	((string= arg "g") (setq goth 1))
	((string= arg "w") (setq wave 1)))
  (setq frames (list (list 0 10) (list 2 10) (list 4 10) (list 6 10) (list 8 10) (list 0 10)))
  (switch-to-buffer "Parrot")
  ;window sizes
  (setq winh (window-height) winw (window-width))
  (setq slow-count 10)
  (while 1
    (erase-buffer)
    (draw-parrot)
    ;make more newlines for text
;    (newline (- (/ winh 2) 5))
    ;put text before the bottom end 
;    (goto-char (point-min))
;    (forward-line (- winh 5))
;    (insert (make-string (- (/ winw 2) 5) ? ))
;    (insert "C-g to stop")
    ;colorize
;    (colorize-appropriate)
    ;show everything
    (redisplay) 
    ;keeping track of frames
    ;if slow, then skip changing frame by setting it to previous frame before final increment
    ;keep skipping on fourth frame for slow-count times
;    (if (and slow (= frame 4) (> slow-count 0)) (setq frame (1- frame)
;						      slow-count (1- slow-count)))
    ;after skipping change need to skip the fourth frame to let frames increment further
;    (if (and (= slow-count 0) (= frame 3)) (setq frame 5 slow-count 10))
;    (if (= frame 9)
;	(setq frame 0)
;      (setq frame (1+ frame)))
    (sleep-for speed)))

(defun draw-parrot ()
  (let* ((return-frame
	  (lambda (parrotframe) (cond ((= (car parrotframe) 0) frame-zero)
			   ((= (car parrotframe) 1) frame-one)
			   ((= (car parrotframe) 2) frame-two)
			   ((= (car parrotframe) 3) frame-three)
			   ((= (car parrotframe) 4) frame-four)
			   ((= (car parrotframe) 5) frame-five)
			   ((= (car parrotframe) 6) frame-six)
			   ((= (car parrotframe) 7) frame-seven)
			   ((= (car parrotframe) 8) frame-eight)
			   ((= (car parrotframe) 9) frame-nine))))
	(draw-line
	 (lambda (line-from-frame)
;	   (insert (make-string (- (/ winw 2) 25) ? ))
	   (end-of-line)
	   (insert line-from-frame)
	   (or (= (forward-line) 0) (newline))))
	(parlist (mapcar return-frame frames))
	(change-frame
	 (lambda (frame)
	   (cond ((and slow (= (car frame) 4) (> (car (cdr frame)) 0)) (list (car frame) (1- (car (cdr frame)))))
		 ((and (= (car (cdr frame)) 0) (= (car frame) 3)) (list 4 10))
		 ((= (car frame) 9) (list 0 (car (cdr frame))))
		 (t (list (1+ (car frame)) (car (cdr frame))))))))
    (mapc (lambda (pf) (mapc draw-line pf) (cond ((< (point) 2500) (goto-char (point-min)))
						 ((< (point) 3500) (goto-char (point-max)))
						 (t (forward-line -19))))
	  parlist)
    (setq frames (mapcar change-frame frames))))

					;    (newline (- (/ winh 2) 10))


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
