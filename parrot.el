;; PARTY PARROT
;; Don't forget to load parrotframes.el

(defun p-parrot (&optional arg)
  "Party Parrot"
  (interactive "sf for fast, s for slow, g for goth, w for wave: ")
  ;;setting various variables
  ;;frames is a list of pairs with a frame number and a count for a pause
  (setq goth nil slow nil wave nil speed 0.07 frames (list (list 0 10)))
  (cond ((string= arg "f") (setq speed 0.03))
	((string= arg "s") (setq slow 1))
	((string= arg "g") (setq goth 1))
	((string= arg "w") (setq wave 1)))
  (if wave (setq slow 1 frames (list (list 0 10) (list 2 10) (list 4 10) (list 6 10) (list 8 10) (list 0 10))))
  (switch-to-buffer "Parrot")
  ;;window sizes
  (setq winh (window-height) winw (window-width))
  (while 1
    (erase-buffer)
    (draw-parrot)
    ;;show everything
    (redisplay) 
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
	   (if wave (progn (end-of-line)
			   (insert (make-string (/ (- (/ winw 3) 50) 2) ? ))
			   (setq p-min (point))
			   (insert line-from-frame)
			   (setq p-max (point))
			   (insert (make-string (/ (- (/ winw 3) 50) 3) ? ))
			   (or (= (forward-line) 0)
			       (newline))
			   (colorize))
	     (progn (insert (make-string (- (/ winw 2) 25) ? ))
		    (insert line-from-frame)
		    (newline)))))
	(parlist (mapcar return-frame frames))
	;;keeping track of frames
	(change-frame
	 (lambda (frame)
	   (cond ((and slow (= (car frame) 4) (> (car (cdr frame)) 0)) (list (car frame) (1- (car (cdr frame)))))
		 ((and (= (car (cdr frame)) 0) (= (car frame) 3)) (list 4 10))
		 ((= (car frame) 9) (list 0 (car (cdr frame))))
		 (t (list (1+ (car frame)) (car (cdr frame))))))))
    ;;end of let*
    (if wave (progn
	       ;;map over parlist which is a list of six frames
	       (mapc (lambda (pf)
		       (setq current-color (car pf))
		       ;;map draw-line over a frame
		       (mapc draw-line (cdr pf))
		       ;;logic for arranging parrots on screen
		       (cond ((< (point) 2500) (goto-char (point-min)))
			     ((< (point) 3500) (goto-char (point-max)))
			     (t (forward-line -19))))
		     parlist)
	       (insert-text))
      ;;else
      (progn (newline (- (/ winh 2) 10))
	     ;;draw every line from a frame, parlist has only one frame
	     (mapc draw-line (cdr (car parlist)))
	     (insert-text)
	     ;;set color and range for colorize
	     (setq current-color (car (car parlist)) p-min (point-min) p-max (point-max))
	     (colorize)))
    ;;end of if-else
    ;;change frames
    (setq frames (mapcar change-frame frames))))

(defun colorize ()
  (if goth
      (add-face-text-property (point-min) (point-max) '(:foreground "#787B80" :weight bold))
    (add-face-text-property p-min p-max `(:foreground ,current-color))))

(defun insert-text ()
  ;;make more newlines for text
  (if wave (progn (goto-char (point-max))
		  (newline (/ (- winh 38) 2)))
    (newline (/ (- winh 19) 4)))
  ;;put text before the bottom end
  (insert (make-string (- (/ winw 2) 5) ? ))
  (insert "C-g to stop"))
