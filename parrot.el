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
    ;show everything
    (redisplay)
    ;keeping track of frames
    (if (= start 4072)
	(if slow (sleep-for 1)))
    (if (= start 9162)
	(setq start 0 end (+ start length))
      (setq start (+ start length) end (+ end length)))
    (sleep-for speed)))

;for test
(start-parrot)

;1018

(* 1018 4)
