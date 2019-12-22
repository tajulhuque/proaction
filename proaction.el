  

(require 'cl)
(require 'org)

(defvar *goal-list* nil)


(defun start-goal-program ()
  (interactive)
  (setq *goal-list*  nil)
  (create-goals-buffer))

(defun add-goal (goal)
  (setq *goal-list* (append *goal-list* (list (list goal nil)))))


(defun create-goals-buffer ()
  (let ((goals-buffer (get-buffer-create "*Goals*"))
	(goals-display (get-goal-string *goal-list*)))
    (with-current-buffer goals-buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert goals-display)
      (goto-char (point-min))
      (setup-buttons)
      (setq buffer-read-only t))
    (switch-to-buffer goals-buffer)))


(defun get-goal-string (goals-list)
  (concat 
   "  *** Goals for this week *** \n\n"
   (let ((goal-num 0))
     
     (apply #'concat (mapcar #'(lambda (goal)
				 (setq goal-num (1+ goal-num))
				 (concat (format "%s. %s (schedule) %s\n" goal-num (car goal) (car (cdr goal)))))
			     goals-list)))
   "\n\n  Add Goal         Edit Roles\n"))


(defun setup-buttons ()

  (cl-flet ((create-goal-button ()
				(let* ((goal-btn-text "Add Goal")
				       (goal-btn-width (length goal-btn-text)))
				  (goto-char 1)
				  (re-search-forward goal-btn-text)
				  (make-button (- (point) goal-btn-width) (point) :type 'goal-button)))

	    (create-schedule-buttons ()
				     (let* ((btn-text "(schedule)")
					    (btn-text-width (length btn-text)))
				       (goto-char 1)
				       (while (re-search-forward btn-text nil t)
					 (make-button (- (point) btn-text-width) (point) :type 'schedule-button)))))
    
    (create-schedule-buttons)
    (create-goal-button)))
	  


(defun schedule-button-pressed (button)
  (let* ((line-text (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
	 (goal-num-of-line (first (split-string line-text "\\.")))
	 (goal-number (string-to-number goal-num-of-line))
	 (goal (nth (- goal-number 1) *goal-list*))
	 (selected-date (org-read-date)))
    (setf (car (cdr goal)) selected-date))
  (create-goals-buffer))


(defun goal-button-pressed (button)
  (add-goal (read-string "Enter a goal: "))
  (create-goals-buffer))


(define-button-type 'schedule-button
  'action 'schedule-button-pressed
  'follow-link t
  'help-echo "Schedule this Goal"
  'help-args nil)


(define-button-type 'goal-button
  'action 'goal-button-pressed
  'follow-link t
  'help-echo "Add a Goal for this week"
  'help-args "test")

(start-goal-program)


