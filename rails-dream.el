(defun open-rails-console-buffer ()
  (let (old-buffer)
    (setq old-buffer (current-buffer))
    (shell "rails-console-buffer")
    (sit-for 1)
    (insert "rails c")
    (comint-send-input)
    (sit-for 12)
    (switch-to-buffer old-buffer)))

(defun get-superclass ()
  (save-excursion
    (beginning-of-buffer)
    (search-forward "class ")
    (search-forward "<")
    (search-forward-regexp "[ \t]*")
    (let (begin-of-class)
      (setq begin-of-class (point))
      (search-forward-regexp "[_A-Za-z0-9]+")
      (setq class (buffer-substring begin-of-class (point))))))


(defun get-instance-methods (class-name)
  (get-methods class-name "instance_methods"))

(defun get-class-methods (class-name)
  (get-methods class-name "methods"))

(defun get-methods (class-name method-type)
  (save-excursion
    (create-rails-console-buffer-if-does-not-exist)
    (set-buffer "rails-console-buffer")
    (erase-buffer)

    (process-send-string "rails-console-buffer" (concat "(" class-name "." method-type " - Object.methods).sort.collect {|method| puts method.to_s}\n"))
    (sit-for 2)

    (beginning-of-buffer)
    (next-line)
    (next-line)
    (beginning-of-line)

    (let (begin-of-methods-pos)
      (setq begin-of-methods-pos (point))
      (search-forward ">")
      (previous-line)
      (end-of-line)
      (setq list-of-methods (buffer-substring begin-of-methods-pos (point)))
      (get-buffer-create (concat class-name " " method-type))
      (switch-to-buffer (concat class-name " " method-type))
      (erase-buffer)
      (insert list-of-methods)
      (beginning-of-buffer))))


(defun create-rails-console-buffer-if-does-not-exist ()
  (if (not (get-buffer "rails-console-buffer"))
      (open-rails-console-buffer)))


(defun get-class-methods-current ()
  (interactive)
  (get-class-methods (get-superclass)))

(defun get-instance-methods-current ()
  (interactive)
  (get-instance-methods (get-superclass)))

;(open-rails-console-buffer)
;(get-instance-methods "ActionController::Base")
;(get-class-methods "ActionController::Base")
;(get-instance-methods "HoneyPieController")

  
