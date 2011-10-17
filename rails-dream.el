(defun open-rails-console-buffer ()
  (shell "rails-console-buffer")
  (sit-for 2)
  (insert "rails c")
  (comint-send-input))

(defun get-superclass ()
  (beginning-of-buffer)
  (search-forward "class ")
  (search-forward "<")
  (search-forward-regexp "[ \t]*")
  (let (begin-of-class)
    (setq begin-of-class (point))
    (search-forward-regexp "[_A-Za-z0-9]+")
    (setq class (buffer-substring begin-of-class (point)))))


(defun get-instance-methods (class-name)
  (get-methods class-name "instance_methods"))

(defun get-class-methods (class-name)
  (get-methods class-name "methods"))

(defun get-methods (class-name method-type)
  (set-buffer "rails-console-buffer")
  (erase-buffer)

  (process-send-string "rails-console-buffer" (concat "(" class-name "." method-type " - Object.methods).sort.collect {|method| puts method.to_s}\n"))
  (sit-for 0.5)

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
    (beginning-of-buffer)))

(open-rails-console-buffer)
(get-instance-methods "ActionController::Base")
(get-class-methods "ActionController::Base")
(get-instance-methods "HoneyPieController")

  
