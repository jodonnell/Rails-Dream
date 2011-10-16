(defun open-rails-console-buffer ()
  (shell "rails-console-buffer")
  (sleep-for 2)
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

(defun get-instance-methods ()
  (set-buffer "rails-console-buffer")
  (erase-buffer)

  (process-send-string "rails-console-buffer" "(ActionController::Base.instance_methods - Object.methods).collect {|method| puts method.to_s}\n")
  (sit-for 0.5)

  (beginning-of-buffer)
  (next-line)
  (next-line)
  (beginning-of-line)

  (let (begin-of-methods)
    (setq begin-of-methods (point))
    (search-forward ">")
    (previous-line)
    (end-of-line)
    (setq class (buffer-substring begin-of-methods (point)))
    (get-buffer-create "ActionController::Base instance methods")
    (switch-to-buffer "ActionController::Base instance methods")


    (insert class)))

(open-rails-console-buffer)
(get-instance-methods)

  
