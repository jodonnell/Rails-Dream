(defun open-rails-console-buffer ()
  (let (old-buffer)
    (setq old-buffer (current-buffer))
    (shell "rails-console-buffer")
    (sit-for 1)
    (insert "rails c")
    (comint-send-input)
    (sit-for 12)
    (switch-to-buffer old-buffer)))

(defun get-class ()
  (save-excursion
    (beginning-of-buffer)
    (search-forward-regexp "class[ \t]*")
    (get-token)))

(defun get-token ()
  (let (begin-of-token)
    (setq begin-of-token (point))
    (search-forward-regexp (token-regexp))
    (buffer-substring begin-of-token (point))))

(defun token-regexp ()
  "[_A-Za-z0-9]+")

(defun get-instance-methods (class-name)
  (get-methods class-name "instance_methods"))

(defun get-class-methods (class-name)
  (get-methods class-name "methods"))

(defun get-rails-console-clean-method-output ()
  (beginning-of-buffer)
  (next-line)
  (next-line)
  (beginning-of-line)

  (let (begin-of-methods-pos)
    (setq begin-of-methods-pos (point))
    (search-forward ">")
    (previous-line)
    (end-of-line)
    (buffer-substring begin-of-methods-pos (point))))

(defun get-methods (class-name method-type)
  (save-excursion
    (create-rails-console-buffer-if-does-not-exist)
    (set-buffer "rails-console-buffer")
    (erase-buffer)

    (process-send-string "rails-console-buffer" (concat "(" class-name "." method-type " - Object.methods).sort.collect {|method| puts method.to_s}\n"))
    (sit-for 2)

    (setq list-of-methods (get-rails-console-clean-method-output))
    (switch-to-buffer (get-buffer-create (concat class-name " " method-type)))
    (erase-buffer)
    (insert list-of-methods)
    (beginning-of-buffer)))


(defun create-rails-console-buffer-if-does-not-exist ()
  (if (not (get-buffer "rails-console-buffer"))
      (open-rails-console-buffer)))


(defun get-class-methods-current ()
  (interactive)
  (get-class-methods (get-class)))

(defun get-instance-methods-current ()
  (interactive)
  (get-instance-methods (get-class)))

;(open-rails-console-buffer)
;(get-instance-methods "ActionController::Base")
;(get-class-methods "ActionController::Base")
;(get-instance-methods "HoneyPieController")

  
