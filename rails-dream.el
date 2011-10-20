(defun create-console-buffer-if-does-not-exist (buffer-name command)
  (if (not (get-buffer buffer-name))
      (open-console-buffer buffer-name command)))

(defun open-console-buffer (buffer-name command)
  (create-console-buffer buffer-name)
  (sit-for 1)
  (process-send-string buffer-name command)
  (sit-for 8))

(defun create-console-buffer (buffer-name)
  (let (old-buffer)
    (setq old-buffer (current-buffer))
    (shell buffer-name)
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

(defun get-rails-console-clean-method-output ()
  (save-excursion
    (set-buffer "rails-console-buffer")
    (beginning-of-buffer)
    (next-line)
    (next-line)
    (beginning-of-line)

    (let (begin-of-methods-pos)
      (setq begin-of-methods-pos (point))
      (search-forward ">")
      (previous-line)
      (end-of-line)
      (buffer-substring begin-of-methods-pos (point)))))

(defun get-methods (class-name method-type)
  (save-excursion
    (create-console-buffer-if-does-not-exist "rails-console-buffer" "rails c\n")
    (set-buffer "rails-console-buffer")
    (erase-buffer)

    (process-send-string "rails-console-buffer" (concat "(" class-name "." method-type " - Object.methods).sort.collect {|method| puts method.to_s}\n"))
    (sit-for 2)

    (switch-to-buffer (get-buffer-create (concat class-name " " method-type)))
    (erase-buffer)
    (insert (get-rails-console-clean-method-output))
    (beginning-of-buffer)))

(defun get-instance-methods (class-name)
  (get-methods class-name "instance_methods"))

(defun get-class-methods (class-name)
  (get-methods class-name "methods"))

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

