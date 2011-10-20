;  "class User"

(defun create-console-buffer-if-does-not-exist (buffer-name command)
  (defun open-console-buffer ()
    (create-console-buffer)
    (sit-for 1)
    (process-send-string buffer-name command)
    (sit-for 8))

  (defun create-console-buffer ()
    (let (old-buffer)
      (setq old-buffer (current-buffer))
      (shell buffer-name)
      (switch-to-buffer old-buffer)))

  (if (not (get-buffer buffer-name))
      (open-console-buffer)))

(defun get-class ()
  (save-excursion
    (beginning-of-buffer)
    (search-forward-regexp "class[ \t]*")
    (thing-at-point 'word)))

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


(defun call-interactive-shell-command (buffer start-interactive-shell-command interactive-shell-command)
  "Use to call an interactive shell command, first call will create the buffer and open the command, all calls will run the command you pass in interactive-shell-command"
  (create-console-buffer-if-does-not-exist buffer start-interactive-shell-command)
  (set-buffer buffer)
  (erase-buffer)

  (process-send-string buffer interactive-shell-command)
  (sit-for 2))

(defun get-methods (class-name method-type)
  (save-excursion
    (call-interactive-shell-command "rails-console-buffer" "rails c\n" (concat "(" class-name "." method-type " - Object.methods).sort.collect {|method| puts method.to_s}\n"))
    (create-cleaned-output-buffer (concat class-name " " method-type) 'get-rails-console-clean-method-output)))

(defun create-cleaned-output-buffer (buffer-name clean-output-function)
    (switch-to-buffer (get-buffer-create buffer-name))
    (erase-buffer)
    (insert (funcall clean-output-function))
    (beginning-of-buffer))

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

; (create-console-buffer-if-does-not-exist "rails-console-buffer" "rails c\n")
;(get-instance-methods "ActionController::Base")
;(get-class-methods "ActionController::Base")
;(get-instance-methods "HoneyPieController")

(defun get-rails-documentation ()
  (interactive)
  (call-interactive-shell-command "ri-console-buffer" "ri -i -T\n" (concat (thing-at-point 'symbol) "\n"))
  (create-cleaned-output-buffer (concat (thing-at-point 'symbol) " documentation") 'get-ri-console-clean-output))

(defun get-ri-console-clean-output ()
  (save-excursion
    (set-buffer "ri-console-buffer")
    (beginning-of-buffer)
    (next-line 3)
    (beginning-of-line)

    (let (begin-of-methods-pos)
      (setq begin-of-methods-pos (point))
      (end-of-buffer)
      (previous-line)
      (end-of-line)
      (buffer-substring begin-of-methods-pos (point)))))
