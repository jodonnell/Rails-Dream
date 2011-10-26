;  "class User"

(defun create-console-buffer-if-does-not-exist (buffer-name command)
  (defun open-console-buffer ()
    (create-console-buffer)
    (sit-for 1)
    (process-send-string buffer-name command)
    (sit-for 8))

  (defun create-console-buffer ()
    (let ((old-buffer (current-buffer)))
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

    (let ((begin-of-methods-pos (point)))
      (search-forward ">")
      (previous-line)
      (end-of-line)
      (buffer-substring begin-of-methods-pos (point)))))


(defun call-interactive-shell-command (buffer start-interactive-shell-command interactive-shell-command)
  "Use to call an interactive shell command, first call will create the buffer and open the command, all calls will run the command you pass in interactive-shell-command"
  (create-console-buffer-if-does-not-exist buffer start-interactive-shell-command)
  (set-buffer buffer)
  (erase-buffer)

  (process-send-string buffer interactive-shell-command))

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

(defun get-rails-documentation-for-function (function)
  (save-excursion
    (get-buffer-create (concat function "-documentation"))
    (setq output-buffer (concat function "-documentation"))
    (create-console-buffer-if-does-not-exist "ri-console-buffer" "ri -i -T\n")
    (set-process-filter (get-buffer-process "ri-console-buffer") 'collect-and-cleanse-output)

    (call-interactive-shell-command "ri-console-buffer" "ri -i -T\n" (concat function "\n"))))


(defun get-rails-documentation ()
  (interactive)
  (get-rails-documentation-for-function (thing-at-point 'symbol)))
  

(defun get-ri-console-clean-output ()
  (save-excursion
    (set-buffer "ri-console-buffer")
    (beginning-of-buffer)
    (next-line 3)
    (beginning-of-line)

    (let ((begin-of-methods-pos (point)))
      (end-of-buffer)
      (previous-line)
      (end-of-line)
      (buffer-substring begin-of-methods-pos (point)))))


(defun get-rails-function-argument-list (function) 
    (call-interactive-shell-command "ri-console-buffer" "ri -i -T\n" (concat function "\n"))
    (create-cleaned-output-buffer (concat function " args") 'get-ri-console-clean-output)
    (search-forward (concat function "(") nil t)t
    (let ((begin-of-methods-pos (point)))
      (search-forward ")" nil t)
      (backward-char)
      (buffer-substring begin-of-methods-pos (point))))

; code to use filter to get shell rather than sit-for
(defun remove-all-in-current-buffer (from)
  (beginning-of-buffer)
  (while (search-forward from nil t)
    (replace-match "" nil t)))

(defun collect-and-cleanse-output (process output)
  (set-buffer output-buffer)
  (if (string-match "Nothing known about" output)
      (message "You need to have buffer created")) ; do something
  (if (string-match ">>" output)
      (progn 
        (insert (ansi-color-apply output))
        (font-lock-mode)
        (remove-all-in-current-buffer "\r")
        (remove-all-in-current-buffer ">>")

        (set-process-buffer process)
        (erase-buffer)
        (switch-to-buffer output-buffer))

    (insert (ansi-color-apply output))))

(defun call-interactive-shell-command (buffer start-interactive-shell-command interactive-shell-command)
  (create-console-buffer-if-does-not-exist buffer start-interactive-shell-command)
  (set-process-filter (get-buffer-process "ri-console-buffer") 'collect-and-cleanse-output)
  (process-send-string buffer interactive-shell-command))



