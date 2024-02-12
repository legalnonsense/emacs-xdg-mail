;; -*- lexical-binding: t; -*-

(require 'dash)
(require 'org-msg)
(require 's)

(defcustom xdg-email-new-message-func #'compose-mail
  "Function to open new message buffer.")

(defcustom xdg-email-attach-function #'org-msg-attach-attach
  "function to attach a file to the current message buffer.  I use `org-msg'.
Another possibility is `mml-attach-file'.")

(defun xdg-email-parser (url)
  "parse a mailto url
note this won't hanlde multple file names.

It will return an alist based on the keys and values
in the mailto url.

This will err out if it does not like the url."
  (setq xxx url)
  (unless (s-starts-with-p "mailto:" url t)
    (setq url (concat "mailto:" url)))
  
  (let* ((to (cons 'to (--> (if (s-ends-with-p "?" url)
				url
			      (concat url "?"))  
			    (cadr (s-split ":" it))
			    (car (s-split "?" it))
			    (url-unhex-string it))))
	 (rest (s-split "&" (or (cadr (s-split "?" url))
				"")))
	 (rest (cl-loop for arg in rest
			collect (cons (intern (url-unhex-string
					       (car (s-split "=" arg))))
				      (url-unhex-string
				       (cadr (s-split "=" arg)))))))
    (xdg-email-create-msg (append (list to) rest))))

(defun xdg-email-create-msg (args)
  "Create a new email message and populate the
relevant fields."
  (interactive)
  (funcall xdg-email-new-message-func)
  (save-excursion 
  (when-let ((to (alist-get 'to args)))
    (message-goto-to)
    (insert to))
  (when-let ((cc (alist-get 'cc args)))
    (message-goto-cc)
    (insert cc))
  (when-let ((bcc (alist-get 'bcc args)))
    (message-goto-bcc)
    (insert bcc))
  (when-let ((subject (alist-get 'subject args)))
    (message-goto-subject)
    (insert subject))
  (when-let ((attach (alist-get 'attach args)))
    (->> attach
	 ;; thunar adds this garbage prefix 
	 (string-remove-prefix "file://")
	 (funcall xdg-email-attach-function)))))
    
(provide 'emacs-xdg-email)













