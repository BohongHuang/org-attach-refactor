;;; org-attach-refactor.el --- Auto move attachments managed by org-attach when add or remove the ID of current heading when org-attach-use-inheritance is t -*- lexical-binding: t; coding: utf-8 -*-

(require 'org-attach)
(require 'outline)
(require 'dash)

(defun org-attach-refactor-directory-files (filename)
  (directory-files filename t "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))

(defun org-attach-refactor-remove-id ()
  "Remove ID of current heading and move all attachments in current heading into parent attachment directory."
  (interactive)
  (unless (org-id-get) (error "Cannot find the existent ID of current heading"))
  (unless org-attach-use-inheritance (error "org-attach-use-inheritance must be t"))
  (let ((parent-attach-dir (concat (save-restriction (widen) (save-excursion (condition-case nil (outline-up-heading 1) (error (beginning-of-buffer) (org-id-get-create))) (org-attach-dir-get-create))) "/")))
    (unless parent-attach-dir (error "Cannot remove the ID of current heading, because its parent doesn't have attachment directory"))
    (save-excursion
      (org-back-to-heading t)
      (let ((attach-dir (org-attach-dir)))
        (print (org-attach-refactor-directory-files attach-dir))
        (-each (org-attach-refactor-directory-files attach-dir)
          (lambda (file)
            (rename-file file parent-attach-dir)))
        (delete-directory attach-dir)
        (when (org-entry-delete (point) "ID")
          (org-id-update-id-locations nil 'silent))))))

(defun org-attach-refactor-add-id ()
  "Add ID for current heading and move all previous inherited attachments into the attachment directory of current heading."
  (interactive)
  (when (org-id-get) (error "Current heading already has ID"))
  (unless org-attach-use-inheritance (error "org-attach-use-inheritance must be t"))
  (save-restriction
    (org-narrow-to-subtree)
    (let ((original-attach-dir (org-attach-dir))
          (attach-filenames nil))
      (save-excursion
        (while (re-search-forward "attachment:" nil t)
          (let ((link (org-element-context)))
    	    (when (and (eq 'link (org-element-type link))
    		       (string-equal "attachment"
    				     (org-element-property :type link)))
    	      (let* ((description (and (org-element-property :contents-begin link)
    				       (buffer-substring-no-properties
    				        (org-element-property :contents-begin link)
    				        (org-element-property :contents-end link))))
    		     (file (org-element-property :path link)))
                (add-to-list 'attach-filenames (concat original-attach-dir "/" file)))))))
      (org-id-get-create)
      (let ((attach-dir (concat (concat (org-attach-dir-get-create) "/"))))
           (-each attach-filenames (lambda (filename)
                                     (rename-file filename attach-dir)))))))

(provide 'org-attach-refactor)
