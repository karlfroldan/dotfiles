(defun relative-org-dir (name)
  "Return a string that's supposed to be a file
   relative to the org-mode notes directory"
  (concat (getenv "HOME") "/Documents/notes/" name))

(defun karl/org-roam-capture-template (key name title)
  '(key name entry
       "* %?"
       :target (file+head (concat "%<%m-%d-%Y>-" name ".org")
                          (concat "#+title: " title))
       :unarrowed t))

;; https://stackoverflow.com/questions/17473478/how-to-enter-a-space-in-the-minibuffer-instead-of-completing-a-word
;; I need to use this for org-mode
(define-key minibuffer-local-completion-map "\M- "
            (lambda () (interactive) (insert " ")))

;; General org-mode settings
(setq
 ;; Use RET on keyboard to go to a pecified link
 org-return-follows-link t
 ;; I want to see everything unfolded
 org-startup-folded nil
 ;; Use indentation for all org files
 org-startup-indented t

 ;; default notes file for org-mode
 org-default-notes-file (relative-org-dir "tasks/default.org")

 org-capture-templates '(("w" "Work-related Task" entry
                          (file (relative-org-dir "tasks/work.org"))
                          "* TODO %?" :empty-lines 1)
                         ("j" "Journal" entry
                          (file+datetree (relative-org-dir "tasks/journal.org"))
                          "* %?\nEntered on %U\n %i\n %a")
                         ("t" "Personal Task" entry
                          (file org-default-notes-file)
                          "* TODO %?" :empty-lines 1)))

;; easily insert latex environment template
(add-hook 'org-mode-hook #'turn-on-org-cdlatex)
(add-hook 'org-mode-hook #'visual-line-mode)

(global-set-key (kbd "C-c o l") #'org-store-link)
(global-set-key (kbd "C-c o a") #'org-agenda)
(global-set-key (kbd "C-c o c") #'org-capture)

(use-package org-roam
  :ensure t
  :init
  (setq
   org-roam-v2-ack t
   ;; Encrypted notes
   org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "${slug}.org" "#+title: ${title}\n"))
     ("c" "Cryptology" plain "%?"
      :target (file+head "crypto/${title}.org"
                         "#+title: ${title}\n")
      :unarrowed t)
     ("w" "Work" entry
      "* Work - %?"
      :target (file+head "%<%m-%d-%Y>-work.org"
                         "#+title: %<%m-%d-%Y> - Work\n")
      :unarrowed t)
     ("j" "Journal" entry
      "* Journal Entry - %?"
      :target (file+head "%<%m-%d-%Y>-journal.org"
                         "#+title: %<%m-%d-%Y> - Journal\n")
      :unarrowed t))
   ;; Path to daily-notes. The path is relative to
   ;; org-roam-directory
   org-roam-dailies-directory "daily/"
   org-roam-dailies-capture-templates
   '(("d" "default" entry
      "* %?"
      :target (file+head "%<%m-%d-%Y>.org"
                         "#+title: %<%m-%d-%Y>\n"))))

  :custom
  (org-roam-directory (relative-org-dir "roam"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n t" . org-roam-tag-add)
         ("C-c n d c" . org-roam-dailies-capture-today)
         ("C-c n d t" . org-roam-dailies-goto-today)
         ("C-c n d g" . org-roam-dailies-goto-date)
         ("C-c n d p" . org-roam-dailies-goto-previous-note)
         ("C-c n d n" . org-roam-dailies-goto-next-note)
         ("C-c n u s" . org-roam-ui-open))
  :config
  (org-roam-setup))

(use-package org-roam-bibtex
  :after org-roam
  :config)
