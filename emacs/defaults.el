(setq
 inhibit-startup-screen t
 ;; No need to remind me what a scratch buffer is
 initial-scratch-message nil
 ;; Double spaces after periods is wrong
 sentence-end-double-space nil
 ;; Never ding at me, ever.
 ring-bell-function 'ignore
 ;; Save existing clipboard text into kill ring before replacing
 save-interprogram-paste-before-kill t
 ;; prompts should go in the minibuffer, not in a GUI
 use-dialog-box nil
 ;; Fix undo in commands affecting the mark
 mark-even-if-inactive nil
 ;; Let C-k delete the whole line
 kill-whole-line t
 ;; search should be case-sensitive by default
 case-fold-search nil
 ;; No need to prompt for read commend _every_ time
 compilation-read-command nil
 ;; scroll to first error
 compilation-scroll-output 'first-error
 ;; Indent using spaces instead of tabs
 indent-tabs-mode nil
 ;; Set tab as 4 spaces
 tab-width 4
 ;; C programming style
 c-default-style '((java-mode . "java")
                   (awk-mode . "awk")
                   (otther . "k&r"))
 ;; C programming should have 4 spaces
 c-basic-offset 4
 ;; Unicode ellipses are better
 truncate-string-ellipsis "…"
 ;; Backup directory but I think we remove this anyways
 backup-directory-alist '(("." . "~/.bak.emacs")))

;; Set linum-mode for line numbers. Note that for newer versions of emacs,
;; we no longer use linum-mode.
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Remove the toolbar and scrollbars because I don't use them anyways.
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(setq-default indent-tabs-mode nil)

(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8-unix)

(when (display-graphic-p)
  (require 'hl-line)
  (add-hook 'prog-mode-hook #'hl-line-mode)
  (add-hook 'text-mode-hook #'hl-line-mode))

;;; Add different binary directories for exec-path
(defun add-to-exec-path (path)
  (let ((path-name (concat (getenv "HOME") "/" path)))
    (when (file-directory-p path-name)
      (message (concat "Adding \'" path-name "\' to exec-path"))
      (add-to-list 'exec-path path-name))))

(setq exec-path-list '(".ghcup/bin"
                       ".cabal/bin"
                       ".cargo/bin"))

(mapc #'add-to-exec-path exec-path-list)

;; etags
;; Do not load etags related stuff if it's not needed. For example, I don't really need
;; it on my personal PC.

(if (file-exists-p (relative-emacs-dir ".workpc"))
    (progn
      (defun create-tags (dir-name)
        "Create tags file"
        (interactive "DDirectory: ")
        (shell-command
         (format "find %s -type f -name \"*.[ch]\" | etags -" dir-name)))
;;;  etags autorefresh
      ;; From https://www.emacswiki.org/emacs/BuildTags
      (defadvice find-tag (around refresh-etags activate)
        "Rerun etags and reload tags if tag not found and redo find-tag.              
   If buffer is modified, ask about save before running etags."
        (let ((extension (file-name-extension (buffer-file-name))))
          (condition-case err
              ad-do-it
            (error (and (buffer-modified-p)
                        (not (ding))
                        (y-or-n-p "Buffer is modified, save it? ")
                        (save-buffer))
                   (er-refresh-etags extension)
                   ad-do-it))))

      (defun er-refresh-etags (&optional extension)
        "Run etags on all peer files in current dir and reload them silently."
        (interactive)
        (shell-command (format "etags *.%s" (or extension "el")))
        (let ((tags-revert-without-query t))  ; don't query, revert silently          
          (visit-tags-table default-directory nil)))))

;;; TAB BAR MODE stuff
(global-set-key (kbd "s-[") 'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "s-]") 'tab-bar-switch-to-next-tab)
(global-set-key (kbd "s-t") 'tab-bar-new-tab)
(global-set-key (kbd "s-w") 'tab-bar-close-tab)

;; Enable tab-bar-mode (Currently WIP so disable for now)
(tab-bar-mode -1)
(setq
 ;; hide tab close/X button
 tab-bar-close-button-show nil
 ;; Buffer to show in new tabs
 tab-bar-new-tab-choice "*scratch*"
 ;; Show tab numbers
 tab-bar-tab-hints t
 ;; Elements to include in bar
 tab-bar-format '(tab-bar-format-tabs tab-bar-separator)
 ;; Hide bar if less than 2 tabs are open
 tab-bar-show 1)
