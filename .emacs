;;
;; File   : .emacs
;; Author : Romain Deterre <romain@alazartech.com>
;;

;; Utility functions

(defun system-is-mac ()
  (interactive)
  (string-equal system-type "darwin"))

(defun system-is-linux ()
  (interactive)
  (string-equal system-type "gnu/linux"))

;; Path issues on OS X

(if (system-is-mac)
    (progn
      (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
      (setenv "PYTHONPATH" "/usr/local/lib/python2.7/site-packages:")))

;; Based on http://melpa.milkbox.net/#/getting-started .

(require 'package)
(add-to-list 'package-archives
  ;; The 't' means to append, so that MELPA comes after the more
  ;; stable ELPA archive.
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Add ELPA if necessary. Looking at the El-Get package.rcp recipe in
;; ~/local/opt/el-get/recipes it seems this is probably unnecessary.
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

(require 'el-get-elpa)
;; Build the El-Get copy of the package.el packages if we have not
;; built it before.  Will have to look into updating later ...
(unless (file-directory-p el-get-recipe-path-elpa)
  (el-get-elpa-build-local-recipes))

(setq my-packages
      (append
       '(ace-jump-mode
	 auto-complete
	 autopair
	 c-eldoc
;;	 cmake-project
	 column-marker
	 disaster
;;	 doctags
;;	 ecb
         ediff-trees
	 ein
	 fill-column-indicator
	 flymake
	 helm
         helm-dash
	 hideshow-org
	 ido-ubiquitous
	 jedi
	 minimap
	 multiple-cursors
	 nav
	 org
	 pallet
	 powerline
	 projectile
;;	 revive
         rtags
	 solarized-theme
	 smartparens
	 yasnippet
	 zenburn-theme
	 git-auto-commit-mode
	 sr-speedbar
         rebox2
;;	 color-theme-heroku
	 heroku-theme
	 magit)

       (mapcar 'el-get-as-symbol (mapcar 'el-get-source-name el-get-sources))))

(el-get 'sync my-packages)

(global-auto-revert-mode t)

(load-theme 'heroku t)
;; (load-theme 'zenburn t)
;; (load-theme 'solarized-light t)

(server-start)

;; Font
;; (if (equal system-type 'darwin)
;;     (set-frame-font "Source Code Pro-12" nil t)
;;   (set-frame-font "Source Code Pro-9" nil t))
(if (system-is-mac)
    (set-face-attribute 'default nil :height 120)
  (if (member "Source Code Pro" (font-family-list))
      (set-frame-font "Source Code Pro-9" nil t)
    (set-face-attribute 'default nil :height 100)))


;; Scrolling
(setq mouse-wheel-progressive-speed nil)

;; Gives the Emacs window focus when emacsclient is called
;; Source : http://askubuntu.com/a/288483
(defun px-raise-frame-and-give-focus ()
  (when window-system
    (raise-frame)
    (x-focus-frame (selected-frame))
    (set-mouse-pixel-position (selected-frame) 4 4)
    ))
(add-hook 'server-switch-hook 'px-raise-frame-and-give-focus)

;; Ace jump
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; Smartparens
(require 'smartparens)
(smartparens-global-mode t)

;; Yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"
        "~/.emacs.d/yasnippet-snippets"
        ))
(yas-global-mode 1)
(add-hook 'term-mode-hook (lambda()
        (setq yas-dont-activate t)))

;; c-eldoc
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

;; disaster
(require 'disaster)
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (define-key c-mode-base-map (kbd "C-c a") 'disaster)))

;; git-auto-commit-mode
(require 'git-auto-commit-mode)
(setq gac-automatically-push-p nil)

;; org-mode
(add-hook 'org-mode-hook
          '(lambda ()
             (auto-fill-mode nil)
             (visual-line-mode t)))

(setq org-todo-keywords
      '((sequence "TODO" "FEEDBACK" "VERIFY" "|" "DONE" "DELEGATED" "CHANGED")))
(setq org-log-done 'time)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)
   (calc . t)
   (python . t))) ; this line activates dot

(setq org-default-notes-file (concat "~/aztdc1/notes/notes.org"))
(define-key global-map "\C-cc" 'org-capture)

(setq org-capture-templates
 '(("c" "Phone call" entry
    (file+headline "~/aztdc1/notes/notes.org" "Phone Calls")
    "* TODO %?\n %U\n")
   ("t" "Todo" entry (file+headline "~/aztdc1/notes/notes.org" "Tasks")
    "* %?\nEntered on %U\n  %i\n  %a")))

(setq org-image-actual-width 300)

;; Set Fill Column
(setq fill-column 80)

;;Disable things
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-splash-screen t)
(scroll-bar-mode -1)

;; Prevent fractionned display
;; http://www.masteringemacs.org/articles/2011/10/02/improving-performance-emacs-display-engine/
(setq redisplay-dont-pause t)

;;EIN - M-x ein:notebooklist-openq

;;Eproject

;; Frame size
(setq default-frame-alist
      '(
        (width . 242)  ; characters
        (height . 60)  ; lines
        ))

;;Ido-mode
(ido-mode t)
(ido-ubiquitous-mode t)

;; Jedi
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

;; ;; Powerline
;; (powerline-default-theme)

;; ;; (setq powerline-color1 "#586e75")
;; (setq powerline-color1 "#FF0000")
;; (setq powerline-color2 "#00FF00")

;; (set-face-attribute 'mode-line nil
;;                     :foreground "#0000FF"
;;                     :background "#FFFF00"
;;                     :box nil)
;; (set-face-attribute 'mode-line-inactive nil
;;                     :box nil)


;; Projectile
;(projectile-global-mode)

;; Outshine
;(add-hook 'outline-minor-mode-hook 'outshine-hook-function)
;(add-hook 'python-mode-hook 'outline-minor-mode)

;; Fullscreen function.
(defun djcb-full-screen-toggle ()
  "toggle full-screen mode"
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))
(global-set-key (kbd "<f11>") 'djcb-full-screen-toggle)

;; Indentation and alignment
(setq c-default-style "k&r")
(setq-default indent-tabs-mode nil)
(setq-default c-basic-indent 2)
(setq-default c-basic-offset 2)
(defun my-c-setup ()
  (c-set-offset 'innamespace 0))
(add-hook 'c++-mode-hook 'my-c-setup)

;; Flymake with pyflakes
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))

;(add-hook 'find-file-hook 'flymake-find-file-hook)

;; TODO : Add CMake support again.
;; ;Emacs CMake project mode
;; (defun maybe-cmake-project-hook ()
;;  (if (file-exists-p "CMakeLists.txt") (cmake-project-mode)))
;; (add-hook 'c-mode-hook 'maybe-cmake-project-hook)
;; (add-hook 'c++-mode-hook 'maybe-cmake-project-hook)

;; Comment code
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
   If no region is selected and current line is not blank and we
   are not at the end of the line, then comment current line.
   Replaces default behaviour of comment-dwim, when it inserts
   comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
(global-set-key "\M-;" 'comment-dwim-line)

;; Org Latex
(setq org-latex-pdf-process
      '("xelatex -interaction nonstopmode %f"
        "xelatex -interaction nonstopmode %f")) ;; for multiple passes

(require 'org-latex)
(setq org-export-latex-listings t)

(require 'org)
(add-to-list 'org-export-backends 'md)

;; Originally taken from Bruno Tavernier: http://thread.gmane.org/gmane.emacs.orgmode/31150/focus=31432
;; but adapted to use latexmk 4.20 or higher.
(defun my-auto-tex-cmd ()
  "When exporting from .org with latex, automatically run latex,
     pdflatex, or xelatex as appropriate, using latexmk."
  (let ((texcmd)))
  ;; default command: oldstyle latex via dvi
  (setq texcmd "latexmk -dvi -pdfps -quiet %f")
  ;; pdflatex -> .pdf
  (if (string-match "LATEX_CMD: pdflatex" (buffer-string))
      (setq texcmd "latexmk -pdf -quiet %f"))
  ;; xelatex -> .pdf
  (if (string-match "LATEX_CMD: xelatex" (buffer-string))
      (setq texcmd "latexmk -pdflatex=xelatex -pdf -quiet %f"))
  ;; LaTeX compilation command
  (setq org-latex-to-pdf-process (list texcmd)))

(add-hook 'org-export-latex-after-initial-vars-hook 'my-auto-tex-cmd)

;; Specify default packages to be included in every tex file, whether pdflatex or xelatex
(setq org-export-latex-packages-alist
      '(("" "graphicx" t)
            ("" "longtable" nil)
            ("" "float" nil)))

(defun my-auto-tex-parameters ()
      "Automatically select the tex packages to include."
      ;; default packages for ordinary latex or pdflatex export
      (setq org-export-latex-default-packages-alist
            '(("AUTO" "inputenc" t)
              ("T1"   "fontenc"   t)
              (""     "fixltx2e"  nil)
              (""     "wrapfig"   nil)
              (""     "soul"      t)
              (""     "textcomp"  t)
              (""     "marvosym"  t)
              (""     "wasysym"   t)
              (""     "latexsym"  t)
              (""     "amssymb"   t)
              (""     "hyperref"  nil)))

      ;; Packages to include when xelatex is used
      (if (string-match "LATEX_CMD: xelatex" (buffer-string))
          (setq org-export-latex-default-packages-alist
                '(("" "fontspec" t)
                  ("" "xunicode" t)
                  ("" "url" t)
                  ("" "rotating" t)
                  ("american" "babel" t)
                  ("babel" "csquotes" t)
                  ("" "soul" t)
                  ("xetex" "hyperref" nil)
                  )))

      (if (string-match "LATEX_CMD: xelatex" (buffer-string))
          (setq org-export-latex-classes
                (cons '("article"
                        "\\documentclass[11pt,article,oneside]{memoir}"
                        ("\\section{%s}" . "\\section*{%s}")
                        ("\\subsection{%s}" . "\\subsection*{%s}")
                        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                        ("\\paragraph{%s}" . "\\paragraph*{%s}")
                        ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                      org-export-latex-classes))))

(add-hook 'org-export-latex-after-initial-vars-hook 'my-auto-tex-parameters)

;; Nav
(require 'nav)
(nav-disable-overeager-window-splitting)

;; Header-Implementation switch
(global-set-key  (kbd "C-c o") 'ff-find-other-file)

;; Header Guards
; Create Header Guards with f12
(global-set-key [f12]
                '(lambda ()
                   (interactive)
                   (if (buffer-file-name)
                       (let*
                           ((fName (upcase (file-name-nondirectory (file-name-sans-extension buffer-file-name))))
                            (ifDef (concat "#ifndef " fName "_H" "\n#define " fName "_H" "\n"))
                            (begin (point-marker))
                            )
                         (progn
                                        ; If less then 5 characters are in the buffer, insert the class definition
                           (if (< (- (point-max) (point-min)) 5 )
                               (progn
                                 (insert "\nclass " (capitalize fName) "{\npublic:\n\nprivate:\n\n};\n")
                                 (goto-char (point-min))
                                 (next-line-nomark 3)
                                 (setq begin (point-marker))
                                 )
                             )

                                        ;Insert the Header Guard
                           (goto-char (point-min))
                           (insert ifDef)
                           (goto-char (point-max))
                           (insert "\n#endif" " //" fName "_H")
                           (goto-char begin))
                         )
                                        ;else
                     (message (concat "Buffer " (buffer-name) " must have a filename"))
                     )
                   )
                )

;; Rebox2
(setq rebox-style-loop '(13 33 111))
(setq rebox-min-fill-column '80)
(require 'rebox2)
(global-set-key (kbd "C-;") 'rebox-cycle)

;; RTags
(add-to-list 'load-path "~/.emacs.d/el-get/rtags/src")
(add-to-list 'load-path "~/.emacs.d/el-get/rtags/bin")
(setq rtags-path "~/.emacs.d/el-get/rtags/")
(require 'rtags)
(rtags-enable-standard-keybindings c-mode-base-map)
(rtags-start-process-maybe)

;; (require 'hideshow-org)
;; (global-set-key "\C-ch" 'hs-org/minor-mode)

(defun my-c++-hooks ()
  "Enable the hooks in the preferred order: 'yas -> auto-complete -> irony'."
  ;; if yas is not set before (auto-complete-mode 1), overlays may persist after
  ;; an expansion.
  ;; (yas/minor-mode-on)
  (auto-complete-mode t))
  ;; (hs-org/minor-mode t))
;; )
;  (irony-mode t))

(add-hook 'c-mode-common-hook 'my-c++-hooks)

;(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
; '(irony-server-executable "~/.emacs.d/irony-mode/build/server/irony-server" nil (irony)))

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(minimap-semantic-function-face ((t (:inherit (font-lock-function-name-face minimap-font-face) :background "gray10" :box (:line-width 1 :color "white") :height 30))))
;;  '(minimap-semantic-type-face ((t (:inherit (font-lock-type-face minimap-font-face) :background "gray10" :box (:line-width 1 :color "white") :height 30))))
;;  '(minimap-semantic-variable-face ((t (:inherit (font-lock-variable-name-face minimap-font-face) :background "gray10" :box (:line-width 1 :color "white") :height 30)))))


;; TODO : Invesigate.
;;(require 'doctags)

(set 'shift-selection-mode nil)

;; Windmove
;; (windmove-default-keybindings 'shift)
(global-set-key (kbd "C-M-h")  'windmove-left)
(global-set-key (kbd "C-M-l") 'windmove-right)
(global-set-key (kbd "C-M-k")    'windmove-up)
(global-set-key (kbd "C-M-j")  'windmove-down)
(defun my-windmove-hook ()
  (local-set-key (kbd "C-M-h")  'windmove-left)
  (local-set-key (kbd "C-M-l") 'windmove-right)
  (local-set-key (kbd "C-M-k")    'windmove-up)
  (local-set-key (kbd "C-M-j")  'windmove-down))
(add-hook 'c-mode-hook 'my-windmove-hook)
(add-hook 'c++-mode-hook 'my-windmove-hook)

;; Make windmove work in term mode (http://stackoverflow.com/a/12509277/1857952)
(eval-after-load "term"
  '(progn
     (define-key term-raw-map (kbd "C-M-h") 'windmove-left)
     (define-key term-raw-map (kbd "C-M-l") 'windmove-right)
     (define-key term-raw-map (kbd "C-M-k") 'windmove-up)
     (define-key term-raw-map (kbd "C-M-j") 'windmove-down)))

;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;; Enum classes indentation support
(defun inside-class-enum-p (pos)
  "Checks if POS is within the braces of a C++ \"enum class\"."
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (up-list -1)
      (backward-sexp 1)
      (looking-back "enum[ \t]+class[ \t]+[^}]+"))))

(defun align-enum-class (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
    (c-lineup-topmost-intro-cont langelem)))

(defun align-enum-class-closing-brace (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      '-
    '+))

(defun fix-enum-class ()
  "Setup `c++-mode' to better handle \"class enum\"."
  (add-to-list 'c-offsets-alist '(topmost-intro-cont . align-enum-class))
  (add-to-list 'c-offsets-alist
               '(statement-cont . align-enum-class-closing-brace)))

(add-hook 'c++-mode-hook 'fix-enum-class)

(global-set-key [f5] 'compile)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "146d24de1bb61ddfa64062c29b5ff57065552a7c4019bee5d869e938782dfc2a" "96efbabfb6516f7375cdf85e7781fe7b7249b6e8114676d65337a1ffe78b78d9" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(minimap-recenter-type (quote free))
 '(python-shell-interpreter (if (system-is-mac) (quote "/usr/local/bin/python") (quote "python")))
 '(safe-local-variable-values (quote ((c-basic-indent . 2) (c-basic-indent . 4) (c-basic-indent 4) (c-basic-offset 4)))))

;; Trucate lines
;;(set-default 'truncate-lines nil)
;;(setq truncate-partial-width-windows nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; (require 'whitespace)
;; (setq whitespace-line-column 80) ;; limit line length
;; (setq whitespace-style '(face lines-tail))

;; (add-hook 'prog-mode-hook 'whitespace-mode)
(require 'column-marker)
(add-hook 'prog-mode-hook (lambda () (interactive) (column-marker-1 80)))

;; Scrollers - M-n and M-p
(global-set-key "\M-n" "\C-u3\C-v")
(global-set-key "\M-p" "\C-u3\M-v")

;; Mark word
(defun my-mark-word (N)
  (interactive "p")
  (if (and
       (not (eq last-command this-command))
       (not (eq last-command 'my-mark-word-backward)))
      (set-mark (point)))
  (forward-word N))


(defun my-mark-word-backward (N)
  (interactive "p")
  (if (and
       (not (eq last-command this-command))
       (not (eq last-command 'my-mark-word)))
      (set-mark (point)))
  (backward-word N))

(local-set-key (kbd "C-c k") 'my-mark-word)

(local-set-key (kbd "C-c j") 'my-mark-word-backward)




;; Multiple cursors
(require 'multiple-cursors)
;; (add-hook 'prog-mode-hook 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;; Clang-format
;;; Clang-format emacs integration for use with C/Objective-C/C++.

;; This defines a function clang-format-region that you can bind to a key.
;; A minimal .emacs would contain:
;;
;;   (load "<path-to-clang>/tools/clang-format/clang-format.el")
;;   (global-set-key [C-M-tab] 'clang-format-region)
;;
;; Depending on your configuration and coding style, you might need to modify
;; 'style' in clang-format, below.

(require 'json)

;; *Location of the clang-format binary.
(if (file-exists-p "~/Documents/llvm-build/Debug+Asserts/bin/clang-format")
    (defvar clang-format-binary
      "~/Documents/llvm-build/Debug+Asserts/bin/clang-format")
  (defvar clang-format-binary "clang-format"))

(defun clang-format-region ()
  "Use clang-format to format the currently active region."
  (interactive)
  (let ((beg (if mark-active
                 (region-beginning)
               (min (line-beginning-position) (1- (point-max)))))
        (end (if mark-active
                 (region-end)
               (line-end-position))))
    (clang-format beg end)))

(defun clang-format-buffer ()
  "Use clang-format to format the current buffer."
  (interactive)
  (clang-format (point-min) (point-max)))

;; (defun clang-format (begin end)
;;   "Use clang-format to format the code between BEGIN and END."
;;   (let* ((orig-windows (get-buffer-window-list (current-buffer)))
;;          (orig-window-starts (mapcar #'window-start orig-windows))
;;          (orig-point (point))
;;          (style "Google"))
;;     (unwind-protect
;;         (call-process-region (point-min) (point-max) clang-format-binary
;;                              t (list t t) nil
;;                              "-offset" (number-to-string (1- begin))
;;                              "-length" (number-to-string (- end begin))
;;                              ;; "-cursor" (number-to-string (1- (point)))
;;                              ;; "-assume-filename" (buffer-file-name)
;;                              "-style" style)
;;       (goto-char (point-min))
;;       (let ((json-output (json-read-from-string
;;                            (buffer-substring-no-properties
;;                              (point-min) (line-beginning-position 2)))))
;;         (delete-region (point-min) (line-beginning-position 2))
;;         (goto-char (1+ (cdr (assoc 'Cursor json-output))))
;;         (dotimes (index (length orig-windows))
;;           (set-window-start (nth index orig-windows)
;;                             (nth index orig-window-starts)))))))

(defun clang-format (begin end)
  "Use clang-format to format the code between BEGIN and END."
  (let* ((orig-windows (get-buffer-window-list (current-buffer)))
         (orig-window-starts (mapcar #'window-start orig-windows))
         (orig-point (point))
         (style "file"))
    (unwind-protect
        (call-process-region (point-min) (point-max) clang-format-binary t t nil
                             "-offset" (number-to-string (1- begin))
                             "-length" (number-to-string (- end begin))
                             "-style" style)
      (goto-char orig-point)
      (dotimes (index (length orig-windows))
        (set-window-start (nth index orig-windows)
                          (nth index orig-window-starts))))))

(global-set-key (kbd "C-c f") 'clang-format-region)
(add-hook 'c-mode-hook
          (lambda () (local-set-key (kbd "<C-tab>") #'clang-format-region)))
(add-hook 'c++-mode-hook
          (lambda () (local-set-key (kbd "<C-tab>") #'clang-format-region)))

;; Helm
(require 'helm)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c i") 'helm-imenu)
(global-set-key (kbd "C-x b") 'helm-buffers-list)

;; Helm-dash
(require 'helm-dash)
(setq helm-dash-common-docsets '("C"
                                 "C++"
                                 "Common Lisp"
                                 "Emacs Lisp"
                                 "NumPy"
                                 "Python 2"))

(defun rdeterre/dash-install (docset)
  ; Taken from http://jwintz.me/blog/2014/02/16/helm-dash-makes-you-efficient/
  (message (file-exists-p (concat docset ".docset")))
  (unless (file-exists-p
           (concat
            (concat helm-dash-docsets-path "/")
           ;;  (concat (nth 0 (split-string docset "_"))
           ;;          ".docset")))
            (concat docset ".docset")))
    (helm-dash-install-docset
     (replace-regexp-in-string " " "_" docset))))

(defun rdeterre/dash-install-all-common-docsets ()
  (mapcar 'rdeterre/dash-install helm-dash-common-docsets))

(rdeterre/dash-install-all-common-docsets)
(global-set-key (kbd "C-c d") 'helm-dash)
(add-hook 'python-mode-hook
          (lambda () (local-set-key (kbd "C-c c") 'helm-dash)))

;; Follow compilation buffer until first error happens
(setq compilation-scroll-output 'first-error)

;; ;; Revive
;; (require 'revive)
;; (autoload 'save-current-configuration "revive" "Save status" t)
;; (autoload 'resume "revive" "Resume Emacs" t)
;; (autoload 'wipe "revive" "Wipe Emacs" t)
;; (define-key ctl-x-map "S" 'save-current-configuration)
;; (define-key ctl-x-map "F" 'resume)
;; (define-key ctl-x-map "K" 'wipe)

(autoload 'expand-member-functions
  "member-functions" "Expand C++ member function declarations" t)
(add-hook 'c++-mode-hook (lambda ()
                           (local-set-key "\C-cm" #'expand-member-functions)))

;; Limit shell width
(defun comint-fix-window-size ()
  "Change process window size."
  (when (derived-mode-p 'comint-mode)
    (let ((process (get-buffer-process (current-buffer))))
      (unless (eq nil process)
        (set-process-window-size process (window-height) (window-width))))))

(defun my-shell-mode-hook ()
  ;; add this hook as buffer local, so it runs once per window.
  (add-hook 'window-configuration-change-hook 'comint-fix-window-size nil t))

(add-hook 'shell-mode-hook 'my-shell-mode-hook)

;; term
(defface term-color-black
  '((t (:foreground "#3f3f3f" :background "#272822")))
  "Unhelpful docstring.")
(defface term-color-red
  '((t (:foreground "#cc9393" :background "#272822")))
  "Unhelpful docstring.")
(defface term-color-green
  '((t (:foreground "#7f9f7f" :background "#272822")))
  "Unhelpful docstring.")
(defface term-color-yellow
  '((t (:foreground "#f0dfaf" :background "#272822")))
  "Unhelpful docstring.")
(defface term-color-blue
  '((t (:foreground "#6d85ba" :background "#272822")))
  "Unhelpful docstring.")
(defface term-color-magenta
  '((t (:foreground "#dc8cc3" :background "#272822")))
  "Unhelpful docstring.")
(defface term-color-cyan
  '((t (:foreground "#93e0e3" :background "#272822")))
  "Unhelpful docstring.")
(defface term-color-white
  '((t (:foreground "#dcdccc" :background "#272822")))
  "Unhelpful docstring.")
'(term-default-fg-color ((t (:inherit term-color-white))))
'(term-default-bg-color ((t (:inherit term-color-black))))

;; ansi-term colors
(setq ansi-term-color-vector
  [term term-color-black term-color-red term-color-green term-color-yellow
    term-color-blue term-color-magenta term-color-cyan term-color-white])

(require 'sr-speedbar)

(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; Split direction
(setq split-height-threshold 48) ;; Do not create pane less than 24 chars high
(setq split-width-threshold 160) ;; Do not create pane less than 80 chars wide

(defun my-split-window-sensibly (&optional window)
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
             ;; Split window horizontally.
             (with-selected-window window
               (split-window-right)))
        (and (window-splittable-p window)
             ;; Split window vertically.
             (with-selected-window window
               (split-window-below)))
        (and (eq window (frame-root-window (window-frame window)))
             (not (window-minibuffer-p window))
             ;; If WINDOW is the only window on its frame and is not the
             ;; minibuffer window, try to split it horizontally disregarding
             ;; the value of `split-width-threshold'.
             (let ((split-width-threshold 0))
               (when (window-splittable-p window t)
                 (with-selected-window window
                   (split-window-right))))))))

(setq split-window-preferred-function 'my-split-window-sensibly)

(require 'tramp)
(add-to-list 'tramp-default-proxies-alist
             '(nil "\\`root\\'" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist
             '((regexp-quote (system-name)) nil nil))

;; Winner-mode
(when (fboundp 'winner-mode)
      (winner-mode 1))

;; Arduino files
(add-to-list 'auto-mode-alist '("\\.ino\\'" . c-mode))

;; Keyboard for OS X
(if (system-is-mac)
    (setq mac-command-modifier 'meta))

;; Uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
