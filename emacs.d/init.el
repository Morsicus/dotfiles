
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (dracula)))
 '(custom-safe-themes
   (quote
    ("ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" default)))
 '(package-selected-packages
   (quote
    (evil-easymotion powerline-evil evil-ediff neotree evil-anzu ahungry-theme dracula-theme evil-tabs evil evil-leader))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "bd" 'kill-buffer
  "bb" 'switch-to-buffer
  "bp" 'switch-to-prev-buffer
  "bn" 'switch-to-next-buffer)

; https://github.com/cofi/evil-leader/issues/6
; I don'y know why i need to do this.
; Worked well for buffer shorcut :s  
(evil-leader/set-key "w" (let ((map (make-sparse-keymap)))
                           (define-key map (kbd "/") 'split-window-horizontally)
                           (define-key map (kbd "-") 'split-window-vertically)
                           (define-key map (kbd "d") 'kill-buffer-and-window)
                           (define-key map (kbd "<up>") 'windmove-up)
                           (define-key map (kbd "<down>") 'windmove-down)
                           (define-key map (kbd "<left>") 'windmove-left)
                           (define-key map (kbd "<right>") 'windmove-right)
                           map))

(require 'evil)
(evil-mode 1)
; Make Evil Normal State the Initial State Always
(setq evil-emacs-state-modes nil)
(setq evil-insert-state-modes nil)
(setq evil-motion-state-modes nil)

(with-eval-after-load 'evil
  (require 'evil-anzu))

(global-evil-tabs-mode t)

(require 'neotree)
(evil-leader/set-key
  "t" 'neotree-toggle
  "T" 'neotree-toggle)

(require 'powerline)
(powerline-center-evil-theme)

; Evil-easymotion
(evilem-default-keybindings ",")

; Fullscreen on start (GUI)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

