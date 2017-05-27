
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
    (alchemist elixir-mode exec-path-from-shell intero hindent haskell-mode python-mode rainbow-delimiters evil-easymotion powerline-evil evil-ediff neotree evil-anzu ahungry-theme dracula-theme evil-tabs evil evil-leader))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

; ------------- Evil-leader setup -------------
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

; ------------- Evil setup -------------
(require 'evil)

(evil-mode 1)

; Make Evil Normal State the Initial State Always
(setq evil-emacs-state-modes nil)
(setq evil-insert-state-modes nil)
(setq evil-motion-state-modes nil)

; ------------- Evil-anzu setup -------------
(with-eval-after-load 'evil
  (require 'evil-anzu))

; ------------- Evil-tabs setup -------------
(global-evil-tabs-mode t)

; ------------- Neotree setup -------------
(require 'neotree)

(evil-leader/set-key
  "t" 'neotree-toggle
  "T" 'neotree-toggle)

(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "R") 'neotree-change-root)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)

; ------------- Powerline setup -------------
(require 'powerline)

(powerline-center-evil-theme)

; ------------- Easymotion setup -------------
(evilem-default-keybindings ",")

; ------------- General setup -------------
; Fullscreen on start (GUI)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
; Ensure we have same $PATH in eshell and user shell
(exec-path-from-shell-initialize)

; ------------- Company setup -------------
(require 'company)

(global-company-mode 1)

(setq company-idle-delay 0.1)
(setq company-tooltip-limit 10)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-flip-when-above t)

; ------------- Haskell Config -------------
(add-hook 'haskell-mode-hook #'hindent-mode)
(add-hook 'haskell-mode-hook 'intero-mode)

; ------------- Elixir Config -------------
(require 'elixir-mode)
(require 'alchemist)
