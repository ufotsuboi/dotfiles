(setq load-path (append
		 '("~/.emacs.d/site-lisp")
		 load-path))

(set-locale-environment nil)

;;; パッケージ管理
(require 'package)
;; MELPAを追加
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
;; Marmaladeを追加
(add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/"))
;; 初期化
(package-initialize)

;;; カラーテーマ設定
(require 'color-theme)
(color-theme-initialize)
(color-theme-clarity)

;;; 背景透過
;(if window-system (progn
;		    (set-frame-parameter nil 'alpha 90)
;))

;;; 日本語環境用
;(setq auto-coding-functions nil)
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)

;;; キーバインド
;(define-key global-map (kbd "C-z") 'undo)               ; undo
(define-key global-map (kbd "C-x C-;") 'comment-dwim)     ;コメントアウト

;;; C-x:で指定行数にジャンプ
(global-set-key "\C-x:" 'goto-line)

;;; 行数を表示
;(global-linum-mode t)

;;; 起動時の画面を表示しない
(setq inhibit-splash-screen t)

;;; C-kで改行文字も削除
;(setq kill-whole-line t)

;;; 保存時に自動で最終行に改行を追加
(setq require-final-newline t)

;;; 暫定マーク時に色を表示しない
(setq transient-mark-mode nil)

;;; メニューバーを消す
(menu-bar-mode -1)

;;; ツールバーを消す
;(tool-bar-mode -1)

;;;カーソルの点滅を止める
(blink-cursor-mode 0)

;;;対応する括弧を光らせる
(show-paren-mode 1)

;;;ウィンドウ内に収まらないときだけ括弧内も光らせる
(setq show-paren-style 'mixed)

;;; 現在行を目立たせる
;(global-hl-line-mode)

;;; スクロールを一行ずつにする
(setq scroll-step 1)

;;; スクロールバーを右側に
;(set-scroll-bar-mode 'right)

;;; カーソル位置が何文字目かを表示する
(column-number-mode t)

;;; カーソル位置が何行目かを表示する
(line-number-mode t)

;; 時間をモードラインに表示
(display-time-mode t)

;; 時間を24時間表記で表示
(setq display-time-24hr-format t)

;;; カーソルの場所を保存する
(when (require 'saveplace nil t)
  (setq-default save-place t)
  (setq save-place-file "~/.emacs.d/saved-places"))

;;; バックアップファイルを作らない
(setq backup-inhibited t)

;;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)


;; 保存時に自動で行末にある無駄な空白を削除
(defvar delete-trailing-whitespece-before-save t)
(defun my-delete-trailing-whitespace ()
  (if delete-trailing-whitespece-before-save
      (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'my-delete-trailing-whitespace)

;; C-aで行の本当に先頭ではなく，行の非空白文字の先頭へ
(defun back-to-indentation-or-beginning ()
  (interactive)
  (if (= (point) (save-excursion (back-to-indentation) (point)))
      (beginning-of-line) (back-to-indentation)))
(global-set-key "\C-a" 'back-to-indentation-or-beginning)

;; C-xkで現在バッファを削除
;(global-set-key "\C-xk" 'kill-this-buffer)



;;; コーディング
;;; デフォルトのインデント設定
(setq-default c-basic-offset 4)

;;; インデントTAB文字を使わない（空白文字を使う）
(setq-default indent-tabs-mode nil)


;;; PHPモードの設定
;(require 'php-mode)
;;;php-modeの時は文字コードをUTF-8にする
;(custom-set-variables '(php-mode-force-pear t))
;(add-hook 'php-mode-hook (defun php-mode-hook () (c-set-style "bsd")))
;; (add-hook 'php-mode-hook
;;           (lambda ()
;;             (c-set-offset 'case-label' c-basic-offset)
;;             (c-set-offset 'arglist-intro' c-basic-offset)
;;             ;(c-set-offset 'arglist-cont-nonempty' 4)
;;             (c-set-offset 'arglist-close' 0)
;;             (set-default-coding-systems 'utf-8)
;;             (set-buffer-file-coding-system  'utf-8)
;;             (set-terminal-coding-system 'utf-8)
;;             ))

;;; web-mode
(require 'web-mode)
;; 拡張子の設定
(add-to-list 'auto-mode-alist '("\\.phtml$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x$"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.tt2$"       . web-mode))
(add-to-list 'auto-mode-alist '("\\.tx$"        . web-mode))
(add-to-list 'auto-mode-alist '("\\.js[x]?$"    . web-mode))

;; (defadvice web-mode-highlight-part (around tweak-jsx activate)
;;   (if (equal web-mode-content-type "jsx")
;;       (let ((web-mode-enable-part-face nil))
;;         ad-do-it)
;;         ad-do-it))

(add-to-list 'web-mode-comment-formats '("jsx" . "//" ))
(add-to-list 'web-mode-comment-formats '("javascript" . "//" ))

(defun web-mode-hook ()
  "Hooks for Web mode."
  ;; html indent
  (setq web-mode-markup-indent-offset 2)
  ;; css indent
  (setq web-mode-css-indent-offset 2)
  ;; script indent(js,php,etc..)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-content-types-alist '(("jsx"  . "\\.js[x]$")))

  (setq web-mode-comment-style 2)
  ;; 終了タグの自動補完をしない
  ;;(setq web-mode-disable-auto-pairing t)
  ;; ;; color:#ff0000;等とした場合に指定した色をbgに表示しない
  ;; (setq web-mode-disable-css-colorization t)
  ;; ;;css,js,php,etc..の範囲をbg色で表示
  ;; (setq web-mode-enable-block-faces t)
  ;; (custom-set-faces
  ;;  '(web-mode-server-face
  ;;    ((t (:background "grey"))))                  ; template Blockの背景色
  ;;  '(web-mode-css-face
  ;;    ((t (:background "grey18"))))                ; CSS Blockの背景色
  ;;  '(web-mode-javascript-face
  ;;    ((t (:background "red"))))                ; javascript Blockの背景色
  ;;  )
  (setq web-mode-enable-heredoc-fontification t)
  )
(add-hook 'web-mode-hook  'web-mode-hook)

;;色の設定
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(web-mode-comment-face ((t (:foreground "orangered"))))
 '(web-mode-css-at-rule-face ((t (:foreground "#FF7F00"))))
 '(web-mode-css-pseudo-class-face ((t (:foreground "#FF7F00"))))
 '(web-mode-css-rule-face ((t (:foreground "#A0D8EF"))))
 '(web-mode-doctype-face ((t (:foreground "#82AE46"))))
 '(web-mode-html-attr-name-face ((t (:foreground "lightgoldenrod"))))
 '(web-mode-html-attr-value-face ((t (:foreground "lightsalmon"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "gray60"))))
 '(web-mode-html-tag-face ((t (:foreground "lightskyblue" :weight bold))))
 '(web-mode-server-comment-face ((t (:foreground "#D9333F")))))

;;; js-mode
(setq js-indent-level 2)
(load-file "~/.emacs.d/sgml-mode-patch.el")
(require 'sgml-mode)
(require 'js2-mode)
;(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . js2-jsx-mode))
(setq-default js2-auto-indent-p nil)
(setq-default js2-strict-missing-semi-warning t)
(setq-default js2-strict-trailing-comma-warning nil)
(setq-default js2-indent-level 2)

(add-hook 'js2-mode-hook
          (lambda ()
            (set (make-local-variable 'js2-indent-switch-body) t)
            (setq js2-basic-offset 2
                  tab-width        4
                  indent-tabs-mode nil
                  js2-cleanup-whitespace nil)))
;; (custom-set-faces
;;  '(js2-object-property
;;    ((t (:foreground "lightgoldenrod"))))
;; )

;; (when (load "js2" t)
;;   (setq js2-cleanup-whitespace nil
;; 	js2-mirror-mode nil
;; 	js2-bounce-indent-flag nil)

;;   (defun indent-and-back-to-indentation ()
;;     (interactive)
;;     (indent-for-tab-command)
;;     (let ((point-of-indentation
;; 	   (save-excursion
;; 	     (back-to-indentation)
;; 	     (point))))
;;       (skip-chars-forward "\s " point-of-indentation)))
;;   (define-key js2-mode-map "\C-i" 'indent-and-back-to-indentation)

;;   (define-key js2-mode-map "\C-m" nil)

;;   ;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;;
;;   )

;;; ruby-mode
(add-to-list 'auto-mode-alist '("\\.ruby$" . ruby-mode))

;;; coffee-mode
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(defun coffee-custom ()
  "coffee-mode-hook"
  (and (set (make-local-variable 'tab-width) 2)
       (set (make-local-variable 'coffee-tab-width) 2))
  )

(add-hook 'coffee-mode-hook
          '(lambda() (coffee-custom)))

;;; ts-mode
(require 'typescript-mode)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(setq typescript-indent-level 2)

;;; CSS

;; scss-mode
;; https://github.com/antonj/scss-mode
(autoload 'scss-mode "scss-mode" nil t)
; (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.\\(scss\\|css\\)\\'" . scss-mode))
(add-to-list 'auto-mode-alist '("\\.styl$" . scss-mode))
;(add-hook 'scss-mode-hook 'ac-css-mode-setup)
(add-hook 'scss-mode-hook
          (lambda ()
            (setq css-indent-offset 2)
            (setq scss-compile-at-save nil)
            (setq ac-sources '(ac-source-yasnippet
                               ;; ac-source-words-in-same-mode-buffers
                               ac-source-words-in-all-buffer
                               ac-source-dictionary
                               ))
            ))
;(add-to-list 'ac-modes 'scss-mode)

;; stylus-mode
;(require 'stylus-mode)

;; cperl-mode
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))

(add-to-list 'auto-mode-alist '("\\.p[lm]$" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.t$" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.psgi$" . cperl-mode))
(add-to-list 'auto-mode-alist '("cpanfile$" . cperl-mode))
(add-to-list 'auto-mode-alist '("Daikufile$" . cperl-mode))

(add-hook 'cperl-mode-hook
          '(lambda ()
             ;; インデント設定
             (cperl-set-style "PerlStyle")
             (custom-set-variables
              '(cperl-indent-parens-as-block t)
              '(cperl-close-paren-offset -4)
              '(cperl-indent-subs-specially nil))

             ;; ドキュメントを表示する
             ;(define-key global-map (kbd "M-p") 'cperl-perldoc)
             ))

;; kolon-mode
;; (autoload 'kolon-mode "kolon-mode")
;; (add-to-list 'auto-mode-alist '("\\.tx$" . kolon-mode))
;; (add-to-list 'auto-mode-alist '("\\.tt2$" . kolon-mode))

;; yaml-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; sh-mode
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zshrc.*" . sh-mode))

;; sql-mode
;; (eval-after-load "sql"
;;   '(load-library "sql-indent"))
;; (sql-set-product "mysql")

;;; Ruby
;; ruby-mode
(require 'ruby-mode)
(defun ruby-mode-set-encoding () nil)
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))

;; rspec-mode
;; (require 'rspec-mode)
;; (custom-set-variables '(rspec-use-rake-flag nil))
;; (eval-after-load 'rspec-mode
;;   '(rspec-install-snippets))

;;: markdown-mode
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-hook 'markdown-mode-hook
          '(lambda ()
             (set (make-local-variable 'delete-trailing-whitespece-before-save) nil)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((encoding . utf-8)))))
