;;; facundo-complete.el --- indent and tab behavior commands  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Facundo Olano

;; Author: Facundo Olano; <facundo@madmobile>
;; Keywords: indentation

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(use-package orderless
  ;; fixes weird lsp-mode override of orderless behavior
  ;; https://magnus.therning.org/2024-05-04-orderless-completion-in-lsp-mode.html
  ;; https://github.com/minad/corfu/issues/41
  :hook (lsp-completion-mode . (lambda ()
                                 (setq-local completion-category-defaults
                                             (assoc-delete-all 'lsp-capf completion-category-defaults))))
  :custom
  (completion-styles '(orderless))
  (orderless-smart-case t)
  (orderless-matching-styles '(orderless-prefixes)))


(use-package company
  ;; only for programming. alternatively consider disabling auto complete
  ;; which is the annoying part in text editing
  :hook (prog-mode-hook . company-mode)
  ;; (company-mode . company-tng-mode)
  :custom
  (company-tooltip-align-annotations t)
  (company-minimum-prefix-length 2)
  (company-tooltip-limit 10)
  (company-tooltip-flip-when-above t)
  (company-backends '(company-capf company-files))
  (company-frontends '(
                       ;; company-tng-frontend
                       company-pseudo-tooltip-unless-just-one-frontend
                       company-preview-if-just-one-frontend
                       ;; company-preview-if-just-one-frontend
                       company-echo-metadata-frontend))
  
  
  (company-idle-delay 0.5)
  :bind (:map company-active-map
              ("C-w" . backward-kill-word)))


(setq my-indentation-offset 2)

(defun my-indent ()
  "If mark is active indent code block, otherwise indent or complete based on \
the position and the mode."
  (interactive)
  (if mark-active
      (save-mark-and-excursion
        (let ((beg (region-beginning)) (end (region-end)))
          (save-excursion
            (setq beg (progn (goto-char beg) (line-beginning-position))
                  end (progn (goto-char end) (line-end-position)))
            (indent-code-rigidly beg end my-indentation-offset)))
        (setq deactivate-mark nil))
    ;; FIXME this is getting hackier not sure I like it
    ;; triggers completion if at end of symbol or after stuff like dot and :
    ;; otherwise tries to indent
    (if (or (looking-at "\\_>")
            (member (char-before) '(?. ?: ?> ?/)))
        (company-complete)
      (indent-according-to-mode))))

(defun my-unindent ()
  "If mark is active shift left the code block, extending the region to include whole lines.  if no mark is set shift the current line."
  (interactive)
  (if mark-active
      (save-mark-and-excursion
        (let ((beg (region-beginning)) (end (region-end)))
          (save-excursion
            (setq beg (progn (goto-char beg) (line-beginning-position))
                  end (progn (goto-char end) (line-end-position)))
            (indent-code-rigidly beg end (- my-indentation-offset)))
          (setq deactivate-mark nil)))
    (indent-code-rigidly (line-beginning-position) (line-end-position) (- my-indentation-offset))))

(defun backspace-whitespace-to-tab-stop ()
  "Delete whitespace backwards to the next tab-stop, otherwise delete one character."
  (interactive)
  (if (or indent-tabs-mode
          (region-active-p)
          (save-excursion
            (> (point) (progn (back-to-indentation)
                              (point)))))
      (call-interactively 'backward-delete-char-untabify)
    (let ((movement (% (current-column) my-indentation-offset))
          (p (point)))
      (when (= movement 0) (setq movement my-indentation-offset))
      ;; Account for edge case near beginning of buffer
      (setq movement (min (- p 1) movement))
      (save-match-data
        (if (string-match "[^\t ]*\\([\t ]+\\)$" (buffer-substring-no-properties (- p movement) p))
            (backward-delete-char (- (match-end 1) (match-beginning 1)))
          (call-interactively 'backward-delete-char))))))

(define-key prog-mode-map (kbd "<tab>") 'my-indent)
(define-key prog-mode-map (kbd "<backtab>") 'my-unindent)
(provide 'facundo-complete)
;;; facundo-complete.el ends here
