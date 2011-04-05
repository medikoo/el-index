;; el-index.el --- Generic index mode for Emacs

;; Author:	Mariusz Nowak <mariusz+el-index@medikoo.com>;
;;          Karl Fogel <kfogel@red-bean.com>
;; Copyright (C) 2011 Mariusz Nowak <mariusz+el-index@medikoo.com>;
;;                    Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE. See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; See README.

(defface el-index-heading
	'((t (:inherit font-lock-type-face)))
	"Face used to highlight index heading.")

(defvar el-index-func-map
	'((select . nil)
		(rename . nil)
		(delete . nil)
		(reload . nil))
	"Method map for current index buffer")

(defvar el-index-data-map nil
	"Data map for current index")

(make-local-variable 'el-index-func-map)
(make-local-variable 'el-index-data-map)

(defun el-index-get-item-at-position ()
	"Returns current item."
	(let ((item (assoc
					(intern (int-to-string (line-number-at-pos))) el-index-data-map)))
		(if item
			(cdr item))))

(defun el-index-select ()
	"Selects curent item."
	(interactive)
	(el-index-funcall-with-quit-window (cdr (assoc 'select el-index-func-map))))

(defun el-index-funcall-with-quit-window (method)
	"Quits window and calls METHOD on current item."
	(let ((item (el-index-get-item-at-position)))
		(when item
			(quit-window)
			(funcall method item))))

(defun el-index-funcall (method)
	"Calls METHOD on current item."
	(let ((item (el-index-get-item-at-position)))
		(when item
			(funcall method item))))

(defun el-index-mark-for-delete ()
	"Mark current item for deletion."
	(interactive)
	(beginning-of-line)
	(let ((inhibit-read-only t))
		(delete-char 1)
		(insert ?D)
		(forward-line 1)))

(defun el-index-item-delete (item)
	"Deletes given ITEM"
	(if item
		(funcall (cdr (assoc 'delete el-index-func-map)) item)))

(defun el-index-mark-for-delete-backwards ()
	"Mark item on this line to be deleted, then move up one line.
	To carry out the deletions that you've marked, use `el-index-execute-deletions'."
	(interactive)
	(el-index-mark-for-delete)
	(forward-line -2))

(defun el-index-unmark ()
	"Cancel all requested operations on index on this line and move down."
	(interactive)
	(let ((item (el-index-get-item-at-position)))
		(when item
			(save-excursion
				(beginning-of-line)
				(let ((inhibit-read-only t))
					(delete-char 1)
					(insert " ")))
			(forward-line 1))))

(defun el-index-execute-deletions ()
	"Delete items flagged `D'."
	(interactive)
	(message "Deleting...")
	(goto-char (point-min))
	(forward-line 1)
	(while (re-search-forward "^D" (point-max) t)
		(el-index-item-delete (el-index-get-item-at-position)))
	(funcall (cdr (assoc 'reload el-index-func-map)))
	(beginning-of-line)
	(message "Deleting... done"))

(defun el-index-rename ()
	"Rename item on current line. Prompts for a new name."
	(interactive)
	(let ((item (el-index-get-item-at-position))
			(thispoint (point)))
		(when item
			(funcall (cdr (assoc 'rename el-index-func-map)) item
				(read-from-minibuffer "New name: "))
			(funcall (cdr (assoc 'reload el-index-func-map)))
			(goto-char thispoint))))

(defun el-index-reload ()
	"Reloads index list."
	(interactive)
	(funcall (cdr (assoc 'reload el-index-func-map))))

;;;###autoload
(defun el-index-display (name data func-map)
	"Display index.
	Index is displayed in a buffer named NAME.
	The leftmost column displays a D if the item is flagged for
	deletion."
	(let ((buf (get-buffer-create (concat "*" name "*"))))
		(if (or (window-dedicated-p) (window-minibuffer-p))
			(pop-to-buffer buf)
			(switch-to-buffer buf)))
	(let ((inhibit-read-only t) point-cache)
		(erase-buffer)
		(setq el-index-data-map ())
		(dolist (section data)
			(setq point-cache (point))
			(insert "% " (car section) "\n- --------\n")
			(add-text-properties point-cache (point)
				'(font-lock-face el-index-heading))
			(dolist (assoc (cdr section))
				(let ((start (point)) end)
					(insert "  ")
					(funcall (cdr (assoc 'write func-map)) assoc)
					(setq end (point))
					(if el-index-data-map
						(nconc el-index-data-map (list
								(cons (intern (int-to-string (line-number-at-pos))) assoc)))
						(setq el-index-data-map (list
								(cons (intern (int-to-string (line-number-at-pos))) assoc))))
					(when (display-mouse-p)
						(add-text-properties
							(+ 2 start) end
							'(mouse-face highlight
								follow-link t)))
					(insert "\n")))
			(insert "\n"))
		(goto-char (point-min))
		(forward-line 2)
		(el-index-mode)
		(setq el-index-func-map func-map)))

(defun el-index-mode ()
	"Major mode for index display.

\\{el-index-mode-map}"
	(kill-all-local-variables)
	(use-local-map el-index-mode-map)
	(setq truncate-lines t)
	(setq buffer-read-only t)
	(setq major-mode 'el-index-mode)
	(setq mode-name "Index mode"))

(setq el-index-mode-map
	(let ((map (make-keymap)))
		(suppress-keymap map t)
		(define-key map "q" 'quit-window)
		(define-key map "j" 'el-index-select)
		(define-key map "\C-c\C-c" 'el-index-select)
		(define-key map "f" 'el-index-select)
		(define-key map "\C-m" 'el-index-select)
		(define-key map "\C-d" 'el-index-mark-for-delete-backwards)
		(define-key map "x" 'el-index-execute-deletions)
		(define-key map "d" 'el-index-mark-for-delete)
		(define-key map "u" 'el-index-unmark)
		(define-key map "g" 'el-index-reload)
		(define-key map " " 'next-line)
		(define-key map "n" 'next-line)
		(define-key map "p" 'previous-line)
		(define-key map "?" 'describe-mode)
		(define-key map "r" 'el-index-rename)
		map))

(provide 'el-index/el-index)
