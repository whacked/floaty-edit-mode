;;; floaty-edit-mode.el --- Minor mode for floating window editing -*- lexical-binding: t; -*-

;;; Commentary:
;; This minor mode provides a way to do editing with floating child frames in Emacs.

;;; Code:
(require 'cl-lib)

(defgroup floaty-edit nil
  "Floating window editing using child frames."
  :group 'editing)

(defcustom floaty-edit-open-directive-regexp "^\\(?:##\\|;;;\\|<!--\\) *f@\\([0-9]+\\)x\\([0-9]+\\)\\(?:\\+\\([0-9]+\\)\\)?\\(?:\\+\\([0-9]+\\)\\)? *\\(?:-->\\|*/\\)?$"
  "Regular expression to match floaty edit opening directives."
  :type 'regexp
  :group 'floaty-edit)

(defcustom floaty-edit-end-directive-regexp "^\\(?:##\\|;;;\\|<!--\\) *f@end *\\(?:-->\\|*/\\)?$"
  "Regular expression to match floaty edit end directives."
  :type 'regexp
  :group 'floaty-edit)

(defcustom floaty-edit-default-x-offset 80
  "Default x-offset for child frames when not specified in the directive."
  :type 'integer
  :group 'floaty-edit)

(defface floaty-edit-directive-face
  '((t :inherit font-lock-comment-face :weight bold :extend t
       :box (:line-width -1 :color "red" :style released-button)
       :background "lightblue"))
  "Face for floaty edit directives."
  :group 'floaty-edit)

(defvar floaty-edit-frames nil
  "List of child frames for the current floaty-edit session.")

(defvar floaty-edit-host-buffer nil
  "The host buffer of the current floaty-edit session.")

(defvar floaty-edit-host-frame nil
  "The host frame of the current floaty-edit session.")

(defvar floaty-edit-indirect-buffers nil
  "List of indirect buffers created for child frames.")

(defvar floaty-edit-directive-cache nil
  "Cache of parsed directives.")

(cl-defstruct floaty-edit-directive
  line width height x y start end frame)

(defun floaty-edit-parse-buffer ()
  "Parse the host buffer for floaty edit directives."
  (with-current-buffer (or floaty-edit-host-buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (let ((directives '())
            (line 1)
            (total-height 0)
            (current-directive nil)
            (buffer-end (point-max)))
        (while (< (point) buffer-end)
          (let ((line-start (point))
                (line-end (line-end-position)))
            (cond
             ;; Check for opening directive
             ((string-match floaty-edit-open-directive-regexp 
                            (buffer-substring-no-properties line-start line-end))
              (when current-directive
                (setf (floaty-edit-directive-end current-directive) line-start))
              (let* ((width (string-to-number (match-string 1 (buffer-substring-no-properties line-start line-end))))
                     (height (string-to-number (match-string 2 (buffer-substring-no-properties line-start line-end))))
                     (x (if (match-string 3 (buffer-substring-no-properties line-start line-end))
                            (string-to-number (match-string 3 (buffer-substring-no-properties line-start line-end)))
                          floaty-edit-default-x-offset))
                     (y (if (match-string 4 (buffer-substring-no-properties line-start line-end))
                            (string-to-number (match-string 4 (buffer-substring-no-properties line-start line-end)))
                          total-height)))
                (setq current-directive (make-floaty-edit-directive
                                         :line line
                                         :width width
                                         :height height
                                         :x x
                                         :y y
                                         :start line-start
                                         :end nil
                                         :frame nil))
                (push current-directive directives)
                (setq total-height (+ y height 1))))
             
             ;; Check for ending directive
             ((string-match floaty-edit-end-directive-regexp
                            (buffer-substring-no-properties line-start line-end))
              (when current-directive
                (setf (floaty-edit-directive-end current-directive) line-start)
                (setq current-directive nil))))
            
            (forward-line 1)
            (setq line (1+ line))))
        
        ;; Handle case where last directive extends to end of buffer
        (when current-directive
          (setf (floaty-edit-directive-end current-directive) buffer-end))
        
        (nreverse directives)))))

(defun floaty-edit-create-child-frame (directive)
  "Create a child frame for the given DIRECTIVE."
  (let* ((parent-frame floaty-edit-host-frame)
         (parent-width (frame-pixel-width parent-frame))
         (parent-height (frame-pixel-height parent-frame))
         (char-width (frame-char-width parent-frame))
         (char-height (frame-char-height parent-frame))
         (pixel-width (* (floaty-edit-directive-width directive) char-width))
         (pixel-height (* (floaty-edit-directive-height directive) char-height))
         (pixel-x (* (floaty-edit-directive-x directive) char-width))
         (pixel-y (* (floaty-edit-directive-y directive) char-height))
         (indirect-buffer (make-indirect-buffer floaty-edit-host-buffer
                                                (generate-new-buffer-name
                                                 (format "*floaty-%s*" (buffer-name floaty-edit-host-buffer)))
                                                t))
         (child-frame (make-frame
                       `((parent-frame . ,parent-frame)
                         (width . ,(floaty-edit-directive-width directive))
                         (height . ,(floaty-edit-directive-height directive))
                         (min-width . 1)
                         (min-height . 1)
                         (left . ,(min pixel-x (- parent-width pixel-width)))
                         (top . ,(min pixel-y (- parent-height pixel-height)))
                         (auto-hide-function . t)
                         (auto-hide . t)))))
    (push indirect-buffer floaty-edit-indirect-buffers)
    (with-selected-frame child-frame
      (switch-to-buffer indirect-buffer)
      (narrow-to-region (floaty-edit-directive-start directive) (floaty-edit-directive-end directive))
      (goto-char (point-min))
      ;; Add mouse event handlers to the child frame
      ;; (define-key special-event-map [down-mouse-1] 'floaty-edit-mouse-down)
      ;; (define-key special-event-map [mouse-1] 'floaty-edit-mouse-up)
      )
    (setf (floaty-edit-directive-frame directive) child-frame)
    child-frame))

(defun floaty-edit-directive-equal-p (dir1 dir2)
  "Check if two directives DIR1 and DIR2 are equal."
  (and (= (floaty-edit-directive-line dir1) (floaty-edit-directive-line dir2))
       (= (floaty-edit-directive-width dir1) (floaty-edit-directive-width dir2))
       (= (floaty-edit-directive-height dir1) (floaty-edit-directive-height dir2))
       (= (floaty-edit-directive-x dir1) (floaty-edit-directive-x dir2))
       (= (floaty-edit-directive-y dir1) (floaty-edit-directive-y dir2))))

(defun floaty-edit-update-frames ()
  "Update or create child frames based on directives in the host buffer."
  (interactive)
  (when (and floaty-edit-mode floaty-edit-host-buffer)
    (with-selected-frame floaty-edit-host-frame
      (let ((new-directives (floaty-edit-parse-buffer))
            (old-directives floaty-edit-directive-cache)
            (frames-to-delete '()))
        (setq floaty-edit-frames nil)
        ;; Update or create frames
        (cl-loop for new-dir in new-directives
                 for old-dir = (cl-find-if (lambda (d) (= (floaty-edit-directive-line d) (floaty-edit-directive-line new-dir))) old-directives)
                 do (if (and old-dir (floaty-edit-directive-equal-p new-dir old-dir))
                        (progn
                          (setf (floaty-edit-directive-frame new-dir) (floaty-edit-directive-frame old-dir))
                          (push (floaty-edit-directive-frame new-dir) floaty-edit-frames))
                      (when old-dir
                        (push (floaty-edit-directive-frame old-dir) frames-to-delete))
                      (push (floaty-edit-create-child-frame new-dir) floaty-edit-frames)))
        ;; Delete unused frames
        (dolist (frame frames-to-delete)
          (when frame (delete-frame frame)))
        ;; Update cache
        (setq floaty-edit-directive-cache new-directives)
        ;; Clean up unused indirect buffers
        (setq floaty-edit-indirect-buffers
              (cl-remove-if-not (lambda (buf) (buffer-live-p buf)) floaty-edit-indirect-buffers))

        (message "current floaty edit frames")
        (message "%s" floaty-edit-frames)))))

(defun floaty-edit-focus-host-frame ()
  "Focus the host frame of the floaty-edit session."
  (interactive)
  (when floaty-edit-mode
    (select-frame-set-input-focus floaty-edit-host-frame)))

(defun floaty-edit-update-directive (frame new-x new-y)
  "Update the directive in the host buffer for FRAME with NEW-X and NEW-Y."
  (with-current-buffer floaty-edit-host-buffer
    (save-excursion
      (goto-char (point-min))
      (let ((directive (cl-find-if (lambda (d) (eq (floaty-edit-directive-frame d) frame))
                                   floaty-edit-directive-cache)))
        (when directive
          (goto-char (floaty-edit-directive-start directive))
          (when (looking-at floaty-edit-directive-regexp)
            (let ((width (floaty-edit-directive-width directive))
                  (height (floaty-edit-directive-height directive)))
              (replace-match (format ";; @%dx%d+%d+%d" width height new-x new-y))
              (setf (floaty-edit-directive-x directive) new-x
                    (floaty-edit-directive-y directive) new-y))))))))

(defun floaty-edit-fold-all ()
  "Fold all floaty-edit sections in the buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward floaty-edit-open-directive-regexp nil t)
      (outline-hide-entry))))

(defun floaty-edit-unfold-all ()
  "Unfold all floaty-edit sections in the buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward floaty-edit-open-directive-regexp nil t)
      (outline-show-entry))))

(defun floaty-edit-toggle-fold ()
  "Toggle folding of the floaty-edit section at point."
  (interactive)
  (save-excursion
    (end-of-line)
    (if (outline-invisible-p)
        (outline-show-entry)
      (outline-hide-entry))))

;; (defvar floaty-edit-dragging nil
;;   "The frame currently being dragged, or nil if no dragging is in progress.")
;; 
;; (defvar floaty-edit-drag-start nil
;;   "The starting position of the mouse when dragging began.")
;; 
;; (defun floaty-edit-mouse-down (event)
;;   "Handle mouse down event to start dragging a child frame."
;;   (interactive "e")
;;   (message "MOUSE DOWN!")
;;   (let* ((mouse-pos (event-start event))
;;          (frame (window-frame (posn-window mouse-pos))))
;;     (message "CURRENT FRAME: %s" frame)
;;     (message "floaty frames: %s" floaty-edit-frames)
;;     (when (member frame floaty-edit-frames)
;;       (setq floaty-edit-drag-start (cons frame (mouse-pixel-position))))))
;; 
;; (defun floaty-edit-mouse-dragging (event)
;;   "Handle mouse dragging event to move a child frame."
;;   (interactive "e") ;; Ensure it's interactive
;;   (message "delete me %s" floaty-edit-drag-start)
;;   (when floaty-edit-drag-start
;;     (message "DRAG START %s" 1)
;;     (let* ((start-frame (car floaty-edit-drag-start))
;;            (start-pos (cdr floaty-edit-drag-start))
;;            (end-pos (mouse-pixel-position))
;;            (dx (- (cadr end-pos) (cadr start-pos)))
;;            (dy (- (cddr end-pos) (cddr start-pos)))
;;            (frame-pos (frame-position start-frame))
;;            (new-x (+ (car frame-pos) dx))
;;            (new-y (+ (cdr frame-pos) dy)))
;;       (set-frame-position start-frame new-x new-y)
;;       (floaty-edit-update-directive start-frame 
;;                                     (floor new-x (frame-char-width))
;;                                     (floor new-y (frame-char-height))))
;;     (setq floaty-edit-drag-start nil)))
;; 
;; (defun floaty-edit-mouse-up (event)
;;   "Handle mouse up event to reposition a child frame."
;;   (interactive "e")
;;   (when floaty-edit-drag-start
;;     (let* ((start-frame (car floaty-edit-drag-start))
;;            (start-pos (cdr floaty-edit-drag-start))
;;            (end-pos (mouse-pixel-position))
;;            (dx (- (cadr end-pos) (cadr start-pos)))
;;            (dy (- (cddr end-pos) (cddr start-pos)))
;;            (frame-pos (frame-position start-frame))
;;            (new-x (+ (car frame-pos) dx))
;;            (new-y (+ (cdr frame-pos) dy)))
;;       (set-frame-position start-frame new-x new-y)
;;       (floaty-edit-update-directive start-frame 
;;                                     (floor new-x (frame-char-width))
;;                                     (floor new-y (frame-char-height))))
;;     (setq floaty-edit-drag-start nil)))

(defun floaty-edit-child-mode ()
  "Minor mode for child frames in floaty-edit."
  (setq-local floaty-edit-mode t)
  (setq-local floaty-edit-host-buffer floaty-edit-host-buffer)
  (setq-local floaty-edit-host-frame floaty-edit-host-frame))

;;;###autoload
(define-minor-mode floaty-edit-mode
  "Toggle Floaty Edit mode."
  :lighter " Floaty"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-u") #'floaty-edit-update-frames)
            (define-key map (kbd "C-c C-h") #'floaty-edit-focus-host-frame)
            (define-key map (kbd "TAB") #'floaty-edit-toggle-fold)
            (define-key map (kbd "C-c C-f") #'floaty-edit-fold-all)
            (define-key map (kbd "C-c C-n") #'floaty-edit-unfold-all)
            map)
  (if floaty-edit-mode
      (progn
        (setq floaty-edit-host-buffer (current-buffer)
              floaty-edit-host-frame (selected-frame))
        (font-lock-add-keywords nil `((,floaty-edit-directive-regexp 0 'floaty-edit-directive-face prepend)))
        (font-lock-flush)
        (outline-minor-mode 1)
        (setq-local outline-regexp floaty-edit-directive-regexp)
        (floaty-edit-update-frames)
        (floaty-edit-fold-all))
    (dolist (frame floaty-edit-frames)
      (delete-frame frame))
    (dolist (buf floaty-edit-indirect-buffers)
      (kill-buffer buf))
    (setq floaty-edit-frames nil
          floaty-edit-indirect-buffers nil
          floaty-edit-directive-cache nil
          floaty-edit-host-buffer nil
          floaty-edit-host-frame nil)
    (font-lock-remove-keywords nil `((,floaty-edit-directive-regexp 0 'floaty-edit-directive-face)))
    (font-lock-flush)
    (outline-minor-mode -1)))

(provide 'floaty-edit-mode)

;;; floaty-edit-mode.el ends here
