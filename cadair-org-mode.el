;; Org Mode Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;

;; This contains all the org mode config to break it out of the main spacemacs
;; file.

;; Export
;;;;;;;;;

;; Latex Export Settings
(setq org-latex-caption-above nil)

(setq org-export-backends (quote (
                                  md
                                  odt
                                  latex
                                  confluence
                                  taskjuggler
                                  )))

;; Editing
;;;;;;;;;;

;; (setq org-startup-indented t)


;; Links
(setq cadair-default-gh-repo "sunpy/sunpy")

(defun cadair-gh-open (link)
  """Complete a link to a github issue / PR"""
  (if (string-prefix-p "#" link)
      (setq link2 (concat cadair-default-gh-repo link))
    (setq link2 link)
    )
  (setq ghlink (concat "https://github.com/" (replace-regexp-in-string "#" "/issues/" link2)))
  ;; (message ghlink)
  (org-open-link-from-string ghlink)
  )

(org-add-link-type "gh" 'cadair-gh-open)

(defun cadair-jira-open (link)
  """Complete a link to a jira ticket"""
  (setq ghlink (concat "https://nso-atst.atlassian.net/browse/DCS-" link))
  ;; (message ghlink)
  (org-open-link-from-string ghlink)
  )

(org-add-link-type "DCS" 'cadair-jira-open)
;; (defun org-make-gh-link-description (link desc)
;;   (when (string-prefix-p "gh:" link)
;;     (concat "#" (nth 1 (split-string link "#")))
;;     )
;;   )
;; (setq org-make-link-description-function #'org-make-gh-link-description)

;; Capture
;;;;;;;;;;

(defadvice org-capture
    (after make-full-window-frame activate)
  "Advise capture to be the only window when used as a popup"
  (if (equal "emacs-capture" (frame-parameter nil 'name))
      (delete-other-windows)))

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame"
  (if (equal "emacs-capture" (frame-parameter nil 'name))
      (delete-frame)))

;;file to save todo items
(setq org-agenda-files (file-expand-wildcards "~/Notebooks/*.org"))

;;set priority range from A to C with default A
(setq org-highest-priority ?A)
(setq org-lowest-priority ?D)
(setq org-default-priority ?C)

;;set colours for priorities
(setq org-priority-faces '((?A . (:foreground "#F0DFAF" :weight bold))
                            (?B . (:foreground "LightSteelBlue"))
                            (?C . (:foreground "OliveDrab"))))


(defvar cadair-capture-file "~/Notebooks/refile.org")
(setq org-default-notes-file cadair-capture-file)

;; This seems to work for protocol setup: http://www.mediaonfire.com/blog/2017_07_21_org_protocol_firefox.html
;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file cadair-capture-file)
               "* TODO %i%?\n" :clock-in t :clock-resume t)
              ("x" "review" entry (file cadair-capture-file)
               "* TODO Review %?%c\n" :clock-in t :clock-resume t)
              ("L" "Protocol" entry (file cadair-capture-file)
               "* TODO Review %? [[%:link][%:description]] \nCaptured On: %U")
              ("p" "Protocol" entry (file cadair-capture-file)
               "* TODO %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
              ("n" "note" entry (file cadair-capture-file)
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file cadair-capture-file)
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))


;; Refile
;;;;;;;;;

; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))
; Use the current window when visiting files and buffers with ido
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)

; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)


;; Task States
;;;;;;;;;;;;;;

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")
              (sequence "PROJECT" "|"))
      )
)

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "yellow" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              )))

(setq org-use-fast-todo-selection t)

;; The triggers break down to the following rules:
;;
;; Moving a task to CANCELLED adds a CANCELLED tag
;; Moving a task to WAITING adds a WAITING tag
;; Moving a task to HOLD adds WAITING and HOLD tags
;; Moving a task to a done state removes WAITING and HOLD tags
;; Moving a task to TODO removes WAITING, CANCELLED, and HOLD tags
;; Moving a task to NEXT removes WAITING, CANCELLED, and HOLD tags
;; Moving a task to DONE removes WAITING, CANCELLED, and HOLD tags


(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              (done ("WAITING") ("HOLD"))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))


;; Agenda
;;;;;;;;;
; Ignore taskjuggler tag in the agenda
(setq org-agenda-hide-tags-regexp "taskjuggler_project")
;; Agenda clock report parameters
(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 10 :fileskip0 t :compact t :narrow 80)))
;;open agenda in current window
(setq org-agenda-window-setup (quote current-window))
;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)
;; Compact the block agenda view
(setq org-agenda-compact-blocks nil)

(defvar bh/hide-scheduled-and-waiting-next-tasks t)

;; org-super-agenda
;; (require 'org-super-agenda)
;; (org-super-agenda-mode)
;; (setq org-super-agenda-groups '(
;;                                 (:name "Overdue"
;;                                        :deadline past
;;                                        :scheduled past)
;;                                 (:name "Today"
;;                                        :time-grid t
;;                                        )
;;                                 (:name "High Priority"
;;                                        :priority "A"
;;                                        )
;;                                 ))

;; Custom agenda command definitions

(setq org-agenda-custom-commands
      (quote (("N" "Notes" tags "NOTE"
              ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              (" " "Primary Agenda"
                ((agenda "" (
                             (org-agenda-span (quote day))
                             (org-agenda-skip-scheduled-if-deadline-is-shown nil)
                             ))
                (tags "REFILE"
                      ((org-agenda-overriding-header "Tasks to Refile")
                      (org-tags-match-list-sublevels nil)))
                ;; DKIST Projects
                (tags-todo "dkist&activesprint&-HOLD-CANCELLED"
                           ((org-agenda-overriding-header "Active Sprint Tasks")
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "dkist&-HOLD-CANCELLED/!"
                           ((org-agenda-overriding-header "DKIST Projects")
                            (org-agenda-skip-function 'bh/skip-non-projects)
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                ;; Priority Tasks
                (tags-todo "+PRIORITY=\"A\"|+PRIORITY=\"B\""
                           (
                            (org-agenda-overriding-header (concat "Priority Tasks"))
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-sorting-strategy
                             '(priority-down))
                            ))
                ;; Waiting and Postponed
                (tags-todo "-CANCELLED+WAITING|HOLD/!"
                           ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-non-tasks)
                            (org-tags-match-list-sublevels nil)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
                ;; Stuck
                (tags-todo "-CANCELLED/!"
                           ((org-agenda-overriding-header "Stuck Projects")
                            (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                ;; Project Next Tasks
                (tags-todo "-CANCELLED/!NEXT"
                          ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                            '(todo-state-down priority-down effort-up category-keep))))
                ;; (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                ;;           ((org-agenda-overriding-header (concat "Project Subtasks"
                ;;                                                   (if bh/hide-scheduled-and-waiting-next-tasks
                ;;                                                       ""
                ;;                                                     " (including WAITING and SCHEDULED tasks)")))
                ;;             (org-agenda-skip-function 'bh/skip-non-project-tasks)
                ;;             (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                ;;             (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                ;;             (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                ;;             (org-agenda-sorting-strategy
                ;;             '(category-keep))))
                ;; (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                ;;           ((org-agenda-overriding-header (concat "Standalone Tasks"
                ;;                                                   (if bh/hide-scheduled-and-waiting-next-tasks
                ;;                                                       ""
                ;;                                                     " (including WAITING and SCHEDULED tasks)")))
                ;;             (org-agenda-skip-function 'bh/skip-project-tasks)
                ;;             (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                ;;             (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                ;;             (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                ;;             (org-agenda-sorting-strategy
                ;;             '(category-keep))))
                (tags "-REFILE/"
                      ((org-agenda-overriding-header "Tasks to Archive")
                      (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                      (org-tags-match-list-sublevels nil))))
              nil))))
;;;;open agenda in current window
(setq org-agenda-window-setup (quote current-window))
;;;;warn me of any deadlines in next 7 days
(setq org-deadline-warning-days 7)
;; Weeks start on Monday you nutters
(setq org-agenda-start-on-weekday 1)
;;;;show me tasks scheduled or due in next fortnight
;;;; (setq org-agenda-span (quote fortnight))
;;;;don't show tasks as scheduled if they are already shown as a deadline
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
;;;;don't give awarning colour to tasks with impending deadlines
;;;;if they are scheduled to be done
;;;;(setq org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled))
;;;;don't show tasks that are scheduled or have deadlines in the
;;;;normal todo list
;;;;(setq org-agenda-todo-ignore-deadlines (quote all))
;;;;(setq org-agenda-todo-ignore-scheduled (quote all))
;;(setq org-agenda-skip-scheduled-if-done t)
;;;;sort tasks in order of when they are due and then by priority
;;(setq org-agenda-sorting-strategy
;;      (quote
;;       ((agenda deadline-up priority-down)
;;        (todo priority-down category-keep)
;;        (tags priority-down category-keep)
;;        (search category-keep)))
;;      )
(setq org-fontify-done-headline t)
(custom-set-faces
  '(org-done ((t (
                              :weight normal
                              :strike-through t))))
  '(org-headline-done
    ((((class color) (min-colors 16))
      (:strike-through t)))))

(setq org-startup-folded nil)
