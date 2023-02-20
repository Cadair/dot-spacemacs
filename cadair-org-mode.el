;; Org Mode Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;

;; This contains all the org mode config to break it out of the main spacemacs
;; file.
(setq org-directory "~/Notebooks/")
(setq org-duration-format 'h:mm)


(add-hook 'org-clock-in-hook #'save-buffer)
(add-hook 'org-clock-out-hook #'save-buffer)

;; Plugins etc
;;;;;;;;;;;;;;

(setq calendar-latitude 53.584)
(setq calendar-longitude -1.778)
(setq calendar-location-name "Holmfirth")

(setq request-log-level 'debug)
(setq request-message-level 'debug)

(use-package secretaria
  :config
  ;; use this for getting a reminder every 30 minutes of those tasks scheduled
  ;; for today and which have no time of day defined.
  (add-hook 'after-init-hook #'secretaria-unknown-time-always-remind-me))

(setq secretaria-clocked-task-save-file "~/Notebooks/secretaria-clocked-task")
(setq secretaria-notification-to-html t)


(defun secretaria-remind-task-clocked-in ()
  "Fires an alert for the user reminding him which task he is working on."
  (when org-clock-current-task
    (if (not org-clock-task-overrun)
        (notifications-notify :body (secretaria--org-to-html org-clock-current-task)
                              :title "Currently clocked"
                              :severity 'trivial)
      (notifications-notify :body (secretaria--org-to-html org-clock-current-task)
             :title (format "Task effort exceeded (%s)" (secretaria-task-clocked-time))
             :severity 'critical))))

(defun secretaria-notification-handler (notification)
  "Handle `org-mode' notifications.

`NOTIFICATION' is, well, the notification from `org-mode'"
  (if (not (s-contains? "should be finished by now" notification))
      (notifications-notify :title notification :resident)
    (notifications-notify :body (secretaria--org-to-html org-clock-current-task)
                          :title (format "Task effort reached (%s)" (secretaria-task-clocked-time))
                          :severity 'high
                          :resident)))

(defun secretaria-task-clocked-in ()
  "Start a timer when a task is clocked-in."
  (secretaria-task-save-clocked-task)
  (setf secretaria-clocked-in-reminder-timer (run-at-time (format "%s min" (or secretaria-clocked-in-reminder-every-minutes 10)) (* (or secretaria-clocked-in-reminder-every-minutes 10) 60) 'secretaria-remind-task-clocked-in))
  (notifications-notify :body(secretaria--org-to-html org-clock-current-task)
                        :title (format "Task clocked in (%s)" (secretaria-task-clocked-time))))

(defun secretaria-task-clocked-out ()
  "Stop reminding the clocked-in task."
  (secretaria--task-delete-save-clocked-task)
  (ignore-errors (cancel-timer secretaria-clocked-in-reminder-timer))
  (when org-clock-current-task
    (notifications-notify :body(secretaria--org-to-html org-clock-current-task)
                          :title (format "Task clocked out! (%s)" (secretaria-task-clocked-time))
                          :severity 'critical)))

(defun secretaria-task-clocked-canceled ()
  "Stop reminding the clocked-in task if it's canceled."
  (cancel-timer secretaria-clocked-in-reminder-timer)
  (when org-clock-current-task
    (notifications-notify :body(secretaria--org-to-html org-clock-current-task)
                          :title (format "Task canceled! (%s)" (secretaria-task-clocked-time))
                          :severity 'critical)))


;; auto-save all org files every 10s
;; (require 'real-auto-save)
;; (add-hook 'org-mode-hook 'real-auto-save-mode)

;; org-now config
;; (spacemacs/set-leader-keys "on" 'org-now)
;; (spacemacs/set-leader-keys "ow" 'org-now-refile-to-now)
;; (spacemacs/set-leader-keys "oW" 'org-now-refile-to-previous-location)

;;; ORG-MODE:  * My Task
;;;              SCHEDULED: <%%(diary-last-day-of-month date)>
;;; DIARY:  %%(diary-last-day-of-month date) Last Day of the Month
;;; See also:  (setq org-agenda-include-diary t)
;;; (diary-last-day-of-month '(2 28 2017))
(defun diary-last-day-of-month (date)
  "Return `t` if DATE is the last day of the month."
  (let* ((day (calendar-extract-day date))
         (month (calendar-extract-month date))
         (year (calendar-extract-year date))
         (last-day-of-month
          (calendar-last-day-of-month month year)))
    (= day last-day-of-month)))

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
  (setq ghlink (concat "https://nso.atlassian.net/browse/DCS-" link))
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
(setq cadair-default-org-files (file-expand-wildcards "~/Notebooks/*.org"))
(setq cadair-extra-org-files '())

(setq org-agenda-files (append cadair-default-org-files cadair-extra-org-files))

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
      (quote ((sequence "TODO(t)" "NEXT(n)" "WIP(i)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))
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

;; Custom agenda command definitions
(setq org-agenda-start-with-log-mode t)
(setq org-agenda-start-with-clockreport-mode t)

(setq org-agenda-custom-commands
      (quote
       (
        ("N" "Notes" tags "NOTE"
         ((org-agenda-overriding-header "Notes")
          (org-tags-match-list-sublevels t)))
        ("B" "Billable Agenda"
         ((agenda "" (
                      (org-agenda-span (quote month))
                      (org-agenda-skip-scheduled-if-deadline-is-shown nil)
                      (org-agenda-filter-by-tag 'billable)
                      ))
          ))
        ("p" "Primary Agenda"
         ((agenda "" (
                      (org-agenda-span (quote day))
                      (org-agenda-skip-scheduled-if-deadline-is-shown nil)
                      ))
          (tags "REFILE"
                ((org-agenda-overriding-header "Tasks to Refile")
                 (org-tags-match-list-sublevels nil)))
          ;; Reoccurring Tasks
          (tags-todo "+reoccurring-HOLD-CANCELLED"
                     ((org-agenda-overriding-header "Reoccurring Tasks")
                      (org-tags-match-list-sublevels nil)
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
          ;; DKIST Sprint
          (tags-todo "dkist&activesprint&-HOLD-CANCELLED"
                     ((org-agenda-overriding-header "This Sprint Tasks")
                      (org-tags-match-list-sublevels 'indented)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          ;; NASA Grant
          (tags-todo "sunpy&billable&-HOLD-CANCELLED"
                     ((org-agenda-overriding-header "SunPy NASA Tasks")
                      (org-tags-match-list-sublevels 'indented)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          ;; DKIST Projects
          (tags-todo "dkist&activemarathon&-HOLD-CANCELLED"
                     ((org-agenda-overriding-header "This Marathon Tasks")
                      (org-tags-match-list-sublevels 'indented)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          ;; Active Contracts
          (tags-todo "aperiocontracts&-HOLD-CANCELLED/!"
                     ((org-agenda-overriding-header "Active Contracts")
                      (org-tags-match-list-sublevels 'indented)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
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

;; Waybar
(load-file (expand-file-name "~/.config/spacemacs/org-clock-waybar/org-clock-waybar.el"))
(org-clock-waybar-setup)
(require 'org-clock-waybar)

(defun org-clock-waybar--get-tooltip ()
  "The default tooltip to send to waybar."
  (when (org-clocking-p)
    (let ((clocked-time (org-clock-get-clocked-time)))
      (format "%s: %s [%s] %s"
              (org-clock-waybar--get-task-category)
              (org-clock-waybar--get-task-title)
              (org-duration-from-minutes clocked-time)
              (format "%s" (org-clock-waybar--get-tags))))))
