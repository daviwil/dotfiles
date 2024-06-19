;; -*- lexical-binding: t; -*-

(require 'dw-org)

;;; -- General Org Mode Config -----

(setq org-directory
      (if dw/is-termux
          "~/storage/shared/Notes"
        "~/Notes"))

(defvar dw/base-agenda-files '("Inbox.org" "Schedule.org") ;; "Mesche.org" "SystemCrafters.org")
  "The base agenda files that will always be included.")

(defun dw/org-path (path)
  (expand-file-name path org-directory))

(setq org-default-notes-file (dw/org-path "Inbox.org"))

;; Turn on indentation and auto-fill mode for Org files
(defun dw/org-mode-setup ()
  ;; (variable-pitch-mode 1)
  (org-indent-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq corfu-auto nil)
  (setq evil-auto-indent nil))

(defun dw/org-move-done-tasks-to-bottom ()
  "Sort all tasks in the topmost heading by TODO state."
  (interactive)
  (save-excursion
    (while (org-up-heading-safe))
    (org-sort-entries nil ?o))

  ;; Reset the view of TODO items
  (org-overview)
  (org-show-entry)
  (org-show-children))

(defun dw/org-todo-state-change-hook ()
  (when (string= org-state "DONE")
    (dw/org-move-done-tasks-to-bottom)))

;; NOTE: This seems to run before log entry is added so the log gets added to the parent heading...
                                        ;(add-hook 'org-after-todo-state-change-hook 'dw/org-todo-state-change-hook)

(use-package org
  :hook (org-mode . dw/org-mode-setup)
  :bind (("C-c o n" . org-toggle-narrow-to-subtree)
         ("C-c o a" . org-agenda)
         ("C-c o t" . (lambda ()
                        (interactive)
                        ;; Display tasks after selecting tags to filter by
                        (org-tags-view t)))
         ("C-c o c" . 'org-capture)
         ("C-c o x" . 'org-export-dispatch)
         ("C-c o D" . 'dw/org-move-done-tasks-to-bottom)
         :map org-mode-map
         ("M-n" . org-move-subtree-down)
         ("M-p" . org-move-subtree-up))
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-fontify-quote-and-verse-blocks t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 2
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'content
        org-cycle-separator-lines 2
        org-capture-bookmark nil)

  (setq org-modules
        '(org-crypt
          org-habit
          org-bookmark
          org-eshell
          org-irc))

  (setq org-refile-targets '((nil :maxlevel . 1)
                             (org-agenda-files :maxlevel . 1))
        ;; Refile items to the top of parent heading
        org-reverse-note-order t)

  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)))
                                        ;(ledger . t))) -- Not working right now

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(use-package org-modern
  :hook (org-mode . org-modern-mode))

(use-package org-faces
  :ensure nil
  :after org
  :config
  ;; Increase the size of various headings
  (set-face-attribute 'org-document-title nil :font dw/org-heading-font :weight 'medium :height 1.3)
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font dw/org-heading-font :weight 'medium :height (cdr face))))

;; This is needed as of Org 9.2
(use-package org-tempo
  :ensure nil
  :after org
  :config
  (dolist (item '(("sh" . "src sh")
                  ("el" . "src emacs-lisp")
                  ("li" . "src lisp")
                  ("sc" . "src scheme")
                  ("ts" . "src typescript")
                  ("py" . "src python")
                  ("go" . "src go")
                  ("einit" . "src emacs-lisp :tangle ~/.config/emacs/init.el :mkdirp yes")
                  ("emodule" . "src emacs-lisp :tangle ~/.config/emacs/modules/dw-MODULE.el :mkdirp yes")
                  ("yaml" . "src yaml")
                  ("json" . "src json")))
    (add-to-list 'org-structure-template-alist item)))

(use-package org-pomodoro
  :commands org-pomodoro
  :config
  (setq org-pomodoro-start-sound "~/.dotfiles/.emacs.d/sounds/focus_bell.wav")
  (setq org-pomodoro-short-break-sound "~/.dotfiles/.emacs.d/sounds/three_beeps.wav")
  (setq org-pomodoro-long-break-sound "~/.dotfiles/.emacs.d/sounds/three_beeps.wav")
  (setq org-pomodoro-finished-sound "~/.dotfiles/.emacs.d/sounds/meditation_bell.wav"))

(require 'org-protocol)

;; (use-package org-make-toc
;;   :hook (org-mode . org-make-toc))

;; (use-package org-wild-notifier
;;   :after org
;;   :config
;;   ; Make sure we receive notifications for non-TODO events
;;   ; like those synced from Google Calendar
;;   (setq org-wild-notifier-keyword-whitelist nil)
;;   (setq org-wild-notifier-notification-title "Agenda Reminder")
;;   (setq org-wild-notifier-alert-time 15)
;;   (org-wild-notifier-mode))

(defvar dw/org-roam-project-template
  '("p" "project" plain "** TODO %?"
    :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                           "#+title: ${title}\n#+category: ${title}\n#+filetags: Project\n"
                           ("Tasks"))))

(defun my/org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun my/org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
          (seq-filter
           (my/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (push arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(defun dw/org-roam-goto-month ()
  (interactive)
  (org-roam-capture- :goto (when (org-roam-node-from-title-or-alias (format-time-string "%Y-%B")) '(4))
                     :node (org-roam-node-create)
                     :templates '(("m" "month" plain "\n* Goals\n\n%?* Summary\n\n"
                                   :if-new (file+head "%<%Y-%B>.org"
                                                      "#+title: %<%Y-%B>\n#+filetags: Project\n")
                                   :unnarrowed t))))

(defun dw/org-roam-goto-year ()
  (interactive)
  (org-roam-capture- :goto (when (org-roam-node-from-title-or-alias (format-time-string "%Y")) '(4))
                     :node (org-roam-node-create)
                     :templates '(("y" "year" plain "\n* Goals\n\n%?* Summary\n\n"
                                   :if-new (file+head "%<%Y>.org"
                                                      "#+title: %<%Y>\n#+filetags: Project\n")
                                   :unnarrowed t))))

(defun dw/org-roam-capture-task ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Capture the new task, creating the project file if necessary
  (org-roam-capture- :node (org-roam-node-read
                            nil
                            (my/org-roam-filter-by-tag "Project"))
                     :templates (list dw/org-roam-project-template)))

(defun my/org-roam-refresh-agenda-list ()
  (interactive)
  (setq org-agenda-files (my/org-roam-list-notes-by-tag "Project")))

(defhydra dw/org-roam-jump-menu (:hint nil)
  "
^Dailies^        ^Capture^       ^Jump^
^^^^^^^^-------------------------------------------------
_t_: today       _T_: today       _m_: current month
_r_: tomorrow    _R_: tomorrow    _e_: current year
_y_: yesterday   _Y_: yesterday   ^ ^
_d_: date        ^ ^              ^ ^
"
  ("t" org-roam-dailies-goto-today)
  ("r" org-roam-dailies-goto-tomorrow)
  ("y" org-roam-dailies-goto-yesterday)
  ("d" org-roam-dailies-goto-date)
  ("T" org-roam-dailies-capture-today)
  ("R" org-roam-dailies-capture-tomorrow)
  ("Y" org-roam-dailies-capture-yesterday)
  ("m" dw/org-roam-goto-month)
  ("e" dw/org-roam-goto-year)
  ("c" nil "cancel"))

(use-package org-roam
  :custom
  (org-roam-directory "~/Notes/Roam/")
  (org-roam-dailies-directory "Journal/")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)))
  (org-roam-dailies-capture-templates
   `(("d" "default" entry
      "* %?"
      :if-new (file+head ,dw/daily-note-filename
                         ,dw/daily-note-header))
     ("t" "task" entry
      "* TODO %?\n  %U\n  %a\n  %i"
      :if-new (file+head+olp ,dw/daily-note-filename
                             ,dw/daily-note-header
                             ("Tasks"))
      :empty-lines 1)
     ("l" "log entry" entry
      "* %<%I:%M %p> - %?"
      :if-new (file+head+olp ,dw/daily-note-filename
                             ,dw/daily-note-header
                             ("Log")))
     ("j" "journal" entry
      "* %<%I:%M %p> - Journal  :journal:\n\n%?\n\n"
      :if-new (file+head+olp ,dw/daily-note-filename
                             ,dw/daily-note-header
                             ("Log")))
     ("m" "meeting" entry
      "* %<%I:%M %p> - %^{Meeting Title}  :meetings:\n\n%?\n\n"
      :if-new (file+head+olp ,dw/daily-note-filename
                             ,dw/daily-note-header
                             ("Log")))))
  ;; :bind (("C-c n l" . org-roam-buffer-toggle)
  ;;        ("C-c n d" . dw/org-roam-jump-menu/body)
  ;;        ("C-c n c" . org-roam-dailies-capture-today)
  ;;        ("C-c n t" . dw/org-roam-capture-task)
  ;;        ("C-c n g" . org-roam-graph)
  ;;        :map org-mode-map
  ;;        ("C-c n i" . org-roam-node-insert)
  ;;        ("C-c n I" . org-roam-insert-immediate))

  :config
  (setq org-roam-v2-ack t)
  (setq dw/daily-note-filename "%<%Y-%m-%d>.org"
        dw/daily-note-header "#+title: %<%Y-%m-%d %a>\n\n[[roam:%<%Y-%B>]]\n\n")

  (org-roam-db-autosync-mode))

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(defun dw/refresh-agenda-files ()
  (interactive)
  (setq org-agenda-files
        (append (denote-directory-files-matching-regexp "_pra")
                dw/base-agenda-files)))

(defun dw/insert-topic-links ()
  (interactive)
  (let* ((topics (mapcar (lambda (file)
                           (cons (denote-retrieve-front-matter-title-value file 'org)
                                 (denote-retrieve-filename-identifier file)))
                         (denote-directory-files-matching-regexp "_kt")))
         (selected (completing-read-multiple "Select topics: " topics nil t)))
    (insert (string-join (mapcar (lambda (topic)
                                   (format "[[denote:%s][%s]]"
                                           (alist-get topic
                                                      topics
                                                      nil
                                                      nil
                                                      #'string=)
                                           topic))
                                 selected)
                         " "))))

(use-package denote
  :demand t
  :bind (("C-c n l" . denote-link-or-create)
         ("C-c n o" . denote-open-or-create)
         ("C-c n r" . denote-rename-file-using-front-matter))
  :custom
  (denote-directory "~/Notes/Denote")
  (denote-rename-buffer-format "Denote: %t (%k)")
  (denote-infer-keywords nil)
  (denote-known-keywords
   '("pra" "prb" "prc"
     "ply" "plm" "plw"
     "kt" "ke" "kp" "kl" "ka" "kap"
     "kcp" "kca" "kcc"
     "kra" "krb" "krv"
     "rn"))
  :config

  ;; Refresh agenda files the first time
  (dw/refresh-agenda-files)

  (require 'denote-rename-buffer)
  (require 'denote-org-extras)

  ;; Rename buffers with the note name
  (denote-rename-buffer-mode 1)

  ;; Update agenda files after notes are created or renamed
  (add-hook 'denote-after-rename-file-hook #'dw/refresh-agenda-files)
  (add-hook 'denote-after-new-note-hook #'dw/refresh-agenda-files)

  ;; Buttonize all denote links in text buffers
  (add-hook 'find-file-hook #'denote-link-buttonize-buffer))

(use-package consult-notes
  :ensure t
  :bind (("C-c n f" . consult-notes))
  :custom
  (consult-notes-denote-display-id nil)
  :config
  (consult-notes-org-roam-mode)
  (consult-notes-denote-mode))

(defun dw/denote-find-daily-log ()
  (interactive)
  (let* ((default-directory denote-directory)
         (existing-file (denote-directory-files-matching-regexp (format-time-string "^%Y%m%d.*_daily"))))
    (if existing-file
        (find-file (expand-file-name (car existing-file)))
      ;; TODO: Initialize with daily note format
      (denote (format-time-string "%A, %B %e, %Y")
              '("daily")))))

(with-eval-after-load 'org-roam
  (defun my/org-roam-project-finalize-hook ()
    "Adds the captured project file to `org-agenda-files' if the
capture was not aborted."
    ;; Remove the hook since it was added temporarily
    (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

    ;; Add project file to the agenda list if the capture was confirmed
    (unless org-note-abort
      (with-current-buffer (org-capture-get :buffer)
        (add-to-list 'org-agenda-files (buffer-file-name)))))

  (defun my/org-roam-find-project ()
    (interactive)
    ;; Add the project file to the agenda after capture is finished
    (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

    ;; Select a project file to open, creating it if necessary
    (org-roam-node-find
     nil
     nil
     (my/org-roam-filter-by-tag "Project")
     :templates
     '(("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
        :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
        :unnarrowed t))))

  (defun my/org-roam-capture-inbox ()
    (interactive)
    (org-roam-capture- :node (org-roam-node-create)
                       :templates '(("i" "inbox" plain "* %?"
                                     :if-new (file+head "Inbox.org" "#+title: Inbox\n")))))

  (defun my/org-roam-copy-todo-to-today ()
    (interactive)
    (let ((org-refile-keep t) ;; Set this to nil to delete the original!
          (org-roam-dailies-capture-templates
           '(("t" "tasks" entry "%?"
              :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
          (org-after-refile-insert-hook #'save-buffer)
          today-file
          pos)
      (save-window-excursion
        (org-roam-dailies--capture (current-time) t)
        (setq today-file (buffer-file-name))
        (setq pos (point)))

      ;; Only refile if the target file is different than the current file
      (unless (equal (file-truename today-file)
                     (file-truename (buffer-file-name)))
        (org-refile nil nil (list "Tasks" today-file nil pos)))))

  ;; (add-to-list 'org-after-todo-state-change-hook
  ;;              (lambda ()
  ;;                (when (equal org-state "DONE")
  ;;                  (my/org-roam-copy-todo-to-today))))
  )

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d!)")
        (sequence "GOAL(G)" "PROJ(P)" "|" "DONE(d!)")
        (sequence  "PLAN(p)" "REVIEW(r)" "|" "WAIT(w)" "BACK(b)")))

;; TODO: org-todo-keyword-faces
(setq org-todo-keyword-faces
      '(("GOAL" . (:foreground "orange red" :weight bold))
        ("WAIT" . (:foreground "HotPink2" :weight bold))
        ("BACK" . (:foreground "MediumPurple3" :weight bold))))

(setq org-modern-todo-faces
      '(("GOAL"
         :background "orange red"
         :foreground "white")
        ("PROJ"
         :background "gold"
         :foreground "black")))

;; Configure common tags
(setq-default org-tag-alist
              '((:startgroup)
                ("Areas")
                (:grouptags)
                ("@home" . ?H)
                ("@work" . ?W)
                (:endgroup)

                (:startgrouptag . nil)
                ("Contexts")
                (:grouptags)
                ("@computer" . ?C)
                ("@mobile" . ?M)
                ("@calls" . ?A)
                ("@errands" . ?E)
                (:endgrouptag)

                ;; Task Types
                (:startgrouptag . nil)
                ("Types")
                (:grouptags)
                ("@easy" . ?e)
                ("@hacking" . ?h)
                ("@writing" . ?w)
                ("@creative" . ?v)
                ("@accounting" . ?a)
                ("@email" . ?m)
                ("@system" . ?s)
                (:endgrouptag)

                ;; Workflow states
                (:startgroup . nil)
                ("States")
                (:grouptags)
                ("@plan" . ?p)
                ("@review" . ?r)
                ("@followup" . ?f)
                (:endgroup)))

(setq org-agenda-window-setup 'current-window)
(setq org-agenda-span 'day)
(setq org-agenda-start-with-log-mode t)

;; Make done tasks show up in the agenda log
(setq org-log-done 'time)
(setq org-log-into-drawer t)

;; Only make context tags inheritable (what about noexport?)
(setq org-use-tag-inheritance "^@")

;; Customize category display to improve Denote file appearance
(defun dw/workflow--get-agenda-category ()
  (if-let ((category (or (and (buffer-file-name)
                              (denote-retrieve-filename-title (buffer-file-name)))
                         (org-get-category))))
      (truncate-string-to-width
       category
       25
       nil
       ?\s
       "...")
    ""))

(setf (alist-get 'todo org-agenda-prefix-format)
      " %i %-12:(dw/workflow--get-agenda-category)")

(setf (alist-get 'agenda org-agenda-prefix-format)
      " %i %-26:(dw/workflow--get-agenda-category)%?-13t% s")

(setq org-agenda-custom-commands
      `(("d" "Dashboard"
         ((agenda "" ((org-deadline-warning-days 7)))
          (tags-todo "+PRIORITY=\"A\""
                     ((org-agenda-overriding-header "High Priority")))
          (todo "*" ((org-agenda-files '("~/Notes/Inbox.org"))
                     (org-agenda-overriding-header "Unfiled Inbox Tasks")))
          (tags-todo "+@followup" ((org-agenda-overriding-header "Needs Follow Up")))))

        ("u" tags-todo "+ALLTAGS=\"\""
         ((org-agenda-overriding-header "Untagged Tasks")))

        ("n" "Next Tasks"
         ((agenda "" ((org-deadline-warning-days 7)))
          (todo "NEXT"
                ((org-agenda-overriding-header "Next Tasks")))))

        ;; Low-effort next actions
        ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
         ((org-agenda-overriding-header "Low Effort Tasks")
          (org-agenda-max-todos 20)
          (org-agenda-files org-agenda-files)))))

(add-hook 'org-timer-set-hook #'org-clock-in)

(defun dw/get-todays-journal-file-name ()
  "Gets the journal file name for today's date"
  (interactive)
  (let* ((journal-file-name
          (expand-file-name
           (format-time-string "%Y/%Y-%2m-%B.org")
           (dw/org-path "Journal/")))
         (journal-year-dir (file-name-directory journal-file-name)))
    (if (not (file-directory-p journal-year-dir))
        (make-directory journal-year-dir))
    journal-file-name))


(defun dw/on-org-capture ()
  ;; Don't show the confirmation header text
  (setq header-line-format nil)

  ;; Control how some buffers are handled
  (let ((template (org-capture-get :key t)))
    (pcase template
      ("jj" (delete-other-windows)))))

(add-hook 'org-capture-mode-hook 'dw/on-org-capture)

(setq org-capture-templates
      `(("t" "Tasks")
        ("tt" "Task" entry (file ,(dw/org-path "Inbox.org"))
         "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
        ("ts" "Clocked Entry Subtask" entry (clock)
         "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

        ("j" "Journal Entries")
        ("je" "General Entry" entry
         (file+olp+datetree ,(dw/org-path "Journal.org"))
         "\n* %<%I:%M %p> - %^{Title} \n\n%?\n\n"
         :tree-type week
         :clock-in :clock-resume
         :empty-lines 1)
        ("jt" "Task Entry" entry
         (file+olp+datetree ,(dw/org-path "Journal.org"))
         "\n* %<%I:%M %p> - Task Notes: %a\n\n%?\n\n"
         :tree-type week
         :clock-in :clock-resume
         :empty-lines 1)
        ("jj" "Journal" entry
         (file+olp+datetree ,(dw/org-path "Journal.org"))
         "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
         :tree-type week
         :clock-in :clock-resume
         :empty-lines 1)

        ("m" "Metrics Capture")
        ("mw" "Weight" table-line (file+headline "~/Notes/Metrics.org" "Weight")
         "| %U | %^{Weight} | %^{Notes} |"
         :immediate-finish t
         :jump-to-captured t)
        ("mp" "Blood Pressure" table-line (file+headline "~/Notes/Metrics.org" "Blood Pressure")
         "| %U | %^{Systolic} | %^{Diastolic} | %^{BPM} | %^{Stress 1-5}"
         :immediate-finish t
         :jump-to-captured t)))

(string-split "| [2023-11-28 Tue 21:09] |      135 |        87 |  74 |            2 |" "|" t "[ ]*")

(defun dw/extract-bp-numbers-csv ()
  (interactive)
  (with-temp-file "bp-csv"
    (let* ((region (buffer-substring-no-properties (mark) (point))))
      (dolist (line (string-split region "\n" t "[ ]*"))
        (message "LINE: %s" line)
        (let ((parts (string-split line "|" t "[ ]*")))
          (insert (format "%s,%s,%s,%s,%s\n"
                          (nth 1 parts)
                          (nth 2 parts)
                          (nth 3 parts)
                          (nth 4 parts)
                          (nth 5 parts))))))))

(defun dw/extract-bp-numbers ()
  (interactive)
  (let* ((region (buffer-substring-no-properties (mark) (point)))
         (matched? (string-match (rx (and "|"
                                          (zero-or-more blank)
                                          (group (one-or-more digit))
                                          (zero-or-more blank)
                                          "|"
                                          (zero-or-more blank)
                                          (group (one-or-more digit))
                                          (zero-or-more blank)
                                          "|"
                                          (zero-or-more blank)
                                          (group (one-or-more digit))
                                          (zero-or-more blank)))
                                 region)))
                                        ;(message region)
    (if matched? (with-temp-buffer
                   (insert
                    (format "%s/%s, %s bpm"
                            (match-string 1 region)
                            (match-string 2 region)
                            (match-string 3 region)))
                   (clipboard-kill-region (point-min) (point-max)))
      (message "NO MATCH"))))

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(provide 'dw-workflow)
