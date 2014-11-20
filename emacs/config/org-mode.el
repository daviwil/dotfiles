;; Add org-mode git repository to the load path
(add-to-list 'load-path (expand-file-name "~/Repositories/org-mode/lisp"))

(use-package org
  :ensure t
  :config
  (progn
    ;; Configure paths
    (setq org-directory "~/Notes")
    (setq org-default-notes-file (concat org-directory "/Inbox.org"))
    ;; (setq org-blank-before-new-entry
    ;; 	  '((heading . t)
    ;; 	    (plain-list-item . auto)))

    ;; Configure the agenda
    (setq org-agenda-window-setup 'other-window)
    (setq org-agenda-span 'day)
    (setq org-stuck-projects '("+LEVEL=2/TODO" ("NEXT") nil ""))
    (setq org-agenda-start-with-log-mode t)
    (setq org-agenda-files
	  '("~/Notes/Inbox.org" 
	    "~/Notes/Habits.org" 
	    "~/Notes/Personal.org" 
	    "~/Notes/Work.org" 
	    "~/Notes/Projects.org" 
	    "~/Notes/Workflow.org"))
    
    ;; Configure archive and refile
    (setq org-archive-location "~/Notes/Journal.org::datetree/* Completed Tasks")
    (setq org-refile-targets 
	  (quote ((nil :maxlevel . 9)
		  (org-agenda-files :maxlevel . 9))))
    (setq org-refile-use-outline-path 'file)
    (setq org-outline-path-complete-in-steps nil)
    
    ;; Configure TODO settings
    (setq org-log-done 'time)
    (setq org-log-into-drawer t)
    (setq org-log-reschedule 'time)
    (setq org-log-refile 'time)
    (setq org-datetree-add-timestamp 'inactive)
    (setq org-habit-graph-column 60)
    (setq org-todo-keywords
	  '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
	    (sequence "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

    ;; Configure capture templates
    (setq org-capture-templates
	  '(("t" "Task" entry (file+headline "~/Notes/Inbox.org" "Tasks")
             "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
	    ("j" "Journal" entry (file+datetree "~/Notes/Journal.org")
             "* %<%l:%M %p> %?\n " :empty-lines 1)
	    ("w" "Weight" table-line (file+headline "~/Notes/Metrics.org" "Weight")
	     "| %U | %^{Weight} | %^{Notes} |" :kill-buffer)
	    ("p" "Blood Pressure" table-line (file+headline "~/Notes/Metrics.org" "Blood Pressure")
	     "| %U | %^{Systolic} | %^{Diastolic} | %^{Notes}" :kill-buffer)
	    ))

    ;; This variable is my own, not part of org-mode.  It is
    ;; used in the custom agenda commands block below.
    (setq org-project-files 
	  '("~/Notes/Personal.org"
	    "~/Notes/Projects.org"
	    "~/Notes/Work.org"))

    ;; Configure custom agenda views
    (setq org-agenda-custom-commands
	  '(
	    ;; Daily dashboard
	    ("d" "Dashboard" 
	     ((agenda "")
	      ;; (tags-todo "+PRIORITY=\"A\""
	      ;; 	    ((org-agenda-overriding-header "High Priority Tasks")
	      ;;        (org-agenda-files org-project-files)))
	      (todo "TODO"
	      	    ((org-agenda-overriding-header "Workflow")
	             (org-agenda-files '("~/Notes/Workflow.org"))))
	      (todo "NEXT"
	      	    ((org-agenda-overriding-header "Next Actions")
	             (org-agenda-files org-project-files)))
	      (tags-todo "-recurring+LEVEL=2/TODO"
	      	    ((org-agenda-overriding-header "Active Projects")
	             (org-agenda-files org-project-files)))
	      (todo "TODO"
		    ((org-agenda-overriding-header "Unprocessed Inbox Tasks")
		     (org-agenda-files '("~/Notes/Inbox.org"))
		     (org-agenda-text-search-extra-files nil)))))

	    ;; Active projects
	    ("p" "Active Projects"
	     ((agenda "")
	      (todo "ACTIVE"
		    ((org-agenda-overriding-header "Active Projects")
		     (org-agenda-max-todos 5)
		     (org-agenda-files org-project-files)))))

	    ;; Workflow status dashboard
	    ("w" "Workflow Status"
	     ((todo "ACTIVE"
	      	    ((org-agenda-overriding-header "Active Projects")
	             (org-agenda-files org-project-files)))
	      (todo "READY"
	      	    ((org-agenda-overriding-header "Projects Ready for Work")
	             (org-agenda-files org-project-files)))
	      (todo "PLAN"
	      	    ((org-agenda-overriding-header "Projects in Planning")
		     (org-agenda-todo-list-sublevels nil)
	             (org-agenda-files org-project-files)))
	      (todo "REVIEW"
	      	    ((org-agenda-overriding-header "Projects in Review")
	             (org-agenda-files org-project-files)))
	      (tags-todo "+LEVEL<3/TODO"
	      	    ((org-agenda-overriding-header "Projects Needing Conversion")
		     (org-agenda-todo-list-sublevels nil)
	             (org-agenda-files org-project-files)))
	      (todo "COMPLETED"
	      	    ((org-agenda-overriding-header "Completed Projects")
	             (org-agenda-files org-project-files)))
	      (todo "CANC"
	      	    ((org-agenda-overriding-header "Cancelled Projects")
	             (org-agenda-files org-project-files)))))
	      
	    ;; Projects on hold
	    ("h" tags-todo "+LEVEL=2/+HOLD"
	     ((org-agenda-overriding-header "On-hold Projects")
	      (org-agenda-files org-project-files)))
	    
	    ;; Low-effort next actions
	    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
	     ((org-agenda-overriding-header "Low Effort Next Actions")
	      (org-agenda-max-todos 20)
	      (org-agenda-files org-project-files)))
	    ))

    ;; Configure common tags
    (setq org-tag-alist (quote ((:startgroup)
				; Put mutually exclusive tags here
				(:endgroup)
				("@errand" . ?e)
				("@home" . ?H)
				("@work" . ?w)
				("waiting" . ?w)
				("onhold" . ?h)
				("projects" ?p)
				("personal" . ?P)
				("note" . ?n)
				("idea" . ?i)
				("journal" . ?j)
				("publish" . ?b)
				("recurring" . ?r)
				("cancelled" . ?c))))

    ;; Configure task state change tag triggers
    (setq org-todo-state-tags-triggers
      (quote (("CANC" ("cancelled" . t))
              ("WAIT" ("waiting" . t))
              ("HOLD" ("waiting") ("onhold" . t))
              (done ("waiting") ("onhold"))
              ("TODO" ("waiting") ("cancelled") ("onhold"))
              ("DONE" ("waiting") ("cancelled") ("onhold")))))

    ;; Configure publishing projects
    (require 'ox-publish)
    (setq org-publish-project-alist
	  '(("daviwil.com"
	     :base-directory "~/Writing/daviwil-com/org-files"
	     :base-extension "org"
	     :publishing-directory "~/Writing/daviwil-com/_posts"
	     :recursive t
	     :publishing-function org-html-publish-to-html
	     :headline-levels 4 
	     :html-extension "html"
	     :with-toc nil
	     :section-numbers nil
	     :html-preamble nil
	     :body-only t)))
    
    ;; Configure modules
    (setq org-modules 
	  '(org-bbdb org-crypt org-gnus org-habit org-bookmark org-eshell org-eval org-notmuch org-man org-irc))


    ;; Configure org-crypt
    (require 'org-crypt)
    (org-crypt-use-before-save-magic)
    (setq org-tags-exclude-from-inheritance (quote ("crypt")))
    (setq org-crypt-key nil) ; No key, just use a passphrase

    ;; Configure key bindings
    (global-set-key "\C-cl" 'org-store-link)
    (global-set-key "\C-cc" 'org-capture)
    (global-set-key "\C-ca" 'org-agenda)
    (global-set-key "\C-cb" 'org-iswitchb)
))

(use-package org-pomodoro
  :ensure t
  :config
  (progn
    ; Re-bind org-clock-in/out keys to org-pomodoro
    (global-unset-key (kbd "C-c C-x C-i"))
    (global-unset-key (kbd "C-c C-x C-o"))
    (define-key org-mode-map (kbd "C-c C-x C-i") 'org-pomodoro)
    (define-key org-mode-map (kbd "C-c C-x C-o") 'org-pomodoro)
    (global-set-key (kbd "C-c C-x C-i") 'org-pomodoro)
    (global-set-key (kbd "C-c C-x C-o") 'org-pomodoro)))

(use-package org-journal
  :ensure t
  :config
  (progn
    (setq org-journal-dir "~/Notes/Journal/")
    (setq org-journal-file-format "%Y-%m-%d.org")))
 
(use-package deft
  :ensure t
  :config
  (progn
    ;; Helpful page: http://www.jontourage.com/2013/08/15/setting-up-deft-mode-in-emacs-with-org-mode/
    (setq deft-extension "org")
    (setq deft-text-mode 'org-mode)
    (setq deft-directory "~/Notes")
    (setq deft-use-filename-as-title t)
    (global-set-key (kbd "C-c <C-return>") 'deft)))
   
