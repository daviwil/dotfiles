;; -*- lexical-binding: t; -*-

(defvar dw/autorest-last-run nil
  "Details on the last AutoRest run to use with dw/rerun-autorest-command")

(defun dw/rerun-autorest-command ()
  (interactive)
  (if dw/autorest-last-run
      (apply #'dw/run-autorest-command dw/autorest-last-run)
      (message "No previous AutoRest run!")))

(defun dw/run-autorest-command (command-string run-name pre-command)
  (let ((run-message (format "Running AutoRest - %s" command-string)))
    (setq autorest-buffer (get-buffer-create (format "*AutoRest Output: %s*" run-name)))
    (with-current-buffer autorest-buffer
      (ansi-color-for-comint-mode-on)
      (comint-mode)
      (display-line-numbers-mode 0)
      (end-of-buffer)
      (insert "\n"
              (format-time-string "[%m/%d/%Y - %I:%M:%S %p]")
              "\n\n"
              run-message
              "\n\n")
      (when pre-command
        (insert "Pre-run command: " pre-command "\n\n")))

    ;; TODO: This buffer display function isn't perfect
    (message run-message)
    (display-buffer-pop-up-window autorest-buffer '((window-height . 13)))

    (setq dw/autorest-last-run (list command-string run-name pre-command))

    (let ((default-directory "~/Projects/Code/autorest/")
          (full-command
             (string-join (list (when pre-command
                                      (format "%s &&" pre-command))
                                "TERM=xterm-256color"
                                command-string)
                          " ")))
      (setq autorest-process (start-process-shell-command "server" autorest-buffer full-command)))

    (set-process-filter autorest-process 'comint-output-filter)))

(cl-defun dw/run-autorest (&key (run-name "adhoc")
                                input-file
                                (use '())
                                language
                                version
                                args
                                (inspector t)
                                debug
                                verbose
                                pre-command)
  (let* ((use-param (mapconcat (lambda (u) (format "--use:%s" u)) use " "))
         (misc-args (mapconcat (lambda (arg) arg) args " "))
         (args (list "autorest"
                     (when language (format "--%s" language))
                     use-param
                     misc-args
                     (if (s-ends-with? ".md" input-file)
                         input-file
                         (format "--input-file:%s" input-file))
                     (when inspector (format "--inspector --inspector.output-folder:./outputs/%s --inspector.clear-output-folder" run-name))
                     (when version (format "--version:%s" version))
                     (when verbose "--verbose")
                     (when debug "--debug")))
         (command-string (string-join args " ")))
    (message command-string)
    (dw/run-autorest-command command-string run-name pre-command)))

;; Keybindings to set up:
;; q - Close panel
;; C-c o - List output files

;; (dw/run-autorest
;;  :run-name "body-formdata"
;;  :language 'python
;;  :input-file "~/Projects/Code/autorest.megarepo/testserver/swagger/body-formdata.json"
;;  :use '("./modelerfour/modelerfour")
;;  :version "./autorest/core"
;;  :pre-command "pushd ./modelerfour/modelerfour && npm run build && popd")

(provide 'dw-autorest)
