(setq mac? (eq system-type 'darwin))
(when mac?
  (let ((usr-local "/usr/local/bin"))
    (add-to-list 'exec-path usr-local)
    (setenv "PATH" (concat usr-local path-separator (getenv "PATH")))))
