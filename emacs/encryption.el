(require 'epa)

(setenv "GPA_AGENT_INFO" nil)

(setq karl/gpg-key-email-address "karlfroldan@gmail.com")

(setq-default epa-file-select-keys '(karl/gpg-key-email-address))
(setq epa-file-encrypt-to karl/gpg-key-email-address)
