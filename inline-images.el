(defun replace-image-refs (output)
  (when (string-match "#P\"\\(.*\\)\"" output)
    (let* ((filename (match-string 1 output))
           (type (intern-soft (file-name-extension filename)))
           (image (and (file-exists-p filename)
                       (image-type-available-p type)
                       (create-image filename type nil :nonce (random)))))
      (when image
        (put-text-property (match-beginning 0) (match-end 0)
                           'display image output))))
  output)

(add-hook 'comint-preoutput-filter-functions 'replace-image-refs)
