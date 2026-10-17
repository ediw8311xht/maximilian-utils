
(in-package :maximilian-utils)

(defun timestamp-to-ntp (s &optional (epoch :unix))
  (case epoch
    (:unix (- s 2208988800))
    (t     s)))

(defun utc-format (s &key (epoch :ntp) utc stream)
  (multiple-value-call #'format stream s
    (if utc (decode-universal-time 
              (timestamp-to-ntp s epoch)) 
        (get-decoded-time))))

(defun utc-alist (&optional utc)
  "return a plist of utc datetime elements (default is current datetime)"
  (mapcar #'cons '(:second :minute :hour :day :month :year :day-of-week :daylight-savings :timezone)
          (multiple-value-list (if utc (decode-universal-time utc) 
                                   (get-decoded-time)))))
