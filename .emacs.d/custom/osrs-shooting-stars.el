(setq osrs-shooting-stars-locations
      '((0 . "Asgarnia")
	(1 . "Karamja or Crandor")
	(2 . "Feldip Hills or Isle of Souls")
	(3 . "Fossil Island or Mos Le'Harmless")
	(4 . "Fremnik Lands or Lunar Isle")
	(5 . "Great Kourend")
	(6 . "Kandarin")
	(7 . "Kebos Lowlands")
	(8 . "Kharidian Desert")
	(9 . "Misthalin")
	(10 . "Morytania")
	(11 . "Piscatoris or the Gnome Stronghold")
	(12 . "Tirannwn")
	(13 . "Wilderness")
	(14 . "Unknown")))

(defun osrs-get-shooting-stars ()
  "Fetches the current known list of shooting stars in Old School RuneScape."
  (let ((url-request-extra-headers
	 '(("Authorization" . "global"))))
    (with-current-buffer
	(url-retrieve-synchronously "https://z9smj03u77.execute-api.us-east-1.amazonaws.com/stars")
      (goto-char url-http-end-of-headers)
      (let ((json-key-type 'string))
	(json-read)))))

(defun osrs-shooting-star-world (star_data)
  "Returns the world a shooting star is on given a STAR_DATA object.

STAR_DATA is single object from the JSON array that is returned from a call to the `osrs-get-shooting-stars'
function."
  (cdr (car (cdr star_data))))

(defun osrs-shooting-star-location (star_data)
  "Returns the location a shooting star is on given a STAR_DATA object.

STAR_DATA is a single object from the JSON arry that is returned from a call to the `osrs-get-shooting-stars'
function."
  (cdr (assq (cdr (car star_data)) osrs-shooting-stars-locations)))

(defun osrs-shooting-star-min-time (star_data)
  "Returns the minimum time in minutes until a shooting star spawns given a STAR_DATA object.

STAR_DATA is a single object from the JSON arry that is returned from a call to the `osrs-get-shooting-stars'
function."
  (/ (subtract-time
      (cdr (car (cdr (cdr star_data))))
      (time-convert (current-time) 'integer))
     60))

(defun osrs-shooting-star-max-time (star_data)
  "Returns the maximum time in minutes until a shooting star spawns given a STAR_DATA object.

STAR_DATA is a single object from the JSON arry that is returned from a call to the `osrs-get-shooting-stars'
function."
  ( / (subtract-time
       (cdr (car (cdr (cdr (cdr star_data)))))
       (time-convert (current-time) 'integer))
      60))

(defun osrs-shooting-stars (minutes)
  "Opens a temporary buffer containing a list of worlds and locations of future shooting stars in Old School Runescape within the last MINUTES."
  (interactive "p")
  (with-output-to-temp-buffer "*OSRS Shooting Stars*"
    (temp-buffer-resize-mode)
    (message "Minutes: %d" minutes)
    (mapcar (lambda (location)
	      (let ((world (osrs-shooting-star-world location))
		    (location (osrs-shooting-star-location location))
		    (min-time (osrs-shooting-star-min-time location))
		    (max-time (osrs-shooting-star-max-time location)))
		(when (>= max-time (* -1 minutes))
		  (princ (format "World %d: %s (%d minutes - %d minutes)\n"
				 world
				 location
				 min-time
				 max-time)))))
	    (osrs-get-shooting-stars))))

(provide 'osrs-shooting-stars)
