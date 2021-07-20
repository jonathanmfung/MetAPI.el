;;; MetAPI.el ---                                       -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Jonathan Fung

;; Author: Jonathan Fung <jonathanfung2000@gmail.com>
;; Keywords: art, API

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; A small package to fetch and display random art pieces from
;;; The Metropoliton Museum of Art Collection API.
;;;
;;; To use, run M-x met-run
;;; To customize output, (setq met-FIELD) defined on
;;; https://metmuseum.github.io/#object

;;; Code:

(require 'url)

(defconst met-BASE_URL "https://collectionapi.metmuseum.org")
(defconst met-OBJECTS_URL "/public/collection/v1/objects")

(defun met-url-request (url)
  "Retrives json request of URL as Lisp object."
  (with-current-buffer
      (url-retrieve-synchronously url)
    (goto-char (+ 1 url-http-end-of-headers))
    (json-read-object)))

(defun met-get-objects ()
  "Get all current Objects in API."
  (met-url-request (concat met-BASE_URL met-OBJECTS_URL)))

(defconst met-ALL-OBJECTS (met-get-objects))

(defun met-set-total (objects)
  "Check and set Total OBJECTS."
  (let ((total-objects (cdr (assoc 'total objects)))
        (total-objectIDs (length  (cdr (assoc 'objectIDs objects)))))
    (if (= total-objects total-objectIDs)
        total-objects
      nil)
    ))

(defconst met-TOTAL-OBJECTS (met-set-total met-ALL-OBJECTS))

(defun met-get-object (id)
  "Get object json from ID (Int).
METADATADATE as \"YYYY-MM-DD\"
DEPARTMENTIDS as Int."
  (met-url-request (concat
                    met-BASE_URL
                    met-OBJECTS_URL
                    "/"
                    (number-to-string id))))

(defun met-random-object-id ()
  "Get one random ID out of met-TOTAL-OBJECTS."
  (random (1+ met-TOTAL-OBJECTS)))

;; objectDate of this has, which does not get rendered correctly
;; http://www.ltg.ed.ac.uk/~richard/utf-8.cgi?input=2013&mode=hex
;; also seen in many other objects
(setq met-test-object (met-get-object 248731))

;; all custom variables for fields
;; for some reasion objectID needs to be t, or else no other vars are respected
(setq met-objectID t
      met-isHighlight nil
      met-accessionNumber nil
      met-accessionYear nil
      met-isPublicDomain nil
      met-primaryImage nil
      met-primaryImageSmall t
      met-additionalImages nil
      met-constituents nil
      met-department t
      met-objectName t
      met-culture t
      met-period t
      met-dynasty nil
      met-reign nil
      met-portfolio t
      met-artistRole t
      met-artistPrefix t
      met-artistDisplayName t
      met-artistDisplayBio t
      met-artistSuffix nil
      met-artistAlphaSort nil
      met-artistNationality t
      met-artistBeginDate nil
      met-artistEndDate nil
      met-artistGender nil
      met-artistWikidata_URL t
      met-artistULAN_URL nil
      met-objectDate t
      met-objectBeginDate nil
      met-objectEndDate nil
      met-medium t
      met-dimensions t                  ; dimensionsParsed doesn't seem to exist
      met-measurements nil
      met-title t
      met-creditLine nil
      met-geographyType t
      met-city t
      met-state nil
      met-county nil
      met-country t
      met-region nil
      met-subregion nil
      met-locale nil
      met-locus nil
      met-excavation nil
      met-river nil
      met-classification t
      met-rightsAndReproduction nil
      met-linkResource nil                ; same as objectURL
      met-metadataDate nil
      met-repository nil
      met-objectURL t                ; same as linkResource
      met-objectWikidata_URL nil
      met-tags nil
      met-isTimelineWork nil
      met-GalleryNumber nil)

(defun met-remove-field (object field)
  "Check value of met-FIELD defined by user.
Then delete FIELD from OBJECT object."
  ;; intern only returns raw symbol, needs to be eval'd
  (if (not (eval (intern (concat "met-" field))))
      (assq-delete-all
       (intern field)
       object)
    nil))

(defconst met-all-fields (cl-loop for cons in met-test-object collect (car cons)))
(defconst met-all-fields-str (mapcar #'symbol-name met-all-fields))

(defun met-filter-clean (object)
  "Apply 'met-remove-field' on OBJECT.
To remove all met-FIELD that are t."
  (let  ((obj (-distinct (cl-mapcar
                (apply-partially
                 #'met-remove-field
                 object)
                met-all-fields-str))))
    (rassq-delete-all "" (rassq-delete-all '[] (car (-non-nil obj))))))

;;; Printing and Formatting

(defun met-generate (filter-obj)
  "Insert FILTER-OBJ fields into buffer."
  (let ((inhibit-read-only t)
        (buf (get-buffer-create "*Met*")))
    (with-current-buffer
        buf
      (read-only-mode)
      (org-mode)
      (font-lock-mode -1)
      (spell-fu-mode -1)
      (erase-buffer)
      (newline 3)
      (insert (propertize (assoc-default 'title filter-obj) 'face `(:foreground ,(modus-themes-color 'blue) :height 200)))
      (newline 2)
      (if (assoc 'primaryImageSmall filter-obj) (progn (insert (concat "[[" (assoc-default 'primaryImageSmall filter-obj) "]]")) (newline)))
      (insert (make-string (window-width (get-buffer-window buf)) ?━))
      (newline 2)
      (insert (propertize (mapconcat #'identity (--map
                                                 (concat
                                                  (symbol-name (car it))
                                                  ": "
                                                  (format "%s" (cdr it)))
                                                 filter-obj) "\n") 'face `(:foreground ,(modus-themes-color 'magenta-intense))))
      (align-regexp (point-min) (point-max) "\\(\\s-*\\): ")
      (org-display-inline-images))
    (switch-to-buffer-other-window buf)))

(defun met-run ()
  "Run met program.
The Metropolitan Museum of Art Collection API.
https://metmuseum.github.io/"
  (interactive)
  (met-generate
   (met-filter-clean
    (let ((rand-obj (met-get-object (met-random-object-id))))
      (progn (while (eq 'message (caar rand-obj))
               (setq rand-obj (met-get-object (met-random-object-id))))
             rand-obj)))))

(provide 'MetAPI.el)
;;; MetAPI.el ends here
