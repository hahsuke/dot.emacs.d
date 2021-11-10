;;; w32-symlinks.el --- MS Windows symbolic link (shortcut) support

;; Copyright (C) 2002, 2003 Francis J. Wright, 2005 Lars Hansen

;; Author: Francis J. Wright <F.J.Wright at qmul.ac.uk>
;; Last-Updated: 22-11-2005 18:00 UTC
;; By: Lars Hansen <larsh at soem dot dk>
;; URL: http://www.emacswiki.org/emacs/w32-symlinks.el
;; Keywords: convenience, files, unix

;; This file is not part of GNU Emacs.

;; w32-symlinks is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; w32-symlinks is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(defun w32-symlinks-parse-symlink (file-name)
  "Optionally parse FILE-NAME as a MS Windows symlink file, if possible."
  (condition-case nil
      (and (string-match "\\.lnk\\'" file-name)
           (w32-symlinks-parse-shortcut file-name))
    (error nil)))

(defun w32-symlinks-buffer-substring-as-int (start length)
  "Return contents of part of the current buffer as an unsigned integer.
START is a character position\; LENGTH specifies the length of the
integer in bytes and should be 1, 2 or 4.
Assumes byte order is low to high (little-endian)."
  (let ((idx (+ start length))
        (int 0))
    ;; Base (radix) using unsigned char digits is 2^8 = 256.
    (while (>= (setq idx (1- idx)) start)
      (setq int (+ (* 256 int) (char-after idx))))
    int))

(defun w32-symlinks-parse-shortcut (file)
  "Return file or directory referenced by MS Windows shortcut (.lnk) FILE.
Return nil if the file cannot be parsed."
  ;; Based on "The Windows Shortcut File Format" as
  ;; reverse-engineered by Jesse Hager <jessehager at iname.com>
  ;; available from http://www.wotsit.org/download.asp?f=shortcut.
  (with-temp-buffer
    (set-buffer-multibyte nil) ; added by smzht
    (let ((inhibit-file-name-handlers
           (cons 'w32-symlinks-file-name-handler
                 (and (eq inhibit-file-name-operation 'insert-file-contents)
                      inhibit-file-name-handlers))))
      (insert-file-contents-literally file)) ; Eli Zaretskii
    (and
     ;; Parse the File Header Table.
     ;; Check for Shell Link identifier (4 bytes)
     ;; followed by Shell Link GUID (16 bytes):
     (string= (buffer-substring 1 21)  ; otherwise not a shortcut file
              "L\0\0\0\x01\x14\x02\0\0\0\0\0\xC0\0\0\0\0\0\0\x46")
     ;; Get the main flags dword at offset 14h.
     (let ((flags (w32-symlinks-buffer-substring-as-int (+ (point) ?\x14) 4))
           target)
       ;; Skip to end of Header:
       (forward-char ?\x4C)
       (if (= (logand flags 1) 1)
           ;; Flag 0 (2^0=1) set means Shell Item Id List present, so
           ;; skip it.  The list length is the first word, which must
           ;; also be skipped:
           (forward-char
            (+ 2 (w32-symlinks-buffer-substring-as-int (point) 2))))
       (if (= (logand flags 2) 2)
           ;; Flag 1 (2^1=2) set means File Location Info Table
           ;; present, so parse it.
           (progn
             ;; The full file pathname is (generally) stored in two
             ;; pieces: a head depending on whether the file is on a
             ;; local or network volume and a remaining pathname tail.
             ;; Get and check the volume flags dword at offset 8h:
             (setq flags (w32-symlinks-buffer-substring-as-int
                          (+ (point) ?\x8) 4))
             (if (/= (logand flags 3) 0) ; Must have bit 0 or 1 set.
                 (let ((head            ; Get local or network
                        (save-excursion ; pathname head.
                          ;; If bit 0 then local else network:
                          (if (setq flags (= (logand flags 1) 1))
                              ;; Go to the base pathname on the local
                              ;; system at the offset specified as a
                              ;; dword at offset 10h:
                              (forward-char
                               (w32-symlinks-buffer-substring-as-int
                                (+ (point) ?\x10) 4))
                            ;; Go to the network volume table at the
                            ;; offset specified as a dword at offset 14h:
                            (forward-char
                             (w32-symlinks-buffer-substring-as-int
                              (+ (point) ?\x14) 4))
                            ;; Go to the network share name at offset 14h:
                            (forward-char ?\x14))
                          (buffer-substring (point)
                                            (1- (search-forward "\0")))))
                       (tail         ; Get the remaining pathname tail
                        (progn          ; specified as a dword at
                          (forward-char         ; offset 18h.
                           (w32-symlinks-buffer-substring-as-int
                            (+ (point) ?\x18) 4))
                          (buffer-substring (point)
                                            (1- (search-forward "\0"))))))
                   (setq target
                         ;; Network share name needs trailing \ added:
                         (concat head
                                 (unless (or flags (string= tail "")) "\\")
                                 tail)))))
         ;; Otherwise, continue parsing...
         ;; NB: Shortcuts generated using WSH seem to use Unicode.
         ;; May be flag bit 7 indicates use of Unicode (other than in
         ;; the Shell Item Id List), but I have no confirmation of
         ;; that, so for now I use the hack below to detect Unicode.
         (if (= (logand flags 4) 4)
             ;; Flag 2 (2^2=4) set means Description String present,
             ;; so skip it.  The string length is the first word,
             ;; which must also be skipped.
             (let ((len (w32-symlinks-buffer-substring-as-int (point) 2)))
               (forward-char 2)                 ; skip length word
               (forward-char
                (if (eq (char-after (1+ (point))) 0) ; assume unicode
                    (* len 2)
                  len))))
         (if (= (logand flags 8) 8)
             ;; Flag 3 (2^3=8) set means Relative Path String present,
             ;; so parse it.  The string length is the first word.
             (let ((len (w32-symlinks-buffer-substring-as-int (point) 2)))
               (forward-char 2)                 ; skip length word
               (setq target
                     (if (eq (char-after (1+ (point))) 0) ; assume unicode
                         (decode-coding-string
                          (buffer-substring (point) (+ (point) (* len 2)))
                          'utf-16le) ; modified by smzht
                       (buffer-substring (point) (+ (point) len)))))))
       (when target
         (setq target (decode-coding-string target 'undecided)) ; modified by smzht
         (let ((i (length target)))
           (while (>= (setq i (1- i)) 0)
             (if (eq (aref target i) ?\\) (aset target i ?/))))
         target)))))

(defun set-attr-symlink (file-and-attr)
  (when (and (cdr file-and-attr)
             (not (cadr file-and-attr))
             (setcar (cdr file-and-attr) (w32-symlinks-parse-symlink (car file-and-attr))))
    (aset (nth 9 file-and-attr) 0 ?l)))
