;;; module-notmuch.el

;; Author: Mark Feller <mark.feller@member.fsf.org>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(use-package notmuch
  :config
  (progn (setq user-full-name "Mark Feller")

         (setq message-sendmail-envelope-from 'header)
         (add-hook 'message-send-mail-hook 'cg-feed-msmtp)

         (setq notmuch-saved-searches
               '((:name "inbox"       :query "tag:inbox"              :key "i")
                 (:name "unread"      :query "tag:unread"             :key "u")
                 (:name "sent"        :query "tag:sent"               :key "t")
                 (:name "drafts"      :query "tag:draft"              :key "d")
                 (:name "guix-devel"  :query "guix-devel tag:unread"  :key "g")
                 (:name "emacs-devel" :query "emacs-devel tag:unread" :key "e")))))

(provide 'module-notmuch)

;;; module-notmuch.el ends here
