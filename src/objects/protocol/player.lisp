;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; objects/protocol/player.lisp

(in-package #:gateway.objects/protocol)

(define-protocol player
    (:documentation "The PLAYER protocol describes objects representing ~
players in the Gateway system. Each player has a login and password that are ~
used for accessing the system, an email used for contacting, a display name, ~
and an activation status."
     :tags (:gateway :gateway-object :player)
     :dependencies (gateway-object identifiable with-creation-time activatable)
     :export t)
  (:class player (gateway-object identifiable with-creation-time activatable)
          ())
  "A player object. See protocol PLAYER for details."
  (:function username ((player player)) string)
  "Returns the username of the player."
  (:function (setf username) ((new-value string) (player player)) string)
  "Sets the username of the player."
  (:function email ((player player)) string)
  "Returns the email of the player."
  (:function (setf email) ((new-value string) (player player)) string)
  "Sets the email of the player."
  (:function display-name ((player player)) string)
  "Returns the display name of the player."
  (:function (setf display-name) ((new-value string) (player player)) string)
  "Sets the display name of the player."
  (:function password-matches-p ((player player) (password string)) boolean)
  "Returns true if the password matches the player's, and false otherwise."
  (:function (setf password) ((new-value string) (player player)) string)
  "Sets the password of the player.")

(execute-protocol player)
