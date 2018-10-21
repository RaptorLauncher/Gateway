;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; engine/protocol/player.lisp

(in-package #:gateway.engine/protocol)

(define-protocol player
    (:documentation "The PLAYER protocol describes objects representing ~
players in the Gateway system. Each player has a login and password that are ~
used for accessing the system, an email used for contacting, a display name, ~
and an activation status."
     :tags (:gateway :engine :gateway-object :player)
     :dependencies (gateway-object identifiable)
     :export t)
  (:class player (gateway-object identifiable with-creation-time) ())
  "A player object. See protocol PLAYER for details."
  (:function login ((player player)) string)
  "Returns the login of the player."
  (:function (setf login) ((new-value string) (player player)) string)
  "Sets the login of the player."
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
  "Sets the password of the player."
  (:function activatedp ((player player)) boolean)
  "Returns true if the player is activated, false otherwise."
  (:function (setf activatedp) ((new-value boolean) (player player)) boolean)
  "Sets the activation status of the player.")

(execute-protocol player)
