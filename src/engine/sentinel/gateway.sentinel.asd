;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; GATEWAY
;;;; © Michał "phoe" Herda 2016-2018
;;;; connector/gateway.sentinel.asd

;; Class SENTINEL
;; - AUTHENTICATION authenticator connection -> T/account/NIL
;; - (SETF AUTHENTICATION) new-value authenticator connection
;; - AUTHORIZE authenticator connection command -> ()/!?
;; - ENCRYPT authenticator connection command -> string/!?
;; - DECRYPT authenticator connection string -> command/!?

;; TODO take care of SERIALIZABLE protocol, somehow
;; TODO define a command as a CLOS object
;; TODO Gateway protocol on top of cable
