;;;; Keys - server

(defparameter *server-x*
  #xD845F10CE76048DE98DC5192D8D3D0FEB96AF4AB264BE5613F8889B832165859)

(defparameter *server-y*
  #xD91E1A48857E3FE5D6868CE0AE2A28EF57FB9EF8069EA07BB14C730EB467780D)

(defparameter *server-keys*
  (let ((x (ironclad:integer-to-octets *server-x*))
        (y (ironclad:integer-to-octets *server-y*)))
    (list (ironclad:make-private-key :curve25519 :x x :y y)
          (ironclad:make-public-key :curve25519 :y y))))

;;;; Keys - client

(defparameter *client-x*
  #x781E5749D420EF0E19E4E70E1B1FD243B76589F4B0A1E7B45325F56BD573F669)

(defparameter *client-y*
  #x416B8EB05BE0394FF374AC5ED20EAD2AF352685143D966C33EE9696A9848B971)

(defparameter *client-keys*
  (let ((x (ironclad:integer-to-octets *client-x*))
        (y (ironclad:integer-to-octets *client-y*)))
    (list (ironclad:make-private-key :curve25519 :x x :y y)
          (ironclad:make-public-key :curve25519 :y y))))

;;;; Keys - shared

(defparameter *shared-key*
  #x410171045F235931BEC252BB5DD4A5E9BCF7B635D62BF894E7F961DB759C6174)

;;;; Encryption handshake

S> (0 :HELLO "Gateway Server" "git-1234abc" "raptor.systems")
C> (0 :OK "Gateway Client" "git-7654fed" "")

C> (2 :CRYPTOGRAPHY?)
S> (2 :OK :CURVE25519
      "D91E1A48857E3FE5D6868CE0AE2A28EF57FB9EF8069EA07BB14C730EB467780D")

C> (4 :CRYPTOGRAPHY :CURVE25519
      "416B8EB05BE0394FF374AC5ED20EAD2AF352685143D966C33EE9696A9848B971")
S> (4 :OK :ENCRYPTING)
