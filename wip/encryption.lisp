;;; Keys - server

(defparameter *server-x*
  #xD845F10CE76048DE98DC5192D8D3D0FEB96AF4AB264BE5613F8889B832165859)

(defparameter *server-y*
  #xD91E1A48857E3FE5D6868CE0AE2A28EF57FB9EF8069EA07BB14C730EB467780D)

(defparameter *server-keys*
  (let ((x (ironclad:integer-to-octets *server-x*))
        (y (ironclad:integer-to-octets *server-y*)))
    (list (ironclad:make-private-key :curve25519 :x x :y y)
          (ironclad:make-public-key :curve25519 :y y))))

;;; Keys - client

(defparameter *client-x*
  #x781E5749D420EF0E19E4E70E1B1FD243B76589F4B0A1E7B45325F56BD573F669)

(defparameter *client-y*
  #x416B8EB05BE0394FF374AC5ED20EAD2AF352685143D966C33EE9696A9848B971)

(defparameter *client-keys*
  (let ((x (ironclad:integer-to-octets *client-x*))
        (y (ironclad:integer-to-octets *client-y*)))
    (list (ironclad:make-private-key :curve25519 :x x :y y)
          (ironclad:make-public-key :curve25519 :y y))))

;;; Keys - shared

(defparameter *shared-key*
  #x410171045F235931BEC252BB5DD4A5E9BCF7B635D62BF894E7F961DB759C6174)

;;; Encryption handshake
;;; Lowercase letters mean plaintext, uppercase - AES-encrypted

c> (0 :HELLO "Gateway Client" "git-7654fed")
s> (0 :HELLO "Gateway Server" "git-1234abc" "raptor.systems")

c> (2 :CRYPTOGRAPHY?)
s> (2 :OK :AES-CTR :CURVE25519 :KEY
      "d91e1a48857e3fe5d6868ce0ae2a28ef57fb9ef8069ea07bb14c730eb467780d"
          :IV "2ed95c6bbdb7672ed9120da14517da06")

c> (4 :CRYPTOGRAPHY :AES-CTR :CURVE25519
      "416b8eb05be0394ff374ac5ed20ead2af352685143d966c33ee9696a9848b971")
S> (4 :OK)

C> (6 :SAY "Hello, world!")
S> (6 :OK)

S> (1 :SAY "Hello, world!")
C> (1 :OK)
