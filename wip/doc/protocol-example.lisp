-> '(HELLO CLIENT "Gateway" VERSION "0.0.0")

<- '((OK HELLO)
     SERVER "Gateway"
     VERSION "0.0.0"
     DOMAIN "origin.gateway.local")

-> '(LOGIN EMAIL "aza@dorano.com" PASSWORD "superSecretPassword")

<- '((OK LOGIN))

-> '(DESCRIBE GATEWAYS)

<- '((OK DESCRIBE)
     (GATEWAY "origin.gateway.local"
      DISPLAY-NAME "Origin"
      ADMINISTRATOR "778a9f5a-0bfc-4881-a187-bcf803933675"
      ONLINE 3
      PLAYERS 40
      PERSONAS 203
      TIMELINES 3
      CHAPTERS 27
      POSTS 2846))

-> '(DESCRIBE TIMELINES GATEWAY "origin.gateway.local")

<- '((OK DESCRIBE)
     (TIMELINE "066e410872fe11e8adc0fa7ae01bbebc"
      GATEWAY "origin.gateway.local"
      DISPLAY-NAME "Actually This Is Just Casual Chat Here"
      CHAPTERS 8 POSTS 194 PERSONAS 12 VISIBILITY PUBLIC
      MODIFIED "2018-10-21T14:32:27.187698+02:00")
     (TIMELINE "066e36f472fe11e8adc0fa7ae01bbebc"
      GATEWAY "origin.gateway.local"
      DISPLAY-NAME "Lots of Very Good RP"
      CHAPTERS 6 POSTS 80 PERSONAS 7 VISIBILITY UNLISTED
      MODIFIED "2018-10-21T14:35:52.191077+02:00")
     (TIMELINE "066e3c3a72fe11e8adc0fa7ae01bbebc"
      GATEWAY "origin.gateway.local"
      DISPLAY-NAME "The Castle of Raptors"
      CHAPTERS 2 POSTS 16 PERSONAS 2 VISIBILITY PRIVATE
      MODIFIED "2018-10-21T14:36:40.340785+02:00"))

<- '(DESCRIBE CHAPTERS TIMELINE "3364f6760e4f4575beee052f4e3a0462")

-> '((OK DESCRIBE)
     (CHAPTER "066e422072fe11e8adc0fa7ae01bbebc"
      TIMELINE "3364f6760e4f4575beee052f4e3a0462"
      DISPLAY-NAME "Prologue"
      POSTS 2 PERSONAS 2 VISIBILITY PRIVATE
      MODIFIED "2018-10-21T14:32:29.826630+02:00")
     (CHAPTER "066e45f472fe11e8adc0fa7ae01bbebc"
      TIMELINE "3364f6760e4f4575beee052f4e3a0462"
      DISPLAY-NAME "First Raptors"
      POSTS 14 PERSONAS 2 VISIBILITY PRIVATE
      MODIFIED "2018-10-21T14:36:40.340785+02:00"))

-> '(DESCRIBE POSTS CHAPTER "066e422072fe11e8adc0fa7ae01bbebc")

<- '((OK DESCRIBE)
     (POST "765bff81-78d5-410c-a4fc-630e0007fd49"
      CONTENTS "Purus cursus gravida mollis sapien semper gravida..."
      CHAPTER "066e422072fe11e8adc0fa7ae01bbebc"
      PLAYER "778a9f5a-0bfc-4881-a187-bcf803933675"
      PERSONA "bccde48d-64ee-4498-87e7-30ed2bb78c67"
      CREATED "2018-10-20T15:29:23.724431+02:00"
      MODIFIED "2018-10-20T15:29:23.724431+02:00")
     (POST "a9a78521-ac01-4e77-8199-82d4bec9207b"
      CONTENTS "Eiusmod sed placerat interdum elementum. Magnis..."
      CHAPTER "066e422072fe11e8adc0fa7ae01bbebc"
      PLAYER "778a9f5a-0bfc-4881-a187-bcf803933675"
      PERSONA "80a10982-ef60-4789-aa21-cebdf9635b2f"
      CREATED "2018-10-21T14:32:29.826630+02:00"
      MODIFIED "2018-10-21T14:32:29.826630+02:00"))

-> '(DESCRIBE PLAYER "778a9f5a-0bfc-4881-a187-bcf803933675")

<- '((OK DESCRIBE)
     (PLAYER "778a9f5a-0bfc-4881-a187-bcf803933675"
      USERNAME "Aza_dOrano"
      DISPLAY-NAME "Aza d'Orano"
      PERSONAS 5
      POSTS 173
      GATEWAY LOCAL
      STATUS (ADMINISTRATOR MODERATOR YOU)))

-> '(DESCRIBE PERSONA "bccde48d-64ee-4498-87e7-30ed2bb78c67")

<- '((OK DESCRIBE)
     (PERSONA "bccde48d-64ee-4498-87e7-30ed2bb78c67"
      DISPLAY-NAME "Aza d'Orano"
      PLAYER "778a9f5a-0bfc-4881-a187-bcf803933675"
      BORROWERS ()
      DESCRIPTION "Lorem ipsum dolor sit amet enim..."
      GENDER F
      POSTS 83
      CHAPTERS 15
      TIMELINES 2
      CREATED "2018-10-18T14:32:29.826630+02:00"
      MODIFIED "2018-10-18T14:32:29.826630+02:00"))
