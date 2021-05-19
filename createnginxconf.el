;; we want this :
;;       location /bluespark {
;;                alias /data/dist.verdazo.com/bluespark;
;;                autoindex on;
;;                auth_basic "Private download";
;;                auth_basic_user_file /data/users/bluespark;
;;        }

(defun my/write (name)
  (insert (format
           "      location /%s {                                  \n\
               alias /data/dist.verdazo.com/%s;            \n\
               autoindex on;                               \n\
               auth_basic \"Private download\";            \n\
               auth_basic_user_file /data/users/%s;        \n\
       }                                                   \n\ "
           name name name)))



(with-current-buffer "*scratch*"
  (mapc
   #'my/write
   '(athabasca   capp      corex     hammerhead  longrun    perpetual    razor     taqa
    bankers     cardinal  cp        harvest     mancal     petronas     saguaro   torc
    birchcliff  cenovus   crew      inplay      mcdaniel   peyto        sproule   tourmaline
    blackswan   cequence  ember     karve       obsidian   prairieprov  strath    velvet
    bluespark   cona      enerplus  kelt        orlen      prairiesky   surge     verdazo
    burgess     cor4      gear      leucrotta   paramount  progress     tamarack)))


