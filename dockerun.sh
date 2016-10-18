#!/usr/bin/sh

sudo docker run -it -d -p 80:80 --name web_site -v `pwd`/public:/var/www/public -v `pwd`/images:/var/www/images -v `pwd`/nginx_conf:/etc/nginx/conf.d -v `pwd`/nginx_log:/var/log/nginx nginx /bin/sh -c "/usr/sbin/nginx && while true; do sleep 100; done"
