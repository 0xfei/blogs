+++
date = "2016-10-18T16:34:21+08:00"
draft = true
title = "把网站部署到docker"
description = "use docker"
tags = ["docker"]
topics = ["docker"]
+++

这个博客是hugo做的纯静态网站，部署到docker可以说毫无难度。直接映射本地文件作数据卷。其实只是把nginx放到docker中运行，方便之后的扩展。


安装docker之后，pull官方nginx源。然后运行下面的脚本。


```

#!/usr/bin/sh

sudo docker run -it -d -p 80:80 --name web_site -v `pwd`/public:/var/www/public -v `pwd`/images:/var/www/images -v `pwd`/nginx_conf:/etc/nginx/conf.d -v `pwd`/nginx_log:/var/log/nginx nginx /bin/sh -c "/usr/sbin/nginx && while true; do sleep 100; done"

```

因为是静态网站，我的发布形式很野蛮——打包之后远程传到服务器再解包。由于中间把public目录删除了，所以容器找不到数据卷。修改auto_push.sh脚本就行。


```
#!/bin/bash

echo "Push public/* to 0x01f.com !"

echo "Start pushing..."

filename="1.tar.gz"
origfiles="public/*"
remote="root@0x01f.com:/var/www/"
sshdir="/var/www/"
# Do not execute `rm -rf public`
cmd="cd ${sshdir}; tar -xzf $filename -C $sshdir; rm $filename"

tar -czf $filename $origfiles
scp $filename $remote
ssh root@0x01f.com $cmd

echo "Remove $filename."
rm $filename

echo "Done!"
```
