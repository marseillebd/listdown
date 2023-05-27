# listdown

Build with
```
cabal build
```

Spin up a local test server with something like the following:
```
cabal run listdown-server -- --port 3000 --users-dir users --static-dir app-web/static
```

Deploy to a server with
```
cabal build
scp -r deploy $SERVER:listdown
scp "$(find dist-newstyle -type f -executable -name 'listdown-server')" $SERVER:listdown/usr/lib/listdown
scp -r app-web/static $SERVER:listdown/usr/lib/listdown
ssh $SERVER

# move application into place
cd listdown
chgrp -R www-data usr/lib/listdown/
sudo rm -r /usr/lib/listdown
sudo mv usr/lib/listdown /usr/lib/listdown
rmdir -p usr/lib

# set up variable data (skip this after the first time)
sudo mkdir -p /var/lib/listdown
sudo chown okuno:www-data /var/lib/listdown

# if data formats have changed, backup and upgrade those data

# move nginx config into place
sudo chown root:root etc/nginx/sites-available/*
sudo mv etc/nginx/sites-available/* /etc/nginx/sites-available/
sudo ln -s /etc/nginx/sites-available/ld.okuno.info /etc/nginx/sites-enabled/ld.okuno.info
sudo service nginx restart

# move systemd service into place
sudo chown root:root etc/systemd/system/listdown.service
sudo mv etc/systemd/system/listdown.service /etc/systemd/system/listdown.service
sudo service listdown restart

# cleanup
cd ~
rmdir listdown/
```

When deployed, the data is stored at `/var/lib/listdown/users`.