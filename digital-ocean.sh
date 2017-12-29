#!/usr/bin/env sh
set -o errexit -o xtrace

# This script provisions a machine to run Monadoc. It expects to be run on a
# new DigitalOcean droplet running Ubuntu 17.10. It should be safe to run
# multiple times.

# Install dependencies.
add-apt-repository --yes ppa:certbot/certbot
apt-get update
apt-get upgrade --assume-yes
apt-get install --assume-yes nginx python-certbot-nginx python3-pip ufw

# Set up firewall.
yes | ufw enable
ufw allow OpenSSH
ufw allow 'Nginx Full'

# Set up Nginx.
rm --force /etc/nginx/sites-enabled/default
cat <<EOF | tee /etc/nginx/sites-available/monadoc
server {
  listen 80 default_server;
  server_name monadoc.com;
  location / {
    proxy_pass http://127.0.0.1:8080;
  }
}
EOF
ln --force --symbolic /etc/nginx/sites-available/monadoc /etc/nginx/sites-enabled/monadoc

# Set up SSL.
certbot --agree-tos -d monadoc.com --email taylor@fausak.me -n --nginx
systemctl restart nginx

# Install Docker.
which docker || curl --location get.docker.com | sh

# Install Docker Compose.
which docker-compose || pip3 install docker-compose

# Install Monadoc.
test -d /root/monadoc || git clone https://github.com/tfausak/monadoc /root/monadoc
cd /root/monadoc

# Run Monadoc.
git reset --hard origin/master
git pull
docker-compose pull
docker-compose up -d
