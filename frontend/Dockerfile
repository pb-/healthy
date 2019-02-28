FROM nginx:alpine

COPY resources/nginx.conf /etc/nginx/conf.d/default.conf
COPY out/main.js resources/public/* /usr/share/nginx/html/
