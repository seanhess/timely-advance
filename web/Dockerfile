FROM node:8

WORKDIR /root

ADD build build
ADD src src
ADD elm.json elm.json
ADD package.json package.json
ADD package-lock.json package-lock.json

RUN npm install
RUN npm run build
RUN mkdir -p /var/www
RUN cp -R ./build/ /var/www/html/
