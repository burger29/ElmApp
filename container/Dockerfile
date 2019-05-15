FROM node:10.14.2

WORKDIR /root/elm
COPY package.json .
RUN npm install
COPY build.sh watch.sh /usr/local/bin/
ENV PATH="/root/elm/node_modules/.bin:$PATH"
