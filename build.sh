mkdir -p ebin
find src/ -name *.erl | grep -v playdar_http | xargs erlc -I include/ -o ebin/  && \
cp src/playdar.app ebin/ && \
cd src/playdar_httpd/ && make ; cd -
