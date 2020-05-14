FROM library/postgres:alpine

COPY party.sql /docker-entrypoint-initdb.d/
